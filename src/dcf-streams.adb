--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2008 - 2018 Gautier de Montmollin (maintainer)
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

with Ada.IO_Exceptions;

package body DCF.Streams is

   function Open (File_Name : String) return Open_File is
   begin
      return Result : Open_File do
         Stream_IO.Open (Result.File, Stream_IO.File_Mode (In_File), File_Name);
      end return;
   end Open;

   function Create (File_Name : String) return Open_File is
   begin
      return Result : Open_File do
         Stream_IO.Create (Result.File, Stream_IO.File_Mode (Out_File), File_Name);
      end return;
   end Create;

   overriding
   procedure Finalize (Object : in out Open_File) is
   begin
      if not Object.Finalized then
         if Stream_IO.Is_Open (Object.File) then
            Stream_IO.Close (Object.File);
         end if;
         Object.Finalized := True;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------

   procedure Set_Name (S : in out Root_Zipstream_Type; Name : String; UTF_8 : Boolean := False) is
   begin
      S.Name  := To_Unbounded_String (Name);
      S.UTF_8 := UTF_8;
   end Set_Name;

   function Get_Name (S : in Root_Zipstream_Type) return String is
   begin
      return To_String (S.Name);
   end Get_Name;

   function UTF_8_Encoding (S : in Root_Zipstream_Type) return Boolean is (S.UTF_8);

   procedure Set_Time (S : in out Root_Zipstream_Type; Modification_Time : Time) is
   begin
      S.Modification_Time := Modification_Time;
   end Set_Time;

   function Get_Time (S : in Root_Zipstream_Type) return Time is
   begin
      return S.Modification_Time;
   end Get_Time;

   function Convert (Date : in Dos_Time) return Time is
   begin
      return Time (Date);  --  Currently a trivial conversion
   end Convert;

   function Convert (Date : in Time) return Dos_Time is
   begin
      return Dos_Time (Date);  --  Currently a trivial conversion
   end Convert;

   ----------------------------------------------
   --  File_Zipstream: stream based on a file  --
   ----------------------------------------------

   function Open (File_Name : String) return File_Zipstream is
   begin
      return (Root_Stream_Type with
        File   => Open (File_Name),
        Name   => To_Unbounded_String (File_Name),
        others => <>);
   end Open;

   function Create (File_Name : String) return File_Zipstream is
   begin
      return (Root_Stream_Type with
        File   => Create (File_Name),
        Name   => To_Unbounded_String (File_Name),
        others => <>);
   end Create;

   overriding
   procedure Read
     (Stream : in out File_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
   begin
      Ada.Streams.Stream_IO.Read (Stream.File.File, Item, Last);
   end Read;

   overriding
   procedure Write (Stream : in out File_Zipstream; Item : Stream_Element_Array) is
   begin
      Ada.Streams.Stream_IO.Write (Stream.File.File, Item);
   end Write;

   overriding
   procedure Set_Index (S : in out File_Zipstream; To : Zs_Index_Type) is
   begin
      Ada.Streams.Stream_IO.Set_Index (S.File.File, Ada.Streams.Stream_IO.Positive_Count (To));
   end Set_Index;

   overriding
   function Size (S : in File_Zipstream) return Zs_Size_Type is
   begin
      return Zs_Size_Type (Ada.Streams.Stream_IO.Size (S.File.File));
   end Size;

   overriding
   function Index (S : in File_Zipstream) return Zs_Index_Type is
   begin
      return Zs_Index_Type (Ada.Streams.Stream_IO.Index (S.File.File));
   end Index;

   overriding
   function End_Of_Stream (S : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.End_Of_File (S.File.File);
   end End_Of_Stream;

   --------------------------------------------------------------
   --  Array_Zipstream: stream based on a Stream_Element_Array --
   --------------------------------------------------------------

   overriding
   procedure Read
     (Stream : in out Array_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      if Stream.EOF then
         --  No elements transferred: Last := Item'First - 1 (See RM 13.13.1 (8))
         --  T'Read will raise End_Error (See RM 13.13.2 (37))
         if Item'First > Stream_Element_Offset'First then
            Last := Item'First - 1;
            return;
         else
            --  RM 13.13.1 (11) requires Read to raise Constraint_Error
            --  if Item'First = Stream_Element_Offset'First
            raise Constraint_Error;
         end if;
      end if;

      if Stream.Elements'Last - Stream.Index >= Item'Length then
         --  Normal case: even after reading, the index will be in the range
         Last         := Item'Last;
         Item         := Stream.Elements (Stream.Index .. Stream.Index + Item'Length - 1);
         Stream.Index := Stream.Index + Item'Length;

         pragma Assert (Stream.Index <= Stream.Elements'Last);
         --  At least one element is left to be read, EOF not possible
      else
         --  Special case: we exhaust the buffer
         Last                      := Item'First + (Stream.Elements'Last - Stream.Index);
         Item (Item'First .. Last) := Stream.Elements (Stream.Index .. Stream.Elements'Last);

         Stream.EOF := True;
         --  If Last < Item'Last, the T'Read attribute raises End_Error
         --  because of the incomplete reading
      end if;
   end Read;

   overriding
   procedure Write
     (Stream : in out Array_Zipstream;
      Item   :        Stream_Element_Array) is
   begin
      if Stream.EOF then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      if Stream.Elements'Last - Stream.Index >= Item'Length then
         Stream.Elements (Stream.Index .. Stream.Index + Item'Length - 1) := Item;
         Stream.Index := Stream.Index + Item'Length;

         pragma Assert (Stream.Index <= Stream.Elements'Last);
         --  At least one element is left to be written, EOF not possible
      elsif Stream.Elements'Last - Stream.Index + 1 = Item'Length then
         Stream.Elements (Stream.Index .. Stream.Elements'Last) := Item;

         Stream.EOF := True;
      else
         --  RM 13.13.1 (9) requires the item to be appended to the stream,
         --  but this might fail because the Stream_Element_Array is bounded.
         --
         --  Don't write if writing would be incomplete
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Write;

   overriding
   procedure Set_Index (Stream : in out Array_Zipstream; To : Zs_Index_Type) is
      Index : constant Stream_Element_Offset := Stream_Element_Offset (To);
   begin
      if (if Stream.Elements'Length > 0 then
            Index not in Stream.Elements'Range
          else
            Index /= Stream.Elements'First)
      then
         raise Constraint_Error;
      end if;

      Stream.Index := Index;
      Stream.EOF   := False;
   end Set_Index;

end DCF.Streams;
