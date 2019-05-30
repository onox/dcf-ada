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

with Ada.Characters.Latin_1;

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

   procedure Set_Name (S : in out Root_Zipstream_Type; Name : String) is
   begin
      S.Name := To_Unbounded_String (Name);
   end Set_Name;

   function Get_Name (S : in Root_Zipstream_Type) return String is
   begin
      return To_String (S.Name);
   end Get_Name;

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

   -----------------------------------------------------------------------
   --  Unbounded_Stream: stream based on an in-memory Unbounded_String  --
   -----------------------------------------------------------------------

   procedure Get (Str : Memory_Zipstream; Unb : out Unbounded_String) is
   begin
      Unb := Str.Unb;
   end Get;

   procedure Set (Str : in out Memory_Zipstream; Unb : Unbounded_String) is
   begin
      Str.Unb := Null_Unbounded_String;  --  Clear the content of the stream
      Str.Unb := Unb;
      Str.Loc := 1;
   end Set;

   overriding
   procedure Read
     (Stream : in out Memory_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      --  Item is read from the stream. If (and only if) the stream is
      --  exhausted, Last will be < Item'Last. In that case, T'Read will
      --  raise an End_Error exception.
      --
      --  Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
      --  explanations by Tucker Taft
      --
      Last := Item'First - 1;
      --  If Item is empty, the following loop is skipped; if Stream.Loc
      --  is already indexing out of Stream.Unb, that value is also appropriate
      for I in Item'Range loop
         Item (I)   := Character'Pos (Element (Stream.Unb, Stream.Loc));
         Stream.Loc := Stream.Loc + 1;
         Last       := I;
      end loop;
   exception
      when Ada.Strings.Index_Error =>
         --  What could be read has been read; T'Read will raise End_Error
         null;
   end Read;

   Max_Chunk_Size : constant := 16 * 1024;

   overriding
   procedure Write (Stream : in out Memory_Zipstream; Item : Stream_Element_Array) is
      I          : Stream_Element_Offset := Item'First;
      Chunk_Size : Integer;
      Tmp        : String (1 .. Max_Chunk_Size);
   begin
      while I <= Item'Last loop
         Chunk_Size := Integer'Min (Integer (Item'Last - I + 1), Max_Chunk_Size);
         if Stream.Loc > Length (Stream.Unb) then
            --  ...we are off the string's bounds, we need to extend it
            for J in 1 .. Chunk_Size loop
               Tmp (J) := Character'Val (Item (I));
               I       := I + 1;
            end loop;
            Append (Stream.Unb, Tmp (1 .. Chunk_Size));
         else
            --  ...we can work (at least for a part) within the string's bounds
            Chunk_Size := Integer'Min (Chunk_Size, Length (Stream.Unb) - Stream.Loc + 1);
            for J in 0 .. Chunk_Size - 1 loop
               Replace_Element (Stream.Unb, Stream.Loc + J, Character'Val (Item (I)));
               --  GNAT 2008's Replace_Slice does something very general
               --  even in the trivial case where one can make:
               --  Source.Reference(Low..High):= By;
               --  -> still faster with elem by elem replacement
               --  Anyway, this place is not critical for zipping: only the
               --  local header before compressed data is rewritten after
               --  compression. So usually, we are off bounds.
               I := I + 1;
            end loop;
         end if;
         Stream.Loc := Stream.Loc + Chunk_Size;
      end loop;
   end Write;

   overriding
   procedure Set_Index (S : in out Memory_Zipstream; To : Zs_Index_Type) is
      I, Chunk_Size : Zs_Size_Type;

      use Ada.Characters.Latin_1;
   begin
      if To > Zs_Size_Type (Length (S.Unb)) then
         --  ...we are off the string's bounds, we need to extend it
         I := Zs_Size_Type (Length (S.Unb)) + 1;
         while I <= To loop
            Chunk_Size := Zs_Size_Type'Min (To - I + 1, Zs_Size_Type (Max_Chunk_Size));
            Append (S.Unb, (1 .. Integer (Chunk_Size) => NUL));
            I := I + Chunk_Size;
         end loop;
      end if;
      S.Loc := Integer (To);
   end Set_Index;

   overriding
   function Size (S : in Memory_Zipstream) return Zs_Size_Type is
   begin
      return Zs_Size_Type (Length (S.Unb));
   end Size;

   overriding
   function Index (S : in Memory_Zipstream) return Zs_Index_Type is
   begin
      return Zs_Index_Type (S.Loc);
   end Index;

   overriding
   function End_Of_Stream (S : in Memory_Zipstream) return Boolean is
   begin
      return Size (S) < Index (S);
   end End_Of_Stream;

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

   function Is_Open (Str : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.Is_Open (Str.File.File);
   end Is_Open;

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

end DCF.Streams;