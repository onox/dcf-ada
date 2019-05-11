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

package body Zip_Streams is

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

   --  Ada.Calendar versions

   procedure Set_Time (S : out Root_Zipstream_Type'Class; Modification_Time : Ada.Calendar.Time) is
   begin
      Set_Time (S, Calendar.Convert (Modification_Time));
   end Set_Time;

   function Get_Time (S : in Root_Zipstream_Type'Class) return Ada.Calendar.Time is
   begin
      return Calendar.Convert (Get_Time (S));
   end Get_Time;

   procedure Set_Unicode_Name_Flag (S : out Root_Zipstream_Type; Value : in Boolean) is
   begin
      S.Is_Unicode_Name := Value;
   end Set_Unicode_Name_Flag;

   function Is_Unicode_Name (S : in Root_Zipstream_Type) return Boolean is
   begin
      return S.Is_Unicode_Name;
   end Is_Unicode_Name;

   procedure Set_Read_Only_Flag (S : out Root_Zipstream_Type; Value : in Boolean) is
   begin
      S.Is_Read_Only := Value;
   end Set_Read_Only_Flag;

   function Is_Read_Only (S : in Root_Zipstream_Type) return Boolean is
   begin
      return S.Is_Read_Only;
   end Is_Read_Only;

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
   procedure Open (Str : in out File_Zipstream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Open
        (Str.File,
         Ada.Streams.Stream_IO.File_Mode (Mode),
         To_String (Str.Name),
         Form => To_String (Form_For_Io_Open_And_Create));
   end Open;

   procedure Create (Str : in out File_Zipstream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Create
        (Str.File,
         Ada.Streams.Stream_IO.File_Mode (Mode),
         To_String (Str.Name),
         Form => To_String (Form_For_Io_Open_And_Create));
   end Create;

   procedure Close (Str : in out File_Zipstream) is
   begin
      Ada.Streams.Stream_IO.Close (Str.File);
   end Close;

   function Is_Open (Str : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.Is_Open (Str.File);
   end Is_Open;

   overriding
   procedure Read
     (Stream : in out File_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
   begin
      Ada.Streams.Stream_IO.Read (Stream.File, Item, Last);
   end Read;

   overriding
   procedure Write (Stream : in out File_Zipstream; Item : Stream_Element_Array) is
   begin
      Ada.Streams.Stream_IO.Write (Stream.File, Item);
   end Write;

   overriding
   procedure Set_Index (S : in out File_Zipstream; To : Zs_Index_Type) is
   begin
      Ada.Streams.Stream_IO.Set_Index (S.File, Ada.Streams.Stream_IO.Positive_Count (To));
   end Set_Index;

   overriding
   function Size (S : in File_Zipstream) return Zs_Size_Type is
   begin
      return Zs_Size_Type (Ada.Streams.Stream_IO.Size (S.File));
   end Size;

   overriding
   function Index (S : in File_Zipstream) return Zs_Index_Type is
   begin
      return Zs_Index_Type (Ada.Streams.Stream_IO.Index (S.File));
   end Index;

   overriding
   function End_Of_Stream (S : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.End_Of_File (S.File);
   end End_Of_Stream;

   package body Calendar is

      ------------------------------------------------
      --  Time = DOS Time. Valid through Year 2107  --
      ------------------------------------------------

      procedure Split
        (Date    :     Time;
         Year    : out Year_Number;
         Month   : out Month_Number;
         Day     : out Day_Number;
         Seconds : out Day_Duration)
      is
         D_Date       : constant Integer := Integer (Date / 65536);
         D_Time       : constant Integer := Integer (Date and 65535);
         X            : Integer;
         Hours        : Integer;
         Minutes      : Integer;
         Seconds_Only : Integer;
      begin
         Year := 1980 + D_Date / 512;
         X    := (D_Date / 32) mod 16;
         if X not in Month_Number then -- that is 0, or in 13..15
            raise Time_Error;
         end if;
         Month := X;
         X     := D_Date mod 32;
         if X not in Day_Number then -- that is 0
            raise Time_Error;
         end if;
         Day          := X;
         Hours        := D_Time / 2048;
         Minutes      := (D_Time / 32) mod 64;
         Seconds_Only := 2 * (D_Time mod 32);
         if Hours not in 0 .. 23 or Minutes not in 0 .. 59 or Seconds_Only not in 0 .. 59 then
            raise Time_Error;
         end if;
         Seconds := Day_Duration (Hours * 3600 + Minutes * 60 + Seconds_Only);
      end Split;

      function Time_Of
        (Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration := 0.0) return Time
      is
         Year_2       : Integer := Year;
         Hours        : Unsigned_32;
         Minutes      : Unsigned_32;
         Seconds_Only : Unsigned_32;
         Seconds_Day  : Unsigned_32;
         Result       : Unsigned_32;
      begin
         if Year_2 < 1980 then  --  Avoid invalid DOS date
            Year_2 := 1980;
         end if;
         Seconds_Day  := Unsigned_32 (Seconds);
         Hours        := Seconds_Day / 3600;
         Minutes      := (Seconds_Day / 60) mod 60;
         Seconds_Only := Seconds_Day mod 60;
         Result       :=
           --  MSDN formula for encoding:
           Unsigned_32 ((Year_2 - 1980) * 512 + Month * 32 + Day) * 65536  --  Date
           +
           Hours * 2048 +
           Minutes * 32 +
           Seconds_Only / 2; -- Time
         return Time (Result);
      end Time_Of;

      function ">" (Left, Right : Time) return Boolean is
      begin
         return Unsigned_32 (Left) > Unsigned_32 (Right);
      end ">";

      function Convert (Date : in Ada.Calendar.Time) return Time is
         Year            : Year_Number;
         Month           : Month_Number;
         Day             : Day_Number;
         Seconds_Day_Dur : Day_Duration;
      begin
         Split (Date, Year, Month, Day, Seconds_Day_Dur);
         return Time_Of (Year, Month, Day, Seconds_Day_Dur);
      end Convert;

      function Convert (Date : in Time) return Ada.Calendar.Time is
         Year            : Year_Number;
         Month           : Month_Number;
         Day             : Day_Number;
         Seconds_Day_Dur : Day_Duration;
      begin
         Split (Date, Year, Month, Day, Seconds_Day_Dur);
         return Time_Of (Year, Month, Day, Seconds_Day_Dur);
      end Convert;

      function Convert (Date : in Dos_Time) return Time is
      begin
         return Time (Date);  --  Currently a trivial conversion
      end Convert;

      function Convert (Date : in Time) return Dos_Time is
      begin
         return Dos_Time (Date);  --  Currently a trivial conversion
      end Convert;

   end Calendar;

end Zip_Streams;
