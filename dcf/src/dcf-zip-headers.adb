--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2000 - 2018 Gautier de Montmollin
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

package body DCF.Zip.Headers is

   use Ada.Streams;

   -----------------------------------------------------------
   -- Byte array <-> various integers, with Intel endianess --
   -----------------------------------------------------------

   --  Get numbers with correct trucmuche endian, to ensure
   --  correct header loading on some non-Intel machines

   generic
      type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
   function Intel_X86_Number (B : Stream_Element_Array) return Number;

   function Intel_X86_Number (B : Stream_Element_Array) return Number is
      N : Number := 0;
   begin
      for I in reverse B'Range loop
         N := N * 256 + Number (B (I));
      end loop;
      return N;
   end Intel_X86_Number;

   function Intel_Nb is new Intel_X86_Number (Unsigned_16);
   function Intel_Nb is new Intel_X86_Number (Unsigned_32);

   --  Put numbers with correct endianess as bytes

   generic
      type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
      Size : Stream_Element_Count;
   function Intel_X86_Buffer (N : Number) return Stream_Element_Array;

   function Intel_X86_Buffer (N : Number) return Stream_Element_Array is
      B : Stream_Element_Array (1 .. Size);
      M : Number := N;
   begin
      for I in B'Range loop
         B (I) := Stream_Element (M and 255);
         M     := M / 256;
      end loop;
      return B;
   end Intel_X86_Buffer;

   function Intel_Bf is new Intel_X86_Buffer (Unsigned_16, 2);
   function Intel_Bf is new Intel_X86_Buffer (Unsigned_32, 4);

   ---------------------
   --  PK signatures  --
   ---------------------

   function Pk_Signature (Buf : Stream_Element_Array; Code : Stream_Element) return Boolean is
   begin
      return Buf (Buf'First .. Buf'First + 3) = (16#50#, 16#4B#, Code, Code + 1);
      --  PK12, PK34, ...
   end Pk_Signature;

   procedure Pk_Signature (Buf : in out Stream_Element_Array; Code : Stream_Element) is
   begin
      Buf (1 .. 4) := (16#50#, 16#4B#, Code, Code + 1); -- PK12, PK34, ...
   end Pk_Signature;

   ---------------------------------------------------------
   --  PKZIP file header, as in central directory - PK12  --
   ---------------------------------------------------------
   procedure Read_And_Check
     (Stream : in out Root_Zipstream_Type'Class;
      Header :    out Central_File_Header)
   is
      Chb : Stream_Element_Array (1 .. 46);
   begin
      Blockread (Stream, Chb);

      if not Pk_Signature (Chb, 1) then
         raise Bad_Central_Header;
      end if;

      Header.Made_By_Version                   := Intel_Nb (Chb (5 .. 6));
      Header.Short_Info.Needed_Extract_Version := Intel_Nb (Chb (7 .. 8));
      Header.Short_Info.Bit_Flag               := Intel_Nb (Chb (9 .. 10));
      Header.Short_Info.Zip_Type               := Intel_Nb (Chb (11 .. 12));
      Header.Short_Info.File_Timedate          :=
        DCF.Streams.Convert (Unsigned_32'(Intel_Nb (Chb (13 .. 16))));
      Header.Short_Info.Dd.Crc_32            := Intel_Nb (Chb (17 .. 20));
      Header.Short_Info.Dd.Compressed_Size   := Intel_Nb (Chb (21 .. 24));
      Header.Short_Info.Dd.Uncompressed_Size := Intel_Nb (Chb (25 .. 28));
      Header.Short_Info.Filename_Length      := Intel_Nb (Chb (29 .. 30));
      Header.Short_Info.Extra_Field_Length   := Intel_Nb (Chb (31 .. 32));
      Header.Comment_Length                  := Intel_Nb (Chb (33 .. 34));
      Header.Disk_Number_Start               := Intel_Nb (Chb (35 .. 36));
      Header.Internal_Attributes             := Intel_Nb (Chb (37 .. 38));
      Header.External_Attributes             := Intel_Nb (Chb (39 .. 42));
      Header.Local_Header_Offset             := Intel_Nb (Chb (43 .. 46));

      if not Valid_Version (Header.Short_Info) then
         raise Bad_Central_Header with "Archive needs invalid version to extract";
      end if;

      if Header.Disk_Number_Start /= 0 then
         raise Bad_Central_Header with "Archive may not span multiple volumes";
      end if;

      if not Valid_Bitflag (Header.Short_Info) then
         raise Bad_Central_Header with "Archive uses prohibited features";
      end if;
   end Read_And_Check;

   procedure Write (Stream : in out Root_Zipstream_Type'Class; Header : in Central_File_Header) is
      Chb : Stream_Element_Array (1 .. 46);
   begin
      Pk_Signature (Chb, 1);

      Chb (5 .. 6)   := Intel_Bf (Header.Made_By_Version);
      Chb (7 .. 8)   := Intel_Bf (Header.Short_Info.Needed_Extract_Version);
      Chb (9 .. 10)  := Intel_Bf (Header.Short_Info.Bit_Flag);
      Chb (11 .. 12) := Intel_Bf (Header.Short_Info.Zip_Type);
      Chb (13 .. 16) := Intel_Bf (DCF.Streams.Convert (Header.Short_Info.File_Timedate));
      Chb (17 .. 20) := Intel_Bf (Header.Short_Info.Dd.Crc_32);
      Chb (21 .. 24) := Intel_Bf (Header.Short_Info.Dd.Compressed_Size);
      Chb (25 .. 28) := Intel_Bf (Header.Short_Info.Dd.Uncompressed_Size);
      Chb (29 .. 30) := Intel_Bf (Header.Short_Info.Filename_Length);
      Chb (31 .. 32) := Intel_Bf (Header.Short_Info.Extra_Field_Length);
      Chb (33 .. 34) := Intel_Bf (Header.Comment_Length);
      Chb (35 .. 36) := Intel_Bf (Header.Disk_Number_Start);
      Chb (37 .. 38) := Intel_Bf (Header.Internal_Attributes);
      Chb (39 .. 42) := Intel_Bf (Header.External_Attributes);
      Chb (43 .. 46) := Intel_Bf (Header.Local_Header_Offset);

      Stream.Write (Chb);
   end Write;

   -------------------------------------------------------------------------
   --  PKZIP local file header, in front of every file in archive - PK34  --
   -------------------------------------------------------------------------
   procedure Read_And_Check
     (Stream : in out Root_Zipstream_Type'Class;
      Header :    out Local_File_Header)
   is
      Lhb : Stream_Element_Array (1 .. 30);
   begin
      Blockread (Stream, Lhb);

      if not Pk_Signature (Lhb, 3) then
         raise Bad_Local_Header;
      end if;

      Header.Needed_Extract_Version := Intel_Nb (Lhb (5 .. 6));
      Header.Bit_Flag               := Intel_Nb (Lhb (7 .. 8));
      Header.Zip_Type               := Intel_Nb (Lhb (9 .. 10));
      Header.File_Timedate          :=
        DCF.Streams.Convert (Unsigned_32'(Intel_Nb (Lhb (11 .. 14))));
      Header.Dd.Crc_32            := Intel_Nb (Lhb (15 .. 18));
      Header.Dd.Compressed_Size   := Intel_Nb (Lhb (19 .. 22));
      Header.Dd.Uncompressed_Size := Intel_Nb (Lhb (23 .. 26));
      Header.Filename_Length      := Intel_Nb (Lhb (27 .. 28));
      Header.Extra_Field_Length   := Intel_Nb (Lhb (29 .. 30));

      if not Valid_Version (Header) then
         raise Bad_Local_Header with "Archived file needs invalid version to extract";
      end if;

      if not Valid_Bitflag (Header) then
         raise Bad_Local_Header with "Archived file uses prohibited features";
      end if;
   end Read_And_Check;

   procedure Write (Stream : in out Root_Zipstream_Type'Class; Header : in Local_File_Header) is
      Lhb : Stream_Element_Array (1 .. 30);
   begin
      Pk_Signature (Lhb, 3);

      Lhb (5 .. 6)   := Intel_Bf (Header.Needed_Extract_Version);
      Lhb (7 .. 8)   := Intel_Bf (Header.Bit_Flag);
      Lhb (9 .. 10)  := Intel_Bf (Header.Zip_Type);
      Lhb (11 .. 14) := Intel_Bf (DCF.Streams.Convert (Header.File_Timedate));
      Lhb (15 .. 18) := Intel_Bf (Header.Dd.Crc_32);
      Lhb (19 .. 22) := Intel_Bf (Header.Dd.Compressed_Size);
      Lhb (23 .. 26) := Intel_Bf (Header.Dd.Uncompressed_Size);
      Lhb (27 .. 28) := Intel_Bf (Header.Filename_Length);
      Lhb (29 .. 30) := Intel_Bf (Header.Extra_Field_Length);

      Stream.Write (Lhb);
   end Write;

   ---------------------------------------------
   --  PKZIP end-of-central-directory - PK56  --
   ---------------------------------------------
   procedure Copy_And_Check (Buffer : in Stream_Element_Array; The_End : out End_Of_Central_Dir) is
      O : constant Stream_Element_Offset := Buffer'First - 1;
   begin
      if not Pk_Signature (Buffer, 5) then
         raise Bad_End;
      end if;

      The_End.Disknum             := Intel_Nb (Buffer (O + 5 .. O + 6));
      The_End.Disknum_With_Start  := Intel_Nb (Buffer (O + 7 .. O + 8));
      The_End.Disk_Total_Entries  := Intel_Nb (Buffer (O + 9 .. O + 10));
      The_End.Total_Entries       := Intel_Nb (Buffer (O + 11 .. O + 12));
      The_End.Central_Dir_Size    := Intel_Nb (Buffer (O + 13 .. O + 16));
      The_End.Central_Dir_Offset  := Intel_Nb (Buffer (O + 17 .. O + 20));
      The_End.Main_Comment_Length := Intel_Nb (Buffer (O + 21 .. O + 22));
   end Copy_And_Check;

   procedure Read_And_Check
     (Stream  : in out Root_Zipstream_Type'Class;
      The_End :    out End_Of_Central_Dir)
   is
      Buffer : Stream_Element_Array (1 .. 22);
   begin
      Blockread (Stream, Buffer);
      Copy_And_Check (Buffer, The_End);
   end Read_And_Check;

   procedure Load (Stream : in out Root_Zipstream_Type'Class; The_End : out End_Of_Central_Dir) is
      Min_End_Start : Zs_Index_Type;  --  min_end_start >= 1
      Max_Comment   : constant := 65_535;
   --  In appnote.txt :
   --  .ZIP file comment length        2 bytes
   begin
      if Size (Stream) < 22 then
         raise Bad_End;
      end if;
      --  20-Jun-2001: abandon search below min_end_start.
      if Size (Stream) <= Max_Comment then
         Min_End_Start := 1;
      else
         Min_End_Start := Size (Stream) - Max_Comment;
      end if;
      Set_Index (Stream, Min_End_Start);
      declare
         --  We copy a large chunk of the zip stream's tail into a buffer.
         Large_Buffer : Stream_Element_Array
           (0 .. Stream_Element_Count (Size (Stream) - Min_End_Start));
         Ilb          : Stream_Element_Offset;
         X            : Zs_Size_Type;
      begin
         Blockread (Stream, Large_Buffer);
         for I in reverse Min_End_Start .. Size (Stream) - 21 loop
            --  Yes, we must _search_ for the header...
            --  because PKWARE put a variable-size comment _after_ it 8-(
            Ilb := Stream_Element_Offset (I - Min_End_Start);
            if Pk_Signature (Large_Buffer (Ilb .. Ilb + 3), 5) then
               Copy_And_Check (Large_Buffer (Ilb .. Ilb + 21), The_End);
               --  At this point, the buffer was successfully read, the_end is
               --  is set with its standard contents.
               --
               --  This is the *real* position of the end-of-central-directory block to begin with:
               X := I;
               --  We subtract the *theoretical* (stored) position of the end-of-central-directory.
               --  The theoretical position is equal to central_dir_offset + central_dir_size.
               --  The theoretical position should be smaller or equal than the real position -
               --  unless the archive is corrupted.
               --  We do it step by step, because ZS_Size_Type was modular until rev. 644.
               --  Now it's a signed 64 bits, but we don't want to change anything again...
               X := X - 1;
               --  i >= 1, so no dragons here. The "- 1" is for adapting
               --  from the 1-based Ada index.

               --  Fuzzy value, will trigger bad_end
               exit when Zs_Size_Type (The_End.Central_Dir_Offset) > X;

               --  Fuzzy value, will trigger bad_end
               X := X - Zs_Size_Type (The_End.Central_Dir_Offset);
               exit when Zs_Size_Type (The_End.Central_Dir_Size) > X;

               X := X - Zs_Size_Type (The_End.Central_Dir_Size);
               --  Now, x is the difference : real - theoretical.
               --    x > 0  if the archive was appended to another file (typically an executable
               --           for self-extraction purposes).
               --    x = 0  if this is a "pure" Zip archive.
               The_End.Offset_Shifting := X;
               Set_Index (Stream, I + 22);
               return;  -- The_End found and filled -> exit
            end if;
         end loop;
         raise Bad_End;  --  Definitely no "end-of-central-directory" in this stream
      end;
   end Load;

   procedure Write (Stream : in out Root_Zipstream_Type'Class; The_End : in End_Of_Central_Dir) is
      Eb : Stream_Element_Array (1 .. 22);
   begin
      Pk_Signature (Eb, 5);

      Eb (5 .. 6)   := Intel_Bf (The_End.Disknum);
      Eb (7 .. 8)   := Intel_Bf (The_End.Disknum_With_Start);
      Eb (9 .. 10)  := Intel_Bf (The_End.Disk_Total_Entries);
      Eb (11 .. 12) := Intel_Bf (The_End.Total_Entries);
      Eb (13 .. 16) := Intel_Bf (The_End.Central_Dir_Size);
      Eb (17 .. 20) := Intel_Bf (The_End.Central_Dir_Offset);
      Eb (21 .. 22) := Intel_Bf (The_End.Main_Comment_Length);

      Stream.Write (Eb);
   end Write;

   --------------------------------------------------------------------
   --  PKZIP data descriptor, after streamed compressed data - PK78  --
   --------------------------------------------------------------------
   procedure Copy_And_Check
     (Buffer     : in  Stream_Element_Array;
      Descriptor : out Data_Descriptor) is
   begin
      if not Pk_Signature (Buffer, 7) then
         raise Bad_Data_Descriptor;
      end if;

      Descriptor.Crc_32            := Intel_Nb (Buffer (5 .. 8));
      Descriptor.Compressed_Size   := Intel_Nb (Buffer (9 .. 12));
      Descriptor.Uncompressed_Size := Intel_Nb (Buffer (13 .. 16));
   end Copy_And_Check;

   procedure Read_And_Check
     (Stream     : in out Root_Zipstream_Type'Class;
      Descriptor :    out Data_Descriptor)
   is
      Buffer : Stream_Element_Array (1 .. 16);
   begin
      Blockread (Stream, Buffer);
      Copy_And_Check (Buffer, Descriptor);
   end Read_And_Check;

   procedure Write
     (Stream     : in out Root_Zipstream_Type'Class;
      Descriptor : in     Data_Descriptor)
   is
      Buffer : Stream_Element_Array (1 .. 16);
   begin
      Pk_Signature (Buffer, 7);

      Buffer (5 .. 8)   := Intel_Bf (Descriptor.Crc_32);
      Buffer (9 .. 12)  := Intel_Bf (Descriptor.Compressed_Size);
      Buffer (13 .. 16) := Intel_Bf (Descriptor.Uncompressed_Size);

      Stream.Write (Buffer);
   end Write;

end DCF.Zip.Headers;
