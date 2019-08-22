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

--  Zip.Headers
--  -----------
--
--  This package provides:
--
--   * Definition of PKZIP information structures (cf appnote.txt)
--   * Reading a header from a data stream (Read_and_check)
--   * Copying a header from a buffer (Copy_and_check)
--   * Writing a header to a data stream (Write)
--
--  Some quick explanations about the Zip file structure - GdM 2001, 2012
--
--  The zip archive containing N entries can be roughly seen as
--  a data stream with the following structure:
--
--  1) {local header, then compressed data} - that, N times
--  2) central directory, with a summary of each of the N entries
--  3) end-of-central-directory, with a summary of the central directory
--
--  Since N is not necessarily known before or during the phase 1,
--  the central directory's size is also potentially unknown.
--  Then obvious place for the central directory is *after* the data,
--  it is why it appears on phase 2.
--
--  An advantage of that structure is that the .ZIP archive can be later
--  appended to an .exe, for self-extracting purposes, or to other
--  kind of files.
--
--  So, the most general infos are at the end, and we crawl back
--  for more precise infos:
--
--  1) end-of-central-directory
--  2) central directory
--  3) zipped data entries

with DCF.Streams;

use DCF.Streams;

package DCF.Zip.Headers is
   pragma Preelaborate;

   ------------------------------------------------------------------------
   --  PKZIP data descriptor, put after streamed compressed data - PK78  --
   ------------------------------------------------------------------------

   type Data_Descriptor is record
      --  PK78                          --  1.. 4
      Crc_32            : Unsigned_32;  --  5.. 8
      Compressed_Size   : Unsigned_32;
      Uncompressed_Size : Unsigned_32;
   end record;

   Data_Descriptor_Length : constant := 16;

   --  This header needs to be read in continuation of
   --  the compressed data -> access to a buffer
   procedure Copy_And_Check
     (Buffer     : in  Ada.Streams.Stream_Element_Array;
      Descriptor : out Data_Descriptor);

   procedure Read_And_Check
     (Stream     : in out Root_Zipstream_Type'Class;
      Descriptor :    out Data_Descriptor);

   Bad_Data_Descriptor : exception;

   procedure Write
     (Stream     : in out Root_Zipstream_Type'Class;
      Descriptor : in     Data_Descriptor);

   -----------------------------------------------------------------------
   -- PKZIP local file header, in front of every file in archive - PK34 --
   -----------------------------------------------------------------------

   --  Appnote: 4.4.4 general purpose bit flag: (2 bytes)
   --
   --  Bit 0:  If set, indicates that the file is encrypted.
   Encryption_Flag_Bit : constant := 2**0;

   --  Bit 3:  If set, indicates data is followed by a data descriptor
   --  See 4.3.9 of appnote
   Descriptor_Flag_Bit : constant := 2**3;

   --  Bit 11: Language encoding flag (EFS). If this bit is set, the filename and
   --          comment fields for this file MUST be encoded using UTF-8.
   Language_Encoding_Flag_Bit : constant := 2**11;

   type Local_File_Header is record
      --  PK34                               --  1.. 4
      Needed_Extract_Version : Unsigned_16;  --  5.. 6
      Bit_Flag               : Unsigned_16;  --  Appnote: 4.4.4 general purpose bit flag
      Zip_Type               : Unsigned_16;
      File_Timedate          : Time;
      Dd                     : Data_Descriptor;
      Filename_Length        : Unsigned_16;
      Extra_Field_Length     : Unsigned_16;
   end record;

   Local_Header_Length : constant := 30;

   procedure Read_And_Check
     (Stream : in out Root_Zipstream_Type'Class;
      Header :    out Local_File_Header);

   Bad_Local_Header : exception;

   procedure Write
     (Stream : in out Root_Zipstream_Type'Class;
      Header : in     Local_File_Header);

   -------------------------------------------------------
   -- PKZIP file header, as in central directory - PK12 --
   -------------------------------------------------------
   --  NB: a central header contains a local header in the middle

   type Central_File_Header is record
      --  PK12                                  --   1 ..  4
      Made_By_Version     : Unsigned_16;        --   5 ..  6
      Short_Info          : Local_File_Header;  --   7 .. 32
      Comment_Length      : Unsigned_16;        --  33 .. 34
      Disk_Number_Start   : Unsigned_16;
      Internal_Attributes : Unsigned_16;  --  Internal properties of data
      External_Attributes : Unsigned_32;  --  First byte if MS-DOS: see below
      Local_Header_Offset : Unsigned_32;
   end record;

   --  MS-DOS external attributes:
   --
   --   Bit 0     Read-Only
   --   Bit 1     Hidden
   --   Bit 2     System
   --   Bit 3     Volume Label
   --   Bit 4     Directory
   --   Bit 5     Archive

   Central_Header_Length : constant := 46;

   procedure Read_And_Check
     (Stream : in out Root_Zipstream_Type'Class;
      Header :    out Central_File_Header);

   function Valid_Version (Header : Local_File_Header) return Boolean is
     (Header.Needed_Extract_Version <= 45);

   function Valid_Bitflag (Header : Local_File_Header) return Boolean is
     ((Header.Bit_Flag and 2#1111_0111_1111_0001#) = 0);

   Bad_Central_Header : exception;

   procedure Write (Stream : in out Root_Zipstream_Type'Class; Header : in Central_File_Header);

   ---------------------------------------------
   --  PKZIP end-of-central-directory - PK56  --
   ---------------------------------------------

   type End_Of_Central_Dir is record
      --  PK56                            --  1 .. 4
      Disknum             : Unsigned_16;  --  5 .. 6
      Disknum_With_Start  : Unsigned_16;
      Disk_Total_Entries  : Unsigned_16;
      Total_Entries       : Unsigned_16;
      Central_Dir_Size    : Unsigned_32;
      Central_Dir_Offset  : Unsigned_32;
      Main_Comment_Length : Unsigned_16;
      --  The Zip archive may be appended to another file (for instance an
      --  executable for self-extracting purposes) of size N.
      --  Then, all offsets need to be shifted by N.
      --  N=0 if the Zip archive is on its own.
      --  The real offset of the end-of-central-dir
      --  will be N + central_dir_size + central_dir_offset.
      --  This way, we have an unique chance to determine N when reading the
      --  end-of-central-dir. N is stored in the field hereafter:
      Offset_Shifting : Zs_Size_Type;  --  NB: type is at least 32 bits.
   end record;

   End_Of_Central_Dir_Length : constant := 22;

   --  The End-of-Central-Dir header is followed by a comment of
   --  unkown size and hence needs to be searched in special ways (see Load).

   --  Copy_and_check and Read_and_check assume a buffer or a stream
   --  pointing to the End-of-Central-Dir signature
   procedure Copy_And_Check
     (Buffer  : in  Ada.Streams.Stream_Element_Array;
      The_End : out End_Of_Central_Dir);

   procedure Read_And_Check
     (Stream  : in out Root_Zipstream_Type'Class;
      The_End :    out End_Of_Central_Dir);

   Bad_End : exception;

   --  A bit more elaborated variant: find the End-of-Central-Dir and load it
   procedure Load
     (Stream  : in out Root_Zipstream_Type'Class;
      The_End : out    End_Of_Central_Dir);

   procedure Write
     (Stream  : in out Root_Zipstream_Type'Class;
      The_End : in     End_Of_Central_Dir);

end DCF.Zip.Headers;
