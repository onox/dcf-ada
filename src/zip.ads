--  Copyright (c) 1999 - 2019 Gautier de Montmollin
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

--  Zip
--  ---
--
--  Library for manipulating archive files in the Zip format
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  Version / date / download info: see the version, reference, web strings
--  defined at the end of the public part of this package.

with Interfaces;

with System;

with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Zip_Streams;

package Zip is

   --  Data sizes in archive
   subtype File_Size_Type is Interfaces.Unsigned_32;

   subtype Time is Zip_Streams.Time;

   type Zip_Name_Encoding is (IBM_437, UTF_8);
   --  Entry names within Zip archives are encoded either with
   --    * the IBM PC (the one with a monochrome screen, only text mode)'s
   --        character set: IBM 437
   --  or
   --    * Unicode UTF-8
   --
   --  Documentation: PKWARE's Appnote.txt, APPENDIX D - Language Encoding (EFS)

   type Archived_File is tagged private;

   function Name (Object : Archived_File) return String;

   function Name_Encoding (Object : Archived_File) return Zip_Name_Encoding;

   function Compressed_Size (Object : Archived_File) return File_Size_Type;

   function Uncompressed_Size (Object : Archived_File) return File_Size_Type;

   function Date_Time (Object : Archived_File) return Time;

   function Compressed (Object : Archived_File) return Boolean;

   function Encrypted (Object : Archived_File) return Boolean;

   -----------------------------------------------------------------
   --  Zip_info                                                   --
   -----------------------------------------------------------------
   --  Zip_info contains the Zip file name (if it is a file)      --
   --  or its input stream access, and the archive's directory.   --
   -----------------------------------------------------------------

   type Zip_Info is new Ada.Finalization.Controlled with private;

   -----------------------------------------------
   --  Load the whole .zip directory contained  --
   --  in archive (from) for quick searching.   --
   -----------------------------------------------

   type Duplicate_Name_Policy is
     (Admit_Duplicates,     --  Two entries in the Zip archive may have the same full name
      Error_On_Duplicate);  --  Raise exception on attempt to add twice the same entry name

   procedure Load
     (Info            :    out Zip_Info;
      From            : in out Zip_Streams.Root_Zipstream_Type'Class;
      From_Name       : in     String; -- Zip file name
      Case_Sensitive  : in     Boolean               := False;
      Duplicate_Names : in     Duplicate_Name_Policy := Error_On_Duplicate)
   with Post => Info.Is_Loaded;
   --  Load from a stream

   Archive_Corrupted   : exception;
   Zip_File_Open_Error : exception;
   Duplicate_Name      : exception;

   --  Old name for Archive_corrupted. Change: 22-Oct-2017
   --  Issues: archive stream is not necessarily a *file*; the naming
   --  ("Error") didn't clarify that it covered cases where the data
   --  is corrupted, which is different than an usual I/O error.

   function Is_Loaded (Info : in Zip_Info) return Boolean;

   function Name (Info : in Zip_Info) return String
     with Pre => Info.Is_Loaded;

   function Comment (Info : in Zip_Info) return String
     with Pre => Info.Is_Loaded;

   function Stream (Info : in Zip_Info) return Zip_Streams.Zipstream_Class_Access
     with Pre => Info.Is_Loaded;

   function Entries (Info : in Zip_Info) return Natural;

   --  Compression "methods" - actually, formats - in the "official" PKWARE Zip format
   --  Details in appnote.txt, part V.J
   --   C: supported by Zip-Ada for compressing
   --   D: supported by Zip-Ada for decompressing

   type Pkzip_Method is
     (Store,     -- C,D
      Shrink,    -- C,D
      Reduce_1,  -- C,D
      Reduce_2,  -- C,D
      Reduce_3,  -- C,D
      Reduce_4,  -- C,D
      Implode,   --   D
      Tokenize,
      Deflate,   -- C,D
      Deflate_E, --   D - "Enhanced deflate" or "Deflate64"
      Bzip2,     --   D
      Lzma_Meth, -- C,D
      Ppmd,
      Unknown);

   --  Return a String image, nicer than the 'Image attribute
   function Image (M : Pkzip_Method) return String;

   --  Technical: translates the method code as set in zip archives
   function Method_From_Code (X : Interfaces.Unsigned_16) return Pkzip_Method;
   function Method_From_Code (X : Natural) return Pkzip_Method;

   --  Internal time definition
   function Convert (Date : in Ada.Calendar.Time) return Time renames Zip_Streams.Calendar.Convert;
   function Convert (Date : in Time) return Ada.Calendar.Time renames Zip_Streams.Calendar.Convert;

   generic
      with procedure Action (File : Archived_File);
   procedure Traverse (Z : Zip_Info);
   --  Traverse a whole Zip_info directory in sorted order, giving the
   --  name and other technical information for each archived file to an
   --  user-defined "Action" procedure.
   --
   --  Concretely, you can process a whole Zip file that way, by extracting data
   --  with Extract, or open a reader stream with UnZip.Streams.

   --  Academic: see how well the name tree is balanced
   procedure Tree_Stat
     (Z         : in     Zip_Info;
      Total     :    out Natural;
      Max_Depth :    out Natural;
      Avg_Depth :    out Float);

   ----------------------------------------------------------------------------
   --  Offsets - various procedures giving 1-based indexes to local headers  --
   ----------------------------------------------------------------------------

   --  Find first offset in a Zip stream (i.e. the first's archived entry's offset)

   procedure Find_First_Offset
     (File       : in out Zip_Streams.Root_Zipstream_Type'Class;
      File_Index :    out Zip_Streams.Zs_Index_Type);

   --  If the archive is empty (the 22 byte .zip file), there is no first
   --  entry or offset
   Archive_Is_Empty : exception;

   procedure Find_Offset
     (Info          : in     Zip_Info;
      Name          : in     String;
      Name_Encoding :    out Zip_Name_Encoding;
      File_Index    :    out Zip_Streams.Zs_Index_Type;
      Comp_Size     :    out File_Size_Type;
      Uncomp_Size   :    out File_Size_Type;
      Crc_32        :    out Interfaces.Unsigned_32)
   with Pre => Info.Is_Loaded;
   --  Find offset of a certain compressed file in a pre-loaded Zip_Info data

   File_Name_Not_Found : exception;

   function Exists (Info : Zip_Info; Name : String) return Boolean
     with Pre => Info.Is_Loaded;

   procedure Get_Sizes
     (Info        : in     Zip_Info;
      Name        : in     String;
      Comp_Size   :    out File_Size_Type;
      Uncomp_Size :    out File_Size_Type);

   --  User-defined procedure for feedback occuring during
   --  compression or decompression (entry_skipped meaningful
   --  only for the latter)

   type Feedback_Proc is access procedure
     (Percents_Done : in     Natural;   --  %'s completed
      Entry_Skipped : in     Boolean;   --  Indicates one can show "skipped", no %'s
      User_Abort    :    out Boolean);  --  e.g. transmit a "click on Cancel" here

   -------------------------------------------------------------------------
   -- Goodies - things used internally by Zip-Ada but are not bound to    --
   -- Zip archive purposes and that might be generally useful.            --
   -------------------------------------------------------------------------

   --  BlockRead: general-purpose procedure (nothing really specific to Zip /
   --  UnZip): reads either the whole buffer from a file, or if the end of
   --  the file lays inbetween, a part of the buffer.
   --
   --  The procedure's names and parameters match Borland Pascal / Delphi

   subtype Byte is Interfaces.Unsigned_8;
   type Byte_Buffer is array (Integer range <>) of aliased Byte;
   type P_Byte_Buffer is access Byte_Buffer;

   procedure Blockread
     (File          : in     Ada.Streams.Stream_IO.File_Type;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural);
      --  = buffer'Length if no end of file before last buffer element

   --  Same for general streams

   procedure Blockread
     (Stream        : in out Zip_Streams.Root_Zipstream_Type'Class;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural);
      --  = buffer'Length if no end of stream before last buffer element

   --  Same, but instead of giving actually_read, raises End_Error if
   --  the buffer cannot be fully read.
   --  This mimics the 'Read stream attribute; can be a lot faster, depending
   --  on the compiler's run-time library.
   procedure Blockread
     (Stream : in out Zip_Streams.Root_Zipstream_Type'Class;
      Buffer :    out Byte_Buffer);

   --  This mimics the 'Write stream attribute; can be a lot faster, depending
   --  on the compiler's run-time library.
   --  NB: here we can use the root stream type: no question of size, index, etc.
   procedure Blockwrite
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer : in     Byte_Buffer);

   --  Copy a chunk from a stream into another one, using a temporary buffer
   procedure Copy_Chunk
     (From        : in out Zip_Streams.Root_Zipstream_Type'Class;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Bytes       :        Natural;
      Buffer_Size :        Positive      := 1024 * 1024;
      Feedback    :        Feedback_Proc := null);

   --  Copy a whole file into a stream, using a temporary buffer
   procedure Copy_File
     (File_Name   :        String;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer_Size :        Positive := 1024 * 1024);

   --  This does the same as Ada 2005's Ada.Directories.Exists
   --  Just there as helper for Ada 95 only systems

   --  Write a string containing line endings (possibly from another system)
   --   into a text file, with the "correct", native line endings.
   --   Works for displaying/saving correctly
   --   CR&LF (DOS/Win), LF (UNIX), CR (Mac OS < 9)

   procedure Put_Multi_Line (Out_File : Ada.Text_IO.File_Type; Text : String);

   procedure Write_As_Text
     (Out_File  :        Ada.Text_IO.File_Type;
      Buffer    :        Byte_Buffer;
      Last_Char : in out Character);  --  Track line-ending characters between writes

   function Hexadecimal (X : Interfaces.Unsigned_32) return String;

   -----------------------------------------------------------------
   --  Information about this package - e.g., for an "about" box  --
   -----------------------------------------------------------------

   Version   : constant String := "0.1.0";

private

   --  Zip_info, 23.VI.1999.
   --
   --  The PKZIP central directory is coded here as a binary tree
   --  to allow a fast retrieval of the searched offset in zip file.
   --  E.g. for a 1000-file archive, the offset will be found in less
   --  than 11 moves: 2**10=1024 (balanced case), without any read
   --  in the archive.
   --
   --  *Note* 19-Oct-2018: rev. 670 to 683 used a Hashed Map and a
   --  Vector (Ada.Containers). The loading of the dictionary was
   --  much faster (2x), but there were performance bottlenecks elsewhere,
   --  not solved by profiling. On an archive with 18000 small entries of
   --  around 1 KB each, comp_zip ran 100x slower!
   --  Neither the restricted use of Unbounded_String, nor the replacement
   --  of the Vector by an array helped solving the performance issue.

   type Dir_Node;
   type P_Dir_Node is access Dir_Node;

   type Dir_Node (Name_Len : Natural) is record
      Left, Right   : P_Dir_Node;
      Dico_Name     : String (1 .. Name_Len);  --  UPPER if case-insensitive search
      File_Name     : String (1 .. Name_Len);
      File_Index    : Zip_Streams.Zs_Index_Type;
      Comp_Size     : File_Size_Type;
      Uncomp_Size   : File_Size_Type;
      Crc_32        : Interfaces.Unsigned_32;
      Date_Time     : Time;
      Method        : Pkzip_Method;
      Name_Encoding : Zip_Name_Encoding;
      Read_Only     : Boolean;  --  TBD: attributes of most supported systems
      Encrypted_2_X : Boolean;
   end record;

   type Zip_Archive_Format_Type is (Zip_32, Zip_64);  --  Supported so far: Zip_32

   type P_String is access String;

   type Zip_Info is new Ada.Finalization.Controlled with record
      Loaded           : Boolean := False;
      Case_Sensitive   : Boolean;
      Zip_File_Name    : P_String;                            --  A file name
      Zip_Input_Stream : Zip_Streams.Zipstream_Class_Access;  --  Or an input stream
      --  ^ When not null, we use this, and not zip_file_name
      Dir_Binary_Tree    : P_Dir_Node;
      Total_Entries      : Natural;
      Zip_File_Comment   : P_String;
      Zip_Archive_Format : Zip_Archive_Format_Type := Zip_32;
   end record;

   --  After a copy, need to clone a few things
   overriding procedure Adjust (Info : in out Zip_Info);
   --  Free heap-allocated memory
   overriding procedure Finalize (Info : in out Zip_Info);

   --  System.Word_Size: 13.3(8): A word is the largest amount of storage
   --  that can be conveniently and efficiently manipulated by the hardware,
   --  given the implementation's run-time model

   Min_Bits_32 : constant := Integer'Max (32, System.Word_Size);

   --  We define an Integer type which is at least 32 bits, but n bits
   --  on a native n (> 32) bits architecture (no performance hit on 64+
   --  bits architectures).
   --  Integer_M16 not needed: Integer already guarantees 16 bits

   type Integer_M32 is range -2**(Min_Bits_32 - 1) .. 2**(Min_Bits_32 - 1) - 1;
   subtype Natural_M32 is Integer_M32 range 0 .. Integer_M32'Last;
   subtype Positive_M32 is Integer_M32 range 1 .. Integer_M32'Last;

   --  Codes for compression formats in Zip archives
   --  See PKWARE's Appnote, "4.4.5 compression method"

   package Compression_Format_Code is
      Store     : constant := 0;
      Shrink    : constant := 1;
      Reduce    : constant := 2;
      Implode   : constant := 6;
      Tokenize  : constant := 7;
      Deflate   : constant := 8;
      Deflate_E : constant := 9;
      Bzip2     : constant := 12;
      Lzma      : constant := 14;
      Ppmd      : constant := 98;
   end Compression_Format_Code;

   type Archived_File is tagged record
      Node : P_Dir_Node;
   end record;

end Zip;
