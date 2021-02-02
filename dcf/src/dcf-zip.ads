--  SPDX-License-Identifier: MIT
--
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

with Ada.Finalization;
with Ada.Streams;

with DCF.Streams;

package DCF.Zip is
   pragma Preelaborate;

   subtype File_Size_Type is Unsigned_32;
   --  Data sizes in archive

   subtype Time is DCF.Streams.Time;

   type Zip_Name_Encoding is (IBM_437, UTF_8);
   --  Entry names within Zip archives are encoded either with
   --    * the IBM PC (the one with a monochrome screen, only text mode)'s
   --        character set: IBM 437
   --  or
   --    * Unicode UTF-8
   --
   --  Documentation: PKWARE's Appnote.txt, APPENDIX D - Language Encoding (EFS)

   -----------------------------------------------------------------------------

   type Archived_File is tagged limited private;

   function Name (Object : Archived_File) return String;

   function Name_Encoding (Object : Archived_File) return Zip_Name_Encoding;

   function Compressed_Size (Object : Archived_File) return File_Size_Type;

   function Uncompressed_Size (Object : Archived_File) return File_Size_Type;

   function Date_Time (Object : Archived_File) return Time;

   function Compressed (Object : Archived_File) return Boolean;

   function Encrypted (Object : Archived_File) return Boolean;

   function File_Index (Object : Archived_File) return DCF.Streams.Zs_Index_Type;

   function CRC_32 (Object : Archived_File) return Unsigned_32;

   -----------------------------------------------------------------------------

   type Zip_Info is new Ada.Finalization.Controlled with private;

   procedure Load
     (Info            :    out Zip_Info;
      From            : in out DCF.Streams.Root_Zipstream_Type'Class;
      Case_Sensitive  : in     Boolean := False)
   with Post => Info.Is_Loaded;
   --  Load an archive from a stream

   Archive_Corrupted   : exception;
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

   function Stream (Info : in Zip_Info) return not null DCF.Streams.Zipstream_Class_Access
     with Pre => Info.Is_Loaded;

   function Entries (Info : in Zip_Info) return Natural;

   --  Compression "methods" - actually, formats - in the "official" PKWARE Zip format
   --  Details in appnote.txt, part V.J

   type Pkzip_Method is (Store, Deflate);

   --  Technical: translates the method code as set in zip archives
   function Method_From_Code (Code : Unsigned_16) return Pkzip_Method;

   generic
      with procedure Action (File : Archived_File);
   procedure Traverse (Z : Zip_Info);
   --  Traverse a whole Zip_info directory in sorted order, giving the
   --  name and other technical information for each archived file to an
   --  user-defined "Action" procedure.
   --
   --  Concretely, you can process a whole Zip file that way, by extracting data
   --  with Extract, or open a reader stream with UnZip.Streams.

   generic
      with procedure Action (File : Archived_File);
   procedure Traverse_One_File (Z : Zip_Info; Name : String);

   File_Name_Not_Found : exception;

   function Exists (Info : Zip_Info; Name : String) return Boolean
     with Pre => Info.Is_Loaded;

   --  User-defined procedure for feedback occuring during
   --  compression or decompression

   type Feedback_Proc is access procedure
     (Percents_Done : in     Natural;   --  %'s completed
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

   subtype Byte is Unsigned_8;
   type Byte_Buffer is array (Integer range <>) of aliased Byte;
   type P_Byte_Buffer is access Byte_Buffer;

   --  Same for general streams

   procedure Blockread
     (Stream        : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural);
      --  = buffer'Length if no end of stream before last buffer element

   --  Same, but instead of giving actually_read, raises End_Error if
   --  the buffer cannot be fully read.
   --  This mimics the 'Read stream attribute; can be a lot faster, depending
   --  on the compiler's run-time library.
   procedure Blockread
     (Stream : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer :    out Byte_Buffer);

   procedure Blockread
     (Stream : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer :    out Ada.Streams.Stream_Element_Array);

   --  This mimics the 'Write stream attribute; can be a lot faster, depending
   --  on the compiler's run-time library.
   --  NB: here we can use the root stream type: no question of size, index, etc.
   procedure Blockwrite
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer : in     Byte_Buffer);

   procedure Copy_Chunk
     (From        : in out DCF.Streams.Root_Zipstream_Type'Class;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Bytes       :        Ada.Streams.Stream_Element_Count;
      Feedback    :        Feedback_Proc := null);
   --  Copy a chunk from a stream into another one, using a temporary buffer

   User_Abort : exception;

   -----------------------------------------------------------------
   --  Information about this package - e.g., for an "about" box  --
   -----------------------------------------------------------------

   Version   : constant String := "2.0.2";

private

   Default_Buffer_Size : constant := 1024 * 1024;  --  1 MiB

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
      File_Index    : DCF.Streams.Zs_Index_Type;
      Comp_Size     : File_Size_Type;
      Uncomp_Size   : File_Size_Type;
      Crc_32        : Unsigned_32;
      Date_Time     : Time;
      Method        : Pkzip_Method;
      Name_Encoding : Zip_Name_Encoding;
      Read_Only     : Boolean;  --  TBD: attributes of most supported systems
      Encrypted_2_X : Boolean;
   end record;

   type Zip_Archive_Format_Type is (Zip_32, Zip_64);  --  Supported so far: Zip_32

   type P_String is access String;

   type Zip_Info is new Ada.Finalization.Controlled with record
      Loaded             : Boolean := False;
      Case_Sensitive     : Boolean;
      Zip_Input_Stream   : DCF.Streams.Zipstream_Class_Access;
      Dir_Binary_Tree    : P_Dir_Node;
      Total_Entries      : Natural;
      Zip_File_Comment   : P_String;
      Zip_Archive_Format : Zip_Archive_Format_Type := Zip_32;
   end record;

   overriding procedure Adjust (Info : in out Zip_Info);
   --  After a copy, need to clone a few things

   overriding procedure Finalize (Info : in out Zip_Info);
   --  Free heap-allocated memory

   --  Codes for compression formats in Zip archives
   --  See PKWARE's Appnote, "4.4.5 compression method"

   package Compression_Format_Code is
      Store     : constant := 0;
      Deflate   : constant := 8;
   end Compression_Format_Code;

   type Archived_File is tagged limited record
      Node : P_Dir_Node;
   end record;

end DCF.Zip;
