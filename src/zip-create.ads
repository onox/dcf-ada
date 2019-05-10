--  Zip archive creation
--
--  Contributed by ITEC - NXP Semiconductors
--  June 2008
--

--  Legal licensing note:

--  Copyright (c) 2008 .. 2018 Gautier de Montmollin (maintainer)
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

--
--  Change log:
--  ==========
--
--  23-Mar-2016: GdM: Create with Duplicate_name_policy
--  14-Feb-2015: GdM: Added "Is_Created" function
--  13-Feb-2015: GdM: Added "Password" parameter
--  30-Oct-2012: GdM: Removed all profiles using Zip_Streams' objects
--                       with accesses (cf 25-Oct's modifications)
--  26-Oct-2012: GdM: Added Add_Compressed_Stream
--  25-Oct-2012: GdM: Some procedures using Zip_Streams' objects also with
--                      pointer-free profiles (no more 'access' or access type)
--  14-Oct-2012: GdM: Added Set procedure for changing compression method
--  30-Mar-2010: GdM: Added Name function
--  25-Feb-2010: GdM: Fixed major bottlenecks around Dir_entries
--                      -> 5x faster overall for 1000 files, 356x for 100'000 !
--  17-Feb-2009: GdM: Added procedure Add_String
--  10-Feb-2009: GdM: Create / Finish: if Info.Stream is to a file,
--                      the underling file is also created / closed in time
--   4-Feb-2009: GdM: Added procedure Add_File
--

with Zip.Compress;
with Zip.Headers;
with Zip_Streams;

use Zip.Headers;
use Zip.Compress;
use Zip_Streams;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Zip.Create is

   type Zip_Create_Info is private;

   --  Create the Zip archive; create the file if the stream is a file

   procedure Create
     (Info       :    out Zip_Create_Info;
      Z_Stream   : in     Zipstream_Class_Access;
      Name       :        String;
      Compress   :        Zip.Compress.Compression_Method := Zip.Compress.Deflate_1;
      Duplicates :        Duplicate_Name_Policy           := Admit_Duplicates);

   function Is_Created (Info : Zip_Create_Info) return Boolean;

   --  Set a new compression format for the next data to be added to the archive.
   --  Can be useful if data are known to be already compressed - or not.

   procedure Set (Info : in out Zip_Create_Info; New_Method : Zip.Compress.Compression_Method);

   function Name (Info : Zip_Create_Info) return String;

   --  Add a new entry to a Zip archive, from a general input Zipstream

   procedure Add_Stream (Info : in out Zip_Create_Info; Stream : in out Root_Zipstream_Type'Class);

   procedure Add_Stream
     (Info            : in out Zip_Create_Info;
      Stream          : in out Root_Zipstream_Type'Class;
      Feedback        : in     Feedback_Proc;
      Compressed_Size :    out Zip.File_Size_Type;
      Final_Method    :    out Natural);

   --  Add a new entry to a Zip archive, from an entire file

   procedure Add_File
     (Info            : in out Zip_Create_Info;
      Name            :        String;
      Name_In_Archive :        String := "";
      --  Default: add the file in the archive under the same name
      Delete_File_After : Boolean := False;
      --  Practical to delete temporary file after adding
      Name_Encoding     : Zip_Name_Encoding := IBM_437;
      Modification_Time : Time              := Default_Time;
      Is_Read_Only      : Boolean           := False;
      Feedback          : Feedback_Proc     := null);

   --  Add a new entry to a Zip archive, from a buffer stored in a string

   procedure Add_String
     (Info            : in out Zip_Create_Info;
      Contents        :        String;
      Name_In_Archive :        String;
      --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
      Name_Utf_8_Encoded : Boolean := False;
      --  Time stamp for this entry, e.g. Zip.Convert (Ada.Calendar.Clock)
      Creation_Time : Zip.Time := Default_Time);

   use Ada.Strings.Unbounded;

   procedure Add_String
     (Info            : in out Zip_Create_Info;
      Contents        :        Unbounded_String;
      Name_In_Archive :        String;
      --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
      Name_Utf_8_Encoded : Boolean := False;
      --  Time stamp for this entry, e.g. Zip.Convert (Ada.Calendar.Clock)
      Creation_Time : Zip.Time := Default_Time);

   --  Add a new entry to a Zip archive, copied from another Zip archive.
   --  This is useful for duplicating archives with some differences, like
   --  adding, replacing, removing or recompressing entries. See the AZip
   --  file manager for an application example.
   --  The streams' indices are set at the beginning of local headers in
   --  both archives.

   procedure Add_Compressed_Stream
     (Info     : in out Zip_Create_Info;            --  Destination
      Stream   : in out Root_Zipstream_Type'Class;  --  Source
      Feedback : in     Feedback_Proc);

   --  Complete the Zip archive; close the file if the stream is a file

   procedure Finish (Info : in out Zip_Create_Info);

   --  The following is raised on cases when the Zip archive creation exceeds
   --  the Zip_32 format's capacity: 4GB total size, 65535 entries.

   Zip_Capacity_Exceeded : exception;

private

   type P_String is access String;

   type Dir_Entry is record
      Head : Zip.Headers.Central_File_Header;
      Name : P_String;
   end record;

   type Dir_Entries is array (Positive range <>) of Dir_Entry;
   type Pdir_Entries is access Dir_Entries;

   --  The use of Hashed_Maps makes Test_Zip_Create_Info_Timing run ~10x faster than
   --  with the unbalanced binary tree of previous versions.

   package Name_Mapping is new Ada.Containers.Hashed_Maps
     (Unbounded_String, Positive, Hash, "=");

   type Zip_Create_Info is record
      Stream   : Zipstream_Class_Access;
      Compress : Zip.Compress.Compression_Method;
      Contains : Pdir_Entries := null;
      --  'Contains' has unused room, to avoid reallocating each time:
      Last_Entry : Natural := 0;
      Duplicates : Duplicate_Name_Policy;
      --  We set up a name dictionary just for avoiding duplicate entries:
      Dir                : Name_Mapping.Map;
      Zip_Archive_Format : Zip_Archive_Format_Type := Zip_32;
   end record;

end Zip.Create;
