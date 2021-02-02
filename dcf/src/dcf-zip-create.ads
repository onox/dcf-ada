--  SPDX-License-Identifier: MIT
--
--  Contributed by ITEC - NXP Semiconductors
--  June 2008
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

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

with DCF.Streams;
with DCF.Zip.Compress;
with DCF.Zip.Headers;

use DCF.Streams;
use DCF.Zip.Headers;

package DCF.Zip.Create is
   pragma Preelaborate;

   type Zip_Create_Info is private;

   procedure Create
     (Info       : in out   Zip_Create_Info;
      Stream     : not null Zipstream_Class_Access;
      Compress   :          Zip.Compress.Compression_Method := Zip.Compress.Deflate_1)
   with Pre  => Stream.Get_Name /= "",
        Post => Is_Created (Info);

   function Is_Created (Info : Zip_Create_Info) return Boolean;

   --  Set a new compression format for the next data to be added to the archive.
   --  Can be useful if data are known to be already compressed - or not.

   procedure Set (Info : in out Zip_Create_Info; New_Method : Zip.Compress.Compression_Method);

   procedure Set_Comment (Info : in out Zip_Create_Info; Comment : String);

   function Name (Info : Zip_Create_Info) return String;

   procedure Add_Stream
     (Info            : in out Zip_Create_Info;
      Stream          : in out Root_Zipstream_Type'Class;
      Feedback        : in     Feedback_Proc;
      Compressed_Size :    out Zip.File_Size_Type;
      Final_Method    :    out Natural);
   --  Add a new entry to a Zip archive, from a general input Zipstream

   procedure Add_Stream (Info : in out Zip_Create_Info; Stream : in out Root_Zipstream_Type'Class);

   procedure Add_Compressed_Stream
     (Info     : in out Zip_Create_Info;            --  Destination
      Stream   : in out Root_Zipstream_Type'Class;  --  Source
      Feedback : in     Feedback_Proc);
   --  Add a new entry to a Zip archive, copied from another Zip archive.
   --  This is useful for duplicating archives with some differences, like
   --  adding, replacing, removing or recompressing entries. See the AZip
   --  file manager for an application example.
   --  The streams' indices are set at the beginning of local headers in
   --  both archives.

   procedure Finish (Info : in out Zip_Create_Info);
   --  Complete the Zip archive; close the file if the stream is a file

   Zip_Capacity_Exceeded : exception;
   --  The following is raised on cases when the Zip archive creation exceeds
   --  the Zip_32 format's capacity: 4GB total size, 65535 entries.

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

   package SU renames Ada.Strings.Unbounded;

   package Name_Mapping is new Ada.Containers.Hashed_Maps
     (SU.Unbounded_String, Positive, SU.Hash, SU."=");

   type Zip_Create_Info is record
      Stream   : Zipstream_Class_Access;
      Compress : Zip.Compress.Compression_Method;
      Contains : Pdir_Entries := null;
      --  'Contains' has unused room, to avoid reallocating each time:
      Last_Entry : Natural := 0;
      --  We set up a name dictionary just for avoiding duplicate entries
      Dir                : Name_Mapping.Map;
      Zip_Archive_Format : Zip_Archive_Format_Type := Zip_32;
      Comment : SU.Unbounded_String;
   end record;

end DCF.Zip.Create;
