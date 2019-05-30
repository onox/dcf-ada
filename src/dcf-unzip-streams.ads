--  Copyright (c) 1999 - 2018 Gautier de Montmollin
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

--  UnZip.Streams
--  -------------
--
--  Extracts, as a stream, a file which is has been compressed into a Zip archive.
--  The Zip archive itself (the input) can be a file or a more general stream.
--  This package is resembling Ada.Streams.Stream_IO, to facilitate transition.

with Ada.IO_Exceptions;
with Ada.Streams;

with DCF.Zip;
with DCF.Streams;

package DCF.Unzip.Streams is
   pragma Preelaborate;

   ----------------------------------------------------------------------------
   --                           ** Input Stream **                           --
   --                                                                        --
   --  Extract a Zip archive entry as an input stream.                       --
   --                                                                        --
   --  The workflow is similar to a "physical" file's:                       --
   --                                                                        --
   --    - Open z: Zipped_File_Type                                          --
   --    - do something with Stream(z), usually: read the data               --
   --    - Close z                                                           --
   --                                                                        --
   --  NB: the whole entry is unpacked into memory at Open, so it uses       --
   --      the uncompressed amount as work memory between Open and Close.    --
   ----------------------------------------------------------------------------

   type Zipped_File_Type is private;

   type Count is new DCF.Streams.Zs_Size_Type;

   use type DCF.Streams.Zipstream_Class_Access;

   procedure Open
     (File             : in out Zipped_File_Type;   --  File-in-archive handle
      Archive_Info     : in     Zip.Zip_Info;       --  Archive's Zip_info
      Name             : in     Zip.Archived_File;  --  Zipped entry
      Verify_Integrity : in     Boolean)
   with Pre  => Archive_Info.Is_Loaded and Archive_Info.Stream /= null,
        Post => Is_Open (File);
   --  Opens an input stream for the compressed entry named Name stored
   --  in the archive file Archive_Info
   --
   --  Uses the pre-loaded contents of the Central Directory of the archive.
   --  Requires calling Zip.Load (Archive_Info, ...) prior to opening a
   --  compressed file in the archive.
   --
   --  Use the function Stream to get access to the opened stream.

   procedure Close (File : in out Zipped_File_Type)
     with Pre  => Is_Open (File),
          Post => not Is_Open (File);

   function Name (File : in Zipped_File_Type) return String;

   function Is_Open (File : in Zipped_File_Type) return Boolean;
   function End_Of_File (File : in Zipped_File_Type) return Boolean;

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   function Stream (File : Zipped_File_Type) return Stream_Access
     with Pre => Is_Open (File);
   --  Return access to the uncompressed data as input

   function Size (File : in Zipped_File_Type) return Count;

   Use_Error : exception renames Ada.IO_Exceptions.Use_Error;
   End_Error : exception renames Ada.IO_Exceptions.End_Error;

   ------------------------------------------------------------------

   procedure Extract
     (Destination      : in out Ada.Streams.Root_Stream_Type'Class;
      Archive_Info     : in     Zip.Zip_Info;       --  Archive's Zip_info
      File             : in     Zip.Archived_File;  --  Zipped entry
      Verify_Integrity : in     Boolean)
   with Pre  => Archive_Info.Is_Loaded and Archive_Info.Stream /= null;
   --  Extract a Zip archive entry to the given output stream
   --
   --  The memory footprint is limited to the decompression structures and
   --  buffering, so the outward stream can be an interesting alternative
   --  to the inward, albeit less comfortable.

private

   type Uzs_State is
     (Uninitialized,
      Data_Uncompressed,  --  In that model, all data is unzipped in one
                          --  time, into memory. If you have a smarter
                          --  idea (small buffer with tasking, write me!)
      End_Of_Zip);        --  We have reached the end, not yet closed

   type P_String is access String;

   type Unzip_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      State        : Uzs_State := Uninitialized;
      Archive_Info : Zip.Zip_Info;            --  Archive info (.zip file, directory)
      File_Name    : P_String;                --  Name of zipped file to unzip from archive
      Uncompressed : P_Stream_Element_Array;  --  Whole uncompressed data
      Index        : Ada.Streams.Stream_Element_Offset;
   end record;

   overriding
   procedure Read
     (Stream : in out Unzip_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out Unzip_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   type Zipped_File_Type is access Unzip_Stream_Type;

end DCF.Unzip.Streams;
