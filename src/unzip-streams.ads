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

with Zip;
with Zip_Streams;

package Unzip.Streams is

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

   type Count is new Zip_Streams.Zs_Size_Type;

   --  Opens an input stream for the compressed file named Name stored
   --  in the archive file named Archive_Name. The function Stream(..)
   --  then gives access to the opened stream.

   --  Version: Zip as a file
   procedure Open
     (File             : in out Zipped_File_Type;  --  File-in-archive handle
      Archive_Name     : in     String;            --  Name of archive file
      Name             : in     String;            --  Name of zipped entry
      Case_Sensitive   : in     Boolean := False;
      Ignore_Directory : in Boolean := False);     --  Open Name in first directory found

   --  Version: Zip as a stream
   procedure Open
     (File             : in out Zipped_File_Type;  --  File-in-archive handle
      Archive_Stream   : in out Zip_Streams.Root_Zipstream_Type'Class; -- Archive's stream
      Name             : in     String;            --  Name of zipped entry
      Case_Sensitive   : in     Boolean := False;
      Ignore_Directory : in Boolean := False);     --  Open Name in first directory found

   --  Same as above, but uses a the pre-loaded contents of the archive's
   --  Central Directory; hence Archive_Info is passed instead of
   --  Archive_Name or Archive_Stream.
   --  You need to call Zip.Load( Archive_Info... ) prior to opening the
   --  compressed file.

   procedure Open
     (File             : in out Zipped_File_Type;  --  File-in-archive handle
      Archive_Info     : in     Zip.Zip_Info;      --  Archive's Zip_info
      Name             : in     String;            --  Name of zipped entry
      Ignore_Directory : in Boolean := False);     --  Open Name in first directory found

   procedure Close (File : in out Zipped_File_Type);

   function Name (File : in Zipped_File_Type) return String;

   function Is_Open (File : in Zipped_File_Type) return Boolean;
   function End_Of_File (File : in Zipped_File_Type) return Boolean;

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   --------------------------------------------------------------------------
   --  The function Stream gives access to the uncompressed data as input  --
   --------------------------------------------------------------------------
   function Stream (File : Zipped_File_Type) return Stream_Access;

   function Size (File : in Zipped_File_Type) return Count;

   Use_Error : exception renames Ada.IO_Exceptions.Use_Error;
   End_Error : exception renames Ada.IO_Exceptions.End_Error;

   ------------------------------------------------------------------
   --                    ** Output Stream **                       --
   --                                                              --
   --  Extract a Zip archive entry to an available output stream.  --
   --                                                              --
   --  NB: the memory footprint is limited to the decompression    --
   --      structures and buffering, so the outward stream can be  --
   --      an interesting alternative to the inward, albeit less   --
   --      comfortable.                                            --
   ------------------------------------------------------------------

   procedure Extract
     (Destination      : in out Ada.Streams.Root_Stream_Type'Class;
      Archive_Info     : in     Zip.Zip_Info;   --  Archive's Zip_info
      Name             : in     String;         --  Name of zipped entry
      Ignore_Directory : in Boolean := False);  --  Open Name in first directory found

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

end Unzip.Streams;
