--  Copyright (c) 2007 - 2018 Gautier de Montmollin
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

--  UnZip.Decompress
--  ---------------
--
--  Private, internal to the UnZip package.
--
--  This package includes the decompression algorithms for methods Store
--  and Deflate.
--
--  The package body contains the packages UnZ_IO, UnZ_Glob, UnZ_Infl,
--  UnZ_Olds and UnZ_Misc that were separate in previous versions of Zip-Ada.
--  They became local packages inside the Decompress_Data procedure.
--  Previously global variables are since then local and task-safe
--  with one copy per concurrent call.

with Zip.Headers;
with Zip_Streams;

private package Unzip.Decompress is

   procedure Decompress_Data
     (Zip_File : in out Zip_Streams.Root_Zipstream_Type'Class;
      --  Zip_File must be open and its index is meant
      --  to point to the beginning of compressed data
      Format                     :        Pkzip_Method;
      Mode                       :        Write_Mode;
      Output_File_Name           :        String;   --  Relevant only if mode = write_to_file
      Output_Memory_Access       :    out P_Stream_Element_Array;  --  \ = write_to_memory
      Output_Stream_Access       :        P_Stream;                --  \ = write_to_stream
      Feedback                   :        Zip.Feedback_Proc;
      Explode_Literal_Tree       :        Boolean;  --  Relevant for the "explode" format
      Explode_Slide_8kb_Lzma_Eos :        Boolean;  --  Relevant for the "explode" and "LZMA" formats
      Data_Descriptor_After_Data :        Boolean;
      Hint                       : in out Zip.Headers.Local_File_Header);
      --  Values are known, or smart fakes, and are later corrected if a closing
      --  Data_descriptor is appended to the compressed data (1-pass written
      --  zip files, like JAR, OpenDocument, etc.)

private

   --  When deflate_strict = True, stop if there is an incomplete Huffman
   --  code set for decoding LZ distances. This is the correct and safe behaviour.
   --  When dealing with Zip files from some old compression programs like PKZIP 1.93a,
   --  the check can be bypassed with deflate_strict = False, but this lessens the
   --  data error detection.
   Deflate_Strict : constant Boolean := True;

   --  Primitive tracing using Ada.Text_IO, plus a few statistics
   type Trace_Type is (None, Some_T, Full);

   Trace : constant Trace_Type := None; --  <==  Choice is here

   No_Trace   : constant Boolean := Trace = None;
   Some_Trace : constant Boolean := Trace >= Some_T;
   Full_Trace : constant Boolean := Trace = Full;

end Unzip.Decompress;
