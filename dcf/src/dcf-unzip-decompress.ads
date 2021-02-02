--  SPDX-License-Identifier: MIT
--
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

with DCF.Zip.Headers;
with DCF.Streams;

private package DCF.Unzip.Decompress is
   pragma Preelaborate;

   procedure Decompress_Data
     (Zip_File : in out DCF.Streams.Root_Zipstream_Type'Class;
      --  Zip_File must be open and its index is meant
      --  to point to the beginning of compressed data
      Format                     :        Pkzip_Method;
      Output_Stream_Access       :        P_Stream;
      Data_Descriptor_After_Data :        Boolean;
      Hint                       : in out Zip.Headers.Local_File_Header;
      Verify_Integrity           :        Boolean);
      --  Values are known, or smart fakes, and are later corrected if a closing
      --  Data_descriptor is appended to the compressed data (1-pass written
      --  zip files, like JAR, OpenDocument, etc.)

end DCF.Unzip.Decompress;
