--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2007 - 2018 Gautier de Montmollin (Maintainer of the Ada version)
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

--  Zip.Compress
--  ------------
--
--  This package facilitates the storage or compression of data.
--
--  Note that unlike decompression where the decoding is unique,
--  there is a quasi indefinite number of ways of compressing data into
--  most Zip-supported formats, including LZW (Shrink), Reduce, Deflate, or LZMA.
--  As a result, you may want to use your own way for compressing data.
--  This package is a portable one and doesn't claim to be the "best".
--  The term "best" is relative to the needs, since there are at least
--  two criteria that usually go in opposite directions: speed and
--  compression ratio, a bit like risk and return in finance.

with DCF.Streams;

package DCF.Zip.Compress is
   pragma Preelaborate;

   --  Compression_Method is actually reflecting the way of compressing
   --  data, not only the final compression format called "method" in
   --  Zip specifications.

   type Compression_Method is
      --  No compression:
     (Store,
      --  Deflate combines LZ and Huffman encoding; 4 strengths available:
      Deflate_Fixed, Deflate_1, Deflate_2, Deflate_3);

   type Method_To_Format_Type is array (Compression_Method) of Pkzip_Method;
   Method_To_Format : constant Method_To_Format_Type;

   --  Deflate_Fixed compresses the data into a single block and with predefined
   --  ("fixed") compression structures. The data are basically LZ-compressed
   --  only, since the Huffman code sets are flat and not tailored for the data.
   subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_3;

   --  The multi-block Deflate methods use refined techniques to decide when to
   --  start a new block and what sort of block to put next.
   subtype Taillaule_Deflation_Method is Compression_Method range Deflate_1 .. Deflate_3;

   User_Abort : exception;

   --  Compress data from an input stream to an output stream until
   --  End_Of_File(input) = True, or number of input bytes = input_size.
   --  If password /= "", an encryption header is written.

   procedure Compress_Data
     (Input, Output    : in out DCF.Streams.Root_Zipstream_Type'Class;
      Input_Size       :        File_Size_Type;
      Method           :        Compression_Method;
      Feedback         :        Feedback_Proc;
      CRC              :    out Unsigned_32;
      Output_Size      :    out File_Size_Type;
      Zip_Type         :    out Unsigned_16);
      --  ^ code corresponding to the compression method actually used

private

   Method_To_Format : constant Method_To_Format_Type :=
     (Store => Store, Deflation_Method => Deflate);

end DCF.Zip.Compress;
