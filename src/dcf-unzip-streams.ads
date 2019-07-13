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

with Ada.Streams;

with DCF.Zip;
with DCF.Streams;

package DCF.Unzip.Streams is
   pragma Preelaborate;

   use type DCF.Streams.Zipstream_Class_Access;

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

end DCF.Unzip.Streams;
