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

--  UnZip
--  -----
--
--  This library allows to uncompress deflated, enhanced deflated, and
--  stored streams from a Zip archive stream.
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Ada translation and substantial rewriting by Gautier de Montmollin
--    On the web: see the Zip.web constant.
--  based on Pascal version 2.10 by Abimbola A Olowofoyeku,
--    http://www.foyeh.org/
--  itself based on Pascal version by Christian Ghisler,
--  itself based on C code by Info-Zip group (Mark Adler et al.)
--    http://www.info-zip.org/UnZip.html

--  Technical documentation: read appnote.txt

with DCF.Zip;

private with Ada.Streams;

package DCF.Unzip is
   pragma Preelaborate;

   subtype Pkzip_Method is Zip.Pkzip_Method;

   ----------------------------------------------
   -- Extraction procedures for user interface --
   ----------------------------------------------

   --  NB: the *_proc types are accesses to procedures - their usage
   --  may require the non-standard attribute "unrestricted_access",
   --  or some changes.
   --  Read unzipada.adb for details and examples.

   type Name_Conflict_Intervention is (Yes, No, Yes_To_All, None, Rename_It);

   type Resolve_Conflict_Proc is access procedure
     (Name            : in     String;
      Name_Encoding   : in     Zip.Zip_Name_Encoding;
      Action          :    out Name_Conflict_Intervention;
      New_Name        :    out String;
      New_Name_Length :    out Natural);

   --  Data sizes in archive
   subtype File_Size_Type is Zip.File_Size_Type;

   CRC_Error               : exception;
   Uncompressed_Size_Error : exception;
   Write_Error             : exception;

private

   type Write_Mode is
     (Write_To_Memory, Write_To_Stream);

   type P_Stream is access all Ada.Streams.Root_Stream_Type'Class;

   type P_Stream_Element_Array is access all Ada.Streams.Stream_Element_Array;

end DCF.Unzip;
