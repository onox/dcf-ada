--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2016 - 2018 Gautier de Montmollin (maintainer of the Ada version)
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

--  Standalone LZ77 compression (encoding) package
--  ----------------------------------------------
--
--  This is a collection of various free LZ77 match finders and encoders.
--  The differences reside in the way matches are found, or skipped.
--  See body (lz77.adb) for details and credits.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

private package DCF.Lz77 is
   pragma Preelaborate;

   type Method_Type is
     (Iz_4, Iz_5, Iz_6, Iz_7, Iz_8, Iz_9, Iz_10);
   --  Use the Info-Zip algorithm, levels 4-10 (see body for details and credits)

   subtype Byte is Unsigned_8;

   generic
      --  LZSS Parameters
      String_Buffer_Size : Integer := 2**12;
      Look_Ahead         : Integer := 65;
      Threshold          : Integer := 2;

      Method : Method_Type;

      --  Input of data
      with function Read_Byte return Byte;
      with function More_Bytes return Boolean;

      --  Output of LZ-compressed data
      with procedure Write_Literal (B : Byte);
      with procedure Write_Dl_Code (Distance, Length : Integer);
   procedure Encode;

end DCF.Lz77;
