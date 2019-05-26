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

--  Zip.CRC_Crypto deals with hash-like functions for data integrity check

with Ada.Streams;

package Zip.Crc_Crypto is

   use Interfaces;

   -------------------------------------------------------------
   --  CRC: Cyclic Redundancy Check to verify data integrity  --
   -------------------------------------------------------------

   procedure Init (Crc : out Unsigned_32);

   procedure Update (Crc : in out Unsigned_32; Inbuf : Zip.Byte_Buffer);
   pragma Inline (Update);

   procedure Update_Stream_Array
     (Crc   : in out Unsigned_32;
      Inbuf : Ada.Streams.Stream_Element_Array);
   pragma Inline (Update_Stream_Array);

   function Final (Crc : Unsigned_32) return Unsigned_32;
   pragma Inline (Final);

end Zip.Crc_Crypto;
