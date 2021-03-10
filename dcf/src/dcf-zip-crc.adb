--  SPDX-License-Identifier: MIT
--
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

package body DCF.Zip.CRC is

   CRC32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

   procedure Prepare_Table is
      --  CRC-32 algorithm, section 4.4.7 of .zip file format specification
      Seed : constant := 16#EDB8_8320#;
      L    : Unsigned_32;
   begin
      for I in CRC32_Table'Range loop
         L := I;
         for Bit in 0 .. 7 loop
            if (L and 1) = 0 then
               L := Shift_Right (L, 1);
            else
               L := Shift_Right (L, 1) xor Seed;
            end if;
         end loop;
         CRC32_Table (I) := L;
      end loop;
   end Prepare_Table;

   procedure Update (CRC : in out Unsigned_32; Inbuf : Zip.Byte_Buffer) is
      Local_CRC : Unsigned_32;
   begin
      Local_CRC := CRC;
      for I in Inbuf'Range loop
         Local_CRC :=
           CRC32_Table (16#FF# and (Local_CRC xor Unsigned_32 (Inbuf (I)))) xor
           Shift_Right (Local_CRC, 8);
      end loop;
      CRC := Local_CRC;
   end Update;

   procedure Update_Stream_Array
     (CRC   : in out Unsigned_32;
      Inbuf : Ada.Streams.Stream_Element_Array)
   is
      Local_CRC : Unsigned_32;
   begin
      Local_CRC := CRC;
      for I in Inbuf'Range loop
         Local_CRC :=
           CRC32_Table (16#FF# and (Local_CRC xor Unsigned_32 (Inbuf (I)))) xor
           Shift_Right (Local_CRC, 8);
      end loop;
      CRC := Local_CRC;
   end Update_Stream_Array;

   Table_Empty : Boolean := True;

   procedure Init (CRC : out Unsigned_32) is
   begin
      if Table_Empty then
         Prepare_Table;
         Table_Empty := False;
      end if;
      CRC := 16#FFFF_FFFF#;
   end Init;

   function Final (CRC : Unsigned_32) return Unsigned_32 is
   begin
      return not CRC;
   end Final;

   function Image (Value : Unsigned_32) return String is
      Alphabet : constant String := "0123456789abcdef";

      V : array (1 .. 4) of Unsigned_8
        with Import, Convention => Ada, Address => Value'Address;

      function Byte (Value : Unsigned_8) return String is
        (Alphabet (Natural (Value) / 16 + 1) & Alphabet (Natural (Value) mod 16 + 1));
   begin
      return Byte (V (4)) & Byte (V (3)) & Byte (V (2)) & Byte (V (1));
   end Image;

end DCF.Zip.CRC;
