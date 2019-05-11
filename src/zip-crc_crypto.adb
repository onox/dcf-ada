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

package body Zip.Crc_Crypto is

   Crc32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

   procedure Prepare_Table is
      --  CRC-32 algorithm, ISO-3309
      Seed : constant := 16#EDB88320#;
      L    : Unsigned_32;
   begin
      for I in Crc32_Table'Range loop
         L := I;
         for Bit in 0 .. 7 loop
            if (L and 1) = 0 then
               L := Shift_Right (L, 1);
            else
               L := Shift_Right (L, 1) xor Seed;
            end if;
         end loop;
         Crc32_Table (I) := L;
      end loop;
   end Prepare_Table;

   procedure Update (Crc : in out Unsigned_32; Inbuf : Zip.Byte_Buffer) is
      Local_Crc : Unsigned_32;
   begin
      Local_Crc := Crc;
      for I in Inbuf'Range loop
         Local_Crc :=
           Crc32_Table (16#FF# and (Local_Crc xor Unsigned_32 (Inbuf (I)))) xor
           Shift_Right (Local_Crc, 8);
      end loop;
      Crc := Local_Crc;
   end Update;

   Table_Empty : Boolean := True;

   procedure Init (Crc : out Unsigned_32) is
   begin
      if Table_Empty then
         Prepare_Table;
         Table_Empty := False;
      end if;
      Crc := 16#FFFF_FFFF#;
   end Init;

   function Final (Crc : Unsigned_32) return Unsigned_32 is
   begin
      return not Crc;
   end Final;

end Zip.Crc_Crypto;
