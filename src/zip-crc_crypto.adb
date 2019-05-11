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

   procedure Set_Mode (Obj : in out Crypto_Pack; New_Mode : Crypto_Mode) is
   begin
      Obj.Current_Mode := New_Mode;
   end Set_Mode;

   function Get_Mode (Obj : Crypto_Pack) return Crypto_Mode is
   begin
      return Obj.Current_Mode;
   end Get_Mode;

   procedure Update_Keys (Obj : in out Crypto_Pack; By : Zip.Byte) is
   begin
      Update (Obj.Keys (0), (0 => By));
      Obj.Keys (1) := Obj.Keys (1) + (Obj.Keys (0) and 16#000000ff#);
      Obj.Keys (1) := Obj.Keys (1) * 134775813 + 1;
      Update (Obj.Keys (2), (0 => Zip.Byte (Shift_Right (Obj.Keys (1), 24))));
   end Update_Keys;

   --  Crypto_code: Pseudo-random byte to be XOR'ed with
   function Crypto_Code (Obj : Crypto_Pack) return Zip.Byte is
      pragma Inline (Crypto_Code);
      Temp : Unsigned_16;
   begin
      Temp := Unsigned_16 (Obj.Keys (2) and 16#ffff#) or 2;
      return Zip.Byte (Shift_Right (Temp * (Temp xor 1), 8));
   end Crypto_Code;

   procedure Encode (Obj : in out Crypto_Pack; Buf : in out Zip.Byte_Buffer) is
      Bc : Zip.Byte;
   begin
      if Obj.Current_Mode = Encrypted then
         for I in Buf'Range loop
            Bc      := Buf (I);
            Buf (I) := Bc xor Crypto_Code (Obj);
            Update_Keys (Obj, Bc);  --  Keys are updated with the unencrypted byte
         end loop;
      end if;
   end Encode;

   procedure Decode (Obj : in out Crypto_Pack; B : in out Unsigned_8) is
   begin
      if Obj.Current_Mode = Encrypted then
         B := B xor Crypto_Code (Obj);
         Update_Keys (Obj, B);  --  Keys are updated with the unencrypted byte
      end if;
   end Decode;

end Zip.Crc_Crypto;
