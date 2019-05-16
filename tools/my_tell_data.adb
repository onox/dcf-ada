with Interfaces;

with Ada.Text_IO;

with Unzip;

with Summary;

use Ada.Text_IO;
use Interfaces;

procedure My_Tell_Data
  (Name               : String;
   Compressed_Bytes   : Unzip.File_Size_Type;
   Uncompressed_Bytes : Unzip.File_Size_Type;
   Method             : Unzip.Pkzip_Method)
is
   package Iio is new Integer_IO (Integer);
   package Mio is new Modular_IO (Unzip.File_Size_Type);

   Verbose : constant Boolean := True;
begin
   if Summary.Total_Entries = 0 then
      if Verbose then
         Put_Line (" Length   Method    Size  Cmpr     Date    Time    CRC-32   Name");
         Put_Line ("--------  ------  ------- ----  ---------- -----  --------  ----");
      else
         Put_Line (" Length      Date    Time   Name");
         Put_Line ("--------  ---------- -----  ----");
      end if;
   end if;

   Mio.Put (Uncompressed_Bytes, 8);

   if Verbose then
      Put ("  xxxxxx");
      Mio.Put (Compressed_Bytes, 9);
      if Uncompressed_Bytes = 0 then
         Put ("0");
      else
         Iio.Put
           (Natural
              (100.0 - (100.0 * Long_Float (Compressed_Bytes)) /
               Long_Float (Uncompressed_Bytes)),
            4);
      end if;
      Put ("%");
   end if;

   Put ("  yyyy-mm-dd hh:mm");

   if Verbose then
      Put ("  xxxxxxxx");
   end if;

   Put_Line ("  " & Name);

   --  We summarize here the length of processed files
   Summary.Total_Uncompressed := Summary.Total_Uncompressed + Uncompressed_Bytes;
   Summary.Total_Compressed   := Summary.Total_Compressed + Compressed_Bytes;
   Summary.Total_Entries      := Summary.Total_Entries + 1;
   --  Per-method statistics:
   Summary.Files_Per_Method (Method)        := Summary.Files_Per_Method (Method) + 1;
   Summary.Uncompressed_Per_Method (Method) :=
     Summary.Uncompressed_Per_Method (Method) + Uncompressed_Bytes;
   Summary.Compressed_Per_Method (Method) :=
     Summary.Compressed_Per_Method (Method) + Compressed_Bytes;
end My_Tell_Data;
