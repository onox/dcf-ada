with Interfaces;

with Ada.Text_IO;

with Unzip;

with My_Dots;
with Summary;

use Ada.Text_IO;
use Interfaces;

procedure My_Tell_Data
  (Name               : String;
   Compressed_Bytes   : Unzip.File_Size_Type;
   Uncompressed_Bytes : Unzip.File_Size_Type;
   Method             : Unzip.Pkzip_Method)
is
   package Mio is new Modular_IO (Unzip.File_Size_Type);

   function Cutname (N : String; L : Natural) return String is
      Dots : constant String := "...";
   begin
      if N'Length > L then
         return Dots & N (N'Last - (L - 1) + Dots'Length .. N'Last);
      else
         return N;
      end if;
   end Cutname;
begin
   New_Line;
   if Summary.Total_Entries = 0 then
      Put_Line (" Name                      Method    Compressed size      Uncompressed size");
      Put_Line (" ------------------------- --------- ---------------      -----------------");
   end if;
   Put (' ');
   My_Dots.Done_Dots := 0;
   declare
      Maxlen : constant        := 24;
      Cut    : constant String := Cutname (Name, Maxlen);
   begin
      Put (Cut);
      for L in Cut'Length .. Maxlen loop
         Put (' ');
      end loop;
   end;
   Put (' ' & Summary.Nice_Image (Method));
   Mio.Put (Compressed_Bytes, 10);
   if Uncompressed_Bytes = 0 then
      Put (" :         ");
   else
      Put (" :");
      Mio.Put
        (Unzip.File_Size_Type
           ((100.0 * Long_Float (Compressed_Bytes)) / Long_Float (Uncompressed_Bytes)),
         4);
      Put ("% of ");
   end if;
   Mio.Put (Uncompressed_Bytes, 10);
   Put (' ');
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
