------------------------------------------------------------------------------
--  File:            Comp_Zip_Prc.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Demonstrates the new Zip.Traverse procedure
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Interfaces;

with Ada.Characters.Handling;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Unzip.Streams;
with Zip;

use Ada.Text_IO;

use Ada.Integer_Text_IO;
use Ada.Characters.Handling;
use Interfaces;

use Unzip.Streams;

procedure Comp_Zip_Prc (Z1, Z2 : Zip.Zip_Info; Quiet : Natural) is
   Z : array (1 .. 2) of Zip.Zip_Info;
   Total_1,
   Total_2,
   Common,
   Size_Failures,
   Compare_Failures,
   Missing_1_In_2,
   Just_A_Directory,
   Missing_2_In_1,
   Total_Errors : Natural := 0;
   Total_Bytes : Integer_64 := 0;

   procedure Compare_1_File (Name : String) is
      F : array (1 .. 2) of Zipped_File_Type;
      S : array (1 .. 2) of Stream_Access;
      C : array (1 .. 2) of Character;
      P : Integer_64 := 1;

      function Cutname (N : String; L : Natural) return String is
         Dots : constant String := "...";
      begin
         if N'Length > L then
            return Dots & N (N'Last - (L - 1) + Dots'Length .. N'Last);
         else
            return N;
         end if;
      end Cutname;

      L        : constant                                       := 20;
      Mininame : constant String                                := To_Lower (Cutname (Name, L));
      Stuffing : constant String (1 .. L - Mininame'Length + 1) := (others => ' ');

   begin
      if Quiet = 0 then
         Put ("   [" & Stuffing & Mininame & "] ");
      end if;
      for I in 1 .. 2 loop
         begin
            Open (F (I), Z (I), Name);
            if I = 1 then
               Total_1 := Total_1 + 1;
            end if;
         exception
            when Zip.File_Name_Not_Found =>
               if Quiet = 0 then
                  Put ("   # Not found in archive [" & Z (I).Name & ']');
               end if;
               if I = 1 then
                  Put_Line ("-- internal error!");
               else
                  Close (F (1));
               end if;
               if Name (Name'Last) = '/' or Name (Name'Last) = '\' then
                  Just_A_Directory := Just_A_Directory + 1;
                  if Quiet = 0 then
                     Put_Line (" (just a dir.)");
                  end if;
               else
                  if Quiet = 0 then
                     New_Line;
                  end if;
               end if;
               Missing_1_In_2 := Missing_1_In_2 + 1;
               return;
         end;
         S (I) := Stream (F (I));
      end loop;
      --  File found, now the comparison:
      while not End_Of_File (F (1)) loop
         if End_Of_File (F (2)) then
            if Quiet = 0 then
               Put_Line
                 ("   # Shorter in [" &
                  Z (2).Name &
                  "] at position" &
                  Integer_64'Image (P));
            end if;
            Close (F (1));
            Close (F (2));
            Size_Failures := Size_Failures + 1;
            return;
         end if;
         --  Read one character in each stream
         for I in 1 .. 2 loop
            Character'Read (S (I), C (I));
         end loop;
         if C (1) /= C (2) then
            if Quiet = 0 then
               Put_Line ("   # Difference at position" & Integer_64'Image (P));
            end if;
            Close (F (1));
            Close (F (2));
            Compare_Failures := Compare_Failures + 1;
            return;
         end if;
         P := P + 1;
      end loop;
      if not End_Of_File (F (2)) then
         if Quiet = 0 then
            Put_Line ("   # Longer in [" & Z (2).Name & "]");
         end if;
         Close (F (1));
         Close (F (2));
         Size_Failures := Size_Failures + 1;
         return;
      end if;
      Close (F (1));
      Close (F (2));
      if Quiet = 0 then
         Put_Line ("OK -" & Integer_64'Image (P - 1) & " bytes compared");
      end if;
      Total_Bytes := Total_Bytes + (P - 1);
   end Compare_1_File;

   procedure Compare_All_Files is new Zip.Traverse (Compare_1_File);

   Err_Str : String (1 .. 5);
begin
   Z (1) := Z1;
   Z (2) := Z2;
   Put_Line ("* Comparing [" & Z (1).Name & "] and [" & Z (2).Name & "]");
   Compare_All_Files (Z (1));
   Total_2 := Zip.Entries (Z (2));
   Common  := Total_1 - Missing_1_In_2;
   if Quiet < 2 then
      Put_Line ("* === Results ===");
      Put_Line
        ("  1st archive: [" & Z (1).Name & "], total files:" & Natural'Image (Total_1));
      Put_Line
        ("  2nd archive: [" & Z (2).Name & "], total files:" & Natural'Image (Total_2));
      Put_Line ("  Total files compared: " & Natural'Image (Common));
      Put_Line ("  Total of correct bytes: " & Integer_64'Image (Total_Bytes));
   end if;
   Put_Line ("* === Comparison summary ===");
   Put (Err_Str, Size_Failures);
   Put_Line ("    Size failures . . . . . . . . . . . :" & Err_Str);
   Put (Err_Str, Compare_Failures);
   Put_Line ("    Content comparison failures . . . . :" & Err_Str);
   Put (Err_Str, Missing_1_In_2);
   Put ("    Files of 1st archive missing in 2nd :" & Err_Str);

   if Just_A_Directory > 0 then
      Put_Line (" (" & Integer'Image (Just_A_Directory) & " useless dir. names)");
   else
      New_Line;
   end if;

   Missing_2_In_1 := Total_2 - Common;
   --  t2 - m21 = t1 - m12 = # common files
   Put (Err_Str, Missing_2_In_1);
   for I in Err_Str'Range loop
      if Err_Str (I) = ' ' then
         Err_Str (I) := '_';
      end if;
   end loop;
   Put_Line ("  __Files of 2nd archive missing in 1st :" & Err_Str & "__");

   Total_Errors := Size_Failures + Compare_Failures + Missing_1_In_2 + Missing_2_In_1;

   Put (Err_Str, Total_Errors);
   Put_Line ("  Total of errors . . . . . . . . . . . :" & Err_Str);
end Comp_Zip_Prc;
