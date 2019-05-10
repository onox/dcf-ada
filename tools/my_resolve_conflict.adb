with Ada.Text_IO;

with Zip;
with Unzip;

use Unzip;
use Ada.Text_IO;

procedure My_Resolve_Conflict
  (Name            : in     String;
   Name_Encoding   : in     Zip.Zip_Name_Encoding;
   Action          :    out Name_Conflict_Intervention;
   New_Name        :    out String;
   New_Name_Length :    out Natural)
is
   pragma Unreferenced (Name_Encoding);
   C : Character;
begin
   loop
      New_Line;
      Put_Line ("File " & Name & " already exists.");
      Put (" Overwrite ?  (y)es / (n)o / (A)ll / (N)one / (r)ename / (q)uit ");
      Get_Immediate (C);
      Put_Line ("-> " & C);
      exit when C = 'y' or C = 'n' or C = 'A' or C = 'N' or C = 'r' or C = 'q';
   end loop;
   case C is
      when 'y' =>
         Action := Yes;
      when 'n' =>
         Action := No;
      when 'A' =>
         Action := Yes_To_All;
      when 'N' =>
         Action := None;
      when 'q' =>
         Action := Abort_Now;
      when 'r' =>
         Action := Rename_It;
         Put ("New name: ");
         Get_Line (New_Name, New_Name_Length);
      when others =>
         null;
   end case;

   --  Cosmetic : position for the [.....]
   Put ("                                                                    ");
end My_Resolve_Conflict;
