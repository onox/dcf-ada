with Ada.Text_IO;

with DCF.Zip;
with DCF.Unzip;

use DCF.Unzip;
use Ada.Text_IO;

procedure My_Resolve_Conflict
  (Name            : in     String;
   Name_Encoding   : in     DCF.Zip.Zip_Name_Encoding;
   Action          :    out Name_Conflict_Intervention;
   New_Name        :    out String;
   New_Name_Length :    out Natural)
is
   C : Character;
begin
   loop
      Put ("replace " & Name & "? [y]es, [n]o, [A]ll, [N]one, [r]ename: ");
      declare
         Input : constant String := Get_Line;
      begin
         C := Input (Input'First);
         exit when C = 'y' or C = 'n' or C = 'A' or C = 'N' or C = 'r';
         Put_Line ("error: invalid response [" & Input & "]");
      end;
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
      when 'r' =>
         Action := Rename_It;
         Put ("new name: ");
         Get_Line (New_Name, New_Name_Length);
      when others =>
         raise Program_Error;
   end case;
end My_Resolve_Conflict;
