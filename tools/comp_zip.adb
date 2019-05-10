------------------------------------------------------------------------------
--  File:            Comp_Zip.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Core moved to Comp_Zip_Prc (22-Sep-2009)
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with Zip;

with Comp_Zip_Prc;

use Ada.Command_Line;
use Ada.Text_IO;

procedure Comp_Zip is
   Z : array (1 .. 2) of Zip.Zip_Info;

   function Try_With_Zip (Zip_Name : String) return String is
   begin
      if Zip.Exists (Zip_Name) then
         return Zip_Name;
      else
         return Zip_Name & ".zip";
         --  Maybe the file doesn't exist, but we tried our best...
      end if;
   end Try_With_Zip;

   Quiet : Natural := 0;

   procedure Blurb is
   begin
      Put_Line ("Comp_Zip * compare two zip archive files, incl. contents");
      Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
      Put_Line ("Library version " & Zip.Version & " dated " & Zip.Reference);
      Put_Line ("URL: " & Zip.Web);
      New_Line;
   end Blurb;
begin
   if Argument_Count < 2 then
      Blurb;
      Put_Line ("Usage: comp_zip archive1[.zip] archive2[.zip] [options]");
      New_Line;
      Put_Line ("Options: -q1: (quiet level 1): summary only");
      Put_Line ("         -q2: (quiet level 2): shorter summary only");
      return;
   end if;
   for I in 1 .. 2 loop
      declare
         N : constant String := Try_With_Zip (Argument (I));
      begin
         Zip.Load (Z (I), N);
      exception
         when Zip.Zip_File_Open_Error =>
            Put ("Can't open archive [" & N & ']');
            raise;
      end;
   end loop;
   for A in 3 .. Argument_Count loop
      declare
         Arg : String renames Argument (A);
      begin
         if Arg'Length > 2 and then Arg (Arg'First .. Arg'First + 1) = "-q" then
            Quiet := Natural'Value (Arg (Arg'First + 2 .. Arg'Last));
         end if;
      end;
   end loop;

   if Quiet = 0 then
      Blurb;
   end if;
   Comp_Zip_Prc (Z (1), Z (2), Quiet);
end Comp_Zip;
