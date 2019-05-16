------------------------------------------------------------------------------
--  File:            UnZipAda.adb
--  Description:     A minimal standalone command-line unzipping tool
--                     using the Zip-Ada library.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Zip;
with Unzip;

use Ada.Characters.Handling;
use Ada.Command_Line;
use Ada.Text_IO;

with File_Operations;
with My_Resolve_Conflict;
with My_Tell_Data;
with Summary;

procedure Unzipada is

   Set_Time_Stamp : constant Unzip.Set_Time_Stamp_Proc := null;

   use Unzip;

   Z_Options : Unzip.Option_Set := Unzip.No_Option;
   Quiet     : Boolean          := False;
   Verbose   : Boolean          := True;
   Comment   : Boolean          := False;

   Fda : Zip.Feedback_Proc     := null;
   Rca : Resolve_Conflict_Proc := My_Resolve_Conflict'Access;
   Tda : Tell_Data_Proc        := My_Tell_Data'Access;

   Last_Option : Natural := 0;

   My_Fs_Routines : constant Fs_Routines_Type :=
     (Create_Path       => Ada.Directories.Create_Path'Access, -- Ada 2005
      Set_Time_Stamp    => Set_Time_Stamp,
      Compose_File_Name => File_Operations.Compose_File_Name'Access);

   package Iio is new Integer_IO (Integer);
   package Mio is new Modular_IO (Unzip.File_Size_Type);

   procedure Help is
   begin
      Put_Line ("UnZipAda " & Zip.Version & " - unzipping tool using dcf-ada");
      New_Line;
      Put_Line ("Usage: unzipada [options] file[.zip] [list]");
      New_Line;
      Put_Line ("options:  -t     : test .zip file integrity, no write");
      Put_Line ("          -j     : junk archived directory structure");
      Put_Line ("          -d dir : extract to ""dir"" instead of current");
      Put_Line ("          -c     : case sensitive name matching");
      Put_Line ("          -l     : force lower case on stored names");
      Put_Line ("          -a     : output as text file, with native line endings");
      Put_Line ("          -z     : display .zip archive comment only");
      Put_Line ("          -q     : quiet mode");
   end Help;

   Zi : Zip.Zip_Info;
begin
   if Argument_Count = 0 then
      Help;
      return;
   end if;
   for I in 1 .. Argument_Count loop
      if Argument (I) (1) = '-' or else Argument (I) (1) = '/' then
         if Last_Option = I then
            null; -- was in fact an argument for previous option (e.g. "-s")
         else
            Last_Option := I;
            if Argument (I)'Length = 1 then
               Help;
               return;
            end if;
            case To_Lower (Argument (I) (2)) is
               when 't' =>
                  Z_Options (Test_Only) := True;
               when 'j' =>
                  Z_Options (Junk_Directories) := True;
               when 'd' =>
                  if I = Argument_Count then
                     Help;
                     return;  --  "-d" without the directory or anything ?!
                  end if;
                  declare
                     Arg_Exdir : constant String := Argument (I + 1);
                  begin
                     File_Operations.Exdir (1 .. Arg_Exdir'Length) := Arg_Exdir;
                     File_Operations.Exdir_Len                     := Arg_Exdir'Length;
                  end;
                  Last_Option := I + 1;
               when 'c' =>
                  Z_Options (Case_Sensitive_Match) := True;
               when 'l' =>
                  File_Operations.Lower_Case_Match := True;
               when 'a' =>
                  Z_Options (Extract_As_Text) := True;
               when 'q' =>
                  Quiet := True;
               when 'z' =>
                  Comment := True;
               when others =>
                  Help;
                  return;
            end case;
         end if;
      end if;
   end loop;

   Current_User_Attitude := Yes;

   if Quiet then
      Fda := null;
      Rca := null;
      Tda := null;
   end if;

   Summary.Reset;

   if Argument_Count = Last_Option then -- options only ?!
      Help;
      return;
   end if;

   declare
      Archive_Given : constant String := Argument (Last_Option + 1);
      Zip_Ext       : Boolean         := False;
      Extract_All   : Boolean;

      function Archive return String is
      begin
         if Zip_Ext then
            return Archive_Given & ".zip";
         else
            return Archive_Given;
         end if;
      end Archive;
   begin
      if not Zip.Exists (Archive) then
         Zip_Ext := True;
         if not Zip.Exists (Archive) then
            Put_Line ("Archive file '" & Archive_Given & "' or '" & Archive & "' not found");
            return;
         end if;
      end if;
      Extract_All := Argument_Count = Last_Option + 1;
      --  Options and zipfile only

      if not (Quiet or Comment) then
         Put_Line ("Archive:  " & Archive);
      end if;

      if Comment then -- Option: -z , diplay comment only
         Zip.Load (Zi, Archive);
         Zip.Put_Multi_Line (Standard_Output, Zi.Comment);
      elsif Extract_All then
         Extract (Archive, Fda, Rca, Tda, Z_Options, My_Fs_Routines);
      else
         Zip.Load (Zi, Archive);
         for I in Last_Option + 2 .. Argument_Count loop
            Extract (Zi, Argument (I), Argument (I), Fda, Rca, Tda, Z_Options, My_Fs_Routines);
         end loop;
      end if;
   end;

   if not (Quiet or Comment) then
      if Verbose then
         Put_Line ("--------          ------- ----                              -------");
      else
         Put_Line ("--------                    -------");
      end if;

      Mio.Put (Summary.Total_Uncompressed, 8);
      if Verbose then
         Mio.Put (Summary.Total_Compressed, 17);
         Iio.Put
           (Natural
              (100.0 - (100.0 * Long_Float (Summary.Total_Compressed)) /
               Long_Float (Summary.Total_Uncompressed)),
            4);
         Put ("%");
         Put ("                    ");
      else
         Put ("          ");
      end if;

      Iio.Put (Summary.Total_Entries);
      Put_Line (if Summary.Total_Entries > 1 then " files" else " file");
   end if;

end Unzipada;
