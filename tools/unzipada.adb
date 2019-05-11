------------------------------------------------------------------------------
--  File:            UnZipAda.adb
--  Description:     A minimal standalone command-line unzipping tool
--                     using the Zip-Ada library.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Interfaces;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Float_Text_IO;
with Ada.Text_IO;

with Zip;
with Unzip;

use Ada.Characters.Handling;
use Ada.Command_Line;
use Ada.Calendar;
use Ada.Text_IO;
use Ada.Float_Text_IO;
use Interfaces;

--  Pure Ada Text_IO-fashion feedback; should work on every
--  computer having a screen [and some text console too] :

with My_Feedback;
with My_Resolve_Conflict;
with My_Tell_Data;
with Summary;

procedure Unzipada is

   Set_Time_Stamp : constant Unzip.Set_Time_Stamp_Proc := null;
   --  If you want the time stamps, uncomment the following
   --  and look into Set_Modification_Time_B above.
   --
   --  Set_Modification_Time_B'Unrestricted_Access;

   use Unzip;

   Z_Options        : Unzip.Option_Set := Unzip.No_Option;
   Quiet            : Boolean          := False;
   Lower_Case_Match : Boolean          := False;
   Comment          : Boolean          := False;

   Fda : Zip.Feedback_Proc     := My_Feedback'Access;
   Rca : Resolve_Conflict_Proc := My_Resolve_Conflict'Access;
   Tda : Tell_Data_Proc        := My_Tell_Data'Access;

   Last_Option : Natural := 0;

   Exdir     : String (1 .. 1024);
   Exdir_Len : Natural := 0;

   Directory_Separator : constant Character := '/';
   --  '/' is also accepted by Windows

   function Add_Extract_Directory (File_Name : String) return String is
   --  OK for UNIX & Windows, but VMS has "[x.y.z]filename.ext"
   begin
      if Exdir_Len = 0 then
         return File_Name;
      elsif Exdir (Exdir_Len) = '\' or Exdir (Exdir_Len) = '/' then
         return Exdir (1 .. Exdir_Len) & File_Name;
      else
         return Exdir (1 .. Exdir_Len) & Directory_Separator & File_Name;
      end if;
   end Add_Extract_Directory;

   function Compose_File_Name
     (File_Name     : String;
      Name_Encoding : Zip.Zip_Name_Encoding) return String
   is
      Fn1 : String := File_Name;
   begin
      if Lower_Case_Match then
         Fn1 := To_Lower (Fn1);
      end if;
      return Add_Extract_Directory (Fn1);
   end Compose_File_Name;

   My_Fs_Routines : constant Fs_Routines_Type :=
     (Create_Path       => Ada.Directories.Create_Path'Access, -- Ada 2005
      Set_Time_Stamp    => Set_Time_Stamp,
      Compose_File_Name => Compose_File_Name'Unrestricted_Access,
      others            => null);

   T0, T1          : Time;
   Seconds_Elapsed : Duration;

   package Iio is new Integer_IO (Integer);
   package Mio is new Modular_IO (Unzip.File_Size_Type);

   procedure Blurb is
   begin
      Put_Line ("UnZipAda " & Zip.Version & " - unzipping tool using dcf-ada");
      New_Line;
   end Blurb;

   procedure Help is
   begin
      Blurb;
      Put_Line ("Usage: unzipada [options] zipfile[.zip] [files...]");
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
                     Exdir (1 .. Arg_Exdir'Length) := Arg_Exdir;
                     Exdir_Len                     := Arg_Exdir'Length;
                  end;
                  Last_Option := I + 1;
               when 'c' =>
                  Z_Options (Case_Sensitive_Match) := True;
               when 'l' =>
                  Lower_Case_Match := True;
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
      --
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

      if not Quiet then
         Blurb;
      end if;
      if not (Quiet or Comment) then
         if Z_Options (Test_Only) then
            Put ("Testing");
         else
            if Set_Time_Stamp = null then
               Put_Line
                 (" Warning: time stamps and attributes of files" &
                  " in archive are not reproduced !");
               New_Line;
            end if;
            Put ("Extracting");
         end if;
         if not Extract_All then
            Put (" some file(s) from");
         end if;
         Put_Line (" archive " & Archive);
      end if;

      T0 := Clock;
      if Comment then -- Option: -z , diplay comment only
         Zip.Load (Zi, Archive);
         Zip.Put_Multi_Line (Standard_Output, Zip.Zip_Comment (Zi));
      elsif Extract_All then
         Extract (Archive, Fda, Rca, Tda, Z_Options, My_Fs_Routines);
      else
         Zip.Load (Zi, Archive);
         for I in Last_Option + 2 .. Argument_Count loop
            Extract (Zi, Argument (I), Fda, Rca, Tda, Z_Options, My_Fs_Routines);
         end loop;
      end if;
      T1 := Clock;
   end;

   Seconds_Elapsed := T1 - T0;

   if not (Quiet or Comment) then
      New_Line (2);
      Iio.Put (Summary.Total_Entries, 7);
      Put (" entries  ------ Total ------ ");
      Mio.Put (Summary.Total_Compressed, 10);
      if Summary.Total_Uncompressed = 0 then
         Put (" :         ");
      else
         Put (" :");
         Iio.Put
           (Natural
              ((100.0 * Long_Float (Summary.Total_Compressed)) /
               Long_Float (Summary.Total_Uncompressed)),
            4);
         Put ("% of ");
      end if;
      Mio.Put (Summary.Total_Uncompressed, 10);
      New_Line (2);

      if Z_Options (Test_Only) then
         Put_Line ("Test: no error found");
         New_Line;
         Put_Line ("Statistics per Zip sub-format (""method""):");
         for M in Summary.Files_Per_Method'Range loop
            if Summary.Files_Per_Method (M) > 0 then
               Put ("  " & Summary.Nice_Image (M) & "... ");
               Iio.Put (Summary.Files_Per_Method (M), 5);
               Put (" files");
               if Summary.Uncompressed_Per_Method (M) > 0 then
                  Put (",");
                  Iio.Put
                    (Natural
                       ((100.0 * Long_Float (Summary.Uncompressed_Per_Method (M))) /
                        Long_Float (Summary.Total_Uncompressed)),
                     4);
                  Put ("% of all data; compr.-to-decompr. ratio: ");
                  Iio.Put
                    (Natural
                       ((100.0 * Long_Float (Summary.Compressed_Per_Method (M))) /
                        Long_Float (Summary.Uncompressed_Per_Method (M))),
                     4);
                  Put ('%');
               end if;
               New_Line;
            end if;
         end loop;
         New_Line;
      end if;

      Put ("Time elapsed : ");
      Put (Float (Seconds_Elapsed), 4, 2, 0);
      Put_Line (" sec");

      Put_Line ("Archive successfully processed (or empty archive, or no archive!)");
   end if;

end Unzipada;
