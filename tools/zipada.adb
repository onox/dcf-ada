------------------------------------------------------------------------------
--  File:            ZipAda.adb
--  Description:     A minimal standalone command-line zip archiving utility
--                     using the Zip-Ada library.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
--  Important changes:
--
--  ZA v. 49: password can be set
--  ZA v. 28: uses the Zip.Create package
--  ZA v. 26: modified for the new Zip_Stream package

with Interfaces;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Zip_Streams;
with Zip.Compress;
with Zip.Create;

use Ada.Calendar;
use Ada.Command_Line;
use Ada.Directories;
use Ada.Text_IO;
use Ada.Float_Text_IO;
use Ada.Strings.Fixed;
use Ada.Characters.Handling;

use Zip_Streams;
use Zip.Create;

procedure Zipada is

   T0, T1          : Ada.Calendar.Time;
   Seconds_Elapsed : Duration;

   procedure Blurb is
   begin
      Put_Line ("ZipAda " & Zip.Version & " - zipping tool using dcf-ada");
      New_Line;
   end Blurb;

   function Cutname (N : String; L : Natural) return String is
      Dots : constant String := "...";
   begin
      if N'Length > L then
         return Dots & N (N'Last - (L - 1) + Dots'Length .. N'Last);
      else
         return N;
      end if;
   end Cutname;

   --  Final zipfile stream
   Mystream : aliased File_Zipstream;
   Info     : Zip_Create_Info;

   procedure Add_1_Stream (Stream : in out Root_Zipstream_Type'Class) is
      use Interfaces;

      Compressed_Size : Zip.File_Size_Type;
      Final_Method    : Unsigned_16;
   begin
      Put ("  Adding ");
      declare
         Maxlen : constant        := 24;
         Cut    : constant String := Cutname (Get_Name (Stream), Maxlen);
      begin
         Put (Cut & (1 + Maxlen - Cut'Length) * ' ');
      end;

      Add_Stream (Info, Stream, null, Compressed_Size, Natural (Final_Method));

      if Size (Stream) = 0 then
         Put ("          ");
      end if;
      Put (' ');
      declare
         Meth : constant String := Zip.Image (Zip.Method_From_Code (Final_Method));
      begin
         Put (Meth & (Zip.Pkzip_Method'Width - Meth'Length) * ' ');
      end;
      if Size (Stream) > 0 then
         Put (", to ");
         Put (100.0 * Float (Compressed_Size) / Float (Size (Stream)), 3, 2, 0);
         Put ('%');
      end if;
      Put_Line (", done.");
   end Add_1_Stream;

   function Add_Zip_Ext (S : String) return String is
   begin
      if S'Length < 4 or else To_Upper (S (S'Last - 3 .. S'Last)) /= ".ZIP" then
         return S & ".zip";
      else
         return S;
      end if;
   end Add_Zip_Ext;

   use Zip.Compress;

   Method       : Compression_Method := Deflate_1;
   Zip_Name_Set : Boolean            := False;

   procedure Zip_A_File (Arg : String) is
      Instream : File_Zipstream := Open (Arg);
   begin
      Set_Time (Instream, Ada.Directories.Modification_Time (Arg));
      Add_1_Stream (Instream);
   exception
      when Ada.Text_IO.Use_Error =>
         Put_Line ("  ** Warning: skipping invalid entry: " & Arg);
   end Zip_A_File;

   Len : Natural := 0;  --  absolute directory prefix, to be skipped.

   --  Recursive directory scan expanded from this example:
   --
   --  http://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

   procedure Walk (Name : String; Pattern : String; Level : Natural; Recursive : Boolean) is
      procedure Process_File (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            declare
               Fn : constant String := Full_Name (Item);
            begin
               Zip_A_File (Fn (Fn'First + Len .. Fn'Last));
            end;
         end if;
      end Process_File;

      procedure Walk_Subdirectory (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), Pattern, Level + 1, True);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end Walk_Subdirectory;
   begin
      if Level = 0 then  --  Figure out the length of the absolute path
         Len := Full_Name (".")'Length + 1;
      end if;
      --  Process files
      Search (Name, Pattern, (Directory => False, others => True), Process_File'Access);
      --  Process subdirectories
      if Recursive then
         Search (Name, "", (Directory => True, others => False), Walk_Subdirectory'Access);
      end if;
   exception
      when Ada.Directories.Name_Error => -- "unknown directory" -> probably a file.
         if Level = 0 then
            if Ada.Directories.Exists (Name) then
               Zip_A_File (Name);
            else
               Put_Line ("  ** Warning [a]: name not matched: " & Name);
            end if;
            Zip_A_File (Name);
         end if;
   end Walk;

   type Scan_Mode is (Files_Only, Files_And_Dirs, Files_And_Dirs_Recursive, Patterns_Recursive);
   Scan : Scan_Mode := Files_Only;

   Overwrite_Disallowed : exception;

   procedure Process_Argument (I : Positive) is
      Arg     : constant String := Argument (I);
      Arg_Zip : constant String := Add_Zip_Ext (Arg);
      Answer  : Character;
   begin
      if Arg (Arg'First) = '-' or Arg (Arg'First) = '/' then
         --  Options
         declare
            --  Spaces to avoid too short slices
            Opt : constant String := Arg (Arg'First + 1 .. Arg'Last) & "    ";
            Ex  : constant String := Opt (Opt'First .. Opt'First + 1);
         begin
            if Ex = "e0" then
               Method := Store;
            elsif Ex = "ed" then
               case Opt (Opt'First + 2) is
                  when 'f' =>
                     Method := Deflate_Fixed;
                  when '1' =>
                     Method := Deflate_1;
                  when '2' =>
                     Method := Deflate_2;
                  when others =>
                     Method := Deflate_3;
               end case;
            elsif Opt (Opt'First .. Opt'First + 3) = "dir " then
               Scan := Scan_Mode'Max (Scan, Files_And_Dirs);
            elsif Ex = "r " then
               Scan := Files_And_Dirs_Recursive;
            elsif Ex = "r2" then
               Scan := Patterns_Recursive;
            end if;
         end;
      elsif not Zip_Name_Set then
         Zip_Name_Set := True;
         if Ada.Directories.Exists (Arg_Zip) then
            Put ("Archive " & Arg_Zip & " already exists! Overwrite (y/n) ?");
            Get_Immediate (Answer);
            Answer := To_Upper (Answer);
            Put_Line (" -> " & Answer);
            if Answer /= 'Y' then
               Put_Line ("Stopped.");
               raise Overwrite_Disallowed;
            end if;
         end if;
         Put_Line ("Creating archive " & Arg_Zip);
         Put_Line ("Method: " & Compression_Method'Image (Method));
         T0 := Clock;
         Create (Info, Mystream'Unchecked_Access, Arg_Zip, Method, Zip.Error_On_Duplicate);
      else
         --  First real argument has already been used for archive's name
         if To_Upper (Arg) = To_Upper (Name (Info)) then
            Put_Line ("  ** Warning: skipping archive's name as entry: " & Arg);
         --  Avoid zipping the archive itself!
         --  NB: case insensitive
         else
            case Scan is
               when Files_Only =>
                  if Ada.Directories.Exists (Arg) then
                     Zip_A_File (Arg);
                  else
                     Put_Line ("  ** Warning [b]: name not matched: " & Arg);
                  end if;
               when Files_And_Dirs =>
                  Walk (Arg, "*", 0, False);
               when Files_And_Dirs_Recursive =>
                  Walk (Arg, "*", 0, True);
               when Patterns_Recursive =>
                  Walk (".", Arg, 0, True);
            end case;
         end if;
      end if;
   end Process_Argument;

begin
   Blurb;
   for I in 1 .. Argument_Count loop
      Process_Argument (I);
   end loop;
   if Is_Created (Info) then
      Finish (Info);
      T1              := Clock;
      Seconds_Elapsed := T1 - T0;
      Put ("Time elapsed : ");
      Put (Float (Seconds_Elapsed), 4, 2, 0);
      Put_Line (" sec");
   else
      Put_Line ("Usage: zipada [options] archive[.zip] name(s)");
      New_Line;
      Put_Line ("Options:  -e0    : ""Store"": zero compression, archiving only (like tar)");
      Put_Line ("          -edf   : ""Deflate"" method, with one ""fixed"" block (weak)");
      Put_Line
        ("          -edN   : ""Deflate"" method, ""dynamic"" compression, strength N = 1..3");
      New_Line;
      Put_Line ("      NB: default method is ""Deflate"", strength 1 (-ed1)");
      New_Line;
      Put_Line ("          -dir   : name(s) may be also directories,");
      Put_Line ("                      whose entire contents will be archived");
      Put_Line
        ("          -r     : same as ""-dir"", but recursively archives full subdirectories");
      Put_Line ("                      of the named directories as well");
      Put_Line ("          -r2    : search name(s) in current and all subdirectories as well;");
      Put_Line ("                      please enclose name(s) that have wildcards with");
      Put_Line ("                      single quotes, for example: '*.adb'");
   end if;
end Zipada;
