--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2009 - 2018 Gautier de Montmollin
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with DCF.Streams.Calendar;
with DCF.Unzip.Streams;
with DCF.Zip;

use Ada.Command_Line;
use Ada.Text_IO;

procedure UnzipDCF is
   package Dirs renames Ada.Directories;
   package SU   renames Ada.Strings.Unbounded;

   List_Files : Boolean := False;
   Test_Data  : Boolean := False;
   Comment    : Boolean := False;

   Quiet      : Boolean := False;

   No_Directories : Boolean := False;
   Lower_Case     : Boolean := False;

   Last_Option : Natural := 0;

   Extraction_Directory : SU.Unbounded_String
     := SU.To_Unbounded_String (Dirs.Current_Directory);

   Name_Conflict_Decision : DCF.Unzip.Name_Conflict_Intervention := DCF.Unzip.Yes;

   procedure Help is
   begin
      Put_Line ("UnZipDCF " & DCF.Zip.Version & " - unzip document container files");
      New_Line;
      Put_Line ("Usage: unzipdcf [-options[modifiers]] [-d exdir] file [list]");
      New_Line;
      Put_Line ("  -l  list files");
      Put_Line ("  -t  test integrity of files, no write");
      Put_Line ("  -z  display archive comment only");
      Put_Line ("  -d  extract to ""exdir""");
      Put_Line ("modifiers:");
      Put_Line ("  -n  never overwrite existing files        -q  quiet mode");
      Put_Line ("  -o  always overwrite existing files");
      Put_Line ("  -j  junk archived directory structure     -L  make names lower case");
   end Help;

   procedure Resolve_Conflict
     (Name            : in     String;
      Action          :    out DCF.Unzip.Name_Conflict_Intervention;
      New_Name        :    out String;
      New_Name_Length :    out Natural)
   is
      C : Character;

      use all type DCF.Unzip.Name_Conflict_Intervention;
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
   end Resolve_Conflict;

   function Get_Out_File
     (Containing_Directory   : String;
      File                   : DCF.Zip.Archived_File;
      Name_Conflict_Decision : in out DCF.Unzip.Name_Conflict_Intervention) return String
   is
      pragma Assert (Containing_Directory (Containing_Directory'Last) /= '/');

      use Ada.Characters.Handling;
      use all type DCF.Unzip.Name_Conflict_Intervention;
      use all type Dirs.File_Kind;

      function Maybe_Trash_Dir (File_Name : String) return String is
         Name  : constant String  := (if Lower_Case then To_Lower (File_Name) else File_Name);
         Index : constant Natural := Ada.Strings.Fixed.Index (Name, "/", Ada.Strings.Backward);
      begin
         return (if No_Directories then Name (Index + 1 .. Name'Last) else Name);
      end Maybe_Trash_Dir;

      --  Optionally trash the archived directory structure and then concatenate with
      --  extraction directory
      Possible_Name : constant String := Maybe_Trash_Dir (File.Name);
      Possible_Path : constant String := Containing_Directory & '/' & Possible_Name;

      New_Name        : String (1 .. 1024);
      New_Name_Length : Natural;
   begin
      if Test_Data or else Possible_Name = ""
        or else not Dirs.Exists (Possible_Path)
        or else Dirs.Kind (Possible_Path) = Directory
      then
         return Possible_Name;
      end if;

      loop
         case Name_Conflict_Decision is
            when Yes | No | Rename_It =>
               --  Then ask for this name too
               Resolve_Conflict
                 (File.Name,
                  Name_Conflict_Decision,
                  New_Name,
                  New_Name_Length);
            when Yes_To_All | None =>
               --  Nothing to decide: previous decision was definitive
               exit;
         end case;
         exit when not
           (Name_Conflict_Decision = Rename_It
            and then  --  New name exists too!
            Dirs.Exists (Containing_Directory & '/' & New_Name (1 .. New_Name_Length)));
      end loop;

      --  User has decided
      case Name_Conflict_Decision is
         when Yes | Yes_To_All =>
            return Possible_Name;
         when No | None =>
            return "";
         when Rename_It =>
            return New_Name (1 .. New_Name_Length);
      end case;
   end Get_Out_File;
begin
   if Argument_Count = 0 then
      Help;
      return;
   end if;

   for I in 1 .. Argument_Count loop
      if Argument (I) (1) = '-' then
         if Last_Option = I then
            null; -- was in fact an argument for previous option (e.g. "-s")
         else
            Last_Option := I;

            if Argument (I)'Length = 1 then
               Help;
               return;
            end if;

            for J in 2 .. Argument (I)'Last loop
               case Argument (I) (J) is
                  when 't' =>
                     Test_Data := True;
                  when 'l' =>
                     List_Files := True;
                  when 'j' =>
                     No_Directories := True;
                  when 'd' =>
                     if I = Argument_Count then
                        Help;
                        return;  --  "-d" without the directory or anything ?!
                     end if;
                     Extraction_Directory := SU.To_Unbounded_String
                       (Dirs.Full_Name (Argument (I + 1)));
                     Last_Option := I + 1;
                  when 'L' =>
                     Lower_Case := True;
                  when 'n' =>
                     Name_Conflict_Decision := DCF.Unzip.None;
                  when 'o' =>
                     Name_Conflict_Decision := DCF.Unzip.Yes_To_All;
                  when 'q' =>
                     Quiet := True;
                  when 'z' =>
                     Comment := True;
                  when others =>
                     Help;
                     return;
               end case;
            end loop;
         end if;
      end if;
   end loop;

   if Argument_Count = Last_Option then
      Help;
      return;
   end if;

   declare
      Archive     : constant String  := Argument (Last_Option + 1);
      Extract_All : constant Boolean := Argument_Count = Last_Option + 1;
   begin
      if not Dirs.Exists (Archive) then
         Put_Line ("Archive file '" & Archive & "' not found");
         return;
      end if;

      declare
         Archive_Stream : aliased DCF.Streams.File_Zipstream
           := DCF.Streams.Open (Archive);
         Info : DCF.Zip.Zip_Info;
      begin
         DCF.Zip.Load (Info, Archive_Stream);

         if not Quiet then
            Put_Line ("Archive:  " & Info.Name);
         end if;

         if (Comment or not Quiet) and Info.Comment'Length > 0 then
            Put_Line (Info.Comment);
         end if;

         if Comment then
            null;
         elsif List_Files then
            declare
               package Mod_IO is new Modular_IO (DCF.Unzip.File_Size_Type);
               package Int_IO is new Integer_IO (Integer);

               use type DCF.Unzip.File_Size_Type;

               function Percentage
                 (Left, Right : DCF.Unzip.File_Size_Type) return Integer is
               begin
                  if Left = Right or else Right = 0 then
                     return 0;
                  else
                     return Integer (100.0 - (100.0 * Float (Left)) / Float (Right));
                  end if;
               end Percentage;

               Total_Uncompressed_Size : DCF.Unzip.File_Size_Type := 0;
               Total_Compressed_Size   : DCF.Unzip.File_Size_Type := 0;

               procedure List_File_From_Stream (File : DCF.Zip.Archived_File) is
                  Date_Time : constant Ada.Calendar.Time
                    := DCF.Streams.Calendar.Convert (File.Date_Time);
                  Date : constant String := Ada.Calendar.Formatting.Image
                    (Date_Time, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (Date_Time));
               begin
                  Total_Uncompressed_Size := Total_Uncompressed_Size + File.Uncompressed_Size;
                  Total_Compressed_Size   := Total_Compressed_Size + File.Compressed_Size;

                  --  Print date and time without seconds
                  Mod_IO.Put (File.Uncompressed_Size, 10);
                  Int_IO.Put (Percentage (File.Compressed_Size, File.Uncompressed_Size), 4);
                  Put_Line ("%  " & Date (Date'First .. Date'Last - 3) & "   " & File.Name);
               end List_File_From_Stream;

               procedure List_All_Files is new DCF.Zip.Traverse (List_File_From_Stream);
            begin
               Put_Line ("  Length   Cmpr     Date    Time    Name");
               Put_Line ("---------- ----  ---------- -----   ----");

               List_All_Files (Info);

               Put_Line ("---------- ----                     -------");
               Mod_IO.Put (Total_Uncompressed_Size, 10);
               Int_IO.Put (Percentage (Total_Compressed_Size, Total_Uncompressed_Size), 4);
               Put ("%                    " & Info.Entries'Image);
               Put_Line (if Info.Entries > 1 then " files" else " file");
            end;
         else
            declare
               Extraction_Folder : constant String := SU.To_String (Extraction_Directory);
               pragma Assert (Extraction_Folder (Extraction_Folder'Last) /= '/');

               function No_Directory (Name : String) return String is
                 (if Name (Name'Last) = '/' then Name (Name'First .. Name'Last - 1) else Name);

               procedure Extract_From_Stream (File : DCF.Zip.Archived_File) is
                  Name : constant String
                    := Get_Out_File (Extraction_Folder, File, Name_Conflict_Decision);
                  File_Is_Directory : constant Boolean := File.Name (File.Name'Last) = '/';
               begin
                  if Name = "" then
                     return;
                  end if;

                  declare
                     Path : constant String := Extraction_Folder & "/" & No_Directory (Name);

                     use type Dirs.File_Kind;
                  begin
                     if not Test_Data and then Path /= Dirs.Full_Name (Path) then
                        raise DCF.Unzip.Write_Error with
                          "Entry " & Name & " is located outside extraction directory";
                        --  TODO Ask to resolve conflict (rename) Name in Get_Out_File instead
                     end if;

                     if not Quiet then
                        if Test_Data then
                           Put ("    testing: " & Name);
                        elsif File_Is_Directory then
                           pragma Assert (not No_Directories);
                           if not Dirs.Exists (Path)
                             or else Dirs.Kind (Path) /= Dirs.Directory
                           then
                              Put_Line ("   creating: " & Name);
                              Dirs.Create_Path (Path);
                           end if;
                        elsif File.Compressed then
                           Put_Line ("  inflating: " & Name);
                        else
                           Put_Line (" extracting: " & Name);
                        end if;
                     end if;

                     if Test_Data then
                        declare
                           Stream_Writer : DCF.Unzip.Streams.Stream_Writer (null);
                        begin
                           DCF.Unzip.Streams.Extract
                             (Destination      => Stream_Writer,
                              Archive_Info     => Info,
                              File             => File,
                              Verify_Integrity => Test_Data);
                           Put_Line (" OK");
                        exception
                           when DCF.Unzip.CRC_Error =>
                              Put_Line (" ERROR");
                        end;
                     elsif not File_Is_Directory then
                        declare
                           Directory_Path : constant String := Dirs.Containing_Directory (Path);
                        begin
                           --  Create folder if necessary
                           if not Dirs.Exists (Directory_Path) then
                              Dirs.Create_Path (Directory_Path);
                           end if;
                        end;

                        declare
                           File_Stream : aliased DCF.Streams.File_Zipstream
                             := DCF.Streams.Create (Path);
                           Stream_Writer : DCF.Unzip.Streams.Stream_Writer (File_Stream'Access);
                        begin
                           DCF.Unzip.Streams.Extract
                             (Destination      => Stream_Writer,
                              Archive_Info     => Info,
                              File             => File,
                              Verify_Integrity => Test_Data);
                        end;
                     end if;
                  end;
               end Extract_From_Stream;

               procedure Extract_All_Files is new DCF.Zip.Traverse (Extract_From_Stream);
               procedure Extract_One_File  is new DCF.Zip.Traverse_One_File (Extract_From_Stream);
            begin
               if not Test_Data and then not Dirs.Exists (Extraction_Folder) then
                  Dirs.Create_Path (Extraction_Folder);
               end if;

               if Extract_All then
                  Extract_All_Files (Info);
               else
                  for I in Last_Option + 2 .. Argument_Count loop
                     Extract_One_File (Info, Argument (I));
                  end loop;
               end if;
            end;
         end if;
      end;
   end;
end UnzipDCF;
