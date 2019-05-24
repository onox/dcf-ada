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
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Unzip.Streams;
with Zip;
with Zip_Streams;

use Ada.Command_Line;
use Ada.Text_IO;

with My_Resolve_Conflict;

procedure UnzipDCF is
   package Dirs renames Ada.Directories;
   package SU   renames Ada.Strings.Unbounded;

   List_Files       : Boolean := False;
   Test_Data        : Boolean := False;
   Comment          : Boolean := False;

   Quiet            : Boolean := False;

   Junk_Directories : Boolean := False;
   Lower_Case_Match : Boolean := False;
   Extract_As_Text  : Boolean := False;

   Last_Option : Natural := 0;

   Extraction_Directory : SU.Unbounded_String
     := SU.To_Unbounded_String (Dirs.Current_Directory);

   Name_Conflict_Decision : Unzip.Name_Conflict_Intervention := Unzip.Yes;

   procedure Help is
   begin
      Put_Line ("UnZipDCF " & Zip.Version & " - unzip document container files");
      New_Line;
      Put_Line ("Usage: unzipdcf [-options[modifiers]] file [list] [-d exdir]");
      New_Line;
      Put_Line ("  -l  list files");
      Put_Line ("  -t  test integrity of files, no write");
      Put_Line ("  -z  display .zip archive comment only");
      Put_Line ("  -d  extract to ""exdir""");
      Put_Line ("modifiers:");
      Put_Line ("  -n  never overwrite existing files        -q  quiet mode");
      Put_Line ("  -o  always overwrite existing files       -a  output with native line endings");
      Put_Line ("  -j  junk archived directory structure     -L  force lower case on stored names");
   end Help;

   function Get_Out_File
     (Containing_Directory   : String;
      File                   : Zip.Archived_File;
      Name_Conflict_Decision : in out Unzip.Name_Conflict_Intervention) return String
   is
      pragma Assert (Containing_Directory (Containing_Directory'Last) /= '/');

      use Ada.Characters.Handling;
      use all type Unzip.Name_Conflict_Intervention;
      use all type Dirs.File_Kind;

      function Maybe_Trash_Dir (File_Name : String) return String is
         Name  : constant String  := (if Lower_Case_Match then To_Lower (File_Name) else File_Name);
         Index : constant Natural := Ada.Strings.Fixed.Index (Name, "/", Ada.Strings.Backward);
      begin
         return (if Junk_Directories then Name (Index + 1 .. Name'Last) else Name);
      end Maybe_Trash_Dir;

      --  Optionally trash the archived directory structure and then concatenate with
      --  extraction directory
      Possible_Name : constant String := Maybe_Trash_Dir (File.Name);
      Possible_Path : constant String := Containing_Directory & '/' & Possible_Name;
      --  TODO Use File.Name_Encoding

      New_Name        : String (1 .. 1024);
      New_Name_Length : Natural;
   begin
      if Possible_Name = ""
        or else not Dirs.Exists (Possible_Path)
        or else Dirs.Kind (Possible_Path) = Directory
      then
         return Possible_Name;
      end if;

      loop
         case Name_Conflict_Decision is
            when Yes | No | Rename_It =>
               --  Then ask for this name too
               My_Resolve_Conflict
                 (File.Name,
                  File.Name_Encoding,
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
      if Argument (I) (1) = '-' or else Argument (I) (1) = '/' then
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
                     Junk_Directories := True;
                  when 'd' =>
                     if I = Argument_Count then
                        Help;
                        return;  --  "-d" without the directory or anything ?!
                     end if;
                     Extraction_Directory := SU.To_Unbounded_String
                       (Dirs.Full_Name (Argument (I + 1)));
                     Last_Option := I + 1;
                  when 'L' =>
                     Lower_Case_Match := True;
                  when 'a' =>
                     Extract_As_Text := True;
                  when 'n' =>
                     Name_Conflict_Decision := Unzip.None;
                  when 'o' =>
                     Name_Conflict_Decision := Unzip.Yes_To_All;
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

      if not Quiet then
         Put_Line ("Archive:  " & Archive);
      end if;

      declare
         Archive_Stream : aliased Zip_Streams.File_Zipstream
           := Zip_Streams.Open (Archive);
         Zi : Zip.Zip_Info;
      begin
         Zip.Load (Zi, Archive_Stream, Archive);

         if Comment or not Quiet then
            Zip.Put_Multi_Line (Standard_Output, Zi.Comment);
         end if;

         if Comment then
            null;
         elsif List_Files then
            declare
               package Mod_IO is new Modular_IO (Unzip.File_Size_Type);

               Total_Uncompressed_Size : Unzip.File_Size_Type := 0;

               procedure List_File_From_Stream (File : Zip.Archived_File) is
                  use type Unzip.File_Size_Type;

                  Date_Time : constant Ada.Calendar.Time := Zip.Convert (File.Date_Time);
                  Date : constant String := Ada.Calendar.Formatting.Image
                    (Date_Time, Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset (Date_Time));
               begin
                  Total_Uncompressed_Size := Total_Uncompressed_Size + File.Uncompressed_Size;

                  --  Print date and time without seconds
                  Mod_IO.Put (File.Uncompressed_Size, 9);
                  Put_Line ("  " & Date (Date'First .. Date'Last - 3) & "   " & File.Name);
               end List_File_From_Stream;

               procedure List_All_Files is new Zip.Traverse (List_File_From_Stream);
            begin
               Put_Line ("  Length      Date    Time    Name");
               Put_Line ("---------  ---------- -----   ----");

               List_All_Files (Zi);

               Put_Line ("---------                     -------");
               Mod_IO.Put (Total_Uncompressed_Size, 9);
               Put ("                    " & Zi.Entries'Image);
               Put_Line (if Zi.Entries > 1 then " files" else " file");
            end;
         elsif Extract_All then
            declare
               Extraction_Folder : constant String := SU.To_String (Extraction_Directory);
               pragma Assert (Extraction_Folder (Extraction_Folder'Last) /= '/');

               type File_Stream_Writer
                 (File : access Zip_Streams.File_Zipstream)
               is new Ada.Streams.Root_Stream_Type with record
                  Index : Zip_Streams.Zs_Index_Type := Zip_Streams.Zs_Index_Type'First;
               end record;

               overriding procedure Read
                 (Stream : in out File_Stream_Writer;
                  Item   :    out Ada.Streams.Stream_Element_Array;
                  Last   :    out Ada.Streams.Stream_Element_Offset) is null;

               overriding procedure Write
                 (Stream : in out File_Stream_Writer;
                  Item   : in     Ada.Streams.Stream_Element_Array)
               is
                  use type Zip_Streams.Zs_Index_Type;
               begin
                  Zip_Streams.Root_Zipstream_Type'Class (Stream.File.all).Set_Index (Stream.Index);
                  Zip_Streams.Root_Zipstream_Type'Class (Stream.File.all).Write (Item);
                  Stream.Index := Stream.Index + Item'Length;
               end Write;

               function No_Directory (Name : String) return String is
                 (if Name (Name'Last) = '/' then Name (Name'First .. Name'Last - 1) else Name);

               procedure Extract_File_From_Stream (File : Zip.Archived_File) is
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
                     if Path /= Dirs.Full_Name (Path) then
                        raise Unzip.Write_Error with
                          "Entry " & Name & " is located outside extraction directory";
                        --  TODO Ask to resolve conflict (rename) Name in Get_Out_File instead
                     end if;

                     if not Quiet then
                        if File_Is_Directory then
                           pragma Assert (not Junk_Directories);
                           if not Dirs.Exists (Path) or else Dirs.Kind (Path) /= Dirs.Directory then
                              Put_Line ("   creating: " & Name);
                              Dirs.Create_Path (Name);
                           end if;
                        elsif File.Compressed then
                           Put_Line ("  inflating: " & Name);
                        else
                           Put_Line (" extracting: " & Name);
                        end if;
                     end if;

                     if not File_Is_Directory then
                        declare
                           File_Stream : aliased Zip_Streams.File_Zipstream
                             := Zip_Streams.Create (Path);
                           Stream_Writer : File_Stream_Writer (File_Stream'Access);
                        begin
                           Unzip.Streams.Extract
                             (Destination  => Stream_Writer,
                              Archive_Info => Zi,
                              Name         => File.Name);
                        end;
                     end if;
                  end;
               end Extract_File_From_Stream;

               procedure Extract_All_Files is new Zip.Traverse (Extract_File_From_Stream);
            begin
               if not Dirs.Exists (Extraction_Folder) then
                  Dirs.Create_Path (Extraction_Folder);
               end if;

               Extract_All_Files (Zi);
            end;
         else
            for I in Last_Option + 2 .. Argument_Count loop
               Put_Line (Argument (I));
               --  TODO List or extract single file
            end loop;
         end if;
      end;
   end;
end UnzipDCF;
