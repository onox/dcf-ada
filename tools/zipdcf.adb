--  SPDX-License-Identifier: MIT
--
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with DCF.Streams.Calendar;
with DCF.Zip.Compress;
with DCF.Zip.Create;

use Ada.Command_Line;
use Ada.Text_IO;

procedure ZipDCF is
   package Dirs renames Ada.Directories;
   package SU   renames Ada.Strings.Unbounded;

   Quiet : Boolean := False;

   Add_Directories  : Boolean := True;
   Recursive        : Boolean := False;
   Junk_Directories : Boolean := False;

   Comment            : SU.Unbounded_String;
   Compression_Method : DCF.Zip.Compress.Compression_Method := DCF.Zip.Compress.Deflate_2;

   Last_Option : Natural := 0;

   procedure Help is
   begin
      Put_Line ("ZipDCF " & DCF.Zip.Version & " - create document container files");
      New_Line;
      Put_Line ("Usage: zipdcf [-options] [-z comment] file list");
      New_Line;
      Put_Line ("  -D  no directory entries           -q  quiet mode");
      Put_Line ("  -r  recurse into directories       -j  junk directory structure");
      Put_Line ("  -0  store files uncompressed");
      Put_Line ("  -1  use faster compression         -z  add archive file comment");
      Put_Line ("  -9  use better compression");
   end Help;

   function Maybe_Trash_Dir (Name : String) return String is
      Index : constant Natural := Ada.Strings.Fixed.Index (Name, "/", Ada.Strings.Backward);
   begin
      return (if Junk_Directories then Name (Index + 1 .. Name'Last) else Name);
   end Maybe_Trash_Dir;
begin
   if Argument_Count = 0 then
      Help;
      return;
   end if;

   for I in 1 .. Argument_Count loop
      if Argument (I) (1) = '-' then
         if Last_Option = I then
            null;  -- Argument for a previous option
         else
            Last_Option := I;

            if Argument (I)'Length = 1 then
               Help;
               return;
            end if;

            for J in 2 .. Argument (I)'Last loop
               case Argument (I) (J) is
                  when 'D' =>
                     Add_Directories := False;
                  when 'r' =>
                     Recursive := True;
                  when '0' =>
                     Compression_Method := DCF.Zip.Compress.Store;
                  when '1' =>
                     Compression_Method := DCF.Zip.Compress.Deflate_1;
                  when '9' =>
                     Compression_Method := DCF.Zip.Compress.Deflate_3;
                  when 'q' =>
                     Quiet := True;
                  when 'j' =>
                     Junk_Directories := True;
                  when 'z' =>
                     if I = Argument_Count then
                        Help;
                        return;
                     end if;

                     Comment := SU.To_Unbounded_String (Argument (I + 1));
                     Last_Option := I + 1;
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
      Archive : constant String := Argument (Last_Option + 1);
   begin
      if Dirs.Exists (Archive) then
         Put_Line ("Archive file '" & Archive & "' already exists");
         return;
      end if;

      if not Quiet then
         Put_Line ("Archive:  " & Archive);
      end if;

      declare
         Archive_Stream : aliased DCF.Streams.File_Zipstream
           := DCF.Streams.Create (Archive);
         Info : DCF.Zip.Create.Zip_Create_Info;

         procedure Add_File (Path : String) is
            Name : constant String := Maybe_Trash_Dir (Path);
         begin
            if not Dirs.Exists (Path) then
               Put_Line ("warning: " & Name & " does not exist");
               return;
            end if;

            declare
               use all type Dirs.File_Kind;

               File_Is_Directory : constant Boolean := Dirs.Kind (Path) = Directory;
            begin
               if not File_Is_Directory then
                  if not Quiet then
                     Put_Line ("  adding: " & Name);
                  end if;

                  declare
                     File_Stream : aliased DCF.Streams.File_Zipstream
                       := DCF.Streams.Open (Path);
                  begin
                     DCF.Streams.Set_Name (File_Stream, Name);
                     DCF.Streams.Set_Time (File_Stream,
                       DCF.Streams.Calendar.Convert (Dirs.Modification_Time (Path)));
                     DCF.Zip.Create.Add_Stream (Info, File_Stream);
                  end;
               else
                  if Add_Directories then
                     if not Quiet then
                        Put_Line ("  adding: " & Name & '/');
                     end if;

                     declare
                        Empty_Array : aliased Ada.Streams.Stream_Element_Array := (1 .. 0 => <>);
                        Dir_Stream  : aliased DCF.Streams.Array_Zipstream (Empty_Array'Access);
                     begin
                        DCF.Streams.Set_Name (Dir_Stream, Name & '/');
                        DCF.Streams.Set_Time (Dir_Stream,
                          DCF.Streams.Calendar.Convert (Dirs.Modification_Time (Path)));
                        DCF.Zip.Create.Add_Stream (Info, Dir_Stream);
                     end;
                  end if;

                  if Recursive then
                     declare
                        procedure Add_Entry (Next_Entry : Dirs.Directory_Entry_Type) is
                           Entry_Name : constant String := Dirs.Simple_Name (Next_Entry);
                        begin
                           if Entry_Name not in "." | ".." then
                              Add_File (Dirs.Compose (Path, Entry_Name));
                           end if;
                        end Add_Entry;
                     begin
                        Dirs.Search (Path, "", Process => Add_Entry'Access);
                     end;
                  end if;
               end if;
            end;
         end Add_File;

         use type SU.Unbounded_String;

         String_Comment : constant String := SU.To_String (Comment);
      begin
         DCF.Zip.Create.Create
           (Info, Archive_Stream'Unchecked_Access, Compress => Compression_Method);

         for I in Last_Option + 2 .. Argument_Count loop
            Add_File (Argument (I));
         end loop;

         --  Add optional archive file comment
         if Comment /= "" then
            if not Quiet then
               Put_Line (" comment: " & String_Comment);
            end if;
            DCF.Zip.Create.Set_Comment (Info, String_Comment);
         end if;

         DCF.Zip.Create.Finish (Info);
      end;
   end;
end ZipDCF;
