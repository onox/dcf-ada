------------------------------------------------------------------------------
--  File:            Find_Zip.adb
--  Description:     Search a text string in files packed in a zip archive.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Unzip.Streams;
with Zip;

use Ada.Command_Line;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Characters.Handling;
use Ada.Strings.Fixed;

use Unzip.Streams;

procedure Find_Zip is

   Max : constant := 2**10; -- 1024
   Str : String (1 .. Max);  -- str(1..stl) = string to search
   Stl : Natural; -- string length
   L   : Character; -- last character of the search string

   Z : Zip.Zip_Info;

   Ignore_Case : constant Boolean := True;

   procedure Search_1_File_Using_Output_Stream (Name : String) is
      Occ : Natural := 0;
      --  Define a circular buffer
      Siz : constant := Max;
      type Buffer_Range is mod Siz;
      Buf : array (Buffer_Range) of Character := (others => ' ');
      Bup : Buffer_Range                      := 0;

      --  We define a local, ad-hoc stream type.
      type Search_Stream is new Ada.Streams.Root_Stream_Type with null record;

      overriding procedure Read
        (Stream : in out Search_Stream;
         Item   :    out Ada.Streams.Stream_Element_Array;
         Last   :    out Ada.Streams.Stream_Element_Offset) is null;  --  Not used.

      overriding procedure Write
        (Stream : in out Search_Stream;
         Item   : in     Ada.Streams.Stream_Element_Array);

      --  Implementation of Write:
      overriding procedure Write
        (Stream : in out Search_Stream;
         Item   : in     Ada.Streams.Stream_Element_Array)
      is
         pragma Unreferenced (Stream);
         C : Character;
         I : Buffer_Range := 0;
         J : Natural;
      begin
         for Sei in Item'Range loop
            C := Character'Val (Item (Sei));
            if Ignore_Case then
               C := To_Upper (C);
            end if;
            if C = L then -- last character do match, search further...
               I := Bup;
               J := Stl;
               Match :
               loop
                  I := I - 1;  --  this loops modulo max: 3, 2, 1, 0, max-1, max-2, ...
                  J := J - 1;
                  if J = 0 then -- we survived the whole search string
                     Occ := Occ + 1;
                     exit Match;
                  end if;
                  exit Match when Str (J) /= Buf (I);
               end loop Match;
            end if;
            Buf (Bup) := C;
            Bup       := Bup + 1;
         end loop;
      end Write;

      Sst : Search_Stream;

   begin
      Extract (Destination => Sst, Archive_Info => Z, Name => Name, Ignore_Directory => False);
      if Occ > 0 then
         Put (Occ, 5);
         Put_Line (" in [" & To_Lower (Name) & "]'s contents");
      end if;
   end Search_1_File_Using_Output_Stream;

   --  Old variant using an input stream (memory footprint is uncompressed
   --  size plus fixed amounts: can be large!)

   procedure Search_1_File_Using_Input_Stream (Name : String) is
      F   : Zipped_File_Type;
      S   : Stream_Access;
      C   : Character;
      Occ : Natural := 0;
      --  Define a circular buffer
      Siz : constant := Max;
      type Buffer_Range is mod Siz;
      Buf    : array (Buffer_Range) of Character := (others => ' ');
      I, Bup : Buffer_Range                      := 0;
      J      : Natural;
   begin
      Open (F, Z, Name);
      S := Stream (F);
      while not End_Of_File (F) loop
         Character'Read (S, C);
         if Ignore_Case then
            C := To_Upper (C);
         end if;
         if C = L then -- last character do match, search further...
            I := Bup;
            J := Stl;
            Match :
            loop
               I := I - 1;  --  this loops modulo max: 3, 2, 1, 0, max-1, max-2, ...
               J := J - 1;
               if J = 0 then -- we survived the whole search string
                  Occ := Occ + 1;
                  exit Match;
               end if;
               exit Match when Str (J) /= Buf (I);
            end loop Match;
         end if;
         Buf (Bup) := C;
         Bup       := Bup + 1;
      end loop;
      Close (F);
      if Occ > 0 then
         Put (Occ, 5);
         Put_Line (" in [" & To_Lower (Name) & "] (inward stream method)");
      end if;
   end Search_1_File_Using_Input_Stream;
   pragma Unreferenced (Search_1_File_Using_Input_Stream);

   procedure Search_All_Files is new Zip.Traverse (Search_1_File_Using_Output_Stream);

   procedure Search_In_Entry_Name (Name : String) is
      Un : String := Name;
   begin
      if Ignore_Case then
         Un := To_Upper (Un);
      end if;
      if Index (Un, Str (1 .. Stl)) > 0 then
         Put_Line (" Found in [" & To_Lower (Name) & "]'s entry name");
      end if;
   end Search_In_Entry_Name;

   procedure Search_All_File_Names is new Zip.Traverse (Search_In_Entry_Name);

   function Try_With_Zip (Name : String) return String is
   begin
      if Zip.Exists (Name) then
         return Name;
      else
         return Name & ".zip";
         --  Maybe the file doesn't exist, but we tried our best...
      end if;
   end Try_With_Zip;
begin
   if Argument_Count < 2 then
      Put_Line ("Find_Zip * Search a text string in files packed in a zip archive.");
      Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
      Put_Line ("Library version " & Zip.Version & " dated " & Zip.Reference);
      Put_Line ("URL: " & Zip.Web);
      New_Line;
      Put_Line ("Usage: find_zip archive[.zip] [""]text[""]");
      return;
   end if;
   declare
      N : constant String := Try_With_Zip (Argument (1));
   begin
      Zip.Load (Z, N);
   exception
      when Zip.Zip_File_Open_Error =>
         Put ("Can't open archive [" & N & ']');
         raise;
   end;
   declare
      S : String := Argument (2);
   begin
      Put_Line ("Searching string [" & S & "]");
      if Ignore_Case then
         S := To_Upper (S);
      end if;
      Stl := S'Length;
      if Stl > Str'Length then
         raise Constraint_Error;
      end if;
      Str (1 .. Stl) := S;
      L              := Str (Stl);
   end;
   Search_All_Files (Z);
   Search_All_File_Names (Z);
end Find_Zip;
