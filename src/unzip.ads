--  ________  ___   ______       ______      ___
--  /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--    /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
--  /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

--  UnZip
--  -----
--
--  This library allows to uncompress deflated, enhanced deflated, bzip2-ed, lzma-ed,
--  imploded, reduced, shrunk and stored streams from a Zip archive stream.
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Ada translation and substantial rewriting by Gautier de Montmollin
--    On the web: see the Zip.web constant.
--  based on Pascal version 2.10 by Abimbola A Olowofoyeku,
--    http://www.foyeh.org/
--  itself based on Pascal version by Christian Ghisler,
--  itself based on C code by Info-Zip group (Mark Adler et al.)
--    http://www.info-zip.org/UnZip.html

--  Technical documentation: read appnote.txt

--  Legal licensing note:

--  Copyright (c) 1999 .. 2018 Gautier de Montmollin
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip;

with Ada.Calendar;
with Ada.Streams;

package Unzip is

   type Option is
     (Test_Only,             --  test .zip file integrity, no write
      Junk_Directories,      --  ignore directory info -> extract to current one
      Case_Sensitive_Match,  --  case sensitive name matching
      Extract_As_Text);      --  files will be written with native line endings

   type Option_Set is array (Option) of Boolean;

   No_Option : constant Option_Set := (others => False);

   --  Ada 2005's Ada.Directories.Create_Path
   --  For Ada 95 compatibility we pass it as an optional procedure access
   type Create_Path_Proc is access procedure (New_Directory : in String; Form : in String := "");

   --  This is system-dependent (or in a future Ada)
   type Set_Time_Stamp_Proc is access procedure (File_Name : String; Stamp : Ada.Calendar.Time);

   --  Alternatively, you can use Zip.Time to set file time stamps
   type Set_Ztime_Stamp_Proc is access procedure (File_Name : String; Stamp : Zip.Time);
   --  NB: you can use Zip.Convert to change Ada.Calendar.Time from/to Zip.Time
   --     or use our Split to avoid using Ada.Calendar at all

   --  This is for modifying output file names (e.g. adding a
   --  work directory, modifying the archived path, etc.)
   type Compose_Func is access function
     (File_Name     : String;
      Name_Encoding : Zip.Zip_Name_Encoding) return String;

   --  File System dependent settings
   type Fs_Routines_Type is record
      Create_Path       : Create_Path_Proc;
      Set_Time_Stamp    : Set_Time_Stamp_Proc;
      Compose_File_Name : Compose_Func;
      Set_Ztime_Stamp   : Set_Ztime_Stamp_Proc;  --  alt. to Set_Time_Stamp
   end record;

   Null_Routines : constant Fs_Routines_Type := (null, null, null, null);

   ----------------------------------
   -- Simple extraction procedures --
   ----------------------------------

   --  Extract all files from an archive (from)

   procedure Extract
     (From                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from)

   procedure Extract
     (From                 : String;
      What                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from),
   --  but save under a new name (rename)

   procedure Extract
     (From                 : String;
      What                 : String;
      Rename               : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   -------------------------------------------------------------------------
   -- Simple extraction procedures without re-searching central directory --
   -------------------------------------------------------------------------

   --  Extract all files from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from),
   --  but save under a new name (rename)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Rename               : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   subtype Pkzip_Method is Zip.Pkzip_Method;

   ----------------------------------------------
   -- Extraction procedures for user interface --
   ----------------------------------------------

   --  NB: the *_proc types are accesses to procedures - their usage
   --  may require the non-standard attribute "unrestricted_access",
   --  or some changes.
   --  Read unzipada.adb for details and examples.

   type Name_Conflict_Intervention is (Yes, No, Yes_To_All, None, Rename_It, Abort_Now);

   Current_User_Attitude : Name_Conflict_Intervention := Yes;
   --  reset to "yes" for a new session (in case of yes_to_all / none state!)

   type Resolve_Conflict_Proc is access procedure
     (Name            : in     String;
      Name_Encoding   : in     Zip.Zip_Name_Encoding;
      Action          :    out Name_Conflict_Intervention;
      New_Name        :    out String;
      New_Name_Length :    out Natural);

   --  Data sizes in archive
   subtype File_Size_Type is Zip.File_Size_Type;

   --  Inform user about some archive data

   type Tell_Data_Proc is access procedure
     (Name               : String;
      Compressed_Bytes   : File_Size_Type;
      Uncompressed_Bytes : File_Size_Type;
      Method             : Pkzip_Method);

   --  Extract all files from an archive (from)

   procedure Extract
     (From                 : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from)

   procedure Extract
     (From                 : String;
      What                 : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from),
   --  but save under a new name (rename)

   procedure Extract
     (From                 : String;
      What                 : String;
      Rename               : String;
      Feedback             : Zip.Feedback_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Using Zip_info structure:

   --  Extract all files from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   --  Extract one precise file (what) from an archive (from),
   --  but save under a new name (rename)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Rename               : String;
      Feedback             : Zip.Feedback_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines);

   Crc_Error               : exception;
   Uncompressed_Size_Error : exception;
   Write_Error             : exception;
   Read_Error              : exception;
   User_Abort              : exception;
   Not_Supported           : exception;
   Unsupported_Method      : exception;

private

   type Write_Mode is
     (Write_To_Binary_File, Write_To_Text_File, Write_To_Memory, Write_To_Stream, Just_Test);

   subtype Write_To_File is Write_Mode range Write_To_Binary_File .. Write_To_Text_File;

   type P_Stream is access all Ada.Streams.Root_Stream_Type'Class;

   type P_Stream_Element_Array is access all Ada.Streams.Stream_Element_Array;

end Unzip;
