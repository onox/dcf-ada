--  Copyright (c) 1999 - 2018 Gautier de Montmollin
--  SWITZERLAND
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

with Interfaces;

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;

with Unzip.Decompress;
with Zip.Headers;
with Zip_Streams;

use Ada.Exceptions;
use Interfaces;

package body Unzip is

   use Ada.Strings.Unbounded;

   Boolean_To_Encoding : constant array (Boolean) of Zip.Zip_Name_Encoding :=
     (False => Zip.IBM_437, True => Zip.UTF_8);

   Fallback_Compressed_Size : constant File_Size_Type := File_Size_Type'Last;

   ----------------------------------------------------
   --  *The* internal 1-file unzipping procedure.    --
   --  Input must be _open_ and won't be _closed_ !  --
   ----------------------------------------------------

   procedure Unzipfile
     (Zip_File             : in out Zip_Streams.Root_Zipstream_Type'Class;
      Out_Name             :        String;
      Out_Name_Encoding    :        Zip.Zip_Name_Encoding;
      Name_From_Header     :        Boolean;
      Header_Index         : in out Zip_Streams.Zs_Index_Type;
      Hint_Comp_Size       :        File_Size_Type;  --  Added 2007 for .ODS files
      Hint_Crc_32          :        Unsigned_32;     --  Added 2012 for decryption
      Feedback             :        Zip.Feedback_Proc;
      Help_The_File_Exists :        Resolve_Conflict_Proc;
      Tell_Data            :        Tell_Data_Proc;
      Options              :        Option_Set;
      File_System_Routines :        Fs_Routines_Type)
   is
      Work_Index                 : Zip_Streams.Zs_Index_Type := Header_Index;
      Local_Header               : Zip.Headers.Local_File_Header;
      Data_Descriptor_After_Data : Boolean;
      Method                     : Pkzip_Method;

      Skip_This_File : Boolean                                := False;
      Bin_Text_Mode  : constant array (Boolean) of Write_Mode :=
        (Write_To_Binary_File, Write_To_Text_File);
      Mode : constant array (Boolean) of Write_Mode :=
        (Bin_Text_Mode (Options (Extract_As_Text)), Just_Test);
      Actual_Mode : Write_Mode := Mode (Options (Test_Only));

      True_Packed_Size : File_Size_Type;  --  Encryption adds 12 to packed size

      The_Output_Name : Unbounded_String;

      --  27-Jun-2001 : possibility of trashing directory part of a name
      --               e.g. :  unzipada\uza_src\unzip.ads -> unzip.ads
      function Maybe_Trash_Dir (N : String) return String is
         Idx : Integer := N'First - 1;
      begin
         if Options (Junk_Directories) then
            for I in N'Range loop
               if N (I) = '/' or N (I) = '\' then
                  Idx := I;
               end if;
            end loop;
            --  idx points on the index just before the interesting part
            return N (Idx + 1 .. N'Last);
         else
            return N;
         end if;
      end Maybe_Trash_Dir;

      procedure Set_Definitively_Named_Outfile (Composed_Name : String) is
         Idx           : Integer := Composed_Name'First - 1;
         First_In_Name : Integer;
      begin
         for I in Composed_Name'Range loop
            if Composed_Name (I) = '/' or Composed_Name (I) = '\' then
               Idx := I;
            end if;
         end loop;
         --  idx points on the index just before the name part

         if Idx >= Composed_Name'First
           and then Actual_Mode in Write_To_File
           and then File_System_Routines.Create_Path /= null
         then
            --  Not only the name, also a path.
            --  In that case, we may need to create parts of the path.
            declare
               Directory_Separator : constant Character := '/';
               --  The '/' separator is also recognized by Windows' routines,
               --  so we can just use it as a standard. See the discussion started
               --  in July 2010 in the Ada Comment mailing list about it
               --  for the 2012 standard.
               Path : String := Composed_Name (Composed_Name'First .. Idx - 1);
            begin
               --  Set the file separator recognized by the OS
               for I in Path'Range loop
                  if Path (I) = '\' or Path (I) = '/' then
                     Path (I) := Directory_Separator;
                  end if;
               end loop;
               if Path = "" then
                  null;
               elsif Path (Path'Last) = ':' then
                  null;  --  We are on Windows and cannot create drives (like "C:")
               else
                  File_System_Routines.Create_Path (Path);
               end if;
            end;
         end if;
         --  Now we can create the file itself
         First_In_Name := Composed_Name'First;

         The_Output_Name :=
           To_Unbounded_String (Composed_Name (First_In_Name .. Composed_Name'Last));
      end Set_Definitively_Named_Outfile;

      function Full_Path_Name
        (File_Name_In_Archive : String;
         Encoding             : Zip.Zip_Name_Encoding) return String
      is
      begin
         if File_System_Routines.Compose_File_Name = null then
            return File_Name_In_Archive;
         else
            return File_System_Routines.Compose_File_Name (File_Name_In_Archive, Encoding);
         end if;
      end Full_Path_Name;

      procedure Set_Outfile (Long_Not_Composed_Name : String; Encoding : Zip.Zip_Name_Encoding) is
         --  Eventually trash the archived directory structure, then
         --  eventually add/modify/... another one
         Name : constant String :=
           Full_Path_Name (Maybe_Trash_Dir (Long_Not_Composed_Name), Encoding);
      begin
         Set_Definitively_Named_Outfile (Name);
      end Set_Outfile;

      procedure Set_Outfile_Interactive
        (Long_Not_Composed_Possible_Name : String;
         Encoding                        : Zip.Zip_Name_Encoding)
      is
         --  Eventually trash the archived directory structure, then
         --  eventually add/modify/... another one
         Possible_Name : constant String :=
           Full_Path_Name (Maybe_Trash_Dir (Long_Not_Composed_Possible_Name), Encoding);
         --  Possible_name may have a different encoding depending on Compose_File_Name
         New_Name        : String (1 .. 1024);
         New_Name_Length : Natural;
      begin
         if Help_The_File_Exists /= null and then Zip.Exists (Possible_Name) then
            loop
               case Current_User_Attitude is
                  when Yes | No | Rename_It =>  --  Then ask for this name too
                     Help_The_File_Exists
                       (Long_Not_Composed_Possible_Name,
                        Encoding,
                        Current_User_Attitude,
                        New_Name,
                        New_Name_Length);
                  when Yes_To_All | None | Abort_Now =>
                     exit;  --  Nothing to decide: previous decision was definitive
               end case;
               exit when not
                 (Current_User_Attitude = Rename_It
                  and then  --  New name exists too!
                  Zip.Exists (New_Name (1 .. New_Name_Length)));
            end loop;

            --  User has decided
            case Current_User_Attitude is
               when Yes | Yes_To_All =>
                  Skip_This_File := False;
                  Set_Definitively_Named_Outfile (Possible_Name);
               when No | None =>
                  Skip_This_File := True;
               when Rename_It =>
                  Skip_This_File := False;
                  Set_Definitively_Named_Outfile (New_Name (1 .. New_Name_Length));
               when Abort_Now =>
                  raise User_Abort;
            end case;
         else  --  No name conflict or non-interactive (help_the_file_exists=null)
            Skip_This_File := False;
            Set_Definitively_Named_Outfile (Possible_Name);
         end if;
      end Set_Outfile_Interactive;

      procedure Inform_User (Name : String; Comp, Uncomp : File_Size_Type) is
      begin
         if Tell_Data /= null then
            Tell_Data (Name, Comp, Uncomp, Method);
         end if;
      end Inform_User;

      The_Name : String (1 .. 65_535);  --  Seems overkill, but Zip entry names can be that long!
      The_Name_Len : Natural;
      use Zip, Zip_Streams;

      Actual_Feedback : Zip.Feedback_Proc;

      Dummy_Memory          : P_Stream_Element_Array;
      Dummy_Stream          : constant P_Stream := null;
      Encrypted, Dummy_Bool : Boolean;
   begin
      begin
         Set_Index (Zip_File, Work_Index);
         Zip.Headers.Read_And_Check (Zip_File, Local_Header);
      exception
         when Zip.Headers.Bad_Local_Header =>
            raise;  --  Processed later, on Extract
         when others =>
            raise Zip.Archive_Corrupted;
      end;

      Method := Zip.Method_From_Code (Local_Header.Zip_Type);
      if Method = Unknown then
         Ada.Exceptions.Raise_Exception
           (Unzip.Unsupported_Method'Identity,
            "Format (method) #" & Unsigned_16'Image (Local_Header.Zip_Type) & " is unknown");
      end if;

      --  Calculate offset of data
      Work_Index :=
        Work_Index +
        Zs_Size_Type
          (Local_Header.Filename_Length +
           Local_Header.Extra_Field_Length +
           Zip.Headers.Local_Header_Length);

      Data_Descriptor_After_Data := (Local_Header.Bit_Flag and 8) /= 0;

      if Data_Descriptor_After_Data then
         --  Sizes and CRC are stored after the data
         --  We set size to avoid getting a sudden Zip_EOF !
         if Local_Header.Zip_Type = 0 and then Hint_Comp_Size = Fallback_Compressed_Size then
            --  For Stored (Method 0) data we need a correct "compressed" size.
            --  If the hint is the bogus fallback value, it is better to trust
            --  the local header, since this size is known in advance. Case found
            --  in Microsoft's OneDrive cloud storage (in 2018). Zip files,
            --  created by the server for downloading more than one file, are
            --  using the "Store" format and a postfixed Data Descriptor for
            --  writing the CRC value.
            null;  --  Do not overwrite the compressed size in that case
         else
            Local_Header.Dd.Compressed_Size := Hint_Comp_Size;
         end if;
         Local_Header.Dd.Crc_32            := Hint_Crc_32;
         Local_Header.Dd.Uncompressed_Size := File_Size_Type'Last;
         Actual_Feedback                   := null;  --  No feedback possible: unknown sizes
      else
         --  Sizes and CRC are stored before the data, inside the local header
         Actual_Feedback := Feedback;  --  Use the given feedback procedure
      end if;

      Encrypted := (Local_Header.Bit_Flag and Zip.Headers.Encryption_Flag_Bit) /= 0;
      pragma Assert (not Encrypted);

      True_Packed_Size := File_Size_Type (Local_Header.Dd.Compressed_Size);

      if Name_From_Header then  --  Name from local header is used as output name
         The_Name_Len := Natural (Local_Header.Filename_Length);
         if The_Name_Len > 0 then
            String'Read (Zip_File'Access, The_Name (1 .. The_Name_Len));
         end if;
         if not Data_Descriptor_After_Data then
            Inform_User
              (The_Name (1 .. The_Name_Len),
               True_Packed_Size,
               File_Size_Type (Local_Header.Dd.Uncompressed_Size));
         end if;
         if The_Name_Len = 0
           or else (The_Name (The_Name_Len) = '/' or The_Name (The_Name_Len) = '\')
         then
            --  This is a directory name (12-feb-2000)
            Skip_This_File := True;
         elsif Actual_Mode in Write_To_File then
            Set_Outfile_Interactive
              (The_Name (1 .. The_Name_Len),
               Boolean_To_Encoding
                 ((Local_Header.Bit_Flag and Zip.Headers.Language_Encoding_Flag_Bit) /= 0));
         else
            --  Only informational, no need for interaction
            Set_Outfile
              (The_Name (1 .. The_Name_Len),
               Boolean_To_Encoding
                 ((Local_Header.Bit_Flag and Zip.Headers.Language_Encoding_Flag_Bit) /= 0));
         end if;
      else
         --  Output name is given: out_name
         if not Data_Descriptor_After_Data then
            Inform_User
              (Out_Name,
               True_Packed_Size,
               File_Size_Type (Local_Header.Dd.Uncompressed_Size));
         end if;
         if Out_Name'Length = 0
           or else (Out_Name (Out_Name'Last) = '/' or Out_Name (Out_Name'Last) = '\')
         then
            --  This is a directory name, so do not write anything (30-Jan-2012)
            Skip_This_File := True;
         elsif Actual_Mode in Write_To_File then
            Set_Outfile_Interactive (Out_Name, Out_Name_Encoding);
         else
            --  Only informational, no need for interaction
            Set_Outfile (Out_Name, Out_Name_Encoding);
         end if;
      end if;

      if Skip_This_File then
         Actual_Mode := Just_Test;
      end if;

      if Skip_This_File and not Data_Descriptor_After_Data then
         --  We can skip actually since sizes are known
         if Feedback /= null then
            Feedback (Percents_Done => 0, Entry_Skipped => True, User_Abort => Dummy_Bool);
         end if;
      else
         begin
            Set_Index (Zip_File, Work_Index);  --  Eventually skips the file name
         exception
            when others =>
               Raise_Exception
                 (Zip.Archive_Corrupted'Identity,
                  "End of stream reached (location: between local header and archived data)");
         end;
         Unzip.Decompress.Decompress_Data
           (Zip_File                   => Zip_File,
            Format                     => Method,
            Mode                       => Actual_Mode,
            Output_File_Name           => To_String (The_Output_Name),
            Output_Memory_Access       => Dummy_Memory,
            Output_Stream_Access       => Dummy_Stream,
            Feedback                   => Actual_Feedback,
            Explode_Literal_Tree       => (Local_Header.Bit_Flag and 4) /= 0,
            Explode_Slide_8kb_Lzma_Eos =>
              (Local_Header.Bit_Flag and Zip.Headers.Lzma_Eos_Flag_Bit) /= 0,
            Data_Descriptor_After_Data => Data_Descriptor_After_Data,
            Hint                       => Local_Header);

         if Actual_Mode /= Just_Test then
            begin
               if File_System_Routines.Set_Time_Stamp /= null then
                  File_System_Routines.Set_Time_Stamp
                    (To_String (The_Output_Name),
                     Convert (Local_Header.File_Timedate));
               end if;
            exception
               when Zip_Streams.Calendar.Time_Error | Ada.Calendar.Time_Error =>
                  null;  --  Invalid time, we give up setting the time stamp
            end;
         end if;

         if Data_Descriptor_After_Data then  --  Sizes and CRC at the end
            --  Inform after decompression
            Inform_User
              (To_String (The_Output_Name),
               Local_Header.Dd.Compressed_Size,
               Local_Header.Dd.Uncompressed_Size);
         end if;
      end if; -- not ( skip_this_file and not data_descriptor )

      --  Set the offset on the next zipped file
      Header_Index :=
        Header_Index +
        Zs_Size_Type
          (Local_Header.Filename_Length +
           Local_Header.Extra_Field_Length +
           Zip.Headers.Local_Header_Length) +
        Zs_Size_Type (Local_Header.Dd.Compressed_Size);

      if Data_Descriptor_After_Data then
         Header_Index := Header_Index + Zs_Size_Type (Zip.Headers.Data_Descriptor_Length);
      end if;
   exception
      when Ada.IO_Exceptions.End_Error =>
         Raise_Exception (Zip.Archive_Corrupted'Identity, "End of stream reached");
   end Unzipfile;

   ----------------------------------
   -- Simple extraction procedures --
   ----------------------------------

   --  Extract all files from an archive (from)

   procedure Extract
     (From                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, null, null, null, Options, File_System_Routines);
   end Extract;

   procedure Extract
     (From                 : String;
      What                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, What, What, null, null, null, Options, File_System_Routines);
   end Extract;

   procedure Extract
     (From                 : String;
      What                 : String;
      Rename               : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, What, Rename, null, null, null, Options, File_System_Routines);
   end Extract;

   procedure Extract
     (From                 : Zip.Zip_Info;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, null, null, null, Options, File_System_Routines);
   end Extract;

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, What, What, null, null, null, Options, File_System_Routines);
   end Extract;

   procedure Extract
     (From                 : Zip.Zip_Info;
      What                 : String;
      Rename               : String;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines) is
   begin
      Extract (From, What, Rename, null, null, null, Options, File_System_Routines);
   end Extract;

   --  All previous extract call the following ones, with bogus UI arguments

   --------------------------------------------------------------
   --  All previous extraction procedures, for user interface  --
   --------------------------------------------------------------

   --  Extract one precise file (what) from an archive (from),
   --  but save under a new name (rename)

   procedure Extract
     (From                 : String;
      What                 : String;
      Rename               : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines)
   is
      use Zip, Zip_Streams;
      Zip_File : aliased File_Zipstream;
      --  Was Unbounded_Stream & file->buffer copy in v.26
      Header_Index : Zip_Streams.Zs_Index_Type;
      Comp_Size    : File_Size_Type;
      Uncomp_Size  : File_Size_Type;
      Crc_32       : Unsigned_32;
   begin
      if Feedback = null then
         Current_User_Attitude := Yes_To_All;  --  Non-interactive
      end if;
      Set_Name (Zip_File, From);
      Open (Zip_File, In_File);
      Zip.Find_Offset
        (File           => Zip_File,
         Name           => What,
         Case_Sensitive => Options (Case_Sensitive_Match),
         File_Index     => Header_Index,
         Comp_Size      => Comp_Size,
         Uncomp_Size    => Uncomp_Size,
         Crc_32         => Crc_32);
      Unzipfile
        (Zip_File             => Zip_File,
         Out_Name             => Rename,
         Out_Name_Encoding    => IBM_437,  --  Assumption...
         Name_From_Header     => False,
         Header_Index         => Header_Index,
         Hint_Comp_Size       => Comp_Size,
         Hint_Crc_32          => Crc_32,
         Feedback             => Feedback,
         Help_The_File_Exists => Help_The_File_Exists,
         Tell_Data            => Tell_Data,
         Options              => Options,
         File_System_Routines => File_System_Routines);
      Close (Zip_File);
   exception
      when Zip.Headers.Bad_Local_Header =>
         raise Zip.Archive_Corrupted with "Bad local header";
   end Extract;

   --  Extract all files from an archive (from)

   procedure Extract
     (From                 : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines)
   is
      use Zip, Zip_Streams;
      Zip_File : File_Zipstream;
      --   Was Unbounded_Stream & file->buffer copy in v.26
      Header_Index : Zip_Streams.Zs_Index_Type;
   begin
      if Feedback = null then
         Current_User_Attitude := Yes_To_All; -- non-interactive
      end if;
      Set_Name (Zip_File, From);
      Open (Zip_File, In_File);
      Zip.Find_First_Offset (Zip_File, Header_Index); -- >= 13-May-2001
      --  We simply unzip everything sequentially, until the end:
      All_Files :
      loop
         Unzipfile
           (Zip_File          => Zip_File,
            Out_Name          => "",
            Out_Name_Encoding => IBM_437, -- ignored
            Name_From_Header  => True,
            Header_Index      => Header_Index,
            Hint_Comp_Size    => Fallback_Compressed_Size,
            --                   ^ no better hint available if comp_size is 0 in local header
            Hint_Crc_32          => 0,  --  2.0 decryption can fail if data descriptor after data
            Feedback             => Feedback,
            Help_The_File_Exists => Help_The_File_Exists,
            Tell_Data            => Tell_Data,
            Options              => Options,
            File_System_Routines => File_System_Routines);
      end loop All_Files;
   exception
      when Zip.Headers.Bad_Local_Header | Zip.Archive_Is_Empty =>
         Close (Zip_File);  --  Normal case: end of archived entries (of fuzzy data) was hit
      when Zip.Zip_File_Open_Error =>
         raise;  --  Couldn't open zip file
      when others =>
         Close (Zip_File);
         raise;  --  Something else went wrong
   end Extract;

   --  Extract all files from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines)
   is
      procedure Extract_1_File (Name : String) is
      begin
         Extract
           (From                 => From,
            What                 => Name,
            Rename               => Name,
            Feedback             => Feedback,
            Help_The_File_Exists => Help_The_File_Exists,
            Tell_Data            => Tell_Data,
            Options              => Options,
            File_System_Routines => File_System_Routines);
      end Extract_1_File;

      procedure Extract_All_Files is new Zip.Traverse (Extract_1_File);
   begin
      Extract_All_Files (From);
   end Extract;

   --  Extract one precise file (what) from an archive (from)
   --  Needs Zip.Load(from, ...) prior to the extraction

   procedure Extract
     (From                 : Zip.Zip_Info;
      What, Rename         : String;
      Feedback             : Zip.Feedback_Proc;
      Help_The_File_Exists : Resolve_Conflict_Proc;
      Tell_Data            : Tell_Data_Proc;
      Options              : Option_Set       := No_Option;
      File_System_Routines : Fs_Routines_Type := Null_Routines)
   is
      use Zip;
      use Zip_Streams;

      Header_Index : Zip_Streams.Zs_Index_Type;
      Comp_Size    : File_Size_Type;
      Uncomp_Size  : File_Size_Type;
      Crc_32       : Unsigned_32;

      Zip_File      : aliased File_Zipstream;
      Input_Stream  : Zipstream_Class_Access;
      Use_A_File    : constant Boolean := From.Stream = null;
      Name_Encoding : Zip.Zip_Name_Encoding;
   begin
      if Use_A_File then
         Input_Stream := Zip_File'Unchecked_Access;
         Set_Name (Zip_File, From.Name);
         Open (Zip_File, In_File);
      else -- use the given stream
         Input_Stream := From.Stream;
      end if;
      if Feedback = null then
         Current_User_Attitude := Yes_To_All;  --  Non-interactive
      end if;
      Zip.Find_Offset
        (Info          => From,
         Name          => What,
         Name_Encoding => Name_Encoding,
         File_Index    => Header_Index,
         Comp_Size     => Comp_Size,
         Uncomp_Size   => Uncomp_Size,
         Crc_32        => Crc_32);
      Unzipfile
        (Zip_File             => Input_Stream.all,
         Out_Name             => Rename,
         Out_Name_Encoding    => Name_Encoding,
         Name_From_Header     => False,
         Header_Index         => Header_Index,
         Hint_Comp_Size       => Comp_Size,
         Hint_Crc_32          => Crc_32,
         Feedback             => Feedback,
         Help_The_File_Exists => Help_The_File_Exists,
         Tell_Data            => Tell_Data,
         Options              => Options,
         File_System_Routines => File_System_Routines);
      if Use_A_File then
         Close (Zip_File);
      end if;
   exception
      when Zip.Headers.Bad_Local_Header =>
         if Use_A_File and then Is_Open (Zip_File) then
            Close (Zip_File);
         end if;
         raise Zip.Archive_Corrupted with "Bad local header";
      when others =>
         if Use_A_File and then Is_Open (Zip_File) then
            Close (Zip_File);
         end if;
         raise;
   end Extract;

end Unzip;
