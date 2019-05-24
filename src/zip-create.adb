--  Copyright (c) 2000 - 2018 Gautier de Montmollin (maintainer)
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

use Interfaces;

package body Zip.Create is

   procedure Create
     (Info       :    out Zip_Create_Info;
      Z_Stream   : in     Zipstream_Class_Access;
      Name       :        String;
      Compress   :        Zip.Compress.Compression_Method := Zip.Compress.Deflate_1;
      Duplicates :        Duplicate_Name_Policy           := Admit_Duplicates) is
   begin
      Info.Stream   := Z_Stream;
      Info.Compress := Compress;
      Info.Duplicates := Duplicates;
      pragma Assert (Info.Stream.Get_Name /= "");
--      if Name /= "" then
--         Set_Name (Info.Stream.all, Name);
--      end if;

      --  If we have a real file (File_Zipstream or descendent), create the file too
      --  FIXME User has to create file themselves
      raise Program_Error with "Create file yourself";
--      if Z_Stream.all in File_Zipstream'Class then
--         Zip_Streams.Create (File_Zipstream (Z_Stream.all), Zip_Streams.Out_File);
--      end if;
   end Create;

   function Is_Created (Info : Zip_Create_Info) return Boolean is
   begin
      return Info.Stream /= null;
   end Is_Created;

   procedure Set (Info : in out Zip_Create_Info; New_Method : Zip.Compress.Compression_Method) is
   begin
      Info.Compress := New_Method;
   end Set;

   function Name (Info : Zip_Create_Info) return String is
   begin
      return Get_Name (Info.Stream.all);
   end Name;

   procedure Dispose is new Ada.Unchecked_Deallocation (Dir_Entries, Pdir_Entries);

   procedure Resize (A : in out Pdir_Entries; Size : Integer) is
      Hlp : constant Pdir_Entries := new Dir_Entries (1 .. Size);
   begin
      if A = null then
         A := Hlp;
      else
         Hlp (1 .. Integer'Min (Size, A'Length)) := A (1 .. Integer'Min (Size, A'Length));
         Dispose (A);
         A := Hlp;
      end if;
   end Resize;

   --  Internal - add the catalogue entry corresponding to a
   --  compressed file in the Zip archive.
   --  The entire catalogue will be written at the end of the zip stream,
   --  and the entry as a local header just before the compressed data.
   --  The entry's is mostly incomplete in the end (name, size, ...); stream
   --  operations on the archive being built are not performed here,
   --  see Add_Stream for that.
   procedure Add_Catalogue_Entry (Info : in out Zip_Create_Info) is
   begin
      if Info.Last_Entry = 0 then
         Info.Last_Entry := 1;
         Resize (Info.Contains, 32);
      else
         Info.Last_Entry := Info.Last_Entry + 1;
         if Info.Last_Entry > Info.Contains'Last then
            --  Info.Contains is full, time to resize it!
            --  We do nothing less than double the size - better than
            --  whatever offer you'd get in your e-mails.
            Resize (Info.Contains, Info.Contains'Last * 2);
         end if;
      end if;
      declare
         Cfh : Central_File_Header renames Info.Contains (Info.Last_Entry).Head;
      begin
         --  Administration
         Cfh.Made_By_Version                   := 23;  --  Version 2.30
         Cfh.Comment_Length                    := 0;
         Cfh.Disk_Number_Start                 := 0;
         Cfh.Internal_Attributes               := 0;   --  0: binary; 1: text
         Cfh.External_Attributes               := 0;
         Cfh.Short_Info.Needed_Extract_Version := 10;  --  Value put by Zip/PKZip
         Cfh.Short_Info.Bit_Flag               := 0;
      end;
   end Add_Catalogue_Entry;

   --  This is just for detecting duplicates
   procedure Insert_To_Name_Dictionary (File_Name : String; M : in out Name_Mapping.Map) is
      Cm : Name_Mapping.Cursor;
      Ok : Boolean;
   begin
      M.Insert (To_Unbounded_String (File_Name), Cm, Ok);
      if not Ok then
         --  Name already registered
         raise Duplicate_Name with "Entry name = " & File_Name;
      end if;
   end Insert_To_Name_Dictionary;

   procedure Add_Stream
     (Info   : in out Zip_Create_Info;
      Stream : in out Root_Zipstream_Type'Class)
   is
      Compressed_Size : Zip.File_Size_Type;  --  Dummy
      Final_Method    : Natural;             --  Dummy
   begin
      Add_Stream (Info, Stream, null, Compressed_Size, Final_Method);
   end Add_Stream;

   Four_Gb : constant := 4 * (1024**3);

   Zip_32_Exceeded_Message : constant String :=
     "Zip file too large (for Zip_32 archive format): more than 4GB.";

   procedure Add_Stream
     (Info            : in out Zip_Create_Info;
      Stream          : in out Root_Zipstream_Type'Class;
      Feedback        : in     Feedback_Proc;
      Compressed_Size :    out Zip.File_Size_Type;
      Final_Method    :    out Natural)
   is
      Mem1, Mem2 : Zs_Index_Type := 1;
      Entry_Name : String        := Get_Name (Stream);
      Last       : Positive;
   begin
      --  Appnote.txt, V. J. :
      --    " All slashes should be forward slashes '/' as opposed to
      --    backwards slashes '\' "
      for I in Entry_Name'Range loop
         if Entry_Name (I) = '\' then
            Entry_Name (I) := '/';
         end if;
      end loop;
      if Info.Duplicates = Error_On_Duplicate then
         --  Check for duplicates; raises Duplicate_name in this case
         Insert_To_Name_Dictionary (Entry_Name, Info.Dir);
      end if;
      Add_Catalogue_Entry (Info);
      Last := Info.Last_Entry;
      declare
         Cfh : Central_File_Header renames Info.Contains (Last).Head;
         Shi : Local_File_Header renames Cfh.Short_Info;
      begin
         --  Administration - continued
         if Zip_Streams.Is_Unicode_Name (Stream) then
            Shi.Bit_Flag := Shi.Bit_Flag or Zip.Headers.Language_Encoding_Flag_Bit;
         end if;
         pragma Assert (Zip.Headers.Encryption_Flag_Bit not in Shi.Bit_Flag);
         if Is_Read_Only (Stream) then
            Cfh.External_Attributes := Cfh.External_Attributes or 1;
         end if;
         Shi.File_Timedate         := Get_Time (Stream);
         Shi.Dd.Uncompressed_Size  := Unsigned_32 (Size (Stream));
         Shi.Filename_Length       := Entry_Name'Length;
         Info.Contains (Last).Name := new String'(Entry_Name);
         Shi.Extra_Field_Length    := 0;

         Mem1                    := Index (Info.Stream.all);
         Cfh.Local_Header_Offset := Unsigned_32 (Mem1) - 1;
         --  Write the local header with incomplete informations
         Zip.Headers.Write (Info.Stream.all, Shi);

         String'Write (Info.Stream, Entry_Name);

         --  Write compressed file
         Zip.Compress.Compress_Data
           (Input            => Stream,
            Output           => Info.Stream.all,
            Input_Size_Known => True,
            Input_Size       => Shi.Dd.Uncompressed_Size,
            Method           => Info.Compress,
            Feedback         => Feedback,
            Crc              => Shi.Dd.Crc_32,
            Output_Size      => Shi.Dd.Compressed_Size,
            Zip_Type         => Shi.Zip_Type);
         if Shi.Zip_Type = Compression_Format_Code.Lzma then
            --  For LZMA, we always put an EOS marker. From PKWARE's Appnote:
            --
            --      5.8.9 Data compressed with method 14, LZMA, may include an end-of-stream
            --      (EOS) marker ending the compressed data stream.  This marker is not
            --      required, but its use is highly recommended to facilitate processing
            --      and implementers should include the EOS marker whenever possible.
            --      When the EOS marker is used, general purpose bit 1 must be set.  If
            --      general purpose bit 1 is not set, the EOS marker is not present.
            Shi.Bit_Flag := Shi.Bit_Flag or Lzma_Eos_Flag_Bit;
         end if;
         Mem2 := Index (Info.Stream.all);
         if Info.Zip_Archive_Format = Zip_32 and then Mem2 > Four_Gb then
            raise Zip_Capacity_Exceeded with Zip_32_Exceeded_Message;
         end if;
         --  Go back to the local header to rewrite it with complete informations
         --  known after the compression: CRC value, compressed size, actual compression format.
         Set_Index (Info.Stream.all, Mem1);
         Zip.Headers.Write (Info.Stream.all, Shi);
         --  Return to momentaneous end of file
         Set_Index (Info.Stream.all, Mem2);
         --
         Compressed_Size := Shi.Dd.Compressed_Size;
         Final_Method    := Natural (Shi.Zip_Type);
      end;
   end Add_Stream;

   procedure Add_File
     (Info            : in out Zip_Create_Info;
      Name            :        String;
      Name_In_Archive :        String := "";
      --  Default: add the file in the archive under the same name
      Delete_File_After : Boolean := False;
      --  Practical to delete temporary file after adding
      Name_Encoding     : Zip_Name_Encoding := IBM_437;
      Modification_Time : Time              := Default_Time;
      Is_Read_Only      : Boolean           := False;
      Feedback          : Feedback_Proc     := null)
   is
      Temp_Zip_Stream : aliased File_Zipstream := Open (Name);
      use Ada.Text_IO;
      Fd              : File_Type;
      Compressed_Size : Zip.File_Size_Type;  --  Unused
      Final_Method    : Natural;             --  Unused
   begin
      --  Read the file
      --  Eventually we set a new name for archiving:
      if Name_In_Archive /= "" then
         raise Program_Error with "Why change name to " & Name_In_Archive & "?";
--         Set_Name (Temp_Zip_Stream, Name_In_Archive);
      end if;
      Set_Unicode_Name_Flag (Temp_Zip_Stream, Name_Encoding = UTF_8);
      Set_Read_Only_Flag (Temp_Zip_Stream, Is_Read_Only);
      Set_Time (Temp_Zip_Stream, Modification_Time);
      --  Stuff into the .zip archive:
      Add_Stream (Info, Temp_Zip_Stream, Feedback, Compressed_Size, Final_Method);
      if Delete_File_After then
         Open (Fd, In_File, Name);
         Delete (Fd);
      end if;
   end Add_File;

   procedure Add_String
     (Info            : in out Zip_Create_Info;
      Contents        :        String;
      Name_In_Archive :        String;
      --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
      Name_Utf_8_Encoded : Boolean := False;
      --  Time stamp for this entry, e.g. Zip.Convert (Ada.Calendar.Clock)
      Creation_Time : Zip.Time := Default_Time)
   is
   begin
      Add_String
        (Info               => Info,
         Contents           => To_Unbounded_String (Contents),
         Name_In_Archive    => Name_In_Archive,
         Name_Utf_8_Encoded => Name_Utf_8_Encoded,
         Creation_Time      => Creation_Time);
   end Add_String;

   procedure Add_String
     (Info            : in out Zip_Create_Info;
      Contents        :        Unbounded_String;
      Name_In_Archive :        String;
      --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
      Name_Utf_8_Encoded : Boolean := False;
      --  Time stamp for this entry, e.g. Zip.Convert (Ada.Calendar.Clock)
      Creation_Time : Zip.Time := Default_Time)
   is
      Temp_Zip_Stream : aliased Memory_Zipstream;
   begin
      Set (Temp_Zip_Stream, Contents);
--      Set_Name (Temp_Zip_Stream, Name_In_Archive);
      Set_Time (Temp_Zip_Stream, Creation_Time);
      Set_Unicode_Name_Flag (Temp_Zip_Stream, Name_Utf_8_Encoded);
      Add_Stream (Info, Temp_Zip_Stream);
      raise Program_Error with "Why change name to " & Name_In_Archive & "?";
   end Add_String;

   procedure Add_Compressed_Stream
     (Info     : in out Zip_Create_Info;            --  Destination
      Stream   : in out Root_Zipstream_Type'Class;  --  Source
      Feedback : in     Feedback_Proc)
   is
      Lh                         : Zip.Headers.Local_File_Header;
      Data_Descriptor_After_Data : Boolean;
   begin
      Zip.Headers.Read_And_Check (Stream, Lh);
      Data_Descriptor_After_Data := (Lh.Bit_Flag and 8) /= 0;
      --  Copy name and ignore extra field
      declare
         Name  : String (1 .. Positive (Lh.Filename_Length));
         Extra : String (1 .. Natural (Lh.Extra_Field_Length));
      begin
         String'Read (Stream'Access, Name);
         String'Read (Stream'Access, Extra);
         if Info.Duplicates = Error_On_Duplicate then
            --  Check for duplicates; raises Duplicate_name in this case:
            Insert_To_Name_Dictionary (Name, Info.Dir);
         end if;
         Add_Catalogue_Entry (Info);
         Info.Contains (Info.Last_Entry).Head.Local_Header_Offset :=
           Unsigned_32 (Index (Info.Stream.all)) - 1;
         Info.Contains (Info.Last_Entry).Name := new String'(Name);
         Lh.Extra_Field_Length := 0;  --  Extra field is zeroed (causes problems if not)
         Zip.Headers.Write (Info.Stream.all, Lh);  --  Copy local header to new stream
         String'Write (Info.Stream, Name);         --  Copy entry name to new stream
      end;
      Zip.Copy_Chunk
        (Stream,
         Info.Stream.all,
         Integer (Lh.Dd.Compressed_Size),
         Feedback => Feedback);
      --  Postfixed data descriptor contains the correct values for
      --  CRC and sizes. Example of Zip files using that descriptor: those
      --  created by Microsoft's OneDrive cloud storage (for downloading
      --  more than one file), in 2018.
      if Data_Descriptor_After_Data then
         --  NB: some faulty JAR files may fail with Read_and_check.
         --  See UnZip.Decompress, Process_descriptor.
         Zip.Headers.Read_And_Check (Stream, Lh.Dd);
         --  lh's values have been corrected on the way.
         Zip.Headers.Write (Info.Stream.all, Lh.Dd);  --  Copy descriptor to new stream.
      end if;
      Info.Contains (Info.Last_Entry).Head.Short_Info := Lh;
   end Add_Compressed_Stream;

   procedure Finish (Info : in out Zip_Create_Info) is
      Ed : Zip.Headers.End_Of_Central_Dir;
      procedure Dispose is new Ada.Unchecked_Deallocation (String, P_String);
      Current_Index : Zs_Index_Type;
      procedure Get_Index_And_Check_Zip_32_Limit is
      begin
         Current_Index := Index (Info.Stream.all);
         if Info.Zip_Archive_Format = Zip_32 and then Current_Index > Four_Gb then
            raise Zip_Capacity_Exceeded with Zip_32_Exceeded_Message;
         end if;
      end Get_Index_And_Check_Zip_32_Limit;
   begin
      --  2/ Almost done - write Central Directory
      Get_Index_And_Check_Zip_32_Limit;
      Ed.Central_Dir_Offset  := Unsigned_32 (Current_Index) - 1;
      Ed.Total_Entries       := 0;
      Ed.Central_Dir_Size    := 0;
      Ed.Main_Comment_Length := 0;
      if Info.Zip_Archive_Format = Zip_32
        and then Info.Last_Entry > Integer (Unsigned_16'Last)
      then
         raise Zip_Capacity_Exceeded with
           "Too many entries (for Zip_32 archive format): more than 65535.";
      end if;
      if Info.Contains /= null then
         for E in 1 .. Info.Last_Entry loop
            Ed.Total_Entries := Ed.Total_Entries + 1;
            Zip.Headers.Write (Info.Stream.all, Info.Contains (E).Head);
            String'Write (Info.Stream, Info.Contains (E).Name.all);
            --  The extra field here is assumed to be empty!
            Ed.Central_Dir_Size :=
              Ed.Central_Dir_Size +
              Zip.Headers.Central_Header_Length +
              Unsigned_32 (Info.Contains (E).Head.Short_Info.Filename_Length);
            Dispose (Info.Contains (E).Name);
            Get_Index_And_Check_Zip_32_Limit;
         end loop;
         Dispose (Info.Contains);
      end if;
      Info.Last_Entry       := 0;
      Ed.Disknum            := 0;
      Ed.Disknum_With_Start := 0;
      Ed.Disk_Total_Entries := Ed.Total_Entries;
      Zip.Headers.Write (Info.Stream.all, Ed);
      Get_Index_And_Check_Zip_32_Limit;
      --  If we have a real file (File_Zipstream or descendent),
      --  close the file stream too. File will be closed automatically
      --  when stream goes out of scope
   end Finish;

end Zip.Create;
