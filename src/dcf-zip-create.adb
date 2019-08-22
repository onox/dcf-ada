--  SPDX-License-Identifier: MIT
--
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

with Ada.Unchecked_Deallocation;

package body DCF.Zip.Create is

   Four_GB : constant := 4 * 1024 ** 3;

   Zip_32_Exceeded_Message : constant String :=
     "Zip file larger than 4 GB limit of Zip_32 archive format";

   procedure Create
     (Info       : in out   Zip_Create_Info;
      Stream     : not null Zipstream_Class_Access;
      Compress   :          Zip.Compress.Compression_Method := Zip.Compress.Deflate_1) is
   begin
      Info.Stream   := Stream;
      Info.Compress := Compress;
   end Create;

   function Is_Created (Info : Zip_Create_Info) return Boolean is
     (Info.Stream /= null);

   procedure Set (Info : in out Zip_Create_Info; New_Method : Zip.Compress.Compression_Method) is
   begin
      Info.Compress := New_Method;
   end Set;

   procedure Set_Comment (Info : in out Zip_Create_Info; Comment : String) is
   begin
      Info.Comment := SU.To_Unbounded_String (Comment);
   end Set_Comment;

   function Name (Info : Zip_Create_Info) return String is
     (Get_Name (Info.Stream.all));

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
         Cfh.Made_By_Version                   := 20;  --  Version 2.0
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
      M.Insert (SU.To_Unbounded_String (File_Name), Cm, Ok);
      if not Ok then
         --  Name already registered
         raise Duplicate_Name with "Entry name '" & File_Name & "' already in archive";
      end if;
   end Insert_To_Name_Dictionary;

   procedure Add_Stream
     (Info            : in out Zip_Create_Info;
      Stream          : in out Root_Zipstream_Type'Class;
      Feedback        : in     Feedback_Proc;
      Compressed_Size :    out Zip.File_Size_Type;
      Final_Method    :    out Natural)
   is
      function To_File_Name (Path : String) return String is
         Name : String := Path;
      begin
         --  Appnote: 4.4.17.1, slashes must be forward slashes '/' instead of '\'
         for C of Name loop
            if C = '\' then
               C := '/';
            end if;
         end loop;

         return Name;
      end To_File_Name;

      Mem1, Mem2 : Zs_Index_Type := Zs_Index_Type'First;
      Last       : Positive;

      Entry_Name : constant String  := To_File_Name (Get_Name (Stream));
      Is_Folder  : constant Boolean := Entry_Name (Entry_Name'Last) = '/';
   begin
      if Is_Folder and then Stream.Size > 0 then
         raise Constraint_Error;
      end if;

      --  Check for duplicates; raises Duplicate_name in this case
      Insert_To_Name_Dictionary (Entry_Name, Info.Dir);

      Add_Catalogue_Entry (Info);
      Last := Info.Last_Entry;

      declare
         Cfh : Central_File_Header renames Info.Contains (Last).Head;
         Shi : Local_File_Header renames Cfh.Short_Info;
      begin
         pragma Assert (Zip.Headers.Encryption_Flag_Bit not in Shi.Bit_Flag);
         if Stream.UTF_8_Encoding then
            Shi.Bit_Flag := Shi.Bit_Flag or Zip.Headers.Language_Encoding_Flag_Bit;
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
            Input_Size       => Shi.Dd.Uncompressed_Size,
            Method           => (if Is_Folder then Zip.Compress.Store else Info.Compress),
            Feedback         => Feedback,
            CRC              => Shi.Dd.Crc_32,
            Output_Size      => Shi.Dd.Compressed_Size,
            Zip_Type         => Shi.Zip_Type);

         Mem2 := Index (Info.Stream.all);
         if Info.Zip_Archive_Format = Zip_32 and then Mem2 > Four_GB then
            raise Zip_Capacity_Exceeded with Zip_32_Exceeded_Message;
         end if;

         --  Go back to the local header to rewrite it with complete informations
         --  known after the compression: CRC value, compressed size, actual compression format.
         Set_Index (Info.Stream.all, Mem1);
         Zip.Headers.Write (Info.Stream.all, Shi);

         --  Return to momentaneous end of file
         Set_Index (Info.Stream.all, Mem2);

         Compressed_Size := Shi.Dd.Compressed_Size;
         Final_Method    := Natural (Shi.Zip_Type);
      end;
   end Add_Stream;

   procedure Add_Stream
     (Info   : in out Zip_Create_Info;
      Stream : in out Root_Zipstream_Type'Class)
   is
      Compressed_Size : Zip.File_Size_Type;  --  Dummy
      Final_Method    : Natural;             --  Dummy
   begin
      Add_Stream (Info, Stream, null, Compressed_Size, Final_Method);
   end Add_Stream;

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

         --  Check for duplicates; raises Duplicate_name in this case:
         Insert_To_Name_Dictionary (Name, Info.Dir);

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
         Ada.Streams.Stream_Element_Count (Lh.Dd.Compressed_Size),
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
      procedure Dispose is new Ada.Unchecked_Deallocation (String, P_String);

      Ed : Zip.Headers.End_Of_Central_Dir;
      Current_Index : Zs_Index_Type;

      procedure Get_Index_And_Check_Zip_32_Limit is
      begin
         Current_Index := Index (Info.Stream.all);
         if Info.Zip_Archive_Format = Zip_32 and then Current_Index > Four_GB then
            raise Zip_Capacity_Exceeded with Zip_32_Exceeded_Message;
         end if;
      end Get_Index_And_Check_Zip_32_Limit;
   begin
      --  2/ Almost done - write Central Directory
      Get_Index_And_Check_Zip_32_Limit;
      Ed.Central_Dir_Offset  := Unsigned_32 (Current_Index) - 1;
      Ed.Total_Entries       := 0;
      Ed.Central_Dir_Size    := 0;
      Ed.Main_Comment_Length := Unsigned_16 (SU.Length (Info.Comment));

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

      if Ed.Main_Comment_Length > 0 then
         String'Write (Info.Stream, SU.To_String (Info.Comment));
      end if;

      Get_Index_And_Check_Zip_32_Limit;

      --  File will be closed automatically when stream goes out of scope
      Info.Stream := null;
   end Finish;

end DCF.Zip.Create;
