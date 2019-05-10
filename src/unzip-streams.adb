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

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip.Headers, Unzip.Decompress;

with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;
use Interfaces;

package body Unzip.Streams is

   procedure Dispose is new Ada.Unchecked_Deallocation (String, P_String);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Ada.Streams.Stream_Element_Array,
      P_Stream_Element_Array);

   procedure Dispose is new Ada.Unchecked_Deallocation (Unzip_Stream_Type, Zipped_File_Type);

   ----------------------------------------------------
   --  *The* internal 1-file unzipping procedure.    --
   --  Input must be _open_ and won't be _closed_ !  --
   ----------------------------------------------------

   procedure Unzipfile
     (Zip_Stream     : in out Zip_Streams.Root_Zipstream_Type'Class;
      Header_Index   : in out Zip_Streams.Zs_Index_Type;
      Mem_Ptr        :    out P_Stream_Element_Array;
      Out_Stream_Ptr :        P_Stream;
      --  If not null, extract to out_stream_ptr, not to memory
      Hint_Comp_Size  : in File_Size_Type;  --  Added 2007 for .ODS files
      Hint_Crc_32     : in Unsigned_32;     --  Added 2012 for decryption
      Cat_Uncomp_Size : in File_Size_Type)
   is
      Work_Index                 : Zip_Streams.Zs_Index_Type := Header_Index;
      Local_Header               : Zip.Headers.Local_File_Header;
      Data_Descriptor_After_Data : Boolean;
      Encrypted                  : Boolean;
      Method                     : Pkzip_Method;
      use Zip;
      Mode : Write_Mode;
   begin
      begin
         Zip_Streams.Set_Index (Zip_Stream, Header_Index);
         Zip.Headers.Read_And_Check (Zip_Stream, Local_Header);
      exception
         when Zip.Headers.Bad_Local_Header =>
            Raise_Exception (Zip.Archive_Corrupted'Identity, "Bad local header");
         when others =>
            raise Zip.Archive_Corrupted;
      end;

      Method := Method_From_Code (Local_Header.Zip_Type);
      if Method = Unknown then
         raise Unsupported_Method;
      end if;

      --  Calculate offset of data
      Work_Index :=
        Work_Index +
        Zip_Streams.Zs_Size_Type
          (Local_Header.Filename_Length +
           Local_Header.Extra_Field_Length +
           Zip.Headers.Local_Header_Length);

      Data_Descriptor_After_Data := (Local_Header.Bit_Flag and 8) /= 0;

      if Data_Descriptor_After_Data then
         --  Sizes and crc are after the data
         Local_Header.Dd.Crc_32            := Hint_Crc_32;
         Local_Header.Dd.Uncompressed_Size := Cat_Uncomp_Size;
         Local_Header.Dd.Compressed_Size   := Hint_Comp_Size;
      else
         --  Sizes and crc are before the data
         if Cat_Uncomp_Size /= Local_Header.Dd.Uncompressed_Size then
            raise Uncompressed_Size_Error;
         end if;
      end if;

      Encrypted := (Local_Header.Bit_Flag and Zip.Headers.Encryption_Flag_Bit) /= 0;
      pragma Assert (not Encrypted);

      begin
         Zip_Streams.Set_Index (Zip_Stream, Work_Index); -- eventually skips the file name
      exception
         when others =>
            Raise_Exception
              (Zip.Archive_Corrupted'Identity,
               "End of stream reached (location: between local header and archived data)");
      end;

      if Out_Stream_Ptr = null then
         Mode := Write_To_Memory;
      else
         Mode := Write_To_Stream;
      end if;
      --  Unzip correct type
      Unzip.Decompress.Decompress_Data
        (Zip_File                   => Zip_Stream,
         Format                     => Method,
         Mode                       => Mode,
         Output_File_Name           => "",
         Output_Memory_Access       => Mem_Ptr,
         Output_Stream_Access       => Out_Stream_Ptr,
         Feedback                   => null,
         Explode_Literal_Tree       => (Local_Header.Bit_Flag and 4) /= 0,
         Explode_Slide_8kb_Lzma_Eos =>
           (Local_Header.Bit_Flag and Zip.Headers.Lzma_Eos_Flag_Bit) /= 0,
         Data_Descriptor_After_Data => Data_Descriptor_After_Data,
         Hint                       => Local_Header);

      --  Set the offset on the next zipped file
      Header_Index :=
        Header_Index +
        Zip_Streams.Zs_Size_Type
          (Local_Header.Filename_Length +
           Local_Header.Extra_Field_Length +
           Zip.Headers.Local_Header_Length) +
        Zip_Streams.Zs_Size_Type (Local_Header.Dd.Compressed_Size);

      if Data_Descriptor_After_Data then
         Header_Index :=
           Header_Index + Zip_Streams.Zs_Size_Type (Zip.Headers.Data_Descriptor_Length);
      end if;

   end Unzipfile;

   procedure S_Extract
     (From             :        Zip.Zip_Info;
      Zip_Stream       : in out Zip_Streams.Root_Zipstream_Type'Class;
      What             :        String;
      Mem_Ptr          :    out P_Stream_Element_Array;
      Out_Stream_Ptr   :        P_Stream;
      Ignore_Directory : in     Boolean)
   is
      Header_Index        : Zip_Streams.Zs_Index_Type;
      Comp_Size           : File_Size_Type;
      Uncomp_Size         : File_Size_Type;
      Crc_32              : Interfaces.Unsigned_32;
      Dummy_Name_Encoding : Zip.Zip_Name_Encoding;

   begin
      if Ignore_Directory then
         Zip.Find_Offset_Without_Directory
           (Info          => From,
            Name          => What,
            Name_Encoding => Dummy_Name_Encoding,
            File_Index    => Header_Index,
            Comp_Size     => Comp_Size,
            Uncomp_Size   => Uncomp_Size,
            Crc_32        => Crc_32);
      else
         Zip.Find_Offset
           (Info          => From,
            Name          => What,
            Name_Encoding => Dummy_Name_Encoding,
            File_Index    => Header_Index,
            Comp_Size     => Comp_Size,
            Uncomp_Size   => Uncomp_Size,
            Crc_32        => Crc_32);
      end if;
      Unzipfile
        (Zip_Stream      => Zip_Stream,
         Header_Index    => Header_Index,
         Mem_Ptr         => Mem_Ptr,
         Out_Stream_Ptr  => Out_Stream_Ptr,
         Hint_Comp_Size  => Comp_Size,
         Hint_Crc_32     => Crc_32,
         Cat_Uncomp_Size => Uncomp_Size);
   end S_Extract;

   -------------------- for exportation:

   procedure Close (File : in out Zipped_File_Type) is
   begin
      if File = null or else File.State = Uninitialized then
         raise Use_Error;
      end if;
      Dispose (File.File_Name);
      Dispose (File.Uncompressed);
      Dispose (File);
      File := null;
   end Close;

   function Name (File : in Zipped_File_Type) return String is
   begin
      return File.File_Name.all;
   end Name;

   function Is_Open (File : in Zipped_File_Type) return Boolean is
   begin
      return File /= null and then File.State /= Uninitialized;
   end Is_Open;

   function End_Of_File (File : in Zipped_File_Type) return Boolean is
   begin
      if File = null or else File.State = Uninitialized then
         raise Use_Error;
      end if;
      return File.State = End_Of_Zip;
   end End_Of_File;

   procedure Open
     (File             : in out Zipped_File_Type;  --  File-in-archive handle
      Archive_Info     : in     Zip.Zip_Info;      --  Archive's Zip_info
      Name             : in     String;            --  Name of zipped entry
      Ignore_Directory : in Boolean := False)      --  Open Name in first directory found
   is
      use Zip_Streams, Ada.Streams;
      Zip_Stream   : aliased File_Zipstream;
      Input_Stream : Zipstream_Class_Access;
      Use_A_File   : constant Boolean := Zip.Zip_Stream (Archive_Info) = null;
   begin
      if File = null then
         File := new Unzip_Stream_Type;
      elsif File.State /= Uninitialized then
         --  Forgot to close last time!
         raise Use_Error;
      end if;
      if Use_A_File then
         Input_Stream := Zip_Stream'Unchecked_Access;
         Set_Name (Zip_Stream, Zip.Zip_Name (Archive_Info));
         Open (Zip_Stream, In_File);
      else
         --  Use the given stream
         Input_Stream := Zip.Zip_Stream (Archive_Info);
      end if;

      File.Archive_Info := Archive_Info;  --  Full clone. Now a copy is safely with File
      File.File_Name    := new String'(Name);
      begin
         S_Extract
           (File.Archive_Info,
            Input_Stream.all,
            Name,
            File.Uncompressed,
            null,
            Ignore_Directory);
         if Use_A_File then
            Close (Zip_Stream);
         end if;
      exception
         when others =>
            if Use_A_File then
               Close (Zip_Stream);
            end if;
            raise;
      end;
      File.Index := File.Uncompressed'First;
      File.State := Data_Uncompressed;

      --  Bug fix for data of size 0 - 29-Nov-2002
      if File.Uncompressed'Last < File.Index then  --  (1..0) array
         File.State := End_Of_Zip;
      end if;
   end Open;

   procedure Open
     (File             : in out Zipped_File_Type;  --  File-in-archive handle
      Archive_Name     : in     String;            --  Name of archive file
      Name             : in     String;            --  Name of zipped entry
      Case_Sensitive   : in     Boolean := False;
      Ignore_Directory : in Boolean := False)      --  Open Name in first directory found
   is
      Temp_Info : Zip.Zip_Info;
      --  This local record (but not the full tree) is copied by Open (..)
   begin
      Zip.Load (Temp_Info, Archive_Name, Case_Sensitive);
      Open (File, Temp_Info, Name);
   end Open;

   procedure Open
     (File             : in out Zipped_File_Type;                       --  File-in-archive handle
      Archive_Stream   : in out Zip_Streams.Root_Zipstream_Type'Class;  --  Archive's stream
      Name             : in     String;            --  Name of zipped entry
      Case_Sensitive   : in     Boolean := False;
      Ignore_Directory : in Boolean := False)      --  Open Name in first directory found
   is
      Temp_Info : Zip.Zip_Info;
      --  This local record (but not the full tree) is copied by Open (..)
   begin
      Zip.Load (Temp_Info, Archive_Stream, Case_Sensitive);
      Open (File, Temp_Info, Name);
   end Open;

   --------------------------------------------
   --  Read procedure for Unzip_Stream_Type  --
   --------------------------------------------

   overriding
   procedure Read
     (Stream : in out Unzip_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
   begin
      if Stream.State = Uninitialized then
         raise Use_Error;
      end if;
      if Stream.State = End_Of_Zip then
         --  Zero transfer -> Last:= Item'First - 1, see RM 13.13.1(8)
         --  No End_Error here, T'Read will raise it: RM 13.13.2(37)
         if Item'First > Stream_Element_Offset'First then
            Last := Item'First - 1;
            return;
         else
            --  Well, we cannot return Item'First - 1...
            raise Constraint_Error; -- RM 13.13.1(11) requires this.
         end if;
      end if;
      if Item'Length = 0 then
         --  Nothing to be read actually
         Last := Item'Last;  --  This is < Item'First
         return;
      end if;
      --  From now on, we can assume Item'Length > 0

      if Stream.Index + Item'Length <= Stream.Uncompressed'Last then
         --  * Normal case: even after reading, the index will be in the range
         Last         := Item'Last;
         Item         := Stream.Uncompressed (Stream.Index .. Stream.Index + Item'Length - 1);
         Stream.Index := Stream.Index + Item'Length;
         --  Now: Stream.index <= Stream.uncompressed'Last,
         --  then at least one element is left to be read, end_of_zip not possible
      else
         --  * Special case: we exhaust the buffer
         Last                      := Item'First + (Stream.Uncompressed'Last - Stream.Index);
         Item (Item'First .. Last) :=
           Stream.Uncompressed (Stream.Index .. Stream.Uncompressed'Last);
         Stream.State := End_Of_Zip;
         --  If Last < Item'Last, the T'Read attribute raises End_Error
         --  because of the incomplete reading
      end if;
   end Read;

   function Stream (File : Zipped_File_Type) return Stream_Access is
   begin
      return Stream_Access (File);
   end Stream;

   function Size (File : in Zipped_File_Type) return Count is
      Comp_Size   : File_Size_Type;
      Uncomp_Size : File_Size_Type;
   begin
      Zip.Get_Sizes (File.Archive_Info, File.File_Name.all, Comp_Size, Uncomp_Size);
      return Count (Uncomp_Size);
   end Size;

   overriding
   procedure Write
     (Stream : in out Unzip_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array)
   is
      Write_Not_Supported : exception;
   begin
      raise Write_Not_Supported;
   end Write;

   procedure Extract
     (Destination      : in out Ada.Streams.Root_Stream_Type'Class;
      Archive_Info     : in     Zip.Zip_Info;  --  Archive's Zip_info
      Name             : in     String;        --  Name of zipped entry
      Ignore_Directory : in Boolean := False)  --  Open Name in first directory found
   is
      use Zip_Streams;
      Zip_Stream   : aliased File_Zipstream;
      Input_Stream : Zipstream_Class_Access;
      Use_A_File   : constant Boolean := Zip.Zip_Stream (Archive_Info) = null;
   begin
      if Use_A_File then
         Input_Stream := Zip_Stream'Unchecked_Access;
         Set_Name (Zip_Stream, Zip.Zip_Name (Archive_Info));
         Open (Zip_Stream, In_File);
      else
         --  Use the given stream
         Input_Stream := Zip.Zip_Stream (Archive_Info);
      end if;
      declare
         Dummy_Mem_Ptr : P_Stream_Element_Array;
      begin
         S_Extract
           (Archive_Info,
            Input_Stream.all,
            Name,
            Dummy_Mem_Ptr,
            Destination'Unchecked_Access,
            Ignore_Directory);
         if Use_A_File then
            Close (Zip_Stream);
         end if;
      exception
         when others =>
            if Use_A_File then
               Close (Zip_Stream);
            end if;
            raise;
      end;
   end Extract;

end Unzip.Streams;
