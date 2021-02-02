--  SPDX-License-Identifier: MIT
--
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

with Ada.IO_Exceptions;

with DCF.Unzip.Decompress;
with DCF.Zip.Headers;

package body DCF.Unzip.Streams is

   overriding procedure Write
     (Stream : in out Stream_Writer;
      Item   : in     Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Target /= null then
         Stream.Target.all.Set_Index (Stream.Index);
         Stream.Target.all.Write (Item);
         Stream.Index := Stream.Index + Item'Length;
      end if;
   end Write;

   procedure Unzip_File
     (Zip_Stream       : in out DCF.Streams.Root_Zipstream_Type'Class;
      Header_Index     : in     DCF.Streams.Zs_Index_Type;
      Out_Stream_Ptr   :        P_Stream;
      Hint_Comp_Size   : in File_Size_Type;  --  Added 2007 for .ODS files
      Hint_Crc_32      : in Unsigned_32;     --  Added 2012 for decryption
      Cat_Uncomp_Size  : in File_Size_Type;
      Verify_Integrity : in Boolean)
   is
      Work_Index                 : DCF.Streams.Zs_Index_Type := Header_Index;
      Local_Header               : Zip.Headers.Local_File_Header;
      Data_Descriptor_After_Data : Boolean;
      Encrypted                  : Boolean;
      Method                     : Pkzip_Method;
      use Zip;
   begin
      begin
         DCF.Streams.Set_Index (Zip_Stream, Header_Index);
         Zip.Headers.Read_And_Check (Zip_Stream, Local_Header);
      exception
         when Zip.Headers.Bad_Local_Header =>
            raise Zip.Archive_Corrupted with "Bad local header";
         when others =>
            raise Zip.Archive_Corrupted;
      end;

      Method := Method_From_Code (Local_Header.Zip_Type);

      --  Calculate offset of data
      Work_Index :=
        Work_Index +
        DCF.Streams.Zs_Size_Type
          (Local_Header.Filename_Length +
           Local_Header.Extra_Field_Length +
           Zip.Headers.Local_Header_Length);

      Data_Descriptor_After_Data :=
        (Local_Header.Bit_Flag and Zip.Headers.Descriptor_Flag_Bit) /= 0;

      if Data_Descriptor_After_Data then
         --  Sizes and CRC are stored after the data
         --  We set size to avoid getting a sudden Zip_EOF !
         if Method = Store and then Hint_Comp_Size = File_Size_Type'Last then
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
         Local_Header.Dd.Uncompressed_Size := Cat_Uncomp_Size;
      else
         --  Sizes and CRC are before the data
         if Cat_Uncomp_Size /= Local_Header.Dd.Uncompressed_Size then
            raise Uncompressed_Size_Error;
         end if;
      end if;

      Encrypted := (Local_Header.Bit_Flag and Zip.Headers.Encryption_Flag_Bit) /= 0;
      if Encrypted then
         raise Constraint_Error with "Encryption is not supported";
      end if;

      begin
         DCF.Streams.Set_Index (Zip_Stream, Work_Index); -- eventually skips the file name
      exception
         when others =>
            raise Zip.Archive_Corrupted with
              "End of stream reached (location: between local header and archived data)";
      end;

      --  Unzip correct type
      Unzip.Decompress.Decompress_Data
        (Zip_File                   => Zip_Stream,
         Format                     => Method,
         Output_Stream_Access       => Out_Stream_Ptr,
         Data_Descriptor_After_Data => Data_Descriptor_After_Data,
         Hint                       => Local_Header,
         Verify_Integrity           => Verify_Integrity);
   exception
      when Ada.IO_Exceptions.End_Error =>
         raise Zip.Archive_Corrupted with "End of stream reached";
   end Unzip_File;

   procedure Extract
     (Destination      : in out Ada.Streams.Root_Stream_Type'Class;
      Archive_Info     : in     Zip.Zip_Info;  --  Archive's Zip_info
      File             : in     Zip.Archived_File;
      Verify_Integrity : in     Boolean) is
   begin
      Unzip_File
        (Zip_Stream       => Archive_Info.Stream.all,
         Header_Index     => File.File_Index,
         Out_Stream_Ptr   => Destination'Unchecked_Access,
         Hint_Comp_Size   => File.Compressed_Size,
         Hint_Crc_32      => File.CRC_32,
         Cat_Uncomp_Size  => File.Uncompressed_Size,
         Verify_Integrity => Verify_Integrity);
   end Extract;

end DCF.Unzip.Streams;
