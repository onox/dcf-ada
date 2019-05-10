--  Legal licensing note:

--  Copyright (c) 2007 .. 2018 Gautier de Montmollin (Maintainer of the Ada version)
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

with Zip.Crc_Crypto, Zip.Compress.Deflate;

with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

package body Zip.Compress is

   use Zip_Streams, Zip.Crc_Crypto;

   ---------------------
   --  Compress_data  --
   ---------------------

   procedure Compress_Data
     (Input, Output    : in out Zip_Streams.Root_Zipstream_Type'Class;
      Input_Size_Known :        Boolean;
      Input_Size       :        File_Size_Type;
      Method           :        Compression_Method;
      Feedback         :        Feedback_Proc;
      Content_Hint     :        Data_Content_Type;
      Crc              :    out Interfaces.Unsigned_32;
      Output_Size      :    out File_Size_Type;
      Zip_Type         :    out Interfaces.Unsigned_16)
   is
      use Interfaces;
      Counted        : File_Size_Type;
      User_Aborting  : Boolean;
      Idx_In         : constant Zs_Index_Type := Index (Input);
      Idx_Out        : constant Zs_Index_Type := Index (Output);
      Compression_Ok : Boolean;
      First_Feedback : Boolean                := True;
      --
      Encrypt_Pack : Crypto_Pack;
      package Byte_Soup is new Ada.Numerics.Discrete_Random (Byte);
      use Byte_Soup;
      Cg : Byte_Soup.Generator;
      --  Store data as is, or, if do_write = False, just compute
      --  CRC (this is for encryption)
      procedure Store_Data (Do_Write : Boolean) is
         Buffer    : Byte_Buffer (1 .. Buffer_Size);
         Last_Read : Natural;
      begin
         Zip_Type := Compression_Format_Code.Store;
         Counted  := 0;
         while not End_Of_Stream (Input) loop
            if Input_Size_Known and Counted >= Input_Size then
               exit;
            end if;
            --  Copy data
            Blockread (Input, Buffer, Last_Read);
            Counted := Counted + File_Size_Type (Last_Read);
            Update (Crc, Buffer (1 .. Last_Read));
            if Do_Write then
               Encode (Encrypt_Pack, Buffer (1 .. Last_Read));
               Blockwrite (Output, Buffer (1 .. Last_Read));
            end if;
            --  Feedback
            if Feedback /= null
              and then
              (First_Feedback or
               (Counted mod (2**16) = 0) or
               (Input_Size_Known and Counted = Input_Size))
            then
               if Input_Size_Known then
                  Feedback
                    (Percents_Done => Natural ((100.0 * Float (Counted)) / Float (Input_Size)),
                     Entry_Skipped => False,
                     User_Abort    => User_Aborting);
               else
                  Feedback
                    (Percents_Done => 0,
                     Entry_Skipped => False,
                     User_Abort    => User_Aborting);
               end if;
               First_Feedback := False;
               if User_Aborting then
                  raise User_Abort;
               end if;
            end if;
         end loop;
         Output_Size    := Counted;
         Compression_Ok := True;
      end Store_Data;
      --
      procedure Compress_Data_Single_Method (Actual_Method : Compression_Method) is
      begin
         Init (Crc);
         Set_Mode (Encrypt_Pack, Clear);
         --  Dispatch the work to child procedures doing the stream compression
         --  in different formats, depending on the actual compression method.
         --  For example, for methods LZMA_for_JPEG, LZMA_for_WAV, or LZMA_3, we
         --  logically call Zip.Compress.LZMA_E for the job.
         case Actual_Method is
            when Store =>
               Store_Data (Do_Write => True);
            when Deflation_Method =>
               Zip.Compress.Deflate
                 (Input,
                  Output,
                  Input_Size_Known,
                  Input_Size,
                  Feedback,
                  Actual_Method,
                  Crc,
                  Encrypt_Pack,
                  Output_Size,
                  Compression_Ok);
               Zip_Type := Compression_Format_Code.Deflate;
         end case;
         Crc := Final (Crc);
         --  Handle case where compression has been unefficient:
         --  data to be compressed is too "random"; then compressed data
         --  happen to be larger than uncompressed data
         if not Compression_Ok then
            --  Go back to the beginning and just store the data
            Set_Index (Input, Idx_In);
            Set_Index (Output, Idx_Out);
            Init (Crc);
            Store_Data (Do_Write => True);
            Crc := Final (Crc);
         end if;
      end Compress_Data_Single_Method;
   begin
      Compress_Data_Single_Method (Method);
   end Compress_Data;

   function Guess_Type_From_Name (Name : String) return Data_Content_Type is
      Up    : constant String := To_Upper (Name);
      Ext_1 : constant String := Tail (Up, 2);
      Ext_2 : constant String := Tail (Up, 3);
      Ext_3 : constant String := Tail (Up, 4);
      Ext_4 : constant String := Tail (Up, 5);
   begin
      if Ext_3 = ".JPG" or else Ext_4 = ".JPEG" then
         return Jpeg;
      end if;
      if Ext_3 = ".ADA"
        or else Ext_3 = ".ADS"
        or else Ext_3 = ".ADB"
        or else Ext_1 = ".C"
        or else Ext_1 = ".H"
        or else Ext_3 = ".CPP"
        or else Ext_3 = ".HPP"
        or else Ext_3 = ".DEF"
        or else Ext_3 = ".ASM"
        or else Ext_4 = ".JAVA"
        or else Ext_2 = ".CS"
        or else Ext_3 = ".PAS"
        or else Ext_3 = ".INC"
        or else Ext_2 = ".PP"
        or else Ext_3 = ".LPR"
        or else Ext_3 = ".MAK"
        or else Ext_2 = ".IN"
        or else Ext_2 = ".SH"
        or else Ext_3 = ".BAT"
        or else Ext_3 = ".CMD"
        or else Ext_3 = ".XML"
        or else Ext_3 = ".XSL"
        or else Ext_4 = ".SGML"
        or else Ext_3 = ".HTM"
        or else Ext_4 = ".HTML"
        or else Ext_2 = ".JS"
        or else Ext_3 = ".LSP"
        or else Ext_3 = ".CSV"
        or else Ext_3 = ".SQL"
      then
         return Source_Code;
      end if;
      --  Zip archives happen to be zipped...
      if Ext_4 = ".EPUB"  --  EPUB: e-book reader format
        or else Ext_3 = ".JAR"
        or else Ext_3 = ".ZIP"
        or else Ext_3 = ".ODB"
        or else Ext_3 = ".ODS"
        or else Ext_3 = ".ODT"
        or else Ext_3 = ".OTR"
        or else Ext_3 = ".OTS"
        or else Ext_3 = ".OTT"
        or else Ext_3 = ".CRX"
        or else Ext_3 = ".NTH"
        or else Ext_4 = ".DOCX"
        or else Ext_4 = ".PPTX"
        or else Ext_4 = ".XLSX"
      then
         return Zip_In_Zip;
      end if;
      --  Some raw camera picture data
      if Ext_3 = ".ORF"          --  Raw Olympus
        or else Ext_3 = ".CR2"   --  Raw Canon
        or else Ext_3 = ".RAF"   --  Raw Fujifilm
        or else Ext_3 = ".SRW"   --  Raw Samsung
      then
         return Orf_Cr2;
      end if;
      if Ext_3 = ".ARW"          --  Raw Sony
        or else Ext_3 = ".RW2"   --  Raw Panasonic
        or else Ext_3 = ".NEF"   --  Raw Nikon
        or else Ext_3 = ".DNG"   --  Raw Leica, Pentax
        or else Ext_3 = ".X3F"   --  Raw Sigma
      then
         return Arw_Rw2;
      end if;
      if Ext_3 = ".PGM" then
         return Pgm;
      end if;
      if Ext_3 = ".PPM" then
         return Ppm;
      end if;
      if Ext_3 = ".MP3" then
         return Mp3;
      end if;
      if Ext_3 = ".MTS" or else Ext_3 = ".MP4" or else Ext_3 = ".M4A" or else Ext_3 = ".M4P" then
         return Mp4;
      end if;
      if Ext_3 = ".PNG" then
         return Png;
      end if;
      if Ext_3 = ".GIF" then
         return Gif;
      end if;
      if Ext_3 = ".WAV" or else Ext_3 = ".UAX" then
         return Wav;
      end if;
      return Neutral;
   end Guess_Type_From_Name;

end Zip.Compress;
