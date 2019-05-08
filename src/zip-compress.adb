-- Legal licensing note:

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

-- NB: this is the MIT License, as found on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip.CRC_Crypto,
     Zip.Compress.Deflate;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

package body Zip.Compress is

  use Zip_Streams, Zip.CRC_Crypto;

  -------------------
  -- Compress_data --
  -------------------

  procedure Compress_data
   (input,
    output          : in out Zip_Streams.Root_Zipstream_Type'Class;
    input_size_known: Boolean;
    input_size      : File_size_type;
    method          : Compression_Method;
    feedback        : Feedback_proc;
    content_hint    : Data_content_type;
    CRC             : out Interfaces.Unsigned_32;
    output_size     : out File_size_type;
    zip_type        : out Interfaces.Unsigned_16
   )
  is
    use Interfaces;
    counted: File_size_type;
    user_aborting: Boolean;
    idx_in:  constant ZS_Index_Type:= Index(input);
    idx_out: constant ZS_Index_Type:= Index(output);
    compression_ok: Boolean;
    first_feedback: Boolean:= True;
    --
    encrypt_pack: Crypto_pack;
    package Byte_soup is new Ada.Numerics.Discrete_Random(Byte);
    use Byte_soup;
    cg: Byte_soup.Generator;
    --
    --  Store data as is, or, if do_write = False, just compute CRC (this is for encryption).
    --
    procedure Store_data(do_write: Boolean) is
      Buffer      : Byte_Buffer (1 .. buffer_size);
      Last_Read   : Natural;
    begin
      zip_type:= compression_format_code.store;
      counted:= 0;
      while not End_Of_Stream(input) loop
        if input_size_known and counted >= input_size then
          exit;
        end if;
        -- Copy data
        BlockRead (input, Buffer, Last_Read);
        counted:= counted + File_size_type (Last_Read);
        Update(CRC, Buffer (1 .. Last_Read));
        if do_write then
          Encode(encrypt_pack, Buffer (1 .. Last_Read));
          BlockWrite(output, Buffer (1 .. Last_Read));
        end if;
        -- Feedback
        if feedback /= null and then
          (first_feedback or (counted mod (2**16)=0) or
          (input_size_known and counted = input_size))
        then
          if input_size_known then
            feedback(
              percents_done =>
                Natural( (100.0 * Float(counted)) / Float(input_size) ),
              entry_skipped => False,
              user_abort    => user_aborting );
          else
            feedback(
              percents_done => 0,
              entry_skipped => False,
              user_abort    => user_aborting );
          end if;
          first_feedback:= False;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end loop;
      output_size:= counted;
      compression_ok:= True;
    end Store_data;
    --
    procedure Compress_data_single_method(actual_method: Compression_Method) is
    begin
      Init(CRC);
      Set_mode(encrypt_pack, clear);
      --
      --  Dispatch the work to child procedures doing the stream compression
      --  in different formats, depending on the actual compression method.
      --  For example, for methods LZMA_for_JPEG, LZMA_for_WAV, or LZMA_3, we
      --  logically call Zip.Compress.LZMA_E for the job.
      --
      case actual_method is
        when Store =>
          Store_data(do_write => True);
        when Deflation_Method =>
          Zip.Compress.Deflate(
            input, output, input_size_known, input_size, feedback,
            actual_method,
            CRC, encrypt_pack, output_size, compression_ok
          );
          zip_type:= compression_format_code.deflate;
      end case;
      CRC:= Final(CRC);
      --
      -- Handle case where compression has been unefficient:
      -- data to be compressed is too "random"; then compressed data
      -- happen to be larger than uncompressed data
      --
      if not compression_ok then
        -- Go back to the beginning and just store the data
        Set_Index(input, idx_in);
        Set_Index(output, idx_out);
        Init(CRC);
        Store_data(do_write => True);
        CRC:= Final(CRC);
      end if;
    end Compress_data_single_method;
  begin
    Compress_data_single_method(method);
  end Compress_data;

  function Guess_type_from_name(name: String) return Data_content_type is
    up: constant String:= To_Upper(name);
    ext_1: constant String:= Tail(up, 2);
    ext_2: constant String:= Tail(up, 3);
    ext_3: constant String:= Tail(up, 4);
    ext_4: constant String:= Tail(up, 5);
  begin
    if ext_3 = ".JPG" or else ext_4 = ".JPEG" then
      return JPEG;
    end if;
    if ext_3 = ".ADA" or else ext_3 = ".ADS" or else ext_3 = ".ADB"
      or else ext_1 = ".C" or else ext_1 = ".H"
      or else ext_3 = ".CPP" or else ext_3 = ".HPP"
      or else ext_3 = ".DEF" or else ext_3 = ".ASM"
      or else ext_4 = ".JAVA" or else ext_2 = ".CS"
      or else ext_3 = ".PAS" or else ext_3 = ".INC" or else ext_2 = ".PP" or else ext_3 = ".LPR"
      or else ext_3 = ".MAK" or else ext_2 = ".IN"
      or else ext_2 = ".SH" or else ext_3 = ".BAT" or else ext_3 = ".CMD"
      or else ext_3 = ".XML" or else ext_3 = ".XSL" or else ext_4 = ".SGML"
      or else ext_3 = ".HTM" or else ext_4 = ".HTML"
      or else ext_2 = ".JS" or else ext_3 = ".LSP"
      or else ext_3 = ".CSV" or else ext_3 = ".SQL"
    then
      return Source_code;
    end if;
    --  Zip archives happen to be zipped...
    if ext_4 = ".EPUB"  --  EPUB: e-book reader format
      or else ext_3 = ".JAR" or else ext_3 = ".ZIP"
      or else ext_3 = ".ODB" or else ext_3 = ".ODS" or else ext_3 = ".ODT"
      or else ext_3 = ".OTR" or else ext_3 = ".OTS" or else ext_3 = ".OTT"
      or else ext_3 = ".CRX" or else ext_3 = ".NTH"
      or else ext_4 = ".DOCX" or else ext_4 = ".PPTX" or else ext_4 = ".XLSX"
    then
      return Zip_in_Zip;
    end if;
    --  Some raw camera picture data
    if ext_3 = ".ORF"          --  Raw Olympus
      or else ext_3 = ".CR2"   --  Raw Canon
      or else ext_3 = ".RAF"   --  Raw Fujifilm
      or else ext_3 = ".SRW"   --  Raw Samsung
    then
      return ORF_CR2;
    end if;
    if ext_3 = ".ARW"          --  Raw Sony
      or else ext_3 = ".RW2"   --  Raw Panasonic
      or else ext_3 = ".NEF"   --  Raw Nikon
      or else ext_3 = ".DNG"   --  Raw Leica, Pentax
      or else ext_3 = ".X3F"   --  Raw Sigma
    then
      return ARW_RW2;
    end if;
    if ext_3 = ".PGM" then
      return PGM;
    end if;
    if ext_3 = ".PPM" then
      return PPM;
    end if;
    if ext_3 = ".MP3" then
      return MP3;
    end if;
    if ext_3 = ".MTS" or else ext_3 = ".MP4" or else ext_3 = ".M4A" or else ext_3 = ".M4P" then
      return MP4;
    end if;
    if ext_3 = ".PNG" then
      return PNG;
    end if;
    if ext_3 = ".GIF" then
      return GIF;
    end if;
    if ext_3 = ".WAV" or else ext_3 = ".UAX" then
      return WAV;
    end if;
    return Neutral;
  end Guess_type_from_name;

end Zip.Compress;
