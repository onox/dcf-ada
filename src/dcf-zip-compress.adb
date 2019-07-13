--  Copyright (c) 2007 - 2018 Gautier de Montmollin (Maintainer of the Ada version)
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

with DCF.Zip.Compress.Deflate;
with DCF.Zip.CRC;

package body DCF.Zip.Compress is

   use DCF.Streams;
   use DCF.Zip.CRC;

   ---------------------
   --  Compress_data  --
   ---------------------

   procedure Compress_Data
     (Input, Output    : in out DCF.Streams.Root_Zipstream_Type'Class;
      Input_Size       :        File_Size_Type;
      Method           :        Compression_Method;
      Feedback         :        Feedback_Proc;
      CRC              :    out Unsigned_32;
      Output_Size      :    out File_Size_Type;
      Zip_Type         :    out Unsigned_16)
   is
      Counted        : File_Size_Type;
      User_Aborting  : Boolean;
      Idx_In         : constant Zs_Index_Type := Index (Input);
      Idx_Out        : constant Zs_Index_Type := Index (Output);
      Compression_Ok : Boolean := False;
      First_Feedback : Boolean := True;

      procedure Store_Data is
         Buffer    : Byte_Buffer (1 .. Default_Buffer_Size);
         Last_Read : Natural;
      begin
         Zip_Type := Compression_Format_Code.Store;
         Counted  := 0;
         while not End_Of_Stream (Input) loop
            if Counted >= Input_Size then
               exit;
            end if;

            --  Copy data
            Blockread (Input, Buffer, Last_Read);
            Counted := Counted + File_Size_Type (Last_Read);
            Update (CRC, Buffer (1 .. Last_Read));
            Blockwrite (Output, Buffer (1 .. Last_Read));

            --  Feedback
            if Feedback /= null
              and then
              (First_Feedback or
               (Counted mod (2**16) = 0) or
               (Counted = Input_Size))
            then
               Feedback
                 (Percents_Done => Natural ((100.0 * Float (Counted)) / Float (Input_Size)),
                  Entry_Skipped => False,
                  User_Abort    => User_Aborting);
               First_Feedback := False;
               if User_Aborting then
                  raise User_Abort;
               end if;
            end if;
         end loop;
         Output_Size    := Counted;
         Compression_Ok := True;
      end Store_Data;

      procedure Compress_Data_Single_Method (Actual_Method : Compression_Method) is
      begin
         Init (CRC);

         case Actual_Method is
            when Store =>
               Store_Data;
            when Deflation_Method =>
               Zip.Compress.Deflate
                 (Input,
                  Output,
                  Input_Size,
                  Feedback,
                  Actual_Method,
                  CRC,
                  Output_Size,
                  Compression_Ok);
               Zip_Type := Compression_Format_Code.Deflate;
         end case;

         CRC := Final (CRC);
      end Compress_Data_Single_Method;
   begin
      Compress_Data_Single_Method (Method);

      --  Handle case where compression has been unefficient:
      --  data to be compressed is too "random"; then compressed data
      --  happen to be larger than uncompressed data
      if not Compression_Ok then
         --  Go back to the beginning and just store the data
         Set_Index (Input, Idx_In);
         Set_Index (Output, Idx_Out);
         Compress_Data_Single_Method (Store);
      end if;
   end Compress_Data;

end DCF.Zip.Compress;
