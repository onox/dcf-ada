--  SPDX-License-Identifier: MIT
--
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

   use DCF.Zip.CRC;

   procedure Compress_Data
     (Input, Output    : in out DCF.Streams.Root_Zipstream_Type'Class;
      Input_Size       :        File_Size_Type;
      Method           :        Compression_Method;
      Feedback         :        Feedback_Proc;
      CRC              :    out Unsigned_32;
      Output_Size      :    out File_Size_Type;
      Zip_Type         :    out Unsigned_16)
   is
      Index_In  : constant DCF.Streams.Zs_Index_Type := Input.Index;
      Index_Out : constant DCF.Streams.Zs_Index_Type := Output.Index;

      Compression_OK : Boolean := False;

      procedure Store_Data is
         use type Ada.Streams.Stream_Element_Offset;

         function Percentage (Left, Right : Ada.Streams.Stream_Element_Count) return Natural is
           (Natural ((100.0 * Float (Left)) / Float (Right)));

         Buffer    : Ada.Streams.Stream_Element_Array (1 .. Default_Buffer_Size);
         Last_Read : Ada.Streams.Stream_Element_Offset;

         Counted   : Ada.Streams.Stream_Element_Count := 0;
         Size      : constant Ada.Streams.Stream_Element_Count :=
           Ada.Streams.Stream_Element_Count (Input_Size);

         User_Aborting  : Boolean := False;
      begin
         while not Input.End_Of_Stream loop
            if Feedback /= null then
               Feedback (Percentage (Counted, Size), User_Aborting);
               if User_Aborting then
                  raise User_Abort;
               end if;
            end if;

            if Counted >= Size then
               exit;
            end if;

            Input.Read (Buffer, Last_Read);
            Counted := Counted + Last_Read;
            Output.Write (Buffer (1 .. Last_Read));

            Update_Stream_Array (CRC, Buffer (1 .. Last_Read));
         end loop;

         Output_Size    := File_Size_Type (Counted);
         Zip_Type       := Compression_Format_Code.Store;
         Compression_OK := True;
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
                  Compression_OK);
               Zip_Type := Compression_Format_Code.Deflate;
         end case;

         CRC := Final (CRC);
      end Compress_Data_Single_Method;
   begin
      Compress_Data_Single_Method (Method);

      --  Handle case where compression has been unefficient:
      --  data to be compressed is too "random"; then compressed data
      --  happen to be larger than uncompressed data
      if not Compression_OK then
         --  Go back to the beginning and just store the data
         Input.Set_Index (Index_In);
         Output.Set_Index (Index_Out);
         Compress_Data_Single_Method (Store);
      end if;
   end Compress_Data;

end DCF.Zip.Compress;
