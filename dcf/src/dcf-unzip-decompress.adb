--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2007 - 2018 Gautier de Montmollin
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

with DCF.Unzip.Decompress.Huffman;
with DCF.Zip.CRC;

package body DCF.Unzip.Decompress is

   procedure Decompress_Data
     (Zip_File                   : in out DCF.Streams.Root_Zipstream_Type'Class;
      Format                     :        Pkzip_Method;
      Output_Stream_Access       :        P_Stream;                   -- \ = write_to_stream
      Data_Descriptor_After_Data :        Boolean;
      Hint                       : in out Zip.Headers.Local_File_Header;
      Verify_Integrity           :        Boolean)
   is
      --  I/O Buffers: Size of input buffer
      Inbuf_Size : constant := 16#8000#;  -- (orig: 16#1000# B =  4 KB)
      --  I/O Buffers: Size of sliding dictionary and output buffer
      Wsize : constant := 16#10000#; -- (orig: 16#8000# B = 32 KB)

      CRC_Value : Unsigned_32;
      --  CRC calculated from data

      Compressed_Size : constant Unzip.File_Size_Type :=
        Unzip.File_Size_Type'Min (Hint.Dd.Compressed_Size, File_Size_Type'Last - 2);
      --  Compressed size of file

      ----------------------------------------------------------------------------
      -- Specifications of UnZ_* packages (remain of Info Zip's code structure) --
      ----------------------------------------------------------------------------

      package Unz_Glob is -- Not global anymore, since local to Decompress_data :-)
         --  I/O Buffers: Sliding dictionary for unzipping, and output buffer as well
         Slide       : Zip.Byte_Buffer (0 .. Wsize);
         Slide_Index : Integer := 0; -- Current Position in slide
         --  I/O Buffers: Input buffer
         Inbuf          : Zip.Byte_Buffer (0 .. Inbuf_Size - 1);
         Inpos, Readpos : Integer;  -- pos. in input buffer, pos. read from file
         Reachedsize : Unzip.File_Size_Type;  --  Number of bytes read from zipfile
      end Unz_Glob;

      Zip_Eof   : Boolean; -- read over end of zip section for this file

      package Unz_Io is
         procedure Init_Buffers;

         function Read_Byte_Decrypted return Unsigned_8; -- NB: reading goes on a while even if
         pragma Inline (Read_Byte_Decrypted);           -- Zip_EOF is set: just gives garbage

         package Bit_Buffer is
            procedure Init;
            --  Read at least n bits into the bit buffer, returns the n first bits
            function Read (N : Natural) return Integer;
            pragma Inline (Read);
            function Read_U32 (N : Natural) return Unsigned_32;
            pragma Inline (Read_U32);
            --  Dump n bits no longer needed from the bit buffer
            procedure Dump (N : Natural);
            pragma Inline (Dump);
            procedure Dump_To_Byte_Boundary;
            function Read_And_Dump (N : Natural) return Integer;
            pragma Inline (Read_And_Dump);
            function Read_And_Dump_U32 (N : Natural) return Unsigned_32;
            pragma Inline (Read_And_Dump_U32);
         end Bit_Buffer;

         procedure Flush (X : Natural); -- directly from slide to output stream

         procedure Flush_If_Full (W : in out Integer);
         pragma Inline (Flush_If_Full);

         procedure Copy (Distance, Length : Natural; Index : in out Natural);
         pragma Inline (Copy);
      end Unz_Io;

      package Unz_Meth is
         procedure Copy_Stored (Size : Ada.Streams.Stream_Element_Offset);
         procedure Inflate;
      end Unz_Meth;

      ------------------------------
      -- Bodies of UnZ_* packages --
      ------------------------------
      package body Unz_Io is

         procedure Init_Buffers is
         begin
            Unz_Glob.Inpos            := 0;  -- Input buffer position
            Unz_Glob.Readpos          := -1;  -- Nothing read
            Unz_Glob.Slide_Index      := 0;
            Unz_Glob.Reachedsize      := 0;
            Zip_Eof                   := False;
            Zip.CRC.Init (CRC_Value);
            Bit_Buffer.Init;
         end Init_Buffers;

         procedure Process_Compressed_End_Reached is
         begin
            if Zip_Eof then -- We came already here once
               raise Zip.Archive_Corrupted with
                 "Decoding went past compressed data size plus one buffer length";
            --  Avoid infinite loop on data with exactly buffer's length and no end marker
            else
               Unz_Glob.Readpos := Unz_Glob.Inbuf'Length;
               --  Simulates reading -> no blocking.
               --  The buffer is full of "random" data and we hope for a wrong code or a CRC error
               Zip_Eof := True;
            end if;
         end Process_Compressed_End_Reached;

         procedure Read_Buffer is
         begin
            if Unz_Glob.Reachedsize > Compressed_Size + 2 then
               --  +2: last code is smaller than requested!
               Process_Compressed_End_Reached;
            else
               begin
                  Zip.Blockread
                    (Stream        => Zip_File,
                     Buffer        => Unz_Glob.Inbuf,
                     Actually_Read => Unz_Glob.Readpos);
               exception
                  when others => -- I/O error
                     Process_Compressed_End_Reached;
               end;
               if Unz_Glob.Readpos = 0 then -- No byte at all was read
                  Process_Compressed_End_Reached;
               end if;
               Unz_Glob.Reachedsize :=
                 Unz_Glob.Reachedsize + Unzip.File_Size_Type (Unz_Glob.Readpos);
               Unz_Glob.Readpos := Unz_Glob.Readpos - 1; -- Reason: index of inbuf starts at 0
            end if;
            Unz_Glob.Inpos := 0;
         end Read_Buffer;

         function Read_Byte_Decrypted return Unsigned_8 is
            Bt : Zip.Byte;
         begin
            if Unz_Glob.Inpos > Unz_Glob.Readpos then
               Read_Buffer;
            end if;
            Bt             := Unz_Glob.Inbuf (Unz_Glob.Inpos);
            Unz_Glob.Inpos := Unz_Glob.Inpos + 1;
            return Bt;
         end Read_Byte_Decrypted;

         package body Bit_Buffer is
            B : Unsigned_32;
            K : Integer;

            procedure Init is
            begin
               B := 0;
               K := 0;
            end Init;

            procedure Need (N : Natural) is
               pragma Inline (Need);
            begin
               while K < N loop
                  B := B or Shift_Left (Unsigned_32 (Read_Byte_Decrypted), K);
                  K := K + 8;
               end loop;
            end Need;

            procedure Dump (N : Natural) is
            begin
               B := Shift_Right (B, N);
               K := K - N;
            end Dump;

            procedure Dump_To_Byte_Boundary is
            begin
               Dump (K mod 8);
            end Dump_To_Byte_Boundary;

            function Read_U32 (N : Natural) return Unsigned_32 is
            begin
               Need (N);
               return B and (Shift_Left (1, N) - 1);
            end Read_U32;

            function Read (N : Natural) return Integer is
            begin
               return Integer (Read_U32 (N));
            end Read;

            function Read_And_Dump (N : Natural) return Integer is
               Res : Integer;
            begin
               Res := Read (N);
               Dump (N);
               return Res;
            end Read_And_Dump;

            function Read_And_Dump_U32 (N : Natural) return Unsigned_32 is
               Res : Unsigned_32;
            begin
               Res := Read_U32 (N);
               Dump (N);
               return Res;
            end Read_And_Dump_U32;

         end Bit_Buffer;

         procedure Flush (X : Natural) is
            use Zip;
            use Ada.Streams;
         begin
            begin
               Blockwrite (Output_Stream_Access.all, Unz_Glob.Slide (0 .. X - 1));
            exception
               when others =>
                  raise Unzip.Write_Error;
            end;
            Zip.CRC.Update (CRC_Value, Unz_Glob.Slide (0 .. X - 1));
         end Flush;

         procedure Flush_If_Full (W : in out Integer) is
         begin
            if W = Wsize then
               Flush (Wsize);
               W := 0;
            end if;
         end Flush_If_Full;

         ----------------------------------------------------
         -- Reproduction of sequences in the output slide. --
         ----------------------------------------------------

         --  Internal:

         procedure Adjust_To_Slide
           (Source : in out Integer;
            Remain : in out Natural;
            Part   :    out Integer;
            Index  :        Integer)
         is
            pragma Inline (Adjust_To_Slide);
         begin
            Source := Source mod Wsize;
            --  Source and Index are now in 0..WSize-1
            if Source > Index then
               Part := Wsize - Source;
            else
               Part := Wsize - Index;
            end if;
            --  NB: part is in 1..WSize (part cannot be 0)
            if Part > Remain then
               Part := Remain;
            end if;
            --  Now part <= remain
            Remain := Remain - Part;
            --  NB: remain cannot be < 0
         end Adjust_To_Slide;

         procedure Copy_Range (Source, Index : in out Natural; Amount : Positive) is
            pragma Inline (Copy_Range);
         begin
            if abs (Index - Source) < Amount then
               --  if source >= index, the effect of copy is just like the non-overlapping case
               for Count in reverse 1 .. Amount loop
                  Unz_Glob.Slide (Index) := Unz_Glob.Slide (Source);
                  Index                  := Index + 1;
                  Source                 := Source + 1;
               end loop;
            else -- non-overlapping -> copy slice
               Unz_Glob.Slide (Index .. Index + Amount - 1) :=
                 Unz_Glob.Slide (Source .. Source + Amount - 1);
               Index  := Index + Amount;
               Source := Source + Amount;
            end if;
         end Copy_Range;

         --  The copying routines:

         procedure Copy (Distance, Length : Natural; Index : in out Natural) is
            Source, Part, Remain : Integer;
         begin
            Source := Index - Distance;
            Remain := Length;
            loop
               Adjust_To_Slide (Source, Remain, Part, Index);
               Copy_Range (Source, Index, Part);
               Flush_If_Full (Index);
               exit when Remain = 0;
            end loop;
         end Copy;

      end Unz_Io;

      package body Unz_Meth is

         --------[ Method: Copy stored ]--------

         procedure Copy_Stored (Size : Ada.Streams.Stream_Element_Offset) is
            use Ada.Streams;

            Buffer_Size : constant := 64 * 1024;  -- 64 KiB
            Read : Stream_Element_Offset := 0;
         begin
            while Read < Size loop
               declare
                  Remaining : constant Stream_Element_Offset
                    := Stream_Element_Offset'Min (Size - Read, Buffer_Size);

                  Buffer : Stream_Element_Array (1 .. Remaining);
                  Last   : Stream_Element_Offset;
               begin
                  Zip_File.Read (Item => Buffer, Last => Last);
                  Read := Read + Last;

                  if Last /= Remaining then
                     raise Zip.Archive_Corrupted with
                       "Expected to read " & Remaining'Image & " bytes";
                  end if;

                  begin
                     Output_Stream_Access.all.Write (Buffer);
                  exception
                     when others =>
                        raise Unzip.Write_Error;
                  end;
                  if Verify_Integrity then
                     Zip.CRC.Update_Stream_Array (CRC_Value, Buffer);
                  end if;
               end;
            end loop;
         end Copy_Stored;

         --------[ Method: Inflate ]--------

         use Unzip.Decompress.Huffman;

         procedure Inflate_Codes (Tl, Td : P_Table_List; Bl, Bd : Integer) is
            Ct      : P_Huft_Table;  --  Current table
            Ct_Idx  : Natural;       --  Current table's index
            Length  : Natural;
            E       : Integer;       -- Table entry flag/number of extra bits
            W       : Integer := Unz_Glob.Slide_Index;  --  More local variable for slide index
            Literal : Zip.Byte;
         begin
            --  Inflate the coded data
            Main_Loop :
            while not Zip_Eof loop
               if Tl = null then
                  raise Zip.Archive_Corrupted with
                    "Null table list (on data decoding, Huffman tree for literals or LZ lengths)";
               end if;
               Ct     := Tl.Table;
               Ct_Idx := Unz_Io.Bit_Buffer.Read (Bl);
               loop
                  E := Ct (Ct_Idx).Extra_Bits;
                  exit when E <= 16;
                  if E = Invalid then
                     raise Zip.Archive_Corrupted;
                  end if;

                  --  Then it's a literal
                  Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);
                  E      := E - 16;
                  Ct     := Ct (Ct_Idx).Next_Table;
                  Ct_Idx := Unz_Io.Bit_Buffer.Read (E);
               end loop;

               Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);

               case E is
                  when 16 =>     -- CT(CT_idx).N is a Literal (code 0 .. 255)
                     Literal := Zip.Byte (Ct (Ct_Idx).N);
                     Unz_Glob.Slide (W) := Literal;
                     W                  := W + 1;
                     Unz_Io.Flush_If_Full (W);

                  when 15 =>     -- End of block (EOB, code 256)
                     exit Main_Loop;

                  when others => -- We have a length/distance code
                     --  Get length of block to copy:
                     Length := Ct (Ct_Idx).N + Unz_Io.Bit_Buffer.Read_And_Dump (E);

                     --  Decode distance of block to copy:
                     if Td = null then
                        raise Zip.Archive_Corrupted with
                          "Null table list (on data decoding, Huffman tree for LZ distances)";
                     end if;
                     Ct     := Td.Table;
                     Ct_Idx := Unz_Io.Bit_Buffer.Read (Bd);
                     loop
                        E := Ct (Ct_Idx).Extra_Bits;
                        exit when E <= 16;
                        if E = Invalid then
                           raise Zip.Archive_Corrupted;
                        end if;
                        Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);
                        E      := E - 16;
                        Ct     := Ct (Ct_Idx).Next_Table;
                        Ct_Idx := Unz_Io.Bit_Buffer.Read (E);
                     end loop;
                     Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);
                     Unz_Io.Copy
                       (Distance => Ct (Ct_Idx).N + Unz_Io.Bit_Buffer.Read_And_Dump (E),
                        Length   => Length,
                        Index    => W);
               end case;
            end loop Main_Loop;

            Unz_Glob.Slide_Index := W;
         end Inflate_Codes;

         procedure Inflate_Stored_Block is -- Actually, nothing to inflate
            N : Integer;
         begin
            Unz_Io.Bit_Buffer.Dump_To_Byte_Boundary;
            --  Get the block length and its complement
            N := Unz_Io.Bit_Buffer.Read_And_Dump (16);
            if N /= Integer ((not Unz_Io.Bit_Buffer.Read_And_Dump_U32 (16)) and 16#ffff#) then
               raise Zip.Archive_Corrupted;
            end if;
            while N > 0 and then not Zip_Eof loop
               --  Read and output the non-compressed data
               N                                     := N - 1;
               Unz_Glob.Slide (Unz_Glob.Slide_Index) :=
                 Zip.Byte (Unz_Io.Bit_Buffer.Read_And_Dump (8));
               Unz_Glob.Slide_Index := Unz_Glob.Slide_Index + 1;
               Unz_Io.Flush_If_Full (Unz_Glob.Slide_Index);
            end loop;
         end Inflate_Stored_Block;

         --  Copy lengths for literal codes 257..285

         Copy_Lengths_Literal : constant Length_Array (0 .. 30) :=
           (3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            13,
            15,
            17,
            19,
            23,
            27,
            31,
            35,
            43,
            51,
            59,
            67,
            83,
            99,
            115,
            131,
            163,
            195,
            227,
            258,
            0,
            0);

         --  Extra bits for literal codes 257..285

         Extra_Bits_Literal : constant Length_Array (0 .. 30) :=
           (0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            1,
            1,
            1,
            1,
            2,
            2,
            2,
            2,
            3,
            3,
            3,
            3,
            4,
            4,
            4,
            4,
            5,
            5,
            5,
            5,
            0,
            Invalid,
            Invalid);

         --  Copy offsets for distance codes 0..29 (30..31: deflate_e)

         Copy_Offset_Distance : constant Length_Array (0 .. 31) :=
           (1,
            2,
            3,
            4,
            5,
            7,
            9,
            13,
            17,
            25,
            33,
            49,
            65,
            97,
            129,
            193,
            257,
            385,
            513,
            769,
            1025,
            1537,
            2049,
            3073,
            4097,
            6145,
            8193,
            12289,
            16385,
            24577,
            32769,
            49153);

         --  Extra bits for distance codes

         Extra_Bits_Distance : constant Length_Array (0 .. 31) :=
           (0,
            0,
            0,
            0,
            1,
            1,
            2,
            2,
            3,
            3,
            4,
            4,
            5,
            5,
            6,
            6,
            7,
            7,
            8,
            8,
            9,
            9,
            10,
            10,
            11,
            11,
            12,
            12,
            13,
            13,
            14,
            14);

         Max_Dist : constant Integer := 29; -- changed to 31 for deflate_e

         Length_List_For_Fixed_Block_Literals : constant Length_Array (0 .. 287) :=
           (0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8);

         procedure Inflate_Fixed_Block is
            Tl,                        --   literal/length code table
            Td              : P_Table_List;            --  distance code table
            Bl, Bd          : Integer;          --  lookup bits for tl/bd
            Huft_Incomplete : Boolean;
         begin
            --  Make a complete, but wrong [why ?] code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
            Bl := 7;
            Huft_Build
              (Length_List_For_Fixed_Block_Literals,
               257,
               Copy_Lengths_Literal,
               Extra_Bits_Literal,
               Tl,
               Bl,
               Huft_Incomplete);
            --  Make an incomplete code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
            Bd := 5;
            begin
               Huft_Build
                 ((0 .. Max_Dist => 5),
                  0,
                  Copy_Offset_Distance,
                  Extra_Bits_Distance,
                  Td,
                  Bd,
                  Huft_Incomplete);
            exception
               when Huft_Out_Of_Memory | Huft_Error =>
                  Huft_Free (Tl);
                  raise Zip.Archive_Corrupted;
            end;
            --  Decompress the block's data, until an end-of-block code.
            Inflate_Codes (Tl, Td, Bl, Bd);
            --  Done with this block, free resources.
            Huft_Free (Tl);
            Huft_Free (Td);
         end Inflate_Fixed_Block;

         Bit_Order_For_Dynamic_Block : constant array (0 .. 18) of Natural :=
           (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

         procedure Inflate_Dynamic_Block is

            Lbits : constant := 9;
            Dbits : constant := 6;

            Current_Length             : Natural;
            Defined, Number_Of_Lengths : Natural;

            Tl : P_Table_List;            -- literal/length code tables
            Td : P_Table_List;            -- distance code tables

            Ct     : P_Huft_Table;       -- current table
            Ct_Idx : Natural;            -- current table's index

            Bl, Bd : Integer;                  -- lookup bits for tl/bd
            Nb     : Natural;  -- number of bit length codes
            Nl     : Natural;  -- number of literal length codes
            Nd     : Natural;  -- number of distance codes

            --  literal/length and distance code lengths
            Ll : Length_Array (0 .. 288 + 32 - 1) := (others => 0);

            Huft_Incomplete : Boolean;

            procedure Repeat_Length_Code (Amount : Natural) is
            begin
               if Defined + Amount > Number_Of_Lengths then
                  raise Zip.Archive_Corrupted;
               end if;
               for C in reverse 1 .. Amount loop
                  Ll (Defined) := Current_Length;
                  Defined      := Defined + 1;
               end loop;
            end Repeat_Length_Code;

         begin
            --  Read in table lengths
            Nl := 257 + Unz_Io.Bit_Buffer.Read_And_Dump (5);
            Nd := 1 + Unz_Io.Bit_Buffer.Read_And_Dump (5);
            Nb := 4 + Unz_Io.Bit_Buffer.Read_And_Dump (4);

            if Nl > 288 or else Nd > 32 then
               raise Zip.Archive_Corrupted;
            end if;

            --  Read in bit-length-code lengths for decoding the compression structure.
            --  The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
            for J in 0 .. Nb - 1 loop
               Ll (Bit_Order_For_Dynamic_Block (J)) := Unz_Io.Bit_Buffer.Read_And_Dump (3);
            end loop;

            --  Build decoding table for trees--single level, 7 bit lookup
            Bl := 7;
            begin
               Huft_Build (Ll (0 .. 18), 19, Empty, Empty, Tl, Bl, Huft_Incomplete);
               if Huft_Incomplete then
                  Huft_Free (Tl);
                  raise Zip.Archive_Corrupted with
                    "Incomplete code set for compression structure";
               end if;
            exception
               when Zip.Archive_Corrupted =>
                  raise;
               when others =>
                  raise Zip.Archive_Corrupted with
                    "Error when building tables for compression structure";
            end;

            --  Read in the compression structure: literal and distance code lengths
            Number_Of_Lengths := Nl + Nd;
            Defined           := 0;
            Current_Length    := 0;

            while Defined < Number_Of_Lengths loop
               if Tl = null then
                  raise Zip.Archive_Corrupted with
                    "Null table list (on compression structure)";
               end if;
               Ct     := Tl.Table;
               Ct_Idx := Unz_Io.Bit_Buffer.Read (Bl);
               Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);

               case Ct (Ct_Idx).N is
                  when 0 .. 15 =>
                     --  Length of code for symbol of index 'defined', in bits (0..15)
                     Current_Length := Ct (Ct_Idx).N;
                     Ll (Defined)   := Current_Length;
                     Defined        := Defined + 1;
                  when 16 =>
                     --  16 means: repeat last bit length 3 to 6 times
                     if Defined = 0 then
                        --  Nothing in the Ll array has been defined so far.
                        --  Then, current_length is (theoretically) undefined
                        --  and cannot be repeated. This unspecified case is
                        --  treated as an error by zlib's inflate.c.
                        raise Zip.Archive_Corrupted with
                          "Illegal data for compression structure" &
                          " (repeat an undefined code length)";
                     end if;
                     Repeat_Length_Code (3 + Unz_Io.Bit_Buffer.Read_And_Dump (2));
                  when 17 =>
                     --  17 means: the next 3 to 10 symbols' codes have zero bit lengths
                     Current_Length := 0;
                     Repeat_Length_Code (3 + Unz_Io.Bit_Buffer.Read_And_Dump (3));
                  when 18 =>
                     --  18 means: the next 11 to 138 symbols' codes have zero bit lengths
                     Current_Length := 0;
                     Repeat_Length_Code (11 + Unz_Io.Bit_Buffer.Read_And_Dump (7));
                  when others =>
                     --  Shouldn't occur if this tree is correct
                     raise Zip.Archive_Corrupted with
                       "Illegal data for compression structure" &
                       " (values should be in the range 0 .. 18): " &
                       Integer'Image (Ct (Ct_Idx).N);
               end case;
            end loop;
            --  Free the Huffman tree that was used for decoding the compression
            --  structure, which is contained now in Ll.
            Huft_Free (Tl);
            if Ll (256) = 0 then
               --  No code length for the End-Of-Block symbol, implies infinite stream!
               --  This case is unspecified but obviously we must stop here.
               raise Zip.Archive_Corrupted with
                 "No code for End-Of-Block symbol #256";
            end if;
            --  Build the decoding tables for literal/length codes
            Bl := Lbits;
            begin
               Huft_Build
                 (Ll (0 .. Nl - 1),
                  257,
                  Copy_Lengths_Literal,
                  Extra_Bits_Literal,
                  Tl,
                  Bl,
                  Huft_Incomplete);
               if Huft_Incomplete then
                  Huft_Free (Tl);
                  raise Zip.Archive_Corrupted with
                    "Incomplete code set for literals/lengths";
               end if;
            exception
               when Zip.Archive_Corrupted =>
                  raise;
               when others =>
                  raise Zip.Archive_Corrupted with
                    "Error when building tables for literals/lengths";
            end;
            --  Build the decoding tables for distance codes
            Bd := Dbits;
            begin
               Huft_Build
                 (Ll (Nl .. Nl + Nd - 1),
                  0,
                  Copy_Offset_Distance,
                  Extra_Bits_Distance,
                  Td,
                  Bd,
                  Huft_Incomplete);
               if Huft_Incomplete then
                  --  Incomplete Huffman code set for decoding LZ distances
                  raise Zip.Archive_Corrupted with
                    "Incomplete code set for distances";
               end if;
            exception
               when Huft_Out_Of_Memory | Huft_Error =>
                  Huft_Free (Tl);
                  raise Zip.Archive_Corrupted with
                    "Error when building tables for distances";
            end;
            --  Decompress the block's data, until an end-of-block code.
            Inflate_Codes (Tl, Td, Bl, Bd);
            --  Done with this block, free resources.
            Huft_Free (Tl);
            Huft_Free (Td);
         end Inflate_Dynamic_Block;

         procedure Inflate_Block (Last_Block : out Boolean; Fix, Dyn : in out Long_Integer) is
         begin
            Last_Block := Boolean'Val (Unz_Io.Bit_Buffer.Read_And_Dump (1));
            case Unz_Io.Bit_Buffer.Read_And_Dump (2) is -- Block type = 0,1,2,3
               when 0 =>
                  Inflate_Stored_Block;
               when 1 =>
                  Inflate_Fixed_Block;
                  Fix := Fix + 1;
               when 2 =>
                  Inflate_Dynamic_Block;
                  Dyn := Dyn + 1;
               when others =>
                  raise Zip.Archive_Corrupted;  --  Bad block type (3)
            end case;
         end Inflate_Block;

         procedure Inflate is
            Is_Last_Block                  : Boolean;
            Blocks, Blocks_Fix, Blocks_Dyn : Long_Integer := 0;
         begin
            loop
               Blocks := Blocks + 1;
               Inflate_Block (Is_Last_Block, Blocks_Fix, Blocks_Dyn);
               exit when Is_Last_Block;
            end loop;
            Unz_Io.Flush (Unz_Glob.Slide_Index);
            Unz_Glob.Slide_Index := 0;
         end Inflate;
      end Unz_Meth;

      procedure Process_Descriptor_Store (Descriptor : out Zip.Headers.Data_Descriptor) is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 16);
      begin
         Zip.Blockread (Zip_File, Buffer);
         Zip.Headers.Copy_And_Check (Buffer, Descriptor);
      end Process_Descriptor_Store;

      procedure Process_Descriptor_Deflate (Descriptor : out Zip.Headers.Data_Descriptor) is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 16);
      begin
         Unz_Io.Bit_Buffer.Dump_To_Byte_Boundary;
         for I in Buffer'Range loop
            Buffer (I) := Ada.Streams.Stream_Element (Unz_Io.Read_Byte_Decrypted);
         end loop;
         Zip.Headers.Copy_And_Check (Buffer, Descriptor);
      end Process_Descriptor_Deflate;

      use Zip;
   begin
      Unz_Io.Init_Buffers;

      --  Unzip correct type
      case Format is
         when Store =>
            Unz_Meth.Copy_Stored (Ada.Streams.Stream_Element_Offset (Compressed_Size));
         when Deflate =>
            Unz_Meth.Inflate;
      end case;

      if Verify_Integrity then
         CRC_Value := Zip.CRC.Final (CRC_Value);
      end if;

      if Data_Descriptor_After_Data then -- Sizes and CRC at the end
         declare
            Memo_Uncomp_Size : constant Unsigned_32 := Hint.Dd.Uncompressed_Size;
         begin
            case Format is
               when Store =>
                  Process_Descriptor_Store (Hint.Dd);
               when Deflate =>
                  Process_Descriptor_Deflate (Hint.Dd);
            end case;

            --  CRC is for checking; sizes are for informing user
            if Memo_Uncomp_Size < Unsigned_32'Last
              and then Memo_Uncomp_Size /= Hint.Dd.Uncompressed_Size
            then
               raise Uncompressed_Size_Error;
            end if;
         exception
            when Zip.Headers.Bad_Data_Descriptor =>
               raise Zip.Archive_Corrupted;
         end;
      end if;

      if Verify_Integrity and then Hint.Dd.Crc_32 /= CRC_Value then
         raise CRC_Error with
           "CRC stored in archive: " &
           Zip.CRC.Image (Hint.Dd.Crc_32) &
           "; CRC computed now: " &
           Zip.CRC.Image (CRC_Value);
      end if;
   end Decompress_Data;

end DCF.Unzip.Decompress;
