--  Legal licensing note:

--  Copyright (c) 2007 .. 2018 Gautier de Montmollin
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

with Interfaces;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Zip.Crc_Crypto;
with Unzip.Decompress.Huffman;

package body Unzip.Decompress is

   procedure Decompress_Data
     (Zip_File                   : in out Zip_Streams.Root_Zipstream_Type'Class;
      Format                     :        Pkzip_Method;
      Mode                       :        Write_Mode;
      Output_File_Name           :        String; -- relevant only if mode = write_to_file
      Output_Memory_Access       :    out P_Stream_Element_Array; -- \ = write_to_memory
      Output_Stream_Access       :        P_Stream;                   -- \ = write_to_stream
      Feedback                   :        Zip.Feedback_Proc;
      Explode_Literal_Tree       :        Boolean;
      Explode_Slide_8kb_Lzma_Eos :        Boolean;
      Data_Descriptor_After_Data :        Boolean;
      Hint                       : in out Zip.Headers.Local_File_Header)
   is
      --  I/O Buffers: Size of input buffer
      Inbuf_Size : constant := 16#8000#;  -- (orig: 16#1000# B =  4 KB)
      --  I/O Buffers: Size of sliding dictionary and output buffer
      Wsize : constant := 16#10000#; -- (orig: 16#8000# B = 32 KB)

      ----------------------------------------------------------------------------
      -- Specifications of UnZ_* packages (remain of Info Zip's code structure) --
      ----------------------------------------------------------------------------
      use Ada.Exceptions, Interfaces;

      package Unz_Glob is -- Not global anymore, since local to Decompress_data :-)
         --  I/O Buffers: Sliding dictionary for unzipping, and output buffer as well
         Slide       : Zip.Byte_Buffer (0 .. Wsize);
         Slide_Index : Integer := 0; -- Current Position in slide
         --  I/O Buffers: Input buffer
         Inbuf          : Zip.Byte_Buffer (0 .. Inbuf_Size - 1);
         Inpos, Readpos : Integer;  -- pos. in input buffer, pos. read from file
         Compsize,            -- compressed size of file
         Reachedsize,         -- number of bytes read from zipfile
         Uncompsize,          -- uncompressed size of file
         Effective_Writes : Unzip.File_Size_Type;
         --  ^ count of effective bytes written or tested, for feedback only
         Percents_Done      : Natural;
         Crc32val           : Unsigned_32;  -- crc calculated from data
         Uncompressed_Index : Ada.Streams.Stream_Element_Offset;
      end Unz_Glob;

      Zip_Eof   : Boolean; -- read over end of zip section for this file
      Lz77_Dump : Ada.Text_IO.File_Type;

      package Unz_Io is
         Out_Bin_File : Ada.Streams.Stream_IO.File_Type;
         Out_Txt_File : Ada.Text_IO.File_Type;
         Last_Char    : Character := ' ';

         procedure Init_Buffers;

         procedure Read_Byte_No_Decrypt (Bt : out Zip.Byte);
         pragma Inline (Read_Byte_No_Decrypt);

         function Read_Byte_Decrypted return Unsigned_8; -- NB: reading goes on a while even if
         pragma Inline (Read_Byte_Decrypted);           -- Zip_EOF is set: just gives garbage

         package Bit_Buffer is
            procedure Init;
            --  Read at least n bits into the bit buffer, returns the n first bits
            function Read (N : Natural) return Integer;
            pragma Inline (Read);
            function Read_U32 (N : Natural) return Unsigned_32;
            pragma Inline (Read_U32);
            --  Inverts (NOT operator) the result before masking by n bits
            function Read_Inverted (N : Natural) return Integer;
            pragma Inline (Read_Inverted);
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

         procedure Flush_If_Full (W : in out Integer; Unflushed : in out Boolean);
         pragma Inline (Flush_If_Full);

         procedure Flush_If_Full (W : in out Integer);
         pragma Inline (Flush_If_Full);

         procedure Copy (Distance, Length : Natural; Index : in out Natural);
         pragma Inline (Copy);

         procedure Copy_Or_Zero
           (Distance, Length :        Natural;
            Index            : in out Natural;
            Unflushed        : in out Boolean);
         pragma Inline (Copy_Or_Zero);

         procedure Delete_Output;  --  An error has occured (bad compressed data)
      end Unz_Io;

      package Unz_Meth is
         procedure Copy_Stored;
         Deflate_E_Mode : Boolean := False;
         procedure Inflate;
      end Unz_Meth;

      procedure Process_Feedback (New_Bytes : File_Size_Type) is
         pragma Inline (Process_Feedback);
         New_Percents_Done : Natural;
         User_Aborting     : Boolean;
         use Zip;
      begin
         if Feedback = null or Unz_Glob.Uncompsize = 0 then
            return; -- no feedback proc. or cannot calculate percentage
         end if;
         Unz_Glob.Effective_Writes := Unz_Glob.Effective_Writes + New_Bytes;
         New_Percents_Done         :=
           Natural ((100.0 * Float (Unz_Glob.Effective_Writes)) / Float (Unz_Glob.Uncompsize));
         if New_Percents_Done > Unz_Glob.Percents_Done then
            Feedback
              (Percents_Done => New_Percents_Done,
               Entry_Skipped => False,
               User_Abort    => User_Aborting);
            if User_Aborting then
               raise User_Abort;
            end if;
            Unz_Glob.Percents_Done := New_Percents_Done;
         end if;
      end Process_Feedback;

      use Zip.Crc_Crypto;
      Local_Crypto_Pack : Crypto_Pack;

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
            Unz_Glob.Effective_Writes := 0;
            Unz_Glob.Percents_Done    := 0;
            Zip_Eof                   := False;
            Zip.Crc_Crypto.Init (Unz_Glob.Crc32val);
            Bit_Buffer.Init;
         end Init_Buffers;

         procedure Process_Compressed_End_Reached is
         begin
            if Zip_Eof then -- We came already here once
               Raise_Exception
                 (Zip.Archive_Corrupted'Identity,
                  "Decoding went past compressed data size plus one buffer length");
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
            if Full_Trace then
               Ada.Text_IO.Put ("[Read_buffer...");
            end if;
            if Unz_Glob.Reachedsize > Unz_Glob.Compsize + 2 then
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
            if Full_Trace then
               Ada.Text_IO.Put_Line ("finished]");
            end if;
         end Read_Buffer;

         procedure Read_Byte_No_Decrypt (Bt : out Zip.Byte) is
         begin
            if Unz_Glob.Inpos > Unz_Glob.Readpos then
               Read_Buffer;
            end if;
            Bt             := Unz_Glob.Inbuf (Unz_Glob.Inpos);
            Unz_Glob.Inpos := Unz_Glob.Inpos + 1;
         end Read_Byte_No_Decrypt;

         function Read_Byte_Decrypted return Unsigned_8 is
            Bt : Zip.Byte;
         begin
            Read_Byte_No_Decrypt (Bt);
            Decode (Local_Crypto_Pack, Bt);
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

            function Read_Inverted (N : Natural) return Integer is
            begin
               Need (N);
               return Integer ((not B) and (Shift_Left (1, N) - 1));
            end Read_Inverted;

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
            use Zip, Ada.Streams;
         begin
            if Full_Trace then
               Ada.Text_IO.Put ("[Flush...");
            end if;
            begin
               case Mode is
                  when Write_To_Binary_File =>
                     Blockwrite
                       (Ada.Streams.Stream_IO.Stream (Out_Bin_File).all,
                        Unz_Glob.Slide (0 .. X - 1));
                  when Write_To_Text_File =>
                     Zip.Write_As_Text
                       (Unz_Io.Out_Txt_File,
                        Unz_Glob.Slide (0 .. X - 1),
                        Unz_Io.Last_Char);
                  when Write_To_Memory =>
                     for I in 0 .. X - 1 loop
                        Output_Memory_Access (Unz_Glob.Uncompressed_Index) :=
                          Ada.Streams.Stream_Element (Unz_Glob.Slide (I));
                        Unz_Glob.Uncompressed_Index := Unz_Glob.Uncompressed_Index + 1;
                     end loop;
                  when Write_To_Stream =>
                     Blockwrite (Output_Stream_Access.all, Unz_Glob.Slide (0 .. X - 1));
                  when Just_Test =>
                     null;
               end case;
            exception
               when others =>
                  raise Unzip.Write_Error;
            end;
            Zip.Crc_Crypto.Update (Unz_Glob.Crc32val, Unz_Glob.Slide (0 .. X - 1));
            Process_Feedback (File_Size_Type (X));
            if Full_Trace then
               Ada.Text_IO.Put_Line ("finished]");
            end if;
         end Flush;

         procedure Flush_If_Full (W : in out Integer; Unflushed : in out Boolean) is
         begin
            if W = Wsize then
               Flush (Wsize);
               W         := 0;
               Unflushed := False;
            end if;
         end Flush_If_Full;

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
            if Full_Trace then
               Ada.Text_IO.Put
                 ("(Copy_range: source=" &
                  Integer'Image (Source) &
                  " index=" &
                  Integer'Image (Index) &
                  " amount=" &
                  Integer'Image (Amount));
            end if;
            if abs (Index - Source) < Amount then
               if Full_Trace and then Source < Index then
                  Ada.Text_IO.Put
                    ("; replicates" &
                     Integer'Image (Amount) &
                     " /" &
                     Integer'Image (Index - Source) &
                     " )");
                  --  ...times the range source..index-1
               end if;
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
            if Full_Trace then
               Ada.Text_IO.Put (')');
            end if;
         end Copy_Range;

         --  The copying routines:

         procedure Copy (Distance, Length : Natural; Index : in out Natural) is
            Source, Part, Remain : Integer;
         begin
            if Some_Trace then
               Ada.Text_IO.Put_Line
                 (Lz77_Dump,
                  "DLE" & Integer'Image (Distance) & Integer'Image (Length));
            end if;
            Source := Index - Distance;
            Remain := Length;
            loop
               Adjust_To_Slide (Source, Remain, Part, Index);
               Copy_Range (Source, Index, Part);
               Flush_If_Full (Index);
               exit when Remain = 0;
            end loop;
         end Copy;

         procedure Copy_Or_Zero
           (Distance, Length :        Natural;
            Index            : in out Natural;
            Unflushed        : in out Boolean)
         is
            Source, Part, Remain : Integer;
         begin
            Source := Index - Distance;
            Remain := Length;
            loop
               Adjust_To_Slide (Source, Remain, Part, Index);
               if Unflushed and then Index <= Source then
                  Unz_Glob.Slide (Index .. Index + Part - 1) := (others => 0);
                  Index                                      := Index + Part;
                  Source                                     := Source + Part;
               else
                  Copy_Range (Source, Index, Part);
               end if;
               Flush_If_Full (Index, Unflushed);
               exit when Remain = 0;
            end loop;
         end Copy_Or_Zero;

         procedure Delete_Output is -- an error has occured (bad compressed data)
         begin
            if No_Trace then -- if there is a trace, we are debugging
               case Mode is   --  and want to keep the malformed file
                  when Write_To_Binary_File =>
                     Ada.Streams.Stream_IO.Delete (Unz_Io.Out_Bin_File);
                  when Write_To_Text_File =>
                     Ada.Text_IO.Delete (Unz_Io.Out_Txt_File);
                  when Write_To_Memory | Write_To_Stream | Just_Test =>
                     null; -- Nothing to delete!
               end case;
            end if;
         end Delete_Output;

      end Unz_Io;

      package body Unz_Meth is

         --------[ Method: Copy stored ]--------

         procedure Copy_Stored is
            Size              : constant Unzip.File_Size_Type := Unz_Glob.Compsize;
            Read_In, Absorbed : Unzip.File_Size_Type;
         begin
            Absorbed := 0;
            if Get_Mode (Local_Crypto_Pack) = Encrypted then
               Absorbed := 12;
            end if;
            while Absorbed < Size loop
               Read_In := Size - Absorbed;
               if Read_In > Wsize then
                  Read_In := Wsize;
               end if;
               begin
                  for I in 0 .. Read_In - 1 loop
                     Unz_Glob.Slide (Natural (I)) := Unz_Io.Read_Byte_Decrypted;
                  end loop;
               exception
                  when others =>
                     Raise_Exception
                       (Zip.Archive_Corrupted'Identity,
                        "End of stream reached (format: Store)");
               end;
               begin
                  Unz_Io.Flush (Natural (Read_In));  -- Takes care of CRC too
               exception
                  when User_Abort =>
                     raise;
                  when others =>
                     raise Unzip.Write_Error;
               end;
               Absorbed := Absorbed + Read_In;
            end loop;
         end Copy_Stored;

         --------[ Method: Inflate ]--------

         use Unzip.Decompress.Huffman;

         Lt_Count,
         Dl_Count,
         Lt_Count_0,
         Dl_Count_0,
         Lt_Count_Dyn,
         Dl_Count_Dyn,
         Lt_Count_Fix,
         Dl_Count_Fix : Long_Integer := 0;  --  Statistics of LZ codes per block

         procedure Inflate_Codes (Tl, Td : P_Table_List; Bl, Bd : Integer) is
            Ct      : P_Huft_Table;  --  Current table
            Ct_Idx  : Natural;       --  Current table's index
            Length  : Natural;
            E       : Integer;       -- Table entry flag/number of extra bits
            W       : Integer := Unz_Glob.Slide_Index;  --  More local variable for slide index
            Literal : Zip.Byte;
         begin
            if Some_Trace then
               Lt_Count_0 := Lt_Count;
               Dl_Count_0 := Dl_Count;
               Ada.Text_IO.Put_Line ("Begin Inflate_codes");
            end if;

            --  Inflate the coded data
            Main_Loop :
            while not Zip_Eof loop
               if Tl = null then
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Null table list (on data decoding, Huffman tree for literals or LZ lengths)");
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
                     if Some_Trace then
                        Lt_Count := Lt_Count + 1;
                        Ada.Text_IO.Put (Lz77_Dump, "Lit" & Zip.Byte'Image (Literal));
                        if Literal in 32 .. 126 then
                           Ada.Text_IO.Put (Lz77_Dump, " '" & Character'Val (Literal) & ''');
                        end if;
                        Ada.Text_IO.New_Line (Lz77_Dump);
                     end if;
                     Unz_Glob.Slide (W) := Literal;
                     W                  := W + 1;
                     Unz_Io.Flush_If_Full (W);

                  when 15 =>     -- End of block (EOB, code 256)
                     if Full_Trace then
                        Ada.Text_IO.Put_Line ("Exit  Inflate_codes, e=15 -> EOB");
                     end if;
                     exit Main_Loop;

                  when others => -- We have a length/distance code
                     if Some_Trace then
                        Dl_Count := Dl_Count + 1;
                     end if;
                     --  Get length of block to copy:
                     Length := Ct (Ct_Idx).N + Unz_Io.Bit_Buffer.Read_And_Dump (E);

                     --  Decode distance of block to copy:
                     if Td = null then
                        Raise_Exception
                          (Zip.Archive_Corrupted'Identity,
                           "Null table list (on data decoding, Huffman tree for LZ distances)");
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

            if Some_Trace then
               Ada.Text_IO.Put_Line
                 ("End   Inflate_codes;  " &
                  Long_Integer'Image (Lt_Count - Lt_Count_0) &
                  " literals," &
                  Long_Integer'Image (Dl_Count - Dl_Count_0) &
                  " DL codes," &
                  Long_Integer'Image (Dl_Count + Lt_Count - Lt_Count_0 - Dl_Count_0) &
                  " in total");
            end if;
         end Inflate_Codes;

         procedure Inflate_Stored_Block is -- Actually, nothing to inflate
            N : Integer;
         begin
            Unz_Io.Bit_Buffer.Dump_To_Byte_Boundary;
            --  Get the block length and its complement
            N := Unz_Io.Bit_Buffer.Read_And_Dump (16);
            if Some_Trace then
               Ada.Text_IO.Put_Line
                 ("Begin Inflate_stored_block, bytes stored: " & Integer'Image (N));
            end if;
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
            if Some_Trace then
               Ada.Text_IO.Put_Line ("End   Inflate_stored_block");
            end if;
         end Inflate_Stored_Block;

         --  Copy lengths for literal codes 257..285

         Copy_Lengths_Literal : Length_Array (0 .. 30) :=
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

         Extra_Bits_Literal : Length_Array (0 .. 30) :=
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

         Max_Dist : Integer := 29; -- changed to 31 for deflate_e

         Length_List_For_Fixed_Block_Literals : constant Length_Array (0 .. 287) :=
           (0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8);

         procedure Inflate_Fixed_Block is
            Tl,                        --   literal/length code table
            Td              : P_Table_List;            --  distance code table
            Bl, Bd          : Integer;          --  lookup bits for tl/bd
            Huft_Incomplete : Boolean;
         begin
            if Some_Trace then
               Ada.Text_IO.Put_Line ("Begin Inflate_fixed_block");
            end if;
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
               if Huft_Incomplete then
                  if Full_Trace then
                     Ada.Text_IO.Put_Line
                       ("td is incomplete, pointer=null: " & Boolean'Image (Td = null));
                  end if;
               end if;
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
            if Some_Trace then
               Ada.Text_IO.Put_Line ("End   Inflate_fixed_block");
               Lt_Count_Fix := Lt_Count_Fix + (Lt_Count - Lt_Count_0);
               Dl_Count_Fix := Dl_Count_Fix + (Dl_Count - Dl_Count_0);
            end if;
         end Inflate_Fixed_Block;

         Bit_Order_For_Dynamic_Block : constant array (0 .. 18) of Natural :=
           (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

         procedure Inflate_Dynamic_Block is

            Lbits : constant := 9;
            Dbits : constant := 6;

            Current_Length             : Natural;
            Defined, Number_Of_Lengths : Natural;

            Tl,                             -- literal/length code tables
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
            if Some_Trace then
               Ada.Text_IO.Put_Line ("Begin Inflate_dynamic_block");
            end if;

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
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Incomplete code set for compression structure");
               end if;
            exception
               when others =>
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Error when building tables for compression structure");
            end;

            --  Read in the compression structure: literal and distance code lengths
            Number_Of_Lengths := Nl + Nd;
            Defined           := 0;
            Current_Length    := 0;

            while Defined < Number_Of_Lengths loop
               if Tl = null then
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Null table list (on compression structure)");
               end if;
               Ct     := Tl.Table;
               Ct_Idx := Unz_Io.Bit_Buffer.Read (Bl);
               Unz_Io.Bit_Buffer.Dump (Ct (Ct_Idx).Bits);

               case Ct (Ct_Idx).N is
                  when
                      0 ..
                        15 =>     --  Length of code for symbol of index 'defined', in bits (0..15)
                     Current_Length := Ct (Ct_Idx).N;
                     Ll (Defined)   := Current_Length;
                     Defined        := Defined + 1;
                  when 16 =>          --  16 means: repeat last bit length 3 to 6 times
                     if Defined = 0 then
                     --  Nothing in the Ll array has been defined so far. Then, current_length is
                     --  (theoretically) undefined and cannot be repeated.
                     --  This unspecified case is treated as an error by zlib's inflate.c.
                        Raise_Exception
                          (Zip.Archive_Corrupted'Identity,
                           "Illegal data for compression structure (repeat an undefined code length)");
                     end if;
                     Repeat_Length_Code (3 + Unz_Io.Bit_Buffer.Read_And_Dump (2));
                  when 17 =>          --  17 means: the next 3 to 10 symbols' codes have zero bit lengths
                     Current_Length := 0;
                     Repeat_Length_Code (3 + Unz_Io.Bit_Buffer.Read_And_Dump (3));
                  when 18 =>          --  18 means: the next 11 to 138 symbols' codes have zero bit lengths
                     Current_Length := 0;
                     Repeat_Length_Code (11 + Unz_Io.Bit_Buffer.Read_And_Dump (7));
                  when others =>      --  Shouldn't occur if this tree is correct
                     Raise_Exception
                       (Zip.Archive_Corrupted'Identity,
                        "Illegal data for compression structure (values should be in the range 0 .. 18): " &
                        Integer'Image (Ct (Ct_Idx).N));
               end case;
            end loop;
            --  Free the Huffman tree that was used for decoding the compression
            --  structure, which is contained now in Ll.
            Huft_Free (Tl);
            if Ll (256) = 0 then
               --  No code length for the End-Of-Block symbol, implies infinite stream!
               --  This case is unspecified but obviously we must stop here.
               Raise_Exception
                 (Zip.Archive_Corrupted'Identity,
                  "No code for End-Of-Block symbol #256");
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
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Incomplete code set for literals/lengths");
               end if;
            exception
               when others =>
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Error when building tables for literals/lengths");
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
                  if Deflate_Strict then
                     Raise_Exception
                       (Zip.Archive_Corrupted'Identity,
                        "Incomplete code set for distances");
                  elsif Some_Trace then  --  not deflate_strict => don't stop
                     Ada.Text_IO.Put_Line ("Huffman tree incomplete - PKZIP 1.93a bug workaround");
                  end if;
               end if;
            exception
               when Huft_Out_Of_Memory | Huft_Error =>
                  Huft_Free (Tl);
                  Raise_Exception
                    (Zip.Archive_Corrupted'Identity,
                     "Error when building tables for distances");
            end;
            --  Decompress the block's data, until an end-of-block code.
            Inflate_Codes (Tl, Td, Bl, Bd);
            --  Done with this block, free resources.
            Huft_Free (Tl);
            Huft_Free (Td);
            if Some_Trace then
               Ada.Text_IO.Put_Line ("End   Inflate_dynamic_block");
               Lt_Count_Dyn := Lt_Count_Dyn + (Lt_Count - Lt_Count_0);
               Dl_Count_Dyn := Dl_Count_Dyn + (Dl_Count - Dl_Count_0);
            end if;
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
                  raise Zip.Archive_Corrupted; -- Bad block type (3)
            end case;
         end Inflate_Block;

         procedure Inflate is
            Is_Last_Block                  : Boolean;
            Blocks, Blocks_Fix, Blocks_Dyn : Long_Integer := 0;
         begin
            if Deflate_E_Mode then
               Copy_Lengths_Literal (28) := 3; -- instead of 258
               Extra_Bits_Literal (28)   := 16;  -- instead of 0
               Max_Dist                  := 31;
            end if;
            loop
               Blocks := Blocks + 1;
               Inflate_Block (Is_Last_Block, Blocks_Fix, Blocks_Dyn);
               exit when Is_Last_Block;
            end loop;
            Unz_Io.Flush (Unz_Glob.Slide_Index);
            Unz_Glob.Slide_Index := 0;
            if Some_Trace then
               Ada.Text_IO.Put_Line
                 ("# blocks:" &
                  Long_Integer'Image (Blocks) &
                  "; fixed:" &
                  Long_Integer'Image (Blocks_Fix) &
                  "; dynamic:" &
                  Long_Integer'Image (Blocks_Dyn));
               if Blocks_Fix > 0 then
                  Ada.Text_IO.Put_Line
                    ("Averages per fixed block: literals:" &
                     Long_Integer'Image (Lt_Count_Fix / Blocks_Fix) &
                     "; DL codes:" &
                     Long_Integer'Image (Dl_Count_Fix / Blocks_Fix) &
                     "; all codes:" &
                     Long_Integer'Image ((Lt_Count_Fix + Dl_Count_Fix) / Blocks_Fix));
               end if;
               if Blocks_Dyn > 0 then
                  Ada.Text_IO.Put_Line
                    ("Averages per dynamic block: literals:" &
                     Long_Integer'Image (Lt_Count_Dyn / Blocks_Dyn) &
                     "; DL codes:" &
                     Long_Integer'Image (Dl_Count_Dyn / Blocks_Dyn) &
                     "; all codes:" &
                     Long_Integer'Image ((Lt_Count_Dyn + Dl_Count_Dyn) / Blocks_Dyn));
               end if;
            end if;
         end Inflate;

      end Unz_Meth;

      procedure Process_Descriptor (Dd : out Zip.Headers.Data_Descriptor) is
         Start     : Integer;
         B         : Unsigned_8;
         Dd_Buffer : Zip.Byte_Buffer (1 .. 30);
      begin
         Unz_Io.Bit_Buffer.Dump_To_Byte_Boundary;
         Set_Mode
           (Local_Crypto_Pack,
            Clear); -- We are after compressed data, switch off decryption.
         B := Unz_Io.Read_Byte_Decrypted;
         if B = 75 then -- 'K' ('P' is before, this is a Java/JAR bug!)
            Dd_Buffer (1) := 80;
            Dd_Buffer (2) := 75;
            Start         := 3;
         else
            Dd_Buffer (1) := B; -- hopefully = 80 (will be checked)
            Start         := 2;
         end if;
         for I in Start .. 16 loop
            Dd_Buffer (I) := Unz_Io.Read_Byte_Decrypted;
         end loop;
         Zip.Headers.Copy_And_Check (Dd_Buffer, Dd);
      exception
         when Zip.Headers.Bad_Data_Descriptor =>
            raise Zip.Archive_Corrupted;
      end Process_Descriptor;

      use Zip, Unz_Meth;

      package SU renames Ada.Strings.Unbounded;
   begin -- Decompress_Data
      if Some_Trace then
         Ada.Text_IO.Create (Lz77_Dump, Ada.Text_IO.Out_File, "dump.lz77");
      end if;
      Output_Memory_Access := null;
      --  ^ this is an 'out' parameter, we have to set it anyway
      case Mode is
         when Write_To_Binary_File =>
            Ada.Streams.Stream_IO.Create
              (Unz_Io.Out_Bin_File,
               Ada.Streams.Stream_IO.Out_File,
               Output_File_Name,
               Form => SU.To_String (Zip_Streams.Form_For_Io_Open_And_Create));
         when Write_To_Text_File =>
            Ada.Text_IO.Create
              (Unz_Io.Out_Txt_File,
               Ada.Text_IO.Out_File,
               Output_File_Name,
               Form => SU.To_String (Zip_Streams.Form_For_Io_Open_And_Create));
         when Write_To_Memory =>
            Output_Memory_Access :=
              new Ada.Streams
                .Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Offset (Hint.Dd.Uncompressed_Size));
            Unz_Glob.Uncompressed_Index := Output_Memory_Access'First;
         when Write_To_Stream | Just_Test =>
            null;
      end case;

      Unz_Glob.Compsize := Hint.Dd.Compressed_Size;
      if Unz_Glob.Compsize > File_Size_Type'Last - 2 then -- This means: unknown size
         Unz_Glob.Compsize := File_Size_Type'Last - 2;      -- Avoid wraparound in read_buffer
      end if;                                             -- From TT's version, 2008
      Unz_Glob.Uncompsize := Hint.Dd.Uncompressed_Size;
      Unz_Io.Init_Buffers;
      Set_Mode (Local_Crypto_Pack, Clear);

      --  UnZip correct type
      begin
         case Format is
            when Store =>
               Copy_Stored;
            when Deflate | Deflate_E =>
               Unz_Meth.Deflate_E_Mode := Format = Deflate_E;
               Unz_Meth.Inflate;
            when others =>
               Raise_Exception
                 (Unsupported_Method'Identity,
                  "Format/method " &
                  Pkzip_Method'Image (Format) &
                  " not supported for decompression");
         end case;
      exception
         when others =>
            Unz_Io.Delete_Output;
            raise;
      end;
      Unz_Glob.Crc32val := Zip.Crc_Crypto.Final (Unz_Glob.Crc32val);
      --  Decompression done !

      if Data_Descriptor_After_Data then -- Sizes and CRC at the end
         declare
            Memo_Uncomp_Size : constant Unsigned_32 := Hint.Dd.Uncompressed_Size;
         begin
            Process_Descriptor (Hint.Dd); -- CRC is for checking; sizes are for informing user
            if Memo_Uncomp_Size < Unsigned_32'Last
              and then --
              Memo_Uncomp_Size /= Hint.Dd.Uncompressed_Size
            then
               Unz_Io.Delete_Output;
               raise Uncompressed_Size_Error;
            end if;
         end;
      end if;

      if Hint.Dd.Crc_32 /= Unz_Glob.Crc32val then
         Unz_Io.Delete_Output;
         Raise_Exception
           (Crc_Error'Identity,
            "CRC stored in archive: " &
            Hexadecimal (Hint.Dd.Crc_32) &
            "; CRC computed now: " &
            Hexadecimal (Unz_Glob.Crc32val));
      end if;

      case Mode is
         when Write_To_Binary_File =>
            Ada.Streams.Stream_IO.Close (Unz_Io.Out_Bin_File);
         when Write_To_Text_File =>
            Ada.Text_IO.Close (Unz_Io.Out_Txt_File);
         when Write_To_Memory | Write_To_Stream | Just_Test =>
            null; -- Nothing to close!
      end case;
      if Some_Trace then
         Ada.Text_IO.Close (Lz77_Dump);
      end if;

   exception
      when others => -- close the file in case of an error, if not yet closed
         case Mode is -- or deleted
            when Write_To_Binary_File =>
               if Ada.Streams.Stream_IO.Is_Open (Unz_Io.Out_Bin_File) then
                  Ada.Streams.Stream_IO.Close (Unz_Io.Out_Bin_File);
               end if;
            when Write_To_Text_File =>
               if Ada.Text_IO.Is_Open (Unz_Io.Out_Txt_File) then
                  Ada.Text_IO.Close (Unz_Io.Out_Txt_File);
               end if;
            when Write_To_Memory | Write_To_Stream | Just_Test =>
               null; -- Nothing to close!
         end case;
         raise;
   end Decompress_Data;

end Unzip.Decompress;
