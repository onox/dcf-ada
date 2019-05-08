-- Legal licensing note:

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

-- NB: this is the MIT License, as found on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip.CRC_Crypto, UnZip.Decompress.Huffman;

with Ada.Exceptions, Ada.Streams.Stream_IO, Ada.Text_IO, Interfaces;

package body UnZip.Decompress is

  procedure Decompress_data(
    zip_file                   : in out Zip_Streams.Root_Zipstream_Type'Class;
    format                     : PKZip_method;
    mode                       : Write_mode;
    output_file_name           : String; -- relevant only if mode = write_to_file
    output_memory_access       : out p_Stream_Element_Array; -- \ = write_to_memory
    output_stream_access       : p_Stream;                   -- \ = write_to_stream
    feedback                   : Zip.Feedback_proc;
    explode_literal_tree       : Boolean;
    explode_slide_8KB_LZMA_EOS : Boolean;
    data_descriptor_after_data : Boolean;
    hint                       : in out Zip.Headers.Local_File_Header
  )
  is
    -- Disable AdaControl rule for detecting global variables, they have become local here.
    --## RULE OFF Directly_Accessed_Globals
    --
    --  I/O Buffers: Size of input buffer
    inbuf_size: constant:= 16#8000#;  -- (orig: 16#1000# B =  4 KB)
    --  I/O Buffers: Size of sliding dictionary and output buffer
    wsize     : constant:= 16#10000#; -- (orig: 16#8000# B = 32 KB)

    ----------------------------------------------------------------------------
    -- Specifications of UnZ_* packages (remain of Info Zip's code structure) --
    ----------------------------------------------------------------------------
    use Ada.Exceptions, Interfaces;

    package UnZ_Glob is -- Not global anymore, since local to Decompress_data :-)
      -- I/O Buffers: Sliding dictionary for unzipping, and output buffer as well
      slide: Zip.Byte_Buffer( 0..wsize );
      slide_index: Integer:= 0; -- Current Position in slide
      -- I/O Buffers: Input buffer
      inbuf: Zip.Byte_Buffer( 0 .. inbuf_size - 1 );
      inpos, readpos: Integer;  -- pos. in input buffer, pos. read from file
      compsize,            -- compressed size of file
      reachedsize,         -- number of bytes read from zipfile
      uncompsize,          -- uncompressed size of file
      effective_writes : UnZip.File_size_type;
      -- ^ count of effective bytes written or tested, for feedback only
      percents_done    : Natural;
      crc32val : Unsigned_32;  -- crc calculated from data
      uncompressed_index  : Ada.Streams.Stream_Element_Offset;
    end UnZ_Glob;

    Zip_EOF  : Boolean; -- read over end of zip section for this file
    LZ77_dump : Ada.Text_IO.File_Type;

    package UnZ_IO is
      out_bin_file: Ada.Streams.Stream_IO.File_Type;
      out_txt_file: Ada.Text_IO.File_Type;
      last_char   : Character:= ' ';

      procedure Init_Buffers;

      procedure Read_byte_no_decrypt( bt : out Zip.Byte );
        pragma Inline( Read_byte_no_decrypt );

      function Read_byte_decrypted return Unsigned_8; -- NB: reading goes on a while even if
        pragma Inline(Read_byte_decrypted);           -- Zip_EOF is set: just gives garbage

      package Bit_buffer is
        procedure Init;
        -- Read at least n bits into the bit buffer, returns the n first bits
        function Read ( n: Natural ) return Integer;
          pragma Inline(Read);
        function Read_U32 ( n: Natural ) return Unsigned_32;
          pragma Inline(Read_U32);
        -- Inverts (NOT operator) the result before masking by n bits
        function Read_inverted ( n: Natural ) return Integer;
          pragma Inline(Read_inverted);
        -- Dump n bits no longer needed from the bit buffer
        procedure Dump ( n: Natural );
          pragma Inline(Dump);
        procedure Dump_to_byte_boundary;
        function Read_and_dump( n: Natural ) return Integer;
          pragma Inline(Read_and_dump);
        function Read_and_dump_U32( n: Natural ) return Unsigned_32;
          pragma Inline(Read_and_dump_U32);
      end Bit_buffer;

      procedure Flush ( x: Natural ); -- directly from slide to output stream

      procedure Flush_if_full(W: in out Integer; unflushed: in out Boolean);
      pragma Inline(Flush_if_full);

      procedure Flush_if_full(W: in out Integer);
      pragma Inline(Flush_if_full);

      procedure Copy(distance, length: Natural; index: in out Natural);
      pragma Inline(Copy);

      procedure Copy_or_zero(
        distance, length:        Natural;
        index           : in out Natural;
        unflushed       : in out Boolean );
      pragma Inline(Copy_or_zero);

      procedure Delete_output; -- an error has occured (bad compressed data)

    end UnZ_IO;

    package UnZ_Meth is
      procedure Copy_stored;
      deflate_e_mode: Boolean:= False;
      procedure Inflate;
    end UnZ_Meth;

    procedure Process_feedback(new_bytes: File_size_type) is
    pragma Inline(Process_feedback);
      new_percents_done: Natural;
      user_aborting: Boolean;
      use Zip;
    begin
      if feedback = null or UnZ_Glob.uncompsize = 0 then
        return; -- no feedback proc. or cannot calculate percentage
      end if;
      UnZ_Glob.effective_writes:= UnZ_Glob.effective_writes + new_bytes;
      new_percents_done:= Natural(
        (100.0 * Float(UnZ_Glob.effective_writes)) / Float(UnZ_Glob.uncompsize)
      );
      if new_percents_done > UnZ_Glob.percents_done then
        feedback(
          percents_done => new_percents_done,
          entry_skipped => False,
          user_abort    => user_aborting
        );
        if user_aborting then
          raise User_abort;
        end if;
        UnZ_Glob.percents_done:= new_percents_done;
      end if;
    end Process_feedback;

    use Zip.CRC_Crypto;
    local_crypto_pack: Crypto_pack;

    ------------------------------
    -- Bodies of UnZ_* packages --
    ------------------------------
    package body UnZ_IO is

      procedure Init_Buffers is
      begin
        UnZ_Glob.inpos   :=  0;  -- Input buffer position
        UnZ_Glob.readpos := -1;  -- Nothing read
        UnZ_Glob.slide_index := 0;
        UnZ_Glob.reachedsize      := 0;
        UnZ_Glob.effective_writes := 0;
        UnZ_Glob.percents_done    := 0;
        Zip_EOF := False;
        Zip.CRC_Crypto.Init( UnZ_Glob.crc32val );
        Bit_buffer.Init;
      end Init_Buffers;

      procedure Process_compressed_end_reached is
      begin
        if Zip_EOF then -- We came already here once
          Raise_Exception(Zip.Archive_corrupted'Identity,
            "Decoding went past compressed data size plus one buffer length");
          -- Avoid infinite loop on data with exactly buffer's length and no end marker
        else
          UnZ_Glob.readpos := UnZ_Glob.inbuf'Length;
          -- Simulates reading -> no blocking.
          -- The buffer is full of "random" data and we hope for a wrong code or a CRC error
          Zip_EOF := True;
        end if;
      end Process_compressed_end_reached;

      procedure Read_buffer is
      begin
        if full_trace then
          Ada.Text_IO.Put("[Read_buffer...");
        end if;
        if UnZ_Glob.reachedsize > UnZ_Glob.compsize + 2 then
          -- +2: last code is smaller than requested!
          Process_compressed_end_reached;
        else
          begin
            Zip.BlockRead(
              stream        => zip_file,
              buffer        => UnZ_Glob.inbuf,
              actually_read => UnZ_Glob.readpos
            );
          exception
            when others => -- I/O error
              Process_compressed_end_reached;
          end;
          if UnZ_Glob.readpos = 0 then -- No byte at all was read
            Process_compressed_end_reached;
          end if;
          UnZ_Glob.reachedsize:=
            UnZ_Glob.reachedsize + UnZip.File_size_type(UnZ_Glob.readpos);
          UnZ_Glob.readpos:= UnZ_Glob.readpos - 1; -- Reason: index of inbuf starts at 0
        end if;
        UnZ_Glob.inpos:= 0;
        if full_trace then
          Ada.Text_IO.Put_Line("finished]");
        end if;
      end Read_buffer;

      procedure Read_byte_no_decrypt( bt : out Zip.Byte ) is
      begin
        if UnZ_Glob.inpos > UnZ_Glob.readpos then
          Read_buffer;
        end if;
        bt := UnZ_Glob.inbuf ( UnZ_Glob.inpos );
        UnZ_Glob.inpos := UnZ_Glob.inpos + 1;
      end Read_byte_no_decrypt;

      function Read_byte_decrypted return Unsigned_8 is
        bt : Zip.Byte;
      begin
        Read_byte_no_decrypt( bt );
        Decode(local_crypto_pack, bt);
        return bt;
      end Read_byte_decrypted;

      package body Bit_buffer is
        B : Unsigned_32;
        K : Integer;

        procedure Init is
        begin
          B := 0;
          K := 0;
        end Init;

        procedure Need( n : Natural ) is
          pragma Inline(Need);
        begin
          while K < n loop
            B:= B or Shift_Left( Unsigned_32( Read_byte_decrypted ), K );
            K:= K + 8;
          end loop;
        end Need;

        procedure Dump ( n : Natural ) is
        begin
          B := Shift_Right(B, n );
          K := K - n;
        end Dump;

        procedure Dump_to_byte_boundary is
        begin
          Dump ( K mod 8 );
        end Dump_to_byte_boundary;

        function Read_U32 ( n: Natural ) return Unsigned_32 is
        begin
          Need(n);
          return B and (Shift_Left(1,n) - 1);
        end Read_U32;

        function Read_inverted ( n: Natural ) return Integer is
        begin
          Need(n);
          return Integer((not B) and (Shift_Left(1,n) - 1));
        end Read_inverted;

        function Read ( n: Natural ) return Integer is
        begin
          return Integer(Read_U32(n));
        end Read;

        function Read_and_dump( n: Natural ) return Integer is
          res: Integer;
        begin
          res:= Read(n);
          Dump(n);
          return res;
        end Read_and_dump;

        function Read_and_dump_U32( n: Natural ) return Unsigned_32 is
          res: Unsigned_32;
        begin
          res:= Read_U32(n);
          Dump(n);
          return res;
        end Read_and_dump_U32;

      end Bit_buffer;

      procedure Flush ( x: Natural ) is
        use Zip, Ada.Streams;
      begin
        if full_trace then
          Ada.Text_IO.Put("[Flush...");
        end if;
        begin
          case mode is
            when write_to_binary_file =>
              BlockWrite(Ada.Streams.Stream_IO.Stream(out_bin_file).all, UnZ_Glob.slide(0..x-1));
            when write_to_text_file =>
              Zip.Write_as_text(
                UnZ_IO.out_txt_file, UnZ_Glob.slide(0..x-1), UnZ_IO.last_char
              );
            when write_to_memory =>
              for i in 0..x-1 loop
                output_memory_access(UnZ_Glob.uncompressed_index):=
                  Ada.Streams.Stream_Element(UnZ_Glob.slide(i));
                UnZ_Glob.uncompressed_index:= UnZ_Glob.uncompressed_index + 1;
              end loop;
            when write_to_stream =>
              BlockWrite(output_stream_access.all, UnZ_Glob.slide(0..x-1));
            when just_test =>
              null;
          end case;
        exception
          when others =>
            raise UnZip.Write_Error;
        end;
        Zip.CRC_Crypto.Update( UnZ_Glob.crc32val, UnZ_Glob.slide( 0..x-1 ) );
        Process_feedback(File_size_type(x));
        if full_trace then
          Ada.Text_IO.Put_Line("finished]");
        end if;
      end Flush;

      procedure Flush_if_full(W: in out Integer; unflushed: in out Boolean) is
      begin
        if W = wsize then
          Flush(wsize);
          W:= 0;
          unflushed:= False;
        end if;
      end Flush_if_full;

      procedure Flush_if_full(W: in out Integer) is
      begin
        if W = wsize then
          Flush(wsize);
          W:= 0;
        end if;
      end Flush_if_full;

      ----------------------------------------------------
      -- Reproduction of sequences in the output slide. --
      ----------------------------------------------------

      -- Internal:

      procedure Adjust_to_Slide(
          source         : in out Integer;
          remain         : in out Natural;
          part           :    out Integer;
          index:                  Integer)
      is
        pragma Inline(Adjust_to_Slide);
      begin
        source:= source mod wsize;
        -- source and index are now in 0..WSize-1
        if  source > index then
          part:= wsize-source;
        else
          part:= wsize-index;
        end if;
        -- NB: part is in 1..WSize (part cannot be 0)
        if part > remain then
          part:= remain;
        end if;
        -- Now part <= remain
        remain:= remain - part;
        -- NB: remain cannot be < 0
      end Adjust_to_Slide;

      procedure Copy_range(source, index: in out Natural; amount: Positive) is
        pragma Inline(Copy_range);
      begin
        if full_trace then
          Ada.Text_IO.Put(
            "(Copy_range: source=" & Integer'Image(source) &
            " index=" & Integer'Image(index) &
            " amount=" & Integer'Image(amount) );
        end if;
        if abs (index - source) < amount then
          if full_trace and then source < index then
            Ada.Text_IO.Put(
              "; replicates" &
              Integer'Image(amount) & " /" & Integer'Image(index-source) &
              " )"
            );
            -- ...times the range source..index-1
          end if;
          -- if source >= index, the effect of copy is just like the non-overlapping case
          for count in reverse 1..amount loop
            UnZ_Glob.slide(index):= UnZ_Glob.slide(source);
            index := index  + 1;
            source:= source + 1;
          end loop;
        else -- non-overlapping -> copy slice
          UnZ_Glob.slide( index .. index+amount-1 ):=
            UnZ_Glob.slide( source..source+amount-1 );
          index := index  + amount;
          source:= source + amount;
        end if;
        if full_trace then
          Ada.Text_IO.Put(')');
        end if;
      end Copy_range;

      -- The copying routines:

      procedure Copy(distance, length: Natural; index: in out Natural ) is
        source, part, remain: Integer;
      begin
        if some_trace then
          Ada.Text_IO.Put_Line(LZ77_dump, "DLE" & Integer'Image(distance) & Integer'Image(length));
        end if;
        source:= index - distance;
        remain:= length;
        loop
          Adjust_to_Slide(source,remain,part, index);
          Copy_range(source, index, part);
          Flush_if_full(index);
          exit when remain = 0;
        end loop;
      end Copy;

      procedure Copy_or_zero(
          distance, length:        Natural;
          index           : in out Natural;
          unflushed       : in out Boolean )
      is
        source,part,remain: Integer;
      begin
        source:= index - distance;
        remain:= length;
        loop
          Adjust_to_Slide(source,remain,part, index);
          if unflushed and then index <= source then
            UnZ_Glob.slide( index .. index+part-1 ):= (others=> 0);
            index := index  + part;
            source:= source + part;
          else
            Copy_range(source, index, part);
          end if;
          Flush_if_full(index,unflushed);
          exit when remain = 0;
        end loop;
      end Copy_or_zero;

      procedure Delete_output is -- an error has occured (bad compressed data)
      begin
        if no_trace then -- if there is a trace, we are debugging
          case mode is   --  and want to keep the malformed file
            when write_to_binary_file =>
              Ada.Streams.Stream_IO.Delete( UnZ_IO.out_bin_file );
            when write_to_text_file =>
              Ada.Text_IO.Delete( UnZ_IO.out_txt_file );
            when write_to_memory | write_to_stream | just_test =>
              null; -- Nothing to delete!
          end case;
        end if;
      end Delete_output;

    end UnZ_IO;

    package body UnZ_Meth is

      --------[ Method: Copy stored ]--------

      procedure Copy_stored is
        size: constant UnZip.File_size_type:= UnZ_Glob.compsize;
        read_in, absorbed : UnZip.File_size_type;
      begin
        absorbed:= 0;
        if Get_mode(local_crypto_pack) = encrypted then
          absorbed:= 12;
        end if;
        while absorbed < size loop
          read_in := size - absorbed;
          if read_in > wsize then
            read_in := wsize;
          end if;
          begin
            for I in 0 .. read_in-1 loop
              UnZ_Glob.slide( Natural(I) ) := UnZ_IO.Read_byte_decrypted;
            end loop;
          exception
            when others=>
              Raise_Exception(Zip.Archive_corrupted'Identity,
                "End of stream reached (format: Store)");
          end;
          begin
            UnZ_IO.Flush ( Natural(read_in) );  -- Takes care of CRC too
          exception
            when User_abort =>
              raise;
            when others =>
              raise UnZip.Write_Error;
          end;
          absorbed:= absorbed + read_in;
        end loop;
      end Copy_stored;

      --------[ Method: Inflate ]--------

      use Unzip.Decompress.Huffman;

      lt_count,     dl_count,
      lt_count_0,   dl_count_0,
      lt_count_dyn, dl_count_dyn,
      lt_count_fix, dl_count_fix: Long_Integer:= 0;  --  Statistics of LZ codes per block

      procedure Inflate_Codes ( Tl, Td: p_Table_list; Bl, Bd: Integer ) is
        CT     : p_HufT_table;       -- current table
        CT_idx : Natural;            -- current table's index
        length : Natural;
        E      : Integer;      -- table entry flag/number of extra bits
        W      : Integer:= UnZ_Glob.slide_index;  -- more local variable for slide index
        literal: Zip.Byte;
      begin
        if some_trace then
          lt_count_0:= lt_count;
          dl_count_0:= dl_count;
          Ada.Text_IO.Put_Line("Begin Inflate_codes");
        end if;

        -- inflate the coded data
        main_loop:
        while not Zip_EOF loop
          if Tl = null then
            Raise_Exception(Zip.Archive_corrupted'Identity,
              "Null table list (on data decoding, Huffman tree for literals or LZ lengths)");
          end if;
          CT:= Tl.table;
          CT_idx:= UnZ_IO.Bit_buffer.Read(Bl);
          loop
            E := CT(CT_idx).extra_bits;
            exit when E <= 16;
            if E = invalid then
              raise Zip.Archive_corrupted;
            end if;

            -- then it's a literal
            UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );
            E:= E - 16;
            CT:= CT(CT_idx).next_table;
            CT_idx := UnZ_IO.Bit_buffer.Read(E);
          end loop;

          UnZ_IO.Bit_buffer.Dump ( CT(CT_idx).bits );

          case E is
            when 16 =>     -- CT(CT_idx).N is a Literal (code 0 .. 255)
              literal:= Zip.Byte( CT(CT_idx).n );
              if some_trace then
                lt_count:= lt_count + 1;
                Ada.Text_IO.Put(LZ77_dump, "Lit" & Zip.Byte'Image(literal));
                if literal in 32..126 then
                  Ada.Text_IO.Put(LZ77_dump, " '" & Character'Val(literal) & ''');
                end if;
                Ada.Text_IO.New_Line(LZ77_dump);
              end if;
              UnZ_Glob.slide ( W ) :=  literal;
              W:= W + 1;
              UnZ_IO.Flush_if_full(W);

            when 15 =>     -- End of block (EOB, code 256)
              if full_trace then
                Ada.Text_IO.Put_Line("Exit  Inflate_codes, e=15 -> EOB");
              end if;
              exit main_loop;

            when others => -- We have a length/distance code
              if some_trace then
                dl_count:= dl_count + 1;
              end if;
              -- Get length of block to copy:
              length:= CT(CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump(E);

              -- Decode distance of block to copy:
              if Td = null then
                Raise_Exception(Zip.Archive_corrupted'Identity,
                  "Null table list (on data decoding, Huffman tree for LZ distances)");
              end if;
              CT:= Td.table;
              CT_idx:= UnZ_IO.Bit_buffer.Read(Bd);
              loop
                E := CT(CT_idx).extra_bits;
                exit when E <= 16;
                if E = invalid then
                  raise Zip.Archive_corrupted;
                end if;
                UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );
                E:= E - 16;
                CT:= CT(CT_idx).next_table;
                CT_idx:= UnZ_IO.Bit_buffer.Read(E);
              end loop;
              UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );
              UnZ_IO.Copy(
                distance => CT(CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump(E),
                length   => length,
                index    => W
              );
          end case;
        end loop main_loop;

        UnZ_Glob.slide_index:= W;

        if some_trace then
          Ada.Text_IO.Put_Line("End   Inflate_codes;  " &
            Long_Integer'Image(lt_count-lt_count_0) & " literals," &
            Long_Integer'Image(dl_count-dl_count_0) & " DL codes," &
            Long_Integer'Image(dl_count+lt_count-lt_count_0-dl_count_0) & " in total");
        end if;
      end Inflate_Codes;

      procedure Inflate_stored_block is -- Actually, nothing to inflate
        N : Integer;
      begin
        UnZ_IO.Bit_buffer.Dump_to_byte_boundary;
        -- Get the block length and its complement
        N:= UnZ_IO.Bit_buffer.Read_and_dump( 16 );
        if some_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_stored_block, bytes stored: " & Integer'Image(N));
        end if;
        if  N /= Integer(
         (not UnZ_IO.Bit_buffer.Read_and_dump_U32(16))
         and 16#ffff#)
        then
          raise Zip.Archive_corrupted;
        end if;
        while N > 0 and then not Zip_EOF loop
          -- Read and output the non-compressed data
          N:= N - 1;
          UnZ_Glob.slide ( UnZ_Glob.slide_index ) :=
            Zip.Byte( UnZ_IO.Bit_buffer.Read_and_dump(8) );
          UnZ_Glob.slide_index:= UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full(UnZ_Glob.slide_index);
        end loop;
        if some_trace then
          Ada.Text_IO.Put_Line("End   Inflate_stored_block");
        end if;
      end Inflate_stored_block;

      -- Copy lengths for literal codes 257..285

      copy_lengths_literal : Length_array( 0..30 ) :=
           (  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
             35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );

      -- Extra bits for literal codes 257..285

      extra_bits_literal : Length_array( 0..30 ) :=
             ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
               3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid, invalid );

      -- Copy offsets for distance codes 0..29 (30..31: deflate_e)

      copy_offset_distance : constant Length_array( 0..31 ) :=
           ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
             257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
             8193, 12289, 16385, 24577, 32769, 49153 );

      -- Extra bits for distance codes

      extra_bits_distance : constant Length_array( 0..31 ) :=
           ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
             7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14 );

      max_dist: Integer:= 29; -- changed to 31 for deflate_e

      length_list_for_fixed_block_literals: constant Length_array( 0..287 ):=
          ( 0..143=> 8, 144..255=> 9, 256..279=> 7, 280..287=> 8);

      procedure Inflate_fixed_block is
        Tl,                        --   literal/length code table
            Td : p_Table_list;            --  distance code table
        Bl, Bd : Integer;          --  lookup bits for tl/bd
        huft_incomplete : Boolean;
      begin
        if some_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_fixed_block");
        end if;
        --  Make a complete, but wrong [why ?] code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
        Bl := 7;
        HufT_build(
          length_list_for_fixed_block_literals, 257, copy_lengths_literal,
          extra_bits_literal, Tl, Bl, huft_incomplete
        );
        --  Make an incomplete code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
        Bd := 5;
        begin
          HufT_build(
            (0..max_dist => 5), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then
            if full_trace then
              Ada.Text_IO.Put_Line(
                "td is incomplete, pointer=null: " &
                Boolean'Image(Td=null)
              );
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free( Tl );
            raise Zip.Archive_corrupted;
        end;
        --  Decompress the block's data, until an end-of-block code.
        Inflate_Codes ( Tl, Td, Bl, Bd );
        --  Done with this block, free resources.
        HufT_free ( Tl );
        HufT_free ( Td );
        if some_trace then
          Ada.Text_IO.Put_Line("End   Inflate_fixed_block");
          lt_count_fix:= lt_count_fix + (lt_count-lt_count_0);
          dl_count_fix:= dl_count_fix + (dl_count-dl_count_0);
        end if;
      end Inflate_fixed_block;

      bit_order_for_dynamic_block : constant array ( 0..18 ) of Natural :=
         ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

      procedure Inflate_dynamic_block is

        Lbits : constant:= 9;
        Dbits : constant:= 6;

        current_length: Natural;
        defined, number_of_lengths: Natural;

        Tl,                             -- literal/length code tables
          Td : p_Table_list;            -- distance code tables

        CT     : p_HufT_table;       -- current table
        CT_idx : Natural;            -- current table's index

        Bl, Bd : Integer;                  -- lookup bits for tl/bd
        Nb : Natural;  -- number of bit length codes
        Nl : Natural;  -- number of literal length codes
        Nd : Natural;  -- number of distance codes

        -- literal/length and distance code lengths
        Ll: Length_array( 0 .. 288+32-1 ):= (others=> 0);

        huft_incomplete : Boolean;

        procedure Repeat_length_code( amount: Natural ) is
        begin
          if defined + amount > number_of_lengths then
            raise Zip.Archive_corrupted;
          end if;
          for c in reverse 1..amount loop
            Ll ( defined ) := current_length;
            defined:= defined + 1;
          end loop;
        end Repeat_length_code;

      begin
        if some_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_dynamic_block");
        end if;

        -- Read in table lengths
        Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump(4);

        if Nl > 288 or else Nd > 32 then
          raise Zip.Archive_corrupted;
        end if;

        -- Read in bit-length-code lengths for decoding the compression structure.
        -- The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
        for J in  0 .. Nb - 1  loop
          Ll ( bit_order_for_dynamic_block( J ) ) := UnZ_IO.Bit_buffer.Read_and_dump(3);
        end loop;

        -- Build decoding table for trees--single level, 7 bit lookup
        Bl := 7;
        begin
          HufT_build (
            Ll( 0..18 ), 19, empty, empty, Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free(Tl);
            Raise_Exception(Zip.Archive_corrupted'Identity, "Incomplete code set for compression structure");
          end if;
        exception
          when others =>
            Raise_Exception(Zip.Archive_corrupted'Identity, "Error when building tables for compression structure");
        end;

        -- Read in the compression structure: literal and distance code lengths
        number_of_lengths := Nl + Nd;
        defined := 0;
        current_length := 0;

        while  defined < number_of_lengths  loop
          if Tl = null then
            Raise_Exception(Zip.Archive_corrupted'Identity,
            "Null table list (on compression structure)");
          end if;
          CT:= Tl.table;
          CT_idx:= UnZ_IO.Bit_buffer.Read(Bl);
          UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );

          case CT(CT_idx).n is
            when 0 .. 15 =>     --  Length of code for symbol of index 'defined', in bits (0..15)
              current_length:= CT(CT_idx).n;
              Ll (defined) := current_length;
              defined:= defined + 1;
            when 16 =>          --  16 means: repeat last bit length 3 to 6 times
              if defined = 0 then
                --  Nothing in the Ll array has been defined so far. Then, current_length is
                --  (theoretically) undefined and cannot be repeated.
                --  This unspecified case is treated as an error by zlib's inflate.c.
                Raise_Exception (Zip.Archive_corrupted'Identity,
                  "Illegal data for compression structure (repeat an undefined code length)");
              end if;
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(2));
            when 17 =>          --  17 means: the next 3 to 10 symbols' codes have zero bit lengths
              current_length:= 0;
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(3));
            when 18 =>          --  18 means: the next 11 to 138 symbols' codes have zero bit lengths
              current_length:= 0;
              Repeat_length_code(11 + UnZ_IO.Bit_buffer.Read_and_dump(7));
            when others =>      --  Shouldn't occur if this tree is correct
              Raise_Exception(Zip.Archive_corrupted'Identity,
                "Illegal data for compression structure (values should be in the range 0 .. 18): "
                & Integer'Image(CT(CT_idx).n));
          end case;
        end loop;
        --  Free the Huffman tree that was used for decoding the compression
        --  structure, which is contained now in Ll.
        HufT_free ( Tl );
        if Ll (256) = 0 then
          --  No code length for the End-Of-Block symbol, implies infinite stream!
          --  This case is unspecified but obviously we must stop here.
          Raise_Exception(Zip.Archive_corrupted'Identity, "No code for End-Of-Block symbol #256");
        end if;
        --  Build the decoding tables for literal/length codes
        Bl := Lbits;
        begin
          HufT_build (
            Ll( 0..Nl-1 ), 257,
            copy_lengths_literal, extra_bits_literal,
            Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free(Tl);
            Raise_Exception(Zip.Archive_corrupted'Identity, "Incomplete code set for literals/lengths");
          end if;
        exception
          when others =>
            Raise_Exception(Zip.Archive_corrupted'Identity, "Error when building tables for literals/lengths");
        end;
        --  Build the decoding tables for distance codes
        Bd := Dbits;
        begin
          HufT_build (
            Ll( Nl..Nl+Nd-1 ), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then
            if deflate_strict then
              Raise_Exception(Zip.Archive_corrupted'Identity, "Incomplete code set for distances");
            elsif some_trace then  --  not deflate_strict => don't stop
              Ada.Text_IO.Put_Line("Huffman tree incomplete - PKZIP 1.93a bug workaround");
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free(Tl);
            Raise_Exception(Zip.Archive_corrupted'Identity, "Error when building tables for distances");
        end;
        --  Decompress the block's data, until an end-of-block code.
        Inflate_Codes ( Tl, Td, Bl, Bd );
        --  Done with this block, free resources.
        HufT_free ( Tl );
        HufT_free ( Td );
        if some_trace then
          Ada.Text_IO.Put_Line("End   Inflate_dynamic_block");
          lt_count_dyn:= lt_count_dyn + (lt_count-lt_count_0);
          dl_count_dyn:= dl_count_dyn + (dl_count-dl_count_0);
        end if;
      end Inflate_dynamic_block;

      procedure Inflate_Block( last_block: out Boolean; fix, dyn: in out Long_Integer ) is
      begin
        last_block:= Boolean'Val(UnZ_IO.Bit_buffer.Read_and_dump(1));
        case UnZ_IO.Bit_buffer.Read_and_dump(2) is -- Block type = 0,1,2,3
          when 0 =>      Inflate_stored_block;
          when 1 =>      Inflate_fixed_block;
                         fix:= fix + 1;
          when 2 =>      Inflate_dynamic_block;
                         dyn:= dyn + 1;
          when others => raise Zip.Archive_corrupted; -- Bad block type (3)
        end case;
      end Inflate_Block;

      procedure Inflate is
        is_last_block: Boolean;
        blocks, blocks_fix, blocks_dyn: Long_Integer:= 0;
      begin
        if deflate_e_mode then
          copy_lengths_literal(28):= 3; -- instead of 258
          extra_bits_literal(28):= 16;  -- instead of 0
          max_dist:= 31;
        end if;
        loop
          blocks:= blocks + 1;
          Inflate_Block ( is_last_block, blocks_fix, blocks_dyn );
          exit when is_last_block;
        end loop;
        UnZ_IO.Flush( UnZ_Glob.slide_index );
        UnZ_Glob.slide_index:= 0;
        if some_trace then
          Ada.Text_IO.Put_Line(
            "# blocks:" & Long_Integer'Image(blocks) &
            "; fixed:" & Long_Integer'Image(blocks_fix) &
            "; dynamic:" & Long_Integer'Image(blocks_dyn));
          if blocks_fix > 0 then
            Ada.Text_IO.Put_Line(
              "Averages per fixed block: literals:" & Long_Integer'Image(lt_count_fix / blocks_fix) &
              "; DL codes:" & Long_Integer'Image(dl_count_fix / blocks_fix) &
              "; all codes:" & Long_Integer'Image((lt_count_fix + dl_count_fix) / blocks_fix));
          end if;
          if blocks_dyn > 0 then
            Ada.Text_IO.Put_Line(
              "Averages per dynamic block: literals:" & Long_Integer'Image(lt_count_dyn / blocks_dyn) &
              "; DL codes:" & Long_Integer'Image(dl_count_dyn / blocks_dyn) &
              "; all codes:" & Long_Integer'Image((lt_count_dyn + dl_count_dyn) / blocks_dyn));
          end if;
        end if;
      end Inflate;

    end UnZ_Meth;

    procedure Process_descriptor(dd: out Zip.Headers.Data_descriptor) is
      start: Integer;
      b: Unsigned_8;
      dd_buffer: Zip.Byte_Buffer(1..30);
    begin
      UnZ_IO.Bit_buffer.Dump_to_byte_boundary;
      Set_mode(local_crypto_pack, clear); -- We are after compressed data, switch off decryption.
      b:= UnZ_IO.Read_byte_decrypted;
      if b = 75 then -- 'K' ('P' is before, this is a Java/JAR bug!)
        dd_buffer(1):= 80;
        dd_buffer(2):= 75;
        start:= 3;
      else
        dd_buffer(1):= b; -- hopefully = 80 (will be checked)
        start:= 2;
      end if;
      for i in start..16 loop
        dd_buffer(i) := UnZ_IO.Read_byte_decrypted;
      end loop;
      Zip.Headers.Copy_and_check( dd_buffer, dd );
    exception
      when Zip.Headers.bad_data_descriptor =>
        raise Zip.Archive_corrupted;
    end Process_descriptor;

    use Zip, UnZ_Meth;

  begin -- Decompress_Data
    if some_trace then
      Ada.Text_IO.Create(LZ77_dump, Ada.Text_IO.Out_File, "dump.lz77");
    end if;
    output_memory_access:= null;
    -- ^ this is an 'out' parameter, we have to set it anyway
    case mode is
      when write_to_binary_file =>
         Ada.Streams.Stream_IO.Create(UnZ_IO.out_bin_file,Ada.Streams.Stream_IO.Out_File, output_file_name,
                                      Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_text_file =>
         Ada.Text_IO.Create(UnZ_IO.out_txt_file, Ada.Text_IO.Out_File, output_file_name,
                               Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_memory =>
        output_memory_access:= new
          Ada.Streams.Stream_Element_Array(
            1 .. Ada.Streams.Stream_Element_Offset(hint.dd.uncompressed_size)
          );
        UnZ_Glob.uncompressed_index := output_memory_access'First;
      when write_to_stream | just_test =>
        null;
    end case;

    UnZ_Glob.compsize  := hint.dd.compressed_size;
    if UnZ_Glob.compsize > File_size_type'Last - 2 then -- This means: unknown size
      UnZ_Glob.compsize:= File_size_type'Last - 2;      -- Avoid wraparound in read_buffer
    end if;                                             -- From TT's version, 2008
    UnZ_Glob.uncompsize:= hint.dd.uncompressed_size;
    UnZ_IO.Init_Buffers;
    Set_mode(local_crypto_pack, clear);

    -- UnZip correct type
    begin
      case format is
        when store   => Copy_stored;
        when deflate | deflate_e =>
          UnZ_Meth.deflate_e_mode:= format = deflate_e;
          UnZ_Meth.Inflate;
        when others =>
          Raise_Exception(Unsupported_method'Identity,
             "Format/method " & PKZip_method'Image(format) & " not supported for decompression");
      end case;
    exception
      when others =>
        UnZ_IO.Delete_output;
        raise;
    end;
    UnZ_Glob.crc32val := Zip.CRC_Crypto.Final( UnZ_Glob.crc32val );
    -- Decompression done !

    if data_descriptor_after_data then -- Sizes and CRC at the end
      declare
        memo_uncomp_size: constant Unsigned_32:= hint.dd.uncompressed_size;
      begin
        Process_descriptor(hint.dd); -- CRC is for checking; sizes are for informing user
        if memo_uncomp_size < Unsigned_32'Last and then --
           memo_uncomp_size /= hint.dd.uncompressed_size
        then
          UnZ_IO.Delete_output;
          raise Uncompressed_size_Error;
        end if;
      end;
    end if;

    if hint.dd.crc_32 /= UnZ_Glob.crc32val then
      UnZ_IO.Delete_output;
      Raise_Exception(CRC_Error'Identity,
        "CRC stored in archive: " & Hexadecimal(hint.dd.crc_32) &
        "; CRC computed now: " & Hexadecimal(UnZ_Glob.crc32val));
    end if;

    case mode is
      when write_to_binary_file =>
        Ada.Streams.Stream_IO.Close( UnZ_IO.out_bin_file );
      when write_to_text_file =>
        Ada.Text_IO.Close( UnZ_IO.out_txt_file );
      when write_to_memory | write_to_stream | just_test =>
        null; -- Nothing to close!
    end case;
    if some_trace then
      Ada.Text_IO.Close(LZ77_dump);
    end if;

  exception
    when others => -- close the file in case of an error, if not yet closed
      case mode is -- or deleted
        when write_to_binary_file =>
          if Ada.Streams.Stream_IO.Is_Open( UnZ_IO.out_bin_file ) then
            Ada.Streams.Stream_IO.Close( UnZ_IO.out_bin_file );
          end if;
        when write_to_text_file =>
          if Ada.Text_IO.Is_Open( UnZ_IO.out_txt_file ) then
            Ada.Text_IO.Close( UnZ_IO.out_txt_file );
          end if;
        when write_to_memory | write_to_stream | just_test =>
          null; -- Nothing to close!
      end case;
      raise;
  end Decompress_data;

end UnZip.Decompress;
