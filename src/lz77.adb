--  There are three LZ77 encoders at choice here:
--
--    1/  LZ77_using_LZHuf, based on LZHuf
--
--    2/  LZ77_using_IZ, based on Info-Zip's Zip's deflate.c which is
--          actually the LZ77 part of Zip's compression.
--
--    3/  LZ77_using_BT4, based on LZMA SDK's BT4 algorithm.
--
--  Variant 1/, LZ77_using_LZHuf, is working since 2009. Two problems: it is slow
--     and not well adapted to the Deflate format (mediocre compression).
--
--  Variant 2/, LZ77_using_IZ, is much faster, and better suited for Deflate.
--     It has been added on 05-Mar-2016.
--     The code is tailored and optimized for a single set of
--     the String_buffer_size, Look_Ahead, Threshold LZ77 parameters - those for Deflate.
--
--  Variant 3/, LZ77_using_BT4, was added on 06-Sep-2016.
--     The seems to be the best match finder for LZMA on data of the >= 1 MB scale.

--  To do:
--
--  2/
--    - LZ77 / IZ: similar to the test with TOO_FAR, try to cluster distances around
--        values needing less extra bits (may not work at all...)
--    - LZ77 / IZ: tune TOO_FAR (max: 32767), see http://optipng.sf.net/pngtech/too_far.html
--        "TOO_FAR in zlib Is Not Too Far" for discussion

--  Legal licensing note:

--  Copyright (c) 2016 .. 2018 Gautier de Montmollin (maintainer of the Ada version)
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

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Unchecked_Deallocation;
with Interfaces;
use Interfaces;
with System;

package body Lz77 is

   --  System.Word_Size: 13.3(8): A word is the largest amount of storage
   --  that can be conveniently and efficiently manipulated by the hardware,
   --  given the implementation's run-time model
   Min_Bits_32 : constant := Integer'Max (32, System.Word_Size);
   Min_Bits_16 : constant := Integer'Max (16, System.Word_Size);

   --  We define an Integer type which is at least 32 bits, but n bits
   --  on a native n (> 32) bits architecture (no performance hit on 64+
   --  bits architectures)
   --  Integer_M16 not needed: Integer already guarantees 16 bits
   type Integer_M32 is range -2**(Min_Bits_32 - 1) .. 2**(Min_Bits_32 - 1) - 1;
   subtype Natural_M32 is Integer_M32 range 0 .. Integer_M32'Last;

   type Unsigned_M16 is mod 2**Min_Bits_16;
   type Unsigned_M32 is mod 2**Min_Bits_32;

   procedure Encode is

      -----------------------
      --  LZHuf algorithm  --
      -----------------------

      procedure Lz77_Using_Lzhuf is
         --  Based on LZHUF by OKUMURA & YOSHIZAKI.
         --  Here the adaptive Huffman coding is thrown away:
         --  algorithm is used only to find matching patterns

         N_Char : constant Integer := 256 - Threshold + Look_Ahead;
         --  Character code (= 0..N_CHAR-1)
         Max_Table : constant Integer := N_Char * 2 - 1;

         type Text_Buffer is array (0 .. String_Buffer_Size + Look_Ahead - 1) of Byte;
         Empty_Buffer : constant Text_Buffer := (others => 32); -- ' '

         --  > The Huffman frequency handling is made generic so we have
         --   one copy of the tree and of the frequency table for Encode
         --   and one for Decode

         generic
         package Huffman is
            --- Pointing parent nodes
            --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
            Parent : array (0 .. Max_Table + N_Char - 1) of Natural;
            --- Pointing children nodes (son[], son[] + 1)
            Son : array (0 .. Max_Table - 1) of Natural;

            Root_Position : constant Integer := Max_Table - 1; -- (can be always Son'last ?)

            procedure Start;
            procedure Update_Freq_Tree (C0 : Natural);
         end Huffman;

         package body Huffman is

            Freq : array (0 .. Max_Table) of Natural; -- Cumulative freq table

            Max_Freq : constant := 16#8000#;
            --  ^-- update when cumulative frequency reaches to this value

            procedure Start is
               I : Natural;
            begin
               for J in 0 .. N_Char - 1 loop
                  Freq (J)               := 1;
                  Son (J)                := J + Max_Table;
                  Parent (J + Max_Table) := J;
               end loop;

               I := 0;
               for J in N_Char .. Root_Position loop
                  Freq (J)       := Freq (I) + Freq (I + 1);
                  Son (J)        := I;
                  Parent (I)     := J;
                  Parent (I + 1) := J;
                  I              := I + 2;
               end loop;

               Freq (Freq'Last)       := 16#FFFF#; -- ( Max_Table )
               Parent (Root_Position) := 0;
            end Start;

            procedure Update_Freq_Tree (C0 : Natural) is

               procedure Reconstruct_Freq_Tree is
                  I, J, K, F, L : Natural;
               begin
                  --  Halven cumulative freq for leaf nodes
                  J := 0;
                  for I in 0 .. Root_Position loop
                     if Son (I) >= Max_Table then
                        Freq (J) := (Freq (I) + 1) / 2;
                        Son (J)  := Son (I);
                        J        := J + 1;
                     end if;
                  end loop;

                  --  Make a tree : first, connect children nodes
                  I := 0;
                  for J in N_Char .. Root_Position loop -- J : free nodes
                     K        := I + 1;
                     F        := Freq (I) + Freq (K); -- new frequency
                     Freq (J) := F;
                     K        := J - 1;
                     while F < Freq (K) loop
                        K := K - 1;
                     end loop;

                     K := K + 1;
                     L := J - K; -- 2007: fix: was L:= (J-K)*2, memcopy parameter remain

                     Freq (K + 1 .. K + L) := Freq (K .. K + L - 1); -- shift by one cell right
                     Freq (K)              := F;
                     Son (K + 1 .. K + L)  := Son (K .. K + L - 1); -- shift by one cell right
                     Son (K)               := I;
                     I                     := I + 2;
                  end loop;

                  --  Connect parent nodes
                  for I in 0 .. Max_Table - 1 loop
                     K          := Son (I);
                     Parent (K) := I;
                     if K < Max_Table then
                        Parent (K + 1) := I;
                     end if;
                  end loop;

               end Reconstruct_Freq_Tree;

               C, I, J, K, L : Natural;

            begin -- Update_Freq_Tree;
               if Freq (Root_Position) = Max_Freq then
                  Reconstruct_Freq_Tree;
               end if;
               C := Parent (C0 + Max_Table);
               loop
                  Freq (C) := Freq (C) + 1;
                  K        := Freq (C);
                  --  Swap nodes to keep the tree freq-ordered
                  L := C + 1;
                  if K > Freq (L) then
                     while K > Freq (L + 1) loop
                        L := L + 1;
                     end loop;

                     Freq (C) := Freq (L);
                     Freq (L) := K;

                     I          := Son (C);
                     Parent (I) := L;
                     if I < Max_Table then
                        Parent (I + 1) := L;
                     end if;

                     J       := Son (L);
                     Son (L) := I;

                     Parent (J) := C;
                     if J < Max_Table then
                        Parent (J + 1) := C;
                     end if;
                     Son (C) := J;

                     C := L;
                  end if;
                  C := Parent (C);
                  exit when C = 0;
               end loop;        -- do it until reaching the root
            end Update_Freq_Tree;

         end Huffman;

         Node_Nil : constant Integer := String_Buffer_Size;    -- End of tree's node

         Lson, Dad : array (0 .. String_Buffer_Size) of Natural;
         Rson      : array (0 .. String_Buffer_Size + 256) of Natural;

         procedure Init_Tree is
         begin
            for I in String_Buffer_Size + 1 .. Rson'Last loop
               Rson (I) := Node_Nil;
            end loop; -- root
            for I in 0 .. String_Buffer_Size - 1 loop
               Dad (I) := Node_Nil;
            end loop; -- node
         end Init_Tree;

         Match_Position : Natural;
         Match_Length   : Natural;

         Text_Buf : Text_Buffer := Empty_Buffer;

         procedure Insert_Node (R : Integer) is
            I, P : Integer;
            Geq  : Boolean := True;
            C    : Natural;
         begin
            P            := String_Buffer_Size + 1 + Integer (Text_Buf (R));
            Rson (R)     := Node_Nil;
            Lson (R)     := Node_Nil;
            Match_Length := 0;
            loop
               if Geq then
                  if Rson (P) = Node_Nil then
                     Rson (P) := R;
                     Dad (R)  := P;
                     return;
                  end if;
                  P := Rson (P);
               else
                  if Lson (P) = Node_Nil then
                     Lson (P) := R;
                     Dad (R)  := P;
                     return;
                  end if;
                  P := Lson (P);
               end if;
               I := 1;
               while I < Look_Ahead and then Text_Buf (R + I) = Text_Buf (P + I) loop
                  I := I + 1;
               end loop;

               Geq := Text_Buf (R + I) >= Text_Buf (P + I) or I = Look_Ahead;

               if I > Threshold then
                  if I > Match_Length then
                     Match_Position := (R - P) mod String_Buffer_Size - 1;
                     Match_Length   := I;
                     exit when Match_Length >= Look_Ahead;
                  end if;
                  if I = Match_Length then
                     C := (R - P) mod String_Buffer_Size - 1;
                     if C < Match_Position then
                        Match_Position := C;
                     end if;
                  end if;
               end if;
            end loop;

            Dad (R)        := Dad (P);
            Lson (R)       := Lson (P);
            Rson (R)       := Rson (P);
            Dad (Lson (P)) := R;
            Dad (Rson (P)) := R;
            if Rson (Dad (P)) = P then
               Rson (Dad (P)) := R;
            else
               Lson (Dad (P)) := R;
            end if;
            Dad (P) := Node_Nil; -- remove P
         end Insert_Node;

         procedure Delete_Node (P : Natural) is
            Q : Natural;
         begin
            if Dad (P) = Node_Nil then  -- unregistered
               return;
            end if;
            if Rson (P) = Node_Nil then
               Q := Lson (P);
            elsif Lson (P) = Node_Nil then
               Q := Rson (P);
            else
               Q := Lson (P);
               if Rson (Q) /= Node_Nil then
                  loop
                     Q := Rson (Q);
                     exit when Rson (Q) = Node_Nil;
                  end loop;
                  Rson (Dad (Q)) := Lson (Q);
                  Dad (Lson (Q)) := Dad (Q);
                  Lson (Q)       := Lson (P);
                  Dad (Lson (P)) := Q;
               end if;
               Rson (Q)       := Rson (P);
               Dad (Rson (P)) := Q;
            end if;
            Dad (Q) := Dad (P);
            if Rson (Dad (P)) = P then
               Rson (Dad (P)) := Q;
            else
               Lson (Dad (P)) := Q;
            end if;
            Dad (P) := Node_Nil;
         end Delete_Node;

         package Huffman_E is new Huffman;

         I, R, S, Last_Match_Length : Natural;
         Len                        : Integer;
         C                          : Byte;
      begin
         if not More_Bytes then
            return;
         end if;
         Huffman_E.Start;
         Init_Tree;
         S   := 0;
         R   := String_Buffer_Size - Look_Ahead;
         Len := 0;
         while Len < Look_Ahead and More_Bytes loop
            Text_Buf (R + Len) := Read_Byte;
            Len                := Len + 1;
         end loop;

         --  Seems: fill dictionary with default value
         --
         --  for I in 1.. Look_Ahead loop
         --    Insert_Node(R - I);
         --  end loop;

         Insert_Node (R);

         loop
            if Match_Length > Len then
               Match_Length := Len;
            end if;
            if Match_Length <= Threshold then
               Match_Length := 1;
               Huffman_E.Update_Freq_Tree (Natural (Text_Buf (R)));
               Write_Literal (Text_Buf (R));
            else
               Write_Dl_Code (Match_Position + 1, Match_Length);
            end if;
            Last_Match_Length := Match_Length;
            I                 := 0;
            while I < Last_Match_Length and More_Bytes loop
               I := I + 1;
               Delete_Node (S);
               C            := Read_Byte;
               Text_Buf (S) := C;
               if S < Look_Ahead - 1 then
                  Text_Buf (S + String_Buffer_Size) := C;
               end if;
               S := (S + 1) mod String_Buffer_Size;
               R := (R + 1) mod String_Buffer_Size;
               Insert_Node (R);
            end loop;

            while I < Last_Match_Length loop
               I := I + 1;
               Delete_Node (S);
               S   := (S + 1) mod String_Buffer_Size;
               R   := (R + 1) mod String_Buffer_Size;
               Len := Len - 1;
               if Len > 0 then
                  Insert_Node (R);
               end if;
            end loop;

            exit when Len = 0;
         end loop;
      end Lz77_Using_Lzhuf;

      --------------------------
      --  Info-Zip algorithm  --
      --------------------------

      --  LZ77_using_IZ: based on deflate.c by Jean-Loup Gailly.
      --  Core description of the algorithm:
      --
      --     The most straightforward technique turns out to be the fastest for
      --     most input files: try all possible matches and select the longest.
      --     The key feature of this algorithm is that insertions into the string
      --     dictionary are very simple and thus fast, and deletions are avoided
      --     completely. Insertions are performed at each input character, whereas
      --     string matches are performed only when the previous match ends. So it
      --     is preferable to spend more time in matches to allow very fast string
      --     insertions and avoid deletions. The matching algorithm for small
      --     strings is inspired from that of Rabin & Karp [1]. A brute force approach
      --     is used to find longer strings when a small match has been found.
      --
      --     The idea of lazy evaluation of matches is due to Jan-Mark Wams.
      --
      --     [1] A description of the Rabin and Karp algorithm is given in the book
      --         "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
      --
      --  About hashing: chapter 6.4 of The Art of Computer Programming, Volume 3, D.E. Knuth
      --  Rabin and Karp algorithm: http://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm

      --  Compression level: 0: store, 1: best speed, 9: best compression, 10: variant of level 9
      --  Ada code: only levels 4 to 10 are supported.

      procedure Lz77_Using_Iz (Level : Natural) is
         Hash_Bits : constant              := 15;  --  13..15
         Hash_Size : constant              := 2**Hash_Bits;
         Hash_Mask : constant              := Hash_Size - 1;
         Wsize     : constant Integer_M32  := Integer_M32 (String_Buffer_Size);
         Wmask     : constant Unsigned_M16 := Unsigned_M16 (Wsize - 1);
         --  HASH_SIZE and WSIZE must be powers of two
         Nil     : constant := 0;  --  Tail of hash chains
         Too_Far : constant :=
           4096;  --  Matches of length 3 are discarded if their distance exceeds TOO_FAR
         --
         subtype Ulg is Unsigned_M32;
         subtype Unsigned is Unsigned_M16;
         subtype Ush is Unsigned_M16;
         --  subtype long is Integer_M32;
         --  subtype int is Integer;
         subtype Pos is Unsigned_M32;  --  must be at least 32 bits
         --  subtype IPos is unsigned;
         --  A Pos is an index in the character window. IPos is used only for parameter passing.
         Window : array (0 .. 2 * Wsize - 1) of Byte;
         --  Sliding window. Input bytes are read into the second half of the window,
         --  and move to the first half later to keep a dictionary of at least WSIZE
         --  bytes. With this organization, matches are limited to a distance of
         --  WSIZE-MAX_MATCH bytes, but this ensures that IO is always
         --  performed with a length multiple of the block size.
         Prev : array (0 .. Unsigned (Wsize - 1)) of Pos;
         --  Link to older string with same hash index.
         --  This link is maintained only for the last 32K strings.
         --  An index in this array is thus a window index modulo 32K.
         Head : array (0 .. Unsigned (Hash_Size - 1)) of Pos;
         --  Heads of the hash chains or NIL.
         Window_Size : Ulg;
         --  window size, 2*WSIZE except for MMAP or BIG_MEM, where it is the
         --  input file length plus MIN_LOOKAHEAD.
         Sliding : Boolean;  --  Set to False when the input file is already in memory  [was: int]
         Ins_H     : Unsigned;   --  hash index of string to be inserted
         Min_Match : constant Integer_M32 := Integer_M32 (Threshold) + 1;  --  Deflate: 3
         Max_Match : constant Integer_M32 := Integer_M32 (Look_Ahead);     --  Deflate: 258
         --  Minimum amount of lookahead, except at the end of the input file.
         Min_Lookahead : constant Integer_M32 := Max_Match + Min_Match + 1;  --  Deflate: 262
      --  This LZ77 compression doesn't use the full possible distance range: 32507..32768 unused!
         Max_Dist : constant Integer_M32 := Wsize - Min_Lookahead;  --  Deflate: 32506
         H_Shift  : constant Integer     := Integer ((Hash_Bits + Min_Match - 1) / Min_Match);
         --  Number of bits by which ins_h and del_h must be shifted at each
         --  input step. It must be such that after MIN_MATCH steps, the oldest
         --  byte no longer takes part in the hash key, that is:
         --  H_SHIFT * MIN_MATCH >= HASH_BITS
         Prev_Length : Natural_M32; --  [was: unsigned]
         --  Length of the best match at previous step. Matches not greater than this
         --  are discarded. This is used in the lazy match evaluation.
         Strstart         : Natural_M32;   --  start of string to insert [was: unsigned]
         Match_Start      : Natural_M32;   --  start of matching string [was: unsigned]
         Eofile           : Boolean;       --  flag set at end of input file [was: int]
         Lookahead : Natural_M32;   --  number of valid bytes ahead in window  [was: unsigned]
         Max_Chain_Length : Unsigned;
         --  To speed up deflation, hash chains are never searched beyond this length.
         --  A higher limit improves compression ratio but degrades the speed.
         Max_Lazy_Match : Natural_M32;  --  [was: unsigned]
         --  Attempt to find a better match only when the current match is strictly
         --  smaller than this value. This mechanism is used only for compression
         --  levels >= 4.
         Good_Match : Natural_M32;  --  [was: unsigned]
         --  Use a faster search when the previous match is longer than this
         Nice_Match : Integer_M32;  --  Stop searching when current match exceeds this
         --  Values for max_lazy_match, good_match, nice_match and max_chain_length,
         --  depending on the desired pack level (0..9). The values given below have
         --  been tuned to exclude worst case performance for pathological files.
         --  Better values may be found for specific files.
         type Config is record
            Good_Length : Natural_M32;  --  reduce lazy search above this match length [was: ush]
            Max_Lazy    : Natural_M32;  --  do not perform lazy search above this match length
            Nice_Length : Integer_M32;  --  quit search above this match length
            Max_Chain   : Ush;
         end record;

         Configuration_Table : constant array (0 .. 10) of Config :=
           (
         --  good lazy nice chain
         (0, 0, 0, 0),    --  0: store only
            (4, 4, 8, 4),    --  1: maximum speed, no lazy matches
            (4, 5, 16, 8),
            (4, 6, 32, 32),
            (4, 4, 16, 16),    --  4: lazy matches
            (8, 16, 32, 32),
            (8, 16, 128, 128),
            (8, 32, 128, 256),
            (32, 128, 258, 1024),
            (32, 258, 258, 4096),   --  9: maximum compression
            (34, 258, 258, 4096));  --  "secret" variant of level 9

         --  Update a hash value with the given input byte
         --  IN  assertion: all calls to to UPDATE_HASH are made with consecutive
         --     input characters, so that a running hash key can be computed from the
         --     previous key instead of complete recalculation each time.

         procedure Update_Hash (H : in out Unsigned; C : Byte) is
            pragma Inline (Update_Hash);
         begin
            H := (Unsigned (Shift_Left (Unsigned_32 (H), H_Shift)) xor Unsigned (C)) and Hash_Mask;
         end Update_Hash;

         --  Insert string starting at s in the dictionary and set match_head to the previous head
         --  of the hash chain (the most recent string with same hash key). Return
         --  the previous length of the hash chain.
         --  IN  assertion: all calls to to INSERT_STRING are made with consecutive
         --     input characters and the first MIN_MATCH bytes of s are valid
         --     (except for the last MIN_MATCH-1 bytes of the input file).

         procedure Insert_String (S : Integer_M32; Match_Head : out Natural_M32) is
            pragma Inline (Insert_String);
         begin
            Update_Hash (Ins_H, Window (S + Min_Match - 1));
            Match_Head                    := Natural_M32 (Head (Ins_H));
            Prev (Unsigned (S) and Wmask) := Pos (Match_Head);
            Head (Ins_H)                  := Pos (S);
         end Insert_String;

         procedure Read_Buf (From : Integer_M32; Amount : Unsigned; Actual : out Integer_M32) is
            Need : Unsigned := Amount;
         begin
            --  put_line("Read buffer: from:" & from'img & ";  amount:" & amount'img);
            Actual := 0;
            while Need > 0 and then More_Bytes loop
               Window (From + Actual) := Read_Byte;
               Actual                 := Actual + 1;
               Need                   := Need - 1;
            end loop;
            --  put_line("Read buffer: actual:" & actual'img);
         end Read_Buf;

         --  Fill the window when the lookahead becomes insufficient.
         --  Updates strstart and lookahead, and sets eofile if end of input file.
         --
         --  IN assertion: lookahead < MIN_LOOKAHEAD && strstart + lookahead > 0
         --  OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
         --     At least one byte has been read, or eofile is set; file reads are
         --     performed for at least two bytes (required for the translate_eol option).

         procedure Fill_Window is
            More : Unsigned;
            M    : Pos;
            N    : Natural_M32;
         begin
            loop
               More := Unsigned (Window_Size - Ulg (Lookahead) - Ulg (Strstart));
               if False
               then  --  C: "if (more == (unsigned)EOF) {" ?... GdM: seems a 16-bit code for EOF
                  --  Very unlikely, but possible on 16 bit machine if strstart == 0
                  --  and lookahead == 1 (input done one byte at time)
                  More := More - 1;
               elsif Strstart >= Wsize + Max_Dist and then Sliding then
                  --  By the IN assertion, the window is not empty so we can't confuse
                  --  more == 0 with more == 64K on a 16 bit machine.
                  Window (0 .. Wsize - 1) := Window (Wsize .. 2 * Wsize - 1);
            --  GdM: in rare cases (e.g. level 9 on test file "enwik8"), match_start happens
            --  to be < WSIZE. We do as in the original 16-bit C code: mod 2**16, such that the
            --  index is the window's range.
            --  This assumes WSIZE = 2**15, which is checked at startup of LZ77_using_IZ.
            --  Very likely, match_start is garbage anyway - see http://sf.net/p/infozip/bugs/49/
                  Match_Start :=
                    Natural_M32 (Unsigned_16 (Match_Start) - Unsigned_16 (Wsize mod (2**16)));
                  Strstart := Strstart - Wsize; -- we now have strstart >= MAX_DIST:
                  for Nn in 0 .. Unsigned'(Hash_Size - 1) loop
                     M := Head (Nn);
                     if M >= Pos (Wsize) then
                        Head (Nn) := M - Pos (Wsize);
                     else
                        Head (Nn) := Nil;
                     end if;
                  end loop;
                  --
                  for Nn in 0 .. Unsigned (Wsize - 1) loop
                     M := Prev (Nn);
                     if M >= Pos (Wsize) then
                        Prev (Nn) := M - Pos (Wsize);
                     else
                        Prev (Nn) := Nil;
                     end if;
            --  If n is not on any hash chain, prev[n] is garbage but its value will never be used.
                  end loop;
                  More := More + Unsigned (Wsize);
               end if;
               exit when Eofile;
               --  If there was no sliding:
               --     strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
               --     more == window_size - lookahead - strstart
               --  => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
               --  => more >= window_size - 2*WSIZE + 2
               --  In the MMAP or BIG_MEM case (not yet supported in gzip),
               --    window_size == input_size + MIN_LOOKAHEAD  &&
               --    strstart + lookahead <= input_size => more >= MIN_LOOKAHEAD.
               --  Otherwise, window_size == 2*WSIZE so more >= 2.
               --  If there was sliding, more >= WSIZE. So in all cases, more >= 2.
               --
               --  Assert(more >= 2, "more < 2");
               --
               Read_Buf (Strstart + Lookahead, More, N);
               if N = 0 then
                  Eofile := True;
               else
                  Lookahead := Lookahead + N;
               end if;
               exit when Lookahead >= Min_Lookahead or Eofile;
            end loop;
            --  put_line("Fill done - eofile = " & eofile'img);
         end Fill_Window;

         --  Initialize the "longest match" routines for a new file
         --
         --  IN assertion: window_size is > 0 if the input file is already read or
         --     mapped in the window array, 0 otherwise. In the first case,
         --     window_size is sufficient to contain the whole input file plus
         --     MIN_LOOKAHEAD bytes (to avoid referencing memory beyond the end
         --     of window when looking for matches towards the end).

         procedure Lm_Init (Pack_Level : Natural) is
         begin
            --  Do not slide the window if the whole input is already in
            --  memory (window_size > 0)
            Sliding := False;
            if Window_Size = 0 then
               Sliding     := True;
               Window_Size := 2 * Ulg (Wsize);
            end if;
            --  Initialize the hash table.
            --  prev will be initialized on the fly.
            Head := (others => Nil);
            --  Set the default configuration parameters:
            Max_Lazy_Match   := Configuration_Table (Pack_Level).Max_Lazy;
            Good_Match       := Configuration_Table (Pack_Level).Good_Length;
            Nice_Match       := Configuration_Table (Pack_Level).Nice_Length;
            Max_Chain_Length := Configuration_Table (Pack_Level).Max_Chain;
            --  Info-Zip comment: ??? reduce max_chain_length for binary files
            Strstart := 0;
            Read_Buf (0, Unsigned (Wsize), Lookahead);
            if Lookahead = 0 then
               Eofile := True;
               return;
            end if;
            Eofile := False;
            --  Make sure that we always have enough lookahead. This is important
            --  if input comes from a device such as a tty.
            if Lookahead < Min_Lookahead then
               Fill_Window;
            end if;
            Ins_H := 0;
            for J in 0 .. Natural_M32 (Min_Match) - 2 loop
               Update_Hash (Ins_H, Window (J));
            end loop;
            --  If lookahead < MIN_MATCH, ins_h is garbage, but this is
            --  not important since only literal bytes will be emitted.
         end Lm_Init;

         --  Set match_start to the longest match starting at the given string and
         --  return its length. Matches shorter or equal to prev_length are discarded,
         --  in which case the result is equal to prev_length and match_start is
         --  garbage.
         --  IN assertions: current_match is the head of the hash chain for the current
         --    string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1

         procedure Longest_Match (Current_Match : in out Integer_M32; Longest : out Integer_M32) is
            Chain_Length : Unsigned             := Max_Chain_Length;  --  max hash chain length
            Scan         : Integer_M32          := Strstart;       --  current string
            Match        : Integer_M32;                   --  matched string
            Len          : Integer_M32;                   --  length of current match
            Best_Len     : Integer_M32          := Prev_Length;    --  best match length so far
            Limit        : Natural_M32;  --  [was: IPos]
            Strend       : constant Integer_M32 := Strstart + Max_Match;
            Scan_End     : Integer_M32          := Scan + Best_Len;
         begin
            --  Stop when current_match becomes <= limit. To simplify the code,
            --  we prevent matches with the string of window index 0.
            if Strstart > Max_Dist then
               Limit := Strstart - Max_Dist;
            else
               Limit := Nil;
            end if;
            --  Do not waste too much time if we already have a good match:
            if Prev_Length >= Good_Match then
               Chain_Length := Chain_Length / 4;
            end if;
            --  Assert(strstart <= window_size-MIN_LOOKAHEAD, "insufficient lookahead");
            loop
               --  Assert(current_match < strstart, "no future");
               Match := Current_Match;
               --  Skip to next match if the match length cannot increase
               --  or if the match length is less than 2:
               --
               --  NB: this is the Not-UNALIGNED_OK variant in the C code.
               --      Translation of the UNALIGNED_OK variant is left as an exercise ;-).
               --      (!! worth a try: GNAT optimizes window(match..match+1[3]) to 16[32] bit)
               --
               if Window (Match + Best_Len) /= Window (Scan_End)
                 or else Window (Match + Best_Len - 1) /= Window (Scan_End - 1)
                 or else Window (Match) /= Window (Scan)
                 or else Window (Match + 1) /= Window (Scan + 1)
               then
                  Match := Match + 1;  --  C: continue
               else
                  --  The check at best_len - 1 can be removed because it will be made
                  --  again later. (This heuristic is not always a win.)
                  --
                  --  It is not necessary to compare window(scan + 2) and
                  --  window(match + 2) since they are always equal when
                  --  the other bytes match, given that the hash keys are
                  --  equal and that HASH_BITS >= 8
                  Scan  := Scan + 2;
                  Match := Match + 2;
                  --  C: The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
                  --     It is easy to get rid of this optimization if necessary
                  --  Ada: see the "else" part below
                  if Max_Match = 258 then
                     --  We check for insufficient lookahead only every 8th comparison;
                     --  the 256th check will be made at strstart + 258
                     loop
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match) or else Scan >= Strend;
                     end loop;
                  else
                     --  We check for insufficient lookahead after every comparison
                     loop
                        Scan  := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match) or else Scan >= Strend;
                     end loop;
                  end if;
                  --  Assert(scan <= window+(unsigned)(window_size-1), "wild scan");
                  Len  := Max_Match - (Strend - Scan);
                  Scan := Strend - Max_Match;
                  if Len > Best_Len then
                     Match_Start := Current_Match;
                     Best_Len    := Len;
                     exit when Len >= Nice_Match;
                     Scan_End := Scan + Best_Len;
                  end if;
               end if;
               Current_Match := Integer_M32 (Prev (Unsigned (Current_Match) and Wmask));
               exit when Current_Match <= Limit;
               Chain_Length := Chain_Length - 1;
               exit when Chain_Length = 0;
            end loop;
            Longest := Best_Len;
         end Longest_Match;

         procedure Lz77_Part_Of_Iz_Deflate is
            Hash_Head       : Natural_M32 := Nil;            --  head of hash chain
            Prev_Match      : Natural_M32;                   --  previous match  [was: IPos]
            Match_Available : Boolean     := False;          --  set if previous match exists
            Match_Length    : Natural_M32 := Min_Match - 1;  --  length of best match
            Max_Insert      : Natural_M32;
         begin
            Match_Start := 0;  --  NB: no initialization in deflate.c
            --  NB: level <= 3 would call deflate_fast;
            --
            --  Process the input block
            while Lookahead /= 0 loop
               --  Insert the string window(strstart .. strstart + 2) in the
               --  dictionary, and set hash_head to the head of the hash chain:
               if Lookahead >= Min_Match then
                  Insert_String (Strstart, Hash_Head);
               end if;
               --  Find the longest match, discarding those <= prev_length
               Prev_Length  := Match_Length;
               Prev_Match   := Match_Start;
               Match_Length := Min_Match - 1;
               if Hash_Head /= Nil
                 and then Prev_Length < Max_Lazy_Match
                 and then Strstart - Hash_Head <= Max_Dist
               then
                  --  To simplify the code, we prevent matches with the string
                  --  of window index 0 (in particular we have to avoid a match
                  --  of the string with itself at the start of the input file).
                  --
                  --  Do not look for matches beyond the end of the input.
                  --  This is necessary to make deflate deterministic.
                  if Nice_Match > Lookahead then
                     Nice_Match := Lookahead;
                  end if;
                  Longest_Match (Hash_Head, Match_Length);
                  --  Longest_Match sets match_start
                  if Match_Length > Lookahead then
                     Match_Length := Lookahead;
                  end if;
                  --  Ignore a length 3 match if it is too distant:
                  if Match_Length = Min_Match and then Strstart - Match_Start > Too_Far then
                     --  If prev_match is also MIN_MATCH, match_start is garbage
                     --  but we will ignore the current match anyway
                     Match_Length := Min_Match - 1;
                  end if;
               end if;
               --  If there was a match at the previous step and the current
               --  match is not better, output the previous match:
               if Prev_Length >= Min_Match and then Match_Length <= Prev_Length then
                  Max_Insert := Strstart + Lookahead - Min_Match;
                  --  C: in DEBUG mode: check_match(strstart-1, prev_match, prev_length);
                  --
                  ------------------------------------
                  --  Output a Distance-Length code --
                  ------------------------------------
                  Write_Dl_Code (Positive (Strstart - 1 - Prev_Match), Positive (Prev_Length));
                  --  Insert in hash table all strings up to the end of the match.
                  --  strstart-1 and strstart are already inserted
                  Lookahead   := Lookahead - (Prev_Length - 1);
                  Prev_Length := Prev_Length - 2;
                  loop
                     Strstart := Strstart + 1;
                     if Strstart <= Max_Insert then
                        Insert_String (Strstart, Hash_Head);
                        --  strstart never exceeds WSIZE - MAX_MATCH, so there
                        --  are always MIN_MATCH bytes ahead
                     end if;
                     Prev_Length := Prev_Length - 1;
                     exit when Prev_Length = 0;
                  end loop;
                  Strstart        := Strstart + 1;
                  Match_Available := False;
                  Match_Length    := Min_Match - 1;
               elsif Match_Available then
                  --  If there was no match at the previous position, output a
                  --  single literal. If there was a match but the current match
                  --  is longer, truncate the previous match to a single literal

                  ------------------------
                  --  Output a literal  --
                  ------------------------
                  Write_Literal (Window (Strstart - 1));
                  Strstart  := Strstart + 1;
                  Lookahead := Lookahead - 1;
               else
                  --  There is no previous match to compare with, wait
                  --  for the next step to decide
                  Match_Available := True;
                  Strstart        := Strstart + 1;
                  Lookahead       := Lookahead - 1;
               end if;
               --  Assert(strstart <= isize && lookahead <= isize, "a bit too far");
               --
               --  Make sure that we always have enough lookahead, except
               --  at the end of the input file. We need MAX_MATCH bytes
               --  for the next match, plus MIN_MATCH bytes to insert the
               --  string following the next match.
               if Lookahead < Min_Lookahead then
                  Fill_Window;
               end if;
            end loop;

            -----------------------------------
            --  Output last literal, if any  --
            -----------------------------------
            if Match_Available then
               Write_Literal (Window (Strstart - 1));
            end if;
         end Lz77_Part_Of_Iz_Deflate;

         Code_Too_Clever : exception;
      begin
         if Look_Ahead /= 258 or String_Buffer_Size /= 2**15 or Threshold /= 2 then
            raise Code_Too_Clever;  --  was optimized for these parameters
         end if;
         Window_Size := 0;
         Lm_Init (Level);
         Lz77_Part_Of_Iz_Deflate;
      end Lz77_Using_Iz;

      ---------------------------------------------------------------------
      --  BT4  -  Binary tree of match positions selected with           --
      --          the leading 2 to 4 bytes of each possible match.       --
      ---------------------------------------------------------------------

      --  Based on BT4.java by Lasse Collin, itself based on LzFind.c by Igor Pavlov.

      procedure Lz77_Using_Bt4 is
         Match_Len_Min : constant Integer := Threshold + 1;
         --
         Readpos     : Integer          := -1;
         Cur_Literal : Byte;
         Readlimit   : Integer          := -1;
         Finishing   : constant Boolean := False;
         Writepos    : Integer          := 0;
         Pendingsize : Integer          := 0;
         --
         Opts              : constant := 4096;
         Extra_Size_Before : constant := Opts;
         Extra_Size_After  : constant := Opts;

         Keepsizebefore : constant Integer := Extra_Size_Before + String_Buffer_Size;
         Keepsizeafter  : constant Integer := Extra_Size_After + Look_Ahead;
         Reservesize    : constant Integer :=
           Integer'Min (String_Buffer_Size / 2 + 256 * (2**10),  --  256 KB
           512 * (2**20)   --  512 MB
           );
         Getbufsize : constant Integer := Keepsizebefore + Keepsizeafter + Reservesize;

         type Int_Array is array (Natural range <>) of Integer;
         type P_Int_Array is access Int_Array;
         procedure Dispose is new Ada.Unchecked_Deallocation (Int_Array, P_Int_Array);

         procedure Normalize (Positions : in out Int_Array; Normalizationoffset : Integer) is
         begin
            for I in 0 .. Positions'Length - 1 loop
               if Positions (I) <= Normalizationoffset then
                  Positions (I) := 0;
               else
                  Positions (I) := Positions (I) - Normalizationoffset;
               end if;
            end loop;
         end Normalize;

         function Getavail return Integer is
            pragma Inline (Getavail);
         begin
            --  !! - 1 added for getting readPos in buf'Range upon: cur_literal:= buf(readPos);
            return Writepos - Readpos - 1;
         end Getavail;

         function Movepos (Requiredforflushing, Requiredforfinishing : Integer) return Integer is
            Avail : Integer;
         begin
            --  assert requiredForFlushing >= requiredForFinishing;
            Readpos := Readpos + 1;
            Avail   := Getavail;
            if Avail < Requiredforflushing then
               if Avail < Requiredforfinishing or else not Finishing then
                  Pendingsize := Pendingsize + 1;
                  --  GdM: this causes cyclicPos and lzpos not being in sync with readPos,
                  --       the pendingSize value is there for catching up
                  Avail := 0;
               end if;
            end if;
            return Avail;
         end Movepos;

         function Gethash4size return Integer is
            H : Unsigned_32 := Unsigned_32 (String_Buffer_Size - 1);
         begin
            H := H or Shift_Right (H, 1);
            H := H or Shift_Right (H, 2);
            H := H or Shift_Right (H, 4);
            H := H or Shift_Right (H, 8);
            H := Shift_Right (H, 1);
            H := H or 16#FFFF#;  --  LzFind.c: "don't change it! It's required for Deflate"
            if H > 2**24 then
               H := Shift_Right (H, 1);
            end if;
            return Integer (H + 1);
         end Gethash4size;

         type Byte_Array is array (Natural range <>) of Byte;
         type P_Byte_Array is access Byte_Array;
         procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, P_Byte_Array);

         package Hash234 is
            Hash_2_Size : constant             := 2**10;
            Hash_2_Mask : constant             := Hash_2_Size - 1;
            Hash_3_Size : constant             := 2**16;
            Hash_3_Mask : constant             := Hash_3_Size - 1;
            Hash_4_Size : constant Integer     := Gethash4size;
            Hash_4_Mask : constant Unsigned_32 := Unsigned_32 (Hash_4_Size) - 1;
            --
            Hash2table : Int_Array (0 .. Hash_2_Size - 1) :=
              (others => 0);  --  !! initialization added
            Hash3table : Int_Array (0 .. Hash_3_Size - 1) :=
              (others => 0);  --  !! initialization added
            Hash4table : P_Int_Array;
            --
            Hash2value, Hash3value, Hash4value : Unsigned_32 := 0;
            --
            procedure Calchashes (Buf : Byte_Array; Off : Integer);
            procedure Updatetables (Pos : Integer);
            procedure Normalize (Normalizeoffset : Integer);
         end Hash234;

         package body Hash234 is

            Crctable   : array (Byte) of Unsigned_32;
            Crc32_Poly : constant := 16#EDB8_8320#;

            procedure Calchashes (Buf : Byte_Array; Off : Integer) is
               Temp : Unsigned_32 := Crctable (Buf (Off)) xor Unsigned_32 (Buf (Off + 1));
            begin
               Hash2value := Temp and Hash_2_Mask;
               Temp       := Temp xor Shift_Left (Unsigned_32 (Buf (Off + 2)), 8);
               Hash3value := Temp and Hash_3_Mask;
               Temp       := Temp xor Shift_Left (Crctable (Buf (Off + 3)), 5);
               Hash4value := Temp and Hash_4_Mask;
            end Calchashes;

            procedure Updatetables (Pos : Integer) is
            begin
               Hash2table (Integer (Hash2value)) := Pos;
               Hash3table (Integer (Hash3value)) := Pos;
               Hash4table (Integer (Hash4value)) := Pos;
            end Updatetables;

            procedure Normalize (Normalizeoffset : Integer) is
            begin
               Normalize (Hash2table, Normalizeoffset);
               Normalize (Hash3table, Normalizeoffset);
               Normalize (Hash4table.all, Normalizeoffset);
            end Normalize;

            R : Unsigned_32;
         begin
            --  NB: heap allocation used only for convenience because of
            --      small default stack sizes on some compilers
            Hash4table     := new Int_Array (0 .. Hash_4_Size - 1);
            Hash4table.all := (others => 0);  --  !! initialization added
            for I in Byte loop
               R := Unsigned_32 (I);
               for J in 0 .. 7 loop
                  if (R and 1) = 0 then
                     R := Shift_Right (R, 1);
                  else
                     R := Shift_Right (R, 1) xor Crc32_Poly;
                  end if;
               end loop;
               Crctable (I) := R;
            end loop;
         end Hash234;

         Nicelen    : constant Integer := Integer'Min (162, Look_Ahead);  --  const. was 64
         Depthlimit : constant         := 48;  --  Alternatively: 16 + niceLen / 2

         --  !! nicer: unconstr. array of (dist, len) pairs, 1-based array

         type Any_Matches_Type (Countmax : Integer) is record
            Count : Integer := 0;
            Len   : Int_Array (0 .. Countmax);
            Dist  : Int_Array (0 .. Countmax);
         end record;

         --  Subtracting 1 because the shortest match that this match
         --  finder can find is 2 bytes, so there's no need to reserve
         --  space for one-byte matches
         subtype Matches_Type is Any_Matches_Type (Nicelen - 1);

         Cyclicsize : constant Integer := String_Buffer_Size;  --  Had: + 1;
         Cyclicpos  : Integer          := -1;
         Lzpos      : Integer          := Cyclicsize;

         Max_Dist : constant Integer := Cyclicsize;

         package Bt4_Algo is
            procedure Skip (Len : Natural);
            pragma Inline (Skip);
            function Getmatches return Matches_Type;
         end Bt4_Algo;

         Buf  : P_Byte_Array;
         Tree : P_Int_Array;

         package body Bt4_Algo is

            function Movepos return Integer is
               Avail : constant Integer :=
                 Movepos (Requiredforflushing => Nicelen, Requiredforfinishing => 4);
               Normalizationoffset : Integer;
            begin
               --  Put_Line("BT4_movePos");
               if Avail /= 0 then
                  Lzpos := Lzpos + 1;
                  if Lzpos = Integer'Last then
                     Normalizationoffset := Integer'Last - Cyclicsize;
                     Hash234.Normalize (Normalizationoffset);
                     Normalize (Tree.all, Normalizationoffset);
                     Lzpos := Lzpos - Normalizationoffset;
                  end if;
                  Cyclicpos := Cyclicpos + 1;
                  if Cyclicpos = Cyclicsize then
                     --  Put_Line("cyclicPos zeroed");
                     Cyclicpos := 0;
                  end if;
               end if;
               return Avail;
            end Movepos;

            Null_Position : constant := -1;  --  LzFind.c: kEmptyHashValue, 0

            procedure Skip_Update_Tree (Nicelenlimit : Integer; Currentmatch : in out Integer) is
               Delta0, Depth, Ptr0, Ptr1, Pair, Len, Len0, Len1 : Integer;
            begin
               --  Put("BT4.skip_update_tree... ");
               Depth := Depthlimit;
               Ptr0  := Cyclicpos * 2 + 1;
               Ptr1  := Cyclicpos * 2;
               Len0  := 0;
               Len1  := 0;
               loop
                  Delta0 := Lzpos - Currentmatch;
                  if Depth = 0 or else Delta0 >= Max_Dist then
                     Tree (Ptr0) := Null_Position;
                     Tree (Ptr1) := Null_Position;
                     return;
                  end if;
                  Depth := Depth - 1;
                  if Cyclicpos - Delta0 < 0 then
                     Pair := Cyclicsize;
                  else
                     Pair := 0;
                  end if;
                  Pair := (Cyclicpos - Delta0 + Pair) * 2;
                  Len  := Integer'Min (Len0, Len1);
                  --  Match ?
                  if Buf (Readpos + Len - Delta0) = Buf (Readpos + Len) then
                     --  No need to look for longer matches than niceLenLimit
                     --  because we only are updating the tree, not returning
                     --  matches found to the caller
                     loop
                        Len := Len + 1;
                        if Len = Nicelenlimit then
                           Tree (Ptr1) := Tree (Pair);
                           Tree (Ptr0) := Tree (Pair + 1);
                           return;
                        end if;
                        exit when Buf (Readpos + Len - Delta0) /= Buf (Readpos + Len);
                     end loop;
                  end if;
                  --  Bytes are no more matching. The past value is either smaller...
                  if Buf (Readpos + Len - Delta0) < Buf (Readpos + Len) then
                     Tree (Ptr1)  := Currentmatch;
                     Ptr1         := Pair + 1;
                     Currentmatch := Tree (Ptr1);
                     Len1         := Len;
                  else  --  ... or larger
                     Tree (Ptr0)  := Currentmatch;
                     Ptr0         := Pair;
                     Currentmatch := Tree (Ptr0);
                     Len0         := Len;
                  end if;
               end loop;
            end Skip_Update_Tree;

            procedure Skip (Len : Natural) is
               procedure Skip_One is
                  pragma Inline (Skip_One);
                  Nicelenlimit, Avail, Currentmatch : Integer;
               begin
                  Nicelenlimit := Nicelen;
                  Avail        := Movepos;
                  if Avail < Nicelenlimit then
                     if Avail = 0 then
                        return;
                     end if;
                     Nicelenlimit := Avail;
                  end if;
                  Hash234.Calchashes (Buf.all, Readpos);
                  Currentmatch := Hash234.Hash4table (Integer (Hash234.Hash4value));
                  Hash234.Updatetables (Lzpos);
                  Skip_Update_Tree (Nicelenlimit, Currentmatch);
               end Skip_One;
            begin
               for Count in reverse 1 .. Len loop
                  Skip_One;
               end loop;
            end Skip;

            function Getmatches return Matches_Type is
               Matches       : Matches_Type;
               Matchlenlimit : Integer := Look_Ahead;
               Nicelenlimit  : Integer := Nicelen;
               Avail         : Integer;
               Delta0,
               Delta2,
               Delta3,
               Currentmatch,
               Lenbest,
               Depth,
               Ptr0,
               Ptr1,
               Pair,
               Len,
               Len0,
               Len1 : Integer;
            begin
               --  Put("BT4.getMatches... ");
               Matches.Count := 0;
               Avail         := Movepos;
               if Avail < Matchlenlimit then
                  if Avail = 0 then
                     return Matches;
                  end if;
                  Matchlenlimit := Avail;
                  if Nicelenlimit > Avail then
                     Nicelenlimit := Avail;
                  end if;
               end if;

               Hash234.Calchashes (Buf.all, Readpos);
               Delta2       := Lzpos - Hash234.Hash2table (Integer (Hash234.Hash2value));
               Delta3       := Lzpos - Hash234.Hash3table (Integer (Hash234.Hash3value));
               Currentmatch := Hash234.Hash4table (Integer (Hash234.Hash4value));
               Hash234.Updatetables (Lzpos);
               --
               Lenbest := 0;
               --  See if the hash from the first two bytes found a match.
               --  The hashing algorithm guarantees that if the first byte
               --  matches, also the second byte does, so there's no need to
               --  test the second byte.
               if Delta2 < Max_Dist and then Buf (Readpos - Delta2) = Buf (Readpos) then
                  --  Match of length 2 found and checked
                  Lenbest          := 2;
                  Matches.Len (0)  := 2;
                  Matches.Dist (0) := Delta2 - 1;
                  Matches.Count    := 1;
               end if;
               --  See if the hash from the first three bytes found a match that
               --  is different from the match possibly found by the two-byte hash.
               --  Also here the hashing algorithm guarantees that if the first byte
               --  matches, also the next two bytes do.
               if Delta2 /= Delta3
                 and then Delta3 < Max_Dist
                 and then Buf (Readpos - Delta3) = Buf (Readpos)
               then
                  --  Match of length 3 found and checked
                  Lenbest                          := 3;
                  Matches.Count                    := Matches.Count + 1;
                  Matches.Dist (Matches.Count - 1) := Delta3 - 1;
                  Delta2                           := Delta3;
               end if;
               --  If a match was found, see how long it is
               if Matches.Count > 0 then
                  while Lenbest < Matchlenlimit
                    and then Buf (Readpos + Lenbest - Delta2) = Buf (Readpos + Lenbest)
                  loop
                     Lenbest := Lenbest + 1;
                  end loop;
                  Matches.Len (Matches.Count - 1) := Lenbest;
                  --  Return if it is long enough (niceLen or reached the
                  --  end of the dictionary)
                  if Lenbest >= Nicelenlimit then
                     Skip_Update_Tree (Nicelenlimit, Currentmatch);
                     return Matches;
                  end if;
               end if;
               --  Long enough match wasn't found so easily. Look for better
               --  matches from the binary tree
               if Lenbest < 3 then
                  Lenbest := 3;
               end if;
               Depth := Depthlimit;
               Ptr0  := Cyclicpos * 2 + 1;
               Ptr1  := Cyclicpos * 2;
               Len0  := 0;
               Len1  := 0;
               --
               loop
                  Delta0 := Lzpos - Currentmatch;
                  --  Return if the search depth limit has been reached or
                  --  if the distance of the potential match exceeds the
                  --  dictionary size
                  if Depth = 0 or else Delta0 >= Max_Dist then
                     Tree (Ptr0) := Null_Position;
                     Tree (Ptr1) := Null_Position;
                     return Matches;
                  end if;
                  Depth := Depth - 1;

                  if Cyclicpos - Delta0 < 0 then
                     Pair := Cyclicsize;
                  else
                     Pair := 0;
                  end if;
                  Pair := (Cyclicpos - Delta0 + Pair) * 2;
                  Len  := Integer'Min (Len0, Len1);

                  --  Match ?
                  if Buf (Readpos + Len - Delta0) = Buf (Readpos + Len) then
                     loop
                        Len := Len + 1;
                        exit when Len >= Matchlenlimit
                          or else Buf (Readpos + Len - Delta0) /= Buf (Readpos + Len);
                     end loop;
                     if Len > Lenbest then
                        Lenbest                      := Len;
                        Matches.Len (Matches.Count)  := Len;
                        Matches.Dist (Matches.Count) := Delta0 - 1;
                        Matches.Count                := Matches.Count + 1;
                        if Len >= Nicelenlimit then
                           Tree (Ptr1) := Tree (Pair);
                           Tree (Ptr0) := Tree (Pair + 1);
                           return Matches;
                        end if;
                     end if;
                  end if;

                  --  Bytes are no more matching. The past value is either smaller...
                  if Buf (Readpos + Len - Delta0) < Buf (Readpos + Len) then
                     Tree (Ptr1)  := Currentmatch;
                     Ptr1         := Pair + 1;
                     Currentmatch := Tree (Ptr1);
                     Len1         := Len;
                  else  --  ... or larger
                     Tree (Ptr0)  := Currentmatch;
                     Ptr0         := Pair;
                     Currentmatch := Tree (Ptr0);
                     Len0         := Len;
                  end if;
               end loop;
            end Getmatches;

         begin
            --  NB: heap allocation used only for convenience because of
            --      small default stack sizes on some compilers
            Tree     := new Int_Array (0 .. Cyclicsize * 2 - 1);
            Tree.all := (others => Null_Position);
         end Bt4_Algo;

         --  Moves data from the end of the buffer to the beginning, discarding
         --  old data and making space for new input

         procedure Movewindow is
            --  Align the move to a multiple of 16 bytes (LZMA-friendly, see pos_bits)
            Moveoffset : constant Integer := ((Readpos + 1 - Keepsizebefore) / 16) * 16;
            Movesize   : constant Integer := Writepos - Moveoffset;
         begin
            --  Put_Line("  Move window, size=" & moveSize'Img & " offset=" & moveOffset'Img);
            Buf (0 .. Movesize - 1) := Buf (Moveoffset .. Moveoffset + Movesize - 1);
            Readpos                 := Readpos - Moveoffset;
            Readlimit               := Readlimit - Moveoffset;
            Writepos                := Writepos - Moveoffset;
         end Movewindow;

         --  Copies new data into the buffer
         function Fillwindow (Len_Initial : Integer) return Integer is
            --  Process pending data that hasn't been ran through the match finder yet.
            --  Run it through the match finder now if there is enough new data
            --  available (readPos < readLimit) that the encoder may encode at
            --  least one more input byte.
            procedure Processpendingbytes is
               Oldpendingsize : Integer;
            begin
               if Pendingsize > 0 and then Readpos < Readlimit then
                  Readpos        := Readpos - Pendingsize;
                  Oldpendingsize := Pendingsize;
                  Pendingsize    := 0;
                  Bt4_Algo.Skip (Oldpendingsize);
               end if;
            end Processpendingbytes;

            Len        : Integer := Len_Initial;
            Actual_Len : Integer := 0;
         begin
            --  Put_Line("Fill window - start");
            --  Move the sliding window if needed
            if Readpos >= Buf'Length - Keepsizeafter then
               Movewindow;
            end if;

            --  Try to fill the dictionary buffer up to its boundary
            if Len > Buf'Length - Writepos then
               Len := Buf'Length - Writepos;
            end if;

            while Len > 0 and then More_Bytes loop
               Buf (Writepos) := Read_Byte;
               Writepos       := Writepos + 1;
               Len            := Len - 1;
               Actual_Len     := Actual_Len + 1;
            end loop;

            --  Set the new readLimit but only if there's enough data to allow
            --  encoding of at least one more byte
            if Writepos >= Keepsizeafter then
               Readlimit := Writepos - Keepsizeafter;
            end if;

            Processpendingbytes;

         --  Put_Line("Fill window, requested=" & len_initial'Img & " actual=" & actual_len'Img);
            --  Tell the caller how much input we actually copied into
            --  the dictionary
            return Actual_Len;
         end Fillwindow;

         Matches   : Matches_Type;
         Readahead : Integer := -1;  -- LZMAEncoder.java

         function Getmatches return Matches_Type is
         begin
            Readahead := Readahead + 1;
            return Bt4_Algo.Getmatches;
         end Getmatches;

         procedure Skip (Len : Natural) is
            pragma Inline (Skip);
         begin
            Readahead := Readahead + Len;
            Bt4_Algo.Skip (Len);
         end Skip;

         --  Small stack of recent distances used for LZ
         subtype Repeat_Stack_Range is Integer range 0 .. 3;
         Rep_Dist : array (Repeat_Stack_Range) of Natural := (others => 0);

         procedure Getnextsymbol is
            Avail, Mainlen, Maindist, Newlen, Newdist, Limit : Integer;

            function Changepair (Smalldist, Bigdist : Integer) return Boolean is
               pragma Inline (Changepair);
            begin
               return Smalldist < Bigdist / 128;
            end Changepair;

            --  This function is for debugging. The matches stored in the 'tree' array
            --  may be wrong if the variables cyclicPos, lzPos and readPos are not in sync.
            --  The issue seems to have been solved now (rev. 489).
            function Is_Match_Correct (Shift : Natural) return Boolean is
               pragma Inline (Is_Match_Correct);
               Paranoid : constant Boolean := True;
            begin
               if Paranoid then
                  for I in reverse -1 + Shift .. Mainlen - 2 + Shift loop
                     if Buf (Readpos - (Maindist + 1) + I) /= Buf (Readpos + I) then
                        return False;  --  Should not occur (check with code coverage)
                     end if;
                  end loop;
               end if;
               return True;
            end Is_Match_Correct;

            function Getmatchlen (Dist, Lenlimit : Integer) return Natural is
               pragma Inline (Getmatchlen);
               Backpos : constant Integer := Readpos - Dist - 1;
               Len     : Integer          := 0;
            begin
               if Dist < 1 then
                  return 0;
               end if;
               --  @ if readPos+len not in buf.all'Range then
               --  @   Put("**** readpos " & buf'Last'Img & readPos'Img);
               --  @ end if;
               --  @ if backPos+len not in buf.all'Range then
               --  @   Put("**** backpos " & buf'Last'Img & backPos'Img);
               --  @ end if;
               while Len < Lenlimit and then Buf (Readpos + Len) = Buf (Backpos + Len) loop
                  Len := Len + 1;
               end loop;
               return Len;
            end Getmatchlen;

            procedure Send_First_Literal_Of_Match is
            begin
               Write_Literal (Cur_Literal);
               Readahead := Readahead - 1;
            end Send_First_Literal_Of_Match;

            procedure Send_Dl_Code (Distance, Length : Integer) is
               Found_Repeat : Integer := Rep_Dist'First - 1;
               Aux          : Integer;
            begin
               Write_Dl_Code (Distance + 1, Length);
               Readahead := Readahead - Length;
               if Lzma_Friendly then
                  --  Manage the stack of recent distances in the same way
                  --  the "MA" part of LZMA does
                  for I in Rep_Dist'Range loop
                     if Distance = Rep_Dist (I) then
                        Found_Repeat := I;
                        exit;
                     end if;
                  end loop;
                  if Found_Repeat >= Rep_Dist'First then
                     --  Roll the stack of recent distances up to the item
                     --  with index found_repeat, which becomes first.
                     --  If found_repeat = rep_dist'First, no actual change occurs
                     Aux := Rep_Dist (Found_Repeat);
                     for I in reverse Rep_Dist'First + 1 .. Found_Repeat loop
                        Rep_Dist (I) := Rep_Dist (I - 1);
                     end loop;
                     Rep_Dist (Rep_Dist'First) := Aux;
                  else
                     --  Shift the stack of recent distances; the new
                     --  distance becomes the first item
                     for I in reverse Rep_Dist'First + 1 .. Rep_Dist'Last loop
                        Rep_Dist (I) := Rep_Dist (I - 1);
                     end loop;
                     Rep_Dist (0) := Distance;
                  end if;
               end if;
            end Send_Dl_Code;

            Bestreplen, Bestrepindex, Len : Integer;

         begin
            --  Get the matches for the next byte unless readAhead indicates
            --  that we already got the new matches during the previous call
            --  to this procedure
            if Readahead = -1 then
               Matches := Getmatches;
            end if;
            --  @ if readPos not in buf.all'Range then
            --  @   Put("**** " & buf'Last'Img & keepSizeAfter'Img & readPos'Img & writePos'Img);
            --  @ end if;
            Cur_Literal := Buf (Readpos);
            --  Get the number of bytes available in the dictionary, but
            --  not more than the maximum match length. If there aren't
            --  enough bytes remaining to encode a match at all, return
            --  immediately to encode this byte as a literal.
            Avail := Integer'Min (Getavail, Look_Ahead);
            if Avail < Match_Len_Min then
               --  Put("[a]");
               Send_First_Literal_Of_Match;
               return;
            end if;

            if Lzma_Friendly then
               --  Look for a match from the previous four different match distances
               Bestreplen   := 0;
               Bestrepindex := 0;
               for Rep in Repeat_Stack_Range loop
                  Len := Getmatchlen (Rep_Dist (Rep), Avail);
                  if Len >= Match_Len_Min then
                     --  If it is long enough, return it
                     if Len >= Nicelen then
                        Skip (Len - 1);
                        --  Put_Line("[DL RA]");
                        Send_Dl_Code (Rep_Dist (Rep), Len);
                        return;
                     end if;
                     --  Remember the index and length of the best repeated match
                     if Len > Bestreplen then
                        Bestrepindex := Rep;
                        Bestreplen   := Len;
                     end if;
                  end if;
               end loop;
            end if;

            Mainlen  := 0;
            Maindist := 0;
            if Matches.Count > 0 then
               Mainlen  := Matches.Len (Matches.Count - 1);
               Maindist := Matches.Dist (Matches.Count - 1);
               if Mainlen >= Nicelen then
                  if Is_Match_Correct (1) then
                     Skip (Mainlen - 1);
                     --  Put_Line("[DL A]" & mainDist'Img & mainLen'Img);
                     Send_Dl_Code (Maindist, Mainlen);
                     return;
                  else
                     --  Put_Line("Wrong match [A]! pos=" & Integer'Image(lzPos - cyclicSize));
                     Send_First_Literal_Of_Match;
                     return;
                  end if;
               end if;
               while Matches.Count > 1 and then Mainlen = Matches.Len (Matches.Count - 2) + 1 loop
                  exit when not Changepair (Matches.Dist (Matches.Count - 2), Maindist);
                  Matches.Count := Matches.Count - 1;
                  Mainlen       := Matches.Len (Matches.Count - 1);
                  Maindist      := Matches.Dist (Matches.Count - 1);
               end loop;
               if Mainlen = Match_Len_Min and then Maindist >= 128 then
                  Mainlen := 1;
               end if;
            end if;

            if Lzma_Friendly
              and then Bestreplen >= Match_Len_Min
              and then
              (Bestreplen + 1 >= Mainlen
               or else (Bestreplen + 2 >= Mainlen and then Maindist >= 2**9)
               or else (Bestreplen + 3 >= Mainlen and then Maindist >= 2**15))
            then
               Skip (Bestreplen - 1);
               --  Put_Line("[DL RB]");
               Send_Dl_Code (Rep_Dist (Bestrepindex), Bestreplen);
               return;
            end if;

            if Mainlen < Match_Len_Min or else Avail <= Match_Len_Min then
               --  Put("[b]");
               Send_First_Literal_Of_Match;
               return;
            end if;

            --  Get the next match. Test if it is better than the current match.
            --  If so, encode the current byte as a literal.
            Matches := Getmatches;

            if Matches.Count > 0 then
               Newlen  := Matches.Len (Matches.Count - 1);
               Newdist := Matches.Dist (Matches.Count - 1);
               if (Newlen >= Mainlen and then Newdist < Maindist)
                 or else (Newlen = Mainlen + 1 and then not Changepair (Maindist, Newdist))
                 or else Newlen > Mainlen + 1
                 or else
                 (Newlen + 1 >= Mainlen
                  and then Mainlen >= Match_Len_Min + 1
                  and then Changepair (Newdist, Maindist))
               then
                  --  Put("[c]");
                  --  Put(Character'Val(cur_literal));
                  Send_First_Literal_Of_Match;
                  return;
               end if;
            end if;

            Limit := Integer'Max (Mainlen - 1, Match_Len_Min);
            for Rep in Rep_Dist'Range loop
               if Getmatchlen (Rep_Dist (Rep), Limit) = Limit then
                  Send_First_Literal_Of_Match;
                  return;
               end if;
            end loop;

            if Is_Match_Correct (0) then
               Skip (Mainlen - 2);
               --  Put_Line("[DL B]" & mainDist'Img & mainLen'Img);
               Send_Dl_Code (Maindist, Mainlen);
            else
               --  Put_Line("Wrong match [B]!");
               Send_First_Literal_Of_Match;
            end if;
         end Getnextsymbol;

         Actual_Written, Avail : Integer;
      begin
         --  NB: heap allocation used only for convenience because of
         --      small default stack sizes on some compilers
         Buf            := new Byte_Array (0 .. Getbufsize);
         Actual_Written := Fillwindow (String_Buffer_Size);
         if Actual_Written > 0 then
            loop
               Getnextsymbol;
               Avail := Getavail;
               if Avail = 0 then
                  Actual_Written := Fillwindow (String_Buffer_Size);
                  exit when Actual_Written = 0;
               end if;
            end loop;
         end if;
         Dispose (Buf);
         Dispose (Tree);
         Dispose (Hash234.Hash4table);
      end Lz77_Using_Bt4;
   begin
      case Method is
         when Lzhuf =>
            Lz77_Using_Lzhuf;
         when Iz_4 .. Iz_10 =>
            Lz77_Using_Iz (4 + Method_Type'Pos (Method) - Method_Type'Pos (Iz_4));
         when Bt4 =>
            Lz77_Using_Bt4;
      end case;
   end Encode;

end Lz77;
