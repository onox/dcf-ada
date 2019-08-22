--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2016 - 2018 Gautier de Montmollin (maintainer of the Ada version)
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

with System;

package body DCF.Lz77 is

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
   begin
      Lz77_Using_Iz (4 + Method_Type'Pos (Method) - Method_Type'Pos (Iz_4));
   end Encode;

end DCF.Lz77;
