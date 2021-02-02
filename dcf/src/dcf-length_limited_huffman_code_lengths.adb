--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2016 - 2018 Gautier de Montmollin
--  SWITZERLAND
--
--  The copyright holder is only the maintainer of the Ada version;
--  authors of the C code and those of the algorithm are cited below.
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

--  Author: lode.vandevenne [*] gmail [*] com (Lode Vandevenne)
--  Author: jyrki.alakuijala [*] gmail [*] com (Jyrki Alakuijala)
--
--  Bounded package merge algorithm, based on the paper
--    "A Fast and Space-Economical Algorithm for Length-Limited Coding
--    Jyrki Katajainen, Alistair Moffat, Andrew Turpin".
--
--  Translated by G. de Montmollin to Ada from katajainen.c (Zopfli project), 7-Feb-2016
--
--  Main technical differences to katajainen.c:
--
--    - Pointers are not used, array indices instead
--    - All structures are allocated on stack
--    - Sub-programs are nested, then unneeded parameters are removed

procedure DCF.Length_Limited_Huffman_Code_Lengths
  (Frequencies : in     Count_Array;
   Bit_Lengths :    out Length_Array)
is
   subtype Index_Type is Count_Type;

   Null_Index : constant Index_Type := Index_Type'Last;

   --  Nodes forming chains
   type Node is record
      Weight : Count_Type;
      Count  : Count_Type;                --  Number of leaves before this chain
      Tail   : Index_Type := Null_Index;  --  Previous node(s) of this chain, or null_index if none
      In_Use : Boolean    := False;       --  Tracking for garbage collection
   end record;

   type Leaf_Node is record
      Weight : Count_Type;
      Symbol : Alphabet;
   end record;

   --  Memory pool for nodes
   Pool      : array (0 .. Index_Type (2 * Max_Bits * (Max_Bits + 1) - 1)) of Node;
   Pool_Next : Index_Type := Pool'First;

   type Index_Pair is array (Index_Type'(0) .. 1) of Index_Type;
   Lists : array (0 .. Index_Type (Max_Bits - 1)) of Index_Pair;

   type Leaf_Array is array (Index_Type range <>) of Leaf_Node;
   Leaves : Leaf_Array (0 .. Frequencies'Length - 1);

   Num_Symbols          : Count_Type := 0;  --  Amount of symbols with frequency > 0
   Num_Boundary_Pm_Runs : Count_Type;

   Too_Many_Symbols_For_Length_Limit : exception;
   Zero_Length_But_Nonzero_Frequency : exception;
   Nonzero_Length_But_Zero_Frequency : exception;
   Length_Exceeds_Length_Limit       : exception;
   Buggy_Sorting                     : exception;

   procedure Init_Node (Weight, Count : Count_Type; Tail, Node_Idx : Index_Type) is
   begin
      Pool (Node_Idx).Weight := Weight;
      Pool (Node_Idx).Count  := Count;
      Pool (Node_Idx).Tail   := Tail;
      Pool (Node_Idx).In_Use := True;
   end Init_Node;

   --  Finds a free location in the memory pool. Performs garbage collection if needed
   --  If use_lists = True, used to mark in-use nodes during garbage collection

   function Get_Free_Node (Use_Lists : Boolean) return Index_Type is
      Node_Idx : Index_Type;
   begin
      loop
         if Pool_Next > Pool'Last then
            --  Garbage collection
            for I in Pool'Range loop
               Pool (I).In_Use := False;
            end loop;
            if Use_Lists then
               for I in 0 .. Index_Type (Max_Bits * 2 - 1) loop
                  Node_Idx := Lists (I / 2) (I mod 2);
                  while Node_Idx /= Null_Index loop
                     Pool (Node_Idx).In_Use := True;
                     Node_Idx               := Pool (Node_Idx).Tail;
                  end loop;
               end loop;
            end if;
            Pool_Next := Pool'First;
         end if;
         exit when not Pool (Pool_Next).In_Use;  -- Found one
         Pool_Next := Pool_Next + 1;
      end loop;
      Pool_Next := Pool_Next + 1;
      return Pool_Next - 1;
   end Get_Free_Node;

   --  Performs a Boundary Package-Merge step. Puts a new chain in the given list. The
   --  new chain is, depending on the weights, a leaf or a combination of two chains
   --  from the previous list.
   --  index: The index of the list in which a new chain or leaf is required.
   --  final: Whether this is the last time this function is called. If it is then it
   --  is no more needed to recursively call self.

   procedure Boundary_Pm (Index : Index_Type; Final : Boolean) is
      Newchain  : Index_Type;
      Oldchain  : Index_Type;
      Lastcount : constant Count_Type :=
        Pool (Lists (Index) (1)).Count;  --  Count of last chain of list
      Sum : Count_Type;
   begin
      if Index = 0 and Lastcount >= Num_Symbols then
         return;
      end if;
      Newchain := Get_Free_Node (Use_Lists => True);
      Oldchain := Lists (Index) (1);
      --  These are set up before the recursive calls below, so that there is a list
      --  pointing to the new node, to let the garbage collection know it's in use
      Lists (Index) := (Oldchain, Newchain);

      if Index = 0 then
         --  New leaf node in list 0
         Init_Node (Leaves (Lastcount).Weight, Lastcount + 1, Null_Index, Newchain);
      else
         Sum := Pool (Lists (Index - 1) (0)).Weight + Pool (Lists (Index - 1) (1)).Weight;
         if Lastcount < Num_Symbols and then Sum > Leaves (Lastcount).Weight then
            --  New leaf inserted in list, so count is incremented
            Init_Node (Leaves (Lastcount).Weight, Lastcount + 1, Pool (Oldchain).Tail, Newchain);
         else
            Init_Node (Sum, Lastcount, Lists (Index - 1) (1), Newchain);
            if not Final then
               --  Two lookahead chains of previous list used up, create new ones
               Boundary_Pm (Index - 1, False);
               Boundary_Pm (Index - 1, False);
            end if;
         end if;
      end if;
   end Boundary_Pm;

   --  Initializes each list with as lookahead chains the two leaves with lowest weights

   procedure Init_Lists is
      Node0 : constant Index_Type := Get_Free_Node (Use_Lists => False);
      Node1 : constant Index_Type := Get_Free_Node (Use_Lists => False);
   begin
      Init_Node (Leaves (0).Weight, 1, Null_Index, Node0);
      Init_Node (Leaves (1).Weight, 2, Null_Index, Node1);
      Lists := (others => (Node0, Node1));
   end Init_Lists;

   --  Converts result of boundary package-merge to the bit_lengths. The result in the
   --  last chain of the last list contains the amount of active leaves in each list.
   --  chain: Chain to extract the bit length from (last chain from last list).

   procedure Extract_Bit_Lengths (Chain : Index_Type) is
      Node_Idx : Index_Type := Chain;
   begin
      while Node_Idx /= Null_Index loop
         for I in 0 .. Pool (Node_Idx).Count - 1 loop
            Bit_Lengths (Leaves (I).Symbol) := Bit_Lengths (Leaves (I).Symbol) + 1;
         end loop;
         Node_Idx := Pool (Node_Idx).Tail;
      end loop;
   end Extract_Bit_Lengths;

   function "<" (A, B : Leaf_Node) return Boolean is
   begin
      return A.Weight < B.Weight;
   end "<";

   procedure Quick_Sort (A : in out Leaf_Array) is
      N    : constant Index_Type := A'Length;
      I, J : Index_Type;
      P, T : Leaf_Node;
   begin
      if N < 2 then
         return;
      end if;
      P := A (N / 2 + A'First);
      I := 0;
      J := N - 1;
      loop
         while A (I + A'First) < P loop
            I := I + 1;
         end loop;
         while P < A (J + A'First) loop
            J := J - 1;
         end loop;
         exit when I >= J;
         T               := A (I + A'First);
         A (I + A'First) := A (J + A'First);
         A (J + A'First) := T;
         I               := I + 1;
         J               := J - 1;
      end loop;
      Quick_Sort (A (A'First .. A'First + I - 1));
      Quick_Sort (A (A'First + I .. A'Last));
   end Quick_Sort;

   Paranoid : constant Boolean := False;

begin
   Bit_Lengths := (others => 0);
   --  Count used symbols and place them in the leaves
   for A in Alphabet loop
      if Frequencies (A) > 0 then
         Leaves (Num_Symbols) := (Frequencies (A), A);
         Num_Symbols          := Num_Symbols + 1;
      end if;
   end loop;
   --  Check special cases and error conditions
   if Num_Symbols > 2**Max_Bits then
      raise Too_Many_Symbols_For_Length_Limit;  --  Error, too few max_bits to represent symbols
   end if;
   if Num_Symbols = 0 then
      return;  --  No symbols at all. OK
   end if;
   if Num_Symbols = 1 then
      Bit_Lengths (Leaves (0).Symbol) := 1;
      return;  --  Only one symbol, give it bit length 1, not 0. OK
   end if;
   --  Sort the leaves from lightest to heaviest
   Quick_Sort (Leaves (0 .. Num_Symbols - 1));
   if Paranoid then
      for I in 1 .. Num_Symbols - 1 loop
         if Leaves (I) < Leaves (I - 1) then
            raise Buggy_Sorting;
         end if;
      end loop;
   end if;
   Init_Lists;
   --  In the last list, 2 * num_symbols - 2 active chains need to be created. Two
   --  are already created in the initialization. Each Boundary_PM run creates one.
   Num_Boundary_Pm_Runs := 2 * Num_Symbols - 4;
   for I in 1 .. Num_Boundary_Pm_Runs loop
      Boundary_Pm (Index_Type (Max_Bits - 1), I = Num_Boundary_Pm_Runs);
   end loop;
   Extract_Bit_Lengths (Lists (Index_Type (Max_Bits - 1)) (1));
   if Paranoid then
      --  Done; some checks before leaving. Not checked: completeness of Huffman codes
      for A in Alphabet loop
         if Frequencies (A) = 0 then
            if Bit_Lengths (A) > 0 then
               raise Nonzero_Length_But_Zero_Frequency;  --  Never happened so far
            end if;
         else
            if Bit_Lengths (A) = 0 then
               raise Zero_Length_But_Nonzero_Frequency;  --  Happened before null_index fix
            elsif Bit_Lengths (A) > Max_Bits then
               raise Length_Exceeds_Length_Limit;        --  Never happened so far
            end if;
         end if;
      end loop;
   end if;
end DCF.Length_Limited_Huffman_Code_Lengths;
