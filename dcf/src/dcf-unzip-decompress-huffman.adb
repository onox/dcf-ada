--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 1999 - 2018 Gautier de Montmollin
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

with Ada.Unchecked_Deallocation;

package body DCF.Unzip.Decompress.Huffman is

   --  Note from Pascal source:
   --  C code by info-zip group, translated to pascal by Christian Ghisler
   --  based on unz51g.zip

   --  Free huffman tables starting with table where t points to

   procedure Huft_Free (Tl : in out P_Table_List) is
      procedure Dispose is new Ada.Unchecked_Deallocation (Huft_Table, P_Huft_Table);
      procedure Dispose is new Ada.Unchecked_Deallocation (Table_List, P_Table_List);

      Current : P_Table_List;
   begin
      while Tl /= null loop
         Dispose (Tl.Table);  --  Destroy the Huffman table
         Current := Tl;
         Tl      := Tl.Next;
         Dispose (Current);   --  Destroy the current node
      end loop;
   end Huft_Free;

   --  Build huffman table from code lengths given by array b

   procedure Huft_Build
     (B               :        Length_Array;
      S               :        Integer;
      D, E            :        Length_Array;
      Tl              :    out P_Table_List;
      M               : in out Integer;
      Huft_Incomplete :    out Boolean)
   is
      B_Max   : constant := 16;
      B_Maxp1 : constant := B_Max + 1;

      --  Bit length count table
      Count : array (0 .. B_Maxp1) of Integer := (others => 0);

      F : Integer;    --  I repeats in table every f entries
      G : Integer;    --  Maximum code length
      I : Integer;    --  Counter, current code
      J   : Integer;  --  Counter
      Kcc : Integer;  --  Number of bits in current code

      C_Idx, V_Idx : Natural;  --  Array indices

      Current_Table_Ptr : P_Huft_Table := null;
      Current_Node_Ptr  : P_Table_List := null;  --  Current node for the current table
      New_Node_Ptr      : P_Table_List;          --  New node for the new table

      New_Entry : Huft;                        -- Table entry for structure assignment

      U : array (0 .. B_Max) of P_Huft_Table;  --  Table stack

      N_Max : constant := 288;
      --  Values in order of bit length
      V              : array (0 .. N_Max) of Integer := (others => 0);
      El_V, El_V_M_S : Integer;

      W : Natural := 0;                        --  Bits before this table

      Offset, Code_Stack : array (0 .. B_Maxp1) of Integer;

      Table_Level : Integer := -1;
      Bits        : array (Integer'(-1) .. B_Maxp1) of Integer;
      --  ^ bits(table_level) = # bits in table of level table_level

      Y  : Integer;       --  Number of dummy codes added
      Z  : Natural := 0;  --  Number of entries in current table
      El : Integer;       --  Length of eob code=code 256

      No_Copy_Length_Array : constant Boolean := D'Length = 0 or E'Length = 0;

   begin
      Tl := null;

      if B'Length > 256 then  --  Set length of EOB code, if any
         El := B (256);
      else
         El := B_Max;
      end if;

      --  Generate counts for each bit length

      for K in B'Range loop
         if B (K) > B_Max then
            --  m := 0; -- GNAT 2005 doesn't like it (warning).
            raise Huft_Error;
         end if;
         Count (B (K)) := Count (B (K)) + 1;
      end loop;

      if Count (0) = B'Length then
         M               := 0;
         Huft_Incomplete := False;  --  Spotted by Tucker Taft, 19-Aug-2004
         return;  --  Complete
      end if;

      --  Find minimum and maximum length, bound m by those

      J := 1;
      while J <= B_Max and then Count (J) = 0 loop
         J := J + 1;
      end loop;
      Kcc := J;
      if M < J then
         M := J;
      end if;
      I := B_Max;
      while I > 0 and then Count (I) = 0 loop
         I := I - 1;
      end loop;
      G := I;
      if M > I then
         M := I;
      end if;

      --  Adjust last length count to fill out codes, if needed

      Y := Integer (Shift_Left (Unsigned_32'(1), J)); -- y:= 2 ** j;
      while J < I loop
         Y := Y - Count (J);
         if Y < 0 then
            raise Huft_Error;
         end if;
         Y := Y * 2;
         J := J + 1;
      end loop;

      Y := Y - Count (I);
      if Y < 0 then
         raise Huft_Error;
      end if;
      Count (I) := Count (I) + Y;

      --  Generate starting offsets into the value table for each length

      Offset (1) := 0;
      J          := 0;
      for Idx in 2 .. I loop
         J            := J + Count (Idx - 1);
         Offset (Idx) := J;
      end loop;

      --  Make table of values in order of bit length

      for Idx in B'Range loop
         J := B (Idx);
         if J /= 0 then
            V (Offset (J)) := Idx - B'First;
            Offset (J)     := Offset (J) + 1;
         end if;
      end loop;

      --  Generate huffman codes and for each, make the table entries

      Code_Stack (0) := 0;
      I              := 0;
      V_Idx          := V'First;
      Bits (-1)      := 0;

      --  Go through the bit lengths (kcc already is bits in shortest code)
      for K in Kcc .. G loop
         for Am1 in reverse 0 .. Count (K) - 1 loop  --  A counts codes of length k
            --  Here i is the huffman code of length k bits for value v(v_idx)
            while K > W + Bits (Table_Level) loop
               W           := W + Bits (Table_Level);  --  Length of tables to this position
               Table_Level := Table_Level + 1;
               Z           := G - W;                   --  Compute min size table <= m bits
               if Z > M then
                  Z := M;
               end if;
               J := K - W;
               F := Integer (Shift_Left (Unsigned_32'(1), J)); -- f:= 2 ** j;
               if F > Am1 + 2 then
                  --  Try a k-w bit table
                  F     := F - (Am1 + 2);
                  C_Idx := K;

                  --  Try smaller tables up to z bits
                  loop
                     J := J + 1;
                     exit when J >= Z;
                     F     := F * 2;
                     C_Idx := C_Idx + 1;
                     exit when F - Count (C_Idx) <= 0;
                     F := F - Count (C_Idx);
                  end loop;
               end if;

               if W + J > El and then W < El then
                  J := El - W;  --  Make EOB code end at table
               end if;
               if W = 0 then
                  J := M;  --  Fix: main table always m bits!
               end if;
               Z                  := Integer (Shift_Left (Unsigned_32'(1), J)); -- z:= 2 ** j;
               Bits (Table_Level) := J;

               --  Allocate and link new table

               begin
                  Current_Table_Ptr := new Huft_Table (0 .. Z);
                  New_Node_Ptr      := new Table_List'(Current_Table_Ptr, null);
               exception
                  when Storage_Error =>
                     raise Huft_Out_Of_Memory;
               end;

               if Current_Node_Ptr = null then  --  First table
                  Tl := New_Node_Ptr;
               else
                  Current_Node_Ptr.Next := New_Node_Ptr;  --  Not my first...
               end if;

               Current_Node_Ptr := New_Node_Ptr;  --  Always non-Null from there

               U (Table_Level) := Current_Table_Ptr;

               --  Connect to last table, if there is one

               if Table_Level > 0 then
                  Code_Stack (Table_Level) := I;
                  New_Entry.Bits           := Bits (Table_Level - 1);
                  New_Entry.Extra_Bits     := 16 + J;
                  New_Entry.Next_Table     := Current_Table_Ptr;

                  J :=
                    Integer
                      (Shift_Right
                         (Unsigned_32 (I) and (Shift_Left (Unsigned_32'(1), W) - 1),
                          W - Bits (Table_Level - 1)));

                  --  Test against bad input!

                  if J > U (Table_Level - 1)'Last then
                     raise Huft_Error;
                  end if;
                  U (Table_Level - 1) (J) := New_Entry;
               end if;

            end loop;

            --  Set up table entry in new_entry

            New_Entry.Bits       := K - W;
            New_Entry.Next_Table := null;   -- Unused

            if V_Idx >= B'Length then
               New_Entry.Extra_Bits := Invalid;
            else
               El_V     := V (V_Idx);
               El_V_M_S := El_V - S;
               if El_V_M_S < 0 then
                  --  Simple code, raw value
                  if El_V < 256 then
                     New_Entry.Extra_Bits := 16;
                  else
                     New_Entry.Extra_Bits := 15;
                  end if;
                  New_Entry.N := El_V;
               else
                  --  Non-simple -> lookup in lists
                  if No_Copy_Length_Array then
                     raise Huft_Error;
                  end if;
                  New_Entry.Extra_Bits := E (El_V_M_S);
                  New_Entry.N          := D (El_V_M_S);
               end if;
               V_Idx := V_Idx + 1;
            end if;

            --  Fill code-like entries with new_entry
            F := Integer (Shift_Left (Unsigned_32'(1), K - W));
            --  i.e. f := 2 ** (k-w);
            J := Integer (Shift_Right (Unsigned_32 (I), W));
            while J < Z loop
               Current_Table_Ptr (J) := New_Entry;
               J                     := J + F;
            end loop;

            --  Backwards increment the k-bit code i
            J := Integer (Shift_Left (Unsigned_32'(1), K - 1));
            --  i.e.: j:= 2 ** (k-1)
            while (Unsigned_32 (I) and Unsigned_32 (J)) /= 0 loop
               I := Integer (Unsigned_32 (I) xor Unsigned_32 (J));
               J := J / 2;
            end loop;
            I := Integer (Unsigned_32 (I) xor Unsigned_32 (J));

            --  Backup over finished tables
            while Integer (Unsigned_32 (I) and (Shift_Left (1, W) - 1)) /= Code_Stack (Table_Level)
            loop
               Table_Level := Table_Level - 1;
               W           := W - Bits (Table_Level);  --  Size of previous table!
            end loop;
         end loop;
      end loop;

      Huft_Incomplete := Y /= 0 and G /= 1;
   exception
      when others =>
         Huft_Free (Tl);
         raise;
   end Huft_Build;

end DCF.Unzip.Decompress.Huffman;
