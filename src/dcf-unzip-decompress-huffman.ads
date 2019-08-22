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

--  UnZip.Decompress.Huffman
--  -----------------------
--
--  Huffman tree generation and deletion.
--  Originally from info-zip's unzip, data structure rewritten by G. de Montmollin

private package DCF.Unzip.Decompress.Huffman is
   pragma Preelaborate;

   type Huft_Table;
   type P_Huft_Table is access Huft_Table;

   type Huft is record
      Extra_Bits : Natural;
      Bits       : Natural;
      N          : Natural;
      Next_Table : P_Huft_Table;
   end record;

   Invalid : constant := 99;  --  Invalid value for extra bits

   type Huft_Table is array (Natural range <>) of Huft;

   --  Linked list just for destroying Huffman tables

   type Table_List;
   type P_Table_List is access Table_List;

   type Table_List is record
      Table : P_Huft_Table;
      Next  : P_Table_List;
   end record;

   type Length_Array is array (Integer range <>) of Natural;

   Empty : constant Length_Array (1 .. 0) := (others => 0);

   --  Free huffman tables starting with table where t points to
   procedure Huft_Free (Tl : in out P_Table_List);

   --  Build huffman table from code lengths given by array b.all
   procedure Huft_Build
     (B               :        Length_Array;
      S               :        Integer;
      D, E            :        Length_Array;
      Tl              :    out P_Table_List;
      M               : in out Integer;
      Huft_Incomplete :    out Boolean);

   Huft_Error         : exception;  --  Bad tree constructed
   Huft_Out_Of_Memory : exception;  --  Not enough memory

end DCF.Unzip.Decompress.Huffman;
