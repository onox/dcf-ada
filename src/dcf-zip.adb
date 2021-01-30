--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 1999 .. 2018 Gautier de Montmollin
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

with DCF.Zip.Headers;

with Ada.Characters.Handling;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

package body DCF.Zip is

   function Name (Object : Archived_File) return String is
     (Object.Node.File_Name);

   function Name_Encoding (Object : Archived_File) return Zip_Name_Encoding is
     (Object.Node.Name_Encoding);

   function Compressed_Size (Object : Archived_File) return File_Size_Type is
     (Object.Node.Comp_Size);

   function Uncompressed_Size (Object : Archived_File) return File_Size_Type is
     (Object.Node.Uncomp_Size);

   function Date_Time (Object : Archived_File) return Time is
     (Object.Node.Date_Time);

   function Compressed (Object : Archived_File) return Boolean is
     (Object.Node.Method = Deflate);

   function Encrypted (Object : Archived_File) return Boolean is
     (Object.Node.Encrypted_2_X);

   function File_Index (Object : Archived_File) return DCF.Streams.Zs_Index_Type is
     (Object.Node.File_Index);

   function CRC_32 (Object : Archived_File) return Unsigned_32 is
     (Object.Node.Crc_32);

   procedure Dispose is new Ada.Unchecked_Deallocation (Dir_Node, P_Dir_Node);
   procedure Dispose is new Ada.Unchecked_Deallocation (String, P_String);

   package Binary_Tree_Rebalancing is
      procedure Rebalance (Root : in out P_Dir_Node);
   end Binary_Tree_Rebalancing;

   package body Binary_Tree_Rebalancing is

      -------------------------------------------------------------------
      -- Tree Rebalancing in Optimal Time and Space                    --
      -- QUENTIN F. STOUT and BETTE L. WARREN                          --
      -- Communications of the ACM September 1986 Volume 29 Number 9   --
      -------------------------------------------------------------------
      --  http://www.eecs.umich.edu/~qstout/pap/CACM86.pdf
      --
      --  Translated by (New) P2Ada v. 15-Nov-2006

      procedure Tree_To_Vine (Root : P_Dir_Node; Size : out Integer) is
         --  Transform the tree with pseudo-root "root^" into a vine with
         --  pseudo-root node "root^", and store the number of nodes in "size"
         Vine_Tail, Remainder, Temp : P_Dir_Node;
      begin
         Vine_Tail := Root;
         Remainder := Vine_Tail.Right;
         Size      := 0;
         while Remainder /= null loop
            if Remainder.Left = null then
               --  Move vine-tail down one:
               Vine_Tail := Remainder;
               Remainder := Remainder.Right;
               Size      := Size + 1;
            else
               --  Rotate:
               Temp            := Remainder.Left;
               Remainder.Left  := Temp.Right;
               Temp.Right      := Remainder;
               Remainder       := Temp;
               Vine_Tail.Right := Temp;
            end if;
         end loop;
      end Tree_To_Vine;

      procedure Vine_To_Tree (Root : P_Dir_Node; Size_Given : Integer) is
         --  Convert the vine with "size" nodes and pseudo-root
         --  node "root^" into a balanced tree
         Leaf_Count : Integer;
         Size       : Integer := Size_Given;

         procedure Compression (Root_Compress : P_Dir_Node; Count : Integer) is
            --  Compress "count" spine nodes in the tree with pseudo-root "root_compress^"
            Scanner, Child : P_Dir_Node;
         begin
            Scanner := Root_Compress;
            for Counter in reverse 1 .. Count loop
               Child         := Scanner.Right;
               Scanner.Right := Child.Right;
               Scanner       := Scanner.Right;
               Child.Right   := Scanner.Left;
               Scanner.Left  := Child;
            end loop;
         end Compression;

         --  Returns n - 2 ** Integer( Float'Floor( log( Float(n) ) / log(2.0) ) )
         --  without Float-Point calculation and rounding errors with too short floats
         function Remove_Leading_Binary_1 (N : Integer) return Integer is
            X : Integer := 2**16; -- supposed maximum
         begin
            if N < 1 then
               return N;
            end if;
            while N mod X = N loop
               X := X / 2;
            end loop;
            return N mod X;
         end Remove_Leading_Binary_1;

      begin --  Vine_to_tree
         Leaf_Count := Remove_Leading_Binary_1 (Size + 1);
         Compression (Root, Leaf_Count); -- create deepest leaves
         --  Use Perfect_leaves instead for a perfectly balanced tree
         Size := Size - Leaf_Count;
         while Size > 1 loop
            Compression (Root, Size / 2);
            Size := Size / 2;
         end loop;
      end Vine_To_Tree;

      procedure Rebalance (Root : in out P_Dir_Node) is
         --  Rebalance the binary search tree with root "root.all",
         --  with the result also rooted at "root.all".
         --  Uses the Tree_to_vine and Vine_to_tree procedures
         Pseudo_Root : P_Dir_Node;
         Size        : Integer;
      begin
         Pseudo_Root       := new Dir_Node (Name_Len => 0);
         Pseudo_Root.Right := Root;
         Tree_To_Vine (Pseudo_Root, Size);
         Vine_To_Tree (Pseudo_Root, Size);
         Root := Pseudo_Root.Right;
         Dispose (Pseudo_Root);
      end Rebalance;

   end Binary_Tree_Rebalancing;

   --  19-Jun-2001: Enhanced file name identification
   --              a) when case insensitive  -> all UPPER (current)
   --              b) '\' and '/' identified -> all '/'   (new)

   function Normalize (S : String; Case_Sensitive : Boolean) return String is
      Sn : String := (if Case_Sensitive then S else Ada.Characters.Handling.To_Upper (S));
   begin
      for I in Sn'Range loop
         if Sn (I) = '\' then
            Sn (I) := '/';
         end if;
      end loop;
      return Sn;
   end Normalize;

   function Get_Node (Info : in Zip_Info; Name : in String) return P_Dir_Node is
      Aux     : P_Dir_Node      := Info.Dir_Binary_Tree;
      Up_Name : constant String := Normalize (Name, Info.Case_Sensitive);
   begin
      while Aux /= null loop
         if Up_Name > Aux.Dico_Name then
            Aux := Aux.Right;
         elsif Up_Name < Aux.Dico_Name then
            Aux := Aux.Left;
         else  -- entry found !
            return Aux;
         end if;
      end loop;
      return null;
   end Get_Node;

   Boolean_To_Encoding : constant array (Boolean) of Zip_Name_Encoding :=
     (False => IBM_437, True => UTF_8);

   -------------------------------------------------------------
   -- Load Zip_info from a stream containing the .zip archive --
   -------------------------------------------------------------

   procedure Load
     (Info            :    out Zip_Info;
      From            : in out DCF.Streams.Root_Zipstream_Type'Class;
      Case_Sensitive  : in     Boolean := False)
   is
      procedure Insert
        (Dico_Name              :        String; -- UPPER if case-insensitive search
         File_Name              :        String;
         File_Index             :        DCF.Streams.Zs_Index_Type;
         Comp_Size, Uncomp_Size :        File_Size_Type;
         Crc_32                 :        Unsigned_32;
         Date_Time              :        Time;
         Method                 :        Pkzip_Method;
         Name_Encoding          :        Zip_Name_Encoding;
         Read_Only              :        Boolean;
         Encrypted_2_X          :        Boolean;
         Root_Node              : in out P_Dir_Node)
      is
         procedure Insert_Into_Tree (Node : in out P_Dir_Node) is
         begin
            if Node = null then
               Node :=
                 new Dir_Node'
                   ((Name_Len     => File_Name'Length,
                    Left          => null,
                    Right         => null,
                    Dico_Name     => Dico_Name,
                    File_Name     => File_Name,
                    File_Index    => File_Index,
                    Comp_Size     => Comp_Size,
                    Uncomp_Size   => Uncomp_Size,
                    Crc_32        => Crc_32,
                    Date_Time     => Date_Time,
                    Method        => Method,
                    Name_Encoding => Name_Encoding,
                    Read_Only     => Read_Only,
                    Encrypted_2_X => Encrypted_2_X));
            elsif Dico_Name > Node.Dico_Name then
               Insert_Into_Tree (Node.Right);
            elsif Dico_Name < Node.Dico_Name then
               Insert_Into_Tree (Node.Left);
            else
               --  Here we have a case where the entry name already exists
               --  in the dictionary
               raise Duplicate_Name with
                 "Entry name '" & Dico_Name & "' appears twice in archive";
            end if;
         end Insert_Into_Tree;
      begin
         Insert_Into_Tree (Root_Node);
      end Insert;

      The_End : Zip.Headers.End_Of_Central_Dir;
      Header  : Zip.Headers.Central_File_Header;
      P       : P_Dir_Node := null;
      Main_Comment : P_String;
   begin
      if Info.Loaded then
         raise Program_Error;
      end if;

      Zip.Headers.Load (From, The_End);

      --  We take the opportunity to read the main comment, which is right
      --  after the end-of-central-directory block
      Main_Comment := new String (1 .. Integer (The_End.Main_Comment_Length));
      String'Read (From'Access, Main_Comment.all);

      --  Process central directory
      From.Set_Index
        (DCF.Streams.Zs_Index_Type (1 + The_End.Central_Dir_Offset) + The_End.Offset_Shifting);

      for I in 1 .. The_End.Total_Entries loop
         Zip.Headers.Read_And_Check (From, Header);
         declare
            This_Name : String (1 .. Natural (Header.Short_Info.Filename_Length));
         begin
            String'Read (From'Access, This_Name);
            --  Skip extra field and entry comment
            From.Set_Index
              (From.Index +
               DCF.Streams.Zs_Size_Type
                 (Header.Short_Info.Extra_Field_Length + Header.Comment_Length));
            --  Now the whole i_th central directory entry is behind
            Insert
              (Dico_Name  => Normalize (This_Name, Case_Sensitive),
               File_Name  => Normalize (This_Name, True),
               File_Index =>
                 DCF.Streams.Zs_Index_Type (1 + Header.Local_Header_Offset) +
                 The_End.Offset_Shifting,
               Comp_Size     => Header.Short_Info.Dd.Compressed_Size,
               Uncomp_Size   => Header.Short_Info.Dd.Uncompressed_Size,
               Crc_32        => Header.Short_Info.Dd.Crc_32,
               Date_Time     => Header.Short_Info.File_Timedate,
               Method        => Method_From_Code (Header.Short_Info.Zip_Type),
               Name_Encoding =>
                 Boolean_To_Encoding
                   ((Header.Short_Info.Bit_Flag and Zip.Headers.Language_Encoding_Flag_Bit) /= 0),
               Read_Only =>
                 Header.Made_By_Version / 256 = 0 and -- DOS-like
                 (Header.External_Attributes and 1) = 1,
               Encrypted_2_X =>
                 (Header.Short_Info.Bit_Flag and Zip.Headers.Encryption_Flag_Bit) /= 0,
               Root_Node => P);
            --  Since the files are usually well ordered, the tree as inserted
            --  is very unbalanced; we need to rebalance it from time to time
            --  during loading, otherwise the insertion slows down dramatically
            --  for zip files with plenty of files - converges to
            --  O(total_entries ** 2)...
            if I mod 256 = 0 then
               Binary_Tree_Rebalancing.Rebalance (P);
            end if;
         end;
      end loop;

      Binary_Tree_Rebalancing.Rebalance (P);

      Info.Loaded             := True;
      Info.Case_Sensitive     := Case_Sensitive;
      Info.Zip_Input_Stream   := From'Unchecked_Access;
      Info.Dir_Binary_Tree    := P;
      Info.Total_Entries      := Integer (The_End.Total_Entries);
      Info.Zip_File_Comment   := Main_Comment;
      Info.Zip_Archive_Format := Zip_32;
   exception
      when Zip.Headers.Bad_End =>
         raise Zip.Archive_Corrupted with "Bad (or no) end-of-central-directory";
      when Zip.Headers.Bad_Central_Header =>
         raise Zip.Archive_Corrupted with "Bad central directory entry header";
   end Load;

   function Is_Loaded (Info : in Zip_Info) return Boolean is (Info.Loaded);

   function Name (Info : in Zip_Info) return String is
     (Info.Zip_Input_Stream.Get_Name);

   function Comment (Info : in Zip_Info) return String is
     (Info.Zip_File_Comment.all);

   function Stream (Info : in Zip_Info) return not null DCF.Streams.Zipstream_Class_Access is
     (Info.Zip_Input_Stream);

   function Entries (Info : in Zip_Info) return Natural is
     (Info.Total_Entries);

   ------------
   -- Delete --
   ------------

   procedure Delete (Info : in out Zip_Info) is
      procedure Delete (P : in out P_Dir_Node) is
      begin
         if P /= null then
            Delete (P.Left);
            Delete (P.Right);
            Dispose (P);
            P := null;
         end if;
      end Delete;
   begin
      Delete (Info.Dir_Binary_Tree);
      Dispose (Info.Zip_File_Comment);
      Info.Loaded := False; -- <-- added 14-Jan-2002
   end Delete;

   procedure Traverse (Z : Zip_Info) is
      procedure Traverse_Tree (P : P_Dir_Node) is
      begin
         if P /= null then
            Traverse_Tree (P.Left);
            Action ((Node => P));
            Traverse_Tree (P.Right);
         end if;
      end Traverse_Tree;
   begin
      Traverse_Tree (Z.Dir_Binary_Tree);
   end Traverse;

   procedure Traverse_One_File (Z : Zip_Info; Name : String) is
      Dn : constant P_Dir_Node := Get_Node (Z, Name);
   begin
      if Dn = null then
         raise File_Name_Not_Found with
           "Entry " & Name & " not found in " & Z.Name;
      end if;
      Action ((Node => Dn));
   end Traverse_One_File;

   function Exists (Info : in Zip_Info; Name : in String) return Boolean is
     (Get_Node (Info, Name) /= null);

   procedure Blockread
     (Stream        : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural)
   is
      use Ada.Streams;
      use DCF.Streams;

      Se_Buffer : Stream_Element_Array (1 .. Buffer'Length);
      for Se_Buffer'Address use Buffer'Address;
      pragma Import (Ada, Se_Buffer);
      Last_Read : Stream_Element_Offset;
   begin
      Read (Stream, Se_Buffer, Last_Read);
      Actually_Read := Natural (Last_Read);
   end Blockread;

   procedure Blockread
     (Stream : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer :    out Byte_Buffer)
   is
      Actually_Read : Natural;
   begin
      Blockread (Stream, Buffer, Actually_Read);
      if Actually_Read < Buffer'Last then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Blockread;

   procedure Blockwrite
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer : in     Byte_Buffer)
   is
      use Ada.Streams;
      Se_Buffer : Stream_Element_Array (1 .. Buffer'Length);
      for Se_Buffer'Address use Buffer'Address;
      pragma Import (Ada, Se_Buffer);
   begin
      Stream.Write (Se_Buffer);
   end Blockwrite;

   procedure Blockread
     (Stream : in out DCF.Streams.Root_Zipstream_Type'Class;
      Buffer :    out Ada.Streams.Stream_Element_Array)
   is
      Last_Read : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element_Offset;
   begin
      Stream.Read (Buffer, Last_Read);
      if Last_Read < Buffer'Last then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Blockread;

   function Method_From_Code (Code : Unsigned_16) return Pkzip_Method is
   --  An enumeration clause might be more elegant, but needs
   --  curiously an Unchecked_Conversion... (RM 13.4)
   begin
      case Code is
         when Compression_Format_Code.Store =>
            return Store;
         when Compression_Format_Code.Deflate =>
            return Deflate;
         when others =>
            raise Constraint_Error with "Unknown compression method " & Code'Image;
      end case;
   end Method_From_Code;

   procedure Copy_Chunk
     (From        : in out DCF.Streams.Root_Zipstream_Type'Class;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Bytes       :        Ada.Streams.Stream_Element_Count;
      Feedback    :        Feedback_Proc := null)
   is
      use type Ada.Streams.Stream_Element_Offset;

      function Percentage (Left, Right : Ada.Streams.Stream_Element_Count) return Natural is
        (Natural ((100.0 * Float (Left)) / Float (Right)));

      Buffer     : Ada.Streams.Stream_Element_Array (1 .. Default_Buffer_Size);
      Last_Read  : Ada.Streams.Stream_Element_Offset;
      Counted    : Ada.Streams.Stream_Element_Count := 0;

      User_Aborting : Boolean := False;
   begin
      while Counted < Bytes loop
         if Feedback /= null then
            Feedback (Percentage (Counted, Bytes), User_Aborting);
            if User_Aborting then
               raise User_Abort;
            end if;
         end if;
         From.Read (Buffer, Last_Read);
         if Last_Read < Bytes - Counted then  --  Premature end, unexpected
            raise Zip.Archive_Corrupted;
         end if;
         Counted := Counted + Last_Read;
         Into.Write (Buffer (1 .. Last_Read));
      end loop;
   end Copy_Chunk;

   overriding
   procedure Adjust (Info : in out Zip_Info) is
      function Tree_Clone (P : in P_Dir_Node) return P_Dir_Node is
         Q : P_Dir_Node;
      begin
         if P = null then
            return null;
         else
            Q       := new Dir_Node'(P.all);
            Q.Left  := Tree_Clone (P.Left);
            Q.Right := Tree_Clone (P.Right);
            return Q;
         end if;
      end Tree_Clone;
   begin
      Info.Dir_Binary_Tree  := Tree_Clone (Info.Dir_Binary_Tree);
      Info.Zip_File_Comment := new String'(Info.Zip_File_Comment.all);
   end Adjust;

   overriding
   procedure Finalize (Info : in out Zip_Info) is
   begin
      Delete (Info);
   end Finalize;

end DCF.Zip;
