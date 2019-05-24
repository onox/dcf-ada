--  Legal licensing note:

--  Copyright (c) 1999 .. 2018 Gautier de Montmollin
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

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip.Headers;

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Zip is

   use Interfaces;

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
     (Object.Node.Method in Deflate | Deflate_E);

   function Encrypted (Object : Archived_File) return Boolean is
     (Object.Node.Encrypted_2_X);

--  Dn.File_Index, Dn.Crc_32, Dn.Read_Only,
--  Zip_Streams.Zs_Index_Type, Interfaces.Unsigned_32, Boolean;
   --  TODO Implement File_Index, CRC, and Read_Only

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
      Sn : String (S'Range);
   begin
      if Case_Sensitive then
         Sn := S;
      else
         Sn := Ada.Characters.Handling.To_Upper (S);
      end if;
      for I in Sn'Range loop
         if Sn (I) = '\' then
            Sn (I) := '/';
         end if;
      end loop;
      return Sn;
   end Normalize;

   Boolean_To_Encoding : constant array (Boolean) of Zip_Name_Encoding :=
     (False => IBM_437, True => UTF_8);

   -------------------------------------------------------------
   -- Load Zip_info from a stream containing the .zip archive --
   -------------------------------------------------------------

   procedure Load
     (Info            :    out Zip_Info;
      From            : in out Zip_Streams.Root_Zipstream_Type'Class;
      From_Name       : in     String; -- Zip file name
      Case_Sensitive  : in     Boolean               := False;
      Duplicate_Names : in     Duplicate_Name_Policy := Error_On_Duplicate)
   is
      procedure Insert
        (Dico_Name              :        String; -- UPPER if case-insensitive search
         File_Name              :        String;
         File_Index             :        Zip_Streams.Zs_Index_Type;
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
               --  Here we have a case where the entry name already exists in the dictionary.
               case Duplicate_Names is
                  when Error_On_Duplicate =>
                     raise Duplicate_Name with
                       "Same full entry name (in dictionary: " &
                       Dico_Name &
                       ") appears twice in archive directory; " &
                       "procedure Load was called with strict name policy.";
                  when Admit_Duplicates =>
                     if File_Index > Node.File_Index then
                        Insert_Into_Tree (Node.Right);
                     elsif File_Index < Node.File_Index then
                        Insert_Into_Tree (Node.Left);
                     else
                        raise Duplicate_Name with
                          "Archive directory corrupt: same full entry name (in dictionary: " &
                          Dico_Name &
                          "), with same data position, appear twice.";
                     end if;
               end case;
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
      --  after the end-of-central-directory block.
      Main_Comment := new String (1 .. Integer (The_End.Main_Comment_Length));
      String'Read (From'Access, Main_Comment.all);

      --  Process central directory:
      Zip_Streams.Set_Index
        (From,
         Zip_Streams.Zs_Index_Type (1 + The_End.Central_Dir_Offset) + The_End.Offset_Shifting);

      for I in 1 .. The_End.Total_Entries loop
         Zip.Headers.Read_And_Check (From, Header);
         declare
            This_Name : String (1 .. Natural (Header.Short_Info.Filename_Length));
            use Zip_Streams;
         begin
            String'Read (From'Access, This_Name);
            --  Skip extra field and entry comment.
            Set_Index
              (From,
               Index (From) +
               Zs_Size_Type (Header.Short_Info.Extra_Field_Length + Header.Comment_Length));
            --  Now the whole i_th central directory entry is behind
            Insert
              (Dico_Name  => Normalize (This_Name, Case_Sensitive),
               File_Name  => Normalize (This_Name, True),
               File_Index =>
                 Zip_Streams.Zs_Index_Type (1 + Header.Local_Header_Offset) +
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
      Info.Zip_File_Name      := new String'(From_Name);
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

   function Is_Loaded (Info : in Zip_Info) return Boolean is
   begin
      return Info.Loaded;
   end Is_Loaded;

   function Name (Info : in Zip_Info) return String is
   begin
      return Info.Zip_File_Name.all;
   end Name;

   function Comment (Info : in Zip_Info) return String is
   begin
      return Info.Zip_File_Comment.all;
   end Comment;

   function Stream (Info : in Zip_Info) return Zip_Streams.Zipstream_Class_Access is
   begin
      return Info.Zip_Input_Stream;
   end Stream;

   function Entries (Info : in Zip_Info) return Natural is
   begin
      return Info.Total_Entries;
   end Entries;

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
      Dispose (Info.Zip_File_Name);
      Dispose (Info.Zip_File_Comment);
      Info.Loaded := False; -- <-- added 14-Jan-2002
   end Delete;

   --  Traverse a whole Zip_info directory in sorted order, giving the
   --  name for each entry to an user-defined "Action" procedure.

   generic
      with procedure Action_Private (Dn : P_Dir_Node);
      --  Dir_node is private: only known to us, contents subject to change
   procedure Traverse_Private (Z : Zip_Info);

   procedure Traverse_Private (Z : Zip_Info) is
      procedure Traverse_Tree (P : P_Dir_Node) is
      begin
         if P /= null then
            Traverse_Tree (P.Left);
            Action_Private (P);
            Traverse_Tree (P.Right);
         end if;
      end Traverse_Tree;
   begin
      Traverse_Tree (Z.Dir_Binary_Tree);
   end Traverse_Private;

   -----------------------
   --  Public versions  --
   -----------------------

   procedure Traverse (Z : Zip_Info) is
      procedure My_Action_Private (Dn : P_Dir_Node) is
         pragma Inline (My_Action_Private);
      begin
         Action ((Node => Dn));
      end My_Action_Private;

      procedure My_Traverse_Private is new Traverse_Private (My_Action_Private);
   begin
      My_Traverse_Private (Z);
   end Traverse;

   procedure Tree_Stat
     (Z         : in     Zip_Info;
      Total     :    out Natural;
      Max_Depth :    out Natural;
      Avg_Depth :    out Float)
   is
      Sum_Depth : Natural := 0;

      procedure Traverse_Tree (P : P_Dir_Node; Depth : Natural) is
      begin
         if P /= null then
            Total := Total + 1;
            if Depth > Max_Depth then
               Max_Depth := Depth;
            end if;
            Sum_Depth := Sum_Depth + Depth;
            Traverse_Tree (P.Left, Depth + 1);
            Traverse_Tree (P.Right, Depth + 1);
         end if;
      end Traverse_Tree;
   begin
      Total     := 0;
      Max_Depth := 0;
      Traverse_Tree (Z.Dir_Binary_Tree, 0);
      if Total = 0 then
         Avg_Depth := 0.0;
      else
         Avg_Depth := Float (Sum_Depth) / Float (Total);
      end if;
   end Tree_Stat;

   --  13-May-2001: Find_first_offset

   --  For an all-files unzipping of an appended (e.g. self-extracting) archive
   --  (not beginning with ZIP contents), we cannot start with
   --  index 1 in file.
   --  But the offset of first entry in ZIP directory is not valid either,
   --  as this excerpt of appnote.txt states:

   --  "   4)  The entries in the central directory may not necessarily
   --         be in the same order that files appear in the zipfile.    "

   procedure Find_First_Offset
     (File       : in out Zip_Streams.Root_Zipstream_Type'Class;
      File_Index :    out Zip_Streams.Zs_Index_Type)
   is
      The_End    : Zip.Headers.End_Of_Central_Dir;
      Header     : Zip.Headers.Central_File_Header;
      Min_Offset : File_Size_Type;
      use Zip_Streams;
   begin
      Zip.Headers.Load (File, The_End);
      Set_Index (File, Zs_Index_Type (1 + The_End.Central_Dir_Offset) + The_End.Offset_Shifting);

      Min_Offset := The_End.Central_Dir_Offset; -- will be lowered if the archive is not empty.

      if The_End.Total_Entries = 0 then
         raise Archive_Is_Empty;
      end if;

      for I in 1 .. The_End.Total_Entries loop
         Zip.Headers.Read_And_Check (File, Header);
         Set_Index
           (File,
            Index (File) +
            Zs_Size_Type
              (Header.Short_Info.Filename_Length +
               Header.Short_Info.Extra_Field_Length +
               Header.Comment_Length));
         --  Now the whole i_th central directory entry is behind

         if Header.Local_Header_Offset < Min_Offset then
            Min_Offset := Header.Local_Header_Offset;
         end if;
      end loop;

      File_Index := Zip_Streams.Zs_Index_Type (1 + Min_Offset) + The_End.Offset_Shifting;

   exception
      when Zip.Headers.Bad_End | Ada.IO_Exceptions.End_Error =>
         raise Zip.Archive_Corrupted with "Bad (or no) end-of-central-directory";
      when Zip.Headers.Bad_Central_Header =>
         raise Zip.Archive_Corrupted with "Bad central directory entry header";
   end Find_First_Offset;

   --  Internal: find offset of a zipped file using the zip_info tree 8-)

   procedure Find_Offset
     (Info          : in     Zip_Info;
      Name          : in     String;
      Name_Encoding :    out Zip_Name_Encoding;
      File_Index    :    out Zip_Streams.Zs_Index_Type;
      Comp_Size     :    out File_Size_Type;
      Uncomp_Size   :    out File_Size_Type;
      Crc_32        :    out Interfaces.Unsigned_32)
   is
      Aux     : P_Dir_Node      := Info.Dir_Binary_Tree;
      Up_Name : constant String := Normalize (Name, Info.Case_Sensitive);
   begin
      while Aux /= null loop
         if Up_Name > Aux.Dico_Name then
            Aux := Aux.Right;
         elsif Up_Name < Aux.Dico_Name then
            Aux := Aux.Left;
         else  -- entry found !
            Name_Encoding := Aux.Name_Encoding;
            File_Index    := Aux.File_Index;
            Comp_Size     := Aux.Comp_Size;
            Uncomp_Size   := Aux.Uncomp_Size;
            Crc_32        := Aux.Crc_32;
            return;
         end if;
      end loop;
      raise File_Name_Not_Found with
        "Archive: [" & Info.Zip_File_Name.all & "], entry: [" & Name & ']';
   end Find_Offset;

   function Exists (Info : in Zip_Info; Name : in String) return Boolean is
      Aux     : P_Dir_Node      := Info.Dir_Binary_Tree;
      Up_Name : constant String := Normalize (Name, Info.Case_Sensitive);
   begin
      while Aux /= null loop
         if Up_Name > Aux.Dico_Name then
            Aux := Aux.Right;
         elsif Up_Name < Aux.Dico_Name then
            Aux := Aux.Left;
         else  -- entry found !
            return True;
         end if;
      end loop;
      return False;
   end Exists;

   procedure Get_Sizes
     (Info        : in     Zip_Info;
      Name        : in     String;
      Comp_Size   :    out File_Size_Type;
      Uncomp_Size :    out File_Size_Type)
   is
      Dummy_File_Index    : Zip_Streams.Zs_Index_Type;
      Dummy_Name_Encoding : Zip_Name_Encoding;
      Dummy_Crc_32        : Interfaces.Unsigned_32;
   begin
      Find_Offset
        (Info,
         Name,
         Dummy_Name_Encoding,
         Dummy_File_Index,
         Comp_Size,
         Uncomp_Size,
         Dummy_Crc_32);
   end Get_Sizes;

   --  Workaround for the severe xxx'Read xxx'Write performance
   --  problems in the GNAT and ObjectAda compilers (as in 2009)
   --  This is possible if and only if Byte = Stream_Element and
   --  arrays types are both packed and aligned the same way.
   subtype Size_Test_A is Byte_Buffer (1 .. 19);
   subtype Size_Test_B is Ada.Streams.Stream_Element_Array (1 .. 19);
   Workaround_Possible : constant Boolean :=
     Size_Test_A'Size = Size_Test_B'Size and Size_Test_A'Alignment = Size_Test_B'Alignment;

   --  BlockRead - general-purpose procedure (nothing really specific
   --  to Zip / UnZip): reads either the whole buffer from a file, or
   --  if the end of the file lays inbetween, a part of the buffer.

   procedure Blockread
     (File          : in     Ada.Streams.Stream_IO.File_Type;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural)
   is
      use Ada.Streams, Ada.Streams.Stream_IO;
      Se_Buffer : Stream_Element_Array (1 .. Buffer'Length);
      for Se_Buffer'Address use Buffer'Address;
      pragma Import (Ada, Se_Buffer);
      Last_Read : Stream_Element_Offset;
   begin
      if Workaround_Possible then
         Read (Stream (File).all, Se_Buffer, Last_Read);
         Actually_Read := Natural (Last_Read);
      else
         if End_Of_File (File) then
            Actually_Read := 0;
         else
            Actually_Read := Integer'Min (Buffer'Length, Integer (Size (File) - Index (File) + 1));
            Byte_Buffer'Read
              (Stream (File),
               Buffer (Buffer'First .. Buffer'First + Actually_Read - 1));
         end if;
      end if;
   end Blockread;

   procedure Blockread
     (Stream        : in out Zip_Streams.Root_Zipstream_Type'Class;
      Buffer        :    out Byte_Buffer;
      Actually_Read :    out Natural)
   is
      use Ada.Streams, Zip_Streams;
      Se_Buffer : Stream_Element_Array (1 .. Buffer'Length);
      for Se_Buffer'Address use Buffer'Address;
      pragma Import (Ada, Se_Buffer);
      Last_Read : Stream_Element_Offset;
   begin
      if Workaround_Possible then
         Read (Stream, Se_Buffer, Last_Read);
         Actually_Read := Natural (Last_Read);
      else
         if End_Of_Stream (Stream) then
            Actually_Read := 0;
         else
            Actually_Read :=
              Integer'Min (Buffer'Length, Integer (Size (Stream) - Index (Stream) + 1));
            Byte_Buffer'Read
              (Stream'Access,
               Buffer (Buffer'First .. Buffer'First + Actually_Read - 1));
         end if;
      end if;
   end Blockread;

   procedure Blockread
     (Stream : in out Zip_Streams.Root_Zipstream_Type'Class;
      Buffer :    out Byte_Buffer)
   is
      Actually_Read : Natural;
   begin
      Blockread (Stream, Buffer, Actually_Read);
      if Actually_Read < Buffer'Length then
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
      if Workaround_Possible then
         Ada.Streams.Write (Stream, Se_Buffer);
      else
         Byte_Buffer'Write (Stream'Access, Buffer);
         --  ^This is 30x to 70x slower on GNAT 2009 !
      end if;
   end Blockwrite;

   function Image (M : Pkzip_Method) return String is
   begin
      case M is
         when Store =>
            return "Store";
         when Shrink =>
            return "Shrink";
         when Reduce_1 =>
            return "Reduce 1";
         when Reduce_2 =>
            return "Reduce 2";
         when Reduce_3 =>
            return "Reduce 3";
         when Reduce_4 =>
            return "Reduce 4";
         when Implode =>
            return "Implode";
         when Tokenize =>
            return "Tokenize";
         when Deflate =>
            return "Deflate";
         when Deflate_E =>
            return "Deflate64";
         when Bzip2 =>
            return "BZip2";
         when Lzma_Meth =>
            return "LZMA";
         when Ppmd =>
            return "PPMd";
         when Unknown =>
            return "(unknown)";
      end case;
   end Image;

   function Method_From_Code (X : Natural) return Pkzip_Method is
   --  An enumeration clause might be more elegant, but needs
   --  curiously an Unchecked_Conversion... (RM 13.4)
   begin
      case X is
         when Compression_Format_Code.Store =>
            return Store;
         when Compression_Format_Code.Shrink =>
            return Shrink;
         when Compression_Format_Code.Reduce =>
            return Reduce_1;
         when Compression_Format_Code.Reduce + 1 =>
            return Reduce_2;
         when Compression_Format_Code.Reduce + 2 =>
            return Reduce_3;
         when Compression_Format_Code.Reduce + 3 =>
            return Reduce_4;
         when Compression_Format_Code.Implode =>
            return Implode;
         when Compression_Format_Code.Tokenize =>
            return Tokenize;
         when Compression_Format_Code.Deflate =>
            return Deflate;
         when Compression_Format_Code.Deflate_E =>
            return Deflate_E;
         when Compression_Format_Code.Bzip2 =>
            return Bzip2;
         when Compression_Format_Code.Lzma =>
            return Lzma_Meth;
         when Compression_Format_Code.Ppmd =>
            return Ppmd;
         when others =>
            return Unknown;
      end case;
   end Method_From_Code;

   function Method_From_Code (X : Interfaces.Unsigned_16) return Pkzip_Method is
   begin
      return Method_From_Code (Natural (X));
   end Method_From_Code;

   --  Copy a chunk from a stream into another one, using a temporary buffer
   procedure Copy_Chunk
     (From        : in out Zip_Streams.Root_Zipstream_Type'Class;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Bytes       :        Natural;
      Buffer_Size :        Positive      := 1024 * 1024;
      Feedback    :        Feedback_Proc := null)
   is
      Buf                    : Zip.Byte_Buffer (1 .. Buffer_Size);
      Actually_Read, Remains : Natural;
      User_Abort             : Boolean := False;
   begin
      Remains := Bytes;
      while Remains > 0 loop
         if Feedback /= null then
            Feedback (100 - Integer (100.0 * Float (Remains) / Float (Bytes)), False, User_Abort);
            --  !! do something if user_abort = True !!
         end if;
         Zip.Blockread (From, Buf (1 .. Integer'Min (Remains, Buf'Last)), Actually_Read);
         if Actually_Read = 0 then  --  Premature end, unexpected
            raise Zip.Archive_Corrupted;
         end if;
         Remains := Remains - Actually_Read;
         Zip.Blockwrite (Into, Buf (1 .. Actually_Read));
      end loop;
   end Copy_Chunk;

   --  Copy a whole file into a stream, using a temporary buffer
   procedure Copy_File
     (File_Name   :        String;
      Into        : in out Ada.Streams.Root_Stream_Type'Class;
      Buffer_Size :        Positive := 1024 * 1024)
   is
      use Ada.Streams.Stream_IO;
      F             : File_Type;
      Buf           : Zip.Byte_Buffer (1 .. Buffer_Size);
      Actually_Read : Natural;
   begin
      Open (F, In_File, File_Name);
      loop
         Zip.Blockread (F, Buf, Actually_Read);
         exit when Actually_Read = 0; -- this is expected
         Zip.Blockwrite (Into, Buf (1 .. Actually_Read));
      end loop;
      Close (F);
   end Copy_File;

   procedure Put_Multi_Line (Out_File : Ada.Text_IO.File_Type; Text : String) is
      Last_Char : Character := ' ';
      C         : Character;

      use Ada.Characters.Latin_1;
   begin
      for I in Text'Range loop
         C := Text (I);
         case C is
            when CR =>
               Ada.Text_IO.New_Line (Out_File);
            when LF =>
               if Last_Char /= CR then
                  Ada.Text_IO.New_Line (Out_File);
               end if;
            when others =>
               Ada.Text_IO.Put (Out_File, C);
         end case;
         Last_Char := C;
      end loop;
      if Text'Length > 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Put_Multi_Line;

   procedure Write_As_Text
     (Out_File  :        Ada.Text_IO.File_Type;
      Buffer    :        Byte_Buffer;
      Last_Char : in out Character)  --  Track line-ending characters across writes
   is
      C : Character;

      use Ada.Characters.Latin_1;
   begin
      for I in Buffer'Range loop
         C := Character'Val (Buffer (I));
         case C is
            when CR =>
               Ada.Text_IO.New_Line (Out_File);
            when LF =>
               if Last_Char /= CR then
                  Ada.Text_IO.New_Line (Out_File);
               end if;
            when others =>
               Ada.Text_IO.Put (Out_File, C);
         end case;
         Last_Char := C;
      end loop;
   end Write_As_Text;

   function Hexadecimal (X : Interfaces.Unsigned_32) return String is
      package Mio is new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_32);
      Str : String (1 .. 12);
      use Ada.Strings.Fixed;
   begin
      Mio.Put (Str, X, 16);
      return Str (Index (Str, "#") + 1 .. 11);
   end Hexadecimal;

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
      Info.Zip_File_Name    := new String'(Info.Zip_File_Name.all);
      Info.Zip_File_Comment := new String'(Info.Zip_File_Comment.all);
   end Adjust;

   overriding
   procedure Finalize (Info : in out Zip_Info) is
   begin
      Delete (Info);
   end Finalize;

end Zip;
