--  Contributed by ITEC - NXP Semiconductors
--  June 2008
--
--  Copyright (c) 2008 - 2018 Gautier de Montmollin (maintainer)
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

--  The Zip_Streams package defines an abstract stream
--  type, Root_Zipstream_Type, with name, time and an index for random access.
--
--  In addition, this package provides two ready-to-use derivations:
--
--    - Memory_Zipstream, for using in-memory streaming
--    - File_Zipstream, for accessing files
--
--  The Zip_Streams package can be used as such, independently
--  of the Zip-Ada library.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

with Interfaces;

with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

use Ada.Streams;
use Ada.Strings.Unbounded;

use Interfaces;

package Zip_Streams is

   --  We define an own Time (Ada.Calendar's body can be very time-consuming!)
   --  See subpackage Calendar below for own Split, Time_Of and Convert from/to
   --  Ada.Calendar.Time.
   type Time is private;

   Default_Time : constant Time;  --  some default time

   ------------------------------------------------------
   --  Root_Zipstream_Type: root abstract stream type  --
   ------------------------------------------------------

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with private;
   type Zipstream_Class_Access is access all Root_Zipstream_Type'Class;

   subtype Zs_Size_Type is Integer_64 range 0 .. Integer_64'Last;
   subtype Zs_Index_Type is Zs_Size_Type range 1 .. Zs_Size_Type'Last;

   --  Set the index on the stream
   procedure Set_Index (S : in out Root_Zipstream_Type; To : Zs_Index_Type) is abstract;

   --  Returns the index of the stream
   function Index (S : in Root_Zipstream_Type) return Zs_Index_Type is abstract;

   --  Returns the Size of the stream
   function Size (S : in Root_Zipstream_Type) return Zs_Size_Type is abstract;

   procedure Set_Name (S : in out Root_Zipstream_Type; Name : String);

   --  This procedure returns the name of the stream
   function Get_Name (S : in Root_Zipstream_Type) return String;

   --  This procedure sets the Modification_Time of the stream
   procedure Set_Time (S : in out Root_Zipstream_Type; Modification_Time : Time);

   --  Set_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   procedure Set_Time (S : out Root_Zipstream_Type'Class; Modification_Time : Ada.Calendar.Time);

   --  This procedure returns the ModificationTime of the stream
   function Get_Time (S : in Root_Zipstream_Type) return Time;

   --  Get_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   function Get_Time (S : in Root_Zipstream_Type'Class) return Ada.Calendar.Time;

   --  Returns true if the index is at the end of the stream, else false
   function End_Of_Stream (S : in Root_Zipstream_Type) return Boolean is abstract;

   -----------------------------------------------------------------------
   --  Memory_Zipstream: stream based on an in-memory Unbounded_String  --
   -----------------------------------------------------------------------
   type Memory_Zipstream is new Root_Zipstream_Type with private;

   --  Get the complete value (contents) of the stream
   procedure Get (Str : Memory_Zipstream; Unb : out Unbounded_String);

   --  Set a value in the stream, the index will be set
   --  to null and old data in the stream will be lost.
   procedure Set (Str : in out Memory_Zipstream; Unb : Unbounded_String);

   ----------------------------------------------
   --  File_Zipstream: stream based on a file  --
   ----------------------------------------------
   type File_Zipstream is new Root_Zipstream_Type with private;

   type File_Mode is new Ada.Streams.Stream_IO.File_Mode;

   function Open (File_Name : String) return File_Zipstream;
   --  Open a file for reading

   function Create (File_Name : String) return File_Zipstream;
   --  Create a file on the disk

   --  Is the File_Zipstream open ?
   function Is_Open (Str : in File_Zipstream) return Boolean
     with Post => Is_Open'Result;

   ----------------------------
   --  Routines around Time  --
   ----------------------------

   package Calendar is
      function Convert (Date : in Ada.Calendar.Time) return Time;
      function Convert (Date : in Time) return Ada.Calendar.Time;

      subtype Dos_Time is Interfaces.Unsigned_32;

      function Convert (Date : in Dos_Time) return Time;
      function Convert (Date : in Time) return Dos_Time;

      use Ada.Calendar;

      procedure Split
        (Date    :     Time;
         Year    : out Year_Number;
         Month   : out Month_Number;
         Day     : out Day_Number;
         Seconds : out Day_Duration);

      function Time_Of
        (Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration := 0.0) return Time;

      function ">" (Left, Right : Time) return Boolean;

      Time_Error : exception;
   end Calendar;

   --  Parameter Form added to *_IO.[Open|Create]
   --  See RM A.8.2: File Management
   --  Example: "encoding=8bits", "encoding=utf8"
   Form_For_Io_Open_And_Create : Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.Null_Unbounded_String;

private

   --  Time. Currently, DOS format (pkzip appnote.txt: part V., J.), as stored
   --  in Zip archives. Subject to change, this is why this type is private.
   type Time is new Interfaces.Unsigned_32;

   Default_Time : constant Time := 16789 * 65536;

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with record
      Name              : Unbounded_String;
      Modification_Time : Time := Default_Time;
   end record;

   --  Memory_Zipstream spec
   type Memory_Zipstream is new Root_Zipstream_Type with record
      Unb : Unbounded_String;
      Loc : Integer := 1;
   end record;

   --  Read data from the stream
   overriding
   procedure Read
     (Stream : in out Memory_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   --  Write data to the stream, starting from the current index.
   --  Data will be overwritten from index if already available.
   overriding
   procedure Write (Stream : in out Memory_Zipstream; Item : Stream_Element_Array);

   --  Set the index on the stream
   overriding
   procedure Set_Index (S : in out Memory_Zipstream; To : Zs_Index_Type);

   --  Returns the index of the stream
   overriding
   function Index (S : in Memory_Zipstream) return Zs_Index_Type;

   --  Returns the Size of the stream
   overriding
   function Size (S : in Memory_Zipstream) return Zs_Size_Type;

   --  Returns true if the index is at the end of the stream
   overriding
   function End_Of_Stream (S : in Memory_Zipstream) return Boolean;

   -----------------------------------------------------------------------------

   type Open_File is limited new Ada.Finalization.Limited_Controlled with record
      File      : Ada.Streams.Stream_IO.File_Type;
      Finalized : Boolean := False;
   end record;

   overriding
   procedure Finalize (Object : in out Open_File);

   -----------------------------------------------------------------------------

   --  File_Zipstream spec
   type File_Zipstream is new Root_Zipstream_Type with record
      File : Open_File;
   end record;

   --  Read data from the stream
   overriding
   procedure Read
     (Stream : in out File_Zipstream;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   --  Write data to the stream, starting from the current index.
   --  Data will be overwritten from index if already available.
   overriding
   procedure Write (Stream : in out File_Zipstream; Item : Stream_Element_Array);

   --  Set the index on the stream
   overriding
   procedure Set_Index (S : in out File_Zipstream; To : Zs_Index_Type);

   --  Returns the index of the stream
   overriding
   function Index (S : in File_Zipstream) return Zs_Index_Type;

   --  Returns the Size of the stream
   overriding
   function Size (S : in File_Zipstream) return Zs_Size_Type;

   --  Returns true if the index is at the end of the stream
   overriding
   function End_Of_Stream (S : in File_Zipstream) return Boolean;

end Zip_Streams;
