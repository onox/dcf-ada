package body DCF.Streams.Calendar is

   procedure Set_Time (S : out Root_Zipstream_Type'Class; Modification_Time : Ada.Calendar.Time) is
   begin
      Set_Time (S, Calendar.Convert (Modification_Time));
   end Set_Time;

   function Get_Time (S : in Root_Zipstream_Type'Class) return Ada.Calendar.Time is
   begin
      return Calendar.Convert (Get_Time (S));
   end Get_Time;

   ------------------------------------------------
   --  Time = DOS Time. Valid through Year 2107  --
   ------------------------------------------------

   procedure Split
     (Date    :     Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      D_Date       : constant Integer := Integer (Date / 65536);
      D_Time       : constant Integer := Integer (Date and 65535);
      X            : Integer;
      Hours        : Integer;
      Minutes      : Integer;
      Seconds_Only : Integer;
   begin
      Year := 1980 + D_Date / 512;
      X    := (D_Date / 32) mod 16;
      if X not in Month_Number then -- that is 0, or in 13..15
         raise Time_Error;
      end if;
      Month := X;
      X     := D_Date mod 32;
      if X not in Day_Number then -- that is 0
         raise Time_Error;
      end if;
      Day          := X;
      Hours        := D_Time / 2048;
      Minutes      := (D_Time / 32) mod 64;
      Seconds_Only := 2 * (D_Time mod 32);
      if Hours not in 0 .. 23 or Minutes not in 0 .. 59 or Seconds_Only not in 0 .. 59 then
         raise Time_Error;
      end if;
      Seconds := Day_Duration (Hours * 3600 + Minutes * 60 + Seconds_Only);
   end Split;

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time
   is
      Year_2       : Integer := Year;
      Hours        : Unsigned_32;
      Minutes      : Unsigned_32;
      Seconds_Only : Unsigned_32;
      Seconds_Day  : Unsigned_32;
      Result       : Unsigned_32;
   begin
      if Year_2 < 1980 then  --  Avoid invalid DOS date
         Year_2 := 1980;
      end if;
      Seconds_Day  := Unsigned_32 (Seconds);
      Hours        := Seconds_Day / 3600;
      Minutes      := (Seconds_Day / 60) mod 60;
      Seconds_Only := Seconds_Day mod 60;
      Result       :=
        --  MSDN formula for encoding:
        Unsigned_32 ((Year_2 - 1980) * 512 + Month * 32 + Day) * 65536  --  Date
        +
        Hours * 2048 +
        Minutes * 32 +
        Seconds_Only / 2; -- Time
      return Time (Result);
   end Time_Of;

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Unsigned_32 (Left) > Unsigned_32 (Right);
   end ">";

   function Convert (Date : in Ada.Calendar.Time) return Time is
      Year            : Year_Number;
      Month           : Month_Number;
      Day             : Day_Number;
      Seconds_Day_Dur : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds_Day_Dur);
      return Time_Of (Year, Month, Day, Seconds_Day_Dur);
   end Convert;

   function Convert (Date : in Time) return Ada.Calendar.Time is
      Year            : Year_Number;
      Month           : Month_Number;
      Day             : Day_Number;
      Seconds_Day_Dur : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds_Day_Dur);
      return Time_Of (Year, Month, Day, Seconds_Day_Dur);
   end Convert;

end DCF.Streams.Calendar;
