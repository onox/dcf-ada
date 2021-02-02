with Ada.Calendar;

package DCF.Streams.Calendar is

   --  Set_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   procedure Set_Time (S : out Root_Zipstream_Type'Class; Modification_Time : Ada.Calendar.Time);

   --  Get_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   function Get_Time (S : in Root_Zipstream_Type'Class) return Ada.Calendar.Time;

   ----------------------------
   --  Routines around Time  --
   ----------------------------

   function Convert (Date : in Ada.Calendar.Time) return Time;
   function Convert (Date : in Time) return Ada.Calendar.Time;

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

end DCF.Streams.Calendar;
