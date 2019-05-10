with Ada.Text_IO;

with My_Dots;

use My_Dots;
use Ada.Text_IO;

procedure My_Feedback
  (Percents_Done : in     Natural;
   Entry_Skipped : in     Boolean;
   User_Abort    :    out Boolean)
is
   New_Done_Dots : constant Natural := (Dots * Percents_Done) / 100;
begin
   if Entry_Skipped then
      Put ("-skipped-");
   else
      for I in Done_Dots + 1 .. New_Done_Dots loop
         if I = 1 then
            Put ('[');
         end if;
         Put ('.');
         if I = Dots then
            Put (']');
         end if;
      end loop;
      Done_Dots := New_Done_Dots;
   end if;
   User_Abort := False;  --  Pointless in this command-line version (Ctrl-C is ok)
end My_Feedback;
