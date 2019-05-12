with Ada.Characters.Handling;

package body File_Operations is

   Directory_Separator : constant Character := '/';
   --  '/' is also accepted by Windows

   function Add_Extract_Directory (File_Name : String) return String is
   --  OK for UNIX & Windows, but VMS has "[x.y.z]filename.ext"
   begin
      if Exdir_Len = 0 then
         return File_Name;
      elsif Exdir (Exdir_Len) = '\' or Exdir (Exdir_Len) = '/' then
         return Exdir (1 .. Exdir_Len) & File_Name;
      else
         return Exdir (1 .. Exdir_Len) & Directory_Separator & File_Name;
      end if;
   end Add_Extract_Directory;

   function Compose_File_Name
     (File_Name     : String;
      Name_Encoding : Zip.Zip_Name_Encoding) return String
   is
      use Ada.Characters.Handling;

      Fn1 : String := File_Name;
   begin
      if Lower_Case_Match then
         Fn1 := To_Lower (Fn1);
      end if;
      return Add_Extract_Directory (Fn1);
   end Compose_File_Name;

end File_Operations;
