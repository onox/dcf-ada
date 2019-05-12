with Zip;

package File_Operations is

   Lower_Case_Match : Boolean := False;

   Exdir     : String (1 .. 1024);
   Exdir_Len : Natural := 0;

   function Compose_File_Name
     (File_Name     : String;
      Name_Encoding : Zip.Zip_Name_Encoding) return String;

end File_Operations;
