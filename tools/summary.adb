with Zip;

package body Summary is

   procedure Reset is
   begin
      Total_Uncompressed      := 0;
      Total_Compressed        := 0;
      Total_Entries           := 0;
      Files_Per_Method        := (others => 0);
      Uncompressed_Per_Method := (others => 0);
      Compressed_Per_Method   := (others => 0);
   end Reset;

   function Nice_Image (Format : Unzip.Pkzip_Method) return String is
      Img_Stuffed : String (1 .. Unzip.Pkzip_Method'Width) := (others => ' ');
      Img         : constant String                        := Zip.Image (Format);
   begin
      Img_Stuffed (1 .. Img'Length) := Img;
      return Img_Stuffed;
   end Nice_Image;

end Summary;
