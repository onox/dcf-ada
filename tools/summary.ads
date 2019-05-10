with Unzip;

package Summary is

   Total_Uncompressed, Total_Compressed : Unzip.File_Size_Type;
   Total_Entries                        : Natural;
   Files_Per_Method                     : array (Unzip.Pkzip_Method) of Natural;
   Uncompressed_Per_Method,
   Compressed_Per_Method : array (Unzip.Pkzip_Method) of Unzip.File_Size_Type;

   procedure Reset;

   function Nice_Image (Format : Unzip.Pkzip_Method) return String;

end Summary;
