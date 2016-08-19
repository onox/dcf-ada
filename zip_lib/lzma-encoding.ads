--  LZMA_Encoding - a standalone, generic LZMA encoder.
--  See body for credits and other informations.

--  Examples of use:
--    LZMA_Enc, a standalone encoder to .lzma files
--    Zip.Compress.LZMA_E, creates Zip files entries with LZMA encoding

with Interfaces;

package LZMA.Encoding is

  subtype Byte is Interfaces.Unsigned_8;

  type Compression_level is (
    Level_1,  --  Faster but weaker compression
    Level_2   --  Slower but stronger compression
  );

  generic
    -- Input of data:
    with function  Read_byte return Byte;
    with function  More_bytes return Boolean;
    -- Output of LZMA-compressed data:
    with procedure Write_byte (b: Byte);
    --
  procedure Encode(
    level                 : Compression_level           := Level_1;
    literal_context_bits  : Literal_context_bits_range  := 3;  --  Bits of last byte are used.
    literal_position_bits : Literal_position_bits_range := 0;  --  Position mod 2**bits is used.
    position_bits         : Position_bits_range         := 2;  --  Position mod 2**bits is used.
    end_marker            : Boolean:= True;  --  Produce an End-Of-Stream marker ?
    uncompressed_size_info: Boolean:= False  --  Optional header appendix, needed for .lzma files
  );

end LZMA.Encoding;
