-- LZMA_Decoding - Ada 95 translation of LzmaSpec.cpp, LZMA Reference Decoder 9.31
-- LzmaSpec.cpp : 2013-07-28 : Igor Pavlov : Public domain

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;                    use Ada.Exceptions;

package body LZMA.Decoding is

  type Byte_buffer is array(UInt32 range <>) of Byte;
  type p_Byte_buffer is access Byte_buffer;

  type Out_Window is record
    buf       : p_Byte_buffer := null;
    pos       : UInt32        := 0;
    size      : UInt32;
    is_full   : Boolean       := False;
    total_pos : Unsigned      := 0;
  end record;

  procedure Create(o: in out Out_Window; dictSize: UInt32) is
  begin
    o.buf  := new Byte_buffer(0..dictSize-1);
    o.size := dictSize;
  end Create;

  type Range_Decoder is record
    width     : UInt32  := 16#FFFF_FFFF#;  --  (*)
    code      : UInt32  := 0;
    corrupted : Boolean := False;
  end record;
  --  (*) called "range" in LZMA spec and "remaining width" in G.N.N. Martin's
  --      article about range encoding.

  procedure Init_Range_Decoder(o: in out Range_Decoder) is
  begin
    if Read_Byte /= 0 then
      o.corrupted := True;
    end if;
    for i in 0..3 loop
      o.code := Shift_Left(o.code, 8) or UInt32(Read_Byte);
    end loop;
    if o.code = o.width then
      o.corrupted := True;
    end if;
  end Init_Range_Decoder;

  kNumLenToPosStates  : constant := 4;
  kEndPosModelIndex   : constant := 14;
  kNumFullDistances   : constant := 2 ** (kEndPosModelIndex / 2);

  subtype Slot_coder_range is Unsigned range 0 .. kNumLenToPosStates - 1;
  type Slot_Coder_Probs is array(Slot_coder_range) of Probs_6_bits;

  LZMA_DIC_MIN : constant := 2 ** 12;

  procedure Decode_Properties(o: in out LZMA_Decoder_Info; b: Byte_buffer) is
    d: Unsigned := Unsigned(b(b'First));
  begin
    if d >= 9 * 5 * 5 then
      Raise_Exception(LZMA_Error'Identity, "Incorrect LZMA properties");
      -- raise LZMA_Error with "Incorrect LZMA properties"; -- Ada 2005+
    end if;
    o.lc := Literal_context_bits_range(d mod 9);
    d := d / 9;
    o.lp := Literal_position_bits_range(d mod 5);
    o.pb := Position_bits_range(d / 5);
    o.dictSizeInProperties := 0;
    for i in 0..3 loop
      o.dictSizeInProperties := o.dictSizeInProperties +
        UInt32(b(UInt32(i) + 1 + b'First)) * 2 ** (8 * i);
    end loop;
    o.dictSize := o.dictSizeInProperties;
    if o.dictSize < LZMA_DIC_MIN then
      o.dictSize := LZMA_DIC_MIN;
    end if;
  end Decode_Properties;

  procedure Decode_Contents(o: in out LZMA_Decoder_Info; res: out LZMA_Result) is
    state : State_range := 0;
    rep0, rep1, rep2, rep3 : UInt32 := 0;
    pos_state: Pos_state_range;
    -- Local copies of invariant properties.
    unpack_size_def: constant Boolean:= o.unpackSizeDefined;
    literal_pos_mask: constant UInt32:= 2 ** o.lp - 1;
    lc: constant Literal_context_bits_range:= o.lc;
    --
    use type BIO.Count;
    Marker_exit: exception;
    out_win : Out_Window;
    -- Local range decoder
    loc_range_dec: Range_Decoder;
    --
    -- Literals:
    subtype Lit_range is Unsigned range 0 .. 16#300# * 2 ** (o.lc + o.lp) - 1;  -- max 3,145,727
    LitProbs             : CProb_array(Lit_range):= (others => Initial_probability);
    -- Distances:
    subtype Pos_dec_range is Unsigned range 0..kNumFullDistances - kEndPosModelIndex;
    PosSlotDecoder       : Slot_Coder_Probs := (others => (others => Initial_probability));
    AlignDecoder         : Probs_NAB_bits:= (others => Initial_probability);
    PosDecoders          : CProb_array(Pos_dec_range):= (others => Initial_probability);
    --
    subtype Long_range is Unsigned range 0..States_count * Max_pos_states_count - 1;
    IsRep                : CProb_array(State_range):= (others => Initial_probability);
    IsRepG0              : CProb_array(State_range):= (others => Initial_probability);
    IsRepG1              : CProb_array(State_range):= (others => Initial_probability);
    IsRepG2              : CProb_array(State_range):= (others => Initial_probability);
    IsRep0Long           : CProb_array(Long_range):= (others => Initial_probability);
    IsMatch              : CProb_array(Long_range):= (others => Initial_probability);
    len_decoder          : Probs_for_LZ_Lengths;
    rep_len_decoder      : Probs_for_LZ_Lengths;
    --

    --  This corresponds to G.N.N. Martin's revised algorithm's adding of
    --  trailing digits - for encoding. Here we decode and know the encoded
    --  data, brought by Read_Byte.
    procedure Normalize_Q is
    pragma Inline(Normalize_Q);
      kTopValue : constant := 2**24;
    begin
      if loc_range_dec.width < kTopValue then
        loc_range_dec.width := Shift_Left(loc_range_dec.width, 8);
        loc_range_dec.code  := Shift_Left(loc_range_dec.code, 8) or UInt32(Read_Byte);
      end if;
    end Normalize_Q;

    procedure Decode_Bit_Q(prob_io: in out CProb; symbol: out Unsigned) is
    pragma Inline(Decode_Bit_Q);
      prob: constant CProb:= prob_io; -- Local copy
      bound: constant UInt32:= Shift_Right(loc_range_dec.width, Probability_model_bits) * prob;
    begin
      if loc_range_dec.code < bound then
        --  Increase probability. In [0, 1] it would be: prob:= prob + (1 - prob / 2 ** m)
        prob_io:= prob + Shift_Right(Probability_model_count - prob, Probability_change_bits);
        --  The new range is [0, bound[.
        --  Set new width.
        loc_range_dec.width := bound;
        Normalize_Q;
        symbol := 0;
      else
        --  Decrease probability: prob:= prob - prob / 2 ** m = prob:= prob * (1 - 2 ** m)
        prob_io:= prob - Shift_Right(prob, Probability_change_bits);
        --  The new range is [bound, width[. We shift the code and implicitely
        --  the range's limits by -bound in order to have a 0 lower limit for the range.
        loc_range_dec.code  := loc_range_dec.code - bound;
        --  Set new width.
        loc_range_dec.width := loc_range_dec.width - bound;
        Normalize_Q;
        symbol := 1;
      end if;
    end Decode_Bit_Q;

    function Is_Empty return Boolean is
    pragma Inline(Is_Empty);
    begin
      return out_win.pos = 0 and then not out_win.is_full;
    end Is_Empty;

    procedure Put_Byte_Q(b: Byte) is
    pragma Inline(Put_Byte_Q);
    begin
      out_win.total_pos := out_win.total_pos + 1;
      out_win.buf(out_win.pos):= b;
      out_win.pos := out_win.pos + 1;
      if out_win.pos = out_win.size then
        out_win.pos := 0;
        out_win.is_full := True;
      end if;
      Write_Byte(b);
    end Put_Byte_Q;

    function Get_Byte_Q(dist: UInt32) return Byte is
    pragma Inline(Get_Byte_Q);
    begin
      if dist <= out_win.pos then
        return out_win.buf(out_win.pos - dist);
      else
        return out_win.buf(out_win.pos - dist + out_win.size);
      end if;
    end Get_Byte_Q;

    procedure Process_Literal is
    pragma Inline(Process_Literal);
      prevByte     : Byte:= 0;
      symbol       : Unsigned:= 1;
      lit_state    : Unsigned;
      probs_idx    : Unsigned;
      bit          : Unsigned;
    begin
      if o.unpackSize = 0 and then unpack_size_def then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (Process_Literal)"
        );
      end if;
      --
      if not Is_Empty then
        prevByte := Get_Byte_Q(1);
      end if;
      lit_state :=
        Unsigned(
          Shift_Left(UInt32(out_win.total_pos) and literal_pos_mask, lc) +
          Shift_Right(UInt32(prevByte), 8 - lc)
        );
      probs_idx:= 16#300# * lit_state;
      if state < 7 then
        loop
          Decode_Bit_Q(LitProbs(probs_idx + symbol), bit);
          symbol := (symbol + symbol) or bit;
          exit when symbol >= 16#100#;
        end loop;
      else
        declare
          --
          --  The probabilities used for decoding this literal assume
          --  that the current literal sequence resembles to the last
          --  distance-length copied sequence.
          --
          match_byte     : UInt32 := UInt32(Get_Byte_Q(rep0 + 1));
          match_bit      : UInt32;    -- either 0 or 16#100#
          prob_idx_match : Unsigned;  -- either 0 (normal case without match), 16#100# or 16#200#
          bit, bit_b     : Unsigned;
        begin
          loop
            match_byte := match_byte + match_byte;
            match_bit  := match_byte and 16#100#;
            prob_idx_match:= Unsigned(16#100# + match_bit);
            Decode_Bit_Q(LitProbs(probs_idx + prob_idx_match + symbol), bit);
            symbol := (symbol + symbol) or bit;
            exit when symbol >= 16#100#;
            if match_bit /= Shift_Left(UInt32(bit), 8) then
              -- No bit match, then give up byte match
              loop
                Decode_Bit_Q(LitProbs(probs_idx + symbol), bit_b);
                symbol := (symbol + symbol) or bit_b;
                exit when symbol >= 16#100#;
              end loop;
              exit;
            end if;
          end loop;
        end;
      end if;
      Put_Byte_Q(Byte(symbol - 16#100#)); -- The output of a simple literal happens here.
      --
      state := Update_State_Literal(state);
      o.unpackSize:= o.unpackSize - 1;
    end Process_Literal;

    dict_size : constant UInt32:= o.dictSize;

    function Is_Finished_OK return Boolean is
    pragma Inline(Is_Finished_OK);
    begin
      return loc_range_dec.code = 0;
    end Is_Finished_OK;

    procedure Process_Distance_and_Length is
    pragma Inline(Process_Distance_and_Length);
      --
      procedure Bit_Tree_Decode(
        prob     : in out CProb_array;
        num_bits :        Positive;
        m        :    out Unsigned)
      is
      pragma Inline(Bit_Tree_Decode);
        bit: Unsigned;
      begin
        m:= 1;
        for count in reverse 1..num_bits loop
          Decode_Bit_Q(prob(m + prob'First), bit);
          m:= m + m + bit;
        end loop;
        m:= m - 2**num_bits;
      end Bit_Tree_Decode;
      --
      len: Unsigned:= 0;
      --
      procedure Copy_Match_Q2(dist: UInt32) is
        pragma Inline(Copy_Match_Q2);
        b2, b3: Byte;
        len32: constant UInt32:= UInt32(len);
        will_fill: constant Boolean:= out_win.pos + len32 >= out_win.size;
        --
        procedure Easy_case is
        pragma Inline(Easy_case);
          src_from, src_to: UInt32;
          b: Byte;
        begin
          -- src and dest within circular buffer bounds. May overlap (len32 > dist).
          src_from := out_win.pos - dist;
          src_to   := out_win.pos - dist + len32 - 1;
          if len32 <= dist then -- No overlap: src_to < out_win.pos
            out_win.buf(out_win.pos .. out_win.pos + len32 - 1):= out_win.buf(src_from .. src_to);
            for i in src_from .. src_to loop
              Write_Byte(out_win.buf(i));
            end loop;
          else -- Overlap: to >= out_win.pos . Need to copy in forward order.
            for i in src_from .. src_to loop
              b:= out_win.buf(i);
              out_win.buf(i + dist):= b;
              Write_Byte(b);
            end loop;
          end if;
          out_win.pos := out_win.pos + len32;
        end Easy_case;
        --
        procedure Modulo_case is
        pragma Inline(Modulo_case);
        begin
          -- src starts below 0 or dest goes beyond size-1
          for count in reverse 1..len loop
            if dist <= out_win.pos then
              b2:= out_win.buf(out_win.pos - dist);
              out_win.buf(out_win.pos):= b2;
              out_win.pos := out_win.pos + 1;
              if out_win.pos = out_win.size then
                out_win.pos := 0;
              end if;
              Write_Byte(b2);
            else
              b3:= out_win.buf(out_win.size - dist + out_win.pos);
              out_win.buf(out_win.pos):= b3;
              out_win.pos := out_win.pos + 1;
              if out_win.pos = out_win.size then
                out_win.pos := 0;
              end if;
              Write_Byte(b3);
            end if;
          end loop;
        end Modulo_case;
      begin
        out_win.is_full := will_fill or else out_win.is_full;
        out_win.total_pos := out_win.total_pos + len;
        if dist <= out_win.pos and not will_fill then
          Easy_case;
        else
          Modulo_case;
        end if;
      end Copy_Match_Q2;
      --
      procedure Decode_Distance(dist: out UInt32) is
      pragma Inline(Decode_Distance);
        --
        decode_direct: UInt32;
        --
        procedure Decode_Direct_Bits(num_bits : Natural) is
        pragma Inline(Decode_Direct_Bits);
          t: UInt32;
        begin
          decode_direct := 0;
          for count in reverse 1..num_bits loop
            loc_range_dec.width := Shift_Right(loc_range_dec.width, 1);
            loc_range_dec.code := loc_range_dec.code - loc_range_dec.width;
            t := - Shift_Right(loc_range_dec.code, 31);
            loc_range_dec.code := loc_range_dec.code + (loc_range_dec.width and t);
            if loc_range_dec.code = loc_range_dec.width then
              loc_range_dec.corrupted := True;
            end if;
            Normalize_Q;
            decode_direct := decode_direct + decode_direct + t + 1;
          end loop;
        end Decode_Direct_Bits;
        --
        procedure Bit_Tree_Reverse_Decode(prob: in out CProb_array; num_bits: in Natural) is
        pragma Inline(Bit_Tree_Reverse_Decode);
          m: Unsigned := 1;
          bit: Unsigned;
        begin
          for i in 0..num_bits-1 loop
            Decode_Bit_Q(prob(m + prob'First), bit);
            m := m + m + bit;
            dist := dist or Shift_Left(UInt32(bit), i);
          end loop;
        end Bit_Tree_Reverse_Decode;
        --
        len_state     : Unsigned := len;  --  len has been set up by Decode_Length previously
        posSlot       : Unsigned;
        numDirectBits : Natural;
        --
      begin -- Decode_Distance
        if len_state > kNumLenToPosStates - 1 then
          len_state := kNumLenToPosStates - 1;
        end if;
        Bit_Tree_Decode(PosSlotDecoder(len_state), 6, posSlot);
        if posSlot < 4 then
          dist:= UInt32(posSlot);
          return;
        end if;
        numDirectBits := Natural(Shift_Right(UInt32(posSlot), 1) - 1);
        dist := Shift_Left(2 or (UInt32(posSlot) and 1), numDirectBits);
        if posSlot < kEndPosModelIndex then
          Bit_Tree_Reverse_Decode(
            PosDecoders(Unsigned(dist) - posSlot .. Pos_dec_range'Last),
            numDirectBits
          );
        else
          Decode_Direct_Bits(numDirectBits - Align_bits);
          dist:= dist + Shift_Left(decode_direct, Align_bits);
          Bit_Tree_Reverse_Decode(AlignDecoder, Align_bits);
        end if;
      end Decode_Distance;
      --
      procedure Decode_Length(o: in out Probs_for_LZ_Lengths) is
      pragma Inline(Decode_Length);
        bit_a, bit_b: Unsigned;
      begin
        Decode_Bit_Q(o.choice_1, bit_a);
        if bit_a = 0 then
          Bit_Tree_Decode(o.low_coder(pos_state), 3, len);
          return;
        end if;
        Decode_Bit_Q(o.choice_2, bit_b);
        if bit_b = 0 then
          Bit_Tree_Decode(o.mid_coder(pos_state), 3, len);
          len:= len + 8;
          return;
        end if;
        Bit_Tree_Decode(o.high_coder, 8, len);
        len:= len + 16;
      end Decode_Length;
      --
      function Check_Distance return Boolean is
      pragma Inline(Check_Distance);
      begin
        return rep0 <= out_win.pos or out_win.is_full;
      end Check_Distance;
      --
      isError: Boolean;
      dist: UInt32;
      bit_a, bit_b, bit_c, bit_d, bit_e: Unsigned;
      kMatchMinLen : constant := 2;
      --
    begin -- Process_Distance_and_Length
      Decode_Bit_Q(IsRep(state), bit_a);
      if bit_a /= 0 then
        if o.unpackSize = 0 and then unpack_size_def then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #1)"
          );
        end if;
        if Is_Empty then
          Raise_Exception(
            LZMA_Error'Identity,
            "Output window buffer is empty (in Process_Distance_and_Length)"
          );
        end if;
        Decode_Bit_Q(IsRepG0(state), bit_b);
        if bit_b = 0 then
          Decode_Bit_Q(IsRep0Long(state * Max_pos_states_count + pos_state), bit_c);
          if bit_c = 0 then
            state := Update_State_ShortRep(state);
            Put_Byte_Q(Get_Byte_Q(rep0 + 1));
            o.unpackSize:= o.unpackSize - 1;
            return;  -- GdM: this way, we go to the next iteration (C++: continue)
          end if;
        else
          Decode_Bit_Q(IsRepG1(state), bit_d);
          if bit_d = 0 then
            dist := rep1;
          else
            Decode_Bit_Q(IsRepG2(state), bit_e);
            if bit_e = 0 then
              dist := rep2;
            else
              dist := rep3;
              rep3 := rep2;
            end if;
            rep2 := rep1;
          end if;
          rep1 := rep0;
          rep0 := dist;
        end if;
        Decode_Length(rep_len_decoder);
        state := Update_State_Rep(state);
      else
        rep3 := rep2;
        rep2 := rep1;
        rep1 := rep0;
        Decode_Length(len_decoder);
        state := Update_State_Match(state);
        Decode_Distance(dist => rep0);
        if rep0 = 16#FFFF_FFFF# then
          if Is_Finished_OK then
            raise Marker_exit;
          else
            Raise_Exception(
              LZMA_Error'Identity,
              "Range decoder not finished on EOS marker (in Process_Distance_and_Length)"
            );
          end if;
        end if;
        if (o.unpackSize = 0 and then unpack_size_def) or
            rep0 >= dict_size or not Check_Distance
        then
          Raise_Exception(
            LZMA_Error'Identity,
            "Decoded data will exceed expected data size (in Process_Distance_and_Length, #2)"
          );
        end if;
      end if;
      len := len + kMatchMinLen;
      isError := False;
      if o.unpackSize < Data_Bytes_Count(len) and then unpack_size_def then
        len := Unsigned(o.unpackSize);
        isError := True;
      end if;
      -- The LZ distance/length copy happens here.
      -- Copy_Match(out_win, rep0 + 1, len);
      Copy_Match_Q2(rep0 + 1);
      o.unpackSize:= o.unpackSize - Data_Bytes_Count(len);
      if isError then
        Raise_Exception(
          LZMA_Error'Identity,
          "Decoded data will exceed expected data size (in Process_Distance_and_Length, #3)"
        );
      end if;
    end Process_Distance_and_Length;

    bit_choice: Unsigned;
    pos_bits_mask : constant UInt32 := 2 ** o.pb - 1;
    size_defined_and_marker_not_mandatory: constant Boolean:=
      unpack_size_def and not o.markerIsMandatory;

    procedure Finalize is
      procedure Dispose is new Ada.Unchecked_Deallocation(Byte_buffer, p_Byte_buffer);
    begin
      Dispose(out_win.buf);
      o.range_dec_corrupted:= loc_range_dec.corrupted;
    end Finalize;

  begin
    Create(out_win, o.dictSize);
    Init_Range_Decoder(loc_range_dec);
    loop
      if o.unpackSize = 0
        and then Is_Finished_OK
        and then size_defined_and_marker_not_mandatory
      then
        res:= LZMA_finished_without_marker;
        Finalize;
        return;
      end if;
      pos_state := Pos_state_range(UInt32(out_win.total_pos) and pos_bits_mask);
      Decode_Bit_Q(IsMatch(state * Max_pos_states_count + pos_state), bit_choice);
      -- LZ decoding happens here: either we have a new literal in 1 byte, or we copy past data.
      if bit_choice = 0 then
        Process_Literal;
      else
        Process_Distance_and_Length;
      end if;
    end loop;
  exception
    when Marker_exit =>
      res:= LZMA_finished_with_marker;
      Finalize;
  end Decode_Contents;

  procedure Decode_Header(o: in out LZMA_Decoder_Info; hints: LZMA_Hints) is
    header: Byte_buffer(0..12);
    b: Byte;
    use type BIO.Count;
    last_bit: Natural;
  begin
    o.unpackSize := 0;
    o.unpackSizeDefined := False;

    for i in header'Range loop
      header(i):= Read_Byte;
      exit when i = 4 and not hints.has_size;
    end loop;

    Decode_Properties(o, header);

    if hints.has_size then
      for i in UInt32'(0)..7 loop
        b:= header(5 + i);
        if b /= 16#FF# then
          o.unpackSizeDefined := True;
        end if;
      end loop;
      if o.unpackSizeDefined then
        for i in UInt32'(0)..7 loop
          b:= header(5 + i);
          if b /= 0 then
            for bit in 0..7 loop
              if (b and Shift_Left(Byte'(1),bit)) /= 0 then
                last_bit:= bit;
              end if;
            end loop;
            last_bit:= last_bit + Natural(8 * i);
            if last_bit > Data_Bytes_Count'Size - 1 then
              Raise_Exception(
                LZMA_Error'Identity,
                "Indicated size bits for decoded data," &
                Natural'Image(last_bit) &
                ", exceeds the maximum file size bits," &
                Natural'Image(Data_Bytes_Count'Size - 1)
              );
            else
              o.unpackSize := o.unpackSize + Data_Bytes_Count(b) * 2 ** Natural(8 * i);
            end if;
          end if;
        end loop;
        o.unpackSize_as_defined:= o.unpackSize;
      else
        o.unpackSize:= Data_Bytes_Count'Last;
      end if;
    else
      o.unpackSize:= hints.given_size;
      o.unpackSizeDefined:= True;
    end if;
    o.markerIsMandatory := hints.marker_expected or not o.unpackSizeDefined;
  end Decode_Header;

  procedure Decode(o: in out LZMA_Decoder_Info; hints: LZMA_Hints; res: out LZMA_Result) is
  begin
    Decode_Header(o, hints);
    Decode_Contents(o, res);
    if hints.fail_on_bad_range_code and o.range_dec_corrupted then
      Raise_Exception(LZMA_Error'Identity, "Range decoder had a corrupted value (code = range)");
    end if;
  end Decode;

  procedure Decompress(hints: LZMA_Hints) is
    o: LZMA_Decoder_Info;
    res: LZMA_Result;
  begin
    Decode(o, hints, res);
  end Decompress;

  function Literal_context_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.lc;
  end Literal_context_bits;

  function Literal_pos_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.lp;
  end Literal_pos_bits;

  function Pos_bits(o: LZMA_Decoder_Info) return Natural is
  begin
    return o.pb;
  end Pos_bits;

  function Unpack_size_defined(o: LZMA_Decoder_Info) return Boolean is
  begin
    return o.unpackSizeDefined;
  end Unpack_size_defined;

  function Unpack_size_as_defined(o: LZMA_Decoder_Info) return Data_Bytes_Count is
  begin
    return o.unpackSize_as_defined;
  end Unpack_size_as_defined;

  function Dictionary_size(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return o.dictSize;
  end Dictionary_size;

  function Dictionary_size_in_properties(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32 is
  begin
    return o.dictSizeInProperties;
  end Dictionary_size_in_properties;

  function Range_decoder_corrupted(o: LZMA_Decoder_Info) return Boolean is
  begin
    return o.range_dec_corrupted;
  end Range_decoder_corrupted;

end LZMA.Decoding;
