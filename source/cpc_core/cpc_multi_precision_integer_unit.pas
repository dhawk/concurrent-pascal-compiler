UNIT cpc_multi_precision_integer_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses mp_types, mp_base;

type
   TLargestPascalSupportedInt = Int64;
   TLargestPascalSupportedUnsigned = UInt64;

type
   TMultiPrecisionInteger =
      class
      private
         mpi: mp_int;
         int_value_for_debug_display: integer;
         procedure set_int(i: TLargestPascalSupportedInt);
         function get_int: TLargestPascalSupportedInt;
         procedure set_unsigned(u: TLargestPascalSupportedUnsigned);
         function get_unsigned: TLargestPascalSupportedUnsigned;
         function get_real: real;
      public
         constructor Create; overload;
         constructor Create(i: TMultiPrecisionInteger); overload;
         constructor Create(i: integer); overload;
         destructor Destroy; override;

         function InIntegerRange: boolean; // true if low(TLargestPascalSupportedInt) <= value <= high(TLargestPascalSupportedInt)
         function InUnsignedRange: boolean; // true if 0 <= value <= high(TLargestPascalSupportedUnsigned)

         property AsInteger: TLargestPascalSupportedInt read get_int write set_int;
         property AsUnsigned: TLargestPascalSupportedUnsigned read get_unsigned write set_unsigned;
         property AsReal: real read get_real;
         function AsByte(idx: cardinal): byte;
         function AsString: string;

         procedure Assign (i: TMultiPrecisionInteger);     // self := i

         procedure Add(i: TMultiPrecisionInteger); // self := self + i
            overload;
         procedure Add(i: integer); // self := self + i
            overload;
         procedure Subtract(i: TMultiPrecisionInteger); // self := self - i
            overload;
         procedure Subtract(i: integer); // self := self - i
            overload;
         procedure Multiply(i: TMultiPrecisionInteger); // self := self * i
            overload;
         procedure Multiply(i: integer); // self := self * i
            overload;
         procedure Divide(i: TMultiPrecisionInteger); // self := self div i
            overload;
         procedure Divide(i: integer); // self := self div i
            overload;
         procedure iso_std_pascal_mod (j: TMultiPrecisionInteger);  // self := self mod j
         procedure delphi_mod (j: TMultiPrecisionInteger);  // self := self mod j

         function BitsRequiredForUnsignedValue: integer;
         function BitsRequiredForSignedValue: integer;
         function  IsPow2(var n: longint): boolean;
            overload;
         function  IsPow2: boolean;
            overload;

         procedure SetMaxSignedValue (num_bits: cardinal);
         procedure SetMinSignedValue (num_bits: cardinal);
         procedure SetMaxUnsignedValue (num_bits: cardinal);

         function IsNegative: boolean;
         procedure ChangeSign;
         procedure Set2ToExp(exp: cardinal); // self := 2 ^ exp
         procedure Abs;

         procedure shift_left (count: cardinal);
         procedure shift_right (count: cardinal);
         procedure logical_and (mask: TMultiPrecisionInteger);

         function lt(i: TMultiPrecisionInteger): boolean; overload;
         function le(i: TMultiPrecisionInteger): boolean; overload;
         function eq(i: TMultiPrecisionInteger): boolean; overload;
         function ne(i: TMultiPrecisionInteger): boolean; overload;
         function ge(i: TMultiPrecisionInteger): boolean; overload;
         function gt(i: TMultiPrecisionInteger): boolean; overload;

         function lt(i: integer): boolean; overload;
         function le(i: integer): boolean; overload;
         function eq(i: integer): boolean; overload;
         function ne(i: integer): boolean; overload;
         function ge(i: integer): boolean; overload;
         function gt(i: integer): boolean; overload;

         procedure Max (i: TMultiPrecisionInteger);   // self := max (self, i)
            overload;
         procedure Min (i: TMultiPrecisionInteger);
            overload;
         procedure Max (i: integer);   // self := max (self, i)
            overload;
         procedure Min (i: integer);
            overload;
      end;

IMPLEMENTATION

uses
   SysUtils;

var
   max_integer, min_integer, max_unsigned, lsb_mask, temp: TMultiPrecisionInteger;

constructor TMultiPrecisionInteger.Create;
   begin
      mp_init(mpi);
      mp_zero(mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

constructor TMultiPrecisionInteger.Create(i: TMultiPrecisionInteger);
   begin
      mp_init(mpi);
      mp_copy(i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

constructor TMultiPrecisionInteger.Create(i: integer);
   begin
      mp_init(mpi);
      mp_set_int(mpi, i);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

destructor TMultiPrecisionInteger.Destroy;
   begin
      mp_clear(mpi);
      inherited
   end;

function TMultiPrecisionInteger.InIntegerRange: boolean; // true if low(TLargestPascalSupportedInt) <= value <= high(TLargestPascalSupportedInt)
   begin
      result := (mp_cmp(mpi, min_integer.mpi) >= 0) and (mp_cmp(mpi, max_integer.mpi) <= 0)
   end;

function TMultiPrecisionInteger.InUnsignedRange: boolean; // true if 0 <= value <= high(TLargestPascalSupportedUnsigned)
   begin
      result := (mp_sign(mpi) >= 0) and (mp_cmp(mpi, max_unsigned.mpi) <= 0)
   end;

function TMultiPrecisionInteger.AsByte(idx: cardinal): byte;
   // note: change max_bytes, int64 and uint64 to larger if your pascal compiler supports it (e.g. 16, int128, uint128)
   var
      tempi: int64;   // 8 bytes
   begin
      if IsNegative then
         begin
            assert (idx < 8);
            tempi := AsInteger;
            result := (tempi shr (idx*8)) and $ff
         end
      else  // non-negative number
         begin
            temp.Assign (Self);
            temp.shift_right (idx * 8);
            temp.logical_and (lsb_mask);
            result := temp.AsUnsigned
         end
   end;

function TMultiPrecisionInteger.AsString: string;
   begin
      result := String(mp_adecimal (mpi))
   end;

procedure TMultiPrecisionInteger.set_int(i: TLargestPascalSupportedInt);
   begin
      mp_read_decimal_str(mpi, ShortString(IntToStr(i)));
      int_value_for_debug_display := mp_get_int (mpi)
   end;

function TMultiPrecisionInteger.get_int: TLargestPascalSupportedInt;
   begin
      assert(InIntegerRange);
      result := StrToInt64(String(mp_adecimal(mpi)))
   end;

procedure TMultiPrecisionInteger.set_unsigned(u: TLargestPascalSupportedUnsigned);
   begin
      mp_read_decimal_str(mpi, ShortString(format('%u', [u])));
      int_value_for_debug_display := mp_get_int (mpi)
   end;

function TMultiPrecisionInteger.get_unsigned: TLargestPascalSupportedUnsigned;
   var
      s: AnsiString;
      i: integer;
   begin
      assert(InUnsignedRange);
      s := mp_adecimal(mpi);
      result := 0;
      for i := 1 to Length(s)
      do result := (result * 10) + ord (s[i]) - ord('0')
   end;

function TMultiPrecisionInteger.get_real: real;
   begin
      result := StrToFloat (String(mp_adecimal(mpi)))
   end;

procedure TMultiPrecisionInteger.Assign (i: TMultiPrecisionInteger);
   begin
      mp_copy(i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Add(i: TMultiPrecisionInteger);
   begin
      mp_add(mpi, i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Add(i: integer);
   begin
      mp_add_int(mpi, i, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

function TMultiPrecisionInteger.BitsRequiredForUnsignedValue: integer;
   begin
      assert (ge(0));
      result := mp_bitsize(mpi)
   end;

function TMultiPrecisionInteger.BitsRequiredForSignedValue: integer;
   var
      test_value: TMultiPrecisionInteger;
   begin
      if ge(0) then
         result := mp_bitsize(mpi)+1
      else
         begin
            test_value := TMultiPrecisionInteger.Create;
            result := 0;
            repeat
               result := result + 1;
               test_value.AsInteger := -1;
               test_value.shift_left (result)
            until ge(test_value);
            result := result + 1;
            test_value.Free
         end
   end;

function  TMultiPrecisionInteger.IsPow2 (var n: longint): boolean;
   begin
      result := ge(0) and mp_is_pow2(mpi, n)
   end;

function  TMultiPrecisionInteger.IsPow2: boolean;
   var n: longint;
   begin
      result := ge(0) and mp_is_pow2(mpi, n)
   end;


function TMultiPrecisionInteger.IsNegative: boolean;
   begin
      result := mpi.sign = MP_NEG
   end;

procedure TMultiPrecisionInteger.ChangeSign;
   begin
      mp_chs(mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Set2ToExp(exp: cardinal);
   begin
      mp_2expt(mpi, exp);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.shift_left (count: cardinal);
   begin
      mp_shl (mpi, count, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.shift_right (count: cardinal);
   begin
      mp_shr (mpi, count, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.logical_and (mask: TMultiPrecisionInteger);
   begin
      assert (AsInteger >= 0);      // underlying mp_and does not handle negative numbers correctly
      assert (mask.AsInteger >= 0);
      mp_and (mpi, mask.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Abs;
   begin
      mp_abs (mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Subtract(i: TMultiPrecisionInteger);
   begin
      mp_sub(mpi, i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Subtract(i: integer);
   begin
      mp_sub_int(mpi, i, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Multiply(i: TMultiPrecisionInteger);
   begin
      mp_mul (mpi, i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Multiply(i: integer);
   begin
      mp_mul_int (mpi, i, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Divide(i: TMultiPrecisionInteger);
   begin
      mp_div (mpi, i.mpi, mpi);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Divide(i: integer);
   var remainder: integer;
   begin
      mp_div_int (mpi, i, @mpi, remainder);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.iso_std_pascal_mod (j: TMultiPrecisionInteger);  // self := self mod i
   var
      i: TMultiPrecisionInteger;
   begin
      i := TMultiPrecisionInteger.Create(self);
      assert (j.gt(0));
      Divide(j);
      Multiply(j);
      ChangeSign;
      Add(i);
      if lt(0) then
         Add(j);
      i.Free;
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.delphi_mod (j: TMultiPrecisionInteger);
   begin
      // use Delphi's mod IMPLEMENTATION - note unsigned mod signed doesn't work??? so use signed for all
      AsInteger := AsInteger mod j.AsInteger;
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.SetMaxSignedValue
   (num_bits: cardinal
   );
   begin
      assert (num_bits > 0);
      Set2ToExp (num_bits-1);
      Subtract (1)
   end;

procedure TMultiPrecisionInteger.SetMinSignedValue
   (num_bits: cardinal
   );
   begin
      assert (num_bits > 0);
      Set2ToExp (num_bits-1);
      ChangeSign
   end;

procedure TMultiPrecisionInteger.SetMaxUnsignedValue
   (num_bits: cardinal
   );
   begin
      assert (num_bits > 0);
      Set2ToExp (num_bits);
      Subtract (1)
   end;


function TMultiPrecisionInteger.lt(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_lt(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.le(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_le(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.eq(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_eq(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.ne(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_ne(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.ge(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_ge(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.gt(i: TMultiPrecisionInteger): boolean;
   begin
      result := mp_is_gt(mpi, i.mpi)
   end;

function TMultiPrecisionInteger.lt(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_lt(mpi, ii);
      mp_clear(ii)
   end;

function TMultiPrecisionInteger.le(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_le(mpi, ii);
      mp_clear(ii)
   end;

function TMultiPrecisionInteger.eq(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_eq(mpi, ii);
      mp_clear(ii)
   end;

function TMultiPrecisionInteger.ne(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_ne(mpi, ii);
      mp_clear(ii)
   end;

function TMultiPrecisionInteger.ge(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_ge(mpi, ii);
      mp_clear(ii)
   end;

function TMultiPrecisionInteger.gt(i: integer): boolean;
   var
      ii: mp_int;
   begin
      mp_init(ii);
      mp_set_int(ii, i);
      result := mp_is_gt(mpi, ii);
      mp_clear(ii)
   end;

procedure TMultiPrecisionInteger.Max (i: TMultiPrecisionInteger);
   begin
      if self.le(i) then
         Assign(i);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Max (i: integer);
   var t: mp_int;
   begin
      mp_init (t);
      mp_set_int (t, i);
      if mp_cmp (t, mpi) > 1 then
         mp_copy (t, mpi);
      mp_clear (t);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Min (i: TMultiPrecisionInteger);
   begin
      if self.ge(i) then
         Assign(i);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

procedure TMultiPrecisionInteger.Min (i: integer);
   var t: mp_int;
   begin
      mp_init (t);
      mp_set_int (t, i);
      if mp_cmp (t, mpi) < 1 then
         mp_copy (t, mpi);
      mp_clear (t);
      int_value_for_debug_display := mp_get_int (mpi)
   end;

//{$IFDEF CP_SYNTAX_TEST}
//procedure test_mp_math;
//   procedure test_max_signed_value;
//      var i: TMultiPrecisionInteger;
//      begin
//         i := TMultiPrecisionInteger.Create;
//         display ('testing max_signed_value');
//         i.SetMaxSignedValue (8);
//         if i.ne (127) then
//            begin
//               record_bad_test_result;
//               display ('max_signed_value calculation failed')
//            end;
//         i.Free
//      end;
//   procedure test_min_signed_value;
//      var i: TMultiPrecisionInteger;
//      begin
//         i := TMultiPrecisionInteger.Create;
//         display ('testing min_signed_value');
//         i.SetMinSignedValue (8);
//         if i.ne (-128) then
//            begin
//               record_bad_test_result;
//               display ('min_signed_value calculation failed')
//            end;
//         i.Free
//      end;
//   procedure test_max_unsigned_value;
//      var i: TMultiPrecisionInteger;
//      begin
//         i := TMultiPrecisionInteger.Create;
//         display ('testing max_unsigned_value');
//         i.SetMaxUnsignedValue (8);
//         if i.ne (255) then
//            begin
//               record_bad_test_result;
//               display ('max_unsigned_value calculation failed')
//            end;
//         i.Free
//      end;
//   procedure test_unsigned_bits_required;
//      var i: TMultiPrecisionInteger;
//      begin
//         i := TMultiPrecisionInteger.Create(0);
//         display ('testing unsigned_bits_required');
//         if i.BitsRequiredForUnsignedValue <> 0 then
//            begin
//               record_bad_test_result;
//               display ('test_unsigned_bits_required 0 calculation failed')
//            end;
//         i.AsInteger := 255;
//         if i.BitsRequiredForUnsignedValue <> 8 then
//            begin
//               record_bad_test_result;
//               display ('test_unsigned_bits_required pos calculation failed')
//            end;
//         i.AsInteger := 256;
//         if i.BitsRequiredForUnsignedValue <> 9 then
//            begin
//               record_bad_test_result;
//               display ('test_unsigned_bits_required pos calculation failed')
//            end;
//         i.Free
//      end;
//   procedure test_signed_bits_required;
//      var a: TMultiPrecisionInteger;
//      begin
//         a := TMultiPrecisionInteger.Create(0);
//         display ('testing signed_bits_required');
//         if a.BitsRequiredForSignedValue <> 1 then
//            begin
//               record_bad_test_result;
//               display ('test_signed_bits_required 0 calculation failed')
//            end;
//
//         a.AsInteger := 127;
//         if a.BitsRequiredForSignedValue <> 8 then
//            begin
//               record_bad_test_result;
//               display ('test_signed_bits_required pos calculation failed for 127')
//            end;
//
//         a.AsInteger := 128;
//         if a.BitsRequiredForSignedValue <> 9 then
//            begin
//               record_bad_test_result;
//               display ('test_signed_bits_required pos calculation failed for 128')
//            end;
//
//         a.AsInteger := -128;
//         if a.BitsRequiredForSignedValue <> 8 then
//            begin
//               record_bad_test_result;
//               display ('test_signed_bits_required neg calculation failed for -128')
//            end;
//
//         a.AsInteger := -129;
//         if a.BitsRequiredForSignedValue <> 9 then
//            begin
//               record_bad_test_result;
//               display ('test_signed_bits_required neg calculation failed for -129')
//            end;
//
//         a.Free
//      end;
//   begin
//      display ('=======================');
//      display ('TESTING TARGET_CPU_UNIT');
//      display ('======================');
//      test_max_signed_value;
//      test_min_signed_value;
//      test_max_unsigned_value;
//      test_unsigned_bits_required;
//      test_signed_bits_required;
//      display ('')
//   end;
//{$ENDIF}


INITIALIZATION
   max_integer := TMultiPrecisionInteger.Create;
   max_integer.AsInteger := High(TLargestPascalSupportedInt);

   min_integer := TMultiPrecisionInteger.Create;
   min_integer.AsInteger := Low(TLargestPascalSupportedInt);

   max_unsigned := TMultiPrecisionInteger.Create;
   max_unsigned.AsUnsigned := High(TLargestPascalSupportedUnsigned);

   lsb_mask := TMultiPrecisionInteger.Create (255);

   temp := TMultiPrecisionInteger.Create;

FINALIZATION
   max_integer.Free;
   min_integer.Free;
   max_unsigned.Free;
   lsb_mask.Free;
   temp.Free;

END.
