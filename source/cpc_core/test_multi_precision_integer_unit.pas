unit test_multi_precision_integer_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure test_mp_math;

implementation

uses cpc_multi_precision_integer_unit, test_subroutines_unit;

procedure test_mp_math;
   procedure test_max_signed_value;
      var i: TMultiPrecisionInteger;
      begin
         i := TMultiPrecisionInteger.Create;
         display ('testing max_signed_value');
         i.SetMaxSignedValue (8);
         if i.ne (127) then
            begin
               record_bad_test_result;
               display ('max_signed_value calculation failed')
            end;
         i.Free
      end;
   procedure test_min_signed_value;
      var i: TMultiPrecisionInteger;
      begin
         i := TMultiPrecisionInteger.Create;
         display ('testing min_signed_value');
         i.SetMinSignedValue (8);
         if i.ne (-128) then
            begin
               record_bad_test_result;
               display ('min_signed_value calculation failed')
            end;
         i.Free
      end;
   procedure test_max_unsigned_value;
      var i: TMultiPrecisionInteger;
      begin
         i := TMultiPrecisionInteger.Create;
         display ('testing max_unsigned_value');
         i.SetMaxUnsignedValue (8);
         if i.ne (255) then
            begin
               record_bad_test_result;
               display ('max_unsigned_value calculation failed')
            end;
         i.Free
      end;
   procedure test_unsigned_bits_required;
      var i: TMultiPrecisionInteger;
      begin
         i := TMultiPrecisionInteger.Create(0);
         display ('testing unsigned_bits_required');
         if i.BitsRequiredForUnsignedValue <> 0 then
            begin
               record_bad_test_result;
               display ('test_unsigned_bits_required 0 calculation failed')
            end;
         i.AsInteger := 255;
         if i.BitsRequiredForUnsignedValue <> 8 then
            begin
               record_bad_test_result;
               display ('test_unsigned_bits_required pos calculation failed')
            end;
         i.AsInteger := 256;
         if i.BitsRequiredForUnsignedValue <> 9 then
            begin
               record_bad_test_result;
               display ('test_unsigned_bits_required pos calculation failed')
            end;
         i.Free
      end;
   procedure test_signed_bits_required;
      var a: TMultiPrecisionInteger;
      begin
         a := TMultiPrecisionInteger.Create(0);
         display ('testing signed_bits_required');
         if a.BitsRequiredForSignedValue <> 1 then
            begin
               record_bad_test_result;
               display ('test_signed_bits_required 0 calculation failed')
            end;

         a.AsInteger := 127;
         if a.BitsRequiredForSignedValue <> 8 then
            begin
               record_bad_test_result;
               display ('test_signed_bits_required pos calculation failed for 127')
            end;

         a.AsInteger := 128;
         if a.BitsRequiredForSignedValue <> 9 then
            begin
               record_bad_test_result;
               display ('test_signed_bits_required pos calculation failed for 128')
            end;

         a.AsInteger := -128;
         if a.BitsRequiredForSignedValue <> 8 then
            begin
               record_bad_test_result;
               display ('test_signed_bits_required neg calculation failed for -128')
            end;

         a.AsInteger := -129;
         if a.BitsRequiredForSignedValue <> 9 then
            begin
               record_bad_test_result;
               display ('test_signed_bits_required neg calculation failed for -129')
            end;

         a.Free
      end;
   begin
      display ('=======================');
      display ('TESTING TARGET_CPU_UNIT');
      display ('======================');
      test_max_signed_value;
      test_min_signed_value;
      test_max_unsigned_value;
      test_unsigned_bits_required;
      test_signed_bits_required;
      display ('')
   end;


end.
