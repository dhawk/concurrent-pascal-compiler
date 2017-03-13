UNIT pic18x_run_time_error_check_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_source_analysis_unit,
   pic18x_instructions_unit;

type
   TSetErrorCodeRoutine =
      class (TSubroutine)
         const
            stack_usage = 0;
      protected
         procedure adjust_error_code;
            virtual;
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
            overload;
      end;
   TSetErrorCodeM1Routine =
      class (TSetErrorCodeRoutine)
      protected
         procedure adjust_error_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TSetErrorCodeM2Routine =
      class (TSetErrorCodeRoutine)
      protected
         procedure adjust_error_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TGetErrorCodeRoutine =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

var
   set_errorcode_routine: TSetErrorCodeRoutine;
   set_errorcode_m1_routine: TSetErrorCodeM1Routine;
   set_errorcode_m2_routine: TSetErrorCodeM2Routine;
   get_errorcode_routine: TGetErrorCodeRoutine;


// run-time error messages
const
   rterr_assignment_of_out_of_range_value = 'assignment of out-of-range value';
   rterr_attempted_divide_by_zero = 'integer zero divide';
   rterr_bad_case_index = 'bad case index';
   rterr_dropped_interrupt = '%s interrupt dropped';
   rterr_floating_point_overflow = 'floating point overflow';
   rterr_floating_point_underflow = 'floating point underflow';
   rterr_floating_point_zero_divide = 'floating point zero divide';
   rterr_integer_overflow = 'integer overflow';
   rterr_invalid_strlen = 'invalid strlen assignment attempted';
   rterr_out_of_bounds_array_index = 'out-of-bounds array index';
   rterr_string_overflow = 'string overflow';

function rterr_assignment_range_check_error (lo, hi: integer): string;
   overload;
function rterr_assignment_range_check_error (typedef: TOrdinalDataType): string;
   overload;
function rterr_array_bounds_check_error (lo, hi: integer): string;
   overload;
function rterr_array_bounds_check_error (typedef: TOrdinalDataType): string;
   overload;


procedure GenerateRangeCheckCode (target_typedef: TOrdinalDataType;
                                  tos_size: integer;
                                  tos_info: TTypeInfo;
                                  src_loc: TSourceLocation;
                                  error_message: string
                                 );
procedure GenerateUndimensionedStringIndexRangeCheckCode
                                 (tos_size: integer;
                                  tos_info: TTypeInfo;
                                  src_loc: TSourceLocation
                                 );
procedure GenerateZeroDivideCheckCode (tos_expression: TExpression);

procedure GenerateRunTimeErrorCheckSubroutines;

procedure ClearRunTimeErrorLists;
procedure RecordRunTimeErrorLocation (instruction: TInstruction;
                                      message: string;
                                      src_loc: TSourceLocation
                                     );
   overload;
procedure RecordRunTimeErrorLocation (instruction: TInstruction;
                                      offset: integer;
                                      message: string;
                                      src_loc: TSourceLocation
                                     );
   overload;
function GetRunTimeErrorInfo (error_pc: integer;
                              var message: string;
                              var src_loc: TSourceLocation
                             ): boolean;
procedure OutputRuntimeErrorInfo (fn: string);

IMPLEMENTATION

uses
   cpc_multi_precision_integer_unit,
   pic18x_core_objects_unit,
   pic18x_cpu_unit,
   pic18x_expressions_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   pic18x_microprocessor_information_unit,
   SysUtils;

function rterr_assignment_range_check_error (lo, hi: integer): string;
   begin
      result := 'attempted assignment of value outside of ' + IntToStr(lo) + '..' + IntToStr(hi)
   end;

function rterr_assignment_range_check_error (typedef: TOrdinalDataType): string;
   begin
      rterr_assignment_range_check_error (typedef.info.min_value.AsInteger, typedef.info.max_value.AsInteger)
   end;

function rterr_array_bounds_check_error (lo, hi: integer): string;
   begin
      result := 'attempted use of array index outside of ' + IntToStr(lo) + '..' + IntToStr(hi)
   end;

function rterr_array_bounds_check_error (typedef: TOrdinalDataType): string;
   begin
      result := rterr_array_bounds_check_error (typedef.info.min_value.AsInteger, typedef.info.max_value.AsInteger)
   end;

var
   RunTimeErrorLocations:
      array of
         record
            error_location_instruction: TInstruction;   // usually the next address after the call to seterror
            offset: integer;
            error_message: string;
            src_loc: TSourceLocation
         end;

procedure OutputRuntimeErrorInfo (fn: string);
   var
      f: textfile;
      i, error_code: integer;
   begin
      AssignFile (f, fn);
      Rewrite (f);
      writeln (f, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
      writeln (f, '<RunTimeErrors>');
      for i := 0 to Length(RunTimeErrorLocations)-1 do
         with RunTimeErrorLocations[i] do
            begin
               error_code := error_location_instruction.rom_addr + offset;
               writeln (f, '   <RunTimeError ErrorCode="' + IntToStr(error_code) + '" ErrorLocation="$' + format ('%6.6X', [error_code]) + '"');
               writeln (f, '      ErrorMessage="' + error_message + '"');
               writeln (f, '      SourceFile="' + src_loc.file_name + '" LineNo="' + IntToStr(src_loc.line_no) + '" Pos="' + IntToStr(src_loc.line_idx) + '"');
               writeln (f, '   />')
            end;
      writeln (f, '</RunTimeErrors>');
      Closefile (f)
   end;

procedure RecordRunTimeErrorLocation (instruction: TInstruction;
                                      message: string;
                                      src_loc: TSourceLocation
                                     );
   begin
      RecordRunTimeErrorLocation (instruction, 0, message, src_loc)
   end;

procedure RecordRunTimeErrorLocation (instruction: TInstruction;
                                      offset: integer;
                                      message: string;
                                      src_loc: TSourceLocation
                                     );
   var
      i: integer;
   begin
      i := Length(RunTimeErrorLocations);
      SetLength (RunTimeErrorLocations, i+1);
      RunTimeErrorLocations[i].error_location_instruction := instruction;
      RunTimeErrorLocations[i].offset := offset;
      RunTimeErrorLocations[i].error_location_instruction.AddRef;
      RunTimeErrorLocations[i].error_message := message;
      RunTimeErrorLocations[i].src_loc := src_loc
   end;

function GetRunTimeErrorInfo (error_pc: integer;
                              var message: string;
                              var src_loc: TSourceLocation
                             ): boolean;
   var
      i: integer;
   begin
      if error_pc = 0 then
         begin
            result := false;
            message := 'no error occurred';
            exit
         end;
      result := true;
      for i := 0 to Length(RunTimeErrorLocations)-1 do
         if RunTimeErrorLocations[i].error_location_instruction.rom_addr + RunTimeErrorLocations[i].offset = error_pc then
            begin
               message := RunTimeErrorLocations[i].error_message;
               src_loc := RunTimeErrorLocations[i].src_loc;
               exit
            end;
      message := 'error message not found???'
   end;

type
   Tsubroutine_kind = (std_signed_integer_check,                       // int8. int16. int24...
                       std_unsigned_integer_check,                     // char, uint8, uint16, uint24...
                       zero_lower_bound_unsigned_integer_check,        // 0..nnn
                       minus_one_upper_bound_negative_integer_check,   // nnn..-1
                       same_signed_integer_check,                      // both tos and target have same sign
                       full_integer_check                              // tos and target may have different signs
                      );

   Trange_check_subroutine =
      class
         subroutine_kind: Tsubroutine_kind;
         tos_expression_signed: boolean;
         tos_size: integer;
         target_typedef: TOrdinalDataType;
         callers: array of TInstruction;
         constructor Create (kind: Tsubroutine_kind; _tos_size: integer; tos_info: TTypeInfo; _target_typedef: TOrdinalDataType);
         destructor Destroy;
            override;
         class procedure add_to_subroutine_list (subr: Trange_check_subroutine);
         class function same_in_base_class (subr: Trange_check_subroutine;
                                            _subroutine_kind: Tsubroutine_kind;
                                            _tos_size: integer;
                                            _tos_info: TTypeInfo;
                                            _target_typedef: TOrdinalDataType
                                           ): boolean;
         procedure add_to_callers_list (caller: TInstruction);
         procedure output_mini_header;
         procedure set_callers_dest (dest: TInstruction);
         procedure generate_code;
            virtual; abstract;
      end;
   Tstd_signed_integer_range_check_subroutine =
      class (Trange_check_subroutine)
         negative_tos_possible, non_negative_tos_possible: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType;
                              _negative_tos_possible, _non_negative_tos_possible: boolean
                             );
         procedure generate_code;
            override;
      end;
   Tstd_unsigned_integer_range_check_subroutine =
      class (Trange_check_subroutine)
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType
                             );
         procedure generate_code;
            override;
      end;
   Tzero_lower_bound_unsigned_integer_check =    // for lower_limit = 0
      class (Trange_check_subroutine)
         lower_limit_test_needed, upper_limit_test_needed: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType;
                              _lower_limit_test_needed, _upper_limit_test_needed: boolean
                             );
         procedure generate_code;
            override;
      end;
   Tminus_one_upper_bound_negative_integer_check =     // for upper_limit = -1
      class (Trange_check_subroutine)
         lower_limit_test_needed, upper_limit_test_needed: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType;
                              _lower_limit_test_needed, _upper_limit_test_needed: boolean
                             );
         procedure generate_code;
            override;
      end;
   Tsame_signed_integer_check =
      class (Trange_check_subroutine)
         lower_limit_test_needed, upper_limit_test_needed: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType;
                              _lower_limit_test_needed, _upper_limit_test_needed: boolean
                             );
         procedure generate_code;
            override;
      end;
   Tfull_integer_check =
      class (Trange_check_subroutine)
         lower_limit_test_needed, upper_limit_test_needed: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _target_typedef: TOrdinalDataType;
                              _lower_limit_test_needed, _upper_limit_test_needed: boolean
                             );
         procedure generate_code;
            override;
      end;
   Tundimensioned_string_index_check =
      class (Trange_check_subroutine)
         neg_test_needed, zero_test_needed: boolean;
         class procedure add (caller: TInstruction;
                              _tos_size: integer;
                              _tos_info: TTypeInfo;
                              _neg_test_needed, _zero_test_needed: boolean
                             );
         procedure generate_code;
            override;
      end;

var
   lower_limit, upper_limit, temp: TMultiPrecisionInteger;
   range_check_subroutine_list: array of Trange_check_subroutine;

function range_check_annotation (tos_signed: boolean; tos_size: integer; target_typedef: TOrdinalDataType): string;
   begin
      result := 'range check tos' + tos_size_description(tos_signed, tos_size) + ' in ' + target_typedef.info.min_value.AsString + '..' + target_typedef.info.max_value.AsString
   end;

procedure GenerateRangeCheckCode (target_typedef: TOrdinalDataType;
                                  tos_size: integer;
                                  tos_info: TTypeInfo;
                                  src_loc: TSourceLocation;
                                  error_message: string
                                 );
   var
      caller: TInstruction;
      lower_limit_test_needed, upper_limit_test_needed: boolean;
      i: integer;
   begin
      lower_limit_test_needed := tos_info.min_value.lt(target_typedef.info.min_value);
      upper_limit_test_needed := tos_info.max_value.gt(target_typedef.info.max_value);

      if (not lower_limit_test_needed) and (not upper_limit_test_needed) then
         exit;

      // [1] is MSB,
      // [tos_size] is LSB,
      // [1..tos_size-target_size] should be either all $00 or all $ff (sign extension),
      // [tos_size-target_size+1..tos_size] will contain all legal values of target typedef including sign if signed

      caller := TCallMacro.Create;
      caller.annotation := range_check_annotation (TPIC18x_TypeInfo(tos_info).IntegerRange <> irAlwaysNonNegative, tos_size, target_typedef);
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, error_message, src_loc);

      if target_typedef.info.min_value.eq(0) then
         // try simple tests for char, uint8, uint16, uint24...
         for i := 1 to tos_size do
            begin
               upper_limit.AsInteger := 1;
               upper_limit.shift_left (i*8);
               upper_limit.Subtract (1);     // generates $ff, $ffff, $ffffff, ...
               if upper_limit.eq(target_typedef.info.max_value) then
                  begin
                     Tstd_unsigned_integer_range_check_subroutine.add (caller,
                                                                       tos_size,
                                                                       tos_info,
                                                                       target_typedef
                                                                      );
                     exit
                  end
            end
         else  // try simple tests for int8, int16, int24...
            for i := 1 to tos_size do
               begin
                  lower_limit.AsInteger := -1;
                  lower_limit.shift_left ((i*8)-1); // generates $FFFF80, $FF8000, $800000, ...
                  upper_limit.AsInteger := 1;
                  upper_limit.shift_left ((i*8)-1);
                  upper_limit.Subtract (1);     // generates $00007f, $007fff, $7fffff, ...
                  if lower_limit.eq(target_typedef.info.min_value)
                     and
                     upper_limit.eq(target_typedef.info.max_value) then
                     begin
                        Tstd_signed_integer_range_check_subroutine.add (caller,
                                                                        tos_size,
                                                                        tos_info,
                                                                        target_typedef,
                                                                        tos_info.min_value.lt(0),
                                                                        tos_info.max_value.ge(0)
                                                                       );
                        exit
                     end
               end;

      // try tests where one bound is easy negative test
      if target_typedef.info.min_value.eq(0) then
         begin
            Tzero_lower_bound_unsigned_integer_check.add (caller,
                                                          tos_size,
                                                          tos_info,
                                                          target_typedef,
                                                          lower_limit_test_needed,
                                                          upper_limit_test_needed
                                                         );
            exit
         end
      else if target_typedef.info.max_value.eq(-1) then
         begin
            Tminus_one_upper_bound_negative_integer_check.add (caller,
                                                               tos_size,
                                                               tos_info,
                                                               target_typedef,
                                                               lower_limit_test_needed,
                                                               upper_limit_test_needed
                                                              );


            exit
         end;

      if (target_typedef.info.min_value.ge(0) and tos_info.min_value.ge(0))
         or
         (target_typedef.info.max_value.lt(0) and tos_info.max_value.lt(0)) then
         begin    // target and tos_expression both have same sign
            Tsame_signed_integer_check.add (caller,
                                            tos_size,
                                            tos_info,
                                            target_typedef,
                                            lower_limit_test_needed,
                                            upper_limit_test_needed
                                           );
            exit
         end
      else
         begin
            Tfull_integer_check.add (caller,
                                     tos_size,
                                     tos_info,
                                     target_typedef,
                                     lower_limit_test_needed,
                                     upper_limit_test_needed
                                    );
            exit
         end
   end;    // GenerateRangeCheckCode

procedure GenerateUndimensionedStringIndexRangeCheckCode
                                 (tos_size: integer;
                                  tos_info: TTypeInfo;
                                  src_loc: TSourceLocation
                                 );
   var
      caller: TInstruction;
   begin
      // [1] is maxstrlen
      // [2] is index MSB,
      // [tos_size+1] is index LSB,
      // [2..tos_size] should be either $00,
      // [tos_size+1] will contain all legal values of target typedef

      caller := TCallMacro.Create;
      caller.annotation := 'range check tos*u8 in 0..?';
      StackUsageCounter.Pop (1);  // pops maxstrlen
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_out_of_bounds_array_index, src_loc);

      Tundimensioned_string_index_check.add (caller, tos_size, tos_info, tos_info.min_value.AsInteger < 0, tos_info.min_value.AsInteger < 1)
   end;    // GenerateUndimensionedStringIndexRangeCheckCode

type
   TSubstitutionValue = (svEither, svMinusOne, svPlusOne);
var
   zero_divide_checks:
      array of
         record
            size: integer;
            substitution_value: TSubstitutionValue;
            callers: array of TCallMacro;
         end;

procedure GenerateZeroDivideCheckCode (tos_expression: TExpression);
   var
      i: integer;
      caller: TCallMacro;
   procedure add_caller;
      var j: integer;
      begin
         j := Length(zero_divide_checks[i].callers);
         SetLength (zero_divide_checks[i].callers, j+1);
         zero_divide_checks[i].callers[j] := caller
      end;
   var
      size: integer;
      substitution_value: TSubstitutionValue;
   begin
      if tos_expression.info.min_value.gt(0) then
         begin
            TAssemblyComment.Create ('      zero divide check unnecessary since tos min value >= ' + tos_expression.info.min_value.AsString);
            exit
         end;

      if tos_expression.info.max_value.lt(0) then
         begin
            TAssemblyComment.Create ('      zero divide check unnecessary since tos max value <= ' + tos_expression.info.max_value.AsString);
            exit
         end;

      caller := TCallMacro.Create;
      caller.annotation := 'zero divide check';
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_attempted_divide_by_zero, tos_expression.src_loc);

      size := TPIC18x_TypeInfo(tos_expression.info).Size;

      if tos_expression.info.min_value.eq (0) then
         substitution_value := svPlusOne
      else if tos_expression.info.max_value.eq (0) then
         substitution_value := svMinusOne
      else
         substitution_value := svEither;

      for i := 0 to Length (zero_divide_checks)-1 do
         if zero_divide_checks[i].size = size then
            if zero_divide_checks[i].substitution_value = substitution_value then
               begin
                  add_caller;
                  exit
               end
            else if zero_divide_checks[i].substitution_value = svEither then
               begin
                  zero_divide_checks[i].substitution_value := substitution_value;
                  add_caller;
                  exit
               end;

      i := Length(zero_divide_checks);
      SetLength (zero_divide_checks, i+1);
      zero_divide_checks[i].size := size;
      zero_divide_checks[i].substitution_value := substitution_value;
      add_caller
   end;

var
   tos_zero_replacement_list: array of array of TGotoMacro;
   tos_value_replacement_list:
      array of
         record
            tos_size: integer;
            legal_value: TMultiPrecisionInteger;
            callers: array of TGotoMacro
         end;

function generate_goto_tos_value_replacement_code (tos_size: integer; legal_value: TMultiPrecisionInteger): TGotoMacro;
   var i,j: integer;
   begin
      result := TGotoMacro.Create;
      if legal_value.eq(0) then
         begin
            if Length(tos_zero_replacement_list) < tos_size then
               SetLength (tos_zero_replacement_list, tos_size);
            i := Length(tos_zero_replacement_list[tos_size-1]);
            SetLength (tos_zero_replacement_list[tos_size-1], i+1);
            tos_zero_replacement_list[tos_size-1, i] := result
         end
      else  // non-zero legal value
         begin
            for i := 0 to Length(tos_value_replacement_list)-1 do
               if (tos_value_replacement_list[i].tos_size = tos_size)
                  and
                  (tos_value_replacement_list[i].legal_value.eq(legal_value)) then
                  begin
                     j := Length(tos_value_replacement_list[i].callers);
                     SetLength(tos_value_replacement_list[i].callers, j+1);
                     tos_value_replacement_list[i].callers[j] := result;
                     exit
                  end;
            i := Length(tos_value_replacement_list);
            SetLength (tos_value_replacement_list, i+1);
            tos_value_replacement_list[i].tos_size := tos_size;
            tos_value_replacement_list[i].legal_value := TMultiPrecisionInteger.Create (legal_value);
            SetLength (tos_value_replacement_list[i].callers, 1);
            tos_value_replacement_list[i].callers[0] := result
         end
   end;

constructor Trange_check_subroutine.Create (kind: Tsubroutine_kind; _tos_size: integer; tos_info: TTypeInfo; _target_typedef: TOrdinalDataType);
   begin
      subroutine_kind := kind;
      tos_expression_signed := tos_info.IntegerRange <> irAlwaysNonNegative;
      tos_size := _tos_size;
      target_typedef := _target_typedef;
      if target_typedef <> nil then
         target_typedef.AddRef
   end;

destructor Trange_check_subroutine.Destroy;
   begin
      target_typedef.Release
   end;

procedure Trange_check_subroutine.set_callers_dest (dest: TInstruction);
   var
      i: integer;
   begin
      for i := 0 to Length(callers)-1 do
         callers[i].dest := dest
   end;

procedure Trange_check_subroutine.add_to_callers_list (caller: TInstruction);
   var i: integer;
   begin
      i := Length (callers);
      SetLength (callers, i+1);
      callers[i] := caller
   end;

procedure Trange_check_subroutine.output_mini_header;
   begin
      TAssemblySourceBlankLine.Create;
      TAssemblyComment.Create (range_check_annotation (tos_expression_signed, tos_size, target_typedef));
   end;

class procedure Trange_check_subroutine.add_to_subroutine_list (subr: Trange_check_subroutine);
   var i: integer;
   begin
      i := Length(range_check_subroutine_list);
      SetLength (range_check_subroutine_list, i+1);
      range_check_subroutine_list[i] := subr
   end;

class function Trange_check_subroutine.same_in_base_class (subr: Trange_check_subroutine;
                                                           _subroutine_kind: Tsubroutine_kind;
                                                           _tos_size: integer;
                                                           _tos_info: TTypeInfo;
                                                           _target_typedef: TOrdinalDataType
                                                          ): boolean;
   begin
      result := (subr.subroutine_kind = _subroutine_kind)
                and
                (subr.tos_expression_signed = TPIC18x_TypeInfo(_tos_info).Signed)
                and
                (subr.tos_size = _tos_size);
      if _target_typedef <> nil then
         result :=
                result
                and
                (subr.target_typedef.info.min_value.eq (_target_typedef.info.min_value))
                and
                (subr.target_typedef.info.max_value.eq (_target_typedef.info.max_value))
   end;


//============================================
// Tstd_signed_integer_range_check_subroutine

class procedure Tstd_signed_integer_range_check_subroutine.add (caller: TInstruction;
                                                                _tos_size: integer;
                                                                _tos_info: TTypeInfo;
                                                                _target_typedef: TOrdinalDataType;
                                                                _negative_tos_possible, _non_negative_tos_possible: boolean
                                                               );
   var
      i: integer;
      subr: Tstd_signed_integer_range_check_subroutine;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                std_signed_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin
               subr := Tstd_signed_integer_range_check_subroutine(range_check_subroutine_list[i]);
               if (subr.negative_tos_possible = _negative_tos_possible)
                  and
                  (subr.non_negative_tos_possible = _non_negative_tos_possible) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tstd_signed_integer_range_check_subroutine.Create (std_signed_integer_check,
                                                                 _tos_size,
                                                                 _tos_info,
                                                                 _target_typedef
                                                                );
      subr.negative_tos_possible := _negative_tos_possible;
      subr.non_negative_tos_possible := _non_negative_tos_possible;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tstd_signed_integer_range_check_subroutine.generate_code;
   var
      i: integer;
      bn: TPIC18x_BN;
      bra: TPIC18x_BRA;
      bnz: TPIC18x_BNZ;
      target_size: integer;
   begin
      output_mini_header;
      target_size := TPIC18x_TypeInfo(target_typedef.info).Size;
      if target_size = tos_size then
         begin
            set_callers_dest (TPIC18x_MOVF.Create (1, dest_w, access_mode));
            bn := TPIC18x_BN.Create;
            TPIC18x_RETURN.Create;
            temp.AsInteger := 0;
            bn.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
         end
      else  // target_size < tos_size
         begin
            // bytes sfr[1]..sfr[tos_size-target_size] should be match sign ($00 or $ff),
            // bytes sfr[tos_size-target_size+1]..sfr[tos_size] may be any value,
            //    bit 7 of sfr[tos_size] is sign (0 or 1)
            if negative_tos_possible and non_negative_tos_possible then
               begin
                  set_callers_dest (TPIC18x_MOVF.Create (tos_size+1-target_size, dest_w, access_mode));
                  bn := TPIC18x_BN.Create;
                     TPIC18x_MOVF.Create (tos_size-target_size, dest_w, access_mode);
                     for i := target_size+2 to tos_size do
                        TPIC18x_IORWF.Create (tos_size+1-i, dest_w, access_mode);
                     // Z if okay
                     bra := TPIC18x_BRA.Create;
                  bn.dest :=
                     TPIC18x_MOVF.Create (tos_size-target_size, dest_w, access_mode);
                     for i := target_size+2 to tos_size do
                        TPIC18x_ANDWF.Create (tos_size+1-i, dest_w, access_mode);
                     TPIC18x_COMF.Create (WREG, dest_w, access_mode);
                     // Z if ok
                  bnz := TPIC18x_BNZ.Create;
                  TPIC18x_RETURN.Create;
                  bra.dest := bnz;
                  temp.AsInteger := 0;
                  bnz.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
               end
            else if negative_tos_possible then
               begin   // tos is not >= 0
                  set_callers_dest (TPIC18x_MOVF.Create (tos_size+1-target_size, dest_w, access_mode));
                  TPIC18x_IORLW.Create ($7F);
                  for i := target_size+1 to tos_size do
                     TPIC18x_ANDWF.Create (tos_size+1-i, dest_w, access_mode);
                  TPIC18x_COMF.Create (WREG, dest_w, access_mode);
                  bnz := TPIC18x_BNZ.Create;
                  TPIC18x_RETURN.Create;
                  temp.AsInteger := 0;
                  bnz.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
               end
            else if non_negative_tos_possible then
               begin   // tos should be positive
                  set_callers_dest (TPIC18x_MOVF.Create (tos_size+1-target_size, dest_w, access_mode));
                  TPIC18x_ANDLW.Create ($80);
                  for i := target_size+1 to tos_size do
                     TPIC18x_IORWF.Create (tos_size+1-i, dest_w, access_mode);
                  bnz := TPIC18x_BNZ.Create;
                  TPIC18x_RETURN.Create;
                  temp.AsInteger := 0;
                  bnz.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
               end
            else
               assert (false)
         end
   end;

class procedure Tstd_unsigned_integer_range_check_subroutine.add (caller: TInstruction;
                                                                  _tos_size: integer;
                                                                  _tos_info: TTypeInfo;
                                                                  _target_typedef: TOrdinalDataType
                                                                 );
   var
      i: integer;
      subr: Tstd_unsigned_integer_range_check_subroutine;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                std_unsigned_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin   // it does exist, so use it
               range_check_subroutine_list[i].add_to_callers_list (caller);
               exit
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tstd_unsigned_integer_range_check_subroutine.Create (std_unsigned_integer_check,
                                                                   _tos_size,
                                                                   _tos_info,
                                                                   _target_typedef
                                                                  );
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tstd_unsigned_integer_range_check_subroutine.generate_code;
   var
      i: integer;
      bnz: TPIC18x_BNZ;
      bn: TPIC18x_BN;
      target_size: integer;
   begin
      output_mini_header;
      target_size := TPIC18x_TypeInfo(target_typedef.info).Size;

      if target_size = tos_size then
         begin
            // tos is signed, target is unsigned.  tos will not be in range if negative
            set_callers_dest (TPIC18x_MOVF.Create (1, dest_w, access_mode));
            bn := TPIC18x_BN.Create;
            TPIC18x_RETURN.Create;
            temp.AsInteger := 0;
            bn.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
         end
      else  // target_size < tos_size
         begin
            // bytes sfr[1]..sfr[addr-size] should all be zero
            // bytes sfr[addr+1-size]..sfr[addr] may be any value and will be <= upper_limit
            set_callers_dest (TPIC18x_MOVF.Create (tos_size - target_size, dest_w, access_mode));
            for i := target_size+2 to tos_size do
               TPIC18x_IORWF.Create (tos_size+1-i, dest_w, access_mode);
            bnz := TPIC18x_BNZ.Create;
            TPIC18x_RETURN.Create;
            temp.AsInteger := 0;
            bnz.dest := generate_goto_tos_value_replacement_code (tos_size, temp)
         end
   end;

class procedure Tzero_lower_bound_unsigned_integer_check.add (caller: TInstruction;
                                                              _tos_size: integer;
                                                              _tos_info: TTypeInfo;
                                                              _target_typedef: TOrdinalDataType;
                                                              _lower_limit_test_needed, _upper_limit_test_needed: boolean
                                                             );
   var
      i: integer;
      subr: Tzero_lower_bound_unsigned_integer_check;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                zero_lower_bound_unsigned_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin
               subr := Tzero_lower_bound_unsigned_integer_check(range_check_subroutine_list[i]);
               if (subr.lower_limit_test_needed = _lower_limit_test_needed)
                  and
                  (subr.upper_limit_test_needed = _upper_limit_test_needed) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tzero_lower_bound_unsigned_integer_check.Create (zero_lower_bound_unsigned_integer_check,
                                                               _tos_size,
                                                               _tos_info,
                                                               _target_typedef
                                                              );
      subr.lower_limit_test_needed := _lower_limit_test_needed;
      subr.upper_limit_test_needed := _upper_limit_test_needed;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tzero_lower_bound_unsigned_integer_check.generate_code;
   var
//      target_size: integer;
      bn: TPIC18x_BN;
      bc: TPIC18x_BC;
      i: integer;
      err_lbl: TInstruction;
      tos_size_in_bytes: integer;
   begin
      output_mini_header;
      if not lower_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be >= ' + target_typedef.info.min_value.AsString + ')');
      if not upper_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be <= ' + target_typedef.info.max_value.AsString + ')');

//      target_size := TPIC18x_TypeInfo(target_typedef.info).Size;
      set_callers_dest (TAssemblyLabel.Create);
      tos_size_in_bytes := tos_size;

      bn := nil;
      bc := nil;

      if lower_limit_test_needed then
         begin
            TPIC18x_MOVF.Create (1, dest_w, access_mode);
            bn := TPIC18x_BN.Create;
         end;

      if upper_limit_test_needed then
         begin
            upper_limit.Assign (target_typedef.info.max_value);
            upper_limit.Add (1);
            TPIC18x_MOVLW.Create (upper_limit.AsByte(0));
            TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
            for i := 2 to tos_size_in_bytes do
               begin
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(i-1));
                  TPIC18x_SUBWFB.Create (tos_size+1-i, dest_w, access_mode)
               end;
            bc := TPIC18x_BC.Create
         end;

      TPIC18x_RETURN.Create;

      temp.AsInteger := 0;
      err_lbl := generate_goto_tos_value_replacement_code (tos_size, temp);
      if bn <> nil then
         bn.dest := err_lbl;
      if bc <> nil then
         bc.dest := err_lbl
   end;

class procedure Tminus_one_upper_bound_negative_integer_check.add (caller: TInstruction;
                                                                   _tos_size: integer;
                                                                   _tos_info: TTypeInfo;
                                                                   _target_typedef: TOrdinalDataType;
                                                                   _lower_limit_test_needed, _upper_limit_test_needed: boolean
                                                                  );
   var
      i: integer;
      subr: Tminus_one_upper_bound_negative_integer_check;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                minus_one_upper_bound_negative_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin
               subr := Tminus_one_upper_bound_negative_integer_check(range_check_subroutine_list[i]);
               if (subr.lower_limit_test_needed = _lower_limit_test_needed)
                  and
                  (subr.upper_limit_test_needed = _upper_limit_test_needed) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tminus_one_upper_bound_negative_integer_check.Create (minus_one_upper_bound_negative_integer_check,
                                                                    _tos_size,
                                                                    _tos_info,
                                                                    _target_typedef
                                                                   );
      subr.lower_limit_test_needed := _lower_limit_test_needed;
      subr.upper_limit_test_needed := _upper_limit_test_needed;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tminus_one_upper_bound_negative_integer_check.generate_code;
   var
      bnn: TPIC18x_BNN;
      bnc: TPIC18x_BNC;
      err_lbl: TInstruction;
      i: integer;
   begin
      output_mini_header;
      if not lower_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be >= ' + target_typedef.info.min_value.AsString + ')');
      if not upper_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be <= ' + target_typedef.info.max_value.AsString + ')');

      set_callers_dest (TAssemblyLabel.Create);

      bnn := nil;
      bnc := nil;

      if lower_limit_test_needed then
         begin
            lower_limit.Assign (target_typedef.info.min_value);
            TPIC18x_MOVLW.Create (lower_limit.AsByte(0));
            TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
            for i := 2 to tos_size do
               begin
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(i-1));
                  TPIC18x_SUBWFB.Create (tos_size+1-i, dest_w, access_mode)
               end;
            bnc := TPIC18x_BNC.Create
         end;

      if upper_limit_test_needed then
         begin
            TPIC18x_MOVF.Create (1, dest_w, access_mode);
            bnn := TPIC18x_BNN.Create
         end;

      TPIC18x_RETURN.Create;

      temp.AsInteger := -1;
      err_lbl := generate_goto_tos_value_replacement_code (tos_size, temp);
      if bnc <> nil then
         bnc.dest := err_lbl;
      if bnn <> nil then
         bnn.dest := err_lbl
   end;

class procedure Tsame_signed_integer_check.add (caller: TInstruction;
                                                _tos_size: integer;
                                                _tos_info: TTypeInfo;
                                                _target_typedef: TOrdinalDataType;
                                                _lower_limit_test_needed, _upper_limit_test_needed: boolean
                                               );
   var
      i: integer;
      subr: Tsame_signed_integer_check;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                same_signed_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin
               subr := Tsame_signed_integer_check(range_check_subroutine_list[i]);
               if (subr.lower_limit_test_needed = _lower_limit_test_needed)
                  and
                  (subr.upper_limit_test_needed = _upper_limit_test_needed) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tsame_signed_integer_check.Create (same_signed_integer_check,
                                                 _tos_size,
                                                 _tos_info,
                                                 _target_typedef
                                                );
      subr.lower_limit_test_needed := _lower_limit_test_needed;
      subr.upper_limit_test_needed := _upper_limit_test_needed;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tsame_signed_integer_check.generate_code;
   var
      j: integer;
      bnc: TPIC18x_BNC;
      bc: TPIC18x_BC;
      err_lbl: TInstruction;
   begin
      output_mini_header;
      if not lower_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be >= ' + target_typedef.info.min_value.AsString + ')');
      if not upper_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be <= ' + target_typedef.info.max_value.AsString + ')');

      set_callers_dest (TAssemblyLabel.Create);

      bnc := nil;
      bc := nil;

      if lower_limit_test_needed then
         begin
            lower_limit.Assign (target_typedef.info.min_value);
            if tos_size = 1 then
               begin
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode)
               end
            else
               begin
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
                  for j := 2 to tos_size do
                     begin
                        TPIC18x_MOVLW.Create (lower_limit.AsByte(j-1));
                        TPIC18x_SUBWFB.Create (tos_size+1-j, dest_w, access_mode)
                     end
               end;
            bnc := TPIC18x_BNC.Create
         end;

      if  upper_limit_test_needed then
         begin
            upper_limit.Assign (target_typedef.info.max_value);
            upper_limit.Add (1);
            if tos_size = 1 then
               begin
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode)
               end
            else
               begin
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
                  for j := 2 to tos_size do
                     begin
                        TPIC18x_MOVLW.Create (upper_limit.AsByte(j-1));
                        TPIC18x_SUBWFB.Create (tos_size+1-j, dest_w, access_mode)
                     end
               end;
            bc := TPIC18x_BC.Create
         end;

      TPIC18x_RETURN.Create;

      err_lbl := generate_goto_tos_value_replacement_code (tos_size, target_typedef.info.min_value);
      if bnc <> nil then
         bnc.dest := err_lbl;
      if bc <> nil then
         bc.dest := err_lbl
   end;

class procedure Tfull_integer_check.add (caller: TInstruction;
                                         _tos_size: integer;
                                         _tos_info: TTypeInfo;
                                         _target_typedef: TOrdinalDataType;
                                         _lower_limit_test_needed, _upper_limit_test_needed: boolean
                                        );
   var
      i: integer;
      subr: Tfull_integer_check;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                full_integer_check,
                                _tos_size,
                                _tos_info,
                                _target_typedef
                               ) then
            begin
               subr := Tfull_integer_check(range_check_subroutine_list[i]);
               if (subr.lower_limit_test_needed = _lower_limit_test_needed)
                  and
                  (subr.upper_limit_test_needed = _upper_limit_test_needed) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

      // subroutine to handle this case does not exist, create it
      subr := Tfull_integer_check.Create (full_integer_check,
                                          _tos_size,
                                          _tos_info,
                                          _target_typedef
                                         );
      subr.lower_limit_test_needed := _lower_limit_test_needed;
      subr.upper_limit_test_needed := _upper_limit_test_needed;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tfull_integer_check.generate_code;
   var
      j: integer;
      bc: TPIC18x_BC;
      bnc: TPIC18x_BNC;
      err_lbl: TInstruction;
   begin
      output_mini_header;
      if not lower_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be >= ' + target_typedef.info.min_value.AsString + ')');
      if not upper_limit_test_needed then
         TAssemblyComment.Create ('   (tos known to be <= ' + target_typedef.info.max_value.AsString + ')');

      set_callers_dest (TPIC18x_BTG.Create (1, 7, access_mode));

      bc := nil;
      bnc := nil;
      if lower_limit_test_needed then
         begin
            lower_limit.Assign (target_typedef.info.min_value);
            if tos_size = 1 then
               begin
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(0) xor $80);
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode)
               end
            else
               begin
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
                  for j := 2 to tos_size-1 do
                     begin
                        TPIC18x_MOVLW.Create (lower_limit.AsByte(j-1));
                        TPIC18x_SUBWFB.Create (tos_size+1-j, dest_w, access_mode)
                     end;
                  TPIC18x_MOVLW.Create (lower_limit.AsByte(tos_size-1) xor $80);
                  TPIC18x_SUBWFB.Create (tos_size+1-tos_size, dest_w, access_mode)
               end;
            bnc := TPIC18x_BNC.Create
         end;

      if  upper_limit_test_needed then
         begin
            upper_limit.Assign (target_typedef.info.max_value);
            upper_limit.Add (1);
            if tos_size = 1 then
               begin
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(0) xor $80);
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode)
               end
            else
               begin
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(0));
                  TPIC18x_SUBWF.Create (tos_size, dest_w, access_mode);
                  for j := 2 to tos_size-1 do
                     begin
                        TPIC18x_MOVLW.Create (upper_limit.AsByte(j-1));
                        TPIC18x_SUBWFB.Create (tos_size+1-j, dest_w, access_mode)
                     end;
                  TPIC18x_MOVLW.Create (upper_limit.AsByte(tos_size-1) xor $80);
                  TPIC18x_SUBWFB.Create (1, dest_w, access_mode)
               end;
            bc := TPIC18x_BC.Create
         end;

      TPIC18x_BTG.Create (1, 7, access_mode);
      TPIC18x_RETURN.Create;

      if target_typedef.info.min_value.le(0) and target_typedef.info.max_value.ge(0) then
         temp.AsInteger := 0
      else if target_typedef.info.min_value.le(-1) and target_typedef.info.max_value.ge(-1) then
         temp.AsInteger := -1
      else
         temp.Assign (target_typedef.info.min_value);

      err_lbl := generate_goto_tos_value_replacement_code (tos_size, temp);
      if bc <> nil then
         bc.dest := err_lbl;
      if bnc <> nil then
         bnc.dest := err_lbl
   end;

//      Tundimensioned_string_index_check =
//      class (Trange_check_subroutine)
//         neg_test_needed, zero_test_needed: boolean;
class procedure Tundimensioned_string_index_check.add (caller: TInstruction;
                                                       _tos_size: integer;
                                                       _tos_info: TTypeInfo;
                                                       _neg_test_needed, _zero_test_needed: boolean
                                                      );
   var
      i: integer;
      subr: Tundimensioned_string_index_check;
   begin
      // see if subroutine to handle this case already exists
      for i := 0 to Length(range_check_subroutine_list)-1 do
         if same_in_base_class (range_check_subroutine_list[i],
                                full_integer_check,
                                _tos_size,
                                _tos_info,
                                nil
                               ) then
            begin
               subr := Tundimensioned_string_index_check(range_check_subroutine_list[i]);
               if (subr.neg_test_needed = _neg_test_needed)
                  and
                  (subr.zero_test_needed = _zero_test_needed) then
                  begin   // it does exist, so use it
                     subr.add_to_callers_list (caller);
                     exit
                  end
            end;

     // subroutine to handle this case does not exist, create it
      subr := Tundimensioned_string_index_check.Create (full_integer_check,
                                                        _tos_size,
                                                        _tos_info,
                                                        nil
                                                       );
      subr.neg_test_needed := _neg_test_needed;
      subr.zero_test_needed := _zero_test_needed;
      subr.add_to_callers_list (caller);
      add_to_subroutine_list (subr)
   end;

procedure Tundimensioned_string_index_check.generate_code;
   var
      err_lbl: TInstruction;
      bn1, bn2: TInstruction;
      bnz: TPIC18x_BNZ;
      i: integer;
   begin
      bn2 := nil;
      bnz := nil;

      TAssemblySourceBlankLine.Create;
      TAssemblyComment.Create ('range check tos' + tos_size_description(false, tos_size) + ' in ' + '1..maxstrlen');

      // test index.lsb against maxstrlen
      set_callers_dest (TPIC18x_MOVF.Create (tos_size + 1, dest_w, access_mode));
      TPIC18x_SUBWF.Create (PREINC2, dest_w, access_mode);
      bn1 := TPIC18x_BN.Create;

      if neg_test_needed
         or
         (tos_size > 1)
      then
         TPIC18x_MOVF.Create (1, dest_w, access_mode);

      if neg_test_needed then
         bn2 := TPIC18x_BN.Create;

      if tos_size > 1 then
         begin  // all non-lsb bytes should be 0
            for i := 2 to tos_size-1 do
               TPIC18x_IORWF.Create (i, dest_w, access_mode);
            bnz := TPIC18x_BNZ.Create
         end;

      if zero_test_needed then
         TPIC18x_TSTFSZ.Create (tos_size, access_mode);
      TPIC18x_RETURN.Create;

      temp.AsInteger := 1;  // always a legal string index value
      err_lbl := generate_goto_tos_value_replacement_code (tos_size, temp);
      if bn1 <> nil then
         bn1.dest := err_lbl;
      if bn2 <> nil then
         bn2.dest := err_lbl;
      if bnz <> nil then
         bnz.dest := err_lbl
   end;

procedure GenerateRunTimeErrorCheckSubroutines;
   var
      i,j: integer;
      lbl: TInstruction;
   begin
      if (Length(range_check_subroutine_list) > 0) then
         begin
            for i := 0 to Length(range_check_subroutine_list)-1 do
               begin
                  range_check_subroutine_list[i].generate_code;
                  range_check_subroutine_list[i].Free
               end;
            SetLength (range_check_subroutine_list, 0);

            if Length(tos_zero_replacement_list) > 0 then
               begin
                  TAssemblySourceBlankLine.Create;
                  TAssemblyComment.Create ('replace TOS expression with 0');
                  for i := Length(tos_zero_replacement_list)-1 downto 0 do
                     begin
                        lbl := TPIC18x_CLRF.Create (i+1, access_mode);
                        for j := 0 to Length (tos_zero_replacement_list[i])-1 do
                           tos_zero_replacement_list[i,j].dest := lbl
                     end;
                  set_errorcode_routine.ComeFrom (TGotoMacro.Create)
               end;

            for i := 0 to Length(tos_value_replacement_list)-1 do
               begin
                  TAssemblySourceBlankLine.Create;
                  TAssemblyComment.Create ('replace tos*' + IntToStr(tos_value_replacement_list[i].tos_size) + ' with ' + IntToStr(tos_value_replacement_list[i].legal_value.AsInteger));
                  lbl := TLoadFMacro.Create (1, access_mode, tos_value_replacement_list[i].legal_value.AsByte(tos_value_replacement_list[i].tos_size-1));
                  for j := 2 to tos_value_replacement_list[i].tos_size do
                     TLoadFMacro.Create (j, access_mode, tos_value_replacement_list[i].legal_value.AsByte(tos_value_replacement_list[i].tos_size-j));
                  for j := 0 to Length(tos_value_replacement_list[i].callers)-1 do
                     tos_value_replacement_list[i].callers[j].dest := lbl;
                  set_errorcode_routine.ComeFrom (TGotoMacro.Create)
               end
         end;

      if Length (zero_divide_checks) > 0 then
         begin
//      result := TAssemblyLabel.Create;
//      result.annotation := 'subroutine to set error code';

            for i := 0 to Length(zero_divide_checks)-1 do
               begin
                  TAssemblySourceBlankLine.Create;
                  lbl := TPIC18x_MOVF.Create (1, dest_w, access_mode);
                  lbl.annotation := 'check tos*' + IntToStr(zero_divide_checks[i].size) + ', replace with ';
                  for j := 0 to Length (zero_divide_checks[i].callers)-1 do
                     zero_divide_checks[i].callers[j].dest := lbl;
                  for j := 2 to zero_divide_checks[i].size do
                     TPIC18x_IORWF.Create (j, dest_w, access_mode);
                  TPIC18x_TSTFSZ.Create (WREG, access_mode);
                  TPIC18x_RETURN.Create;
                  case zero_divide_checks[i].substitution_value of
                     svEither,
                     svPlusOne:
                        begin
                           lbl.annotation := lbl.annotation + '1 if zero';
                           TPIC18x_INCF.Create (zero_divide_checks[i].size, dest_f, access_mode)
                        end;
                     svMinusOne:
                        begin
                           lbl.annotation := lbl.annotation + '-1 if zero';
                           for j := 1 to zero_divide_checks[i].size do
                              TPIC18x_SETF.Create (j, access_mode)
                        end;
                  else
                     assert (false)
                  end;
                  set_errorcode_routine.ComeFrom (TGOTOMacro.Create)
               end;
            SetLength (zero_divide_checks, 0)
         end
   end;

procedure ClearRunTimeErrorLists;
   var i: integer;
   begin
      for i := 0 to Length(RunTimeErrorLocations)-1 do
         RunTimeErrorLocations[i].error_location_instruction.Release;
      SetLength (RunTimeErrorLocations, 0);
      SetLength (zero_divide_checks, 0);
      for i := 0 to Length(tos_value_replacement_list)-1 do
         tos_value_replacement_list[i].legal_value.Free;
      SetLength (tos_value_replacement_list, 0);
      SetLength (tos_zero_replacement_list, 0);
      SetLength (range_check_subroutine_list, 0)
   end;

constructor TSetErrorCodeRoutine.Create;
   begin
      inherited Create (0, 0, 'set error code to PC')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TSetErrorCodeRoutine.report_stack_sizes;
   begin
      check_stack_sizes (stack_usage, stack_usage, 1)
   end;
{$endif}

procedure TSetErrorCodeRoutine.generate_subroutine_code;
   var
      bnz: TPIC18x_BNZ;
   begin
      TurnInterruptsOff;
      TPIC18x_MOVF.Create (error_logU, dest_w, bank_mode);
      TPIC18x_IORWF.Create (error_logH, dest_w, bank_mode);
      TPIC18x_IORWF.Create (error_logL, dest_w, bank_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_MOVFF.Create (TOSU, error_logU);
      TPIC18x_MOVFF.Create (TOSH, error_logH);
      TPIC18x_MOVFF.Create (TOSL, error_logL);
      adjust_error_code;
      bnz.dest := ExitKernel
   end;

procedure TSetErrorCodeRoutine.adjust_error_code;
   begin
      // no adjustment needed
   end;

constructor TSetErrorCodeM1Routine.Create;
   begin
      inherited Create (0, 0, 'set error code to PC-1')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TSetErrorCodeM1Routine.report_stack_sizes;
   begin
      check_stack_sizes (0, 0, 1)
   end;
{$endif}

procedure TSetErrorCodeM1Routine.adjust_error_code;
   begin
      TPIC18x_DECF.Create (error_logL, dest_f, bank_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_SUBWFB.Create (error_logH, dest_f, bank_mode);
      TPIC18x_SUBWFB.Create (error_logU, dest_f, bank_mode)
   end;

constructor TSetErrorCodeM2Routine.Create;
   begin
      inherited Create (0, 0, 'set error code to PC-2')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TSetErrorCodeM2Routine.report_stack_sizes;
   begin
      check_stack_sizes (0, 0, 1)
   end;
{$endif}

procedure TSetErrorCodeM2Routine.adjust_error_code;
   begin
      TPIC18x_MOVLW.Create (2);
      TPIC18x_SUBWF.Create (error_logL, dest_f, bank_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_SUBWFB.Create (error_logH, dest_f, bank_mode);
      TPIC18x_SUBWFB.Create (error_logU, dest_f, bank_mode)
   end;

constructor TGetErrorCodeRoutine.Create;
   begin
      inherited Create (3, 0, 'get and clear error code')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TGetErrorCodeRoutine.report_stack_sizes;
   begin
      check_stack_sizes (3, 0, 1)
   end;
{$endif}

procedure TGetErrorCodeRoutine.generate_subroutine_code;
   begin
      TurnInterruptsOff;
      TPIC18x_SUBFSR.Create (2, 3);
      TPIC18x_MOVF.Create (error_logU, dest_w, bank_mode);
      TPIC18x_MOVWF.Create (1, access_mode);
      TPIC18x_MOVF.Create (error_logH, dest_w, bank_mode);
      TPIC18x_MOVWF.Create (2, access_mode);
      TPIC18x_MOVF.Create (error_logL, dest_w, bank_mode);
      TPIC18x_MOVWF.Create (3, access_mode);
      TPIC18x_CLRF.Create (error_logL, bank_mode);
      TPIC18x_CLRF.Create (error_logH, bank_mode);
      TPIC18x_CLRF.Create (error_logU, bank_mode);
      ExitKernel
   end;


INITIALIZATION
   upper_limit := TMultiPrecisionInteger.Create;
   lower_limit := TMultiPrecisionInteger.Create;
   temp := TMultiPrecisionInteger.Create;
   set_errorcode_routine := TSetErrorCodeRoutine.Create;
   set_errorcode_m1_routine := TSetErrorCodeM1Routine.Create;
   set_errorcode_m2_routine := TSetErrorCodeM2Routine.Create;
   get_errorcode_routine := TGetErrorCodeRoutine.Create;

FINALIZATION
   ClearRunTimeErrorLists;
   upper_limit.Free;
   lower_limit.Free;
   temp.Free;
   set_errorcode_routine.Free;
   set_errorcode_m1_routine.Free;
   set_errorcode_m2_routine.Free;
   get_errorcode_routine.Free;

END.
