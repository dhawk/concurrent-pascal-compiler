UNIT pic18x_floating_point_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF FPC}
{$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_source_analysis_unit,
   pic18x_core_objects_unit,
   pic18x_instructions_unit;

const
   bool_size  = 1;
   int24_size = 3;
   int32_size = 4;
   real_size  = 4;

type
   TFloatingPointAdd =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointSubtract =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointMultiply =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointDivide =
      class (TSubroutine)
      protected
         function generate_call_code: TInstruction;
            override;
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;

type
   TFloatingPointEqualsComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TFloatingPointNotEqualsComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TFloatingPointLessThanComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TFloatingPointLessThanOrEqualsComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TFloatingPointGreaterThanComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TFloatingPointGreaterThanOrEqualsComparison =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

type
   TFloatingPointRound24Function =
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
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointRound32Function =
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
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointTrunc24Function =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TFloatingPointTrunc32Function =
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
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;

var
   FPA32: TFloatingPointAdd;
   FPS32: TFloatingPointSubtract;
   FPM32: TFloatingPointMultiply;
   FPD32: TFloatingPointDivide;

   TALTB32: TFloatingPointLessThanComparison;
   TALEB32: TFloatingPointLessThanOrEqualsComparison;
   TAGTB32: TFloatingPointGreaterThanComparison;
   TAGEB32: TFloatingPointGreaterThanOrEqualsComparison;
   TAEQB32: TFloatingPointEqualsComparison;
   TANEB32: TFloatingPointNotEqualsComparison;

   FPRound24: TFloatingPointRound24Function;
   FPRound32: TFloatingPointRound32Function;
   FPTrunc24: TFloatingPointTrunc24Function;
   FPTrunc32: TFloatingPointTrunc32Function;

procedure generate_integer_expression_to_real_code (expr: TExpression);
procedure convert_tos_integer_to_real (tos_size: integer; tos_info: TPIC18x_TypeInfo; src_loc: TSourceLocation);
function optimal_int_size_before_float (tos_info: TTypeInfo): integer;

procedure convert_tos_from_pic_to_ieee;
procedure convert_tos_from_ieee_to_pic;

// The following type might need some additional work if the object pascal compiler
// is running on an cpu which doesn't support IEEE real format or if the real is
// not the same endian as assumed in Delphi.
type
   TPIC18x_Real =
      record
      private
         function get_pic_byte (idx: integer): byte;
         function get_ieee_byte (idx: integer): byte;
      public
         r: real;
         property pic_real_AsByte[idx: integer]: byte read get_pic_byte;
         property ieee_single_AsByte [idx: integer]: byte read get_ieee_byte;
      end;


IMPLEMENTATION

uses
   cpc_target_cpu_unit,
   pic18x_cpu_unit,
   pic18x_expressions_unit,
   pic18x_macro_instructions_unit,
   pic18x_microprocessor_information_unit,
   pic18x_run_time_error_check_unit;

const
   attribution_header = 'This subroutine is derived from:';
   an575_attribution  = '   Microchip AN575: "IEEE 754 Compliant Floating Point Routines" by Frank J. Testa';
   an660_attribution  = '   Microchip AN660: "Floating Point Math Routines" by Frank J. Testa';

const
   fadd_AARGB3 =  1;
   fadd_BARGB3 =  2;
   fadd_SIGN   =  3;
   fadd_TEMPB0 =  4;
   fadd_TEMPB1 =  5;
   fadd_BEXP   =  6;
   fadd_BARGB0 =  7;
   fadd_BARGB1 =  8;
   fadd_BARGB2 =  9;
   fadd_AEXP   = 10;
   fadd_AARGB0 = 11;
   fadd_AARGB1 = 12;
   fadd_AARGB2 = 13;
   fads_total_stack = 13;
   fadd_temp_stack_push_count = fads_total_stack - (2 * real_size);
   fadd_temp_stack_pop_count = fads_total_stack - real_size;
   fsub_temp_stack_push_count = fadd_temp_stack_push_count;
   fsub_temp_stack_pop_count = fadd_temp_stack_pop_count;

const
   fmult_AARGB3 =  1;
   fmult_AARGB4 =  2;
   fmult_AARGB5 =  3;
   fmult_BARGB3 =  4;
   fmult_SIGN   =  5;
   fmult_TEMPB0 =  6;
   fmult_TEMPB1 =  7;
   fmult_TEMPB2 =  8;
   fmult_BEXP   =  9;
   fmult_BARGB0 = 10;
   fmult_BARGB1 = 11;
   fmult_BARGB2 = 12;
   fmult_AEXP   = 13;
   fmult_AARGB0 = 14;
   fmult_AARGB1 = 15;
   fmult_AARGB2 = 16;
   fmult_total_stack = 16;
   fmult_temp_stack_push_count = fmult_total_stack - (2 * real_size);
   fmult_temp_stack_pop_count = fmult_total_stack - real_size;

const
   fdiv_AARGB3 =  1;
   fdiv_AARGB4 =  2;
   fdiv_AARGB5 =  3;
   fdiv_BARGB3 =  4;
   fdiv_SIGN   =  5;
   fdiv_TEMPB0 =  6;
   fdiv_TEMPB1 =  7;
   fdiv_TEMPB2 =  8;
   fdiv_TEMPB3 =  9;
   fdiv_BEXP   = 10;
   fdiv_BARGB0 = 11;
   fdiv_BARGB1 = 12;
   fdiv_BARGB2 = 13;
   fdiv_AEXP   = 14;
   fdiv_AARGB0 = 15;
   fdiv_AARGB1 = 16;
   fdiv_AARGB2 = 17;
   fdiv_total_stack = 17;
   fdiv_temp_stack_push_count = fdiv_total_stack - (2 * real_size);
   fdiv_temp_stack_pop_count = fdiv_total_stack - real_size;

const
   int3224_AARGB3  = 1;
   int3224_SIGN    = 2;
   int3224_FPFLAGS = 3;
   int3224_AEXP    = 4;
   int3224_AARGB0  = 5;
   int3224_AARGB1  = 6;
   int3224_AARGB2  = 7;
   int3224_total_stack = 7;
   int3224_temp_stack_push_count = int3224_total_stack - real_size;
   int3224_temp_stack_pop_count = int3224_total_stack - int24_size;

const
   int3232_SIGN    = 1;
   int3232_FPFLAGS = 2;
   int3232_AARGB3  = 3;
   int3232_AARGB4  = 4;
   int3232_AEXP    = 5;
   int3232_AARGB0  = 6;
   int3232_AARGB1  = 7;
   int3232_AARGB2  = 8;
   int3232_total_stack = 8;
   int3232_temp_stack_push_count = int3232_total_stack - real_size;
   int3232_temp_stack_pop_count = int3232_total_stack - int32_size;

const
   flo2432_SIGN   = 1;
   flo2432_TEMP   = 2;
   flo2432_AEXP   = 3;
   flo2432_AARGB0 = 4;
   flo2432_AARGB1 = 5;
   flo2432_AARGB2 = 6;
   flo2432_total_stack = 6;
   flo2432_temp_stack_push_count = flo2432_total_stack - int24_size;
   flo2432_temp_stack_pop_count  = flo2432_total_stack - real_size;

const
   norm40_TEMP   = 1;
   norm40_SIGN   = 2;
   norm40_AARGB3 = 3;
   norm40_EXP    = 4;
   norm40_AARGB0 = 5;
   norm40_AARGB1 = 6;
   norm40_AARGB2 = 7;
   norm40_total_stack = 7;
   norm40_temp_stack_pop_count = norm40_total_stack - real_size;

const
   flo3232_total_stack = norm40_total_stack;
   FLO3232_temp_stack_push_count = flo3232_total_stack - int32_size;
   FLO3232_temp_stack_pop_count  = norm40_temp_stack_pop_count;

const
   fcmp_BEXP   = 1;
   fcmp_BARGB0 = 2;
   fcmp_BARGB1 = 3;
   fcmp_BARGB2 = 4;
   fcmp_AEXP   = 5;
   fcmp_AARGB0 = 6;
   fcmp_AARGB1 = 7;
   fcmp_AARGB2 = 8;
   fcmp_bool_result = 8;
   fcmp_total_stack = 8;
   fcmp_temp_stack_push_count = fcmp_total_stack - (2 * real_size);
   fcmp_temp_stack_pop_count = fcmp_total_stack - bool_size;


type
   TInt24ToFloatConversion =
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
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TInt32ToFloatConversion =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TINT3224 =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TINT3232 =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TNRM4032 =
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
   TNRMRND4032 =
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
   Tset_true_and_return =
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
   Tset_false_and_return =
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
   FLO2432: TInt24ToFloatConversion;
   FLO3232: TInt32ToFloatConversion;
   INT3224: TINT3224;
   INT3232: TINT3232;
   NRM4032: TNRM4032;
   NRMRND4032: TNRMRND4032;
   set_true_and_return: Tset_true_and_return;
   set_false_and_return: Tset_false_and_return;

const
   EXPBIAS = 127;
   sign_bit = 7;
   RND = 0;

function TPIC18x_Real.get_pic_byte (idx: integer): byte;
   var
      v: record
            case integer of
               0: (s: single);    // warning! assumes IEEE-754 single precision float in pascal compiler implementation
               1: (b: array [0..3] of byte)
         end;
      sign: boolean;
   begin
      v.s := r;
      sign := (v.b[3] and $80) = $80;
      v.b[3] := (v.b[3] shl 1) and $ff;
      if v.b[2] and $80 = $80 then
         v.b[3] := v.b[3] or $01;
      if sign then
         v.b[2] := v.b[2] or $80
      else
         v.b[2] := v.b[2] and $7f;
      result := v.b[idx]
   end;

function TPIC18x_Real.get_ieee_byte (idx: integer): byte;
   var
      v: record
            case integer of
               0: (s: single);    // warning! assumes IEEE-754 single precision float in pascal compiler implementation
               1: (b: array [0..3] of byte)
         end;
   begin
      v.s := r;
      result := v.b[idx]
   end;

procedure convert_tos_from_pic_to_ieee;
   begin
      TPIC18x_RLCF.Create (2, dest_f, access_mode).annotation := 'convert tos from pic real to ieee single';
      TPIC18x_RRCF.Create (1, dest_f, access_mode);
      TPIC18x_RRCF.Create (2, dest_f, access_mode)
   end;

procedure convert_tos_from_ieee_to_pic;
   begin
      TPIC18x_RLCF.Create (2, dest_f, access_mode).annotation := 'convert tos from ieee single to pic real';
      TPIC18x_RLCF.Create (1, dest_f, access_mode);
      TPIC18x_RRCF.Create (2, dest_f, access_mode)
   end;

procedure convert_tos_integer_to_real (tos_size: integer; tos_info: TPIC18x_TypeInfo; src_loc: TSourceLocation);
   var
      tint24, tint32: TTypeDef;
   begin
      tint24 := target_cpu.get_supported_data_type ('int24');
      tint32 := target_cpu.get_supported_data_type ('int32');
      if tos_info.min_value.ge (tint24.info.min_value)
         and
         tos_info.max_value.le (tint24.info.max_value)
      then  // can use 24 bit conversion without checks
         begin
            generate_stack_fix_and_sign_extend_code (tos_size, 0, int24_size, tos_info.IntegerRange);
            FLO2432.Call (src_loc)
         end
      else if tos_info.min_value.ge (tint32.info.min_value)
              and
              tos_info.max_value.le (tint32.info.max_value)
      then  // can use 32 bit conversion without checks
         begin
            generate_stack_fix_and_sign_extend_code (tos_size, 0, int32_size, tos_info.IntegerRange);
            FLO3232.Call (src_loc)
         end
      else
         begin
            GenerateRangeCheckCode (TOrdinalDataType(tint32), tos_size, tint32.info, src_loc, 'integer result exeeds 32 bits');
            if tos_size > 4 then
               begin
                  TPIC18x_ADDFSR.Create (2, tos_size-4);
                  StackUsageCounter.Pop (tos_size-4)
               end;
            FLO3232.Call (src_loc)
         end
   end;

function optimal_int_size_before_float (tos_info: TTypeInfo): integer;
   var
      tint24, tint32: TTypeDef;
   begin
      tint24 := target_cpu.get_supported_data_type ('int24');
      tint32 := target_cpu.get_supported_data_type ('int32');
      if tos_info.min_value.ge (tint24.info.min_value)
         and
         tos_info.max_value.le (tint24.info.max_value)
      then  // will be able use 24 bit conversion without checks
         result := int24_size
      else if tos_info.min_value.ge (tint32.info.min_value)
              and
              tos_info.max_value.le (tint32.info.max_value)
      then  // will be able to use 32 bit conversion without checks
         result := int32_size
      else  // will need checks
         result := TPIC18x_TypeInfo(tos_info).Size
   end;

procedure generate_integer_expression_to_real_code (expr: TExpression);
   var
      tint24, tint32: TTypeDef;
      expression_size: integer;
   begin
      assert (expr.expression_kind = integer_expression);
      tint24 := target_cpu.get_supported_data_type ('int24');
      tint32 := target_cpu.get_supported_data_type ('int32');
      if expr.info.min_value.ge (tint24.info.min_value)
         and
         expr.info.max_value.le (tint24.info.max_value)
      then  // can use 24 bit conversion without checks
         begin
            expr.Generate (GenerateCode, int24_size);
            FLO2432.Call (expr.src_loc)
         end
      else if expr.info.min_value.ge (tint32.info.min_value)
              and
              expr.info.max_value.le (tint32.info.max_value)
      then  // can use 32 bit conversion without checks
         begin
            expr.Generate (GenerateCode, int32_size);
            FLO3232.Call (expr.src_loc)
         end
      else  // integer expression can exceed 32 bit integer
         begin
            expression_size := TPIC18x_TypeInfo(expr.info).Size;
            expr.Generate (GenerateCode, expression_size);
            GenerateRangeCheckCode (TOrdinalDataType(tint32), expression_size, expr.info, expr.src_loc, 'integer result exeeds 32 bits');
            if expression_size > 4 then
               begin
                  TPIC18x_ADDFSR.Create (2, expression_size - real_size);
                  StackUsageCounter.Pop (expression_size - real_size)
               end;
            FLO3232.Call (expr.src_loc)
         end
   end;

function generate_fixsign32_code (AARGB0, SIGN, pop_count: integer): TInstruction;
   begin
      result := TPIC18x_BTFSS.Create (SIGN, sign_bit, access_mode);
      TPIC18x_BCF.Create (AARGB0, sign_bit, access_mode);
      TPIC18x_ADDULNK.Create (pop_count)
   end;

function generate_res032_code (EXP, AARGB0, AARGB1, AARGB2, AARGB3, pop_count: integer): TInstruction;
   begin
      result := TPIC18x_CLRF.Create (AARGB0,access_mode);
      TPIC18x_CLRF.Create (AARGB1,access_mode);
      TPIC18x_CLRF.Create (AARGB2,access_mode);
      if AARGB3 <> -1 then
         TPIC18x_CLRF.Create (AARGB3,access_mode);
      TPIC18x_CLRF.Create (EXP,access_mode);
      TPIC18x_ADDULNK.Create (pop_count)
   end;

function generate_set_ov_code (AEXP, AARGB0, AARGB1, AARGB2, SIGN, pop_count: integer): TInstruction;
   begin
      result := TPIC18x_SETF.Create (AEXP, access_mode);
      TPIC18x_SETF.Create (AARGB0, access_mode);
      TPIC18x_SETF.Create (AARGB1, access_mode);
      TPIC18x_SETF.Create (AARGB2, access_mode);
      TPIC18x_RLCF.Create (SIGN, dest_f, access_mode);
      TPIC18x_RRCF.Create (AARGB0, dest_f, access_mode);
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create)
   end;

function generate_set_fun_code (AEXP, AARGB0, AARGB1, AARGB2, SIGN, pop_count: integer): TInstruction;
   begin
      result := TPIC18x_MOVLW.Create (1);
      TPIC18x_MOVWF.Create (AEXP, access_mode);
      TPIC18x_CLRF.Create (AARGB0,access_mode);
      TPIC18x_CLRF.Create (AARGB1,access_mode);
      TPIC18x_CLRF.Create (AARGB2,access_mode);
      TPIC18x_RLCF.Create (SIGN, dest_f, access_mode);
      TPIC18x_RRCF.Create (AARGB0, dest_f, access_mode);
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_m1_routine.ComeFrom (TGOTOMacro.Create)
   end;


// ===================
//  TFloatingPointAdd

constructor TFloatingPointAdd.Create;
   begin
      inherited Create (fadd_temp_stack_push_count, fadd_temp_stack_pop_count, 'add two real numbers')
   end;

function TFloatingPointAdd.Call (src_loc: TSourceLocation): TInstruction;
   var
      lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do real addition';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointAdd.report_stack_sizes;
   begin
      check_stack_sizes (fadd_temp_stack_push_count, fadd_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointAdd.generate_subroutine_code;
   var
      ACOMP32,
      ALIGNED32,
      ALOOP32,
      ALOOP32A,
      ALOOP32B,
      ALOOP32C,
      ANIB32,
      ANIB32A,
      ANIB32B,
      ANIB32C,
      AOK32,
      BLIGNED32,
      BLOOP32,
      BLOOP32A,
      BLOOP32B,
      BLOOP32C,
      BNE032,
      BNIB32,
      BNIB32A,
      BNIB32B,
      BNIB32C,
      BRETURN32,
      USEA32,
      USEB32,
      SETFOV32,
      norm40,
      norm40rnd: TBranchTarget;
   begin
      ACOMP32 := TBranchTarget.Create;
      ALIGNED32 := TBranchTarget.Create;
      ALOOP32 := TBranchTarget.Create;
      ALOOP32A := TBranchTarget.Create;
      ALOOP32B := TBranchTarget.Create;
      ALOOP32C := TBranchTarget.Create;
      ANIB32 := TBranchTarget.Create;
      ANIB32A := TBranchTarget.Create;
      ANIB32B := TBranchTarget.Create;
      ANIB32C := TBranchTarget.Create;
      AOK32 := TBranchTarget.Create;
      BLIGNED32 := TBranchTarget.Create;
      BLOOP32 := TBranchTarget.Create;
      BLOOP32A := TBranchTarget.Create;
      BLOOP32B := TBranchTarget.Create;
      BLOOP32C := TBranchTarget.Create;
      BNE032 := TBranchTarget.Create;
      BNIB32 := TBranchTarget.Create;
      BNIB32A := TBranchTarget.Create;
      BNIB32B := TBranchTarget.Create;
      BNIB32C := TBranchTarget.Create;
      BRETURN32 := TBranchTarget.Create;
      USEA32 := TBranchTarget.Create;
      USEB32 := TBranchTarget.Create;
      SETFOV32 := TBranchTarget.Create;
      norm40 := TBranchTarget.Create;
      norm40rnd := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, fadd_temp_stack_push_count);
      TPIC18x_MOVF.Create (fadd_AARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_TEMPB0, access_mode);
      TPIC18x_CLRF.Create (fadd_AARGB3, access_mode);
      TPIC18x_MOVF.Create (fadd_AEXP, dest_w, access_mode);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      USEA32.ComeFrom (TPIC18x_BRA.Create);
      USEB32.target_label := TPIC18x_MOVF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_SIGN, access_mode);
      TPIC18x_BSF.Create (fadd_BARGB0, sign_bit, access_mode);
      TPIC18x_BSF.Create (fadd_AARGB0, sign_bit, access_mode);
      TPIC18x_MOVF.Create (fadd_AEXP, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_TEMPB1, access_mode);
      TPIC18x_MOVF.Create (fadd_BEXP, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_AEXP, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CPFSGT.Create (fadd_TEMPB1, access_mode);
      BRETURN32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVF.Create (fadd_TEMPB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BNIB32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (fadd_AARGB2, fadd_AARGB3);
      TPIC18x_MOVSS.Create (fadd_AARGB1, fadd_AARGB2);
      TPIC18x_MOVSS.Create (fadd_AARGB0, fadd_AARGB1);
      TPIC18x_CLRF.Create (fadd_AARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BNIB32A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (fadd_AARGB2, fadd_AARGB3);
      TPIC18x_MOVSS.Create (fadd_AARGB1, fadd_AARGB2);
      TPIC18x_CLRF.Create (fadd_AARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BNIB32B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (fadd_AARGB2, fadd_AARGB3);
      TPIC18x_CLRF.Create (fadd_AARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BNIB32C.ComeFrom (TPIC18x_BRA.Create);
      BRETURN32.target_label := TPIC18x_MOVSS.Create (fadd_SIGN, fadd_AARGB0);
      TPIC18x_MOVSS.Create (fadd_BARGB1, fadd_AARGB1);
      TPIC18x_MOVSS.Create (fadd_BARGB2, fadd_AARGB2);
      TPIC18x_CLRF.Create (fadd_AARGB3, access_mode);
      TPIC18x_ADDULNK.Create (fadd_temp_stack_pop_count);
      BNIB32C.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BLOOP32C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB3, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BLOOP32C.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BNIB32B.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BLOOP32B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BLOOP32B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BNIB32A.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BLOOP32A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BLOOP32A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BNIB32.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      BLOOP32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_AARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      BLOOP32.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      BLIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      BLIGNED32.target_label := TPIC18x_CLRF.Create (fadd_BARGB3, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_BTFSS.Create (fadd_TEMPB0, sign_bit, access_mode);
      AOK32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_COMF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB0, dest_f, access_mode);
      AOK32.ComeFrom (TPIC18x_BRA.Create);
      USEA32.target_label := TPIC18x_TSTFSZ.Create (fadd_BEXP, access_mode);
      BNE032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_ADDULNK.Create (fadd_temp_stack_pop_count);
      BNE032.target_label := TPIC18x_CLRF.Create (fadd_BARGB3, access_mode);
      TPIC18x_MOVSS.Create (fadd_AARGB0, fadd_SIGN);
      TPIC18x_BSF.Create (fadd_AARGB0, sign_bit, access_mode);
      TPIC18x_BSF.Create (fadd_BARGB0, sign_bit, access_mode);
      TPIC18x_MOVF.Create (fadd_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fadd_AEXP, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BEXP, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ANIB32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB2, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB1, access_mode);
      TPIC18x_CLRF.Create (fadd_BARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ANIB32A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB2, access_mode);
      TPIC18x_CLRF.Create (fadd_BARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ANIB32B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_CLRF.Create (fadd_BARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ANIB32C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVSS.Create (fadd_SIGN, fadd_AARGB0);
      TPIC18x_ADDULNK.Create (fadd_temp_stack_pop_count);
      ANIB32C.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ALOOP32C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ALOOP32C.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ANIB32B.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ALOOP32B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ALOOP32B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ANIB32A.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ALOOP32A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB2, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ALOOP32A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ANIB32.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (fadd_BEXP, access_mode);
      ALOOP32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fadd_BEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB3, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB2, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB1, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (fadd_BARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      ALOOP32.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (fadd_BEXP, dest_f, access_mode);
      ALIGNED32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_BARGB3, dest_f, access_mode);
      ALIGNED32.target_label := TPIC18x_CLRF.Create (fadd_AARGB3, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_BTFSS.Create (fadd_TEMPB0, sign_bit, access_mode);
      AOK32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_COMF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_BARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (fadd_BARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_BARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_BARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_BARGB0, dest_f, access_mode);
      AOK32.target_label := TPIC18x_MOVF.Create (fadd_BARGB3, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB2, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB1, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (fadd_BARGB0, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (fadd_TEMPB0, sign_bit, access_mode);
      ACOMP32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      norm40rnd.ComeFrom (TGOTOMacro.Create);
      TPIC18x_RRCF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_INCFSZ.Create (fadd_AEXP, dest_f, access_mode);
      norm40rnd.ComeFrom (TGOTOMacro.Create);
      SETFOV32.ComeFrom (TPIC18x_BRA.Create);
      ACOMP32.target_label := TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      norm40.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (fadd_AARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fadd_AARGB0, dest_f, access_mode);
      TPIC18x_BTG.Create (fadd_SIGN, sign_bit, access_mode);
      norm40.target_label := TPIC18x_MOVSS.Create (fadd_SIGN, fads_total_stack - norm40_total_stack + norm40_sign);
      TPIC18x_MOVSS.Create (fadd_AARGB3, fads_total_stack - norm40_total_stack + norm40_AARGB3);
      TPIC18x_ADDFSR.Create (2, fads_total_stack - norm40_total_stack);
      NRM4032.ComeFrom (TGOTOMacro.Create);
      norm40rnd.target_label := TPIC18x_MOVSS.Create (fadd_SIGN, fads_total_stack - norm40_total_stack + norm40_sign);
      TPIC18x_MOVSS.Create (fadd_AARGB3, fads_total_stack - norm40_total_stack + norm40_AARGB3);
      TPIC18x_ADDFSR.Create (2, fads_total_stack - norm40_total_stack);
      NRMRND4032.ComeFrom (TGOTOMacro.Create);
      SETFOV32.target_label := generate_set_ov_code (fadd_AEXP, fadd_AARGB0, fadd_AARGB1, fadd_AARGB2, fadd_SIGN, fadd_temp_stack_pop_count);

      ACOMP32.set_client_destinations;
      ALIGNED32.set_client_destinations;
      ALOOP32.set_client_destinations;
      ALOOP32A.set_client_destinations;
      ALOOP32B.set_client_destinations;
      ALOOP32C.set_client_destinations;
      ANIB32.set_client_destinations;
      ANIB32A.set_client_destinations;
      ANIB32B.set_client_destinations;
      ANIB32C.set_client_destinations;
      AOK32.set_client_destinations;
      BLIGNED32.set_client_destinations;
      BLOOP32.set_client_destinations;
      BLOOP32A.set_client_destinations;
      BLOOP32B.set_client_destinations;
      BLOOP32C.set_client_destinations;
      BNE032.set_client_destinations;
      BNIB32.set_client_destinations;
      BNIB32A.set_client_destinations;
      BNIB32B.set_client_destinations;
      BNIB32C.set_client_destinations;
      BRETURN32.set_client_destinations;
      USEA32.set_client_destinations;
      USEB32.set_client_destinations;
      SETFOV32.set_client_destinations;
      norm40.set_client_destinations;
      norm40rnd.set_client_destinations;

      ACOMP32.Free;
      ALIGNED32.Free;
      ALOOP32.Free;
      ALOOP32A.Free;
      ALOOP32B.Free;
      ALOOP32C.Free;
      ANIB32.Free;
      ANIB32A.Free;
      ANIB32B.Free;
      ANIB32C.Free;
      AOK32.Free;
      BLIGNED32.Free;
      BLOOP32.Free;
      BLOOP32A.Free;
      BLOOP32B.Free;
      BLOOP32C.Free;
      BNE032.Free;
      BNIB32.Free;
      BNIB32A.Free;
      BNIB32B.Free;
      BNIB32C.Free;
      BRETURN32.Free;
      USEA32.Free;
      USEB32.Free;
      SETFOV32.Free;
      norm40.Free;
      norm40rnd.Free
   end;

procedure TFloatingPointAdd.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (NRMRND4032, -(fads_total_stack - norm40_total_stack));
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -fadd_temp_stack_pop_count);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_m1_routine, -fadd_temp_stack_pop_count);
   end;


// ========================
//  TFloatingPointSubtract

constructor TFloatingPointSubtract.Create;
   begin
      inherited Create (fsub_temp_stack_push_count, fsub_temp_stack_pop_count, 'subtract two real numbers')
   end;

function TFloatingPointSubtract.Call (src_loc: TSourceLocation): TInstruction;
   var
      lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do real subtraction';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc)   // behavior not seen?
   end;

procedure TFloatingPointSubtract.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (FPA32, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointSubtract.report_stack_sizes;
   begin
      check_stack_sizes (fsub_temp_stack_push_count, fsub_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointSubtract.generate_subroutine_code;
   const
      BARGB0 = 2;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_BTG.Create (BARGB0, sign_bit, access_mode);
      FPA32.ComeFrom (TGOTOMacro.Create)
   end;


// ========================
//  TFloatingPointMultiply

constructor TFloatingPointMultiply.Create;
   begin
      inherited Create (fmult_temp_stack_push_count, fmult_temp_stack_pop_count, 'subroutine for real multiplication')
   end;

function TFloatingPointMultiply.Call (src_loc: TSourceLocation): TInstruction;
   var
      lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do real multiplication';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointMultiply.report_stack_sizes;
   begin
      check_stack_sizes (fmult_temp_stack_push_count, fmult_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointMultiply.generate_subroutine_code;
   var
      M32BNE0,
      MOK32,
      MROUND32,
      MTUN32,
      MUL32OK,
      RES032,
      SETFOV32,
      SETFUN32: TBranchTarget;
   begin
      M32BNE0 := TBranchTarget.Create;
      MOK32 := TBranchTarget.Create;
      MROUND32 := TBranchTarget.Create;
      MTUN32 := TBranchTarget.Create;
      MUL32OK := TBranchTarget.Create;
      RES032 := TBranchTarget.Create;
      SETFOV32 := TBranchTarget.Create;
      SETFUN32 := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, fmult_temp_stack_push_count);
      TPIC18x_CLRF.Create (fmult_AARGB3, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CPFSEQ.Create (fmult_BEXP, access_mode);
      TPIC18x_CPFSGT.Create (fmult_AEXP, access_mode);
      RES032.ComeFrom (TGOTOMacro.Create);
      M32BNE0.target_label := TPIC18x_MOVF.Create (fmult_AARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fmult_BARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fmult_SIGN, access_mode);
      TPIC18x_MOVF.Create (fmult_BEXP, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AEXP, dest_f, access_mode);
      TPIC18x_MOVLW.Create (EXPBIAS-1);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      MTUN32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (fmult_AEXP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      SETFOV32.ComeFrom (TGOTOMacro.Create);
      MOK32.ComeFrom (TPIC18x_BRA.Create);
      MTUN32.target_label := TPIC18x_SUBWF.Create (fmult_AEXP, dest_f, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      SETFUN32.ComeFrom (TGOTOMacro.Create);
      MOK32.target_label := TPIC18x_BSF.Create (fmult_AARGB0, sign_bit, access_mode);
      TPIC18x_BSF.Create (fmult_BARGB0, sign_bit, access_mode);
      TPIC18x_MOVSS.Create (fmult_AARGB0, fmult_TEMPB0);
      TPIC18x_MOVSS.Create (fmult_AARGB1, fmult_TEMPB1);
      TPIC18x_MOVSS.Create (fmult_AARGB2, fmult_TEMPB2);
      TPIC18x_MOVF.Create (fmult_AARGB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fmult_AARGB4, access_mode);
      TPIC18x_MOVF.Create (fmult_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB1, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fmult_AARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fmult_AARGB3, access_mode);
      TPIC18x_MOVF.Create (fmult_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_TEMPB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB1, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB1, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CLRF.Create (fmult_AARGB1, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_TEMPB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CLRF.Create (fmult_AARGB0, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_TEMPB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fmult_TEMPB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fmult_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (fmult_AARGB0, sign_bit, access_mode);
      MROUND32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (fmult_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_DECF.Create (fmult_AEXP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
      SETFUN32.ComeFrom (TGOTOMacro.Create);
      MROUND32.target_label := TPIC18x_BTFSS.Create (fmult_AARGB3, sign_bit, access_mode);
      MUL32OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BSF.Create (STATUS, status_c, access_mode);
      TPIC18x_MOVLW.Create (128);
      TPIC18x_CPFSGT.Create (fmult_AARGB3, access_mode);
      TPIC18x_RRCF.Create (fmult_AARGB2, dest_w, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      MUL32OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RRCF.Create (fmult_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fmult_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fmult_AARGB2, dest_f, access_mode);
      TPIC18x_INFSNZ.Create (fmult_AEXP, dest_f, access_mode);
      SETFOV32.ComeFrom (TGOTOMacro.Create);
      MUL32OK.target_label := TPIC18x_BTFSS.Create (fmult_SIGN, sign_bit, access_mode);
      TPIC18x_BCF.Create (fmult_AARGB0, sign_bit, access_mode);
      TPIC18x_ADDULNK.Create (fmult_temp_stack_pop_count);
      RES032.target_label := generate_res032_code (fmult_AEXP, fmult_AARGB0, fmult_AARGB1, fmult_AARGB2, fmult_AARGB3, fmult_temp_stack_pop_count);
      SETFOV32.target_label := generate_set_ov_code (fmult_AEXP, fmult_AARGB0, fmult_AARGB1, fmult_AARGB2, fmult_SIGN, fmult_temp_stack_pop_count);
      SETFUN32.target_label := generate_set_fun_code (fmult_AEXP, fmult_AARGB0, fmult_AARGB1, fmult_AARGB2, fmult_SIGN, fmult_temp_stack_pop_count);

      M32BNE0.set_client_destinations;
      MOK32.set_client_destinations;
      MROUND32.set_client_destinations;
      MTUN32.set_client_destinations;
      MUL32OK.set_client_destinations;
      RES032.set_client_destinations;
      SETFOV32.set_client_destinations;
      SETFUN32.set_client_destinations;

      M32BNE0.Free;
      MOK32.Free;
      MROUND32.Free;
      MTUN32.Free;
      MUL32OK.Free;
      RES032.Free;
      SETFOV32.Free;
      SETFUN32.Free
   end;

procedure TFloatingPointMultiply.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -fmult_temp_stack_pop_count);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_m1_routine, -fmult_temp_stack_pop_count)
   end;



// ======================
//  TFloatingPointDivide

constructor TFloatingPointDivide.Create;
   begin
      inherited Create (fdiv_temp_stack_push_count, fdiv_temp_stack_pop_count, 'subroutine for real divide')
   end;

function TFloatingPointDivide.generate_call_code: TInstruction;
   begin
      result := TPIC18x_CALL.Create;    // use 4 byte CALL instead of 2 byte RCALL since we need 3 error addresses
      call_instruction := result
   end;

function TFloatingPointDivide.Call (src_loc: TSourceLocation): TInstruction;
   var
      lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do real division';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -2, rterr_floating_point_zero_divide, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointDivide.report_stack_sizes;
   begin
      check_stack_sizes (fdiv_temp_stack_push_count, fdiv_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointDivide.generate_subroutine_code;
   var
      AGEB32,
      ALTB32,
      D32BNE0,
      DAOK32,
      DBOK32,
      DEXP32,
      DIV32OK,
      DROUND32,
      SETFDZ32,
      RES032,
      SETFOV32,
      SETFUN32: TBranchTarget;
      IBTBL256I: TInstruction;
      add_b0: TADDLW_ROMAddr_B0;
      add_b1: TMOVLW_ROMAddr_B1;
   begin
      AGEB32 := TBranchTarget.Create;
      ALTB32 := TBranchTarget.Create;
      D32BNE0 := TBranchTarget.Create;
      DAOK32 := TBranchTarget.Create;
      DBOK32 := TBranchTarget.Create;
      DEXP32 := TBranchTarget.Create;
      DIV32OK := TBranchTarget.Create;
      DROUND32 := TBranchTarget.Create;
      SETFDZ32 := TBranchTarget.Create;
      RES032 := TBranchTarget.Create;
      SETFOV32 := TBranchTarget.Create;
      SETFUN32 := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, fdiv_temp_stack_push_count);
      TPIC18x_CLRF.Create (fdiv_TEMPB3, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CPFSGT.Create (fdiv_BEXP, access_mode);
      SETFDZ32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (fdiv_AEXP, access_mode);
      RES032.ComeFrom (TPIC18x_BRA.Create);
      D32BNE0.target_label := TPIC18x_MOVF.Create (fdiv_AARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fdiv_BARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_SIGN, access_mode);
      TPIC18x_BSF.Create (fdiv_AARGB0, sign_bit, access_mode);
      TPIC18x_BSF.Create (fdiv_BARGB0, sign_bit, access_mode);
      add_b1 := TMOVLW_ROMAddr_B1.Create;   {.dest := IBTBL256I;}
      TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB1, dest_w, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB0, dest_w, access_mode);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RLCF.Create (WREG, dest_w, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      TPIC18x_INCF.Create (TBLPTRH, dest_f, access_mode);
      add_b0 := TADDLW_ROMAddr_B0.Create;  {.dest := IBTBL256I;}
      TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      TPIC18x_INCF.Create (TBLPTRH, dest_f, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB5, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB5, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fdiv_TEMPB1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB5, access_mode);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_AARGB5, access_mode);
      TPIC18x_MOVFF.Create (PRODH, TBLPTRH);
      TPIC18x_RLCF.Create (fdiv_BARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_AARGB5, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (TBLPTRH, dest_f, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      TPIC18x_INCF.Create (PRODH, dest_f, access_mode);
      TPIC18x_CLRF.Create (fdiv_TEMPB2,access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fdiv_TEMPB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_SUBWFB.Create (fdiv_TEMPB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_SUBWFB.Create (fdiv_TEMPB0, dest_f, access_mode);
      TPIC18x_MOVSS.Create (fdiv_AARGB0, fdiv_AARGB5);
      TPIC18x_MOVSF.Create (fdiv_AARGB1, TBLPTRH);
      TPIC18x_MOVSF.Create (fdiv_AARGB2, TBLPTRL);
      TPIC18x_MOVF.Create (fdiv_AARGB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB4, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB3, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB1, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB0, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB5, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (fdiv_AARGB0, sign_bit, access_mode);
      DAOK32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_DECF.Create (fdiv_TEMPB3, dest_f, access_mode);
      DAOK32.target_label := TPIC18x_MOVSS.Create (fdiv_BARGB0, fdiv_AARGB5);
      TPIC18x_MOVSF.Create (fdiv_BARGB1, TBLPTRH);
      TPIC18x_MOVSF.Create (fdiv_BARGB2, TBLPTRL);
      TPIC18x_MOVF.Create (fdiv_BARGB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB4, access_mode);
      TPIC18x_MOVF.Create (fdiv_BARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_BARGB3, access_mode);
      TPIC18x_MOVF.Create (fdiv_BARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_BARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_BARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB1, access_mode);
      TPIC18x_CLRF.Create (fdiv_BARGB1, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_CLRF.Create (fdiv_BARGB0, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB5, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_TEMPB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_BTFSS.Create (fdiv_BARGB0, sign_bit, access_mode);
      TPIC18x_BTFSC.Create (fdiv_BARGB0, sign_bit-1, access_mode);
      DBOK32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (fdiv_TEMPB3, dest_f, access_mode);
      DBOK32.target_label := TPIC18x_COMF.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (fdiv_BARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_BARGB0, dest_f, access_mode);
      TPIC18x_MOVSS.Create (fdiv_AARGB0, fdiv_TEMPB0);
      TPIC18x_MOVSS.Create (fdiv_AARGB1, fdiv_TEMPB1);
      TPIC18x_MOVSS.Create (fdiv_AARGB2, fdiv_TEMPB2);
      TPIC18x_MOVSF.Create (fdiv_AARGB3, TBLPTRL);
      TPIC18x_MOVF.Create (fdiv_AARGB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB4, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB3, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB2, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB3, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB1, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB2, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB1, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB1, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB2, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_AARGB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB1, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB1, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB3, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB0, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (fdiv_AARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB4, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB2, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_MOVF.Create (fdiv_TEMPB1, dest_w, access_mode);
      TPIC18x_MULWF.Create (fdiv_BARGB0, access_mode);
      TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (fdiv_AARGB0, sign_bit, access_mode);
      DEXP32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_DECF.Create (fdiv_TEMPB3, dest_f, access_mode);
      TPIC18x_BTFSC.Create (fdiv_AARGB0, sign_bit, access_mode);
      DEXP32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (fdiv_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_DECF.Create (fdiv_TEMPB3, dest_f, access_mode);
      DEXP32.target_label := TPIC18x_MOVF.Create (fdiv_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fdiv_AEXP, dest_f, access_mode);
      TPIC18x_MOVLW.Create (EXPBIAS+1);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      ALTB32.ComeFrom (TPIC18x_BRA.Create);
      AGEB32.target_label := TPIC18x_ADDWF.Create (fdiv_TEMPB3, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AEXP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
      SETFOV32.ComeFrom (TPIC18x_BRA.Create);
      DROUND32.ComeFrom (TPIC18x_BRA.Create);
      ALTB32.target_label := TPIC18x_ADDWF.Create (fdiv_TEMPB3, dest_w, access_mode);
      TPIC18x_ADDWF.Create (fdiv_AEXP, dest_f, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      SETFUN32.ComeFrom (TPIC18x_BRA.Create);
      DROUND32.target_label := TPIC18x_BTFSS.Create (fdiv_AARGB3, sign_bit, access_mode);
      DIV32OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BSF.Create (STATUS, status_c, access_mode);
      TPIC18x_MOVLW.Create (128);
      TPIC18x_CPFSGT.Create (fdiv_AARGB3, access_mode);
      TPIC18x_RRCF.Create (fdiv_AARGB2, dest_w, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      DIV32OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RRCF.Create (fdiv_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (fdiv_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (fdiv_AARGB2, dest_f, access_mode);
      TPIC18x_INFSNZ.Create (fdiv_AEXP, dest_f, access_mode);
      SETFOV32.ComeFrom (TPIC18x_BRA.Create);
      DIV32OK.target_label := TPIC18x_BTFSS.Create (fdiv_SIGN, sign_bit, access_mode);
      TPIC18x_BCF.Create (fdiv_AARGB0, sign_bit, access_mode);
      TPIC18x_ADDULNK.Create (fdiv_temp_stack_pop_count);
      SETFDZ32.target_label := TPIC18x_CLRF.Create (fdiv_AEXP, access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB0,access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB1,access_mode);
      TPIC18x_CLRF.Create (fdiv_AARGB2,access_mode);
      TPIC18x_ADDFSR.Create (2, fdiv_temp_stack_pop_count);
      set_errorcode_M2_routine.ComeFrom (TGOTOMacro.Create);
      RES032.target_label := generate_res032_code (fdiv_AEXP, fdiv_AARGB0, fdiv_AARGB1, fdiv_AARGB2, fdiv_AARGB3, fdiv_temp_stack_pop_count);
      SETFOV32.target_label := generate_set_ov_code (fdiv_AEXP, fdiv_AARGB0, fdiv_AARGB1, fdiv_AARGB2, fdiv_SIGN, fdiv_temp_stack_pop_count);
      SETFUN32.target_label := generate_set_fun_code (fdiv_AEXP, fdiv_AARGB0, fdiv_AARGB1, fdiv_AARGB2, fdiv_SIGN, fdiv_temp_stack_pop_count);

      TAssemblySourceBlankLine_Packed.Create;
      IBTBL256I := TAssemblyLabel_Packed.Create;
      IBTBL256I.annotation := 'tables for floating point divide routine';
      add_b0.dest := IBTBL256I;
      add_b1.dest := IBTBL256I;
      TPIC18x_DW_Packed.Create ($FFFF);
      TPIC18x_DW_Packed.Create ($FF01);
      TPIC18x_DW_Packed.Create ($FE04);
      TPIC18x_DW_Packed.Create ($FD09);
      TPIC18x_DW_Packed.Create ($FC10);
      TPIC18x_DW_Packed.Create ($FB19);
      TPIC18x_DW_Packed.Create ($FA23);
      TPIC18x_DW_Packed.Create ($F930);
      TPIC18x_DW_Packed.Create ($F83E);
      TPIC18x_DW_Packed.Create ($F74E);
      TPIC18x_DW_Packed.Create ($F660);
      TPIC18x_DW_Packed.Create ($F574);
      TPIC18x_DW_Packed.Create ($F48A);
      TPIC18x_DW_Packed.Create ($F3A1);
      TPIC18x_DW_Packed.Create ($F2BA);
      TPIC18x_DW_Packed.Create ($F1D5);
      TPIC18x_DW_Packed.Create ($F0F1);
      TPIC18x_DW_Packed.Create ($F00F);
      TPIC18x_DW_Packed.Create ($EF2F);
      TPIC18x_DW_Packed.Create ($EE50);
      TPIC18x_DW_Packed.Create ($ED73);
      TPIC18x_DW_Packed.Create ($EC98);
      TPIC18x_DW_Packed.Create ($EBBE);
      TPIC18x_DW_Packed.Create ($EAE5);
      TPIC18x_DW_Packed.Create ($EA0F);
      TPIC18x_DW_Packed.Create ($E939);
      TPIC18x_DW_Packed.Create ($E866);
      TPIC18x_DW_Packed.Create ($E793);
      TPIC18x_DW_Packed.Create ($E6C3);
      TPIC18x_DW_Packed.Create ($E5F3);
      TPIC18x_DW_Packed.Create ($E526);
      TPIC18x_DW_Packed.Create ($E459);
      TPIC18x_DW_Packed.Create ($E38E);
      TPIC18x_DW_Packed.Create ($E2C5);
      TPIC18x_DW_Packed.Create ($E1FC);
      TPIC18x_DW_Packed.Create ($E136);
      TPIC18x_DW_Packed.Create ($E070);
      TPIC18x_DW_Packed.Create ($DFAC);
      TPIC18x_DW_Packed.Create ($DEE9);
      TPIC18x_DW_Packed.Create ($DE28);
      TPIC18x_DW_Packed.Create ($DD68);
      TPIC18x_DW_Packed.Create ($DCA9);
      TPIC18x_DW_Packed.Create ($DBEB);
      TPIC18x_DW_Packed.Create ($DB2F);
      TPIC18x_DW_Packed.Create ($DA74);
      TPIC18x_DW_Packed.Create ($D9BA);
      TPIC18x_DW_Packed.Create ($D902);
      TPIC18x_DW_Packed.Create ($D84A);
      TPIC18x_DW_Packed.Create ($D794);
      TPIC18x_DW_Packed.Create ($D6DF);
      TPIC18x_DW_Packed.Create ($D62C);
      TPIC18x_DW_Packed.Create ($D579);
      TPIC18x_DW_Packed.Create ($D4C7);
      TPIC18x_DW_Packed.Create ($D417);
      TPIC18x_DW_Packed.Create ($D368);
      TPIC18x_DW_Packed.Create ($D2BA);
      TPIC18x_DW_Packed.Create ($D20D);
      TPIC18x_DW_Packed.Create ($D161);
      TPIC18x_DW_Packed.Create ($D0B7);
      TPIC18x_DW_Packed.Create ($D00D);
      TPIC18x_DW_Packed.Create ($CF64);
      TPIC18x_DW_Packed.Create ($CEBD);
      TPIC18x_DW_Packed.Create ($CE17);
      TPIC18x_DW_Packed.Create ($CD71);
      TPIC18x_DW_Packed.Create ($CCCD);
      TPIC18x_DW_Packed.Create ($CC29);
      TPIC18x_DW_Packed.Create ($CB87);
      TPIC18x_DW_Packed.Create ($CAE6);
      TPIC18x_DW_Packed.Create ($CA46);
      TPIC18x_DW_Packed.Create ($C9A6);
      TPIC18x_DW_Packed.Create ($C908);
      TPIC18x_DW_Packed.Create ($C86A);
      TPIC18x_DW_Packed.Create ($C7CE);
      TPIC18x_DW_Packed.Create ($C733);
      TPIC18x_DW_Packed.Create ($C698);
      TPIC18x_DW_Packed.Create ($C5FE);
      TPIC18x_DW_Packed.Create ($C566);
      TPIC18x_DW_Packed.Create ($C4CE);
      TPIC18x_DW_Packed.Create ($C437);
      TPIC18x_DW_Packed.Create ($C3A1);
      TPIC18x_DW_Packed.Create ($C30C);
      TPIC18x_DW_Packed.Create ($C278);
      TPIC18x_DW_Packed.Create ($C1E5);
      TPIC18x_DW_Packed.Create ($C152);
      TPIC18x_DW_Packed.Create ($C0C1);
      TPIC18x_DW_Packed.Create ($C030);
      TPIC18x_DW_Packed.Create ($BFA0);
      TPIC18x_DW_Packed.Create ($BF11);
      TPIC18x_DW_Packed.Create ($BE83);
      TPIC18x_DW_Packed.Create ($BDF6);
      TPIC18x_DW_Packed.Create ($BD69);
      TPIC18x_DW_Packed.Create ($BCDD);
      TPIC18x_DW_Packed.Create ($BC52);
      TPIC18x_DW_Packed.Create ($BBC8);
      TPIC18x_DW_Packed.Create ($BB3F);
      TPIC18x_DW_Packed.Create ($BAB6);
      TPIC18x_DW_Packed.Create ($BA2F);
      TPIC18x_DW_Packed.Create ($B9A8);
      TPIC18x_DW_Packed.Create ($B921);
      TPIC18x_DW_Packed.Create ($B89C);
      TPIC18x_DW_Packed.Create ($B817);
      TPIC18x_DW_Packed.Create ($B793);
      TPIC18x_DW_Packed.Create ($B710);
      TPIC18x_DW_Packed.Create ($B68D);
      TPIC18x_DW_Packed.Create ($B60B);
      TPIC18x_DW_Packed.Create ($B58A);
      TPIC18x_DW_Packed.Create ($B50A);
      TPIC18x_DW_Packed.Create ($B48A);
      TPIC18x_DW_Packed.Create ($B40B);
      TPIC18x_DW_Packed.Create ($B38D);
      TPIC18x_DW_Packed.Create ($B30F);
      TPIC18x_DW_Packed.Create ($B292);
      TPIC18x_DW_Packed.Create ($B216);
      TPIC18x_DW_Packed.Create ($B19B);
      TPIC18x_DW_Packed.Create ($B120);
      TPIC18x_DW_Packed.Create ($B0A6);
      TPIC18x_DW_Packed.Create ($B02C);
      TPIC18x_DW_Packed.Create ($AFB3);
      TPIC18x_DW_Packed.Create ($AF3B);
      TPIC18x_DW_Packed.Create ($AEC3);
      TPIC18x_DW_Packed.Create ($AE4C);
      TPIC18x_DW_Packed.Create ($ADD6);
      TPIC18x_DW_Packed.Create ($AD60);
      TPIC18x_DW_Packed.Create ($ACEB);
      TPIC18x_DW_Packed.Create ($AC77);
      TPIC18x_DW_Packed.Create ($AC03);
      TPIC18x_DW_Packed.Create ($AB8F);
      TPIC18x_DW_Packed.Create ($AB1D);
      TPIC18x_DW_Packed.Create ($AAAB);
      TPIC18x_DW_Packed.Create ($AA39);
      TPIC18x_DW_Packed.Create ($A9C8);
      TPIC18x_DW_Packed.Create ($A958);
      TPIC18x_DW_Packed.Create ($A8E8);
      TPIC18x_DW_Packed.Create ($A879);
      TPIC18x_DW_Packed.Create ($A80B);
      TPIC18x_DW_Packed.Create ($A79C);
      TPIC18x_DW_Packed.Create ($A72F);
      TPIC18x_DW_Packed.Create ($A6C2);
      TPIC18x_DW_Packed.Create ($A656);
      TPIC18x_DW_Packed.Create ($A5EA);
      TPIC18x_DW_Packed.Create ($A57F);
      TPIC18x_DW_Packed.Create ($A514);
      TPIC18x_DW_Packed.Create ($A4AA);
      TPIC18x_DW_Packed.Create ($A440);
      TPIC18x_DW_Packed.Create ($A3D7);
      TPIC18x_DW_Packed.Create ($A36E);
      TPIC18x_DW_Packed.Create ($A306);
      TPIC18x_DW_Packed.Create ($A29F);
      TPIC18x_DW_Packed.Create ($A238);
      TPIC18x_DW_Packed.Create ($A1D1);
      TPIC18x_DW_Packed.Create ($A16B);
      TPIC18x_DW_Packed.Create ($A106);
      TPIC18x_DW_Packed.Create ($A0A1);
      TPIC18x_DW_Packed.Create ($A03C);
      TPIC18x_DW_Packed.Create ($9FD8);
      TPIC18x_DW_Packed.Create ($9F74);
      TPIC18x_DW_Packed.Create ($9F11);
      TPIC18x_DW_Packed.Create ($9EAF);
      TPIC18x_DW_Packed.Create ($9E4D);
      TPIC18x_DW_Packed.Create ($9DEB);
      TPIC18x_DW_Packed.Create ($9D8A);
      TPIC18x_DW_Packed.Create ($9D29);
      TPIC18x_DW_Packed.Create ($9CC9);
      TPIC18x_DW_Packed.Create ($9C69);
      TPIC18x_DW_Packed.Create ($9C0A);
      TPIC18x_DW_Packed.Create ($9BAB);
      TPIC18x_DW_Packed.Create ($9B4C);
      TPIC18x_DW_Packed.Create ($9AEE);
      TPIC18x_DW_Packed.Create ($9A91);
      TPIC18x_DW_Packed.Create ($9A34);
      TPIC18x_DW_Packed.Create ($99D7);
      TPIC18x_DW_Packed.Create ($997B);
      TPIC18x_DW_Packed.Create ($991F);
      TPIC18x_DW_Packed.Create ($98C4);
      TPIC18x_DW_Packed.Create ($9869);
      TPIC18x_DW_Packed.Create ($980E);
      TPIC18x_DW_Packed.Create ($97B4);
      TPIC18x_DW_Packed.Create ($975A);
      TPIC18x_DW_Packed.Create ($9701);
      TPIC18x_DW_Packed.Create ($96A8);
      TPIC18x_DW_Packed.Create ($9650);
      TPIC18x_DW_Packed.Create ($95F8);
      TPIC18x_DW_Packed.Create ($95A0);
      TPIC18x_DW_Packed.Create ($9549);
      TPIC18x_DW_Packed.Create ($94F2);
      TPIC18x_DW_Packed.Create ($949C);
      TPIC18x_DW_Packed.Create ($9446);
      TPIC18x_DW_Packed.Create ($93F0);
      TPIC18x_DW_Packed.Create ($939B);
      TPIC18x_DW_Packed.Create ($9346);
      TPIC18x_DW_Packed.Create ($92F1);
      TPIC18x_DW_Packed.Create ($929D);
      TPIC18x_DW_Packed.Create ($9249);
      TPIC18x_DW_Packed.Create ($91F6);
      TPIC18x_DW_Packed.Create ($91A3);
      TPIC18x_DW_Packed.Create ($9150);
      TPIC18x_DW_Packed.Create ($90FE);
      TPIC18x_DW_Packed.Create ($90AC);
      TPIC18x_DW_Packed.Create ($905A);
      TPIC18x_DW_Packed.Create ($9009);
      TPIC18x_DW_Packed.Create ($8FB8);
      TPIC18x_DW_Packed.Create ($8F68);
      TPIC18x_DW_Packed.Create ($8F17);
      TPIC18x_DW_Packed.Create ($8EC8);
      TPIC18x_DW_Packed.Create ($8E78);
      TPIC18x_DW_Packed.Create ($8E29);
      TPIC18x_DW_Packed.Create ($8DDA);
      TPIC18x_DW_Packed.Create ($8D8C);
      TPIC18x_DW_Packed.Create ($8D3E);
      TPIC18x_DW_Packed.Create ($8CF0);
      TPIC18x_DW_Packed.Create ($8CA3);
      TPIC18x_DW_Packed.Create ($8C56);
      TPIC18x_DW_Packed.Create ($8C09);
      TPIC18x_DW_Packed.Create ($8BBC);
      TPIC18x_DW_Packed.Create ($8B70);
      TPIC18x_DW_Packed.Create ($8B24);
      TPIC18x_DW_Packed.Create ($8AD9);
      TPIC18x_DW_Packed.Create ($8A8E);
      TPIC18x_DW_Packed.Create ($8A43);
      TPIC18x_DW_Packed.Create ($89F8);
      TPIC18x_DW_Packed.Create ($89AE);
      TPIC18x_DW_Packed.Create ($8964);
      TPIC18x_DW_Packed.Create ($891B);
      TPIC18x_DW_Packed.Create ($88D2);
      TPIC18x_DW_Packed.Create ($8889);
      TPIC18x_DW_Packed.Create ($8840);
      TPIC18x_DW_Packed.Create ($87F8);
      TPIC18x_DW_Packed.Create ($87AF);
      TPIC18x_DW_Packed.Create ($8768);
      TPIC18x_DW_Packed.Create ($8720);
      TPIC18x_DW_Packed.Create ($86D9);
      TPIC18x_DW_Packed.Create ($8692);
      TPIC18x_DW_Packed.Create ($864C);
      TPIC18x_DW_Packed.Create ($8605);
      TPIC18x_DW_Packed.Create ($85BF);
      TPIC18x_DW_Packed.Create ($8579);
      TPIC18x_DW_Packed.Create ($8534);
      TPIC18x_DW_Packed.Create ($84EF);
      TPIC18x_DW_Packed.Create ($84AA);
      TPIC18x_DW_Packed.Create ($8465);
      TPIC18x_DW_Packed.Create ($8421);
      TPIC18x_DW_Packed.Create ($83DD);
      TPIC18x_DW_Packed.Create ($8399);
      TPIC18x_DW_Packed.Create ($8356);
      TPIC18x_DW_Packed.Create ($8312);
      TPIC18x_DW_Packed.Create ($82CF);
      TPIC18x_DW_Packed.Create ($828D);
      TPIC18x_DW_Packed.Create ($824A);
      TPIC18x_DW_Packed.Create ($8208);
      TPIC18x_DW_Packed.Create ($81C6);
      TPIC18x_DW_Packed.Create ($8185);
      TPIC18x_DW_Packed.Create ($8143);
      TPIC18x_DW_Packed.Create ($8102);
      TPIC18x_DW_Packed.Create ($80C1);
      TPIC18x_DW_Packed.Create ($8081);
      TPIC18x_DW_Packed.Create ($8040);
      TPIC18x_DW_Packed.Create ($8001);

      AGEB32.set_client_destinations;
      ALTB32.set_client_destinations;
      D32BNE0.set_client_destinations;
      DAOK32.set_client_destinations;
      DBOK32.set_client_destinations;
      DEXP32.set_client_destinations;
      DIV32OK.set_client_destinations;
      DROUND32.set_client_destinations;
      SETFDZ32.set_client_destinations;
      RES032.set_client_destinations;
      SETFOV32.set_client_destinations;
      SETFUN32.set_client_destinations;

      AGEB32.Free;
      ALTB32.Free;
      D32BNE0.Free;
      DAOK32.Free;
      DBOK32.Free;
      DEXP32.Free;
      DIV32OK.Free;
      DROUND32.Free;
      SETFDZ32.Free;
      RES032.Free;
      SETFOV32.Free;
      SETFUN32.Free;
   end;

procedure TFloatingPointDivide.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use(set_errorcode_routine, -fdiv_temp_stack_pop_count);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_m1_routine, -fdiv_temp_stack_pop_count);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_m2_routine, -fdiv_temp_stack_pop_count)
   end;

// =========================
//  TInt24ToFloatConversion

constructor TInt24ToFloatConversion.Create;
   begin
      inherited Create (flo2432_temp_stack_push_count, flo2432_temp_stack_pop_count, 'convert int24 to real')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TInt24ToFloatConversion.report_stack_sizes;
   begin
      check_stack_sizes (flo2432_temp_stack_push_count, flo2432_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TInt24ToFloatConversion.generate_subroutine_code;
   var
      FIXSIGN32,
      RES032,
      SETFUN32,
      NORM3232,
      NORM3232A,
      NORM3232B,
      NRM3232,
      NRM3232A,
      NRM3232B,
      TNIB3232,
      TNIB3232A,
      TNIB3232B,
      TNORMUN3232: TBranchTarget;
   begin    // generate_int24_to_flt_code
      FIXSIGN32 := TBranchTarget.Create;
      RES032 := TBranchTarget.Create;
      SETFUN32 := TBranchTarget.Create;
      NORM3232 := TBranchTarget.Create;
      NORM3232A := TBranchTarget.Create;
      NORM3232B := TBranchTarget.Create;
      NRM3232 := TBranchTarget.Create;
      NRM3232A := TBranchTarget.Create;
      NRM3232B := TBranchTarget.Create;
      TNIB3232 := TBranchTarget.Create;
      TNIB3232A := TBranchTarget.Create;
      TNIB3232B := TBranchTarget.Create;
      TNORMUN3232 := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, flo2432_temp_stack_push_count);
      TPIC18x_MOVLW.Create (23+EXPBIAS);
      TPIC18x_MOVWF.Create (flo2432_AEXP, access_mode);
      TPIC18x_MOVSS.Create (flo2432_AARGB0, flo2432_SIGN);
      TPIC18x_BTFSS.Create (flo2432_AARGB0, sign_bit, access_mode);
      NRM3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_COMF.Create (flo2432_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (flo2432_AARGB0, dest_f, access_mode);
      NRM3232.target_label := TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (flo2432_TEMP,access_mode);
      TPIC18x_CPFSGT.Create (flo2432_AARGB0, access_mode);
      NRM3232A.ComeFrom (TPIC18x_BRA.Create);
      TNIB3232.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (flo2432_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (flo2432_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (flo2432_AARGB2, access_mode);
      TPIC18x_BSF.Create (flo2432_TEMP, 2, access_mode);
      NORM3232.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TNORMUN3232.target_label := TPIC18x_MOVF.Create (flo2432_TEMP, dest_w, access_mode);
      TPIC18x_CPFSGT.Create (flo2432_AEXP, access_mode);
      SETFUN32.ComeFrom (TGOTOMacro.Create);        // is this possible???
      TPIC18x_SUBWF.Create (flo2432_AEXP, dest_f, access_mode);
      FIXSIGN32.ComeFrom (TPIC18x_BRA.Create);
      NRM3232A.target_label := TPIC18x_MOVSS.Create (flo2432_AARGB1, flo2432_AARGB0);
      TPIC18x_MOVSS.Create (flo2432_AARGB2, flo2432_AARGB1);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (flo2432_AARGB2,access_mode);
      TPIC18x_BSF.Create (flo2432_TEMP, 3, access_mode);
      TPIC18x_CPFSGT.Create (flo2432_AARGB0, access_mode);
      NRM3232B.ComeFrom (TPIC18x_BRA.Create);
      TNIB3232A.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (flo2432_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM3232A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (flo2432_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (flo2432_AARGB1, access_mode);
      TPIC18x_BSF.Create (flo2432_TEMP, 2, access_mode);
      NORM3232A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      NRM3232B.target_label := TPIC18x_MOVSS.Create (flo2432_AARGB1, flo2432_AARGB0);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (flo2432_AARGB1,access_mode);
      TPIC18x_BCF.Create (flo2432_TEMP, 3, access_mode);
      TPIC18x_BSF.Create (flo2432_TEMP, 4, access_mode);
      TPIC18x_CPFSGT.Create (flo2432_AARGB0, access_mode);
      RES032.ComeFrom (TPIC18x_BRA.Create);
      TNIB3232B.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (flo2432_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM3232B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_BSF.Create (flo2432_TEMP, 2, access_mode);
      NORM3232B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (flo2432_AARGB0, sign_bit, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (flo2432_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (flo2432_TEMP, dest_f, access_mode);
      TNORMUN3232.ComeFrom (TPIC18x_BRA.Create);
      FIXSIGN32.target_label := generate_fixsign32_code (flo2432_AARGB0, flo2432_SIGN, flo2432_temp_stack_pop_count);
      RES032.target_label := generate_res032_code (flo2432_AEXP, flo2432_AARGB0, flo2432_AARGB1, flo2432_AARGB2, -1, flo2432_temp_stack_pop_count);
      SETFUN32.target_label := generate_set_fun_code (flo2432_AEXP, flo2432_AARGB0, flo2432_AARGB1, flo2432_AARGB2, flo2432_SIGN, flo2432_temp_stack_pop_count);

      FIXSIGN32.set_client_destinations;
      RES032.set_client_destinations;
      SETFUN32.set_client_destinations;
      NORM3232.set_client_destinations;
      NORM3232A.set_client_destinations;
      NORM3232B.set_client_destinations;
      NRM3232.set_client_destinations;
      NRM3232A.set_client_destinations;
      NRM3232B.set_client_destinations;
      TNIB3232.set_client_destinations;
      TNIB3232A.set_client_destinations;
      TNIB3232B.set_client_destinations;
      TNORMUN3232.set_client_destinations;

      FIXSIGN32.Free;
      RES032.Free;
      SETFUN32.Free;
      NORM3232.Free;
      NORM3232A.Free;
      NORM3232B.Free;
      NRM3232.Free;
      NRM3232A.Free;
      NRM3232B.Free;
      TNIB3232.Free;
      TNIB3232A.Free;
      TNIB3232B.Free;
      TNORMUN3232.Free;
   end;

function TInt24ToFloatConversion.Call (src_loc: TSourceLocation): TInstruction;
   var lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do int24 to real conversion';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc)
   end;


// =========================
//  TInt32ToFloatConversion

constructor TInt32ToFloatConversion.Create;
   begin
      inherited Create (FLO3232_temp_stack_push_count, FLO3232_temp_stack_pop_count, 'convert int32 to real')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TInt32ToFloatConversion.report_stack_sizes;
   begin
      check_stack_sizes (FLO3232_temp_stack_push_count, FLO3232_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TInt32ToFloatConversion.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, FLO3232_temp_stack_push_count);
      TPIC18x_MOVSS.Create (norm40_AARGB2, norm40_AARGB3);
      TPIC18x_MOVSS.Create (norm40_AARGB1, norm40_AARGB2);
      TPIC18x_MOVSS.Create (norm40_AARGB0, norm40_AARGB1);
      TPIC18x_MOVSS.Create (norm40_EXP, norm40_AARGB0);
      TPIC18x_MOVLW.Create (31+EXPBIAS);
      TPIC18x_MOVWF.Create (norm40_EXP, access_mode);
      TPIC18x_MOVF.Create (norm40_AARGB0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (norm40_SIGN, access_mode);
      NRM4032.ComeFrom (TBranchOnNonNegativeStatusMacro.Create);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_COMF.Create (norm40_AARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_AARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB0, dest_f, access_mode);
      NRM4032.ComeFrom (TGOTOMacro.Create)
   end;

procedure TInt32ToFloatConversion.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (NRM4032, 0)
   end;


function TInt32ToFloatConversion.Call (src_loc: TSourceLocation): TInstruction;
   var lbl: TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'do int32 to real conversion';
      lbl := TAssemblyLabel.Create;
      RecordRunTimeErrorLocation (lbl, 0, rterr_floating_point_overflow, src_loc);
      RecordRunTimeErrorLocation (lbl, -1, rterr_floating_point_underflow, src_loc)
   end;

constructor TINT3224.Create;
   begin
      inherited Create (0, int3224_temp_stack_pop_count, 'convert real to int24')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TINT3224.report_stack_sizes;
   begin
      check_stack_sizes (0, int3224_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TINT3224.generate_subroutine_code;
   var
      INT3224OK,
      SETIOV3224,
      SHIFT3224,
      SHIFT3224A,
      SHIFT3224B,
      SHIFT3224C,
      SHIFT3224OK,
      SNIB3224,
      SNIB3224A,
      SNIB3224B,
      SNIB3224C: TBranchTarget;
   begin
      INT3224OK := TBranchTarget.Create;
      SETIOV3224 := TBranchTarget.Create;
      SHIFT3224 := TBranchTarget.Create;
      SHIFT3224A := TBranchTarget.Create;
      SHIFT3224B := TBranchTarget.Create;
      SHIFT3224C := TBranchTarget.Create;
      SHIFT3224OK := TBranchTarget.Create;
      SNIB3224 := TBranchTarget.Create;
      SNIB3224A := TBranchTarget.Create;
      SNIB3224B := TBranchTarget.Create;
      SNIB3224C := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (int3224_AARGB3,access_mode);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      TPIC18x_ADDULNK.Create (int3224_temp_stack_pop_count);
      TPIC18x_MOVSS.Create (int3224_AARGB0, int3224_SIGN);
      TPIC18x_BSF.Create (int3224_AARGB0, sign_bit, access_mode);
      TPIC18x_MOVLW.Create (EXPBIAS+23);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (WREG, sign_bit, access_mode);
      SETIOV3224.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVWF.Create (int3224_AEXP, access_mode);
      TPIC18x_NEGF.Create (int3224_AEXP, access_mode);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SNIB3224.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3224_AARGB2, int3224_AARGB3);
      TPIC18x_MOVSS.Create (int3224_AARGB1, int3224_AARGB2);
      TPIC18x_MOVSS.Create (int3224_AARGB0, int3224_AARGB1);
      TPIC18x_CLRF.Create (int3224_AARGB0,access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SNIB3224A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3224_AARGB2, int3224_AARGB3);
      TPIC18x_MOVSS.Create (int3224_AARGB1, int3224_AARGB2);
      TPIC18x_CLRF.Create (int3224_AARGB1,access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SNIB3224B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3224_AARGB2, int3224_AARGB3);
      TPIC18x_CLRF.Create (int3224_AARGB2,access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3224C.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SHIFT3224C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (int3224_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB3, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3224C.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3224B.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SHIFT3224B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3224_AARGB3, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3224B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3224A.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SHIFT3224A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3224_AARGB3, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3224A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3224.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3224_AEXP, access_mode);
      SHIFT3224.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3224_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3224_AARGB3, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3224_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3224_AARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3224.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3224_AEXP, dest_f, access_mode);
      SHIFT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB3, dest_f, access_mode);
      SHIFT3224OK.target_label := TPIC18x_BTFSC.Create (int3224_FPFLAGS, RND, access_mode);
      TPIC18x_BTFSS.Create (int3224_AARGB3, sign_bit, access_mode);
      INT3224OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BSF.Create (STATUS, status_c, access_mode);
      TPIC18x_MOVLW.Create (128);
      TPIC18x_CPFSGT.Create (int3224_AARGB3, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB2, dest_w, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_ADDWFC.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (int3224_AARGB0, sign_bit, access_mode);
      SETIOV3224.ComeFrom (TPIC18x_BRA.Create);
      INT3224OK.target_label := TPIC18x_BTFSS.Create (int3224_SIGN, sign_bit, access_mode);
      TPIC18x_ADDULNK.Create (int3224_temp_stack_pop_count);
      TPIC18x_COMF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (int3224_AARGB2, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_ADDWFC.Create (int3224_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_ADDULNK.Create (int3224_temp_stack_pop_count);
      SETIOV3224.target_label := TPIC18x_CLRF.Create (int3224_AARGB0,access_mode);
      TPIC18x_BTFSS.Create (int3224_SIGN, sign_bit, access_mode);
      TPIC18x_SETF.Create (int3224_AARGB0, access_mode);
      TPIC18x_MOVSS.Create (int3224_AARGB0, int3224_AARGB1);
      TPIC18x_MOVSS.Create (int3224_AARGB0, int3224_AARGB2);
      TPIC18x_RLCF.Create (int3224_SIGN, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3224_AARGB0, dest_f, access_mode);
      TPIC18x_ADDFSR.Create (2, int3224_temp_stack_pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);

      INT3224OK.set_client_destinations;
      SETIOV3224.set_client_destinations;
      SHIFT3224.set_client_destinations;
      SHIFT3224A.set_client_destinations;
      SHIFT3224B.set_client_destinations;
      SHIFT3224C.set_client_destinations;
      SHIFT3224OK.set_client_destinations;
      SNIB3224.set_client_destinations;
      SNIB3224A.set_client_destinations;
      SNIB3224B.set_client_destinations;
      SNIB3224C.set_client_destinations;

      INT3224OK.Free;
      SETIOV3224.Free;
      SHIFT3224.Free;
      SHIFT3224A.Free;
      SHIFT3224B.Free;
      SHIFT3224C.Free;
      SHIFT3224OK.Free;
      SNIB3224.Free;
      SNIB3224A.Free;
      SNIB3224B.Free;
      SNIB3224C.Free;
   end;

procedure TINT3224.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -int3224_temp_stack_pop_count)
   end;


constructor TINT3232.Create;
   begin
      inherited Create (0, int3232_temp_stack_pop_count, 'convert real to int32')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TINT3232.report_stack_sizes;
   begin
      check_stack_sizes (0, int3232_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TINT3232.generate_subroutine_code;
   var
      INT3232OK,
      SETIOV3232,
      SHIFT3232,
      SHIFT3232A,
      SHIFT3232B,
      SHIFT3232C,
      SHIFT3232D,
      SHIFT3232OK,
      SNIB3232,
      SNIB3232A,
      SNIB3232B,
      SNIB3232C,
      SNIB3232D,
      exit_ok: TBranchTarget;
   begin
      INT3232OK := TBranchTarget.Create;
      SETIOV3232 := TBranchTarget.Create;
      SHIFT3232 := TBranchTarget.Create;
      SHIFT3232A := TBranchTarget.Create;
      SHIFT3232B := TBranchTarget.Create;
      SHIFT3232C := TBranchTarget.Create;
      SHIFT3232D := TBranchTarget.Create;
      SHIFT3232OK := TBranchTarget.Create;
      SNIB3232 := TBranchTarget.Create;
      SNIB3232A := TBranchTarget.Create;
      SNIB3232B := TBranchTarget.Create;
      SNIB3232C := TBranchTarget.Create;
      SNIB3232D := TBranchTarget.Create;
      exit_ok := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (int3232_AARGB3,access_mode);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      exit_ok.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_SIGN);
      TPIC18x_BSF.Create (int3232_AARGB0, sign_bit, access_mode);
      TPIC18x_CLRF.Create (int3232_AARGB4,access_mode);
      TPIC18x_MOVLW.Create (EXPBIAS+31);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (WREG, sign_bit, access_mode);
      SETIOV3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_MOVWF.Create (int3232_AEXP, access_mode);
      TPIC18x_NEGF.Create (int3232_AEXP, access_mode);
      TPIC18x_MOVLW.Create (7);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SNIB3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3232_AARGB3, int3232_AARGB4);
      TPIC18x_MOVSS.Create (int3232_AARGB2, int3232_AARGB3);
      TPIC18x_MOVSS.Create (int3232_AARGB1, int3232_AARGB2);
      TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_AARGB1);
      TPIC18x_CLRF.Create (int3232_AARGB0,access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SNIB3232A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3232_AARGB3, int3232_AARGB4);
      TPIC18x_MOVSS.Create (int3232_AARGB2, int3232_AARGB3);
      TPIC18x_MOVSS.Create (int3232_AARGB1, int3232_AARGB2);
      TPIC18x_CLRF.Create (int3232_AARGB1,access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SNIB3232B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3232_AARGB3, int3232_AARGB4);
      TPIC18x_MOVSS.Create (int3232_AARGB2, int3232_AARGB3);
      TPIC18x_CLRF.Create (int3232_AARGB2,access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SNIB3232C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_MOVSS.Create (int3232_AARGB3, int3232_AARGB4);
      TPIC18x_CLRF.Create (int3232_AARGB3,access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3232D.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SHIFT3232D.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (int3232_AARGB4, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB4, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3232D.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3232C.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SHIFT3232C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB3, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3232_AARGB4, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB3, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3232C.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3232B.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SHIFT3232B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB3, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3232_AARGB4, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB2, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3232B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3232A.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SHIFT3232A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB3, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3232_AARGB4, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB1, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3232A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SNIB3232.target_label := TPIC18x_MOVLW.Create (3);
      TPIC18x_CPFSGT.Create (int3232_AEXP, access_mode);
      SHIFT3232.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SUBWF.Create (int3232_AEXP, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB3, dest_w, access_mode);
      TPIC18x_MOVWF.Create (int3232_AARGB4, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB3, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_ADDWF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (int3232_AARGB0, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_MOVWF.Create (int3232_AARGB0, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      SHIFT3232.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      TPIC18x_DCFSNZ.Create (int3232_AEXP, dest_f, access_mode);
      SHIFT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB4, dest_f, access_mode);
      SHIFT3232OK.target_label := TPIC18x_BTFSC.Create (int3232_FPFLAGS, RND, access_mode);
      TPIC18x_BTFSS.Create (int3232_AARGB4, sign_bit, access_mode);
      INT3232OK.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BSF.Create (STATUS, status_c, access_mode);
      TPIC18x_MOVLW.Create (128);
      TPIC18x_CPFSGT.Create (int3232_AARGB4, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB3, dest_w, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSC.Create (int3232_AARGB0, sign_bit, access_mode);
      SETIOV3232.ComeFrom (TPIC18x_BRA.Create);
      INT3232OK.target_label := TPIC18x_BTFSS.Create (int3232_SIGN, sign_bit, access_mode);
      exit_ok.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_COMF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_COMF.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_COMF.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_COMF.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (int3232_AARGB3, dest_f, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (int3232_AARGB0, dest_f, access_mode);
      exit_ok.ComeFrom (TPIC18x_BRA.Create);
      SETIOV3232.target_label := TPIC18x_CLRF.Create (int3232_AARGB0,access_mode);
      TPIC18x_BTFSS.Create (int3232_SIGN, sign_bit, access_mode);
      TPIC18x_SETF.Create (int3232_AARGB0, access_mode);
      TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_AARGB1);
      TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_AARGB2);
      TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_AARGB3);
      TPIC18x_RLCF.Create (int3232_SIGN, dest_f, access_mode);
      TPIC18x_RRCF.Create (int3232_AARGB0, dest_f, access_mode);
      TPIC18x_ADDFSR.Create (2, int3232_temp_stack_pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);
      exit_ok.target_label := TPIC18x_MOVSS.Create (int3232_AARGB0, int3232_AEXP);
      TPIC18x_MOVSS.Create (int3232_AARGB1, int3232_AARGB0);
      TPIC18x_MOVSS.Create (int3232_AARGB2, int3232_AARGB1);
      TPIC18x_MOVSS.Create (int3232_AARGB3, int3232_AARGB2);
      TPIC18x_ADDULNK.Create (int3232_temp_stack_pop_count);

      INT3232OK.set_client_destinations;
      SETIOV3232.set_client_destinations;
      SHIFT3232.set_client_destinations;
      SHIFT3232A.set_client_destinations;
      SHIFT3232B.set_client_destinations;
      SHIFT3232C.set_client_destinations;
      SHIFT3232D.set_client_destinations;
      SHIFT3232OK.set_client_destinations;
      SNIB3232.set_client_destinations;
      SNIB3232A.set_client_destinations;
      SNIB3232B.set_client_destinations;
      SNIB3232C.set_client_destinations;
      SNIB3232D.set_client_destinations;
      exit_ok.set_client_destinations;

      INT3232OK.Free;
      SETIOV3232.Free;
      SHIFT3232.Free;
      SHIFT3232A.Free;
      SHIFT3232B.Free;
      SHIFT3232C.Free;
      SHIFT3232D.Free;
      SHIFT3232OK.Free;
      SNIB3232.Free;
      SNIB3232A.Free;
      SNIB3232B.Free;
      SNIB3232C.Free;
      SNIB3232D.Free;
      exit_ok.Free
   end;

procedure TINT3232.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -int3232_temp_stack_pop_count)
   end;

constructor TNRM4032.Create;
   begin
      inherited Create (0, norm40_temp_stack_pop_count, 'normalize 40 bits')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TNRM4032.report_stack_sizes;
   begin
      check_stack_sizes (0, norm40_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TNRM4032.generate_subroutine_code;
   var
      FIXSIGN32,
      RES032,
      SETFUN32,
      SETFOV32,
      NORM4032,
      NORM4032A,
      NORM4032B,
      NORM4032C,
      NRM4032A,
      NRM4032B,
      NRM4032C,
      TNIB4032,
      TNIB4032A,
      TNIB4032B,
      TNIB4032C,
      TNORMUN4032: TBranchTarget;
   begin  // generate_norm40_code
      FIXSIGN32 := TBranchTarget.Create;
      RES032 := TBranchTarget.Create;
      SETFUN32 := TBranchTarget.Create;
      SETFOV32 := TBranchTarget.Create;
      NORM4032 := TBranchTarget.Create;
      NORM4032A := TBranchTarget.Create;
      NORM4032B := TBranchTarget.Create;
      NORM4032C := TBranchTarget.Create;
      NRM4032A := TBranchTarget.Create;
      NRM4032B := TBranchTarget.Create;
      NRM4032C := TBranchTarget.Create;
      TNIB4032 := TBranchTarget.Create;
      TNIB4032A := TBranchTarget.Create;
      TNIB4032B := TBranchTarget.Create;
      TNIB4032C := TBranchTarget.Create;
      TNORMUN4032 := TBranchTarget.Create;

      TPIC18x_CLRF.Create (norm40_TEMP, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CPFSGT.Create (norm40_AARGB0, access_mode);
      NRM4032A.ComeFrom (TPIC18x_BRA.Create);
      TNIB4032.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (norm40_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB2, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB3, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB3, access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 2, access_mode);
      NORM4032.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB3, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TNORMUN4032.target_label := TPIC18x_MOVF.Create (norm40_TEMP, dest_w, access_mode);
      TPIC18x_CPFSGT.Create (norm40_EXP, access_mode);
      SETFUN32.ComeFrom (TGOTOMacro.Create);
      TPIC18x_SUBWF.Create (norm40_EXP, dest_f, access_mode);
      NRMRND4032.set_target_label (TPIC18x_BTFSS.Create (norm40_AARGB3, sign_bit, access_mode));
      NRMRND4032.set_client_destinations;
      FIXSIGN32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_BSF.Create (STATUS, status_c, access_mode);
      TPIC18x_MOVLW.Create (128);
      TPIC18x_CPFSGT.Create (norm40_AARGB3, access_mode);
      TPIC18x_RRCF.Create (norm40_AARGB2, dest_w, access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_ADDWFC.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      FIXSIGN32.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RRCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_RRCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RRCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_INFSNZ.Create (norm40_EXP, dest_f, access_mode);
      SETFOV32.ComeFrom (TPIC18x_BRA.Create);
      FIXSIGN32.ComeFrom (TPIC18x_BRA.Create);
      NRM4032A.target_label := TPIC18x_MOVSS.Create (norm40_AARGB1, norm40_AARGB0);
      TPIC18x_MOVSS.Create (norm40_AARGB2, norm40_AARGB1);
      TPIC18x_MOVSS.Create (norm40_AARGB3, norm40_AARGB2);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_CLRF.Create (norm40_AARGB3,access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 3, access_mode);
      TPIC18x_CPFSGT.Create (norm40_AARGB0, access_mode);
      NRM4032B.ComeFrom (TPIC18x_BRA.Create);
      TNIB4032A.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (norm40_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM4032A.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB1, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB2, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB2, access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 2, access_mode);
      NORM4032A.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB2, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      NRM4032B.target_label := TPIC18x_MOVSS.Create (norm40_AARGB1, norm40_AARGB0);
      TPIC18x_MOVSS.Create (norm40_AARGB2, norm40_AARGB1);
      TPIC18x_CLRF.Create (norm40_AARGB2,access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_BCF.Create (norm40_TEMP, 3, access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 4, access_mode);
      TPIC18x_CPFSGT.Create (norm40_AARGB0, access_mode);
      NRM4032C.ComeFrom (TPIC18x_BRA.Create);
      TNIB4032B.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (norm40_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM4032B.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (15);
      TPIC18x_ADDWF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_SWAPF.Create (norm40_AARGB1, dest_w, access_mode);
      TPIC18x_ANDLW.Create (240);
      TPIC18x_MOVWF.Create (norm40_AARGB1, access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 2, access_mode);
      NORM4032B.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB1, dest_f, access_mode);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      NRM4032C.target_label := TPIC18x_MOVSS.Create (norm40_AARGB1, norm40_AARGB0);
      TPIC18x_CLRF.Create (norm40_AARGB1,access_mode);
      TPIC18x_CLRF.Create (WREG,access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 3, access_mode);
      TPIC18x_CPFSGT.Create (norm40_AARGB0, access_mode);
      RES032.ComeFrom (TPIC18x_BRA.Create);
      TNIB4032C.target_label := TPIC18x_MOVLW.Create (240);
      TPIC18x_ANDWF.Create (norm40_AARGB0, dest_w, access_mode);
      TPIC18x_TSTFSZ.Create (WREG, access_mode);
      NORM4032C.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_SWAPF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_BSF.Create (norm40_TEMP, 2, access_mode);
      NORM4032C.target_label := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TPIC18x_BTFSC.Create (norm40_AARGB0, sign_bit, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);
      TPIC18x_RLCF.Create (norm40_AARGB0, dest_f, access_mode);
      TPIC18x_INCF.Create (norm40_TEMP, dest_f, access_mode);
      TNORMUN4032.ComeFrom (TPIC18x_BRA.Create);

      FIXSIGN32.target_label := generate_fixsign32_code (norm40_AARGB0, norm40_SIGN, norm40_temp_stack_pop_count);
      RES032.target_label := generate_res032_code (norm40_EXP, norm40_AARGB0, norm40_AARGB1, norm40_AARGB2, -1, norm40_temp_stack_pop_count);
      SETFUN32.target_label := generate_set_fun_code (norm40_EXP, norm40_AARGB0, norm40_AARGB1, norm40_AARGB2, norm40_SIGN, norm40_temp_stack_pop_count);
      SETFOV32.target_label := generate_set_ov_code (norm40_EXP, norm40_AARGB0, norm40_AARGB1, norm40_AARGB2, norm40_SIGN, norm40_temp_stack_pop_count);

      FIXSIGN32.set_client_destinations;
      RES032.set_client_destinations;
      SETFUN32.set_client_destinations;
      SETFOV32.set_client_destinations;
      NORM4032.set_client_destinations;
      NORM4032A.set_client_destinations;
      NORM4032B.set_client_destinations;
      NORM4032C.set_client_destinations;
      NRM4032A.set_client_destinations;
      NRM4032B.set_client_destinations;
      NRM4032C.set_client_destinations;
      TNIB4032.set_client_destinations;
      TNIB4032A.set_client_destinations;
      TNIB4032B.set_client_destinations;
      TNIB4032C.set_client_destinations;
      TNORMUN4032.set_client_destinations;

      FIXSIGN32.Free;
      RES032.Free;
      SETFUN32.Free;
      SETFOV32.Free;
      NORM4032.Free;
      NORM4032A.Free;
      NORM4032B.Free;
      NORM4032C.Free;
      NRM4032A.Free;
      NRM4032B.Free;
      NRM4032C.Free;
      TNIB4032.Free;
      TNIB4032A.Free;
      TNIB4032B.Free;
      TNIB4032C.Free;
      TNORMUN4032.Free
   end;  // generate_norm40_code

constructor TNRMRND4032.Create;
   begin
      inherited Create (0, norm40_temp_stack_pop_count, 'convert real to int24')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TNRMRND4032.report_stack_sizes;
   begin
      check_stack_sizes (0, norm40_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TNRMRND4032.generate_subroutine_code;
   begin
      // tricky - handled inside TNRM4032
   end;

constructor Tset_true_and_return.Create;
   begin
      inherited Create (0, fcmp_temp_stack_pop_count, 'routine to set true and return')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tset_true_and_return.report_stack_sizes;
   begin
      check_stack_sizes (0, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure Tset_true_and_return.generate_subroutine_code;
   begin
      TPIC18x_MOVLW.Create (1);
      TPIC18x_MOVWF.Create (fcmp_bool_result, access_mode);
      TPIC18x_ADDULNK.Create (fcmp_temp_stack_pop_count)
   end;

constructor Tset_false_and_return.Create;
   begin
      inherited Create (0, fcmp_temp_stack_pop_count, 'routine to set false and return')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tset_false_and_return.report_stack_sizes;
   begin
      check_stack_sizes (0, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure Tset_false_and_return.generate_subroutine_code;
   begin
      TPIC18x_CLRF.Create (fcmp_bool_result, access_mode);
      TPIC18x_ADDULNK.Create (fcmp_temp_stack_pop_count)
   end;


// ================================
//  TFloatingPointEqualsComparison

constructor TFloatingPointEqualsComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real equal comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointEqualsComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointEqualsComparison.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BEXP, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB0, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB1, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB2, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      set_true_and_return.ComeFrom (TGOTOMacro.Create)
   end;

procedure TFloatingPointEqualsComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// ===================================
//  TFloatingPointNotEqualsComparison

constructor TFloatingPointNotEqualsComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real not equal comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointNotEqualsComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointNotEqualsComparison.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BEXP, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB0, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB1, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_CPFSEQ.Create (fcmp_BARGB2, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create)
   end;

procedure TFloatingPointNotEqualsComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// ==================================
//  TFloatingPointLessThanComparison

constructor TFloatingPointLessThanComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real less than comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointLessThanComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointLessThanComparison.generate_subroutine_code;
   var
      TALTB32N,
      TALTB32O,
      TALTB32P: TBranchTarget;
   begin
      TALTB32N := TBranchTarget.Create;
      TALTB32O := TBranchTarget.Create;
      TALTB32P := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSC.Create (WREG, sign_bit, access_mode);
      TALTB32O.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSC.Create (fcmp_AARGB0, sign_bit, access_mode);
      TALTB32N.ComeFrom (TGOTOMacro.Create);
      TALTB32P.target_label := TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TALTB32N.target_label := TPIC18x_MOVF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TALTB32O.target_label := TPIC18x_BTFSS.Create (fcmp_BARGB0, sign_bit, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);

      TALTB32N.set_client_destinations;
      TALTB32O.set_client_destinations;
      TALTB32P.set_client_destinations;

      TALTB32N.Free;
      TALTB32O.Free;
      TALTB32P.Free
   end;

procedure TFloatingPointLessThanComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// ==========================================
//  TFloatingPointLessThanOrEqualsComparison

constructor TFloatingPointLessThanOrEqualsComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real less than or equal to comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointLessThanOrEqualsComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointLessThanOrEqualsComparison.generate_subroutine_code;
   var
      TALEB32N,
      TALEB32O,
      TALEB32P: TBranchTarget;
   begin
      TALEB32N := TBranchTarget.Create;
      TALEB32O := TBranchTarget.Create;
      TALEB32P := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSC.Create (WREG, sign_bit, access_mode);
      TALEB32O.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSC.Create (fcmp_AARGB0, sign_bit, access_mode);
      TALEB32N.ComeFrom (TGOTOMacro.Create);
      TALEB32P.target_label := TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TALEB32N.target_label := TPIC18x_MOVF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TALEB32O.target_label := TPIC18x_BTFSS.Create (fcmp_BARGB0, sign_bit, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);

      TALEB32N.set_client_destinations;
      TALEB32O.set_client_destinations;
      TALEB32P.set_client_destinations;

      TALEB32N.Free;
      TALEB32O.Free;
      TALEB32P.Free;
   end;

procedure TFloatingPointLessThanOrEqualsComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// =====================================
//  TFloatingPointGreaterThanComparison

constructor TFloatingPointGreaterThanComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real greater than comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointGreaterThanComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointGreaterThanComparison.generate_subroutine_code;
   var
      TAGTB32N,
      TAGTB32O,
      TAGTB32P: TBranchTarget;
   begin
      TAGTB32N := TBranchTarget.Create;
      TAGTB32O := TBranchTarget.Create;
      TAGTB32P := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSC.Create (WREG, sign_bit, access_mode);
      TAGTB32O.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSC.Create (fcmp_BARGB0, sign_bit, access_mode);
      TAGTB32N.ComeFrom (TGOTOMacro.Create);
      TAGTB32P.target_label := TPIC18x_MOVF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TAGTB32N.target_label := TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TAGTB32O.target_label := TPIC18x_BTFSS.Create (fcmp_AARGB0, sign_bit, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);

      TAGTB32N.set_client_destinations;
      TAGTB32O.set_client_destinations;
      TAGTB32P.set_client_destinations;

      TAGTB32N.Free;
      TAGTB32O.Free;
      TAGTB32P.Free
   end;

procedure TFloatingPointGreaterThanComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// =============================================
//  TFloatingPointGreaterThanOrEqualsComparison

constructor TFloatingPointGreaterThanOrEqualsComparison.Create;
   begin
      inherited Create (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 'subroutine for real greater than or equal comparisons')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointGreaterThanOrEqualsComparison.report_stack_sizes;
   begin
      check_stack_sizes (fcmp_temp_stack_push_count, fcmp_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointGreaterThanOrEqualsComparison.generate_subroutine_code;
   var
      TAGEB32N,
      TAGEB32O,
      TAGEB32P: TBranchTarget;
   begin
      TAGEB32N := TBranchTarget.Create;
      TAGEB32O := TBranchTarget.Create;
      TAGEB32P := TBranchTarget.Create;

      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an660_attribution);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_XORWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSC.Create (WREG, sign_bit, access_mode);
      TAGEB32O.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSC.Create (fcmp_BARGB0, sign_bit, access_mode);
      TAGEB32N.ComeFrom (TGOTOMacro.Create);
      TAGEB32P.target_label := TPIC18x_MOVF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TAGEB32N.target_label := TPIC18x_MOVF.Create (fcmp_AEXP, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BEXP, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB0, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB1, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TPIC18x_MOVF.Create (fcmp_AARGB2, dest_w, access_mode);
      TPIC18x_SUBWF.Create (fcmp_BARGB2, dest_w, access_mode);
      TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      TAGEB32O.target_label :=  TPIC18x_BTFSS.Create (fcmp_AARGB0, sign_bit, access_mode);
      set_true_and_return.ComeFrom (TGOTOMacro.Create);
      set_false_and_return.ComeFrom (TGOTOMacro.Create);

      TAGEB32N.set_client_destinations;
      TAGEB32O.set_client_destinations;
      TAGEB32P.set_client_destinations;

      TAGEB32N.Free;
      TAGEB32O.Free;
      TAGEB32P.Free
   end;

procedure TFloatingPointGreaterThanOrEqualsComparison.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_true_and_return, 0);
      evaluate_gotoed_subroutine_stack_use (set_false_and_return, 0)
   end;


// ===============================
//  TFloatingPointRound24Function

constructor TFloatingPointRound24Function.Create;
   begin
      inherited Create (int3224_temp_stack_push_count, int3224_temp_stack_pop_count, 'round real to int24')
   end;

function TFloatingPointRound24Function.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'round real to int24';
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_integer_overflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointRound24Function.report_stack_sizes;
   begin
      check_stack_sizes (int3224_temp_stack_push_count, int3224_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointRound24Function.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, int3224_temp_stack_push_count);
      TPIC18x_BSF.Create (int3224_FPFLAGS, RND, access_mode);
      INT3224.ComeFrom (TGOTOMacro.Create)
   end;


// ===============================
//  TFloatingPointRound32Function

constructor TFloatingPointRound32Function.Create;
   begin
      inherited Create (int3232_temp_stack_push_count, int3232_temp_stack_pop_count, 'round real to int32')
   end;

function TFloatingPointRound32Function.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'round real to int32';
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_integer_overflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointRound32Function.report_stack_sizes;
   begin
      check_stack_sizes (int3232_temp_stack_push_count, int3232_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointRound32Function.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, int3232_temp_stack_push_count);
      TPIC18x_BSF.Create (int3232_FPFLAGS, RND, access_mode);
      INT3232.ComeFrom (TGOTOMacro.Create)
   end;


// ===============================
//  TFloatingPointTrunc24Function

constructor TFloatingPointTrunc24Function.Create;
   begin
      inherited Create (int3224_temp_stack_push_count, int3224_temp_stack_pop_count, 'trunc real to int24')
   end;

function TFloatingPointTrunc24Function.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'trunc real to int24';
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_integer_overflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointTrunc24Function.report_stack_sizes;
   begin
      check_stack_sizes (int3224_temp_stack_push_count, int3224_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointTrunc24Function.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, int3224_temp_stack_push_count);
      TPIC18x_BCF.Create (int3224_FPFLAGS, RND, access_mode);
      INT3224.ComeFrom (TGOTOMacro.Create)
   end;

procedure TFloatingPointTrunc24Function.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (INT3224, -int3224_temp_stack_push_count)
   end;


// ===============================
//  TFloatingPointTrunc32Function

constructor TFloatingPointTrunc32Function.Create;
   begin
      inherited Create (int3232_temp_stack_push_count, int3232_temp_stack_pop_count, 'trunc real to int32')
   end;

function TFloatingPointTrunc32Function.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      result.annotation := 'trunc real to int32';
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_integer_overflow, src_loc)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TFloatingPointTrunc32Function.report_stack_sizes;
   begin
      check_stack_sizes (int3232_temp_stack_push_count, int3232_temp_stack_pop_count, 1)
   end;
{$endif}

procedure TFloatingPointTrunc32Function.generate_subroutine_code;
   begin
      TAssemblyComment.Create (attribution_header);
      TAssemblyComment.Create (an575_attribution);
      TPIC18x_SUBFSR.Create (2, int3232_temp_stack_push_count);
      TPIC18x_BCF.Create (int3232_FPFLAGS, RND, access_mode);
      INT3232.ComeFrom (TGOTOMacro.Create)
   end;


INITIALIZATION
   FPA32 := TFloatingPointAdd.Create;
   FPS32 := TFloatingPointSubtract.Create;
   FPM32 := TFloatingPointMultiply.Create;
   FPD32 := TFloatingPointDivide.Create;
   FLO3232 := TInt32ToFloatConversion.Create;
   FLO2432 := TInt24ToFloatConversion.Create;
   TALTB32 := TFloatingPointLessThanComparison.Create;
   TALEB32 := TFloatingPointLessThanOrEqualsComparison.Create;
   TAGTB32 := TFloatingPointGreaterThanComparison.Create;
   TAGEB32 := TFloatingPointGreaterThanOrEqualsComparison.Create;
   TAEQB32 := TFloatingPointEqualsComparison.Create;
   TANEB32 := TFloatingPointNotEqualsComparison.Create;
   FPRound24 := TFloatingPointRound24Function.Create;
   FPRound32 := TFloatingPointRound32Function.Create;
   FPTrunc24 := TFloatingPointTrunc24Function.Create;
   FPTrunc32 := TFloatingPointTrunc32Function.Create;
   INT3224 := TINT3224.Create;
   INT3232 := TINT3232.Create;
   NRM4032 := TNRM4032.Create;
   NRMRND4032 := TNRMRND4032.Create;
   set_true_and_return := Tset_true_and_return.Create;
   set_false_and_return := Tset_false_and_return.Create;

FINALIZATION
   FPA32.Free;
   FPS32.Free;
   FPM32.Free;
   FPD32.Free;
   FLO3232.Free;
   FLO2432.Free;
   TALTB32.Free;
   TALEB32.Free;
   TAGTB32.Free;
   TAGEB32.Free;
   TAEQB32.Free;
   TANEB32.Free;
   FPRound24.Free;
   FPRound32.Free;
   FPTrunc24.Free;
   FPTrunc32.Free;
   INT3224.Free;
   INT3232.Free;
   NRM4032.Free;
   NRMRND4032.Free;
   set_true_and_return.Free;
   set_false_and_return.Free;

END.
