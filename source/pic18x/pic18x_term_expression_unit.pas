UNIT pic18x_term_expression_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses cpc_term_expression_unit;

type
   TPIC18x_Term =
      class (TTerm)
         function Generate (param1, param2: integer): integer;
            override;
      end;

IMPLEMENTATION

uses
   pic18x_multiply_divide_unit, pic18x_cpu_unit, pic18x_instructions_unit, pic18x_core_objects_unit,
   cpc_core_objects_unit, pic18x_microprocessor_information_unit, SysUtils, pic18x_expressions_unit,
   cpc_multi_precision_integer_unit, pic18x_macro_instructions_unit, pic18x_run_time_error_check_unit,
   pic18x_floating_point_unit, pic18x_access_unit;

var
   temp: TMultiPrecisionInteger;

type
   // the following gives an array of a,b,result info for N operators where a[n] info is same as result[n-1] info
   TOperationSchedule =
      class
         public type
            TScheduleRecord =
               class
                  private
                     idx: integer;
                     op_sched: TOperationSchedule;
                     function get_a_integer_info: TIntegerInfo;
                     procedure set_a_integer_info (info: TIntegerInfo);
                     function get_a_size: Integer;
                     procedure set_a_size (size: integer);
                     function get_b_size: Integer;
                     function get_result_size: Integer;
                     procedure set_result_size (size: integer);
                  public
                     a_info, b_info: TPIC18x_TypeInfo;
                     result_integer_info: TIntegerInfo;
                     property a_integer_info: TIntegerInfo read get_a_integer_info write set_a_integer_info;
                     property a_size: integer read get_a_size write set_a_size;
                     property b_size: integer read get_b_size;
                     property result_size: integer read get_result_size write set_result_size;
                     constructor Create (_idx: integer; _op_sched: TOperationSchedule);
                     destructor Destroy;
                        override;
               end;
         private
            first_a: TIntegerInfo;
            fschedule: array of TScheduleRecord;
            function read_schedule (idx: integer): TScheduleRecord;
         public
            property schedule [idx: integer]: TScheduleRecord read read_schedule; default;
            constructor Create (len: integer);
            destructor Destroy;
               override;
      end;

constructor TOperationSchedule.TScheduleRecord.Create (_idx: integer; _op_sched: TOperationSchedule);
   begin
      idx := _idx;
      op_sched := _op_sched;
      result_integer_info := TIntegerInfo.Create
   end;

destructor TOperationSchedule.TScheduleRecord.Destroy;
   begin
      a_info.Release;
      b_info.Release;
      result_integer_info.Release;
      inherited
   end;

function TOperationSchedule.TScheduleRecord.get_a_integer_info: TIntegerInfo;
   begin
      if idx = 0 then
         result := op_sched.first_a
      else
         result := op_sched.fschedule[idx-1].result_integer_info
   end;

procedure TOperationSchedule.TScheduleRecord.set_a_integer_info (info: TIntegerInfo);
   begin
      if idx = 0 then
         op_sched.first_a := info
      else
         op_sched.fschedule[idx-1].result_integer_info := info
   end;

function TOperationSchedule.TScheduleRecord.get_a_size: Integer;
   begin
      if idx = 0 then
         result := op_sched.first_a.size
      else
         result := op_sched.fschedule[idx-1].result_integer_info.size
   end;

procedure TOperationSchedule.TScheduleRecord.set_a_size (size: integer);
   begin
      if idx = 0 then
         op_sched.first_a.size := size
      else
         op_sched.fschedule[idx-1].result_integer_info.size := size
   end;

function TOperationSchedule.TScheduleRecord.get_b_size: Integer;
   begin
      result := b_info.size
   end;

function TOperationSchedule.TScheduleRecord.get_result_size: Integer;
   begin
      result := result_integer_info.size
   end;

procedure TOperationSchedule.TScheduleRecord.set_result_size (size: integer);
   begin
      result_integer_info.size := size
   end;

constructor TOperationSchedule.Create (len: integer);
   var
      i: integer;
   begin
      first_a := TIntegerInfo.Create;
      SetLength (fschedule, len);
      for i := 0 to len-1 do
         fschedule[i] := TScheduleRecord.Create (i, Self)
   end;

function TOperationSchedule.read_schedule (idx: integer): TScheduleRecord;
   begin
      result := fschedule[idx]
   end;

destructor TOperationSchedule.Destroy;
   var i: integer;
   begin
      first_a.Free;
      for i := 0 to Length(fschedule)-1 do
         fschedule[i].Free;
      inherited
   end;

function TPIC18x_Term.Generate (param1, param2: integer): integer;

   procedure generate_numeric_term_code;
      var
         idx, i, j, addr: integer;
         lbl: TInstruction;
         annotation: string;
         intermediate_calculation_info: TOperationSchedule;
      begin
         intermediate_calculation_info := TOperationSchedule.Create (Length(additional_factors));

         case additional_factors[0].mulop of
            mulop_mult_int_by_int,
            mulop_integer_mod,
            mulop_shl,
            mulop_integer_div,
            mulop_shr,
            mulop_mask,
            mulop_mult_int_by_flt,
            mulop_divide_int_by_flt:
               intermediate_calculation_info[0].a_integer_info.init (TPIC18x_TypeInfo(first_factor.info));
            mulop_mult_flt_by_flt,
            mulop_divide_flt_by_flt:
               ;
         else
            assert (false)
         end;

         for idx := 0 to Length(additional_factors)-1 do
            case additional_factors[idx].mulop of
               mulop_mult_int_by_int,
               mulop_integer_mod,
               mulop_shl:
                  begin
                     intermediate_calculation_info[idx].b_info := TPIC18x_TypeInfo(additional_factors[idx].factor.info);
                     intermediate_calculation_info[idx].b_info.AddRef;
                     intermediate_calculation_info[idx].result_integer_info.init (TPIC18x_TypeInfo(additional_factors[idx].result_info));
                  end;
               mulop_integer_div:
                  begin
                     intermediate_calculation_info[idx].b_info := TPIC18x_TypeInfo(additional_factors[idx].factor.info);
                     intermediate_calculation_info[idx].b_info.AddRef;
                     intermediate_calculation_info[idx].result_integer_info.init (TPIC18x_TypeInfo(additional_factors[idx].result_info));
                     intermediate_calculation_info[idx].a_size := intermediate_calculation_info[idx].result_size
                        // allow for case of eg. int8 value of -128 div -1 => 128 which requires 2 byte result
                  end;
               mulop_shr:
                  begin
                     intermediate_calculation_info[idx].b_info := TPIC18x_TypeInfo(additional_factors[idx].factor.info);
                     intermediate_calculation_info[idx].b_info.AddRef;
                     intermediate_calculation_info[idx].result_integer_info.init (TPIC18x_TypeInfo(additional_factors[idx].result_info));
                     intermediate_calculation_info[idx].result_size := intermediate_calculation_info[idx].a_size
                  end;
               mulop_mask:
                  begin
                     intermediate_calculation_info[idx].b_info := TPIC18x_TypeInfo(additional_factors[idx].factor.info);
                     intermediate_calculation_info[idx].b_info.AddRef;
                     intermediate_calculation_info[idx].result_integer_info.init (TPIC18x_TypeInfo(additional_factors[idx].result_info));
                     intermediate_calculation_info[idx].a_size := intermediate_calculation_info[idx].result_size
                  end;
               mulop_mult_flt_by_flt,
               mulop_divide_flt_by_flt:
                  ;
               mulop_mult_int_by_flt,
               mulop_divide_int_by_flt:
                  begin
                     if idx = 0 then
                        intermediate_calculation_info[idx].a_info := TPIC18x_TypeInfo(first_factor.info)
                     else
                        intermediate_calculation_info[idx].a_info := TPIC18x_TypeInfo(additional_factors[idx-1].result_info);
                     intermediate_calculation_info[idx].a_info.AddRef
                  end;
            else
               assert (false)
            end;

         case additional_factors[0].mulop of
            mulop_mult_int_by_int,
            mulop_real_divide,
            mulop_integer_div,
            mulop_integer_mod,
            mulop_mask,
            mulop_shl,
            mulop_shr:
               first_factor.Generate (GenerateCode, intermediate_calculation_info[0].a_size);
            mulop_mult_flt_by_flt,
            mulop_divide_flt_by_flt:
               PushRealExpression (first_factor);
            mulop_mult_int_by_flt:
               begin
                  PushRealExpression (first_factor);
                  additional_factors[0].mulop := mulop_mult_flt_by_flt
               end;
            mulop_divide_int_by_flt:
               begin
                  PushRealExpression (first_factor);
                  additional_factors[0].mulop := mulop_divide_flt_by_flt
               end;
         else
            assert (false)
         end;

         for idx := 0 to Length(additional_factors)-1 do
            begin
               case additional_factors[idx].mulop of
                  mulop_mult_int_by_int,
                  mulop_shl:  // optimization of integer multiply - not implemented since hw mult available
                     begin
                        if intermediate_calculation_info[idx].result_size > intermediate_calculation_info[idx].a_size then
                           begin
                              TPIC18x_SUBFSR.Create (2, intermediate_calculation_info[idx].result_size-intermediate_calculation_info[idx].a_size);
                              StackUsageCounter.Push (intermediate_calculation_info[idx].result_size-intermediate_calculation_info[idx].a_size)
                           end;
                        additional_factors[idx].factor.Generate (GenerateCode, intermediate_calculation_info[idx].b_size);
                        GenerateMultiplyCode (intermediate_calculation_info[idx].result_size,
                                              intermediate_calculation_info[idx].a_integer_info,
                                              intermediate_calculation_info[idx].b_info
                                             )
                     end;
                  mulop_integer_div:
                     begin
                        additional_factors[idx].factor.Generate (GenerateCode, intermediate_calculation_info[idx].b_size);
                        GenerateZeroDivideCheckCode (additional_factors[idx].factor);
                        GenerateDivideCode (intermediate_calculation_info[idx].result_size,
                                            intermediate_calculation_info[idx].a_integer_info,
                                            intermediate_calculation_info[idx].b_info
                                           )
                     end;
                  mulop_shr: // optimization of integer divide
                     if additional_factors[idx].mulop_param <= 2 then
                        begin  // div by 2^1 or 2^2
                           for i := 1 to additional_factors[idx].mulop_param do
                              begin
                                 case additional_factors[idx].left_info.IntegerRange of
                                    irAlwaysNegative:
                                       TPIC18x_BSF.Create (STATUS, status_c, access_mode);
                                    irNegativeOrPositive:
                                       TPIC18x_RLCF.Create (1, dest_w, access_mode);
                                    irAlwaysNonNegative:
                                       TPIC18x_BCF.Create (STATUS, status_c, access_mode);
                                 else
                                    assert (false)
                                 end;
                                 for j := 1 to intermediate_calculation_info[idx].a_size do
                                    TPIC18x_RRCF.Create (j, dest_f, access_mode)
                              end
                        end
                     else   // div by 2^3 or more
                        begin
                           TPIC18x_PUSHL.Create (additional_factors[idx].mulop_param);
                           StackUsageCounter.Push(1);
                           lbl := nil;  // suppress compiler warning
                           case additional_factors[idx].left_info.IntegerRange of
                              irAlwaysNegative:
                                 lbl := TPIC18x_BSF.Create (STATUS, status_c, access_mode);
                              irNegativeOrPositive:
                                 lbl := TPIC18x_RLCF.Create (2, dest_w, access_mode);
                              irAlwaysNonNegative:
                                 lbl := TPIC18x_BCF.Create (STATUS, status_c, access_mode);
                           else
                              assert (false)
                           end;
                           for j := 1 to intermediate_calculation_info[idx].a_size do
                              TPIC18x_RRCF.Create (j+1, dest_f, access_mode);
                           TPIC18x_DECFSZ.Create (1, dest_f, access_mode);
                           TPIC18x_BRA.Create.dest := lbl;
                           TPIC18x_ADDFSR.Create (2, 1);
                           StackUsageCounter.Pop(1)
                        end;
                  mulop_integer_mod:
                     begin
                        additional_factors[idx].factor.Generate (GenerateCode, intermediate_calculation_info[idx].b_size);
                        GenerateZeroDivideCheckCode (additional_factors[idx].factor);
                        GenerateRemainderCode (intermediate_calculation_info[idx].result_size,
                                               intermediate_calculation_info[idx].a_integer_info,
                                               intermediate_calculation_info[idx].b_info
                                              )
                     end;
                  mulop_mask:  // optimization of mod
                     begin
                        annotation := 'Remainder: tos*' + IntToStr(intermediate_calculation_info[idx].result_size) + ' := tos*u' + IntToStr(intermediate_calculation_info[idx].result_size) + ' mod ' + IntToStr(additional_factors[idx].factor.constant.ordinal_value.AsInteger);
                        temp.Assign (additional_factors[idx].factor.constant.ordinal_value);
                        temp.Subtract (1);
                        for i := 0 to intermediate_calculation_info[idx].result_size-1 do
                           begin
                              addr := intermediate_calculation_info[idx].result_size-i;
                              case temp.AsByte(i) of
                                 0: begin
                                       TPIC18x_CLRF.Create (addr, access_mode).annotation := annotation;
                                       annotation := ''
                                    end;
                               $ff: {nop};
                              else
                                 begin
                                    TPIC18x_MOVLW.Create (temp.AsByte(i)).annotation := annotation;
                                    annotation := '';
                                    TPIC18x_ANDWF.Create (addr, dest_f, access_mode)
                                 end
                              end
                           end;
                        if annotation <> '' then
                           TAssemblyComment.Create ('      ' + annotation +  ' (nop)')
                     end;
                  mulop_mult_int_by_flt:
                     begin
                        convert_tos_integer_to_real (intermediate_calculation_info[idx].a_size,
                                                     intermediate_calculation_info[idx].a_info,
                                                     additional_factors[idx].mulop_src_loc
                                                    );
                        PushRealExpression (additional_factors[idx].factor);
                        FPM32.Call (additional_factors[idx].mulop_src_loc)
                     end;
                  mulop_mult_flt_by_flt:
                     begin
                        PushRealExpression (additional_factors[idx].factor);
                        FPM32.Call (additional_factors[idx].mulop_src_loc)
                     end;
                  mulop_divide_int_by_flt:
                     begin
                        convert_tos_integer_to_real (intermediate_calculation_info[idx].a_size,
                                                     intermediate_calculation_info[idx].a_info,
                                                     additional_factors[idx].mulop_src_loc
                                                    );
                        PushRealExpression (additional_factors[idx].factor);
                        FPD32.Call (additional_factors[idx].mulop_src_loc)
                     end;
                  mulop_divide_flt_by_flt:
                     begin
                        PushRealExpression (additional_factors[idx].factor);
                        FPD32.Call (additional_factors[idx].mulop_src_loc)
                     end;
               else
                  assert (false)
               end
            end;

         if expression_kind = integer_expression then
            generate_stack_fix_and_sign_extend_code (intermediate_calculation_info[Length(additional_factors)-1].result_size, 0, param2, info.IntegerRange);

         intermediate_calculation_info.Free
      end;

   procedure generate_boolean_term_code;
      var
         bras: array of TPIC18x_BRA;
         dummy: boolean;

      procedure add_bra;
         var i: integer;
         begin
            i := Length(bras);
            SetLength (bras, i+1);
            bras[i] := TPIC18x_BRA.Create
         end;

      procedure generate_factor_code (factor: TExpression; skip_sense: boolean);
         begin
            if expression_can_be_evaluated_with_simple_bit_test (factor, dummy) <> nil then
               GenerateCodeForConditionalSkip (factor, skip_sense)
            else
               begin
                  factor.Generate (GenerateCode, 1);
                  if skip_sense then
                     TPIC18x_BTFSS.Create (PREINC2, 0, access_mode)
                  else
                     TPIC18x_BTFSC.Create (PREINC2, 0, access_mode);
                  StackUsageCounter.Pop (1)
               end
         end;

      var
         idx: integer;
         accessA, accessB: TPIC18x_Access;
         lbl: TInstruction;
      begin  // generate_boolean_term_code
         // try optimization for two single-bit factors
         if Length(additional_factors) = 1 then
            begin
               assert (additional_factors[0].mulop = mulop_boolean_and);
               accessA := expression_can_be_evaluated_with_simple_bit_test (first_factor, dummy);
               accessB := expression_can_be_evaluated_with_simple_bit_test (additional_factors[0].factor, dummy);
               if (accessA <> nil) and (accessB <> nil) then
                  begin
                     TPIC18x_PUSHL.Create(1);
                     StackUsageCounter.Push(1);
                     GenerateCodeForConditionalSkip (first_factor, false);
                     GenerateCodeForConditionalSkip (additional_factors[0].factor, true);
                     TPIC18x_CLRF.Create (1, access_mode);
                     EXIT
                  end
            end;

         TPIC18x_PUSHL.Create (0);
         StackUsageCounter.Push(1);
         generate_factor_code (first_factor, true);
         add_bra;
         for idx := 0 to Length(additional_factors)-2 do
            begin
               generate_factor_code (additional_factors[idx].factor, true);
               add_bra
          end;
         generate_factor_code (additional_factors[Length(additional_factors)-1].factor, false);
         TPIC18x_INCF.Create (1, dest_f, access_mode);
         lbl := TAssemblyLabel.Create;
         for idx := 0 to Length(bras)-1 do
            bras[idx].dest := lbl
      end;  // generate_boolean_term_code

   procedure generate_set_term_code;
      var
         idx,i: integer;
         annotation: string;
      begin
         first_factor.Generate (GenerateCode, param2);
         for idx := 0 to Length(additional_factors)-1 do
            begin
               annotation := 'set intersection(*) of tos*' + IntToStr(param2) + ' with (tos-1)*' + IntToStr(param2) + ', pop tos*' + IntToStr(param2);
               assert (additional_factors[idx].mulop = mulop_set_intersection);
               additional_factors[idx].factor.Generate (GenerateCode, param2);
               for i := 1 to param2 do
                  begin
                     TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation;
                     annotation := '';
                     TPIC18x_ANDWF.Create (param2, dest_f, access_mode)
                  end;
               StackUsageCounter.Pop (param2)
            end
      end;

   procedure generate_string_term_code;
      begin
         assert (false)
      end;

   begin
      result := 0;  // suppress compiler warning
      case param1 of
         GenerateCode:
            case expression_kind of
               integer_expression,
               real_expression:
                  generate_numeric_term_code;
               boolean_expression:
                  generate_boolean_term_code;
               set_expression:
                  generate_set_term_code;
               string_expression:
                  generate_string_term_code;
            else
               assert (false)
            end;
      else
         assert (false, 'TPIC18x_Term.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

INITIALIZATION
   temp := TMultiPrecisionInteger.Create;

FINALIZATION
   temp.Free;

END.
