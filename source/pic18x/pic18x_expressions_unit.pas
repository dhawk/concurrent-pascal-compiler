UNIT pic18x_expressions_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_access_unit,
   cpc_blocks_unit,
   cpc_core_objects_unit,
   cpc_expressions_unit,
   pic18x_access_unit,
   pic18x_instructions_unit;

type
   TPIC18x_AbsFunctionPrimary =
      class (TAbsFunctionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_ChrTypeConversionPrimary =
      class (TChrTypeConversionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_ConstantPrimary =
      class (TConstantPrimary)
         function Generate (param1, param2: integer): integer;
            override;
         procedure PushRealConstant;
         procedure PushIEEESingleConstant;
      end;
   TPIC18x_FunctionAccessPrimary =
      class (TFunctionAccessPrimary)
         expr: TExpression;
         constructor CreateFromSourceTokens (_access: TAccess);
         function Generate (param1, param2: integer): integer;
            override;
         destructor Destroy;
            override;
      end;
   TPIC18x_NotPrimary =
      class (TNotPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_PredFunctionPrimary =
      class (TPredFunctionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_RelationalExpression =
      class (TRelationalExpression)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_RoundFunctionPrimary =
      class (TRoundFunctionPrimary)
      end;
   TPIC18x_SetConstructorPrimary =
      class (TSetConstructorPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_StrPosPrimary =
      class (TstrPosPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_SuccFunctionPrimary =
      class (TSuccFunctionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_TruncFunctionPrimary =
      class (TTruncFunctionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_UnaryMinusPrimary =
      class (TUnaryMinusPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_VariableAccessPrimary =
      class (TVariableAccessPrimary)
      private
         addr: integer;     // used only for global or local variables with no index calculation
         // load_next_... functions are used for normal (unpacked) variables
         function load_next_global_ram_byte (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_near_stack_byte (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_ram_byte_indirect_via_F1 (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_rom_byte (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_eeprom_byte (do_post_ptr_adjust: boolean): TInstruction;
         // load_next_..._packed functions are used for packed variables
         function load_next_global_ram_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_near_stack_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_ram_byte_indirect_via_F1_packed (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_rom_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
         function load_next_eeprom_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
      public
         function Generate (param1, param2: integer): integer;
            override;
      end;

function tos_size_description (signed: boolean; significant_bytes: integer): string;
procedure generate_stack_fix_and_sign_extend_code
            (current_result_size, garbage_tos_bytes, desired_result_size: integer;
             IntegerRange: TIntegerRange
            );
procedure PushRealExpression (expr: TExpression);
function expression_can_be_evaluated_with_simple_bit_test (expr: TExpression; var bit_sense: boolean): TPIC18x_Access;
procedure generate_bit_test_skip_code (access: TPIC18x_Access; bit_sense: boolean);
procedure GenerateCodeForConditionalSkip (boolean_expression: TExpression; skip_sense: boolean);

var
   get_bit_mask_routine: TInstruction;

IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_definitions_unit,
   cpc_multi_precision_integer_unit,
   cpc_source_analysis_unit,
   cpc_target_cpu_unit,
   Math,
   pic18x_blocks_unit,
   pic18x_core_objects_unit,
   pic18x_cpu_unit,
   pic18x_floating_point_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   pic18x_microprocessor_information_unit,
   pic18x_ram_map_unit,
   pic18x_run_time_error_check_unit,
   pic18x_string_unit,
   pic18x_types_unit,
   SysUtils;

type
   Tpush_ioreg_1bit_param_Subroutine =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      end;

procedure Tpush_ioreg_1bit_param_Subroutine.generate_subroutine_code;
   var
      bz: TPIC18x_BZ;
   begin
      TPIC18x_MOVSF.Create (1, FSR0H);
      TPIC18x_MOVSF.Create (2, FSR0L);
      TPIC18x_CLRF.Create (2, access_mode);
      TPIC18x_SWAPF.Create (PREINC2, dest_w, access_mode);
      TCallMacro.Create.dest := get_bit_mask_routine;
      TPIC18x_ANDWF.Create (INDF0, dest_w, access_mode);
      bz := TPIC18x_BZ.Create;
      TPIC18x_INCF.Create (1, dest_f, access_mode);
      bz.dest := TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tpush_ioreg_1bit_param_Subroutine.report_stack_sizes;
   begin
      check_stack_sizes (0, 1, 1)
   end;
{$endif}

var
   temp: TMultiPrecisionInteger;
   push_ioreg_1bit_param_Subroutine: Tpush_ioreg_1bit_param_Subroutine;

procedure gen_skip_instruction (bit_sense: boolean; addr, bit_position: integer; mode: TPIC18x_RAM_Access_Mode);
   begin
      case bit_sense of
         true:  TPIC18x_BTFSS.Create (addr, bit_position, mode);
         false: TPIC18x_BTFSC.Create (addr, bit_position, mode);
      end
   end;

procedure generate_bit_test_skip_code (access: TPIC18x_Access; bit_sense: boolean);
   var
      typinfo: TPIC18x_TypeInfo;
      offset: integer;
      bit_num: TBitNum;
   begin
      if access.node_is_packed_field then
         begin
            offset := TPIC18x_PackedRecordFieldInfo (access.node_packed_record_field.info).Offset;
            bit_num := TPIC18x_PackedRecordFieldInfo (access.node_packed_record_field.info).Position
         end
      else
         begin
            offset := 0;
            bit_num := 0
         end;
      typinfo := TPIC18x_TypeInfo(access.node_typedef.info);
      if access.absolute_address_with_no_indexing_required_mode then
         if access.absolute_address (offset) < 256 then
            gen_skip_instruction (bit_sense,
                                  access.absolute_address (offset),
                                  bit_num,
                                  bank_mode
                                 )
         else
            gen_skip_instruction (bit_sense,
                                  access.absolute_address (offset),
                                  bit_num,
                                  access_mode
                                 )
      else if access.near_stack_address_with_no_indexing_required_mode6 (offset) then
         gen_skip_instruction (bit_sense,
                               access.near_stack_address(offset),
                               bit_num,
                               access_mode
                              )
      else
         begin
            access.Generate_Load_Ptr2_Code (pFSR1, typinfo.Size-1+Offset);
            gen_skip_instruction (bit_sense,
                                  INDF1,
                                  bit_num,
                                  access_mode
                                 )
         end;
   end;

procedure GenerateCodeForConditionalSkip (boolean_expression: TExpression; skip_sense: boolean);
   var
      rel_exp: TPIC18x_RelationalExpression;
      access: TPIC18x_Access;
   begin
      if boolean_expression is TPIC18x_VariableAccessPrimary then
         begin
            access := TPIC18x_Access (TPIC18x_VariableAccessPrimary(boolean_expression).access);
            if access.can_be_evaluated_with_simple_bit_test then
               begin
                  generate_bit_test_skip_code (access, skip_sense);
                  exit
               end
         end;

      if boolean_expression is TPIC18x_RelationalExpression then
         begin
            rel_exp := TPIC18x_RelationalExpression(boolean_expression);

            if (rel_exp.left_simple_expression is TPIC18x_VariableAccessPrimary)
               and
               (rel_exp.right_simple_expression.contains_constant)
               and
               (rel_exp.relop <> relop_in)
            then
               begin
                  access := TPIC18x_Access (TPIC18x_VariableAccessPrimary (rel_exp.left_simple_expression).access);
                  if access.can_be_evaluated_with_simple_bit_test then
                     case rel_exp.right_simple_expression.constant.AsOrdinal of
                        0: case rel_exp.relop of
                              relop_equals,
                              relop_le:
                                 begin
                                    generate_bit_test_skip_code (access, not skip_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_gt:
                                 begin
                                    generate_bit_test_skip_code (access, skip_sense);
                                    exit
                                 end;
                              relop_lt,
                              relop_ge:
                                 assert (false);  // should have been optimized out
                           else
                              assert (false)
                           end;

                        1: case rel_exp.relop of
                              relop_equals,
                              relop_ge:
                                 begin
                                    generate_bit_test_skip_code (access, skip_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_lt:
                                 begin
                                    generate_bit_test_skip_code (access, not skip_sense);
                                    exit
                                 end;
                              relop_le,
                              relop_gt:
                                 assert (false);  // should have been optimized out
                           else
                              assert (false)
                           end;
                     else
                        assert (false)
                     end
               end;

            if (rel_exp.right_simple_expression is TPIC18x_VariableAccessPrimary)
               and
               (rel_exp.left_simple_expression.contains_constant)
               and
               (rel_exp.relop <> relop_in)
            then
               begin
                  access := TPIC18x_Access (TPIC18x_VariableAccessPrimary (rel_exp.right_simple_expression).access);
                  if access.can_be_evaluated_with_simple_bit_test then
                     case rel_exp.left_simple_expression.constant.AsOrdinal of
                        0: case rel_exp.relop of
                              relop_equals,
                              relop_ge:
                                 begin
                                    generate_bit_test_skip_code (access, not skip_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_lt:
                                 begin
                                    generate_bit_test_skip_code (access, skip_sense);
                                    exit
                                 end;
                              relop_gt,
                              relop_le:
                                 assert (false);  // should have been optimized out
                           else
                              assert (false)
                           end;

                        1: case rel_exp.relop of
                              relop_equals,
                              relop_le:
                                 begin
                                    generate_bit_test_skip_code (access, skip_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_gt:
                                 begin
                                    generate_bit_test_skip_code (access, not skip_sense);
                                    exit
                                 end;
                              relop_lt,
                              relop_ge:
                                 assert (false);  // should have been optimized out
                           else
                              assert (false)
                           end;
                     else
                        assert (false)
                     end
               end
         end;

      // if no exit by now, then full evaluation required
      boolean_expression.Generate (GenerateCode, 1);
      gen_skip_instruction (skip_sense, PREINC2, 0, access_mode);
      StackUsageCounter.Pop(1)
   end;

function expression_can_be_evaluated_with_simple_bit_test (expr: TExpression; var bit_sense: boolean): TPIC18x_Access;
   // returns access if true, nil if not
   var
      rel_expr: TPIC18x_RelationalExpression;
      vap: TPIC18x_VariableAccessPrimary;
      const_val: TConstant;
   begin
      if expr is TPIC18x_VariableAccessPrimary then
         begin
            result := TPIC18x_Access (TPIC18x_VariableAccessPrimary (expr).access);
            bit_sense := true;
            if result.can_be_evaluated_with_simple_bit_test then
               exit
         end;

      if (expr is TPIC18x_RelationalExpression)
         and
         (TPIC18x_RelationalExpression(expr).relop in [relop_equals, relop_notequals])
      then
         begin
            rel_expr := TPIC18x_RelationalExpression(expr);
            if (rel_expr.left_simple_expression is TPIC18x_VariableAccessPrimary)
               and
               (rel_expr.right_simple_expression.contains_constant)
            then
               begin
                  vap := TPIC18x_VariableAccessPrimary(rel_expr.left_simple_expression);
                  const_val := rel_expr.right_simple_expression.constant
               end
            else
               if (rel_expr.right_simple_expression is TPIC18x_VariableAccessPrimary)
                  and
                  (rel_expr.left_simple_expression.contains_constant)
               then
                  begin
                     vap := TPIC18x_VariableAccessPrimary(rel_expr.right_simple_expression);
                     const_val := rel_expr.left_simple_expression.constant
                  end
               else
                  begin
                     vap := nil;
                     const_val := nil
                  end;

            if vap <> nil then
               begin
                  result := TPIC18x_Access (vap.access);
                  if result.can_be_evaluated_with_simple_bit_test then
                     begin
                        case rel_expr.relop of
                           relop_equals:
                              bit_sense := const_val.ordinal_value.AsInteger = 1;
                           relop_notequals:
                              bit_sense := const_val.ordinal_value.AsInteger = 0;
                        else
                           assert (false)
                        end;
                        exit
                     end
               end
         end;

      result := nil
   end;

function tos_size_description (signed: boolean; significant_bytes: integer): string;
   begin
      result := '*';
      if signed then
         result := result + 's'
      else if significant_bytes > 0 then
         result := result + 'u';
      result := result + IntToStr (significant_bytes)
   end;

procedure generate_stack_fix_and_sign_extend_code
             (current_result_size, garbage_tos_bytes, desired_result_size: integer;
              IntegerRange: TIntegerRange
             );
   var
      i: integer;
      annotation: string;
   begin
      if current_result_size < desired_result_size then
         annotation := 'sign extend result by ' + IntToStr(desired_result_size-current_result_size)
      else if current_result_size > desired_result_size then
         annotation := 'truncate result by ' + IntToStr (current_result_size - desired_result_size)
      else
         annotation := '';
      if current_result_size + garbage_tos_bytes < desired_result_size then
         begin
            case IntegerRange of
               irAlwaysNonNegative:
                  begin
                     for i := garbage_tos_bytes downto 1 do
                        begin
                           TPIC18x_CLRF.Create (i, access_mode).annotation := annotation;
                           annotation := ''
                        end;
                     for i := 1 to desired_result_size - current_result_size - garbage_tos_bytes do
                        begin
                           TPIC18x_PUSHL.Create (0).annotation := annotation;
                           annotation := ''
                        end
                  end;
               irNegativeOrPositive:
                  if (current_result_size + 1 = desired_result_size) then
                     begin
                        case garbage_tos_bytes of
                           0: TPIC18x_PUSHL.Create (0).annotation := annotation;
                           1: TPIC18x_CLRF.Create (1, access_mode).annotation := annotation;
                        else
                           assert (false)
                        end;
                        TPIC18x_BTFSC.Create (2, 7, access_mode);    // test current result sign bit
                        TPIC18x_SETF.Create (1, access_mode)
                     end
                  else
                     begin
                        TPIC18x_MOVLW.Create (0).annotation := annotation;
                        TPIC18x_BTFSC.Create (1 + garbage_tos_bytes, 7, access_mode);   // test current result sign bit
                        TPIC18x_MOVLW.Create ($ff);
                        for i := garbage_tos_bytes downto 1 do
                           TPIC18x_MOVWF.Create (i, access_mode);
                        for i := 1 to desired_result_size - current_result_size - garbage_tos_bytes do
                           TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
                     end;
               irAlwaysNegative:
                  begin
                     for i := garbage_tos_bytes downto 1 do
                        begin
                           TPIC18x_SETF.Create (i, access_mode).annotation := annotation;
                           annotation := ''
                        end;
                     for i := 1 to desired_result_size - current_result_size - garbage_tos_bytes do
                        begin
                           TPIC18x_PUSHL.Create ($ff).annotation := annotation;
                           annotation := ''
                        end
                  end;
            else
               assert (false)
            end;
            StackUsageCounter.Push (desired_result_size - current_result_size - garbage_tos_bytes)
         end
      else if current_result_size + garbage_tos_bytes > desired_result_size then
         begin
            TPIC18x_ADDFSR.Create (2, current_result_size + garbage_tos_bytes - desired_result_size).annotation := annotation;
            StackUsageCounter.Pop (current_result_size + garbage_tos_bytes - desired_result_size);
            if current_result_size < desired_result_size then
               begin
                  case IntegerRange of
                     irAlwaysNonNegative:
                        for i := desired_result_size - current_result_size downto 1 do
                           TPIC18x_CLRF.Create (i, access_mode);
                     irNegativeOrPositive:
                        if desired_result_size - current_result_size = 1 then
                           begin
                              TPIC18x_CLRF.Create (1, access_mode);
                              TPIC18x_BTFSC.Create (2, 7, access_mode);
                              TPIC18x_SETF.Create (1, access_mode)
                           end
                        else
                           begin
                              TPIC18x_MOVLW.Create (0);
                              TPIC18x_BTFSC.Create (desired_result_size - current_result_size + 1, 7, access_mode);   // test current result sign bit
                              TPIC18x_MOVLW.Create ($ff);
                              for i := 1 to desired_result_size - current_result_size do
                                 TPIC18x_MOVWF.Create (i, access_mode)
                           end;
                     irAlwaysNegative:
                        for i := desired_result_size - current_result_size downto 1 do
                           TPIC18x_SETF.Create (i, access_mode);
                  else
                     assert (false)
                  end
               end
         end
      else  // current_result_size + garbage_tos_bytes = desired_result_size
         case IntegerRange of
            irAlwaysNonNegative:
               for i := 1 to garbage_tos_bytes do
                  begin
                     TPIC18x_CLRF.Create (i, access_mode).annotation := annotation;
                     annotation := ''
                  end;
            irNegativeOrPositive:
               if garbage_tos_bytes = 1 then
                  begin
                     TPIC18x_CLRF.Create (1, access_mode).annotation := annotation;
                     TPIC18x_BTFSC.Create (2, 7, access_mode);
                     TPIC18x_SETF.Create (1, access_mode)
                  end
               else if garbage_tos_bytes > 1 then
                  begin
                     TPIC18x_MOVLW.Create (0).annotation := annotation;
                     TPIC18x_BTFSC.Create (1 + garbage_tos_bytes, 7, access_mode);   // test current result sign bit
                     TPIC18x_MOVLW.Create ($ff);
                     for i := 1 to garbage_tos_bytes do
                        TPIC18x_MOVWF.Create (i, access_mode)
                  end;
            irAlwaysNegative:
               for i := 1 to garbage_tos_bytes do
                  begin
                     TPIC18x_SETF.Create (i, access_mode).annotation := annotation;
                     annotation := ''
                  end;
         else
            assert (false)
         end
   end;

function TPIC18x_AbsFunctionPrimary.Generate (param1, param2: integer): integer;
   const
      ARGB0 = 2;
      REAL_SIGN_BIT = 7;
   var
      annotation: string;
      result_size, i: integer;
      bnn: TPIC18x_BNN;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            case expression_kind of
               integer_expression:
                  begin
                     result_size := TPIC18x_TypeInfo(info).Size;
                     expr.Generate (GenerateCode, result_size);
                     annotation := 'tos := iabs(tos)';
                     if TPIC18x_TypeInfo(expr.info).IntegerRange <> irAlwaysNonNegative then
                        begin
                           bnn := nil;
                           if TPIC18x_TypeInfo(expr.info).IntegerRange = irNegativeOrPositive then
                              begin
                                 TPIC18x_MOVF.Create (1, dest_w, access_mode).annotation := annotation;
                                 annotation := '';
                                 bnn := TPIC18x_BNN.Create
                              end;
                           for i := 1 to result_size-1 do
                              begin
                                 TPIC18x_COMF.Create (i, dest_f, access_mode).annotation := annotation;
                                 annotation := ''
                              end;
                           TPIC18x_NEGF.Create (result_size, access_mode);
                           if result_size > 1 then
                              begin
                                 TPIC18x_MOVLW.Create (0);
                                 for i := result_size-1 downto 1 do
                                    TPIC18x_ADDWFC.Create (i, dest_f, access_mode)
                              end;
                           if bnn <> nil then
                              bnn.dest := TAssemblyLabel.Create
                        end
                  end;
               real_expression:
                  begin
                     PushRealExpression (expr);
                     TPIC18x_BCF.Create (ARGB0, REAL_SIGN_BIT, access_mode).annotation := 'tos := rabs(tos)'
                  end;
            else
               assert (false)
            end
      else
         assert (false, 'TPIC18x_AbsFunctionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ChrTypeConversionPrimary.Generate (param1, param2: integer): integer;
   var
      expr_size: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               expr_size := TPIC18x_TypeInfo(expr.info).Size;
               expr.Generate (GenerateCode, expr_size);
               GenerateRangeCheckCode (TOrdinalDataType(target_cpu.get_supported_data_type('char')),
                                       expr_size,
                                       expr.info,
                                       src_loc,
                                       'range error'
                                      );
               if expr_size > 1 then
                  begin
                     TPIC18x_ADDFSR.Create (2, expr_size-1);
                     StackUsageCounter.Pop(expr_size - 1)
                  end
            end;
      else
         assert (false, 'TPIC18x_ChrTypeConversionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ConstantPrimary.Generate (param1, param2: integer): integer;
   var
      annotation: string;
      i, rom_addr: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            case TConstant(the_constant).constant_kind of
               integer_constant:
                  begin
                     annotation := 'push ' + IntToStr (constant.ordinal_value.AsInteger);
                     for i := 0 to param2-1 do
                        begin
                           TPIC18x_PUSHL.Create (constant.ordinal_value.AsByte(i)).annotation := annotation;
                           annotation := ''
                        end;
                     StackUsageCounter.push (param2)
                  end;
               boolean_constant:
                  begin
                     assert (param2 = 1);
                     if constant.ordinal_value.AsUnsigned = 1 then
                        annotation := 'push true'
                     else
                        annotation := 'push false';
                     TPIC18x_PUSHL.Create (constant.ordinal_value.AsUnsigned).annotation := annotation;
                     StackUsageCounter.push (param2)
                  end;
               enum_constant:
                  begin
                     assert (param2 = 1);
                     TPIC18x_PUSHL.Create (constant.ordinal_value.AsUnsigned).annotation := 'push enum';
                     StackUsageCounter.push (param2)
                  end;
               real_constant:
                  PushRealConstant;
               set_constant:
                  begin
                     if constant.sett = [] then
                        annotation := 'push set []'
                     else
                        begin
                           annotation := 'push set [';
                           for i := min_set to max_set do
                              if i in constant.sett then
                                 annotation := annotation + IntToStr(i) + ',';
                           annotation[Length(annotation)] := ']'  // replace final comma
                        end;
                     for i := 0 to param2-1 do
                        begin
                           TPIC18x_PUSHL.Create (SetByte (constant, i)).annotation := annotation;
                           annotation := ''
                        end;
                     StackUsageCounter.push (param2)
                  end;
               string_constant:
                  if Length(constant.s) = 1 then
                     begin
                        TPIC18x_PUSHL.Create (ord(constant.s[1])).annotation := annotation;
                        StackUsageCounter.Push(1)
                     end
                  else
                     assert (false);
            else
               assert (false)
            end;
         GenerateCodeToCopyToRAMString:
            begin
               rom_addr := TPIC18x_CPU(target_cpu).anonymous_string_constant_rom_addr (constant.s);
               set_absolute (TBLPTRL, lsb(rom_addr));
               set_absolute (TBLPTRH, msb(rom_addr));
               TPIC18x_TBLRD.Create (tblrd_post_inc);
               TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
               AssignROMStrToRAMStr.Call (src_loc)
            end;
      else
         assert (false, 'TPIC18x_ConstantPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

procedure TPIC18x_ConstantPrimary.PushRealConstant;
   var
      annotation: string;
      i: integer;
      pic_real: TPIC18x_Real;
   begin
      annotation := 'push ' + FloatToStr(constant.r);
      pic_real.r := constant.r;
      for i := 0 to 3 do
         begin
            TPIC18x_PUSHL.Create (pic_real.pic_real_AsByte[i]).annotation := annotation;
            annotation := ''
         end;
      StackUsageCounter.push (real_size)
   end;

procedure TPIC18x_ConstantPrimary.PushIEEESingleConstant;
   var
      annotation: string;
      i: integer;
      pic_real: TPIC18x_Real;
   begin
      annotation := 'push ' + FloatToStr(constant.r);
      pic_real.r := constant.r;
      for i := 0 to 3 do
         begin
            TPIC18x_PUSHL.Create (pic_real.ieee_single_AsByte[i]).annotation := annotation;
            annotation := ''
         end;
      StackUsageCounter.push (real_size)
   end;

constructor TPIC18x_FunctionAccessPrimary.CreateFromSourceTokens (_access: TAccess);
   begin
      inherited Create;
      access := _access;
      if (access.node_routine = TPIC18x_CPU(target_cpu).Round24)
         or
         (access.node_routine = TPIC18x_CPU(target_cpu).Round32)
         or
         (access.node_routine = TPIC18x_CPU(target_cpu).Trunc24)
         or
         (access.node_routine = TPIC18x_CPU(target_cpu).Trunc32)
      then
         begin
            access.AddRef;

            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            expr := CreateExpressionFromSourceTokens;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token;

            if expr.contains_real_constant then
               if (access.node_routine = TPIC18x_CPU(target_cpu).Round24)
                  or
                  (access.node_routine = TPIC18x_CPU(target_cpu).Round32) then
                  raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(round(expr.constant.r)), src_loc))
               else   // trunc24 or trunc32
                  raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(trunc(expr.constant.r)), src_loc))
            else if expr.contains_integer_constant then
               begin
                  expr.constant.AddRef;
                  raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(expr.constant, src_loc))
               end
            else if expr.expression_kind = integer_expression then
               begin
                  expr.AddRef;
                  raise EPrimaryExpressionSimplification.Create(expr)
               end;

            if expr.expression_kind <> real_expression then
               raise compile_error.Create (err_numeric_expression_expected, expr.src_loc);

            info.range_kind := irOrdinalRange;
            if (access.node_routine = TPIC18x_CPU(target_cpu).Round24)
               or
               (access.node_routine = TPIC18x_CPU(target_cpu).Trunc24) then
               begin
                  info.min_value.Assign (target_cpu.get_supported_data_type('int24').info.min_value);
                  info.max_value.Assign (target_cpu.get_supported_data_type('int24').info.max_value)
               end
            else  // round32 or trunc32
               begin
                  info.min_value.Assign (target_cpu.get_supported_data_type('int32').info.min_value);
                  info.max_value.Assign (target_cpu.get_supported_data_type('int32').info.max_value)
               end
         end
      else
         inherited
   end;

function TPIC18x_FunctionAccessPrimary.Generate (param1, param2: integer): integer;

   procedure generate_function_call_code (routine: TPIC18x_Routine);
      var
         push_return_address_macro: TPushLabelMacro;
         instr: TInstruction;
         is_monitor_entry_routine: boolean;
      begin
         is_monitor_entry_routine := false;
         if routine.function_result.typedef.type_kind <> string_type then
            routine.PushDefaultResultValue;

         push_return_address_macro := TPushLabelMacro.Create;
         push_return_address_macro.annotation := 'push return address';
         StackUsageCounter.Push (3);

         if routine.entry then
            begin
               TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2).annotation := 'save this pointer';
               TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2);
               StackUsageCounter.Push (2);

               assert (routine.context.definition_kind = type_definition);
               assert (TTypeDef(routine.context).type_kind = system_type);
               if TSystemType(routine.context).system_type_kind = monitor_system_type then
                  begin
                     is_monitor_entry_routine := true;
                     TPIC18x_MOVFF.Create (current_prio, POSTDEC2).annotation := 'save running_prio;';
                     StackUsageCounter.Push (1)
                  end
            end;

         if routine.parameter_definitions <> nil then
            TPIC18x_ParamList(routine.parameter_definitions).PushParameters (actual_parameters);

         if routine.entry then
            TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTHIS, 0);

         TPIC18x_CallRecord(call_record).caller_relative_stk_ptr_at_call := StackUsageCounter.Current;

         instr := TGOTOMacro.Create;
         instr.dest := routine.entry_point_label;
         instr.annotation := 'call ' + access.path_src;
         push_return_address_macro.dest := TAssemblyLabel.Create;

         StackUsageCounter.PushPop (routine.stack_usage);
         if routine.parameter_definitions <> nil then
            StackUsageCounter.Pop (TPIC18x_ParamList(routine.parameter_definitions).Size);
         if is_monitor_entry_routine then
            StackUsageCounter.Pop (1);   // prio
         if routine.entry then
            StackUsageCounter.Pop (2);   // this
         StackUsageCounter.Pop (3);      // rtrn

         if (routine.function_result.typedef.type_kind = basic_data_type)
            and
            (TBasicDataType(routine.function_result.typedef).basic_data_type_kind = ordinal_data_type) then
            generate_stack_fix_and_sign_extend_code (TPIC18x_TypeInfo(routine.function_result.typedef.info).Size, 0, param2, routine.function_result.typedef.info.IntegerRange)
      end;

   var
      i: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            if access.node_routine = TPIC18x_CPU (target_cpu).ErrorCode then
               begin
                  get_errorcode_routine.Call;
                  if param2 < 3 then
                     begin
                        TPIC18x_ADDFSR.Create (2, 3-param2);
                        StackUsageCounter.pop (3-param2)
                     end
                  else if param2 > 3 then
                     begin
                        for i := 4 to param2 do
                           TPIC18x_CLRF.Create (POSTDEC2, access_mode);
                        StackUsageCounter.push (param2-3)
                     end
               end
            else if access.node_routine = TPIC18x_CPU (target_cpu).Round24 then
               begin
                  expr.Generate (GenerateCode, real_size);
                  FPRound24.Call (expr.src_loc)
               end
            else if access.node_routine = TPIC18x_CPU (target_cpu).Round32 then
               begin
                  expr.Generate (GenerateCode, real_size);
                  FPRound32.Call (expr.src_loc)
               end
            else if access.node_routine = TPIC18x_CPU (target_cpu).Trunc24 then
               begin
                  expr.Generate (GenerateCode, real_size);
                  FPTrunc24.Call (expr.src_loc)
               end
            else if access.node_routine = TPIC18x_CPU (target_cpu).Trunc32 then
               begin
                  expr.Generate (GenerateCode, real_size);
                  FPTrunc32.Call (expr.src_loc)
               end
            else if access.node_routine <> nil then
               generate_function_call_code (TPIC18x_Routine (access.node_routine))
            else if access.node_property <> nil then
               generate_function_call_code (TPIC18x_Routine (access.node_property.get_func))
            else
               assert (false);
      else
         assert (false, 'TPIC18x_FunctionAccessPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

destructor TPIC18x_FunctionAccessPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

function TPIC18x_NotPrimary.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               boolean_expr.Generate (GenerateCode, 1);
               TPIC18x_BTG.Create (1, 0, access_mode).annotation := 'tos := not tos'
            end;
      else
         assert (false, 'TPIC18x_NotPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_PredFunctionPrimary.Generate (param1, param2: integer): integer;
   var
      result_size, i: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               result_size := TPIC18x_TypeInfo(info).Size;
               expr.Generate (GenerateCode, result_size);
               TPIC18x_DECF.Create (result_size, dest_f, access_mode).annotation := 'tos := pred(tos)';
               if result_size > 1 then
                  begin
                     TPIC18x_MOVLW.Create (0);
                     for i := result_size-1 downto 1 do
                        TPIC18x_SUBWFB.Create (i, dest_f, access_mode)
                  end
            end;
      else
         assert (false, 'TPIC18x_PredFunctionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_RelationalExpression.Generate (param1, param2: integer): integer;

   procedure generate_ordinal_relational_expression_code;
      var
         a_size, b_size, result_size: integer;
         b_info: TTypeInfo;
         result_addr, b_addr: integer;
         i: integer;
         bn: TPIC18x_BN;
         bnn: TPIC18x_BNN;
         bz: TPIC18x_BZ;
      begin
         b_size := 0;  // suppress compiler warning
         b_info := nil; // suppress compiler warning
         result_size := 0;  // suppress compiler warning

         if (TPIC18x_TypeInfo(left_simple_expression.info).Size = 1)
            and
            (TPIC18x_TypeInfo(right_simple_expression.info).Size = 1)
            and
            (relop in [relop_equals, relop_notequals]) then
            begin
               left_simple_expression.Generate (GenerateCode, 1);
               right_simple_expression.Generate (GenerateCode, 1);
               TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
               StackUsageCounter.Pop (1);
               TPIC18x_SUBWF.Create (1, dest_f, access_mode);    // sets N,Z status
               case relop of
                  relop_equals:
                     begin
                        TPIC18x_TSTFSZ.Create (1, access_mode);
                        TPIC18x_SETF.Create (1, access_mode);
                        TPIC18x_INCF.Create (1, dest_f, access_mode);   // -1 -> 0  or  0 -> 1
                     end;
                  relop_notequals:
                     begin
                        bz := TPIC18x_BZ.Create;
                        TPIC18x_MOVLW.Create (1);
                        TPIC18x_MOVWF.Create (1, access_mode);
                        bz.dest := TAssemblyLabel.Create
                     end;
               else
                  assert (false)
               end
            end
         else
            begin
               case relop of
                  relop_equals,
                  relop_notequals:
                     // push larger expression as a, then smaller expression as b
                     if TPIC18x_TypeInfo(left_simple_expression.info).Size > TPIC18x_TypeInfo(right_simple_expression.info).Size then
                        begin
                           a_size := TPIC18x_TypeInfo(left_simple_expression.info).Size;
                           b_size := TPIC18x_TypeInfo(right_simple_expression.info).Size;
                           result_size := a_size;
                           left_simple_expression.Generate (GenerateCode, result_size);
                           right_simple_expression.Generate (GenerateCode, b_size);
                           b_info := right_simple_expression.info
                        end
                     else
                        begin
                           a_size := TPIC18x_TypeInfo(right_simple_expression.info).Size;
                           b_size := TPIC18x_TypeInfo(left_simple_expression.info).Size;
                           result_size := a_size;
                           right_simple_expression.Generate (GenerateCode, result_size);
                           left_simple_expression.Generate (GenerateCode, b_size);
                           b_info := left_simple_expression.info
                        end;
                  relop_lt,
                  relop_ge:
                     begin
                        result_size := TPIC18x_TypeInfo(comparison_info).Size;
                        // a_size := min (TPIC18x_TypeInfo(left_simple_expression.info).Size, result_size);
                        b_size := TPIC18x_TypeInfo(right_simple_expression.info).Size;
                        left_simple_expression.Generate (GenerateCode, result_size);
                        right_simple_expression.Generate (GenerateCode, b_size);
                        b_info := right_simple_expression.info
                     end;
                  relop_le,
                  relop_gt:
                     begin
                        result_size := TPIC18x_TypeInfo(comparison_info).Size;
                        // a_size := min (TPIC18x_TypeInfo(right_simple_expression.info).Size, result_size);
                        b_size := TPIC18x_TypeInfo(left_simple_expression.info).Size;
                        right_simple_expression.Generate (GenerateCode, result_size);
                        left_simple_expression.Generate (GenerateCode, b_size);
                        b_info := left_simple_expression.info
                     end;
               else
                  assert (false)
               end;

               result_addr := result_size + b_size;
               b_addr := b_size;
               TPIC18x_MOVF.Create (b_addr, dest_w, access_mode);
               TPIC18x_SUBWF.Create (result_addr, dest_f, access_mode);    // sets N,Z status
               i := 1;
               while i < b_size do
                  begin
                     TPIC18x_MOVF.Create (b_addr-i, dest_w, access_mode);
                     TPIC18x_SUBWFB.Create (result_addr-i, dest_f, access_mode);    // sets N,Z status
                     i := i + 1
                  end;
               if i < result_size then
                  begin
                     case TPIC18x_TypeInfo(b_info).IntegerRange of
                        irAlwaysNonNegative:
                           TPIC18x_MOVLW.Create (0);
                        irNegativeOrPositive:
                           begin
                              TPIC18x_MOVLW.Create (0);
                              TPIC18x_BTFSC.Create (1, 7, access_mode);
                              TPIC18x_MOVLW.Create ($FF)
                           end;
                        irAlwaysNegative:
                           TPIC18x_MOVLW.Create ($FF);
                     else
                        assert (false)
                     end;
                     while i < result_size do
                        begin
                           TPIC18x_SUBWFB.Create (result_addr-i, dest_f, access_mode);    // sets N,Z status
                           i := i + 1
                        end
                  end;

               case relop of
                  relop_equals:
                     begin
                        // if equal then all result bytes are 0
                        if result_size > 1 then
                           begin
                              TPIC18x_MOVF.Create (result_addr-(result_size-1), dest_w, access_mode);
                              for i := result_size-2 downto 1 do
                                 TPIC18x_IORWF.Create (result_addr-i, dest_w, access_mode);
                              TPIC18x_IORWF.Create (result_addr, dest_f, access_mode)    // sets Z
                           end;
                        // at this pt if equal: [result_addr] if 0; if <> [result_addr] is non-zero
                        TPIC18x_TSTFSZ.Create (result_addr, access_mode);
                        TPIC18x_SETF.Create (result_addr, access_mode);
                        TPIC18x_INCF.Create (result_addr, dest_f, access_mode);   // -1 -> 0  or  0 -> 1
                        TPIC18x_ADDFSR.Create (2, result_size + b_size - 1);
                        StackUsageCounter.Pop (result_size + b_size - 1)
                     end;
                  relop_notequals:
                     begin
                        // if equal then all result bytes are 0
                        if result_size > 1 then
                           begin
                              TPIC18x_MOVF.Create (result_addr-(result_size-1), dest_w, access_mode);
                              for i := result_size-2 downto 1 do
                                 TPIC18x_IORWF.Create (result_addr-i, dest_w, access_mode);
                              TPIC18x_IORWF.Create (result_addr, dest_f, access_mode)    // sets Z
                           end;
                        // at this pt if equal: [result_addr] if 0 and Z is set; if <> Z is clr and [result_addr] is non-zero
                        bz := TPIC18x_BZ.Create;
                        TPIC18x_MOVLW.Create (1);
                        TPIC18x_MOVWF.Create (result_addr, access_mode);
                        bz.dest := TPIC18x_ADDFSR.Create (2, result_size+b_size-1);
                        StackUsageCounter.Pop (result_size+b_size-1)
                     end;
                  relop_lt,
                  relop_gt:
                     begin
                        TPIC18x_ADDFSR.Create (2, result_size+b_size);
                        StackUsageCounter.Pop (result_size+b_size);
                        TPIC18x_PUSHL.Create (0);
                        StackUsageCounter.Push (1);
                        bnn := TPIC18x_BNN.Create;
                        TPIC18x_INCF.Create (1, dest_f, access_mode);
                        bnn.dest := TAssemblyLabel.Create
                     end;
                  relop_le,
                  relop_ge:
                     begin
                        TPIC18x_ADDFSR.Create (2, result_size+b_size);
                        StackUsageCounter.Pop (result_size+b_size);
                        TPIC18x_PUSHL.Create (0);
                        StackUsageCounter.Push (1);
                        bn := TPIC18x_BN.Create;
                        TPIC18x_INCF.Create (1, dest_f, access_mode);
                        bn.dest := TAssemblyLabel.Create
                     end;
               else
                  assert (false)
               end
            end
      end;

   procedure generate_set_relational_expression_code;
      var
         a_addr, a_size, b_size: integer;
         bz: TPIC18x_BZ;
      begin
         a_size := 1;
         // do full evaluation or only one byte????
         left_simple_expression.Generate (GenerateCode, a_size);
         // do range check????       if out of range will only read a random bit (no trashing anything), but no error code...
         b_size := TPIC18x_TypeInfo(right_simple_expression.info).Size;
         right_simple_expression.Generate(GenerateCode, b_size);
         a_addr := b_size + a_size;
         TPIC18x_SWAPF.Create (a_addr, dest_w, access_mode).annotation := format ('tos*1 := tos*%d@[%d] in tos*%d', [a_size, a_addr, b_size]);
         TPIC18x_RLNCF.Create (WREG, dest_w, access_mode);
         TPIC18x_ANDLW.Create ($1F);
         TPIC18x_SUBLW.Create (b_size);
         TPIC18x_MOVFF.Create (PLUSW2, FSR1L);               // use FSR1.lsb as temp save register
         TPIC18x_RLNCF.Create (a_addr, dest_w, access_mode);
         TCallMacro.Create.dest := get_bit_mask_routine;
         TPIC18x_ANDWF.Create (FSR1L, dest_w, access_mode);  // end use of FSR1.lsb as temp save register
         TPIC18x_ADDFSR.Create (2, b_size+1);
         StackUsageCounter.Pop (b_size+1);
         TPIC18x_PUSHL.Create (0);
         StackUsageCounter.Push (1);
         bz := TPIC18x_BZ.Create;
         TPIC18x_INCF.Create (1, dest_f, access_mode);
         bz.dest := TAssemblyLabel.Create
      end;

   procedure generate_string_relational_expression_code;
      var
         operands_reversed: boolean;

      procedure compare_string_to_char_expression (a: TPIC18x_VariableAccessPrimary; b: TExpression);
         begin
            b.Generate (GenerateCode, 1);
            case TPIC18x_Access(a.access).base_variable.descriptor of
               rw_const,
               rw_var:
                  begin
                     TPIC18x_Access(a.access).Generate_Load_Ptr2_Code (pFSR1, 0);
                     CompareRAMStrToChar.Call
                  end;
               rw_rom:
                  begin
                     TPIC18x_Access(a.access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                     CompareROMStrToChar.Call
                  end;
               rw_eeprom:
                  begin
                     TPIC18x_Access(a.access).Generate_Load_Ptr1_Code (pFSR1, 0);
                     CompareEEPROMStrToChar.Call
                  end;
            else
               assert (false)
            end
         end;

      procedure compare_ram_string_to_ram_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor in [rw_const, rw_var]);
            assert (b.access.base_variable.descriptor in [rw_const, rw_var]);
            TPIC18x_Access(a.access).Generate_Push_Address2_Code (0, false);
            TPIC18x_Access(b.access).Generate_Load_Ptr2_Code (pFSR1, 0);
            CompareRAMStrToRAMStr.Call
         end;

      procedure compare_ram_string_to_rom_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor in [rw_const, rw_var]);
            assert (b.access.base_variable.descriptor = rw_rom);
            TPIC18x_Access(a.access).Generate_Push_Address2_Code (0, false);
            TPIC18x_Access(b.access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
            CompareRAMStrToROMStr.Call
         end;

      procedure compare_ram_string_to_eeprom_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor in [rw_const, rw_var]);
            assert (b.access.base_variable.descriptor = rw_eeprom);
            TPIC18x_Access(a.access).Generate_Push_Address2_Code (0, false);
            TPIC18x_Access(b.access).Generate_Load_Ptr1_Code (pFSR1, 0);
            CompareRAMStrToEEPROMStr.Call
         end;

      procedure compare_rom_string_to_rom_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor = rw_rom);
            assert (b.access.base_variable.descriptor = rw_rom);
            TPIC18x_Access(a.access).Generate_Push_Address2_Code (0, false);
            TPIC18x_Access(b.access).Generate_Push_Address2_Code (0, false);
            CompareROMStrToROMStr.Call
         end;

      procedure compare_rom_string_to_eeprom_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor = rw_rom);
            assert (b.access.base_variable.descriptor = rw_eeprom);
            TPIC18x_Access(a.access).Generate_Push_Address2_Code (0, false);
            TPIC18x_Access(b.access).Generate_Load_Ptr1_Code (pFSR1, 0);
            CompareROMStrToEEPROMStr.Call
         end;

      procedure compare_eeprom_string_to_eeprom_string (a, b: TPIC18x_VariableAccessPrimary);
         begin
            assert (a.access.base_variable.descriptor = rw_eeprom);
            assert (b.access.base_variable.descriptor = rw_eeprom);
            TPIC18x_Access(a.access).Generate_Push_Address1_Code (0, false);
            TPIC18x_Access(b.access).Generate_Push_Address1_Code (0, false);
            CompareEEPROMStrToEEPROMStr.Call
         end;

      begin  // generate_string_relational_expression_code
         operands_reversed := false;
         case left_simple_expression.expression_kind of
            char_expression:
               begin
                  assert (right_simple_expression.expression_kind = string_expression);
                  assert (right_simple_expression is TVariableAccessPrimary);
                  compare_string_to_char_expression (TPIC18x_VariableAccessPrimary(right_simple_expression), left_simple_expression);
                  operands_reversed := true
               end;
            string_expression:
               case right_simple_expression.expression_kind of
                  char_expression:
                     begin
                        assert (left_simple_expression is TVariableAccessPrimary);
                        compare_string_to_char_expression (TPIC18x_VariableAccessPrimary(left_simple_expression), right_simple_expression)
                     end;
                  string_expression:
                     begin
                        assert (left_simple_expression is TVariableAccessPrimary);
                        assert (right_simple_expression is TVariableAccessPrimary);
                        case TVariableAccessPrimary(left_simple_expression).access.base_variable.descriptor of
                           rw_const,
                           rw_var:
                              case TVariableAccessPrimary(right_simple_expression).access.base_variable.descriptor of
                                 rw_const,
                                 rw_var:
                                    compare_ram_string_to_ram_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                                 rw_rom:
                                    compare_ram_string_to_rom_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                                 rw_eeprom:
                                    compare_ram_string_to_eeprom_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                              else
                                 assert (false)
                              end;
                           rw_rom:
                              case TVariableAccessPrimary(right_simple_expression).access.base_variable.descriptor of
                                 rw_const,
                                 rw_var:
                                    begin
                                       compare_ram_string_to_rom_string (TPIC18x_VariableAccessPrimary(right_simple_expression), TPIC18x_VariableAccessPrimary(left_simple_expression));
                                       operands_reversed := true
                                    end;
                                 rw_rom:
                                    compare_rom_string_to_rom_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                                 rw_eeprom:
                                    compare_rom_string_to_eeprom_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                              else
                                 assert (false)
                              end;
                           rw_eeprom:
                              case TVariableAccessPrimary(right_simple_expression).access.base_variable.descriptor of
                                 rw_const,
                                 rw_var:
                                    begin
                                       compare_ram_string_to_eeprom_string (TPIC18x_VariableAccessPrimary(right_simple_expression), TPIC18x_VariableAccessPrimary(left_simple_expression));
                                       operands_reversed := true
                                    end;
                                 rw_rom:
                                    begin
                                       compare_rom_string_to_eeprom_string (TPIC18x_VariableAccessPrimary(right_simple_expression), TPIC18x_VariableAccessPrimary(left_simple_expression));
                                       operands_reversed := true
                                    end;
                                 rw_eeprom:
                                    compare_eeprom_string_to_eeprom_string (TPIC18x_VariableAccessPrimary(left_simple_expression), TPIC18x_VariableAccessPrimary(right_simple_expression));
                              else
                                 assert (false)
                              end;
                        else
                           assert (false)
                        end
                     end;
               else
                  assert (false)
               end;
         else
            assert (false)
         end;

         // tos is 0, STATUS N,Z is set to comparison result
         // now set boolean comparison result in tos
         if operands_reversed then
            case relop of
               relop_equals:
                  begin
                     TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_notequals:
                  begin
                     TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_lt:
                  begin
                     TPIC18x_MOVLW.Create ($14);   // mask N and Z
                     TPIC18x_ANDWF.Create (STATUS, dest_w, access_mode);  // sets Z if >
                     TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_gt:
                  begin
                     TPIC18x_BTFSC.Create (STATUS, status_n, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_le:
                  begin
                     TPIC18x_BTFSS.Create (STATUS, status_n, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_ge:
                  begin
                     TPIC18x_MOVLW.Create ($14);   // mask N and Z
                     TPIC18x_ANDWF.Create (STATUS, dest_w, access_mode);  // sets Z if >
                     TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
            else
               assert (false)
            end
         else  // operands not reversed
            case relop of
               relop_equals:
                  begin
                     TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_notequals:
                  begin
                     TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_lt:
                   begin
                     TPIC18x_BTFSC.Create (STATUS, status_n, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_gt:
                  begin
                     TPIC18x_MOVLW.Create ($14);   // mask N and Z
                     TPIC18x_ANDWF.Create (STATUS, dest_w, access_mode);  // sets Z if >
                     TPIC18x_BTFSC.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_le:
                  begin
                     TPIC18x_MOVLW.Create ($14);   // mask N and Z
                     TPIC18x_ANDWF.Create (STATUS, dest_w, access_mode);  // sets Z if >
                     TPIC18x_BTFSS.Create (STATUS, status_z, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
               relop_ge:
                   begin
                     TPIC18x_BTFSS.Create (STATUS, status_n, access_mode);
                     TPIC18x_INCF.Create (1, dest_f, access_mode)
                  end;
            else
               assert (false)
            end;
      end;   // generate_string_relational_expression_code

   procedure generate_real_relational_expression_code;
      begin
         case left_simple_expression.expression_kind of
            integer_expression:
               assert (false);
            real_expression:
               begin
                  left_simple_expression.Generate (GenerateCode, 4);
                  if TPIC18x_Expression_TypeInfo (left_simple_expression.info).is_ieee_single then
                     convert_tos_from_ieee_to_pic
               end;
         else
            assert (false)
         end;

         case right_simple_expression.expression_kind of
            integer_expression:
               assert (false);
            real_expression:
               begin
                  right_simple_expression.Generate (GenerateCode, 4);
                  if TPIC18x_Expression_TypeInfo (right_simple_expression.info).is_ieee_single then
                     convert_tos_from_ieee_to_pic
               end;
         else
            assert (false)
         end;

         case relop of
            relop_equals:
               TAEQB32.Call;
            relop_notequals:
               TANEB32.Call;
            relop_lt:
               TALTB32.Call;
            relop_gt:
               TAGTB32.Call;
            relop_le:
               TALEB32.Call;
            relop_ge:
               TAGEB32.Call;
         else
            assert (false)
         end
      end;

   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               if (left_simple_expression.expression_kind in ordinal_expression_kinds)
                  and
                  (right_simple_expression.expression_kind in ordinal_expression_kinds) then
                  generate_ordinal_relational_expression_code
               else if (left_simple_expression.expression_kind in ordinal_expression_kinds)
                       and
                       (right_simple_expression.expression_kind = set_expression) then
                  generate_set_relational_expression_code
               else if (left_simple_expression.expression_kind in [char_expression, string_expression])
                       and
                       (right_simple_expression.expression_kind in [char_expression, string_expression]) then
                  generate_string_relational_expression_code
               else if (left_simple_expression.expression_kind in [integer_expression, real_expression])
                       and
                       (right_simple_expression.expression_kind in [integer_expression, real_expression]) then
                  generate_real_relational_expression_code
               else
                  assert (false)
            end;
      else
         assert (false, 'TPIC18x_RelationalExpression.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_SetConstructorPrimary.Generate (param1, param2: integer): integer;

   procedure handle_single_bit_insertion (expr: TExpression);
      var
         i: integer;
         a_size: integer;
         bn1: TPIC18x_conditional_branch;
         bn2: TPIC18x_BN;
         lbl: TAssemblyLabel;
      function beyond_tos_insertion_possible: boolean;
         begin
            result := expr.info.max_value.AsInteger >= ((param2+a_size)*8)
         end;
      function opt_possible: boolean;
         // test for optimization where expr can be popped off stack early and skip ADDFSR instruction at end
         begin
            result := (a_size = 1)
                      and
                      (expr.info.UnSigned)
                      and
                      (not beyond_tos_insertion_possible)
         end;
      begin
         a_size := TPIC18x_TypeInfo(expr.info).Size;
         expr.Generate (GenerateCode, a_size);
         bn1 := nil;
         if (a_size = 1)
            and
            (expr.info.Signed) then
            begin
               TPIC18x_MOVF.Create (1, dest_w, access_mode);
               bn1 := TPIC18x_BN.Create;
            end
         else
            if a_size > 1 then
               begin
                  TPIC18x_MOVF.Create (1, dest_w, access_mode);
                  for i := 2 to a_size-1 do
                     TPIC18x_IORWF.Create (i, dest_w, access_mode);
                  bn1 := TPIC18x_BNZ.Create
               end;
         // if we didn't branch, then [a_size] represents value of expr (0..255)
         TPIC18x_SWAPF.Create (a_size, dest_w, access_mode).annotation := 'insert tos*1 into set*' + IntToStr(param2);
         TPIC18x_RLNCF.Create (WREG, dest_w, access_mode);
         TPIC18x_ANDLW.Create ($1F);
         TPIC18x_SUBLW.Create (param2+a_size-1);

         bn2 := nil;
         if beyond_tos_insertion_possible then
            bn2 := TPIC18x_BN.Create;
               // note: this test will still allow out of range bits to be harmlessly inserted into the expr value on
               //    the top of the stack, but it will prevent any bits being written above the TOS.

         TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
         TPIC18x_MOVWF.Create (FSR1L, access_mode);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
         TPIC18x_MOVWF.Create (FSR1H, access_mode);
         if opt_possible then
            begin
               TPIC18x_RLNCF.Create (PREINC2, dest_w, access_mode);
               StackUsageCounter.Pop(1)
            end
         else
            TPIC18x_RLNCF.Create (a_size, dest_w, access_mode);
         TCallMacro.Create.dest := get_bit_mask_routine;
         TPIC18x_IORWF.Create (PREINC1, dest_f, access_mode);
         if (bn1 <> nil) or (bn2 <> nil) then
            begin
               lbl := TAssemblyLabel.Create;
               if bn1 <> nil then
                  bn1.dest := lbl;
               if bn2 <> nil then
                  bn2.dest := lbl
            end;
         if not opt_possible then
            begin
               TPIC18x_ADDFSR.Create (2, a_size);
               StackUsageCounter.Pop (a_size)
            end
      end;

   procedure handle_ranged_bits_insertion (first, last: TExpression);
      var
         i: integer;
         first_size, last_size: integer;
         bn1: TPIC18x_conditional_branch;
         bn2: TPIC18x_BN;
         bnn: TPIC18x_BNN;
         bnz: TPIC18x_BNZ;
         lbl: TAssemblyLabel;
         b_signed: boolean;
         range_info: TPIC18x_TypeInfo;
      begin
         if first.contains_constant then
            begin
               if first.constant.ordinal_value.ge(param2*8) then
                  exit;

               if last.constant.ordinal_value.lt(0) then
                  exit;

               first_size := 1;
               if first.constant.ordinal_value.lt(0) then
                  TPIC18x_PUSHL.Create (0)
               else
                  TPIC18x_PUSHL.Create (first.constant.ordinal_value.AsByte(0));
               StackUsageCounter.Push(1)
            end
         else   // not a constant
            begin
               first_size := TPIC18x_TypeInfo(first.info).Size;
               first.Generate (GenerateCode, first_size);

               // clamp lower bound at 0
               if first.info.Signed then
                  begin
                     TPIC18x_MOVF.Create (1, dest_w, access_mode);
                     bnn := TPIC18x_BNN.Create;
                     for i := 1 to first_size do
                        TPIC18x_CLRF.Create (i, access_mode);
                     bnn.dest := TAssemblyLabel.Create
                  end
            end;

         // range_info will provide info for last-first calculation
         range_info := TPIC18x_TypeInfo.Create (last.info);
         range_info.max_value.Subtract(first.info.min_value);
         range_info.min_value.Subtract(first.info.max_value);
         last_size := range_info.Size;

         bn1 := nil;
         if last.contains_constant then
            begin
               temp.Assign (last.constant.ordinal_value);
               temp.Min ((param2*8)-1);
               for i := 0 to last_size-1 do
                  TPIC18x_PUSHL.Create (temp.AsByte(i));
               StackUsageCounter.Push(last_size)
            end
         else
            begin
               last.Generate (GenerateCode, last_size);

               b_signed := last.info.Signed;
               if b_signed or last.info.max_value.gt ((param2*8)-1) then
                  begin   // may have to clamp at (param2*8)-1
                     bnz := nil;
                     if b_signed or (last_size > 1) then
                        begin
                           TPIC18x_MOVF.Create (1, dest_w, access_mode);
                           if b_signed then
                              bn1 := TPIC18x_BN.Create;   // if second is negative then no bits will be set
                           if last_size > 1 then
                              begin
                                 for i := 2 to last_size-1 do
                                    TPIC18x_IORWF.Create (i, dest_w, access_mode);
                                 bnz := TPIC18x_BNZ.Create
                              end
                        end;
                     TPIC18x_MOVLW.Create ((param2*8)-1);
                     TPIC18x_SUBWF.Create (last_size, dest_w, access_mode);
                     bn2 := TPIC18x_BN.Create;
                     if bnz <> nil then
                        bnz.dest := TAssemblyLabel.Create;
                     // clamp at (param2*8)-1
                     for i := 1 to last_size-1 do
                         TPIC18x_CLRF.Create (i, access_mode);
                     TPIC18x_MOVLW.Create ((param2*8)-1);
                     TPIC18x_MOVWF.Create (last_size, access_mode);
                     bn2.dest := TAssemblyLabel.Create;
                  end
            end;

         // subtract first from last to produce count
         TPIC18x_MOVF.Create (last_size + first_size, dest_w, access_mode);
         TPIC18x_SUBWF.Create (last_size, dest_f, access_mode);
         i := 1;
         while i < first_size do
            begin
               TPIC18x_MOVF.Create (last_size-i, dest_w, access_mode);
               TPIC18x_SUBWFB.Create (last_size-i, dest_f, access_mode);    // sets N status
               i := i + 1
            end;

         if i < last_size then
            begin
               case TPIC18x_TypeInfo(first.info).IntegerRange of
                  irAlwaysNonNegative:
                     TPIC18x_MOVLW.Create (0);
                  irNegativeOrPositive:
                     begin
                        TPIC18x_MOVLW.Create (0);
                        TPIC18x_BTFSC.Create (last_size+1, 7, access_mode);  // test first's sign bit
                        TPIC18x_MOVLW.Create ($FF)
                     end;
                  irAlwaysNegative:
                     TPIC18x_MOVLW.Create ($FF);
               else
                  assert (false)
               end;
               while i < last_size do
                  begin
                     TPIC18x_SUBWFB.Create (last_size-i, dest_f, access_mode);    // sets N status
                     i := i + 1
                  end
            end;
         bn2 := nil;
         if range_info.Signed then
            bn2 := TPIC18x_BN.Create;

         TPIC18x_INCF.Create (last_size, dest_f, access_mode);

         // load FSR1 using first's lsb value
         TPIC18x_SWAPF.Create (last_size+first_size, dest_w, access_mode);
         TPIC18x_RLNCF.Create (WREG, dest_w, access_mode);
         TPIC18x_ANDLW.Create ($1F);
         TPIC18x_SUBLW.Create (param2+first_size+last_size);
         TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
         TPIC18x_MOVWF.Create (FSR1L, access_mode);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
         TPIC18x_MOVWF.Create (FSR1H, access_mode);

         // load bitmask using first value
         TPIC18x_RLNCF.Create (last_size+first_size, dest_w, access_mode);
         TCallMacro.Create.dest := get_bit_mask_routine;

         lbl := TAssemblyLabel.Create;
         TPIC18x_IORWF.Create (INDF1, dest_f, access_mode);
         TPIC18x_RLNCF.Create (WREG, dest_w, access_mode);
         TPIC18x_BTFSC.Create (WREG, 0, access_mode);
         TPIC18x_SUBFSR.Create (1, 1);
         TPIC18x_DECFSZ.Create (last_size, dest_f, access_mode);
         TPIC18x_BRA.Create.dest := lbl;

         if (bn1 <> nil) or (bn2 <> nil) then
            begin
               lbl := TAssemblyLabel.Create;
               if bn1 <> nil then
                  bn1.dest := lbl;
               if bn2 <> nil then
                  bn2.dest := lbl
            end;
         TPIC18x_ADDFSR.Create (2, first_size + last_size);
         StackUsageCounter.Pop (first_size + last_size);

         range_info.Release
      end;

   var
      i: integer;
      annotation: string;

   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               if constant_members <> nil then
                  begin
                     annotation := 'push set [';
                     for i := 0 to max_set do
                        if i in constant.sett then
                           annotation := annotation + IntToStr(i) + ',';
                     annotation[Length(annotation)] := ']';  // replace final comma (note: annotation is not used if empty set)
                     for i := 0 to param2-1 do
                        begin
                           TPIC18x_PUSHL.Create (SetByte (constant, i)).annotation := annotation;
                           annotation := ''
                        end;
                     StackUsageCounter.Push (param2)
                  end
               else
                  begin
                     annotation := 'push set []';
                     for i := 1 to param2 do
                        begin
                           TPIC18x_PUSHL.Create (0).annotation := annotation;
                           annotation := ''
                        end;
                     StackUsageCounter.Push (param2)
                  end;
               for i := 0 to Length(variable_members)-1 do
                  if variable_members[i].last = nil then
                     handle_single_bit_insertion (variable_members[i].first)
                  else
                     handle_ranged_bits_insertion (variable_members[i].first, variable_members[i].last)
            end;
      else
         assert (false, 'TPIC18x_SetConstructorPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_StrPosPrimary.Generate (param1, param2: integer): integer;
   var
      substr: TPIC18x_VariableAccessPrimary;
      rom_addr: integer;
   begin;
      // initialize result
      TPIC18x_PUSHL.Create(0);
      StackUsageCounter.Push(1);

      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            case access.node_strpos_substr_expression.expression_kind of
               char_expression:
                  begin
                     access.node_strpos_substr_expression.Generate (GenerateCode, 1);
                     case TPIC18x_Access(access).base_variable.descriptor of
                        rw_var,
                        rw_const:
                           begin
                              TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                              StrPosOfCharInRAMStr.Call
                           end;
                        rw_rom:
                           begin
                              TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                              StrPosOfCharInROMStr.Call
                           end;
                        rw_eeprom:
                           begin
                              TPIC18x_Access(access).Generate_Push_Address1_Code (0, false);
                              StrPosOfCharInEEPROMStr.Call
                           end;
                     else
                        assert (false)
                     end
                  end;
               string_expression:
                  if access.node_strpos_substr_expression is TPIC18x_VariableAccessPrimary then
                     begin
                        substr := TPIC18x_VariableAccessPrimary(access.node_strpos_substr_expression);
                        case TPIC18x_Access(access).base_variable.descriptor of
                           rw_var,
                           rw_const:
                              case substr.access.base_variable.descriptor of
                                 rw_var,
                                 rw_const:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfRAMStrInRAMStr.Call
                                    end;
                                 rw_rom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfROMStrInRAMStr.Call
                                    end;
                                 rw_eeprom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address1_Code (0, false);
                                       StrPosOfEEPROMStrInRAMStr.Call
                                    end;
                              else
                                 assert (false)
                              end;
                           rw_rom:
                              case substr.access.base_variable.descriptor of
                                 rw_var,
                                 rw_const:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfRAMStrInROMStr.Call
                                    end;
                                 rw_rom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfROMStrInROMStr.Call
                                    end;
                                 rw_eeprom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address1_Code (0, false);
                                       StrPosOfEEPROMStrInROMStr.Call
                                    end;
                              else
                                 assert (false)
                              end;
                           rw_eeprom:
                              case substr.access.base_variable.descriptor of
                                 rw_var,
                                 rw_const:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address1_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfRAMStrInEEPROMStr.Call
                                    end;
                                 rw_rom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address1_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address2_Code (0, false);
                                       StrPosOfROMStrInEEPROMStr.Call
                                    end;
                                 rw_eeprom:
                                    begin
                                       TPIC18x_Access(access).Generate_Push_Address1_Code (0, false);
                                       TPIC18x_Access(substr.access).Generate_Push_Address1_Code (0, false);
                                       StrPosOfEEPROMStrInEEPROMStr.Call
                                    end;
                              else
                                 assert (false)
                              end;
                        else
                           assert (false)
                        end
                     end
                  else  // substr is anonymous constant
                     begin
                        assert (access.node_strpos_substr_expression is TConstantPrimary);
                        rom_addr := TPIC18x_CPU(target_cpu).anonymous_string_constant_rom_addr (TConstant(TConstantPrimary(access.node_strpos_substr_expression).the_constant).s);
                        case TPIC18x_Access(access).base_variable.descriptor of
                           rw_var,
                           rw_const:
                              begin
                                 TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                 TPIC18x_PUSHL.Create (lsb(rom_addr));
                                 TPIC18x_PUSHL.Create (msb(rom_addr));
                                 StackUsageCounter.Push (2);
                                 StrPosOfROMStrInRAMStr.Call
                              end;
                           rw_rom:
                              begin
                                 TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                                 TPIC18x_PUSHL.Create (lsb(rom_addr));
                                 TPIC18x_PUSHL.Create (msb(rom_addr));
                                 StackUsageCounter.Push (2);
                                 StrPosOfROMStrInROMStr.Call
                              end;
                           rw_eeprom:
                              begin
                                 TPIC18x_Access(access).Generate_Push_Address1_Code (0, false);
                                 TPIC18x_PUSHL.Create (lsb(rom_addr));
                                 TPIC18x_PUSHL.Create (msb(rom_addr));
                                 StackUsageCounter.Push (2);
                                 StrPosOfROMStrInEEPROMStr.Call
                              end;
                        else
                           assert (false)
                        end
                     end;
            else
               assert (false)
            end;   // case access.node_strpos_substr_expression.expression_kind of
      else
         assert (false, 'TPIC18x_SetConstructorPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_SuccFunctionPrimary.Generate (param1, param2: integer): integer;
   var
      result_size, i: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               result_size := TPIC18x_TypeInfo(info).Size;
               expr.Generate (GenerateCode, result_size);
               TPIC18x_INCF.Create (result_size, dest_f, access_mode).annotation := 'tos := succ(tos)';
               if result_size > 1 then
                  begin
                     TPIC18x_MOVLW.Create (0);
                     for i := result_size-1 downto 1 do
                        TPIC18x_ADDWFC.Create (i, dest_f, access_mode)
                  end
            end;
      else
         assert (false, 'TPIC18x_SuccFunctionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_TruncFunctionPrimary.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            raise compile_error.Create (err_use_trunc24_or_trunc32, src_loc);
      else
         assert (false, 'TPIC18x_TruncFunctionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_UnaryMinusPrimary.Generate (param1, param2: integer): integer;
   const
      ARGB0 = 2;
      REAL_SIGN_BIT = 7;
   var
      annotation: string;
      result_size, i: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            case expression_kind of
               integer_expression:
                  begin
                     result_size := TPIC18x_TypeInfo(info).Size;
                     primary.Generate (GenerateCode, result_size);
                     annotation := 'tos := -tos';
                     for i := 1 to result_size-1 do
                        begin
                           TPIC18x_COMF.Create (i, dest_f, access_mode).annotation := annotation;
                           annotation := ''
                        end;
                     TPIC18x_NEGF.Create (result_size, access_mode);
                     if result_size > 1 then
                        begin
                           TPIC18x_MOVLW.Create (0);
                           for i := result_size-1 downto 1 do
                              TPIC18x_ADDWFC.Create (i, dest_f, access_mode)
                        end
                  end;
               real_expression:
                  begin
                     PushRealExpression (primary);
                     TPIC18x_BTG.Create (ARGB0, REAL_SIGN_BIT, access_mode).annotation := 'tos := -tos'
                  end;
            else
               assert (false)
            end;
      else
         assert (false, 'TPIC18x_UnaryMinusPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

type
   TLoadByteFunction = function (do_post_ptr_adjust: boolean): TInstruction of object;


//==================================
//  TPushSingleBitPackedRecordField
//==================================

type
   TPushSingleBitPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         generate_load_next_byte: TLoadByteFunction;
         addr: integer;
         constructor Create (access: TAccess;
                             _generate_load_next_byte: TLoadByteFunction;
                             _addr: integer
                            );
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TPushSingleBitPackedRecordField.Create (access: TAccess;
                                                    _generate_load_next_byte: TLoadByteFunction;
                                                    _addr: integer
                                                   );
   begin
      generate_load_next_byte := _generate_load_next_byte;
      addr := _addr;
      inherited Create (access, true, 1, 0, 'push single bit')
   end;

function TPushSingleBitPackedRecordField.generate_code_segment: TInstruction;
   var
      prfi: TPIC18x_PackedRecordFieldInfo;
   begin
      result := nil;  // to suppress compiler warning
      prfi := TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info);

      if (prfi.Position in [0, 1, 4, 7])
         and
         (TPIC18x_TypeInfo(TPIC18x_Access(access).node_typedef.info).IntegerRange = irAlwaysNonNegative)
      then
         begin
            // do this first since final instr is TPIC18x_MOVWF.Create (POSTDEC2, access_mode) which might be eliminated with peep-hole optimization
            if TPIC18x_Access(access).directly_addressable_absolute_address then
               if addr < 256 then
                  case prfi.Position of
                     0: result := TPIC18x_MOVF.Create (addr, dest_w, bank_mode);
                     1: result := TPIC18x_RRNCF.Create (addr, dest_w, bank_mode);
                     4: result := TPIC18x_SWAPF.Create (addr, dest_w, bank_mode);
                     7: result := TPIC18x_RLNCF.Create (addr, dest_w, bank_mode);
                  else
                     assert (false)
                  end
               else
                  case prfi.Position of
                     0: result := TPIC18x_MOVF.Create (addr, dest_w, access_mode);
                     1: result := TPIC18x_RRNCF.Create (addr, dest_w, access_mode);
                     4: result := TPIC18x_SWAPF.Create (addr, dest_w, access_mode);
                     7: result := TPIC18x_RLNCF.Create (addr, dest_w, access_mode);
                  else
                     assert (false)
                  end
            else if TPIC18x_Access(access).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(TPIC18x_Access(access).node_typedef.info).Size-1) then
               case prfi.Position of
                  0: result := TPIC18x_MOVF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
                  1: result := TPIC18x_RRNCF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
                  4: result := TPIC18x_SWAPF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
                  7: result := TPIC18x_RLNCF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
               else
                  assert (false)
               end
            else if TPIC18x_Access(access).base_variable.descriptor in [rw_var, rw_const, rw_ioreg] then   // will be INDF1
               case prfi.Position of
                  0: result := TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                  1: result := TPIC18x_RRNCF.Create (INDF1, dest_w, access_mode);
                  4: result := TPIC18x_SWAPF.Create (INDF1, dest_w, access_mode);
                  7: result := TPIC18x_RLNCF.Create (INDF1, dest_w, access_mode);
               else
                  assert (false)
               end
            else if TPIC18x_Access(access).base_variable.descriptor = rw_rom then
               begin
                  result := TPIC18x_TBLRD.Create(tblrd);
                  case prfi.Position of
                     0: TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
                     1: TPIC18x_RRNCF.Create (TABLAT, dest_w, access_mode);
                     4: TPIC18x_SWAPF.Create (TABLAT, dest_w, access_mode);
                     7: TPIC18x_RLNCF.Create (TABLAT, dest_w, access_mode);
                  else
                     assert (false)
                  end
               end
            else  // eeprom
               begin
                  assert (TPIC18x_Access(access).base_variable.descriptor = rw_eeprom);
                  result := generate_load_next_byte (false);
                  case prfi.Position of
                     0: {nop};
                     1: TPIC18x_RRNCF.Create (WREG, dest_w, access_mode);
                     4: TPIC18x_SWAPF.Create (WREG, dest_w, access_mode);
                     7: TPIC18x_RLNCF.Create (WREG, dest_w, access_mode)
                  else
                     assert (false)
                  end
               end;
            TPIC18x_ANDLW.Create ($01);
            TPIC18x_MOVWF.Create (POSTDEC2, access_mode)  // might be peephole optimized away
         end
      else  // irNegativeOrPositive or Position [2,3,5,6] or ...
         begin
            result := TPIC18x_PUSHL.Create (0);
            if TPIC18x_Access(access).directly_addressable_absolute_address then
               if addr < 256 then
                  TPIC18x_BTFSC.Create (addr, prfi.Position, bank_mode)
               else
                  TPIC18x_BTFSC.Create (addr, prfi.Position, access_mode)
            else if TPIC18x_Access(access).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(TPIC18x_Access(access).node_typedef.info).Size-1) then
               TPIC18x_BTFSC.Create (addr + 1 + StackUsageCounter.Current, prfi.Position, access_mode)
            else if TPIC18x_Access(access).base_variable.descriptor in [rw_var, rw_const, rw_ioreg] then   // will be INDF1
               TPIC18x_BTFSC.Create (INDF1, prfi.Position, access_mode)
            else if TPIC18x_Access(access).base_variable.descriptor = rw_rom then
               begin
                  TPIC18x_TBLRD.Create(tblrd);
                  TPIC18x_BTFSC.Create (TABLAT, prfi.Position, access_mode)
               end
            else   // eeprom
               begin
                  assert (TPIC18x_Access(access).base_variable.descriptor = rw_eeprom);
                  generate_load_next_byte (false);
                  TPIC18x_BTFSC.Create (WREG, prfi.Position, access_mode)
               end;
            case TPIC18x_TypeInfo(TPIC18x_Access(access).node_typedef.info).IntegerRange of
               irNegativeOrPositive:     // dest is -1..0
                  TPIC18x_DECF.Create (1, dest_f, access_mode);    // $00 -> $FF
               irAlwaysNonNegative:      // dest is 0..1
                  TPIC18x_INCF.Create (1, dest_f, access_mode);    // $00 -> $01
            else
               assert (false)
            end
         end
   end;


//================================
//  TPushMultiBiPackedRecordField
//================================

type
   TPushMultiBitPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         Span: integer;
         LoadNextByte: TLoadByteFunction;
         constructor Create (access: TAccess; _LoadNextByte: TLoadByteFunction);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TPushMultiBitPackedRecordField.Create (access: TAccess; _LoadNextByte: TLoadByteFunction);
   begin
      LoadNextByte := _LoadNextByte;
      Span := TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info).Span;
      if Span = 1 then
         inherited Create (access, true, 1, 0, 'push packed record field')
      else
         inherited Create (access, false, Span, 0,'push packed record field')
   end;

function TPushMultiBitPackedRecordField.generate_code_segment: TInstruction;
   var
      i: integer;
   begin
      assert (Span > 0);
      result := LoadNextByte (1 < Span);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      for i := 2 to Span do
         begin
            LoadNextByte (i < Span);
            TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
         end
   end;


//================================
//  TPIC18x_VariableAccessPrimary
//================================

function TPIC18x_VariableAccessPrimary.load_next_global_ram_byte (do_post_ptr_adjust: boolean): TInstruction;
   begin
      assert ((0 <= addr) and (addr <= 255));    // must be bank 0
      result := TPIC18x_MOVF.Create (addr, dest_w, bank_mode);
      addr := addr - 1
   end;

function TPIC18x_VariableAccessPrimary.load_next_near_stack_byte (do_post_ptr_adjust: boolean): TInstruction;
   begin
      assert ((1 <= addr + StackUsageCounter.Current) and (addr + StackUsageCounter.Current <= $5F));    // must be near
      result := TPIC18x_MOVF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
      addr := addr - 1
   end;

function TPIC18x_VariableAccessPrimary.load_next_ram_byte_indirect_via_F1 (do_post_ptr_adjust: boolean): TInstruction;
   begin
      result := TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode)   // sets status
   end;

function TPIC18x_VariableAccessPrimary.load_next_rom_byte (do_post_ptr_adjust: boolean): TInstruction;
   begin
      result := TPIC18x_TBLRD.Create(tblrd_post_dec);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode)  // sets status
   end;

function TPIC18x_VariableAccessPrimary.load_next_eeprom_byte (do_post_ptr_adjust: boolean): TInstruction;
   begin
      result := GetEEPROMByte.Call        // decrements FSR1
   end;

function TPIC18x_VariableAccessPrimary.load_next_global_ram_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
   begin
      if (0 <= addr) and (addr <= 255) then
         result := TPIC18x_MOVF.Create (addr, dest_w, bank_mode)
      else  // access bank ioreg
         result := TPIC18x_MOVF.Create (addr, dest_w, access_mode);
      if TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order then
         addr := addr + 1
      else
         addr := addr - 1
   end;

function TPIC18x_VariableAccessPrimary.load_next_near_stack_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
   begin
      assert ((1 <= addr + StackUsageCounter.Current) and (addr + StackUsageCounter.Current<= $5F));    // must be near
      result := TPIC18x_MOVF.Create (addr + StackUsageCounter.Current, dest_w, access_mode);
      if TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order then
         addr := addr + 1
      else
         addr := addr - 1
   end;

function TPIC18x_VariableAccessPrimary.load_next_ram_byte_indirect_via_F1_packed (do_post_ptr_adjust: boolean): TInstruction;
   begin
      if TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order then
         result := TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode)   // sets status
      else
         result := TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode)   // sets status
   end;

function TPIC18x_VariableAccessPrimary.load_next_rom_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
   begin
      if TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order then
         result := TPIC18x_TBLRD.Create(tblrd_post_inc)
      else
         result := TPIC18x_TBLRD.Create(tblrd_post_dec);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode)  // sets status
   end;

function TPIC18x_VariableAccessPrimary.load_next_eeprom_byte_packed (do_post_ptr_adjust: boolean): TInstruction;
   begin
      result := GetEEPROMByte.Call;        // decrements FSR1
      if (TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order)
         and
         (do_post_ptr_adjust)
      then
         TPIC18x_ADDFSR.Create(1, 2)          // net result is increment FSR1
   end;

function TPIC18x_VariableAccessPrimary.Generate (param1, param2: integer): integer;
   var
      annotation: string;

   procedure push_variable (generate_load_next_byte: TLoadByteFunction);
      var
         i, last: integer;
         bnn: TPIC18x_BNN;
         bra: TPIC18x_BRA;
         tos_size: integer;
         tos_type_info: TTypeInfo;
      begin
         if TPIC18x_Access(access).is_overlay_variable_needing_range_check then
            begin
               tos_size := TPIC18x_TypeInfo(access.node_typedef.info).Size;
               for i := 1 to tos_size do
                  begin
                     generate_load_next_byte (i < tos_size).annotation := annotation;
                     annotation := '';
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1)
                  end;
               // construct a type_info to represent TOS bytes with any bit pattern possible
               tos_type_info := TTypeInfo.Create (access.node_typedef.info);   // pick up desired signedness
               if tos_type_info.Signed then
                  begin
                     tos_type_info.min_value.SetMinSignedValue (tos_size * 8);
                     tos_type_info.max_value.SetMaxSignedValue (tos_size * 8)
                  end
               else  // unsigned
                  begin
                     tos_type_info.min_value.AsInteger := 0;
                     tos_type_info.max_value.SetMaxUnsignedValue (tos_size * 8)
                  end;
               GenerateRangeCheckCode (TOrdinalDataType(access.node_typedef),
                                       tos_size,
                                       tos_type_info,
                                       src_loc,
                                       rterr_assignment_range_check_error (TOrdinalDataType(access.node_typedef))
                                      );
               tos_type_info.Release;
               generate_stack_fix_and_sign_extend_code (tos_size, 0, param2, access.node_typedef.info.IntegerRange)
            end
         else if access.node_typedef.IsOrdinal then
            begin
               for i := 1 to TPIC18x_TypeInfo(access.node_typedef.info).Size do
                  if i <= param2 then
                     begin
                        generate_load_next_byte (i < param2).annotation := annotation;
                        annotation := '';
                        TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                        StackUsageCounter.Push (1)
                     end;
               if TOrdinalDataType(access.node_typedef).info.min_value.AsInteger >= 0 then
                  begin
                     for i := TPIC18x_TypeInfo(access.node_typedef.info).Size+1 to param2 do
                        if i <= param2 then
                           begin
                              TPIC18x_CLRF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push (1)
                           end
                  end
               else if TOrdinalDataType(access.node_typedef).info.max_value.AsInteger < 0 then
                  begin
                     for i := TPIC18x_TypeInfo(access.node_typedef.info).Size+1 to param2 do
                        if i <= param2 then
                           begin
                              TPIC18x_SETF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push (1)
                           end
                  end
               else  // value could be pos or neg
                  if param2 > TPIC18x_TypeInfo(access.node_typedef.info).Size then
                     begin  // extend sign
                        bnn := TPIC18x_BNN.Create;
                        for i := TPIC18x_TypeInfo(access.node_typedef.info).Size+1 to param2 do
                           TPIC18x_SETF.Create (POSTDEC2, access_mode);
                        bra := TPIC18x_BRA.Create;
                        bnn.dest := TPIC18x_CLRF.Create (POSTDEC2, access_mode);
                        for i := TPIC18x_TypeInfo(access.node_typedef.info).Size+2 to param2 do
                           TPIC18x_CLRF.Create (POSTDEC2, access_mode);
                        bra.dest := TAssemblyLabel.Create;
                        StackUsageCounter.Push (param2 - TPIC18x_TypeInfo(access.node_typedef.info).Size)
                     end
            end
         else if access.node_typedef.type_kind = set_type then
            begin
               last := min(param2, TPIC18x_TypeInfo(access.node_typedef.info).Size);
               for i := 1 to last do
                  begin
                     generate_load_next_byte (i < last).annotation := annotation;
                     annotation := '';
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1)
                  end;
               for i := TPIC18x_TypeInfo(access.node_typedef.info).Size+1 to param2 do
                  begin
                     TPIC18x_CLRF.Create (POSTDEC2, access_mode).annotation := annotation;
                     StackUsageCounter.Push (1);
                     annotation := ''
                  end
            end
         else
            begin
               assert (param2 = TPIC18x_TypeInfo(access.node_typedef.info).Size);
               for i := 1 to param2 do
                  begin
                     generate_load_next_byte (i < param2).annotation := annotation;
                     annotation := '';
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1)
                  end
            end
      end;

   procedure push_packed_variable (generate_load_next_byte: TLoadByteFunction);
      type
         byte_array = array [0..7] of byte;
      const
         upper_byte_mask: byte_array = ($FF, $01, $03, $07, $0F, $1F, $3F, $7F);
      var
         i, j: integer;
         bra1, bra2: TPIC18x_BRA;
         tos_size: integer;
      begin
         with TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info) do
            begin
               if access.node_typedef.info.PackedSizeInBits = 1 then
                  begin
                     tos_size := 1;
                     TPushSingleBitPackedRecordField.Create (access, generate_load_next_byte, addr)
                  end
               else   // PackedSizeInBits > 1
                  begin
                     tos_size := TPIC18x_TypeInfo(access.node_typedef.info).Size;
                     TPushMultiBitPackedRecordField.Create (access, generate_load_next_byte);

                     // normalize right
                     if (Span = 1)
                        and
                        (Position >= 4)
                        and
                        (Position - Width >= 3)
                     then
                        begin
                           TPIC18x_SWAPF.Create (1, dest_f, access_mode);
                           for i := 1 to Position - Width - 3 do
                              TPIC18x_RRCF.Create (1, dest_f, access_mode)
                        end
                     else
                        for i := 1 to (Position - (Width and $07) + 1) and $07 do
                           for j := 1 to Span do
                              TPIC18x_RRCF.Create (j, dest_f, access_mode);

                     // cut back stack after all relevant bits were shifted out of tos
                     case Span - tos_size of
                        0: { nop };
                        1: begin
                              TPIC18x_ADDFSR.Create (2, 1);
                              StackUsageCounter.Pop (1)
                           end;
                     else
                        assert (false)   // should never have to discard more than one byte
                     end;

                     // mask or sign extend unused upper bits
                     if Width mod 8 <> 0 then
                        case TPIC18x_TypeInfo(access.node_typedef.info).IntegerRange of
                           irAlwaysNegative:
                              begin
                                 TPIC18x_MOVLW.Create ((not upper_byte_mask[Width mod 8]) and $ff);
                                 TPIC18x_IORWF.Create (1, dest_f, access_mode)
                              end;
                           irNegativeOrPositive:
                              begin
                                 TPIC18x_BTFSS.Create (1, (Width-1) mod 8, access_mode);
                                 bra1 := TPIC18x_BRA.Create;
                                 TPIC18x_MOVLW.Create ((not upper_byte_mask[Width mod 8]) and $ff);
                                 TPIC18x_IORWF.Create (1, dest_f, access_mode);
                                 bra2 := TPIC18x_BRA.Create;
                                 bra1.dest := TPIC18x_MOVLW.Create (upper_byte_mask[Width mod 8]);
                                 TPIC18x_ANDWF.Create (1, dest_f, access_mode);
                                 bra2.dest := TAssemblyLabel.Create
                              end;
                           irAlwaysNonNegative:
                              begin
                                 TPIC18x_MOVLW.Create (upper_byte_mask[Width mod 8]);
                                 TPIC18x_ANDWF.Create (1, dest_f, access_mode)
                              end;
                        else
                           assert (false)
                        end
                  end
            end;

         if TPIC18x_Access(access).is_overlay_variable_needing_range_check then
            begin
//re-think range check here....
//               tos_type_info := TTypeInfo.Create (access.node_typedef.info);   // pick up desired signedness
//               if tos_type_info.Signed then
//                  begin
//                     tos_type_info.min_value.SetMinSignedValue (tos_size * 8);
//                     tos_type_info.max_value.SetMaxSignedValue (tos_size * 8)
//                  end
//               else  // unsigned
//                  begin
//                     tos_type_info.min_value.AsInteger := 0;
//                     tos_type_info.max_value.SetMaxUnsignedValue (tos_size * 8)
//                  end;
//               GenerateRangeCheckCode (TOrdinalDataType(access.node_typedef),
//                                       tos_size,
//                                       tos_type_info,
//                                       src_loc,
//                                       rterr_assignment_range_check_error (TOrdinalDataType(access.node_typedef))
//                                      );
//               tos_type_info.Release
            end;

         generate_stack_fix_and_sign_extend_code (tos_size,
                                                  0,
                                                  param2,
                                                  TPIC18x_TypeInfo(access.node_typedef.info).IntegerRange
                                                 )
      end;

   var
      offset: integer;

   begin   // TPIC18x_VariableAccessPrimary.Generate
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               annotation := 'push ' + lex.identifiers[access.node_id_idx] + '*' + IntToStr(param2);
               if access.base_variable.is_ioreg_1bit_param then
                  begin
                     TPIC18x_Access(access).Generate_Push_Address2_Code (0, false);
                     push_ioreg_1bit_param_Subroutine.Call.annotation := 'push ioreg bit'
                  end
               else if access.node_is_packed_field then
                  begin
                     if TPIC18x_PackedRecordFieldInfo(access.node_packed_record_field.info).reversed_byte_order then
                        offset := TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info).Offset
                                  -
                                  TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info).Span
                                  +
                                  1
                     else  // normal byte order
                        offset := TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info).Offset
                                  +
                                  TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(access).node_packed_record_field.info).Span
                                  -
                                  1;
                     case access.base_variable.descriptor of
                        rw_var,
                        rw_const,
                        rw_ioreg:
                           if TPIC18x_Access(access).directly_addressable_absolute_address then
                              begin
                                 addr := TPIC18x_Access(access).absolute_address(offset);
                                 push_packed_variable (load_next_global_ram_byte_packed)
                              end
                           else if TPIC18x_Access(access).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(access.node_typedef.info).Size-1) then
                              begin
                                 addr := TPIC18x_Access(access).base_variable.address + TPIC18x_Access(access).total_fixed_offsets + offset;
                                 push_packed_variable (load_next_near_stack_byte_packed)
                              end
                           else
                              begin
                                 TPIC18x_Access(access).Generate_Load_Ptr2_Code (pFSR1, offset);
                                 push_packed_variable (load_next_ram_byte_indirect_via_F1_packed)
                              end;
                        rw_rom:
                           begin
                              TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTBLPTR, offset);
                              push_packed_variable (load_next_rom_byte_packed)
                           end;
                        rw_eeprom:
                           begin
                              TPIC18x_Access(access).Generate_Load_Ptr1_Code (pFSR1, offset);
                              push_packed_variable (load_next_eeprom_byte_packed)
                           end;
                     else
                        assert (false)
                     end
                  end
               else  // non-packed variable
                  case access.base_variable.descriptor of
                     rw_var,
                     rw_const,
                     rw_for:
                        if TPIC18x_Access(access).absolute_address_with_no_indexing_required_mode then
                           begin
                              addr := TPIC18x_Access(access).absolute_address(TPIC18x_TypeInfo(access.node_typedef.info).Size-1);
                              push_variable (load_next_global_ram_byte)
                           end
                        else if TPIC18x_Access(access).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(access.node_typedef.info).Size-1) then
                           begin
                              addr := TPIC18x_Access(access).base_variable.address + TPIC18x_Access(access).total_fixed_offsets + TPIC18x_TypeInfo(access.node_typedef.info).Size-1;
                              push_variable (load_next_near_stack_byte)
                           end
                        else
                           begin
                              TPIC18x_Access(access).Generate_Load_Ptr2_Code (pFSR1, TPIC18x_TypeInfo(access.node_typedef.info).Size-1);
                              push_variable (load_next_ram_byte_indirect_via_F1)
                           end;
                     rw_rom:
                        begin
                           TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTBLPTR, TPIC18x_TypeInfo(access.node_typedef.info).Size-1);
                           push_variable (load_next_rom_byte)
                        end;
                     rw_eeprom:
                        begin
                           TPIC18x_Access(access).Generate_Load_Ptr1_Code (pFSR1, TPIC18x_TypeInfo(access.node_typedef.info).Size-1);
                           push_variable (load_next_eeprom_byte)
                        end;
                     rw_ioreg:
                        assert (false, 'all ioreg vars should be packed');
                  else
                     assert (false)
                  end
            end;

         GenerateCodeToCopyToRAMString:
            case access.base_variable.descriptor of
               rw_var,
               rw_const:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr2_Code (pFSR1, 0);
                     TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);   // get length & set Z if 0
                     AssignRAMStrToRAMStr.Call (src_loc)
                  end;
               rw_rom:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                     TPIC18x_TBLRD.Create (tblrd_post_inc);
                     TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
                     AssignROMStrToRAMStr.Call (src_loc)
                  end;
               rw_eeprom:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr1_Code (pFSR1, 0);
                     GetEEPROMByte.Call;    // decrements FSR1
                     TPIC18x_ADDFSR.Create (1, 2);     // net result is increment FSR1
                     AssignEEPROMStrToRAMStr.Call (src_loc)
                  end;
            else
               assert (false)
            end;

         GenerateCodeToCopyToEEPROMString:
            case access.base_variable.descriptor of
               rw_var,
               rw_const:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr2_Code (pFSR1, 0);
                     TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);   // get length & set Z if 0
                     AssignRAMStrToEEPROMStr.Call (src_loc)
                  end;
               rw_rom:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                     TPIC18x_TBLRD.Create (tblrd_post_inc);
                     TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
                     AssignROMStrToEEPROMStr.Call (src_loc)
                  end;
               rw_eeprom:
                  begin
                     TPIC18x_Access(access).Generate_Load_Ptr1_Code (pFSR1, 0);
                     GetEEPROMByte.Call;    // decrements FSR1
                     TPIC18x_ADDFSR.Create (1, 2);     // net result is increment FSR1
                     AssignEEPROMStrToEEPROMStr.Call (src_loc)
                  end;
            else
               assert (false)
            end;

      else
         assert (false, 'TPIC18x_VariableAccessPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;      // TPIC18x_VariableAccessPrimary.Generate

procedure PushRealExpression (expr: TExpression);
   begin
      if expr is TPIC18x_ConstantPrimary then
         TPIC18x_ConstantPrimary(expr).PushRealConstant
      else
         case expr.expression_kind of
            integer_expression:
               generate_integer_expression_to_real_code (expr);
            real_expression:
               begin
                  expr.Generate (GenerateCode, real_size);
                  if TPIC18x_Expression_TypeInfo(expr.info).is_ieee_single then
                     convert_tos_from_ieee_to_pic
               end
         else
            assert (false)
         end;
   end;

INITIALIZATION
   temp := TMultiPrecisionInteger.Create;
   push_ioreg_1bit_param_Subroutine := Tpush_ioreg_1bit_param_Subroutine.Create (0, 1, 'push ioreg 1-bit parameter');

FINALIZATION
   temp.Free;
   push_ioreg_1bit_param_Subroutine.Free;

END.
