UNIT pic18x_statements_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes,

   cpc_access_unit,
   cpc_core_objects_unit,
   cpc_source_analysis_unit,
   cpc_statements_unit,

   pic18x_instructions_unit;

type
   TPIC18x_CaseStatement =
      class (TCaseStatement)
      private type
         TPIC18x_CaseEntry =
            class (TCaseStatement.TCaseLabelRangeEntry)
               function GenerateCode: TInstruction;
            end;
      protected
         function create_case_entry (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer): TCaseStatement.TCaseLabelRangeEntry;
            override;
      public
         selection_expression_size: integer;
         come_from_list: array of TBranchTarget;
         br_otherwise_list: TBranchTarget;
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_CycleStatement =
      class (TCycleStatement)
         start_loop_label: TInstruction;
         initial_stack_level: integer;
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_ExitLoopStatement =
      class (TExitLoopStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_ForStatement =
      class (TForStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_IfStatement =
      class (TIfStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_InitStatement =
      class (TInitStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_LoopStatement =
      class (TLoopStatement)
         initial_stack_level: integer;
         start_loop_label: TInstruction;
         end_loop: TBranchTarget;
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_ReCycleStatement =
      class (TReCycleStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_ReLoopStatement =
      class (TReLoopStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_RoutineCallStatement =
      class (TRoutineCallStatement)
         subtest: integer;  // used by Test
         error_message: string;     // used by SetError and Test
         constructor CreateFromSourceTokens
            (acc: TAccess
            );
         constructor Create (acc: TAccess; exp: TExpression; _src_loc: TSourceLocation);
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_StatementList =
      class (TStatementList)
         function Generate (param1, param2: integer): integer;
            override;
      end;

    TPIC18x_UntilStatement =
      class (TUntilStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

    TPIC18x_WhileStatement =
      class (TWhileStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_WithProperty =
      class (TWithProperty)
      end;

   TPIC18x_WithRoutine =
      class (TWithRoutine)
      end;

   TPIC18x_WithStatement =
      class (TWithStatement)
         address: integer;
         function Generate (param1, param2: integer): integer;
            override;
      end;

   TPIC18x_WithVariable =
      class (TWithVariable)
      end;

   function err_timer_cycle_count_exceeded (timer_number: integer): string;


IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
  test_pic18x_kernel_unit,
{$endif}
  SysUtils, Math, pic18x_expressions_unit, pic18x_core_objects_unit, wirth_balanced_binary_tree_unit,
   cpc_blocks_unit,
   cpc_expressions_unit,
   cpc_constant_expression_unit,
   pic18x_cpu_unit,
   pic18x_macro_instructions_unit,
  pic18x_microprocessor_information_unit, pic18x_run_time_error_check_unit, pic18x_access_unit,
  pic18x_blocks_unit, cpc_target_cpu_unit, cpc_multi_precision_integer_unit,
  pic18x_kernel_unit, pic18x_string_unit, pic18x_types_unit, cpc_common_unit, cpc_definitions_unit,
  pic18x_assignment_statement_unit;

var
   expression_lower_fence, expression_upper_fence, temp: TMultiPrecisionInteger;

function generate_bit_test_conditional_branch_code (access: TPIC18x_Access;  bit_sense: boolean): TInstruction;
   // generates branch if bit = bit_sense code
   // result is the branch instruction for filling in destination field later
   procedure gen_skip_instruction (addr, bit_position: integer; mode: TPIC18x_RAM_Access_Mode);
      begin
         case bit_sense of
            false: TPIC18x_BTFSS.Create (addr, bit_position, mode);
            true:  TPIC18x_BTFSC.Create (addr, bit_position, mode);
         end
      end;
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
            gen_skip_instruction (access.absolute_address (offset),
                                  bit_num,
                                  bank_mode
                                 )
         else
            gen_skip_instruction (access.absolute_address (offset),
                                  bit_num,
                                  access_mode
                                 )
      else if access.near_stack_address_with_no_indexing_required_mode6 (offset) then
         gen_skip_instruction (access.near_stack_address(offset),
                               bit_num,
                               access_mode
                              )
      else
         begin
            access.Generate_Load_Ptr2_Code (pFSR1, typinfo.Size-1+Offset);
            gen_skip_instruction (INDF1,
                                  bit_num,
                                  access_mode
                                 )
         end;
      result := TGOTOMacro.Create
   end;

function GenerateCodeForConditionalBranch (boolean_expression: TExpression; branch_on_sense: boolean): TInstruction;
   // result is branch instruction, result.dest is to be filled in later

   function can_be_evaluated_with_simple_bit_test (access: TPIC18x_Access): boolean;
      begin
         result := (access.node_is_in_data_address_space)
                   and
                   (access.node_typedef.info.PackedSizeInBits = 1)
                   and
                   (not ((access.base_variable.descriptor = rw_ioreg)
                         and
                         (TPIC18x_TypeInfo(access.base_variable.typedef.info).is_in_alternate_shared_address_space)
                        )
                   )
      end;
   var
      rel_exp: TPIC18x_RelationalExpression;
      access: TPIC18x_Access;
   begin
      if boolean_expression is TPIC18x_VariableAccessPrimary then
         begin
            access := TPIC18x_Access (TPIC18x_VariableAccessPrimary(boolean_expression).access);
            if can_be_evaluated_with_simple_bit_test (access) then
               begin
                  result := generate_bit_test_conditional_branch_code (access, branch_on_sense);
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
                  if can_be_evaluated_with_simple_bit_test (access) then
                     case rel_exp.right_simple_expression.constant.AsOrdinal of
                        0: case rel_exp.relop of
                              relop_equals,
                              relop_le:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, not branch_on_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_gt:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, branch_on_sense);
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
                                    result := generate_bit_test_conditional_branch_code (access, branch_on_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_lt:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, not branch_on_sense);
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
                  if can_be_evaluated_with_simple_bit_test (access) then
                     case rel_exp.left_simple_expression.constant.AsOrdinal of
                        0: case rel_exp.relop of
                              relop_equals,
                              relop_ge:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, not branch_on_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_lt:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, branch_on_sense);
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
                                    result := generate_bit_test_conditional_branch_code (access, branch_on_sense);
                                    exit
                                 end;
                              relop_notequals,
                              relop_gt:
                                 begin
                                    result := generate_bit_test_conditional_branch_code (access, not branch_on_sense);
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
      if branch_on_sense then
         result := TBranchOnTOSTrueMacro.Create
      else
         result := TBranchOnTOSFalseMacro.Create;
      StackUsageCounter.Pop(1)
   end;

function TPIC18x_CaseStatement.TPIC18x_CaseEntry.GenerateCode: TInstruction;
   var
      selection_expression_size: integer;     // local copy of case statements's
   function sign_adjust (constant: TMultiPrecisionInteger; byte_no: integer): byte;
      begin
         result := constant.AsByte (byte_no);
         if (case_stmt.selection_expression.info.Signed)
            and
            (byte_no = selection_expression_size-1) then
            result := result xor $80    // flip sign bit to upshift signed to unsigned
      end;
   var
      i: integer;
      instr: TInstruction;
   begin
      // assert: lower_fence < first_of_range <= last_of_range < upper_fence
      result := nil;
      selection_expression_size := TPIC18x_CaseStatement(case_stmt).selection_expression_size;
      if left_test_needed then
         begin
            temp.AsInteger := first_of_range;
            result := TPIC18x_MOVLW.Create (sign_adjust (temp, 0));
            TPIC18x_SUBWF.Create (selection_expression_size, dest_w, access_mode);
            for i := 1 to selection_expression_size-1 do
               begin
                  TPIC18x_MOVLW.Create (sign_adjust (temp, i));
                  TPIC18x_SUBWFB.Create (selection_expression_size-i, dest_w, access_mode)
               end;
            // N status set, ** - Z status is valid if selection_expression_size=1

            if lesser_values <> nil then
               begin
                  if (not TCaseLabelRangeEntry(lesser_values).left_test_needed)
                     and
                     (not TCaseLabelRangeEntry(lesser_values).right_test_needed)
                  then
                     TPIC18x_CaseStatement(case_stmt).come_from_list[TCaseLabelRangeEntry(lesser_values).labeled_statement_idx].ComeFrom (TBranchOnNegativeStatusMacro.Create)
                  else
                     begin
                        instr := TBranchOnNonNegativeStatusMacro.Create;
                        TPIC18x_CaseEntry(lesser_values).GenerateCode;
                        instr.dest := TAssemblyLabel.Create
                     end
               end
            else
               TPIC18x_CaseStatement(case_stmt).br_otherwise_list.ComeFrom (TBranchOnNegativeStatusMacro.Create)
         end;

      if right_test_needed then
         begin
            if (selection_expression_size = 1)
               and
               (first_of_range = last_of_range) then
               begin
                  // z status still valid from above **
                  instr := TBranchOnZeroStatusMacro.Create;
                  if result = nil then
                     result := instr;
                  TPIC18x_CaseStatement(case_stmt).come_from_list[labeled_statement_idx].ComeFrom (instr)
               end
            else
               begin
                  temp.AsInteger := last_of_range + 1;
                  instr := TPIC18x_MOVLW.Create (sign_adjust (temp, 0));
                  if result = nil then
                     result := instr;
                  TPIC18x_SUBWF.Create (selection_expression_size, dest_w, access_mode);
                  for i := 1 to selection_expression_size-1 do
                     begin
                        TPIC18x_MOVLW.Create (sign_adjust (temp, i));
                        TPIC18x_SUBWFB.Create (selection_expression_size-i, dest_w, access_mode)
                     end;
                  TPIC18x_CaseStatement(case_stmt).come_from_list[labeled_statement_idx].ComeFrom (TBranchOnNegativeStatusMacro.Create)
               end;
            if greater_values = nil then
               TPIC18x_CaseStatement(case_stmt).br_otherwise_list.ComeFrom (TGotoMacro.Create)
            else
               TPIC18x_CaseEntry(greater_values).GenerateCode
        end
      else
         begin
            instr := TGotoMacro.Create;
            if result = nil then
               result := instr;
            TPIC18x_CaseStatement(case_stmt).come_from_list[labeled_statement_idx].ComeFrom (instr)
         end
   end;

function TPIC18x_CaseStatement.Generate (param1, param2: integer): integer;
   var
      i: integer;
      br_end_list: TBranchTarget;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               SetLength (come_from_list, Length(labeled_statements));
               for i := 0 to Length(labeled_statements)-1 do
                  come_from_list[i] := TBranchTarget.Create;
               br_end_list := TBranchTarget.Create;
               br_otherwise_list := TBranchTarget.Create;

               TSourceSyncPoint.Create (of_src_loc);

               selection_expression_size := TPIC18x_TypeInfo(selection_expression.info).Size;
               selection_expression.Generate (GenerateCode, selection_expression_size);
               if selection_expression.info.Signed then
                  TPIC18x_BTG.Create (1, 7, access_mode);    // flip sign bit to upshift signed to unsigned

               TPIC18x_CaseEntry(case_label_range_tree.root).GenerateCode.annotation := 'do case branching';

               for i := 0 to Length(labeled_statements)-1 do
                  begin
                     TSourceSyncPoint.Create (labeled_statements[i].colon_src_loc);
                     come_from_list[i].target_label := TAssemblyLabel.Create;
                     labeled_statements[i].statement.Generate (GenerateCode, 0);
                     if not ((i = Length(labeled_statements)-1)
                             and
                             (not br_otherwise_list.has_clients)
                            ) then
                        br_end_list.ComeFrom (TGotoMacro.Create)
                  end;

               if otherwise_statement <> nil then
                  TSourceSyncPoint.Create (otherwise_src_loc)
               else
                  TSourceSyncPoint.Create (end_src_loc);
               if br_otherwise_list.has_clients then
                  begin
                     br_otherwise_list.target_label := TAssemblyLabel.Create;
                     if otherwise_statement <> nil then
                        otherwise_statement.Generate (GenerateCode, 0)
                     else
                        begin
                           set_errorcode_routine.Call.annotation := rterr_bad_case_index;
                           RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_bad_case_index, selection_expression.src_loc)
                        end
                  end;

               TSourceSyncPoint.Create (end_src_loc);
               br_end_list.target_label := TPIC18x_ADDFSR.Create (2, selection_expression_size);
               StackUsageCounter.Pop (selection_expression_size);

               for i := 0 to Length(labeled_statements)-1 do
                  come_from_list[i].set_client_destinations;
               br_otherwise_list.set_client_destinations;
               br_end_list.set_client_destinations;

               for i := 0 to Length(labeled_statements)-1 do
                  come_from_list[i].Free;
               br_otherwise_list.Free;
               br_end_list.Free
            end;
      else
         assert (false, 'TPIC18x_CaseStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_CaseStatement.create_case_entry (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer): TCaseStatement.TCaseLabelRangeEntry;
   begin
      result := TPIC18x_CaseEntry.create (_case_stmt, _labeled_statement_idx, _first_of_range, _last_of_range)
   end;

function TPIC18x_CycleStatement.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            if not is_empty_loop_at_end_of_program_initial_statement then
               begin
                  TSourceSyncPoint.Create (src_loc);
                  start_loop_label := TAssemblyLabel.Create;
                  initial_stack_level := StackUsageCounter.Current;
                  statement_list.Generate (GenerateCode, 0);
                  TSourceSyncPoint.Create (repeat_token_src_loc);
                  TGOTOMacro.Create.dest := start_loop_label
               end;
      else
         assert (false, 'TPIC18x_CycleStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ExitLoopStatement.Generate (param1, param2: integer): integer;
   var
      br: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               if exitloop_condition = nil then
                  begin
                     if StackUsageCounter.Current > TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level then
                        TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement (containing_loop_statement).initial_stack_level);
                     TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (TGOTOMacro.Create)
                  end
               else if StackUsageCounter.Current = TPIC18x_LoopStatement (containing_loop_statement).initial_stack_level then
                  TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (GenerateCodeForConditionalBranch (exitloop_condition, true))
               else
                  begin
                     br := GenerateCodeForConditionalBranch (exitloop_condition, false);
                     TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level);
                     TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (TGOTOMacro.Create);
                     br.dest := TAssemblyLabel.Create
                  end
            end;
      else
         assert (false, 'TPIC18x_ExitLoopStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ForStatement.Generate (param1, param2: integer): integer;

   procedure clamp_to_range_of_control_variable (range: TTypeInfo);
      begin
         range.max_value.min (control_variable.TypeDef.info.max_value);
         range.min_value.max (control_variable.TypeDef.info.min_value)
      end;

   var
      comparison_range, temp_range: TPIC18x_TypeInfo;    // range of loop_count-1
      comparison_size: integer;
      control_variable_size: integer;
      initial_value_expression_size: integer;
      final_value_expression_size: integer;
      i: integer;
      go2: TInstruction;
      be1, be2: TInstruction;
      loop: TAssemblyLabel;
      end_loop: TInstruction;
      annotation: string;
      br_array: array of TInstruction;
      lbl: TAssemblyLabel;
      control_variable_access: TPIC18x_Access;
      stmt: TPIC18x_AssignmentStatement;
      loop_control_mode: (lcm_bank0, lcm_near_stack, lcm_indirect_single_byte, lcm_indirect_multi_byte);
      control_variable_pointer_address: integer;
   begin   // TPIC18x_ForStatement.Generate
      result := 0;  // to suppress compiler warning

      case param1 of
         GenerateCode:
            begin
               comparison_range := nil;  // to suppress compiler warning
               temp_range := nil;        // to suppress compiler warning
               be1 := nil;               // to suppress compiler warning

               TSourceSyncPoint.Create (do_src_loc);

               case step of
                  increment_control_variable:
                     begin
                        comparison_range := TPIC18x_TypeInfo (target_cpu.TTypeInfo_Create (final_value_expression.info));
                        clamp_to_range_of_control_variable (comparison_range);  // this will be done by bounds check of final val expression
                        temp_range := TPIC18x_TypeInfo (target_cpu.TTypeInfo_Create (initial_value_expression.info));
                        clamp_to_range_of_control_variable (temp_range);  // this will be done by bounds check of initial val expression
                        comparison_range.max_value.Subtract (temp_range.min_value);
                        comparison_range.min_value.Subtract (temp_range.max_value)
                     end;
                  decrement_control_variable:
                     begin
                        comparison_range := TPIC18x_TypeInfo (target_cpu.TTypeInfo_Create (initial_value_expression.info));
                        clamp_to_range_of_control_variable (comparison_range);  // this will be done by bounds check of final val expression
                        temp_range := TPIC18x_TypeInfo (target_cpu.TTypeInfo_Create (final_value_expression.info));
                        clamp_to_range_of_control_variable (temp_range);  // this will be done by bounds check of initial val expression
                        comparison_range.max_value.Subtract (temp_range.min_value);
                        comparison_range.min_value.Subtract (temp_range.max_value)
                     end
               end;
               if (statement = nil)
                  or
                  comparison_range.max_value.lt(0) then
                  begin    // loop is nop, exit without emitting any code
                     comparison_range.Release;
                     temp_range.Release;
                     exit
                  end;

               control_variable_access := TPIC18x_Access.CreateFromVariable (control_variable);
               control_variable_size := TPIC18x_TypeInfo(control_variable.typedef.info).Size;
               comparison_size := max (control_variable_size, comparison_range.Size);

               case control_variable.address_mode of
                  absolute_address_mode:
                     if control_variable.address + control_variable_size < $100 then
                        loop_control_mode := lcm_bank0
                     else if control_variable_size = 1 then
                        loop_control_mode := lcm_indirect_single_byte
                     else
                        loop_control_mode := lcm_indirect_multi_byte;
                  local_address_mode:
                     if (final_value_expression.contains_constant)
                        and
                        (control_variable.address + (control_variable_size-1) + StackUsageCounter.Current <= $5F)
                     then
                        loop_control_mode := lcm_near_stack
                     else if (not final_value_expression.contains_constant)
                             and
                             (control_variable.address + (control_variable_size-1) + StackUsageCounter.Current + (2*comparison_size) <= $5F)
                          then
                             loop_control_mode := lcm_near_stack
                     else if control_variable_size = 1 then
                        loop_control_mode := lcm_indirect_single_byte
                     else
                        loop_control_mode := lcm_indirect_multi_byte;
               else
                  if control_variable_size = 1 then
                     loop_control_mode := lcm_indirect_single_byte
                  else
                     loop_control_mode := lcm_indirect_multi_byte;
               end;

               if loop_control_mode in [lcm_indirect_single_byte, lcm_indirect_multi_byte] then
                  begin
                     control_variable_access.Generate_Push_Address2_Code (control_variable_size-1, false);
                     if final_value_expression.contains_constant then
                        control_variable_pointer_address := 1
                     else
                        control_variable_pointer_address := control_variable_size + 1
                  end
               else
                  control_variable_pointer_address := maxint;    // shouldn't be used

               if ((step = increment_control_variable)
                   and
                   initial_value_expression.info.max_value.le (final_value_expression.info.min_value)
                  )
                  or
                  ((step = decrement_control_variable)
                   and
                   initial_value_expression.info.min_value.ge (final_value_expression.info.max_value)
                  )
               then  // loop will always execute at least once, no need for loop-at-least-once test
                  begin
                     stmt := TPIC18x_AssignmentStatement.Create (control_variable_access, initial_value_expression, initial_value_expression.src_loc);
                     stmt.last_token_src_loc := to_or_downto_src_loc;
                     stmt.Generate (GenerateCode, 0);
                     stmt.Release;

                     if not final_value_expression.contains_constant then
                        begin
                           final_value_expression_size := TPIC18x_TypeInfo (final_value_expression.info).Size;
                           final_value_expression.Generate (GenerateCode, final_value_expression_size);
                           GenerateRangeCheckCode (TOrdinalDataType(control_variable.TypeDef),
                                                   final_value_expression_size,
                                                   final_value_expression.info,
                                                   final_value_expression.src_loc,
                                                   'range error'
                                                  );
                           generate_stack_fix_and_sign_extend_code (final_value_expression_size, 0, control_variable_size, final_value_expression.info.IntegerRange)
                        end
                  end
               else  // loop-at-least-once test needed
                  begin
                     final_value_expression_size := TPIC18x_TypeInfo (final_value_expression.info).Size;
                     final_value_expression.Generate (GenerateCode, final_value_expression_size);
                     GenerateRangeCheckCode (TOrdinalDataType(control_variable.TypeDef),
                                             final_value_expression_size,
                                             final_value_expression.info,
                                             final_value_expression.src_loc,
                                             'range error'
                                            );
                     generate_stack_fix_and_sign_extend_code (final_value_expression_size, 0, comparison_size, final_value_expression.info.IntegerRange);

                     initial_value_expression_size := TPIC18x_TypeInfo (initial_value_expression.info).Size;
                     initial_value_expression.Generate (GenerateCode, initial_value_expression_size);
                     GenerateRangeCheckCode (TOrdinalDataType(control_variable.TypeDef),
                                             initial_value_expression_size,
                                             initial_value_expression.info,
                                             initial_value_expression.src_loc,
                                             'range error'
                                            );
                     generate_stack_fix_and_sign_extend_code (initial_value_expression_size, 0, comparison_size, initial_value_expression.info.IntegerRange);

                     if loop_control_mode in [lcm_indirect_single_byte, lcm_indirect_multi_byte] then
                        begin
                           TPIC18x_MOVSF.Create ((2*comparison_size)+1, FSR1H).annotation := 'FSR1 := @' + control_variable.name + '.b0';
                           TPIC18x_MOVSF.Create ((2*comparison_size)+2, FSR1L)
                        end;

                     annotation := 'set initial value';
                     for i := control_variable_size-1 downto 0 do
                        begin
                           TPIC18x_MOVF.Create (i + 1 + comparison_size - control_variable_size, dest_w, access_mode).annotation := annotation;
                           annotation := '';

                           case loop_control_mode of
                              lcm_bank0:
                                 TPIC18x_MOVWF.Create (control_variable.address + i, bank_mode);
                              lcm_near_stack:
                                 TPIC18x_MOVWF.Create (control_variable.address + StackUsageCounter.Current + i, access_mode);
                              lcm_indirect_single_byte:
                                 TPIC18x_MOVWF.Create (INDF1, access_mode);
                              lcm_indirect_multi_byte:
                                 TPIC18x_MOVWF.Create (POSTDEC1, access_mode);
                           else
                              assert (false)
                           end
                        end;

                     annotation := 'check for no-loop initial conditons';
                     case step of
                        increment_control_variable:
                           if comparison_size = 1 then
                              begin
                                 TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation;
                                 StackUsageCounter.Pop (1);
                                 if final_value_expression.contains_constant then
                                    begin  // clear TOS
                                       TPIC18x_SUBWF.Create (PREINC2, dest_w, access_mode);
                                       StackUsageCounter.Pop (1)
                                    end
                                 else   // leave final value on stack sized same as control variable
                                    TPIC18x_SUBWF.Create (1, dest_w, access_mode)
                              end
                           else  // comparison_size > 1
                              begin
                                 TPIC18x_MOVF.Create (comparison_size, dest_w, access_mode).annotation := annotation;
                                 TPIC18x_SUBWF.Create (2*comparison_size, dest_w, access_mode);
                                 for i := comparison_size-1 downto 1 do
                                    begin
                                       TPIC18x_MOVF.Create (i, dest_w, access_mode);
                                       TPIC18x_SUBWFB.Create (comparison_size + i, dest_w, access_mode)
                                    end;
                                 if final_value_expression.contains_constant then
                                    begin  // clear TOS
                                       TPIC18x_ADDFSR.Create (2, 2*comparison_size);
                                       StackUsageCounter.Pop (2*comparison_size)
                                    end
                                 else
                                    begin  // leave final value on stack sized same as control variable
                                       TPIC18x_ADDFSR.Create (2, comparison_size + (comparison_size - control_variable_size));
                                       StackUsageCounter.Pop (comparison_size + (comparison_size - control_variable_size))
                                    end
                              end;
                        decrement_control_variable:
                           if (comparison_size = 1)
                              and
                              (not final_value_expression.contains_constant) then
                              begin
                                 TPIC18x_MOVF.Create (2, dest_w, access_mode).annotation := annotation;
                                 TPIC18x_SUBWF.Create (PREINC2, dest_w, access_mode);
                                 StackUsageCounter.Pop (1)
                                 // leave final value on stack sized same as control variable
                              end
                           else  // comparison_size > 1  or  final_value_expression.contains_constant
                              begin
                                 TPIC18x_MOVF.Create (2*comparison_size, dest_w, access_mode).annotation := annotation;
                                 TPIC18x_SUBWF.Create (comparison_size, dest_w, access_mode);
                                 for i := comparison_size-1 downto 1 do
                                    begin
                                       TPIC18x_MOVF.Create (comparison_size + i, dest_w, access_mode);
                                       TPIC18x_SUBWFB.Create (i, dest_w, access_mode)
                                    end;
                                 if final_value_expression.contains_constant then
                                    begin  // clear TOS
                                       TPIC18x_ADDFSR.Create (2, 2*comparison_size);
                                       StackUsageCounter.Pop (2*comparison_size)
                                    end
                                 else
                                    begin  // leave final value on stack sized same as control variable
                                       TPIC18x_ADDFSR.Create (2, comparison_size + (comparison_size - control_variable_size));
                                       StackUsageCounter.Pop (comparison_size + (comparison_size - control_variable_size))
                                    end
                              end
                     end;

                     be1 := TBranchOnNegativeStatusMacro.Create;
                     be1.annotation := 'branch if loop will never execute'
                  end;   // at least one loop limit is a variable

               loop := TAssemblyLabel.Create;
               statement.Generate (GenerateCode, 0);
               TSourceSyncPoint.Create (last_src_loc);

               if loop_control_mode in [lcm_indirect_single_byte, lcm_indirect_multi_byte] then
                  begin
                     TPIC18x_MOVSF.Create (control_variable_pointer_address, FSR1H).annotation := 'FSR1 := @' + control_variable.name + '.b0';
                     TPIC18x_MOVSF.Create (control_variable_pointer_address+1, FSR1L)
                  end;
               annotation := 'test for ' + control_variable.name + ' = final value';
               SetLength (br_array, control_variable_size);
               for i := control_variable_size-1 downto 0 do
                  begin
                     if final_value_expression.contains_constant then
                        case final_value_expression.constant.ordinal_value.AsByte(control_variable_size - 1 - i) of
                           0: case loop_control_mode of
                                 lcm_bank0:
                                    TPIC18x_MOVF.Create (control_variable.address + i, dest_w, bank_mode).annotation := annotation;
                                 lcm_near_stack:
                                    TPIC18x_MOVF.Create (control_variable.address + StackUsageCounter.Current + i, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_single_byte:
                                    TPIC18x_MOVF.Create (INDF1, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_multi_byte:
                                    TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode).annotation := annotation;
                              else
                                 assert (false)
                              end;
                           1: case loop_control_mode of
                                 lcm_bank0:
                                    TPIC18x_DECF.Create (control_variable.address + i, dest_w, bank_mode).annotation := annotation;
                                 lcm_near_stack:
                                    TPIC18x_DECF.Create (control_variable.address + StackUsageCounter.Current + i, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_single_byte:
                                    TPIC18x_DECF.Create (INDF1, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_multi_byte:
                                    TPIC18x_DECF.Create (POSTDEC1, dest_w, access_mode).annotation := annotation;
                              else
                                 assert (false)
                              end;
                         $FF: case loop_control_mode of
                                 lcm_bank0:
                                    TPIC18x_INCF.Create (control_variable.address + i, dest_w, bank_mode).annotation := annotation;
                                 lcm_near_stack:
                                    TPIC18x_INCF.Create (control_variable.address + StackUsageCounter.Current + i, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_single_byte:
                                    TPIC18x_INCF.Create (INDF1, dest_w, access_mode).annotation := annotation;
                                 lcm_indirect_multi_byte:
                                    TPIC18x_INCF.Create (POSTDEC1, dest_w, access_mode).annotation := annotation;
                              else
                                 assert (false)
                              end;
                         else  // not 0, 1 or $FF
                              begin
                                 case loop_control_mode of
                                    lcm_bank0:
                                       TPIC18x_MOVF.Create (control_variable.address + i, dest_w, bank_mode).annotation := annotation;
                                    lcm_near_stack:
                                       TPIC18x_MOVF.Create (control_variable.address + StackUsageCounter.Current + i, dest_w, access_mode).annotation := annotation;
                                    lcm_indirect_single_byte:
                                       TPIC18x_MOVF.Create (INDF1, dest_w, access_mode).annotation := annotation;
                                    lcm_indirect_multi_byte:
                                       TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode).annotation := annotation;
                                 else
                                    assert (false)
                                 end;
                                 TPIC18x_SUBLW.Create (final_value_expression.constant.ordinal_value.AsByte(control_variable_size - 1 - i))
                              end
                         end
                     else  // final value is not a constant, is on tos
                        begin
                           case loop_control_mode of
                              lcm_bank0:
                                 TPIC18x_MOVF.Create (control_variable.address + i, dest_w, bank_mode).annotation := annotation;
                              lcm_near_stack:
                                 TPIC18x_MOVF.Create (control_variable.address + StackUsageCounter.Current + i, dest_w, access_mode).annotation := annotation;
                              lcm_indirect_single_byte:
                                 TPIC18x_MOVF.Create (INDF1, dest_w, access_mode).annotation := annotation;
                              lcm_indirect_multi_byte:
                                 TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode).annotation := annotation;
                           else
                              assert (false)
                           end;
                           TPIC18x_SUBWF.Create (i + 1, dest_w, access_mode)
                        end;
                     annotation := '';
                     if i > 0 then
                        br_array[i] := TPIC18x_BNZ.Create
                  end;
               be2 := TPIC18x_BZ.Create;
               lbl := TAssemblyLabel.Create;
               for i := 1 to control_variable_size-1 do
                  br_array[i].dest := lbl;

               if loop_control_mode = lcm_indirect_multi_byte then
                  begin
                     TPIC18x_MOVSF.Create (control_variable_pointer_address, FSR1H).annotation := 'FSR1 := @' + control_variable.name + '.b0';
                     TPIC18x_MOVSF.Create (control_variable_pointer_address+1, FSR1L)
                  end;
               case step of
                  increment_control_variable:
                     case loop_control_mode of
                        lcm_bank0:
                           begin
                              TPIC18x_INCF.Create (control_variable.address + control_variable_size - 1, dest_f, bank_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_ADDWFC.Create (control_variable.address + i, dest_f, bank_mode)
                                 end
                           end;
                        lcm_near_stack:
                           begin
                              TPIC18x_INCF.Create (control_variable.address + control_variable_size - 1 + StackUsageCounter.Current, dest_f, access_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_ADDWFC.Create (control_variable.address + i + StackUsageCounter.Current, dest_f, access_mode)
                                 end
                           end;
                        lcm_indirect_single_byte:
                           TPIC18x_INCF.Create (INDF1, dest_f, access_mode).annotation := 'increment loop control variable';
                        lcm_indirect_multi_byte:
                           begin
                              TPIC18x_INCF.Create (POSTDEC1, dest_f, access_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_ADDWFC.Create (POSTDEC1, dest_f, access_mode)
                                 end
                           end;
                     else
                        assert (false)
                     end;
                  decrement_control_variable:
                     case loop_control_mode of
                        lcm_bank0:
                           begin
                              TPIC18x_DECF.Create (control_variable.address + control_variable_size - 1, dest_f, bank_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_SUBWFB.Create (control_variable.address + i, dest_f, bank_mode)
                                 end
                           end;
                        lcm_near_stack:
                           begin
                              TPIC18x_DECF.Create (control_variable.address + control_variable_size - 1 + StackUsageCounter.Current, dest_f, access_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_SUBWFB.Create (control_variable.address + i + StackUsageCounter.Current, dest_f, access_mode)
                                 end
                           end;
                        lcm_indirect_single_byte:
                           TPIC18x_DECF.Create (INDF1, dest_f, access_mode).annotation := 'increment loop control variable';
                        lcm_indirect_multi_byte:
                           begin
                              TPIC18x_DECF.Create (POSTDEC1, dest_f, access_mode).annotation := 'increment loop control variable';
                              if control_variable_size > 1 then
                                 begin
                                    TPIC18x_MOVLW.Create (0);
                                    for i := control_variable_size-2 downto 0 do
                                       TPIC18x_SUBWFB.Create (POSTDEC1, dest_f, access_mode)
                                 end
                           end;
                     else
                        assert (false)
                     end;
               end;

               go2 := TGOTOMacro.Create;
               go2.dest := loop;
               go2.annotation := 'repeat for loop';

               end_loop := nil;  // suppress compiler warning
               case loop_control_mode of
                  lcm_bank0,
                  lcm_near_stack:
                     if final_value_expression.contains_constant then
                        end_loop := TAssemblyLabel.Create
                     else
                        begin
                           end_loop := TPIC18x_ADDFSR.Create (2, control_variable_size);
                           StackUsageCounter.Pop (control_variable_size)
                        end;
                  lcm_indirect_single_byte,
                  lcm_indirect_multi_byte:
                     if final_value_expression.contains_constant then
                        begin
                           end_loop := TPIC18x_ADDFSR.Create (2, ram_ptr_size);
                           StackUsageCounter.Pop (ram_ptr_size)
                        end
                     else
                        begin
                           end_loop := TPIC18x_ADDFSR.Create (2, control_variable_size + ram_ptr_size);
                           StackUsageCounter.Pop (control_variable_size + ram_ptr_size)
                        end;
               else
                  assert (false)
               end;
               end_loop.annotation := 'end of for loop';
               if be1 <> nil then
                  be1.dest := end_loop;
               be2.dest := end_loop;

               control_variable_access.Release;
               comparison_range.Release;
               temp_range.Release
            end;
      else
         assert (false)
      end
   end;   // TPIC18x_ForStatement.Generate

function TPIC18x_IfStatement.Generate (param1, param2: integer): integer;
   var
      i: integer;
      arr: array of
              record
                 start_label: TAssemblyLabel;
                 conditional_branch: TInstruction;
                 final_goto: TGOTOMacro
              end;
      else_label, final_label: TAssemblyLabel;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               else_label := nil;  // suppress compiler warning
               SetLength (arr, Length(conditional_statement_list));
               for i := 0 to Length(conditional_statement_list)-1 do
                  begin
                     TSourceSyncPoint.Create (conditional_statement_list[i].last_boolean_expression_token_src_loc);
                     if i > 0 then
                        arr[i].start_label := TAssemblyLabel.Create;

                     arr[i].conditional_branch := GenerateCodeForConditionalBranch (conditional_statement_list[i].boolean_expression, false);

                     if conditional_statement_list[i].statement <> nil then
                        begin
                           conditional_statement_list[i].statement.Generate (GenerateCode, 0);
                           TSourceSyncPoint.Create (conditional_statement_list[i].last_statement_token_src_loc)
                        end;

                     if (i < Length(conditional_statement_list)-1)
                        or
                        (else_statement <> nil) then
                        arr[i].final_goto := TGOTOMacro.Create
                  end;
               if else_statement <> nil then
                  begin
                     TSourceSyncPoint.Create (last_else_statement_token_src_loc);
                     else_label := TAssemblyLabel.Create;
                     else_statement.Generate (GenerateCode, 0)
                  end;
               final_label := TAssemblyLabel.Create;
               for i := 0 to Length(conditional_statement_list)-2 do
                  begin
                     arr[i].conditional_branch.dest := arr[i+1].start_label;
                     arr[i].final_goto.dest := final_label
                  end;
               if else_statement <> nil then
                  begin
                     arr[Length(arr)-1].conditional_branch.dest := else_label;
                     arr[Length(arr)-1].final_goto.dest := final_label
                  end
               else
                  arr[Length(arr)-1].conditional_branch.dest := final_label
            end;
      else
         assert (false, 'TPIC18x_IfStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_InitStatement.Generate (param1, param2: integer): integer;
   var
      i, param_blk_size, var_blk_size, stk_size, stk_base_adjustment: integer;
      instr, push_return_address_macro: TInstruction;
      dw: TPIC18x_DW;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            for i := 0 to Length(initlist)-1 do
               begin
                  NoteHWStackUsage (TPIC18x_SystemType(initlist[i].access.node_typedef).initial_statement_hw_stack_usage);
                  TSourceSyncPoint.Create (initlist[i].src_loc);
                  case TSystemType(initlist[i].access.node_typedef).system_type_kind of
                     process_system_type:
                        begin
                           param_blk_size := TPIC18x_ParamList(TPIC18x_SystemType(initlist[i].access.node_typedef).parameters).Size;
                           var_blk_size := TPIC18x_DataItemList(TPIC18x_SystemType(initlist[i].access.node_typedef).permanent_ram_vars).Size;
                           stk_size := TPIC18x_SystemType(initlist[i].access.node_typedef).process_stack_size;

                           TPIC18x_ParamList(TSystemType(initlist[i].access.node_typedef).parameters).PushParameters (initlist[i].parameters);
                           TPIC18x_Access(initlist[i].access).Generate_Load_Ptr2_Code (pFSR1, 0);

                           InitProcessSubroutine.Call.annotation := 'call init process';
                           StackUsageCounter.Pop (param_blk_size);
                           stk_base_adjustment := var_blk_size + stk_size - 1;

                           dw := TPIC18x_DW.Create (0);
                           dw.lsb := PriorityMapper.ReadyQueueAddr (TSystemType(initlist[i].access.node_typedef).priority);
                           dw.msb := TPIC18x_Variable(initlist[i].access.base_variable).pcb_address;
                           dw.annotation := '   - ready_queue_address, pcb_address';

                           dw := TPIC18x_DW.Create (0);
                           dw.lsb := param_blk_size;
                           dw.msb := stk_base_adjustment and $ff;
                           dw.annotation := '   - param_blk_size, stk_base_adjustment.lsb';

                           dw := TPIC18x_DW.Create (0);
                           dw.lsb := (stk_base_adjustment and $ff00) shr 8;
                           dw.msb_from_labelU := TPIC18x_SystemType(initlist[i].access.node_typedef).init_stmt_entry_point_label;
                           dw.annotation := '   - stk_base_adjustment.msb, @initial statement entry point';

                           dw := TPIC18x_DW.Create (0);
                           dw.lsb_from_labelH := TPIC18x_SystemType(initlist[i].access.node_typedef).init_stmt_entry_point_label;
                           dw.msb_from_labelL := TPIC18x_SystemType(initlist[i].access.node_typedef).init_stmt_entry_point_label
                        end;
                     class_system_type,
                     monitor_system_type:
                        begin
                            push_return_address_macro := TPushLabelMacro.Create;
                            push_return_address_macro.annotation := 'push return address';
                            StackUsageCounter.Push (3);
                            TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2).annotation := 'save current this pointer';
                            TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2);
                            StackUsageCounter.Push(2);

                            TPIC18x_ParamList(TSystemType(initlist[i].access.node_typedef).parameters).PushParameters (initlist[i].parameters);

                            TPIC18x_Access(initlist[i].access).Generate_Load_Ptr2_Code (pTHIS, 0);

                            instr := TGOTOMacro.Create;
                            instr.dest := TPIC18x_SystemType(initlist[i].access.node_typedef).init_stmt_entry_point_label;
                            instr.annotation := 'call ' + initlist[i].access.path_src + ' initial statement';
                            push_return_address_macro.dest := TAssemblyLabel.Create;

                            StackUsageCounter.Pop (TPIC18x_ParamList(TSystemType(initlist[i].access.node_typedef).parameters).Size);
                            StackUsageCounter.PushPop (TPIC18x_SystemType(initlist[i].access.node_typedef).initial_stmt_stack_usage);
                            StackUsageCounter.Pop (2);    // this pointer
                            StackUsageCounter.Pop (3)     // return address
                        end;
                     interrupt_system_type:
                        begin
                           ProgramCode.AppendInlineCode (TPIC18x_SystemType(initlist[i].access.node_typedef).inline_code);
                           StackUsageCounter.PushPop (TPIC18x_SystemType(initlist[i].access.node_typedef).initial_stmt_stack_usage)
                        end;
                  else
                     assert (false)
                  end
               end;
      else
         assert (false, 'TPIC18x_InitStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_LoopStatement.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               end_loop := TBranchTarget.Create;
               TSourceSyncPoint.Create (src_loc);
               start_loop_label := TAssemblyLabel.Create;
               initial_stack_level := StackUsageCounter.Current;
               statement_list.Generate (GenerateCode, 0);
               TSourceSyncPoint.Create (repeat_token_src_loc);
               TGOTOMacro.Create.dest := start_loop_label;
               end_loop.target_label := TAssemblyLabel.Create;
               end_loop.set_client_destinations;
               end_loop.Free;
               end_loop := nil
            end;
      else
         assert (false, 'TPIC18x_LoopStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ReLoopStatement.Generate (param1, param2: integer): integer;
   var
      br: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               if reloop_condition = nil then
                  begin
                     if StackUsageCounter.Current > TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level then
                        TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level);
                     TGOTOMacro.Create.dest := TPIC18x_LoopStatement(containing_loop_statement).start_loop_label
                  end
               else if StackUsageCounter.Current = TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level then
                  GenerateCodeForConditionalBranch (reloop_condition, true).dest := TPIC18x_LoopStatement(containing_loop_statement).start_loop_label
               else
                  begin
                     br := GenerateCodeForConditionalBranch (reloop_condition, false);
                     TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level);
                     TGOTOMacro.Create.dest := TPIC18x_LoopStatement(containing_loop_statement).start_loop_label;
                     br.dest := TAssemblyLabel.Create
                  end
            end;
      else
         assert (false, 'TPIC18x_ReLoopStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ReCycleStatement.Generate (param1, param2: integer): integer;
   var
      br: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               if recycle_condition = nil then
                  begin
                     if StackUsageCounter.Current > TPIC18x_CycleStatement (containing_cycle_stmt).initial_stack_level then
                        TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_CycleStatement (containing_cycle_stmt).initial_stack_level);
                     TGOTOMacro.Create.dest := TPIC18x_CycleStatement (containing_cycle_stmt).start_loop_label
                  end
               else if StackUsageCounter.Current = TPIC18x_CycleStatement (containing_cycle_stmt).initial_stack_level then
                  GenerateCodeForConditionalBranch (recycle_condition, true).dest := TPIC18x_CycleStatement (containing_cycle_stmt).start_loop_label
               else
                  begin
                     br := GenerateCodeForConditionalBranch (recycle_condition, false);
                     TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_CycleStatement (containing_cycle_stmt).initial_stack_level);
                     TGOTOMacro.Create.dest := TPIC18x_CycleStatement (containing_cycle_stmt).start_loop_label;
                     br.dest := TAssemblyLabel.Create
                  end
            end;
      else
         assert (false, 'TPIC18x_ReCycleStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

constructor TPIC18x_RoutineCallStatement.CreateFromSourceTokens (acc: TAccess);
   var
      cexpr: TCExpression;
   begin
      inherited Create(routine_call_statement);
      access := acc;
      if (access.node_routine = TPIC18x_CPU(target_cpu).SetError) then
         begin  // do parsing for non-standard parameter list.
                // msg is a constant string not placed in code, only in error file
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            cexpr := TCExpression.CreateFromSourceTokens;
            try
               if cexpr.constant_kind <> string_constant then
                  raise compile_error.Create (err_string_expected, cexpr.src_loc);
               error_message := cexpr.s
            finally
               cexpr.Release
            end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      {$ifdef INCLUDE_SIMULATION}
      else if(access.node_routine = TPIC18x_CPU(target_cpu).Test) then
         begin  // do parsing for non-standard parameter list.
                // msg is a constant string not placed in code, only in error file
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            cexpr := TCExpression.CreateFromSourceTokens;
            try
               if cexpr.constant_kind <> integer_constant then
                  raise compile_error.Create (err_integer_expected, cexpr.src_loc);
               subtest := cexpr.AsOrdinal
            finally
               cexpr.Release
            end;

            if not lex.token_is_symbol(sym_comma) then
               raise compile_error.Create(err_comma_expected);
            lex.advance_token;

            cexpr := TCExpression.CreateFromSourceTokens;
            try
               if cexpr.constant_kind <> string_constant then
                  raise compile_error.Create (err_string_expected, cexpr.src_loc);
               error_message := cexpr.s
            finally
               cexpr.Release
            end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      {$endif}
      else
         inherited
   end;

constructor TPIC18x_RoutineCallStatement.Create (acc: TAccess; exp: TExpression; _src_loc: TSourceLocation);
   begin
      inherited Create(routine_call_statement);
      src_loc := _src_loc;
      access := acc;
      access.AddRef;
      SetLength (actual_parameters, 1);
      actual_parameters[0] := exp;
      actual_parameters[0].AddRef
   end;

type
   tResetTimerCycleCodeSegment_with_const_param =
      class (TInterruptsOffCodeSegment)
         rtc: Treset_TMRn_cycle;
         param: integer;
         constructor Create (_rtc: Treset_TMRn_cycle; cycle_count, _param: integer);
         function generate_code_segment: TInstruction;
            override;
      end;
   tResetTimerCycleCodeSegment_with_param_on_stack =
      class (TInterruptsOffCodeSegment)
         rtc: Treset_TMRn_cycle;
         constructor Create (_rtc: Treset_TMRn_cycle);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor tResetTimerCycleCodeSegment_with_const_param.Create (_rtc: Treset_TMRn_cycle; cycle_count, _param: integer);
   begin
      rtc := _rtc;
      param := _param;
      inherited Create (1, 1, 'reset_TMR' + IntToStr(_rtc.timer_number) + '_cycle (' + IntToStr(cycle_count) + ')')
   end;

function tResetTimerCycleCodeSegment_with_const_param.generate_code_segment: TInstruction;
   var
      tmrH, tmrL: integer;
   begin
      tmrH := pic_info.SFR_Address('TMR' + IntToStr(rtc.timer_number) + 'H');
      tmrL := pic_info.SFR_Address('TMR' + IntToStr(rtc.timer_number) + 'L');
      if tmrL >= pic_info.first_access_bank_absolute_address then
         begin  // use access bank addressing
            result := TPIC18x_MOVLW.Create (lsb(param));
            TPIC18x_SUBWF.Create (tmrL, dest_w, access_mode);       // read TMRL first
            TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
            TPIC18x_MOVLW.Create (msb(param));
            TPIC18x_SUBWFB.Create (tmrH, dest_f, access_mode);      // update TMRH
            TPIC18x_MOVFF.Create (PREINC2, tmrL)                    // write TMRL last
         end
      else
         begin  // use FSR0 to access TMR
            result := TPIC18x_LFSR.Create (0, tmrL);
            assert (tmrL+1 = tmrH);
            TPIC18x_MOVLW.Create (lsb(param));
            TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);   // read TMRL first
            TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
            TPIC18x_MOVLW.Create (msb(param));
            TPIC18x_SUBWFB.Create (POSTDEC0, dest_f, access_mode);  // update TMRH
            TPIC18x_MOVFF.Create (PREINC2, INDF0)                   // write TMRL last
         end
   end;

constructor tResetTimerCycleCodeSegment_with_param_on_stack.Create (_rtc: Treset_TMRn_cycle);
   begin
      rtc := _rtc;
      inherited Create (0, 2, 'reset_TMR' + IntToStr(_rtc.timer_number) + '_cycle')
   end;

function tResetTimerCycleCodeSegment_with_param_on_stack.generate_code_segment: TInstruction;
   var
      tmrH, tmrL: integer;
   begin
      tmrH := pic_info.SFR_Address('TMR' + IntToStr(rtc.timer_number) + 'H');
      tmrL := pic_info.SFR_Address('TMR' + IntToStr(rtc.timer_number) + 'L');

      if tmrL >= pic_info.first_access_bank_absolute_address then
         begin  // use access bank addressing
            result := TPIC18x_MOVF.Create (2, dest_w, access_mode);
            TPIC18x_SUBWF.Create (tmrL, dest_w, access_mode);       // read TMRL first
            TPIC18x_MOVWF.Create (2, access_mode);
            TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
            TPIC18x_SUBWFB.Create (tmrH, dest_f, access_mode);      // update TMRH
            TPIC18x_MOVFF.Create (PREINC2, tmrL)                    // write TMRL last
         end
      else
         begin  // use FSR0 to access TMR
            result := TPIC18x_LFSR.Create (0, tmrL);
            assert (tmrL+1 = tmrH);
            TPIC18x_MOVF.Create (2, dest_w, access_mode);
            TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);   // read TMRL first
            TPIC18x_MOVWF.Create (2, access_mode);
            TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
            TPIC18x_SUBWFB.Create (POSTDEC0, dest_f, access_mode);  // update TMRH
            TPIC18x_MOVFF.Create (PREINC2, INDF0)                   // write TMRL last
         end
   end;

function err_timer_cycle_count_exceeded (timer_number: integer): string;
   begin
      result := 'TMR' + IntToStr(timer_number) + ' cycle count exceeded'
   end;

function TPIC18x_RoutineCallStatement.Generate (param1, param2: integer): integer;

   function reset_TMR_cycle_procedure_idx: integer;
      begin
         for result := 0 to Length(TPIC18x_CPU(target_cpu).reset_TMRn_cycle)-1 do
            if access.node_routine = TPIC18x_CPU(target_cpu).reset_TMRn_cycle[result].routine then
               exit;
         result := -1
      end;

   procedure generate_reset_TMR_cycle_code;
      var
         rtc: Treset_TMRn_cycle;
         cycle_count_adj: integer;
      begin
         rtc := TPIC18x_CPU(target_cpu).reset_TMRn_cycle[reset_TMR_cycle_procedure_idx];
         if actual_parameters[0] is TPIC18x_ConstantPrimary then
            begin
               if rtc.timer_number = 0 then
                  cycle_count_adj := 7
               else
                  cycle_count_adj := 5;
               tResetTimerCycleCodeSegment_with_const_param.Create
                  (rtc,
                   TConstant(TPIC18x_ConstantPrimary(actual_parameters[0]).the_constant).AsOrdinal,
                   TConstant(TPIC18x_ConstantPrimary(actual_parameters[0]).the_constant).AsOrdinal - cycle_count_adj
                  )
            end
         else  // non-constant expression
            begin
               TPIC18x_ParamList(TPIC18x_Routine(access.node_routine).parameter_definitions).PushParameters (actual_parameters);
               if rtc.timer_number = 0 then
                  cycle_count_adj := 7
               else
                  cycle_count_adj := 5;
               TPIC18x_MOVLW.Create (cycle_count_adj);
               TPIC18x_SUBWF.Create (2, dest_f, access_mode);
               TPIC18x_MOVLW.Create (0);
               TPIC18x_SUBWFB.Create (1, dest_f, access_mode);
               tResetTimerCycleCodeSegment_with_param_on_stack.Create (rtc)
            end;
         TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
         set_errorcode_routine.Call.annotation := '';
         RecordRunTimeErrorLocation (TAssemblyLabel.Create, err_timer_cycle_count_exceeded(rtc.timer_number), src_loc);
      end;

   procedure generate_routine_call_code (routine: TPIC18x_Routine);
      var
         push_return_address_macro: TPushLabelMacro;
         instr: TInstruction;
         is_monitor_entry_routine: boolean;
      begin
         is_monitor_entry_routine := false;
         NoteHWStackUsage (routine.hw_stack_usage);

         push_return_address_macro := TPushLabelMacro.Create;
         push_return_address_macro.annotation := 'push return address';
         StackUsageCounter.Push (3);

         if routine.entry then
            begin
               TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2).annotation := 'push caller''s this pointer';
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

         TPIC18x_ParamList(routine.parameter_definitions).PushParameters (actual_parameters);

         if routine.entry then
            TPIC18x_Access(access).Generate_Load_Ptr2_Code (pTHIS, 0);

         instr := TGOTOMacro.Create;
         instr.dest := TPIC18x_Routine(routine).entry_point_label;
         instr.annotation := 'call ' + access.path_src;
         push_return_address_macro.dest := TAssemblyLabel.Create;

         StackUsageCounter.PushPop (routine.stack_usage);
         StackUsageCounter.Pop (TPIC18x_ParamList(routine.parameter_definitions).Size);
         if is_monitor_entry_routine then
            StackUsageCounter.Pop (1);   // prio
         if routine.entry then
            StackUsageCounter.Pop (2);   // this ptr
         StackUsageCounter.Pop (3)       // return address
      end;

   procedure generate_strappend_code;
      var
         rom_addr: integer;
      begin   // generate_strappend_code
         case access.base_variable.descriptor of
            rw_var:
               begin
                  TPIC18x_access(access).Generate_Push_Address2_Code (0, true);
                  case TExpression(access.node_strappend_expression).expression_kind of
                     char_expression:
                        begin
                           access.node_strappend_expression.Generate (GenerateCode, 1);
                           AppendCharToRAMStr.Call (access.node_strappend_expression.src_loc)
                        end;
                     string_expression:
                        if access.node_strappend_expression is TVariableAccessPrimary then
                           case TVariableAccessPrimary(access.node_strappend_expression).access.base_variable.descriptor of
                              rw_var:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr2_Code (pFSR1, 0);
                                    TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);  // pick up strlen and set Z
                                    AppendRAMStrToRAMStr.Call (access.node_strappend_expression.src_loc)
                                 end;
                              rw_rom:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                                    TPIC18x_TBLRD.Create (tblrd_post_inc);
                                    TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
                                    AppendROMStrToRAMStr.Call (access.node_strappend_expression.src_loc)
                                 end;
                              rw_eeprom:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr1_Code (pFSR1, 0);
                                    GetEEPROMByte.Call;    // decrements FSR1
                                    TPIC18x_ADDFSR.Create (1, 2);  // net result is FSR1++
                                    AppendEEPROMStrToRAMStr.Call (access.node_strappend_expression.src_loc)
                                 end;
                           else
                              assert (false)
                           end
                        else if access.node_strappend_expression is TConstantPrimary then
                           begin
                              rom_addr := TPIC18x_CPU(target_cpu).anonymous_string_constant_rom_addr (TConstant(TConstantPrimary(access.node_strappend_expression).the_constant).s);
                              set_absolute (TBLPTRL, lsb(rom_addr));
                              set_absolute (TBLPTRH, msb(rom_addr));
                              TPIC18x_TBLRD.Create (tblrd_post_inc);
                              TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
                              AppendROMStrToRAMStr.Call (src_loc)
                           end
                        else
                           assert (false)
                  else
                     assert (false)
                  end
               end;
            rw_eeprom:
               begin
                  TPIC18x_access(access).Generate_Push_Address1_Code (0, true);
                  case TExpression(access.node_strappend_expression).expression_kind of
                     char_expression:
                        begin
                           access.node_strappend_expression.Generate (GenerateCode, 1);
                           AppendCharToEEPROMString.Call (access.node_strappend_expression.src_loc)
                        end;
                     string_expression:
                        if access.node_strappend_expression is TVariableAccessPrimary then
                           case TVariableAccessPrimary(access.node_strappend_expression).access.base_variable.descriptor of
                              rw_var:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr2_Code (pFSR1, 0);
                                    TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);  // pick up strlen and set Z
                                    AppendRAMStrToEEPROMStr.Call (src_loc)
                                 end;
                              rw_rom:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                                    TPIC18x_TBLRD.Create (tblrd_post_inc);
                                    TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
                                    AppendROMStrToEEPROMStr.Call (src_loc)
                                 end;
                              rw_eeprom:
                                 begin
                                    TPIC18x_Access(TVariableAccessPrimary(access.node_strappend_expression).access).Generate_Load_Ptr1_Code (pFSR1, 0);
                                    GetEEPROMByte.Call;    // decrements FSR1
                                    TPIC18x_ADDFSR.Create (1, 2);  // net result is FSR1++
                                    AppendEEPROMStrToEEPROMStr.Call (src_loc)
                                 end;
                           else
                              assert (false)
                           end
                        else if access.node_strappend_expression is TConstantPrimary then
                           begin
                              rom_addr := TPIC18x_CPU(target_cpu).anonymous_string_constant_rom_addr (TConstant(TConstantPrimary(access.node_strappend_expression).the_constant).s);
                              set_absolute (TBLPTRL, lsb(rom_addr));
                              set_absolute (TBLPTRH, msb(rom_addr));
                              TPIC18x_TBLRD.Create (tblrd_post_inc);
                              TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // get length and set Z if 0
                              AppendROMStrToEEPROMStr.Call (src_loc)
                           end
                        else
                           assert (false)
                  else
                     assert (false)
                  end
               end;
         else
            assert (false)
         end
      end;    // generate_strappend_code

{$ifdef INCLUDE_SIMULATION}
   var
      tokens: TStringList;
      nop: TPIC18x_NOP;
{$endif}
   begin  // TPIC18x_RoutineCallStatement.Generate
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               if access.is_strappend_attribute then
                  generate_strappend_code
               else if access.node_routine = TPIC18x_CPU(target_cpu).ClearWatchdogTimer then
                  TPIC18x_CLRWDT.Create
               else if access.node_routine = TPIC18x_CPU(target_cpu).SetError then
                  begin
                     set_errorcode_routine.call;
                     RecordRunTimeErrorLocation (TAssemblyLabel.Create, error_message, src_loc)
                  end
               else if reset_TMR_cycle_procedure_idx > -1 then
                  generate_reset_TMR_cycle_code
               {$ifdef INCLUDE_SIMULATION}
               else if access.node_routine = TPIC18x_CPU(target_cpu).Test then
                  begin
                     nop := TPIC18x_NOP.Create;
                     nop.annotation := format ('subtest %d: %s', [subtest, error_message]);
                     nop.subtest := subtest;
                     tokens := TStringList.Create;
                     tokens.Delimiter := ' ';
                     tokens.DelimitedText := lowercase (error_message);
                     construct_kernel_test (nop, tokens);
                     tokens.Free
                  end
               {$endif}
               else if access.node_routine <> nil then
                  generate_routine_call_code (TPIC18x_Routine(access.node_routine))
               else if access.node_property <> nil then
                  generate_routine_call_code (TPIC18x_Routine(access.node_property.set_proc))
               else
                  assert (false)
            end;
      else
         assert (false, 'TPIC18x_RoutineCallStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;   // TPIC18x_RoutineCallStatement.Generate

function TPIC18x_StatementList.Generate (param1, param2: integer): integer;
   var
      current_stack_level: integer;
   procedure generate_stmt_and_check_stack (stmt: TDefinition);
      begin
         stmt.Generate (GenerateCode, 0);
         assert (current_stack_level = StackUsageCounter.Current)
      end;
   var
      i: integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               current_stack_level := StackUsageCounter.Current;
               for i := 0 to Length(stmts)-1 do
                  generate_stmt_and_check_stack (stmts[i]);
            end;
      else
         assert (false, 'TPIC18x_StatementList.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_UntilStatement.Generate (param1, param2: integer): integer;
   var
      br: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (boolean_expression.src_loc);
               if StackUsageCounter.Current = TPIC18x_LoopStatement (containing_loop_statement).initial_stack_level then
                  TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (GenerateCodeForConditionalBranch (boolean_expression, true))
               else
                  begin
                     br := GenerateCodeForConditionalBranch (boolean_expression, false);
                     TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level);
                     TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (TGOTOMacro.Create);
                     br.dest := TAssemblyLabel.Create
                  end
            end;
      else
         assert (false, 'TPIC18x_WhileStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_WhileStatement.Generate (param1, param2: integer): integer;
   var
      br: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (boolean_expression.src_loc);
               if StackUsageCounter.Current = TPIC18x_LoopStatement (containing_loop_statement).initial_stack_level then
                  TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (GenerateCodeForConditionalBranch (boolean_expression, false))
               else
                  begin
                     br := GenerateCodeForConditionalBranch (boolean_expression, true);
                     TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current - TPIC18x_LoopStatement(containing_loop_statement).initial_stack_level);
                     TPIC18x_LoopStatement(containing_loop_statement).end_loop.ComeFrom (TGOTOMacro.Create);
                     br.dest := TAssemblyLabel.Create
                  end
            end;
      else
         assert (false, 'TPIC18x_WhileStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_WithStatement.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (last_access_src_loc);
               case access.base_variable.descriptor of
                  rw_eeprom:
                     TPIC18x_Access(access).Generate_Push_Address1_Code(0, false);
                  else
                     TPIC18x_Access(access).Generate_Push_Address2_Code(0, false)
               end;
               address := StackUsageCounter.Current - 1;
               TSourceSyncPoint.Create (do_src_loc);
               statement.Generate (GenerateCode, 0);

               TSourceSyncPoint.Create (last_src_loc);
               case access.base_variable.descriptor of
                  rw_const,
                  rw_ioreg,
                  rw_var:
                     begin
                        TPIC18x_ADDFSR.Create (2, ram_ptr_size);
                        StackUsageCounter.Pop (ram_ptr_size)
                     end;
                  rw_rom:
                     begin
                        TPIC18x_ADDFSR.Create (2, rom_ptr_size);
                        StackUsageCounter.Pop (rom_ptr_size)
                     end;
                  rw_eeprom:
                     begin
                        TPIC18x_ADDFSR.Create (2, eeprom_ptr_size);
                        StackUsageCounter.Pop (eeprom_ptr_size)
                     end;
               else
                  assert (false)
               end
            end;
      else
         assert (false, 'TPIC18x_WithStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;


INITIALIZATION
   temp := TMultiPrecisionInteger.Create;
   expression_lower_fence := TMultiPrecisionInteger.Create;
   expression_upper_fence := TMultiPrecisionInteger.Create;

FINALIZATION
   temp.Free;
   expression_lower_fence.Free;
   expression_upper_fence.Free;

END.
