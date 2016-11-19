UNIT cpc_statements_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
  cpc_definitions_unit, cpc_source_analysis_unit, cpc_access_unit, cpc_core_objects_unit,
  cpc_constant_expression_unit, cpc_blocks_unit,
  wirth_balanced_binary_tree_unit;

type
   TStatementKind =
      (assert_statement,
       assignment_statement,
       await_statement,
       case_statement,
       continue_statement,
       cycle_statement,
       delay_statement,
       exitloop_statement,
       for_statement,
       if_statement,
       init_statement,
       loop_statement,
       err_cant_call_public_routine_from_same_type,
       reloop_statement,
       routine_call_statement,
       statement_list,
       until_statement,
       while_statement,
       with_statement
      );

   TStatementList = class;

   TStatement =
      class(TDefinition)
         statement_kind: TStatementKind;
         constructor Create
            (k: TStatementKind
            );
      end;

   TAssertStatement =
      class(TStatement)
         boolean_expression: TExpression;
         end_src_loc: TSourceLocation;
         assertion_message: string;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TAssignmentStatement =
      class(TStatement)
         assignee: TAccess;
         assignment_operator_src_loc: TSourceLocation;
         expression: TExpression;
         last_token_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens
            (acc: TAccess
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TAwaitInterruptStatement =
      class(TStatement)
         containing_process: TSystemType;
         constructor CreateFromSourceTokens;
      end;

   TLabelRange =
      record
         first_of_range, last_of_range: TCExpression
      end;
   TCaseStatement =
      class(TStatement)
      type
         TLabelSpec =
            class
               case_index: integer;
               case_index_src_loc: TSourceLocation;
               labeled_statement_idx: integer
            end;
         TCaseLabelRangeEntry =
            class (TBalancedTreeEntry)
               case_stmt: TCaseStatement;
               first_of_range, last_of_range: integer;
               labeled_statement_idx: integer;
               lower_fence, upper_fence: integer;
               constructor Create (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer);
               function compare
                  (a: TBalancedTreeEntry
                  ): Shortint;  // a < self :-1  a=self :0  a > self :+1
                  override;
               procedure copy
                  (ToA: TBalancedTreeEntry
                  ); // data
                  override;
               procedure set_fence (_lower_fence, _upper_fence: integer);
               function left_test_needed: boolean;
               function right_test_needed: boolean;
            end;
      protected var
         case_label_range_tree: TBalancedBinaryTree;
      protected
         function create_case_entry (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer): TCaseLabelRangeEntry;
            virtual;
      public
         selection_expression: TExpression;
         of_src_loc, otherwise_src_loc, end_src_loc: TSourceLocation;
         labeled_statements:
            array of
               record
                  colon_src_loc: TSourceLocation;
                  statement: TStatement
               end;
         otherwise_statement: TStatement;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TContinueStatement =
      class(TStatement)
         queue_access: TAccess;
         containing_monitor: TSystemType;
         containing_routine: TRoutine;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
      end;

   TCycleStatement =
      class(TStatement)
         statement_list: TStatementList;
         repeat_token_src_loc: TSourceLocation;
         is_empty_loop_at_end_of_program_initial_statement: boolean;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      private
         function termination_test: boolean;
      end;

   TDelayStatement =
      class(TStatement)
         queue_access: TAccess;
         containing_monitor: TSystemType;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TLoopStatement = class;
   TExitLoopStatement =
      class(TStatement)
         containing_loop_statement: TLoopStatement;
         exitloop_condition: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TForStatement =
      class(TStatement)
         control_variable: TVariable;
         initial_value_expression: TExpression;
         step:
            (increment_control_variable,
             decrement_control_variable
            );
         final_value_expression: TExpression;
         do_src_loc, to_or_downto_src_loc, last_src_loc: TSourceLocation;
         statement: TStatement;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TIfStatement =
      class(TStatement)
         conditional_statement_list:
            array of
               record
                  boolean_expression: TExpression;
                  last_boolean_expression_token_src_loc: TSourceLocation;
                  statement: TStatement;
                  last_statement_token_src_loc: TSourceLocation;
               end;
         else_statement: TStatement;
         last_else_statement_token_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TInitStatement =
      class(TStatement)
         initlist:
            array of
               record
                  access: TAccess;
                  parameters: TArrayOfTDefinition;
                  src_loc: TSourceLocation
               end;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TLoopStatement =
      class(TStatement)
         statement_list: TStatementList;
         repeat_token_src_loc: TSourceLocation;
         first_exit_src_loc: TSourceLocation;
         possible_loop_exit_declared: boolean;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      private
         function termination_test: boolean;
      end;

   TReCycleStatement =
      class(TStatement)
         containing_cycle_stmt: TCycleStatement;
         recycle_condition: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TReLoopStatement =
      class(TStatement)
         containing_loop_statement: TLoopStatement;
         reloop_condition: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TRoutineCallStatement =
      class(TStatement)
         access: TAccess;
         actual_parameters: TArrayOfTDefinition;
         call_record: TRoutineCallRecord;
         constructor CreateFromSourceTokens
            (acc: TAccess
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TWhileStatement =
      class(TStatement)
         boolean_expression: TExpression;
         containing_loop_statement: TLoopStatement;
         last_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TWithStatement =
      class(TStatement)
         access: TAccess;
         last_access_src_loc, do_src_loc, last_src_loc: TSourceLocation;
         statement: TStatement;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         constructor CreateFromSourceTokensStartingAtVariable;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      private
         procedure scan_starting_at_with_variable;
      end;

   TWithVariable =
      class(TDefinition)
         with_statement: TWithStatement;
         record_field: TDefinition; // must be TRecordField or TPackedRecordField
         initialization_assumption_invalid: boolean;  // true if variable is part of an overlay variable
         constructor Create
            (_with_statement: TWithStatement;
             _record_field: TDefinition
            );
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TWithRoutine =
      class(TDefinition)
         with_statement: TWithStatement;
         routine: TRoutine;
         constructor Create
            (_with_statement: TWithStatement;
             _routine: TRoutine
            );
         destructor Destroy;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
     end;

   TWithProperty =
      class(TDefinition)
         with_statement: TWithStatement;
         prop: TProperty;
         constructor Create
            (_with_statement: TWithStatement;
             _prop: TProperty
            );
         destructor Destroy;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TUntilStatement =
      class(TStatement)
         boolean_expression: TExpression;
         containing_loop_statement: TLoopStatement;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TBooleanFunctionOfObject = function: boolean of object;
   TStatementList =
      class(TStatement)
         stmts:
            array of TStatement;
         function get
            (i: integer
            ): TStatement;
         procedure put
            (i: integer;
             stmt: TStatement
            );
         property stmt[i: integer]: TStatement read get write put; default;
         constructor CreateFromSourceTokens
            (terminator: TBooleanFunctionOfObject;
             non_terminated_error_message: string;
             while_or_until_allowed: boolean
            );
            overload;
         constructor Create;
         procedure AddFromSourceTokens
            (terminator: TReservedWordEnumSet;
             non_terminated_error_message: string
            );
         procedure AddStatement
            (stmt: TStatement
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;


IMPLEMENTATION

uses
   SysUtils, cpc_common_unit, cpc_expressions_unit, cpc_target_cpu_unit, cpc_types_unit,
   dijkstra_smoothsort_unit, cpc_multi_precision_integer_unit;

var
   CycleStatementStack: TDefinitionStack;
   LoopStatementStack: TDefinitionStack;
   temp: TMultiPrecisionInteger;


//===========================
// EStatementSimplification
type
   EStatementSimplification =
      class(Exception)
         simplified_statement: TStatement;
         constructor Create
            (stmt: TStatement
            );
      end;

constructor EStatementSimplification.Create
   (stmt: TStatement
   );
   begin
      if stmt <> nil then
         stmt.AddRef;
      simplified_statement := stmt
   end;

function process_statement_from_source_tokens (while_and_until_allowed: boolean): TStatement;
   var
      access: TAccess;
      compound_statement: TStatementList;
      src_loc: TSourceLocation;
   begin
      result := nil; // to suppress warning
      try
         src_loc := lex.token.src_loc;
         if lex.token_is_reserved_word(rw_begin) then
            begin
               lex.advance_token;
               compound_statement := target_cpu.TStatementList_Create;
               compound_statement.AddFromSourceTokens([rw_end], err_semicolon_expected);
               lex.advance_token;
               result := compound_statement
            end
         else if lex.token_is_reserved_word(rw_if) then
            result := target_cpu.TIfStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_while) then
            if while_and_until_allowed then
               result := target_cpu.TWhileStatement_CreateFromSourceTokens
            else
               raise compile_error.Create (err_while_not_allowed_here)
         else if lex.token_is_reserved_word(rw_until) then
            if while_and_until_allowed then
               result := target_cpu.TUntilStatement_CreateFromSourceTokens
            else
               raise compile_error.Create (err_until_not_allowed_here)
         else if lex.token_is_reserved_word(rw_loop) then
            result := target_cpu.TLoopStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_exitloop) then
            result := target_cpu.TExitLoopStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_reloop) then
            result := target_cpu.TReLoopStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_recycle) then
            result := target_cpu.TReCycleStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_with) then
            result := target_cpu.TWithStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_continue) then
            result := target_cpu.TContinueStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_cycle) then
            result := target_cpu.TCycleStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_delay) then
            result := target_cpu.TDelayStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_init) then
            result := target_cpu.TInitStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_for) then
            result := target_cpu.TForStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_case) then
            result := target_cpu.TCaseStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_await) then
            result := target_cpu.TAwaitInterruptStatement_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_assert) then
            result := target_cpu.TAssertStatement_CreateFromSourceTokens
         else if lex.token_is_identifier then
            begin // either procedure call or left side of assignment statement
               access := target_cpu.TAccess_CreateFromSourceTokens;
               case access.node_access_kind of
                  variable_access:
                     result := target_cpu.TAssignmentStatement_CreateFromSourceTokens(access);
                  function_access:
                     try
                        if not access.node_routine.definition_complete then
                           raise compile_error.Create (err_recursive_call_not_allowed_use_result_instead, access.node_id_src_loc)
                        else
                           raise compile_error.Create(err_function_cannot_be_called_without_using_function_result, access.src_loc)
                     finally
                        access.Release
                     end;
                  procedure_access:
                     result := target_cpu.TRoutineCallStatement_CreateFromSourceTokens(access);
                  property_access:
                     if access.node_property.set_proc = nil then
                        try
                           raise compile_error.Create(err_property_has_no_setter, access.node_id_src_loc)
                        finally
                           access.Release
                        end
                     else
                        result := target_cpu.TAssignmentStatement_CreateFromSourceTokens(access);
                  constant_access, structured_constant_access:
                     try
                        raise compile_error.Create(err_constant_not_allowed_here, access.src_loc)
                     finally
                        access.Release
                     end;
               else
                  assert(false)
               end
            end
         else
            result := nil; // empty statement
         if result <> nil then
            result.src_loc := src_loc
      except
         on e: EStatementSimplification do result := e.simplified_statement
      end
   end;


// =============
//  TStatement

constructor TStatement.Create
   (k: TStatementKind
   );
   begin
      inherited Create(statement_definition);
      src_loc := lex.token.src_loc;
      statement_kind := k
   end;

//===================
//  TAssertStatement

constructor TAssertStatement.CreateFromSourceTokens;
   var
      cexpr: TCExpression;
   begin
      inherited Create(assert_statement);
      assert(lex.token_is_reserved_word(rw_assert));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      boolean_expression := CreateBooleanExpressionFromSourceTokens;

      if not lex.token_is_symbol(sym_comma) then
         raise compile_error.Create(err_comma_expected);
      lex.advance_token;

      cexpr := nil;
      try
         cexpr := TCExpression.CreateFromSourceTokens;
         if cexpr.constant_kind <> string_constant then
            raise compile_error.Create (err_string_expected, cexpr.src_loc);
         assertion_message := cexpr.s
      finally
         cexpr.Free
      end;

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      end_src_loc := lex.token.src_loc;
      lex.advance_token
   end;

destructor TAssertStatement.Destroy;
   begin
      boolean_expression.Release;
      inherited
   end;

procedure TAssertStatement.MarkAsReachable;
   begin
      boolean_expression.MarkAsReachable
   end;

function TAssertStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := boolean_expression.CheckForProhibitedDelayCall (err_msg)
   end;

// =======================
// TAssignmentStatement

constructor TAssignmentStatement.CreateFromSourceTokens
   (acc: TAccess
   );
   var
      start_idx: integer;
   begin
      inherited Create(assignment_statement);
      assignee := acc;

      with assignee do
         if node_access_kind <> property_access then
            begin
               BlockStack.CheckAddressReachability(base_variable, src_loc);
               check_for_write_access(base_variable, src_loc)
            end;

      if assignee.is_maxstrlen_attribute then
         raise compile_error.Create (err_maxstrlen_attribute_is_read_only, assignee.node_id_src_loc);

      if assignee.node_typedef.ReadOnly then
         raise compile_error.Create (err_cannot_assign_to_kernel_variable, assignee.node_id_src_loc);

      if assignee.node_typedef.ContainsSystemType then
         raise compile_error.Create (err_cannot_assign_to_system_type, assignee.node_id_src_loc);

      if assignee.node_typedef.ContainsQueueVariables then
         raise compile_error.Create (err_cannot_assign_to_queue_type, assignee.node_id_src_loc);

      if not lex.token_is_symbol(sym_assign) then
         raise compile_error.Create(err_assignment_operator_expected);
      assignment_operator_src_loc := lex.token.src_loc;
      lex.advance_token;

      case assignee.node_typedef.type_kind of
         basic_data_type,
         set_type:
            expression := CreateExpressionFromSourceTokens;
         string_type:  // string assignment allows constant expressions (i.e. "+"), otherwise must be a single primary
            try
               start_idx := lex.token_idx;
               expression := CreateExpressionFromSourceTokens;
               if not expression.contains_constant then
                  begin
                     expression.Release;
                     lex.token_idx := start_idx;
                     raise compile_error.Create ('')
                  end;
            except
               on e: compile_error do
                  expression := CreatePrimaryFromSourceTokens
            end;
         record_type,
         array_type,
         system_type,
         packed_record_type,
         overlay_type:
            expression := CreatePrimaryFromSourceTokens;
      else
         assert (false)
      end;

      assignee.node_typedef.CheckAssignmentCompatability(expression);

      if assignee.node_typedef.type_kind = string_type then
         begin  // check special assignment rules for strings
            if assignee.node_property <> nil then
               begin  // assignee is a property
                  if expression is TVariableAccessPrimary then
                     begin
                        if TVariableAccessPrimary(expression).access.base_variable.descriptor = rw_rom then
                           raise compile_error.Create (err_rom_string_cannot_be_assigned_to_a_property, expression.src_loc);
                        if TVariableAccessPrimary(expression).access.base_variable.descriptor = rw_eeprom then
                           raise compile_error.Create (err_eeprom_string_cannot_be_assigned_to_a_property, expression.src_loc);
                     end
                  else if expression is TConstantPrimary then
                     raise compile_error.Create (err_constant_string_cannot_be_assigned_to_a_property, expression.src_loc)
                  else if expression is TFunctionAccessPrimary then
                     raise compile_error.Create(err_string_function_result_cannot_be_assigned_to_a_property, expression.src_loc);
               end
            else if assignee.base_variable.descriptor = rw_eeprom then
               begin
                  if expression is TFunctionAccessPrimary then
                     raise compile_error.Create(err_string_function_result_may_not_be_assigned_to_eeprom_variable, expression.src_loc);
               end;
         end;

      last_token_src_loc := lex.previous_token_src_loc
   end;

destructor TAssignmentStatement.Destroy;
   begin
      assignee.Release;
      expression.Release
   end;

procedure TAssignmentStatement.MarkAsReachable;
   begin
      assignee.MarkAsReachable;
      expression.MarkAsReachable
   end;

function TAssignmentStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := assignee.CheckForProhibitedDelayCall (err_msg)
                or
                expression.CheckForProhibitedDelayCall (err_msg)
   end;

// =========================
// TAwaitInterruptStatement

constructor TAwaitInterruptStatement.CreateFromSourceTokens;
   var
      idx: integer;
   begin
      inherited Create(await_statement);

      assert(lex.token_is_reserved_word(rw_await));

      if BlockStack.ProcessingRoutineBlock then
         idx := BlockStack.tos_idx-1
      else
         idx := BlockStack.tos_idx;
      if TSystemType(BlockStack.stk[idx]).system_type_kind <> process_system_type then
         raise compile_error.Create(err_await_interrupt_statement_must_be_in_process_code);
      if TSystemType(BlockStack.stk[idx]).priority <= 0 then
         raise compile_error.Create (err_interrupt_process_priority_not_positive);
      containing_process := TSystemType(BlockStack.stk[idx]);

      TSystemType(BlockStack.stk[idx]).interrupt_process := true;

      lex.advance_token;

      if not lex.token_is_reserved_word(rw_interrupt) then
         raise compile_error.Create(err_interrupt_expected);
      lex.advance_token
   end;


// =================
// TCaseStatement

function label_spec_is_ascending (v1,v2: TObject): boolean;  //   result := v1 <= v2
   begin
      if TCaseStatement.TLabelSpec(v1).case_index < TCaseStatement.TLabelSpec(v2).case_index then
         result := true
      else if TCaseStatement.TLabelSpec(v1).case_index > TCaseStatement.TLabelSpec(v2).case_index then
         result := false
      else  // TLabelSpec(v1).case_index = TLabelSpec(v2).case_index -- this will raise an compile error exception later
         result := TCaseStatement.TLabelSpec(v1).labeled_statement_idx <= TCaseStatement.TLabelSpec(v2).labeled_statement_idx
   end;

constructor TCaseStatement.CreateFromSourceTokens;
   var
      labeled_statement_idx, i, first, last: integer;
      first_cexpr, last_cexpr: TCExpression;
      label_specs: TSmoothSortArray;

   procedure add_label_spec (case_index: integer; case_index_src_loc: TSourceLocation);
      var i: integer;
      begin
         i := Length (label_specs);
         SetLength (label_specs, i+1);
         label_specs[i] := TLabelSpec.Create;
         TLabelSpec(label_specs[i]).case_index := case_index;
         TLabelSpec(label_specs[i]).case_index_src_loc := case_index_src_loc;
         TLabelSpec(label_specs[i]).labeled_statement_idx := labeled_statement_idx
      end;

   begin
      inherited Create(case_statement);
      first_cexpr := nil;  // suppress compiler warning
      last_cexpr :=  nil;  // suppress compiler warning

      case_label_range_tree := TBalancedBinaryTree.Create;

      assert(lex.token_is_reserved_word(rw_case));
      lex.advance_token;

      selection_expression := CreateExpressionFromSourceTokens;

      if not (selection_expression.expression_kind in ordinal_expression_kinds) then
         raise compile_error.Create(err_ordinal_expression_expected, selection_expression.src_loc);

      if not lex.token_is_reserved_word(rw_of) then
         raise compile_error.Create(err_of_expected);
      of_src_loc := lex.token.src_loc;
      lex.advance_token;

      try
         repeat    // once per statement
            labeled_statement_idx := Length(labeled_statements);
            SetLength(labeled_statements, labeled_statement_idx + 1);

            repeat    // once per case index or case index..case index
               try
                  first_cexpr := TCExpression.CreateFromSourceTokens;
                  if lex.token_is_symbol(sym_dot_dot) then
                     begin
                        lex.advance_token;
                        last_cexpr := TCExpression.CreateFromSourceTokens
                     end
                  else
                     begin
                        last_cexpr := first_cexpr;
                        last_cexpr.AddRef
                     end;

                  case selection_expression.expression_kind of
                     integer_expression:
                        begin
                           if first_cexpr.constant_kind <> integer_constant then
                              raise compile_error.Create(err_integer_constant_expected, first_cexpr.src_loc);
                           if last_cexpr.constant_kind <> integer_constant then
                              raise compile_error.Create(err_integer_constant_expected, last_cexpr.src_loc)
                        end;
                     char_expression:
                        begin
                           if (first_cexpr.constant_kind <> string_constant)
                              or
                              (Length(first_cexpr.s) <> 1) then
                              raise compile_error.Create(err_char_constant_expected, first_cexpr.src_loc);
                           if (last_cexpr.constant_kind <> string_constant)
                              or
                              (Length(last_cexpr.s) <> 1) then
                              raise compile_error.Create(err_char_constant_expected, last_cexpr.src_loc)
                        end;
                     boolean_expression:
                        begin
                           if (first_cexpr.constant_kind <> boolean_constant) then
                              raise compile_error.Create(err_boolean_constant_expected, first_cexpr.src_loc);
                           if (last_cexpr.constant_kind <> boolean_constant) then
                              raise compile_error.Create(err_boolean_constant_expected, last_cexpr.src_loc)
                        end;
                     enum_expression:
                        begin
                           if first_cexpr.constant_kind <> enum_constant then
                              raise compile_error.Create(err_enum_constant_expected, first_cexpr.src_loc);
                           if first_cexpr.enum_typedef <> selection_expression.enum_typedef then
                              raise compile_error.Create(err_incompatible_enum_type, first_cexpr.src_loc);
                           if last_cexpr.constant_kind <> enum_constant then
                              raise compile_error.Create(err_enum_constant_expected, last_cexpr.src_loc);
                           if last_cexpr.enum_typedef <> selection_expression.enum_typedef then
                              raise compile_error.Create(err_incompatible_enum_type, last_cexpr.src_loc)
                        end;
                     else
                        assert(false)
                  end;

                  if first_cexpr.AsOrdinal > last_cexpr.AsOrdinal then
                     raise compile_error.Create(err_less_than_first_of_range, last_cexpr.src_loc);

                  for i := first_cexpr.AsOrdinal to last_cexpr.AsOrdinal do
                     add_label_spec (i, last_cexpr.src_loc)

               finally
                  first_cexpr.Release;
                  last_cexpr.Release
               end;

               if not lex.token_is_symbol(sym_colon) then
                  begin
                     if not lex.token_is_symbol(sym_comma) then
                        raise compile_error.Create(err_colon_expected);
                     lex.advance_token;

                     if lex.token_is_symbol(sym_colon) then
                        raise compile_error.Create(err_case_label_expected)
                  end
            until lex.token_is_symbol(sym_colon);
            labeled_statements[labeled_statement_idx].colon_src_loc := lex.token.src_loc;
            lex.advance_token;

            labeled_statements[labeled_statement_idx].statement := process_statement_from_source_tokens (false);

            if not lex.token_is_reserved_word([rw_otherwise, rw_end]) then
               begin
                  if not lex.token_is_symbol(sym_semicolon) then
                     raise compile_error.Create(err_semicolon_expected);
                  lex.advance_token
               end

         until lex.token_is_reserved_word([rw_otherwise, rw_end]);

         if lex.token_is_reserved_word(rw_otherwise) then
            begin
               otherwise_src_loc := lex.token.src_loc;
               lex.advance_token;
               otherwise_statement := process_statement_from_source_tokens (false)
            end;

         if not lex.token_is_reserved_word(rw_end) then
            raise compile_error.Create(err_end_expected);
         end_src_loc := lex.token.src_loc;
         lex.advance_token;

         SmoothSort (label_specs, label_spec_is_ascending);

         // ensure all case indexes are unique
         for i := 1 to Length(label_specs)-1 do
            if TLabelSpec(label_specs[i-1]).case_index = TLabelSpec(label_specs[i]).case_index then
               raise compile_error.Create(err_duplicate_case_label, TLabelSpec(label_specs[i]).case_index_src_loc);

         if selection_expression.contains_constant then
            begin
               for i := 0 to Length(label_specs)-1 do
                  if selection_expression.constant.AsOrdinal = TLabelSpec(label_specs[i]).case_index then
                     raise EStatementSimplification.Create(labeled_statements[TLabelSpec(label_specs[i]).labeled_statement_idx].statement);
               raise EStatementSimplification.Create(otherwise_statement)
            end;

         // compbine adjacent case indexes for same statement then place in case label tree
         i := 0;
         while i < Length(label_specs) do
            begin
               first := TLabelSpec(label_specs[i]).case_index;
               last := first;
               labeled_statement_idx := TLabelSpec(label_specs[i]).labeled_statement_idx;
               while (i+1 < Length(label_specs))
                     and
                     (last+1 = TLabelSpec(label_specs[i+1]).case_index)
                     and
                     (labeled_statement_idx = TLabelSpec(label_specs[i+1]).labeled_statement_idx) do
                  begin
                     i := i + 1;
                     last := TLabelSpec(label_specs[i]).case_index
                  end;
               case_label_range_tree.add(create_case_entry (Self, labeled_statement_idx, first, last));
               i := i + 1
            end;

         TCaseLabelRangeEntry(case_label_range_tree.root).set_fence (selection_expression.info.min_value.AsInteger-1, selection_expression.info.max_value.AsInteger+1)
      finally
         for i := 0 to Length(label_specs)-1 do
            label_specs[i].Free
      end
   end;

destructor TCaseStatement.Destroy;
   var
      i: integer;
   begin
      case_label_range_tree.Free;
      selection_expression.Release;
      for i := 0 to Length(labeled_statements) - 1 do
         labeled_statements[i].statement.Release;
      otherwise_statement.Release
   end;

function TCaseStatement.create_case_entry (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer): TCaseLabelRangeEntry;
   begin
      result := TCaseLabelRangeEntry.create (_case_stmt, _labeled_statement_idx, _first_of_range, _last_of_range)
   end;

procedure TCaseStatement.MarkAsReachable;
   var
      i: integer;
   begin
      inherited;
      selection_expression.MarkAsReachable;
      for i := 0 to Length(labeled_statements) - 1 do
         if labeled_statements[i].statement <> nil then
            labeled_statements[i].statement.MarkAsReachable;
      if otherwise_statement <> nil then
         otherwise_statement.MarkAsReachable
   end;

function TCaseStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := selection_expression.CheckForProhibitedDelayCall (err_msg);
      for i := 0 to Length(labeled_statements) - 1 do
         if (labeled_statements[i].statement <> nil)
            and
            labeled_statements[i].statement.CheckForProhibitedDelayCall (err_msg)
         then
            result := true;
      if (otherwise_statement <> nil)
         and
         otherwise_statement.CheckForProhibitedDelayCall (err_msg)
      then
         result := true
   end;

constructor TCaseStatement.TCaseLabelRangeEntry.Create (_case_stmt: TCaseStatement; _labeled_statement_idx, _first_of_range, _last_of_range: integer);
   begin
      inherited Create;
      case_stmt := _case_stmt;
      labeled_statement_idx := _labeled_statement_idx;
      first_of_range := _first_of_range;
      last_of_range := _last_of_range
   end;

function TCaseStatement.TCaseLabelRangeEntry.compare (a: TBalancedTreeEntry): shortint;
   var
      a_entry: TCaseLabelRangeEntry;
   begin
      a_entry := TCaseLabelRangeEntry(a);
      if a_entry.first_of_range < first_of_range then
         result := -1
      else if a_entry.first_of_range > first_of_range then
         result := +1
      else
         result := 0
   end;

procedure TCaseStatement.TCaseLabelRangeEntry.copy (ToA: TBalancedTreeEntry);
   begin
      TCaseLabelRangeEntry(ToA).first_of_range := first_of_range;
      TCaseLabelRangeEntry(ToA).last_of_range := last_of_range;
      TCaseLabelRangeEntry(ToA).labeled_statement_idx := labeled_statement_idx
   end;

procedure TCaseStatement.TCaseLabelRangeEntry.set_fence (_lower_fence, _upper_fence: integer);
   begin
      lower_fence := _lower_fence;
      upper_fence := _upper_fence;
      if lesser_values <> nil then
         TCaseStatement.TCaseLabelRangeEntry(lesser_values).set_fence (lower_fence, first_of_range);
      if greater_values <> nil then
         TCaseStatement.TCaseLabelRangeEntry(greater_values).set_fence (last_of_range, upper_fence)
   end;

function TCaseStatement.TCaseLabelRangeEntry.left_test_needed: boolean;
   begin
      result := lower_fence + 1 <> first_of_range
   end;

function TCaseStatement.TCaseLabelRangeEntry.right_test_needed: boolean;
   begin
      result := upper_fence - 1 <> last_of_range
   end;



// =====================
// TContinueStatement

constructor TContinueStatement.CreateFromSourceTokens;
   begin
      inherited Create(continue_statement);
      assert(lex.token_is_reserved_word(rw_continue));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      queue_access := target_cpu.TAccess_CreateFromSourceTokens;

      if (queue_access.node_typedef = nil)
         or
         (queue_access.node_typedef.type_kind <> queue_type)
      then
         raise compile_error.Create(err_queue_variable_expected, queue_access.node_id_src_loc);

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      if (not BlockStack.ProcessingRoutineBlock)
         or
         (not TRoutine(BlockStack.tos).entry) then
         raise compile_error.Create (err_continue_can_only_be_called_from_a_monitor_entry_routine, src_loc);

      containing_monitor := TSystemType(BlockStack.stk[BlockStack.tos_idx-1]);
      containing_routine := TRoutine(BlockStack.tos)
   end;

destructor TContinueStatement.Destroy;
   begin
      queue_access.Release
   end;

procedure TContinueStatement.MarkAsReachable;
   begin
      inherited;
      queue_access.MarkAsReachable
   end;

//=================
// TCycleStatement

constructor TCycleStatement.CreateFromSourceTokens;
   begin
      inherited Create(cycle_statement);
      CycleStatementStack.push(Self);
      try
         assert(lex.token_is_reserved_word(rw_cycle));
         src_loc := lex.token.src_loc;
         lex.advance_token;

         statement_list :=
            target_cpu.TStatementList_CreateFromSourceTokens(termination_test, err_semicolon_or_repeat_expected, false);

         if not lex.token_is_reserved_word(rw_repeat) then
            raise compile_error.Create(err_repeat_expected);
         repeat_token_src_loc := lex.token.src_loc;
         lex.advance_token;
      finally
         CycleStatementStack.pop
      end
   end;

destructor TCycleStatement.Destroy;
   begin
      statement_list.Release
   end;

procedure TCycleStatement.MarkAsReachable;
   begin
      inherited;
      statement_list.MarkAsReachable
   end;

function TCycleStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := statement_list.CheckForProhibitedDelayCall (err_msg)
   end;

function TCycleStatement.termination_test: boolean;
   begin
      result := lex.token_is_reserved_word(rw_repeat);
      if result then
         case lex.next_token.token_kind of
            reserved_word_token:
               result := lex.next_token.rw in [rw_end, rw_else, rw_otherwise, rw_until, rw_repeat];
            symbol_token:
               result := lex.next_token.symbol = sym_semicolon;
         else
            result := false
         end
   end;


// =================
//  TDelayStatement

constructor TDelayStatement.CreateFromSourceTokens;
   var
      idx: integer;
   begin
      inherited Create(delay_statement);
      assert(lex.token_is_reserved_word(rw_delay));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      queue_access := target_cpu.TAccess_CreateFromSourceTokens;

      if (queue_access.node_typedef = nil)
         or
         (queue_access.node_typedef.type_kind <> queue_type)
      then
         raise compile_error.Create(err_queue_variable_expected, queue_access.node_id_src_loc);

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      if BlockStack.ProcessingRoutineBlock then
         idx := BlockStack.tos_idx-1
      else
         idx := BlockStack.tos_idx;
      containing_monitor := TSystemType(BlockStack.stk[idx])
   end;

destructor TDelayStatement.Destroy;
   begin
      queue_access.Release
   end;

procedure TDelayStatement.MarkAsReachable;
   begin
      inherited;
      queue_access.MarkAsReachable
   end;

function TDelayStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := true
   end;


// ====================
// TExitLoopStatment

constructor TExitLoopStatement.CreateFromSourceTokens;
   begin
      inherited Create(exitloop_statement);

      if LoopStatementStack.tos = nil then
         raise compile_error.Create(err_exitloop_only_allowed_inside_loop);
      containing_loop_statement := TLoopStatement(LoopStatementStack.tos);
      containing_loop_statement.possible_loop_exit_declared := true;

      assert(lex.token_is_reserved_word(rw_exitloop));
      lex.advance_token;

      if lex.token_is_reserved_word(rw_if) then
         begin
            lex.advance_token;

            exitloop_condition := CreateBooleanExpressionFromSourceTokens
         end
   end;

destructor TExitLoopStatement.Destroy;
   begin
      exitloop_condition.Release
   end;

procedure TExitLoopStatement.MarkAsReachable;
   begin
      inherited;
      if exitloop_condition <> nil then
         exitloop_condition.MarkAsReachable
   end;

function TExitLoopStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      if exitloop_condition = nil then
         result := false
      else
         result := exitloop_condition.CheckForProhibitedDelayCall (err_msg)
   end;


// ===============
// TForStatement

constructor TForStatement.CreateFromSourceTokens;
   begin
      inherited Create(for_statement);
      assert(lex.token_is_reserved_word(rw_for));
      lex.advance_token;

      if not lex.token_is_identifier then
         raise compile_error.Create(err_identifier_expected);
      if CurrentDefinitionTable[lex.token.identifier_idx].definition_kind <> variable_definition then
         raise compile_error.Create(err_loop_control_variable_expected);
      control_variable := TVariable(CurrentDefinitionTable[lex.token.identifier_idx]);
      control_variable.AddRef;
      if not ((control_variable.typedef.type_kind = basic_data_type)
              and
              (TBasicDataType(control_variable.typedef).basic_data_type_kind = ordinal_data_type)
             ) then
         raise compile_error.Create(err_loop_control_variable_must_be_ordinal);
      if not BlockStack.IsLocalRAMVariable(control_variable) then
         raise compile_error.Create(err_for_loop_control_variable_must_be_local_variable);
      check_for_write_access(control_variable, lex.token.src_loc);
      lex.advance_token;

      assert(control_variable.descriptor = rw_var);
      control_variable.descriptor := rw_for;

      if not lex.token_is_symbol(sym_assign) then
         raise compile_error.Create(err_assignment_operator_expected);
      lex.advance_token;

      initial_value_expression := CreateExpressionFromSourceTokens;
      control_variable.typedef.CheckAssignmentCompatability(initial_value_expression);

      if lex.token_is_reserved_word(rw_to) then
         step := increment_control_variable
      else if lex.token_is_reserved_word(rw_downto) then
         step := decrement_control_variable
      else
         raise compile_error.Create(err_to_or_downto_expected);
      to_or_downto_src_loc := lex.token.src_loc;
      lex.advance_token;

      final_value_expression := CreateExpressionFromSourceTokens;
      control_variable.typedef.CheckAssignmentCompatability(final_value_expression);

      if not lex.token_is_reserved_word(rw_do) then
         raise compile_error.Create(err_do_expected);
      do_src_loc := lex.token.src_loc;
      lex.advance_token;

      statement := process_statement_from_source_tokens (false);
      last_src_loc := lex.previous_token_src_loc;

      control_variable.descriptor := rw_var
   end;

destructor TForStatement.Destroy;
   begin
      control_variable.Release;
      initial_value_expression.Release;
      final_value_expression.Release;
      statement.Release
   end;

procedure TForStatement.MarkAsReachable;
   begin
      inherited;
      control_variable.MarkAsReachable;
      initial_value_expression.MarkAsReachable;
      final_value_expression.MarkAsReachable;
      if statement <> nil then
         statement.MarkAsReachable
   end;

function TForStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := initial_value_expression.CheckForProhibitedDelayCall (err_msg);
      if final_value_expression.CheckForProhibitedDelayCall (err_msg) then
         result := true;
      if statement <> nil then
         if statement.CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;


// ===============
// TIfStatement

constructor TIfStatement.CreateFromSourceTokens;
   var
      idx, i: integer;
   begin
      inherited Create(if_statement);

      assert(lex.token_is_reserved_word(rw_if));
      lex.advance_token;

      idx := Length(conditional_statement_list);
      SetLength(conditional_statement_list, idx + 1);
      conditional_statement_list[idx].boolean_expression := CreateBooleanExpressionFromSourceTokens;
      conditional_statement_list[idx].last_boolean_expression_token_src_loc := lex.previous_token_src_loc;

      if not lex.token_is_reserved_word(rw_then) then
         raise compile_error.Create(err_then_expected);
      lex.advance_token;

      conditional_statement_list[idx].statement := process_statement_from_source_tokens (false);
      conditional_statement_list[idx].last_statement_token_src_loc := lex.previous_token_src_loc;

      while lex.token_is_reserved_word(rw_else)
            and
            (lex.next_token.token_kind = reserved_word_token)
            and
            (lex.next_token.rw = rw_if) do
         begin
            lex.advance_token;  // else
            lex.advance_token;  // if

            idx := Length(conditional_statement_list);
            SetLength(conditional_statement_list, idx + 1);

            conditional_statement_list[idx].boolean_expression := CreateBooleanExpressionFromSourceTokens;
            conditional_statement_list[idx].last_boolean_expression_token_src_loc := lex.previous_token_src_loc;

            if not lex.token_is_reserved_word(rw_then) then
               raise compile_error.Create(err_then_expected);
            lex.advance_token;

            conditional_statement_list[idx].statement := process_statement_from_source_tokens (false);
            conditional_statement_list[idx].last_statement_token_src_loc := lex.previous_token_src_loc
         end;

      if lex.token_is_reserved_word(rw_else) then
         begin
            last_else_statement_token_src_loc := lex.token.src_loc;
            lex.advance_token;
            else_statement := process_statement_from_source_tokens (false)
         end;

      // optimize out dead code

      // remove all false constant condition blocks
      idx := 0;
      while idx < Length(conditional_statement_list) do
         if (conditional_statement_list[idx].boolean_expression.contains_constant)
            and
            (conditional_statement_list[idx].boolean_expression.boolean_constant_value = false) then
            begin
               conditional_statement_list[idx].boolean_expression.Release;
               conditional_statement_list[idx].statement.Release;
               for i := idx to Length(conditional_statement_list) - 2 do
                  conditional_statement_list[i] := conditional_statement_list[i+1];
               SetLength(conditional_statement_list,
               Length(conditional_statement_list) - 1)
            end
         else
            idx := idx + 1;

      if Length(conditional_statement_list) = 0 then
         raise EStatementSimplification.Create(else_statement)
      else
      // if first condition is always true, everything else is irrelevent
         if (conditional_statement_list[0].boolean_expression.contains_constant)
            and
            (conditional_statement_list[0].boolean_expression.boolean_constant_value = true) then
            raise EStatementSimplification.Create(conditional_statement_list[0].statement);

      // if any subsequent condition is always true, then everything that follows is irrelevent
      idx := 1; // after first block (which is not always true)
      while idx < Length(conditional_statement_list) do
         if (conditional_statement_list[idx].boolean_expression.contains_constant)
            and
            (conditional_statement_list[idx].boolean_expression.boolean_constant_value = true) then
            begin
               else_statement.Release;
               else_statement := conditional_statement_list[idx].statement;
               conditional_statement_list[idx].boolean_expression.Release;
               for i := idx + 1 to Length(conditional_statement_list) - 1 do
                  begin
                     conditional_statement_list[i].boolean_expression.Release;
                     conditional_statement_list[i].statement.Release
                  end;
               SetLength(conditional_statement_list, idx)
            end
         else
            idx := idx + 1
   end;

destructor TIfStatement.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(conditional_statement_list) - 1 do
         begin
            conditional_statement_list[i].boolean_expression.Release;
            conditional_statement_list[i].statement.Release
         end;
      else_statement.Release
   end;

procedure TIfStatement.MarkAsReachable;
   var
      i: integer;
   begin
      inherited;
      for i := 0 to Length(conditional_statement_list) - 1 do
         begin
            conditional_statement_list[i].boolean_expression.MarkAsReachable;
            conditional_statement_list[i].statement.MarkAsReachable
         end;
      else_statement.MarkAsReachable
   end;

function TIfStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(conditional_statement_list) - 1 do
         begin
            if conditional_statement_list[i].boolean_expression.CheckForProhibitedDelayCall (err_msg) then
               result := true;
            if (conditional_statement_list[i].statement <> nil)
               and
               conditional_statement_list[i].statement.CheckForProhibitedDelayCall (err_msg)
            then
               result := true
         end;
      if (else_statement <> nil)
         and
         else_statement.CheckForProhibitedDelayCall (err_msg)
      then
         result := true
   end;


// =================
// TInitStatement

constructor TInitStatement.CreateFromSourceTokens;
   var
      access: TAccess;
      initlist_idx,param_idx: integer;
      done: boolean;
   begin
      inherited Create(init_statement);

      assert(lex.token_is_reserved_word(rw_init));
      lex.advance_token;

      repeat
         initlist_idx := Length(initlist);
         SetLength(initlist, initlist_idx + 1);
         access := target_cpu.TAccess_CreateFromSourceTokens;
         if (access.node_access_kind <> variable_access)
            or
            (access.node_typedef.type_kind <> system_type) then
            raise compile_error.Create(err_system_type_variable_expected, access.src_loc);

         if not BlockStack.IsLocalRAMVariable(access.base_variable) then
            begin
               access.Release;
               raise compile_error.Create(err_system_type_variable_can_only_be_initialized_in_the_initial_statment_of_where_it_was_declared, access.src_loc)
            end;

         initlist[initlist_idx].access := access;
         initlist[initlist_idx].parameters := TSystemType(access.node_typedef).parameters.AssembleAndCheckCallerParameterListFromSourceTokens;
         initlist[initlist_idx].src_loc := lex.previous_token_src_loc;

         access.base_variable.init_statement_called := true;

         if access.node_typedef.IsProcessSystemType then
            for param_idx := 0 to Length(initlist[initlist_idx].parameters)-1 do
               if (initlist[initlist_idx].parameters[param_idx].definition_kind = access_definition)
                  and
                  (TAccess(initlist[initlist_idx].parameters[param_idx]).node_typedef.IsInterruptType)
                  and
                  (TAccess(initlist[initlist_idx].parameters[param_idx]).base_variable <> access.base_variable.interrupt)
               then
                  raise compile_error.Create (format (err_interrupt_variable_parameter_must_match_interrupt_assigned_to_process,
                                                      [access.base_variable.interrupt.name]
                                                     ),
                                              TAccess(initlist[initlist_idx].parameters[param_idx]).src_loc
                                             );

         done := not lex.token_is_symbol(sym_comma);
         if not done then
            lex.advance_token
      until done
   end;

destructor TInitStatement.Destroy;
   var
      i, j: integer;
   begin
      for i := 0 to Length(initlist) - 1 do
         begin
            initlist[i].access.Release;
            for j := 0 to Length(initlist[i].parameters) - 1 do
               initlist[i].parameters[j].Release
         end
   end;

procedure TInitStatement.MarkAsReachable;
   var
      i, j: integer;
   begin
      inherited;
      for i := 0 to Length(initlist)-1 do
         begin
            initlist[i].access.MarkAsReachable;
            for j := 0 to Length(initlist[i].parameters) - 1 do
               initlist[i].parameters[j].MarkAsReachable
         end
   end;

function TInitStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i,j: integer;
   begin
      result := false;
      for i := 0 to Length(initlist)-1 do
         begin
            if (not TSystemType(initlist[i].access.node_typedef).IsProcessSystemType)
               and
               (TSystemType(initlist[i].access.node_typedef).initial_statement.CheckForProhibitedDelayCall (''))
            then
               begin
                  if err_msg <> '' then
                     raise compile_error.Create (err_msg, initlist[i].access.node_id_src_loc);
                  result := true
               end;
            for j := 0 to Length(initlist[i].parameters)-1 do
               if initlist[i].parameters[j].CheckForProhibitedDelayCall ('') then
                  begin
                     if err_msg <> '' then
                        case initlist[i].parameters[j].definition_kind of
                           access_definition:
                              raise compile_error.Create (err_msg,
                                                          TAccess(initlist[i].parameters[j]).node_id_src_loc
                                                         );
                           expression_definition:
                              raise compile_error.Create (err_msg,
                                                          TExpression(initlist[i].parameters[j]).src_loc
                                                         );
                        else
                           assert (false)
                        end;
                     result := true
                  end
         end
   end;


// ====================
// TReCycleStatment

constructor TReCycleStatement.CreateFromSourceTokens;
   begin
      inherited Create(err_cant_call_public_routine_from_same_type);

      if CycleStatementStack.tos = nil then
         raise compile_error.Create(err_recycle_only_allowed_inside_cycle);
      containing_cycle_stmt := TCycleStatement(CycleStatementStack.tos);

      assert(lex.token_is_reserved_word(rw_recycle));
      lex.advance_token;

      if lex.token_is_reserved_word(rw_if) then
         begin
            lex.advance_token;

            recycle_condition := CreateBooleanExpressionFromSourceTokens
         end
   end;

destructor TReCycleStatement.Destroy;
   begin
      recycle_condition.Release
   end;

procedure TReCycleStatement.MarkAsReachable;
   begin
      inherited;
      if recycle_condition <> nil then
         recycle_condition.MarkAsReachable
   end;

function TReCycleStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      if recycle_condition = nil then
         result := false
      else
         result := recycle_condition.CheckForProhibitedDelayCall (err_msg)
   end;


// ====================
// TReLoopStatment

constructor TReLoopStatement.CreateFromSourceTokens;
   begin
      inherited Create(reloop_statement);

      if LoopStatementStack.tos = nil then
         raise compile_error.Create(err_reloop_only_allowed_inside_loop);
      containing_loop_statement := TLoopStatement(LoopStatementStack.tos);

      assert(lex.token_is_reserved_word(rw_reloop));
      lex.advance_token;

      if lex.token_is_reserved_word(rw_if) then
         begin
            lex.advance_token;
            reloop_condition := CreateBooleanExpressionFromSourceTokens
         end
   end;

destructor TReLoopStatement.Destroy;
   begin
      reloop_condition.Release
   end;

procedure TReLoopStatement.MarkAsReachable;
   begin
      inherited;
      if reloop_condition <> nil then
         reloop_condition.MarkAsReachable
   end;

function TReLoopStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      if reloop_condition = nil then
         result := false
      else
         result := reloop_condition.CheckForProhibitedDelayCall (err_msg)
   end;


// ========================
// TRoutineCallStatement

constructor TRoutineCallStatement.CreateFromSourceTokens
   (acc: TAccess
   );
   begin
      inherited Create(routine_call_statement);
      access := acc;
      assert(access.node_access_kind = procedure_access);

      if (access.base_variable <> nil)
         and
         (access.base_variable.typedef.requires_initialization)
         and
         (not access.base_variable.init_statement_called)
      then
         raise compile_error.Create (err_variable_not_initialized, access.src_loc);

      if not access.is_strappend_attribute then
         begin
            if not access.node_routine.definition_complete then
               raise compile_error.Create (err_recursive_call_not_allowed, access.node_id_src_loc);

            if (access.node_routine.routine_kind = monitor_entry_routine) then
               case BlockStack.tos.definition_kind of
                  routine_definition:
                     if TRoutine(BlockStack.tos).context = access.node_routine.context then
                        raise compile_error.Create (err_cant_call_public_routine_of_same_monitor_type_here, access.src_loc);
                  type_definition:
                     begin
                        assert (TTypeDef(BlockStack.tos).type_kind = system_type);
                        if BlockStack.tos = access.node_routine.context then
                           raise compile_error.Create (err_cant_call_public_routine_of_same_monitor_type_here, access.src_loc)
                     end;
                  program_definition:
                     ;
               else
                  assert (false)
               end;

            if not access.node_routine.parameter_definitions.Empty then
               actual_parameters := access.node_routine.AssembleAndCheckCallerParameterListFromSourceTokens
            else   // no parameters allowed
               if lex.token_is_symbol(sym_left_parenthesis) then
                  raise compile_error.Create(err_procedure_has_no_parameters);

            call_record := target_cpu.TRoutineCallRecord_Create (access.node_routine, access.node_id_src_loc);
            BlockStack.tos.AddRoutineCallRecord (call_record)
         end
   end;

destructor TRoutineCallStatement.Destroy;
   var
      i: integer;
   begin
      access.Release;
      for i := 0 to Length(actual_parameters) - 1 do
         actual_parameters[i].Release
   end;

procedure TRoutineCallStatement.MarkAsReachable;
   var
      i: integer;
   begin
      inherited;
      access.MarkAsReachable;
      for i := 0 to Length(actual_parameters) - 1 do
         actual_parameters[i].MarkAsReachable
   end;

function TRoutineCallStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := access.CheckForProhibitedDelayCall (err_msg);
      for i := 0 to Length(actual_parameters) - 1 do
         if actual_parameters[i].CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;


// ==================
// TWhileStatement

constructor TWhileStatement.CreateFromSourceTokens;
   begin
      inherited Create(while_statement);
      assert(lex.token_is_reserved_word(rw_while));
      if LoopStatementStack.tos = nil then
         raise compile_error.Create(err_while_only_allowed_inside_loop);
      containing_loop_statement := TLoopStatement(LoopStatementStack.tos);
      containing_loop_statement.possible_loop_exit_declared := true;
      if containing_loop_statement.first_exit_src_loc.NullSourceLocation then
         containing_loop_statement.first_exit_src_loc := src_loc;
      lex.advance_token;
      boolean_expression := CreateBooleanExpressionFromSourceTokens;
      last_src_loc := lex.previous_token_src_loc
   end;

destructor TWhileStatement.Destroy;
   begin
      boolean_expression.Release;
      inherited
   end;

procedure TWhileStatement.MarkAsReachable;
   begin
      inherited;
      boolean_expression.MarkAsReachable
   end;

function TWhileStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := boolean_expression.CheckForProhibitedDelayCall (err_msg)
   end;


// ==================
// TUntilStatement

constructor TUntilStatement.CreateFromSourceTokens;
   begin
      inherited Create(until_statement);
      assert(lex.token_is_reserved_word(rw_until));
      if LoopStatementStack.tos = nil then
         raise compile_error.Create(err_until_only_allowed_inside_loop);
      containing_loop_statement := TLoopStatement(LoopStatementStack.tos);
      containing_loop_statement.possible_loop_exit_declared := true;
      if containing_loop_statement.first_exit_src_loc.NullSourceLocation then
         containing_loop_statement.first_exit_src_loc := src_loc;
      lex.advance_token;
      boolean_expression := CreateBooleanExpressionFromSourceTokens
   end;

destructor TUntilStatement.Destroy;
   begin
      boolean_expression.Release;
      inherited
   end;

procedure TUntilStatement.MarkAsReachable;
   begin
      inherited;
      boolean_expression.MarkAsReachable
   end;

function TUntilStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := boolean_expression.CheckForProhibitedDelayCall (err_msg)
   end;


// ================
// TWithStatement

constructor TWithStatement.CreateFromSourceTokens;
   begin
      inherited Create(with_statement);

      assert(lex.token_is_reserved_word(rw_with));
      lex.advance_token;

      scan_starting_at_with_variable
   end;

constructor TWithStatement.CreateFromSourceTokensStartingAtVariable;
   begin
      inherited Create(with_statement);
      scan_starting_at_with_variable
   end;

procedure TWithStatement.scan_starting_at_with_variable;
   var
      record_typedef: TRecordType;
      packed_record_typedef: TPackedRecordType;
      overlay_typedef: TOverlayType;
      c: TSystemType;
      i, j: integer;
      with_variable: TWithVariable;
      with_routine: TWithRoutine;
      with_property: TWithProperty;
   begin
      access := target_cpu.TAccess_CreateFromSourceTokens;
      case access.node_access_kind of
         variable_access:
            begin
               if not (access.node_typedef.type_kind in [record_type, packed_record_type, overlay_type, system_type]) then
                  raise compile_error.Create(err_record_or_class_or_monitor_variable_expected, access.src_loc);

               if (access.node_typedef.type_kind = system_type)
                  and
                  (TSystemType(access.node_typedef).system_type_kind = process_system_type) then
                  raise compile_error.Create(err_record_or_class_or_monitor_variable_expected, access.src_loc);

               with access do
                  BlockStack.CheckAddressReachability(base_variable, src_loc);

               CurrentDefinitionTable.EnterNewScope;

               try
                  case access.node_typedef.type_kind of
                     record_type:
                        begin
                           record_typedef := TRecordType(access.node_typedef);
                           for i := 0 to Length(record_typedef.fields) - 1 do
                              begin
                                 with_variable := TWithVariable.Create(Self, record_typedef.fields[i]);
                                 CurrentDefinitionTable.DefineForCurrentScope(record_typedef.fields[i].identifier_idx, with_variable, lex.token.src_loc);
                                 with_variable.Release
                              end
                        end;
                     packed_record_type:
                        begin
                           packed_record_typedef :=
                           TPackedRecordType(access.node_typedef);
                           for i := 0 to Length(packed_record_typedef.fields) - 1
                           do
                              begin
                                 with_variable := TWithVariable.Create(Self,
                                 packed_record_typedef.fields[i]);
                                 CurrentDefinitionTable.DefineForCurrentScope(packed_record_typedef.fields[i].identifier_idx, with_variable, lex.token.src_loc);
                                 with_variable.Release
                              end
                        end;
                     overlay_type:
                        begin
                           overlay_typedef := TOverlayType(access.node_typedef);
                           for i := 0 to Length(overlay_typedef.overlaid_variables) - 1 do
                              if overlay_typedef.overlaid_variables[i].anonymous
                              then
                                 case overlay_typedef.overlaid_variables[i].typedef.type_kind of
                                    record_type:
                                       for j := 0 to Length(TRecordType(overlay_typedef.overlaid_variables[i].typedef).fields)-1 do
                                          begin
                                             with_variable := TWithVariable.Create(Self, TRecordType(overlay_typedef.overlaid_variables[i].typedef).fields[j]);
                                             CurrentDefinitionTable.DefineForCurrentScope(TRecordType(overlay_typedef.overlaid_variables[i].typedef).fields[j].identifier_idx, with_variable, lex.token.src_loc);
                                             with_variable.Release
                                          end;
                                    packed_record_type:
                                       for j := 0 to Length(TPackedRecordType(overlay_typedef.overlaid_variables[i].typedef).fields)-1 do
                                          begin
                                             with_variable := TWithVariable.Create(Self, TPackedRecordType(overlay_typedef.overlaid_variables[i].typedef).fields[j]);
                                             CurrentDefinitionTable.DefineForCurrentScope(TPackedRecordType(overlay_typedef.overlaid_variables[i].typedef).fields[j].identifier_idx, with_variable, lex.token.src_loc);
                                             with_variable.Release
                                          end;
                                    array_type, string_type:
                                       ;  // does not expose any fields
                                    else
                                       assert(false)
                                 end
                              else // not anonymous
                                 begin
                                    with_variable := TWithVariable.Create(Self,
                                    overlay_typedef.overlaid_variables[i]);
                                    CurrentDefinitionTable.DefineForCurrentScope(overlay_typedef.overlaid_variables[i].identifier_idx, with_variable, lex.token.src_loc);
                                    with_variable.Release
                                 end
                        end;
                     system_type:
                        begin
                           c := TSystemType(access.node_typedef);
                           for i := 0 to Length(c.routines) - 1 do
                              begin
                                 with_routine := TWithRoutine.Create(Self, c.routines[i]);
                                 if c.routines[i].entry then
                                    CurrentDefinitionTable.DefineForCurrentScope(c.routines[i].routine_id_idx, with_routine, lex.token.src_loc);
                                 with_routine.Release
                              end;
                           for i := 0 to Length(c.properties) - 1 do
                              if c.properties[i].entry then
                                 begin
                                    with_property := TWithProperty.Create(Self, c.properties[i]);
                                    CurrentDefinitionTable.DefineForCurrentScope(c.properties[i].id, with_property, lex.token.src_loc);
                                    with_property.Release
                                 end
                        end;
                     else
                        raise compile_error.Create(err_record_or_class_or_monitor_variable_expected)
                  end;

                  if lex.token_is_symbol(sym_comma) then
                     begin
                        lex.advance_token;

                        statement := target_cpu.TWithStatement_CreateFromSourceTokensStartingAtVariable
                     end
                  else
                     begin
                        if not lex.token_is_reserved_word(rw_do) then
                           raise compile_error.Create(err_do_expected);
                        last_access_src_loc := lex.previous_token_src_loc;
                        do_src_loc := lex.token.src_loc;
                        lex.advance_token;

                        statement := process_statement_from_source_tokens (false)
                     end;

                  last_src_loc := lex.previous_token_src_loc
               finally
                  CurrentDefinitionTable.ExitScope
               end
            end;
         structured_constant_access:
            begin
               record_typedef := TRecordType(access.node_typedef);
               if not (record_typedef.type_kind = record_type) then
                  raise compile_error.Create(err_record_constant_expected, access.src_loc);

               CurrentDefinitionTable.EnterNewScope;

               try
                  assert (access.node_structured_constant.StructuredConstantKind = scRecord);
                  for i := 0 to Length(record_typedef.fields) - 1 do
                     begin
                        access.node_structured_constant.array_elements[i].AddRef;
                        CurrentDefinitionTable.DefineForCurrentScope
                           (record_typedef.fields[i].identifier_idx,
                            access.node_structured_constant.array_elements[i],
                            lex.token.src_loc
                           );
                        access.node_structured_constant.array_elements[i].Release
                     end;

                  if lex.token_is_symbol(sym_comma) then
                     begin
                        lex.advance_token;

                        statement := target_cpu.TWithStatement_CreateFromSourceTokensStartingAtVariable
                     end
                  else
                     begin
                        if not lex.token_is_reserved_word(rw_do) then
                           raise compile_error.Create(err_do_expected);
                        last_access_src_loc := lex.previous_token_src_loc;
                        do_src_loc := lex.token.src_loc;
                        lex.advance_token;

                        statement := process_statement_from_source_tokens (false)
                     end;

                  last_src_loc := lex.previous_token_src_loc
               finally
                  CurrentDefinitionTable.ExitScope
               end
            end;
         else
            raise compile_error.Create(err_record_or_class_or_monitor_variable_expected, access.src_loc)
      end;
   end;

destructor TWithStatement.Destroy;
   begin
      access.Release;
      statement.Release
   end;

procedure TWithStatement.MarkAsReachable;
   begin
      inherited;
      statement.MarkAsReachable
   end;

function TWithStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := statement.CheckForProhibitedDelayCall (err_msg)
   end;


// ================
// TWithVariable

constructor TWithVariable.Create
   (_with_statement: TWithStatement;
    _record_field: TDefinition
   );
   begin
      inherited Create(with_variable_definition);
      with_statement := _with_statement;
      record_field := _record_field
   end;

function TWithVariable.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := with_statement.CheckForProhibitedDelayCall (err_msg)
   end;


// ==============
// TWithRoutine

constructor TWithRoutine.Create
   (_with_statement: TWithStatement;
    _routine: TRoutine
   );
   begin
      inherited Create(with_routine_definition);
      with_statement := _with_statement;
      routine := _routine
   end;

destructor TWithRoutine.Destroy;
   begin
   end;

function TWithRoutine.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := with_statement.CheckForProhibitedDelayCall (err_msg)
   end;


// ================
// TWithProperty

constructor TWithProperty.Create
   (_with_statement: TWithStatement;
    _prop:
    TProperty
   );
   begin
      inherited Create(with_property_definition);
      with_statement := _with_statement;
      prop := _prop
   end;

destructor TWithProperty.Destroy;
   begin
   end;

function TWithProperty.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := with_statement.CheckForProhibitedDelayCall (err_msg)
   end;


// =================
// TLoopStatement

constructor TLoopStatement.CreateFromSourceTokens;
   begin
      inherited Create(loop_statement);

      LoopStatementStack.push(Self);
      try
         assert(lex.token_is_reserved_word(rw_loop));
         lex.advance_token;

         statement_list := target_cpu.TStatementList_CreateFromSourceTokens(termination_test, err_semicolon_or_repeat_expected, true);

         if not lex.token_is_reserved_word(rw_repeat) then
            raise compile_error.Create(err_repeat_expected);
         repeat_token_src_loc := lex.token.src_loc;
         lex.advance_token;

         if not possible_loop_exit_declared then
            raise compile_error.Create (err_no_loop_exit_defined, repeat_token_src_loc)
      finally
         LoopStatementStack.pop
      end
   end;

destructor TLoopStatement.Destroy;
   begin
      statement_list.Release
   end;

procedure TLoopStatement.MarkAsReachable;
   begin
      inherited;
      statement_list.MarkAsReachable
   end;

function TLoopStatement.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := statement_list.CheckForProhibitedDelayCall (err_msg)
   end;

function TLoopStatement.termination_test: boolean;
   begin
      result := lex.token_is_reserved_word(rw_repeat);
      if result then
         case lex.next_token.token_kind of
            reserved_word_token:
               result := lex.next_token.rw in [rw_end, rw_else, rw_otherwise, rw_until, rw_repeat];
            symbol_token:
               result := lex.next_token.symbol = sym_semicolon;
            else
               result := false
         end
   end;


//=================
// TStatementList

function TStatementList.get
   (i: integer
   ): TStatement;
   begin
      result := stmts[i]
   end;

procedure TStatementList.put
   (i: integer;
    stmt: TStatement
   );
   begin
      stmts[i] := stmt
   end;

constructor TStatementList.Create;
   begin
      inherited Create(statement_list)
   end;

procedure TStatementList.AddFromSourceTokens
   (terminator: TReservedWordEnumSet;
    non_terminated_error_message: string
   );
   var
      stmt: TStatement;
   begin
      while not lex.token_is_reserved_word(terminator) do
         begin
            stmt := process_statement_from_source_tokens (false);
            if stmt = nil then
               begin
                  if not (lex.token_is_symbol(sym_semicolon) or
                  lex.token_is_reserved_word(terminator)) then
                     raise compile_error.Create(err_statement_expected);
               end
            else
               AddStatement(stmt);

            if not lex.token_is_reserved_word(terminator) then
               begin
                  if not lex.token_is_symbol(sym_semicolon) then
                     raise compile_error.Create(non_terminated_error_message);
                  lex.advance_token
               end
         end
   end;

procedure TStatementList.AddStatement
   (stmt: TStatement
   );
   var
      i: integer;
   begin
      i := Length(stmts);
      SetLength(stmts, i + 1);
      stmts[i] := stmt
   end;

constructor TStatementList.CreateFromSourceTokens
   (terminator: TBooleanFunctionOfObject;
    non_terminated_error_message: string;
    while_or_until_allowed: boolean
   );
   var
      stmt: TStatement;
      while_or_until_stmt_found: boolean;
   begin
      inherited Create(statement_list);
      src_loc := lex.token.src_loc;
      while_or_until_stmt_found := false;

      while not terminator do
         begin
            stmt := process_statement_from_source_tokens (while_or_until_allowed);

            if (stmt.statement_kind = while_statement)
               or
               (stmt.statement_kind = until_statement)
            then
               begin
                  if while_or_until_stmt_found then
                     raise compile_error.Create (err_while_or_until_already_defined, stmt.src_loc);
                  while_or_until_stmt_found := true
               end;

            if stmt = nil then
               begin
                  if not (lex.token_is_symbol(sym_semicolon) or terminator) then
                     raise compile_error.Create(err_statement_expected);
               end
            else
               AddStatement(stmt);

            if not terminator then
               begin
                  if not lex.token_is_symbol(sym_semicolon) then
                     raise compile_error.Create(non_terminated_error_message);
                  lex.advance_token
               end
         end
   end;

destructor TStatementList.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(stmts) - 1 do
         stmts[i].Release
   end;

procedure TStatementList.MarkAsReachable;
   var
      i: integer;
   begin
      inherited;
      for i := 0 to Length(stmts) - 1 do
         stmts[i].MarkAsReachable
   end;

function TStatementList.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(stmts) - 1 do
         if stmts[i].CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;


INITIALIZATION
   LoopStatementStack := TDefinitionStack.Create;
   CycleStatementStack := TDefinitionStack.Create;
   temp := TMultiPrecisionInteger.Create;

FINALIZATION
   LoopStatementStack.Free;
   CycleStatementStack.Free;
   temp.Free;

END.
