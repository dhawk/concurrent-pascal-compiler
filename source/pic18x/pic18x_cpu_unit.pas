UNIT pic18x_cpu_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes, SysUtils, cpc_target_cpu_unit,
   cpc_source_analysis_unit, cpc_blocks_unit, cpc_expressions_unit,
   cpc_access_unit, cpc_core_objects_unit, cpc_statements_unit,
   cpc_definitions_unit, cpc_simple_expression_unit, cpc_term_expression_unit;

type
   TInitialValueBytes =
      array of
         record
            b: byte;
            initialization_unnecessary: boolean;   // used only for string parameters
            byte_no: integer;
            path, value: string
         end;

   Treset_TMRn_cycle =
      record
         routine: TRoutine;
         timer_number: integer
      end;

   tRAMVariableGroup = (nosuch_group, kernel_vars_group, process_pcbs_group, kernel_pcb_group, global_vars_phase, kernel_stack_phase);
   tRAMVariableMapNode =
      class
      private
         size: integer;      // only used if typedef is nil
         ParentNode: tRAMVariableMapNode;
         SubNodes: array of tRAMVariableMapNode;
         ram_variable_group: tRAMVariableGroup;
         function too_long (expanded_short_name: string): boolean;
         procedure shorten_if_needed (expanded_short_name: string);
         function separator: char;
      public
         id: string;
         shortened_id: string;
         typedef: TTypedef;  // might be nil!
         typename: string;
         is_array_index: boolean;
         array_index: integer;
         class var
            max_id_len, max_reserved_size: integer;
            reserved_format_string: string;
         constructor Create (_id: string = ''; _typename: string = ''; _size: integer = 0; _group: tRAMVariableGroup = nosuch_group);
            overload;
         constructor Create (_id: string; _typedef: TTypeDef; _ram_variable_group: tRAMVariableGroup = global_vars_phase);
            overload;
         constructor CreateArrIndex (_id: string; _typedef: TTypeDef; idx: integer);
         procedure append (new_sub_node: tRAMVariableMapNode);
         procedure calculate_output_string_lengths (expanded_short_name: string);
         procedure output_reserved (_phase: tRAMVariableGroup; expanded_name, expanded_short_name: string);
         destructor Destroy;
            override;
      end;

   TPIC18x_CPU =
      class (TTargetCPUBaseClass)
      private
         stack_usage:
            array of
               record
                  constant: string;
                  rom_addr: integer
               end;
         config_bytes: TInitialValueBytes;
         procedure append_config_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
      public
         ClearWatchdogTimer: TRoutine;
         ErrorCode: TRoutine;
         Round24, Round32: TRoutine;
         Trunc24, Trunc32: TRoutine;
         reset_TMRn_cycle: array of Treset_TMRn_cycle;
{$ifdef INCLUDE_SIMULATION}
         Test: TRoutine;
{$endif}
         constructor Create;
         function Preamble: TStringList;
            override;
         procedure process_preamble;
            override;
         procedure release_preamble;
            override;
         function process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
            override;
         procedure add_successful_compilation_info (results_listing: TStrings);
            override;
         function has_eeprom: boolean;
            override;
         function load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
            override;
         procedure validate_ioregister_address
            (name: string;
             addr: integer;
             typedef: TTypeDef;
             src_loc: TSourceLocation
            );
            override;
         function initial_process_priority: integer;
            override;
         procedure validate_process_priority
            (priority: integer;
             src_loc: TSourceLocation
            );
            override;
         function max_supported_ordinal_size: integer;
            override;
         function ioregister_width_in_address_units
            (width_in_bits: integer;
             src_loc: TSourceLocation
            ): integer;
            override;
         function round_trunc_result_type: TTypeDef;
            override;
         procedure record_anonymous_string_constant (s: string);
            override;
         function anonymous_string_constant_rom_addr (s: string): integer;
         function mod_operator_implementation: Tmod_operator_implementation;
            override;
         procedure generate_machine_code (_prog: TProgram);
            override;
         function src_directory_relative_to_bin: string;
            override;

         function TAbsFunctionPrimary_CreateFromSourceTokens: TAbsFunctionPrimary;
            override;
         function TAccess_CreateFromSourceTokens: TAccess;
            override;
         function TAccess_CreateFromVariable (v: TVariable): TAccess;
            override;
         function TAssertStatement_CreateFromSourceTokens: TAssertStatement;
            override;
         function TAssignmentStatement_CreateFromSourceTokens (acc: TAccess): TAssignmentStatement;
            override;
         function TAwaitInterruptStatement_CreateFromSourceTokens: TAwaitInterruptStatement;
            override;
         function TCaseStatement_CreateFromSourceTokens: TCaseStatement;
            override;
         function TChrTypeConversionPrimary_CreateFromSourceTokens: TChrTypeConversionPrimary;
            override;
         function TConstantPrimary_CreateFromConstant (_constant: TConstant; _src_loc: TSourceLocation): TConstantPrimary;
            override;
         function TConstantPrimary_CreateFromIdentifierConstant: TConstantPrimary;
            override;
         function TConstantPrimary_CreateFromReservedWordConstant: TConstantPrimary;
            override;
         function TConstantPrimary_CreateFromSourceTokens: TConstantPrimary;
            override;
         function TConstantPrimary_CreateFromStructuredConstantAccess (acc: TAccess): TConstantPrimary;
            override;
         function TContinueStatement_CreateFromSourceTokens: TContinueStatement;
            override;
         function TCycleStatement_CreateFromSourceTokens: TCycleStatement;
            override;
         function TDataItemList_Create: TDataItemList;
            override;
         function TDelayStatement_CreateFromSourceTokens: TDelayStatement;
            override;
         function TEmptyFunctionPrimary_CreateFromSourceTokens: TEmptyFunctionPrimary;
            override;
         function TExitLoopStatement_CreateFromSourceTokens: TExitLoopStatement;
            override;
         function TForStatement_CreateFromSourceTokens: TForStatement;
            override;
         function TFunctionAccessPrimary_CreateFromSourceTokens (acc: TAccess): TFunctionAccessPrimary;
            override;
         function TIfStatement_CreateFromSourceTokens: TIfStatement;
            override;
         function TInitStatement_CreateFromSourceTokens: TInitStatement;
            override;
         function TLoopStatement_CreateFromSourceTokens: TLoopStatement;
            override;
         function TNotPrimary_CreateFromSourceTokens: TNotPrimary;
            override;
         function TParamList_CreateFromSourceTokens (context: TParamListContext; max_scope: integer): TParamList;
            override;
         function TParamList_CreatePropertyPseudoParamList (property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TParamList;
            override;
         function TTypeInfo_Create (_info: TTypeInfo): TTypeInfo;
            override;
         function TPredFunctionPrimary_CreateFromSourceTokens: TPredFunctionPrimary;
            override;
         function TProgram_CreateFromSourceTokens: TProgram;
            override;
         function TProperty_CreateFromSourceTokens (cntxt: TDefinition; entry_property: boolean): TProperty;
            override;
         function TRelationalExpression_CreateFromSourceTokens: TRelationalExpression;
            override;
         function TReCycleStatement_CreateFromSourceTokens: TReCycleStatement;
            override;
         function TReLoopStatement_CreateFromSourceTokens: TReLoopStatement;
            override;
         function TRoundFunctionPrimary_CreateFromSourceTokens: TRoundFunctionPrimary;
            override;
         function TRoutine_CreateFromSourceTokens (cntxt: TDefinition; entry_routine: boolean): TRoutine;
            override;
         function TRoutine_CreatePropertyGetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
            override;
         function TRoutine_CreatePropertySetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
            override;
         function TRoutineCallStatement_CreateFromSourceTokens (acc: TAccess): TRoutineCallStatement;
            override;
         function TSetConstructorPrimary_CreateFromSourceTokens: TSetConstructorPrimary;
            override;
         function TSimpleExpression_CreateFromSourceTokens: TSimpleExpression;
            override;
         function TStatementList_Create: TStatementList;
            override;
         function TStatementList_CreateFromSourceTokens (terminator: TBooleanFunctionOfObject; non_terminated_error_message: string; while_or_until_allowed: boolean): TStatementList;
            override;
         function TStrPosPrimary_Create (acc: TAccess): TStrPosPrimary;
            override;
         function TSuccFunctionPrimary_CreateFromSourceTokens: TSuccFunctionPrimary;
            override;
         function TSystemType_CreateFromSourceTokens: TSystemType;
            override;
         function TTerm_CreateFromSourceTokens: TTerm;
            override;
         function TTruncFunctionPrimary_CreateFromSourceTokens: TTruncFunctionPrimary;
            override;
         function TUnaryMinusPrimary_CreateFromExpression (exp: TExpression): TUnaryMinusPrimary;
            override;
         function TUnaryMinusPrimary_CreateFromSourceTokens: TUnaryMinusPrimary;
            override;
         function TUntilStatement_CreateFromSourceTokens: TUntilStatement;
            override;
         function TVariable_Create (_id_idx: TIdentifierIdx; _context: TDefinition; _typ: TTypeDef; _definition_prefix: TVariableDescriptor; _mode: TAddressMode): TVariable;
            override;
         function TVariable_CreateForLaterDefinition (_id_idx: TIdentifierIdx; _context: TDefinition): TVariable;
            override;
         function TVariable_CreateCopy (v: TVariable): TVariable;
            override;
         function TVariableAccessPrimary_Create (acc: TAccess): TVariableAccessPrimary;
            override;
         function TVariable_CreateAnonymousROMString (_s: string): TVariable;
            override;
         function TWhileStatement_CreateFromSourceTokens: TWhileStatement;
            override;
         function TWithStatement_CreateFromSourceTokens: TWithStatement;
            override;
         function TWithStatement_CreateFromSourceTokensStartingAtVariable: TWithStatement;
            override;
      private
         ram_initialization:  TInitialValueBytes;
         procedure append_initial_value_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
      end;

// TDefinition.Generate param values
const
   GenerateCode                     = 1;
   AssignAddresses                  = 2;
   GenerateCodeToCopyToRAMString    = 3;
   GenerateCodeToCopyToEEPROMString = 4;

const
   rom_data_pointer_size = 2;
   pc_size = 3;
   real_variable_size = 4;

type
   TDynamicByteArray = array of byte;

type
   TStackUsageCounter =
      record
         Current, Max: integer;
         procedure Clear;
         procedure Push (size: integer);
         procedure Pop (size: integer);
         procedure PushPop (size: integer);
      end;
var
   StackUsageCounter: TStackUsageCounter;
   first_rom_addr: integer;

const
   ieee_single_type_name = 'ieee_single';

const
   err_use_round24_or_round32 = 'use "round24" or "round32" to round non-constant expressions';
   err_use_trunc24_or_trunc32 = 'use "trunc24" or "trunc32" to truncate non-constant expressions';
   err_processor_file_doesnt_exist = 'processor ''%s'' not supported';
   err_processor_open_source_file = 'can''t open processor include file %s';

IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
   test_pic18x_simulator_unit,
{$endif}
   pic18x_expressions_unit, math, pic18x_multiply_divide_unit, pic18x_macro_instructions_unit, pic18x_run_time_error_check_unit,
   pic18x_Statements_unit, cpc_multi_precision_integer_unit, pic18x_microprocessor_information_unit,
   cpc_types_unit, pic18x_access_unit, pic18x_term_expression_unit, pic18x_kernel_unit,
   pic18x_simple_expression_unit, pic18x_types_unit, cpc_common_unit,
   pic18x_core_objects_unit, pic18x_instructions_unit,
   pic18x_assignment_statement_unit, pic18x_blocks_unit;

const first_rom_constant_addr = 20;

constructor TPIC18x_CPU.Create;
   begin
      assert (target_cpu = nil);
      inherited Create ('PIC18x');

      add_additional_supported_data_type ('boolean', TBooleanDataType.Create (1));
      add_additional_supported_data_type ('char', TCharDataType.Create (8));
      add_additional_supported_data_type ('queue', TQueueType.Create);

      add_additional_supported_data_type ('real', TFloatingPointDataType.Create(32));
      add_additional_supported_data_type (ieee_single_type_name, TFloatingPointDataType.Create(32))
   end;

function TPIC18x_CPU.has_eeprom: boolean;
   begin
      result := pic_info.available_eeprom_memory > 0
   end;

function TPIC18x_CPU.load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
   begin
      case def.definition_kind of
         type_definition:
            if TTypeDef(def).type_kind = packed_record_type then
               result := TPIC18x_PackedRecord_TypeInfo.Create (def)
            else
               result := TPIC18x_TypeDef_TypeInfo.Create (def);
         expression_definition:
            result := TPIC18x_Expression_TypeInfo.Create (def);
         record_field_definition:
            result := TPIC18x_RecordFieldInfo.Create (def);
         packed_record_field_definition:
            result := TPIC18x_PackedRecordFieldInfo.Create (def)
      else
         result := nil
      end
   end;

function TPIC18x_CPU.preamble: TStringList;
   begin
      result := TStringList.Create;
      result.Add ('procedure ClearWatchdogTimer;');
      result.Add ('function ErrorCode: uint24;');
      result.Add ('function Round24 (r: real): int24;');
      result.Add ('function Round32 (r: real): int32;');
      result.Add ('function Trunc24 (r: real): int24;');
      result.Add ('function Trunc32 (r: real): int32;');
      {$ifdef INCLUDE_SIMULATION}
      result.Add ('procedure Test (subtest: int16; s: string);');
      {$endif}
   end;

procedure TPIC18x_CPU.process_preamble;
   var
      i: integer;
   begin
      ClearWatchdogTimer := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('ClearWatchdogTimer', true));
      ClearWatchdogTimer.AddRef;
      ErrorCode := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('ErrorCode', true));
      ErrorCode.AddRef;
      Round24 := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('Round24', true));
      Round24.AddRef;
      Round32 := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('Round32', true));
      Round32.AddRef;
      Trunc24 := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('Trunc24', true));
      Trunc24.AddRef;
      Trunc32 := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('Trunc32', true));
      Trunc32.AddRef;
      SetLength(reset_TMRn_cycle, Length(pic_info.sixteen_bit_timers));
      for i := 0 to Length(pic_info.sixteen_bit_timers)-1 do
         begin
            reset_TMRn_cycle[i].routine := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('reset_TMR' + IntToStr(pic_info.sixteen_bit_timers[i]) + '_cycle', true));
            reset_TMRn_cycle[i].routine.AddRef;
            reset_TMRn_cycle[i].timer_number := pic_info.sixteen_bit_timers[i]
         end;
      {$ifdef INCLUDE_SIMULATION}
      Test := TRoutine (CurrentDefinitionTable.GetDefinitionForIdentifier ('Test', true));
      Test.AddRef;
      {$endif}
   end;

procedure TPIC18x_CPU.release_preamble;
   var
      i: integer;
   begin
      ClearWatchdogTimer.Release;
      ClearWatchdogTimer := nil;
      ErrorCode.Release;
      ErrorCode := nil;
      Round24.Release;
      Round24 := nil;
      Round32.Release;
      Round32 := nil;
      Trunc24.Release;
      Trunc24 := nil;
      Trunc32.Release;
      Trunc32 := nil;
      for i := 0 to Length(reset_TMRn_cycle)-1 do
         reset_TMRn_cycle[i].routine.Release;
      SetLength(reset_TMRn_cycle, 0);
      {$ifdef INCLUDE_SIMULATION}
      Test.Release;
      Test := nil;
      {$endif}
   end;

function TPIC18x_CPU.process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
   const
      compiler_directive = '{$processor ';
   var
      processor: string;
      full_path_fn: string;
   begin
      if Pos (compiler_directive, simplified_line) = 1 then
         begin
            processor := extract_quoted_compiler_directive_parameter (compiler_directive, simplified_line, 'stmt', src_location);
            full_path_fn := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'pic18x' + PathDelim + 'include' + PathDelim + processor + '.inc';
            try
               read_in_file (full_path_fn, true, src_location);
               create_pic_info (processor);
{$ifdef INCLUDE_SIMULATION}
               CPU.allocate_special_sfrs;
{$endif}
            except
               on EFileDoesntExist do
                  raise compile_error.Create(format (err_processor_file_doesnt_exist, [processor]), FileErrorSourceLocation);
               on ECantOpenFile do
                  raise compile_error.Create(format (err_processor_open_source_file, [full_path_fn]), FileErrorSourceLocation)
            end;
            result := true
         end
      else
         result := false
   end;

procedure TPIC18x_CPU.add_successful_compilation_info (results_listing: TStrings);
   begin
      results_listing.Add ('Target microprocessor: ' + pic_info.microprocessor);
      results_listing.Add (format ('ROM used:     %6d bytes (%3.0f%% of %d bytes)', [program_memory_used, program_memory_used * 100 / pic_info.available_program_memory, pic_info.available_program_memory]));
      results_listing.Add (format ('RAM used:     %6d bytes (%3.0f%% of %d bytes)', [sdram_used, sdram_used * 100 / pic_info.available_SRAM, pic_info.available_SRAM]));
      results_listing.Add (format ('Bank0 used:   %6d bytes (%3.0f%% of %d bytes)', [total_bank0_used, total_bank0_used * 100 / 256, 256]));
      if pic_info.available_eeprom_memory > 0 then
         results_listing.Add (format ('EEPROM used:  %6d bytes (%3.0f%% of %d bytes)', [eeprom_used, eeprom_used * 100 / pic_info.available_eeprom_memory, pic_info.available_eeprom_memory]))
   end;

procedure TPIC18x_CPU.validate_ioregister_address
   (name: string;
    addr: integer;
    typedef: TTypeDef;
    src_loc: TSourceLocation
   );
   begin
      pic_info.validate_pic18_ioreg_address (name, addr, TPIC18x_TypeInfo(typedef.info).Size, src_loc)
   end;

function TPIC18x_CPU.initial_process_priority: integer;
   begin
      result := pic18x_kernel_unit.initial_process_priority
   end;

procedure TPIC18x_CPU.validate_process_priority
   (priority: integer;
    src_loc: TSourceLocation
   );
   begin
      if (priority <= initial_process_priority) or (priority > high(TPriorityRange)) then
         raise compile_error.Create (err_invalid_process_priority, src_loc)
   end;

function TPIC18x_CPU.max_supported_ordinal_size: integer;
   begin
      result := 64
   end;

function TPIC18x_CPU.ioregister_width_in_address_units
   (width_in_bits: integer;
    src_loc: TSourceLocation
   ): integer;
   begin
      if width_in_bits mod 8 <> 0 then
         raise compile_error.Create(err_invalid_total_ioregister_width, src_loc);
      result := width_in_bits div 8
   end;

function TPIC18x_CPU.round_trunc_result_type: TTypeDef;
   begin
      result := get_supported_data_type ('int32')
   end;

procedure TPIC18x_CPU.record_anonymous_string_constant (s: string);
   var
      i: integer;
   begin
      for i := 0 to length(stack_usage)-1 do
         if stack_usage[i].constant = s then
            exit;
      i := Length(stack_usage);
      SetLength (stack_usage, i+1);
      stack_usage[i].constant := s
   end;

function TPIC18x_CPU.anonymous_string_constant_rom_addr (s: string): integer;
   var
      i: integer;
   begin
      result := -1;  // to suppress compiler warning
      for i := 0 to length(stack_usage)-1 do
         if stack_usage[i].constant = s then
            begin
               result := stack_usage[i].rom_addr;
               exit
            end;
      assert (false)
   end;

function TPIC18x_CPU.mod_operator_implementation: Tmod_operator_implementation;
   begin
      result := delphi_mod_operator_implemenation
   end;

function TPIC18x_CPU.TAbsFunctionPrimary_CreateFromSourceTokens: TAbsFunctionPrimary;
   begin
      result := TPIC18x_AbsFunctionPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TAccess_CreateFromSourceTokens: TAccess;
   begin
      result := TPIC18x_Access.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TAccess_CreateFromVariable (v: TVariable): TAccess;
   begin
      result := TPIC18x_Access.CreateFromVariable (v)
   end;

function TPIC18x_CPU.TAssertStatement_CreateFromSourceTokens: TAssertStatement;
   begin
      result := TPIC18x_AssertStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TAssignmentStatement_CreateFromSourceTokens (acc: TAccess): TAssignmentStatement;
   begin
      result := TPIC18x_AssignmentStatement.CreateFromSourceTokens (acc)
   end;

function TPIC18x_CPU.TAwaitInterruptStatement_CreateFromSourceTokens: TAwaitInterruptStatement;
   begin
      result := TPIC18x_AwaitInterruptStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TCaseStatement_CreateFromSourceTokens: TCaseStatement;
   begin
      result := TPIC18x_CaseStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TChrTypeConversionPrimary_CreateFromSourceTokens: TChrTypeConversionPrimary;
   begin
      result := TPIC18x_ChrTypeConversionPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TConstantPrimary_CreateFromConstant (_constant: TConstant; _src_loc: TSourceLocation): TConstantPrimary;
   begin
      result := TPIC18x_ConstantPrimary.CreateFromConstant (_constant, _src_loc)
   end;

function TPIC18x_CPU.TConstantPrimary_CreateFromIdentifierConstant: TConstantPrimary;
   begin
      result := TPIC18x_ConstantPrimary.CreateFromIdentifierConstant
   end;

function TPIC18x_CPU.TConstantPrimary_CreateFromReservedWordConstant: TConstantPrimary;
   begin
      result := TPIC18x_ConstantPrimary.CreateFromReservedWordConstant
   end;

function TPIC18x_CPU.TConstantPrimary_CreateFromSourceTokens: TConstantPrimary;
   begin
      result := TPIC18x_ConstantPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TConstantPrimary_CreateFromStructuredConstantAccess (acc: TAccess): TConstantPrimary;
   begin
      result := TPIC18x_ConstantPrimary.CreateFromStructuredConstantAccess (acc);
      TPIC18x_Expression_TypeInfo(result.info).is_ieee_single :=
         acc.node_typedef = get_supported_data_type (ieee_single_type_name)
   end;

function TPIC18x_CPU.TContinueStatement_CreateFromSourceTokens: TContinueStatement;
   begin
      result := TPIC18x_ContinueStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TCycleStatement_CreateFromSourceTokens: TCycleStatement;
   begin
      result := TPIC18x_CycleStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TDelayStatement_CreateFromSourceTokens: TDelayStatement;
   begin
      result := TPIC18x_DelayStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TEmptyFunctionPrimary_CreateFromSourceTokens: TEmptyFunctionPrimary;
   begin
      result := TPIC18x_EmptyFunctionPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TExitLoopStatement_CreateFromSourceTokens: TExitLoopStatement;
   begin
      result := TPIC18x_ExitLoopStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TForStatement_CreateFromSourceTokens: TForStatement;
   begin
      result := TPIC18x_ForStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TFunctionAccessPrimary_CreateFromSourceTokens (acc: TAccess): TFunctionAccessPrimary;
   begin
      result := TPIC18x_FunctionAccessPrimary.CreateFromSourceTokens (acc);
      TPIC18x_Expression_TypeInfo(result.info).is_ieee_single :=
         acc.node_typedef = get_supported_data_type (ieee_single_type_name)
   end;

function TPIC18x_CPU.TIfStatement_CreateFromSourceTokens: TIfStatement;
   begin
      result := TPIC18x_IfStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TInitStatement_CreateFromSourceTokens: TInitStatement;
   begin
      result := TPIC18x_InitStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TLoopStatement_CreateFromSourceTokens: TLoopStatement;
   begin
      result := TPIC18x_LoopStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TNotPrimary_CreateFromSourceTokens: TNotPrimary;
   begin
      result := TPIC18x_NotPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TParamList_CreateFromSourceTokens (context: TParamListContext; max_scope: integer): TParamList;
   begin
      result := TPIC18x_ParamList.CreateFromSourceTokens (context, max_scope)
   end;

function TPIC18x_CPU.TParamList_CreatePropertyPseudoParamList (property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TParamList;
   begin
      result := TPIC18x_ParamList.CreatePropertyPseudoParamList (property_id, property_id_src_loc, property_typdef)
   end;

function TPIC18x_CPU.TTypeInfo_Create (_info: TTypeInfo): TTypeInfo;
   begin
      result := TPIC18x_TypeInfo.Create (_info)
   end;

function TPIC18x_CPU.TPredFunctionPrimary_CreateFromSourceTokens: TPredFunctionPrimary;
   begin
      result := TPIC18x_PredFunctionPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TProgram_CreateFromSourceTokens: TProgram;
   begin
      result := TPIC18x_Program.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TProperty_CreateFromSourceTokens (cntxt: TDefinition; entry_property: boolean): TProperty;
   begin
      result := TPIC18x_Property.CreateFromSourceTokens (cntxt, entry_property)
   end;

function TPIC18x_CPU.TRelationalExpression_CreateFromSourceTokens: TRelationalExpression;
   begin
      result := TPIC18x_RelationalExpression.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TReCycleStatement_CreateFromSourceTokens: TReCycleStatement;
   begin
      result := TPIC18x_ReCycleStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TReLoopStatement_CreateFromSourceTokens: TReLoopStatement;
   begin
      result := TPIC18x_ReLoopStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TRoundFunctionPrimary_CreateFromSourceTokens: TRoundFunctionPrimary;
   begin
      result := TPIC18x_RoundFunctionPrimary.CreateFromSourceTokens;
      if not result.expr.contains_constant then
         begin
            result.Release;
            raise compile_error.Create (err_use_round24_or_round32, result.src_loc)
         end
   end;

function TPIC18x_CPU.TRoutine_CreateFromSourceTokens (cntxt: TDefinition; entry_routine: boolean): TRoutine;
   begin
      result := TPIC18x_Routine.CreateFromSourceTokens (cntxt, entry_routine)
   end;

function TPIC18x_CPU.TRoutine_CreatePropertyGetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TPIC18x_Routine.CreatePropertyGetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TPIC18x_CPU.TRoutine_CreatePropertySetterFromSourceTokens (cntxt: TDefinition; entre: boolean; property_id: TIdentifierIdx; property_id_src_loc: TSourceLocation; property_typdef: TTypeDef): TRoutine;
   begin
      result := TPIC18x_Routine.CreatePropertySetterFromSourceTokens (cntxt, entre, property_id, property_id_src_loc, property_typdef)
   end;

function TPIC18x_CPU.TRoutineCallStatement_CreateFromSourceTokens (acc: TAccess): TRoutineCallStatement;
   begin
      result := TPIC18x_RoutineCallStatement.CreateFromSourceTokens (acc)
   end;

function TPIC18x_CPU.TSetConstructorPrimary_CreateFromSourceTokens: TSetConstructorPrimary;
   begin
      result := TPIC18x_SetConstructorPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TSimpleExpression_CreateFromSourceTokens: TSimpleExpression;
   begin
      result := TPIC18x_SimpleExpression.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TStatementList_Create: TStatementList;
   begin
      result := TPIC18x_StatementList.Create
   end;

function TPIC18x_CPU.TStatementList_CreateFromSourceTokens (terminator: TBooleanFunctionOfObject; non_terminated_error_message: string; while_or_until_allowed: boolean): TStatementList;
   begin
      result := TPIC18x_StatementList.CreateFromSourceTokens (terminator, non_terminated_error_message, while_or_until_allowed)
   end;

function TPIC18x_CPU.TStrPosPrimary_Create (acc: TAccess): TStrPosPrimary;
   begin
      result := TPIC18x_StrPosPrimary.Create (acc)
   end;

function TPIC18x_CPU.TSuccFunctionPrimary_CreateFromSourceTokens: TSuccFunctionPrimary;
   begin
      result := TPIC18x_SuccFunctionPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TSystemType_CreateFromSourceTokens: TSystemType;
   begin
      result := TPIC18x_SystemType.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TTerm_CreateFromSourceTokens: TTerm;
   begin
      result := TPIC18x_Term.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TTruncFunctionPrimary_CreateFromSourceTokens: TTruncFunctionPrimary;
   begin
      result := TPIC18x_TruncFunctionPrimary.CreateFromSourceTokens;
      if not result.expr.contains_constant then
         begin
            result.Release;
            raise compile_error.Create (err_use_trunc24_or_trunc32, result.src_loc)
         end
   end;

function TPIC18x_CPU.TUnaryMinusPrimary_CreateFromExpression (exp: TExpression): TUnaryMinusPrimary;
   begin
      result := TPIC18x_UnaryMinusPrimary.CreateFromExpression (exp)
   end;

function TPIC18x_CPU.TUnaryMinusPrimary_CreateFromSourceTokens: TUnaryMinusPrimary;
   begin
      result := TPIC18x_UnaryMinusPrimary.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TUntilStatement_CreateFromSourceTokens: TUntilStatement;
   begin
      result := TPIC18x_UntilStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TVariable_Create (_id_idx: TIdentifierIdx; _context: TDefinition; _typ: TTypeDef; _definition_prefix: TVariableDescriptor; _mode: TAddressMode): TVariable;
   begin
      result := TPIC18x_Variable.Create (_id_idx, _context, _typ, _definition_prefix, _mode)
   end;

function TPIC18x_CPU.TVariable_CreateForLaterDefinition (_id_idx: TIdentifierIdx; _context: TDefinition): TVariable;
   begin
      result := TPIC18x_Variable.CreateForLaterDefinition (_id_idx, _context)
   end;

function TPIC18x_CPU.TVariable_CreateCopy (v: TVariable): TVariable;
   begin
      result := TPIC18x_Variable.CreateCopy (v)
   end;

function TPIC18x_CPU.TVariable_CreateAnonymousROMString (_s: string): TVariable;
   begin
      result := TPIC18x_Variable.CreateAnonymousROMString (_s)
   end;

function TPIC18x_CPU.TVariableAccessPrimary_Create (acc: TAccess): TVariableAccessPrimary;
   begin
      result := TPIC18x_VariableAccessPrimary.Create (acc);
      TPIC18x_Expression_TypeInfo(result.info).is_ieee_single :=
         result.access.node_typedef = get_supported_data_type (ieee_single_type_name)
   end;

function TPIC18x_CPU.TDataItemList_Create: TDataItemList;
   begin
      result := TPIC18x_DataItemList.Create
   end;

function TPIC18x_CPU.TWhileStatement_CreateFromSourceTokens: TWhileStatement;
   begin
      result := TPIC18x_WhileStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TWithStatement_CreateFromSourceTokens: TWithStatement;
   begin
      result := TPIC18x_WithStatement.CreateFromSourceTokens
   end;

function TPIC18x_CPU.TWithStatement_CreateFromSourceTokensStartingAtVariable: TWithStatement;
   begin
      result := TPIC18x_WithStatement.CreateFromSourceTokensStartingAtVariable
   end;

function TPIC18x_CPU.src_directory_relative_to_bin: string;
   begin
      result := '..\source\pic18x'
   end;

procedure TPIC18x_CPU.append_config_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
   var i: integer;
   begin
      i := System.Length (config_bytes);
      SetLength (config_bytes, i+1);
      config_bytes[i].b := b;
      config_bytes[i].byte_no := byte_no;
      config_bytes[i].path := path;
      config_bytes[i].value := value
   end;

constructor tRAMVariableMapNode.Create (_id: string = ''; _typename: string = ''; _size: integer = 0; _group: tRAMVariableGroup = nosuch_group);
   begin
      id := _id;
      shortened_id := _id;
      typename := _typename;
      size := _size;
      ram_variable_group := _group
   end;

constructor tRAMVariableMapNode.CreateArrIndex (_id: string; _typedef: TTypeDef; idx: integer);
   begin
      Create (_id, _typedef);
      is_array_index := true;
      array_index := idx
   end;

constructor tRAMVariableMapNode.Create (_id: string; _typedef: TTypeDef; _ram_variable_group: tRAMVariableGroup = global_vars_phase);
   var
      i: integer;
      st: TPIC18x_SystemType;
      arr: TArrayType;
      node: tRAMVariableMapNode;
   begin
      id := _id;
      shortened_id := _id;
      typedef := _typedef;
      assert (_typedef <> nil);
      typename := _typedef.name;
      size := TPIC18x_TypeInfo(typedef.info).Size;
      ram_variable_group := _ram_variable_group;

      case typedef.type_kind of
         system_type:
            begin
               st := TPIC18x_SystemType (typedef);
               if st.system_type_kind = process_system_type then
                  append (process_pcb_map);
               if st.permanent_eeprom_vars.Length > 0 then
                  append (tRAMVariableMapNode.Create ('EEPROM_BASE', 'uint8', 1, ram_variable_group));
               if st.system_type_kind = monitor_system_type then
                  append (tRAMVariableMapNode.Create ('GATE', 'tMonitorGate', 1, ram_variable_group));
               for i := st.parameters.Length-1 downto 0
                  do case st.parameters[i].address_mode of
                        system_type_address_mode:
                           append (tRAMVariableMapNode.Create (LowerCase(st.parameters[i].name), st.parameters[i].typedef));
                        system_type_indirect_address_mode:
                           append (tRAMVariableMapNode.Create (LowerCase(st.parameters[i].name), '^' + st.parameters[i].typedef.name, 2, ram_variable_group));
                     else
                        assert (false)
                     end;
               for i := 0 to st.permanent_ram_vars.Length-1 do
                  append (tRAMVariableMapNode.Create (LowerCase(st.permanent_ram_vars[i].name), st.permanent_ram_vars[i].typedef, ram_variable_group));
               if st.system_type_kind = process_system_type then
                  append (tRAMVariableMapNode.Create ('STACK', '<' + IntToStr(st.process_stack_size) + ' bytes>', st.process_stack_size, ram_variable_group))
            end;
         record_type:
            for i := 0 to Length(TRecordType(typedef).fields)-1 do
               append (tRAMVariableMapNode.Create (LowerCase(TRecordType(typedef).fields[i].name), TRecordType(typedef).fields[i].typedef, ram_variable_group));
         array_type:
            begin
               arr := TArrayType (typedef);
               case arr.index_typedef.ordinal_kind of
                  ordinal_base_is_integer:
                     for i := arr.index_typedef.info.min_value.AsInteger to arr.index_typedef.info.max_value.AsInteger do
                        append (tRAMVariableMapNode.CreateArrIndex (IntToStr(i), arr.element_typedef, i));
                  ordinal_base_is_char:
                     for i := arr.index_typedef.info.min_value.AsInteger to arr.index_typedef.info.max_value.AsInteger do
                        begin
                           if (i <= 31) or (i >= 127) then
                              node := tRAMVariableMapNode.CreateArrIndex (IntToStr(i), arr.element_typedef, i)
                           else
                              begin
                                 node := tRAMVariableMapNode.CreateArrIndex ('''' + chr(i) + '''', arr.element_typedef, i);
                                 node.shortened_id := IntToStr(i)
                              end;
                           append (node)
                        end;
                  ordinal_base_is_bool:
                     for i := arr.index_typedef.info.min_value.AsInteger to arr.index_typedef.info.max_value.AsInteger do
                        case i of
                           0: append (tRAMVariableMapNode.CreateArrIndex ('false', arr.element_typedef, i));
                           1: append (tRAMVariableMapNode.CreateArrIndex ('true', arr.element_typedef, i));
                        else
                           assert (false)
                        end;
                  ordinal_base_is_enum:
                     for i := arr.index_typedef.info.min_value.AsInteger to arr.index_typedef.info.max_value.AsInteger do
                        append (tRAMVariableMapNode.CreateArrIndex (lex.token_string (arr.index_typedef.enum_typedef.enums[i].identifier_src_loc, arr.index_typedef.enum_typedef.enums[i].identifier_src_loc), arr.element_typedef, i));
               else
                  assert (false)
               end
            end;
         packed_record_type:
            if TPIC18x_PackedRecordFieldInfo(typedef.info).reversed_byte_order then
               typename := typename + ' {REVERSED BYTE ORDER}';
         basic_data_type,
         set_type,
         string_type,
         queue_type,
         overlay_type:
            ;
      else
         assert (false)
      end
   end;

procedure tRAMVariableMapNode.append (new_sub_node: tRAMVariableMapNode);
   var i: integer;
   begin
      i := Length(SubNodes);
      SetLength (SubNodes, i+1);
      SubNodes[i] := new_sub_node;
      new_sub_node.ParentNode := self
   end;

function tRAMVariableMapNode.separator: char;
   begin
      if (typedef <> nil)
         and
         (typedef.type_kind = array_type)
      then
         separator := '['
      else
         separator := '.'
   end;

procedure tRAMVariableMapNode.calculate_output_string_lengths (expanded_short_name: string);
   var
      id_len, i: integer;
   begin
      expanded_short_name := expanded_short_name + shortened_id;
      if (Length(SubNodes) = 0)
         and
         (size > 0)
      then
         begin
            id_len := Length(expanded_short_name);
            if id_len > max_id_len then
               max_id_len := id_len;
            if size > max_reserved_size then
               max_reserved_size := size
         end;
      if id = '' then
         for i := 0 to Length(SubNodes)-1 do
            SubNodes[i].calculate_output_string_lengths ('')
      else
         for i := 0 to Length(SubNodes)-1 do
            SubNodes[i].calculate_output_string_lengths (expanded_short_name + '?')
   end;

procedure tRAMVariableMapNode.output_reserved (_phase: tRAMVariableGroup; expanded_name, expanded_short_name: string);
   var
      i: integer;
   begin
      if (Length(expanded_name) > 0)
         and
         (expanded_name[Length(expanded_name)] = '[')
      then
         expanded_name := expanded_name + id + ']'
      else
         expanded_name := expanded_name + id;
      expanded_short_name := expanded_short_name + shortened_id;
      if (Length(SubNodes) = 0)
         and
         (ram_variable_group = _phase)
         and
         (size > 0)
      then
         begin
            expanded_short_name := expanded_short_name;
            TAssemblySourceLine.Create (format (reserved_format_string, [expanded_short_name, size, expanded_name, typename]))
         end;
      if id = '' then
         for i := 0 to Length(SubNodes)-1 do
            SubNodes[i].output_reserved (_phase, '', '')
      else
         for i := 0 to Length(SubNodes)-1 do
            SubNodes[i].output_reserved (_phase, expanded_name + separator, expanded_short_name + '?')
   end;

function tRAMVariableMapNode.too_long (expanded_short_name: string): boolean;
   const
      max_mpasm_label_len = 32;
   var
      i: integer;
   begin
      result := false;
      expanded_short_name := expanded_short_name + shortened_id;
      if Length(SubNodes) = 0 then
         if size = 0 then
            result := false
         else
            result := Length(expanded_short_name) > max_mpasm_label_len
      else
         if id = '' then
            for i := 0 to Length(SubNodes)-1 do
               result := result or SubNodes[i].too_long ('')
         else
            for i := 0 to Length(SubNodes)-1 do
               result := result or SubNodes[i].too_long (expanded_short_name + '?')
   end;

procedure tRAMVariableMapNode.shorten_if_needed (expanded_short_name: string);
   var
      idx: integer;
   function replacement_id: string;
      function no_conflict (s: string): boolean;
         var i: integer;
         begin
            result := false;
            for i := 0 to Length(SubNodes)-1 do
               if i <> idx then
                  if s = SubNodes[i].shortened_id then
                     exit;
            result := true
         end;
      var c1,c2: char;
      begin
         SetLength (result, 1);
         for c1 := 'a' to 'z' do
            begin
               result[1] := c1;
               if no_conflict (result) then
                  exit
            end;
         SetLength (result, 2);
         for c1 := 'a' to 'z' do
            begin
               result[1] := c1;
               for c2 := '0' to '9' do
                  begin
                     result[2] := c2;
                     if no_conflict (result) then
                        exit
                  end;
               for c2 := 'a' to 'z' do
                  begin
                     result[2] := c2;
                     if no_conflict (result) then
                        exit
                  end;
            end;
         assert (false)  // no substitute found!
      end;
   begin
      expanded_short_name := expanded_short_name + shortened_id;
      for idx := 0 to Length(SubNodes)-1 do
         if ((id = '') and (SubNodes[idx].too_long ('')))
            or
            ((id <> '') and (SubNodes[idx].too_long (expanded_short_name + '?')))
         then
            begin
               if SubNodes[idx].is_array_index then
                  begin
                     if SubNodes[idx].shortened_id = 'true' then
                        SubNodes[idx].shortened_id := 'T'
                     else if SubNodes[idx].shortened_id = 'false' then
                        SubNodes[idx].shortened_id := 'F'
                     else
                        SubNodes[idx].shortened_id := IntToStr(SubNodes[idx].array_index)
                  end
               else
                  SubNodes[idx].shortened_id := replacement_id;
               if id = '' then
                  SubNodes[idx].shorten_if_needed ('')
               else
                  SubNodes[idx].shorten_if_needed (expanded_short_name + '?')
            end
   end;

destructor tRAMVariableMapNode.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(SubNodes)-1 do
         SubNodes[i].Free;
      inherited
   end;

procedure TPIC18x_CPU.generate_machine_code (_prog: TProgram);
   var
      romaddr: integer;
      initialization_block_label: TAssemblyLabel_Packed;
      compressed_bytes: array of byte;

   procedure output_config_bytes;
      var
         i: integer;
         cb: TStructuredConstant;
      begin
         cb := TPIC18x_Program(prog).config_bits;
         if cb <> nil then
            begin
               assert (cb.StructuredConstantKind = scRecord);
               TPIC18x_TypeDef_TypeInfo(cb.typedef.info).enumerate_constant_bytes ('', cb, append_config_byte);
               for i := 0 to Length(config_bytes)-1 do
                  TPIC18x_CONFIG.Create (config_bytes[i].b, cb.record_fields[i]);
               SetLength (config_bytes, 0)
            end
      end;

   procedure enumerate_ram_initial_values (variable: TVariable);
      var
         i: integer;
      begin
         case variable.typedef.type_kind of
            basic_data_type,
            set_type,
            packed_record_type,
            string_type,
            queue_type,
            overlay_type,
            record_type,
            array_type:
               if variable.initial_value = nil then
                  TPIC18x_TypeDef_TypeInfo(variable.TypeDef.info).enumerate_constant_bytes
                     ('',
                      variable.typedef.info.DefaultValue,
                      append_initial_value_byte
                     )
               else
                  TPIC18x_TypeDef_TypeInfo(variable.TypeDef.info).enumerate_constant_bytes
                     ('',
                      variable.initial_value,
                      append_initial_value_byte
                     );
            system_type:
               begin
                  case TSystemType(variable.typedef).system_type_kind of
                     class_system_type:
                        init_class_control_block (variable, append_initial_value_byte);
                     monitor_system_type:
                        init_monitor_control_block (variable, append_initial_value_byte);
                     process_system_type:
                        init_process_control_block (variable, append_initial_value_byte);
                     interrupt_system_type:
                        {nop - no eeprom vars};
                  else
                     assert (false)
                  end;
                  // leave space for parameters (arbitrarily zero them)
                  for i := 0 to TPIC18x_ParamList(TSystemType(variable.typedef).parameters).Size-1 do
                     append_initial_value_byte (systyp_param_init, 0, '', '', false);
                  // init permanent vars
                  for i := 0 to TSystemType(variable.typedef).permanent_ram_vars.Length-1 do
                     enumerate_ram_initial_values (TSystemType(variable.typedef).permanent_ram_vars[i]);
                  if TSystemType(variable.typedef).system_type_kind = process_system_type then
                     begin
                        for i := 1 to TPIC18x_SystemType(variable.typedef).initial_stmt_stack_usage do
                           append_initial_value_byte (process_stk_init_value, 0, '', '', false);
                        if PriorityMapper.Interruptable(TSystemType(variable.typedef).priority) then
                           for i := 1 to kernel_interrupt_handler_stack_allowance do
                              append_initial_value_byte (process_stk_init_value, 0, '', '', false)
                     end
               end;
         else
            assert (false)
         end
      end;

   procedure compress_bytes;
      procedure add (b: byte);
         var i: integer;
         begin
            i := System.Length(compressed_bytes);
            SetLength (compressed_bytes, i+1);
            compressed_bytes[i] := b
         end;
      var
         i, j, run_start, run_length: integer;
      begin   // compress_bytes
         i := 0;
         while i < System.Length(ram_initialization) do
            begin
               run_length := 0;
               if ram_initialization[i].b = 0 then
                  begin
                     repeat
                        run_length := run_length + 1;
                        i := i + 1
                     until (i = System.Length(ram_initialization))
                           or
                           (run_length = 127)
                           or
                           (ram_initialization[i].b <> 0);
                     add (run_length)
                  end
               else
                  begin
                     run_start := i;
                     repeat
                        run_length := run_length + 1;
                        i := i + 1
                     until (i = System.Length(ram_initialization))
                           or
                           (run_length = 127)
                           or
                           (ram_initialization[i].b = 0);
                     add ($80 + run_length);
                     for j := run_start to i-1 do
                        add (ram_initialization[j].b)
                  end
            end;
         add (0)
      end;   // compress_bytes

   procedure init_eeprom_data (name: string; variable_typedef: TTypeDef);
      var
         i: integer;
      begin
         case variable_typedef.type_kind of
            basic_data_type,
            set_type,
            packed_record_type,
            string_type,
            queue_type,
            overlay_type:
               {nop};
            record_type:
               for i := 0 to Length(TRecordType(variable_typedef).fields)-1 do
                  init_eeprom_data (name + '.' + TRecordType(variable_typedef).fields[i].name, TRecordType(variable_typedef).fields[i].TypeDef);
            array_type:
               for i := 0 to TArrayType(variable_typedef).index_typedef.info.max_value.AsInteger - TArrayType(variable_typedef).index_typedef.info.min_value.AsInteger + 1 do
                  init_eeprom_data (name + '[' + IntToStr(i) + ']', TArrayType(variable_typedef).element_typedef);
            system_type:
               begin
                  for i := 0 to TSystemType(variable_typedef).permanent_ram_vars.Length-1 do
                     init_eeprom_data (name + '.' + TSystemType(variable_typedef).permanent_ram_vars[i].Name, TSystemType(variable_typedef).permanent_ram_vars[i].TypeDef);
                  TPIC18x_DataItemList(TSystemType(variable_typedef).permanent_eeprom_vars).InitializeEEPROMValues (name)
               end;
         else
            assert (false)
         end
      end;

   procedure generate_ram_variable_map;
      const
         indent_amt = '  ';

//      procedure out_var_infox (v: TPIC18x_Variable; this_ptr: integer; indent, dot: string);
//
//         procedure out_interrupt_var_info;
//            var
//               st: TPIC18x_SystemType;
//               i: integer;
//            begin
//               if TSystemType(v.typedef).permanent_ram_vars.Length > 0 then
//                  begin
//                     TAssemblyComment.Create (format ('%3.3X %s%s%s  size=%d', [v.address, indent, dot, v.name, TPIC18x_TypeInfo(v.typedef.info).Size]));
//                     st := TPIC18x_SystemType (v.typedef);
//                     for i := 0 to st.permanent_ram_vars.Length-1 do
//                        out_var_infox (TPIC18x_Variable(st.permanent_ram_vars[i]), 0, indent + indent_amt, '.')
//                  end
//            end;
//
//         procedure out_systemtype_var_info (this_ptr: integer);
//            var
//               st: TPIC18x_SystemType;
//               i: integer;
//               addr: integer;
//            begin
//               st := TPIC18x_SystemType (v.typedef);
//               addr := this_ptr - $3E;
//               if st.permanent_eeprom_vars.Length > 0 then
//                  addr := addr - 1;
//               TAssemblyComment.Create (format ('%3.3X %s%s%s  size=%d  this=$%3.3X', [addr, indent, dot, v.name, TPIC18x_TypeInfo(v.typedef.info).Size, this_ptr]));
//               indent := indent + indent_amt;
//               dot := '.';
//               if st.permanent_eeprom_vars.Length > 0 then
//                  TAssemblyComment.Create (format ('%3.3X %s.<eeprom base>  size=1', [this_ptr - $3F, indent]));
//               if st.system_type_kind = monitor_system_type then
//                  TAssemblyComment.Create (format ('%3.3X %s.<gate>  size=1', [this_ptr - $3E, indent]));
//               for i := st.parameters.Length-1 downto 0
//                  do case st.parameters[i].address_mode of
//                        system_type_address_mode:
//                           out_var_infox (TPIC18x_Variable(st.parameters[i]), this_ptr, indent, dot);
//                        system_type_indirect_address_mode:
//                           TAssemblyComment.Create (format ('%3.3X %s.@%s  size=2', [this_ptr + st.parameters[i].address, indent, st.parameters[i].name]));
//                     else
//                        assert (false)
//                     end;
//               for i := 0 to st.permanent_ram_vars.Length-1 do
//                  out_var_infox (TPIC18x_Variable(st.permanent_ram_vars[i]), this_ptr, indent, dot);
//               if st.system_type_kind = process_system_type then
//                  TAssemblyComment.Create (format ('%3.3X %s.<stk>  size=%d  bos=$%3.3X', [addr + TPIC18x_TypeInfo(v.typedef.info).Size - st.process_stack_size, indent, st.process_stack_size, addr + TPIC18x_TypeInfo(v.typedef.info).Size - 1]))
//            end;
//
//         procedure out_fields (offset: integer; typedef: TTypeDef);
//            var
//               i: integer;
//               f: TPIC18x_RecordFieldInfo;
//               o: TOverlaidVariable;
//            begin
//               case typedef.type_kind of
//                  record_type:
//                     for i := 0 to Length(TRecordType(typedef).fields)-1 do
//                        begin
//                           f := TPIC18x_RecordFieldInfo(TRecordType(typedef).fields[i].info);
//                           TAssemblyComment.Create (format ('%3.3X %s%s  size=%d', [offset + f.Offset, indent, '.' + TRecordType(typedef).fields[i].name, TPIC18x_TypeInfo(typedef.info).Size]));
//                           out_fields (offset + f.Offset, TRecordType(typedef).fields[i].typedef)
//                        end;
//                  overlay_type:
//                     for i := 0 to Length(TOverlayType(typedef).overlaid_variables)-1 do
//                        begin
//                           o := TOverlayType(typedef).overlaid_variables[i];
//                           TAssemblyComment.Create (format ('%3.3X %s%s  size=%d', [offset, indent, '.' + o.name, TPIC18x_TypeInfo(o.typedef.info).Size]));
//                           out_fields (offset, o.typedef)
//                        end;
//                  basic_data_type,
//                  set_type,
//                  array_type,
//                  packed_record_type,
//                  string_type,
//                  queue_type:
//                     {nop};
//               else
//                  assert (false)
//               end
//            end;
//
//         var
//            addr: integer;
//         begin  // out_var_infox
//            if v.typedef.IsInterruptType then
//               out_interrupt_var_info
//            else if v.typedef.type_kind = system_type then
//               case v.address_mode of
//                  absolute_address_mode:
//                     out_systemtype_var_info (v.address);
//                  system_type_address_mode:
//                     out_systemtype_var_info (this_ptr + v.address);
//                  system_type_indirect_address_mode:
//                     out_systemtype_var_info (this_ptr + v.address);
//               else
//                  assert (false)
//               end
//            else
//               begin
//                  addr := 0;  // to suppress compiler warning
//                  case v.address_mode of
//                     absolute_address_mode:
//                        addr := v.address;
//                     system_type_address_mode,
//                     system_type_indirect_address_mode:
//                        addr := this_ptr + v.address;
//                  else
//                     assert (false)
//                  end;
//                  TAssemblyComment.Create (format ('%3.3X %s%s%s  size=%d', [addr, indent, dot, v.name, TPIC18x_TypeInfo(v.typedef.info).Size]));
//                  indent := indent + indent_amt;
//                  dot := '.';
//                  out_fields (addr, v.typedef)
//               end
//         end;    // out_var_infox

      var
         i: integer;
         ram_map: tRAMVariableMapNode;
         phase: tRAMVariableGroup;
         s: string;
      begin   // generate_ram_variable_map
         TAssemblySourceBlankLine.Create;
         TSourceLine.Create ('==================');
         TSourceLine.Create (' RAM Variable Map ');
         TSourceLine.Create ('==================');
         TAssemblySourceBlankLine.Create;

//         // old map
//         for i := 0 to prog.program_vars.Length-1 do
//            out_var_infox (TPIC18x_Variable(prog.program_vars[i]), 0, '', '');
//         TAssemblyComment.Create (format ('%3.3X <kernel stk>  size=%d  bos=$%3.3X', [kernel_stack_base-kernel_stack_size+1, kernel_stack_size, kernel_stack_base]));

         // new map
         ram_map := tRAMVariableMapNode.Create;
         // 1. init kernel ram
         ram_map.append (kernel_variables_map);
         // 2. init interrupt variables
         for i := 0 to prog.program_vars.Length-1 do
            if (prog.program_vars[i].TypeDef.type_kind = system_type)
               and
               (TSystemType(prog.program_vars[i].typedef).system_type_kind = interrupt_system_type)
            then
               ram_map.append (tRAMVariableMapNode.Create (LowerCase(prog.program_vars[i].name), prog.program_vars[i].typedef));
         // 3. init global variables
         for i := 0 to prog.program_vars.Length-1 do
            if not system_type_or_array_of_system_type (prog.program_vars[i].TypeDef) then
               ram_map.append (tRAMVariableMapNode.Create (LowerCase(prog.program_vars[i].name), prog.program_vars[i].typedef));
         // 4. init non-interrupt system types
         for i := 0 to prog.program_vars.Length-1 do
            if system_type_or_array_of_system_type (prog.program_vars[i].TypeDef) then
               case prog.program_vars[i].TypeDef.type_kind of
                  system_type:
                     if TSystemType(prog.program_vars[i].typedef).system_type_kind <> interrupt_system_type then
                        ram_map.append (tRAMVariableMapNode.Create (LowerCase(prog.program_vars[i].name), prog.program_vars[i].typedef));
                  array_type:
                     ram_map.append (tRAMVariableMapNode.Create (LowerCase(prog.program_vars[i].name), prog.program_vars[i].typedef));
               else
                  assert (false)
               end;

         ram_map.shorten_if_needed ('');
         ram_map.calculate_output_string_lengths ('');
         i := Length(IntToStr(ram_map.max_reserved_size));
         ram_map.reserved_format_string := '%-' + IntToStr(ram_map.max_id_len) + 's RES %' + IntToStr(i) + 'd ;   %s: %s;';
         s := '';
         for i := 1 to ram_map.max_id_len do
            s := s + ' ';
         TAssemblySourceLine.Create (s + ' UDATA 0x000');
         for phase := Low(tRAMVariableGroup) to High(tRAMVariableGroup) do
            ram_map.output_reserved (phase, '', '');
         ram_map.Free
      end;    // generate_ram_variable_map

   var
      i,j,o,f: integer;
      initial_statement_block_idx: integer;
      data_item_list: TPIC18x_DataItemList;
      bz: TPIC18x_BZ;
      bn: TPIC18x_BN;
      lbl, loop: TInstruction;
      partial_cb: TDataByteArray;
      typedef: TStringType;
      sc: TStructuredConstant;
      constant: TConstant;
      prt: TPackedRecordType;
      ot: TOverlayType;
   begin   // TPIC18x_CPU.generate_machine_code
      prog := TPIC18x_Program(_prog);
      try
         for i := 0 to prog.ioregisters.Length-1 do
            if prog.ioregisters[i].typedef.src_loc.in_preamble then
               begin
                  if pic_info.IsReversedType (prog.ioregisters[i].typedef.name) then
                     TPIC18x_TypeInfo(prog.ioregisters[i].typedef.info).set_reversed_byte_order;
                  if (prog.ioregisters[i].address and $1000) = $1000 then
                     begin
                        TPIC18x_TypeInfo(prog.ioregisters[i].typedef.info).is_in_alternate_shared_address_space := true
                           // include file generator guarantees that alternate shared address sfr types are
                           //    distinct from normal ioreg types, so the above modification of a type is permissible
                     end
               end;

         TSubroutine.increment_compilation_counter;
         kernel_dispatch := TBranchTarget.Create;
         kernel_await_interrupt := TBranchTarget.Create;
         kernel_entermon := TBranchTarget.Create;
         kernel_leavemon := TBranchTarget.Create;
         kernel_delay := TBranchTarget.Create;
         kernel_continue := TBranchTarget.Create;
         kernel_idle := TBranchTarget.Create;

         PriorityMapper.Init;
         for i := 0 to prog.program_vars.Length-1 do
            if prog.program_vars[i].typedef.IsProcessSystemType
   //            and
   //            prog.program_vars[i].reachable
            then
               PriorityMapper.NotePriorityLevelUse (TSystemType(prog.program_vars[i].typedef).priority);

         AssignKernelVariableAddresses (prog);

         sdram_used := next_available_absolute_address;  // (after kernel addresses assigned)

         output_config_bytes;

         // Enable Interrupt Priorities
         with TPIC18x_BSF.Create (RCON, 7, access_mode) do
            begin
               rom_addr := 0;
               annotation := 'reset vector';
               instruction_kind := reset_vector_instruction
            end;
         reset_vector_goto := TPIC18x_GOTO.Create;
         with reset_vector_goto do
            begin
               rom_addr := 2;
               instruction_kind := reset_vector_instruction
            end;

         if PriorityMapper.AnyHiPriorityInterruptProcesses then
            begin
               hi_priority_interrupt_vector_goto := TGOTOMacro.Create;
               with hi_priority_interrupt_vector_goto do
                  begin
                     rom_addr := $0008;
                     annotation := 'high priority interrupt vector';
                     instruction_kind := hi_pri_interrupt_vector_instruction
                  end
            end;

         if PriorityMapper.AnyLowPriorityInterruptProcesses then
            begin   // this code will be placed at $0018 in "assign kernel addresses" section below
               TPIC18x_BCF.Create (INTCON, intcon_gieh, access_mode).annotation := 'low priority interrupt vector';
               low_priority_interrupt_vector_goto := TGOTOMacro.Create
            end;

         // Subroutine to generate bit mask
         //    This routine is generated in fixed memory to guarantee that all routine
         //    instructions are in the same PCL block to prevent ADDWF PCL wrap-around.
         //    This routine MUST be located in ROM "bank" $0000--
         TAssemblySourceBlankLine.Create;
         get_bit_mask_routine := TPIC18x_CLRF.Create (PCLATU, access_mode);
         get_bit_mask_routine.annotation := 'subroutine to generate bit mask';
         get_bit_mask_routine.assembler_label_required := true;
         TPIC18x_CLRF.Create (PCLATH, access_mode);
         TPIC18x_ANDLW.Create ($0E);
         TPIC18x_ADDWF.Create (PCL, dest_f, access_mode);
         TPIC18x_RETLW.Create ($01);
         TPIC18x_RETLW.Create ($02);
         TPIC18x_RETLW.Create ($04);
         TPIC18x_RETLW.Create ($08);
         TPIC18x_RETLW.Create ($10);
         TPIC18x_RETLW.Create ($20);
         TPIC18x_RETLW.Create ($40);
         TPIC18x_RETLW.Create ($80);

         // assign kernel rom addresses
         romaddr := $0018;
         for i := 0 to ProgramCode.NumberOfInstructions-1 do
            with ProgramCode.instr_arr[i] do
               if instruction_kind = executable_instruction then
                  begin
                     instruction_kind := kernel_instruction;
                     rom_addr := romaddr;
                     romaddr := romaddr + size
                  end;
         first_rom_addr := romaddr;

         if Length(stack_usage) > 0 then
            begin
               TAssemblySourceBlankLine_Packed.Create;
               TAssemblyComment_Packed.Create ('Anonymous String Constants');
               TAssemblySourceBlankLine_Packed.Create;
               for i := 0 to Length(stack_usage)-1 do
                  begin
                     stack_usage[i].rom_addr := romaddr;
                     romaddr := romaddr + 1 + Length(stack_usage[i].constant);
                     constant := TConstant.CreateStringConstant (stack_usage[i].constant);
                     typedef := TStringType.Create (Length(stack_usage[i].constant));
                     sc := TStructuredConstant.CreateFromConstant (typedef, constant);
                     TPIC18x_ROM_Data.Create ('', sc, NonExistantSourceLocation);
                     sc.Release;
                     typedef.Release;
                     constant.Release
                  end
            end;

         initial_statement_block_idx := Length(prog.CodeBlockList)-1;
         assert (prog.CodeBlockList[initial_statement_block_idx].definition_kind = program_definition);

         // assign ram addresses for all except non-interrupt global variables
         for i := 0 to Length(prog.CodeBlockList)-1 do
            if i <> initial_statement_block_idx then
               prog.CodeBlockList[i].Generate (AssignAddresses, 0);

         // fill in address for interrupt variables from just calculated addresses
         for i := 0 to prog.program_vars.Length-1 do
            if (prog.program_vars[i].typedef.IsInterruptType)
               and
               (TSystemType(prog.program_vars[i].typedef).permanent_ram_vars.Length > 0)
            then
               prog.program_vars[i].address := TPIC18x_SystemType(prog.program_vars[i].typedef).permanent_ram_vars[0].address;

         for i := 0 to Length(prog.CodeBlockList)-1 do
            if i = initial_statement_block_idx then
               begin
                  if odd(romaddr)
                  then
                     romaddr := romaddr + 1
               end
            else if prog.CodeBlockList[i].definition_kind = data_item_list_definition then
               begin
      //      if prog.code_generation_list[i].reachable then
                        begin
                           prog.CodeBlockList[i].Generate(GenerateCode, 0);
                           data_item_list := TPIC18x_DataItemList (prog.CodeBlockList[i]);
                           if data_item_list.descriptor = rw_rom then
                              for j := 0 to data_item_list.Length-1 do
                                 begin
                                    data_item_list[j].address := romaddr;
                                    romaddr := romaddr + TPIC18x_TypeInfo(data_item_list[j].typedef.info).Size
                                 end
                        end
               end
            else
               begin
   ////         if prog.code_generation_list[i].reachable then
                  begin
                     if odd(romaddr)
                     then
                        romaddr := romaddr + 1;
                     prog.CodeBlockList[i].Generate(GenerateCode, 0);
                  end
               end;

         // at this point all process stack sizes are known except for initial statement stack size

         prog.CodeBlockList[initial_statement_block_idx].Generate (AssignAddresses, 0);
         prog.CodeBlockList[initial_statement_block_idx].Generate(GenerateCode, 0);
         // now initial statement stack size is known
         kernel_stack_size := max (TSetErrorCodeRoutine.stack_usage, kernel_interrupt_handler_stack_allowance);
         kernel_stack_size := max (kernel_stack_size, TPIC18x_Program(prog).initial_statement_stack_usage);
         for i := 0 to prog.program_vars.Length-1 do
            if (prog.program_vars[i].typedef.IsProcessSystemType)
               and
               (TSystemType(prog.program_vars[i].typedef).priority > 0)
   //            and
   //            (prog.program_vars[i].reachable)
            then  // interrupt process
               kernel_stack_size :=
                  max (kernel_stack_size,
                       TPIC18x_Routine(TSystemType(prog.program_vars[i].interrupt.typedef).routines[0]).stack_usage + 1 {size of signaled function result}
                      );
         sdram_used := sdram_used + kernel_stack_size;
         kernel_stack_base := sdram_used-1;

         GenerateKernel (prog);

         // generate power-up INITIALIZATION code
         TAssemblySourceBlankLine.Create;
         TSourceLine.Create ('==============================');
         TSourceLine.Create (' Power-Up Initialization Code ');
         TSourceLine.Create ('==============================');
         TAssemblySourceBlankLine.Create;

         allocate_hw_stack (TPIC18x_Program(prog));

         // enumerate initial values for RAM
         SetLength (ram_initialization, 0);
         eeprom_used := 0;
         // 1. init kernel ram
         init_kernel (append_initial_value_byte);
         // 2. init interrupt variables
         for i := 0 to prog.program_vars.Length-1 do
            if (prog.program_vars[i].TypeDef.type_kind = system_type)
               and
               (TSystemType(prog.program_vars[i].typedef).system_type_kind = interrupt_system_type)
            then
               enumerate_ram_initial_values (prog.program_vars[i]);
         // 3. init global variables
         for i := 0 to prog.program_vars.Length-1 do
            if not system_type_or_array_of_system_type (prog.program_vars[i].TypeDef) then
               enumerate_ram_initial_values (prog.program_vars[i]);
         // 4. init non-interrupt system types
         for i := 0 to prog.program_vars.Length-1 do
            if system_type_or_array_of_system_type (prog.program_vars[i].TypeDef) then
               case prog.program_vars[i].TypeDef.type_kind of
                  system_type:
                     if TSystemType(prog.program_vars[i].typedef).system_type_kind <> interrupt_system_type then
                        enumerate_ram_initial_values (prog.program_vars[i]);
                  array_type:
                     enumerate_ram_initial_values (prog.program_vars[i]);
               else
                  assert (false)
               end;
         // 5. init initial process ram
         init_initial_process (TPIC18x_Program(prog).initial_statement_stack_usage, append_initial_value_byte);

         compress_bytes;

         TAssemblySourceBlankLine_Packed.Create;
         initialization_block_label := TAssemblyLabel_Packed.Create;
         initialization_block_label.annotation := 'compressed RAM initialization data';
         i := 0;
         while i < System.Length (compressed_bytes) do
            begin
               for j := i to i+7 do
                  if j < System.Length (compressed_bytes) then
                     begin
                        SetLength (partial_cb, j-i+1);
                        partial_cb[j-i] := compressed_bytes[j]
                     end;
               TPIC18x_DB_Packed.Create (partial_cb);
               SetLength (partial_cb, 0);
               i := i + 8
            end;

         SetLength (ram_initialization, 0);
         SetLength (compressed_bytes, 0);

         reset_vector_goto.dest := TAssemblyLabel.Create;

         // initialize RAM
         lbl := TMOVLW_ROMAddr_B0.Create;
         lbl.dest := initialization_block_label;
         lbl.annotation := 'initialize RAM';
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TMOVLW_ROMAddr_B1.Create.dest := initialization_block_label;
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_CLRF.Create (FSR2L, access_mode);    // note FRS2H is cleared on PON
         loop := TPIC18x_TBLRD.Create (tblrd_post_inc);
         TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
         bz := TPIC18x_BZ.Create;
         bn := TPIC18x_BN.Create;
         // clr N bytes
         lbl := TPIC18x_CLRF.Create (POSTINC2, access_mode);
         TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_BRA.Create.dest := lbl;
         TPIC18x_BRA.Create.dest := loop;
         // copy N bytes
         bn.dest := TPIC18x_ANDLW.Create ($7f);
         lbl := TPIC18x_TBLRD.Create (tblrd_post_inc);
         TPIC18x_MOVFF.Create (TABLAT, POSTINC2);
         TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_BRA.Create.dest := lbl;
         TPIC18x_BRA.Create.dest := loop;
         bz.dest := TAssemblyLabel.Create;

         TGOTOMacro.Create.dest := TPIC18x_Program(prog).initial_statement_label;

         TAssemblySourceBlankLine.Create;
         TSourceLine.Create ('=============');
         TSourceLine.Create (' Subroutines ');
         TSourceLine.Create ('=============');
         TAssemblySourceBlankLine.Create;

         GenerateRunTimeErrorCheckSubroutines;
         GenerateMathRoutines;
         TSubroutine.generate_subroutines;

         if eeprom_used > 0 then
            begin
               TAssemblySourceBlankLine.Create;
               TSourceLine.Create ('=======================');
               TSourceLine.Create (' EEPROM Initialization ');
               TSourceLine.Create ('=======================');
               TAssemblySourceBlankLine.Create;
               for i := 0 to prog.program_vars.Length-1 do
                  init_eeprom_data (prog.program_vars[i].name, prog.program_vars[i].TypeDef)
            end;

         generate_ram_variable_map;

         kernel_dispatch.set_client_destinations;
         kernel_await_interrupt.set_client_destinations;
         kernel_entermon.set_client_destinations;
         kernel_leavemon.set_client_destinations;
         kernel_delay.set_client_destinations;
         kernel_continue.set_client_destinations;
         kernel_idle.set_client_destinations;
      finally
         kernel_dispatch.Free;
         kernel_await_interrupt.Free;
         kernel_entermon.Free;
         kernel_leavemon.Free;
         kernel_delay.Free;
         kernel_continue.Free;
         kernel_idle.Free;
         SetLength (stack_usage, 0)
      end
   end;    // TPIC18x_CPU.generate_machine_code

procedure TPIC18x_CPU.append_initial_value_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
   var i: integer;
   begin
      i := System.Length (ram_initialization);
      SetLength (ram_initialization, i+1);
      ram_initialization[i].b := b;
      ram_initialization[i].byte_no := byte_no;
      ram_initialization[i].path := path;
      ram_initialization[i].value := value
   end;

procedure TStackUsageCounter.Clear;
   begin
      current := 0;
      max := 0
   end;

procedure TStackUsageCounter.Push (size: integer);
   begin
      assert (size >= 0);
      current := current + size;
      if current > max then
         max := current
   end;

procedure TStackUsageCounter.Pop (size: integer);
   begin
      assert (size >= 0);
      current := current - size;
      assert (current >= 0)
   end;

procedure TStackUsageCounter.PushPop (size: integer);
   begin
      Push (size);
      Pop (size)
   end;

INITIALIZATION
   target_cpu := TPIC18x_CPU.create;

FINALIZATION
   target_cpu.Free;

END.
