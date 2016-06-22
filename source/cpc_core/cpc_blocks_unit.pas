UNIT cpc_blocks_unit;

// Note this unit is broken out separately to prevent circularly defined units.
//    Several fields in objects defined here are TDefinition rather than their
//    actual known types.  This seemed to be the least painful place to break the
//    circular references.

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_definitions_unit, cpc_source_analysis_unit, cpc_core_objects_unit;

type
   TParamListContext =
      (process_init_param_list,
       process_local_routine_param_list,
       monitor_init_param_list,
       monitor_entry_routine_param_list,
       monitor_local_routine_param_list,
       class_init_param_list,
       class_entry_routine_param_list,
       class_local_routine_param_list,
       standalone_routine_param_list
      );
   TParamList =
      class(TDefinition)
      private
         function get_definition (idx: integer): TVariable;
         procedure set_definition (idx: integer; v: TVariable);
         function get_length: integer;
         procedure set_length (len: integer);
      protected
         parameter_definitions: array of TVariable;
      public
         property default_anonymous[idx: integer]: TVariable read get_definition write set_definition; default;
         property Length: integer read get_length write set_length;
         constructor CreateFromSourceTokens
            (context: TParamListContext
            );
         constructor CreatePropertyPseudoParamList
            (property_id_idx: TIdentifierIdx;
             property_id_src_loc: TSourceLocation;
             property_typdef: TTypeDef
            );
         destructor Destroy;
            override;
         function AssembleAndCheckCallerParameterListFromSourceTokens: TArrayOfTDefinition;
         function Empty: boolean;
      end;

   TDataItemList =
      class (TDefinition)
      private
         type
            TDataItemInitSpec = (diisNotAllowed, diisOptional, diisRequired);
         var
            va: array of TVariable;
            CheckForProhibitedDelayCall: boolean;
            initspec: TDataItemInitSpec;
            initialization_symbol: TSymbolEnum;
         function get
            (i: integer
            ): TVariable;
         procedure _init
            (_descriptor: TVariableDescriptor;
             _address_mode: TAddressMode;
             interrupt_variable_instance_count: boolean;
             _initspec: TDataItemInitSpec;
             _initialization_symbol: TSymbolEnum    // either sym_assign or sym_equals  (nop if diisNotAllowed)
            );
      public
         descriptor: TVariableDescriptor;
         address_mode: TAddressMode;
         end_src_loc: TSourceLocation;
         property default_anonymous[i: integer]: TVariable read get; default;
         constructor Create;
         constructor Copy (orig: TDataItemList);
         destructor Destroy;
            override;
         procedure define_as_var_list (_descriptor: TVariableDescriptor; _address_mode: TAddressMode);  // ram (var) or eeprom
         procedure define_as_ioreg_list;
         procedure define_as_rom_constant_list;
         function Length: integer;
         procedure AddFromSourceTokens (context: TDefinition);
         function DefinesVariable
            (v: TVariable
            ): boolean;
      end;

   TRoutineKind =
      (standalone_routine,
       system_type_routine,
       class_entry_routine,
       monitor_entry_routine,
       interrupt_signalled_routine
      );

   TRoutine =
      class(TDefinition)
      private
         procedure create_block_from_source_tokens;
      public
         routine_id_idx: TIdentifierIdx;
         routine_id_src_loc: TSourceLocation;
         block_header_end_src_loc, last_var_declaration_src_loc, block_begin_src_loc, block_end_src_loc: TSourceLocation;
         routine_kind: TRoutineKind;
         entry: boolean;
         context: TDefinition; // either a program or a system type
         header_only: boolean;
         parameter_definitions: TParamList;
         function_result: TVariable;
         local_vars: TDataItemList;
         definition_complete: boolean;
         statement_list: TDefinition;   // actually TStatementList
         constructor CreateFromSourceTokens
            (cntxt: TDefinition
            );
         constructor CreatePropertySetterFromSourceTokens
            (cntxt: TDefinition;
             entre: boolean;
             property_id_idx: TIdentifierIdx;
             property_id_src_loc:
             TSourceLocation;
             property_typdef: TTypeDef
            );
         constructor CreatePropertyGetterFromSourceTokens
            (cntxt: TDefinition;
             entre: boolean;
             property_id_idx: TIdentifierIdx;
             property_id_src_loc:
             TSourceLocation;
             property_typdef: TTypeDef
            );
         destructor Destroy;
            override;
         function AssembleAndCheckCallerParameterListFromSourceTokens: TArrayOfTDefinition;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TProperty =
      class(TDefinition)
         id: TIdentifierIdx;
         entry: boolean;
         typedef: TTypeDef;
         typedef_src_loc: TSourceLocation;
         set_proc, get_func: TRoutine;
         constructor CreateFromSourceTokens
            (cntxt: TDefinition
            );
         destructor Destroy;
            override;
      end;

   TSystemTypeKind =
      (class_system_type,
       monitor_system_type,
       process_system_type,
       interrupt_system_type
      );

   TSystemType =
      class(TTypeDef)
         system_type_kind: TSystemTypeKind;
         parameters: TParamList;
         priority: integer; // meaningful for processes, monitors and interrupts, but not classes
         interrupt_process: boolean;
         interrupt_base_type: TDefinition;
         interrupt_base_type_src_loc: TSourceLocation;
         interrupt_instance_count: integer;
         permanent_ram_vars: TDataItemList;
         permanent_eeprom_vars: TDataItemList;
         ioregisters: TDataItemList;    // for interrupt type only
         properties: array of TProperty;
         routines: array of TRoutine;
         initial_statement: TDefinition;   // actually TStatementList
         block_header_end_src_loc, last_var_declaration_src_loc, block_begin_src_loc, block_end_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         function IsClassSystemType: boolean;
            override;
         function IsMonitorSystemType: boolean;
            override;
         function IsProcessSystemType: boolean;
            override;
         function IsInterruptType: boolean;
            override;
         function requires_initialization: boolean;
            override;
         procedure SetMonitorPriorities
            (prio: integer
            );
            override;
         procedure MarkAsReachable;
            override;
      protected
         procedure check_interrupt_routine_signature;
            virtual; abstract;
      end;

   TProgram =
      class(TDefinition)
         program_vars: TDataItemList;
         ioregisters: TDataItemList;
         global_routines: array of TRoutine;
         initial_statement: TDefinition;  // actually TStatementList
         last_var_declaration_src_loc, begin_src_loc, end_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function FinalStatementIsEmptyCycleStatement: boolean;
         procedure global_declarations_examination_hook;
            virtual;
      end;

   TDefinitionStack =
      class
         stk: array of TDefinition;
         destructor Destroy;
            override;
         procedure push
            (stmt: TDefinition
            );
         procedure pop;
         function tos: TDefinition;
         // returns nil if empty
         function tos_idx: integer;
         // returns -1 if stack is empty
      end;

   TBlockStack =   // outer program, system types, and routines
      class(TDefinitionStack)
         function IsLocalRAMVariable
            (v: TVariable
            ): boolean;
         procedure CheckAddressReachability   // throws exception if not accessible
            (v: TVariable;
             src_loc:
             TSourceLocation
            );
         function ProcessingProgramBlock: boolean;
         function ProcessingSystemTypeBlock: boolean;
         function ProcessingMonitorBlock: boolean;
         function ProcessingRoutineBlock: boolean;
      end;

var
   the_program: TProgram;
   BlockStack: TBlockStack;

procedure process_constant_definition_part;
procedure process_type_definition_part;

IMPLEMENTATION

uses
   cpc_common_unit, cpc_access_unit, cpc_target_cpu_unit, cpc_types_unit, cpc_expressions_unit,
   cpc_constant_expression_unit, cpc_statements_unit, SysUtils;

procedure check_for_valid_ioreg_type (typedef: TTypeDef; typedef_src_loc: TSourceLocation);
   var
      i, j, overlay_width: integer;
      prt: TPackedRecordType;
   function max (a,b: integer): integer;
      begin
         if a > b then
            result := a
         else
            result := b
      end;
   begin
      case typedef.type_kind of
         packed_record_type:
            begin
               for i := 0 to System.Length(TPackedRecordType(typedef).fields) - 1 do
                  if (TPackedRecordType(typedef).fields[i].ordtypedef.ordinal_kind = ordinal_base_is_enum)
                     and
                     (TEnumType(TPackedRecordType(typedef).fields[i].ordtypedef).enum_type_kind <> specified_value_enum_type)
                  then
                     raise compile_error.Create(err_ioregister_packed_record_enums_must_be_specified, typedef_src_loc);
               target_cpu.ioregister_width_in_address_units(TTypeInfo(typedef.info).PackedSizeInBits, typedef_src_loc)
                  // will throw a compile_error exception if all bits in ioregister not completely specified
            end;
         overlay_type:
            begin
               overlay_width := 0;
               for i := 0 to System.Length(TOverlayType(typedef).overlaid_variables)-1 do
                  begin
                     if TOverlayType(typedef).overlaid_variables[i].typedef.type_kind <> packed_record_type then
                        raise compile_error.Create(err_ioreg_overlaid_variables_must_all_be_packed_records, typedef_src_loc);
                     prt := TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef);
                     for j := 0 to System.Length(prt.fields) - 1 do
                        if (prt.fields[j].ordtypedef.ordinal_kind = ordinal_base_is_enum)
                           and
                           (TEnumType(prt.fields[j].ordtypedef).enum_type_kind <> specified_value_enum_type)
                        then
                           raise compile_error.Create(err_ioregister_packed_record_enums_must_be_specified, typedef_src_loc);
                     overlay_width := max (overlay_width, target_cpu.ioregister_width_in_address_units(prt.info.PackedSizeInBits, typedef_src_loc));
                        // will throw a compile_error exception if all bits in ioregister not completely specified
                  end;
               for i := 0 to System.Length(TOverlayType(typedef).overlaid_variables)-1 do
                  begin
                     prt := TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef);
                     if overlay_width <> target_cpu.ioregister_width_in_address_units(prt.info.PackedSizeInBits, typedef_src_loc) then
                        raise compile_error.Create (err_all_ioreg_overlay_type_packed_records_must_be_same_width, typedef_src_loc)
                  end;
            end;
      else
         raise compile_error.Create(err_packed_record_type_expected, typedef_src_loc)
      end
   end;

procedure process_constant_definition_part;
   var
      cexpression: TCExpression;
      typedef: TTypeDef;
      structured_constant: TStructuredConstant;
      id_idx: TIdentifierIdx;
      src_loc, typedef_src_loc: TSourceLocation;
   begin
      assert(lex.token_is_reserved_word(rw_const));
      lex.advance_token;
      repeat
         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);
         id_idx := lex.token.identifier_idx;
         src_loc := lex.token.src_loc;
         lex.advance_token;

         if lex.token_is_symbol(sym_colon) then
            begin // structured constant
               lex.advance_token;

               typedef_src_loc := lex.token.src_loc;
               typedef := CreateTypeDenoterFromSourceTokens;
               try
                  if (typedef.type_kind = string_type)
                     and
                     (TStringType(typedef).max_length = -1) then
                     raise compile_error.Create(err_left_bracket_expected);
                  if typedef.ContainsQueueVariables or typedef.ContainsSystemType
                  then
                     raise compile_error.Create(err_type_contains_types_not_allowed_in_structured_constant, typedef_src_loc);
                  if not lex.token_is_symbol(sym_equals) then
                     raise compile_error.Create(err_equals_expected);
                  lex.advance_token;

                  structured_constant := TStructuredConstant.CreateFromSourceTokens(typedef, typedef_src_loc);
                  try
                     CurrentDefinitionTable.DefineForCurrentScope(id_idx, structured_constant, src_loc)
                  finally
                     structured_constant.Release
                  end
               finally
                  typedef.Release;
               end
            end
         else
            begin // regular constant
               if not lex.token_is_symbol(sym_equals) then
                  raise compile_error.Create(err_equals_expected);
               lex.advance_token;

               cexpression := TCExpression.CreateFromSourceTokens;
               try
                  CurrentDefinitionTable.DefineForCurrentScope(id_idx, cexpression, src_loc)
               finally
                  cexpression.Release
               end
            end;

         if not lex.token_is_symbol(sym_semicolon) then
            raise compile_error.Create(err_semicolon_expected);
         lex.advance_token
      until not lex.token_is_identifier
   end;

procedure process_type_definition_part;
   var
      id_idx: TIdentifierIdx;
      id_src_loc: TSourceLocation;
      typedef: TTypeDef;
      temporary_incomplete_typedef: TTypeDef;
   begin
      assert(lex.token_is_reserved_word(rw_type));
      lex.advance_token;
      repeat
         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);
         id_idx := lex.token.identifier_idx;
         id_src_loc := lex.token.src_loc;
         lex.advance_token;

         if not lex.token_is_symbol(sym_equals) then
            raise compile_error.Create(err_equals_expected);
         lex.advance_token;

         temporary_incomplete_typedef := TTypeDef.Create(incomplete_type);
         temporary_incomplete_typedef.name := lex.identifiers[id_idx];
         try
            CurrentDefinitionTable.DefineForCurrentScope(id_idx, temporary_incomplete_typedef, id_src_loc)
         finally
            temporary_incomplete_typedef.Release
         end;
         typedef := CreateTypeDenoterFromSourceTokens;
         typedef.name := lex.identifiers[id_idx];
         if (typedef.type_kind = string_type)
            and
            (TStringType(typedef).max_length = -1) then
            begin
               typedef.Release;
               raise compile_error.Create(err_left_bracket_expected)
            end;
         CurrentDefinitionTable.RedefineForCurrentScope(id_idx, typedef);
         typedef.Release;

         if not lex.token_is_symbol(sym_semicolon) then
            raise compile_error.Create(err_semicolon_expected);
         lex.advance_token
      until not lex.token_is_identifier;
   end;

procedure process_rom_constant_definition_part;
   var
      rom_constant_list: TDataItemList;
   begin
      rom_constant_list := target_cpu.TDataItemList_Create;
      rom_constant_list.define_as_rom_constant_list;
      try
         rom_constant_list.AddFromSourceTokens (nil)
      finally
         rom_constant_list.Release
      end
   end;


// =============
//  TParamList

constructor TParamList.CreateFromSourceTokens
   (context: TParamListContext
   );
   var
      var_count: integer;
      // number of vars with same type defined in a param list, e.g. "var a,b,c: integer" would be 3

   procedure add_one_identifier_to_parameter_list;
      var
         param_idx: integer;
      begin
         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);
         param_idx := Length;
         Length := param_idx + 1;
         parameter_definitions[param_idx] := target_cpu.TVariable_CreateForLaterDefinition(lex.token.identifier_idx, nil);
         parameter_definitions[param_idx].init_statement_called := true;
         CurrentDefinitionTable.DefineForCurrentScope(lex.token.identifier_idx, parameter_definitions[param_idx], lex.token.src_loc);
         var_count := var_count + 1;
         lex.advance_token
      end;

   var
      i: integer;
      typedef: TTypeDef;
      typedef_src_loc: TSourceLocation;
      param_descriptor: TVariableDescriptor;
      allowed_param_descriptors, disallowed_param_descriptors: set of TVariableDescriptor;
      direct_address_mode, indirect_address_mode: TAddressMode;
      temp_src_loc: TSourceLocation;
   begin
      inherited Create(paramlist_definition);
      src_loc := lex.token.src_loc;
      direct_address_mode := local_address_mode; // to suppress warning
      indirect_address_mode := local_indirect_address_mode; // to suppress warning
      if lex.token_is_symbol(sym_left_parenthesis) then
         begin
            lex.advance_token;

            case context of
               process_init_param_list,
               monitor_init_param_list,
               class_init_param_list:
                  begin
                     allowed_param_descriptors := [rw_rom, rw_ioreg];
                     disallowed_param_descriptors := [rw_eeprom, rw_var];
                     direct_address_mode := system_type_address_mode;
                     indirect_address_mode := system_type_indirect_address_mode
                  end;
               process_local_routine_param_list,
               monitor_entry_routine_param_list,
               monitor_local_routine_param_list,
               class_entry_routine_param_list,
               class_local_routine_param_list,
               standalone_routine_param_list:
                  begin
                     allowed_param_descriptors := [rw_eeprom, rw_ioreg, rw_rom, rw_var];
                     disallowed_param_descriptors := [];
                     direct_address_mode := local_address_mode;
                     indirect_address_mode := local_indirect_address_mode
                  end;
            else
               assert(false)
            end;

            repeat // loop once per <descriptor> <id> {,<id>} : <type> [;] sequence in parameter list
               temp_src_loc := lex.token.src_loc;

               if (lex.token_is_reserved_word(rw_eeprom))
                  and
                  (not target_cpu.has_eeprom)
               then
                  raise compile_error.Create (err_processor_does_not_support_eeprom);

               if lex.token_is_reserved_word(disallowed_param_descriptors) then
                  case lex.token.rw of
                     rw_eeprom:
                        raise compile_error.Create(err_eeprom_parameters_not_allowed_here);
                     rw_var:
                        raise compile_error.Create(err_var_parameters_not_allowed_here);
                  else
                     assert(false);
                  end;

               if lex.token_is_reserved_word(allowed_param_descriptors) then
                  begin
                     param_descriptor := lex.token.rw;
                     lex.advance_token
                  end
               else
                  param_descriptor := rw_const; // default

               var_count := 0;

               add_one_identifier_to_parameter_list;
               while lex.token_is_symbol(sym_comma) do
                  begin
                     lex.advance_token;
                     add_one_identifier_to_parameter_list
                  end;

               if not lex.token_is_symbol(sym_colon) then
                  raise compile_error.Create(err_colon_expected);
               lex.advance_token;

               typedef_src_loc := lex.token.src_loc;
               typedef := CreateTypeDenoterFromSourceTokens;
               try
                  if (typedef.type_kind = system_type) and (param_descriptor <> rw_const) then
                     raise compile_error.Create(err_system_type_parameter_must_be_constant_parameter, typedef_src_loc);

                  if (typedef.IsClassSystemType) and (context in [process_init_param_list, monitor_init_param_list]) then
                     raise compile_error.Create(err_class_type_not_allowed, typedef_src_loc);

                  if (typedef.ContainsQueueVariables) then
                     if context <> monitor_local_routine_param_list then
                        raise compile_error.Create(err_queue_parameters_only_allowed_in_monitor_local_routines, typedef_src_loc)
                     else if param_descriptor <> rw_var then
                        raise compile_error.Create(err_queue_param_must_be_var, typedef_src_loc);

                  if param_descriptor = rw_ioreg then
                     check_for_valid_ioreg_type (typedef, typedef_src_loc);

                  for i := 1 to var_count do
                     begin
                        parameter_definitions[Length-i].typedef := typedef;
                        parameter_definitions[Length-i].typedef.AddRef;
                        parameter_definitions[Length-i].typedef_src_loc := typedef_src_loc;
                        parameter_definitions[Length-i].descriptor := param_descriptor;
                        if param_descriptor in [rw_var, rw_eeprom, rw_ioreg] then
                           parameter_definitions[Length-i].address_mode := indirect_address_mode
                        else if typedef.type_kind in [basic_data_type, set_type] then // small, pass by value
                           parameter_definitions[Length-i].address_mode := direct_address_mode
                        else // large, pass by addr
                           parameter_definitions[Length-i].address_mode := indirect_address_mode
                     end;
               finally
                  typedef.Release
               end;

               temp_src_loc := lex.token.src_loc;
               if lex.token_is_symbol(sym_semicolon) then
                  begin
                     lex.advance_token;
                     if lex.token_is_symbol(sym_right_parenthesis) then
                        raise compile_error.Create(err_semicolon_not_allowed_here_before_parenthesis, temp_src_loc)
                  end
            until lex.token_is_symbol(sym_right_parenthesis);
            lex.advance_token
         end
   end;

constructor TParamList.CreatePropertyPseudoParamList
   (property_id_idx:  TIdentifierIdx;
    property_id_src_loc: TSourceLocation;
    property_typdef: TTypeDef
   );
   begin
      inherited Create(paramlist_definition);

      SetLength(parameter_definitions, 1);
      parameter_definitions[0] := target_cpu.TVariable_Create(property_id_idx, nil, property_typdef, rw_const, local_address_mode);
      CurrentDefinitionTable.DefineForCurrentScope(property_id_idx, parameter_definitions[0], property_id_src_loc)
   end;

destructor TParamList.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length-1 do
         parameter_definitions[i].Release
   end;

function TParamList.get_definition(idx: integer): TVariable;
   begin
      result := parameter_definitions[idx]
   end;

procedure TParamList.set_definition (idx: integer; v: TVariable);
   begin
      parameter_definitions[idx] := v
   end;

function TParamList.get_length: integer;
   begin
      result := System.Length(parameter_definitions)
   end;

procedure TParamList.set_length (len: integer);
   begin
      SetLength (parameter_definitions, len)
   end;

function TParamList.AssembleAndCheckCallerParameterListFromSourceTokens: TArrayOfTDefinition;
   var
      param_access: TAccess;
      param_expr: TExpression;
      i, j: integer;
      compatable: boolean;
      v: TVariable;
      sl: TSourceLocation;
   begin
      if (Length = 0) then
         begin
            if lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_no_parameters_expected);
         end
      else // parameters expected
         begin
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            try
               SetLength(result, Length);
               for i := 0 to Length-1 do
                  begin
                     case parameter_definitions[i].ParamMode of
                        ByAddress:
                           begin // passed by address
                              if (parameter_definitions[i].descriptor = rw_rom)
                                 and
                                 (lex.token.token_kind = string_constant_token)
                              then
                                 begin
                                    v := target_cpu.TVariable_CreateAnonymousROMString (lex.token.s);
                                    param_access := target_cpu.TAccess_CreateFromVariable (v);
                                    param_access.src_loc := lex.token.src_loc;
                                    param_access.last_token_src_loc := lex.token.src_loc;
                                    v.Release;
                                    lex.advance_token;
                                 end
                              else
                                 begin
                                    param_access := target_cpu.TAccess_CreateFromSourceTokens;
                                    if (param_access.base_variable <> nil)
                                       and
                                       (param_access.base_variable.typedef.requires_initialization)
                                       and
                                       (not param_access.base_variable.init_statement_called)
                                    then
                                       begin
                                          sl := param_access.src_loc;
                                          param_access.Release;
                                          raise compile_error.Create (err_variable_not_initialized, sl)
                                       end
                                 end;

                              result[i] := param_access;

                              case param_access.node_access_kind of
                                 variable_access:
                                    if parameter_definitions[i].descriptor <> rw_rom then
                                       begin
                                          BlockStack.CheckAddressReachability(param_access.base_variable, param_access.src_loc);
                                          if parameter_definitions[i].descriptor in [rw_var, rw_eeprom, rw_ioreg] then
                                             check_for_write_access(param_access.base_variable, param_access.src_loc)
                                       end;
                                 function_access,
                                 procedure_access:
                                    raise compile_error.Create(err_routine_cant_be_used_as_parameter_here, param_access.node_id_src_loc);
                                 property_access:
                                    raise compile_error.Create(err_property_cant_be_used_as_parameter_here, param_access.node_id_src_loc);
                                 constant_access,
                                 structured_constant_access:
                                    raise compile_error.Create(err_constant_cant_be_used_as_parameter_here, param_access.node_id_src_loc);
                                 else
                                    assert(false)
                              end;

                              if (parameter_definitions[i].descriptor <> rw_const)
                                 and
                                 (parameter_definitions[i].descriptor <> param_access.base_variable.descriptor) then
                                 case parameter_definitions[i].descriptor of
                                    rw_var:
                                       raise compile_error.Create(err_ram_variable_expected, param_access.node_id_src_loc);
                                    rw_eeprom:
                                       raise compile_error.Create(err_eeprom_variable_expected, param_access.node_id_src_loc);
                                    rw_ioreg:
                                       raise compile_error.Create(err_ioregister_expected, param_access.node_id_src_loc);
                                    rw_rom:
                                       raise compile_error.Create(err_rom_constant_expected, param_access.node_id_src_loc);
                                    else
                                       assert(false)
                                 end;

                              if parameter_definitions[i].descriptor = rw_const then
                                 case param_access.base_variable.descriptor of
                                    rw_eeprom:
                                       raise compile_error.Create(err_eeprom_parameter_only_allowed_with_eeprom_parameter_descriptor, param_access.src_loc);
                                    rw_rom:
                                       raise compile_error.Create(err_rom_constant_only_allowed_with_rom_parameter_descriptor, param_access.src_loc);
                                 else
                                    { ok }
                                 end;

                              if parameter_definitions[i].typedef.type_kind = string_type then
                                 compatable := param_access.node_typedef.type_kind = string_type
                              else  // check type - must be exact for reference params
                                 begin
                                    compatable := param_access.node_typedef = parameter_definitions[i].typedef;
                                    if param_access.node_typedef.type_kind = overlay_type then
                                       for j := 0 to System.Length(TOverlayType(param_access.node_typedef).overlaid_variables) - 1 do
                                          with TOverlayType(param_access.node_typedef).overlaid_variables[j] do
                                             if (anonymous) and (typedef = parameter_definitions[i].typedef) then
                                                compatable := true
                                 end;
                              if not compatable then
                                 raise compile_error.Create(err_wrong_type, param_access.src_loc)
                           end;
                        ByValue:
                           begin
                              param_expr := CreateExpressionFromSourceTokens;
                              result[i] := param_expr;
                              parameter_definitions[i].typedef.CheckAssignmentCompatability(param_expr)
                           end;
                     else
                        assert (false)
                     end;

                     if (i <> Length-1) then
                        begin
                           if not lex.token_is_symbol(sym_comma) then
                              raise compile_error.Create(err_comma_expected);
                           lex.advance_token
                        end
                  end;

               if not lex.token_is_symbol(sym_right_parenthesis) then
                  raise compile_error.Create(err_right_parenthesis_expected);
               lex.advance_token

            except
               on compile_error do
                  begin
                     for i := 0 to Length-1 do
                        result[i].Release;
                     raise
                  end
            end
         end
   end;

function TParamList.Empty: boolean;
   begin
      result := Length = 0
   end;


// ===============
//  TDataItemList
// ===============

constructor TDataItemList.Create;
   begin
      inherited Create (data_item_list_definition)
   end;

constructor TDataItemList.Copy (orig: TDataItemList);
   var
      i: integer;
   begin
      inherited Create (data_item_list_definition);
      SetLength (va, orig.Length);
      for i := 0 to orig.Length-1 do
         va[i] := target_cpu.TVariable_CreateCopy (orig.va[i]);
      CheckForProhibitedDelayCall := orig.CheckForProhibitedDelayCall;
      initspec := orig.initspec;
      initialization_symbol := orig.initialization_symbol;
      descriptor := orig.descriptor;
      address_mode := orig.address_mode;
      end_src_loc := orig.end_src_loc
   end;

destructor TDataItemList.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length-1 do
         va[i].Release;
      inherited
   end;

procedure TDataItemList._init
   (_descriptor: TVariableDescriptor;
    _address_mode: TAddressMode;
    interrupt_variable_instance_count: boolean;
    _initspec: TDataItemInitSpec;
    _initialization_symbol: TSymbolEnum
   );
   begin
      descriptor := _descriptor;
      address_mode := _address_mode;
      CheckForProhibitedDelayCall := interrupt_variable_instance_count;
      initspec := _initspec;
      initialization_symbol := _initialization_symbol
   end;

procedure TDataItemList.define_as_var_list (_descriptor: TVariableDescriptor; _address_mode: TAddressMode);  // ram (var) or eeprom
   begin
      assert (_descriptor in [rw_var, rw_eeprom]);
      _init (_descriptor, _address_mode, false, diisOptional, sym_assign)
   end;

procedure TDataItemList.define_as_ioreg_list;
   begin
      _init (rw_ioreg, absolute_address_mode, true, diisNotAllowed, sym_not_equals)
   end;

procedure TDataItemList.define_as_rom_constant_list;
   begin
      _init (rw_rom, absolute_address_mode, true, diisRequired, sym_equals)
   end;

function TDataItemList.get (i: integer): TVariable;
   begin
      result := va[i]
   end;

function TDataItemList.Length: integer;
   begin
      result := System.Length(va)
   end;

function TDataItemList.DefinesVariable (v: TVariable): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length-1 do
         if va[i] = v then
            result := true
   end;

procedure TDataItemList.AddFromSourceTokens (context: TDefinition);
   var
      var_count: integer;

   procedure add_one_identifier_to_var_list;
      var
         idx: integer;
      begin
         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);

         idx := Length;
         SetLength(va, idx+1);
         va[idx] := target_cpu.TVariable_CreateForLaterDefinition(lex.token.identifier_idx, context);
         va[idx].src_loc := lex.token.src_loc;
         CurrentDefinitionTable.DefineForCurrentScope(lex.token.identifier_idx, va[idx], lex.token.src_loc);

         var_count := var_count + 1;
         lex.advance_token
      end;

   function overlapping_addr_range
      (a, b: TVariable
      ): boolean;
      function overlap
         (a_min, a_max, b_min, b_max: integer
         ): boolean;
         begin
            result := not ((b_max < a_min) or (a_max < b_min))
         end;
      var
         dummy_src_loc: TSourceLocation;
      begin
         dummy_src_loc := NonExistantSourceLocation;
         result := overlap (a.address,
                            a.address + target_cpu.ioregister_width_in_address_units(TTypeInfo(a.typedef.info).PackedSizeInBits, dummy_src_loc) - 1,
                            b.address,
                            b.address + target_cpu.ioregister_width_in_address_units(TTypeInfo(b.typedef.info).PackedSizeInBits, dummy_src_loc) - 1)
      end;

   var
      i: integer;
      typedef: TTypeDef;
      typedef_src_loc: TSourceLocation;
      initial_value: TStructuredConstant;
      cexpr: TCExpression;
      interrupt_var: TVariable;
   begin
      assert (lex.token.rw = descriptor);
      lex.advance_token;

      if (descriptor in [rw_var, rw_rom])
         and
         (Length = 0)
      then
         AddSelfToCodeBlockList;

      repeat
         var_count := 0;

         add_one_identifier_to_var_list;
         while lex.token_is_symbol(sym_comma) do
            begin
               if CheckForProhibitedDelayCall then
                  raise compile_error.Create (err_multiple_items_not_allowed_here);
               lex.advance_token;
               add_one_identifier_to_var_list
            end;

         if not lex.token_is_symbol(sym_colon) then
            raise compile_error.Create(err_colon_expected);
         lex.advance_token;

         typedef_src_loc := lex.token.src_loc;
         typedef := CreateTypeDenoterFromSourceTokens;

         if (typedef.type_kind = string_type)
            and
            (TStringType(typedef).max_length = -1) then
            begin
               typedef.Release;
               raise compile_error.Create(err_left_bracket_expected)
            end;

         if (context <> nil)
            and
            (context.definition_kind <> program_definition)
            and
            (typedef.IsProcessSystemType)
         then
            begin
               typedef.Release;
               raise compile_error.Create (err_process_variable_not_allowed_here, typedef_src_loc)
            end;

         if typedef.IsInterruptType then
            for i := var_count downto 1 do
               begin
                  TSystemType(typedef).interrupt_instance_count := TSystemType(typedef).interrupt_instance_count + 1;
                  if TSystemType(typedef).interrupt_instance_count > 1 then
                     begin
                        typedef.Release;
                        raise compile_error.Create (err_an_instance_of_this_interrupt_type_already_exists, va[Length-i].src_loc)
                     end
               end;

         for i := var_count downto 1 do
            begin
               va[Length-i].typedef := typedef;
               va[Length-i].typedef.AddRef;
               va[Length-i].typedef_src_loc := typedef_src_loc;
               va[Length-i].descriptor := descriptor;
               va[Length-i].address_mode := address_mode
            end;

         initial_value := nil;
         try
            case descriptor of
               rw_eeprom,
               rw_var:
                  begin
                     if (descriptor = rw_eeprom)
                        and
                        (not target_cpu.has_eeprom)
                     then
                        raise compile_error.Create (err_processor_does_not_support_eeprom);

                     if (typedef.ContainsQueueVariables)
                        and
                        (not BlockStack.ProcessingMonitorBlock) then
                        raise compile_error.Create(err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor, typedef_src_loc);

                     if (typedef.ContainsQueueVariables)
                        and
                        (descriptor = rw_eeprom) then
                        raise compile_error.Create (err_queue_type_not_allowed_here, typedef_src_loc);

                     if typedef.ContainsSystemType and (not (descriptor in [rw_const, rw_var])) then
                        raise compile_error.Create(err_system_type_variable_must_be_ram, typedef_src_loc);

                     if typedef.ContainsSystemType
                        and
                        TSystemType(typedef).interrupt_process
                     then
                        begin
                           if var_count > 1 then
                              raise compile_error.Create(err_multiple_interrupt_process_variables_not_allowed_in_same_variable_list,
                                                         va[Length-1].src_loc
                                                        );

                           if not lex.token_is_reserved_word(rw_interrupt) then
                              raise compile_error.Create(err_interrupt_specification_expected_for_interrrupt_process);
                           lex.advance_token;

                           if (not lex.token_is_identifier)
                              or
                              (CurrentDefinitionTable[lex.token.identifier_idx].definition_kind <> variable_definition)
                           then
                              raise compile_error.Create (err_interrupt_variable_expected);
                           interrupt_var := TVariable(CurrentDefinitionTable[lex.token.identifier_idx]);
                           if interrupt_var.typedef.type_kind <> system_type then
                              raise compile_error.Create (err_interrupt_variable_expected);
                           if TSystemType(interrupt_var.typedef).system_type_kind <> interrupt_system_type then
                              raise compile_error.Create (err_interrupt_variable_expected);
                           if interrupt_var.interrupt_assigned then
                              raise compile_error.Create (err_interrupt_variable_already_assigned_to_another_process);
                           if TSystemType(typedef).priority <> TSystemType(interrupt_var.typedef).priority then
                              raise compile_error.Create (err_process_priority_must_be_matched_to_interrupt_priority);

                           va[Length-1].interrupt := interrupt_var;
                           va[Length-1].interrupt.AddRef;
                           interrupt_var.interrupt_assigned := true;
                           interrupt_var.reachable := true;

                           lex.advance_token
                        end;

                     if BlockStack.ProcessingRoutineBlock then
                        begin
                           if typedef.IsClassSystemType then
                              raise compile_error.Create(err_class_variable_not_allowed_as_routine_local_variable, typedef_src_loc);
                           if typedef.IsMonitorSystemType then
                              raise compile_error.Create(err_monitor_variable_not_allowed_as_routine_local_variable, typedef_src_loc);
                        end
                  end;
               rw_ioreg:
                  check_for_valid_ioreg_type (typedef, typedef_src_loc);
               rw_rom:
                  begin
                     if typedef.ContainsQueueVariables
                        or
                        typedef.ContainsSystemType then
                        raise compile_error.Create(err_illegal_type_for_rom_constant, typedef_src_loc)
                  end;
               else
                  assert (false)
               end;

            if descriptor = rw_ioreg then
               begin
                  if not lex.token_is_reserved_word(rw_at) then
                     raise compile_error.Create(err_at_expected);
                  lex.advance_token;

                  cexpr := TCExpression.CreateFromSourceTokens;
                  try
                     if cexpr.constant_kind <> integer_constant then
                        raise compile_error.Create(err_invalid_address, cexpr.src_loc);

                     target_cpu.validate_ioregister_address(va[Length-1].name, cexpr.ordinal_value.AsInteger, typedef, cexpr.src_loc);

                     va[Length-1].MarkAsReachable;
                     va[Length-1].address := cexpr.ordinal_value.AsInteger
                  finally
                     cexpr.Release
                  end;
               end
            else
               if lex.token_is_reserved_word(rw_at) then
                  raise compile_error.Create(err_at_only_allowed_for_ioregisters);

            if initspec = diisNotAllowed then
               begin
                  if lex.token_is_symbol ([sym_equals, sym_assign]) then
                     raise compile_error.Create (err_initialization_not_allowed);
               end
            else   // initspec is diisOptional or diisRequired
               begin
                  if (initspec = diisRequired)
                     and
                     (not lex.token_is_symbol ([sym_equals, sym_assign])) then
                     raise compile_error.Create (err_initialization_required);

                  if lex.token_is_symbol ([sym_equals, sym_assign]) then
                     begin
                        if lex.token.symbol <> initialization_symbol then
                           case initialization_symbol of
                              sym_equals:
                                 if not lex.token_is_symbol (sym_equals) then
                                    raise compile_error.Create (err_equals_expected);
                              sym_assign:
                                 if not lex.token_is_symbol (sym_assign) then
                                    raise compile_error.Create (err_assignment_operator_expected);
                           else
                              assert (false)
                           end;
                        lex.advance_token;

                        if lex.token_is_symbol(sym_left_parenthesis) then
                           initial_value := TStructuredConstant.CreateFromSourceTokens (typedef, typedef_src_loc)
                        else if lex.token_is_identifier
                                and
                                (CurrentDefinitionTable[lex.token.identifier_idx].definition_kind = structured_constant_definition) then
                           begin
                              initial_value := TStructuredConstant(CurrentDefinitionTable[lex.token.identifier_idx]);
                              initial_value.AddRef;
                              typedef.CheckAssignmentCompatability(initial_value);
                              lex.advance_token
                           end
                        else
                           begin
                              cexpr := TCExpression.CreateFromSourceTokens;
                              typedef.CheckAssignmentCompatability(cexpr);
                              initial_value := TStructuredConstant.CreateFromConstant (typedef, cexpr);
                              cexpr.Release
                           end
                     end
               end;

            for i := var_count downto 1 do
               begin
                  va[Length-i].decl_end_src_loc := lex.token.src_loc;
                  if initial_value <> nil then
                     begin
                        va[Length-i].initial_value := initial_value;
                        va[Length-i].initial_value.AddRef
                     end
               end
         finally
            typedef.Release;
            initial_value.Release
         end;

         if not lex.token_is_symbol(sym_semicolon) then
            raise compile_error.Create(err_semicolon_expected);
         end_src_loc := lex.token.src_loc;
         lex.advance_token
      until not lex.token_is_identifier
   end;


// ===========
// TRoutine
// ===========

constructor TRoutine.CreateFromSourceTokens
   (cntxt: TDefinition
   );
   var
      routine_kind:
         (procedure_kind,
          function_kind
         );
      typedef: TTypeDef;
      typedef_src_loc: TSourceLocation;
      parameter_context: TParamListContext;
   begin
      inherited Create(routine_definition);
      context := cntxt;
      header_only :=
         (lex.token.in_preamble)
         and
         (context.definition_kind = program_definition);   // stand-alone procedure
      BlockStack.push(Self);
      try
         CurrentDefinitionTable.EnterNewScope;

         // routine context may be program, process, monitor or class (or nil in test cases)
         if context <> nil then
            begin
               assert(context.definition_kind in [type_definition, program_definition]);
               if (context.definition_kind = type_definition) then
                  assert(TTypeDef(context).type_kind = system_type)
            end;

         // PROCEDURE or FUNCTION keyword
         assert(lex.token_is_reserved_word([rw_procedure, rw_function]));
         if lex.token_is_reserved_word(rw_procedure) then
            routine_kind := procedure_kind
         else
            routine_kind := function_kind;
         lex.advance_token;

         // ENTRY modifier
         entry := false;
         if lex.token_is_reserved_word(rw_entry) then
            begin
               if context.definition_kind = program_definition then
                  raise compile_error.Create(err_entry_routines_may_only_be_declared_for_monitors_and_classes);
               if TSystemType(context).system_type_kind = process_system_type then
                  raise compile_error.Create(err_entry_routines_may_only_be_declared_for_monitors_and_classes);
               entry := true;
               lex.advance_token
            end;

         // ROUTINE NAME
         if not lex.token_is_identifier then
            raise compile_error.Create(err_identifier_expected);
         routine_id_idx := lex.token.identifier_idx;
         routine_id_src_loc := lex.token.src_loc;

         CurrentDefinitionTable.DefineForCurrentScope(lex.token.identifier_idx, Self, lex.token.src_loc);
         if routine_kind = function_kind then
            begin
               function_result := target_cpu.TVariable_CreateForLaterDefinition(symbol_id('result'), Self);    // old: lex.token.identifier_idx
               CurrentDefinitionTable.DefineForCurrentScope(symbol_id('result'), function_result, lex.token.src_loc)
            end;
         lex.advance_token;

         // PARAMETER LIST
         parameter_context := process_local_routine_param_list;  // only to suppress warning
         if (context = nil) // test cases
            or
            (context.definition_kind = program_definition)
         then
            parameter_context := standalone_routine_param_list
         else // system type
            case TSystemType(context).system_type_kind of
               process_system_type:
                  parameter_context := process_local_routine_param_list;
               monitor_system_type:
                  if entry then
                     parameter_context := monitor_entry_routine_param_list
                  else
                     parameter_context := monitor_local_routine_param_list;
               class_system_type:
                  if entry then
                     parameter_context := class_entry_routine_param_list
                  else
                     parameter_context := class_local_routine_param_list;
               interrupt_system_type:
                  parameter_context := class_local_routine_param_list;  // irrelevent since parameter list will be empty
            else
               assert(false)
            end;
         parameter_definitions := target_cpu.TParamList_CreateFromSourceTokens(parameter_context);

         // FUNCTION RESULT
         if routine_kind = function_kind then
            begin
               if not lex.token_is_symbol(sym_colon) then
                  raise compile_error.Create(err_colon_expected);
               lex.advance_token;

               typedef_src_loc := lex.token.src_loc;
               typedef := CreateTypeDenoterFromSourceTokens;
               if not (typedef.type_kind in [basic_data_type, set_type, string_type]) then
                  begin
                     typedef.Release;
                     raise compile_error.Create(err_function_result_must_be_simple_type, typedef_src_loc)
                  end;

               function_result.typedef := typedef;
               function_result.typedef.AddRef;
               function_result.typedef_src_loc := typedef_src_loc;
               function_result.descriptor := rw_var;
               if typedef.type_kind = string_type then
                  function_result.address_mode := local_indirect_address_mode
               else
                  begin
                     function_result.address_mode := local_address_mode;
                     if lex.token_is_symbol (sym_assign) then
                        begin   // initializer for function result
                           lex.advance_token;
                           function_result.initial_value := TStructuredConstant.CreateFromSourceTokens (typedef, typedef_src_loc)
                        end
                  end;

               typedef.Release
            end;

         if not lex.token_is_symbol(sym_semicolon) then
            raise compile_error.Create(err_semicolon_expected);
         lex.advance_token;

         if not header_only then
            begin
               create_block_from_source_tokens;
               AddSelfToCodeBlockList
            end;

         definition_complete := true
      finally
         CurrentDefinitionTable.ExitScope;
         BlockStack.pop
      end
   end;

constructor TRoutine.CreatePropertySetterFromSourceTokens
   (cntxt: TDefinition;
    entre: boolean;
    property_id_idx: TIdentifierIdx;
    property_id_src_loc:
    TSourceLocation;
    property_typdef: TTypeDef
   );
   begin
      inherited Create(routine_definition);
      BlockStack.push(Self);
      context := cntxt;
      entry := entre;

      assert(lex.token_is_reserved_word(rw_set));
      lex.advance_token;

      CurrentDefinitionTable.EnterNewScope;
      try
         if not lex.token_is_symbol(sym_colon) then
            raise compile_error.Create(err_colon_expected);
         lex.advance_token;

         parameter_definitions := target_cpu.TParamList_CreatePropertyPseudoParamList(property_id_idx, property_id_src_loc, property_typdef);

         create_block_from_source_tokens;

         AddSelfToCodeBlockList;

         definition_complete := true
      finally
         CurrentDefinitionTable.ExitScope;
         BlockStack.pop
      end
   end;

constructor TRoutine.CreatePropertyGetterFromSourceTokens
   (cntxt: TDefinition;
    entre: boolean;
    property_id_idx: TIdentifierIdx;
    property_id_src_loc: TSourceLocation;
    property_typdef: TTypeDef
   );
   begin
      inherited Create(routine_definition);
      BlockStack.push(Self);
      context := cntxt;
      entry := entre;
      try
         assert(lex.token_is_reserved_word(rw_get));
         lex.advance_token;

         if not lex.token_is_symbol(sym_colon) then
            raise compile_error.Create(err_colon_expected);
         lex.advance_token;

         CurrentDefinitionTable.EnterNewScope;

         if property_typdef.type_kind = string_type then
            function_result := target_cpu.TVariable_Create(property_id_idx, nil, property_typdef, rw_var, local_indirect_address_mode)
         else
            function_result := target_cpu.TVariable_Create(property_id_idx, nil, property_typdef, rw_var, local_address_mode);
         CurrentDefinitionTable.DefineForCurrentScope(property_id_idx, function_result, property_id_src_loc);

         create_block_from_source_tokens;

         AddSelfToCodeBlockList;

         definition_complete := true
      finally
         CurrentDefinitionTable.ExitScope;
         BlockStack.pop
      end
   end;

procedure TRoutine.create_block_from_source_tokens;
   begin
      block_header_end_src_loc := lex.previous_token_src_loc;
      statement_list := target_cpu.TStatementList_Create;
      local_vars := target_cpu.TDataItemList_Create;
      local_vars.define_as_var_list (rw_var, local_address_mode);

      while lex.token_is_reserved_word([rw_const, rw_type, rw_var, rw_rom, rw_eeprom, rw_ioreg]) do
         begin
            if lex.token_is_reserved_word(rw_const) then
               process_constant_definition_part;
            if lex.token_is_reserved_word(rw_type) then
               process_type_definition_part;
            if lex.token_is_reserved_word(rw_rom) then
               process_rom_constant_definition_part;
            if lex.token_is_reserved_word(rw_eeprom) then
               raise compile_error.Create(err_eeprom_variables_only_allowed_as_system_type_permanent_variables);
            if lex.token_is_reserved_word(rw_ioreg) then
               raise compile_error.Create(err_ioregisters_must_be_declared_at_global_level);
            if lex.token_is_reserved_word(rw_var) then
               local_vars.AddFromSourceTokens (Self)
         end;

      last_var_declaration_src_loc := lex.previous_token_src_loc;

      if not lex.token_is_reserved_word(rw_begin) then
         raise compile_error.Create(err_begin_expected);
      block_begin_src_loc := lex.token.src_loc;
      lex.advance_token;

      TStatementList(statement_list).AddFromSourceTokens([rw_end], err_semicolon_expected);
      block_end_src_loc := lex.token.src_loc;
      lex.advance_token;

      if not lex.token_is_symbol(sym_semicolon) then
         raise compile_error.Create(err_semicolon_expected);
      lex.advance_token
   end;

destructor TRoutine.Destroy;
   begin
      parameter_definitions.Release;
      function_result.Release;
      local_vars.Release;
      statement_list.Release;
      inherited
   end;

function TRoutine.AssembleAndCheckCallerParameterListFromSourceTokens: TArrayOfTDefinition;
   begin
      result := parameter_definitions.AssembleAndCheckCallerParameterListFromSourceTokens
   end;

procedure TRoutine.MarkAsReachable;
   begin
      inherited;
      statement_list.MarkAsReachable
   end;

function TRoutine.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := false;
      if statement_list <> nil then
         result := statement_list.CheckForProhibitedDelayCall (err_msg)
   end;


// ============
// TProperty
// ============

constructor TProperty.CreateFromSourceTokens
   (cntxt: TDefinition
   );
   var
      property_id_src_loc: TSourceLocation;
   begin
      inherited Create(property_definition);

      assert(lex.token_is_reserved_word(rw_property));
      lex.advance_token;

      // ENTRY modifier
      entry := false;
      if lex.token_is_reserved_word(rw_entry) then
         begin
            entry := true;
            lex.advance_token
         end;

      if not lex.token_is_identifier then
         raise compile_error.Create(err_identifier_expected);
      id := lex.token.identifier_idx;
      property_id_src_loc := lex.token.src_loc;
      lex.advance_token;

      if not lex.token_is_symbol(sym_colon) then
         raise compile_error.Create(err_colon_expected);
      lex.advance_token;

      typedef_src_loc := lex.token.src_loc;
      typedef := CreateTypeDenoterFromSourceTokens;
      if not (typedef.type_kind in [basic_data_type, set_type, string_type]) then
         raise compile_error.Create(err_property_must_be_simple_type, typedef_src_loc);
      if (typedef.type_kind = string_type)
         and
         (TStringType(typedef).max_length > -1) then
         raise compile_error.Create(err_string_property_must_be_undimensioned, typedef_src_loc);

      if not lex.token_is_symbol(sym_semicolon) then
         raise compile_error.Create(err_semicolon_expected);
      lex.advance_token;

      while lex.token_is_reserved_word([rw_set, rw_get]) do
         begin
            if lex.token_is_reserved_word(rw_set) then
               begin
                  if set_proc <> nil then
                     raise compile_error.Create(err_property_setter_already_defined);
                  try
                     set_proc := target_cpu.TRoutine_CreatePropertySetterFromSourceTokens(cntxt, entry, id, property_id_src_loc, typedef)
                  except
                     on c: compile_error do
                        if c.message = err_cannot_change_constant_parameter then
                           raise compile_error.Create (err_illegal_use_of_property_name_in_property_setter_block, c.source_location)
                        else
                           raise
                  end
               end;
            if lex.token_is_reserved_word(rw_get) then
               begin
                  if get_func <> nil then
                     raise compile_error.Create(err_property_getter_already_defined);
                  get_func := target_cpu.TRoutine_CreatePropertyGetterFromSourceTokens(cntxt, entry, id, property_id_src_loc, typedef)
               end;
         end;

      CurrentDefinitionTable.DefineForCurrentScope(id, Self, property_id_src_loc)
   end;

destructor TProperty.Destroy;
   begin
      inherited;
      typedef.Release;
      set_proc.Release;
      get_func.Release
   end;


// ==============
// TSystemType
// ==============

constructor TSystemType.CreateFromSourceTokens;
   var
      prop: TProperty;
      routine: TRoutine;
      i: integer;
      priority_value: TCExpression;
   begin
      inherited Create(system_type);
      src_loc := lex.token.src_loc;

      priority := target_cpu.initial_process_priority;

      BlockStack.push(Self);

      initial_statement := target_cpu.TStatementList_Create;

      // MONITOR, CLASS, PROCESS or INTERRUPT keyword
      case lex.token.rw of
         rw_monitor:
            system_type_kind := monitor_system_type;
         rw_class:
            system_type_kind := class_system_type;
         rw_process:
            system_type_kind := process_system_type;
         rw_interrupt:
            system_type_kind := interrupt_system_type;
      else
         assert (false)
      end;
      lex.advance_token;

      CurrentDefinitionTable.EnterNewScope;

      try
         permanent_ram_vars := target_cpu.TDataItemList_Create;
         if system_type_kind = interrupt_system_type then
            permanent_ram_vars.define_as_var_list (rw_var, absolute_address_mode)
         else
            permanent_ram_vars.define_as_var_list (rw_var, system_type_address_mode);

         permanent_eeprom_vars := target_cpu.TDataItemList_Create;
         permanent_eeprom_vars.define_as_var_list (rw_eeprom, system_type_address_mode);
         ioregisters := target_cpu.TDataItemList_Create;
         ioregisters.define_as_ioreg_list;

         case system_type_kind of
            class_system_type:
               parameters := target_cpu.TParamList_CreateFromSourceTokens(class_init_param_list);
            monitor_system_type:
               parameters := target_cpu.TParamList_CreateFromSourceTokens(monitor_init_param_list);
            process_system_type:
               parameters := target_cpu.TParamList_CreateFromSourceTokens(process_init_param_list);
            interrupt_system_type:
               begin
                  if lex.token_is_symbol (sym_left_parenthesis) then
                     raise compile_error.Create (err_paramters_for_interrupt_definition_not_allowed);
                  parameters := target_cpu.TParamList_CreateFromSourceTokens(process_init_param_list)  // context irrelevent since parameter list not allowed
               end;
         else
            assert(false)
         end;

         if not parameters.Empty then
            begin
               if not lex.token_is_symbol(sym_semicolon) then
                  raise compile_error.Create(err_semicolon_expected);
               lex.advance_token
            end;

         if system_type_kind in [process_system_type, interrupt_system_type] then
            begin
               if not lex.token_is_reserved_word(rw_priority) then
                  raise compile_error.Create(err_priority_expected);
               lex.advance_token;

               priority_value := TCExpression.CreateFromSourceTokens;
               try
                  if priority_value.constant_kind <> integer_constant then
                     raise compile_error.Create(err_integer_expected, priority_value.src_loc);
                  priority := priority_value.ordinal_value.AsInteger;
                  target_cpu.validate_process_priority(priority, priority_value.src_loc);
                  if (system_type_kind = interrupt_system_type)
                     and
                     (priority <= 0)
                  then
                     raise compile_error.Create (err_interrupt_priority_must_be_greater_than_0, priority_value.src_loc);
               finally
                  priority_value.Release
               end;

               if not lex.token_is_symbol(sym_semicolon) then
                  raise compile_error.Create(err_semicolon_expected);
               lex.advance_token
            end;

         while lex.token_is_reserved_word([rw_const, rw_type, rw_rom, rw_eeprom, rw_var, rw_property, rw_procedure, rw_function, rw_ioreg]) do
            case lex.token.rw of
               rw_const:
                  process_constant_definition_part;
               rw_type:
                  process_type_definition_part;
               rw_rom:
                  process_rom_constant_definition_part;
               rw_eeprom:
                  begin
                     if not target_cpu.has_eeprom then
                        raise compile_error.Create (err_processor_does_not_support_eeprom);
                     if system_type_kind = interrupt_system_type then
                        raise compile_error.Create(err_eeprom_variables_not_allowed_in_interrupt_definitions);
                     permanent_eeprom_vars.AddFromSourceTokens (Self)
                  end;
               rw_var:
                  permanent_ram_vars.AddFromSourceTokens (Self);
               rw_ioreg:
                  raise compile_error.Create(err_ioregister_not_allowed);
               rw_property:
                  begin
                     if system_type_kind = process_system_type then
                        raise compile_error.Create(err_properties_not_allowed_in_processes);
                     if system_type_kind = interrupt_system_type then
                        raise compile_error.Create(err_properties_not_allowed_in_interrupt_definitions);

                     prop := target_cpu.TProperty_CreateFromSourceTokens(Self);
                     i := Length(properties);
                     SetLength(properties, i + 1);
                     properties[i] := prop
                  end;
               rw_procedure,
               rw_function:
                  begin
                     if system_type_kind = interrupt_system_type then
                        check_interrupt_routine_signature;
                     routine := target_cpu.TRoutine_CreateFromSourceTokens(Self);

                     CurrentDefinitionTable.DefineForCurrentScope(routine.routine_id_idx, routine, routine.routine_id_src_loc);
                     i := Length(routines);
                     SetLength(routines, i + 1);
                     routines[i] := routine
                  end;
            else
               assert (false)
            end;

         last_var_declaration_src_loc := lex.previous_token_src_loc;

         block_begin_src_loc := lex.token.src_loc;
         if not lex.token_is_reserved_word(rw_begin) then
            raise compile_error.Create(err_begin_expected);
         lex.advance_token;

         TStatementList(initial_statement).AddFromSourceTokens([rw_end], err_semicolon_expected);

         if IsProcessSystemType then
            begin
               if (Length (TStatementList(initial_statement).stmts) = 0)
                  or
                  (TStatementList(initial_statement).stmts[Length (TStatementList(initial_statement).stmts)-1].statement_kind <> cycle_statement)
               then
                  raise compile_error.Create (err_last_statement_of_process_must_be_cycle_repeat);

               if interrupt_process then
                  begin
                     for i := 0 to Length(routines)-1 do
                        routines[i].CheckForProhibitedDelayCall (err_cant_call_delay_from_an_interrupt_process);
                     initial_statement.CheckForProhibitedDelayCall (err_cant_call_delay_from_an_interrupt_process)
                  end
               else if priority > 0 then  // await interrupt never called
                  raise compile_error.Create (err_positive_priority_process_must_call_await_interrupt, src_loc)
            end
         else if IsInterruptType then
            begin
               assert (parameters.Length = 0);
               if Length(routines) <> 1 then
                  raise compile_error.Create (err_interrupt_definition_must_implement_signalled_function, src_loc);
            end;

         block_end_src_loc := lex.token.src_loc;
         lex.advance_token;

         AddSelfToCodeBlockList   // code part
      finally
         CurrentDefinitionTable.ExitScope;
         BlockStack.pop
      end;

      MarkTypeDefinitionAsComplete
   end;

destructor TSystemType.Destroy;
   var
      i: integer;
   begin
      parameters.Release;
      permanent_ram_vars.Release;
      permanent_eeprom_vars.Release;
      ioregisters.Release;
      for i := 0 to Length(routines) - 1 do
         routines[i].Release;
      for i := 0 to Length(properties) - 1 do
         properties[i].Release;
      initial_statement.Release;
      inherited
   end;

function TSystemType.IsClassSystemType: boolean;
   begin
      result := system_type_kind = class_system_type
   end;

function TSystemType.IsMonitorSystemType: boolean;
   begin
      result := system_type_kind = monitor_system_type
   end;

function TSystemType.IsProcessSystemType: boolean;
   begin
      result := system_type_kind = process_system_type
   end;

function TSystemType.IsInterruptType: boolean;
   begin
      result := system_type_kind = interrupt_system_type
   end;

function TSystemType.requires_initialization: boolean;
   begin
      result := (not parameters.Empty)
                or
                (Length (TStatementList(initial_statement).stmts) > 0)
   end;

procedure TSystemType.SetMonitorPriorities
   (prio: integer
   );
   var
      i: integer;
   begin
      case system_type_kind of
         process_system_type:
            assert (priority = prio);
         monitor_system_type:
            if priority < prio then
               priority := prio;
         class_system_type,
         interrupt_system_type:
            ;
      else
         assert (false)
      end;
      for i := 0 to Length(parameters.parameter_definitions) - 1 do
         parameters.parameter_definitions[i].typedef.SetMonitorPriorities(priority);
      for i := 0 to Length(permanent_ram_vars.va) - 1 do
         permanent_ram_vars.va[i].typedef.SetMonitorPriorities(priority)
   end;

procedure TSystemType.MarkAsReachable;
   begin
      inherited;
      initial_statement.MarkAsReachable
   end;


// ===================
// TDefinitionStack

procedure TDefinitionStack.push
   (stmt: TDefinition
   );
   var
      i: integer;
   begin
      i := Length(stk);
      SetLength(stk, i + 1);
      stk[i] := stmt;
      stmt.AddRef
   end;

procedure TDefinitionStack.pop;
   begin
      stk[tos_idx].Release;
      SetLength(stk, Length(stk) - 1);
   end;

function TDefinitionStack.tos: TDefinition;
   var
      i: integer;
   begin
      i := tos_idx;
      if i = -1 then
         result := nil
      else
         result := stk[i]
   end;

function TDefinitionStack.tos_idx: integer;
   begin
      result := Length(stk) - 1
   end;

destructor TDefinitionStack.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(stk) - 1 do
         stk[i].Release
   end;


// ===========
// TProgram
// ===========

constructor TProgram.CreateFromSourceTokens;
   var
      i: integer;
      gr: TRoutine;
      in_preamble: boolean;
      scope_count: integer;
      final_cycle_stmt: TCycleStatement;
   begin
      scope_count := 0;
      the_program := Self;

      inherited Create(program_definition);

      if lex.token.token_kind = eof_token then
         raise compile_error.Create(err_empty_file, NonExistantSourceLocation);

      BlockStack.push(Self);

      initial_statement := target_cpu.TStatementList_Create;

      program_vars := target_cpu.TDataItemList_Create;
      program_vars.define_as_var_list (rw_var, absolute_address_mode);
      ioregisters := target_cpu.TDataItemList_Create;
      ioregisters.define_as_ioreg_list;

      in_preamble := lex.token.in_preamble;

      try
         while lex.token_is_reserved_word([rw_const, rw_type, rw_rom, rw_var, rw_eeprom, rw_ioreg, rw_procedure, rw_function]) do
            begin
               if lex.token_is_reserved_word(rw_const) then
                  process_constant_definition_part
               else if lex.token_is_reserved_word(rw_type) then
                  process_type_definition_part
               else if lex.token_is_reserved_word(rw_rom) then
                  process_rom_constant_definition_part
               else if lex.token_is_reserved_word(rw_eeprom) then
                  raise compile_error.Create(err_eeprom_variables_only_allowed_as_system_type_permanent_variables)
               else if lex.token_is_reserved_word(rw_ioreg) then
                  ioregisters.AddFromSourceTokens (nil)
               else if lex.token_is_reserved_word([rw_procedure, rw_function]) then
                  begin
                     gr := target_cpu.TRoutine_CreateFromSourceTokens(Self);
                     CurrentDefinitionTable.DefineForCurrentScope(gr.routine_id_idx, gr, gr.routine_id_src_loc);
                     if not gr.header_only then
                        begin
                           i := Length(global_routines);
                           SetLength(global_routines, i + 1);
                           global_routines[i] := gr;
                           global_routines[i].AddRef
                        end;
                     gr.Release
                  end
               else if lex.token_is_reserved_word(rw_var) then
                  program_vars.AddFromSourceTokens (Self);

               if in_preamble and not lex.token.in_preamble then
                  begin
                     in_preamble := false;
                     target_cpu.process_preamble;
                     CurrentDefinitionTable.EnterNewScope;
                     scope_count := 1
                  end
            end;

         last_var_declaration_src_loc := lex.previous_token_src_loc;

         global_declarations_examination_hook;

         begin_src_loc := lex.token.src_loc;
         if not lex.token_is_reserved_word(rw_begin) then
            raise compile_error.Create(err_begin_expected);
         lex.advance_token;

         TStatementList(initial_statement).AddFromSourceTokens([rw_end], err_semicolon_expected);
         initial_statement.CheckForProhibitedDelayCall (err_cant_call_delay_from_initial_process);

         end_src_loc := lex.token.src_loc;
         lex.advance_token;

         if not lex.token_is_symbol(sym_dot) then
            raise compile_error.Create(err_final_period__expected);
         lex.advance_token;

         if not lex.token_is_eof then
            raise compile_error.Create(err_end_of_source_expected);

         if (Length(TStatementList(initial_statement).stmts) > 0)
            and
            (TStatementList(initial_statement).stmts[Length(TStatementList(initial_statement).stmts)-1].statement_kind = cycle_statement) then
            begin
               final_cycle_stmt := TCycleStatement (TStatementList(initial_statement).stmts[Length(TStatementList(initial_statement).stmts)-1]);
               if Length(final_cycle_stmt.statement_list.stmts) > 0 then
                  raise compile_error.Create (err_final_cycle_statement_must_be_empty, final_cycle_stmt.statement_list.stmt[0].src_loc);
               final_cycle_stmt.is_empty_loop_at_end_of_program_initial_statement := true
            end;

         // assign monitor priorities
         for i := 0 to Length(program_vars.va) - 1 do
            if (program_vars.va[i].typedef.type_kind = system_type)
               and
//               (program_vars.va[i].reachable)
//               and
               (TSystemType(program_vars.va[i].typedef).system_type_kind = process_system_type) then
               TSystemType(program_vars.va[i].typedef).SetMonitorPriorities (TSystemType(program_vars.va[i].typedef).priority);

         AddSelfToCodeBlockList
      finally
         if scope_count = 1 then
            CurrentDefinitionTable.ExitScope;
         BlockStack.pop
      end
   end;

destructor TProgram.Destroy;
   var
      i: integer;
   begin
      target_cpu.release_preamble;
      for i := 0 to Length(global_routines) - 1 do
         global_routines[i].Release;
      initial_statement.Release;
      program_vars.Release;
      ioregisters.Release;
      the_program := nil;
      inherited
   end;

procedure TProgram.MarkAsReachable;
   begin
      inherited;
      initial_statement.MarkAsReachable
   end;

function TProgram.FinalStatementIsEmptyCycleStatement: boolean;
   var
      len: integer;
   begin
      len := Length(TStatementList(initial_statement).stmts);
      result := (len > 0)
                and
                (TStatementList(initial_statement)[len-1].statement_kind = cycle_statement)
   end;

procedure TProgram.global_declarations_examination_hook;
   begin
   end;


//==============
//  TBlockStack

function TBlockStack.IsLocalRAMVariable
   (v: TVariable
   ): boolean;
   begin
      result := false; // suppress compiler warning
      case BlockStack.tos.definition_kind of
         program_definition:
            result := TProgram(BlockStack.tos).program_vars.DefinesVariable(v);
         type_definition:
            begin
               assert(TTypeDef(BlockStack.tos).type_kind = system_type);
               result := TSystemType(BlockStack.tos).permanent_ram_vars.DefinesVariable(v)
            end;
         routine_definition:
            result := TRoutine(BlockStack.tos).local_vars.DefinesVariable(v);
      else
         assert(false)
      end;
   end;

procedure TBlockStack.CheckAddressReachability
   (v: TVariable;
    src_loc: TSourceLocation
   );
   var
      reachable: boolean;
   begin
      if Length(stk) = 0 then // test mode
         exit;

      if v.descriptor = rw_rom then
         exit;

      if v.address_mode <> absolute_address_mode then
         exit;

      if v.descriptor = rw_ioreg then
         exit;

      reachable := false;  // suppress compiler warning
      case tos.definition_kind of
         program_definition:
            reachable := true;
         type_definition:   // is initialization section of a system type
            begin
               assert (TTypeDef(tos).type_kind = system_type);
               reachable := false
            end;
         routine_definition:
            reachable := false;
      else
         assert (false)
      end;

      if not reachable then
         raise compile_error.Create(err_global_variable_not_accessible_here, src_loc)
   end;

function TBlockStack.ProcessingProgramBlock: boolean;
   begin
      result := (Length(stk) > 0) and (tos.definition_kind = program_definition)
   end;

function TBlockStack.ProcessingSystemTypeBlock: boolean;
   begin
      result :=
         (Length(stk) > 0)
         and
         (tos.definition_kind = type_definition)
         and
         (TTypeDef(BlockStack.tos).type_kind = system_type)
   end;

function TBlockStack.ProcessingMonitorBlock: boolean;
   begin
      result := ProcessingSystemTypeBlock and (TSystemType(tos).system_type_kind = monitor_system_type)
   end;

function TBlockStack.ProcessingRoutineBlock: boolean;
   begin
      result := (Length(stk) > 0) and (tos.definition_kind = routine_definition)
   end;


INITIALIZATION
   BlockStack := TBlockStack.Create;

FINALIZATION
   BlockStack.Free;

END.
