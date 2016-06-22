UNIT test_cpu_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses Classes, cpc_target_cpu_unit, cpc_source_analysis_unit, cpc_definitions_unit, cpc_expressions_unit,
   cpc_statements_unit, cpc_blocks_unit, cpc_access_unit, cpc_core_objects_unit, cpc_simple_expression_unit,
   cpc_term_expression_unit;

type
   TTestCPU =
      class (TTargetCPUBaseClass)
      private
         f_mod_operator_implementation: Tmod_operator_implementation;
      public
         constructor Create;

         function process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
            override;

         procedure add_successful_compilation_info (results_listing: TStrings);
            override;

         function has_eeprom: boolean;
            override;

         function load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
            override;
         function mod_operator_implementation: Tmod_operator_implementation;
            override;
         procedure validate_ioregister_address
            (name: string;
             addr: integer;
             type_def: TTypeDef;
             src_loc: TSourceLocation
            );
            override;
         procedure validate_process_priority
            (priority: integer;
             src_loc: TSourceLocation
            );
            override;
         function initial_process_priority: integer;
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

         {$include TTestCPU_constructor_function_overload_decls.inc}

         procedure generate_machine_code (prog: TProgram);
            override;
         function src_directory_relative_to_bin: string;
            override;
         procedure set_mod_operator_implementation (impl: Tmod_operator_implementation);
      end;

IMPLEMENTATION

uses cpc_common_unit, SysUtils;

type
   TTestSystemType =
      class (TSystemType)
      protected
         procedure check_interrupt_routine_signature;
            override;
      end;

procedure TTestSystemType.check_interrupt_routine_signature;
   var
      mark: integer;
   begin
      mark := lex.mark_token_position;
      if Length(routines) > 0 then
         raise compile_error.Create (err_only_one_routine_allowed_in_interrupt_definition);
      if lex.token_is_reserved_word(rw_procedure) then
         raise compile_error.Create(err_procedures_not_allowed_in_interrupt_definitions);
      lex.advance_token;
      if (not lex.token_is_identifier)
         or
         (LowerCase (lex.identifiers[lex.token.identifier_idx]) <> 'signalled') then
         raise compile_error.Create (err_signalled_function_required);
      lex.advance_token;
      if not lex.token_is_symbol (sym_colon) then
         raise compile_error.Create (err_colon_expected);
      lex.advance_token;
      if LowerCase (lex.identifiers[lex.token.identifier_idx]) <> 'boolean' then
         raise compile_error.Create (err_signalled_function_result_must_be_boolean);
      lex.backup (mark)
   end;

constructor TTestCPU.Create;
   begin
      inherited Create ('TestCPU');
      add_additional_supported_data_type ('boolean', TBooleanDataType.Create (1));
      add_additional_supported_data_type ('char', TCharDataType.Create (8));
      add_additional_supported_data_type ('queue', TQueueType.Create);
      add_additional_supported_data_type ('real', TFloatingPointDataType.Create(64))
   end;

function TTestCPU.process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
   const
      compiler_directive = '{$stmt';
   var
      stmt: string;
      in_preamble: boolean;
   begin
      if Pos (compiler_directive, simplified_line) = 1 then
         begin
            stmt := extract_quoted_compiler_directive_parameter (compiler_directive, simplified_line, 'stmt', src_location);
            in_preamble := true;
            add_line_to_source (stmt, 0, 0, in_preamble);
            result := true
         end
      else
         result := false
   end;

procedure TTestCPU.add_successful_compilation_info (results_listing: TStrings);
   begin
      results_listing.Add ('..compilation info..')
   end;

function TTestCPU.has_eeprom: boolean;
   begin
      result := true
   end;

function TTestCPU.load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
   begin
      result := nil;  // suppress compiler warning
      case def.definition_kind of
         type_definition:
            result := TTypeInfo.Create (def);
         expression_definition:
            result := TTypeInfo.Create (def);
      else
         assert (false)
      end
   end;

function TTestCPU.mod_operator_implementation: Tmod_operator_implementation;
   begin
      result := f_mod_operator_implementation
   end;

procedure TTestCPU.validate_ioregister_address
   (name: string;
    addr: integer;
    type_def: TTypeDef;
    src_loc: TSourceLocation
   );
   begin
      if (addr < 1000) or (addr > 2000) then   // would check typedef's size bytes
         raise compile_error.Create(err_invalid_ioregister_address, src_loc);
   end;

procedure TTestCPU.validate_process_priority
   (priority: integer;
    src_loc: TSourceLocation
   );
   begin
      if (priority < -128) or (priority > 5) then
         raise compile_error.Create(err_invalid_process_priority, src_loc);
   end;

function TTestCPU.initial_process_priority: integer;
   begin
      result := -maxint
   end;

function TTestCPU.max_supported_ordinal_size: integer;
   begin
      result := 56
   end;

function TTestCPU.ioregister_width_in_address_units
   (width_in_bits: integer;
    src_loc: TSourceLocation
   ): integer;
   begin
      case width_in_bits of
          8: result := 1;
         16: result := 2;
         24: result := 3;
      else
         raise compile_error.Create(err_invalid_total_ioregister_width, src_loc)
      end
   end;

function TTestCPU.round_trunc_result_type: TTypeDef;
   begin
      result := get_supported_data_type ('int16')
   end;

procedure TTestCPU.record_anonymous_string_constant (s: string);
   begin
   end;

{$include TTestCPU_constructor_functions.inc}

procedure TTestCPU.generate_machine_code (prog: TProgram);
   begin
   end;

function TTestCPU.src_directory_relative_to_bin: string;
   begin
      result := '..\source\cpc_core'
   end;

procedure TTestCPU.set_mod_operator_implementation (impl: Tmod_operator_implementation);
   begin
      f_mod_operator_implementation := impl
   end;

INITIALIZATION
   target_cpu := TTestCPU.Create;

FINALIZATION
   target_cpu.Free

END.
