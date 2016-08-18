UNIT dummy_pic18x_cpu_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses Classes, cpc_target_cpu_unit, cpc_source_analysis_unit, cpc_definitions_unit, cpc_expressions_unit,
   cpc_statements_unit, cpc_blocks_unit, cpc_access_unit, cpc_core_objects_unit, cpc_simple_expression_unit,
   cpc_term_expression_unit;

type
   TDummyPIC18xCPU =
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

         {$include TDummyPIC18xCPU_constructor_function_overload_decls.inc}

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
         raise compile_error.Create (err_signaled_function_required);
      lex.advance_token;
      if not lex.token_is_symbol (sym_colon) then
         raise compile_error.Create (err_colon_expected);
      lex.advance_token;
      if LowerCase (lex.identifiers[lex.token.identifier_idx]) <> 'boolean' then
         raise compile_error.Create (err_signaled_function_result_must_be_boolean);
      lex.backup (mark)
   end;


constructor TDummyPIC18xCPU.Create;
   begin
      inherited Create ('PIC18x');
      add_additional_supported_data_type ('boolean', TBooleanDataType.Create (1))
   end;

function TDummyPIC18xCPU.process_compiler_directive (simplified_line: string; src_location: TSourceLocation): boolean;
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
            except
               on EFileDoesntExist do
                  raise compile_error.Create(format ('pic18x .inc file doesn'' exist', [processor]), FileErrorSourceLocation);
               on ECantOpenFile do
                  raise compile_error.Create(format ('can''t open pic18x .inc file', [full_path_fn]), FileErrorSourceLocation)
            end;
            result := true
         end
      else
         result := false
   end;

procedure TDummyPIC18xCPU.add_successful_compilation_info (results_listing: TStrings);
   begin
      results_listing.Add ('..compilation info..')
   end;

function TDummyPIC18xCPU.has_eeprom: boolean;
   begin
      result := true
   end;

function TDummyPIC18xCPU.load_cpu_specific_info (def: TDefinition): TCPUSpecificInfo;
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

function TDummyPIC18xCPU.mod_operator_implementation: Tmod_operator_implementation;
   begin
      result := f_mod_operator_implementation
   end;

procedure TDummyPIC18xCPU.validate_ioregister_address
   (name: string;
    addr: integer;
    type_def: TTypeDef;
    src_loc: TSourceLocation
   );
   begin
   end;

procedure TDummyPIC18xCPU.validate_process_priority
   (priority: integer;
    src_loc: TSourceLocation
   );
   begin
   end;

function TDummyPIC18xCPU.initial_process_priority: integer;
   begin
      result := -maxint
   end;

function TDummyPIC18xCPU.max_supported_ordinal_size: integer;
   begin
      result := 64
   end;

function TDummyPIC18xCPU.ioregister_width_in_address_units
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

function TDummyPIC18xCPU.round_trunc_result_type: TTypeDef;
   begin
      result := get_supported_data_type ('int16')
   end;

procedure TDummyPIC18xCPU.record_anonymous_string_constant (s: string);
   begin
   end;

{$include TDummyPIC18xCPU_constructor_functions.inc}

procedure TDummyPIC18xCPU.generate_machine_code (prog: TProgram);
   begin
   end;

function TDummyPIC18xCPU.src_directory_relative_to_bin: string;
   begin
      result := '..\source\cpc_core'
   end;

procedure TDummyPIC18xCPU.set_mod_operator_implementation (impl: Tmod_operator_implementation);
   begin
      f_mod_operator_implementation := impl
   end;

INITIALIZATION
   target_cpu := TDummyPIC18xCPU.Create;

FINALIZATION
   target_cpu.Free

END.
