UNIT pic18x_blocks_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes,
   cpc_blocks_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit,
   pic18x_core_objects_unit,
   pic18x_cpu_unit,
   pic18x_instructions_unit,
   pic18x_macro_instructions_unit;

type
   TPIC18x_ParamList =
      class (TParamList)
         function Size: integer;
         procedure PushParameters (actual_parameters: TArrayOfTDefinition);
      end;

   TPIC18x_Program =
      class (TProgram, IAssignAddresses)
         initial_statement_label: TInstruction;
         initial_statement_stack_usage: integer;
         initial_statement_hw_stack_usage: integer;
         config_bits: TStructuredConstant;
         destructor Destroy;
            override;
         procedure GenerateCode (param2: integer);
            override;
         procedure AssignAddresses;
         procedure global_declarations_examination_hook;
            override;
      end;

   TPIC18x_Property =
      class (TProperty)
      end;

   TPIC18x_Routine =
      class (TRoutine, IAssignAddresses)
      private
         default_result_value: array of byte;
         procedure enum_bytes (b: byte; i: integer; path, value: string; initialization_unnecessary: boolean);
      public
         entry_point_label: TAssemblyLabel;
         stack_usage: integer;
         hw_stack_usage: integer;
         varlist_initialization: TDynamicByteArray;
         inline_code: TInstructionArray;     // only used for signaled function in interrupt variables
         procedure AssignAddresses;
         procedure GenerateCode (param2: integer);
            override;
         procedure PushDefaultResultValue;
         destructor Destroy;
            override;
      end;

   TPIC18x_SystemType =
      class (TSystemType, IAssignAddresses)
         init_stmt_entry_point_label: TInstruction;
         initial_stmt_stack_usage: integer;
         initial_statement_hw_stack_usage: integer;
         inline_code: TInstructionArray;     // only used for initial statement of interrupt variables
         procedure AssignAddresses;
         procedure GenerateCode (param2: integer);
            override;
         function process_stack_size: integer;
         function control_block_size: integer;
         function contains_eeprom_vars: boolean;
         destructor Destroy;
            override;
      protected
         procedure check_interrupt_routine_signature;
            override;
      end;

   TPIC18x_DataItemList =
      class (TDataItemList, IAssignAddresses)
      private
         variable_initial_value:  TInitialValueBytes;
         procedure append_initial_value_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
      public
         function Size: integer;  // in bytes
         procedure PushInitialValues;
         procedure EnumerateInitialValues (proc: TByteParamProcedureOfObject);
         procedure InitializeEEPROMValues (system_type_name: string);
         procedure AssignAddresses;
         procedure GenerateCode (param2: integer);
            override;
      end;

var
   prog: TPIC18x_Program;
   program_memory_used: integer;
   total_bank0_used: integer;
   sdram_used: integer;
   eeprom_used: integer;
   hw_stack_used: integer;
   current_block: TDefinition;

const
   ram_ptr_size    = 2;
   rom_ptr_size    = 2;
   pc_size         = 3;
   eeprom_ptr_size = 1;

function system_type_or_array_of_system_type (typedef: TTypeDef): boolean;

procedure NoteHWStackUsage (call_hw_stack_usage: integer);


IMPLEMENTATION

uses
   cpc_access_unit,
   cpc_common_unit,
   cpc_expressions_unit,
   cpc_statements_unit,
   cpc_target_cpu_unit,
   cpc_types_unit,
   pic18x_access_unit,
   pic18x_common_unit,
   pic18x_floating_point_unit,
   pic18x_kernel_unit,
   pic18x_microprocessor_information_unit,
   pic18x_run_time_error_check_unit,
   SysUtils;

var
   temp: TConstant;

function system_type_or_array_of_system_type (typedef: TTypeDef): boolean;
   begin
      result := (typedef.type_kind = system_type)
                or
                ((typedef.type_kind = array_type)
                 and
                 (TArrayType(typedef).element_typedef.type_kind = system_type)
                )
   end;

function TPIC18x_ParamList.Size: integer;
   var
      i: integer;
   begin
      result := 0;
      for i := 0 to Length-1 do
         case parameter_definitions[i].ParamMode of
            ByValue:
               result := result + TPIC18x_TypeInfo(parameter_definitions[i].TypeDef.info).Size;
            ByAddress:
               case parameter_definitions[i].descriptor of
                  rw_const,
                  rw_ioreg:
                     result := result + ram_ptr_size;
                  rw_var:
                     begin
                        result := result + ram_ptr_size;
                        if (parameter_definitions[i].TypeDef.type_kind = string_type)
                           and
                           (parameter_definitions[i].descriptor = rw_var) then
                           result := result + 1
                     end;
                  rw_eeprom:
                     begin
                        result := result + eeprom_ptr_size;
                        if (parameter_definitions[i].TypeDef.type_kind = string_type)
                           and
                           (parameter_definitions[i].descriptor = rw_eeprom) then
                           result := result + 1
                     end;
                  rw_rom:
                     result := result + rom_ptr_size;
               else
                  assert (false)
               end;
         else
            assert (false)
         end
   end;

procedure TPIC18x_ParamList.PushParameters (actual_parameters: TArrayOfTDefinition);
   var
      i, j, expression_result_size: integer;
      pic_real: TPIC18x_Real;
      annotation: string;
   begin
      for i := 0 to Length-1 do
         case parameter_definitions[i].ParamMode of
            ByValue:
               if (parameter_definitions[i].TypeDef.type_kind = basic_data_type)
                  and
                  (TBasicDataType(parameter_definitions[i].TypeDef).basic_data_type_kind = floating_point_data_type)
               then  // parameter is real
                  if TExpression(actual_parameters[i]).contains_constant then
                     begin  // parameter is real constant
                        case TConstantPrimary(TExpression(actual_parameters[i])).the_constant.definition_kind of
                           constant_definition:
                              pic_real.r := TConstant(TConstantPrimary(TExpression(actual_parameters[i])).the_constant).r;
                           structured_constant_definition:
                              pic_real.r := TStructuredConstant(TConstantPrimary(TExpression(actual_parameters[i])).the_constant).simple_constant.r;
                        else
                           assert (false)
                        end;
                        annotation := 'push ' + FloatToStr (pic_real.r);
                        if parameter_definitions[i].TypeDef = target_cpu.get_supported_data_type ('real') then
                           for j := 0 to 3 do
                              begin
                                 TPIC18x_PUSHL.Create (pic_real.pic_real_AsByte[j]).annotation := annotation;
                                 annotation := ''
                              end
                        else if parameter_definitions[i].TypeDef = target_cpu.get_supported_data_type (ieee_single_type_name) then
                           for j := 0 to 3 do
                              begin
                                 TPIC18x_PUSHL.Create (pic_real.ieee_single_AsByte[j]).annotation := annotation;
                                 annotation := ''
                              end
                        else
                           assert (false);
                        StackUsageCounter.Push (4)
                     end
                  else   // parameter is real non-constant expression
                     case TExpression(actual_parameters[i]).expression_kind of
                        integer_expression:
                           begin
                              generate_integer_expression_to_real_code (TExpression(actual_parameters[i]));
                              if parameter_definitions[i].typedef = target_cpu.get_supported_data_type (ieee_single_type_name) then
                                 convert_tos_from_pic_to_ieee
                           end;
                        real_expression:
                           begin
                              TExpression(actual_parameters[i]).GenerateCode (4);
                              if (parameter_definitions[i].typedef = target_cpu.get_supported_data_type (ieee_single_type_name))
                                 and
                                 (not TPIC18x_Expression_TypeInfo(TExpression(actual_parameters[i]).info).is_ieee_single) then
                                 convert_tos_from_pic_to_ieee
                              else if (parameter_definitions[i].typedef = target_cpu.get_supported_data_type ('real'))
                                      and
                                      (TPIC18x_Expression_TypeInfo(TExpression(actual_parameters[i]).info).is_ieee_single) then
                                 convert_tos_from_ieee_to_pic
                           end;
                     else
                        assert (false)
                     end
               else  // parameter is unreal
                  begin
                     expression_result_size := TPIC18x_TypeInfo(parameter_definitions[i].TypeDef.info).Size;
                     TExpression(actual_parameters[i]).GenerateCode (expression_result_size);
                     if TExpression(actual_parameters[i]).expression_kind in ordinal_expression_kinds then
                        GenerateRangeCheckCode (TOrdinalDataType(parameter_definitions[i].typedef),
                                                expression_result_size,
                                                TExpression(actual_parameters[i]).info,
                                                TExpression(actual_parameters[i]).src_loc,
                                                'parameter value is of outside legal range'
                                               )
                  end;
            ByAddress:
               if (parameter_definitions[i].TypeDef.type_kind = string_type)
                  and
                  (parameter_definitions[i].descriptor = rw_var)
               then
                  TPIC18x_Access(actual_parameters[i]).Generate_Push_Address2_Code (0, true)
               else if parameter_definitions[i].is_ioreg_1bit_param then
                  case TPIC18x_Access(actual_parameters[i]).base_variable.address_mode of
                     absolute_address_mode:
                        begin
                           assert (TPIC18x_Access(actual_parameters[i]).node_is_packed_field);
                           TPIC18x_Access(actual_parameters[i]).Generate_Push_ioreg_1_bit_address
                        end;
                     system_type_indirect_address_mode,
                     local_indirect_address_mode:
                        TPIC18x_Access(actual_parameters[i]).Generate_Push_Address2_Code (0, false);  // addr already has bit#, just copy it to TOS
                  else
                     assert (false)
                  end
               else
                  if (parameter_definitions[i].TypeDef.type_kind = string_type)
                     and
                     (parameter_definitions[i].descriptor = rw_eeprom)
                  then
                     TPIC18x_Access(actual_parameters[i]).Generate_Push_Address1_Code (0, true)
               else
                  if parameter_definitions[i].descriptor = rw_eeprom then
                     TPIC18x_Access(actual_parameters[i]).Generate_Push_Address1_Code (0, false)
               else
                  TPIC18x_Access(actual_parameters[i]).Generate_Push_Address2_Code (0, false);
         else
            assert (false)
         end
   end;

destructor TPIC18x_Program.Destroy;
   begin
      config_bits.Release;
      inherited
   end;

procedure TPIC18x_Program.AssignAddresses;
   var
      i: integer;
      typ: TTypeDef;
   begin
      // assign simple types first so as to be more likely to be in bank0
      for i := 0 to program_vars.Length-1 do
         if not system_type_or_array_of_system_type (program_vars[i].TypeDef) then
            begin
               program_vars[i].address := sdram_used;
               sdram_used := sdram_used + TPIC18x_TypeInfo(program_vars[i].typedef.info).Size
            end;
      total_bank0_used := sdram_used;
      // assign system and array type addresses
      for i := 0 to program_vars.Length-1 do
         if system_type_or_array_of_system_type (program_vars[i].TypeDef) then
            begin
               typ := program_vars[i].TypeDef;
               if typ.type_kind = array_type then
                  typ := TArrayType(typ).element_typedef;
               assert (typ.type_kind = system_type);
               if TSystemType(typ).system_type_kind <> interrupt_system_type then
                  begin
                     if TPIC18x_SystemType(typ).contains_eeprom_vars then
                        program_vars[i].address := sdram_used + $3F
                     else
                        program_vars[i].address := sdram_used + $3E;
                     sdram_used := sdram_used + TPIC18x_TypeInfo(program_vars[i].typedef.info).Size;
                  end
            end;
      // assign process stack addresses
      for i := 0 to program_vars.Length-1 do
         if (program_vars[i].typedef.type_kind = system_type)
            and
            (TSystemType(program_vars[i].typedef).system_type_kind = process_system_type)
         then
            begin
               TPIC18x_Variable(program_vars[i]).stack_address := sdram_used;
               sdram_used := sdram_used + TPIC18x_SystemType(program_vars[i].typedef).process_stack_size
            end
   end;

procedure TPIC18x_Program.GenerateCode (param2: integer);
   var
      i: integer;
   begin
      current_block := self;
      TSourceSyncPoint.Create (last_var_declaration_src_loc);
      initial_statement_label := TAssemblyLabel.Create;
      StackUsageCounter.Clear;
      TSourceSyncPoint.Create (begin_src_loc);
      initial_statement.GenerateCode (1);
      TSourceSyncPoint.Create (end_src_loc);
      TAssemblySourceBlankLine.Create;
      // initialize any reachable uninitialized interrupt variables
      for i := 0 to program_vars.Length-1 do
         if (program_vars[i].typedef.IsInterruptType)
            and
            (program_vars[i].reachable)
            and
            (not program_vars[i].init_statement_called)
         then
            begin
               TSourceLine.Create ('init ' + program_vars[i].name + ';');
               ProgramCode.AppendInlineCode (TPIC18x_SystemType(program_vars[i].typedef).inline_code);
               StackUsageCounter.PushPop (TPIC18x_SystemType(program_vars[i].typedef).initial_stmt_stack_usage);
            end;
      GenerateKernelStartupCode;
      GenerateInterruptsOffCodeSegmentsAsSubroutines;
      assert (StackUsageCounter.Current = 0);
      initial_statement_stack_usage := StackUsageCounter.Max;
      current_block := nil
   end;

procedure TPIC18x_Program.global_declarations_examination_hook;
   var
      deft, defc: TDefinition;
   begin
      // look to see if config bits constant has been defined
      if pic_info.microprocessor = '*unspecified*' then
         exit;

      deft := CurrentDefinitionTable.GetDefinitionForIdentifier (config_bits_constant_type_name(pic_info.microprocessor), true);
      if deft.definition_kind <> type_definition then
         exit;

      defc := nil;
      try
         defc := CurrentDefinitionTable.GetDefinitionForIdentifier (config_bits_constant_name(pic_info.microprocessor), false)
      except
         on e:compile_error do
            if e.Message <> err_undefined_identifier then
               raise
      end;
      if (defc = nil)
         or
         (defc.definition_kind <> structured_constant_definition)
      then
         exit;

      if TStructuredConstant(defc).typedef <> deft then
         exit;

      config_bits := TStructuredConstant(defc);
      config_bits.AddRef
   end;

procedure TPIC18x_Routine.AssignAddresses;
   var
      addr, i: integer;
   begin
      addr := 1 - TPIC18x_DataItemList(local_vars).Size;

      for i := 0 to local_vars.Length-1 do
         begin
            local_vars[i].address := addr;
            addr := addr + TPIC18x_TypeInfo(local_vars[i].TypeDef.info).Size
         end;

      if parameter_definitions <> nil then
         for i := parameter_definitions.Length-1 downto 0 do
            begin
               if (parameter_definitions[i].descriptor in [rw_var, rw_eeprom])
                  and
                  (parameter_definitions[i].typedef.type_kind = string_type)
               then
                  addr := addr + 1;   // alloc space for hidden size parameter
               parameter_definitions[i].address := addr;
               case parameter_definitions[i].descriptor of
                  rw_const,
                  rw_for:
                     case parameter_definitions[i].ParamMode of
                        ByValue:
                           addr := addr + TPIC18x_TypeInfo(parameter_definitions[i].TypeDef.info).Size;
                        ByAddress:
                           addr := addr + ram_ptr_size;
                     else
                        assert (false)
                     end;
                  rw_ioreg:
                     addr := addr + ram_ptr_size;
                  rw_var:
                     addr := addr + ram_ptr_size;
                  rw_eeprom:
                     addr := addr + eeprom_ptr_size;
                  rw_rom:
                     addr := addr + rom_ptr_size;
               else
                  assert (false)
               end
            end;

      if entry then
         begin
            if TSystemType(context).system_type_kind = monitor_system_type then
               addr := addr + 1;            // prio
            addr := addr + ram_ptr_size     // this ptr
         end;

      case context.definition_kind of
         program_definition:
            addr := addr + pc_size;               // return address
         type_definition:
            if TSystemType(context).system_type_kind <> interrupt_system_type then
               addr := addr + pc_size;               // return address
      else
         assert (false)
      end;

      if function_result <> nil then
         begin
            if function_result.typedef.type_kind = string_type then
               addr := addr + 1;
            function_result.address := addr
         end
   end;

procedure TPIC18x_Routine.GenerateCode (param2: integer);
   var
      locals_size, param_size, i: integer;
   begin
      TSourceSyncPoint.Create (block_header_end_src_loc);
      current_block := self;
      StackUsageCounter.Clear;

      if (context.definition_kind = type_definition)
         and
         (TSystemType(context).system_type_kind = interrupt_system_type)
      then
         ProgramCode.StartRecordingInlineCode (inline_code)
      else
         entry_point_label := TAssemblyLabel.Create;

      if (context.definition_kind = type_definition)
         and
         (TSystemType(context).system_type_kind = monitor_system_type)
         and
         entry
      then
         EnterMonitor (TSystemType(context).priority);

      TSourceSyncPoint.Create (last_var_declaration_src_loc);
      locals_size := TPIC18x_DataItemList(local_vars).Size;
      if parameter_definitions = nil then
         param_size := 0
      else
         param_size := TPIC18x_ParamList(parameter_definitions).Size;
      TPIC18x_DataItemList(local_vars).PushInitialValues;

      TSourceSyncPoint.Create (block_begin_src_loc);

      statement_list.GenerateCode (0);

      TSourceSyncPoint.Create (block_end_src_loc);

      if locals_size > 0 then
         if param_size = 0 then
            adjust_fsr (pFSR2, locals_size).annotation := 'pop local vars'
         else
            adjust_fsr (pFSR2, locals_size + param_size).annotation := 'pop local vars and parameters'
      else
         if param_size > 0 then
            adjust_fsr (pFSR2, param_size).annotation := 'pop parameters';

      if (context.definition_kind = type_definition)
         and
         (TSystemType(context).system_type_kind = monitor_system_type)
         and
         entry
      then
         begin
            i := Length(TStatementList(statement_list).stmts);
            if (i = 0)
               or
               (TStatementList(statement_list).stmts[i-1].statement_kind <> continue_statement)
            then
               LeaveMonitor (TSystemType(context).priority, locals_size + param_size)
         end
      else
         begin
            if entry then
               begin
                  TPIC18x_MOVFF.Create (PREINC2, this_ptrH).annotation := 'restore caller''s this pointer';
                  TPIC18x_MOVFF.Create (PREINC2, this_ptrL)
               end;

            if (context.definition_kind = type_definition)
               and
               (TSystemType(context).system_type_kind = interrupt_system_type)
            then
               ProgramCode.StopRecordingInlineCode
            else
               begin
                  // pop return address into program counter & return
                  TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := 'pop return address';
                  TPIC18x_MOVWF.Create (PCLATU, access_mode);
                  TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (PCLATH, access_mode);
                  TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (PCL, access_mode)
               end
         end;

      stack_usage := StackUsageCounter.Max;
      current_block := nil;

      GenerateInterruptsOffCodeSegmentsAsSubroutines
   end;

procedure TPIC18x_Routine.enum_bytes (b: byte; i: integer; path, value: string; initialization_unnecessary: boolean);
   var idx: integer;
   begin
      idx := Length(default_result_value);
      SetLength (default_result_value, idx+1);
      default_result_value[idx] := b
   end;

procedure TPIC18x_Routine.PushDefaultResultValue;
   var
      annotation: string;
      i: integer;
   begin
      SetLength (default_result_value, 0);
      if function_result.initial_value = nil then
         TPIC18x_TypeDef_TypeInfo(function_result.typedef.info).enumerate_constant_bytes
            ('',
             function_result.typedef.info.DefaultValue,
             enum_bytes
            )
      else
         TPIC18x_TypeDef_TypeInfo(function_result.typedef.info).enumerate_constant_bytes
            ('',
             function_result.initial_value,
             enum_bytes
            );
      annotation := 'initialize result';
      for i := System.Length(default_result_value)-1 downto 0 do
         begin
            TPIC18x_PUSHL.Create (default_result_value[i]).annotation := annotation;
            annotation := ''
         end;
      StackUsageCounter.Push (Length(default_result_value))
   end;

destructor TPIC18x_Routine.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(inline_code)-1
         do inline_code[i].Release;
      inherited
   end;

procedure TPIC18x_SystemType.AssignAddresses;
   var
      addr, i: integer;
      typ: TTypeDef;
   begin
      // Note: AssignAddresses must yield results compatible with the enumeration of
      //    initial values for RAM in TPIC18x_CPU.generate_machine_code.

      // assign RAM addresses
      if system_type_kind = interrupt_system_type then
         begin
            for i := 0 to permanent_ram_vars.Length-1 do
               begin
                  if system_type_or_array_of_system_type (permanent_ram_vars[i].TypeDef) then
                     begin
                        typ := permanent_ram_vars[i].TypeDef;
                        if typ.type_kind = array_type then
                           typ := TArrayType(typ).element_typedef;
                        assert (typ.type_kind = system_type);
                        if TPIC18x_SystemType(typ).contains_eeprom_vars then
                           permanent_ram_vars[i].address := sdram_used + $3F
                        else
                           permanent_ram_vars[i].address := sdram_used + $3E
                     end
                  else
                     permanent_ram_vars[i].address := sdram_used;
                  sdram_used := sdram_used + TPIC18x_TypeInfo(permanent_ram_vars[i].TypeDef.info).Size
               end;
         end
      else
         begin
            addr := -$3E + control_block_size;

            for i := parameters.Length-1 downto 0 do
               begin
                  parameters[i].address := addr;
                  case parameters[i].descriptor of
                     rw_const,
                     rw_for:
                        case parameters[i].ParamMode of
                           ByValue:
                              addr := addr + TPIC18x_TypeInfo(parameters[i].TypeDef.info).Size;
                           ByAddress:
                              addr := addr + ram_ptr_size;
                        else
                           assert (false)
                        end;
                     rw_ioreg,
                     rw_var:
                        addr := addr + ram_ptr_size;
                     rw_eeprom:
                        addr := addr + eeprom_ptr_size;
                     rw_rom:
                        addr := addr + rom_ptr_size;
                  else
                     assert (false)
                  end
               end;

            for i := 0 to permanent_ram_vars.Length-1 do
               begin
                  if system_type_or_array_of_system_type (permanent_ram_vars[i].TypeDef) then
                     begin
                        typ := permanent_ram_vars[i].TypeDef;
                        if typ.type_kind = array_type then
                           typ := TArrayType(typ).element_typedef;
                        assert (typ.type_kind = system_type);
                        if TPIC18x_SystemType(typ).contains_eeprom_vars then
                           permanent_ram_vars[i].address := addr + $3F
                        else
                           permanent_ram_vars[i].address := addr + $3E
                     end
                  else
                     permanent_ram_vars[i].address := addr;
                  addr := addr + TPIC18x_TypeInfo(permanent_ram_vars[i].TypeDef.info).Size
               end;

            // assign eeprom addresses
            addr := 0;
            for i := 0 to permanent_eeprom_vars.Length-1 do
               begin
                  permanent_eeprom_vars[i].address := addr;
                  addr := addr + TPIC18x_TypeInfo(permanent_eeprom_vars[i].TypeDef.info).Size
               end
         end
   end;

procedure TPIC18x_SystemType.GenerateCode (param2: integer);
   var
      lbl: TInstruction;
   begin
      // inital statement
      current_block := self;
      TSourceSyncPoint.Create (block_begin_src_loc);

      if system_type_kind = interrupt_system_type then
         ProgramCode.StartRecordingInlineCode (inline_code)
      else
         init_stmt_entry_point_label := TAssemblyLabel.Create;

      if (system_type_kind <> process_system_type)
         and
         (parameters.Length > 0) then
         begin
            TPIC18x_MOVFF.Create (this_ptrL, FSR1L).annotation := 'copy parameters';
            TPIC18x_MOVFF.Create (this_ptrH, FSR1H);
            adjust_fsr (pFSR1, -$3E + control_block_size);
            TPIC18x_MOVLW.Create (TPIC18x_ParamList(parameters).Size);
            lbl := TPIC18x_MOVFF.Create (PREINC2, POSTINC1);
            TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
            TPIC18x_BRA.Create.dest := lbl
         end;

      StackUsageCounter.Clear;
      initial_statement.GenerateCode (0);
      assert (StackUsageCounter.current = 0);
      initial_stmt_stack_usage := StackUsageCounter.max;

      TSourceSyncPoint.Create (block_end_src_loc);

      case system_type_kind of
         interrupt_system_type:
            ProgramCode.StopRecordingInlineCode;
         process_system_type:
            { will suspend then return via kernel dispatch };
         class_system_type,
         monitor_system_type:
            begin
               TPIC18x_MOVFF.Create (PREINC2, this_ptrH).annotation := 'restore caller''s this pointer';
               TPIC18x_MOVFF.Create (PREINC2, this_ptrL);

               // pop return address into program counter & return
               TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := 'pop return address';
               TPIC18x_MOVWF.Create (PCLATU, access_mode);
               TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
               TPIC18x_MOVWF.Create (PCLATH, access_mode);
               TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
               TPIC18x_MOVWF.Create (PCL, access_mode)
            end;
      else
         assert (false)
      end;

      current_block := nil;

      GenerateInterruptsOffCodeSegmentsAsSubroutines
   end;

function TPIC18x_SystemType.control_block_size: integer;
   begin
      result := 0;  // suppress compiler warning
      case system_type_kind of
         class_system_type:
            result := 0;
         monitor_system_type:
            result := 1;
         process_system_type:
            result := 0;
         interrupt_system_type:
            result := 0;
      else
         assert (false)
      end
   end;

function TPIC18x_SystemType.contains_eeprom_vars: boolean;
   begin
      result := permanent_eeprom_vars.Length > 0
   end;

function TPIC18x_SystemType.process_stack_size: integer;
   begin
      assert (system_type_kind = process_system_type);
      result := initial_stmt_stack_usage;
      if PriorityMapper.Interruptable(priority) then
         result := result + kernel_interrupt_handler_stack_allowance
   end;

procedure TPIC18x_SystemType.check_interrupt_routine_signature;
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
         (LowerCase (lex.identifiers[lex.token.identifier_idx]) <> 'signaled') then
         raise compile_error.Create (err_signaled_function_required);
      lex.advance_token;
      if not lex.token_is_symbol (sym_colon) then
         raise compile_error.Create (err_colon_expected);
      lex.advance_token;
      if LowerCase (lex.identifiers[lex.token.identifier_idx]) <> 'boolean' then
         raise compile_error.Create (err_signaled_function_result_must_be_boolean);
      lex.backup (mark)
   end;

destructor TPIC18x_SystemType.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(inline_code)-1 do
         inline_code[i].Release;
      inherited
   end;

procedure TPIC18x_DataItemList.AssignAddresses;
   begin
   end;

procedure TPIC18x_DataItemList.GenerateCode (param2: integer);
   var
      i: integer;
   begin
      if descriptor = rw_rom then
         for i := 0 to Length-1 do
            if Self[i].initial_value = nil then
               TPIC18x_Data_Initialization.Create ('initialize ' + Self[i].name,
                                                   TPIC18x_TypeInfo(Self[i].typedef.info).DefaultValue,
                                                   Self[i].decl_end_src_loc
                                                  )
            else
               TPIC18x_Data_Initialization.Create ('initialize ' + Self[i].name,
                                                   Self[i].initial_value,
                                                   Self[i].decl_end_src_loc
                                                  )
      else if descriptor = rw_eeprom then
         for i := 0 to Length-1 do
            if Self[i].initial_value = nil then
               TPIC18x_Data_Initialization.Create ('initialize ' + Self[i].name,
                                                   TPIC18x_TypeInfo(Self[i].typedef.info).DefaultValue,
                                                   Self[i].decl_end_src_loc
                                                  )
            else
               TPIC18x_Data_Initialization.Create ('initialize ' + Self[i].name,
                                                   Self[i].initial_value,
                                                   Self[i].decl_end_src_loc
                                                  )
   end;

procedure TPIC18x_DataItemList.EnumerateInitialValues (proc: TByteParamProcedureOfObject);
   var
      i: integer;
   begin
      for i := 0 to Length-1 do
         if Self[i].initial_value = nil then
            TPIC18x_TypeDef_TypeInfo(Self[i].TypeDef.info).enumerate_constant_bytes
               ('',
                Self[i].typedef.info.DefaultValue,
                proc
               )
         else
            TPIC18x_TypeDef_TypeInfo(Self[i].TypeDef.info).enumerate_constant_bytes
               ('',
                Self[i].initial_value,
                proc
               )
   end;

procedure TPIC18x_DataItemList.InitializeEEPROMValues (system_type_name: string);
   var i: integer;
   begin
      for i := 0 to Length-1 do
         if Self[i].initial_value = nil then
            TPIC18x_EEPROM_Data.Create ('initialize ' + system_type_name + '.' + Self[i].name, TPIC18x_TypeInfo(Self[i].typedef.info).DefaultValue, Self[i].decl_end_src_loc)
         else
            TPIC18x_EEPROM_Data.Create ('initialize ' + system_type_name + '.' + Self[i].name, Self[i].initial_value, Self[i].decl_end_src_loc)
   end;

function TPIC18x_DataItemList.Size: integer;
   var i: integer;
   begin
      result := 0;
      for i := 0 to Length-1 do
         result := result + TPIC18x_TypeDef_TypeInfo(Self[i].TypeDef.info).Size
   end;

procedure TPIC18x_DataItemList.append_initial_value_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
   var i: integer;
   begin
      i := System.Length (variable_initial_value);
      SetLength (variable_initial_value, i+1);
      variable_initial_value[i].b := b;
      variable_initial_value[i].byte_no := byte_no;
      variable_initial_value[i].path := path;
      variable_initial_value[i].value := value;
      variable_initial_value[i].initialization_unnecessary := initialization_unnecessary
   end;

procedure TPIC18x_DataItemList.PushInitialValues;
   var
      var_idx: integer;
      byte_idx: integer;
      skip_count: integer;
   begin
      for var_idx := Length-1 downto 0 do
         begin
            SetLength (variable_initial_value, 0);
            if Self[var_idx].initial_value = nil then
               TPIC18x_TypeDef_TypeInfo(Self[var_idx].TypeDef.info).enumerate_constant_bytes
                  ('',
                   Self[var_idx].typedef.info.DefaultValue,
                   append_initial_value_byte
                  )
            else
               TPIC18x_TypeDef_TypeInfo(Self[var_idx].TypeDef.info).enumerate_constant_bytes
                  ('',
                   Self[var_idx].initial_value,
                   append_initial_value_byte
                  );
            byte_idx := System.Length(variable_initial_value)-1;
            TAssemblyComment.Create ('initialize ' + Self[var_idx].name + variable_initial_value[byte_idx].path + ' := ' + variable_initial_value[byte_idx].value);
            while byte_idx >= 0 do
               if variable_initial_value[byte_idx].initialization_unnecessary then
                  begin
                     skip_count := 0;
                     repeat
                        skip_count := skip_count + 1;
                        byte_idx := byte_idx - 1
                     until not variable_initial_value[byte_idx].initialization_unnecessary;
                     adjust_fsr (pFSR2, -skip_count)
                  end
               else
                  begin
                     TPIC18x_PUSHL.Create (variable_initial_value[byte_idx].b);
                     byte_idx := byte_idx - 1
                  end
         end;
      SetLength (variable_initial_value, 0);
      StackUsageCounter.Push (Size)
   end;

procedure NoteHWStackUsage (call_hw_stack_usage: integer);
   procedure set_max (var hw_stack_usage: integer);
      begin
         if call_hw_stack_usage > hw_stack_usage then
            hw_stack_usage := call_hw_stack_usage
      end;
   begin
      if current_block <> nil then
         case current_block.definition_kind of
            routine_definition:
               set_max (TPIC18x_Routine(current_block).hw_stack_usage);
            type_definition:
               set_max (TPIC18x_SystemType(current_block).initial_statement_hw_stack_usage);
            program_definition:
               set_max (TPIC18x_Program(current_block).initial_statement_hw_stack_usage);
         else
            assert (false)
         end
   end;


INITIALIZATION
   temp := TConstant.Create;

FINALIZATION
   temp.Release;

END.
