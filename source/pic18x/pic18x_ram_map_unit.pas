UNIT pic18x_ram_map_unit;

INTERFACE

uses
   cpc_definitions_unit, pic18x_instructions_unit;

type
   TPIC18x_CallRecord =
      class (TCallRecord)
         caller_relative_stk_ptr_at_call: integer;
         function fmt_src_loc: string;
         procedure traverse_call_tree (call_path: string; stk: integer);
      end;

procedure generate_memory_map;

IMPLEMENTATION

uses
   cpc_core_objects_unit, pic18x_kernel_unit, SysUtils, pic18x_blocks_unit,
   cpc_types_unit, pic18x_core_objects_unit, cpc_blocks_unit, pic18x_types_unit,
   cpc_source_analysis_unit, pic18x_macro_instructions_unit, pic18x_access_unit,
   cpc_access_unit, wirth_balanced_binary_tree_unit, dijkstra_smoothsort_unit, 
  Math;

const
   max_mpasm_label_len = 32;
   reserved_format_string = '%-32s RES .%4.4d';
   reserved_format_string_with_var_and_type = reserved_format_string + ' ;   %s: %s;';
var
   udata_section: integer;

function current_udata_section_label: string;      
   begin
      result := format ('M%6.6d', [udata_section])
   end;

procedure output_udata_line (start_addr: integer);
   begin
      TAssemblySourceLine.Create (format ('                                 UDATA 0x%3.3X', [start_addr]))
   end;


//=====================
// tRAMVariableMapNode
//=====================

type
   tRAMVariableGroup = (nosuch_group, kernel_vars_group, process_pcbs_group, kernel_pcb_group, vars_group);
   tRAMVariableMapNode =
      class
      private
         size: integer;      // only used if typedef is nil
         ParentNode: tRAMVariableMapNode;
         SubNodes: array of tRAMVariableMapNode;
         ram_variable_group: tRAMVariableGroup;
         function label_is_too_long (expanded_short_name: string): boolean;
         procedure shorten_labels_as_needed (expanded_short_name: string);
         function separator: char;
      public
         id: string;
         shortened_id: string;
         typedef: TTypedef;  // might be nil!
         typename: string;
         is_array_index: boolean;
         array_index: integer;
         constructor Create (_id: string = ''; _typename: string = ''; _size: integer = 0; _group: tRAMVariableGroup = nosuch_group);
            overload;
         constructor Create (_id: string; _typedef: TTypeDef; _ram_variable_group: tRAMVariableGroup = vars_group);
            overload;
         constructor CreateLocalRoot;
         constructor CreateArrIndex (_id: string; _typedef: TTypeDef; idx: integer);
         procedure append (new_sub_node: tRAMVariableMapNode);
         procedure output_reserved (_phase: tRAMVariableGroup; expanded_name, expanded_short_name: string);
         destructor Destroy;
            override;
      end;

function process_pcb_map (group: tRAMVariableGroup = process_pcbs_group): tRAMVariableMapNode;
   begin
      result := tRAMVariableMapNode.Create ('PCB');
      result.append (tRAMVariableMapNode.Create ('next', '^pcb', 1, group));
      result.append (tRAMVariableMapNode.Create ('fsr2_save', '2 bytes', 2, group));
      result.append (tRAMVariableMapNode.Create ('nesting', 'uint8', 1, group))
   end;

constructor tRAMVariableMapNode.Create (_id: string = ''; _typename: string = ''; _size: integer = 0; _group: tRAMVariableGroup = nosuch_group);
   begin
      id := _id;
      shortened_id := _id;
      typename := _typename;
      size := _size;
      ram_variable_group := _group
   end;

constructor tRAMVariableMapNode.CreateLocalRoot;
   begin
      shortened_id := current_udata_section_label
   end;

constructor tRAMVariableMapNode.CreateArrIndex (_id: string; _typedef: TTypeDef; idx: integer);
   begin
      Create (_id, _typedef);
      is_array_index := true;
      array_index := idx
   end;

constructor tRAMVariableMapNode.Create (_id: string; _typedef: TTypeDef; _ram_variable_group: tRAMVariableGroup = vars_group);
   var
      i: integer;
      st: TPIC18x_SystemType;
      arr: TArrayType;
      node: tRAMVariableMapNode;
      first_idx, last_idx: integer;
      fmt: string;
      ovf_typedef: TOverlayType;
      reversed: boolean;
pfi: TPIC18x_PackedRecordFieldInfo;
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
                  append (tRAMVariableMapNode.Create (LowerCase(st.permanent_ram_vars[i].name), st.permanent_ram_vars[i].typedef, ram_variable_group))
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
            if TPIC18x_PackedRecordFieldInfo(TPackedRecordType(typedef).fields[0].info).reversed_byte_order then
               typename := typename + ' {REVERSED BYTE ORDER}';
         overlay_type:
            begin
               ovf_typedef := TOverlayType(typedef);
               reversed := true;
               for i := 0 to Length(ovf_typedef.overlaid_variables)-1 do
                  if ovf_typedef.overlaid_variables[i].typedef.type_kind = packed_record_type then
                     reversed := reversed and TPIC18x_PackedRecordFieldInfo(TPackedRecordType(ovf_typedef.overlaid_variables[i].typedef).fields[0].info).reversed_byte_order
                  else
                     reversed := false;   // only a reversed type if ALL overlaid variables are packed records
               if reversed then
                  typename := typename + ' {REVERSED BYTE ORDER}';
            end;
         string_type:
            begin
               append (tRAMVariableMapNode.Create ('LEN', 'uint8', 1, ram_variable_group));
               if size <= 10 then
                  fmt := '%d_%d'
               else if size <= 17 then
                  fmt := '%d_%2.2d'
               else if size <= 100 then
                  fmt := '%2.2d_%2.2d'
               else if size <= 105 then
                  fmt := '%2.2d_%3.3d'
               else
                  fmt := '%3.3d_%3.3d';
               i := 1;
               while i <= size-1 do
                  begin
                     first_idx := i;
                     last_idx := min (i+7, size-1);
                     node := tRAMVariableMapNode.Create ('', 'array [' + IntToStr(first_idx) + '..' + IntToStr(last_idx) + '] of char', last_idx-first_idx+1, ram_variable_group);
                     node.shortened_id := format (fmt, [first_idx, last_idx]);
                     append (node);
                     i := i + 8
                  end
            end;
         basic_data_type,
         set_type,
         queue_type:
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
         if (id = '')
            and
            (Length(expanded_name) > 0)
            and
            (expanded_name[Length(expanded_name)] = '.')
         then
            SetLength (expanded_name, Length(expanded_name)-1)
         else
            expanded_name := expanded_name + id;
      expanded_short_name := expanded_short_name + shortened_id;
      if (Length(SubNodes) = 0)
         and
         (ram_variable_group = _phase)
         and
         (size > 0)
      then
         TAssemblySourceLine.Create (format (reserved_format_string_with_var_and_type, [expanded_short_name, size, expanded_name, typename]));
      if id = '' then
         if expanded_short_name = '' then
            for i := 0 to Length(SubNodes)-1 do
               SubNodes[i].output_reserved (_phase, '', '')
         else
            for i := 0 to Length(SubNodes)-1 do
               SubNodes[i].output_reserved (_phase, '', expanded_short_name + '?')
      else
         for i := 0 to Length(SubNodes)-1 do
            SubNodes[i].output_reserved (_phase, expanded_name + separator, expanded_short_name + '?')
   end;

function tRAMVariableMapNode.label_is_too_long (expanded_short_name: string): boolean;
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
               result := result or SubNodes[i].label_is_too_long ('')
         else
            for i := 0 to Length(SubNodes)-1 do
               result := result or SubNodes[i].label_is_too_long (expanded_short_name + '?')
   end;

procedure tRAMVariableMapNode.shorten_labels_as_needed (expanded_short_name: string);
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
                  EXIT
            end;
         SetLength (result, 2);
         for c1 := 'a' to 'z' do
            begin
               result[1] := c1;
               for c2 := '0' to '9' do
                  begin
                     result[2] := c2;
                     if no_conflict (result) then
                        EXIT
                  end;
               for c2 := 'a' to 'z' do
                  begin
                     result[2] := c2;
                     if no_conflict (result) then
                        EXIT
                  end;
            end;
         assert (false)  // no substitute found!
      end;
   begin
      expanded_short_name := expanded_short_name + shortened_id;
      for idx := 0 to Length(SubNodes)-1 do
         if ((id = '') and (SubNodes[idx].label_is_too_long ('')))
            or
            ((id <> '') and (SubNodes[idx].label_is_too_long (expanded_short_name + '?')))
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
                  SubNodes[idx].shorten_labels_as_needed ('')
               else
                  SubNodes[idx].shorten_labels_as_needed (expanded_short_name + '?')
            end
   end;

destructor tRAMVariableMapNode.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(SubNodes)-1 do
         SubNodes[i].Free;
      inherited
   end;

function kernel_variables_map: tRAMVariableMapNode;

   function current_prio_typedef: string;
      var i: integer;
      begin
         if Length(PriorityMapper.priorities) = 1 then
            result := format ('(init {encoded $%2.2x})', [PriorityMapper.priorities[0].ready_addr])
         else
            result := format ('(init {encoded $%2.2x}, ', [PriorityMapper.priorities[0].ready_addr]);
         for i := Low(PriorityMapper.priorities)+1 to High(PriorityMapper.priorities) do
            if i < High(PriorityMapper.priorities) then
               result := result + format ('%d {$%2.2x}, ', [PriorityMapper.priorities[i].prio, PriorityMapper.priorities[i].ready_addr])
            else
               result := result + format ('%d {$%2.2x})', [PriorityMapper.priorities[i].prio, PriorityMapper.priorities[i].ready_addr]);
      end;

   function ready_element (idx: integer): tRAMVariableMapNode;
      var
         save: tRAMVariableMapNode;
         sfr_idx: TProcessStateSFRs;
      begin
         result := tRAMVariableMapNode.Create (format ('ready_%2.2X', [PriorityMapper.priorities[idx].ready_addr]));
         if idx = 0 then
            result.id := 'ready[init]'
         else
            result.id := 'ready[' + IntToStr(PriorityMapper.priorities[idx].prio) + ']';
         result.append (tRAMVariableMapNode.Create ('queue', '^pcb', 1, kernel_vars_group));
         result.append (tRAMVariableMapNode.Create ('state', 'process_state', 1, kernel_vars_group));
         if PriorityMapper.Interruptable (PriorityMapper.priorities[idx].prio) then
            begin
               save := tRAMVariableMapNode.Create ('save');
               for sfr_idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
                  save.append (tRAMVariableMapNode.Create (ProcessStateSFRAddress[sfr_idx].name, 'byte', 1, kernel_vars_group));
               result.append (save)
            end
      end;

   var
      i: integer;
      flags: tRAMVariableMapNode;
   begin  // kernel_variables_map
      result := tRAMVariableMapNode.Create ('KERNEL');
      result.append (tRAMVariableMapNode.Create ('current_prio', current_prio_typedef, 1, kernel_vars_group));
      flags := tRAMVariableMapNode.Create ('flags', '', 1, kernel_vars_group);
      flags.id := 'system_initializing';
      flags.typename := 'bit7; KERNEL.running_at_high_priority: bit6';
      result.append (flags);
      result.append (tRAMVariableMapNode.Create ('this', '2 bytes', 2, kernel_vars_group));
      result.append (tRAMVariableMapNode.Create ('ErrorCode', 'uint24', 3, kernel_vars_group));
      result.append (tRAMVariableMapNode.Create ('temp1', 'byte', 1, kernel_vars_group));
      result.append (tRAMVariableMapNode.Create ('temp2', 'byte', 1, kernel_vars_group));
      result.append (process_pcb_map (kernel_pcb_group));
      for i := Low(PriorityMapper.priorities) to High(PriorityMapper.priorities) do
         result.append (ready_element(i))
   end;  // kernel_variables_map

type
   TStackOverlaySegment =
      class
         start_addr, size: integer;
         process_var_name: string;
         segment_no: integer;
         constructor Create (_start_addr, _size: integer; _process_var_name: string);
         function segment_label: string;
         function OnThisStack (fsr2: integer): boolean;
         procedure output_udata_ovr_line;
         class function FindSegment (addr: integer): TStackOverlaySegment;
         class procedure Clear;
      end;

var
   StackOverlaySegments: array of TStackOverlaySegment;

constructor TStackOverlaySegment.Create (_start_addr, _size: integer; _process_var_name: string);
   begin
      start_addr := _start_addr;
      size := _size;
      process_var_name := _process_var_name;
      segment_no := Length(StackOverlaySegments);
      SetLength (StackOverlaySegments, segment_no+1);
      StackOverlaySegments[segment_no] := Self;
      output_udata_ovr_line;
      TAssemblySourceLine.Create (format (reserved_format_string, [segment_label + '?STACK', size]));
      TAssemblySourceLine.Create ('')
   end;

function TStackOverlaySegment.segment_label: string;
   begin
      result := format ('S%6.6d', [segment_no])
   end;

function TStackOverlaySegment.OnThisStack (fsr2: integer): boolean;
   begin
      result := (start_addr <= fsr2+1) and (fsr2+1 < start_addr + size)
   end;

procedure TStackOverlaySegment.output_udata_ovr_line;
   begin
      TAssemblySourceLine.Create (format ('%s                          UDATA_OVR 0x%3.3X ;  stack for %s', [segment_label, start_addr, process_var_name]))
   end;

class function TStackOverlaySegment.FindSegment (addr: integer): TStackOverlaySegment;
   var i: integer;
   begin
      result := nil;  // suppress compiler warning
      for i := 0 to Length(StackOverlaySegments)-1 do
         if StackOverlaySegments[i].OnThisStack (addr) then
            begin
               result := StackOverlaySegments[i];
               EXIT
            end;
      assert (false, 'not in any known stack segment')
   end;

class procedure TStackOverlaySegment.Clear;
   var i: integer;
   begin
      for i := 0 to Length(StackOverlaySegments)-1 do
         StackOverlaySegments[i].Free;
      SetLength (StackOverlaySegments, 0)
   end;


//===============================
// generate_static_variables_map
//===============================

procedure generate_static_variables_map;
   var
      i: integer;
      ram_map: tRAMVariableMapNode;
      phase: tRAMVariableGroup;
   begin   // generate_ram_variable_map
      TAssemblySourceBlankLine.Create;
      TAssemblySourceLine.Create (';======================');
      TAssemblySourceLine.Create ('; Static Variables Map ');
      TAssemblySourceLine.Create (';======================');
      TAssemblySourceBlankLine.Create;

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

      ram_map.shorten_labels_as_needed ('');

      output_udata_line (0);
      for phase := Low(tRAMVariableGroup) to vars_group do
         ram_map.output_reserved (phase, '', '');

      TAssemblySourceBlankLine.Create;
      for i := 0 to prog.program_vars.Length-1 do
         if prog.program_vars[i].typedef.IsProcessSystemType then
            TStackOverlaySegment.Create (TPIC18x_Variable(prog.program_vars[i]).stack_address,
                                         TPIC18x_SystemType(prog.program_vars[i].typedef).process_stack_size,
                                         prog.program_vars[i].name
                                        );
      TStackOverlaySegment.Create (kernel_stack_addr, kernel_stack_size, 'kernel');

      ram_map.Free
   end;    // generate_ram_variable_map


//====================
// TCalledRoutineList
//====================

type
   TCallPath =
      class
         call_path: string;
         fsr2_at_start_label: integer;
         constructor Create (_call_path: string; _fsr2_at_start_label: integer);
      end;
   TCalledRoutine =
      class (TBalancedTreeEntry)
         typename: string;
         routine_name: string;
         declaration_src_loc: TSourceLocation;
         start_label: TInstruction;
         call_paths: TSmoothSortArray;
         routine: TPIC18x_Routine;
         constructor Create (_typename, _routine_name: string; _declaration_src_loc: TSourceLocation; _start_label: TInstruction; call_path: string; fsr2_at_start_label: integer; _routine: TPIC18x_Routine);
         destructor Destroy;
            override;
         procedure add_call_path (call_path: string; fsr2_at_start_label: integer);
         procedure output_stack_frame_map (fsr2_at_start_label: integer);
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy
            (ToA: TBalancedTreeEntry
            ); // data
            override;
      end;

   TCalledRoutines =
      class
         var
            tree: TBalancedBinaryTree;
         procedure add_routine (cr: TPIC18x_CallRecord; call_path: string; fsr2_at_start_label: integer);
         constructor Create;
         destructor Destroy;
            override;
      end;

var
   CalledRoutines: TCalledRoutines;

constructor TCallPath.Create (_call_path: string; _fsr2_at_start_label: integer);
   begin
      call_path := _call_path;
      fsr2_at_start_label := _fsr2_at_start_label
   end;

procedure TPIC18x_CallRecord.traverse_call_tree (call_path: string; stk: integer);

   procedure traverse_subroutine_calls (def: TDefinition; call_path: string; stk: integer);
      var i: integer;
      begin
         for i := 0 to Length(def.call_record_list)-1 do
            TPIC18x_CallRecord(def.call_record_list[i]).traverse_call_tree (call_path + TPIC18x_CallRecord(def.call_record_list[i]).fmt_src_loc + ' -> ', stk)
      end;

   var
      access: TPIC18x_Access;
   begin
      access := TPIC18x_Access(called_access);

      if call_path <> '' then
         begin
            if call_type = systemtype_init_call then
               call_path := call_path + 'init ';
            stk := stk - caller_relative_stk_ptr_at_call;
         end;
      call_path := call_path + called_name;

      if call_type <> systemtype_init_call then
         CalledRoutines.add_routine (self, call_path, stk);

      if call_type = systemtype_init_call then
         traverse_subroutine_calls (access.node_typedef, call_path, stk)
      else if access.node_routine <> nil then
         traverse_subroutine_calls (access.node_routine, call_path, stk)
      else if access.node_property <> nil then
         if access.node_property.set_proc <> nil then
            traverse_subroutine_calls (access.node_property.set_proc, call_path, stk)
         else if access.node_property.get_func <> nil then
            traverse_subroutine_calls (access.node_property.get_func, call_path, stk)
      else
         assert (false)
   end;

function TPIC18x_CallRecord.fmt_src_loc: string;
   begin
      result := format ('@{%d:%d}', [TAccess(called_access).src_loc.line_no, TAccess(called_access).src_loc.line_idx])
   end;

constructor TCalledRoutine.Create (_typename, _routine_name: string; _declaration_src_loc: TSourceLocation; _start_label: TInstruction; call_path: string; fsr2_at_start_label: integer; _routine: TPIC18x_Routine);
   begin
      typename := _typename;
      routine_name := _routine_name;
      declaration_src_loc := _declaration_src_loc;
      start_label := _start_label;
      routine := _routine;
      SetLength (call_paths, 1);
      call_paths[0] := TCallPath.Create (call_path, fsr2_at_start_label)
   end;

destructor TCalledRoutine.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(call_paths)-1 do
         call_paths[i].Free;
      inherited
   end;

procedure TCalledRoutine.add_call_path (call_path: string; fsr2_at_start_label: integer);
   var i: integer;
   begin
      i := Length (call_paths);
      SetLength (call_paths, i+1);
      call_paths[i] := TCallPath.Create (call_path, fsr2_at_start_label)
   end;

procedure TCalledRoutine.output_stack_frame_map (fsr2_at_start_label: integer);
   var
      i: integer;
      map: tRAMVariableMapNode;
      available_stk_headroom: integer;
      segment: TStackOverlaySegment;
      name, typename: string;
   begin
      segment := TStackOverlaySegment.FindSegment (fsr2_at_start_label);
      segment.output_udata_ovr_line;
      available_stk_headroom := fsr2_at_start_label+1 - TPIC18x_DataItemList(routine.local_vars).Size - segment.start_addr;
      assert (available_stk_headroom >= 0);
      TAssemblySourceLine.Create (format (reserved_format_string_with_var_and_type, [current_udata_section_label + '?STACK', available_stk_headroom, 'avail stack', '<' + IntToStr(available_stk_headroom) +' bytes>']));

      map := tRAMVariableMapNode.CreateLocalRoot;

      for i := 0 to TPIC18x_DataItemList(routine.local_vars).Length-1 do
         map.append (tRAMVariableMapNode.Create (routine.local_vars[i].name, routine.local_vars[i].typedef, vars_group));

      if routine.parameter_definitions <> nil then
         for i := routine.parameter_definitions.Length-1 downto 0 do
            begin
               name := routine.parameter_definitions[i].name;
               typename := routine.parameter_definitions[i].typedef.name;
               case routine.parameter_definitions[i].descriptor of
                  rw_const,
                  rw_for:
                     case routine.parameter_definitions[i].ParamMode of
                        ByValue:
                           map.append (tRAMVariableMapNode.Create (name, typename, TPIC18x_TypeInfo(routine.parameter_definitions[i].TypeDef.info).Size, vars_group));
                        ByAddress:
                           map.append (tRAMVariableMapNode.Create (name, '^' + typename, ram_ptr_size, vars_group));
                     else
                        assert (false)
                     end;
                  rw_ioreg,
                  rw_var:
                     begin
                        if routine.parameter_definitions[i].typedef.type_kind = string_type then
                           map.append (tRAMVariableMapNode.Create (name + '_MAXSIZE', 'uint8', 1, vars_group));
                        map.append (tRAMVariableMapNode.Create (name, '^' + typename, ram_ptr_size, vars_group))
                     end;
                  rw_eeprom:
                     begin
                        if routine.parameter_definitions[i].typedef.type_kind = string_type then
                           map.append (tRAMVariableMapNode.Create (name + '_MAXSIZE', 'uint8', 1, vars_group));
                        map.append (tRAMVariableMapNode.Create (name, '^' + typename, eeprom_ptr_size, vars_group))
                     end;
                  rw_rom:
                     map.append (tRAMVariableMapNode.Create (name, '^' + typename, rom_ptr_size, vars_group));
               else
                  assert (false)
               end
            end;

      if routine.entry then
         begin
            if TSystemType(routine.context).system_type_kind = monitor_system_type then
               map.append (tRAMVariableMapNode.Create ('SAVED_PRIO', 'uint8', 1, vars_group));
            map.append (tRAMVariableMapNode.Create ('SAVED_THIS', '<RAM addr>', ram_ptr_size, vars_group))
         end;

      case routine.context.definition_kind of
         program_definition:
            map.append (tRAMVariableMapNode.Create ('RETURN_ADDRESS', '^rom', pc_size, vars_group));
         type_definition:
            if TSystemType(routine.context).system_type_kind <> interrupt_system_type then
               map.append (tRAMVariableMapNode.Create ('RETURN_ADDRESS', '^rom', pc_size, vars_group));
         else
            assert (false)
         end;

      if routine.function_result <> nil then
         if routine.function_result.typedef.type_kind = string_type then
            begin
               map.append (TRAMVariableMapNode.Create ('RESULT_MAXSIZE', 'uint8', 1, vars_group));
               map.append (TRAMVariableMapNode.Create ('RESULT', '^string', 2, vars_group))
            end
         else
            map.append (TRAMVariableMapNode.Create ('RESULT', routine.function_result.typedef, vars_group));

      map.shorten_labels_as_needed ('');
      map.output_reserved (vars_group, '', '');
      map.Free;
      TAssemblySourceLine.Create ('')
   end;

function TCalledRoutine.compare (a: TBalancedTreeEntry): Shortint;  // a < self :-1  a=self :0  a > self :+1
   begin
      if TCalledRoutine(a).typename < typename then
         result := -1
      else if TCalledRoutine(a).typename > typename then
         result := 1
      else  // same types (or both standalone)
         if TCalledRoutine(a).routine_name < routine_name then
            result := -1
         else if TCalledRoutine(a).routine_name > routine_name then
            result := 1
         else
            result := 0
   end;

procedure TCalledRoutine.copy(ToA: TBalancedTreeEntry);
   var
      i: integer;
      dest: TCalledRoutine;
   begin
      dest := TCalledRoutine(ToA);
      dest.routine_name := routine_name;
      if Length(dest.call_paths) > Length(call_paths) then
         begin
            i := 0;
            while i < Length(call_paths) do
               begin  // reuse existing objects
                  TCallPath(dest.call_paths[i]).call_path := TCallPath(call_paths[i]).call_path;
                  TCallPath(dest.call_paths[i]).fsr2_at_start_label := TCallPath(call_paths[i]).fsr2_at_start_label;
                  i := i + 1
               end;
            while i < Length(dest.call_paths) do
               begin
                  dest.call_paths[i].Free;
                  i := i + 1
               end;
            SetLength(dest.call_paths, Length(call_paths))
         end
      else  // Length(dest.call_paths) <= Length(call_paths)
         begin
            i := 0;
            while i < Length(dest.call_paths) do
               begin  // reuse existing objects
                  TCallPath(dest.call_paths[i]).call_path := TCallPath(call_paths[i]).call_path;
                  TCallPath(dest.call_paths[i]).fsr2_at_start_label := TCallPath(call_paths[i]).fsr2_at_start_label;
                  i := i + 1
               end;
            SetLength(dest.call_paths, Length(call_paths));
            while i < Length(call_paths) do
               begin
                  dest.call_paths[i] := TCallPath.Create (TCallPath(call_paths[i]).call_path, TCallPath(call_paths[i]).fsr2_at_start_label);
                  i := i + 1
               end
         end
   end;

constructor TCalledRoutines.Create;
   begin
      tree := TBalancedBinaryTree.Create
   end;

procedure TCalledRoutines.add_routine (cr: TPIC18x_CallRecord; call_path: string; fsr2_at_start_label: integer);

   function anon_typename (src_loc: TSourceLocation): string;
      begin
         result := format ('<AnonType@{%s:%d:%d}>', [src_loc.file_name, src_loc.line_no, src_loc.line_idx])
      end;

   var
      typename: string;
      block_name: string;
      start_label: TInstruction;
      routine: TPIC18x_routine;

   procedure get_systemtype_info (_routine: TRoutine);
      begin
         routine := TPIC18x_Routine(_routine);
         if TSystemType(_routine.context).name = '' then
            typename := anon_typename(_routine.context.src_loc)
         else
            typename := LowerCase(TSystemType(_routine.context).name);
         block_name := typename + '.' + LowerCase(_routine.name);
         start_label := TPIC18x_Routine(_routine).entry_point_label
      end;

   var
      access: TAccess;
src_loc: TSourceLocation;    // TODO - supply src loc so that definition location in source files can be displayed
   begin
      access := TAccess(cr.called_access);
      case cr.call_type of
         standalone_routine_call:
            begin
               typename := '';
               routine := TPIC18x_Routine (access.node_routine);
               block_name := LowerCase(access.node_routine.name);
               start_label := TPIC18x_Routine(access.node_routine).entry_point_label
            end;
         systemtype_routine_call:
            get_systemtype_info (access.node_routine);
         systemtype_property_call:
            if access.node_property.get_func <> nil then
               get_systemtype_info (access.node_property.get_func)
            else
               get_systemtype_info (access.node_property.set_proc);
      else
         assert (false)
      end;

      try
         tree.add (TCalledRoutine.Create (typename, block_name, src_loc, start_label, call_path, fsr2_at_start_label, routine))
      except
         on e: ESymbolAlreadyInSymbolTable do
            TCalledRoutine(e.entry).add_call_path(call_path, fsr2_at_start_label)
      end
   end;

destructor TCalledRoutines.Destroy;
   begin
      tree.Free;
      inherited
   end;

function CompareCallPaths (v1,v2: TObject): boolean;  // result := v1 <= v2
   begin
      result := TCallPath(v1).fsr2_at_start_label <= TCallPath(v2).fsr2_at_start_label
   end;

procedure generate_stack_frame_maps;

   procedure output_routine_stack_frame_maps (routine: TCalledRoutine);
      var
         fsr2: integer;
         i: integer;
         s0,s1: string;
         lbl: TInstruction;
         map_count: integer;
      begin
         if routine = nil then
            exit;
            
         if routine.lesser_values <> nil then
            output_routine_stack_frame_maps (TCalledRoutine(routine.lesser_values));

         SmoothSort (routine.call_paths, CompareCallPaths);
         map_count := 1;
         for i := 1 to Length(routine.call_paths)-1 do
            if TCallPath(routine.call_paths[i-1]).fsr2_at_start_label <> TCallPath(routine.call_paths[i]).fsr2_at_start_label then
               map_count := map_count + 1;

         if map_count = 1 then
            s1 := '; Stack Frame Map for ' + routine.routine_name
         else
            s1 := '; Stack Frame Maps for ' + routine.routine_name;
         SetLength (s0, Length(s1)+1);
         s0[1] := ';';
         for i := 2 to Length(s1)+1 do
            s0[i] := '=';
         TAssemblySourceBlankLine.Create;
         TAssemblySourceLine.Create (s0);
         TAssemblySourceLine.Create (s1);
         TAssemblySourceLine.Create (s0);
         TAssemblySourceBlankLine.Create;

         i := 0;
         while i < Length(routine.call_paths) do
            begin
               fsr2 := TCallPath(routine.call_paths[i]).fsr2_at_start_label;
               lbl := TAssemblySourceLineWithLabelReference.Create ('; Use this Stack Frame Map when FSR2 is ' + format ('$%3.3X', [fsr2]) + ' at %s');
               lbl.dest := routine.start_label;
               udata_section := udata_section + 1;
               if (i = Length(routine.call_paths)-1)
                  or
                  (TCallPath(routine.call_paths[i+1]).fsr2_at_start_label <> fsr2)
               then
                  begin
                     TAssemblySourceLine.Create (';    Call path: ' + TCallPath(routine.call_paths[i]).call_path);
                     i := i + 1
                  end
               else
                  begin
                     TAssemblySourceLine.Create (';    Call paths:');
                     repeat
                        TAssemblySourceLine.Create (';       ' + TCallPath(routine.call_paths[i]).call_path);
                        i := i + 1
                     until (i = Length(routine.call_paths))
                           or
                           (TCallPath(routine.call_paths[i]).fsr2_at_start_label <> fsr2)
                  end;
               routine.output_stack_frame_map (fsr2)
            end;

         if routine.greater_values <> nil then
            output_routine_stack_frame_maps (TCalledRoutine(routine.greater_values))
      end;

   var
      i: integer;
      acc: TPIC18x_Access;
   begin  // output_stack_frame_map
      CalledRoutines := TCalledRoutines.Create;
      for i := 0 to Length(prog.call_record_list)-1 do
         begin
            acc := TPIC18x_Access (prog.call_record_list[i].called_access);
            if (acc.base_variable <> nil)
               and
               (acc.base_variable.typedef.IsProcessSystemType)
            then
               TPIC18x_CallRecord(prog.call_record_list[i]).traverse_call_tree ('', TPIC18x_Variable(acc.base_variable).stack_address + TPIC18x_SystemType(acc.base_variable.typedef).process_stack_size - 1)
            else
               TPIC18x_CallRecord(prog.call_record_list[i]).traverse_call_tree ('<PROG INIT>' + TPIC18x_CallRecord(prog.call_record_list[i]).fmt_src_loc + ' -> ', kernel_stack_base)
         end;
      output_routine_stack_frame_maps (TCalledRoutine(CalledRoutines.tree.root));
      CalledRoutines.Free;
      CalledRoutines := nil
   end;   // output_stack_frame_map

procedure generate_memory_map;
   begin
      udata_section := 0;
      generate_static_variables_map;
      generate_stack_frame_maps;
      TStackOverlaySegment.Clear
   end;

END.
