UNIT pic18x_access_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
  cpc_access_unit, pic18x_instructions_unit;

type
   TPIC18x_Access =
      class (TAccess)
      private
         function simple_index_calculation (path_idx: integer): boolean;
            // returns true if:
            //    - path[path_idx] is an index (not a field),
            //    - index variable is not itself indexed (it can be a variable's record field),
            //    - index variable is not a with variable
            //    - index variable does not a bounds check,
            //    - index is either a bank 0 global or a near tos-relative local,
            //    - either:
            //         element size is 1 and index variable is 1 or 2 bytes, or
            //         element size is <= 255 and index variable is 1 unsigned byte
         procedure add_total_indexed_offsets (ptr_size: integer);
         function add_simple_indexed_offset1 (ptr: TPointer; i: integer): TInstruction;
         function add_simple_indexed_offset2 (ptr: TPointer; i: integer): TInstruction;
         function tos_relative_with_variable_address: integer;
      public
         function total_fixed_offsets: integer;
         function complex_index_calculations_needed: boolean;
         procedure Generate_Push_Address1_Code (offset: integer; also_push_string_size: boolean);    // 1-byte address
         procedure Generate_Push_Address2_Code (offset: integer; also_push_string_size: boolean);    // 2-byte address
         procedure Generate_Load_Ptr1_Code (ptr: TPointer; offset: integer);     // 1-byte address
         procedure Generate_Load_Ptr2_Code (ptr: TPointer; offset: integer);     // 2-byte address
         function absolute_address_with_no_indexing_required_mode: boolean;
         function absolute_address (offset: integer): integer;   // valid only for global non-indexed variables
         function directly_addressable_absolute_address: boolean;  // true if entire node can be accessed in bank0 or access bank
         function indirectly_addressable_absolute_address: boolean;
         function near_stack_address_with_no_indexing_required_mode6 (offset: integer): boolean;
         function near_stack_address (offset: integer): integer;  // valid only for near stack non-indexed variables
         function is_overlay_variable_needing_range_check: boolean;
         function node_is_in_data_address_space: boolean;   // data is in RAM or ioreg - not eeprom or ROM
         function node_is_single_bit_data_packed_field: boolean;  // data is RAM or ioreg - not eeprom or ROM
      end;

// Array Indexing Notes:
//    Constant indexes are included in "total_fixed_offsets" at compile time and are not considered an index calculation.
//    A "simple" index is one that can be calculated directly with at most a 1x1 multiply and no index register usage,
//       a simple index calculation may be done directly on an index register or on a TOS ptr.
//    Anything else is a "complex" index calculation and is always done on the stack.


IMPLEMENTATION

uses
  SysUtils, pic18x_cpu_unit, cpc_source_analysis_unit, pic18x_macro_instructions_unit, pic18x_microprocessor_information_unit,
  pic18x_types_unit, pic18x_expressions_unit, pic18x_run_time_error_check_unit,
  cpc_multi_precision_integer_unit, math, cpc_expressions_unit, cpc_definitions_unit, cpc_statements_unit,
  pic18x_multiply_divide_unit, cpc_target_cpu_unit, pic18x_statements_unit, cpc_blocks_unit, pic18x_kernel_unit,
  pic18x_core_objects_unit, cpc_core_objects_unit;

var
   temp: TMultiPrecisionInteger;

function TPIC18x_Access.total_fixed_offsets: integer;
   var i: integer;
   begin
      result := 0;
      for i := 0 to Length(path)-1 do
         case path[i].access_path_kind of
            record_field_access:
               result := result + TPIC18x_RecordFieldInfo(path[i].record_field.info).Offset;
            indexed_array_access:
               begin
                  result := result - (path[i].index_typedef.info.min_value.AsInteger * TPIC18x_TypeInfo(path[i].element_typedef.info).Size);
                  if path[i].index_expression.contains_constant then
                     result := result + (path[i].index_expression.constant.AsOrdinal * TPIC18x_TypeInfo(path[i].element_typedef.info).Size);
               end;
            indexed_string_access:
               if (not ((base_variable.typedef.type_kind = string_type)
                        and
                        (TStringType(base_variable.typedef).max_length = -1)  // undimensioned string parameter
                        )
                  )
                  and
                  (path[i].index_expression.contains_constant)
               then
                  result := result + path[i].index_expression.constant.AsOrdinal;
            system_type_access,
            overlay_variable_access,
            packed_record_field_access,
            string_attribute_access:
               ;   // offset is 0
         else
            assert (false)
         end;
   end;

function TPIC18x_Access.simple_index_calculation (path_idx: integer): boolean;
   var
      index_var: TPIC18x_VariableAccessPrimary;
      index_var_size: integer;
   begin
      result := false;

      if (not (path[path_idx].access_path_kind in [indexed_array_access, indexed_string_access]))
         or
         (TPIC18x_TypeInfo(path[path_idx].element_typedef.info).Size > 255)
         or
         not (path[path_idx].index_expression is TVariableAccessPrimary)
      then
         exit;

      index_var := TPIC18x_VariableAccessPrimary (path[path_idx].index_expression);

      if (not (index_var.access.base_variable.address_mode in [absolute_address_mode, local_address_mode]))
         or
         index_var.access.indexed    // index var may be a record field, but not an array element
         or
         (index_var.access.base_variable <> index_var.access.path_start)  // with variable - would need index register to access
         or
         index_var.access.node_typedef.info.min_value.lt(path[path_idx].index_typedef.info.min_value)     // will it need range check?
         or                                                                                        //   (range checks are performed on the stack, the simple index calc will be in-line)
         index_var.access.node_typedef.info.max_value.gt(path[path_idx].index_typedef.info.max_value)
      then
         exit;

      index_var_size := TPIC18x_TypeInfo(index_var.access.node_typedef.info).Size;

      if (TPIC18x_TypeInfo(path[path_idx].element_typedef.info).Size > 1)
         and
         (index_var_size > 1)
      then
         exit;

      if (index_var.access.base_variable.address_mode = absolute_address_mode)
         and
         (index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets > 256 - index_var_size)
      then
         exit;    // can load with MOVF bank 0 instructions - otherwise would need index register

      if (index_var.access.base_variable.address_mode = local_address_mode)
         and
         (index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets + StackUsageCounter.Current > $60 - index_var_size)
      then
         exit;    // can load with MOVF [addr] instructions - otherwise would need index register

      if (TPIC18x_TypeInfo(index_var.access.node_typedef.info).Size = 1)
         and
         (TPIC18x_TypeInfo(path[path_idx].element_typedef.info).Size > 1)
         and
         (index_var.access.node_typedef.info.IntegerRange <> irAlwaysNonNegative)
      then
         exit;

      if (base_variable.typedef.type_kind = string_type)
         and
         (TStringType(base_variable.typedef).max_length = -1)
      then  // index into undimensioned string parameter
         exit;

      result := true
   end;

function TPIC18x_Access.complex_index_calculations_needed: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(path)-1 do
         if (path[i].access_path_kind = indexed_string_access)
            and
            (base_variable.typedef.type_kind = string_type)
            and
            (TStringType(base_variable.typedef).max_length = -1)   // undimensioned string parameter
         then
            result := true
         else
            if (path[i].access_path_kind in [indexed_array_access, indexed_string_access])
               and
               (not path[i].index_expression.contains_constant)
            then
               if not simple_index_calculation(i)
               then
                  result := true
   end;

function TPIC18x_Access.add_simple_indexed_offset1 (ptr: TPointer; i: integer): TInstruction;
   var
      index_var: TPIC18x_VariableAccessPrimary;
      stack_offset: integer;
   begin
      // note, a simple index calculation:
      //    - will be done in-line, not on stack
      //    - cannot use index register anywhere
      //    - ptr size of 1 is only used for eeprom access, hence must be inside system type and index var will not be global
      assert (simple_index_calculation(i));
      index_var := TPIC18x_VariableAccessPrimary (path[i].index_expression);
      assert (index_var.access.base_variable.address_mode = local_address_mode);
      assert (TPIC18x_TypeInfo(index_var.access.node_typedef.info).Size = 1);
      stack_offset := index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets + StackUsageCounter.Current;
      assert (stack_offset <= $5F);
      result := TPIC18x_MOVF.Create (stack_offset, dest_w, access_mode);
      if TPIC18x_TypeInfo(path[i].element_typedef.info).Size = 1 then
         TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, ram_access_mode(ptr))
      else  // element size > 1
         begin
            TPIC18x_MULLW.Create (TPIC18x_TypeInfo(path[i].element_typedef.info).Size);
            TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
            TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, ram_access_mode(ptr))
         end
   end;

function TPIC18x_Access.add_simple_indexed_offset2 (ptr: TPointer; i: integer): TInstruction;
   var
      index_var: TPIC18x_VariableAccessPrimary;

   function load_byte (offset: integer): TInstruction;
      var
         addr: integer;
      begin   // load_byte
         result := nil;  // suppress compiler warning
         case index_var.access.base_variable.address_mode of
            absolute_address_mode:
               begin
                  addr := index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets + offset;
                  assert (addr <= $FF);   // or else this isn't simple indexed offset
                  result := TPIC18x_MOVF.Create (addr, dest_w, bank_mode)
               end;
            local_address_mode:
               begin
                  addr := index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets + StackUsageCounter.Current + offset;
                  assert (addr <= $5F);  // or else this isn't simple indexed offset
                  result := TPIC18x_MOVF.Create (addr, dest_w, access_mode);
               end;
         else
            assert (false)
         end
      end;   // load_byte

   var
      addr : integer;
   begin   // add_simple_indexed_offset2
      result := nil;  // suppress compiler warning
      assert (simple_index_calculation(i));
      index_var := TPIC18x_VariableAccessPrimary (path[i].index_expression);
      case TPIC18x_TypeInfo(index_var.access.node_typedef.info).Size of
         1: begin
               result := load_byte(0);
               if TPIC18x_TypeInfo(path[i].element_typedef.info).Size = 1 then
                  begin
                     TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, ram_access_mode(ptr));
                     case index_var.access.node_typedef.info.IntegerRange of
                        irAlwaysNegative:
                           TPIC18x_MOVLW.Create ($FF);
                        irNegativeOrPositive:
                              case index_var.access.base_variable.address_mode of
                                 absolute_address_mode:
                                    begin
                                       addr := index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets;
                                       assert (addr <= $FF);  // or else isn't simple indexed offset
                                       TPIC18x_MOVLW.Create (0);
                                       TPIC18x_BTFSC.Create (addr, 7, bank_mode);
                                       TPIC18x_MOVLW.Create ($FF)
                                    end;
                                 local_address_mode:
                                    begin
                                       addr := index_var.access.base_variable.address + TPIC18x_Access(index_var.access).total_fixed_offsets + StackUsageCounter.Current;
                                       assert (addr <= $5F);  // or else isn't simple indexed offset
                                       TPIC18x_MOVLW.Create (0);
                                       TPIC18x_BTFSC.Create (addr, 7, access_mode);
                                       TPIC18x_MOVLW.Create ($FF)
                                    end;
                              else
                                 assert (false)
                              end;
                        irAlwaysNonNegative:
                           TPIC18x_MOVLW.Create (0);
                     else
                        assert (false)
                     end;
                     TPIC18x_ADDWFC.Create (ptrH(ptr), dest_f, ram_access_mode(ptr))
                  end
               else  // element size > 1
                  begin
                     assert (index_var.access.node_typedef.info.IntegerRange = irAlwaysNonNegative);
                     TPIC18x_MULLW.Create (TPIC18x_TypeInfo(path[i].element_typedef.info).Size);
                     TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
                     TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, ram_access_mode(ptr));
                     TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                     TPIC18x_ADDWFC.Create (ptrH(ptr), dest_f, ram_access_mode(ptr))
                  end
            end;
         2: begin
               assert (TPIC18x_TypeInfo(path[i].element_typedef.info).Size = 1);
               result := load_byte(1);
               TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, ram_access_mode(ptr));
               load_byte(0);
               TPIC18x_ADDWFC.Create (ptrH(ptr), dest_f, ram_access_mode(ptr))
            end;
      else
         assert (false)
      end
   end;   // add_simple_indexed_offset2

function TPIC18x_Access.tos_relative_with_variable_address: integer;
   begin
      result := 0;  // suppress compiler warning
      case path_start.definition_kind of
         with_property_definition:
            result := StackUsageCounter.Current - TPIC18x_WithStatement(TWithProperty(path_start).with_statement).address;
         with_routine_definition:
            result := StackUsageCounter.Current - TPIC18x_WithStatement(TWithRoutine(path_start).with_statement).address;
         with_variable_definition:
            result := StackUsageCounter.Current - TPIC18x_WithStatement(TWithVariable(path_start).with_statement).address
      else
         assert (false)
      end
   end;

procedure TPIC18x_Access.add_total_indexed_offsets (ptr_size: integer);

   procedure push_indexed_offset (var path_element: TAccessPathElement);
      var
         tos_expression_size, element_size: integer;
         element_size_typeinfo: TPIC18x_TypeInfo;
         a_info: TIntegerInfo;
         annotation: string;
      begin    // push_indexed_offset
         tos_expression_size := TPIC18x_TypeInfo(path_element.index_expression.info).Size;
         path_element.index_expression.Generate (GenerateCode, tos_expression_size);
         if (base_variable.typedef.type_kind = string_type)
            and
            (TStringType(base_variable.typedef).max_length = -1)
         then   // undimensioned string parameter - check against param.maxstrlen
            begin
               if not ((path_element.index_expression.info.min_value.AsInteger = 1)
                       and
                       (path_element.index_expression.info.max_value.AsInteger = 1)
                      )
               then   // string index might be something other than 1
                  begin
                     // push maxstrlen
                     case base_variable.address_mode of
                        system_type_indirect_address_mode:
                           begin
                              assert (base_variable.descriptor = rw_const);
                              // push actual strlen
                              load_ptr (pTHIS, base_variable.address, pFSR1).annotation := 'push strlen in lieu of maxstrlen';
                              TPIC18x_MOVFF.Create (POSTINC1, FSR0H);
                              TPIC18x_MOVFF.Create (POSTINC1, FSR0L);
                              TPIC18x_MOVFF.Create (INDF0, POSTDEC2);
                              StackUsageCounter.Push (1)
                           end;
                        local_indirect_address_mode:
                           case base_variable.descriptor of
                              rw_const:  // push actual strlen
                                 begin
                                    annotation := 'push strlen in lieu of maxstrlen';
                                    if base_variable.address + 1 + StackUsageCounter.Current <= $5F then
                                       begin
                                          TPIC18x_MOVF.Create (base_variable.address + 1 + StackUsageCounter.Current, dest_w, access_mode).annotation := annotation;
                                          TPIC18x_MOVWF.Create (FSR1L, access_mode);
                                          TPIC18x_MOVF.Create (base_variable.address + StackUsageCounter.Current, dest_w, access_mode);
                                          TPIC18x_MOVWF.Create (FSR1H, access_mode);
                                          TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                                          TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                          StackUsageCounter.Push (1)
                                       end
                                    else
                                       begin
                                          load_ptr (pFSR2, base_variable.address + StackUsageCounter.Current, pFSR0).annotation := annotation;
                                          TPIC18x_MOVFF.Create (POSTINC0, FSR1H);
                                          TPIC18x_MOVFF.Create (POSTINC0, FSR1L);
                                          TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                                          TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                          StackUsageCounter.Push (1)
                                       end
                                 end;
                              rw_var:
                                 begin
                                    annotation := 'push hidden size parameter - 1 to give maxstrlen';
                                    if base_variable.address - 1 + StackUsageCounter.Current <= $5F then
                                       begin
                                          TPIC18x_DECF.Create (base_variable.address - 1 + StackUsageCounter.Current, dest_w, access_mode).annotation := annotation;
                                          TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                          StackUsageCounter.Push (1)
                                       end
                                    else
                                       begin
                                          load_ptr (pFSR2, base_variable.address - 1 + StackUsageCounter.Current, pFSR1).annotation := annotation;
                                          TPIC18x_DECF.Create (INDF1, dest_w, access_mode);
                                          TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                          StackUsageCounter.Push (1)
                                       end
                                 end;
                           else
                              assert (false)
                           end;
                     else
                        assert (false)
                     end;
                     GenerateUndimensionedStringIndexRangeCheckCode (tos_expression_size,
                                                                     path_element.index_expression.info,
                                                                     path_element.src_loc
                                                                    )
                  end
            end
         else
            GenerateRangeCheckCode (path_element.index_typedef,
                                    tos_expression_size,
                                    path_element.index_expression.info,
                                    path_element.index_expression.src_loc,
                                    rterr_out_of_bounds_array_index
                                   );
         element_size := TPIC18x_TypeInfo(path_element.element_typedef.info).Size;
         if element_size = 1 then
            begin
               if tos_expression_size > ptr_size then
                  begin
                     TPIC18x_ADDFSR.Create (2, tos_expression_size-ptr_size);
                     StackUsageCounter.Pop (tos_expression_size-ptr_size)
                  end
               else if tos_expression_size < ptr_size then
                  begin
                     // need to extend sign one byte
                     assert ((tos_expression_size = 1) and (ptr_size = 2));
                     case path_element.index_typedef.info.IntegerRange of
                        irAlwaysNonNegative:
                           TPIC18x_CLRF.Create (POSTDEC2, access_mode);
                        irNegativeOrPositive:
                           begin
                              TPIC18x_CLRF.Create (POSTDEC2, access_mode);  // provisionally sign extend with new msb $00
                              TPIC18x_BTFSC.Create (2, 7, access_mode);     // test sign bit
                              TPIC18x_SETF.Create (1, access_mode)          // overwrite msb with $FF sign extension
                           end;
                        irAlwaysNegative:
                           TPIC18x_SETF.Create (POSTDEC2, access_mode);
                     else
                        assert (false)
                     end;
                     StackUsageCounter.Push(1)
                  end
            end
         else   // element_size > 1
            begin
               if tos_expression_size > ptr_size then
                  begin
                     TPIC18x_ADDFSR.Create (2, tos_expression_size-ptr_size);
                     StackUsageCounter.Pop (tos_expression_size-ptr_size)
                  end
               else if tos_expression_size < ptr_size then
                  begin
                     TPIC18x_SUBFSR.Create (2, ptr_size - tos_expression_size);
                     StackUsageCounter.Push (ptr_size - tos_expression_size)
                  end;
               if element_size < 256 then
                  begin
                     TPIC18x_PUSHL.Create (lsb(element_size));
                     StackUsageCounter.Push(1);
                     element_size_typeinfo := TPIC18x_TypeInfo(target_cpu.get_supported_data_type('uint8').info)
                  end
               else
                  begin
                     assert (element_size < 65536);
                     TPIC18x_PUSHL.Create (lsb(element_size));
                     TPIC18x_PUSHL.Create (msb(element_size));
                     StackUsageCounter.Push(2);
                     element_size_typeinfo := TPIC18x_TypeInfo(target_cpu.get_supported_data_type('uint16').info)
                  end;

               a_info := TIntegerInfo.Create (TPIC18x_TypeInfo(path_element.index_typedef.info));
               GenerateMultiplyCode (ptr_size, a_info, element_size_typeinfo);
               a_info.Release
            end
      end;     // push_indexed_offset

   var
      i: integer;
      idx_num: integer;
      annotation: string;
   begin   // add_total_indexed_offsets
      idx_num := 1;
      for i := 0 to Length(path)-1 do
         if ((path[i].access_path_kind in [indexed_array_access, indexed_string_access])
             and
             (not path[i].index_expression.contains_constant)
            )
            or
            ((path[i].access_path_kind = indexed_string_access)
             and
             (base_variable.typedef.type_kind = string_type)
             and
             (TStringType(base_variable.typedef).max_length = -1)   // undimensioned string parameter
            )
         then
            begin
               annotation := 'add index #' + IntToStr(idx_num);
               idx_num := idx_num + 1;
               if simple_index_calculation(i) then
                  case ptr_size of
                     1: add_simple_indexed_offset1 (pTOS, i).annotation := annotation;
                     2: add_simple_indexed_offset2 (pTOS, i).annotation := annotation;
                  else
                     assert (false)
                  end
               else
                  begin
                     TAssemblyLabel.Create.annotation := annotation;
                     push_indexed_offset (path[i]);
                     case ptr_size of
                        1: begin
                              TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
                              StackUsageCounter.Pop(1);
                              TPIC18x_ADDWF.Create (1, dest_f, access_mode)
                           end;
                        2: begin
                              TPIC18x_MOVF.Create (2, dest_w, access_mode);
                              TPIC18x_ADDWF.Create (4, dest_f, access_mode);
                              TPIC18x_MOVF.Create (1, dest_w, access_mode);
                              TPIC18x_ADDWFC.Create (3, dest_f, access_mode);
                              TPIC18x_ADDFSR.Create (2, 2);
                              StackUsageCounter.Pop (2)
                           end;
                     else
                        assert (false)
                     end
                  end
            end
   end;    // add_total_indexed_offsets

procedure TPIC18x_Access.Generate_Push_Address1_Code (offset: integer; also_push_string_size: boolean);

   procedure push_string_size_if_needed;
      begin
         if also_push_string_size then
            begin
               if node_attribute_string_typedef.known_strlen then
                  TPIC18x_PUSHL.Create (TPIC18x_TypeInfo(node_attribute_string_typedef.info).Size and $ff).annotation := 'push maxstrlen;'     // push 0 for size of 256
               else
                  begin
                     TPIC18x_MOVF.Create (base_variable.address-1+StackUsageCounter.Current, dest_w, access_mode).annotation := 'push maxstrlen;';
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
                  end;
               StackUsageCounter.Push (1)
            end
      end;

   const
      ptr_size = 1;
   var
      total_offset: integer;
      annotation: string;
   begin    // Generate_Push_Address1_Code
      assert (base_variable.descriptor = rw_eeprom);
      if node_typedef = nil then
         annotation := 'push @' + path_src
      else
         annotation := 'push @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);

      case path_start.definition_kind of
         variable_definition:
            case base_variable.address_mode of
                system_type_address_mode:
                  begin
                     TPIC18x_MOVFF.Create (this_ptrH, FSR1H).annotation := annotation;;
                     TPIC18x_MOVFF.Create (this_ptrL, FSR1L);
                     case TSystemType(base_variable.context).system_type_kind of
                        class_system_type:
                           TPIC18x_MOVLW.Create ((-$3F) and $FF);
                        monitor_system_type:
                           TPIC18x_MOVLW.Create ((-$3F) and $FF);
                        process_system_type:
                           TPIC18x_MOVLW.Create ((-$3F) and $FF);
                     else
                        assert (false)
                     end;
                     TPIC18x_MOVF.Create (PLUSW1, dest_w, access_mode);
                     total_offset := base_variable.address + total_fixed_offsets + offset;
                     if total_offset <> 0 then
                        TPIC18x_ADDLW.Create (total_offset and $ff);
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1);
                     add_total_indexed_offsets (ptr_size);
                     push_string_size_if_needed
                  end;
               local_indirect_address_mode:
                  begin
                     total_offset := total_fixed_offsets + offset;
                     if total_offset = 0 then
                        if base_variable.address+StackUsageCounter.Current <= $5F then
                           begin   // addr is available near stack
                              TPIC18x_MOVF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode).annotation := annotation;
                              TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push (1)
                           end
                        else
                           begin   // addr is available far stack
                              TPIC18x_MOVLW.Create (lsb(base_variable.address+StackUsageCounter.Current)).annotation := annotation;
                              TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (FSR1L, access_mode);
                              TPIC18x_MOVLW.Create (msb(base_variable.address+StackUsageCounter.Current));
                              TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (FSR1H, access_mode);
                              TPIC18x_MOVFF.Create (INDF1, POSTDEC2);
                              StackUsageCounter.Push (1)
                           end
                     else  // total_offset <> 0
                        if base_variable.address+StackUsageCounter.Current <= $5F then
                           begin  // addr is available near stack
                              TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                              TPIC18x_ADDWF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push (1)
                           end
                        else
                           begin  // addr is available far stack
                              TPIC18x_MOVLW.Create (lsb(base_variable.address+StackUsageCounter.Current)).annotation := annotation;
                              TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (FSR1L, access_mode);
                              TPIC18x_MOVLW.Create (msb(base_variable.address+StackUsageCounter.Current));
                              TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (FSR1H, access_mode);
                              TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                              TPIC18x_ADDLW.Create (lsb(total_offset));
                              TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push(1)
                           end;
                     add_total_indexed_offsets (ptr_size);
                     push_string_size_if_needed
                  end;
            else
               assert (false)
            end;
         with_variable_definition:
            begin
               assert (TWithVariable(path_start).record_field.definition_kind = record_field_definition);
               total_offset := TPIC18x_RecordFieldInfo(TWithVariable(path_start).record_field.info).Offset + total_fixed_offsets + offset;
               if total_offset = 0 then
                  begin
                     TPIC18x_MOVF.Create (tos_relative_with_variable_address, dest_w, access_mode).annotation := annotation;
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1)
                  end
               else
                  begin
                     TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                     TPIC18x_ADDWF.Create (tos_relative_with_variable_address, dest_w, access_mode);
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                     StackUsageCounter.Push (1)
                  end;
               add_total_indexed_offsets (ptr_size);
               push_string_size_if_needed
            end;
      else
         assert (false)
      end
   end;   // Generate_Push_Address1_Code

procedure TPIC18x_Access.Generate_Push_Address2_Code (offset: integer; also_push_string_size: boolean);

   procedure push_string_size_if_needed;
      begin
         if also_push_string_size then
            begin
               if node_attribute_string_typedef.known_strlen then
                  TPIC18x_PUSHL.Create (TPIC18x_TypeInfo(node_attribute_string_typedef.info).Size and $ff).annotation := 'push maxstrlen'     // push 0 for size of 256
               else
                  begin
                     TPIC18x_MOVF.Create (base_variable.address-1+StackUsageCounter.Current, dest_w, access_mode).annotation := 'push maxstrlen';
                     TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
                  end;
               StackUsageCounter.Push (1)
            end
      end;

   var
      ptr_size: integer;

   procedure push_stack_relative_address (offset: integer);
      begin
         assert (ptr_size=2);
         if offset = 0 then
            begin
               TPIC18x_MOVF.Create (FSR2H, dest_w, access_mode);
               TPIC18x_MOVFF.Create (FSR2L, POSTDEC2);
               TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
            end
         else
            begin
               TPIC18x_SUBFSR.Create (2, 2);
               offset := offset + 2;
               TPIC18x_MOVLW.Create (lsb(offset));
               TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
               TPIC18x_MOVWF.Create (2, access_mode);
               TPIC18x_MOVLW.Create (msb(offset));
               TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
               TPIC18x_MOVWF.Create (1, access_mode)
            end;
         StackUsageCounter.Push (2);
         add_total_indexed_offsets (ptr_size)
      end;

   var
      total_offset, i: integer;
      annotation: string;
   begin    // Generate_Push_Address2_Code
      if node_typedef = nil then
         annotation := 'push @' + path_src
      else
         annotation := 'push @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
      if node_access_kind in [function_access, procedure_access, property_access] then
         begin  // remove routine name from annotation
            i := Length(annotation);
            while (i > 0)
                  and
                  (annotation [i] <> '.') do
               i := i - 1;
            if (i > 0)
               and
               (annotation[i] = '.') then
               i := i - 1;
            SetLength (annotation, i)
         end;

      ptr_size := 2;
      if is_maxstrlen_attribute then
         case path_start.definition_kind of
            variable_definition:
               case base_variable.address_mode of
                  local_indirect_address_mode:
                     push_stack_relative_address (base_variable.address + StackUsageCounter.Current + total_fixed_offsets + offset - 1);
               else
                  assert (false)
               end;
         else  // not is_maxstrlen_attribute
            assert (false)
         end
      else
         case path_start.definition_kind of
            variable_definition:
               case base_variable.descriptor of
                  rw_const,
                  rw_var,
                  rw_ioreg,
                  rw_for,
                  rw_rom:
                     case base_variable.address_mode of
                        absolute_address_mode:
                           begin
                              total_offset := base_variable.address + total_fixed_offsets + offset;
                              TPIC18x_PUSHL.Create (lsb(total_offset)).annotation := annotation;
                              TPIC18x_PUSHL.Create (msb(total_offset));
                              StackUsageCounter.Push (2);
                              add_total_indexed_offsets (ptr_size);
                              push_string_size_if_needed
                           end;
                        system_type_address_mode:
                           begin
                              total_offset := base_variable.address + total_fixed_offsets + offset;
                              if total_offset = 0 then
                                 begin
                                    TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2).annotation := annotation;
                                    TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2)
                                 end
                              else  // total_offset <> 0
                                 begin
                                    TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                    TPIC18x_ADDWF.Create (this_ptrL, dest_w, bank_mode);
                                    TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                    TPIC18x_MOVLW.Create (msb(total_offset));
                                    TPIC18x_ADDWFC.Create (this_ptrH, dest_w, bank_mode);
                                    TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
                                 end;
                              StackUsageCounter.Push (2);
                              add_total_indexed_offsets (ptr_size);
                              push_string_size_if_needed
                           end;
                        system_type_indirect_address_mode:
                           begin
                              load_ptr (pTHIS, base_variable.address, pFSR1).annotation := annotation;
                              TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
                              TPIC18x_MOVFF.Create (POSTINC1, POSTDEC2);
                              TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                              StackUsageCounter.Push (2);
                              total_offset := total_fixed_offsets + offset;
                              if total_offset <> 0 then
                                 begin
                                    TPIC18x_MOVLW.Create (lsb(total_offset));
                                    TPIC18x_ADDWF.Create (2, dest_f, access_mode);
                                    TPIC18x_MOVLW.Create (msb(total_offset));
                                    TPIC18x_ADDWFC.Create (1, dest_f, access_mode)
                                 end;
                              add_total_indexed_offsets (ptr_size);
                              push_string_size_if_needed
                           end;
                        local_address_mode:
                           begin
                              push_stack_relative_address (base_variable.address + total_fixed_offsets + offset + StackUsageCounter.Current);
                              push_string_size_if_needed
                           end;
                        local_indirect_address_mode:
                           begin
                              total_offset := total_fixed_offsets + offset;
                              if base_variable.address+1+StackUsageCounter.Current <= $5F then
                                 if total_offset = 0 then
                                    begin
                                       // don't use MOVSF here since dest is POSTDEC2
                                       TPIC18x_MOVF.Create (base_variable.address+1+StackUsageCounter.Current, dest_w, access_mode).annotation := annotation;
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       TPIC18x_MOVF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       add_total_indexed_offsets (ptr_size);
                                       push_string_size_if_needed
                                    end
                                 else  // total_offset <> 0
                                    begin
                                       TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                       TPIC18x_ADDWF.Create (base_variable.address+1+StackUsageCounter.Current, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       TPIC18x_MOVLW.Create (msb(total_offset));
                                       TPIC18x_ADDWFC.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       add_total_indexed_offsets (ptr_size);
                                       push_string_size_if_needed
                                    end
                              else  // base_variable.address+1+StackUsageCounter.Current > $5F
                                 begin
                                    load_ptr (pFSR2, base_variable.address+1+StackUsageCounter.Current, pFSR0).annotation := annotation;
                                    TPIC18x_MOVFF.Create (POSTDEC0, POSTDEC2);
                                    TPIC18x_MOVFF.Create (POSTDEC0, POSTDEC2);
                                    StackUsageCounter.Push (2);
                                    if total_offset <> 0 then
                                       begin
                                          TPIC18x_MOVLW.Create (lsb(total_offset));
                                          TPIC18x_ADDWF.Create (2, dest_f, access_mode);
                                          TPIC18x_MOVLW.Create (msb(total_offset));
                                          TPIC18x_ADDWFC.Create (1, dest_f, access_mode)
                                       end;
                                    if indexed then
                                       begin
                                          add_total_indexed_offsets (ptr_size);
                                          if also_push_string_size then
                                             begin
                                                assert (node_attribute_string_typedef.known_strlen);  // can't have array of undimensioned strings
                                                TPIC18x_PUSHL.Create (TPIC18x_TypeInfo(node_attribute_string_typedef.info).Size and $ff).annotation := 'push maxstrlen';     // push 0 for size of 256
                                                StackUsageCounter.Push (1)
                                             end
                                       end
                                    else  // not indexed
                                       if also_push_string_size then
                                          begin
                                             if node_attribute_string_typedef.known_strlen then
                                                TPIC18x_PUSHL.Create (TPIC18x_TypeInfo(node_attribute_string_typedef.info).Size and $ff).annotation := 'push maxstrlen'     // push 0 for size of 256
                                             else
                                                TPIC18x_MOVFF.Create (POSTDEC0, POSTDEC2).annotation := 'push maxstrlen';
                                             StackUsageCounter.Push (1)
                                          end
                                 end
                           end;
                     else
                        assert (false)
                     end;
               else
                  assert (false, format('desc=%d', [ord(base_variable.descriptor)]))
               end;
            with_variable_definition:
               begin
                  case base_variable.descriptor of
                     rw_const,
                     rw_var,
                     rw_ioreg,
                     rw_for,
                     rw_rom:
                        case TWithVariable(path_start).record_field.definition_kind of
                           record_field_definition:
                              begin
                                 ptr_size := 2;
                                 total_offset := TPIC18x_RecordFieldInfo(TWithVariable(path_start).record_field.info).Offset + total_fixed_offsets + offset;
                                 if total_offset = 0 then
                                    begin
                                       TPIC18x_MOVF.Create (tos_relative_with_variable_address + 1, dest_w, access_mode).annotation := annotation;
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       TPIC18x_MOVF.Create (tos_relative_with_variable_address, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1)
                                    end
                                 else
                                    begin
                                       TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                       TPIC18x_ADDWF.Create (tos_relative_with_variable_address + 1, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1);
                                       TPIC18x_MOVLW.Create (msb(total_offset));
                                       TPIC18x_ADDWFC.Create (tos_relative_with_variable_address, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                                       StackUsageCounter.Push (1)
                                    end;
                                 add_total_indexed_offsets (ptr_size)
                              end;
                        else
                           assert (false)
                        end;
                     else
                        assert (false)
                     end;
                     push_string_size_if_needed
                  end;
         else
            assert (false)
         end
   end;    // Generate_Push_Address2_Code

procedure TPIC18x_Access.Generate_Load_Ptr2_Code (ptr: TPointer; offset: integer);
   const
      ptr_size = 2;
   var
      total_offset, i, j: integer;
      annotation: string;
      temp_ptr: TPointer;
   begin   // Generate_Load_Ptr2_Code
      if complex_index_calculations_needed then
         begin
            Generate_Push_Address2_Code (offset, false);
            TPIC18x_MOVFF.Create (PREINC2, ptrH(ptr));
            TPIC18x_MOVFF.Create (PREINC2, ptrL(ptr));
            StackUsageCounter.Pop (2)
         end
      else  // not indexed or only simple indexes
         begin
            case ptr of
               pFSR0:
                  if node_typedef = nil then
                     annotation := 'FSR0L := @' + path_src
                  else
                     annotation := 'FSR0L := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
               pFSR1:
                  if node_typedef = nil then
                     annotation := 'FSR1L := @' + path_src
                  else
                     annotation := 'FSR1L := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
               pTHIS:
                  if node_typedef = nil then
                     annotation := 'this_ptr := @' + path_src
                  else
                     annotation := 'this_ptr := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
               pTBLPTR:
                  if node_typedef = nil then
                     annotation := 'TBLPTR := @' + path_src
                  else
                     annotation := 'TBLPTR := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
            else
               assert (false)
            end;
            if node_access_kind in [function_access, procedure_access, property_access] then
               begin  // remove procedure name
                  i := Length(annotation);
                  while (i > 0)
                        and
                        (annotation [i] <> '.') do
                     i := i - 1;
                  if (i > 0)
                     and
                     (annotation[i] = '.')
                  then
                     i := i - 1;
                  SetLength (annotation, i)
               end;

            if path_start = nil then
               begin   // must be self: self.routine or self.property
                  if ptr <> pTHIS then
                     begin
                        TPIC18x_MOVFF.Create (ptrL(pTHIS), ptrL(ptr));
                        TPIC18x_MOVFF.Create (ptrH(pTHIS), ptrH(ptr))
                     end
               end
            else
               case path_start.definition_kind of
                  variable_definition:
                     case base_variable.address_mode of
                        absolute_address_mode:
                           begin
                              total_offset := base_variable.address + total_fixed_offsets + offset;
                              case ptr of
                                 pTHIS,
                                 pTBLPTR:
                                    begin
                                       case lsb(total_offset) of
                                          0: TPIC18x_CLRF.Create (ptrL(ptr), ram_access_mode(ptr)).annotation := annotation;
                                        255: TPIC18x_SETF.Create (ptrL(ptr), ram_access_mode(ptr)).annotation := annotation;
                                       else
                                          TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                          TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr))
                                       end;
                                       case msb(total_offset) of
                                          0: TPIC18x_CLRF.Create (ptrH(ptr), ram_access_mode(ptr));
                                        255: TPIC18x_SETF.Create (ptrH(ptr), ram_access_mode(ptr));
                                       else
                                          TPIC18x_MOVLW.Create (msb(total_offset));
                                          TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                                       end
                                    end;
                                 pFSR0,
                                 pFSR1:
                                    TPIC18x_LFSR.Create (fsr(ptr), total_offset).annotation := annotation;
                              else
                                 assert (false)
                              end
                           end;
                        system_type_address_mode:
                           begin
                              total_offset := base_variable.address + total_fixed_offsets + offset;
                              if total_offset = 0 then
                                 begin
                                    if ptr <> pTHIS then
                                       begin
                                          TPIC18x_MOVFF.Create (this_ptrL, ptrL(ptr)).annotation := annotation;
                                          TPIC18x_MOVFF.Create (this_ptrH, ptrH(ptr))
                                       end
                                 end
                              else  // total_offset <> 0
                                 case ptr of
                                    pFSR0,
                                    pFSR1:
                                       begin
                                          TPIC18x_MOVFF.Create (this_ptrL, ptrL(ptr)).annotation := annotation;
                                          TPIC18x_MOVFF.Create (this_ptrH, ptrH(ptr));
                                          adjust_fsr (ptr, total_offset)
                                       end;
                                    pTHIS:
                                       begin
                                          TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                          TPIC18x_ADDWF.Create (this_ptrL, dest_f, bank_mode);
                                          TPIC18x_MOVLW.Create (msb(total_offset));
                                          TPIC18x_ADDWFC.Create (this_ptrH, dest_f, bank_mode)
                                       end;
                                    pTBLPTR:
                                       begin
                                          TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                          TPIC18x_ADDWF.Create (this_ptrL, dest_w, bank_mode);
                                          TPIC18x_MOVWF.Create (TBLPTRL, ram_access_mode(ptr));
                                          TPIC18x_MOVLW.Create (msb(total_offset));
                                          TPIC18x_ADDWFC.Create (this_ptrH, dest_w, bank_mode);
                                          TPIC18x_MOVWF.Create (TBLPTRH, ram_access_mode(ptr))
                                       end;
                                 else
                                    assert (false)
                                 end
                           end;
                        system_type_indirect_address_mode:
                           begin
                              total_offset := total_fixed_offsets + offset;
                              if ptr = pFSR1 then
                                 temp_ptr := pFSR0
                              else
                                 temp_ptr := pFSR1;
                              TPIC18x_MOVFF.Create (this_ptrH, ptrH(temp_ptr)).annotation := annotation;
                              TPIC18x_MOVFF.Create (this_ptrL, ptrL(temp_ptr));
                              adjust_fsr (temp_ptr, base_variable.address);
                              if total_offset = 0 then
                                 begin
                                    TPIC18x_MOVFF.Create (postinc(temp_ptr), ptrH(ptr));
                                    TPIC18x_MOVFF.Create (postinc(temp_ptr), ptrL(ptr))
                                 end
                              else  // total_offset <> 0
                                 case ptr of
                                    pFSR0,
                                    pFSR1:
                                       begin
                                          TPIC18x_MOVFF.Create (postinc(temp_ptr), ptrH(ptr));
                                          TPIC18x_MOVFF.Create (postinc(temp_ptr), ptrL(ptr));
                                          adjust_fsr (ptr, total_offset)
                                       end;
                                    pTHIS,
                                    pTBLPTR:
                                       begin
                                          TPIC18x_MOVFF.Create (postinc(temp_ptr), ptrH(ptr));
                                          TPIC18x_MOVLW.Create (lsb(total_offset));
                                          TPIC18x_ADDWF.Create (postinc(temp_ptr), dest_w, access_mode);
                                          TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                                          TPIC18x_MOVLW.Create (msb(total_offset));
                                          TPIC18x_ADDWFC.Create (ptrH(ptr), dest_f, ram_access_mode(ptr))
                                       end;
                                 else
                                    assert (false)
                                 end
                           end;
                        local_address_mode:
                           load_ptr (pFSR2, base_variable.address + total_fixed_offsets + offset + StackUsageCounter.Current, ptr);
                        local_indirect_address_mode:
                           begin
                              total_offset := total_fixed_offsets + offset;
                              if total_offset = 0 then
                                 if base_variable.address+StackUsageCounter.Current+1 <= $5F then
                                    begin
                                       TPIC18x_MOVF.Create (base_variable.address+StackUsageCounter.Current+1, dest_w, access_mode).annotation := annotation;
                                       TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                                       TPIC18x_MOVF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                                    end
                                 else  // base_variable.address+StackUsageCounter.Current+1 > $5F
                                    begin
                                       if ptr = pFSR0 then
                                          temp_ptr := pFSR1
                                       else
                                          temp_ptr := pFSR0;
                                       load_ptr (pFSR2, base_variable.address+StackUsageCounter.Current, temp_ptr).annotation := annotation;
                                       case temp_ptr of
                                          pFSR0:
                                             begin
                                                TPIC18x_MOVFF.Create (POSTINC0, ptrH(ptr));
                                                TPIC18x_MOVFF.Create (POSTINC0, ptrL(ptr))
                                             end;
                                          pFSR1:
                                             begin
                                                TPIC18x_MOVFF.Create (POSTINC1, ptrH(ptr));
                                                TPIC18x_MOVFF.Create (POSTINC1, ptrL(ptr))
                                             end;
                                       else
                                          assert (false)
                                       end
                                    end
                              else  // total_offset <> 0
                                 if base_variable.address+StackUsageCounter.Current+1 <= $5F then
                                    begin
                                       TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                       TPIC18x_ADDWF.Create (base_variable.address+StackUsageCounter.Current+1, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                                       TPIC18x_MOVLW.Create (msb(total_offset));
                                       TPIC18x_ADDWFC.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                                    end
                                 else  // base_variable.address+StackUsageCounter.Current+1 > $5F
                                    begin
                                       if ptr = pFSR0 then
                                          temp_ptr := pFSR1
                                       else
                                          temp_ptr := pFSR0;
                                       TPIC18x_MOVLW.Create (lsb(base_variable.address+StackUsageCounter.Current)).annotation := annotation;
                                       TPIC18x_ADDWF.Create (FSR2L, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (ptrL(temp_ptr), access_mode);
                                       TPIC18x_MOVLW.Create (msb(base_variable.address+StackUsageCounter.Current));
                                       TPIC18x_ADDWFC.Create (FSR2H, dest_w, access_mode);
                                       TPIC18x_MOVWF.Create (ptrH(temp_ptr), access_mode);
                                       case temp_ptr of
                                          pFSR0:
                                             begin
                                                TPIC18x_ADDFSR.Create (0, 1);
                                                TPIC18x_MOVLW.Create (lsb(total_offset));
                                                TPIC18x_ADDWF.Create (POSTDEC0, dest_w, access_mode);
                                                TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                                                TPIC18x_MOVLW.Create (msb(total_offset));
                                                TPIC18x_ADDWFC.Create (POSTDEC0, dest_w, access_mode);
                                                TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                                             end;
                                          pFSR1:
                                             begin
                                                TPIC18x_ADDFSR.Create (1, 1);
                                                TPIC18x_MOVLW.Create (lsb(total_offset));
                                                TPIC18x_ADDWF.Create (POSTDEC1, dest_w, access_mode);
                                                TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                                                TPIC18x_MOVLW.Create (msb(total_offset));
                                                TPIC18x_ADDWFC.Create (POSTDEC1, dest_w, access_mode);
                                                TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                                             end;
                                       else
                                          assert (false)
                                       end
                                    end
                           end;
                     else
                        assert (false)
                     end;

                  with_variable_definition:
                     begin
                        total_offset := 0;  // to suppress compiler warning
                        case TWithVariable(path_start).record_field.definition_kind of
                           record_field_definition:
                              total_offset := TPIC18x_RecordFieldInfo(TWithVariable(path_start).record_field.info).Offset + total_fixed_offsets + offset;
                           packed_record_field_definition:
                              total_offset := total_fixed_offsets + offset;
                        else
                           assert (false)
                        end;
                        if total_offset = 0 then
                           begin
                              TPIC18x_MOVF.Create (tos_relative_with_variable_address + 1, dest_w, access_mode).annotation := annotation;
                              TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                              TPIC18x_MOVF.Create (tos_relative_with_variable_address, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                           end
                        else
                           begin
                              TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                              TPIC18x_ADDWF.Create (tos_relative_with_variable_address + 1, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                              TPIC18x_MOVLW.Create (msb(total_offset));
                              TPIC18x_ADDWFC.Create (tos_relative_with_variable_address, dest_w, access_mode);
                              TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                           end
                     end;

                  with_routine_definition,
                  with_property_definition:
                     begin
                        TPIC18x_MOVLW.Create ($3F).annotation := annotation;
                        TPIC18x_ADDWF.Create (tos_relative_with_variable_address + 1, dest_w, access_mode);
                        TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr));
                        TPIC18x_MOVLW.Create (0);
                        TPIC18x_ADDWFC.Create (tos_relative_with_variable_address, dest_w, access_mode);
                        TPIC18x_MOVWF.Create (ptrH(ptr), ram_access_mode(ptr))
                     end;

               else
                  assert (false)
               end;

            j := 1;
            for i := 0 to Length(path)-1 do
               if simple_index_calculation(i) then
                  begin
                     add_simple_indexed_offset2 (ptr, i).annotation := 'add index#' + IntToStr(j);
                     j := j + 1
                  end
         end
   end;   // Generate_Load_Ptr2_Code

procedure TPIC18x_Access.Generate_Load_Ptr1_Code (ptr: TPointer; offset: integer);
   const
      ptr_size = 1;  // always ram ptr
   var
      total_offset, i, j: integer;
      annotation: string;
   begin   // Generate_Load_Ptr1_Code
      assert (base_variable.descriptor in [rw_eeprom]);
      assert (ptr in [pFSR0, pFSR1]);
      if complex_index_calculations_needed then
         begin
            Generate_Push_Address1_Code (offset, false);
            TPIC18x_MOVFF.Create (PREINC2, ptrL(ptr));
            StackUsageCounter.Pop (1)
         end
      else  // not indexed or only simple indexes
         begin
            case ptr of
               pFSR0:
                  if node_typedef = nil then
                     annotation := 'FSR0L := @' + path_src
                  else
                     annotation := 'FSR0L := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
               pFSR1:
                  if node_typedef = nil then
                     annotation := 'FSR1L := @' + path_src
                  else
                     annotation := 'FSR1L := @' + path_src + '.b' + IntToStr(TPIC18x_TypeInfo(node_typedef.info).Size-1-offset);
            else
               assert (false)
            end;

            case path_start.definition_kind of
               variable_definition:
                  case base_variable.address_mode of
                     system_type_address_mode:
                        begin
                           TPIC18x_MOVFF.Create (this_ptrL, ptrL(ptr)).annotation := annotation;
                           TPIC18x_MOVFF.Create (this_ptrH, ptrH(ptr));
                           case TSystemType(base_variable.context).system_type_kind of
                              class_system_type:
                                 TPIC18x_MOVLW.Create ((-$3F + class_eeprom_base_addr) and $FF);
                              monitor_system_type:
                                 TPIC18x_MOVLW.Create ((-$3F + monitor_eeprom_base_addr) and $FF);
                              process_system_type:
                                 TPIC18x_MOVLW.Create ((-$3F + process_eeprom_base_addr) and $FF);
                           else
                              assert (false)
                           end;
                           TPIC18x_MOVF.Create (plusw(ptr), dest_w, access_mode);
                           total_offset := base_variable.address + total_fixed_offsets + offset;
                           if total_offset <> 0 then
                              TPIC18x_ADDLW.Create (lsb(total_offset));
                           TPIC18x_MOVWF.Create (ptrL(ptr), access_mode)
                        end;
                     local_indirect_address_mode:
                        begin
                           total_offset := total_fixed_offsets + offset;
                           if total_offset = 0 then
                              if base_variable.address+StackUsageCounter.Current <= $5F then
                                 begin
                                    TPIC18x_MOVF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode).annotation := annotation;
                                    TPIC18x_MOVWF.Create (ptrL(ptr), access_mode)
                                 end
                              else
                                 begin
                                    load_ptr (pFSR2, base_variable.address+StackUsageCounter.Current, pFSR1).annotation := annotation;
                                    TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                                    TPIC18x_MOVWF.Create (ptrL(ptr), access_mode)
                                 end
                           else  // total_offset <> 0
                              if base_variable.address+StackUsageCounter.Current <= $5F then
                                 begin
                                    TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                                    TPIC18x_ADDWF.Create (base_variable.address+StackUsageCounter.Current, dest_w, access_mode);
                                    TPIC18x_MOVWF.Create (ptrL(ptr), access_mode)
                                 end
                              else
                                 begin
                                    load_ptr (pFSR2, base_variable.address+StackUsageCounter.Current, pFSR1).annotation := annotation;
                                    TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
                                    TPIC18x_ADDLW.Create (lsb(total_offset));
                                    TPIC18x_MOVWF.Create (ptrL(ptr), access_mode)
                                 end
                        end;
                  else
                     assert (false)
                  end;

               with_variable_definition:
                  begin
                     total_offset := 0;  // to suppress compiler warning
                     case TWithVariable(path_start).record_field.definition_kind of
                        record_field_definition:
                           total_offset := TPIC18x_RecordFieldInfo(TWithVariable(path_start).record_field.info).Offset + total_fixed_offsets + offset;
                        packed_record_field_definition:
                           total_offset := total_fixed_offsets + offset;
                     else
                        assert (false)
                     end;
                     if total_offset = 0 then
                        begin
                           TPIC18x_MOVF.Create (tos_relative_with_variable_address, dest_w, access_mode).annotation := annotation;
                           TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr))
                        end
                     else
                        begin
                           TPIC18x_MOVLW.Create (lsb(total_offset)).annotation := annotation;
                           TPIC18x_ADDWF.Create (tos_relative_with_variable_address, dest_w, access_mode);
                           TPIC18x_MOVWF.Create (ptrL(ptr), ram_access_mode(ptr))
                        end
                  end;
            else
               assert (false)
            end;

            j := 1;
            for i := 0 to Length(path)-1 do
               if simple_index_calculation(i) then
                  begin
                     add_simple_indexed_offset1 (ptr, i).annotation := 'add index#' + IntToStr(j);
                     j := j + 1
                  end
         end
   end;    // Generate_Load_Ptr1_Code

function TPIC18x_Access.absolute_address_with_no_indexing_required_mode: boolean;
   begin
      result :=
         (path_start.definition_kind = variable_definition)     // not with var
         and
         (base_variable.address_mode = absolute_address_mode)
         and
         (not indexed)
         and
         ((absolute_address(TPIC18x_TypeInfo(node_typedef.info).Size-1) <= $FF)   // will fit in bank0
          or
          (absolute_address(0) >= pic_info.first_access_bank_absolute_address)
         )
   end;

function TPIC18x_Access.absolute_address (offset: integer): integer;
   begin
      assert (base_variable.descriptor in [rw_ioreg, rw_var]);
      assert (base_variable.address_mode = absolute_address_mode);
      assert (not indexed);
      result := base_variable.address + total_fixed_offsets + offset
   end;

function TPIC18x_Access.directly_addressable_absolute_address: boolean;
   begin
      result := (base_variable.descriptor in [rw_ioreg, rw_var])
                and
                (base_variable.address_mode = absolute_address_mode)
                and
                (path_start.definition_kind = variable_definition)     // not a "with var"
                and
                (not indexed)
                and
                ((base_variable.address + total_fixed_offsets + TPIC18x_TypeInfo(node_typedef.info).size - 1 <= $0FF)  // first thrug last byte is in bank 0
                 or
                 (base_variable.address + total_fixed_offsets >= pic_info.first_access_bank_absolute_address)                   // first byte is in access bank
                )
   end;

function TPIC18x_Access.indirectly_addressable_absolute_address: boolean;
   begin
      result := (base_variable.descriptor in [rw_ioreg, rw_var])
                and
                (base_variable.address_mode = absolute_address_mode)
                and
                (path_start.definition_kind = variable_definition)     // not a "with var"
                and
                (not indexed)
   end;

function TPIC18x_Access.near_stack_address_with_no_indexing_required_mode6 (offset: integer): boolean;
   begin
      result :=
         (path_start.definition_kind = variable_definition)    // not with var
         and
         (base_variable.address_mode = local_address_mode)
         and
         (not indexed)
         and
         (near_stack_address(offset) <= $5F)
   end;

function TPIC18x_Access.near_stack_address (offset: integer): integer;
   begin
      assert (base_variable.address_mode = local_address_mode);
      assert (not indexed);
      result := base_variable.address + total_fixed_offsets + offset + StackUsageCounter.Current
   end;

function TPIC18x_Access.is_overlay_variable_needing_range_check: boolean;
   var
      tinfo: TPIC18x_TypeInfo;
   begin
      result := false;  // provisional
      if not node_initialization_assumption_invalid then
         exit;

      if not node_typedef.IsOrdinal then
         exit;

      tinfo := TPIC18x_TypeInfo(node_typedef.info);

      temp.SetMaxUnsignedValue (tinfo.size);
      if tinfo.min_value.eq(0) and tinfo.max_value.eq(temp) then
         exit;  // any combination of bits gives a valid unsigned value

      temp.SetMaxSignedValue (tinfo.size);
      if tinfo.max_value.eq(temp) then
         begin
            temp.SetMinSignedValue (tinfo.size);
            if tinfo.min_value.eq(temp) then
               exit   // any combination of bits gives a valid signed value
         end;

      result := true   // will need range check
   end;

function TPIC18x_Access.node_is_in_data_address_space: boolean;
   begin
      result := not (base_variable.descriptor in [rw_rom, rw_eeprom])
   end;

function TPIC18x_Access.node_is_single_bit_data_packed_field: boolean;
   begin
      result :=
         node_is_packed_field
         and
         (node_typedef.info.PackedSizeInBits = 1)
         and
         node_is_in_data_address_space
   end;


INITIALIZATION
   temp := TMultiPrecisionInteger.Create;

FINALIZATION
   temp.Free;

END.
