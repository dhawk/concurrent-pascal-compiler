UNIT cpc_common_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
  Classes,
  cpc_source_analysis_unit,
  SysUtils;

const
   err_all_ioreg_overlay_type_packed_records_must_be_same_width = 'all ioreg overlay type packed records must be same width';
   err_ambiguous_first_index_base_type_for_anonymous_array = 'ambiguous first index base type for anonymous array';
   err_an_instance_of_this_interrupt_type_already_exists = 'an instance of this interrupt type already exists';
   err_array_may_not_contain_process_variables = 'array may not contain process variables';
   err_procedure_address_not_alowed = 'procedure address not alowed';
   err_property_address_not_alowed = 'property address not alowed';
   err_function_address_not_alowed = 'function address not alowed';
   err_compile_time_constant_address_not_alowed = 'compile time constant address not alowed';
   err_cant_call_public_routine_of_same_monitor_type_here = 'can''t call public routine of same monitor type here';
   err_assignment_operator_expected = '":=" expected';
   err_at_expected = '"at" expected';
   err_at_only_allowed_for_ioregisters = '"at" only allowed for ioregisters';
   err_await_interrupt_statement_must_be_in_process_code = 'await interrupt statement cannot be called outside of a process';
   err_begin_expected = 'begin expected';
   err_boolean_constant_expected = 'boolean constant expected';
   err_boolean_expected = 'boolean expected';
   err_boolean_expression_expected = 'boolean expression expected';
   err_both_operand_sets_must_be_of_same_type = 'both operand sets must be of same type';
   err_cannot_assign_to_kernel_variable = 'cannot assign to kernel variable (read only)';
   err_cannot_assign_to_queue_type = 'cannot assign to queue type';
   err_cannot_assign_to_system_type = 'cannot assign to system type';
   err_cannot_change_constant_parameter = 'cannot change constant parameter';
   err_cannot_change_for_loop_control_variable = 'cannot change for loop control variable';
   err_cannot_change_rom_constant = 'cannot change rom constant';
   err_cant_call_delay_from_an_interrupt_process = 'can''t indirectly call delay from within an interrupt process';
   err_cant_call_delay_from_initial_process = 'can''t indirectly call delay during program initialization';
   err_cant_open_include_file = 'can''t open include file: ''%s''';
   err_cant_open_source_file = 'can''t open source file: ''%s''';
   err_case_label_expected = 'case label expected';
   err_char_constant_expected = 'char constant expected';
   err_char_expected = 'char expected';
   err_char_expression_expected = 'char expression expected';
   err_class_type_not_allowed = 'class type not allowed';
   err_class_variable_not_allowed_as_routine_local_variable = 'class variable not allowed as routine local variable';
   err_closing_curly_bracket_required_for_compiler_directive = 'closing bracket "}" required for compiler directive';
   err_closing_quote_required_for_s_name = 'closing quote ('') required for %s name';
   err_colon_expected = '":" expected';
   err_colon_expected_for_anonymous_structured_constant = '":" expected for anonymous structured constant';
   err_comma_expected = 'comma expected';
   err_comma_not_allowed = 'comma not allowed';
   err_comma_or_right_bracket_expected = '"," or "]" expected';
   err_comment_use_double_slash_comment_style = '{comment style} not suspported, use double slash // comment style instead';
   err_compiler_directive_must_start_in_column_1 = 'compiler directive must start in column 1';
   err_compiler_directives_must_be_at_beginning_of_source = 'compiler directives must be at beginning of source';
   err_constant_cant_be_used_as_parameter_here = 'constant can''t be used as a parameter here';
   err_constant_expected = 'constant expected';
   err_constant_index_expression_expected = 'constant index expression expected';
   err_constant_not_allowed_here = 'constant not allowed here';
   err_constant_string_cannot_be_assigned_to_a_property = 'constant string can''t be assigned to a property';
   err_constant_value_outside_subrange = 'constant value outside subrange';
   err_continue_can_only_be_called_from_a_monitor_entry_routine = 'continue can only be called from a monitor entry routine';
   err_divide_by_zero = 'divide by zero';
   err_division_by_zero = 'division by zero';
   err_divisor_will_always_be_0 = 'mod operator''s right opererand will always be 0';
   err_divisor_will_never_be_ge_1 = 'mod operator''s right opererand will never be >= 1';
   err_do_expected = '"do" expected';
   err_dot_dot_expected = '".." expected';
   err_duplicate_case_label = 'duplicate case label';
   err_duplicate_field_name = 'duplicate field name';
   err_duplicate_overlay_name = 'introduces duplicate overlay name "%s"';
   err_eeprom_parameter_only_allowed_with_eeprom_parameter_descriptor = 'eeprom parameter only allowed with "eeprom" parameter descriptor';
   err_eeprom_parameters_not_allowed_here = 'eeprom parameters not allowed here';
   err_eeprom_string_cannot_be_assigned_to_a_property = 'eeprom string can''t be assigned to a property';
   err_eeprom_variable_expected = 'EEPROM variable expected';
   err_eeprom_variables_not_allowed_in_interrupt_definitions = 'eeprom variables not allowed in interrupt definitions';
   err_eeprom_variables_only_allowed_as_system_type_permanent_variables = 'eeprom variables only allowed as system type permanent variables';
   err_empty_file = 'source file is empty or contains no program source';
   err_empty_overlay_not_allowed = 'empty overlay not allowed';
   err_end_expected = '"end" expected';
   err_public_only_allowed_in_classes_and_monitors = '"public" only allowed in classes and monitors';
   err_end_of_source_expected = 'unexpected tokens after final "." of program';
   err_enum_constant_expected = 'enum constant expected';
   err_enum_expression_expected = 'enum expression expected';
   err_enum_value_expected = 'enum value expected';
   err_enumerated_value_must_be_greater_than_previous = 'enumerated value must be greater than previous';
   err_equals_expected = '"=" expected';
   err_exitloop_only_allowed_inside_loop = '"exitloop" only allowed inside loop';
   err_expression_contains_constant_outside_allowable_range = 'expression contains constant outside allowable range of set';
   err_expression_expected = 'expression expected';
   err_expression_value_outside_legal_range = 'expression value outside legal range';
   err_no_public_procedures_functions_or_properties_defined = 'no public procedures functions or properties defined';
   err_field_identifier_expected = 'field identifier expected';
   err_final_cycle_statement_must_be_empty = 'final cycle statement must be empty';
   err_final_period__expected = 'final "." of program expected';
   err_first_subrange_value_greater_than_last_subrange_value = ' first subrange value greater than last subrange value';
   err_for_loop_control_variable_must_be_local_variable = 'for loop control variable must be a local variable';
   err_function_cannot_be_called_without_using_function_result = 'function cannot be called without using function result';
   err_function_has_no_parameters = 'function has no parameters';
   err_function_result_must_be_simple_type = 'function result must be simple type';
   err_global_variable_not_accessible_here = 'global variable not accessible here';
   err_local_type_not_accessible_to_caller_of_public_routine = 'local type not accessible to caller of public routine';
   err_anonymous_type_can_only_be_a_constant_parameter = 'anonymous type can only be a constant parameter';
   err_greater_than_expected = '">" expected';
   err_identifier_already_defined = 'identifier already defined in this scope';
   err_identifier_expected = 'identifier expected';
   err_identifier_must_be_for_a_constant = 'identifier must be for a constant';
   err_identifier_must_be_for_a_numeric_constant = 'err_identifier_must_be_for_a_numeric_constant';
   err_illegal_type_for_rom_constant = 'illegal type for rom constant';
   err_illegal_type_for_structured_constant = 'illegal type for structured constant';
   err_illegal_use_of_property_name_in_property_setter_block = 'the newly assigned value to a property is read-only within the property setter block';
   err_improper_hex_constant = 'improper hex constant';
   err_improper_real_const = 'improper real constant';
   err_include_file_doesnt_exist = 'include file doesn''t exist: ''%s''';
   err_incompatible_enum_type = 'incompatible enum type';
   err_incompatible_operand_types = 'incompatible operand types';
   err_incomplete_type_definition_for_variable = 'incomplete type definition for variable';
   err_incorrect_type = 'incorrect type';
   err_index_expression_expected = 'index expression expected';
   err_index_signature_does_not_match_any_overlay_variable_anonymous_signature = 'index signature does not match any overlay variable anonymous array index signature';
   err_index_type_not_compatable_with_array_index_type_definition = 'index type not compatable with array index type definition';
   err_initialization_not_allowed = 'INITIALIZATION not allowed';
   err_initialization_required = 'INITIALIZATION required';
   err_integer_constant_expected = 'integer constant expected';
   err_integer_expected = 'integer expected';
   err_integer_expression_expected = 'integer expression expected';
   err_integer_subrange_expected = 'integer subrange expected';
   err_interrupt_definition_must_implement_signaled_function = 'interrupt definition must implement signaled function';
   err_interrupt_expected = '"interrupt" expected';
   err_interrupt_identifier_expected = 'interrupt identifier expected';
   err_interrupt_priority_must_be_greater_than_0 = 'interrupt priority must be > 0';
   err_interrupt_process_priority_not_positive = 'interrupt process priority must be a positive number';
   err_interrupt_specification_expected_for_interrrupt_process = 'interrupt specification expected for interrrupt process';
   err_interrupt_variable_already_assigned_to_another_process = 'interrupt variable already assigned to another process';
   err_interrupt_variable_expected = 'interrupt variable expected';
   err_interrupt_variable_parameter_must_match_interrupt_assigned_to_process = 'only the interrupt variable attached to this process (%s) may be passed as a parameter';
   err_invalid_address = 'invalid address';
   err_invalid_anonymous_type_for_overlay = 'invalid anonymous type for overlay';
   err_invalid_char = 'invalid character';
   err_invalid_compiler_directive = 'invalid compiler directive';
   err_invalid_enumeration_definition = 'invalid enumeration definition';
   err_invalid_exponent = 'invalid exponent';
   err_invalid_identifier = 'invalid identifier';
   err_invalid_ioregister_address = 'invalid ioregister address (out of range)';
   err_invalid_operator = 'invalid operator';
   err_invalid_operator_for_operand = 'invalid operator for operand(s)';
   err_invalid_process_priority = 'invalid process priority';
   err_invalid_set_member_type = 'invalid set member type';
   err_invalid_string_attribute = 'invalid string attribute';
   err_invalid_total_ioregister_width = 'contains packed record with invalid total ioregister width';
   err_invalid_width_value = 'invalid width value (must be > 0)';
   err_ioreg_overlaid_variables_must_all_be_packed_records = 'ioreg overlaid variables must all be packed records';
   err_ioregister_expected = 'IO Register expected';
   err_ioregister_not_allowed = 'ioregister not allowed';
   err_ioregister_packed_record_enums_must_be_specified = 'invalid ioregister type: packed record''s enum values must be fully specified';
   err_ioregisters_must_be_declared_at_global_level = 'ioregisters must be declared at global level';
   err_last_statement_of_process_must_be_cycle_repeat = 'the last statement of a process block must be a cycle/repeat statment';
   err_left_and_right_operand_types_dont_agree = 'operand types don''t agree';
   err_left_bracket_expected = '"[" expected';
   err_left_element_must_be_ordinal = 'left element must be ordinal';
   err_left_operand_must_be_boolean = 'left operand must be boolean';
   err_left_operand_must_be_integer = 'left operand must be integer';
   err_left_operand_must_be_number = 'left operand must be number';
   err_left_operand_of_in_expression_must_be_ordinal = 'left operand of in expression must be ordinal';
   err_left_parenthesis_expected = 'left parenthesis expected';
   err_left_value_must_be_less_than_right_value = 'left value must be less than right value';
   err_less_than_first_of_range = 'less than first of range';
   err_loop_control_variable_expected = 'loop control variable expected';
   err_loop_control_variable_must_be_ordinal = 'loop control variable must be ordinal';
   err_maxstrlen_attribute_is_read_only = 'maxstrlen attribute is read only';
   err_monitor_variable_not_allowed_as_routine_local_variable = 'monitor variable not allowed as routine local variable';
   err_more_than_256_constants_in_enum = 'more than 256 constants in enum';
   err_multiple_interrupt_process_variables_not_allowed_in_same_variable_list = 'multiple interrupt process variables not allowed in same variable list';
   err_multiple_items_not_allowed_here = 'multiple items not allowed here';
   err_no_parameters_expected = 'no parameters expected';
   err_null_chars_not_allowed_in_sets = 'null chars not allowed in sets';
   err_number_expected = 'number expected';
   err_numeric_constant_expected = 'err_numeric_constant_expected';
   err_numeric_expression_expected = 'numeric expression expected';
   err_of_expected = '"of" expected';
   err_on_or_off_expected = '"on" or "off" expected';
   err_only_ioregister_types_allowed_in_ioreg_section = 'only ioregister types allowed in ioreg section';
   err_only_one_routine_allowed_in_interrupt_definition = 'only one routine (signaled) is allowed in interrupt definition';
   err_only_single_char_allowed = 'only single char allowed';
   err_only_single_chars_allowed_in_sets = 'only single chars allowed insets';
   err_opening_quote_required_for_s_name = 'opening quote ('') required for %s name';
   err_operands_not_compatible_with_operator = 'operands not compatible with operator';
   err_operator_not_valid_for_sets = 'operator not valid for sets';
   err_operator_requires_unsigned_operands = 'operator requires unsigned operands';
   err_ord_argument_must_be_ordinal = 'ord argument must be ordinal';
   err_ordinal_expression_expected = 'ordinal expression expected';
   err_ordinal_type_expected = 'ordinal type expected';
   err_ordinal_value_wont_fit_in_field_width = 'ordinal value won''t fit in field width';
   err_out_of_order_index = 'index out of order: "%s" expected';
   err_overlay_constant_not_allowed = 'overlay constant not allowed';
   err_overlay_may_not_contain_class_variables = 'overlay may not contain class variables';
   err_overlay_may_not_contain_monitor_variables = 'overlay may not contain monitor variables';
   err_overlay_may_not_contain_process_variables = 'overlay may not contain process variables';
   err_overlay_may_not_contain_queue_variables = 'overlay may not contain queue variables';
   err_overlay_variable_expected = 'overlay variable expected';
   err_packed_record_enum_not_allowed_here = 'packed record enum not allowed here';
   err_packed_record_field_type_definition_expected = 'packed record field type definition expected';
   err_packed_record_larger_than_64_bits = 'packed record larger than 64 bits';
   err_packed_record_type_expected = 'packed record or overlay type expected';
   err_packed_record_type_expected2 = 'packed record, overlay or bit type expected';
   err_paramters_for_interrupt_definition_not_allowed = 'paramters for interrupt definition not allowed';
   err_positive_priority_process_must_call_await_interrupt = 'interrupt process (positive priority) must call "await interrupt"';
   err_priority_expected = '"priority" expected';
   err_procedure_has_no_parameters = 'procedure has no parameters';
   err_procedure_has_no_result = 'procedure has no result (should it be a function?)';
   err_procedures_not_allowed_in_interrupt_definitions = 'procedures not allowed in interrupt definitions';
   err_process_priority_must_be_matched_to_interrupt_priority = 'process priority must be matched to interrupt priority';
   err_process_variable_not_allowed_here = 'process variable not allowed here';
   err_processor_does_not_support_eeprom = 'this processor does not have eeprom';
   err_properties_not_allowed_in_interrupt_definitions = 'properties not allowed in interrupt definitions';
   err_properties_not_allowed_in_processes = 'properties not allowed in processes';
   err_property_cant_be_used_as_parameter_here = 'property can''t be used as parameter here';
   err_property_getter_already_defined = 'property getter already defined';
   err_property_has_no_setter = 'property is read only (has no setter)';
   err_property_must_be_marked_entry = 'property must be marked entry';
   err_property_must_be_simple_type = 'property must be simple type';
   err_property_or_routine_identifier_expected = 'property or routine identifier expected';
   err_no_loop_exit_defined = 'no loop exit defined (need while, until or exitloop statement)';
   err_property_setter_already_defined = 'property setter already defined';
   err_queue_param_must_be_var = 'queue param must be passed as var';
   err_queue_parameters_only_allowed_in_monitor_local_routines = 'queue parameters only allowed in monitor local routines';
   err_queue_type_not_allowed_here = 'queue type not allowed here';
   err_queue_variable_expected = 'queue variable expected';
   err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor = 'queue variables may only be declared in the permanent variables of a monitor';
   err_ram_variable_expected = 'RAM variable expected';
   err_real_divisor_cant_be_used_with_div_or_mod = 'real divisor can''t be used with div or mod';
   err_record_constant_expected = 'record constant expected';
   err_record_expected = '"record" expected';
   err_record_may_not_contain_process_variables = 'record may not contain process variables';
   err_record_or_class_or_monitor_variable_expected = 'record, overlay, class or monitor variable expected';
   err_recursive_call_not_allowed = 'recusive call not allowed';
   err_recursive_call_not_allowed_use_result_instead = 'recursive call not allowed (use "result" instead?)';
   err_recursive_include_file = 'recursive include file';
   err_recycle_only_allowed_inside_cycle = 'recycle only allowed inside cycle';
   err_reloop_only_allowed_inside_loop = '"reloop" only allowed inside loop';
   err_while_only_allowed_inside_loop = '"while" only allowed inside loop';
   err_unknown_compiler_flag = 'unknown compiler flag';
   err_until_only_allowed_inside_loop = '"until" only allowed inside loop';
   err_repeat_expected = '"repeat" expected';
   err_result_will_be_out_of_range = 'result will be out of range';
   err_right_bracket_expected = '"]" expected';
   err_right_operand_must_be_a_set = 'right operand must be a set';
   err_right_operand_must_be_boolean = 'right operand must be boolean';
   err_right_operand_must_be_integer = 'right operand must be integer';
   err_right_operand_must_be_number = 'right operand must be number';
   err_right_operand_must_be_string = 'right operand must be string';
   err_right_operand_of_in_expression_must_be_set = 'right operand of in expression must be set';
   err_right_parenthesis_expected = 'right parenthesis expected';
   err_rom_constant_expected = 'ROM constant expected';
   err_rom_constant_only_allowed_with_rom_parameter_descriptor = 'rom constant only allowed with "rom" parameter descriptor';
   err_rom_string_cannot_be_assigned_to_a_property = 'rom string can''t be assigned to a property';
   err_routine_cant_be_used_as_parameter_here = 'routine can''t be used as parameter here';
   err_routine_must_be_marked_entry = 'routine must be marked entry';
   err_self_referential_type = 'a type definition may not reference itself';
   err_while_or_until_already_defined = 'while or until already defined for this loop';
   err_semicolon_expected = '";" expected';
   err_while_not_allowed_here = '"while" only allowed in loop statement list';
   err_until_not_allowed_here = '"until" only allowed in loop statement list';
   err_semicolon_not_allowed_here_before_parenthesis = '";" here should be omitted before ")"';
   err_semicolon_or_end_expected = '";" or "end" expected';
   err_semicolon_or_repeat_expected = '";" or "repeat" expected';
   err_set_constant_has_members_outside_specified_range = 'set constant has members outside specified range';
   err_set_constant_is_of_wrong_type = 'set constant is of wrong type';
   err_set_element_type_differs_from_previous_set_elements = 'set element type differs from previous set elements';
   err_set_expected = 'set expected';
   err_set_expression_expected = 'set expression expected';
   err_set_is_of_wrong_type = 'set is of wrong type';
   err_set_member_type_differs_from_previous = 'err_set_member_type_differs_from_previous';
   err_signaled_function_required = '"signaled" function required';
   err_signaled_function_result_must_be_boolean = 'signaled function result must be boolean';
   err_source_file_doesnt_exist = 'source file doesn''t exist: ''%s''';
   err_specific_field_identifer_expected = 'field identifier "%s" expected';
   err_specified_enumeration_not_allowed_as_array_index = 'enumeration with specified values not allowed as array index';
   err_statement_expected = 'statement expected';
   err_strappend_only_allowed_for_var_or_eeprom_variables = 'strappend only allowed for var or eeprom variables';
   err_string_constant_multi_line = 'invalid string constant, must be all on same line.';
   err_string_exceeds_max_length = 'string exceeds max length';
   err_string_expected = 'string expected';
   err_string_expression_expected = 'string expression expected';
   err_string_function_result_cannot_be_assigned_to_a_property = 'string function result or property cannot be assigned to a property';
   err_string_function_result_may_not_be_assigned_to_eeprom_variable = 'string function result or property may not be assigned to eeprom variable';
   err_zero_length_string_not_allowed = 'zero length string not allowed';
   err_string_length_must_be_less_than_256 = 'string length must be < 256';
   err_string_length_must_be_non_negative = 'string length must be non-negative';
   err_string_plus_operator_only_for_constants = 'string + operator only allowed for for constant expressions (use StrAppend instead)';
   err_string_property_must_be_undimensioned = 'string property must be undimensioned';
   err_string_variable_or_constant_expected = 'string variable or constant expected';
   err_subrange_definition_expected = 'subrange definition expected';
   err_subrange_exceeds_CPU_ordinal_range = 'subrange exceeds CPU ordinal range';
   err_subrange_first_and_last_must_be_of_same_type = 'subrange first and last must be of same type';
   err_subrange_must_be_single_char = 'subrange must be single char';
   err_subrange_types_must_be_ordinal = 'subrange types must be ordinal';
   err_system_type_parameter_must_be_constant_parameter = 'system type parameter must be a constant parameter';
   err_system_type_parameters_are_private_to_the_system_type = 'system type parameters are private to the system type';
   err_system_type_variable_can_only_be_initialized_in_the_initial_statment_of_where_it_was_declared = 'system type variable can only be initialized in the initial statment of where it was declared';
   err_system_type_variable_expected = 'system type variable expected';
   err_system_type_variable_must_be_ram = 'system type variable must be declared in RAM (var)';
   err_system_type_variables_are_private_to_the_system_type = 'system type permanent variables are private to the system type';
   err_then_expected = '"then" expected';
   err_to_or_downto_expected = '"to" or "downto" expected';
   err_type_contains_types_not_allowed_in_structured_constant = 'type contains types not allowed in structured constant';
   err_type_definition_expected = 'type definition expected';
   err_type_not_modifiable = 'type not modifiable';
   err_unary_operator_not_allowed_for_this_operand_type = 'unary operator not allowed for operand type to right';
   err_undefined_identifier = 'undefined identifier';
   err_unknown_compiler_directive = 'unknown compiler directive';
   err_until_expected = '"until" expected';
   err_value_outside_legal_range = 'value outside legal range';
   err_var_parameters_not_allowed_here = 'var parameters not allowed here';
   err_variable_not_initialized = 'variable not initialized';
   err_variable_or_function_or_property_name_expected = 'variable or function or property name expected';
   err_width_expected = '"width" expected';
   err_write_only_property = 'property is write-only ("get" not defined)';
   err_wrong_enum_type = 'wrong enum type';
   err_wrong_type = 'wrong type';
   err_stack_underflow = 'stack underflow';
   err_string_dimension_not_allowed_for_rom_string_parameter = 'string dimension not allowed for rom string parameter';
   err_string_dimension_not_allowed_for_eeprom_string_parameter = 'string dimension not allowed for eeprom string parameter';
   err_string_dimension_not_allowed_for_var_string_parameter = 'string dimension not allowed for var string parameter';

type
   compile_error =
      class(Exception)
         source_location: TSourceLocation;
         constructor Create
            (error_msg: string
            );
            overload;
         constructor Create
            (error_msg: string; 
             src_location: TSourceLocation
            );
            overload;
      end;
   code_generation_error =
      class (Exception)
         error_messages: TStrings;
         constructor Create
            (_error_messages: TStrings
            );
         destructor Destroy;
            override;
      end;

type
   TReferenceCountedObject =
      class (TObject, IInterface)
      private
         ref_count: integer;
         obj_num: cardinal;
         class var num_objects: cardinal;
      protected
{$IFDEF FPC}
         function QueryInterface(constref IID: TGUID; out Obj): HResult; stdcall;
{$ELSE}
         function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
{$ENDIF}
         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;
      public
         constructor Create;
         procedure AddRef;
         procedure Release;
      end;

function err_invalid_low_set_range_value: string;
function err_invalid_high_set_range_value: string;
function err_set_member_value_outside_legal_range: string;
function err_char_value_outside_legal_range: string;
function refcount_log_enabled: boolean;

{$IFNDEF FPC}
   {$IF CompilerVersion < 20.0}
function CharInSet (c: char; s: TSysCharSet): boolean;
   {$IFEND}
{$ENDIF}


//============================================
// TRACING A DEFINITION OBJECT LIFETIME
//    To trace the lifetime events of a definition object:
//       - uncomment the DEFINITION_OBJNO_TRACE define below (remember to re-comment when problem is solved}
//{$define DEFINITION_OBJNO_TRACE}
//       - set the ERROBJ constant to the obj_num of the object of interest
const ERROBJ = 236;
//       - for Delphi only: set name of logfile below
const definition_objno_trace_log_file_name = 'c:\temp\cpc_del.txt';
//       - for Lazarus only: set name of logfile
//            set your program's command line parameters for --debug-log=<file>
//               (In Lazarus: Run / Run Parameters / Command line parameters)
//============================================

IMPLEMENTATION

uses
{$ifdef DEFINITION_OBJNO_TRACE}
   {$ifdef FPC}
   LazLogger,
   {$else}
   MadStackTrace,  // requires MacExcept to be installed in Delphi IDE (http://madshi.net)
   {$endif}
{$endif}
   cpc_core_objects_unit;

constructor compile_error.Create
   (error_msg: string
   );
   begin
      Create (error_msg, lex.token.src_loc)
   end;
   
constructor compile_error.Create
   (error_msg: string; 
    src_location: TSourceLocation
   );
   begin
      inherited Create(error_msg);
      source_location := src_location;
   end;

constructor code_generation_error.Create
   (_error_messages: TStrings
   );
   begin
      inherited Create (_error_messages.CommaText);
      error_messages := _error_messages
   end;

destructor code_generation_error.Destroy;
   begin
      error_messages.Free;
      inherited
   end;


//==========================
//  TReferenceCountedObject
//==========================

{$ifdef DEFINITION_OBJNO_TRACE}
   {$ifdef FPC}

procedure dump_stk;
   var
      I: Longint;
      prevbp: Pointer;
      CallerFrame,
      CallerAddress,
      bp: Pointer;
   const
      MaxDepth = 20;
   begin
      DebugLnEnter('---stk trace---');
      bp := get_frame;
      // This trick skip SendCallstack item
      // bp:= get_caller_frame(get_frame);
      try
         prevbp := bp - 1;
         I := 0;
         while bp > prevbp do
            begin
               CallerAddress := get_caller_addr(bp);
               CallerFrame := get_caller_frame(bp);
               if (CallerAddress = nil) then
                  Break;
               DebugLn(BackTraceStrFunc(CallerAddress));
               Inc(I);
               if (I >= MaxDepth) or (CallerFrame = nil) then
                  Break;
               prevbp := bp;
               bp := CallerFrame
            end
      except
         { prevent endless dump if an exception occured }
      end;
      DebugLnExit ('---end stk trace---');
   end;

   {$else}   // Delphi

var logfile: text;

procedure dump_stk;
   begin
      writeln (logfile, '---stk trace---');
      writeln (logfile, MadStackTrace.StackTrace);
      writeln (logfile, '---end stk trace---');
   end;

   {$endif}
{$endif}   // DEFINITION_OBJNO_TRACE

constructor TReferenceCountedObject.Create;
   begin
      ref_count := 1;
      num_objects := num_objects + 1;
      obj_num := num_objects;
{$ifdef DEFINITION_OBJNO_TRACE}
      if obj_num = ERROBJ then
         begin
   {$ifdef FPC}
            DebugLn('created -> 1');
   {$else}
            writeln (logfile, 'created -> 1');
   {$endif}
            dump_stk
         end
{$endif}
   end;

{$IFDEF FPC}
function TReferenceCountedObject.QueryInterface (constref IID: TGUID; out Obj): HResult; stdcall;
{$ELSE}
function TReferenceCountedObject.QueryInterface (const IID: TGUID; out Obj): HResult; stdcall;
{$ENDIF}
   begin
      if GetInterface(IID, Obj) then
         Result := 0
      else
         Result := E_NOINTERFACE
   end;

function TReferenceCountedObject._AddRef: Integer; stdcall;
   begin
      AddRef;
      result := ref_count
   end;

function TReferenceCountedObject._Release: Integer; stdcall;
   begin
      result := ref_count - 1;
      Release
   end;

procedure TReferenceCountedObject.AddRef;
   begin
      ref_count := ref_count + 1;
{$ifdef DEFINITION_OBJNO_TRACE}
      if obj_num = ERROBJ then
         begin
   {$ifdef FPC}
            DebugLn('addref ', IntToStr(ref_count-1), ' -> ', inttostr(ref_count));
   {$else}
            writeln(logfile, 'addref ', IntToStr(ref_count-1), ' -> ', inttostr(ref_count));
   {$endif}
            dump_stk
        end
{$endif}
   end;

procedure TReferenceCountedObject.Release;
   begin
      if Self <> nil then
         begin
{$ifdef DEFINITION_OBJNO_TRACE}
            if obj_num = ERROBJ then
               begin
   {$ifdef FPC}
                  DebugLn('release ', inttostr(ref_count), ' -> ', inttostr(ref_count-1));
   {$else}
                  writeln(logfile, 'release ', inttostr(ref_count), ' -> ', inttostr(ref_count-1));
   {$endif}
                  dump_stk
               end;
{$endif}
            assert(ref_count > 0, 'release with 0 reference count');
            ref_count := ref_count - 1;
            if ref_count = 0 then
                Free
         end
   end;

function refcount_log_enabled: boolean;
   begin
{$ifdef DEFINITION_OBJNO_TRACE}
      result := true
{$else}
      result := false
{$endif}
   end;

//==========================
//  Error Message Functions
//==========================

function err_invalid_low_set_range_value: string;
   begin
      result := format('first element of set must be >= %d', [min_set])
   end;

function err_invalid_high_set_range_value: string;
   begin
      result := format('last element of set must be <= %d', [max_set])
   end;

function err_set_member_value_outside_legal_range: string;
   begin
      result := format('set member value outside legal range of %d..%d', [min_set, max_set])
   end;

function err_char_value_outside_legal_range: string;
   begin
      result := format('char value outside legal range of %d..%d', [min_char, max_char])
   end;

{$IFNDEF FPC}
   {$IF CompilerVersion < 20.0}
function CharInSet (c: char; s: TSysCharSet): boolean;
   begin
      result := c in s
   end;
   {$IFEND}
{$ENDIF}


{$ifdef DEFINITION_OBJNO_TRACE}
   {$ifndef FPC}
INITIALIZATION
   assignfile (logfile, definition_objno_trace_log_file_name);
   rewrite (logfile);

FINALIZATION
   closefile (logfile);
   {$endif}
{$endif}   // DEFINITION_OBJNO_TRACE

END.
