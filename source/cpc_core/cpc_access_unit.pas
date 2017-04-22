UNIT cpc_access_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_blocks_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit,
   cpc_types_unit;

// TAccess handles source token sequences like "a.b[4].c.d[3]" (or even just "a")
// where you don't know what "it" (the node) is until you parse the whole thing.
//
// Example 1:
//
//    var
//       v: record
//             ch: char
//             a: array [1..10] of
//                  record
//                     i,j: int8
//                  end
//          end;
//    with v
//    do a[5].i := ...
//
// for TAccess.CreateFromSourceTokens("a[5].i") in the last line:
// - base_variable is "v"
// - path_start is "a"
// - path[0] is "[5]" indexed_access
// - path[1] is ".i" record_field_access
// - node is "i", and node_... gives info about "i"
//
// Example 2:
//
//    var i: int8;
//    i
//
// for TAccess.CreateFromSourceTokens("i") in the last line:
// - base_variable is "i"
// - path_start is "i"
// - path has 0 length
// - node is "i", and node_... gives info about "i"
//
// Example 3:
//    const c=5
//    c
//
// for TAccess.CreateFromSourceTokens("c") in the last line:
// - base_variable is nil
// - path_start is nil
// - path has 0 length
// - node is "c", and node_... gives info about "c"
//
// access_kind tells what the node is (variable, procedure, etc.).
//
// one of the node_xxx fields should be non-nil.

type
   TAccessKind =
      (variable_access,
       function_access,
       procedure_access,
       property_access,
       constant_access,
       structured_constant_access
      );

   TAccessPathKind =
      (indexed_array_access,
       indexed_string_access,
       record_field_access,
       packed_record_field_access,
       overlay_access,
       string_attribute_access,
       system_type_access
      );

   TAccessPathElement =
      record
         src_loc: TSourceLocation;
         case access_path_kind: TAccessPathKind of
            indexed_array_access,
            indexed_string_access:
               (index_expression: TExpression;
                index_typedef: TOrdinalDataType;
                element_typedef: TTypeDef
               );
            record_field_access:
               (record_field: TRecordField
               );
            packed_record_field_access:
               (packed_record_field: TPackedRecordField
               );
            overlay_access:
               (overlay_variable: TOverlaidVariable
               );
            system_type_access:
               (routine: TRoutine;
                prop: TProperty;
               )
      end;

   TAccess =
      class(TDefinition)
         base_variable: TVariable; // nil for constants, self.property
         path_start: TDefinition; // TVariable, TWithVariable, TWithRoutine, TWithProperty; or nil for constants, self.routine, self.property
         path: array of TAccessPathElement;
         node_access_kind: TAccessKind;
         node_typedef: TTypeDef;  // valid for variables, functions, properties and structured constants; nil for procedures or simple constants
         node_property: TProperty; // valid only if access_kind = property_access
         node_routine: TRoutine;  // valid only if access_kind in [function_access, procedure_access] and not string attribute
         node_constant: TConstant; // valid only for constant_access
         node_structured_constant: TStructuredConstant;  // valid only for structured_constant_access
         node_initialization_assumption_invalid: boolean;  // node is part of an overlay variable
         node_attribute: TReservedWordEnum;
         node_attribute_string_typedef: TStringType;
         node_strappend_expression: TExpression;
         node_strpos_substr_expression: TExpression;

         node_id_idx: TIdentifierIdx;
         node_id_src_loc: TSourceLocation;
         last_token_src_loc: TSourceLocation;
         function path_src: string;
         function indexed: boolean;
         function is_strlen_attribute: boolean;
         function is_maxstrlen_attribute: boolean;
         function is_strappend_attribute: boolean;
         function node_packed_record_field: TPackedRecordField;  // valid only if packed record field
         function node_is_packed_field: boolean;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;

         constructor CreateFromSourceTokens;
         constructor CreateFromVariable
            (v: TVariable
            );
         destructor Destroy;
            override;
      end;


IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_expressions_unit,
   cpc_statements_unit,
   cpc_target_cpu_unit,
   SysUtils;

// ==========
// TAccess
// ==========

constructor TAccess.CreateFromSourceTokens;

   type
      Tparse_object_kind =
         (variable_parse_object,
          constant_parse_object
         );

   procedure parse_access_path
      (typedef: TTypeDef;
       parse_object_kind: Tparse_object_kind
      );
      var
         legal_tokens: TSymbolEnumSet;
         access_path_kind: TAccessPathKind;

      procedure setup_for_access_for_next_possible_path_item;
         begin
            case typedef.type_kind of
               array_type:
                  begin
                     access_path_kind := indexed_array_access;
                     legal_tokens := [sym_left_bracket]
                  end;
               record_type:
                  begin
                     access_path_kind := record_field_access;
                     legal_tokens := [sym_dot]
                  end;
               string_type:
                  begin
                     node_attribute_string_typedef := TStringType(typedef);
                     node_attribute_string_typedef.AddRef;
                     if lex.token_is_symbol (sym_dot) then
                        begin
                           access_path_kind := string_attribute_access;
                           legal_tokens := [sym_dot]
                        end
                     else if lex.token_is_symbol (sym_left_bracket) then
                        begin
                           access_path_kind := indexed_string_access;
                           legal_tokens := [sym_left_bracket]
                        end
                     else
                        legal_tokens := []
                  end;
               packed_record_type:
                  begin
                     access_path_kind := packed_record_field_access;
                     legal_tokens := [sym_dot]
                  end;
               system_type:
                  begin
                     access_path_kind := system_type_access;
                     legal_tokens := [sym_dot]
                  end;
               overlay_type:
                  begin
                     access_path_kind := overlay_access;
                     node_initialization_assumption_invalid := true;
                     legal_tokens := [sym_dot, sym_left_bracket]
                  end;
            else
               legal_tokens := []
            end
         end;

      var
         path_idx, i, j, mark_token_idx: integer;
         last_token: TTokenType;
         first_index_ordinal_kind: TOrdinalBaseType;
         first_index_enum_typedef: TEnumType;
         expr: TExpression;
         overlay_variable_identified, ok: boolean;
      begin  // parse_access_path
         access_path_kind := indexed_array_access; // to suppress warning
         case typedef.type_kind of
            array_type:
               begin
                  access_path_kind := indexed_array_access;
                  legal_tokens := [sym_left_bracket]
               end;
            record_type:
               begin
                  access_path_kind := record_field_access;
                  legal_tokens := [sym_dot]
               end;
            string_type:
               begin
                  node_attribute_string_typedef := TStringType(typedef);
                  node_attribute_string_typedef.AddRef;
                  if lex.token_is_symbol (sym_dot) then
                     begin
                        access_path_kind := string_attribute_access;
                        legal_tokens := [sym_dot]
                     end
                  else if lex.token_is_symbol (sym_left_bracket) then
                     begin
                        access_path_kind := indexed_string_access;
                        legal_tokens := [sym_left_bracket]
                     end
                  else
                     legal_tokens := []
               end;
            packed_record_type:
               begin
                  access_path_kind := packed_record_field_access;
                  legal_tokens := [sym_dot]
               end;
            overlay_type:
               begin
                  node_initialization_assumption_invalid := true;
                  access_path_kind := overlay_access;
                  legal_tokens := [sym_dot, sym_left_bracket]
               end;
            system_type:
               begin
                  access_path_kind := system_type_access;
                  legal_tokens := [sym_dot]
               end;
            else
               legal_tokens := []
         end;

         while lex.token_is_symbol(legal_tokens) do
            begin
               last_token := lex.token;

               path_idx := Length(path);
               SetLength(path, path_idx + 1);
               path[path_idx].access_path_kind := access_path_kind;
               path[path_idx].src_loc := lex.token.src_loc;
               lex.advance_token;

               case access_path_kind of
                  indexed_array_access:
                     begin
                        path[path_idx].index_expression := CreateExpressionFromSourceTokens;
                        if (parse_object_kind = constant_parse_object)
                           and
                           (not path[path_idx].index_expression.contains_constant) then
                           raise compile_error.Create(err_constant_index_expression_expected, path[path_idx].index_expression.src_loc);
                        path[path_idx].index_typedef := TArrayType(typedef).index_typedef;
                        path[path_idx].index_typedef.AddRef;
                        path[path_idx].index_typedef.CheckAssignmentCompatability(path[path_idx].index_expression);
                        typedef := TArrayType(typedef).element_typedef;
                        path[path_idx].element_typedef := typedef;
                        case typedef.type_kind of
                           array_type:
                              begin
                                 access_path_kind := indexed_array_access;
                                 if lex.token_is_symbol(sym_right_bracket) then
                                    begin
                                       lex.advance_token;
                                       legal_tokens := [sym_left_bracket]
                                    end
                                 else
                                    legal_tokens := [sym_comma]
                              end;
                           record_type:
                              begin
                                 if not lex.token_is_symbol(sym_right_bracket) then
                                    raise compile_error.Create(err_right_bracket_expected);
                                 lex.advance_token;
                                 access_path_kind := record_field_access;
                                 legal_tokens := [sym_dot]
                              end;
                           packed_record_type:
                              begin
                                 if not lex.token_is_symbol(sym_right_bracket) then
                                    raise compile_error.Create(err_right_bracket_expected);
                                 lex.advance_token;
                                 access_path_kind := packed_record_field_access;
                                 legal_tokens := [sym_dot]
                              end;
                           overlay_type:
                              begin
                                 if not lex.token_is_symbol(sym_right_bracket) then
                                    raise compile_error.Create(err_right_bracket_expected);
                                 lex.advance_token;
                                 access_path_kind := overlay_access;
                                 legal_tokens := [sym_dot]
                              end;
                           string_type:
                              begin
                                 node_attribute_string_typedef := TStringType(typedef);
                                 node_attribute_string_typedef.AddRef;
                                 if lex.token_is_symbol(sym_right_bracket) then
                                    begin
                                       lex.advance_token;
                                       if lex.token_is_symbol(sym_left_bracket) then
                                          begin
                                             access_path_kind := indexed_string_access;
                                             legal_tokens := [sym_left_bracket]
                                          end
                                       else if lex.token_is_symbol (sym_dot) then
                                          begin
                                             access_path_kind := string_attribute_access;
                                             legal_tokens := [sym_dot]
                                          end
                                       else
                                          legal_tokens := [];
                                    end
                                 else
                                    begin
                                       access_path_kind := indexed_string_access;
                                       legal_tokens := [sym_comma]
                                    end
                              end;
                           system_type:
                              begin
                                 if not lex.token_is_symbol(sym_right_bracket) then
                                    raise compile_error.Create(err_right_bracket_expected);
                                 lex.advance_token;
                                 access_path_kind := system_type_access;
                                 legal_tokens := [sym_dot]
                              end;
                        else
                           if not lex.token_is_symbol(sym_right_bracket) then
                              raise compile_error.Create(err_right_bracket_expected);
                           lex.advance_token;
                           legal_tokens := []
                        end
                     end;
                  indexed_string_access:
                     begin
                        path[path_idx].index_expression := CreateExpressionFromSourceTokens;
                        if (parse_object_kind = constant_parse_object)
                           and
                           (not path[path_idx].index_expression.contains_constant)
                        then
                           raise compile_error.Create(err_constant_index_expression_expected, path[path_idx].index_expression.src_loc);
                        if TStringType(typedef).max_length = -1 then  // undimensioned string
                           path[path_idx].index_typedef := TSubRangeType.CreateIntegerSubrange (1, 255)
                        else  // dimensioned string
                           path[path_idx].index_typedef := TSubRangeType.CreateIntegerSubrange (1, TStringType(typedef).max_length);
                        path[path_idx].index_typedef.CheckAssignmentCompatability(path[path_idx].index_expression);
                        path[path_idx].element_typedef := target_cpu.get_supported_data_type ('char');
                        if not lex.token_is_symbol(sym_right_bracket) then
                           raise compile_error.Create(err_right_bracket_expected);
                        lex.advance_token;
                        legal_tokens := []
                     end;
                  string_attribute_access:
                     begin
                        if not lex.token_is_reserved_word ([rw_maxstrlen, rw_strappend, rw_strlen, rw_strpos])
                        then
                           raise compile_error.Create (err_invalid_string_attribute);
                        node_attribute := lex.token.rw;
                        node_id_src_loc := lex.token.src_loc;
                        lex.advance_token;
                        case node_attribute of
                           rw_strlen:
                              ;
                           rw_maxstrlen:
                              if node_attribute_string_typedef.known_strlen
                              then
                                 raise EConstantExpressionSimplification.Create (TConstantPrimary.CreateFromConstant (TConstant.CreateIntegerConstant (node_attribute_string_typedef.max_length), NonExistantSourceLocation))
                              else
                                 case base_variable.descriptor of
                                    rw_const,
                                    rw_rom:
                                       // return strlen instead (maxstrlen isn't useful and would take an extra byte on the stack)
                                       node_attribute := rw_strlen;
                                    rw_var,
                                    rw_eeprom:
                                       ;
                                 else
                                    assert (false)
                                 end;
                           rw_strappend:
                              begin
                                 if not (base_variable.descriptor in [rw_var, rw_eeprom]) then
                                    raise compile_error.Create (err_strappend_only_allowed_for_var_or_eeprom_variables, lex.previous_token_src_loc);
                                 if not lex.token_is_symbol (sym_left_parenthesis) then
                                    raise compile_error.Create (err_left_parenthesis_expected);
                                 lex.advance_token;
                                 node_strappend_expression := CreateExpressionFromSourceTokens;
                                 case node_strappend_expression.expression_kind of
                                    char_expression:
                                       ok := true;
                                    string_expression:
                                       ok := ((node_strappend_expression is TVariableAccessPrimary)
                                              or
                                              (node_strappend_expression is TConstantPrimary)
                                             );
                                 else
                                    ok := false
                                 end;
                                 if not ok then
                                    raise compile_error.Create (err_string_variable_or_constant_expected, node_strappend_expression.src_loc);
                                 if not lex.token_is_symbol (sym_right_parenthesis) then
                                    raise compile_error.Create (err_right_parenthesis_expected);
                                 lex.advance_token
                              end;
                           rw_strpos:
                              begin
                                 if not lex.token_is_symbol (sym_left_parenthesis) then
                                    raise compile_error.Create (err_left_parenthesis_expected);
                                 lex.advance_token;
                                 node_strpos_substr_expression := CreateExpressionFromSourceTokens;
                                 case node_strpos_substr_expression.expression_kind of
                                    char_expression:
                                       ok := true;
                                    string_expression:
                                       ok := ((node_strpos_substr_expression is TVariableAccessPrimary)
                                              or
                                              (node_strpos_substr_expression is TConstantPrimary)
                                             );
                                 else
                                    ok := false
                                 end;
                                 if not ok then
                                    raise compile_error.Create (err_string_variable_or_constant_expected, node_strpos_substr_expression.src_loc);
                                 if not lex.token_is_symbol (sym_right_parenthesis) then
                                    raise compile_error.Create (err_right_parenthesis_expected);
                                 lex.advance_token
                              end;
                        else
                           assert (false)
                        end
                     end;
                  record_field_access:
                     begin
                        path[path_idx].record_field := nil;
                        for i := 0 to Length(TRecordType(typedef).fields) - 1 do
                           if lex.token.identifier_idx = TRecordType(typedef).fields[i].identifier_idx then
                              path[path_idx].record_field := TRecordType(typedef).fields[i];
                        if path[path_idx].record_field = nil then
                           raise compile_error.Create(err_field_identifier_expected);
                        path[path_idx].src_loc := lex.token.src_loc;
                        node_id_idx := lex.token.identifier_idx;
                        node_id_src_loc := lex.token.src_loc;
                        lex.advance_token;

                        typedef := path[path_idx].record_field.typedef;
                        setup_for_access_for_next_possible_path_item;
                     end;
                  packed_record_field_access:
                     begin
                        path[path_idx].packed_record_field := nil;
                        for i := 0 to Length(TPackedRecordType(typedef).fields) - 1 do
                           if lex.token.identifier_idx = TPackedRecordType(typedef).fields[i].identifier_idx then
                              path[path_idx].packed_record_field := TPackedRecordType(typedef).fields[i];
                        if path[path_idx].packed_record_field = nil then
                           raise compile_error.Create(err_field_identifier_expected);
                        path[path_idx].src_loc := lex.token.src_loc;
                        node_id_idx := lex.token.identifier_idx;
                        node_id_src_loc := lex.token.src_loc;
                        lex.advance_token;
                        typedef := path[path_idx].packed_record_field.ordtypedef;
                        legal_tokens := []
                     end;
                  overlay_access:
                     case last_token.symbol of
                        sym_left_bracket:
                           begin
                              // look ahead and calculate index path signature
                              mark_token_idx := lex.token_idx;
                              expr := CreateExpressionFromSourceTokens;
                              first_index_enum_typedef := nil;
                              first_index_ordinal_kind := ordinal_base_is_integer;  // set only to suppress compiler warning
                              try
                                 case expr.expression_kind of
                                    integer_expression:
                                       first_index_ordinal_kind := ordinal_base_is_integer;
                                    enum_expression:
                                       begin
                                          first_index_ordinal_kind := ordinal_base_is_enum;
                                          first_index_enum_typedef := expr.enum_typedef
                                       end;
                                    boolean_expression:
                                       first_index_ordinal_kind := ordinal_base_is_bool;
                                    char_expression:
                                       first_index_ordinal_kind := ordinal_base_is_char;
                                 else
                                    raise compile_error.Create(err_ordinal_expression_expected, expr.src_loc);
                                 end
                              finally
                                 expr.Release
                              end;

                              path[path_idx].overlay_variable := nil;
                              for i := 0 to Length(TOverlayType(typedef).overlaid_variables) - 1 do
                                 if TOverlayType(typedef).overlaid_variables[i].matches_anonymous_index_path_signature(first_index_ordinal_kind, first_index_enum_typedef) then
                                    path[path_idx].overlay_variable := TOverlayType(typedef).overlaid_variables[i];
                              if path[path_idx].overlay_variable = nil then
                                 raise compile_error.Create(err_index_signature_does_not_match_any_overlay_variable_anonymous_signature);

                              lex.token_idx := mark_token_idx - 1;
                              // reset to beginning of index path signature at "["
                              access_path_kind := indexed_array_access;
                              legal_tokens := [sym_left_bracket];
                              typedef := path[path_idx].overlay_variable.typedef
                           end;
                        sym_dot:
                           begin
                              overlay_variable_identified := false;
                              i := 0;
                              while (not overlay_variable_identified) and (i < Length(TOverlayType(typedef).overlaid_variables)) do
                                 begin
                                    if TOverlayType(typedef).overlaid_variables[i].anonymous then
                                       case TOverlayType(typedef).overlaid_variables[i].typedef.type_kind of
                                          record_type:
                                             begin
                                                j := 0;
                                                while (not overlay_variable_identified)
                                                      and
                                                      (j < Length(TRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields)) do
                                                   begin
                                                      if lex.token.identifier_idx = TRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j].identifier_idx then
                                                         begin
                                                            overlay_variable_identified := true;
                                                            path[path_idx].overlay_variable := TOverlayType(typedef).overlaid_variables[i];
                                                            path_idx := path_idx + 1;
                                                            SetLength(path, path_idx + 1);
                                                            path[path_idx].access_path_kind := record_field_access;
                                                            path[path_idx].record_field := TRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j];
                                                            typedef := TRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j].typedef;
                                                            setup_for_access_for_next_possible_path_item
                                                         end;
                                                      j := j + 1
                                                   end
                                             end;
                                          packed_record_type:
                                             begin
                                                j := 0;
                                                while (not overlay_variable_identified)
                                                      and
                                                      (j < Length(TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields)) do
                                                   begin
                                                      if lex.token.identifier_idx = TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j].identifier_idx then
                                                         begin
                                                            overlay_variable_identified := true;
                                                            path[path_idx].overlay_variable := TOverlayType(typedef).overlaid_variables[i];
                                                            path_idx := path_idx + 1;
                                                            SetLength(path, path_idx + 1);
                                                            path[path_idx].access_path_kind := packed_record_field_access;
                                                            path[path_idx].packed_record_field := TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j];
                                                            typedef := TPackedRecordType(TOverlayType(typedef).overlaid_variables[i].typedef).fields[j].ordtypedef;
                                                            legal_tokens := []
                                                         end;
                                                      j := j + 1
                                                   end
                                             end;
                                          array_type,
                                          string_type:
                                             ;  // doesn't have any fields
                                       else
                                          assert(false)  // not a valid anonymous type
                                       end
                                    else // not anonymous
                                       if lex.token.identifier_idx = TOverlayType(typedef).overlaid_variables[i].identifier_idx then
                                          begin
                                             overlay_variable_identified := true;
                                             path[path_idx].overlay_variable := TOverlayType(typedef).overlaid_variables[i];
                                             typedef := TOverlayType(typedef).overlaid_variables[i].typedef;
                                             setup_for_access_for_next_possible_path_item
                                          end;
                                    i := i + 1
                                 end;
                              if not overlay_variable_identified then
                                 raise compile_error.Create(err_overlay_variable_expected);
                              lex.advance_token
                           end;
                        else
                           assert(false)
                     end;
                  system_type_access:
                     begin
                        // access to system type's parameters and local vars is not allowed
                        for i := 0 to TSystemType(typedef).permanent_ram_vars.Length - 1 do
                           if lex.token.identifier_idx = TSystemType(typedef).permanent_ram_vars[i].id_idx then
                              raise compile_error.Create(err_system_type_variables_are_private_to_the_system_type);

                        path[path_idx].routine := nil;
                        path[path_idx].prop := nil;
                        for i := 0 to Length(TSystemType(typedef).routines) - 1 do
                           if lex.token.identifier_idx = TSystemType(typedef).routines[i].routine_id_idx then
                              begin
                                 path[path_idx].routine := TSystemType(typedef).routines[i];
                                 if not path[path_idx].routine.entry then
                                    raise compile_error.Create(err_routine_must_be_marked_entry)
                              end;
                        for i := 0 to Length(TSystemType(typedef).properties) - 1 do
                           if lex.token.identifier_idx = TSystemType(typedef).properties[i].id then
                              begin
                                 path[path_idx].prop := TSystemType(typedef).properties[i];
                                 if not path[path_idx].prop.entry then
                                    raise compile_error.Create(err_property_must_be_marked_entry)
                              end;
                        if (path[path_idx].routine = nil) and (path[path_idx].prop = nil) then
                           raise compile_error.Create(err_property_or_routine_identifier_expected);
                        node_id_idx := lex.token.identifier_idx;
                        node_id_src_loc := lex.token.src_loc;
                        lex.advance_token;
                        legal_tokens := []
                     end;
               else
                  assert (false)
               end
            end;
      end;   // parse_access_path

   procedure set_node_constant_info
      (def: TDefinition  { either TConstant or TStructuredConstant }
      );
      var
         sc: TStructuredConstant;
      begin
         case def.definition_kind of
            constant_definition:
               begin
                  node_access_kind := constant_access;
                  node_constant := TConstant(def);
                  node_constant.AddRef
               end;
            structured_constant_definition:
               begin
                  sc := TStructuredConstant(def);
                  node_access_kind := structured_constant_access;
                  node_structured_constant := sc;
                  node_structured_constant.AddRef;
                  node_typedef := node_structured_constant.typedef;
                  node_typedef.AddRef
               end;
         else
            assert(false)
         end;
      end;

   function evaluate_structured_constant
      (sc: TStructuredConstant
      ): TDefinition;
      var
         path_idx, idx: integer;
         found: boolean;
      begin
         result := nil; // suppress compiler warning
         path_idx := 0;
         while path_idx < Length(path) do
            begin
               with path[path_idx] do
                  case access_path_kind of
                     indexed_array_access:
                        begin
                           assert (sc.StructuredConstantKind = scArray);
                           idx := path[path_idx].index_expression.ordinal_constant_value - path[path_idx].index_typedef.info.min_value.AsInteger;
                           assert((0 <= idx) and (idx < Length(sc.array_elements)));
                           sc := sc.array_elements[idx]
                        end;
                     record_field_access:
                        begin
                           assert (sc.StructuredConstantKind = scRecord);
                           found := false;
                           for idx := 0 to Length(TRecordType(sc.typedef).fields) - 1 do
                              if (not found)
                                 and
                                 (TRecordType(sc.typedef).fields[idx] = path[path_idx].record_field)
                              then
                                 begin
                                    sc := sc.record_fields[idx];
                                    found := true
                                 end;
                           assert(found)
                        end;
                     packed_record_field_access:
                        begin
                           assert (sc.StructuredConstantKind = scPackedRecord);
                           found := false;
                           for idx := 0 to Length(TPackedRecordType(sc.typedef).fields) - 1 do
                              if (not found)
                                 and
                                 (TPackedRecordType(sc.typedef).fields[idx] = path[path_idx].packed_record_field) then
                                 begin
                                    result := sc.packed_record_fields[idx].c;
                                    sc := nil;
                                    found := true
                                 end;
                           assert(found)
                        end;
                     overlay_access:
                        begin
                           assert (sc.StructuredConstantKind = scOverlay);
                           result := sc.overlay_constant
                        end;
                  else
                     assert(false)
                  end;
               path_idx := path_idx + 1
            end;
         if sc <> nil then
            result := sc
      end;

   procedure set_variable_node_info;
      var
         node_idx: integer;
      begin
         node_idx := Length(path) - 1;
         case path[node_idx].access_path_kind of
            string_attribute_access:
               case node_attribute of
                  rw_maxstrlen:
                     begin
                        assert (not node_attribute_string_typedef.known_strlen);
                        node_access_kind := variable_access;
                        node_typedef := TSubRangeType.CreateIntegerSubrange (0, 255)
                     end;
                  rw_strappend:
                     node_access_kind := procedure_access;
                  rw_strlen:
                     begin
                        node_access_kind := variable_access;
                        if node_attribute_string_typedef.known_strlen then
                           node_typedef := TSubRangeType.CreateIntegerSubrange (0, node_attribute_string_typedef.max_length)
                        else
                           node_typedef := TSubRangeType.CreateIntegerSubrange (0, 255)
                     end;
                  rw_strpos:
                     begin
                        node_access_kind := variable_access;
                        if node_attribute_string_typedef.known_strlen then
                           node_typedef := TSubRangeType.CreateIntegerSubrange (0, node_attribute_string_typedef.max_length)
                        else
                           node_typedef := TSubRangeType.CreateIntegerSubrange (0, 255)
                     end;
               else
                  assert (false)
               end;
            indexed_array_access,
            indexed_string_access:
               begin
                  node_access_kind := variable_access;
                  node_typedef := path[node_idx].element_typedef;
                  node_typedef.AddRef
               end;
            record_field_access:
               begin
                  node_access_kind := variable_access;
                  node_typedef := path[node_idx].record_field.typedef;
                  node_typedef.AddRef
               end;
            packed_record_field_access:
               begin
                  node_access_kind := variable_access;
                  node_typedef := path[node_idx].packed_record_field.ordtypedef;
                  node_typedef.AddRef
               end;
            overlay_access:
               begin
                  node_access_kind := variable_access;
                  node_typedef := path[node_idx].overlay_variable.typedef;
                  node_typedef.AddRef
               end;
            system_type_access:
               if path[node_idx].prop <> nil then
                  begin
                     node_access_kind := property_access;
                     node_property := path[node_idx].prop;
                     node_property.AddRef;
                     node_typedef := node_property.typedef;
                     node_typedef.AddRef
                  end
               else
                  begin
                     node_routine := path[node_idx].routine;
                     node_routine.AddRef;
                     if node_routine.function_result = nil then
                        node_access_kind := procedure_access
                     else
                        begin
                           node_access_kind := function_access;
                           node_typedef := node_routine.function_result.typedef;
                           node_typedef.AddRef
                        end
                  end
         else
            assert(false)
         end
      end;

   var
      sc: TStructuredConstant;
      sc_typedef: TTypeDef;
      sc_typedef_src_loc: TSourceLocation;
   begin // TAccess.CreateFromSourceTokens;
      inherited Create(access_definition);
      src_loc := lex.token.src_loc;
      node_id_idx := lex.token.identifier_idx;
      node_id_src_loc := src_loc;

      if not lex.token_is_identifier then
         raise compile_error.Create(err_identifier_expected);

      case CurrentDefinitionTable[lex.token.identifier_idx].definition_kind of
         variable_definition:
            begin
               path_start := CurrentDefinitionTable[lex.token.identifier_idx];
               path_start.AddRef;
               if TVariable(path_start).typedef = nil then
                  raise compile_error.Create (err_incomplete_type_definition_for_variable);
               lex.advance_token;
               base_variable := TVariable(path_start);
               base_variable.AddRef;
               parse_access_path(TVariable(path_start).typedef, variable_parse_object);
               if Length(path) = 0 then
                  begin
                     node_access_kind := variable_access;
                     node_typedef := TVariable(path_start).typedef;
                     node_typedef.AddRef
                  end
               else
                  set_variable_node_info
            end;
         with_variable_definition:
            begin
               path_start := CurrentDefinitionTable[lex.token.identifier_idx];
               lex.advance_token;
               path_start.AddRef;
               base_variable := TWithVariable(path_start).with_statement.access.base_variable;
               node_initialization_assumption_invalid := TWithVariable(path_start).with_statement.access.node_initialization_assumption_invalid;
               base_variable.AddRef;
               case TWithVariable(path_start).record_field.definition_kind of
                  record_field_definition:
                     parse_access_path(TRecordField(TWithVariable(path_start).record_field).typedef, variable_parse_object);
                  overlay_field_definition:
                     parse_access_path(TOverlaidVariable(TWithVariable(path_start).record_field).typedef, variable_parse_object);
                  packed_record_field_definition:
                     parse_access_path(TPackedRecordField(TWithVariable(path_start).record_field).ordtypedef, variable_parse_object);
               else
                  assert(false)
               end;
               if Length(path) = 0 then
                  begin
                     node_access_kind := variable_access;
                     case TWithVariable(path_start).record_field.definition_kind of
                        record_field_definition:
                           node_typedef := TRecordField(TWithVariable(path_start).record_field).typedef;
                        overlay_field_definition:
                           node_typedef := TOverlaidVariable(TWithVariable(path_start).record_field).typedef;
                        packed_record_field_definition:
                           node_typedef := TPackedRecordField(TWithVariable(path_start).record_field).ordtypedef;
                     else
                        assert(false)
                     end;
                     node_typedef.AddRef
                  end
               else
                  set_variable_node_info
            end;
         routine_definition:
            begin
               node_routine := TRoutine(CurrentDefinitionTable[lex.token.identifier_idx]);
               node_routine.AddRef;
               lex.advance_token;
               if node_routine.function_result = nil then
                  node_access_kind := procedure_access
               else
                  begin
                     node_access_kind := function_access;
                     node_typedef := node_routine.function_result.typedef;
                     node_typedef.AddRef
                  end
            end;
         with_routine_definition:
            begin
               path_start := CurrentDefinitionTable[lex.token.identifier_idx];
               lex.advance_token;
               path_start.AddRef;
               base_variable := TWithRoutine(path_start).with_statement.access.base_variable;
               base_variable.AddRef;
               node_routine := TWithRoutine(path_start).routine;
               node_routine.AddRef;
               if node_routine.function_result = nil then
                  node_access_kind := procedure_access
               else
                  begin
                     node_access_kind := function_access;
                     node_typedef := node_routine.function_result.typedef;
                     node_typedef.AddRef
                  end
            end;
         property_definition:
            begin
               node_property := TProperty(CurrentDefinitionTable[lex.token.identifier_idx]);
               node_property.AddRef;
               lex.advance_token;
               node_access_kind := property_access;
               node_typedef := node_property.typedef;
               node_typedef.AddRef
            end;
         with_property_definition:
            begin
               path_start := CurrentDefinitionTable[lex.token.identifier_idx];
               lex.advance_token;
               path_start.AddRef;
               base_variable := TWithProperty(path_start).with_statement.access.base_variable;
               base_variable.AddRef;
               node_access_kind := property_access;
               node_property := TWithProperty(path_start).prop;
               node_property.AddRef;
               node_typedef := node_property.typedef;
               node_typedef.AddRef
            end;
         constant_definition:
            begin
               node_constant := TConstant(CurrentDefinitionTable[lex.token.identifier_idx]);
               node_constant.AddRef;
               lex.advance_token;
               node_access_kind := constant_access
            end;
         structured_constant_definition:
            begin
               sc := TStructuredConstant(CurrentDefinitionTable[lex.token.identifier_idx]);
               lex.advance_token;
               parse_access_path(TStructuredConstant(sc).typedef, constant_parse_object);
               if Length(path) = 0 then
                  set_node_constant_info(TStructuredConstant(sc))
               else
                  set_node_constant_info(evaluate_structured_constant(sc))
            end;
         type_definition: // anonymous structured constant
            begin
               sc_typedef_src_loc := lex.token.src_loc;
               sc_typedef := TTypeDef(CurrentDefinitionTable[lex.token.identifier_idx]);
               lex.advance_token;

               if not lex.token_is_symbol(sym_colon) then
                  raise compile_error.Create(err_colon_expected_for_anonymous_structured_constant);
               lex.advance_token;

               sc := TStructuredConstant.CreateFromSourceTokens(sc_typedef, sc_typedef_src_loc);
               set_node_constant_info(sc);
               sc.Release
            end;
      else
         raise compile_error.Create(err_variable_or_function_or_property_name_expected, src_loc)
      end;

      last_token_src_loc := lex.previous_token_src_loc
   end; // TAccess.CreateFromSourceTokens;

constructor TAccess.CreateFromVariable
   (v: TVariable
   );
   begin
      inherited Create(access_definition);
      base_variable := v;
      path_start := v;
      src_loc := v.src_loc;
      last_token_src_loc := v.src_loc;
      base_variable.AddRef;
      path_start.AddRef;
      node_access_kind := variable_access;
      node_typedef := v.typedef;
      node_typedef.AddRef
   end;

destructor TAccess.Destroy;
   var
      i: integer;
   begin
      base_variable.Release;
      path_start.Release;
      node_typedef.Release;
      node_property.Release;
      node_routine.Release;
      node_constant.Release;
      node_structured_constant.Release;
      node_attribute_string_typedef.Release;
      node_strappend_expression.Release;
      node_strpos_substr_expression.Release;
      for i := 0 to Length(path) - 1 do
         case path[i].access_path_kind of
            indexed_array_access,
            indexed_string_access:
               begin
                  path[i].index_expression.Release;
                  path[i].index_typedef.Release
               end;
         end
   end;

function TAccess.path_src: string;
   begin
      if src_loc.same_location (NonExistantSourceLocation) then
         result := LowerCase (lex.identifiers [base_variable.id_idx])
      else
         result := LowerCase(lex.token_string (src_loc, last_token_src_loc))
   end;

function TAccess.indexed: boolean;
   var i: integer;
   begin
      result := false;
      for i := 0 to Length(path)-1 do
         if (path[i].access_path_kind in [indexed_array_access, indexed_string_access])
            and
            (not path[i].index_expression.contains_constant)
         then
            result := true
   end;

function TAccess.is_strlen_attribute: boolean;
   begin
      result := node_attribute = rw_strlen
   end;

function TAccess.is_maxstrlen_attribute: boolean;
   begin
      result := node_attribute = rw_maxstrlen
   end;

function TAccess.is_strappend_attribute: boolean;
   begin
      result := node_attribute = rw_strappend
   end;

function TAccess.node_is_packed_field: boolean;
   begin
      if base_variable.is_ioreg_1bit_param then
         result := true
      else if Length(path) = 0 then
         result := (path_start.definition_kind = with_variable_definition)
                   and
                   (TWithVariable(path_start).record_field.definition_kind = packed_record_field_definition)
      else   // Length(path) > 0
         result := path[Length(path)-1].access_path_kind = packed_record_field_access
   end;

function TAccess.node_packed_record_field: TPackedRecordField;
   begin
      assert (node_is_packed_field);
      if Length(path) = 0 then
         result := TPackedRecordField (TWithVariable(path_start).record_field)
      else   // Length(path) > 0
         result := path[Length(path)-1].packed_record_field
   end;

function TAccess.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(path)-1 do
         if path[i].access_path_kind in [indexed_array_access, indexed_string_access] then
            if path[i].index_expression.CheckForProhibitedDelayCall (err_msg) then
               result := true;
      if (node_routine <> nil)
         and
         (node_routine.CheckForProhibitedDelayCall (''))
      then
         begin
            if err_msg <> '' then
               raise compile_error.Create (err_msg, node_id_src_loc);
            result := true
         end;
      if (node_property <> nil)
         and
         (node_property.CheckForProhibitedDelayCall (''))
      then
         begin
            if err_msg <> '' then
               raise compile_error.Create (err_msg, node_id_src_loc);
            result := true
         end
   end;

END.
