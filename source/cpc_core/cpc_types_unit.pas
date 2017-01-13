UNIT cpc_types_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit;

type
   TRecordType = class;
   TRecordField =
      class(TDefinition)
         kind: (normal_field, anonymous_field);
         containing_record: TRecordType;
         identifier_idx: TIdentifierIdx;
         identifier_src_loc: TSourceLocation;
         typedef: TTypeDef;
         typedef_src_loc: TSourceLocation;
         constructor Create;
         destructor Destroy;
            override;
         function name: string;
      end;
   TRecordType =
      class(TTypeDef)
         fields: array of TRecordField;
         constructor CreateFromSourceTokens;
         constructor Create;
         destructor Destroy;
            override;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function ContainsQueueVariables: boolean;
            override;
         function ContainsClassVariables: boolean;
            virtual;
         function ContainsMonitorVariables: boolean;
            virtual;
         function ContainsProcessVariables: boolean;
            virtual;
         procedure SetMonitorPriorities
            (prio: integer
            );
            override;
         function ReadOnly: boolean;
            override;
      end;

   TPackedRecordType = class;
   TPackedRecordField =
      class(TDefinition)
         kind: (normal_packed_field, anonymous_packed_field);
         identifier_idx: TIdentifierIdx;
         identifier_src_loc: TSourceLocation;
         ordtypedef: TOrdinalDataType;
         containing_packed_record: TPackedRecordType;
         constructor Create (_prec: TPackedRecordType);
         destructor Destroy;
            override;
         function name: string;
      end;

   TPackedRecordType =
      class(TTypeDef)
         fields: array of TPackedRecordField;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function ReadOnly: boolean;
            override;
      end;

   TOverlaidVariable =
      class(TRecordField)
         anonymous: boolean;
         anonymous_array_first_index_ordinal_kind: TOrdinalBaseType;
         anonymous_array_first_index_enum_typedef: TEnumType;
         procedure check_for_exposed_identifier_ambiguity
            (new_identifier_idx: TIdentifierIdx;
             new_identifier_src_loc, typedef_src_loc: TSourceLocation
            );
         procedure check_for_index_ambiguity
            (new_anonymous_array_first_index_ordinal_kind: TOrdinalBaseType;
             new_anonymous_array_first_index_enum_typedef: TEnumType;
             new_index_typedef_src_loc: TSourceLocation
            );
         // throws compile error if ambiguious
         function matches_anonymous_index_path_signature
            (index_ordinal_kind: TOrdinalBaseType;
             index_enum_typedef: TEnumType
            ): boolean;
      end;

   TOverlayType =
      class(TTypeDef)
         overlaid_variables: array of TOverlaidVariable;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function ReadOnly: boolean;
            override;
      end;

   TArrayType =
      class(TTypeDef)
         index_typedef: TOrdinalDataType;
         element_typedef: TTypeDef;
         element_typedef_src_loc: TSourceLocation;
         constructor CreateFromSourceTokens;
         constructor CreateFromSourceTokensStartingAtIndex;
         destructor Destroy;
            override;
         procedure CheckAssignmentCompatability
            (def: TDefinition
            );
            override;
         function ContainsQueueVariables: boolean;
            override;
         function ContainsClassVariables: boolean;
            virtual;
         function ContainsMonitorVariables: boolean;
            virtual;
         function ContainsProcessVariables: boolean;
            virtual;
         procedure SetMonitorPriorities
            (prio: integer
            );
            override;
         function ReadOnly: boolean;
            override;
      private
         procedure scan_starting_at_index;
      end;

IMPLEMENTATION

uses SysUtils, cpc_common_unit, cpc_blocks_unit;

function IsProcessType (typedef: TTypeDef): boolean;
   begin
      result := (typedef.type_kind = system_type)
                and
                (TSystemType (typedef).system_type_kind = process_system_type)
   end;

// ===================
// TRecordFieldType
// ===================

constructor TRecordField.Create;
   begin
      inherited Create(record_field_definition)
   end;

destructor TRecordField.Destroy;
   begin
      typedef.Release;
      inherited
   end;

function TRecordField.name: string;
   begin
      result := lex.identifiers [identifier_idx]
   end;


// ==============
// TRecordType
// ==============

constructor TRecordType.Create;
   begin
      inherited Create(record_type);
      MarkTypeDefinitionAsComplete
   end;

constructor TRecordType.CreateFromSourceTokens;
   var
      field_idx: integer;
      var_count: integer; // number of vars defined for a field type, e.g. "a,b,c: integer" would be 3
      i: integer;
      typedef: TTypeDef;
   begin
      inherited Create(record_type);

      assert(lex.token_is_reserved_word(rw_record));
      lex.advance_token;

      var_count := 0;
      while not lex.token_is_reserved_word(rw_end) do
         begin
            field_idx := Length(fields);
            SetLength(fields, field_idx + 1);
            fields[field_idx] := TRecordField.Create;
            fields[field_idx].containing_record := self;
            fields[field_idx].identifier_src_loc := lex.token.src_loc;
            if lex.token_is_symbol (sym_minus) then
               fields[field_idx].kind := anonymous_field
            else
               begin
                  fields[field_idx].kind := normal_field;
                  if not lex.token_is_identifier then
                     raise compile_error.Create(err_identifier_expected);

                  for i := 0 to Length(fields) - 1 do
                     if fields[i].identifier_idx = lex.token.identifier_idx then
                        raise compile_error.Create(err_duplicate_field_name);
                  fields[field_idx].identifier_idx := lex.token.identifier_idx
               end;
            var_count := var_count + 1;
            lex.advance_token;

            if lex.token_is_symbol(sym_comma) then
               lex.advance_token
            else
               begin
                  if not lex.token_is_symbol(sym_colon) then
                     raise compile_error.Create(err_colon_expected);
                  lex.advance_token;

                  fields[field_idx].typedef_src_loc := lex.token.src_loc;
                  typedef := CreateTypeDenoterFromSourceTokens;
                  if (typedef.type_kind = string_type)
                     and
                     (TStringType(typedef).max_length = -1) then
                     begin
                        typedef.Release;
                        raise compile_error.Create(err_left_bracket_expected)
                     end;
                  for i := 0 to var_count - 1 do
                     begin
                        fields[field_idx - i].typedef := typedef;
                        fields[field_idx - i].typedef.AddRef
                     end;
                  try
                     if IsProcessType (typedef) then
                        raise compile_error.Create (err_record_may_not_contain_process_variables, fields[field_idx].typedef_src_loc)
                  finally
                     typedef.Release
                  end;

                  var_count := 0;

                  if not lex.token_is_reserved_word(rw_end) then
                     begin
                        if not lex.token_is_symbol(sym_semicolon) then
                           raise compile_error.Create(err_semicolon_or_end_expected);
                        lex.advance_token
                     end
               end
         end;
      lex.advance_token;
      MarkTypeDefinitionAsComplete
   end;

destructor TRecordType.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(fields) - 1 do
         fields[i].Release;
      inherited
   end;

procedure TRecordType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      i: integer;
      compatable: boolean;
   begin
      case def.definition_kind of
         constant_definition:
            raise compile_error.Create(err_incorrect_type, def.src_loc);
         expression_definition:
            begin
               compatable := TExpression(def).typedef = Self;
               if TExpression(def).expression_kind = overlay_expression then
                  for i := 0 to Length(TOverlayType(TExpression(def).typedef).overlaid_variables) - 1 do
                     with TOverlayType(TExpression(def).typedef).overlaid_variables[i] do
                        if anonymous and (typedef = Self) then
                           compatable := true;
               if not compatable then
                  raise compile_error.Create(err_incorrect_type, TExpression(def).src_loc);
            end;
         structured_constant_definition:
            if TStructuredConstant(def).TypeDef <> Self then
               raise compile_error.Create(err_incorrect_type, TStructuredConstant(def).src_loc);
         else
            assert(false)
      end
   end;

function TRecordType.ContainsQueueVariables: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(fields) - 1 do
         if (fields[i].kind = normal_field)
            and
            (fields[i].typedef.ContainsQueueVariables)
         then
            result := true
   end;

function TRecordType.ContainsClassVariables: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(fields) - 1 do
         if fields[i].typedef.IsClassSystemType then
            result := true
   end;

function TRecordType.ContainsMonitorVariables: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(fields) - 1 do
         if fields[i].typedef.IsMonitorSystemType then
            result := true
   end;

function TRecordType.ContainsProcessVariables: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(fields) - 1 do
         if fields[i].typedef.IsProcessSystemType then
            result := true
   end;

procedure TRecordType.SetMonitorPriorities
   (prio: integer
   );
   var
      i: integer;
   begin
      for i := 0 to Length(fields) - 1 do
         fields[i].typedef.SetMonitorPriorities(prio)
   end;

function TRecordType.ReadOnly: boolean;
   var i: integer;
   begin
      result := false;
      for i := 0 to Length(fields)-1 do
         if (fields[i].typedef <> nil)
            and
            (fields[i].typedef.ReadOnly)
         then
            result := true
   end;

constructor TPackedRecordField.Create;
   begin
      inherited Create(packed_record_field_definition)
   end;

destructor TPackedRecordField.Destroy;
   begin
      ordtypedef.Release;
      inherited
   end;

function TPackedRecordField.name: string;
   begin
      result := lex.identifiers [identifier_idx]
   end;


// ====================
// TPackedRecordType

constructor TPackedRecordType.CreateFromSourceTokens;
   var
      idx: integer;
      i: integer;
   begin
      inherited Create(packed_record_type);
      assert(lex.token_is_reserved_word(rw_packed));
      src_loc := lex.token.src_loc;
      lex.advance_token;

      if not lex.token_is_reserved_word(rw_record) then
         raise compile_error.Create(err_record_expected);
      lex.advance_token;

      repeat // once per field
         idx := Length(fields);
         SetLength(fields, idx + 1);
         fields[idx] := TPackedRecordField.Create (Self);
         fields[idx].containing_packed_record := Self;

         fields[idx].identifier_src_loc := lex.token.src_loc;
         if lex.token_is_symbol (sym_minus) then
            begin
               fields[idx].kind := anonymous_packed_field;
               lex.advance_token
            end
         else
            begin
               if not lex.token_is_identifier then
                  raise compile_error.Create(err_identifier_expected);
               for i := 0 to Length(fields) - 1 do
                  if fields[i].identifier_idx = lex.token.identifier_idx then
                     raise compile_error.Create(err_duplicate_field_name);
               fields[idx].kind := normal_packed_field;
               fields[idx].identifier_idx := lex.token.identifier_idx;
               lex.advance_token
            end;

         if not lex.token_is_symbol(sym_colon) then
            raise compile_error.Create (err_colon_expected);
         lex.advance_token;

         if (lex.token_is_identifier)
            and
            (CurrentDefinitionTable[lex.token.identifier_idx].definition_kind = type_definition) then
            begin
               if (TTypeDef(CurrentDefinitionTable[lex.token.identifier_idx]).type_kind <> basic_data_type)
                  or
                  (TBasicDataType(CurrentDefinitionTable[lex.token.identifier_idx]).basic_data_type_kind <> ordinal_data_type) then
                  raise compile_error.Create(err_ordinal_type_expected);
               fields[idx].ordtypedef := TOrdinalDataType(CurrentDefinitionTable[lex.token.identifier_idx]);
               fields[idx].ordtypedef.AddRef;
               lex.advance_token
            end
         else // new anonymous type
            if lex.token_is_symbol(sym_left_parenthesis) then
               fields[idx].ordtypedef := TEnumType.CreateFromSourceTokens
         else // all else failed, must be an integer subrange...
            begin
               try
                  fields[idx].ordtypedef := TSubRangeType.CreateFromSourceTokens
               except
                  on e: EDefinitelyNotASubRange do
                     raise compile_error.Create(err_packed_record_field_type_definition_expected, e.src_loc)
               end;
               if TSubRangeType(fields[idx].ordtypedef).ordinal_kind <> ordinal_base_is_integer then
                  raise compile_error.Create(err_integer_subrange_expected, fields[idx].ordtypedef.src_loc)
            end;

         if not lex.token_is_reserved_word(rw_end) then
            begin
               if not lex.token_is_symbol(sym_semicolon) then
                  raise compile_error.Create(err_semicolon_expected);
               lex.advance_token
            end
      until lex.token_is_reserved_word(rw_end);

      lex.advance_token;

      MarkTypeDefinitionAsComplete
   end;

function TPackedRecordType.ReadOnly: boolean;
   var i: integer;
   begin
      result := false;
      for i := 0 to Length(fields)-1 do
         if fields[i].ordtypedef.ReadOnly then
            result := true
   end;

destructor TPackedRecordType.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(fields) - 1 do
         fields[i].Release;
      inherited
   end;

procedure TPackedRecordType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      compatable: boolean;
      i: integer;
   begin
      case def.definition_kind of
         constant_definition:
            raise compile_error.Create(err_incorrect_type, def.src_loc);
         expression_definition:
            begin
               compatable := TExpression(def).typedef = Self;
               if TExpression(def).expression_kind = overlay_expression then
                  for i := 0 to Length(TOverlayType(TExpression(def).typedef).overlaid_variables) - 1 do
                     with TOverlayType(TExpression(def).typedef).overlaid_variables[i] do
                        if anonymous and (typedef = Self) then
                           compatable := true;
               if not compatable then
                  raise compile_error.Create(err_incorrect_type, TExpression(def).src_loc)
            end;
         structured_constant_definition:
            if TStructuredConstant(def).TypeDef <> Self then
               raise compile_error.Create(err_incorrect_type, TStructuredConstant(def).src_loc);
      else
         assert(false)
      end
   end;


// ===================
// TOverlaidVariable
// ===================

procedure TOverlaidVariable.check_for_exposed_identifier_ambiguity
   (new_identifier_idx: TIdentifierIdx;
    new_identifier_src_loc, typedef_src_loc: TSourceLocation
   );
   function err_msg: string;
      begin
         result := format(err_duplicate_overlay_name, [lex.identifiers[new_identifier_idx]])
      end;
   var
      i: integer;
   begin
      if anonymous then
         case typedef.type_kind of
            record_type:
               for i := 0 to Length(TRecordType(typedef).fields) - 1 do
                  if TRecordType(typedef).fields[i].identifier_idx = new_identifier_idx then
                     raise compile_error.Create(err_msg, typedef_src_loc);
            packed_record_type:
               for i := 0 to Length(TPackedRecordType(typedef).fields) - 1 do
                  if TPackedRecordType(typedef).fields[i].identifier_idx = new_identifier_idx then
                     raise compile_error.Create(err_msg, typedef_src_loc);
            overlay_type:
               assert(false, 'not implemented');
            array_type,
            string_type:
               begin
                  // doesn't expose id's
               end;
         else
            assert(false)
         end
      else // not anonymous
         if identifier_idx = new_identifier_idx then
            raise compile_error.Create(err_msg, typedef_src_loc)
   end;

function TOverlaidVariable.matches_anonymous_index_path_signature
   (index_ordinal_kind:
    TOrdinalBaseType;
    index_enum_typedef: TEnumType
   ): boolean;
   begin
      result := anonymous_array_first_index_ordinal_kind = index_ordinal_kind;
      if (anonymous_array_first_index_ordinal_kind = ordinal_base_is_enum)
         and
         (anonymous_array_first_index_enum_typedef <> index_enum_typedef) then
         result := false
   end;

procedure TOverlaidVariable.check_for_index_ambiguity
   (new_anonymous_array_first_index_ordinal_kind: TOrdinalBaseType;
    new_anonymous_array_first_index_enum_typedef: TEnumType;
    new_index_typedef_src_loc: TSourceLocation
   );
   begin
      if matches_anonymous_index_path_signature(new_anonymous_array_first_index_ordinal_kind, new_anonymous_array_first_index_enum_typedef) then
         raise compile_error.Create(err_ambiguous_first_index_base_type_for_anonymous_array, new_index_typedef_src_loc)
   end;


// ===============
// TOverlayType
// ===============

constructor TOverlayType.CreateFromSourceTokens;
   var
      field_idx: integer;
      typedef: TTypeDef;
      typedef_src_loc: TSourceLocation;
      i, j: integer;
   begin
      inherited Create(overlay_type);
      assert(lex.token_is_reserved_word(rw_overlay));
      lex.advance_token;

      if lex.token_is_reserved_word(rw_end) then
         raise compile_error.Create (err_empty_overlay_not_allowed);

      while not lex.token_is_reserved_word(rw_end) do
         begin
            field_idx := Length(overlaid_variables);
            SetLength(overlaid_variables, field_idx + 1);
            overlaid_variables[field_idx] := TOverlaidVariable.Create;

            if (lex.next_token.token_kind = symbol_token) and (lex.next_token.symbol = sym_comma) then
               begin
                  lex.advance_token;
                  raise compile_error.Create (err_comma_not_allowed)
               end;

            if (lex.next_token.token_kind = symbol_token) and (lex.next_token.symbol = sym_colon) then
               begin // not anonymous
                  overlaid_variables[field_idx].anonymous := false;

                  if not lex.token_is_identifier then
                     raise compile_error.Create(err_identifier_expected);
                  for i := 0 to Length(overlaid_variables) - 2 do
                     overlaid_variables[i].check_for_exposed_identifier_ambiguity(lex.token.identifier_idx, lex.token.src_loc, lex.token.src_loc);
                  overlaid_variables[field_idx].identifier_idx := lex.token.identifier_idx;
                  overlaid_variables[field_idx].identifier_src_loc := lex.token.src_loc;
                  lex.advance_token;

                  // token is ":"
                  lex.advance_token
               end
            else
               overlaid_variables[field_idx].anonymous := true;

            typedef_src_loc := lex.token.src_loc;
            typedef := CreateTypeDenoterFromSourceTokens;

            if (typedef.type_kind = string_type)
               and
               (TStringType(typedef).max_length = -1) then
               begin
                  typedef.Release;
                  raise compile_error.Create(err_left_bracket_expected)
               end;

//            if typedef.type_kind = overlay_type then
//               begin
//                  typedef.Release;
//                  raise compile_error.Create (err_nested_overlay_types_not_allowed, typedef_src_loc)
//               end;

            overlaid_variables[field_idx].typedef := typedef;
            overlaid_variables[field_idx].typedef_src_loc := typedef_src_loc;
            overlaid_variables[field_idx].typedef.AddRef;

            case typedef.type_kind of
               array_type:
                  begin
                     overlaid_variables[field_idx].anonymous_array_first_index_ordinal_kind := TArrayType(typedef).index_typedef.ordinal_kind;
                     if overlaid_variables[field_idx].anonymous_array_first_index_ordinal_kind = ordinal_base_is_enum then
                        overlaid_variables[field_idx].anonymous_array_first_index_enum_typedef := TArrayType(typedef).index_typedef.enum_typedef
                  end;
               string_type:
                  overlaid_variables[field_idx].anonymous_array_first_index_ordinal_kind := ordinal_base_is_integer;
            else
               // does not have an index
            end;
            try
               if typedef.ContainsQueueVariables then
                  raise compile_error.Create(err_overlay_may_not_contain_queue_variables, typedef_src_loc);
               if typedef.IsClassSystemType then
                  raise compile_error.Create(err_overlay_may_not_contain_class_variables, typedef_src_loc);
               if typedef.IsMonitorSystemType then
                  raise compile_error.Create(err_overlay_may_not_contain_monitor_variables, typedef_src_loc);
               if typedef.IsProcessSystemType then
                  raise compile_error.Create(err_overlay_may_not_contain_process_variables, typedef_src_loc);

               if overlaid_variables[field_idx].anonymous then
               // check for ambiguities
                  case typedef.type_kind of
                     record_type:
                        for i := 0 to Length(TRecordType(typedef).fields) - 1 do
                           for j := 0 to Length(overlaid_variables) - 2 do
                              overlaid_variables[j].check_for_exposed_identifier_ambiguity(TRecordType(typedef).fields[i].identifier_idx, TRecordType(typedef).fields[i].identifier_src_loc, typedef_src_loc);
                     packed_record_type:
                        for i := 0 to Length(TPackedRecordType(typedef).fields) - 1 do
                           for j := 0 to Length(overlaid_variables) - 2 do
                              if TPackedRecordType(typedef).fields[i].kind = normal_packed_field then
                                 overlaid_variables[j].check_for_exposed_identifier_ambiguity(TPackedRecordType(typedef).fields[i].identifier_idx, TPackedRecordType(typedef).fields[i].identifier_src_loc, typedef_src_loc);
                     array_type:
                        for i := 0 to Length(overlaid_variables) - 2 do
                           overlaid_variables[i].check_for_index_ambiguity(overlaid_variables[field_idx].anonymous_array_first_index_ordinal_kind, overlaid_variables[field_idx].anonymous_array_first_index_enum_typedef, typedef_src_loc);
                     string_type:
                        begin
                           for i := 0 to Length(overlaid_variables) - 2 do
                              overlaid_variables[i].check_for_index_ambiguity(overlaid_variables[field_idx].anonymous_array_first_index_ordinal_kind, overlaid_variables[field_idx].anonymous_array_first_index_enum_typedef, typedef_src_loc)
                        end;
                  else
                     raise compile_error.Create(err_invalid_anonymous_type_for_overlay, typedef_src_loc)
                  end;
            finally
               typedef.Release
            end;

            if not lex.token_is_reserved_word(rw_end) then
               begin
                  if not lex.token_is_symbol(sym_semicolon) then
                     raise compile_error.Create(err_semicolon_or_end_expected);
                  lex.advance_token
               end;
         end;
      lex.advance_token;
      MarkTypeDefinitionAsComplete
   end;

function TOverlayType.ReadOnly: boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(overlaid_variables) - 1 do
         if overlaid_variables[i].typedef.ReadOnly then
            result := true
   end;

destructor TOverlayType.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(overlaid_variables) - 1 do
         overlaid_variables[i].Release;
      inherited
   end;

procedure TOverlayType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      i: integer;
      compatable: boolean;
   begin
      compatable := false;
      case def.definition_kind of
         constant_definition:
            {never compatable since constant doesn't have exact type, and there may be ambiguity within the overlay}
            ;
         expression_definition:
            begin
               compatable := TExpression(def).typedef = Self;
               for i := 0 to Length(overlaid_variables) - 1 do
                  if overlaid_variables[i].typedef = TExpression(def).typedef then
                     compatable := true
            end;
         structured_constant_definition:
            for i := 0 to Length(overlaid_variables) - 1 do
               if overlaid_variables[i].typedef = TStructuredConstant(def).typedef then
                  compatable := true;
      else
         assert(false)
      end;
      if not compatable then
         raise compile_error.Create(err_incorrect_type, def.src_loc)
   end;


// =============
//  TArrayType
// =============

constructor TArrayType.CreateFromSourceTokens;
   begin
      inherited Create(array_type);

      assert(lex.token_is_reserved_word(rw_array));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_bracket) then
         raise compile_error.Create(err_left_bracket_expected);
      lex.advance_token;

      scan_starting_at_index
   end;

constructor TArrayType.CreateFromSourceTokensStartingAtIndex;
   begin
      inherited Create(array_type);
      scan_starting_at_index
   end;

destructor TArrayType.Destroy;
   begin
      index_typedef.Release;
      index_typedef := nil;
      element_typedef.Release;
      element_typedef := nil;
      inherited
   end;

procedure TArrayType.scan_starting_at_index;
   var
      index_src_loc: TSourceLocation;
   begin
      index_src_loc := lex.token.src_loc;

      if (lex.token_is_identifier)
         and
         (CurrentDefinitionTable[lex.token.identifier_idx].definition_kind = type_definition)
      then
         begin
            if (TTypeDef(CurrentDefinitionTable[lex.token.identifier_idx]).type_kind <> basic_data_type)
               or
               (TBasicDataType(CurrentDefinitionTable[lex.token.identifier_idx]).basic_data_type_kind <> ordinal_data_type) then
               raise compile_error.Create(err_ordinal_type_expected);
            index_typedef := TOrdinalDataType(CurrentDefinitionTable[lex.token.identifier_idx]);
            index_typedef.AddRef;

            lex.advance_token
         end
      else
         try
            index_typedef := TSubRangeType.CreateFromSourceTokens
         except
            on e: EDefinitelyNotASubRange do
               raise compile_error.Create(err_subrange_definition_expected, e.src_loc)
         end;

      if (index_typedef.ordinal_kind = ordinal_base_is_enum)
         and
         (index_typedef.enum_typedef.enum_type_kind = specified_value_enum_type) then
         raise compile_error.Create (err_specified_enumeration_not_allowed_as_array_index, index_src_loc);

      if lex.token_is_symbol(sym_comma) then
         begin
            lex.advance_token;
            element_typedef_src_loc := lex.token.src_loc;
            element_typedef := TArrayType.CreateFromSourceTokensStartingAtIndex
         end
      else
         begin
            if not lex.token_is_symbol(sym_right_bracket) then
               raise compile_error.Create(err_right_bracket_expected);
            lex.advance_token;

            if not lex.token_is_reserved_word(rw_of) then
               raise compile_error.Create(err_of_expected);
            lex.advance_token;

            element_typedef_src_loc := lex.token.src_loc;
            element_typedef := CreateTypeDenoterFromSourceTokens;
            if (element_typedef.type_kind = string_type)
               and
               (TStringType(element_typedef).max_length = -1) then
               raise compile_error.Create(err_left_bracket_expected)
         end;
      if IsProcessType (element_typedef) then
         raise compile_error.Create (err_array_may_not_contain_process_variables, element_typedef_src_loc);
      MarkTypeDefinitionAsComplete
   end;

procedure TArrayType.CheckAssignmentCompatability
   (def: TDefinition
   );
   var
      compatable: boolean;
      i: integer;
   begin
      case def.definition_kind of
         constant_definition:
            raise compile_error.Create (err_incorrect_type, def.src_loc);
         expression_definition:
            begin
               compatable := TExpression(def).typedef = Self;
               if TExpression(def).expression_kind = overlay_expression then
                  for i := 0 to Length(TOverlayType(TExpression(def).typedef).overlaid_variables) - 1 do
                     with TOverlayType(TExpression(def).typedef).overlaid_variables[i] do
                        if anonymous and (typedef = Self) then
                           compatable := true;
               if not compatable then
                  raise compile_error.Create(err_incorrect_type, TExpression(def).src_loc);
            end;
         structured_constant_definition:
            if TStructuredConstant(def).TypeDef <> Self then
               raise compile_error.Create (err_incorrect_type, TStructuredConstant(def).src_loc);
      else
         assert(false)
      end
   end;

function TArrayType.ContainsQueueVariables: boolean;
   begin
      result := element_typedef.ContainsQueueVariables
   end;

function TArrayType.ContainsClassVariables: boolean;
   begin
      result := element_typedef.IsClassSystemType
   end;

function TArrayType.ContainsMonitorVariables: boolean;
   begin
      result := element_typedef.IsMonitorSystemType
   end;

function TArrayType.ContainsProcessVariables: boolean;
   begin
      result := element_typedef.IsProcessSystemType
   end;

procedure TArrayType.SetMonitorPriorities
   (prio: integer
   );
   begin
      element_typedef.SetMonitorPriorities(prio)
   end;

function TArrayType.ReadOnly: boolean;
   begin
      result := element_typedef.ReadOnly
   end;

END.
