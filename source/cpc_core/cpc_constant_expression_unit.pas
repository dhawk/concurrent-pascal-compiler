UNIT cpc_constant_expression_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_core_objects_unit;

type
   TCExpression =
      class(TConstant)
         constructor CreateFromSourceTokens;
      private
         procedure process_source_tokens;
      end;

type
   // the following types are public only for testing purposes
   Tcprimary =
      class(TConstant)
         constructor CreateFromSourceTokens;
      end;
   Tcexponentiation =
      class(TConstant)
         constructor CreateFromSourceTokens;
      end;
   Tcfactor =
      class(TConstant)
         constructor CreateFromSourceTokens;
      end;
   Tcterm =
      class(TConstant)
         constructor CreateFromSourceTokens;
      end;
   Tcsimple_expression =
      class(TConstant)
         constructor CreateFromSourceTokens;
      end;

IMPLEMENTATION

uses
   cpc_access_unit,
   cpc_common_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit,
   cpc_target_cpu_unit;


constructor Tcprimary.CreateFromSourceTokens;

   procedure parse_set_constant;
      var
         separator:
            (comma,
             dot_dot
            );
         this_ord, last_ord, i: integer;
         cexpression: TCExpression;
         cexpression_identifier_src_loc: TSourceLocation;
      begin // parse_set_constant
         cexpression := nil;
         this_ord := 0;
         last_ord := 0;
         lex.advance_token;
         constant_kind := set_constant;
         sett := [];
         if lex.token_is_symbol(sym_right_bracket) then
            begin
               set_ordinal_base_type := empty_set_ordinal_base_unknown;
               lex.advance_token
            end
         else
            try
               cexpression_identifier_src_loc := lex.token.src_loc;
               cexpression := TCExpression.CreateFromSourceTokens;
               case cexpression.constant_kind of
                  integer_constant:
                     set_ordinal_base_type := ordinal_base_is_integer;
                  string_constant:
                     set_ordinal_base_type := ordinal_base_is_char;
                  enum_constant:
                     begin
                        set_ordinal_base_type := ordinal_base_is_enum;
                        enum_typedef := cexpression.enum_typedef;
                        enum_typedef.AddRef
                     end;
                  boolean_constant:
                     set_ordinal_base_type := ordinal_base_is_bool
                  else
                     raise compile_error.Create(err_invalid_set_member_type)
               end;

               separator := comma;
               repeat
                  assert(cexpression <> nil);
                  // also operator_source_location is pointing to beginning of cexpression
                  case cexpression.constant_kind of
                     integer_constant:
                        begin
                           if set_ordinal_base_type <> ordinal_base_is_integer then
                              raise compile_error.Create(err_set_element_type_differs_from_previous_set_elements, cexpression_identifier_src_loc);
                           if cexpression.ordinal_value.lt (min_set)
                              or
                              cexpression.ordinal_value.gt (max_set) then
                              raise compile_error.Create(err_set_member_value_outside_legal_range, cexpression_identifier_src_loc);
                           this_ord := integer(cexpression.ordinal_value.AsInteger);
                        end;
                     string_constant:
                        begin
                           if set_ordinal_base_type <> ordinal_base_is_char then
                              raise compile_error.Create(err_set_element_type_differs_from_previous_set_elements, cexpression_identifier_src_loc);
                           if Length(cexpression.s) = 0 then
                              raise compile_error.Create(err_null_chars_not_allowed_in_sets, cexpression_identifier_src_loc);
                           if Length(cexpression.s) > 1 then
                              raise compile_error.Create(err_only_single_chars_allowed_in_sets, cexpression_identifier_src_loc);
                           if int(ord(cexpression.s[1])) > max_set then
                              raise compile_error.Create(err_set_member_value_outside_legal_range, cexpression_identifier_src_loc);
                           this_ord := ord(cexpression.s[1]);
                        end;
                     enum_constant:
                        begin
                           if (set_ordinal_base_type <> ordinal_base_is_enum)
                              or
                              (enum_typedef <> cexpression.enum_typedef) then
                              raise compile_error.Create(err_set_element_type_differs_from_previous_set_elements, cexpression_identifier_src_loc);
                           if cexpression.ordinal_value.gt (max_set) then
                              raise compile_error.Create(err_set_member_value_outside_legal_range, cexpression_identifier_src_loc);
                           this_ord := cexpression.ordinal_value.AsInteger;
                        end;
                     boolean_constant:
                        begin
                           if set_ordinal_base_type <> ordinal_base_is_bool then
                              raise compile_error.Create(err_set_element_type_differs_from_previous_set_elements, cexpression_identifier_src_loc);
                           this_ord := ord(cexpression.b)
                        end;
                  else
                     raise compile_error.Create(err_invalid_set_member_type)
                  end;
                  if separator = comma then
                     sett := sett + [this_ord]
                  else
                     begin
                        if this_ord < last_ord then
                           raise compile_error.Create(err_left_value_must_be_less_than_right_value, cexpression_identifier_src_loc);
                        for i := last_ord + 1 to this_ord do
                           sett := sett + [i]
                     end;
                  last_ord := this_ord;

                  cexpression.Release;
                  cexpression := nil;

                  if lex.token_is_symbol(sym_right_bracket) then
                     lex.advance_token
                  else if lex.token_is_symbol(sym_comma) then
                     begin
                        lex.advance_token;
                        separator := comma;
                        cexpression_identifier_src_loc := lex.token.src_loc;
                        cexpression := TCExpression.CreateFromSourceTokens
                     end
                  else if lex.token_is_symbol(sym_dot_dot) then
                     begin
                        lex.advance_token;
                        separator := dot_dot;
                        cexpression_identifier_src_loc := lex.token.src_loc;
                        cexpression := TCExpression.CreateFromSourceTokens
                     end
                  else
                     raise compile_error.Create(err_comma_or_right_bracket_expected)
               until cexpression = nil
            finally
               cexpression.Release
            end
      end; // parse_set_constant

   var
      cexpression: TCExpression;
      cprimary: Tcprimary;
      cexpression_identifier_src_loc, typedef_src_loc: TSourceLocation;
      access: TAccess;
      typedef: TTypeDef;
   begin     // Tcprimary.CreateFromSourceTokens
      Create;
      if lex.token_is_identifier then
         begin
            case CurrentDefinitionTable[lex.token.identifier_idx].definition_kind of
               constant_definition:
                  begin
                     CopyFrom(TConstant(CurrentDefinitionTable[lex.token.identifier_idx]));
                     lex.advance_token;

                     if (constant_kind = string_constant)
                        and
                        (lex.token_is_symbol(sym_left_bracket)) then
                        begin // indexed constant string
                           lex.advance_token;

                           cexpression := TCExpression.CreateFromSourceTokens;
                           try
                              if cexpression.constant_kind <> integer_constant
                              then
                                 raise compile_error.Create(err_integer_expected, cexpression.src_loc);

                              if cexpression.ordinal_value.lt (1) or cexpression.ordinal_value.gt (Length(s)) then
                                 raise compile_error.Create(err_expression_value_outside_legal_range, cexpression.src_loc);

                              s := s[cexpression.ordinal_value.AsInteger]
                           finally
                              cexpression.Release
                           end;

                           if not lex.token_is_symbol(sym_right_bracket) then
                              raise compile_error.Create(err_right_bracket_expected);
                           lex.advance_token
                        end
                  end;
               structured_constant_definition:
                  begin
                     access := target_cpu.TAccess_CreateFromSourceTokens;
                     try
                        if (access.node_access_kind <> structured_constant_access)
                           and
                           (access.node_structured_constant.StructuredConstantKind <> scSimple)
                        then
                           raise compile_error.Create(err_constant_expected, access.src_loc);
                        CopyFrom(access.node_structured_constant.simple_constant)
                     finally
                        access.Release
                     end
                  end;
            else
               raise compile_error.Create(err_identifier_must_be_for_a_constant)
            end
         end
      else if lex.token_is_symbol(sym_left_parenthesis) then
         begin
            lex.advance_token;
            cexpression := TCExpression.CreateFromSourceTokens;
            CopyFrom(cexpression);
            cexpression.Release;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_not) then
         begin
            lex.advance_token;
            cprimary := Tcprimary.CreateFromSourceTokens;
            try
               if (cprimary.constant_kind <> boolean_constant) then
                  raise compile_error.Create(err_boolean_expected);
               CopyFrom(cprimary)
            finally
               cprimary.Release
            end;
            b := not b
         end
      else if lex.token_is_constant then
         begin
            case lex.token.token_kind of
               integer_constant_token:
                  begin
                     constant_kind := integer_constant;
                     ordinal_value.Assign(lex.token.i)
                  end;
               real_constant_token:
                  begin
                     constant_kind := real_constant;
                     r := lex.token.r
                  end;
               string_constant_token:
                  begin
                     constant_kind := string_constant;
                     s := lex.token.s
                  end;
            else
               assert (false)
            end;
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_true) then
         begin
            lex.advance_token;
            constant_kind := boolean_constant;
            b := true
         end
      else if lex.token_is_reserved_word(rw_false) then
         begin
            lex.advance_token;
            constant_kind := boolean_constant;
            b := false
         end
      else if lex.token_is_reserved_word(rw_ord) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               constant_kind := integer_constant;
               case cexpression.constant_kind of
                  integer_constant:
                     ordinal_value.Assign (cexpression.ordinal_value);
                  enum_constant:
                     ordinal_value.AsInteger := cexpression.ordinal_value.AsInteger;
                  boolean_constant:
                     ordinal_value.AsInteger := ord(cexpression.b);
                  string_constant:
                     begin
                        if Length(cexpression.s) <> 1 then
                           raise compile_error.Create(err_only_single_char_allowed, cexpression_identifier_src_loc);
                        ordinal_value.AsInteger := ord(cexpression.s[1])
                     end;
               else
                  raise compile_error.Create(err_ord_argument_must_be_ordinal, cexpression_identifier_src_loc);
               end
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_chr) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if cexpression.constant_kind <> integer_constant then
                  raise compile_error.Create(err_integer_expected, cexpression_identifier_src_loc);
               if cexpression.ordinal_value.lt (min_char)
                  or
                  cexpression.ordinal_value.gt (max_char) then
                  raise compile_error.Create(err_char_value_outside_legal_range, cexpression_identifier_src_loc);
               constant_kind := string_constant;
               s := chr(cexpression.ordinal_value.AsInteger)
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_succ) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if not (cexpression.constant_kind in [integer_constant, boolean_constant, enum_constant, string_constant]) then
                  raise compile_error.Create(err_ordinal_type_expected, cexpression_identifier_src_loc);
               constant_kind := cexpression.constant_kind;
               case cexpression.constant_kind of
                  integer_constant:
                     begin
                        ordinal_value.Assign (cexpression.ordinal_value);
                        ordinal_value.Add (1);
                     end;
                  boolean_constant:
                     if cexpression.b then // succ(true) => ???
                        raise compile_error.Create(err_result_will_be_out_of_range, src_loc)
                     else // succ (false) => true
                        b := true;
                  enum_constant:
                     begin
                        enum_typedef := cexpression.enum_typedef;
                        enum_typedef.AddRef;
                        if cexpression.ordinal_value.AsInteger = enum_typedef.info.max_value.AsInteger then
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        ordinal_value.Assign (cexpression.ordinal_value);
                        ordinal_value.Add (1)
                     end;
                  string_constant:
                     begin
                        if Length(cexpression.s) <> 1 then
                           raise compile_error.Create(err_ordinal_type_expected, cexpression_identifier_src_loc);
                        if cexpression.s[1] = chr(max_char) then
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        s := chr(ord(cexpression.s[1]) + 1)
                     end;
               else
                  assert(false)
               end;
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_pred) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if not (cexpression.constant_kind in [integer_constant, boolean_constant, enum_constant, string_constant]) then
                  raise compile_error.Create(err_ordinal_type_expected, cexpression_identifier_src_loc);
               constant_kind := cexpression.constant_kind;
               case cexpression.constant_kind of
                  integer_constant:
                     begin
                        ordinal_value.Assign (cexpression.ordinal_value);
                        ordinal_value.Subtract (1)
                     end;
                  boolean_constant:
                     if not cexpression.b then   // pred(false) => ???
                        raise compile_error.Create(err_result_will_be_out_of_range, src_loc)
                     else // pred(true) => false
                        b := false;
                  enum_constant:
                     begin
                        enum_typedef := cexpression.enum_typedef;
                        enum_typedef.AddRef;
                        if cexpression.ordinal_value.AsInteger = enum_typedef.info.min_value.AsInteger then
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        ordinal_value.Assign (cexpression.ordinal_value);
                        ordinal_value.Subtract (1)
                     end;
                  string_constant:
                     begin
                        if Length(cexpression.s) <> 1 then
                           raise compile_error.Create(err_ordinal_type_expected, cexpression_identifier_src_loc);
                        if cexpression.s[1] = chr(min_char) then
                           raise  compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        s := chr(ord(cexpression.s[1]) - 1)
                     end;
               else
                  assert(false)
               end;
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_abs) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if not (cexpression.constant_kind in [integer_constant, real_constant]) then
                  raise compile_error.Create(err_numeric_expression_expected, cexpression_identifier_src_loc);
               constant_kind := cexpression.constant_kind;
               case cexpression.constant_kind of
                  integer_constant:
                     begin
                        ordinal_value.Assign (cexpression.ordinal_value);
                        ordinal_value.Abs
                     end;
                  real_constant:
                     r := abs(cexpression.r);
                  else
                     assert(false)
               end;
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_round) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if not (cexpression.constant_kind in [integer_constant, real_constant]) then
                  raise compile_error.Create(err_numeric_expression_expected, cexpression_identifier_src_loc);
               constant_kind := integer_constant;
               case cexpression.constant_kind of
                  integer_constant:
                     ordinal_value.Assign (cexpression.ordinal_value);
                  real_constant:
                     ordinal_value.AsInteger := round(cexpression.r);
               else
                  assert(false)
               end;
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_trunc) then
         begin
            lex.advance_token;
            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;
            cexpression_identifier_src_loc := lex.token.src_loc;
            cexpression := TCExpression.CreateFromSourceTokens;
            try
               if not (cexpression.constant_kind in [integer_constant, real_constant]) then
                  raise compile_error.Create(err_numeric_expression_expected, cexpression_identifier_src_loc);
               constant_kind := integer_constant;
               case cexpression.constant_kind of
                  integer_constant:
                     ordinal_value.Assign (cexpression.ordinal_value);
                  real_constant:
                     ordinal_value.AsInteger := trunc(cexpression.r);
               else
                  assert(false)
               end;
            finally
               cexpression.Release
            end;
            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_high) then
         begin
            lex.advance_token;

            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            typedef_src_loc := lex.token.src_loc;
            try
               typedef := CreateTypeDenoterFromSourceTokens
            except
               on compile_error do
                  raise compile_error.Create(err_ordinal_type_expected, typedef_src_loc)
            end;
            try
               if (typedef.type_kind <> basic_data_type)
                  or
                  (TBasicDataType(typedef).basic_data_type_kind <> ordinal_data_type) then
                  raise compile_error.Create(err_ordinal_type_expected, typedef_src_loc);

               case TOrdinalDataType(typedef).ordinal_kind of
                  ordinal_base_is_integer:
                     begin
                        constant_kind := integer_constant;
                        ordinal_value.Assign (typedef.info.max_value)
                     end;
                  ordinal_base_is_char:
                     begin
                        constant_kind := string_constant;
                        s := chr ((typedef.info.max_value.AsInteger))
                     end;
                  ordinal_base_is_bool:
                     begin
                        constant_kind := boolean_constant;
                        b := boolean (typedef.info.max_value.AsInteger)
                     end;
                  ordinal_base_is_enum:
                     begin
                        constant_kind := enum_constant;
                        enum_typedef := TOrdinalDataType(typedef).enum_typedef;
                        enum_typedef.AddRef;
                        ordinal_value.Assign (typedef.info.max_value)
                     end;
               else
                  assert (false)
               end
            finally
               typedef.Release
            end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_reserved_word(rw_low) then
         begin
            lex.advance_token;

            if not lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_left_parenthesis_expected);
            lex.advance_token;

            typedef_src_loc := lex.token.src_loc;
            try
               typedef := CreateTypeDenoterFromSourceTokens
            except
               on compile_error do
                  raise compile_error.Create(err_ordinal_type_expected, typedef_src_loc)
            end;
            try
               if (typedef.type_kind <> basic_data_type)
                  or
                  (TBasicDataType(typedef).basic_data_type_kind <> ordinal_data_type) then
                  raise compile_error.Create(err_ordinal_type_expected, typedef_src_loc);

               case TOrdinalDataType(typedef).ordinal_kind of
                  ordinal_base_is_integer:
                     begin
                        constant_kind := integer_constant;
                        ordinal_value.Assign (typedef.info.min_value)
                     end;
                  ordinal_base_is_char:
                     begin
                        constant_kind := string_constant;
                        s := chr ((typedef.info.min_value.AsInteger))
                     end;
                  ordinal_base_is_bool:
                     begin
                        constant_kind := boolean_constant;
                        b := boolean (typedef.info.min_value.AsInteger)
                     end;
                  ordinal_base_is_enum:
                     begin
                        constant_kind := enum_constant;
                        enum_typedef := TOrdinalDataType(typedef).enum_typedef;
                        enum_typedef.AddRef;
                        ordinal_value.Assign (typedef.info.min_value)
                     end;
               else
                  assert (false)
               end
            finally
               typedef.Release
            end;

            if not lex.token_is_symbol(sym_right_parenthesis) then
               raise compile_error.Create(err_right_parenthesis_expected);
            lex.advance_token
         end
      else if lex.token_is_symbol(sym_left_bracket) then
         parse_set_constant
      else
         raise compile_error.Create(err_constant_expected)
   end;      // Tcprimary.CreateFromSourceTokens

constructor Tcexponentiation.CreateFromSourceTokens;
   var
      left: Tcprimary;
   begin
      Create;
      left := Tcprimary.CreateFromSourceTokens;
      CopyFrom(left);
      left.Release
   end;

constructor Tcfactor.CreateFromSourceTokens;
   var
      sign: integer;
      operator_source_location: TSourceLocation;
      cfactor: Tcfactor;
      cexponentiation: Tcexponentiation;
   begin
      Create;
      if lex.token_is_symbol([sym_plus, sym_minus]) then
         begin
            operator_source_location := lex.token.src_loc;
            if lex.token_is_symbol(sym_plus) then
               sign := 1
            else
               sign := -1;
            lex.advance_token;
            cfactor := Tcfactor.CreateFromSourceTokens;
            try
               if not (cfactor.constant_kind in [integer_constant, real_constant]) then
                  raise compile_error.Create('err_object_of_unary_sign_must_be_a_number', operator_source_location);
               CopyFrom(cfactor);
               if sign = -1 then
                  if constant_kind = integer_constant then
                     ordinal_value.Multiply (-1)
                  else
                     r := -r
            finally
               cfactor.Release
            end
         end
      else
         begin
            cexponentiation := Tcexponentiation.CreateFromSourceTokens;
            CopyFrom(cexponentiation);
            cexponentiation.Release
         end
   end;

constructor Tcterm.CreateFromSourceTokens;
   var
      factor: Tcfactor;
      operator_source_location, right_operand_source_location: TSourceLocation;
      op: TSymbolEnum;
      rw: TReservedWordEnum;
   begin
      Create;
      src_loc := lex.token.src_loc;
      factor := Tcfactor.CreateFromSourceTokens;
      CopyFrom(factor);
      factor.Release;
      while lex.token_is_symbol([sym_star, sym_slash]) or
      lex.token_is_reserved_word([rw_div, rw_mod, rw_and]) do
         if lex.token_is_symbol([sym_star, sym_slash]) then
            begin
               op := lex.token.symbol;
               operator_source_location := lex.token.src_loc;
               lex.advance_token;
               right_operand_source_location := lex.token.src_loc;
               factor := Tcfactor.CreateFromSourceTokens;
               try
                  if (op = sym_star) and (constant_kind = set_constant) then
                     begin
                        if not (factor.constant_kind = set_constant) then
                           raise compile_error.Create(err_set_expected, right_operand_source_location);
                        if not IsSameTypeAs(factor) then
                           raise compile_error.Create(err_both_operand_sets_must_be_of_same_type, operator_source_location);
                        constant_kind := set_constant;
                        if set_ordinal_base_type <> empty_set_ordinal_base_unknown
                        then
                           begin
                              set_ordinal_base_type := set_ordinal_base_type;
                              if set_ordinal_base_type = ordinal_base_is_enum then
                                 enum_typedef := enum_typedef
                           end
                        else
                           begin
                              set_ordinal_base_type :=
                              factor.set_ordinal_base_type;
                              if set_ordinal_base_type = ordinal_base_is_enum then
                                 enum_typedef := factor.enum_typedef
                           end;
                        sett := sett * factor.sett;
                     end
                  else
                     begin
                        if (not (constant_kind in [integer_constant, real_constant])) then
                           raise compile_error.Create(err_left_operand_must_be_number, src_loc);
                        if (not (factor.constant_kind in [integer_constant, real_constant])) then
                           raise compile_error.Create(err_right_operand_must_be_number, right_operand_source_location);
                        case op of
                           sym_star:
                              if constant_kind = integer_constant
                              then
                                 if factor.constant_kind = integer_constant then
                                    begin
                                       constant_kind := integer_constant;
                                       ordinal_value.Multiply (factor.ordinal_value)
                                    end
                                 else // right.constant_kind is real
                                    begin
                                       constant_kind := real_constant;
                                       r := ordinal_value.AsReal * factor.r
                                    end
                              else // c.constant_kind is real
                                 if factor.constant_kind = integer_constant then
                                    begin
                                       constant_kind := real_constant;
                                       r := r * factor.ordinal_value.AsReal
                                    end
                                 else // right.constant_kind is real
                                    begin
                                       constant_kind := real_constant;
                                       r := r * factor.r
                                    end;
                           sym_slash:
                              begin
                                 if ((factor.constant_kind = integer_constant)
                                     and
                                     (factor.ordinal_value.eq (0))
                                    )
                                    or
                                    ((factor.constant_kind = real_constant)
                                     and
                                     (factor.r = 0)
                                    ) then
                                    raise compile_error.Create(err_divide_by_zero, right_operand_source_location);
                                 if constant_kind = integer_constant then
                                    begin
                                       if factor.constant_kind = integer_constant
                                       then
                                          r := ordinal_value.AsReal / factor.ordinal_value.AsReal
                                       else
                                          // right.c.constant_kind is real
                                          r := ordinal_value.AsReal / factor.r;
                                       constant_kind := real_constant
                                    end
                                 else // c.constant_kind is real
                                    if factor.constant_kind = integer_constant
                                    then
                                       r := r / factor.ordinal_value.AsReal
                                    else
                                       // right.c.constant_kind is real
                                       r := r / factor.r
                              end;
                        else
                           assert(false)
                        end
                     end
               finally
                  factor.Release
               end
            end
         else if lex.token_is_reserved_word([rw_div, rw_mod, rw_and]) then
            begin
               rw := lex.token.rw;
               operator_source_location := lex.token.src_loc;
               lex.advance_token;
               right_operand_source_location := lex.token.src_loc;
               factor := Tcfactor.CreateFromSourceTokens;
               try
                  if rw = rw_and then
                     begin
                        if constant_kind <> boolean_constant then
                           raise compile_error.Create(err_left_operand_must_be_boolean, src_loc);
                        if factor.constant_kind <> boolean_constant then
                           raise compile_error.Create(err_right_operand_must_be_boolean, right_operand_source_location);
                        constant_kind := boolean_constant;
                        b := b and factor.b
                     end
                  else // div or mod
                     begin
                        if constant_kind <> integer_constant then
                           raise compile_error.Create(err_left_operand_must_be_integer, src_loc);
                        if factor.constant_kind <> integer_constant then
                           raise compile_error.Create(err_right_operand_must_be_integer, right_operand_source_location);
                        constant_kind := integer_constant;
                        case rw of
                           rw_div:
                              ordinal_value.Divide (factor.ordinal_value);
                           rw_mod:
                              case target_cpu.mod_operator_implementation of
                                 iso_pascal_mod_operator_implementation:
                                    ordinal_value.iso_std_pascal_mod (factor.ordinal_value);
                                 delphi_mod_operator_implemenation:
                                    ordinal_value.delphi_mod (factor.ordinal_value);
                              else
                                 assert (false)
                              end
                        end
                     end
               finally
                  factor.Release
               end
            end
   end;

constructor Tcsimple_expression.CreateFromSourceTokens;
   var
      right_operand: Tcterm;
      left_operand_source_location, operator_source_location,
      right_operand_source_location: TSourceLocation;
      op: TSymbolEnum;
   begin
      Create;
      left_operand_source_location := lex.token.src_loc;
      right_operand := Tcterm.CreateFromSourceTokens;
      CopyFrom(right_operand);
      right_operand.Release;
      while (lex.token_is_symbol([sym_plus, sym_minus]))
            or
            (lex.token_is_reserved_word(rw_or))
            or
            (lex.token_is_reserved_word(rw_xor)) do
         begin
            operator_source_location := lex.token.src_loc;
            if lex.token_is_reserved_word(rw_or) then
               begin
                  lex.advance_token;
                  right_operand_source_location := lex.token.src_loc;
                  right_operand := Tcterm.CreateFromSourceTokens;
                  try
                     if not (constant_kind = boolean_constant) then
                        raise compile_error.Create(err_left_operand_must_be_boolean, left_operand_source_location);
                     if not (right_operand.constant_kind = boolean_constant) then
                        raise compile_error.Create(err_right_operand_must_be_boolean, right_operand_source_location);
                     b := b or right_operand.b
                  finally
                     right_operand.Release
                  end
               end
            else if lex.token_is_reserved_word(rw_xor) then
               begin
                  lex.advance_token;
                  right_operand_source_location := lex.token.src_loc;
                  right_operand := Tcterm.CreateFromSourceTokens;
                  try
                     if not (constant_kind = boolean_constant) then
                        raise compile_error.Create(err_left_operand_must_be_boolean, left_operand_source_location);
                     if not (right_operand.constant_kind = boolean_constant) then
                        raise compile_error.Create(err_right_operand_must_be_boolean, right_operand_source_location);
                     b := b xor right_operand.b
                  finally
                     right_operand.Release
                  end
               end
            else // + or -
               begin
                  op := lex.token.symbol;
                  lex.advance_token;
                  right_operand_source_location := lex.token.src_loc;
                  right_operand := Tcterm.CreateFromSourceTokens;
                  try
                     if (op = sym_plus) and (constant_kind = string_constant) then
                        begin
                           if right_operand.constant_kind <> string_constant then
                              raise compile_error.Create(err_right_operand_must_be_string, right_operand_source_location);
                           s := s + right_operand.s
                        end
                     else if constant_kind = set_constant then
                        begin
                           if right_operand.constant_kind <> set_constant then
                              raise compile_error.Create(err_set_expected,
                              right_operand_source_location);
                           if not IsSameTypeAs(right_operand) then
                              raise compile_error.Create(err_both_operand_sets_must_be_of_same_type, operator_source_location);
                           case op of
                              sym_plus:
                                 sett := sett + right_operand.sett;
                              sym_minus:
                                 sett := sett - right_operand.sett;
                           else
                              assert(false)
                           end;
                           if set_ordinal_base_type = empty_set_ordinal_base_unknown then
                              begin
                                 set_ordinal_base_type := right_operand.set_ordinal_base_type;
                                 if set_ordinal_base_type = ordinal_base_is_enum then
                                    enum_typedef := right_operand.enum_typedef
                              end
                        end
                     else
                        begin
                           if not (constant_kind in [integer_constant, real_constant]) then
                              raise compile_error.Create(err_left_operand_must_be_number, left_operand_source_location);
                           if not (right_operand.constant_kind in [integer_constant, real_constant]) then
                              raise compile_error.Create(err_right_operand_must_be_number, right_operand_source_location);
                           if op = sym_plus then
                              case constant_kind of
                                 integer_constant:
                                    case right_operand.constant_kind of
                                       integer_constant:
                                          ordinal_value.Add (right_operand.ordinal_value);
                                       real_constant:
                                          begin
                                             constant_kind := real_constant;
                                             r := ordinal_value.AsReal + right_operand.r
                                          end;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right_operand.constant_kind of
                                       integer_constant:
                                          r := r + right_operand.ordinal_value.AsReal;
                                       real_constant:
                                          r := r + right_operand.r;
                                    else
                                       assert(false)
                                    end;
                                 else
                                    assert(false)
                              end
                           else // operator is -
                              case constant_kind of
                                 integer_constant:
                                    case right_operand.constant_kind of
                                       integer_constant:
                                          ordinal_value.Subtract (right_operand.ordinal_value);
                                       real_constant:
                                          begin
                                             constant_kind := real_constant;
                                             r := ordinal_value.AsReal - right_operand.r
                                          end;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right_operand.constant_kind of
                                       integer_constant:
                                          r := r - right_operand.ordinal_value.AsReal;
                                       real_constant:
                                          r := r - right_operand.r;
                                    else
                                       assert(false)
                                    end;
                              else
                                 assert(false)
                              end
                        end
                  finally
                     right_operand.Release
                  end
               end
         end;
   end;

constructor TCExpression.CreateFromSourceTokens;
   begin
      Create;
      src_loc := lex.token.src_loc;
      process_source_tokens
   end;

procedure TCExpression.process_source_tokens;
   var
      left, right: Tcsimple_expression;
      in_operator: boolean;
      op: TSymbolEnum; // valid only if in_operator is false
      left_operand_source_location, operator_source_location,
      right_operand_source_location: TSourceLocation;
      compatible: boolean;
   begin
      op := TSymbolEnum(0); // to suppress uniitialized variable warning
      left_operand_source_location := lex.token.src_loc;
      left := Tcsimple_expression.CreateFromSourceTokens;
      try
         if not (lex.token_is_symbol([sym_equals, sym_not_equals, sym_less_than, sym_greater_than, sym_less_than_or_equal, sym_greater_than_or_equal])
            or
            lex.token_is_reserved_word(rw_in))
         then
            CopyFrom(left)
         else
            begin // relational expression, true or false
               constant_kind := boolean_constant;
               in_operator := lex.token_is_reserved_word(rw_in);
               if not in_operator then
                  op := lex.token.symbol;
               operator_source_location := lex.token.src_loc;
               lex.advance_token;
               right_operand_source_location := lex.token.src_loc;
               right := Tcsimple_expression.CreateFromSourceTokens;
               try
                  if in_operator then
                     begin
                        if not (left.constant_kind in [integer_constant, string_constant, enum_constant, boolean_constant]) then
                           raise compile_error.Create(err_left_element_must_be_ordinal, left_operand_source_location);
                        if right.constant_kind <> set_constant then
                           raise compile_error.Create(err_right_operand_must_be_a_set, right_operand_source_location);
                        if not (((left.constant_kind = integer_constant)
                                 and
                                 (right.set_ordinal_base_type in [ordinal_base_is_integer, empty_set_ordinal_base_unknown])
                                )
                                or
                                ((left.constant_kind = string_constant)
                                 and
                                 (right.set_ordinal_base_type = ordinal_base_is_char)
                                 and
                                 (Length(left.s) = 1)
                                )
                                or
                                ((left.constant_kind = string_constant)
                                 and
                                 (right.set_ordinal_base_type = empty_set_ordinal_base_unknown)
                                )
                                or
                                ((left.constant_kind = enum_constant)
                                 and
                                 (right.set_ordinal_base_type = ordinal_base_is_enum)
                                 and
                                 (left.enum_typedef = right.enum_typedef)
                                )
                                or
                                ((left.constant_kind = enum_constant)
                                 and
                                 (right.set_ordinal_base_type = empty_set_ordinal_base_unknown)
                                )
                                or
                                ((left.constant_kind = boolean_constant)
                                 and
                                 (right.set_ordinal_base_type in [ordinal_base_is_bool, empty_set_ordinal_base_unknown])
                                )
                               ) then
                           raise compile_error.Create(err_left_and_right_operand_types_dont_agree, operator_source_location);
                        case left.constant_kind of
                           string_constant:
                              b := ord(left.s[1]) in right.sett;
                           integer_constant,
                           enum_constant:
                              if left.ordinal_value.lt(0) or left.ordinal_value.gt(255) then
                                 b := false
                              else
                                 b := left.ordinal_value.AsByte(0) in right.sett;
                           boolean_constant:
                              b := ord(left.b) in right.sett;
                        else
                           assert(false)
                        end
                     end
                  else // not "in" operator
                     begin
                        compatible := (left.IsSameTypeAs(right))
                                      or
                                      ((left.constant_kind in [integer_constant, real_constant])
                                       and
                                       (right.constant_kind in [integer_constant, real_constant])
                                      );
                        if not compatible then
                           raise compile_error.Create(err_operands_not_compatible_with_operator, operator_source_location);

                        case op of
                           sym_equals:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.eq (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal = right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r = right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r = right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s = right.s;
                                 enum_constant:
                                    b := left.ordinal_value.AsInteger = right.ordinal_value.AsInteger;
                                 boolean_constant:
                                    b := left.b = right.b;
                                 set_constant:
                                    b := left.sett = right.sett;
                              else
                                 assert(false)
                              end;
                           sym_not_equals:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.ne (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal <> right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r <> right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r <> right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s <> right.s;
                                 enum_constant:
                                    b := left.ordinal_value.ne (right.ordinal_value);
                                 boolean_constant:
                                    b := left.b <> right.b;
                                 set_constant:
                                    b := left.sett <> right.sett;
                              else
                                 assert(false)
                              end;
                           sym_less_than:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.lt (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal < right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r < right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r < right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s < right.s;
                                 enum_constant:
                                    b := left.ordinal_value.AsInteger < right.ordinal_value.AsInteger;
                                 boolean_constant:
                                    b := left.b < right.b;
                                 set_constant:
                                    raise compile_error.Create(err_operator_not_valid_for_sets, operator_source_location);
                              else
                                 assert(false)
                              end;
                           sym_greater_than:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.gt (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal > right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r > right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r > right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s > right.s;
                                 enum_constant:
                                    b := left.ordinal_value.AsInteger > right.ordinal_value.AsInteger;
                                 boolean_constant:
                                    b := left.b > right.b;
                                 set_constant:
                                    raise compile_error.Create(err_operator_not_valid_for_sets, operator_source_location);
                              else
                                 assert(false)
                              end;
                           sym_less_than_or_equal:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.le (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal <= right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r <= right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r <= right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s <= right.s;
                                 enum_constant:
                                    b := left.ordinal_value.le (right.ordinal_value);
                                 boolean_constant:
                                    b := left.b <= right.b;
                                 set_constant:
                                    b := left.sett <= right.sett;
                              else
                                 assert(false)
                              end;
                           sym_greater_than_or_equal:
                              case left.constant_kind of
                                 integer_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.ordinal_value.ge (right.ordinal_value);
                                       real_constant:
                                          b := left.ordinal_value.AsReal >= right.r;
                                    else
                                       assert(false)
                                    end;
                                 real_constant:
                                    case right.constant_kind of
                                       integer_constant:
                                          b := left.r >= right.ordinal_value.AsReal;
                                       real_constant:
                                          b := left.r >= right.r;
                                    else
                                       assert(false)
                                    end;
                                 string_constant:
                                    b := left.s >= right.s;
                                 enum_constant:
                                    b := left.ordinal_value.ge (right.ordinal_value);
                                 boolean_constant:
                                    b := left.b >= right.b;
                                 set_constant:
                                    b := left.sett >= right.sett;
                              else
                                 assert(false)
                              end;
                        else
                           assert(false)
                        end
                     end
               finally
                  right.Release
               end
            end
      finally
         left.Release
      end
   end;

END.
