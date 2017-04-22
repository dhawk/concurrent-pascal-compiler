UNIT cpc_expressions_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_access_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_multi_precision_integer_unit,
   cpc_source_analysis_unit,
   SysUtils;

type
   TAbsFunctionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TChrTypeConversionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TConstantPrimary =
      class(TExpression)
         the_constant: TDefinition;  // either TConstant or TStructuredConstant
         constructor CreateFromSourceTokens;
         constructor CreateFromStructuredConstantAccess
            (access: TAccess
            );
         constructor CreateFromReservedWordConstant;
         constructor CreateFromIdentifierConstant;
         constructor CreateFromConstant
            (_constant: TConstant;
             _src_loc: TSourceLocation
            );
         destructor Destroy;
            override;
         function contains_constant: boolean;
            override;
         function contains_real_constant: boolean;
            override;
         function contains_integer_constant: boolean;
            override;
         function constant: TConstant;
            override;
         function ordinal_constant_value: TLargestPascalSupportedInt;
            override;
         function real_constant_value: real;
            override;
         function boolean_constant_value: boolean;
            override;
         procedure check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
            override;
      end;

   TEmptyFunctionPrimary =
      class(TExpression)
         access: TAccess;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
      end;

   TFunctionAccessPrimary =
      class(TExpression)
         access: TAccess;
         actual_parameters: TArrayOfTDefinition;
         call_record: TCallRecord;
         constructor CreateFromSourceTokens
            (_access: TAccess
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         procedure check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TNotPrimary =
      class(TExpression)
         boolean_expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TPredFunctionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TRelOp =
      (no_relop,
       relop_equals,
       relop_notequals,
       relop_lt,
       relop_gt,
       relop_le,
       relop_ge,
       relop_in
      );
   TRelationalExpression =
      class(TExpression)
         left_simple_expression: TExpression;
         relop: TRelOp;
         right_simple_expression: TExpression; // will be nil if relop = no_relop
         comparison_info: TTypeInfo;  // valid for ordinal equals, notequals, lt, gt, le, ge
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TRoundFunctionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TSetConstructorPrimary =
      class(TExpression)
         // if set containts only constants, they will be in base class constant
         // otherwise constant_members and member_designators will contain members
         constant_members: TConstant;
         variable_members:
            array of
               record
                  first: TExpression;
                  last: TExpression // used for first..second - nil if no ..
               end;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
         procedure check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
            override;
      end;

   TSuccFunctionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TTruncFunctionPrimary =
      class(TExpression)
         expr: TExpression;
         constructor CreateFromSourceTokens;
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TStrPosPrimary =
      class (TExpression)
         access: TAccess;
         constructor Create
            (acc: TAccess
            );
         procedure MarkAsReachable;
            override;
         destructor Destroy;
            override;
      end;

   TUnaryMinusPrimary =
      class(TExpression)
         primary: TExpression;
         constructor CreateFromSourceTokens;
         constructor CreateFromExpression
            (exp: TExpression
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   TVariableAccessPrimary =
      class(TExpression)
         access: TAccess;
         constructor Create
            (acc: TAccess
            );
         destructor Destroy;
            override;
         procedure MarkAsReachable;
            override;
         procedure check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
            override;
         function CheckForProhibitedDelayCall (err_msg: string): boolean;
            override;
      end;

   EExpressionSimplification =
      class(Exception)
         simplified_expr: TExpression;
         constructor Create
            (expr: TExpression
            );
      end;

   EPrimaryExpressionSimplification =
      class(EExpressionSimplification)
      end;

   ERelationalExpressionSimplification =
      class(EExpressionSimplification)
      end;

   EConstantExpressionSimplification =
      class (EExpressionSimplification)
      end;

function CreateExpressionFromSourceTokens: TExpression;
function CreateBooleanExpressionFromSourceTokens: TExpression;
function CreateFactorFromSourceTokens: TExpression;
function CreatePrimaryFromSourceTokens: TExpression;


IMPLEMENTATION

uses
   cpc_blocks_unit,
   cpc_common_unit,
   cpc_simple_expression_unit,
   cpc_target_cpu_unit;

function CreateExpressionFromSourceTokens: TExpression;
   begin
      try
         result := target_cpu.TRelationalExpression_CreateFromSourceTokens;
      except
         on e: ERelationalExpressionSimplification do
            result := e.simplified_expr;
         on e: EConstantExpressionSimplification do
            result := e.simplified_expr
      end
   end;

function CreateBooleanExpressionFromSourceTokens: TExpression;
   var
      src_loc: TSourceLocation;
   begin
      src_loc := lex.token.src_loc;
      try
         try
            result := target_cpu.TRelationalExpression_CreateFromSourceTokens;
         except
            on e: compile_error do
               if (e.Message = err_expression_expected)
                  and
                  (e.source_location.same_location(src_loc)) then
                  raise compile_error.Create(err_boolean_expression_expected, e.source_location)
               else
                  raise
         end
      except
         on e: ERelationalExpressionSimplification do
            result := e.simplified_expr
      end;
      if result.expression_kind <> boolean_expression then
         begin
            result.Release;
            raise compile_error.Create(err_boolean_expression_expected, src_loc)
         end
   end;

function CreatePrimaryFromSourceTokens: TExpression;
   var
      access: TAccess;
      src_loc: TSourceLocation;
      typedef: TTypeDef;
      cnst: TConstant;
      c: TConstant;
   begin
      try
         result := nil; // suppress warning
         if lex.token_is_constant then
            result := target_cpu.TConstantPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word([rw_true, rw_false]) then
            result := target_cpu.TConstantPrimary_CreateFromReservedWordConstant
         else if lex.token_is_identifier then
            begin
               access := target_cpu.TAccess_CreateFromSourceTokens;
               try
                  case access.node_access_kind of
                     variable_access:
                        case access.node_attribute of
                          rw_strpos:
                             result := target_cpu.TStrPosPrimary_Create (access);
                        else
                           result := target_cpu.TVariableAccessPrimary_Create(access)
                        end;
                     function_access:
                        result := target_cpu.TFunctionAccessPrimary_CreateFromSourceTokens(access);
                     procedure_access:
                        raise compile_error.Create(err_procedure_has_no_result, access.node_id_src_loc);
                     property_access:
                        if access.node_property.get_func = nil then
                           raise compile_error.Create(err_write_only_property, access.node_id_src_loc)
                        else
                           result := target_cpu.TFunctionAccessPrimary_CreateFromSourceTokens(access);
                     constant_access,
                     structured_constant_access:
                        result := target_cpu.TConstantPrimary_CreateFromStructuredConstantAccess(access);
                  else
                     assert(false)
                  end
               finally
                  access.Release
               end
            end
         else if lex.token_is_symbol(sym_left_parenthesis) then
            begin
               lex.advance_token;
               result := CreateExpressionFromSourceTokens;
               if not lex.token_is_symbol(sym_right_parenthesis) then
                  begin
                     result.Release;
                     raise compile_error.Create(err_right_parenthesis_expected)
                  end;
               lex.advance_token
            end
         else if lex.token_is_reserved_word(rw_not) then
            result := target_cpu.TNotPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_chr) then
            result := target_cpu.TChrTypeConversionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_ord) then
            begin
               lex.advance_token;

               if not lex.token_is_symbol(sym_left_parenthesis) then
                  raise compile_error.Create(err_left_parenthesis_expected);
               lex.advance_token;

               src_loc := lex.token.src_loc;
               result := CreateExpressionFromSourceTokens;
               try
                  if not (result.expression_kind in ordinal_expression_kinds) then
                     raise compile_error.Create(err_ordinal_expression_expected, src_loc);
                  if not lex.token_is_symbol(sym_right_parenthesis) then
                     raise compile_error.Create(err_right_parenthesis_expected);
                  lex.advance_token
               except
                  on compile_error do
                     begin
                        result.Release;
                        raise
                     end
               end;
               result.expression_kind := integer_expression;
               if result.contains_constant then
                  begin
                     c := nil;  // suppress warning
                     case result.constant.constant_kind of
                        integer_constant,
                        enum_constant:
                           c := TConstant.CreateIntegerConstant (result.constant.ordinal_value);
                        string_constant:
                           c := TConstant.CreateIntegerConstant (ord(result.constant.s[1]));
                        boolean_constant:
                           c := TConstant.CreateIntegerConstant (ord(result.constant.b));
                     else
                        assert(false)
                     end;
                     result.constant.Release;
                     TConstantPrimary(result).the_constant := c
                  end
            end
         else if lex.token_is_symbol(sym_left_bracket) then
            result := target_cpu.TSetConstructorPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_round) then
            result := target_cpu.TRoundFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_trunc) then
            result := target_cpu.TTruncFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_empty) then
            result := target_cpu.TEmptyFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_abs) then
            result := target_cpu.TAbsFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_succ) then
            result := target_cpu.TSuccFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_pred) then
            result := target_cpu.TPredFunctionPrimary_CreateFromSourceTokens
         else if lex.token_is_reserved_word(rw_high) then
            begin
               lex.advance_token;

               if not lex.token_is_symbol(sym_left_parenthesis) then
                  raise compile_error.Create(err_left_parenthesis_expected);
               lex.advance_token;

               src_loc := lex.token.src_loc;
               try
                  typedef := CreateTypeDenoterFromSourceTokens
               except
                  on compile_error do
                     raise compile_error.Create(err_ordinal_type_expected, src_loc)
               end;
               cnst := nil; // suppress compiler warning
               try
                  if (typedef.type_kind <> basic_data_type)
                     or
                     (TBasicDataType(typedef).basic_data_type_kind <> ordinal_data_type) then
                     raise compile_error.Create(err_ordinal_type_expected, src_loc);

                  case TOrdinalDataType(typedef).ordinal_kind of
                     ordinal_base_is_integer:
                        cnst := TConstant.CreateIntegerConstant(typedef.info.max_value);
                     ordinal_base_is_char:
                        cnst := TConstant.CreateStringConstant (chr ((typedef.info.max_value.AsInteger)));
                     ordinal_base_is_bool:
                        cnst := TConstant.CreateBooleanConstant (boolean (typedef.info.max_value.AsInteger));
                     ordinal_base_is_enum:
                        cnst := TConstant.CreateEnumConstant (TOrdinalDataType(typedef).enum_typedef, typedef.info.max_value.AsInteger);
                  else
                     assert (false)
                  end
               finally
                  typedef.Release
               end;
               result := target_cpu.TConstantPrimary_CreateFromConstant (cnst, src_loc);

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

               src_loc := lex.token.src_loc;
               try
                  typedef := CreateTypeDenoterFromSourceTokens
               except
                  on compile_error do
                     raise compile_error.Create(err_ordinal_type_expected, src_loc)
               end;
               cnst := nil;  // suppress compiler warning
               try
                  if (typedef.type_kind <> basic_data_type)
                     or
                     (TBasicDataType(typedef).basic_data_type_kind <> ordinal_data_type) then
                     raise compile_error.Create(err_ordinal_type_expected, src_loc);

                  case TOrdinalDataType(typedef).ordinal_kind of
                     ordinal_base_is_integer:
                        cnst := TConstant.CreateIntegerConstant(typedef.info.min_value);
                     ordinal_base_is_char:
                        cnst := TConstant.CreateStringConstant (chr ((typedef.info.min_value.AsInteger)));
                     ordinal_base_is_bool:
                        cnst := TConstant.CreateBooleanConstant (boolean (typedef.info.min_value.AsInteger));
                     ordinal_base_is_enum:
                        cnst := TConstant.CreateEnumConstant (TOrdinalDataType(typedef).enum_typedef, typedef.info.min_value.AsInteger);
                  else
                     assert (false)
                  end
               finally
                  typedef.Release
               end;
               result := target_cpu.TConstantPrimary_CreateFromConstant (cnst, src_loc);

               if not lex.token_is_symbol(sym_right_parenthesis) then
                  raise compile_error.Create(err_right_parenthesis_expected);
               lex.advance_token
            end
         else
            raise compile_error.Create(err_expression_expected)
      except
         on e: EPrimaryExpressionSimplification do
            result := e.simplified_expr
      end
   end;

function CreateFactorFromSourceTokens: TExpression;
   var
      unary_sign_specified: boolean;
      sign: integer;
   begin
      sign := 1;
      unary_sign_specified := false;
      while lex.token_is_symbol([sym_plus, sym_minus]) do
         begin
            unary_sign_specified := true;
            if lex.token_is_symbol(sym_minus) then
               sign := -sign;
            lex.advance_token
         end;
      if sign = -1 then
         try
            result := target_cpu.TUnaryMinusPrimary_CreateFromSourceTokens
         except
            on e: EPrimaryExpressionSimplification do
               result := e.simplified_expr
         end
      else
         begin
            result := CreatePrimaryFromSourceTokens;
            if unary_sign_specified and not (result.expression_kind in [integer_expression, real_expression]) then
               raise compile_error.Create(err_unary_operator_not_allowed_for_this_operand_type, result.src_loc)
         end
   end;


// =====================
//  TAbsFunctionPrimary

constructor TAbsFunctionPrimary.CreateFromSourceTokens;
   var t: TMultiPrecisionInteger;
   begin
      inherited Create;

      t := TMultiPrecisionInteger.Create;
      try
         assert(lex.token_is_reserved_word(rw_abs));
         lex.advance_token;

         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         expr := CreateExpressionFromSourceTokens;

         if not lex.token_is_symbol(sym_right_parenthesis) then
            raise compile_error.Create(err_right_parenthesis_expected);
         lex.advance_token;

         case expr.expression_kind of
            integer_expression:
               begin
                  if expr.contains_constant then
                     begin
                        t.Assign (expr.constant.ordinal_value);
                        t.Abs;
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(t), src_loc))
                     end;

                  if expr.info.min_value.ge(0) then
                     raise EPrimaryExpressionSimplification.Create(expr);

                  expression_kind := integer_expression;
                  if expr.info.max_value.lt(0) then
                     begin  // always negative
                        info.min_value.Assign (expr.info.max_value);
                        info.min_value.Abs;
                        info.max_value.Assign (expr.info.min_value);
                        info.max_value.Abs
                     end
                  else    // negative or positive
                     begin
                        info.min_value.AsInteger := 0;
                        info.max_value.Assign (expr.info.min_value);
                        info.max_value.Abs;
                        t.Assign (expr.info.max_value);
                        t.Abs;
                        info.max_value.Max(t)
                     end
               end;
            real_expression:
               begin
                  expression_kind := real_expression;
                  if expr.contains_constant then
                     raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateRealConstant(abs(expr.constant.r)), src_loc))
               end;
         else
            raise compile_error.Create(err_numeric_expression_expected, expr.src_loc);
         end
      finally
         t.Free
      end
   end;

destructor TAbsFunctionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TAbsFunctionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TAbsFunctionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


// =====================
// TChrTypeConversion

constructor TChrTypeConversionPrimary.CreateFromSourceTokens;
   var
      expr_src_loc: TSourceLocation;
   begin
      inherited Create;

      assert(lex.token_is_reserved_word(rw_chr));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      expr_src_loc := lex.token.src_loc;
      expr := CreateExpressionFromSourceTokens;

      copy_expression_info(expr);

      expression_kind := char_expression;
      info.min_value.AsInteger := min_char;
      info.max_value.AsInteger := max_char;
      typedef := target_cpu.get_supported_data_type('char');

      if not (expression_kind in ordinal_expression_kinds) then
         raise compile_error.Create(err_ordinal_expression_expected, expr_src_loc);
      expression_kind := char_expression;

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      if expr.contains_constant then
         begin
            if not (expr.constant.constant_kind in [integer_constant, enum_constant, string_constant, boolean_constant]) then
               raise compile_error.Create (err_ordinal_expression_expected, expr_src_loc);
            if (expr.constant.AsOrdinal < min_char) or (expr.constant.AsOrdinal > max_char) then
               raise compile_error.Create(err_char_value_outside_legal_range, expr_src_loc);
            raise  EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateStringConstant(chr(expr.constant.AsOrdinal)), src_loc))
         end;

      if info.max_value.lt(min_char) or info.min_value.gt(max_char) then
         raise compile_error.Create(err_char_value_outside_legal_range, expr_src_loc)
   end;

destructor TChrTypeConversionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TChrTypeConversionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TChrTypeConversionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


//===================
//  TConstantPrimary

constructor TConstantPrimary.CreateFromSourceTokens;
   begin
      inherited Create;
      src_loc := lex.token.src_loc;

      assert(lex.token_is_constant);

      case lex.token.token_kind of
         integer_constant_token:
            begin
               expression_kind := integer_expression;
               the_constant := TConstant.CreateIntegerConstant (lex.token.i);
               info.min_value.Assign (constant.ordinal_value);
               info.max_value.Assign (constant.ordinal_value)
            end;
         real_constant_token:
            begin
               expression_kind := real_expression;
               the_constant := TConstant.CreateRealConstant (lex.token.r)
            end;
         string_constant_token:
            begin
               the_constant := TConstant.CreateStringConstant (lex.token.s);
               if Length(constant.s) = 1 then
                  begin
                     expression_kind := char_expression;
                     info.min_value.AsInteger := ord(constant.s[1]);
                     info.max_value.AsInteger := ord(constant.s[1])
                  end
               else
                  begin
                     expression_kind := string_expression;
                     info.min_value.AsInteger := Length(constant.s);
                     info.max_value.AsInteger := Length(constant.s);
                     target_cpu.record_anonymous_string_constant (constant.s)
                  end
            end;
         else
            assert(false)
      end;
      lex.advance_token
   end;

constructor TConstantPrimary.CreateFromStructuredConstantAccess
   (access: TAccess
   );

   procedure handle_simple_constant (c: TDefinition);
      begin
         the_constant := c;
         the_constant.AddRef;
         case constant.constant_kind of
            boolean_constant:
               begin
                  expression_kind := boolean_expression;
                  info.min_value.Assign (constant.ordinal_value);
                  info.max_value.Assign (constant.ordinal_value)
               end;
            integer_constant:
               begin
                  expression_kind := integer_expression;
                  info.min_value.Assign (constant.ordinal_value);
                  info.max_value.Assign (constant.ordinal_value)
               end;
            real_constant:
               expression_kind := real_expression;
            string_constant:
               if Length(constant.s) = 1 then
                  begin
                     expression_kind := char_expression;
                     info.min_value.AsInteger := ord(constant.s[1]);
                     info.max_value.AsInteger := ord(constant.s[1])
                  end
               else
                  begin
                     expression_kind := string_expression;
                     info.min_value.AsInteger := Length(constant.s);
                     info.max_value.AsInteger := Length(constant.s);
                     target_cpu.record_anonymous_string_constant (constant.s)
                  end;
            enum_constant:
               begin
                  expression_kind := enum_expression;
                  enum_typedef := constant.enum_typedef;
                  info.min_value.Assign (constant.ordinal_value);
                  info.max_value.Assign (constant.ordinal_value)
               end;
            set_constant:
               begin
                  expression_kind := set_expression;
                  enum_typedef := constant.enum_typedef;
                  set_min_max_for_set_constant
               end;
         else
            assert(false)
         end
      end;    // handle_simple_constant

   procedure handle_structured_constant (sc: TStructuredConstant);
      begin
         the_constant := sc;
         the_constant.AddRef;
         typedef := access.node_typedef;
         case typedef.type_kind of
            basic_data_type:
               begin
                  assert (sc.StructuredConstantKind = scSimple);
                  case sc.simple_constant.constant_kind of
                     boolean_constant:
                        begin
                           expression_kind := boolean_expression;
                           info.min_value.Assign (sc.simple_constant.ordinal_value);
                           info.max_value.Assign (sc.simple_constant.ordinal_value)
                        end;
                     integer_constant:
                        begin
                           expression_kind := integer_expression;
                           info.min_value.Assign (sc.simple_constant.ordinal_value);
                           info.max_value.Assign (sc.simple_constant.ordinal_value)
                        end;
                     real_constant:
                        expression_kind := real_expression;
                     string_constant:
                        if Length(sc.simple_constant.s) = 1 then
                           begin
                              expression_kind := char_expression;
                              info.min_value.AsInteger := ord(sc.simple_constant.s[1]);
                              info.max_value.AsInteger := ord(sc.simple_constant.s[1])
                           end
                        else
                           begin
                              expression_kind := string_expression;
                              info.min_value.AsInteger := Length(sc.simple_constant.s);
                              info.max_value.AsInteger := Length(sc.simple_constant.s);
                              target_cpu.record_anonymous_string_constant (sc.simple_constant.s)
                           end;
                     enum_constant:
                        begin
                           expression_kind := enum_expression;
                           enum_typedef := sc.simple_constant.enum_typedef;
                           info.min_value.Assign (sc.simple_constant.ordinal_value);
                           info.max_value.Assign (sc.simple_constant.ordinal_value)
                        end;
                     set_constant:
                        begin
                           expression_kind := set_expression;
                           enum_typedef := sc.simple_constant.enum_typedef;
                           set_min_max_for_set_constant
                        end;
                  else
                     assert(false)
                  end
               end;
            record_type:
               expression_kind := record_expression;
            packed_record_type:
               expression_kind := packed_record_expression;
            array_type:
               expression_kind := array_expression;
            string_type:
               expression_kind := string_expression;
            set_type:
               expression_kind := set_expression;
            overlay_type:
               expression_kind := overlay_expression;
         else
            assert(false)
         end;
      end;   // handle_structured_constant

   begin    // TConstantPrimary.CreateFromStructuredConstantAccess
      inherited Create;
      src_loc := access.src_loc;

      case access.node_access_kind of
         constant_access:
            handle_simple_constant (access.node_constant);
         structured_constant_access:
            case access.node_structured_constant.StructuredConstantKind of
               scSimple:
                  handle_simple_constant (access.node_structured_constant.simple_constant);
               scArray,
               scRecord,
               scPackedRecord:
                  handle_structured_constant (access.node_structured_constant);
               scOverlay:
                  if access.node_structured_constant.overlay_constant.StructuredConstantKind = scSimple then
                     handle_simple_constant (access.node_structured_constant.overlay_constant.simple_constant)
                  else
                     handle_structured_constant (access.node_structured_constant.overlay_constant);
            else  
               assert (false)
            end;
      else
         assert(false)
      end
   end;     // TConstantPrimary.CreateFromStructuredConstantAccess

constructor TConstantPrimary.CreateFromReservedWordConstant;
   begin
      inherited Create;
      src_loc := lex.token.src_loc;

      assert(lex.token_is_reserved_word([rw_true, rw_false]));
      if lex.token_is_reserved_word([rw_false]) then
         begin
            the_constant := TConstant.CreateBooleanConstant(false);
            expression_kind := boolean_expression;
            info.min_value.AsInteger := ord(false);
            info.max_value.AsInteger := ord(false)
         end
      else if lex.token_is_reserved_word([rw_true]) then
         begin
            the_constant := TConstant.CreateBooleanConstant(true);
            expression_kind := boolean_expression;
            info.min_value.AsInteger := ord(true);
            info.max_value.AsInteger := ord(true)
         end;
      lex.advance_token
   end;

constructor TConstantPrimary.CreateFromIdentifierConstant;
   var
      id_idx: TIdentifierIdx;
      i: integer;
   begin
      inherited Create;
      src_loc := lex.token.src_loc;

      id_idx := lex.token.identifier_idx;
      assert(lex.token_is_identifier);
      assert(CurrentDefinitionTable[lex.token.identifier_idx].definition_kind = constant_definition);

      the_constant := CurrentDefinitionTable[id_idx];
      constant.AddRef;
      case constant.constant_kind of
         integer_constant:
            begin
               expression_kind := integer_expression;
               info.min_value.Assign (constant.ordinal_value);
               info.max_value.Assign (constant.ordinal_value)
            end;
         real_constant:
            expression_kind := real_expression;
         string_constant:
            if Length(constant.s) = 1 then
               begin
                  expression_kind := char_expression;
                  info.min_value.AsInteger := ord(constant.s[1]);
                  info.max_value.Assign (info.min_value)
               end
            else
               begin
                  expression_kind := string_expression;
                  target_cpu.record_anonymous_string_constant (constant.s)
               end;
         enum_constant:
            begin
               expression_kind := enum_expression;
               enum_typedef := constant.enum_typedef;
               info.min_value.Assign (constant.ordinal_value);
               info.max_value.Assign (constant.ordinal_value)
            end;
         boolean_constant:
            begin
               expression_kind := boolean_expression;
               info.min_value.AsInteger := ord(constant.b);
               info.max_value.AsInteger := ord(constant.b)
            end;
         set_constant:
            begin
               expression_kind := set_expression;
               set_ordinal_base_type := constant.set_ordinal_base_type;
               if set_ordinal_base_type = ordinal_base_is_enum then
                  enum_typedef := constant.enum_typedef;
               info.min_value.AsInteger := max_set;
               info.max_value.AsInteger := min_set;
               for i := min_set to max_set do
                  if i in constant.sett then
                     begin
                        if info.min_value.gt(i) then
                           info.min_value.AsInteger := i;
                        if info.max_value.lt(i) then
                           info.max_value.AsInteger := i
                     end
            end;
         else
            assert(false)
      end;
      lex.advance_token
   end;

constructor TConstantPrimary.CreateFromConstant
   (_constant: TConstant;
    _src_loc:
    TSourceLocation
   );
   begin
      inherited Create;
      src_loc := _src_loc;
      the_constant := _constant;
      case constant.constant_kind of
         integer_constant:
            begin
               expression_kind := integer_expression;
               info.min_value.Assign (constant.ordinal_value);
               info.max_value.Assign (constant.ordinal_value)
            end;
         real_constant:
            expression_kind := real_expression;
         string_constant:
            if Length(constant.s) = 1 then
               begin
                  expression_kind := char_expression;
                  info.min_value.AsInteger := ord(constant.s[1]);
                  info.max_value.AsInteger := ord(constant.s[1])
               end
            else
               begin
                  expression_kind := string_expression;
                  info.min_value.AsInteger := Length(constant.s);
                  info.max_value.AsInteger := Length(constant.s);
                  target_cpu.record_anonymous_string_constant (constant.s)
               end;
         enum_constant:
            begin
               expression_kind := enum_expression;
               enum_typedef := constant.enum_typedef;
               info.min_value.Assign (constant.ordinal_value);
               info.max_value.Assign (info.min_value)
            end;
         boolean_constant:
            begin
               expression_kind := boolean_expression;
               info.min_value.AsInteger := ord(constant.b);
               info.max_value.Assign (info.min_value)
            end;
         set_constant:
            begin
               expression_kind := set_expression;
               set_ordinal_base_type := constant.set_ordinal_base_type;
               if set_ordinal_base_type = ordinal_base_is_enum then
                  enum_typedef := constant.enum_typedef;
               set_min_max_for_set_constant
            end;
      else
         assert(false)
      end;
   end;

destructor TConstantPrimary.Destroy;
   begin
      the_constant.Release;
      inherited
   end;

function TConstantPrimary.contains_constant: boolean;
   begin
      result := true
   end;

function TConstantPrimary.contains_real_constant: boolean;
   begin
      result := constant.constant_kind = real_constant
   end;

function TConstantPrimary.contains_integer_constant: boolean;
   begin
      result := constant.constant_kind = integer_constant
   end;

function TConstantPrimary.constant: TConstant;
   begin
      assert (the_constant.definition_kind = constant_definition);
      result := TConstant(the_constant)
   end;

function TConstantPrimary.ordinal_constant_value: TLargestPascalSupportedInt;
   begin
      result := 0; // suppress compiler warning
      case constant.constant_kind of
         integer_constant,
         enum_constant:
            result := constant.ordinal_value.AsInteger;
         boolean_constant:
            result := ord(constant.b);
         string_constant:
            begin
               assert(Length(constant.s) = 1);
               result := ord(constant.s[1])
            end;
      else
         assert(false)
      end
   end;

function TConstantPrimary.real_constant_value: real;
   begin
      assert(constant.constant_kind = real_constant);
      result := constant.r
   end;

function TConstantPrimary.boolean_constant_value: boolean;
   begin
      assert(constant.constant_kind = boolean_constant);
      result := constant.b
   end;

procedure TConstantPrimary.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   begin
      assert (constant.constant_kind = set_constant);
      check_set_constant (constant, valid_range, src_loc)
   end;


// =======================
//  TEmptyFunctionPrimary

constructor TEmptyFunctionPrimary.CreateFromSourceTokens;
   begin
      inherited Create;

      assert(lex.token_is_reserved_word(rw_empty));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      access := target_cpu.TAccess_CreateFromSourceTokens;

      if access.node_typedef.type_kind <> queue_type then
         raise compile_error.Create(err_queue_variable_expected, access.node_id_src_loc);

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      expression_kind := boolean_expression
   end;

destructor TEmptyFunctionPrimary.Destroy;
   begin
      access.Release;
      inherited
   end;

procedure TEmptyFunctionPrimary.MarkAsReachable;
   begin
      access.MarkAsReachable;
      inherited
   end;


// ==================
//  TFunctionAccess

constructor TFunctionAccessPrimary.CreateFromSourceTokens
   (_access: TAccess
   );
   var
      routine_access: TAccess;
      odt: TOrdinalDataType;
      st: TSetType;
   begin
      inherited Create;
      access := _access;
      access.AddRef;
      assert(_access.definition_kind = access_definition);
      src_loc := access.src_loc;
      routine_access := access;
      assert(routine_access.node_access_kind in [function_access, property_access]);

      if (access.base_variable <> nil)
         and
         (access.base_variable.typedef.requires_initialization)
         and
         (not access.base_variable.init_statement_called)
      then
         raise compile_error.Create (err_variable_not_initialized, access.src_loc);

      if routine_access.node_access_kind = function_access then
         if not routine_access.node_routine.parameter_definitions.Empty then
            actual_parameters := routine_access.node_routine.AssembleAndCheckCallerParameterListFromSourceTokens
         else // no parameters allowed
            if lex.token_is_symbol(sym_left_parenthesis) then
               raise compile_error.Create(err_function_has_no_parameters);

      case routine_access.node_typedef.type_kind of
         basic_data_type:
            case TBasicDataType(routine_access.node_typedef).basic_data_type_kind of
               ordinal_data_type:
                  begin
                     odt := TOrdinalDataType(routine_access.node_typedef);
                     case odt.ordinal_kind of
                        ordinal_base_is_integer:
                           expression_kind := integer_expression;
                        ordinal_base_is_char:
                           expression_kind := char_expression;
                        ordinal_base_is_bool:
                           expression_kind := boolean_expression;
                        ordinal_base_is_enum:
                           begin
                              expression_kind := enum_expression;
                              enum_typedef := odt.enum_typedef
                           end;
                     else
                        assert(false)
                     end;
                     info.min_value.Assign (odt.info.min_value);
                     info.max_value.Assign (odt.info.max_value)
                  end;
               floating_point_data_type:
                  expression_kind := real_expression;
            else
               assert(false)
            end;
         set_type:
            begin
               st := TSetType(routine_access.node_typedef);
               expression_kind := set_expression;
               set_ordinal_base_type := st.ordinal_kind;
               if set_ordinal_base_type = ordinal_base_is_enum then
                  enum_typedef := st.enum_typedef;
               info.Assign (st.info)
            end;
         string_type:
            begin
               expression_kind := string_expression;
               info.min_value.AsInteger := 0;
               info.max_value.AsInteger := 0
            end;
      else
         assert(false)
      end;

      if BlockStack.tos_idx >= 0 then   // not a fragment test
         case access.node_access_kind of
            function_access:
               if access.node_routine.routine_kind = standalone_routine then
                  begin
                     if not access.node_routine.built_in_routine then
                        begin
                           call_record := target_cpu.TCallRecord_Create (access.node_routine.name, access, standalone_routine_call);
                           BlockStack.tos.AddCallRecord (call_record)
                        end
                  end
               else
                  begin
                     call_record := target_cpu.TCallRecord_Create (access.node_routine.name, access, systemtype_routine_call);
                     BlockStack.tos.AddCallRecord (call_record)
                  end;
            property_access:
               begin
                  call_record := target_cpu.TCallRecord_Create (access.node_property.name, access, systemtype_property_call);
                  BlockStack.tos.AddCallRecord (call_record)
               end;
         else
            assert (false)
         end;
   end;

destructor TFunctionAccessPrimary.Destroy;
   var
      i: integer;
   begin
      access.Release;
      for i := 0 to Length(actual_parameters) - 1 do
         actual_parameters[i].Release;
      inherited
   end;

procedure TFunctionAccessPrimary.MarkAsReachable;
   var
      i: integer;
   begin
      access.MarkAsReachable;
      for i := 0 to Length(actual_parameters) - 1 do
         actual_parameters[i].MarkAsReachable;
      inherited
   end;

function TFunctionAccessPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := access.CheckForProhibitedDelayCall (err_msg);
      for i := 0 to Length(actual_parameters) - 1 do
         if actual_parameters[i].CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;

procedure TFunctionAccessPrimary.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   begin
   end;


// ==============
//  TNotPrimary

constructor TNotPrimary.CreateFromSourceTokens;
   begin
      inherited Create;
      expression_kind := boolean_expression;

      assert(lex.token_is_reserved_word(rw_not));
      lex.advance_token;

      boolean_expr := CreatePrimaryFromSourceTokens;
      if boolean_expr.expression_kind <> boolean_expression then
         raise compile_error.Create(err_boolean_expression_expected, boolean_expr.src_loc);
      if boolean_expr.contains_constant then
         raise  EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(not boolean_expr.constant.b), src_loc));
      assert(boolean_expr.info.min_value <> boolean_expr.info.max_value);
      // should have already been reduced to a constant
      copy_expression_info(boolean_expr)
   end;

destructor TNotPrimary.Destroy;
   begin
      boolean_expr.Release;
      inherited
   end;

procedure TNotPrimary.MarkAsReachable;
   begin
      boolean_expr.MarkAsReachable;
      inherited
   end;

function TNotPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := boolean_expr.CheckForProhibitedDelayCall (err_msg)
   end;


// =====================
// TPredFunctionPrimary

constructor TPredFunctionPrimary.CreateFromSourceTokens;
   var t: TMultiPrecisionInteger;
   begin
      inherited Create;

      t := TMultiPrecisionInteger.Create;
      try
         assert(lex.token_is_reserved_word(rw_pred));
         lex.advance_token;

         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         expr := CreateExpressionFromSourceTokens;

         if not lex.token_is_symbol(sym_right_parenthesis) then
            raise compile_error.Create(err_right_parenthesis_expected);
         lex.advance_token;

         if not (expr.expression_kind in ordinal_expression_kinds) then
            raise compile_error.Create(err_ordinal_expression_expected, expr.src_loc);

         if (expr.expression_kind = enum_expression)
            and
            (TEnumType(expr.enum_typedef).enum_type_kind = specified_value_enum_type)
         then
            raise compile_error.Create(err_packed_record_enum_not_allowed_here,
            expr.src_loc);

         expression_kind := expr.expression_kind;
         enum_typedef := expr.enum_typedef;
         info.min_value.Assign (expr.info.min_value);
         info.min_value.Subtract (1);
         info.max_value.Assign (expr.info.max_value);
         info.max_value.Subtract (1);

         if expr.contains_constant then
            begin
               case expression_kind of
                  integer_expression:
                     begin
                        t.Assign (expr.constant.ordinal_value);
                        t.Subtract (1);
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(t), src_loc))
                     end;
                  enum_expression:
                     begin
                        if expr.constant.ordinal_value.eq (expr.enum_typedef.info.min_value) then // pred(enum_max) => ???
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateEnumConstant(expr.enum_typedef, expr.constant.ordinal_value.AsInteger - 1), src_loc))
                     end;
                  boolean_expression:
                     case expr.constant.b of
                        false: // pred(false) => ???
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        true: // pred(true) => false
                           raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(false), src_loc))
                     end;
                  char_expression:
                     begin
                        if expr.constant.s[1] = chr(min_char) then
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        expr.constant.s := chr(ord(expr.constant.s[1]) - 1);
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateStringConstant(expr.constant.s), src_loc))
                     end;
               else
                  assert(false)
               end;
               expr.Release;
               expr := nil
            end
      finally
         t.Free
      end
   end;

destructor TPredFunctionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TPredFunctionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TPredFunctionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


// ========================
// TRelationalExpression

constructor TRelationalExpression.CreateFromSourceTokens;

   procedure set_constant_result
      (b: boolean
      );
      begin
         raise ERelationalExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(b), src_loc))
      end;

   function compare_constant_values
      (left, right: TConstant;
       relop: TRelOp
      ):
   boolean;
      begin
         result := false; // to suppress compiler warning
         assert(left.constant_kind = right.constant_kind);
         if left.constant_kind in [integer_constant, real_constant,
         enum_constant, boolean_constant] then
            case relop of
               relop_equals:
                  result := left.AsOrdinal = right.AsOrdinal;
               relop_notequals:
                  result := left.AsOrdinal <> right.AsOrdinal;
               relop_lt:
                  result := left.AsOrdinal < right.AsOrdinal;
               relop_gt:
                  result := left.AsOrdinal > right.AsOrdinal;
               relop_le:
                  result := left.AsOrdinal <= right.AsOrdinal;
               relop_ge:
                  result := left.AsOrdinal >= right.AsOrdinal;
            else
               assert(false)
            end
         else if left.constant_kind = string_constant then
            case relop of
               relop_equals:
                  result := left.s = right.s;
               relop_notequals:
                  result := left.s <> right.s;
               relop_lt:
                  result := left.s < right.s;
               relop_gt:
                  result := left.s > right.s;
               relop_le:
                  result := left.s <= right.s;
               relop_ge:
                  result := left.s >= right.s;
            else
               assert(false)
            end
         else if left.constant_kind = set_constant then
            case relop of
               relop_equals:
                  result := left.sett = right.sett;
               relop_notequals:
                  result := left.sett <> right.sett;
               relop_le:
                  result := left.sett <= right.sett;
               relop_ge:
                  result := left.sett >= right.sett;
            else
               assert(false)
            end
         else
            assert(false) // set constant?
      end;

   function always_true
      (left, right: TExpression;
       relop: TRelOp
      ): boolean;
      begin
         result := false; // provisional
         if (left.expression_kind in [integer_expression, enum_expression,  boolean_expression, char_expression])
            and
            (right.expression_kind in [integer_expression, enum_expression, boolean_expression, char_expression]) then
            // min_ord and max_ord are valid
            case relop of
               relop_equals:
                  result := left.info.min_value.eq(left.info.max_value) and right.info.min_value.eq(right.info.max_value) and left.info.min_value.eq(right.info.min_value);
               relop_notequals:
                  result := left.info.min_value.gt(right.info.max_value) or left.info.max_value.lt(right.info.min_value);
               relop_lt:
                  result := left.info.max_value.lt(right.info.min_value);
               relop_gt:
                  result := left.info.min_value.gt(right.info.max_value);
               relop_le:
                  result := left.info.max_value.le(right.info.min_value);
               relop_ge:
                  result := left.info.min_value.ge(right.info.max_value)
            else
               assert(false)
            end
      end;

   function always_false
      (left, right: TExpression;
       relop: TRelOp
      ): boolean;
      begin
         result := false; // provisional
         if (left.expression_kind in [integer_expression, enum_expression, boolean_expression, char_expression])
            and
            (right.expression_kind in [integer_expression, enum_expression, boolean_expression, char_expression]) then
            begin
               // min_ord and max_ord are valid
               case relop of
                  relop_equals:
                     result := left.info.min_value.gt(right.info.max_value)
                               or
                               left.info.max_value.lt(right.info.min_value);
                  relop_notequals:
                     result := left.info.min_value.eq(left.info.max_value)
                               and
                               right.info.min_value.eq(right.info.max_value)
                               and
                               left.info.min_value.eq(right.info.min_value);
                  relop_lt:
                     result := left.info.min_value.ge(right.info.max_value);
                  relop_gt:
                     result := left.info.max_value.le(right.info.min_value);
                  relop_le:
                     result := left.info.min_value.gt(right.info.max_value);
                  relop_ge:
                     result := left.info.max_value.lt(right.info.min_value);
               else
                  assert(false)
               end
            end
      end;

   var
      operator_src_loc: TSourceLocation;
   begin
      inherited Create;
      src_loc := lex.token.src_loc;
      left_simple_expression := CreateSimpleExpressionFromSourceTokens;
      operator_src_loc := lex.token.src_loc;
      if lex.token_is_symbol([sym_equals, sym_not_equals, sym_greater_than, sym_less_than, sym_greater_than_or_equal, sym_less_than_or_equal]) then
         begin
            expression_kind := boolean_expression;
            case lex.token.symbol of
               sym_equals:
                  relop := relop_equals;
               sym_not_equals:
                  relop := relop_notequals;
               sym_greater_than:
                  relop := relop_gt;
               sym_less_than:
                  relop := relop_lt;
               sym_greater_than_or_equal:
                  relop := relop_ge;
               sym_less_than_or_equal:
                  relop := relop_le;
            else
               assert(false)
            end;
            lex.advance_token;
            right_simple_expression := CreateSimpleExpressionFromSourceTokens;

            // do type checking
            case left_simple_expression.expression_kind of
               integer_expression,
               real_expression:
                  if not (right_simple_expression.expression_kind in [integer_expression, real_expression]) then
                     raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
               enum_expression:
                  if not ((right_simple_expression.expression_kind = enum_expression)
                          and (left_simple_expression.enum_typedef = right_simple_expression.enum_typedef)
                         ) then
                     raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
               boolean_expression:
                  if right_simple_expression.expression_kind <> boolean_expression then
                     raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
               set_expression:
                  begin
                     if not ((right_simple_expression.expression_kind = set_expression)
                             and
                             (left_simple_expression.set_ordinal_base_type = right_simple_expression.set_ordinal_base_type)
                            ) then
                        raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     if ((left_simple_expression.set_ordinal_base_type = ordinal_base_is_enum)
                         and
                         (left_simple_expression.enum_typedef <> right_simple_expression.enum_typedef)
                        ) then
                        raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     if relop in [relop_lt, relop_gt] then
                        raise compile_error.Create(err_invalid_operator_for_operand, operator_src_loc)
                  end;
               char_expression,
               string_expression:
                  if not (right_simple_expression.expression_kind in [char_expression, string_expression]) then
                     raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
               array_expression,
               record_expression,
               packed_record_expression:
                  begin
                     if left_simple_expression.typedef <> right_simple_expression.typedef then
                        raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     if relop in [relop_lt, relop_le, relop_gt, relop_ge] then
                        raise compile_error.Create(err_invalid_operator_for_operand, operator_src_loc);
                  end;
               overlay_expression,
               system_type_expression:
                  raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
            else
               assert(false)
            end;

            // do optimizations
            if (left_simple_expression.contains_constant)
               and
               (right_simple_expression.contains_constant) then
               // both operands are constants
               set_constant_result(compare_constant_values(left_simple_expression.constant, right_simple_expression.constant, relop))
            else if always_true(left_simple_expression, right_simple_expression, relop) then
               set_constant_result(true)
            else if always_false(left_simple_expression, right_simple_expression, relop) then
               set_constant_result(false)
            else
               begin
                  info.min_value.AsInteger := ord(false);
                  info.max_value.AsInteger := ord(true)
               end;

            if (left_simple_expression.expression_kind = right_simple_expression.expression_kind)
               and
               (left_simple_expression.expression_kind in [integer_expression, enum_expression, boolean_expression, char_expression]) then
               begin
                  comparison_info := target_cpu.TTypeInfo_Create (left_simple_expression.info);
                  comparison_info.max_value.Subtract(right_simple_expression.info.min_value);
                  comparison_info.min_value.Subtract(right_simple_expression.info.max_value)
               end
         end
      else if lex.token_is_reserved_word(rw_in) then
         begin
            relop := relop_in;
            expression_kind := boolean_expression;
            lex.advance_token;
            right_simple_expression := CreateSimpleExpressionFromSourceTokens;

            if not (left_simple_expression.expression_kind in [integer_expression, enum_expression, boolean_expression, char_expression]) then
               raise compile_error.Create(err_left_operand_of_in_expression_must_be_ordinal, src_loc);
            if right_simple_expression.expression_kind <> set_expression then
               raise compile_error.Create(err_right_operand_of_in_expression_must_be_set, right_simple_expression.src_loc);

            if (right_simple_expression.contains_constant)
               and
               (right_simple_expression.constant.sett = [])
            then
               set_constant_result(false)
            else
               begin
                  case left_simple_expression.expression_kind of
                     integer_expression:
                        if right_simple_expression.set_ordinal_base_type <> ordinal_base_is_integer then
                           raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     enum_expression:
                        if (right_simple_expression.set_ordinal_base_type <> ordinal_base_is_enum)
                           or
                           (left_simple_expression.enum_typedef <> right_simple_expression.enum_typedef) then
                           raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     boolean_expression:
                        if right_simple_expression.set_ordinal_base_type <> ordinal_base_is_bool then
                           raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                     char_expression:
                           if right_simple_expression.set_ordinal_base_type <> ordinal_base_is_char then
                              raise compile_error.Create(err_incompatible_operand_types, operator_src_loc);
                  else
                     assert(false)
                  end;

                  // do optimizations
                  if (left_simple_expression.contains_constant)
                     and
                     (right_simple_expression.contains_constant) then
                     // both operands are constants
                     case left_simple_expression.expression_kind of
                        integer_expression, enum_expression:
                           if left_simple_expression.constant.ordinal_value.lt(min_set)
                              or
                              left_simple_expression.constant.ordinal_value.gt(max_set) then
                              set_constant_result (false)
                           else
                              set_constant_result(left_simple_expression.constant.ordinal_value.AsByte(0) in right_simple_expression.constant.sett);
                        boolean_expression:
                           set_constant_result(ord(left_simple_expression.constant.b) in right_simple_expression.constant.sett);
                        char_expression:
                           set_constant_result(ord(left_simple_expression.constant.s[1]) in right_simple_expression.constant.sett);
                     else
                        assert(false)
                     end
                  else
                     begin
                        info.min_value.AsInteger := ord(false);
                        info.max_value.AsInteger := ord(true)
                     end
               end
         end
      else // no operator, return left expression
         begin
            left_simple_expression.AddRef;
            raise ERelationalExpressionSimplification.Create(left_simple_expression)
         end
   end;

destructor TRelationalExpression.Destroy;
   begin
      left_simple_expression.Release;
      right_simple_expression.Release;
      comparison_info.Release;
      inherited
   end;

procedure TRelationalExpression.MarkAsReachable;
   begin
      inherited;
      left_simple_expression.MarkAsReachable;
      right_simple_expression.MarkAsReachable
   end;

function TRelationalExpression.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result :=
         left_simple_expression.CheckForProhibitedDelayCall (err_msg)
         or
         right_simple_expression.CheckForProhibitedDelayCall (err_msg)
   end;


// ========================
// TRoundFunctionPrimary

constructor TRoundFunctionPrimary.CreateFromSourceTokens;
   begin
      inherited Create;

      assert(lex.token_is_reserved_word(rw_round));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      expr := CreateExpressionFromSourceTokens;
      if not (expr.expression_kind in [integer_expression, real_expression]) then
         raise compile_error.Create(err_numeric_expression_expected, expr.src_loc);

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      if expr.contains_real_constant then
         raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(round(expr.constant.r)), src_loc))
      else if expr.contains_integer_constant then
         begin
            expr.constant.AddRef;
            raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(expr.constant, src_loc))
         end
      else if expr.expression_kind = integer_expression then
         begin
            expr.AddRef;
            raise EPrimaryExpressionSimplification.Create (expr)
         end;

      expression_kind := integer_expression;
      info.min_value.Assign (target_cpu.round_trunc_result_type.info.min_value);
      info.max_value.Assign (target_cpu.round_trunc_result_type.info.max_value)
   end;

destructor TRoundFunctionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TRoundFunctionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TRoundFunctionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


// ==================
//  TSetConstructor

constructor TSetConstructorPrimary.CreateFromSourceTokens;

   procedure check_member_type
      (member: TExpression
      );
      begin
         if not (member.expression_kind in ordinal_expression_kinds) then
            raise compile_error.Create(err_ordinal_expression_expected, member.src_loc);
         case set_ordinal_base_type of
            ordinal_base_is_integer:
               begin
                  if member.expression_kind <> integer_expression then
                     raise compile_error.Create(err_set_member_type_differs_from_previous, member.src_loc);
                  if member.info.max_value.lt(min_set) or member.info.min_value.gt(max_set) then
                     raise compile_error.Create(err_set_member_value_outside_legal_range, member.src_loc)
               end;
            ordinal_base_is_char:
               if member.expression_kind <> char_expression then
                  raise compile_error.Create(err_set_member_type_differs_from_previous, member.src_loc);
            ordinal_base_is_bool:
               if member.expression_kind <> boolean_expression then
                  raise compile_error.Create(err_set_member_type_differs_from_previous, member.src_loc);
            ordinal_base_is_enum:
               if (member.expression_kind <> enum_expression)
                  or
                  (member.enum_typedef <> enum_typedef) then
                  raise compile_error.Create(err_set_member_type_differs_from_previous, member.src_loc);
         else
            assert(false)
         end;
      end;

   procedure adj_limits
      (expr: TExpression
      );
      begin
         if expr.info.min_value.lt(info.min_value) then
            info.min_value.Assign (expr.info.min_value);
         if expr.info.max_value.gt(info.max_value) then
            info.max_value.Assign (expr.info.max_value)
      end;

   var
      i: integer;
      first, second: TExpression;
      is_constant: boolean;
   begin
      inherited Create;
      src_loc := lex.token.src_loc;

      first := nil;
      second := nil; // to suppress warning

      assert(lex.token_is_symbol(sym_left_bracket));
      lex.advance_token;

      expression_kind := set_expression;
      set_ordinal_base_type := empty_set_ordinal_base_unknown; // provisional
      info.min_value.AsInteger := max_set;
      info.max_value.AsInteger := min_set;
      while not lex.token_is_symbol(sym_right_bracket) do
         try
            first := nil;
            second := nil;
            first := CreateExpressionFromSourceTokens;
            if set_ordinal_base_type = empty_set_ordinal_base_unknown then
            // first member determines expression type
               case first.expression_kind of
                  integer_expression:
                     set_ordinal_base_type := ordinal_base_is_integer;
                  boolean_expression:
                     set_ordinal_base_type := ordinal_base_is_bool;
                  char_expression:
                     set_ordinal_base_type := ordinal_base_is_char;
                  enum_expression:
                     begin
                        set_ordinal_base_type := ordinal_base_is_enum;
                        enum_typedef := first.enum_typedef;
                        enum_typedef.AddRef
                     end;
               else
                  raise compile_error.Create(err_ordinal_expression_expected, first.src_loc)
               end;
            check_member_type(first);
            adj_limits(first);
            is_constant := first.contains_constant;

            if lex.token_is_symbol(sym_dot_dot) then
               begin
                  lex.advance_token;
                  second := CreateExpressionFromSourceTokens;
                  check_member_type(second);
                  adj_limits(second);
                  if not second.contains_constant then
                     is_constant := false
               end;

            if not lex.token_is_symbol([sym_comma, sym_right_bracket]) then
               raise compile_error.Create(err_comma_or_right_bracket_expected);
            if lex.token_is_symbol(sym_comma) then
               lex.advance_token;

            if is_constant then
               begin
                  if constant_members = nil then
                     begin
                        constant_members := TConstant.Create;
                        constant_members.constant_kind := set_constant;
                        constant_members.set_ordinal_base_type := set_ordinal_base_type;
                        if set_ordinal_base_type = ordinal_base_is_enum then
                           constant_members.enum_typedef := enum_typedef;
                        constant_members.sett := []
                     end;
                  if second = nil then
                     constant_members.sett := constant_members.sett + [first.constant.AsOrdinal]
                  else
                     for i := first.constant.AsOrdinal to second.constant.AsOrdinal do
                        constant_members.sett := constant_members.sett + [i]
               end
            else
               begin
                  i := Length(variable_members);
                  SetLength(variable_members, i + 1);
                  variable_members[i].first := first;
                  first.AddRef;
                  variable_members[i].last := second;
                  if second <> nil then
                     second.AddRef
               end
         finally
            first.Release;
            second.Release
         end;
      lex.advance_token;

      if Length(variable_members) = 0 then // constant set
         begin
            if constant_members = nil then // empty set
               begin
                  constant_members := TConstant.Create;
                  constant_members.constant_kind := set_constant;
                  constant_members.set_ordinal_base_type := set_ordinal_base_type;
                  constant_members.sett := []
               end;
            constant_members.AddRef;
            raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(constant_members, src_loc))
         end
   end;

destructor TSetConstructorPrimary.Destroy;
   var
      i: integer;
   begin
      constant_members.Release;
      for i := 0 to Length(variable_members) - 1 do
         begin
            variable_members[i].first.Release;
            variable_members[i].last.Release
         end;
      inherited
   end;

procedure TSetConstructorPrimary.MarkAsReachable;
   var
      i: integer;
   begin
      for i := 0 to Length(variable_members) - 1 do
         begin
            variable_members[i].first.MarkAsReachable;
            variable_members[i].last.MarkAsReachable
         end;
      inherited
   end;

function TSetConstructorPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to Length(variable_members) - 1 do
         begin
            if variable_members[i].first.CheckForProhibitedDelayCall (err_msg)
            then
               result := true;
            if (variable_members[i].last <> nil)
               and
               variable_members[i].last.CheckForProhibitedDelayCall (err_msg)
            then
               result := true
         end
   end;

procedure TSetConstructorPrimary.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   begin
      check_set_constant (constant_members, valid_range, src_loc)
   end;


//======================
// TSuccFunctionPrimary

constructor TSuccFunctionPrimary.CreateFromSourceTokens;
   var
      t: TMultiPrecisionInteger;
      s: string;
   begin
      inherited Create;
      t := TMultiPrecisionInteger.Create;
      try
         assert(lex.token_is_reserved_word(rw_succ));
         lex.advance_token;

         if not lex.token_is_symbol(sym_left_parenthesis) then
            raise compile_error.Create(err_left_parenthesis_expected);
         lex.advance_token;

         expr := CreateExpressionFromSourceTokens;

         if not lex.token_is_symbol(sym_right_parenthesis) then
            raise compile_error.Create(err_right_parenthesis_expected);
         lex.advance_token;

         if not (expr.expression_kind in ordinal_expression_kinds) then
            raise compile_error.Create(err_ordinal_expression_expected, expr.src_loc);

         if (expr.expression_kind = enum_expression)
            and
            (TEnumType(expr.enum_typedef).enum_type_kind = specified_value_enum_type) then
            raise compile_error.Create(err_packed_record_enum_not_allowed_here, expr.src_loc);

         expression_kind := expr.expression_kind;
         enum_typedef := expr.enum_typedef;
         info.min_value.Assign (expr.info.min_value);
         info.min_value.Add (1);
         info.max_value.Assign (expr.info.max_value);
         info.max_value.Add (1);

         if expr.contains_constant then
            begin
               case expression_kind of
                  integer_expression:
                     begin
                        t.Assign (expr.constant.ordinal_value);
                        t.Add (1);
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(t), src_loc))
                     end;
                  enum_expression:
                     begin
                        if expr.constant.ordinal_value.eq (expr.enum_typedef.info.max_value)
                        then // succ(enum_max) => ???
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateEnumConstant(expr.enum_typedef, expr.constant.ordinal_value.AsInteger + 1), src_loc))
                     end;
                  boolean_expression:
                     case expr.constant.b of
                        true: // succ(true) => ???
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        false: // succ(false) => true
                           raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(true), src_loc))
                     end;
                  char_expression:
                     begin
                        if expr.constant.s[1] = chr(max_char) then
                           raise compile_error.Create(err_result_will_be_out_of_range, src_loc);
                        s := chr(ord(expr.constant.s[1]) + 1);
                        expr.constant.s := s;
                        raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateStringConstant(expr.constant.s), src_loc))
                     end;
               else
                  assert(false)
               end;
               expr.Release;
               expr := nil
            end
      finally
         t.Free
      end
   end;

destructor TSuccFunctionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TSuccFunctionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TSuccFunctionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


// ========================
//  TTruncFunctionPrimary

constructor TTruncFunctionPrimary.CreateFromSourceTokens;
   begin
      inherited Create;

      assert(lex.token_is_reserved_word(rw_trunc));
      lex.advance_token;

      if not lex.token_is_symbol(sym_left_parenthesis) then
         raise compile_error.Create(err_left_parenthesis_expected);
      lex.advance_token;

      expr := CreateExpressionFromSourceTokens;
      if not (expr.expression_kind in [integer_expression, real_expression]) then
         raise compile_error.Create(err_numeric_expression_expected, expr.src_loc);

      if not lex.token_is_symbol(sym_right_parenthesis) then
         raise compile_error.Create(err_right_parenthesis_expected);
      lex.advance_token;

      if expr.contains_real_constant then
         raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(trunc(expr.constant.r)), src_loc))
      else if expr.contains_integer_constant then
         begin
            expr.constant.AddRef;
            raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(expr.constant, src_loc))
         end
      else if expr.expression_kind = integer_expression then
         begin
            expr.AddRef;
            raise EPrimaryExpressionSimplification.Create(expr)
         end;

      expression_kind := integer_expression;
      info.min_value.Assign (target_cpu.round_trunc_result_type.info.min_value);
      info.max_value.Assign (target_cpu.round_trunc_result_type.info.max_value)
   end;

destructor TTruncFunctionPrimary.Destroy;
   begin
      expr.Release;
      inherited
   end;

procedure TTruncFunctionPrimary.MarkAsReachable;
   begin
      expr.MarkAsReachable;
      inherited
   end;

function TTruncFunctionPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := expr.CheckForProhibitedDelayCall (err_msg)
   end;


// =====================
//  TUnaryMinusPrimary

constructor TUnaryMinusPrimary.CreateFromSourceTokens;
   var t: TMultiPrecisionInteger;
   begin
      inherited Create;
      t := TMultiPrecisionInteger.Create;
      try
         primary := CreatePrimaryFromSourceTokens;
         if not (primary.expression_kind in [integer_expression, real_expression])
         then
            raise compile_error.Create(err_unary_operator_not_allowed_for_this_operand_type, src_loc);
         expression_kind := primary.expression_kind;
         if expression_kind = integer_expression then
            begin
               info.min_value.Assign (primary.info.max_value);
               info.min_value.ChangeSign;
               info.max_value.Assign (primary.info.min_value);
               info.max_value.ChangeSign
            end;
         if (primary.contains_constant) then
            case primary.constant.constant_kind of
               integer_constant:
                  begin
                     t.Assign (primary.constant.ordinal_value);
                     t.ChangeSign;
                     raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(t), src_loc))
                  end;
               real_constant:
                  raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateRealConstant(-primary.constant.r), src_loc));
            else
               assert(false)
            end
      finally
         t.Free
      end
   end;

constructor TUnaryMinusPrimary.CreateFromExpression
   (exp: TExpression
   );
   var t: TMultiPrecisionInteger;
   begin
      inherited Create;
      t := TMultiPrecisionInteger.Create;
      try
         assert(exp.expression_kind in [integer_expression, real_expression]);

         if (exp.contains_constant) then
            case exp.constant.constant_kind of
               integer_constant:
                  begin
                     t.Assign (exp.constant.ordinal_value);
                     t.ChangeSign;
                     raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(t), src_loc))
                  end;
               real_constant:
                  raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateRealConstant(-exp.constant.r), src_loc));
            else
               assert(false)
            end;

         primary := exp;
         primary.AddRef;

         expression_kind := primary.expression_kind;
         if expression_kind = integer_expression then
            begin
               info.min_value.Assign (primary.info.max_value);
               info.min_value.ChangeSign;
               info.max_value.Assign (primary.info.min_value);
               info.max_value.ChangeSign
            end
      finally
         t.Free
      end
   end;

destructor TUnaryMinusPrimary.Destroy;
   begin
      primary.Release;
      inherited
   end;

procedure TUnaryMinusPrimary.MarkAsReachable;
   begin
      primary.MarkAsReachable;
      inherited
   end;

function TUnaryMinusPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := primary.CheckForProhibitedDelayCall (err_msg)
   end;


// ========================
//  TVariableAccessPrimary

constructor TVariableAccessPrimary.Create
   (acc: TAccess
   );
   var
      odt: TOrdinalDataType;
      sett: TSetType;
      strt: TStringType;
   begin
      inherited Create;
      access := acc;
      access.AddRef;
      src_loc := access.src_loc;
      assert(access.node_access_kind = variable_access);

      if access.base_variable <> nil then // not a constant
         BlockStack.CheckAddressReachability(access.base_variable, access.src_loc);

      case access.node_typedef.type_kind of
         basic_data_type:
            case TBasicDataType(access.node_typedef).basic_data_type_kind of
               ordinal_data_type:
                  begin
                     odt := TOrdinalDataType(access.node_typedef);
                     case odt.ordinal_kind of
                        ordinal_base_is_integer:
                           begin
                              expression_kind := integer_expression;
                              info.min_value.Assign (odt.info.min_value);
                              info.max_value.Assign (odt.info.max_value);
                              if info.min_value.eq(info.max_value) then
                                 raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(info.min_value), src_loc))
                           end;
                        ordinal_base_is_char:
                           begin
                              expression_kind := char_expression;
                              info.min_value.Assign (odt.info.min_value);
                              info.max_value.Assign (odt.info.max_value);
                              if info.min_value.eq(info.max_value) then
                                 raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateStringConstant(chr(info.min_value.AsInteger)), src_loc))
                           end;
                        ordinal_base_is_bool:
                           begin
                              expression_kind := boolean_expression;
                              info.min_value.Assign (odt.info.min_value);
                              info.max_value.Assign (odt.info.max_value);
                              if info.min_value.eq(info.max_value) then
                                 raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(boolean(info.min_value.AsInteger)), src_loc))
                           end;
                        ordinal_base_is_enum:
                           begin
                              expression_kind := enum_expression;
                              enum_typedef := odt.enum_typedef;
                              info.min_value.Assign (odt.info.min_value);
                              info.max_value.Assign (odt.info.max_value);
                              if info.min_value.eq(info.max_value) then
                                 raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateEnumConstant(odt.enum_typedef, info.min_value.AsInteger), src_loc))
                           end;
                     else
                        assert(false)
                     end;
                  end;
               floating_point_data_type:
                  expression_kind := real_expression;
            else
               assert(false)
            end;
         set_type:
            begin
               sett := TSetType(access.node_typedef);
               expression_kind := set_expression;
               set_ordinal_base_type := sett.ordinal_kind;
               if set_ordinal_base_type = ordinal_base_is_enum then
                  enum_typedef := sett.enum_typedef;
               info.Assign (sett.info)
            end;
         string_type:
            begin
               strt := TStringType(access.node_typedef);
               expression_kind := string_expression;
               info.min_value.AsInteger := 0;
               info.max_value.AsInteger := strt.max_length
            end;
         record_type:
            begin
               expression_kind := record_expression;
               typedef := access.node_typedef
            end;
         packed_record_type:
            begin
               expression_kind := packed_record_expression;
               typedef := access.node_typedef
            end;
         overlay_type:
            begin
               expression_kind := overlay_expression;
               typedef := access.node_typedef
            end;
         array_type:
            begin
               expression_kind := array_expression;
               typedef := access.node_typedef
            end;
         system_type:
            begin
               expression_kind := system_type_expression;
               typedef := access.node_typedef
            end;
      else
         assert(false)
      end;
   end;

destructor TVariableAccessPrimary.Destroy;
   begin
      access.Release;
      inherited
   end;

procedure TVariableAccessPrimary.MarkAsReachable;
   begin
      access.MarkAsReachable;
      inherited
   end;

function TVariableAccessPrimary.CheckForProhibitedDelayCall (err_msg: string): boolean;
   begin
      result := access.CheckForProhibitedDelayCall (err_msg)
   end;

procedure TVariableAccessPrimary.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   begin
   end;


//=================
//  TStrPosPrimary

constructor TStrPosPrimary.Create (acc: TAccess);
   begin
      inherited Create;
      access := acc;
      access.AddRef;
      src_loc := access.src_loc;
      assert(access.node_access_kind = variable_access);
      assert (access.node_typedef.type_kind = basic_data_type);
      assert (TBasicDataType(access.node_typedef).basic_data_type_kind = ordinal_data_type);
      assert (TOrdinalDataType(access.node_typedef).ordinal_kind = ordinal_base_is_integer);
      expression_kind := integer_expression;
      info.min_value.Assign (TOrdinalDataType(access.node_typedef).info.min_value);
      info.max_value.Assign (TOrdinalDataType(access.node_typedef).info.max_value);
      if info.min_value.eq(info.max_value) then
         raise EPrimaryExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(info.min_value), src_loc))
   end;

procedure TStrPosPrimary.MarkAsReachable;
   begin
      access.MarkAsReachable;
      access.node_strpos_substr_expression.MarkAsReachable;
      inherited
   end;

destructor TStrPosPrimary.Destroy;
   begin
      access.Release;
      inherited
   end;


// ===========================
// EExpressionSimplification

constructor EExpressionSimplification.Create
   (expr: TExpression
   );
   begin
      simplified_expr := expr
   end;


END.
