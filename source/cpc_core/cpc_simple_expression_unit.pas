UNIT cpc_simple_expression_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_expressions_unit,
   cpc_source_analysis_unit;

type
   TAddOp =
      (addop_numeric_add,
       addop_add_int_to_int,
       addop_add_flt_to_int,
       addop_add_flt_to_flt,
       addop_string_concat,
       addop_set_union,       // +
       addop_numeric_subtract,
       addop_subtract_int_from_int,
       addop_subtract_flt_from_int,
       addop_subtract_flt_from_flt,
       addop_set_difference,  // -
       addop_boolean_or,
       addop_boolean_xor
      );
   TAdditionalTerm =
      record
         left_info: TTypeInfo;
         addop: TAddOp;
         addop_src_loc: TSourceLocation;
         right_term: TExpression;
         result_info: TTypeInfo
      end;
   TSimpleExpression =
      class(TExpression)
         first_term: TExpression;
         additional_terms:
            array of TAdditionalTerm;
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

type
   ESimpleExpressionSimplification =
      class(EExpressionSimplification)
      end;

function CreateSimpleExpressionFromSourceTokens: TExpression;

IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_target_cpu_unit,
   cpc_term_expression_unit;

function CreateSimpleExpressionFromSourceTokens: TExpression;
   begin
      try
         result := target_cpu.TSimpleExpression_CreateFromSourceTokens
      except
         on e: ESimpleExpressionSimplification do
            result := e.simplified_expr
      end
   end;

// ====================
// TSimpleExpression

constructor TSimpleExpression.CreateFromSourceTokens;

   procedure delete_additional_term
      (idx: integer
      );
      var
         j: integer;
      begin
         additional_terms[idx].right_term.Release;
         for j := idx to Length(additional_terms) - 2 do
            additional_terms[j] := additional_terms[j + 1];
         SetLength(additional_terms, Length(additional_terms) - 1)
      end;

   procedure schedule_numeric_operations;
      // do faster integer operations first before switching to slower real operations
      // addition is commutative
      var
         i: integer;
         changed: boolean;
         temp: TAdditionalTerm;
      begin
         // bubble sort integer operations to front of additional_terms
         repeat
            changed := false;
            for i := 0 to Length(additional_terms) - 2 do
               if (additional_terms[i].right_term.expression_kind = real_expression)
                  and
                  (additional_terms[i + 1].right_term.expression_kind = integer_expression) then
                  begin
                     temp := additional_terms[i];
                     additional_terms[i] := additional_terms[i + 1];
                     additional_terms[i + 1] := temp;
                     changed := true
                  end
         until not changed;

         if (first_term.expression_kind = real_expression)
            and
            (Length(additional_terms) > 0)
            and
            (additional_terms[0].right_term.expression_kind = integer_expression) then
            begin
               // move real first_term to end of additional terms, then move int term [0] to first_term
               // result is integer evals will be done first, and real evals last
               i := Length(additional_terms);
               SetLength(additional_terms, i + 1);

               additional_terms[i].addop := addop_numeric_add;
               additional_terms[i].addop_src_loc := first_term.src_loc;
               additional_terms[i].right_term := first_term;

               case additional_terms[0].addop of
                  addop_numeric_add:
                     begin
                        first_term := additional_terms[0].right_term;
                        first_term.AddRef
                     end;
                  addop_numeric_subtract:
                     try
                        first_term := target_cpu.TUnaryMinusPrimary_CreateFromExpression(additional_terms[0].right_term)
                     except
                        on e: EPrimaryExpressionSimplification do
                           first_term := e.simplified_expr
                     end;
               else
                  assert(false)
               end;
               delete_additional_term(0)
            end
      end;

   procedure simplify_numeric_expression;
      var
         i, j: integer;
      begin
         // this procedure assumes the numeric operations have been scheduled (sorted)
         // into integer first, real later as per schedule_numeric_operations above.

         // combine all integer constants no matter where they are in the expression
         if first_term.contains_integer_constant then
            begin
               i := 0;
               while i < Length(additional_terms) do
                  if additional_terms[i].right_term.contains_integer_constant then
                     begin
                        case additional_terms[i].addop of
                           addop_numeric_add:
                              first_term.constant.ordinal_value.Add (additional_terms[i].right_term.constant.ordinal_value);
                           addop_numeric_subtract:
                              first_term.constant.ordinal_value.Subtract(additional_terms[i].right_term.constant.ordinal_value);
                        else
                           assert(false)
                        end;
                        first_term.info.min_value.Assign (first_term.constant.ordinal_value);
                        first_term.info.max_value.Assign (first_term.constant.ordinal_value);
                        delete_additional_term(i)
                     end
                  else
                     i := i + 1;
               if first_term.constant.ordinal_value.eq(0) and (Length(additional_terms) > 0) then
                  begin
                     first_term.Release;
                     if additional_terms[0].addop = addop_numeric_add then
                        begin
                           first_term := additional_terms[0].right_term;
                           first_term.AddRef
                        end
                     else // operator is subtract
                        try
                           first_term :=
                           target_cpu.TUnaryMinusPrimary_CreateFromExpression(additional_terms[0].right_term)
                        except
                           on e: EPrimaryExpressionSimplification do
                              first_term := e.simplified_expr
                        end;
                     delete_additional_term(0)
                  end
            end
         else // first term does not contain an integer constant
            begin
               // look for first integer constant later in the expression
               i := -1;
               for j := Length(additional_terms) - 1 downto 0 do
                  if additional_terms[j].right_term.contains_integer_constant then
                     i := j;
               if i <> -1 then
                  begin // there is at least one integer constant
                     // combine with any other integer constants
                     j := i + 1;
                     while j < Length(additional_terms) do
                        if additional_terms[j].right_term.contains_integer_constant then
                           begin
                              case additional_terms[j].addop of
                                 addop_numeric_add:
                                    additional_terms[i].right_term.constant.ordinal_value.Add(additional_terms[j].right_term.constant.ordinal_value);
                                 addop_numeric_subtract:
                                    additional_terms[i].right_term.constant.ordinal_value.Subtract(additional_terms[j].right_term.constant.ordinal_value)
                                 else
                                    assert(false)
                              end;
                              additional_terms[i].right_term.info.min_value.Assign (additional_terms[i].right_term.constant.ordinal_value);
                              additional_terms[i].right_term.info.max_value.Assign (additional_terms[i].right_term.constant.ordinal_value);
                              delete_additional_term(j)
                           end
                        else
                           j := j + 1;
                     // remove constant term if 0
                     if additional_terms[i].right_term.constant.ordinal_value.eq(0) then
                        delete_additional_term(i)
                  end
            end;
         // at this point there is at most one integer constant in the expression

         // next combine all real constants (if any), and incorporate the integer constant (if any)
         if Length(additional_terms) > 0 then
            begin
               if (first_term.contains_constant)
                  and
                  (additional_terms[0].right_term.expression_kind = real_expression) then
                  begin // entire expression will be evaluated in real arithmetic, combine all constants into first
                     if first_term.contains_integer_constant then
                        begin
                           first_term.constant.r := first_term.constant.ordinal_value.AsReal;
                           first_term.constant.constant_kind := real_constant;
                           first_term.expression_kind := real_expression
                        end;
                     i := 0;
                     while i < Length(additional_terms) do
                        if additional_terms[i].right_term.contains_constant then
                           begin
                              case additional_terms[i].addop of
                                 addop_numeric_add:
                                    first_term.constant.r := first_term.constant.r + additional_terms[i].right_term.constant.real_value;
                                 addop_numeric_subtract:
                                    first_term.constant.r := first_term.constant.r - additional_terms[i].right_term.constant.real_value
                                 else
                                    assert(false)
                              end;
                              delete_additional_term(i)
                           end
                        else
                           i := i + 1;

                     if (first_term.constant.r = 0.0)
                        and
                        (Length(additional_terms) > 0) then
                        begin
                           first_term.Release;
                           if additional_terms[0].addop = addop_numeric_add then
                              begin
                                 first_term := additional_terms[0].right_term;
                                 first_term.AddRef
                              end
                           else // operator is subtract
                              try
                                 first_term :=
                                 target_cpu.TUnaryMinusPrimary_CreateFromExpression(additional_terms[0].right_term)
                              except
                                 on e: EPrimaryExpressionSimplification
                                 do first_term := e.simplified_expr
                              end;
                           delete_additional_term(0)
                        end
                  end
               else
                  begin
                  // find first real constant (if any)
                     i := Length(additional_terms); // above bounds
                     for j := Length(additional_terms) - 1 downto 0 do
                        if additional_terms[j].right_term.contains_real_constant then
                           i := j;
                     // if i is in bounds, it is the first (real) constant
                     if i < Length(additional_terms) then
                        begin // additional_term[i] is first real constant
                           if additional_terms[i].addop = addop_numeric_subtract
                           then
                              begin // convert to + term
                                 additional_terms[i].right_term.constant.r := -additional_terms[i].right_term.constant.r;
                                 additional_terms[i].addop := addop_numeric_add
                              end;
                           // combine ALL constant terms into additional_term[i]
                           j := 0;
                           while j < Length(additional_terms) do
                              if (j <> i)
                                 and
                                 (additional_terms[j].right_term.contains_constant) then
                                 begin
                                    case additional_terms[j].addop of
                                       addop_numeric_add:
                                          additional_terms[i].right_term.constant.r :=
                                             additional_terms[i].right_term.constant.r
                                             +
                                             additional_terms[j].right_term.constant.real_value;
                                       addop_numeric_subtract:
                                          additional_terms[i].right_term.constant.r :=
                                             additional_terms[i].right_term.constant.r
                                             -
                                             additional_terms[j].right_term.constant.real_value;
                                    else
                                       assert(false)
                                    end;
                                    delete_additional_term(j);
                                    if j < i then
                                       i := i - 1
                                 end
                              else
                                 j := j + 1;
                           if not first_term.contains_constant then
                              begin
                                 if additional_terms[i].right_term.constant.real_value = 0
                                 then
                                    delete_additional_term(i)
                              end
                           else  // first_term does contain a constant as does i
                           if i = 0 then
                              begin
                                 if first_term.contains_integer_constant then
                                    begin
                                       first_term.constant.r := first_term.constant.ordinal_value.AsReal;
                                       first_term.constant.constant_kind := real_constant;
                                       first_term.expression_kind := real_expression
                                    end;
                                 case additional_terms[0].addop of
                                    addop_numeric_add:
                                       first_term.constant.r := first_term.constant.r + additional_terms[0].right_term.constant.r;
                                    addop_numeric_subtract:
                                       first_term.constant.r := first_term.constant.r - additional_terms[0].right_term.constant.r
                                    else
                                       assert(false)
                                 end;
                                 delete_additional_term(0)
                              end
                           else // i > 0
                              begin // combine it with additional_term[i]
                                 additional_terms[i].right_term.constant.r :=
                                    additional_terms[i].right_term.constant.r +
                                    first_term.constant.real_value;

                                 if additional_terms[i].right_term.constant.r = 0.0
                                 then
                                    delete_additional_term(i);

                                 first_term.Release;
                                 if additional_terms[0].addop = addop_numeric_add
                                 then
                                    begin
                                       first_term := additional_terms[0].right_term;
                                       first_term.AddRef
                                    end
                                 else // operator is subtract
                                    try
                                       first_term := target_cpu.TUnaryMinusPrimary_CreateFromExpression(additional_terms[0].right_term)
                                    except
                                       on e: EPrimaryExpressionSimplification do
                                          first_term := e.simplified_expr
                                    end;
                                 delete_additional_term(0)
                              end
                        end
                  end
            end
      end;

   procedure combine_boolean_constants;
      var
         any_true_constant_terms: boolean;
         i: integer;
      begin
         any_true_constant_terms := first_term.contains_constant and
         first_term.constant.b;
         for i := 0 to Length(additional_terms) - 1 do
            if additional_terms[i].right_term.contains_constant
               and
               additional_terms[i].right_term.constant.b then
               any_true_constant_terms := true;

         if any_true_constant_terms then
            begin // entire OR'd expression evaluates to true
               first_term.Release;
               while Length(additional_terms) > 0 do
                  delete_additional_term(0);
               first_term := target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(true), src_loc)
            end
         else // any constants are all false and irrelevant to OR expression
            begin
               while first_term.contains_constant and (Length(additional_terms) > 0) do
                  begin
                     first_term.Release;
                     first_term := additional_terms[0].right_term;
                     first_term.AddRef;
                     delete_additional_term(0)
                  end;
               i := 0;
               while i < Length(additional_terms) do
                  if additional_terms[i].right_term.contains_constant then
                     delete_additional_term(i)
                  else
                     i := i + 1
            end
      end;

   procedure combine_string_constants;
      procedure set_min_max
         (term: TExpression
         );
         begin
            if Length(term.constant.s) = 1 then
               begin
                  term.expression_kind := char_expression;
                  term.info.min_value.AsInteger := ord(term.constant.s[1]);
                  term.info.max_value.AsInteger := ord(term.constant.s[1])
               end
            else
               begin
                  term.expression_kind := string_expression;
                  term.info.min_value.AsInteger := Length(term.constant.s);
                  term.info.max_value.AsInteger := Length(term.constant.s)
               end
         end;
      var
         term_idx: integer;
      begin // concatenate adjacent constant strings
         if first_term.contains_constant then
            begin
               while (Length(additional_terms) > 0)
                     and
                     (additional_terms[0].right_term.contains_constant) do
                  begin
                     first_term.constant.s :=
                        first_term.constant.s + additional_terms[0].right_term.constant.s;
                     first_term.info.min_value.AsInteger := Length(first_term.constant.s);
                     first_term.info.max_value.AsInteger := Length(first_term.constant.s);
                     delete_additional_term(0)
                  end;
               set_min_max(first_term)
            end;

         term_idx := 0;
         while term_idx < Length(additional_terms) - 1 do
            begin
               if (additional_terms[term_idx].right_term.contains_constant)
                  and
                  (additional_terms[term_idx + 1].right_term.contains_constant) then
                  begin // combine adjacent constant strings
                     additional_terms[term_idx].right_term.constant.s :=
                        additional_terms[term_idx].right_term.constant.s + additional_terms[term_idx + 1].right_term.constant.s;
                     set_min_max(additional_terms[term_idx].right_term);
                     delete_additional_term(term_idx + 1)
                  end
               else
                  term_idx := term_idx + 1
            end
      end;

   procedure combine_set_constants;
      var
         i: integer;
      begin
         // note: set union is assoc & comm, difference is not

         if first_term.contains_constant then
            begin
               while (Length(additional_terms) > 0)
                     and
                     (additional_terms[0].right_term.contains_constant) do
                  begin
                     case additional_terms[0].addop of
                        addop_set_union:
                           first_term.constant.sett := first_term.constant.sett + additional_terms[0].right_term.constant.sett;
                        addop_set_difference:
                           first_term.constant.sett := first_term.constant.sett - additional_terms[0].right_term.constant.sett
                        else
                           assert(false)
                     end;
                     delete_additional_term(0)
                  end;
               first_term.set_min_max_for_set_constant
            end;

         i := 0;
         while i < Length(additional_terms) do
            begin
               if (additional_terms[i].addop = addop_set_union)
                  and
                  (additional_terms[i].right_term.contains_constant) then
                  begin
                     while ((i + 1) < Length(additional_terms))
                           and
                           (additional_terms[i + 1].addop = addop_set_union)
                           and
                           (additional_terms[i + 1].right_term.contains_constant) do
                        begin
                           additional_terms[i].right_term.constant.sett :=
                              additional_terms[i].right_term.constant.sett +
                              additional_terms[i + 1].right_term.constant.sett;
                           delete_additional_term(i + 1)
                        end;
                     additional_terms[i].right_term.set_min_max_for_set_constant
                  end;
               i := i + 1
            end
      end;

   var
      term_idx: integer;
   begin // TSimpleExpression.CreateFromSourceTokens
      inherited Create;
      src_loc := lex.token.src_loc;

      first_term := CreateTermFromSourceTokens;
      copy_expression_info(first_term);

      // gather additional terms and type check them
      while lex.token_is_symbol([sym_plus, sym_minus])
            or
            lex.token_is_reserved_word(rw_or)
      do begin
            term_idx := Length(additional_terms);
            SetLength(additional_terms, term_idx + 1);

            additional_terms[term_idx].addop_src_loc := lex.token.src_loc;
            if lex.token_is_symbol(sym_plus) then
               case expression_kind of
                  integer_expression, real_expression:
                     additional_terms[term_idx].addop := addop_numeric_add;
                  string_expression:
                     additional_terms[term_idx].addop := addop_string_concat;
                  set_expression:
                     additional_terms[term_idx].addop := addop_set_union;
                  char_expression:
                     begin
                        additional_terms[term_idx].addop := addop_string_concat;
                        expression_kind := string_expression
                     end;
               else
                  raise compile_error.Create(err_invalid_operator_for_operand)
               end
            else if lex.token_is_symbol(sym_minus) then
               case expression_kind of
                  integer_expression, real_expression:
                     additional_terms[term_idx].addop := addop_numeric_subtract;
                  set_expression:
                     additional_terms[term_idx].addop := addop_set_difference;
               else
                  raise compile_error.Create(err_invalid_operator_for_operand)
               end
            else if lex.token_is_reserved_word(rw_or) then
               if expression_kind = boolean_expression then
                  additional_terms[term_idx].addop := addop_boolean_or
               else
                  raise compile_error.Create(err_invalid_operator_for_operand)
            else
               assert(false);
            lex.advance_token;

            additional_terms[term_idx].right_term := CreateTermFromSourceTokens;

            // check type
            case expression_kind of
               integer_expression:
                  begin
                     if not (additional_terms[term_idx].right_term.expression_kind in [integer_expression, real_expression]) then
                        raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
                     if additional_terms[term_idx].right_term.expression_kind = real_expression then
                        expression_kind := real_expression
                  end;
               real_expression:
                  if not (additional_terms[term_idx].right_term.expression_kind in [integer_expression, real_expression]) then
                     raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
               string_expression:
                  if not (additional_terms[term_idx].right_term.expression_kind in [char_expression, string_expression]) then
                     raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
               boolean_expression:
                  if additional_terms[term_idx].right_term.expression_kind <> boolean_expression then
                     raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
               set_expression:
                  begin
                     if additional_terms[term_idx].right_term.expression_kind <> set_expression then
                        raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
                     if additional_terms[term_idx].right_term.set_ordinal_base_type <> empty_set_ordinal_base_unknown then
                        if set_ordinal_base_type = empty_set_ordinal_base_unknown then
                           begin
                              set_ordinal_base_type := additional_terms[term_idx].right_term.set_ordinal_base_type;
                              if set_ordinal_base_type = ordinal_base_is_enum then
                                 enum_typedef := additional_terms[term_idx].right_term.enum_typedef
                           end
                        else
                           begin
                              if set_ordinal_base_type <> additional_terms[term_idx].right_term.set_ordinal_base_type then
                                 raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc);
                              if (set_ordinal_base_type = ordinal_base_is_enum)
                                 and
                                 (enum_typedef <> additional_terms[term_idx].right_term.enum_typedef) then
                                 raise compile_error.Create(err_incompatible_operand_types, additional_terms[term_idx].addop_src_loc)
                           end
                  end
               else
                  assert(false)
            end
         end;
      // at this point all terms should be correct type and all operators appropriate

      if expression_kind in [integer_expression, real_expression] then
         schedule_numeric_operations;

      if Length(additional_terms) > 0 then
         // combine constants where possible
         case expression_kind of
            integer_expression,
            real_expression:
               simplify_numeric_expression;
            boolean_expression:
               combine_boolean_constants;
            string_expression:
               combine_string_constants;
            set_expression:
               combine_set_constants;
         else
         // nothing to do
         end;

      // determine intermediate results, final operators, and final min/max
      case expression_kind of
         integer_expression,
         real_expression:
            begin
               expression_kind := first_term.expression_kind;
               info.Assign (first_term.info);
               for term_idx := 0 to Length(additional_terms) - 1 do
                  begin
                     if (expression_kind = integer_expression) then
                        begin
                           if term_idx = 0 then
                              additional_terms[term_idx].left_info := first_term.info
                           else
                              additional_terms[term_idx].left_info := additional_terms[term_idx-1].result_info;
                           additional_terms[term_idx].left_info.AddRef;

                           case additional_terms[term_idx].right_term.expression_kind of
                              integer_expression:
                                 case additional_terms[term_idx].addop of
                                    addop_numeric_add:
                                       additional_terms[term_idx].addop := addop_add_int_to_int;
                                    addop_numeric_subtract:
                                       additional_terms[term_idx].addop := addop_subtract_int_from_int;
                                  else
                                     assert(false)
                                  end;
                              real_expression:
                                 begin  // evaluation switches to real
                                    case additional_terms[term_idx].addop of
                                       addop_numeric_add:
                                          additional_terms[term_idx].addop := addop_add_flt_to_int;
                                       addop_numeric_subtract:
                                          additional_terms[term_idx].addop := addop_subtract_flt_from_int;
                                     else
                                        assert(false)
                                     end;
                                     expression_kind := real_expression
                                 end;
                           else
                              assert (false)
                           end
                        end
                     else  // expression_kind = real_expression
                        begin
                           assert (additional_terms[term_idx].right_term.expression_kind = real_expression);  // should have been sorted with int expr first
                           case additional_terms[term_idx].addop of
                              addop_numeric_add:
                                 additional_terms[term_idx].addop := addop_add_flt_to_flt;
                              addop_numeric_subtract:
                                 additional_terms[term_idx].addop := addop_subtract_flt_from_flt;
                            else
                               assert(false)
                            end;
                        end;

                     if expression_kind = integer_expression then
                        begin
                           case additional_terms[term_idx].addop of
                              addop_add_int_to_int:
                                 begin
                                    info.max_value.Add(additional_terms[term_idx].right_term.info.max_value);
                                    info.min_value.Add(additional_terms[term_idx].right_term.info.min_value)
                                 end;
                              addop_subtract_int_from_int:
                                 begin
                                    info.max_value.Subtract(additional_terms[term_idx].right_term.info.min_value);
                                    info.min_value.Subtract(additional_terms[term_idx].right_term.info.max_value)
                                 end;
                           else
                              assert (false)
                           end;
                           additional_terms[term_idx].result_info := target_cpu.TTypeInfo_Create (info)
                        end
                  end
            end;
         enum_expression,
         char_expression:
            begin
               info.min_value.Assign (first_term.info.min_value);
               info.max_value.Assign (first_term.info.max_value);
            end;
         boolean_expression:
            begin
               info.min_value.Assign (first_term.info.min_value);
               info.max_value.Assign (first_term.info.max_value);
               for term_idx := 0 to Length(additional_terms) - 1 do
                  begin
                     if additional_terms[term_idx].right_term.info.max_value.eq(1) then
                        info.max_value.AsInteger := 1;
                     if additional_terms[term_idx].right_term.info.min_value.eq(1) then
                        begin
                           info.min_value.AsInteger := 1;
                           info.max_value.AsInteger := 1
                        end
                  end;
            end;
         string_expression:
            begin
               info.min_value.Assign (first_term.info.min_value);
               info.max_value.Assign (first_term.info.max_value);
               case first_term.expression_kind of
                  char_expression:
                     begin
                        info.min_value.AsInteger := 1;
                        info.max_value.AsInteger := 1
                     end;
                  string_expression:
                     begin
                        info.min_value.Assign(first_term.info.min_value);
                        info.max_value.Assign(first_term.info.max_value)
                     end;
               else
                  assert(false)
               end;
               for term_idx := 0 to Length(additional_terms) - 1 do
                  begin
                     case additional_terms[term_idx].right_term.expression_kind of
                        char_expression:
                           begin
                              info.min_value.Add(1);
                              info.max_value.Add(1)
                           end;
                        string_expression:
                           begin
                              info.min_value.Add(additional_terms[term_idx].right_term.info.min_value);
                              info.max_value.Add(additional_terms[term_idx].right_term.info.max_value)
                           end;
                     else
                        assert(false)
                     end
                  end
            end;
         set_expression:
            begin
               info.min_value.Assign (first_term.info.min_value);
               info.max_value.Assign (first_term.info.max_value);
               for term_idx := 0 to Length(additional_terms) - 1 do
                  begin
                     case additional_terms[term_idx].addop of
                        addop_set_union:
                           begin
                              info.max_value.Max(additional_terms[term_idx].right_term.info.max_value);
                              info.min_value.Min(additional_terms[term_idx].right_term.info.min_value)
                           end;
                        addop_set_difference:
                           if additional_terms[term_idx].right_term.contains_constant then
                              begin
                                 while (info.min_value.AsInteger <= max_set)
                                       and
                                       (info.min_value.AsByte(0) in additional_terms[term_idx].right_term.constant.sett) do
                                    info.min_value.Add(1);
                                 while (info.max_value.AsInteger >= min_set)
                                       and
                                       (info.max_value.AsByte(0) in additional_terms[term_idx].right_term.constant.sett) do
                                    info.max_value.Subtract(1)
                              end;
                     else
                        assert(false)
                     end
                  end
            end;
      else
         // nop
      end;

      // reduce to single constant if possible
      case expression_kind of
         integer_expression:
            if info.min_value.eq(info.max_value) then
               raise ESimpleExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(info.min_value), src_loc));
         boolean_expression:
            if info.min_value.eq(info.max_value) then
               raise ESimpleExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(boolean(info.min_value.AsInteger)), src_loc));
         set_expression:
            if info.min_value.gt(info.max_value) then
               begin // result will always be empty set
                  if set_ordinal_base_type = ordinal_base_is_enum then
                     raise ESimpleExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateSetConstant([], enum_typedef), src_loc))
                  else
                     raise ESimpleExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateSetConstant([], set_ordinal_base_type), src_loc))
               end;
         enum_expression, char_expression, string_expression, real_expression,
         record_expression, packed_record_expression, overlay_expression,
         array_expression, system_type_expression:
            begin
               // nop
            end;
      else
         assert(false)
      end;

      if Length(additional_terms) = 0 then
         begin
            first_term.AddRef;
            raise ESimpleExpressionSimplification.Create(first_term)
         end;

      if expression_kind = string_expression then
         raise compile_error.Create (err_string_plus_operator_only_for_constants, additional_terms[0].addop_src_loc)
   end; // TSimpleExpression.CreateFromSourceTokens;

destructor TSimpleExpression.Destroy;
   var
      i: integer;
   begin
      first_term.Release;
      for i := 0 to Length(additional_terms) - 1 do
         begin
            additional_terms[i].left_info.Release;
            additional_terms[i].right_term.Release;
            additional_terms[i].result_info.Release
         end;
      inherited
   end;

procedure TSimpleExpression.MarkAsReachable;
   var
      i: integer;
   begin
      first_term.MarkAsReachable;
      for i := 0 to Length(additional_terms)-1 do
         additional_terms[i].right_term.MarkAsReachable;
      inherited
   end;

function TSimpleExpression.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := first_term.CheckForProhibitedDelayCall (err_msg);
      for i := 0 to Length(additional_terms)-1 do
         if additional_terms[i].right_term.CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;


procedure TSimpleExpression.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   var i: integer;
   begin
      first_term.check_set_constants_range (valid_range, src_loc);
      for i := 0 to Length(additional_terms)-1 do
         if additional_terms[i].addop = addop_set_union then
            additional_terms[i].right_term.check_set_constants_range (valid_range, src_loc)
   end;


END.
