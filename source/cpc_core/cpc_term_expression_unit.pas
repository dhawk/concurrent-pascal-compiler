UNIT cpc_term_expression_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_core_objects_unit,
   cpc_source_analysis_unit;

type
   TMulOp =
      (mulop_boolean_and,
       mulop_numeric_mult,
       mulop_mult_int_by_int,
       mulop_mult_int_by_flt,
       mulop_mult_flt_by_flt,
       mulop_set_intersection,  // *
       mulop_real_divide,       // /
       mulop_divide_int_by_flt,
       mulop_divide_flt_by_flt,
       mulop_integer_div,       // div
       mulop_integer_mod,
       mulop_mask,              // optimization of mod
       mulop_shl,               // optimization of integer multiply
       mulop_shr                // optimization of integer divide
      );

   TTerm =
      class(TExpression)
         first_factor: TExpression;
         additional_factors:
            array of
               record
                  left_info: TTypeInfo;
                  mulop: TMulOp;
                  mulop_src_loc: TSourceLocation;
                  mulop_param: longint;  // param for optimzation ops
                  factor: TExpression;
                  result_info: TTypeInfo
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

function CreateTermFromSourceTokens: TExpression;

IMPLEMENTATION

uses
   cpc_common_unit,
   cpc_expressions_unit,
   cpc_multi_precision_integer_unit,
   cpc_target_cpu_unit;


type
   ETermExpressionSimplification =
      class(EExpressionSimplification)
      end;

var
   min_result_value, max_result_value, t1, t2: TMultiPrecisionInteger;

function CreateTermFromSourceTokens: TExpression;
   begin
      try
         result := target_cpu.TTerm_CreateFromSourceTokens;
      except
         on e: ETermExpressionSimplification do
            result := e.simplified_expr
      end
   end;


// ========
//  TTerm

constructor TTerm.CreateFromSourceTokens;

   procedure delete_additional_factor
      (idx: integer
      );
      var
         j: integer;
      begin
         additional_factors[idx].factor.Release;
         for j := idx to Length(additional_factors) - 2 do
            additional_factors[j] := additional_factors[j + 1];
         SetLength(additional_factors, Length(additional_factors) - 1)
      end;

   procedure adjust_mult_range
      (left, right: TExpression
      );
      begin
         // calculate new max ord
         t1.Assign (left.info.min_value);
         t1.Multiply (right.info.min_value);
         max_result_value.Assign (t1);

         t1.Assign (left.info.min_value);
         t1.Multiply (right.info.max_value);
         max_result_value.Max(t1);

         t1.Assign(left.info.max_value);
         t1.Multiply(right.info.min_value);
         max_result_value.Max(t1);

         t1.Assign(left.info.max_value);
         t1.Multiply(right.info.max_value);
         max_result_value.Max(t1);

         // calculate new min ord
         t1.Assign(left.info.min_value);
         t1.Multiply(right.info.min_value);
         min_result_value.Assign(t1);

         t1.Assign(left.info.min_value);
         t1.Multiply(right.info.max_value);
         min_result_value.Min(t1);

         t1.Assign(left.info.max_value);
         t1.Multiply(right.info.min_value);
         min_result_value.Min(t1);

         t1.Assign(left.info.max_value);
         t1.Multiply(right.info.max_value);
         min_result_value.Min(t1)
      end;

   procedure adjust_div_range
      (left, right: TExpression
      );
      begin
         if right.info.min_value.gt(0) then
            begin
               min_result_value.Assign (left.info.min_value);
               min_result_value.Divide (right.info.max_value);
               max_result_value.Assign (left.info.max_value);
               max_result_value.Divide (right.info.min_value)
            end
         else if right.info.min_value.eq(0) then
            begin
               min_result_value.Assign (left.info.min_value);
               min_result_value.Divide (right.info.max_value);
               max_result_value.Assign (left.info.max_value)  // smallest divisor will be 1
            end
         else if right.info.min_value.lt(0) and right.info.max_value.gt(0) then
            begin
               // smallest divisor will be 1 or -1
               max_result_value.Assign (left.info.min_value);
               max_result_value.Abs;
               t1.Assign (left.info.max_value);
               t1.Abs;
               max_result_value.Max (t1);
               min_result_value.Assign (max_result_value);
               min_result_value.ChangeSign
            end
         else if right.info.max_value.eq(0) then
            begin
               min_result_value.Assign (left.info.max_value);
               min_result_value.Divide (right.info.min_value);
               // smallest divisor will be -1
               max_result_value.Assign (left.info.min_value);
               max_result_value.ChangeSign
            end
         else if right.info.max_value.lt(0) then
            begin
               min_result_value.Assign (left.info.max_value);
               min_result_value.Divide (right.info.min_value);
               max_result_value.Assign (left.info.min_value);
               max_result_value.Divide (right.info.max_value)
            end
         else
            assert(false)
      end;

   procedure adjust_mod_range
      (left, right: TExpression
      );
      begin
         case target_cpu.mod_operator_implementation of
            iso_pascal_mod_operator_implementation:
               begin
                  if right.info.max_value.le(0)
                  then
                     raise compile_error.Create(err_divisor_will_never_be_ge_1, right.src_loc);

                  if left.info.min_value.eq(left.info.max_value)
                     and
                     (right.contains_constant)
                  then
                     begin
                        min_result_value.Assign (left.info.min_value);
                        min_result_value.iso_std_pascal_mod (right.constant.ordinal_value);
                        max_result_value.Assign (min_result_value)
                     end
                  else
                     begin
                        // run time check must ensure right > 0
                        min_result_value.AsInteger := 0;
                        max_result_value.Assign (right.info.max_value);
                        max_result_value.Subtract (1);
                        if left.info.min_value.ge(0) and left.info.max_value.le(max_result_value) then
                           max_result_value.Assign(left.info.max_value)
                     end
               end;
            delphi_mod_operator_implemenation:
               begin
                  if right.info.min_value.eq(0) and right.info.max_value.eq(0)
                  then
                     raise compile_error.Create(err_divisor_will_always_be_0, right.src_loc);
                  // range and/or run time check must ensure right <> 0

                  if left.info.min_value.eq(left.info.max_value)
                     and
                     (right.contains_constant)
                  then
                     begin
                        min_result_value.Assign (left.info.min_value);
                        min_result_value.delphi_mod (right.constant.ordinal_value);
                        max_result_value.Assign (min_result_value)
                     end
                  else
                     case left.info.IntegerRange of
                        irAlwaysNegative:
                           begin
                              max_result_value.AsInteger := 0;
                              min_result_value.Assign (right.info.min_value);
                              min_result_value.Abs;
                              if min_result_value.lt(right.info.max_value) then
                                 min_result_value.Assign (right.info.max_value);
                              min_result_value.Subtract (1);
                              min_result_value.ChangeSign;
                              if min_result_value.lt(left.info.min_value) then
                                 min_result_value.Assign (left.info.min_value);
                           end;
                        irNegativeOrPositive:
                           begin
                              max_result_value.Assign (right.info.min_value);
                              max_result_value.Abs;
                              if max_result_value.lt(right.info.max_value) then
                                 max_result_value.Assign (right.info.max_value);
                              max_result_value.Subtract (1);
                              min_result_value.Assign (max_result_value);
                              min_result_value.ChangeSign;
                              if min_result_value.lt(left.info.min_value) then
                                 min_result_value.Assign (left.info.min_value);
                              if max_result_value.gt(left.info.max_value) then
                                 max_result_value.Assign (left.info.max_value)
                           end;
                        irAlwaysNonNegative:
                           begin
                              min_result_value.AsInteger := 0;
                              max_result_value.Assign (right.info.min_value);
                              max_result_value.Abs;
                              if max_result_value.lt(right.info.max_value) then
                                 max_result_value.Assign (right.info.max_value);
                              max_result_value.Subtract (1);
                              if max_result_value.gt(left.info.max_value) then
                                 max_result_value.Assign (left.info.max_value)
                           end;
                     else
                        assert (false)
                     end
               end;
         else
            assert (false)
         end
      end;

   procedure simplify_numeric_expression;
      var
         i, j: integer;
      begin
         if (first_factor.contains_integer_constant) then
            begin
               // start by combining all leading integer constants (if any) up until first variable (if any) for all integer operators
               while (Length(additional_factors) > 0)
                     and
                     (additional_factors[0].mulop in [mulop_numeric_mult, mulop_integer_div, mulop_integer_mod])
                     and
                     (additional_factors[0].factor.contains_integer_constant) do
                  begin // combine first_term with additional_factors[0]
                     case additional_factors[0].mulop of
                        mulop_numeric_mult:
                           first_factor.constant.ordinal_value.Multiply (additional_factors[0].factor.constant.ordinal_value);
                        mulop_integer_div:
                           first_factor.constant.ordinal_value.Divide (additional_factors[0].factor.constant.ordinal_value);
                        mulop_integer_mod:
                           begin
                              case target_cpu.mod_operator_implementation of
                                 iso_pascal_mod_operator_implementation:
                                    begin
                                       assert(additional_factors[0].factor.constant.ordinal_value.gt(0)); // should have been checked above
                                       first_factor.constant.ordinal_value.iso_std_pascal_mod (additional_factors[0].factor.constant.ordinal_value)
                                    end;
                                 delphi_mod_operator_implemenation:
                                    begin
                                       assert(additional_factors[0].factor.constant.ordinal_value.ne(0)); // should have been checked above
                                       first_factor.constant.ordinal_value.delphi_mod (additional_factors[0].factor.constant.ordinal_value)
                                    end;
                              else
                                 assert (false)
                              end;
                           end;
                     else
                        assert(false)
                     end;
                     delete_additional_factor(0)
                  end;

               // combine all integer_constant multiplys until interrupted by first /, div or mod
               i := 0;
               while (i < Length(additional_factors))
                     and
                     (additional_factors[i].mulop = mulop_numeric_mult) do
                  if additional_factors[i].factor.contains_integer_constant then
                     begin
                        first_factor.constant.ordinal_value.Multiply (additional_factors[i].factor.constant.ordinal_value);
                        delete_additional_factor(i)
                     end
                  else
                     i := i + 1;

               // fixup min/max after above two operations
               first_factor.info.min_value.Assign (first_factor.constant.ordinal_value);
               first_factor.info.max_value.Assign (first_factor.constant.ordinal_value)
            end;

         // look for combinable sequences of ...*c1{*v1}*c2... - multiplied integers, where contants c1&c2 can be combined
         i := 0;
         while i < Length(additional_factors) do
            if (additional_factors[i].mulop = mulop_numeric_mult)
               and
               (additional_factors[i].factor.contains_integer_constant) then
               begin // i is at c1
                  j := i + 1;
                  while (j < Length(additional_factors))
                        and
                        (additional_factors[j].mulop = mulop_numeric_mult) do
                     if additional_factors[j].factor.contains_integer_constant then
                        begin // j is at c2
                           additional_factors[i].factor.constant.ordinal_value.Multiply (additional_factors[j].factor.constant.ordinal_value);
                           delete_additional_factor(j)
                        end
                     else
                        j := j + 1;
                  additional_factors[i].factor.info.min_value.Assign (additional_factors[i].factor.constant.ordinal_value);
                  additional_factors[i].factor.info.max_value.Assign (additional_factors[i].factor.constant.ordinal_value);
                  i := j
               end
            else
               i := i + 1;

         // if term is intconst... and rest of term is real, covert intconst to realconst
         if (first_factor.contains_integer_constant)
            and
            (Length(additional_factors) > 0)
            and
            ((additional_factors[0].mulop = mulop_real_divide)
             or
             (additional_factors[0].factor.expression_kind = real_expression)
            ) then
            begin
               first_factor.constant.r := first_factor.constant.ordinal_value.AsReal;
               first_factor.constant.constant_kind := real_constant;
               first_factor.expression_kind := real_expression
            end;

         // initial part of expression may be evaluated with integer arithmetic,
         // only combine real constants in later portion of expression when evaluation
         // with real arithmetic.
         // note: only * and / operators in real portion of expression,
         // associative and commutative rules apply

         if (expression_kind = real_expression) and (Length(additional_factors) > 0)
         then
            // combine all real constants in the portion of the expression evaluated using real arithmetic
            // * and / are associative and commutative
            if first_factor.contains_real_constant then
               // entire expression will be evaluated using real arithmetic
               begin
                  i := 0;
                  while i < Length(additional_factors) do
                     if additional_factors[i].factor.contains_constant then
                        begin
                           case additional_factors[i].mulop of
                              mulop_numeric_mult:
                                 first_factor.constant.r :=
                                    first_factor.constant.r *
                                    additional_factors[i].factor.constant.real_value;
                              mulop_real_divide:
                                 first_factor.constant.r :=
                                    first_factor.constant.r /
                                    additional_factors[i].factor.constant.real_value;
                              else
                                 assert(false)
                           end;
                           delete_additional_factor(i)
                        end
                     else
                        i := i + 1;
                  // now all constants are combined in first_factor
               end
            else  // first part of expression might be evaluated using integer arithmetic
               begin
                  // find first operation that will be done in floating point arithmetic
                  i := -1;
                  if first_factor.expression_kind = real_expression then
                     i := 0
                  else
                     for j := Length(additional_factors) - 1 downto 0 do
                        if (additional_factors[j].mulop = mulop_real_divide)
                           or (additional_factors[j].factor.expression_kind = real_expression) then
                           i := j;
                  assert(i <> -1);
                  // additional_factors[i] is first floating point operation

                  // find first constant (if any) in portion to be done with floating point arithmetic
                  while (i < Length(additional_factors))
                        and
                        (not additional_factors[i].factor.contains_constant) do
                     i := i + 1;

                  if i < Length(additional_factors) then
                     begin // additional_factors[i] is first constant (real or integer) in real part of expression

                     // force first constant to be real and not a reciprocal
                        with additional_factors[i], factor do
                           begin
                              if contains_integer_constant then
                                 begin
                                    constant.r := constant.ordinal_value.AsReal;
                                    constant.constant_kind := real_constant;
                                    expression_kind := real_expression
                                 end;
                              if mulop = mulop_real_divide then
                                 begin
                                    constant.r := 1.0 / constant.r;
                                    mulop := mulop_numeric_mult
                                 end
                           end;

                        // combine any subsequent constants (real or integer) into first factor
                        j := i + 1;
                        while j < Length(additional_factors) do
                           if additional_factors[j].factor.contains_constant then
                              begin
                                 case additional_factors[j].mulop of
                                    mulop_numeric_mult:
                                       additional_factors[i].factor.constant.r :=
                                          additional_factors[i].factor.constant.r
                                          *
                                          additional_factors[j].factor.constant.real_value;
                                    mulop_real_divide:
                                       additional_factors[i].factor.constant.r :=
                                          additional_factors[i].factor.constant.r
                                          /
                                          additional_factors[j].factor.constant.real_value;
                                 else
                                    assert(false)
                                 end;
                                 delete_additional_factor(j)
                              end
                           else
                              j := j + 1
                              // now all constants in real part of expression are combined in additional_factors[i]
                     end
               end;

         // remove all *1, /1 or div 1 additional_factors (integer or real)
         i := 0;
         while i < Length(additional_factors) do
            if (((additional_factors[i].factor.contains_integer_constant)
                 and
                 (additional_factors[i].factor.constant.ordinal_value.eq(1))
                )
                or
                ((additional_factors[i].factor.contains_real_constant)
                 and
                 (additional_factors[i].factor.constant.r = 1.0)
                )
               )
               and
               (additional_factors[i].mulop in [mulop_numeric_mult, mulop_real_divide, mulop_integer_div]) then
               delete_additional_factor(i)
            else
               i := i + 1;

         // if term starts with 1* then delete the 1*
         if (first_factor.contains_constant)
            and
            (first_factor.constant.real_value = 1.0)
            and
            (Length(additional_factors) > 0)
            and
            (additional_factors[0].mulop = mulop_numeric_mult) then
            begin
               first_factor.Release;
               first_factor := additional_factors[0].factor;
               first_factor.AddRef;
               delete_additional_factor(0)
            end;

         if (first_factor.contains_constant) and (first_factor.constant.real_value = 0.0) then // entire term reduces to 0
            raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(0), src_loc));

         for i := 0 to Length(additional_factors) - 1 do
            if (additional_factors[i].factor.contains_constant)
               and
               (additional_factors[i].factor.constant.real_value = 0.0) then
               begin
                  assert(additional_factors[i].mulop = mulop_numeric_mult);
                  // div by 0 should have been caught before now
                  raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(0), src_loc))
               end
      end;

   procedure combine_set_constants;
      procedure combine
         (first_constant_factor: TExpression;
          idx: integer
         );
         begin
            while idx < Length(additional_factors) do
               if additional_factors[idx].factor.contains_constant then
                  begin
                     first_constant_factor.constant.sett :=
                        first_constant_factor.constant.sett *
                        additional_factors[idx].factor.constant.sett;
                     delete_additional_factor(idx);
                     first_constant_factor.set_min_max_for_set_constant
                  end
               else
                  idx := idx + 1
         end;
      var
         first_constant_idx: integer;
         i: integer;
      begin
         // intersection is associative and commutative
         if first_factor.contains_constant then
            combine(first_factor, 0)
         else
            begin
               // find first constant in additional_factors
               first_constant_idx := 0;
               while (first_constant_idx < Length(additional_factors))
                     and
                     (not additional_factors[first_constant_idx].factor.contains_constant) do
                  first_constant_idx := first_constant_idx + 1;
               if first_constant_idx < Length(additional_factors) then
                  combine(additional_factors[first_constant_idx].factor, first_constant_idx + 1)
            end;

         if (first_factor.contains_constant) and (first_factor.constant.sett = [])
         then
            begin // entire term simplifies to []
               first_factor.constant.AddRef;
               raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(first_factor.constant, src_loc))
            end;

         for i := 0 to Length(additional_factors) - 1 do
            if (additional_factors[i].factor.contains_constant)
               and
               (additional_factors[i].factor.constant.sett = []) then
               begin // entire term simplifies to []
                  additional_factors[i].factor.constant.AddRef;
                  raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(additional_factors[i].factor.constant, src_loc))
               end
      end; // combine_set_constants

   procedure combine_boolean_constants;
      var
         any_constant_false_factors: boolean;
         i: integer;
      begin
         any_constant_false_factors := (first_factor.contains_constant) and (first_factor.constant.b = false);
         for i := 0 to Length(additional_factors) - 1 do
            if additional_factors[i].factor.contains_constant
               and
               (additional_factors[i].factor.constant.b = false) then
               any_constant_false_factors := true;

         if any_constant_false_factors then
            // entire AND'd expression evaluates to false
            raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(false), src_loc))
         else // any constants are all true and irrelevant to AND expression
            begin
               while (first_factor.contains_constant) and (Length(additional_factors) > 0) do
                  begin
                     first_factor.Release;
                     first_factor := additional_factors[0].factor;
                     first_factor.AddRef;
                     delete_additional_factor(0)
                  end;
               i := 0;
               while i < Length(additional_factors) do
                  if additional_factors[i].factor.contains_constant then
                     delete_additional_factor(i)
                  else
                     i := i + 1
            end
      end;

   var
      factor_idx: integer;
   begin // TTerm.CreateFromSourceTokens
      inherited Create;
      src_loc := lex.token.src_loc;

      first_factor := CreateFactorFromSourceTokens;
      copy_expression_info(first_factor);

      // gather additional terms and type check them
      while lex.token_is_symbol([sym_star, sym_slash])
            or
            lex.token_is_reserved_word([rw_div, rw_mod, rw_and]) do
         begin
            factor_idx := Length(additional_factors);
            SetLength(additional_factors, factor_idx + 1);

            additional_factors[factor_idx].mulop_src_loc := lex.token.src_loc;
            if lex.token_is_symbol(sym_star) then
               case expression_kind of
                  integer_expression,
                  real_expression:
                     additional_factors[factor_idx].mulop := mulop_numeric_mult;
                  set_expression:
                     additional_factors[factor_idx].mulop := mulop_set_intersection;
               else
                  raise compile_error.Create(err_invalid_operator_for_operand, additional_factors[factor_idx].mulop_src_loc)
               end
            else if lex.token_is_symbol(sym_slash) then
               begin
                  additional_factors[factor_idx].mulop := mulop_real_divide;
                  if not (expression_kind in [integer_expression, real_expression])
                  then
                     raise compile_error.Create(err_invalid_operator_for_operand, additional_factors[factor_idx].mulop_src_loc);
                  expression_kind := real_expression
               end
            else if lex.token_is_reserved_word(rw_div) then
               begin
                  additional_factors[factor_idx].mulop := mulop_integer_div;
                  if expression_kind <> integer_expression then
                     raise compile_error.Create(err_invalid_operator_for_operand, additional_factors[factor_idx].mulop_src_loc)
               end
            else if lex.token_is_reserved_word(rw_mod) then
               begin
                  additional_factors[factor_idx].mulop := mulop_integer_mod;
                  if expression_kind <> integer_expression then
                     raise compile_error.Create(err_invalid_operator_for_operand, additional_factors[factor_idx].mulop_src_loc)
               end
            else if lex.token_is_reserved_word(rw_and) then
               if expression_kind = boolean_expression then
                  additional_factors[factor_idx].mulop := mulop_boolean_and
               else
                  raise compile_error.Create(err_invalid_operator_for_operand, additional_factors[factor_idx].mulop_src_loc)
            else
               assert(false);
            lex.advance_token;

            additional_factors[factor_idx].factor := CreateFactorFromSourceTokens;

            // check type
            case expression_kind of
               integer_expression:
                  begin
                     if not (additional_factors[factor_idx].factor.expression_kind in [integer_expression, real_expression]) then
                        raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].mulop_src_loc);

                     if (additional_factors[factor_idx].mulop in [mulop_real_divide, mulop_integer_div, mulop_integer_mod])
                        and
                        (additional_factors[factor_idx].factor.contains_constant)
                        and
                        (additional_factors[factor_idx].factor.constant.real_value = 0) then
                        raise compile_error.Create(err_division_by_zero, additional_factors[factor_idx].factor.src_loc);

                     if additional_factors[factor_idx].factor.expression_kind = real_expression then
                        begin
                           expression_kind := real_expression;
                           if additional_factors[factor_idx].mulop in [mulop_integer_div, mulop_integer_mod] then
                              raise compile_error.Create(err_real_divisor_cant_be_used_with_div_or_mod, additional_factors[factor_idx].factor.src_loc);
                           assert(additional_factors[factor_idx].mulop = mulop_numeric_mult)
                        end
                  end;
               real_expression:
                  begin
                     if not (additional_factors[factor_idx].factor.expression_kind in [integer_expression, real_expression]) then
                        raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].factor.src_loc);

                     if (additional_factors[factor_idx].mulop = mulop_real_divide)
                        and
                        (additional_factors[factor_idx].factor.contains_constant)
                        and
                        (additional_factors[factor_idx].factor.constant.real_value = 0) then
                        raise compile_error.Create(err_division_by_zero, additional_factors[factor_idx].factor.src_loc)
                  end;
               boolean_expression:
                  if additional_factors[factor_idx].factor.expression_kind <> boolean_expression then
                     raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].factor.src_loc);
               set_expression:
                  begin
                     if additional_factors[factor_idx].factor.expression_kind <> set_expression then
                        raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].factor.src_loc);
                     if additional_factors[factor_idx].factor.set_ordinal_base_type <> empty_set_ordinal_base_unknown then
                        if set_ordinal_base_type = empty_set_ordinal_base_unknown then
                           begin
                              set_ordinal_base_type := additional_factors[factor_idx].factor.set_ordinal_base_type;
                              if set_ordinal_base_type = ordinal_base_is_enum then
                                 enum_typedef := additional_factors[factor_idx].factor.enum_typedef
                           end
                        else
                           begin
                              if set_ordinal_base_type <> additional_factors[factor_idx].factor.set_ordinal_base_type then
                                 raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].factor.src_loc);
                              if (set_ordinal_base_type = ordinal_base_is_enum)
                                 and
                                 (enum_typedef <> additional_factors[factor_idx].factor.enum_typedef) then
                                 raise compile_error.Create(err_incompatible_operand_types, additional_factors[factor_idx].factor.src_loc)
                           end
                  end;
            else
               assert(false)
            end
         end;
      // at this point all factors should be correct type and all operators appropriate

      // simplify expressions by combining constant factors where possible
      if Length(additional_factors) > 0 then
         case expression_kind of
            integer_expression, real_expression:
               simplify_numeric_expression;
            set_expression:
               combine_set_constants;
            boolean_expression:
               combine_boolean_constants;
         else
            assert(false)
         end;

      // determine term kind & limits
      case expression_kind of
         integer_expression,
         real_expression:
            begin
               expression_kind := first_factor.expression_kind;
               info.Assign (first_factor.info);
               for factor_idx := 0 to Length(additional_factors) - 1 do
                  begin
                     if expression_kind = integer_expression then
                        begin
                           if factor_idx = 0 then
                              additional_factors[factor_idx].left_info := first_factor.info
                           else
                              additional_factors[factor_idx].left_info := additional_factors[factor_idx-1].result_info;
                           additional_factors[factor_idx].left_info.AddRef
                        end;

                     case additional_factors[factor_idx].mulop of
                        mulop_numeric_mult:
                           case expression_kind of
                              integer_expression:
                                 case additional_factors[factor_idx].factor.expression_kind of
                                    integer_expression:
                                       begin
                                          if (additional_factors[factor_idx].factor.contains_constant)
                                             and
                                             (additional_factors[factor_idx].factor.constant.ordinal_value.IsPow2(additional_factors[factor_idx].mulop_param)) then
                                              additional_factors[factor_idx].mulop := mulop_shl
                                          else
                                             additional_factors[factor_idx].mulop := mulop_mult_int_by_int;
                                          adjust_mult_range(Self, additional_factors[factor_idx].factor)
                                       end;
                                    real_expression:
                                       begin
                                          additional_factors[factor_idx].mulop := mulop_mult_int_by_flt;
                                          expression_kind := real_expression
                                       end;
                                 else
                                    assert (false)
                                 end;
                              real_expression:
                                 additional_factors[factor_idx].mulop := mulop_mult_flt_by_flt;
                           else
                              assert (false)
                           end;
                        mulop_integer_div:
                           begin
                              if (additional_factors[factor_idx].factor.contains_constant)
                                 and
                                 (additional_factors[factor_idx].factor.constant.ordinal_value.IsPow2(additional_factors[factor_idx].mulop_param)) then
                                 additional_factors[factor_idx].mulop := mulop_shr;
                              adjust_div_range(Self, additional_factors[factor_idx].factor)
                           end;
                        mulop_integer_mod:
                           begin
                              if (additional_factors[factor_idx].left_info.IntegerRange = irAlwaysNonNegative)
                                 and
                                 (additional_factors[factor_idx].factor.contains_constant)
                                 and
                                 (additional_factors[factor_idx].factor.constant.ordinal_value.IsPow2)then
                                 additional_factors[factor_idx].mulop := mulop_mask;
                              adjust_mod_range(Self, additional_factors[factor_idx].factor)
                           end;
                        mulop_real_divide:
                           case expression_kind of
                              integer_expression:
                                 begin
                                    additional_factors[factor_idx].mulop := mulop_divide_int_by_flt;
                                    expression_kind := real_expression
                                 end;
                              real_expression:
                                 additional_factors[factor_idx].mulop := mulop_divide_flt_by_flt;
                           else
                              assert (false)
                           end;
                     else
                        assert(false)
                     end;

                     if expression_kind = integer_expression then
                        begin
                           info.min_value.Assign (min_result_value);
                           info.max_value.Assign (max_result_value);
                           additional_factors[factor_idx].result_info := target_cpu.TTypeInfo_Create (info)
                        end
                  end
            end;
         enum_expression:
            info.Assign (first_factor.info);
         boolean_expression:
            begin
               info.Assign (first_factor.info);
               for factor_idx := 0 to Length(additional_factors) - 1 do
                  if (additional_factors[factor_idx].factor.contains_constant)
                     and
                     (not additional_factors[factor_idx].factor.constant.b) then
                     begin // if any factor is false, entire term is false
                        info.min_value.AsInteger := 0;
                        info.max_value.AsInteger := 0
                     end
                  else if additional_factors[factor_idx].factor.info.min_value.eq(0) then
                     info.min_value.AsInteger := 0
            end;
         set_expression:
            begin
               info.Assign (first_factor.info);
               for factor_idx := 0 to Length(additional_factors) - 1 do
                  begin
                     if additional_factors[factor_idx].factor.info.min_value.gt(info.min_value)
                     then
                        info.min_value.Assign (additional_factors[factor_idx].factor.info.min_value);
                     if info.max_value.gt(additional_factors[factor_idx].factor.info.max_value)
                     then
                        info.max_value.Assign (additional_factors[factor_idx].factor.info.max_value);
                     if info.min_value.gt(info.max_value) then
                        begin
                           info.min_value.AsInteger := max_set;
                           info.max_value.AsInteger := min_set
                        end
                  end
            end
         else
            // no further work needed for other expression_kinds
      end;

      // reduce to single constant if possible
      case expression_kind of
         integer_expression:
            if info.min_value.eq (info.max_value) then
               raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateIntegerConstant(info.min_value), src_loc));
         boolean_expression:
            if info.min_value.eq (info.max_value) then
               raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateBooleanConstant(boolean(info.min_value.AsInteger)), src_loc));
         set_expression:
            if info.min_value.gt(info.max_value) then // result will always be empty set
               if set_ordinal_base_type = ordinal_base_is_enum then
                  raise ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateSetConstant([], enum_typedef), src_loc))
               else
                  raise  ETermExpressionSimplification.Create(target_cpu.TConstantPrimary_CreateFromConstant(TConstant.CreateSetConstant([], set_ordinal_base_type), src_loc));
         string_expression, enum_expression, char_expression, real_expression,
         record_expression, packed_record_expression, overlay_expression,
         array_expression, system_type_expression:
            ; // do nothing
         else
            assert(false)
      end;

      if Length(additional_factors) = 0 then
         begin
            first_factor.AddRef;
            raise ETermExpressionSimplification.Create(first_factor)
         end
   end; // TTerm.CreateFromSourceTokens

destructor TTerm.Destroy;
   var
      i: integer;
   begin
      first_factor.Release;
      for i := 0 to Length(additional_factors) - 1 do
         begin
            additional_factors[i].left_info.Release;
            additional_factors[i].factor.Release;
            additional_factors[i].result_info.Release
         end;
      inherited
   end;

procedure TTerm.MarkAsReachable;
   var
      i: integer;
   begin
      first_factor.MarkAsReachable;
      for i := 0 to Length(additional_factors) - 1 do
         additional_factors[i].factor.MarkAsReachable;
      inherited
   end;

function TTerm.CheckForProhibitedDelayCall (err_msg: string): boolean;
   var
      i: integer;
   begin
      result := first_factor.CheckForProhibitedDelayCall (err_msg);
      for i := 0 to Length(additional_factors) - 1 do
         if additional_factors[i].factor.CheckForProhibitedDelayCall (err_msg) then
            result := true
   end;

procedure TTerm.check_set_constants_range (valid_range: TTypeInfo; src_loc: TSourceLocation);
   var i: integer;
   begin
      first_factor.check_set_constants_range (valid_range, src_loc);
      for i := 0 to Length(additional_factors)-1 do
         additional_factors[i].factor.check_set_constants_range (valid_range, src_loc)
   end;


INITIALIZATION
   min_result_value := TMultiPrecisionInteger.Create;
   max_result_value := TMultiPrecisionInteger.Create;
   t1 := TMultiPrecisionInteger.Create;
   t2 := TMultiPrecisionInteger.Create;

FINALIZATION
   min_result_value.Free;
   max_result_value.Free;
   t1.Free;
   t2.Free;

END.
