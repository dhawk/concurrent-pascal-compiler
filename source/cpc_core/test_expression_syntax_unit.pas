unit test_expression_syntax_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

interface

procedure test_lex_analysis;
procedure test_TPrimary;
procedure test_TFactor;
procedure test_TTerm;
procedure test_TSimpleExpression;
procedure test_TRelationalExpression;

implementation

uses
   cpc_common_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_expressions_unit,
   cpc_simple_expression_unit,
   cpc_target_cpu_unit,
   cpc_term_expression_unit,
   Math,
   SysUtils,
   test_cpu_unit,
   test_subroutines_unit;

function approximately_equal 
   (r1,r2: real
   ): boolean;
   const 
      c = 10000;
   begin
      result := round(r1*c) = round(r2*c)
   end;
   
   //=============
   //  Generators
   
procedure test_constant_reduction 
   (expr: TExpression
   );
   var
      set_min_seen, set_max_seen: int64;
      i: integer;
   procedure test_limits 
      (c: integer
      );
      begin
         if c <> expr.info.min_value.AsInteger then
            raise compile_error.Create('error in constant reduction: min_ord not set to constant value', expr.src_loc);
         if c <> expr.info.max_value.AsInteger then
            raise compile_error.Create('error in constant reduction: max_ord not set to constant value', expr.src_loc);
      end;
   begin
      if expr.contains_constant then
         case expr.expression_kind of
            integer_expression:
               test_limits (expr.constant.ordinal_value.AsInteger);
            real_expression:
               ;
            string_expression:
               test_limits (Length(expr.constant.s));
            enum_expression:
               begin
                  test_limits (expr.constant.ordinal_value.AsInteger);
                  if expr.enum_typedef <> expr.constant.enum_typedef then
                     raise compile_error.Create('error in constant reduction: enum typedef incorrect', expr.src_loc);
               end;
            boolean_expression:
               test_limits (ord(expr.constant.b));
            set_expression:
               begin
                  if expr.set_ordinal_base_type <> expr.constant.set_ordinal_base_type then
                     raise compile_error.Create('error in constant reduction: ordinal basetype incorrect', expr.src_loc);
                  if expr.constant.set_ordinal_base_type = empty_set_ordinal_base_unknown then
                     begin
                        if expr.info.min_value.AsInteger <> max_set then
                           raise compile_error.Create('error in constant reduction: min_ord not set to High(int64) for empty set', expr.src_loc);
                        if expr.info.max_value.AsInteger <> min_set then
                           raise compile_error.Create('error in constant reduction: max_ord not set to Low(int64) for empty set', expr.src_loc);
                     end
                  else
                     begin
                        if (expr.set_ordinal_base_type = ordinal_base_is_enum)
                        and
                        (expr.enum_typedef <> expr.constant.enum_typedef) then
                           raise compile_error.Create('error in constant reduction: set enum typedef incorrect', expr.src_loc);
                        set_min_seen := max_set;
                        set_max_seen := min_set;
                        for i := min_set to max_set do
                           if i in expr.constant.sett then
                              begin
                                 if i > set_max_seen then
                                    set_max_seen := i;
                                 if i < set_min_seen then
                                    set_min_seen := i
                              end;
                        if set_min_seen <> expr.info.min_value.AsInteger then
                           raise compile_error.Create('error in constant reduction: min_ord not set to lowest set value', expr.src_loc);
                        if set_max_seen <> expr.info.max_value.AsInteger then
                           raise compile_error.Create('error in constant reduction: max_ord not set to largest set value', expr.src_loc);
                     end
               end;
            char_expression:
               test_limits (ord(expr.constant.s[1]));
            else
               assert (false);
         end;
   end;

function create_TExpression: TDefinition;
   var 
      expr: TExpression;
   begin
      expr := CreateExpressionFromSourceTokens;
      test_constant_reduction (expr);
      result := expr
   end;
   
function create_TSimpleExpression: TDefinition;
   var
      se: TSimpleExpression;
      real_op_seen: boolean;
      i: integer;
   begin
      try
         se := target_cpu.TSimpleExpression_CreateFromSourceTokens;
         test_constant_reduction (se);
         result := se;
         
         // test scheduling (int ops, then real ops)
         if not se.contains_constant then
            begin
               real_op_seen := se.first_term.expression_kind = real_expression;
               for i := 0 to Length(se.additional_terms)-1 do
                  if (se.additional_terms[i].right_term.expression_kind = integer_expression)
                  and
                  real_op_seen then
                     raise compile_error.Create('term scheduling error (int ops should be first)', se.src_loc)
                  else
                  if se.additional_terms[i].right_term.expression_kind = real_expression then
                     real_op_seen := true
            end
      except
         on e:ESimpleExpressionSimplification
         do result := e.simplified_expr
      end
   end;
   
function create_TPrimary: TDefinition;
   var 
      expr: TExpression;
   begin
      expr := CreatePrimaryFromSourceTokens;
      test_constant_reduction (expr);
      result := expr
   end;
   
function create_TFactor: TDefinition;
   var 
      expr: TExpression;
   begin
      expr := CreateFactorFromSourceTokens;
      test_constant_reduction (expr);
      result := expr
   end;
   
function create_TTerm: TDefinition;
   var 
      expr: TExpression;
   begin
      expr := CreateTermFromSourceTokens; // TTerm.CreateFromSourceTokens;
      test_constant_reduction (expr);
      result := expr
   end;
   
   
   //==================
   //  Test Procedures
   
function setup_test 
   (s: string;
    create_expression: TTestGenerator
   ): TExpression;
   begin
      result := TExpression (test_fragment (s, create_expression))
//      setup_test_for_program_fragment (s);
//      result := TExpression(create_expression)
   end;
   
procedure takedown_test 
   (expr: TExpression; 
    s: string
   );
   begin
      expr.Release;
//      conclude_test_of_program_fragment (s)
   end;
   
procedure test_integer_constant_expected
   (s: string; 
    create_expression: TTestGenerator;
    expected_result: integer
   );
   var 
      expr: TExpression;
   begin
      expr := setup_test (s, create_expression);
      if not expr.contains_constant
      then
         begin
            record_bad_test_result;
            display (s + ': failed wasn''t a constant');
         end;
      if expr.expression_kind <> integer_expression
      then
         begin
            record_bad_test_result;
            display (s + ': failed, wasn''t an integer');
         end;
      if (expr.contains_constant) and (expr.constant.ordinal_value.AsInteger <> expected_result)
      then
         begin
            record_bad_test_result;
            display (s + ': failed, const was ' + IntToStr(expr.constant.ordinal_value.AsInteger) + ' (expected ' + IntToStr(expected_result) + ')');
         end;
      takedown_test (expr, s)
   end;
   
procedure test_integer_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_min_ord, expected_max_ord: int64
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if expr.contains_constant then
         begin
            record_bad_test_result;
            display (s + ': failed (was constant)');
         end;
      if expr.expression_kind <> integer_expression then
         begin
            record_bad_test_result;
            display (s + ': failed, wasn''t integer');
         end;
      if expr.info.min_value.AsInteger <> expected_min_ord then
         begin
            record_bad_test_result;
            display (s + ': failed min_ord was ' + IntToStr(expr.info.min_value.AsInteger) + ' (expected ' + IntToStr(expected_min_ord) + ')');
         end;
      if expr.info.max_value.AsInteger <> expected_max_ord then
         begin
            record_bad_test_result;
            display (s + ': failed max_ord was ' + IntToStr(expr.info.max_value.AsInteger) + ' (expected ' + IntToStr(expected_max_ord) + ')');
         end;
      expr.Release
   end;
   
procedure test_real_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_result: real
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_real_constant)
         or
         (round (expr.constant.r * 1000) <> round (expected_result*1000)) then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
      expr.Release
   end;
   
procedure test_real_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (expr.contains_constant) or (expr.expression_kind <> real_expression)
      then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
      expr.Release
   end;
   
procedure test_char_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_result: char
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_constant) or (expr.expression_kind <> char_expression) or (expr.constant.s <> expected_result)
      then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
      expr.Release
   end;
   
procedure test_char_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_min, expected_max: char
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if expr.contains_constant then
         begin
            record_bad_test_result;
            display (s + ': failed (was constant)');
         end;
      if expr.expression_kind <> char_expression then
         begin
            record_bad_test_result;
            display (s + ': failed, wasn''t integer');
         end;
      if expr.info.min_value.AsInteger <> ord(expected_min) then
         begin
            record_bad_test_result;
            display (s + ': failed min_ord was ' + IntToStr(expr.info.min_value.AsInteger) + ' (expected ''' + expected_min + ''')');
         end;
      if expr.info.max_value.AsInteger <> ord(expected_max) then
         begin
            record_bad_test_result;
            display (s + ': failed max_ord was ' + IntToStr(expr.info.max_value.AsInteger) + ' (expected ''' + expected_max + ''')');
         end;
      expr.Release
   end;
   
procedure test_string_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_result: string
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_constant) or (not (expr.expression_kind in [char_expression, string_expression])) or (expr.constant.s <> expected_result)
      then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
      if (expr.contains_constant) and ((expr.info.min_value.AsInteger <> Length(expected_result)) or (expr.info.max_value.AsInteger <> Length(expected_result))) then
         begin
            record_bad_test_result;
            display (s + ': failed min/max wrong');
         end;
      expr.Release
   end;
   
procedure test_string_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_min_length, expected_max_length: integer
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (expr.contains_constant) or (not (expr.expression_kind in [char_expression, string_expression]))
      then
         begin
            record_bad_test_result;
            display (s + ': failed not char or string expr');
         end;
      if expr.expression_kind = string_expression then
         begin
            if expr.info.min_value.AsInteger <> ord(expected_min_length) then
               begin
                  record_bad_test_result;
                  display (s + ': failed min_ord was ' + IntToStr(expr.info.min_value.AsInteger) + ' (expected ' + IntToStr(ord(expected_min_length)) + ')');
               end;
            if expr.info.max_value.AsInteger <> ord(expected_max_length) then
               begin
                  record_bad_test_result;
                  display (s + ': failed max_ord was ' + IntToStr(expr.info.max_value.AsInteger) + ' (expected ' + IntToStr(ord(expected_max_length)) + ')');
               end;
         end
      else
         begin
            if 1 <> ord(expected_min_length)
            then
               begin
                  record_bad_test_result;
                  display (s + ': failed min_ord was 1 (expected ' + IntToStr(ord(expected_min_length)) + ')');
               end;
            if 1 <> ord(expected_max_length) then
               begin
                  record_bad_test_result;
                  display (s + ': failed max_ord was 1 (expected ' + IntToStr(ord(expected_max_length)) + ')');
               end;
         end;
      expr.Release
   end;
   
procedure test_boolean_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_result: boolean
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_constant) or (expr.expression_kind <> boolean_expression) or (expr.constant.b <> expected_result)
      then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
      expr.Release
   end;
   
procedure test_boolean_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_min, expected_max: boolean
   );
   var 
      expr: TExpression;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if expr.contains_constant then
         begin
            record_bad_test_result;
            display (s + ': failed (was constant)');
         end;
      if expr.expression_kind <> boolean_expression then
         begin
            record_bad_test_result;
            display (s + ': failed, wasn''t boolean');
         end;
      if expr.info.min_value.AsInteger <> ord(expected_min) then
         begin
            record_bad_test_result;
            display (s + ': failed min_ord was ' + IntToStr(expr.info.min_value.AsInteger) + ' (expected ' + IntToStr(ord(expected_min)) + ')');
         end;
      if expr.info.max_value.AsInteger <> ord(expected_max) then
         begin
            record_bad_test_result;
            display (s + ': failed max_ord was ' + IntToStr(expr.info.max_value.AsInteger) + ' (expected ' + IntToStr(ord(expected_max)) + ')');
         end;
      expr.Release
   end;
   
procedure test_enum_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    ord_of_expected_enum: integer; 
    enum_type: string
   );
   var 
      expr: TExpression;
//      info: TSymbolInfo;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_constant)
         or
         (expr.expression_kind <> enum_expression)
         or
         (expr.constant.ordinal_value.AsInteger <> ord_of_expected_enum) then
         begin
            record_bad_test_result;
            display (s + ': failed');
         end;
//      info := symbol_table.find_symbol_table_entry(enum_type);
//      if (info.token_kind <> identifier_token)
//      or
//      (expr.constant.enum_typedef <> CurrentDefinitionTable[info.id]) then
//         begin
//            record_bad_test_result;
//            display (s + ': failed, enum type wrong')
//         end;
      expr.Release
   end;
   
procedure test_enum_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    min_ord_of_expected_enum, max_ord_of_expected_enum: integer; 
    enum_type: string
   );
   var 
      expr: TExpression;
//      info: TSymbolInfo;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if expr.contains_constant then
         begin
            record_bad_test_result;
            display (s + ': failed (was constant)');
         end;
      if expr.expression_kind <> enum_expression then
         begin
            record_bad_test_result;
            display (s + ': failed, wasn''t enum');
         end;
      if expr.info.min_value.AsInteger <> min_ord_of_expected_enum then
         begin
            record_bad_test_result;
            display (s + ': failed min_ord was ' + IntToStr(expr.info.min_value.AsInteger) + ' (expected ' + IntToStr(min_ord_of_expected_enum) + ')');
         end;
      if expr.info.max_value.AsInteger <> max_ord_of_expected_enum then
         begin
            record_bad_test_result;
            display (s + ': failed max_ord was ' + IntToStr(expr.info.max_value.AsInteger) + ' (expected ' + IntToStr(max_ord_of_expected_enum) + ')');
         end;
//      info := symbol_table.find_symbol_table_entry(enum_type);
//      if (info.token_kind <> identifier_token)
//         or
//         (expr.enum_typedef <> CurrentDefinitionTable[info.id]) then
//         begin
//            record_bad_test_result;
//            display (s + ': failed, enum type wrong')
//         end;
      expr.Release
   end;
   
procedure test_set_constant_expected 
   (s: string;
    create_expression: TTestGenerator;
    expected_set: TSet256;
    expected_ordinal_base_type: TOrdinalBaseType;
    expected_set_base_enum_typedef: string;
    expected_min_value, expected_max_value: integer
   );
   var
      expr: TExpression;
//      info: TSymbolInfo;
      min_value, max_value, i: integer;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if (not expr.contains_constant) or (expr.expression_kind <> set_expression) or (expr.constant.sett <> expected_set) then
         begin
            record_bad_test_result;
            display (s + ': failed, not a constant or not a set or wrong result');
         end;
      if (expr.contains_constant) and (expr.constant.set_ordinal_base_type <> expected_ordinal_base_type) then
         begin
            record_bad_test_result;
            display (s + ': failed, set base type wrong')
         end;
//      if (expr.contains_constant) and (expected_ordinal_base_type = ordinal_base_is_enum) then
//         begin
//            info := symbol_table.find_symbol_table_entry(expected_set_base_enum_typedef);
//            if (info.token_kind <> identifier_token)
//            or
//            (expr.constant.enum_typedef <> CurrentDefinitionTable [info.id]) then
//               begin
//                  record_bad_test_result;
//                  display (s + ': failed, set base type wrong')
//               end
//         end;
      if expr.contains_constant then
         begin
            min_value := maxint;
            max_value := -maxint;
            for i := min_set to max_set do
               if i in expr.constant.sett  then
                  begin
                     if i > max_value then
                        max_value := i;
                     if i < min_value then
                        min_value := i
                  end;
            if min_value <> expected_min_value then
               begin
                  record_bad_test_result;
                  display (s + ': failed expected min value wrong')
               end;
            if max_value <> expected_max_value then
               begin
                  record_bad_test_result;
                  display (s + ': failed expected max value wrong')
               end
         end;
      expr.Release
   end;
   
procedure test_set_non_constant_expected 
   (s: string; 
    create_expression: TTestGenerator;
    expected_min_ord, expected_max_ord: integer; 
    expected_ordinal_base_type: TOrdinalBaseType; 
    expected_set_base_enum_typedef: string
   );
   var 
      expr: TExpression;
//      info: TSymbolInfo;
   begin
      expr := TExpression(test_fragment (s, create_expression));
      if expr.contains_constant
      then
         begin
            record_bad_test_result;
            display (s + ': failed (was constant)');
         end;
      if expr.set_ordinal_base_type <> expected_ordinal_base_type then
         begin
            record_bad_test_result;
            display (s + ': failed, set base type wrong')
         end;
//      if expected_ordinal_base_type = ordinal_base_is_enum then
//         begin
//            info := symbol_table.find_symbol_table_entry(expected_set_base_enum_typedef);
//            if (info.token_kind <> identifier_token)
//            or
//            (expr.enum_typedef <> CurrentDefinitionTable [info.id]) then
//               begin
//                  record_bad_test_result;
//                  display (s + ': failed, set base type wrong')
//               end
//         end;
      expr.Release
   end;
   
   
//============================
//  Constant Expression Tests
   
procedure test_primary_constant_expressions 
   (create_expression: TTestGenerator
   );
   type
      x=(a,b,c);
   begin
      test_integer_constant_expected('begin ord(3) end', create_expression, 3);
      test_integer_constant_expected('begin ord(true) end', create_expression, ord(true));
      test_integer_constant_expected('begin ord(''a'') end', create_expression, ord('a'));
      test_integer_constant_expected('type e=(a,b,c); begin ord(c) end', create_expression, 2);
      test_integer_constant_expected('begin 3 end', create_expression, 3);
      test_real_constant_expected ('begin 3.14 end', create_expression, 3.14);
      test_string_constant_expected ('begin ''asdf'' end', create_expression, 'asdf');
      test_boolean_constant_expected ('begin true end', create_expression, true);
      test_boolean_constant_expected ('begin false end', create_expression, false);
      test_integer_constant_expected('const a=5; begin a end', create_expression, 5);
      test_boolean_constant_expected ('begin (true) end', create_expression, true);
      test_string_constant_expected ('begin ''asdf'' end', create_expression, 'asdf');
      test_compile_error_generation_for_program_fragment ('begin a end', create_expression, err_undefined_identifier, 'a end');
      test_enum_constant_expected ('type x = (a,b,c); begin b end', create_expression, 1, 'x');
      test_boolean_constant_expected ('begin not true end', create_expression, false);
      test_boolean_constant_expected ('begin not false end', create_expression, true);
      test_integer_constant_expected('begin ord(55) end', create_expression, 55);
      test_integer_constant_expected('type x = (a,b,c); begin ord(c) end', create_expression, ord(c));
      test_integer_constant_expected('begin ord(''a'') end', create_expression, ord('a'));
      test_integer_constant_expected('begin ord(true) end', create_expression, ord(true));

      test_integer_constant_expected('begin high(int8) end', create_expression, 127);
      test_integer_constant_expected('type x=2..23; begin high(x) end', create_expression, 23);
      test_boolean_constant_expected ('begin high(boolean) end', create_expression, true);
      test_boolean_constant_expected ('type x=false..false; begin high(x) end', create_expression, false);
      test_enum_constant_expected ('type x=(a,b,c); begin high(x) end', create_expression, 2, 'x');
      test_char_constant_expected ('type x=''a''..''c''; begin high(x) end', create_expression, 'c');

      test_integer_constant_expected('begin low(int8) end', create_expression, -128);
      test_integer_constant_expected('type x=2..23; begin low(x) end', create_expression, 2);
      test_boolean_constant_expected ('begin low(boolean) end', create_expression, false);
      test_boolean_constant_expected ('type x=true..true; begin low(x) end', create_expression, true);
      test_enum_constant_expected ('type x=(a,b,c); begin low(x) end', create_expression, 0, 'x');
      test_char_constant_expected ('type x=''a''..''c''; begin low(x) end', create_expression, 'a');

      test_integer_constant_expected ('begin succ(5) end', create_expression, 6);
      test_boolean_constant_expected ('begin succ(false) end', create_expression, true);
      test_enum_constant_expected ('type x=(a,b,c); begin succ(b) end', create_expression, 2, 'x');
      test_char_constant_expected ('begin succ(''b'') end', create_expression, 'c');

      test_integer_constant_expected ('begin pred(5) end', create_expression, 4);
      test_boolean_constant_expected ('begin pred(true) end', create_expression, false);
      test_enum_constant_expected ('type x=(a,b,c); begin pred(b) end', create_expression, 0, 'x');
      test_char_constant_expected ('begin pred(''b'') end', create_expression, 'a');

      test_integer_constant_expected ('begin abs(4) end', create_expression, 4);
      test_integer_constant_expected ('begin abs(-4) end', create_expression, 4);
      test_real_constant_expected ('begin abs(4.3) end', create_expression, 4.3);
      test_real_constant_expected ('begin abs(-4.3) end', create_expression, 4.3);

      test_compile_error_generation_for_program_fragment ('begin ord(1.7) end', create_expression, err_ordinal_expression_expected, '1.7) end');
      test_compile_error_generation_for_program_fragment ('begin ord (''ab'') end', create_expression, err_ordinal_expression_expected, '''ab'') end');
      test_char_constant_expected ('begin chr(ord(''a'')) end', create_expression, 'a');
      test_compile_error_generation_for_program_fragment ('begin chr(1.5) end', create_expression, err_ordinal_expression_expected, '1.5) end');
      test_compile_error_generation_for_program_fragment ('begin chr(666) end', create_expression, err_char_value_outside_legal_range, '666) end');
      test_set_constant_expected ('begin [1,3,4] end', create_expression, [1,3,4], ordinal_base_is_integer, '', 1, 4);
      test_set_constant_expected ('begin [true] end', create_expression, [1], ordinal_base_is_bool, '', 1, 1);
      test_set_constant_expected ('begin [chr(1),chr(3),chr(4)] end', create_expression, [1,3,4], ordinal_base_is_char, '', 1, 4);
      test_set_constant_expected ('type x=(a,b,c); begin [a,c] end', create_expression, [0,2], ordinal_base_is_enum, 'x', 0, 2);
      test_set_constant_expected ('begin[0,255]end', create_expression, [0,255], ordinal_base_is_integer, '', 0, 255);
      test_set_constant_expected ('begin[]end', create_expression, [], empty_set_ordinal_base_unknown, '', maxint, -maxint);
      test_set_constant_expected ('begin [2..4, 6] end', create_expression, [2..4, 6], ordinal_base_is_integer, '', 2, 6);
      test_set_constant_expected ('begin [false..true] end', create_expression, [0,1], ordinal_base_is_bool, '', 0, 1);
      test_set_constant_expected ('begin [''a''..''c'', ''e''] end', create_expression, [ord('a')..ord('c'), ord('e')], ordinal_base_is_char, '', ord('a'), ord('e'));
      test_set_constant_expected ('type x=(a,b,c); begin [a..c] end', create_expression, [ord(a)..ord(c)], ordinal_base_is_enum, 'x', 0, 2);
      test_integer_constant_expected ('begin round (4) end', create_expression, 4);
      test_integer_constant_expected ('begin round (3.7) end', create_expression, 4);
      test_integer_constant_expected ('begin trunc (4) end', create_expression, 4);
      test_integer_constant_expected ('begin trunc (3.7) end', create_expression, 3);
      test_compile_error_generation_for_program_fragment ('type x=(a,b,c);y=(d,e,f);begin[a,d]end', create_expression, err_set_member_type_differs_from_previous, 'd]end');
      test_compile_error_generation_for_program_fragment ('type x=(a,b,c);begin[a,1]end', create_expression, err_set_member_type_differs_from_previous, '1]end');
      test_compile_error_generation_for_program_fragment ('type x=(a,b,c);begin[1,a]end', create_expression, err_set_member_type_differs_from_previous, 'a]end');
      test_compile_error_generation_for_program_fragment ('type x=(a,b,c);begin[true,a]end', create_expression, err_set_member_type_differs_from_previous, 'a]end');
      test_compile_error_generation_for_program_fragment ('type x=(a,b,c);begin[''a'',a]end', create_expression, err_set_member_type_differs_from_previous, 'a]end');
      test_compile_error_generation_for_program_fragment ('begin [''asdf''] end', create_expression, err_ordinal_expression_expected, '''asdf''] end');
      test_compile_error_generation_for_program_fragment ('begin [''''] end', create_expression, err_ordinal_expression_expected, '''''] end');
      test_compile_error_generation_for_program_fragment ('begin[-1,2]end', create_expression, err_set_member_value_outside_legal_range, '-1,2]end');
      test_compile_error_generation_for_program_fragment ('begin[256]end', create_expression, err_set_member_value_outside_legal_range, '256]end');

      test_integer_non_constant_expected ('var i: int8; begin i end', create_expression, -128, 127);
      test_real_non_constant_expected ('var r: real; begin r end', create_expression);
      test_char_non_constant_expected ('var c: ''a''..''d'' := ''c''; begin c end', create_expression, 'a', 'd');
      test_boolean_non_constant_expected ('var b: boolean; begin b end', create_expression, false, true);
      test_enum_non_constant_expected ('type te=(a,b,c);var e:te; begin e end', create_expression, 0, 2, 'te');
      test_set_non_constant_expected ('type ts=set of 3..5; var s: ts; begin s end', create_expression, 3, 5, ordinal_base_is_integer, '');
      test_integer_non_constant_expected ('var r: real; begin round(r) end', create_expression, -32768, 32767);
      test_integer_non_constant_expected ('var r: real; begin trunc(r) end', create_expression, -32768, 32767);

      test_integer_non_constant_expected ('function i: int8; begin end; begin i end', create_expression, -128, 127);
      test_real_non_constant_expected ('function r: real; begin end; begin r end', create_expression);
      test_char_non_constant_expected ('function c: ''a''..''d''; begin end; begin c end', create_expression, 'a', 'd');
      test_boolean_non_constant_expected ('function b: boolean; begin end; begin b end', create_expression, false, true);
      test_enum_non_constant_expected ('type te=(a,b,c); function e:te; begin end; begin e end', create_expression, 0, 2, 'te');
      test_set_non_constant_expected ('type ts=set of 3..5; function s: ts; begin end; begin s end', create_expression, 3, 5, ordinal_base_is_integer, '');

      test_integer_non_constant_expected ('function i: int8; begin end; begin (i) end', create_expression, -128, 127);
      test_real_non_constant_expected ('function r: real; begin end; begin (r) end', create_expression);
      test_char_non_constant_expected ('function c: ''a''..''d''; begin end; begin (c) end', create_expression, 'a', 'd');
      test_boolean_non_constant_expected ('function b: boolean; begin end; begin (b) end', create_expression, false, true);
      test_enum_non_constant_expected ('type te=(a,b,c); function e:te; begin end; begin (e) end', create_expression, 0, 2, 'te');
      test_set_non_constant_expected ('type ts=set of 3..5; function s: ts; begin end; begin (s) end', create_expression, 3, 5, ordinal_base_is_integer, '');
      test_boolean_constant_expected ('var b: true..true := true; begin b end', create_expression, true);
   end;

procedure test_factor_constant_expressions
   (create_expression: TTestGenerator
   );
   begin
      test_integer_constant_expected('begin +3 end', create_expression, 3);
      test_real_constant_expected ('begin +3.14 end', create_expression, 3.14);
      test_integer_constant_expected('begin -3 end', create_expression, -3);
      test_real_constant_expected ('begin -3.14 end', create_expression, -3.14);
      test_integer_constant_expected('begin -+-3 end', create_expression, 3);
      test_real_constant_expected ('begin -+-3.14 end', create_expression, 3.14);

      test_integer_non_constant_expected ('var i: int8; begin -i end', create_expression, -127, 128);
      test_real_non_constant_expected ('var r: real; begin -r end', create_expression);

      test_integer_non_constant_expected ('function i: int8; begin end; begin -i end', create_expression, -127, 128);
      test_real_non_constant_expected ('function r: real; begin end; begin -r end', create_expression);
      
      test_integer_non_constant_expected ('function i: int8; begin end; begin -(i) end', create_expression, -127, 128);
      test_real_non_constant_expected ('function r: real; begin end; begin -(r) end', create_expression);
      
   end;
   
procedure test_term_constant_expressions 
   (create_expression: TTestGenerator
   );
   begin
      test_integer_constant_expected('begin 2 * 3 end', create_expression, 2*3);
      test_integer_constant_expected('begin 27 div 3 end', create_expression, 27 div 3);
      test_integer_constant_expected('begin 27 mod 7 end', create_expression, 27 mod 7);
      test_integer_constant_expected('var a:3..3 := 3; begin a end', create_expression, 3);
      test_integer_constant_expected('var i: int8; begin 7 * 3 * i * 0 end', create_expression, 0);
      test_real_constant_expected ('begin 2.3 * 2 end', create_expression, 2.3 * 2);
      test_real_constant_expected ('begin 2 / 2.3 end', create_expression, 2 / 2.3);
      test_integer_constant_expected ('var r: real; begin r * 3.2 * 0 * 5.2 end', create_expression, 0);
      test_set_constant_expected ('begin [1..5] * [3..7] end', create_expression, [1..5] * [3..7], ordinal_base_is_integer, '', 3, 5);
      test_boolean_constant_expected ('begin true and true end', create_expression, true);
      test_boolean_constant_expected ('var b: boolean; begin b and false end', create_expression, false);
      test_boolean_constant_expected ('var b: boolean; begin false and b and true end', create_expression, false);
      test_boolean_constant_expected ('var b: true..true := true; begin true and b and true end', create_expression, true);
      test_boolean_constant_expected ('var b: false..false; begin true and b and true end', create_expression, false);
   end;
   
procedure test_simple_expression_constant_expressions 
   (create_expression: TTestGenerator
   );
   begin
      test_integer_constant_expected ('begin 2 + 3 end', create_expression, 2+3);
      test_integer_constant_expected ('begin 2 - 3 end', create_expression, 2-3);
      test_real_constant_expected ('begin 2.3 + 4.5 - 1.2 end', create_expression, 2.3 + 4.5 - 1.2);
      test_set_constant_expected ('begin [1..5] + [2..10] - [3] end', create_expression, [1..5] + [2..10] - [3], ordinal_base_is_integer, '', 1, 10);
      test_boolean_constant_expected ('begin false or true or false end', create_expression, true);
      test_boolean_constant_expected ('var b: boolean; begin b or true end', create_expression, true);
      test_string_constant_expected ('begin ''asdf'' + ''qwerty'' end', create_expression, 'asdfqwerty');
      test_string_constant_expected ('begin ''a'' + ''b'' + ''cde'' + ''f'' end', create_expression, 'abcdef');
   end;
   
   
   //==============
   //  Test suites
   
procedure test_TRelationalExpression;
   begin
      display ('=============================');
      display ('TESTING TRelationalExpression');
      display ('=============================');
      test_primary_constant_expressions (create_TExpression);
      test_factor_constant_expressions (create_TExpression);
      test_term_constant_expressions (create_TExpression);
      test_simple_expression_constant_expressions (create_TExpression);
      
      test_only_for_successful_compilation ('procedure p; var a: int8; begin a := 5 + a - 5 + a end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: real; begin a := 3.1 + a - 56 end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: boolean; begin a := a or true or false end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: set of 1..9; begin a := a + [3..4] - a end; begin end.');
      test_only_for_successful_compilation ('procedure p; type en=(a,b,c); var s: set of en; begin s := [] + [b..c] - [] end; begin end.');
      test_only_for_successful_compilation ('procedure p; var r: record a,b: int8 end; begin r := r end; begin end.');
      test_only_for_successful_compilation ('type t = record a,b: int8 end; procedure p; var r: t; begin r := r end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: array[1..10] of int8; begin a := a end; begin end.');
      test_only_for_successful_compilation ('type ta = array[1..10] of int8; procedure p; var a: array [3..6] of ta; begin a[3] := a[6] end; begin end.');
      
      // record relational operators
      test_only_for_successful_compilation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 = r2 end.');
      test_only_for_successful_compilation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 <> r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 > r2 end.',
      err_invalid_operator_for_operand,
      '> r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 < r2 end.',
      err_invalid_operator_for_operand,
      '< r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 >= r2 end.',
      err_invalid_operator_for_operand,
      '>= r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 <= r2 end.',
      err_invalid_operator_for_operand,
      '<= r2 end.');
      
      // packed record relational operators
      test_only_for_successful_compilation ('var r1,r2: packed record i: int8 end; b: boolean; begin b := r1 = r2 end.');
      test_only_for_successful_compilation ('var r1,r2: packed record i: int8 end; b: boolean; begin b := r1 <> r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 > r2 end.',
      err_invalid_operator_for_operand,
      '> r2 end.');
      test_compile_error_generation ('var r1,r2: packed record i: int8 end; b: boolean; begin b := r1 < r2 end.',
      err_invalid_operator_for_operand,
      '< r2 end.');
      test_compile_error_generation ('var r1,r2: packed record i: int8 end; b: boolean; begin b := r1 >= r2 end.',
      err_invalid_operator_for_operand,
      '>= r2 end.');
      test_compile_error_generation ('var r1,r2: packed record i: int8 end; b: boolean; begin b := r1 <= r2 end.',
      err_invalid_operator_for_operand,
      '<= r2 end.');
      
      // array relational operators
      test_only_for_successful_compilation ('var r1,r2: array [1..3] of int8; b: boolean; begin b := r1 = r2 end.');
      test_only_for_successful_compilation ('var r1,r2: array [1..3] of int8; b: boolean; begin b := r1 <> r2 end.');
      test_compile_error_generation ('var r1,r2: record i: int8 end; b: boolean; begin b := r1 > r2 end.',
      err_invalid_operator_for_operand,
      '> r2 end.');
      test_compile_error_generation ('var r1,r2: array [1..3] of int8; b: boolean; begin b := r1 < r2 end.',
      err_invalid_operator_for_operand,
      '< r2 end.');
      test_compile_error_generation ('var r1,r2: array [1..3] of int8; b: boolean; begin b := r1 >= r2 end.',
      err_invalid_operator_for_operand,
      '>= r2 end.');
      test_compile_error_generation ('var r1,r2: array [1..3] of int8; b: boolean; begin b := r1 <= r2 end.',
      err_invalid_operator_for_operand,
      '<= r2 end.');
      
      display ('')
   end;
   
procedure test_TSimpleExpression;
   var
      s: string;
      expr: TExpression;
   procedure setup;
      begin
         expr := setup_test(s, create_TSimpleExpression)
      end;
   procedure takedown;
      begin
         takedown_test (expr, s)
      end;
   begin
      display ('=========================');
      display ('TESTING TSimpleExpression');
      display ('=========================');
      test_primary_constant_expressions (create_TSimpleExpression);
      test_factor_constant_expressions (create_TSimpleExpression);
      test_term_constant_expressions (create_TSimpleExpression);
      test_simple_expression_constant_expressions (create_TSimpleExpression);
      
      
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true + false end; begin end.', err_invalid_operator_for_operand, '+ false end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true - false end; begin end.', err_invalid_operator_for_operand, '- false end; begin end.');
      test_compile_error_generation ('procedure p; var b: int8; begin b := 5 or false end; begin end.', err_invalid_operator_for_operand, 'or false end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 + false end; begin end.', err_incompatible_operand_types, '+ false end; begin end.');
      test_compile_error_generation ('procedure p; var r: real; begin r := 5.1 + false end; begin end.', err_incompatible_operand_types, '+ false end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true or 5 end; begin end.', err_incompatible_operand_types, 'or 5 end; begin end.');
      test_compile_error_generation ('procedure p; var s: set of 1..5; begin s := [3..4] + true end; begin end.', err_incompatible_operand_types, '+ true end; begin end.');
      test_compile_error_generation ('procedure p; var s: set of 1..5; begin s := [] + [3..4] + [] + [true] end; begin end.', err_incompatible_operand_types, '+ [true] end; begin end.');
      test_compile_error_generation ('type en1=(a,b,c); en2=(d,e,f); procedure p; var s: set of en1; begin s := [a] + [d] end; begin end.', err_incompatible_operand_types, '+ [d] end; begin end.');
      
      s := 'var i: int8; r: real; begin 1 + 5.1 - i + r + 3.2 end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if Length(TSimpleExpression(expr).additional_terms) <> 2 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 3 terms')
         end;
      takedown;
      
      s := 'var i: int8; r: real; begin 1.2 + 5.1 - i + r + 3.2 end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
      if Length(TSimpleExpression(expr).additional_terms) <> 2 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 3 terms')
         end;
      takedown;
      
      // test combine where first is int const
      s := 'var i: int8; begin 1 + i + 3 -i - 2 + 8 end';
      test_integer_non_constant_expected (s, create_TSimpleExpression, -245, 265);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
         begin
            if Length(TSimpleExpression(expr).additional_terms) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have 3 terms')
               end;
            if not TSimpleExpression(expr).first_term.contains_integer_constant then
               begin
                  record_bad_test_result;
                  display (s + ': failed, first_term should be integer constant')
               end
            else if TSimpleExpression(expr).first_term.constant.ordinal_value.AsInteger <> 10 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, first_term should evaluate to 10')
               end
         end;
      takedown;
      
      // test combine where total term is consts and eval to 0
      s := 'begin 1 + 1 - 2 end';
      test_integer_constant_expected (s, create_TSimpleExpression, 0);
      setup;
      if (expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be se')
         end;
      takedown;
      
      // test combine where first is int const and combined terms eval to 0
      s := 'var i: int8; begin 1 + i + 3 -i - 2 - 2 end';
      test_integer_non_constant_expected (s, create_TSimpleExpression, -255, 255);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
      if Length(TSimpleExpression(expr).additional_terms) <> 1 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 2 terms')
         end;
      takedown;
      
      // test combine where first is not int const
      s := 'var i: int8; begin i + 1 + 3 -i - 2 + 8 end';
      test_integer_non_constant_expected (s, create_TSimpleExpression, -245, 265);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
         begin
            if Length(TSimpleExpression(expr).additional_terms) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have 3 terms')
               end;
            if not TSimpleExpression(expr).additional_terms[0].right_term.contains_integer_constant then
               begin
                  record_bad_test_result;
                  display (s + ': failed, additional_terms[0] should be integer constant')
               end
            else if TSimpleExpression(expr).additional_terms[0].right_term.constant.ordinal_value.AsInteger <> 10 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, additional_terms[0] should evaluate to 10')
               end
         end;
      takedown;
      
      // test combine where first is not int const, but int consts eval to 0
      s := 'var i: int8; begin i + 1 + 3 -i - 2 - 2 end';
      test_integer_non_constant_expected (s, create_TSimpleExpression, -255, 255);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
      if Length(TSimpleExpression(expr).additional_terms) <> 1 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 2 terms')
         end;
      takedown;
      
      s := 'var i: int8; r: real; begin 1 - r - 5.8 end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
         begin
            if Length(TSimpleExpression(expr).additional_terms) <> 1 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have 2 terms')
               end;
            if (not TSimpleExpression(expr).first_term.contains_real_constant) or (not approximately_equal(TSimpleExpression(expr).first_term.constant.r, -4.8)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, first term should be -4.8')
               end
         end;
      takedown;
      
      s := 'var i,j: int8; r: real; begin i + 5 - 6.2 + r - 5.1 end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
         begin
            if Length(TSimpleExpression(expr).additional_terms) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have 3 terms')
               end;
            if (not TSimpleExpression(expr).additional_terms[0].right_term.contains_real_constant) or (not approximately_equal(TSimpleExpression(expr).additional_terms[0].right_term.constant.r, -6.3)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, first term should be -6.3')
               end
         end;
      takedown;
      
      // int term at front, consts should eval to 0 and be eliminated
      s := 'var r: real; begin 5 + r - 5.0 end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if (expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be se')
         end;
      takedown;
      
      // int term later, consts should eval to 0 and be eliminated
      s := 'var i,j: int8; r: real; begin i + 5 - 5.0 + r end';
      test_real_non_constant_expected (s, create_TSimpleExpression);
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
      if Length(TSimpleExpression(expr).additional_terms) <> 1 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 2 terms')
         end;
      takedown;
      
      // test boolean combination - removal of false constants
      s := 'var b: boolean; begin false or false or b or false or false end';
      test_boolean_non_constant_expected (s, create_TSimpleExpression, false, true);
      setup;
      if (expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be se')
         end;
      takedown;

      s := 'var s: set of 3..10; begin s - [7..12] end';
      test_set_non_constant_expected (s, create_TSimpleExpression, 3, 6, ordinal_base_is_integer, '');
      
      s := 'var s: set of 3..10; begin s + [7..12] end';
      test_set_non_constant_expected (s, create_TSimpleExpression, 3, 12, ordinal_base_is_integer, '');
      
      s := 'var s: set of 3..10; begin s + [7..12] + [15] end';
      test_set_non_constant_expected (s, create_TSimpleExpression, 3, 15, ordinal_base_is_integer, '');
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
         if Length(TSimpleExpression(expr).additional_terms) <> 1 then
            begin
               record_bad_test_result;
               display (s + ': failed, should have 2 terms')
            end;
      takedown;
      
      s := 'var s: set of 3..10; begin [7..12] + [15] + s end';
      test_set_non_constant_expected (s, create_TSimpleExpression, 3, 15, ordinal_base_is_integer, '');
      setup;
      if not(expr is TSimpleExpression) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be se')
         end
      else
      if Length(TSimpleExpression(expr).additional_terms) <> 1 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have 2 terms')
         end;
      takedown;
      
      display ('')
   end;
   
procedure test_TTerm;
   var
      s: string;
      expr: TExpression;
   procedure setup;
      begin
         expr := setup_test(s, create_TTerm)
      end;

   procedure takedown;
      begin
         takedown_test (expr, s)
      end;

   procedure test_mod_ranges;
      function subr (blow, bhi: integer): string;
         begin
            result := IntToStr(blow) + '..' + IntToStr(bhi);
            if blow > 0 then
               result := result + ':=' + IntToStr(blow)
            else
               if bhi < 0 then
                  result := result + ':=' + IntToStr(bhi)
         end;
      procedure test_range (llow, lhi, rlow, rhi: integer);
         var
            low, hi, i, j, res: integer;
         begin
            assert (llow <= lhi);
            assert (rlow <= rhi);
            low := 9999;
            hi := -9999;
            for i := llow to lhi do
               for j := rlow to rhi do
                  if j <> 0 then
                     begin
                        res := i mod j;
                        low := min (low, res);
                        hi := max (hi, res)
                     end;
            s := 'var left: ' + subr(llow, lhi) + '; right:' + subr(rlow, rhi) + '; begin left mod right end.';
            setup;
            if expr.info.min_value.gt(low) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, min_value can be ' + IntToStr(low) + ', was ' + IntToStr(expr.info.min_value.AsInteger))
               end;
            if expr.info.max_value.lt(hi) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, max_value can be ' + IntToStr(hi) + ', was ' + IntToStr(expr.info.max_value.AsInteger))
               end;
            takedown
         end;
      const
         lim=3;
      var
         llo, lhi, rlo, rhi: integer;
      begin
         TTestCPU(target_cpu).set_mod_operator_implementation (delphi_mod_operator_implemenation);
         for llo := -lim to lim do
            for lhi := llo to lim do
               for rlo := -lim to lim do
                  for rhi := rlo to lim do
                     if not ((rlo=0) and (rhi=0)) then
                        test_range (llo, lhi, rlo, rhi);
      end;

   begin
      display ('=============');
      display ('TESTING TTerm');
      display ('=============');

      test_mod_ranges;

      test_primary_constant_expressions (create_TTerm);
      test_factor_constant_expressions (create_TTerm);
      test_term_constant_expressions (create_TTerm);
      
      test_only_for_successful_compilation ('procedure p; var a: int8; begin a := 5 * a div 5 mod a end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: real; begin a := 3 * a / 56 end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: boolean; begin a := a and true and false end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: set of 1..9; begin a := a * [3..4] * a end; begin end.');
      test_only_for_successful_compilation ('procedure p; type en=(a,b,c); var s: set of en; begin s := [] * [b..c] * [] end; begin end.');
      
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true * false end; begin end.', err_invalid_operator_for_operand, '* false end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true / false end; begin end.', err_invalid_operator_for_operand, '/ false end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true div false end; begin end.', err_invalid_operator_for_operand, 'div false end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true mod false end; begin end.', err_invalid_operator_for_operand, 'mod false end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 and 7 end; begin end.', err_invalid_operator_for_operand, 'and 7 end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 * true end; begin end.', err_incompatible_operand_types, '* true end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 div 5.3 end; begin end.', err_real_divisor_cant_be_used_with_div_or_mod, '5.3 end; begin end.');
      test_compile_error_generation ('procedure p; var r: real; begin r := 5.3 * true end; begin end.', err_incompatible_operand_types, 'true end; begin end.');
      test_compile_error_generation ('procedure p; var b: boolean; begin b := true and 5 end; begin end.', err_incompatible_operand_types, '5 end; begin end.');
      test_compile_error_generation ('procedure p; var s: set of 2..5; begin s := [2..3] * true end; begin end.', err_incompatible_operand_types, 'true end; begin end.');
      test_compile_error_generation ('procedure p; type enum=(a,b,c); var s: set of 2..5; e: set of enum; begin s := [2..3] * e end; begin end.', err_incompatible_operand_types, 'e end; begin end.');
      test_compile_error_generation ('procedure p; type enum1=(a,b,c); enum2=(d,e,f); var en: set of enum1; begin en := [a..c] * [d] end; begin end.', err_incompatible_operand_types, '[d] end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 div 0 end; begin end.', err_division_by_zero, '0 end; begin end.');
      test_compile_error_generation ('procedure p; var i: int8; begin i := 5 mod 0 end; begin end.', err_division_by_zero, '0 end; begin end.');
      test_compile_error_generation ('procedure p; var r: real; begin r := 5.0 / 0 end; begin end.', err_division_by_zero, '0 end; begin end.');
      test_compile_error_generation ('procedure p; var r: real; begin r := r * 5.0 / 0 end; begin end.', err_division_by_zero, '0 end; begin end.');
      test_compile_error_generation ('procedure p; var r: real; begin r := r / 0 end; begin end.', err_division_by_zero, '0 end; begin end.');
      TTestCPU(target_cpu).set_mod_operator_implementation (iso_pascal_mod_operator_implementation);
      test_compile_error_generation ('procedure p; var i: -7..-1 := -1; j: int8; begin j := 5 mod i end; begin end.', err_divisor_will_never_be_ge_1, 'i end; begin end.');

      test_integer_non_constant_expected ('var a: 2..5 := 2; begin a*a end', create_TTerm, 4, 25);
      test_integer_non_constant_expected ('var a: 2..5 := 3; begin a*a*a end', create_TTerm, 8, 125);
      test_integer_non_constant_expected ('var a: 2..5 := 4; begin a*a*-1 end', create_TTerm, -25, -4);
      TTestCPU(target_cpu).set_mod_operator_implementation (iso_pascal_mod_operator_implementation);
      test_integer_non_constant_expected ('var a: 2..5 := 5; begin a mod 7 end', create_TTerm, 0, 5);
      
      // test combine_boolean_constants
      s := 'var a,b,c: boolean; begin a and true and b and true and c end.';
      test_boolean_non_constant_expected (s, create_TTerm, false, true);
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
      if Length(TTerm(expr).additional_factors) <> 2 then
         begin
            record_bad_test_result;
            display (s + ': failed, should have removed two factors')
         end;
      takedown;
      
      s := 'var c: boolean; b,a: true..true := true; begin a and true and b and true and c end.';
      test_boolean_non_constant_expected (s, create_TTerm, false, true);
      setup;
      if expr is TTerm then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be TTerm')
         end;
      takedown;
      
      // test combine_set_constants
      s := 'type en=(a,b,c,d,e,f,g,h);var s: set of en; begin [a,b,c]*[c,d,e]*[e,f,g]*s end';
      test_set_constant_expected (s, create_TTerm, [], ordinal_base_is_enum, 'en', maxint, -maxint);
      setup;
      if expr is TTerm then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be TTerm')
         end;
      takedown;
      
      s := 'type en=(a,b,c,d,e,f,g,h);var s: set of en; begin s*[a..c]*[b..e]*[c..g]*s end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have removed two factors')
               end;
            if (not TTerm(expr).additional_factors[0].factor.contains_constant)
            or
            (TTerm(expr).additional_factors[0].factor.constant.sett <> [2]) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be [2]')
               end
         end;
      takedown;
      
      s := 'type en=(a,b,c,d,e,f,g,h);var s: set of en; begin [a..c]*[b..e]*[c..g]*s end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 1 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have removed two factors')
               end;
            if (not TTerm(expr).first_factor.contains_constant)
            or
            (TTerm(expr).first_factor.constant.sett <> [2]) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be [2]')
               end
         end;
      takedown;
      
      // test simplify_numeric_expression
      s := 'var r: real; begin 5 * 6 mod 70 div 3 * r end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 1 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have removed simplified leading int constants')
               end;
            if (not TTerm(expr).first_factor.contains_real_constant)
            or
            (TTerm(expr).first_factor.constant.r <> (5 * 6 mod 70 div 3)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 5 * 6 mod 70 div 3')
               end
         end;
      takedown;
      
      // test combination of leading real constants
      s := 'var r: real; begin 1.2 * 4.5 / 5.6 * r end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 1 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have removed simplified leading int constants')
               end;
            if (not TTerm(expr).first_factor.contains_real_constant)
            or
            (round (10000 * TTerm(expr).first_factor.constant.r) <> round(10000 * 1.2 * 4.5 / 5.6)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 1.2 * 4.5 / 5.6')
               end
         end;
      takedown;
      
      // test combination of leading integer constants evaluating to 1.0
      s := 'var r: real; begin 37 / 37 * r end';
      setup;
      if expr is TTerm then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be TTerm')
         end;
      takedown;
      
      // test combination of real constnats
      s := 'var r: real; begin 5.0 * r * 6 / 5 * r end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have combined real constants')
               end;
            if (not TTerm(expr).first_factor.contains_real_constant)
            or
            (round (10000 * TTerm(expr).first_factor.constant.r) <> round(10000 * 6)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 6')
               end
         end;
      takedown;
      
      // test combination of real constnats
      s := 'var r: real; begin r / 5.0 * r * 6 * r end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 3 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should have combined real constants')
               end;
            if (not TTerm(expr).additional_factors[0].factor.contains_real_constant)
            or
            (round (10000 * TTerm(expr).additional_factors[0].factor.constant.r) <> round(10000 * 6/5)) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 6/5')
               end
         end;
      takedown;
      
      // test combination of real constnats evaluating to 0
      s := 'var r: real; begin r / 5.0 * r * 0 * r end';
      setup;
      if expr is TTerm then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be TTerm')
         end;
      if (not expr.contains_integer_constant)
         or
         (expr.constant.ordinal_value.AsInteger <> 0) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be 0')
         end;
      takedown;
      
      // test combination of real constnats evaluating to 1
      s := 'var r: real; begin r / 5.0 * r * 5.0 * r end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
      if Length(TTerm(expr).additional_factors) <> 2 then
         begin
            record_bad_test_result;
            display (s + ': failed, should simplified to 2')
         end;
      takedown;
      
      s := 'var i: int8; begin 5 * i * 6 end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 1 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should simplified to 2')
               end;
            if (not TTerm(expr).first_factor.contains_integer_constant)
               or
               (TTerm(expr).first_factor.constant.ordinal_value.AsInteger <> 30) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 30')
               end
         end;
      takedown;
      
      s := 'var i: int8; begin i * 5 * i * 6 end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 2 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should simplified to 3')
               end;
            if (not TTerm(expr).additional_factors[0].factor.contains_integer_constant)
               or
               (TTerm(expr).additional_factors[0].factor.constant.ordinal_value.AsInteger <> 30) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 30')
               end
         end;
      takedown;
      
      s := 'var i: int8; begin i * 5 * i * 6 mod 7 * 3 * i * 4 end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 5 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should simplified to 6')
               end;
            if (not TTerm(expr).additional_factors[0].factor.contains_integer_constant)
               or
               (TTerm(expr).additional_factors[0].factor.constant.ordinal_value.AsInteger <> 30) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 30')
               end;
            if (not TTerm(expr).additional_factors[3].factor.contains_integer_constant)
               or
               (TTerm(expr).additional_factors[3].factor.constant.ordinal_value.AsInteger <> 12) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 12')
               end
         end;
      takedown;
      
      s := 'var i: int8; begin i * 1 * i * 1 mod 7 * 3 * i * 4 end';
      setup;
      if not (expr is TTerm) then
         begin
            record_bad_test_result;
            display (s + ': failed, should be TTerm')
         end
      else
         begin
            if Length(TTerm(expr).additional_factors) <> 4 then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should simplified to 5')
               end;
            if (not TTerm(expr).additional_factors[2].factor.contains_integer_constant)
               or
               (TTerm(expr).additional_factors[2].factor.constant.ordinal_value.AsInteger <> 12) then
               begin
                  record_bad_test_result;
                  display (s + ': failed, should be 12')
               end
         end;
      takedown;
      
      s := 'var i: int8; begin 3 div 3 * i end';
      setup;
      if expr is TTerm then
         begin
            record_bad_test_result;
            display (s + ': failed, should not be TTerm')
         end;
      takedown;

      s := 'var i: uint8; begin i mod 8 end';
      setup;
      if TTerm(expr).additional_factors[0].mulop <> mulop_mask then
         begin
            record_bad_test_result;
            display (s + ': failed, should be mask operator')
         end;
      takedown;

      s := 'var i: int8; begin i div 8 end';
      setup;
      if TTerm(expr).additional_factors[0].mulop <> mulop_shr then
         begin
            record_bad_test_result;
            display (s + ': failed, should be shr operator')
         end;
      takedown;

      s := 'var i: uint8; begin i * 8 end';
      setup;
      if TTerm(expr).additional_factors[0].mulop <> mulop_shl then
         begin
            record_bad_test_result;
            display (s + ': failed, should be shl operator')
         end;
      takedown;

      test_mod_ranges;
      display ('')
   end;

procedure test_lex_analysis;
   begin
      display ('=====================');
      display ('TESTING Lex Analsysis');
      display ('=====================');
      test_integer_constant_expected('begin ord($3_aB) end', create_TPrimary, 939);
      test_compile_error_generation ('begin $', err_improper_hex_constant, '$');
      test_compile_error_generation ('begin $x', err_improper_hex_constant, '$x');
      test_compile_error_generation ('begin #', err_invalid_char, '#');
      test_real_constant_expected ('begin 4.2e2 end', create_TPrimary, 4.2e2);
      test_real_constant_expected ('begin 4.2E2 end', create_TPrimary, 4.2E2);
      test_real_constant_expected ('begin 4.2e+2 end', create_TPrimary, 4.2e2);
      test_real_constant_expected ('begin 4.2e-2 end', create_TPrimary, 4.2e-2);
      test_compile_error_generation ('begin 4.2e+', err_improper_real_const, '4.2e+');
      test_compile_error_generation ('begin 4.2e-', err_improper_real_const, '4.2e-');
      test_compile_error_generation ('begin 4.2ex', err_improper_real_const, '4.2ex');
      test_compile_error_generation ('begin 1.x', err_improper_real_const, '1.x');
      test_compile_error_generation ('begin ''', err_string_constant_multi_line, '''');
      test_compile_error_generation ('begin ''xxx', err_string_constant_multi_line, '''xxx');
      test_compile_error_generation ('begin 1e', err_improper_real_const, '1e');
      display ('')
   end;

procedure test_TFactor;
   begin
      display ('===============');
      display ('TESTING TFactor');
      display ('===============');
      test_primary_constant_expressions (create_TFactor);
      test_factor_constant_expressions (create_TFactor);
      display ('')
   end;
   
procedure test_TPrimary;
   begin
      display ('================');
      display ('TESTING TPrimary');
      display ('================');
      test_primary_constant_expressions (create_TPrimary);
      test_only_for_successful_compilation ('procedure p; var c: char; i : int8; begin i := ord(c) end; begin end.');
      test_only_for_successful_compilation ('procedure p; var e: (a,b,c); i : int8; begin i := ord(c) end; begin end.');
      test_only_for_successful_compilation ('procedure p; var i : int8; begin i := ord(i) end; begin end.');
      test_only_for_successful_compilation ('procedure p; var b: boolean; i : int8; begin i := ord(b) end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: int8; begin a := a end; begin end.');
      test_only_for_successful_compilation ('procedure p; var a: int8; begin a := a end; begin end.');
      test_only_for_successful_compilation ('procedure p; var s: string[4]; begin s := ''asdf'' end; begin end.');
      test_only_for_successful_compilation ('function f: int8; begin end; procedure p; var v: int8; begin v := f end; begin end.');
      test_only_for_successful_compilation ('procedure p; var i: int8; r: record j: int8 end; begin i := r.j end; begin end.');
      test_only_for_successful_compilation ('procedure p; var i: int8; r: record j: int8 end; begin with r do i := j end; begin end.');
      test_only_for_successful_compilation ('type tc=class public function f:int8; begin end; begin end; var i: int8; c: tc; begin i := c.f end.');
      test_only_for_successful_compilation ('type tc=class public function f:int8; begin end; begin end; var i: int8; c: tc; begin with c do i := f end.');
      test_only_for_successful_compilation ('type tc=class public property prop: int8; get: begin end; begin end; var i: int8; c: tc; begin i := c.prop end.');
      test_only_for_successful_compilation ('procedure p; var r: real; i: int8; begin i := round (r) end; begin end.');
      test_only_for_successful_compilation ('procedure p; var r: real; i: int8; begin i := trunc (r) end; begin end.');
      test_only_for_successful_compilation ('type t=record a: int8; b: int16 end; const a:t = (a=5,b=6); procedure p; var i: int8; begin i := a.a end; begin end.');
      test_only_for_successful_compilation ('type tm=monitor var q: queue; public procedure p; begin end; begin if empty(q) then end; begin end.');
      test_only_for_successful_compilation ('var i: int8; begin i := abs (i) end.');
      test_only_for_successful_compilation ('var r: real; begin r := abs (r) end.');
      test_only_for_successful_compilation ('var i: int8; begin i := abs (-4) end.');
      test_only_for_successful_compilation ('var r: real; begin r := abs (-4.2) end.');

      test_only_for_successful_compilation ('var i: int8; begin i := succ (i) end.');
      test_only_for_successful_compilation ('var b: boolean; begin b := succ (b) end.');
      test_only_for_successful_compilation ('var c: char; begin c := succ (c) end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec); var e:te; begin e := succ (e) end.');
      test_only_for_successful_compilation ('var i: int8; begin i := pred (i) end.');
      test_only_for_successful_compilation ('var b: boolean; begin b := pred (b) end.');
      test_only_for_successful_compilation ('var c: char; begin c := pred (c) end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec); var e:te; begin e := pred (e) end.');
      
      test_compile_error_generation ('type tc=class public property prop: int8; begin end; var i: int8; c: tc; begin i := c.prop end.', err_write_only_property, 'prop end.');
      test_compile_error_generation ('type tc=class public procedure proc; begin end; begin end; var i: int8; c: tc; begin i := c.proc end.', err_procedure_has_no_result, 'proc end.');
      test_compile_error_generation ('procedure p; var s: string[1]; begin s := ''asdf'' end; begin end.', err_string_exceeds_max_length, '''asdf'' end; begin end.');
      test_compile_error_generation ('procedure p; var i:int8; begin i := round(true) end; begin end.', err_numeric_expression_expected, 'true) end; begin end.');
      test_compile_error_generation ('procedure p; var i:int8; begin i := trunc(true) end; begin end.', err_numeric_expression_expected, 'true) end; begin end.');

      test_compile_error_generation ('var i: int8; begin i := pred end.', err_left_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := succ end.',err_left_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := abs end.', err_left_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := succ (i end.', err_right_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := pred (i end.', err_right_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := abs (i end.', err_right_parenthesis_expected, 'end.');
      test_compile_error_generation ('var i: int8; begin i := succ (4.5) end.', err_ordinal_expression_expected, '4.5) end.');
      test_compile_error_generation ('var i: int8; begin i := pred (4.5) end.', err_ordinal_expression_expected, '4.5) end.');
      test_compile_error_generation ('type e=(a,b,c); var x: e; begin x := succ(c) end.', err_result_will_be_out_of_range, 'succ(c) end.');
      test_compile_error_generation ('type e=(a,b,c); var x: e; begin x := pred(a) end.', err_result_will_be_out_of_range, 'pred(a) end.');
      test_compile_error_generation ('var b: boolean; begin b:=succ(true) end.', err_result_will_be_out_of_range, 'succ(true) end.');
      test_compile_error_generation ('var b: boolean; begin b := pred(false) end.', err_result_will_be_out_of_range, 'pred(false) end.');
      test_compile_error_generation ('var c: char; begin c := succ(chr(255)) end.', err_result_will_be_out_of_range, 'succ(chr(255)) end.');
      test_compile_error_generation ('var c: char; begin c := pred(chr(0)) end.', err_result_will_be_out_of_range, 'pred(chr(0)) end.');
      display ('')
   end;
   
end.

