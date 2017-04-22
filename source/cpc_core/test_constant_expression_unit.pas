unit test_constant_expression_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure test_constant_syntax_unit;
procedure test_data_syntax_unit;

implementation

uses
   cpc_common_unit,
   cpc_constant_expression_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit,
   cpc_target_cpu_unit,
   Math,
   SysUtils,
   test_subroutines_unit,
   test_type_syntax_unit;

function create_cexpression: TDefinition;
   begin
      result := TCExpression.CreateFromSourceTokens
   end;

procedure test_cexpression;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var c: TCexpression;
      begin
         c := TCExpression(test_fragment(s, create_cexpression));
         if (c = nil) or (c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var c: TCexpression;
      begin
         c := TCExpression(test_fragment(s, create_cexpression));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_string_expected
      (s: string;
       expected_result: string
      );
      var c: TCexpression;
      begin
         c := TCExpression(test_fragment(s, create_cexpression));
         if (c = nil) or(c.constant_kind <> string_constant) or (c.s <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_boolean_expected
      (s: string;
       expected_result: boolean
      );
      var c: TCexpression;
      begin
         c := TCExpression(test_fragment(s, create_cexpression));
         if (c = nil) or (c.constant_kind <> boolean_constant) or (c.b <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_set_expected
      (s: string;
       expected_set: TSet256;
       expected_ordinal_base_type: TOrdinalBaseType;
       expected_set_base_enum_typedef:
       string
      );
      var
         c: TCExpression;
      begin
         c := TCExpression(test_fragment(s, create_cexpression));
         if (c = nil) or (c.constant_kind <> set_constant) or (c.sett <> expected_set) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end
         else
            begin
               if c.set_ordinal_base_type <> expected_ordinal_base_type then
                  begin
                     record_bad_test_result;
                     display(s + ': failed, set base type wrong')
                  end;
//               if (expected_ordinal_base_type = ordinal_base_is_enum)
//                  and
//                  (c.enum_typedef <> CurrentDefinitionTable.GetDefinitionForIdentifier(expected_set_base_enum_typedef)) then
//                  begin
//                     record_bad_test_result;
//                     display(s + ': failed, set base type wrong')
//                  end
            end;
         c.Release
      end;
   type
      x =
         (a,
          b,
          c
         );
   begin
      display('testing TCExpression');
      test_integer_expected('begin 3+4 end', 3 + 4);
      test_real_expected('begin 3.14+2 end', 3.14 + 2);
      test_real_expected('begin 2+3.14 end', 3.14 + 2);
      test_real_expected('begin 3.14+2.1 end', 3.14 + 2.1);

      test_integer_expected('begin 3-4 end', 3 - 4);
      test_real_expected('begin 3.14-2 end', 3.14 - 2);
      test_real_expected('begin 2-3.14 end', 2 - 3.14);
      test_real_expected('begin 3.14-2.1 end', 3.14 - 2.1);

      test_integer_expected('begin 1+2+3 end', 6);
      test_integer_expected('begin 1-2-3 end', -4);
      test_real_expected('begin -1-2.0+3 end', 0);

      test_boolean_expected('begin 1 in [1,2] end', true);
      test_boolean_expected('begin 1 in [] end', false);
      test_boolean_expected('begin true in [] end', false);
      test_boolean_expected('begin ''x'' in [] end', false);
      test_boolean_expected('type x=(a,b,c);begin b in [] end', false);
      test_boolean_expected('begin ''a'' in [''b'',''c''] end', false);
      test_boolean_expected('type x=(a,b,c); begin c in [b,c] end', true);
      test_boolean_expected('begin true in [false] end', false);
      test_compile_error_generation_for_program_fragment('begin 4.3 in [1,2] end', create_cexpression, err_left_element_must_be_ordinal, '4.3 in [1,2] end');
      test_compile_error_generation_for_program_fragment('begin 5 in 6 end', create_cexpression, err_right_operand_must_be_a_set, '6 end');
      test_compile_error_generation_for_program_fragment('begin 2 in [true] end', create_cexpression, err_left_and_right_operand_types_dont_agree, 'in [true] end');
      test_compile_error_generation_for_program_fragment('begin '''' in [''a'',''b''] end', create_cexpression, err_left_and_right_operand_types_dont_agree, 'in [''a'',''b''] end');
      test_compile_error_generation_for_program_fragment('begin ''abc'' in [''a'', ''b'', ''c''] end', create_cexpression, err_left_and_right_operand_types_dont_agree, 'in [''a'', ''b'', ''c''] end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f);begin a in [d,e] end', create_cexpression, err_left_and_right_operand_types_dont_agree, 'in [d,e] end');

      test_boolean_expected('type x=(a,b,c); begin [] = [] end', [] = []);
      test_boolean_expected('type x=(a,b,c); begin [] = [c] end', [] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [] = [b] end', [] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [] = [b,c] end', [] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] = [a] end', [] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [] = [a,c] end', [] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [] = [a,b] end', [] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [] = [a,b,c] end', [] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [] end', [c] = []);
      test_boolean_expected('type x=(a,b,c); begin [c] = [c] end', [c] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [b] end', [c] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [b,c] end', [c] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [a] end', [c] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [a,c] end', [c] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [a,b] end', [c] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [c] = [a,b,c] end', [c] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [] end', [b] = []);
      test_boolean_expected('type x=(a,b,c); begin [b] = [c] end', [b] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [b] end', [b] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [b,c] end', [b] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [a] end', [b] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [a,c] end', [b] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [a,b] end', [b] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b] = [a,b,c] end', [b] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [] end', [b, c] = []);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [c] end', [b, c] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [b] end', [b, c] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [b,c] end', [b, c] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [a] end', [b, c] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [a,c] end', [b, c] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [a,b] end', [b, c] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] = [a,b,c] end', [b, c] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [] end', [a] = []);
      test_boolean_expected('type x=(a,b,c); begin [a] = [c] end', [a] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [b] end', [a] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [b,c] end', [a] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [a] end', [a] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [a,c] end', [a] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [a,b] end', [a] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a] = [a,b,c] end', [a] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [] end', [a, c] = []);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [c] end', [a, c] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [b] end', [a, c] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [b,c] end', [a, c] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [a] end', [a, c] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [a,c] end', [a, c] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [a,b] end', [a, c] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] = [a,b,c] end', [a, c] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [] end', [a, b] = []);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [c] end', [a, b] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [b] end', [a, b] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [b,c] end', [a, b] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [a] end', [a, b] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [a,c] end', [a, b] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [a,b] end', [a, b] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] = [a,b,c] end', [a, b] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [] end', [a, b, c] = []);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [c] end', [a, b, c] = [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [b] end', [a, b, c] = [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [b,c] end', [a, b, c] = [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [a] end', [a, b, c] = [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [a,c] end', [a, b, c] = [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [a,b] end', [a, b, c] = [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] = [a,b,c] end', [a, b, c] = [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [] end', [] <> []);
      test_boolean_expected('type x=(a,b,c); begin [] <> [c] end', [] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [b] end', [] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [b,c] end', [] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [a] end', [] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [a,c] end', [] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [a,b] end', [] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [] <> [a,b,c] end', [] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [] end', [c] <> []);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [c] end', [c] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [b] end', [c] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [b,c] end', [c] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [a] end', [c] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [a,c] end', [c] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [a,b] end', [c] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [c] <> [a,b,c] end', [c] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [] end', [b] <> []);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [c] end', [b] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [b] end', [b] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [b,c] end', [b] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [a] end', [b] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [a,c] end', [b] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [a,b] end', [b] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b] <> [a,b,c] end', [b] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [] end', [b, c] <> []);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [c] end', [b, c] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [b] end', [b, c] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [b,c] end', [b, c] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [a] end', [b, c] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [a,c] end', [b, c] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [a,b] end', [b, c] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <> [a,b,c] end', [b, c] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [] end', [a] <> []);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [c] end', [a] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [b] end', [a] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [b,c] end', [a] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [a] end', [a] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [a,c] end', [a] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [a,b] end', [a] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a] <> [a,b,c] end', [a] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [] end', [a, c] <> []);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [c] end', [a, c] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [b] end', [a, c] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [b,c] end', [a, c] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [a] end', [a, c] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [a,c] end', [a, c] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [a,b] end', [a, c] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <> [a,b,c] end', [a, c] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [] end', [a, b] <> []);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [c] end', [a, b] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [b] end', [a, b] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [b,c] end', [a, b] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [a] end', [a, b] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [a,c] end', [a, b] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [a,b] end', [a, b] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <> [a,b,c] end', [a, b] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [] end', [a, b, c] <> []);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [c] end', [a, b, c] <> [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [b] end', [a, b, c] <> [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [b,c] end', [a, b, c] <> [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [a] end', [a, b, c] <> [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [a,c] end', [a, b, c] <> [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [a,b] end', [a, b, c] <> [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <> [a,b,c] end', [a, b, c] <> [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [] end', [] <= []);
      test_boolean_expected('type x=(a,b,c); begin [] <= [c] end', [] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [b] end', [] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [b,c] end', [] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [a] end', [] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [a,c] end', [] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [a,b] end', [] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [] <= [a,b,c] end', [] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [] end', [c] <= []);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [c] end', [c] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [b] end', [c] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [b,c] end', [c] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [a] end', [c] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [a,c] end', [c] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [a,b] end', [c] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [c] <= [a,b,c] end', [c] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [] end', [b] <= []);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [c] end', [b] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [b] end', [b] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [b,c] end', [b] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [a] end', [b] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [a,c] end', [b] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [a,b] end', [b] <= [a,  b]);
      test_boolean_expected('type x=(a,b,c); begin [b] <= [a,b,c] end', [b] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [] end', [b, c] <= []);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [c] end', [b, c] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [b] end', [b, c] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [b,c] end', [b, c] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [a] end', [b, c] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [a,c] end', [b, c] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [a,b] end', [b, c] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] <= [a,b,c] end', [b, c] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [] end', [a] <= []);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [c] end', [a] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [b] end', [a] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [b,c] end', [a] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [a] end', [a] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [a,c] end', [a] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [a,b] end', [a] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a] <= [a,b,c] end', [a] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [] end', [a, c] <= []);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [c] end', [a, c] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [b] end', [a, c] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [b,c] end', [a, c] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [a] end', [a, c] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [a,c] end', [a, c] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [a,b] end', [a, c] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] <= [a,b,c] end', [a, c] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [] end', [a, b] <= []);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [c] end', [a, b] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [b] end', [a, b] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [b,c] end', [a, b] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [a] end', [a, b] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [a,c] end', [a, b] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [a,b] end', [a, b] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] <= [a,b,c] end', [a, b] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [] end', [a, b, c] <= []);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [c] end', [a, b, c] <= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [b] end', [a, b, c] <= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [b,c] end', [a, b, c] <= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [a] end', [a, b, c] <= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [a,c] end', [a, b, c] <= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [a,b] end', [a, b, c] <= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] <= [a,b,c] end', [a, b, c] <= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [] end', [] >= []);
      test_boolean_expected('type x=(a,b,c); begin [] >= [c] end', [] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [b] end', [] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [b,c] end', [] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [a] end', [] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [a,c] end', [] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [a,b] end', [] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [] >= [a,b,c] end', [] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [] end', [c] >= []);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [c] end', [c] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [b] end', [c] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [b,c] end', [c] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [a] end', [c] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [a,c] end', [c] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [a,b] end', [c] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [c] >= [a,b,c] end', [c] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [] end', [b] >= []);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [c] end', [b] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [b] end', [b] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [b,c] end', [b] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [a] end', [b] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [a,c] end', [b] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [a,b] end', [b] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b] >= [a,b,c] end', [b] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [] end', [b, c] >= []);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [c] end', [b, c] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [b] end', [b, c] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [b,c] end', [b, c] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [a] end', [b, c] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [a,c] end', [b, c] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [a,b] end', [b, c] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [b,c] >= [a,b,c] end', [b, c] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [] end', [a] >= []);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [c] end', [a] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [b] end', [a] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [b,c] end', [a] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [a] end', [a] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [a,c] end', [a] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [a,b] end', [a] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a] >= [a,b,c] end', [a] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [] end', [a, c] >= []);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [c] end', [a, c] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [b] end', [a, c] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [b,c] end', [a, c] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [a] end', [a, c] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [a,c] end', [a, c] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [a,b] end', [a, c] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,c] >= [a,b,c] end', [a, c] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [] end', [a, b] >= []);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [c] end', [a, b] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [b] end', [a, b] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [b,c] end', [a, b] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [a] end', [a, b] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [a,c] end', [a, b] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [a,b] end', [a, b] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b] >= [a,b,c] end', [a, b] >= [a, b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [] end', [a, b, c] >= []);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [c] end', [a, b, c] >= [c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [b] end', [a, b, c] >= [b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [b,c] end', [a, b, c] >= [b, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [a] end', [a, b, c] >= [a]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [a,c] end', [a, b, c] >= [a, c]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [a,b] end', [a, b, c] >= [a, b]);
      test_boolean_expected('type x=(a,b,c); begin [a,b,c] >= [a,b,c] end', [a, b, c] >= [a, b, c]);

      test_boolean_expected('begin -1 = -1 end', integer(-1) = integer(-1));
      test_boolean_expected('begin -1 = -1.0 end', -1 = -1.0);
      test_boolean_expected('begin -1.0 = -1 end', -1.0 = -1);
      test_boolean_expected('begin -1.0 = -1.0 end', -1.0 = -1.0);
      test_boolean_expected('begin -1 = 0 end', -1 = integer(0));
      test_boolean_expected('begin -1 = 0.0 end', -1 = 0.0);
      test_boolean_expected('begin -1.0 = 0 end', -1.0 = 0);
      test_boolean_expected('begin -1.0 = 0.0 end', -1.0 = 0.0);
      test_boolean_expected('begin -1 = 1 end', -1 = integer(1));
      test_boolean_expected('begin -1 = 1.0 end', -1 = 1.0);
      test_boolean_expected('begin -1.0 = 1 end', -1.0 = 1);
      test_boolean_expected('begin -1.0 = 1.0 end', -1.0 = 1.0);
      test_boolean_expected('begin 0 = -1 end', 0 = -1);
      test_boolean_expected('begin 0 = -1.0 end', 0 = -1.0);
      test_boolean_expected('begin 0.0 = -1 end', 0.0 = -1);
      test_boolean_expected('begin 0.0 = -1.0 end', 0.0 = -1.0);
      test_boolean_expected('begin 0 = 0 end', 0 = 0);
      test_boolean_expected('begin 0 = 0.0 end', 0 = 0.0);
      test_boolean_expected('begin 0.0 = 0 end', 0.0 = 0);
      test_boolean_expected('begin 0.0 = 0.0 end', 0.0 = 0.0);
      test_boolean_expected('begin 0 = 1 end', 0 = 1);
      test_boolean_expected('begin 0 = 1.0 end', 0 = 1.0);
      test_boolean_expected('begin 0.0 = 1 end', 0.0 = 1);
      test_boolean_expected('begin 0.0 = 1.0 end', 0.0 = 1.0);
      test_boolean_expected('begin 1 = -1 end', 1 = -1);
      test_boolean_expected('begin 1 = -1.0 end', 1 = -1.0);
      test_boolean_expected('begin 1.0 = -1 end', 1.0 = -1);
      test_boolean_expected('begin 1.0 = -1.0 end', 1.0 = -1.0);
      test_boolean_expected('begin 1 = 0 end', 1 = 0);
      test_boolean_expected('begin 1 = 0.0 end', 1 = 0.0);
      test_boolean_expected('begin 1.0 = 0 end', 1.0 = 0);
      test_boolean_expected('begin 1.0 = 0.0 end', 1.0 = 0.0);
      test_boolean_expected('begin 1 = 1 end', 1 = 1);
      test_boolean_expected('begin 1 = 1.0 end', 1 = 1.0);
      test_boolean_expected('begin 1.0 = 1 end', 1.0 = 1);
      test_boolean_expected('begin 1.0 = 1.0 end', 1.0 = 1.0);
      test_boolean_expected('begin -1 <> -1 end', integer(-1) <> integer(-1));
      test_boolean_expected('begin -1 <> -1.0 end', -1 <> -1.0);
      test_boolean_expected('begin -1.0 <> -1 end', -1.0 <> -1);
      test_boolean_expected('begin -1.0 <> -1.0 end', -1.0 <> -1.0);
      test_boolean_expected('begin -1 <> 0 end', -1 <> integer(0));
      test_boolean_expected('begin -1 <> 0.0 end', -1 <> 0.0);
      test_boolean_expected('begin -1.0 <> 0 end', -1.0 <> 0);
      test_boolean_expected('begin -1.0 <> 0.0 end', -1.0 <> 0.0);
      test_boolean_expected('begin -1 <> 1 end', -1 <> integer(1));
      test_boolean_expected('begin -1 <> 1.0 end', -1 <> 1.0);
      test_boolean_expected('begin -1.0 <> 1 end', -1.0 <> 1);
      test_boolean_expected('begin -1.0 <> 1.0 end', -1.0 <> 1.0);
      test_boolean_expected('begin 0 <> -1 end', 0 <> -1);
      test_boolean_expected('begin 0 <> -1.0 end', 0 <> -1.0);
      test_boolean_expected('begin 0.0 <> -1 end', 0.0 <> -1);
      test_boolean_expected('begin 0.0 <> -1.0 end', 0.0 <> -1.0);
      test_boolean_expected('begin 0 <> 0 end', 0 <> 0);
      test_boolean_expected('begin 0 <> 0.0 end', 0 <> 0.0);
      test_boolean_expected('begin 0.0 <> 0 end', 0.0 <> 0);
      test_boolean_expected('begin 0.0 <> 0.0 end', 0.0 <> 0.0);
      test_boolean_expected('begin 0 <> 1 end', 0 <> 1);
      test_boolean_expected('begin 0 <> 1.0 end', 0 <> 1.0);
      test_boolean_expected('begin 0.0 <> 1 end', 0.0 <> 1);
      test_boolean_expected('begin 0.0 <> 1.0 end', 0.0 <> 1.0);
      test_boolean_expected('begin 1 <> -1 end', 1 <> -1);
      test_boolean_expected('begin 1 <> -1.0 end', 1 <> -1.0);
      test_boolean_expected('begin 1.0 <> -1 end', 1.0 <> -1);
      test_boolean_expected('begin 1.0 <> -1.0 end', 1.0 <> -1.0);
      test_boolean_expected('begin 1 <> 0 end', 1 <> 0);
      test_boolean_expected('begin 1 <> 0.0 end', 1 <> 0.0);
      test_boolean_expected('begin 1.0 <> 0 end', 1.0 <> 0);
      test_boolean_expected('begin 1.0 <> 0.0 end', 1.0 <> 0.0);
      test_boolean_expected('begin 1 <> 1 end', 1 <> 1);
      test_boolean_expected('begin 1 <> 1.0 end', 1 <> 1.0);
      test_boolean_expected('begin 1.0 <> 1 end', 1.0 <> 1);
      test_boolean_expected('begin 1.0 <> 1.0 end', 1.0 <> 1.0);
      test_boolean_expected('begin -1 < -1 end', integer(-1) < integer(-1));
      test_boolean_expected('begin -1 < -1.0 end', -1 < -1.0);
      test_boolean_expected('begin -1.0 < -1 end', -1.0 < -1);
      test_boolean_expected('begin -1.0 < -1.0 end', -1.0 < -1.0);
      test_boolean_expected('begin -1 < 0 end', -1 < integer(0));
      test_boolean_expected('begin -1 < 0.0 end', -1 < 0.0);
      test_boolean_expected('begin -1.0 < 0 end', -1.0 < 0);
      test_boolean_expected('begin -1.0 < 0.0 end', -1.0 < 0.0);
      test_boolean_expected('begin -1 < 1 end', integer(-1) < integer(1));
      test_boolean_expected('begin -1 < 1.0 end', -1 < 1.0);
      test_boolean_expected('begin -1.0 < 1 end', -1.0 < 1);
      test_boolean_expected('begin -1.0 < 1.0 end', -1.0 < 1.0);
      test_boolean_expected('begin 0 < -1 end', 0 < -1);
      test_boolean_expected('begin 0 < -1.0 end', 0 < -1.0);
      test_boolean_expected('begin 0.0 < -1 end', 0.0 < -1);
      test_boolean_expected('begin 0.0 < -1.0 end', 0.0 < -1.0);
      test_boolean_expected('begin 0 < 0 end', 0 < 0);
      test_boolean_expected('begin 0 < 0.0 end', 0 < 0.0);
      test_boolean_expected('begin 0.0 < 0 end', 0.0 < 0);
      test_boolean_expected('begin 0.0 < 0.0 end', 0.0 < 0.0);
      test_boolean_expected('begin 0 < 1 end', 0 < 1);
      test_boolean_expected('begin 0 < 1.0 end', 0 < 1.0);
      test_boolean_expected('begin 0.0 < 1 end', 0.0 < 1);
      test_boolean_expected('begin 0.0 < 1.0 end', 0.0 < 1.0);
      test_boolean_expected('begin 1 < -1 end', 1 < -1);
      test_boolean_expected('begin 1 < -1.0 end', 1 < -1.0);
      test_boolean_expected('begin 1.0 < -1 end', 1.0 < -1);
      test_boolean_expected('begin 1.0 < -1.0 end', 1.0 < -1.0);
      test_boolean_expected('begin 1 < 0 end', 1 < 0);
      test_boolean_expected('begin 1 < 0.0 end', 1 < 0.0);
      test_boolean_expected('begin 1.0 < 0 end', 1.0 < 0);
      test_boolean_expected('begin 1.0 < 0.0 end', 1.0 < 0.0);
      test_boolean_expected('begin 1 < 1 end', 1 < 1);
      test_boolean_expected('begin 1 < 1.0 end', 1 < 1.0);
      test_boolean_expected('begin 1.0 < 1 end', 1.0 < 1);
      test_boolean_expected('begin 1.0 < 1.0 end', 1.0 < 1.0);
      test_boolean_expected('begin -1 <= -1 end', integer(-1) <= integer(-1));
      test_boolean_expected('begin -1 <= -1.0 end', -1 <= -1.0);
      test_boolean_expected('begin -1.0 <= -1 end', -1.0 <= -1);
      test_boolean_expected('begin -1.0 <= -1.0 end', -1.0 <= -1.0);
      test_boolean_expected('begin -1 <= 0 end', -1 <= integer(0));
      test_boolean_expected('begin -1 <= 0.0 end', -1 <= 0.0);
      test_boolean_expected('begin -1.0 <= 0 end', -1.0 <= 0);
      test_boolean_expected('begin -1.0 <= 0.0 end', -1.0 <= 0.0);
      test_boolean_expected('begin -1 <= 1 end', -1 <= integer(1));
      test_boolean_expected('begin -1 <= 1.0 end', -1 <= 1.0);
      test_boolean_expected('begin -1.0 <= 1 end', -1.0 <= 1);
      test_boolean_expected('begin -1.0 <= 1.0 end', -1.0 <= 1.0);
      test_boolean_expected('begin 0 <= -1 end', 0 <= -1);
      test_boolean_expected('begin 0 <= -1.0 end', 0 <= -1.0);
      test_boolean_expected('begin 0.0 <= -1 end', 0.0 <= -1);
      test_boolean_expected('begin 0.0 <= -1.0 end', 0.0 <= -1.0);
      test_boolean_expected('begin 0 <= 0 end', 0 <= 0);
      test_boolean_expected('begin 0 <= 0.0 end', 0 <= 0.0);
      test_boolean_expected('begin 0.0 <= 0 end', 0.0 <= 0);
      test_boolean_expected('begin 0.0 <= 0.0 end', 0.0 <= 0.0);
      test_boolean_expected('begin 0 <= 1 end', 0 <= 1);
      test_boolean_expected('begin 0 <= 1.0 end', 0 <= 1.0);
      test_boolean_expected('begin 0.0 <= 1 end', 0.0 <= 1);
      test_boolean_expected('begin 0.0 <= 1.0 end', 0.0 <= 1.0);
      test_boolean_expected('begin 1 <= -1 end', 1 <= -1);
      test_boolean_expected('begin 1 <= -1.0 end', 1 <= -1.0);
      test_boolean_expected('begin 1.0 <= -1 end', 1.0 <= -1);
      test_boolean_expected('begin 1.0 <= -1.0 end', 1.0 <= -1.0);
      test_boolean_expected('begin 1 <= 0 end', 1 <= 0);
      test_boolean_expected('begin 1 <= 0.0 end', 1 <= 0.0);
      test_boolean_expected('begin 1.0 <= 0 end', 1.0 <= 0);
      test_boolean_expected('begin 1.0 <= 0.0 end', 1.0 <= 0.0);
      test_boolean_expected('begin 1 <= 1 end', 1 <= 1);
      test_boolean_expected('begin 1 <= 1.0 end', 1 <= 1.0);
      test_boolean_expected('begin 1.0 <= 1 end', 1.0 <= 1);
      test_boolean_expected('begin 1.0 <= 1.0 end', 1.0 <= 1.0);
      test_boolean_expected('begin -1 > -1 end', integer(-1) > integer(-1));
      test_boolean_expected('begin -1 > -1.0 end', -1 > -1.0);
      test_boolean_expected('begin -1.0 > -1 end', -1.0 > -1);
      test_boolean_expected('begin -1.0 > -1.0 end', -1.0 > -1.0);
      test_boolean_expected('begin -1 > 0 end', -1 > integer(0));
      test_boolean_expected('begin -1 > 0.0 end', -1 > 0.0);
      test_boolean_expected('begin -1.0 > 0 end', -1.0 > 0);
      test_boolean_expected('begin -1.0 > 0.0 end', -1.0 > 0.0);
      test_boolean_expected('begin -1 > 1 end', -1 > integer(1));
      test_boolean_expected('begin -1 > 1.0 end', -1 > 1.0);
      test_boolean_expected('begin -1.0 > 1 end', -1.0 > 1);
      test_boolean_expected('begin -1.0 > 1.0 end', -1.0 > 1.0);
      test_boolean_expected('begin 0 > -1 end', 0 > -1);
      test_boolean_expected('begin 0 > -1.0 end', 0 > -1.0);
      test_boolean_expected('begin 0.0 > -1 end', 0.0 > -1);
      test_boolean_expected('begin 0.0 > -1.0 end', 0.0 > -1.0);
      test_boolean_expected('begin 0 > 0 end', 0 > 0);
      test_boolean_expected('begin 0 > 0.0 end', 0 > 0.0);
      test_boolean_expected('begin 0.0 > 0 end', 0.0 > 0);
      test_boolean_expected('begin 0.0 > 0.0 end', 0.0 > 0.0);
      test_boolean_expected('begin 0 > 1 end', 0 > 1);
      test_boolean_expected('begin 0 > 1.0 end', 0 > 1.0);
      test_boolean_expected('begin 0.0 > 1 end', 0.0 > 1);
      test_boolean_expected('begin 0.0 > 1.0 end', 0.0 > 1.0);
      test_boolean_expected('begin 1 > -1 end', 1 > -1);
      test_boolean_expected('begin 1 > -1.0 end', 1 > -1.0);
      test_boolean_expected('begin 1.0 > -1 end', 1.0 > -1);
      test_boolean_expected('begin 1.0 > -1.0 end', 1.0 > -1.0);
      test_boolean_expected('begin 1 > 0 end', 1 > 0);
      test_boolean_expected('begin 1 > 0.0 end', 1 > 0.0);
      test_boolean_expected('begin 1.0 > 0 end', 1.0 > 0);
      test_boolean_expected('begin 1.0 > 0.0 end', 1.0 > 0.0);
      test_boolean_expected('begin 1 > 1 end', 1 > 1);
      test_boolean_expected('begin 1 > 1.0 end', 1 > 1.0);
      test_boolean_expected('begin 1.0 > 1 end', 1.0 > 1);
      test_boolean_expected('begin 1.0 > 1.0 end', 1.0 > 1.0);
      test_boolean_expected('begin -1 >= -1 end', integer(-1) >= integer(-1));
      test_boolean_expected('begin -1 >= -1.0 end', -1 >= -1.0);
      test_boolean_expected('begin -1.0 >= -1 end', -1.0 >= -1);
      test_boolean_expected('begin -1.0 >= -1.0 end', -1.0 >= -1.0);
      test_boolean_expected('begin -1 >= 0 end', -1 >= integer(0));
      test_boolean_expected('begin -1 >= 0.0 end', -1 >= 0.0);
      test_boolean_expected('begin -1.0 >= 0 end', -1.0 >= 0);
      test_boolean_expected('begin -1.0 >= 0.0 end', -1.0 >= 0.0);
      test_boolean_expected('begin -1 >= 1 end', integer(-1) >= integer(1));
      test_boolean_expected('begin -1 >= 1.0 end', -1 >= 1.0);
      test_boolean_expected('begin -1.0 >= 1 end', -1.0 >= 1);
      test_boolean_expected('begin -1.0 >= 1.0 end', -1.0 >= 1.0);
      test_boolean_expected('begin 0 >= -1 end', 0 >= -1);
      test_boolean_expected('begin 0 >= -1.0 end', 0 >= -1.0);
      test_boolean_expected('begin 0.0 >= -1 end', 0.0 >= -1);
      test_boolean_expected('begin 0.0 >= -1.0 end', 0.0 >= -1.0);
      test_boolean_expected('begin 0 >= 0 end', 0 >= 0);
      test_boolean_expected('begin 0 >= 0.0 end', 0 >= 0.0);
      test_boolean_expected('begin 0.0 >= 0 end', 0.0 >= 0);
      test_boolean_expected('begin 0.0 >= 0.0 end', 0.0 >= 0.0);
      test_boolean_expected('begin 0 >= 1 end', 0 >= 1);
      test_boolean_expected('begin 0 >= 1.0 end', 0 >= 1.0);
      test_boolean_expected('begin 0.0 >= 1 end', 0.0 >= 1);
      test_boolean_expected('begin 0.0 >= 1.0 end', 0.0 >= 1.0);
      test_boolean_expected('begin 1 >= -1 end', 1 >= -1);
      test_boolean_expected('begin 1 >= -1.0 end', 1 >= -1.0);
      test_boolean_expected('begin 1.0 >= -1 end', 1.0 >= -1);
      test_boolean_expected('begin 1.0 >= -1.0 end', 1.0 >= -1.0);
      test_boolean_expected('begin 1 >= 0 end', 1 >= 0);
      test_boolean_expected('begin 1 >= 0.0 end', 1 >= 0.0);
      test_boolean_expected('begin 1.0 >= 0 end', 1.0 >= 0);
      test_boolean_expected('begin 1.0 >= 0.0 end', 1.0 >= 0.0);
      test_boolean_expected('begin 1 >= 1 end', 1 >= 1);
      test_boolean_expected('begin 1 >= 1.0 end', 1 >= 1.0);
      test_boolean_expected('begin 1.0 >= 1 end', 1.0 >= 1);
      test_boolean_expected('begin 1.0 >= 1.0 end', 1.0 >= 1.0);

      test_boolean_expected('begin ''aaa'' = ''aaa'' end', 'aaa' = 'aaa');
      test_boolean_expected('begin ''aaa'' = ''aab'' end', 'aaa' = 'aab');
      test_boolean_expected('begin ''aaa'' = ''aac'' end', 'aaa' = 'aac');
      test_boolean_expected('begin ''aab'' = ''aaa'' end', 'aab' = 'aaa');
      test_boolean_expected('begin ''aab'' = ''aaa'' end', 'aab' = 'aaa');
      test_boolean_expected('begin ''aab'' = ''aab'' end', 'aab' = 'aab');
      test_boolean_expected('begin ''aab'' = ''aac'' end', 'aab' = 'aac');
      test_boolean_expected('begin ''aac'' = ''aaa'' end', 'aac' = 'aaa');
      test_boolean_expected('begin ''aac'' = ''aab'' end', 'aac' = 'aab');
      test_boolean_expected('begin ''aac'' = ''aac'' end', 'aac' = 'aac');
      test_boolean_expected('begin ''aaa'' <> ''aaa'' end', 'aaa' <> 'aaa');
      test_boolean_expected('begin ''aaa'' <> ''aab'' end', 'aaa' <> 'aab');
      test_boolean_expected('begin ''aaa'' <> ''aac'' end', 'aaa' <> 'aac');
      test_boolean_expected('begin ''aab'' <> ''aaa'' end', 'aab' <> 'aaa');
      test_boolean_expected('begin ''aab'' <> ''aab'' end', 'aab' <> 'aab');
      test_boolean_expected('begin ''aab'' <> ''aac'' end', 'aab' <> 'aac');
      test_boolean_expected('begin ''aac'' <> ''aaa'' end', 'aac' <> 'aaa');
      test_boolean_expected('begin ''aac'' <> ''aab'' end', 'aac' <> 'aab');
      test_boolean_expected('begin ''aac'' <> ''aac'' end', 'aac' <> 'aac');
      test_boolean_expected('begin ''aaa'' < ''aaa'' end', 'aaa' < 'aaa');
      test_boolean_expected('begin ''aaa'' < ''aab'' end', 'aaa' < 'aab');
      test_boolean_expected('begin ''aaa'' < ''aac'' end', 'aaa' < 'aac');
      test_boolean_expected('begin ''aab'' < ''aaa'' end', 'aab' < 'aaa');
      test_boolean_expected('begin ''aab'' < ''aab'' end', 'aab' < 'aab');
      test_boolean_expected('begin ''aab'' < ''aac'' end', 'aab' < 'aac');
      test_boolean_expected('begin ''aac'' < ''aaa'' end', 'aac' < 'aaa');
      test_boolean_expected('begin ''aac'' < ''aab'' end', 'aac' < 'aab');
      test_boolean_expected('begin ''aac'' < ''aac'' end', 'aac' < 'aac');
      test_boolean_expected('begin ''aaa'' <= ''aaa'' end', 'aaa' <= 'aaa');
      test_boolean_expected('begin ''aaa'' <= ''aab'' end', 'aaa' <= 'aab');
      test_boolean_expected('begin ''aaa'' <= ''aac'' end', 'aaa' <= 'aac');
      test_boolean_expected('begin ''aab'' <= ''aaa'' end', 'aab' <= 'aaa');
      test_boolean_expected('begin ''aab'' <= ''aab'' end', 'aab' <= 'aab');
      test_boolean_expected('begin ''aab'' <= ''aac'' end', 'aab' <= 'aac');
      test_boolean_expected('begin ''aac'' <= ''aaa'' end', 'aac' <= 'aaa');
      test_boolean_expected('begin ''aac'' <= ''aab'' end', 'aac' <= 'aab');
      test_boolean_expected('begin ''aac'' <= ''aac'' end', 'aac' <= 'aac');
      test_boolean_expected('begin ''aaa'' > ''aaa'' end', 'aaa' > 'aaa');
      test_boolean_expected('begin ''aaa'' > ''aab'' end', 'aaa' > 'aab');
      test_boolean_expected('begin ''aaa'' > ''aac'' end', 'aaa' > 'aac');
      test_boolean_expected('begin ''aab'' > ''aaa'' end', 'aab' > 'aaa');
      test_boolean_expected('begin ''aab'' > ''aab'' end', 'aab' > 'aab');
      test_boolean_expected('begin ''aab'' > ''aac'' end', 'aab' > 'aac');
      test_boolean_expected('begin ''aac'' > ''aaa'' end', 'aac' > 'aaa');
      test_boolean_expected('begin ''aac'' > ''aab'' end', 'aac' > 'aab');
      test_boolean_expected('begin ''aac'' > ''aac'' end', 'aac' > 'aac');
      test_boolean_expected('begin ''aaa'' >= ''aaa'' end', 'aaa' >= 'aaa');
      test_boolean_expected('begin ''aaa'' >= ''aab'' end', 'aaa' >= 'aab');
      test_boolean_expected('begin ''aaa'' >= ''aac'' end', 'aaa' >= 'aac');
      test_boolean_expected('begin ''aab'' >= ''aaa'' end', 'aab' >= 'aaa');
      test_boolean_expected('begin ''aab'' >= ''aab'' end', 'aab' >= 'aab');
      test_boolean_expected('begin ''aab'' >= ''aac'' end', 'aab' >= 'aac');
      test_boolean_expected('begin ''aac'' >= ''aaa'' end', 'aac' >= 'aaa');
      test_boolean_expected('begin ''aac'' >= ''aab'' end', 'aac' >= 'aab');
      test_boolean_expected('begin ''aac'' >= ''aac'' end', 'aac' >= 'aac');

      test_boolean_expected('type x=(a,b,c); begin a = a end', a = a);
      test_boolean_expected('type x=(a,b,c); begin a = b end', a = b);
      test_boolean_expected('type x=(a,b,c); begin a = c end', a = c);
      test_boolean_expected('type x=(a,b,c); begin b = a end', b = a);
      test_boolean_expected('type x=(a,b,c); begin b = b end', b = b);
      test_boolean_expected('type x=(a,b,c); begin b = c end', b = c);
      test_boolean_expected('type x=(a,b,c); begin c = a end', c = a);
      test_boolean_expected('type x=(a,b,c); begin c = b end', c = b);
      test_boolean_expected('type x=(a,b,c); begin c = c end', c = c);
      test_boolean_expected('type x=(a,b,c); begin a <> a end', a <> a);
      test_boolean_expected('type x=(a,b,c); begin a <> b end', a <> b);
      test_boolean_expected('type x=(a,b,c); begin a <> c end', a <> c);
      test_boolean_expected('type x=(a,b,c); begin b <> a end', b <> a);
      test_boolean_expected('type x=(a,b,c); begin b <> b end', b <> b);
      test_boolean_expected('type x=(a,b,c); begin b <> c end', b <> c);
      test_boolean_expected('type x=(a,b,c); begin c <> a end', c <> a);
      test_boolean_expected('type x=(a,b,c); begin c <> b end', c <> b);
      test_boolean_expected('type x=(a,b,c); begin c <> c end', c <> c);
      test_boolean_expected('type x=(a,b,c); begin a < a end', a < a);
      test_boolean_expected('type x=(a,b,c); begin a < b end', a < b);
      test_boolean_expected('type x=(a,b,c); begin a < c end', a < c);
      test_boolean_expected('type x=(a,b,c); begin b < a end', b < a);
      test_boolean_expected('type x=(a,b,c); begin b < b end', b < b);
      test_boolean_expected('type x=(a,b,c); begin b < c end', b < c);
      test_boolean_expected('type x=(a,b,c); begin c < a end', c < a);
      test_boolean_expected('type x=(a,b,c); begin c < b end', c < b);
      test_boolean_expected('type x=(a,b,c); begin c < c end', c < c);
      test_boolean_expected('type x=(a,b,c); begin a <= a end', a <= a);
      test_boolean_expected('type x=(a,b,c); begin a <= b end', a <= b);
      test_boolean_expected('type x=(a,b,c); begin a <= c end', a <= c);
      test_boolean_expected('type x=(a,b,c); begin b <= a end', b <= a);
      test_boolean_expected('type x=(a,b,c); begin b <= b end', b <= b);
      test_boolean_expected('type x=(a,b,c); begin b <= c end', b <= c);
      test_boolean_expected('type x=(a,b,c); begin c <= a end', c <= a);
      test_boolean_expected('type x=(a,b,c); begin c <= b end', c <= b);
      test_boolean_expected('type x=(a,b,c); begin c <= c end', c <= c);
      test_boolean_expected('type x=(a,b,c); begin a > a end', a > a);
      test_boolean_expected('type x=(a,b,c); begin a > b end', a > b);
      test_boolean_expected('type x=(a,b,c); begin a > c end', a > c);
      test_boolean_expected('type x=(a,b,c); begin b > a end', b > a);
      test_boolean_expected('type x=(a,b,c); begin b > b end', b > b);
      test_boolean_expected('type x=(a,b,c); begin b > c end', b > c);
      test_boolean_expected('type x=(a,b,c); begin c > a end', c > a);
      test_boolean_expected('type x=(a,b,c); begin c > b end', c > b);
      test_boolean_expected('type x=(a,b,c); begin c > c end', c > c);
      test_boolean_expected('type x=(a,b,c); begin a >= a end', a >= a);
      test_boolean_expected('type x=(a,b,c); begin a >= b end', a >= b);
      test_boolean_expected('type x=(a,b,c); begin a >= c end', a >= c);
      test_boolean_expected('type x=(a,b,c); begin b >= a end', b >= a);
      test_boolean_expected('type x=(a,b,c); begin b >= b end', b >= b);
      test_boolean_expected('type x=(a,b,c); begin b >= c end', b >= c);
      test_boolean_expected('type x=(a,b,c); begin c >= a end', c >= a);
      test_boolean_expected('type x=(a,b,c); begin c >= b end', c >= b);
      test_boolean_expected('type x=(a,b,c); begin c >= c end', c >= c);
   end;

function create_cprimary: TDefinition;
   begin
      result := Tcprimary.CreateFromSourceTokens
   end;


procedure test_Tcprimary;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_string_expected
      (s: string;
       expected_result: string
      );
      var c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> string_constant) or (c.s <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_boolean_expected
      (s: string;
       expected_result: boolean
      );
      var c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> boolean_constant) or (c.b <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_enum_expected
      (s: string;
       ord_of_expected_enum: integer;
       enum_type: string
      );
      var
         c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> enum_constant) or (c.ordinal_value.AsInteger <> ord_of_expected_enum) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end
;//         else
//            if c.enum_typedef <> CurrentDefinitionTable.GetDefinitionForIdentifier(enum_type) then
//               begin
//                  record_bad_test_result;
//                  display(s + ': failed, enum type wrong')
//               end;
         c.Release
      end;
   procedure test_set_expected
      (s: string;
       expected_set: TSet256;
       expected_ordinal_base_type: TOrdinalBaseType;
       expected_set_base_enum_typedef:
       string
      );
      var
         c: TCPrimary;
      begin
         c := TCPrimary(test_fragment(s, create_cprimary));
         if (c = nil) or (c.constant_kind <> set_constant) or (c.sett <> expected_set) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end
         else
            if c.set_ordinal_base_type <> expected_ordinal_base_type then
               begin
                  record_bad_test_result;
                  display(s + ': failed, set base type wrong')
               end;
         c.Release
      end;
   type
      x = (a, b, c);
   begin
      display('testing Tcprimary');
      test_integer_expected('begin 3 end', 3);
      test_real_expected('begin 3.14 end', 3.14);
      test_string_expected('begin ''asdf'' end', 'asdf');
      test_string_expected('begin #55 end', '7');
      test_boolean_expected('begin true end', true);
      test_boolean_expected('begin false end', false);
      test_boolean_expected('begin not true end', false);
      test_boolean_expected('begin not false end', true);
      test_integer_expected('const a=5; begin a end', 5);
      test_boolean_expected('begin (true) end', true);
      test_compile_error_generation_for_program_fragment('begin a end', create_cprimary, err_undefined_identifier, 'a end');
      test_enum_expected('type x = (a,b,c); begin b end', 1, 'x');

      test_integer_expected('begin succ(5) end', 6);
      test_boolean_expected('begin succ(false) end', true);
      test_enum_expected('type x=(a,b,c); begin succ(b) end', 2, 'x');
      test_string_expected('begin succ(''a'') end', 'b');

      test_string_expected('const s=''asdf''; c=s[2]; begin c end', 's');
      test_string_expected('type ts=array[1..2] of char; const s:ts=([1]=''a'',[2]=''b''); begin s[2] end', 'b');

      test_integer_expected('begin pred(5) end', 4);
      test_boolean_expected('begin pred(true) end', false);
      test_enum_expected('type x=(a,b,c); begin pred(b) end', 0, 'x');
      test_string_expected('begin pred(''b'') end', 'a');

      test_integer_expected('begin abs(5) end', 5);
      test_integer_expected('begin abs (-3333) end', 3333);
      test_real_expected('begin abs (34.56) end', 34.56);
      test_real_expected('begin abs (-8976.2) end', 8976.2);

      test_integer_expected('begin round (3.45) end', round(3.45));
      test_integer_expected('begin round (-12345.56) end', max (round(-12345.56), -32768));
      test_integer_expected('begin trunc (3.45) end', trunc(3.45));
      test_integer_expected('begin trunc (-12345.56) end', max (trunc(-12345.56), -32768));

      test_integer_expected ('begin high(int8) end', 127);
      test_integer_expected ('type x=5..23; begin high(x) end', 23);
      test_string_expected ('begin high(char) end', chr(255));
      test_string_expected ('type x=''a''..''c''; begin high(x) end', 'c');
      test_enum_expected ('type x=(a,b,c); begin high(x) end', 2, 'x');
      test_boolean_expected ('begin high(boolean) end', true);
      test_boolean_expected ('type x=false..false; begin high(x) end', false);

      test_integer_expected ('begin low(int8) end', -128);
      test_integer_expected ('type x=5..23; begin low(x) end', 5);
      test_string_expected ('begin low(char) end', chr(0));
      test_string_expected ('type x=''a''..''c''; begin low(x) end', 'a');
      test_enum_expected ('type x=(a,b,c); begin low(x) end', 0, 'x');
      test_boolean_expected ('begin low(boolean) end', false);
      test_boolean_expected ('type x=true..true; begin low(x) end', true);

      test_integer_expected('begin ord(55) end', 55);
      test_integer_expected('type x = (a,b,c); begin ord(c) end', ord(c));
      test_integer_expected('begin ord(''a'') end', ord('a'));
      test_integer_expected('begin ord(true) end', ord(true));
      test_compile_error_generation_for_program_fragment('begin ord(1.7) end', create_cprimary, err_ord_argument_must_be_ordinal, '1.7) end');
      test_compile_error_generation_for_program_fragment('begin ord (''ab'') end', create_cprimary, err_only_single_char_allowed, '''ab'') end');
      test_string_expected('begin chr(ord(''a'')) end', 'a');
      test_compile_error_generation_for_program_fragment('begin chr(1.5) end', create_cprimary, err_integer_expected, '1.5) end');
      test_compile_error_generation_for_program_fragment('begin chr(666) end', create_cprimary, err_char_value_outside_legal_range, '666) end');
      test_set_expected('begin [1,3,4] end', [1, 3, 4], ordinal_base_is_integer, '');
      test_set_expected('begin [true] end', [1], ordinal_base_is_bool, '');
      test_set_expected('begin [chr(1),chr(3),chr(4)] end', [1, 3, 4], ordinal_base_is_char, '');
      test_set_expected('type x=(a,b,c); begin [a,c] end', [0, 2], ordinal_base_is_enum, 'x');
      test_set_expected('begin[0,255]end', [0, 255], ordinal_base_is_integer, '');
      test_set_expected('begin[]end', [], empty_set_ordinal_base_unknown, '');
      test_set_expected('begin [2..4, 6] end', [2..4, 6], ordinal_base_is_integer, '');
      test_set_expected('begin [false..true] end', [0, 1], ordinal_base_is_bool, '');
      test_set_expected('begin [''a''..''c'', ''e''] end', [ord('a')..ord('c'),  ord('e')], ordinal_base_is_char, '');
      test_set_expected('type x=(a,b,c); begin [a..c] end', [ord(a)..ord(c)], ordinal_base_is_enum, 'x');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f);begin[a,d]end', create_cprimary, err_set_element_type_differs_from_previous_set_elements, 'd]end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);begin[a,1]end', create_cprimary, err_set_element_type_differs_from_previous_set_elements, '1]end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);begin[1,a]end', create_cprimary, err_set_element_type_differs_from_previous_set_elements, 'a]end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);begin[true,a]end', create_cprimary, err_set_element_type_differs_from_previous_set_elements, 'a]end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);begin[''a'',a]end', create_cprimary, err_set_element_type_differs_from_previous_set_elements, 'a]end');
      test_compile_error_generation_for_program_fragment('begin [''asdf''] end', create_cprimary, err_only_single_chars_allowed_in_sets, '''asdf''] end');
      test_compile_error_generation_for_program_fragment('begin [''''] end', create_cprimary, err_null_chars_not_allowed_in_sets, '''''] end');
      test_compile_error_generation_for_program_fragment('begin[-1,2]end', create_cprimary, err_set_member_value_outside_legal_range, '-1,2]end');
      test_compile_error_generation_for_program_fragment('begin[256]end', create_cprimary, err_set_member_value_outside_legal_range, '256]end');
      test_compile_error_generation_for_program_fragment('begin #', create_cprimary, err_char_constant_expected, '#');
      test_compile_error_generation_for_program_fragment('begin #x', create_cprimary, err_char_constant_expected, '#x');
      test_compile_error_generation_for_program_fragment('begin #666', create_cprimary, err_char_value_outside_legal_range, '#666');
   end;

function create_Tcexponentiation: TDefinition;
   begin
      result := Tcexponentiation.CreateFromSourceTokens
   end;


procedure test_Tcexponentiation;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var
         c: Tcexponentiation;
      begin
         c := Tcexponentiation(test_fragment(s, create_Tcexponentiation));
         if (c = nil) or (c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release;
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var
         c: Tcexponentiation;
      begin
         c := Tcexponentiation(test_fragment(s, create_Tcexponentiation));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   begin
      display('testing Tcexponentiation');
      test_integer_expected('begin 3 end', 3);
      test_real_expected('begin 3.14 end', 3.14);
      // test_integer_expected ('begin 3**3 end', 27);
      // test_real_expected ('begin 2**2.1 end', power (2, 2.1));
      // test_real_expected ('begin 2.1**2 end', power (2.1, 2));
      // test_real_expected ('begin 2.1**2.1 end', power (2.1, 2.1));
      // test_compile_error_generation_for_program_fragment ('begin ''asdf''**2 end', create_cexponentiation, err_left_operand_must_be_number, '''asdf''**2 end');
      // test_compile_error_generation_for_program_fragment ('begin 2**''asfd'' end', create_cexponentiation, err_right_operand_must_be_number, '''asfd'' end');
      // test_compile_error_generation_for_program_fragment ('begin [1,2]**2 end', create_cexponentiation, err_left_operand_must_be_number, '[1,2]**2 end');
      // test_compile_error_generation_for_program_fragment ('begin 2**[1,2] end', create_cexponentiation, err_right_operand_must_be_number, '[1,2] end');
   end;

function create_Tcfactor: TDefinition;
   begin
      result := Tcfactor.CreateFromSourceTokens
   end;

procedure test_Tcfactor;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var
         c: Tcfactor;
      begin
         c := Tcfactor(test_fragment(s, create_Tcfactor));
         if (c = nil) or(c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed')
            end;
         c.Release
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var
         c: Tcfactor;
      begin
         c := Tcfactor(test_fragment(s, create_Tcfactor));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_set_expected
      (s: string;
       expected_set: TSet256;
       expected_ordinal_base_type: TOrdinalBaseType;
       expected_set_base_enum_typedef:
       string
      );
      var
         c: Tcfactor;
      begin
         c := Tcfactor(test_fragment(s, create_Tcfactor));
         if (c = nil) or (c.constant_kind <> set_constant) or (c.sett <> expected_set) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end
         else
            if c.set_ordinal_base_type <> expected_ordinal_base_type then
               begin
                  record_bad_test_result;
                  display(s + ': failed, set base type wrong')
               end;
         c.Release
      end;
   begin
      display('testing Tcfactor');
      test_integer_expected('begin 3 end', 3);
      test_real_expected('begin 3.14 end', 3.14);

      // test_integer_expected ('begin 3**3 end', 27);
      // test_real_expected ('begin 2**2.1 end', power (2, 2.1));
      // test_real_expected ('begin 2.1**2 end', power (2.1, 2));
      // test_real_expected ('begin 2.1**2.1 end', power (2.1, 2.1));
      //
      // test_integer_expected ('begin -3**3 end', -27);
      // test_real_expected ('begin -2**2.1 end', -power (2, 2.1));
      // test_real_expected ('begin -2.1**2 end', -power (2.1, 2));
      // test_real_expected ('begin -2.1**2.1 end', -power (2.1, 2.1));
      //
      // test_integer_expected ('begin ++3**3 end', 27);
      // test_real_expected ('begin ++2**2.1 end', power (2, 2.1));
      // test_real_expected ('begin ++2.1**2 end', power (2.1, 2));
      // test_real_expected ('begin ++2.1**2.1 end', power (2.1, 2.1));
      //
      // test_compile_error_generation_for_program_fragment ('begin ''asdf''**2 end', create_cfactor, err_left_operand_must_be_number, '''asdf''**2 end');
      // test_compile_error_generation_for_program_fragment ('begin 2**''asfd'' end', create_cfactor, err_right_operand_must_be_number, '''asfd'' end');
   end;

function create_cterm: TDefinition;
   begin
      result := Tcterm.CreateFromSourceTokens
   end;

procedure test_Tcterm;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var
         c: Tcterm;
      begin
         c := Tcterm(test_fragment(s, create_cterm));
         if (c = nil) or (c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var
         c: Tcterm;
      begin
         c := Tcterm(test_fragment(s, create_cterm));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_string_expected
      (s: string;
       expected_result: string
      );
      var
         c: Tcterm;
      begin
         c := Tcterm(test_fragment(s, create_cterm));
         if (c = nil) or (c.constant_kind <> string_constant) or (c.s <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_boolean_expected
      (s: string;
       expected_result: boolean
      );
      var
         c: Tcterm;
      begin
         c := Tcterm(test_fragment(s, create_cterm));
         if (c = nil) or (c.constant_kind <> boolean_constant) or (c.b <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_set_expected
      (s: string;
       expected_set: TSet256;
       expected_ordinal_base_type: TOrdinalBaseType;
       expected_set_base_enum_typedef:
       string
      );
      var
         c: Tcterm;
      begin
         c := Tcterm(test_fragment(s, create_cterm));
         if (c = nil) or (c.constant_kind <> set_constant) or (c.sett <> expected_set)
         then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end
         else
            if c.set_ordinal_base_type <> expected_ordinal_base_type then
               begin
                  record_bad_test_result;
                  display(s + ': failed, set base type wrong')
               end;
         c.Release
      end;
   begin
      display('testing Tcterm');

      test_integer_expected('begin 3*4 end', 12);
      test_real_expected('begin 3.14*2 end', 3.14 * 2);
      test_real_expected('begin 2*3.14 end', 3.14 * 2);
      test_real_expected('begin 3.14*2.1 end', 3.14 * 2.1);

      test_integer_expected('begin 3*4*5 end', 3 * 4 * 5);
      test_real_expected('begin 3.14*2*.7 end', 3.14 * 2 * 0.7);

      test_real_expected('begin 3/4 end', 3 / 4);
      test_real_expected('begin 3/4/6 end', 3 / 4 / 6);
      test_real_expected('begin 3.14/2 end', 3.14 / 2);
      test_real_expected('begin 2/3.14 end', 2 / 3.14);
      test_real_expected('begin 3.14/2.1 end', 3.14 / 2.1);
      test_compile_error_generation_for_program_fragment('begin 3/0 end', create_cterm, err_divide_by_zero, '0 end');
      test_compile_error_generation_for_program_fragment('begin 3/0.0 end', create_cterm, err_divide_by_zero, '0.0 end');

      test_integer_expected('begin 25 div 5 end', 5);
      test_integer_expected('begin 2578 div 5 div 2 end', 2578 div 5 div 2);
      test_compile_error_generation_for_program_fragment('begin 25.0 div 5 end', create_cterm, err_left_operand_must_be_integer, '25.0 div 5 end');
      test_compile_error_generation_for_program_fragment('begin 25 div 5.0 end', create_cterm, err_right_operand_must_be_integer, '5.0 end');

      test_integer_expected('begin 26 mod 5 end', 1);
      test_integer_expected('begin 273 mod 27 mod 3 end', 273 mod 27 mod 3);
      test_compile_error_generation_for_program_fragment('begin 25.0 mod 5 end', create_cterm, err_left_operand_must_be_integer, '25.0 mod 5 end');
      test_compile_error_generation_for_program_fragment('begin 25 mod 5.0 end', create_cterm, err_right_operand_must_be_integer, '5.0 end');

      test_boolean_expected('begin false and false end', false);
      test_boolean_expected('begin false and true end', false);
      test_boolean_expected('begin true and false end', false);
      test_boolean_expected('begin true and true end', true);

      test_set_expected('begin [1,2]*[3,4] end', [1, 2] * [3, 4], ordinal_base_is_integer, '');
      test_set_expected('begin []*[1,2] end', [] * [1, 2], ordinal_base_is_integer, '');
      test_set_expected('begin [1,2]*[] end', [1, 2] * [], ordinal_base_is_integer, '');
      test_set_expected('begin []*[] end', [] * [], empty_set_ordinal_base_unknown, '');
      test_set_expected('begin [1..6]*[2..12]*[3] end', [1..6] * [2..12] * [3], ordinal_base_is_integer, '');
      test_compile_error_generation_for_program_fragment('begin [1,2]*3 end', create_cterm, err_set_expected, '3 end');
      test_compile_error_generation_for_program_fragment('begin [1,2]*[true] end', create_cterm, err_both_operand_sets_must_be_of_same_type, '*[true] end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f); begin [a,b]*[e,f] end', create_cterm, err_both_operand_sets_must_be_of_same_type, '*[e,f] end');
   end;

function create_csimple_expression: TDefinition;
   begin
      result := Tcsimple_expression.CreateFromSourceTokens
   end;

procedure test_Tcsimple_expression;
   procedure test_integer_expected
      (s: string;
       expected_result: integer
      );
      var
         c: Tcsimple_expression;
      begin
         c := Tcsimple_expression(test_fragment(s, create_csimple_expression));
         if (c = nil) or (c.constant_kind <> integer_constant) or (c.ordinal_value.AsInteger <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_real_expected
      (s: string;
       expected_result: real
      );
      var
         c: Tcsimple_expression;
      begin
         c := Tcsimple_expression(test_fragment(s, create_csimple_expression));
         if (c = nil) or (c.constant_kind <> real_constant) or (round(c.r * 100) <> round(expected_result * 100)) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_string_expected
      (s: string;
       expected_result: string
      );
      var
         c: Tcsimple_expression;
      begin
         c := Tcsimple_expression(test_fragment(s, create_csimple_expression));
         if (c = nil) or (c.constant_kind <> string_constant) or (c.s <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_boolean_expected
      (s: string;
       expected_result: boolean
      );
      var
         c: Tcsimple_expression;
      begin
         c := Tcsimple_expression(test_fragment(s, create_csimple_expression));
         if (c = nil) or (c.constant_kind <> boolean_constant) or (c.b <> expected_result) then
            begin
               record_bad_test_result;
               display(s + ': failed');
            end;
         c.Release
      end;
   procedure test_set_expected
      (s: string;
       expected_set: TSet256;
       expected_ordinal_base_type: TOrdinalBaseType;
       expected_set_base_enum_typedef:
       string
      );
      var
         c: Tcsimple_expression;
      begin
         c := Tcsimple_expression(test_fragment(s, create_csimple_expression));
         if (c = nil) or (c.constant_kind <> set_constant) or (c.sett <> expected_set) then
            begin
               record_bad_test_result;
               display(s + ': failed')
            end
         else
            if c.set_ordinal_base_type <> expected_ordinal_base_type then
               begin
                  record_bad_test_result;
                  display(s + ': failed, set base type wrong')
               end;
         c.Release
      end;
   begin
      display('testing Tcsimple_expression');

      test_integer_expected('begin 3+4 end', 3 + 4);
      test_real_expected('begin 3.14+2 end', 3.14 + 2);
      test_real_expected('begin 2+3.14 end', 3.14 + 2);
      test_real_expected('begin 3.14+2.1 end', 3.14 + 2.1);

      test_integer_expected('begin 3-4 end', 3 - 4);
      test_real_expected('begin 3.14-2 end', 3.14 - 2);
      test_real_expected('begin 2-3.14 end', 2 - 3.14);
      test_real_expected('begin 3.14-2.1 end', 3.14 - 2.1);

      test_integer_expected('begin 1+2+3 end', 6);
      test_integer_expected('begin 1-2-3 end', -4);
      test_real_expected('begin -1-2.0+3 end', 0);

      test_integer_expected('begin 25 div 5 end', 5);
      test_compile_error_generation_for_program_fragment('begin 25.0 div 5 end', create_csimple_expression, err_left_operand_must_be_integer, '25.0 div 5 end');
      test_compile_error_generation_for_program_fragment('begin 25 div 5.0 end', create_csimple_expression, err_right_operand_must_be_integer, '5.0 end');

      test_integer_expected('begin 26 mod 5 end', 1);
      test_compile_error_generation_for_program_fragment('begin 25.0 mod 5 end', create_csimple_expression, err_left_operand_must_be_integer, '25.0 mod 5 end');
      test_compile_error_generation_for_program_fragment('begin 25 mod 5.0 end', create_csimple_expression, err_right_operand_must_be_integer, '5.0 end');

      test_boolean_expected('begin true or true end', true);
      test_boolean_expected('begin true or false end', true);
      test_boolean_expected('begin false or true end', true);
      test_boolean_expected('begin false or false end', false);
      test_boolean_expected('begin false or false or true end', true);

      test_string_expected('begin ''asdf'' end', 'asdf');
      test_integer_expected('begin -3 end', -3);
      test_integer_expected('begin +4 end', 4);
      test_real_expected('begin -3.14 end', -3.14);
      test_real_expected('begin +4.56 end', 4.56);
      test_integer_expected('const b=6; begin b end', 6);
      test_compile_error_generation_for_program_fragment('begin a end', create_csimple_expression, err_undefined_identifier, 'a end');
      test_boolean_expected('begin not true end', false);
      test_boolean_expected('begin not false end', true);
      test_boolean_expected('begin (true) end', true);

      test_string_expected('begin ''abc'' + ''def'' end', 'abcdef');
      test_compile_error_generation_for_program_fragment('begin ''abc'' + 5 end', create_csimple_expression, err_right_operand_must_be_string, '5 end');

      test_set_expected('begin [1,2]+[3,4] end', [1, 2, 3, 4], ordinal_base_is_integer, '');
      test_set_expected('begin [1,2,3]-[2] end', [1, 3], ordinal_base_is_integer, '');
      test_set_expected('begin [true]+[false] end', [0, 1], ordinal_base_is_bool, '');
      test_set_expected('type x=(a,b,c);begin[a,b]-[b,c]end', [0], ordinal_base_is_enum, 'x');
      test_set_expected('begin [5,6]+[] end', [5, 6] + [], ordinal_base_is_integer, '');
      test_set_expected('begin []-[5,6] end', [] - [5, 6], ordinal_base_is_integer, '');
      test_compile_error_generation_for_program_fragment('begin [1]+3 end', create_csimple_expression, err_set_expected, '3 end');
      test_compile_error_generation_for_program_fragment('begin [1,2]+[false] end', create_csimple_expression, err_both_operand_sets_must_be_of_same_type, '+[false] end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f);begin [a,b]+[d,e] end', create_csimple_expression, err_both_operand_sets_must_be_of_same_type, '+[d,e] end');

      // test constants derived from structured constants
      test_integer_expected('type tsc=record i,j:int8 end;const sc:tsc=(i=1,j=2);i=sc.i;j=sc.i+2;begin sc.i end', 1);
      test_integer_expected('type tsc=record i,j:int8 end;const sc:tsc=(i=1,j=2);i=sc.i;j=sc.i+2;begin i end.', 1);
      test_integer_expected('type tsc=record i,j:int8 end;const sc:tsc=(i=1,j=2);i=sc.i;j=sc.i+2;begin j end.', 3);
      test_integer_expected('type tsc=array[1..2]of record i,j:int8 end;const sc:tsc=([1]=(i=1,j=2),[2]=(i=3,j=4));a2=sc[2];i=a2.i;begin i end', 3);
      test_integer_expected('type tsc=array[1..2]of packed record i:int8;j:int8 end;const sc:tsc=([1]=(i=1,j=2),[2]=(i=3,j=4));a2=sc[2];i=a2.i;begin i end', 3);
      test_integer_expected('type tsc=record i:int8;a:array[1..2]of int8 end;const sc:tsc=(i=5,a=([1]=3,[2]=4));a=sc.a;i=a[1];begin i end', 3);
      test_integer_expected('type tov=overlay j:int16;x:int8 end;tsc=record i:int8;o:tov end;const sc:tsc=(i=5,o=(j=6));o=sc.o;i=o.j; begin i end', 6);
      test_set_expected('type tsc=record i:int8;s:set of 0..7 end;const sc: tsc = (i=5,s=[2..3]);a=sc.s; b=[1]+sc.s;begin sc.s end', [2,3], ordinal_base_is_integer, '');
      test_set_expected('type tsc=record i:int8;s:set of 0..7 end;const sc: tsc = (i=5,s=[2..3]);a=sc.s; b=[1]+sc.s;begin a end', [2,3], ordinal_base_is_integer, '');
      test_set_expected('type tsc=record i:int8;s:set of 0..7 end;const sc: tsc = (i=5,s=[2..3]);a=sc.s; b=[1]+sc.s;begin b end', [1,2,3], ordinal_base_is_integer, '');
      test_string_expected('type tsc=record i: int8; s: string[5] end;const sc:tsc=(i=5,s=''abc'');a=sc.s;b=sc.s+''d'';begin sc.s end', 'abc');
      test_string_expected('type tsc=record i: int8; s: string[5] end;const sc:tsc=(i=5,s=''abc'');a=sc.s;b=sc.s+''d'';begin a end', 'abc');
      test_string_expected('type tsc=record i: int8; s: string[5] end;const sc:tsc=(i=5,s=''abc'');a=sc.s;b=sc.s+''d'';begin b end', 'abcd');
   end;

procedure test_constant_syntax_unit;
   begin
      display('============================');
      display('TESTING CONSTANT_SYNTAX_UNIT');
      display('============================');
      test_Tcprimary;
      test_Tcexponentiation;
      test_Tcfactor;
      test_Tcterm;
      test_Tcsimple_expression;
      test_cexpression;
      display('')
   end;

procedure test_TROMConstant;
   procedure test1;
      const
         s = 'type t=record a: int8; b: int16 end; begin a:t = (a=5,b=6); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         if a.descriptor <> rw_rom then
            begin
               record_bad_test_result;
               display(s + ': rom addr space expected')
            end;
         if a.address_mode <> absolute_address_mode then
            begin
               record_bad_test_result;
               display(s + ': abs mode expected')
            end;
         a.Release
      end;
   procedure test2;
      const
         s = 'type t=array [3..5] of int8; begin a:t = ([3]=1,[4]=22,[5]=77); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         a.Release
      end;
   procedure test3;
      const
         s =
         'type t=array [false..true] of int8; begin a:t = ([false]=1,[true]=77); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         a.Release
      end;
   procedure test4;
      const
         s =
         'type e=(e1,e2,e3,e4);t=array [e2..e4] of int8; begin a:t = ([e2]=1,[e3]=22,[e4]=77); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         a.Release
      end;
   procedure test5;
      const
         s =
         'type t=array [''a''..''c''] of int8; begin a:t = ([''a'']=1,[''b'']=22,[''c'']=77); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         a.Release
      end;
   procedure test6;
      const
         s =
         'type t=array [3..5, 1..2] of int8; begin a:t = ([3]=([1]=1,[2]=2),[4]=([1]=1,[2]=2),[5]=([1]=1,[2]=2)); end';
      var
         a: TVariable;
      begin
         a := TVariable(test_fragment (s, create_TROMConstant));
         if not a.IsDataDefinition then
            begin
               record_bad_test_result;
               display(s + ': def data expected')
            end;
         a.Release
      end;
   procedure test7;
      var s: string;
      begin
         s :=     'type';
         s := s + ' e=(a,b);';
         s := s + ' tov=overlay';
         s := s + '      i: int8;';
         s := s + '      array [1..2] of int8;';
         s := s + '      array [e] of int8;';
         s := s + '      record';
         s := s + '         ri, rj: int8';
         s := s + '      end;';
         s := s + '      packed record';
         s := s + '         pri: int8; prj: int8';
         s := s + '      end;';
         s := s + '      r: real;';
         s := s + '      s: set of 0..5;';
         s := s + '    end;';
         s := s + 'rom';
         s := s + ' r1: tov = ([a]=4,[b]=5);';
         s := s + ' r2: tov = (i=5);';
         s := s + ' r3: tov = ([1]=3,[2]=4);';
         s := s + ' r5: tov = (ri=6,rj=7);';
         s := s + ' r6: tov = (pri=3,prj=8);';
         s := s + '   r7: tov = (r = 5.4);';
         s := s + '   r8: tov = (s = [3,4]);';
         s := s + 'begin ';
         s := s + 'end.';
         test_only_for_successful_compilation (s)
      end;
   procedure test8;
      begin
         test_compile_error_generation ('type tov=overlay overlay m: int8 end end; begin end.', err_invalid_anonymous_type_for_overlay, 'overlay m:')
      end;
   procedure test9;
      var s: string;
      begin
         s := 'type';
         s := s + ' tpr = overlay';
         s := s + '         packed record i: int3; j: int5 end;';
         s := s + '         packed record k: int3; l: int5 end;';
         s := s + '         m: int16;';
         s := s + '         string[10];';
         s := s + '         r: real';
         s := s + '   end;';
         s := s + 'const x=''abc'';';
         s := s + 'var';
         s := s + ' v: tpr;';
         s := s + ' s: string[10];';
         s := s + 'begin';
         s := s + ' v := tpr: (i=0,j=0);';
         s := s + ' v := tpr: (k=2,l=5);';
         s := s + ' v := tpr: (m=-1);';
         s := s + ' v := tpr: (''xyz'');';
         s := s + ' v := tpr: (x + ''a'');';
         s := s + ' v := tpr: (r = 1.2);';
         s := s + ' s := ''abc'' + ''def'';';
         s := s + 'end.';
         test_only_for_successful_compilation (s)
      end;

   begin
      display('testing TROMConstant');
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test_compile_error_generation_for_program_fragment('type t=array [3..5] of int8; begin a:t = ([3]=1,[5.3]=22,[5]=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         '5.3]=22,[5]=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [3..5] of int8; begin a:t = ([3]=1,[5]=22,[5]=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['4']),
                                                         '5]=22,[5]=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [''a''..''c''] of int8; begin a:t = ([3]=1,[''b'']=22,[''c'']=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         '3]=1,[''b'']=22,[''c'']=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [''a''..''c''] of int8; begin a:t = ([''ab'']=1,[''b'']=22,[''c'']=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         '''ab'']=1,[''b'']=22,[''c'']=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [''a''..''c''] of int8; begin a:t = ([''b'']=1,[''b'']=22,[''c'']=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['''a''']),
                                                         '''b'']=1,[''b'']=22,[''c'']=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [chr(0)..''c''] of int8; begin a:t = ([''b'']=1,[''b'']=22,[''c'']=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['chr(0)']),
                                                         '''b'']=1,[''b'']=22,[''c'']=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [false..true] of int8; begin a:t = ([false]=1,[5]=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         '5]=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [false..true] of int8; begin a:t = ([false]=1,[false]=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['true']),
                                                         'false]=77); end');
      test_compile_error_generation_for_program_fragment('type t=array [false..true] of int8; begin a:t = ([true]=1,[true]=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['false']),
                                                         'true]=1,[true]=77); end');
      test_compile_error_generation_for_program_fragment('type e=(e1,e2,e3,e4);t=array [e2..e4] of int8; begin a:t = ([e2]=1,[5]=22,[e4]=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         '5]=22,[e4]=77); end');
      test_compile_error_generation_for_program_fragment('type e=(e1,e2,e3,e4);ex=(x1,x2,x3);t=array [e2..e4] of int8; begin a:t = ([e2]=1,[x1]=22,[e4]=77); end',
                                                         create_TROMConstant,
                                                         err_index_type_not_compatable_with_array_index_type_definition,
                                                         'x1]=22,[e4]=77); end');
      test_compile_error_generation_for_program_fragment('type e=(e1,e2,e3,e4);t=array [e2..e4] of int8; begin a:t = ([e2]=1,[e4]=22,[e4]=77); end',
                                                         create_TROMConstant,
                                                         format(err_out_of_order_index, ['e3']),
                                                         'e4]=22,[e4]=77); end');
   end;

procedure test_data_syntax_unit;
   begin
      display('=========================');
      display('TESTING DATA_SYNTAX_UNIT');
      display('========================');
      test_TROMConstant;
      display('')
   end;


END.
