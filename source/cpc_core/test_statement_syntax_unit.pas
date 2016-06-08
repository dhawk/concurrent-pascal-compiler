unit test_statement_syntax_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure test_statements;

implementation

uses test_main_form_unit, cpc_common_unit, cpc_blocks_unit, cpc_target_cpu_unit,
  cpc_statements_unit;

procedure test_routine_call_statement;
   begin
      display('testing routine call statement');
      test_only_for_successful_compilation('procedure x; begin end; begin x end.');
      test_only_for_successful_compilation('procedure x (i: Int8; b: boolean; c: char); begin end; begin x (5, true, ''a'') end.');
      test_compile_error_generation('procedure x; begin end; begin x() end.', err_procedure_has_no_parameters, '() end.');
   end;

procedure test_if_statement;
   var
      p: TProgram;
   procedure setup
      (s: string
      );
      begin
         p := setup_test_no_begin(s);
      end;
   procedure takedown;
      begin
         p.Release;
      end;
   var
      sl: TStatementList;
      ifstmt: TIfStatement;
   begin
      display('testing IF statement');
      // variations with empty stmts
      test_only_for_successful_compilation('begin if true then else if true then else if false then else end.'); // all, multiple elsifs
      test_only_for_successful_compilation('begin if true then else if true then end.'); // no else
      test_only_for_successful_compilation('begin if true then else end.');
      // no elsifs
      test_only_for_successful_compilation('begin if true then end.');
      // variations with single stmts
      test_only_for_successful_compilation('procedure x; begin end; begin if true then x else if true then x else if false then x else end.'); // all, multiple elsifs
      test_only_for_successful_compilation('procedure x; begin end; begin if true then x else if true then x end.'); // no else
      test_only_for_successful_compilation('procedure x; begin end; begin if true then x else x end.'); // no elsifs

      test_compile_error_generation('begin if true end.', err_then_expected, 'end.');

      // test dead code optimization - removal of false blocks
      setup('var i: int8; begin if false then else i:=1 end.');
      // should optimize to single assignment stmt
      if TStatementList(p.initial_statement).stmts[0].statement_kind <> assignment_statement then
         begin
            display('didnt optimize correctly #1');
            record_bad_test_result
         end;
      takedown;

      // test removal of multiple false blocks
      setup('procedure p; begin end; var i: int8; begin if false then p else if false then p else if true then i:= 1 else p end.');
      if TStatementList(p.initial_statement).stmts[0].statement_kind <> assignment_statement then // should optimize to single assignment stmt
         begin
            display('didnt optimize correctly #3');
            record_bad_test_result
         end;
      takedown;

      // test removal irrelevent blocks after a true block
      setup('procedure p; begin end; var b: boolean; begin if b then p else if b then p else if true then p else if b then p else b:=false end.');
      sl := TStatementList(p.initial_statement);
      assert(sl.stmts[0].statement_kind = if_statement);
      ifstmt := TIfStatement(sl.stmts[0]);
      if Length(ifstmt.conditional_statement_list) <> 2 then
         begin
            display('didnt optimize correctly/delete');
            record_bad_test_result
         end;
      if ifstmt.else_statement.statement_kind <> assignment_statement
      then
         if Length(ifstmt.conditional_statement_list) <> 2 then
            begin
               display('didnt optimize correctly/else');
               record_bad_test_result
            end;
      takedown;
   end;

procedure test_while_statement;
   begin
      display('testing WHILE statement');
      test_only_for_successful_compilation('begin loop while true repeat; loop repeat end.');
      test_only_for_successful_compilation('procedure x; begin end; begin loop while true; x repeat; loop repeat end.');
      test_compile_error_generation('begin loop while repeat end.', err_boolean_expression_expected, 'repeat end.');
      test_compile_error_generation('begin while end.', err_while_not_allowed_here, 'while end.');
   end;

procedure test_until_statement;
   begin
      display('testing WHILE statement');
      test_only_for_successful_compilation('begin loop until true repeat; loop repeat end.');
      test_only_for_successful_compilation('procedure x; begin end; begin loop until true; x repeat; loop repeat end.');
      test_compile_error_generation('begin loop until repeat end.', err_boolean_expression_expected, 'repeat end.');
      test_compile_error_generation('begin until end.', err_until_not_allowed_here, 'until end.');
   end;

procedure test_loop_statement;
   begin
      display('testing LOOP statement');
      test_only_for_successful_compilation('begin loop repeat end.');
      test_only_for_successful_compilation('procedure x; begin end; begin loop x repeat; x end.');
      test_only_for_successful_compilation('procedure x; begin end; begin loop x; repeat; x end.');
      test_only_for_successful_compilation('procedure x; begin end; begin loop x;x repeat; x end.');
   end;

procedure test_exitloop_statement;
   begin
      display('testing exitloop statement');
      test_only_for_successful_compilation('begin loop exitloop repeat; loop repeat end.');
      test_only_for_successful_compilation('begin loop exitloop if true repeat; loop repeat end.');
      test_compile_error_generation('begin exitloop end.', err_exitloop_only_allowed_inside_loop, 'exitloop end.');
      test_compile_error_generation('begin loop exitloop if repeat end.', err_boolean_expression_expected, 'repeat end.');
   end;

procedure test_reloop_statement;
   begin
      display('testing reloop statement');
      test_only_for_successful_compilation('begin loop reloop repeat; loop repeat end.');
      test_only_for_successful_compilation('begin loop reloop if true repeat; loop repeat end.');
      test_compile_error_generation('begin reloop end.', err_reloop_only_allowed_inside_loop, 'reloop end.');
      test_compile_error_generation('begin loop reloop if repeat end.', err_boolean_expression_expected, 'repeat end.');
   end;

procedure test_with_statement;
   begin
      display('testing with statement');
      test_only_for_successful_compilation('procedure x; var r: record a: int8 end; begin with r do end; begin end.');
      test_only_for_successful_compilation('procedure x; var r1: record a: int8 end; r2: record a: int8 end; begin with r1, r2 do end; begin end.');
      test_only_for_successful_compilation('procedure x; var r1: record a: int8 end; r2: record b: boolean end; begin with r1, r2 do r1.a := 5; r2.b := true end; begin end.');
      test_only_for_successful_compilation('type tr=record i: int8 end; var ov: overlay tr; ii: int8 end; var k: int8; begin with ov do begin i := k; k := i; ii := k; k := ii end end.');
      test_only_for_successful_compilation('var i: int8; pr: packed record j: int8 end; begin with pr do i := j end.');
   end;

procedure test_assignment_statement;
   begin
      display('testing assignment statement');
      test_only_for_successful_compilation('procedure x; var i: int8; begin i := 5 end; begin end.');

      test_compile_error_generation('procedure x (i: int8); begin i := 5 end; begin end.', err_cannot_change_constant_parameter, 'i := 5 end; begin end.');
      test_compile_error_generation('const i: int8 = 5; begin i := 6 end.', err_constant_not_allowed_here, 'i := 6 end.');

      // test special string rules
      test_compile_error_generation('type tc=class eeprom es: string [10]; property p: string; get: begin end; begin es := p end; begin end.', err_string_function_result_may_not_be_assigned_to_eeprom_variable, 'p end; begin end.');
      test_compile_error_generation('function f: string; begin end; type tc=class eeprom es: string [10]; begin es := f end; begin end.', err_string_function_result_may_not_be_assigned_to_eeprom_variable, 'f end; begin end.');
      test_compile_error_generation('type tc=class eeprom es: string [10]; property p: string; set: begin end; begin p := es end; begin end.', err_eeprom_string_cannot_be_assigned_to_a_property, 'es end; begin end.');
      test_compile_error_generation('rom r: string[3] = ''abc''; type tc=class property p: string; set: begin end; begin p := r end; begin end.', err_rom_string_cannot_be_assigned_to_a_property, 'r end; begin end.');
      test_compile_error_generation('type tc=class eeprom es: string [10]; property p: string; set: begin end; begin p := ''asdf'' end; begin end.', err_constant_string_cannot_be_assigned_to_a_property, '''asdf'' end; begin end.');
      test_compile_error_generation('function f: string; begin f := ''asdf'' end; type tc=class property p: string; set: begin end; begin p := f end; begin end.', err_string_function_result_cannot_be_assigned_to_a_property, 'f end; begin end.');
   end;

procedure test_continue_statement;
   begin
      display('testing continue statement');
      test_only_for_successful_compilation('type m=monitor var q: queue; procedure entry p; begin continue (q) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; begin continue q) end; begin end; begin end.', err_left_parenthesis_expected, 'q) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; var i: int8; begin continue (i) end; begin end; begin end.', err_queue_variable_expected, 'i) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; begin continue (q end; begin end; begin end.', err_right_parenthesis_expected, 'end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; begin continue (q) end; begin end.', err_continue_can_only_be_called_from_a_monitor_entry_routine, 'continue (q) end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; begin continue (q) end; begin end; begin end.', err_continue_can_only_be_called_from_a_monitor_entry_routine, 'continue (q) end; begin end; begin end.');
   end;

procedure test_delay_statement;
   begin
      display('testing delay statement');
      test_only_for_successful_compilation('type m=monitor var q: queue; procedure p; begin delay (q) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; begin delay q) end; begin end; begin end.', err_left_parenthesis_expected, 'q) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; var i: int8; begin delay (i) end; begin end; begin end.', err_queue_variable_expected, 'i) end; begin end; begin end.');
      test_compile_error_generation('type m=monitor var q: queue; procedure p; begin delay (q end; begin end; begin end.', err_right_parenthesis_expected, 'end; begin end; begin end.');
   end;

procedure test_init_statement;
   begin
      display('testing init statement');
      test_compile_error_generation('type mt=monitor (i: int8); begin end; pt=process (m: mt); priority 0; begin init m end; begin end; begin end.',
                                    err_system_type_variable_can_only_be_initialized_in_the_initial_statment_of_where_it_was_declared,
                                    'm end; begin end; begin end.');
      test_compile_error_generation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end;' +
                                    'p: process priority 1;' +
                                    '  var m2: monitor var q: queue; begin delay (q) end;' +
                                    '  begin init m2; loop await interrupt repeat end interrupt i;' +
                                    'begin init p end.',
                                    err_cant_call_delay_from_an_interrupt_process,
                                    'm2; loop'
                                   );
      test_compile_error_generation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end;' +
                                    'p: process priority 1;' +
                                    '  var m1: monitor var q: queue; function entry f: int8; begin delay(q) end; begin end;' +
                                    '  var m2: monitor (i: int8); begin end;' +
                                    '  begin init m2 (m1.f); loop await interrupt repeat end interrupt i;' +
                                    'begin init p end.',
                                    err_cant_call_delay_from_an_interrupt_process,
                                    'm1.f); loop'
                                   );
      test_compile_error_generation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end;' +
                                    'p: process priority 1;' +
                                    '  type tm1 = monitor var q: queue; function entry fd: int8; begin delay(q) end; begin end;' +
                                    '  var m1: array[1..3] of tm1;' +
                                    '  var m2: monitor (m: tm1); begin end;' +
                                    '  begin init m2 (m1[m1[1].fd]); loop await interrupt repeat end interrupt i;' +
                                    'begin init p end.',
                                    err_cant_call_delay_from_an_interrupt_process,
                                    'm1[m1[1].fd]'
                                   );
      test_compile_error_generation ('var m: monitor (b: boolean); function entry f: boolean; begin end; begin end; begin init m (m.f) end.',
                                     err_variable_not_initialized,
                                     'm.f) end.'
                                    );
      test_compile_error_generation ('var m: monitor var b: boolean; procedure entry p; begin end; begin b := true end; begin m.p end.',
                                     err_variable_not_initialized,
                                     'm.p end.'
                                    );
      test_compile_error_generation ('type tm=monitor var b: boolean; procedure entry p; begin end; begin b := true end; var p: process (m: tm); priority 0; begin loop m.p repeat end; m: tm; begin init p (m); end.',
                                     err_variable_not_initialized,
                                     'm); end.'
                                    );
   end;

procedure test_for_statement;
   begin
      display('testing for statement');
      test_only_for_successful_compilation('procedure p; begin end; var i: int8; begin for i := 1 to 8 do p end.');
      test_only_for_successful_compilation('type te=(a,b,c); procedure p; begin end; var e: te; begin for e := a to c do p end.');
      test_only_for_successful_compilation('procedure p; begin end; var i: int8; begin for i := 1 downto 8 do p end.');
      test_compile_error_generation('begin for 6', err_identifier_expected, '6');
      test_compile_error_generation('const a=4; begin for a end.',  err_loop_control_variable_expected, 'a end.');
      test_compile_error_generation('var r: real; begin for r end.', err_loop_control_variable_must_be_ordinal, 'r end.');
      test_compile_error_generation('type ct=class var i: int8; procedure p; begin for i := end.', err_for_loop_control_variable_must_be_local_variable, 'i := end.');
      test_compile_error_generation('type ct=class procedure p; var i: int8; begin for i := 1 to 5 do for i := 6', err_cannot_change_for_loop_control_variable, 'i := 6');
      test_compile_error_generation('var i: int8; begin for i end.', err_assignment_operator_expected, 'end.');
      test_compile_error_generation('var i: int8; begin for i := true end.', err_integer_expression_expected, 'true end.');
      test_compile_error_generation('var i: int8; begin for i := 1 end.', err_to_or_downto_expected, 'end.');
      test_compile_error_generation('var i: int8; begin for i := 1 to true end.', err_integer_expression_expected, 'true end.');
      test_compile_error_generation('var i: int8; begin for i := 1 to 8 end.', err_do_expected, 'end.');
   end;

procedure test_case_statement;
   var
      p: TProgram;
   procedure setup
      (s: string
      );
      begin
         p := setup_test_no_begin(s)
      end;
   procedure takedown;
      begin
         p.Release
      end;
      // var
      // sl: TStatementList;
      // casestmt: TCaseStatement;
   begin
      display('testing case statement');
      test_only_for_successful_compilation('begin case 5 of 1: end end.');
      test_only_for_successful_compilation('begin case 5 of 1,2: otherwise end end.');
      test_only_for_successful_compilation('begin case 5 of 1,2: ; 3,4: otherwise end end.');
      test_only_for_successful_compilation('var c: char; begin case c of ''a'': end end.');
      test_only_for_successful_compilation('var e: (a,b,c); begin case e of a: ; b: end end.');
      test_only_for_successful_compilation('var b: boolean; begin case b of true: ; false: end end.');
      test_only_for_successful_compilation('procedure p; begin end; begin case 5 of 1,2: p; 3,4: p otherwise p end end.');
      test_only_for_successful_compilation('begin case 5 of 1: otherwise end end.');
      test_only_for_successful_compilation('var i: int8; begin case i of 1..4: otherwise end end.');

      test_compile_error_generation('begin case 4.5 of ...',  err_ordinal_expression_expected, '4.5 of ...');
      test_compile_error_generation('begin case 4 begin.', err_of_expected, 'begin.');
      test_compile_error_generation('var a: int8; begin case a of true: ...', err_integer_constant_expected, 'true: ...');
      test_compile_error_generation('var a: char; begin case a of true: ...', err_char_constant_expected, 'true: ...');
      test_compile_error_generation('var a: char; begin case a of ''ab'': ...', err_char_constant_expected, '''ab'': ...');
      test_compile_error_generation('var a: boolean; begin case a of 1: ...', err_boolean_constant_expected, '1: ...');
      test_compile_error_generation('var a: (b,c); begin case a of true: ...', err_enum_constant_expected, 'true: ...');
      test_compile_error_generation('var a: (b,c,d); m:(n,o,p); begin case a of n: end end.', err_incompatible_enum_type, 'n: end end.');
      test_compile_error_generation('begin case 5 of 1 begin.', err_colon_expected, 'begin.');
      test_compile_error_generation('begin case 5 of 1,: end', err_case_label_expected, ': end');
      test_compile_error_generation('procedure p; begin end; begin case 5 of 1: p begin.', err_semicolon_expected, 'begin.');
      test_compile_error_generation('begin case 5 of 1: otherwise true', err_end_expected, 'true');
      test_compile_error_generation('begin case 5 of 1: ; 1: end.', err_duplicate_case_label, '1: end.');
      test_compile_error_generation('begin case true of true: ; true: end.',  err_duplicate_case_label, 'true: end.');
      test_compile_error_generation('var c: char; begin case c of ''a'': ; ''a'': end.', err_duplicate_case_label, '''a'': end.');
      test_compile_error_generation('var e:(a,b,c); begin case e of a: ; a: end.', err_duplicate_case_label, 'a: end.');
      test_compile_error_generation('var i: int8; begin case i of 1..5: ; 1: end.', err_duplicate_case_label, '1: end.');
      test_compile_error_generation('var i: int8; begin case i of 1..5: ; 3: end.', err_duplicate_case_label, '3: end.');
      test_compile_error_generation('var i: int8; begin case i of 1..5: ; 5: end.', err_duplicate_case_label, '5: end.');
      test_compile_error_generation('var i: int8; begin case i of 10..15: ; 5..10: end.', err_duplicate_case_label, '10: end.');
      test_compile_error_generation('var i: int8; begin case i of 10..15: ; 15..20: end.', err_duplicate_case_label, '20: end.');
      test_compile_error_generation('var i: int8; begin case i of 10..15: ; 13..20: end.', err_duplicate_case_label, '20: end.');

      test_compile_error_generation('var i: int8; begin case i of 20..15: ; 13..20: end.', err_less_than_first_of_range, '15: ; 13..20: end.');

      // test dead-code optimizations
      setup('var i: int8; begin case 5 of 1: ; 2: ; 5: i := 66 end end.');
      if TStatementList(p.initial_statement).stmts[0].statement_kind <>
      assignment_statement then
         begin
            display('didnt optimize correctly/sl');
            record_bad_test_result
         end;
      takedown;

      setup('var i: int8; begin case 50 of 1..5: ; 20: ; 40..50: i := 66 end end.');
      if TStatementList(p.initial_statement).stmts[0].statement_kind <>
      assignment_statement then
         begin
            display('didnt optimize correctly/sl');
            record_bad_test_result
         end;
      takedown;

      setup('var i: int8; begin case 5 of 1: ; 2: ; otherwise i := 66 end end.');
      if TStatementList(p.initial_statement).stmts[0].statement_kind <>
      assignment_statement then
         begin
            display('didnt optimize correctly/sl');
            record_bad_test_result
         end;
      takedown;

   end;

procedure test_await_statement;
   begin
      display('testing await statement');

      test_only_for_successful_compilation('type tp=process priority 1; begin loop await interrupt repeat end; begin end.');
      test_compile_error_generation ('begin await interrupt end.', err_await_interrupt_statement_must_be_in_process_code, 'await interrupt end.')
   end;

procedure test_string_attribute_routines;
   begin
      display('testing string attribute routines');
      test_compile_error_generation ('procedure p (s: string); begin s.strappend (', err_strappend_only_allowed_for_var_or_eeprom_variables, 'strappend (');
      test_compile_error_generation ('var s:string[2]; begin s.strappend(4) end.', err_string_variable_or_constant_expected, '4) end.');
      test_compile_error_generation ('rom s:string[2] = ''a''; begin s.strappend(4) end.', err_strappend_only_allowed_for_var_or_eeprom_variables, 'strappend(4) end.')
   end;

procedure test_statements;
   begin
      display('=============================');
      display('TESTING STATEMENT_SYNTAX_UNIT');
      display('=============================');
      test_routine_call_statement;
      test_if_statement;
      test_while_statement;
      test_until_statement;
      test_loop_statement;
      test_exitloop_statement;
      test_reloop_statement;
      test_with_statement;
      test_assignment_statement;
      test_continue_statement;
      test_delay_statement;
      test_init_statement;
      test_for_statement;
      test_case_statement;
      test_await_statement;
      test_string_attribute_routines;
      display('')
   end;

end.
