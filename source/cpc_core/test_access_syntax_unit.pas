unit test_access_syntax_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure test_TAccess;

implementation

uses
   cpc_definitions_unit, test_main_form_unit, cpc_access_unit, cpc_target_cpu_unit,
  cpc_statements_unit, cpc_core_objects_unit, cpc_expressions_unit,
  cpc_common_unit;

function access_generator: TDefinition;
   begin
      result := target_cpu.TAccess_CreateFromSourceTokens
   end;

function with_statement_generator: TDefinition;
   begin
      result := TWithStatement.CreateFromSourceTokens
   end;

procedure test_TAccess;
   var
      c: TAccess;
      w: TWithStatement;
      s: string;
   procedure setup_access;
      begin
         c := TAccess(test_fragment (s, access_generator));
      end;
   procedure setup_with;
      begin
         w := TWithStatement(test_fragment (s, with_statement_generator))
      end;
   procedure takedown_access;
      begin
         c.Free
      end;
   procedure takedown_with;
      begin
         w.Free
      end;
   procedure test_access_classification
      (ss: string;
       expected_access_kind:
       TAccessKind
      );
      begin
         s := ss;
         setup_access;
         if c.node_access_kind <> expected_access_kind then
            begin
               record_bad_test_result;
               display(s + ': failed wrong access kind');
            end;
         takedown_access
      end;
   procedure test_access_node_initialization_assumption_invalid_for_access
      (ss:
       string;
       expected_node_initialization_assumption_invalid: boolean
      );
      begin
         s := ss;
         setup_access;
         if c.node_initialization_assumption_invalid <>
         expected_node_initialization_assumption_invalid then
            begin
               record_bad_test_result;
               display(s +
               ': failed wrong node_initialization_assumption_invalid');
            end;
         takedown_access
      end;
   procedure test_access_node_initialization_assumption_invalid_for_with_access
      (ss:
       string;
       expected_node_initialization_assumption_invalid: boolean
      );
      var
         expr: TExpression;
      begin
         s := ss;
         setup_with;
         assert(w.statement.statement_kind = assignment_statement);
         expr := TAssignmentStatement(w.statement).expression;
         assert(expr is TVariableAccessPrimary);
         if TVariableAccessPrimary(expr).access.node_initialization_assumption_invalid <> expected_node_initialization_assumption_invalid then
            begin
               record_bad_test_result;
               display(s + ': failed wrong node_initialization_assumption_invalid');
            end;
         takedown_with
      end;
   begin
      display('===============');
      display('TESTING ACCESS ');
      display('===============');
      display('testing access classification');
      test_access_classification('var v: record a,b: int8 end; begin v end', variable_access);
      test_access_classification('var v: record a,b: int8 end; begin v.a end', variable_access);
      test_access_classification('var a: array [1..2, 3..4] of int8; begin a end', variable_access);
      test_access_classification('var a: array [1..2, 3..4] of int8; begin a[1] end', variable_access);
      test_access_classification('var a: array [1..2, 3..4] of int8; begin a[1][3] end', variable_access);
      test_access_classification('var a: array [1..2, 3..4] of int8; begin a[1,3] end', variable_access);
      test_access_classification('var v: record a: array [1..3] of int8 end; begin v.a end', variable_access);
      test_access_classification('var v: record a: array [1..3] of int8 end; begin v.a[2] end', variable_access);
      test_access_classification('var v: array [1..2] of record a: array [1..3] of int8 end; begin v[1] end', variable_access);
      test_access_classification('var v: array [1..2] of record a: array [1..3] of int8 end; begin v[1].a end', variable_access);
      test_access_classification('var v: array [1..2] of record a: array [1..3] of int8 end; begin v[1].a[2] end', variable_access);
      test_access_classification('var v: packed record i: 0..255 end; begin v end', variable_access);
      test_access_classification('var v: packed record i: 0..255 end; begin v.i end', variable_access);
      test_access_classification('procedure p; begin end; begin p end.', procedure_access);
      test_access_classification('function f: int8; begin end; begin f end.', function_access);
      test_access_classification('type tc = class public procedure p; begin end; begin end; var c: tc; begin c.p end.', procedure_access);
      test_access_classification('type tc = class public function f: int8; begin end; begin end; var c: tc; begin c.f end.', function_access);
      test_access_classification('type tc = class public property p: int8; set: begin end; begin end; var c: tc; begin c.p end.', property_access);

      test_access_classification('const c=5; begin c end.', constant_access);
      test_access_classification('type tc=record i,j: int8 end; const c:tc=(i=5,j=8); begin c.i end', structured_constant_access);
      test_access_classification('type tc=record i,j: int8 end; const c:tc=(i=5,j=8); begin c end', structured_constant_access);
      test_access_classification('type tc=record i: int8 end; tx=array[1..1] of tc; const c:tx=([1]=(i=5)); begin c[1] end', structured_constant_access);
      test_access_classification('type tc=record i: int8 end; tx=array[1..1] of tc; const c:tx=([1]=(i=5)); begin c[1].i end', structured_constant_access);
      test_access_classification('type tpr=packed record i: 0..255; j: 0..255 end; const c:tpr=(i=55,j=66); begin c end', structured_constant_access);
      test_access_classification('type tpr=packed record i: 0..255; j: 0..255 end; const c:tpr=(i=55,j=66); begin c.j end', constant_access);
      test_access_classification('type tpr=packed record e: (a=5,b=6) end; const c:tpr=(e=a); begin c.e end', constant_access);

      test_compile_error_generation_for_program_fragment('var v: record a,b: int8 end; begin v.x end', access_generator, err_field_identifier_expected, 'x end');
      test_compile_error_generation_for_program_fragment('var v: array [1..2] of record a: array [1..3] of int8 end; begin v[1) end', access_generator, err_right_bracket_expected, ') end');
      test_compile_error_generation_for_program_fragment('var v: array [1..2] of int8; begin v[1) end', access_generator, err_right_bracket_expected, ') end');
      test_compile_error_generation_for_program_fragment('var v: array [1..2] of int8; begin v[3] end', access_generator, err_expression_value_outside_legal_range, '3] end');
      test_compile_error_generation_for_program_fragment('var v: array [1..2] of int8; begin v[5.3] end', access_generator, err_integer_expression_expected, '5.3] end');

      display('testing access type');
      test_only_for_successful_compilation('procedure p; var i: int8; begin i := 5 end; begin end.');
      test_only_for_successful_compilation('type tr=record i: int8 end; procedure p; var r: tr; begin r.i := 5 end; begin end.');
      test_only_for_successful_compilation('type tr=record i: int8 end; procedure p; var r: tr; begin with r do i := 5 end; begin end.');
      test_only_for_successful_compilation('procedure p; var v: array [1..2] of int8; begin v[2] := 4 end; begin end.');
      test_only_for_successful_compilation('procedure p; var v: array [1..2, 3..4] of boolean; begin v[2,4] := false end; begin end.');
      test_only_for_successful_compilation('procedure p; var v: array [1..2, 3..4] of record b: boolean; end; begin v[2,4].b := false end; begin end.');
      test_only_for_successful_compilation('type tc=class public property i: int8; set: begin end; begin end; var c: tc; begin c.i := 5 end.');
      test_only_for_successful_compilation('type tc=class public procedure p; begin end; begin end; var c: tc; begin c.p end.');
      test_only_for_successful_compilation('type tc=class public procedure p; begin end; begin end; var c: tc; begin with c do p end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; var r: tr; begin r := tr:(i=4,j=6) end.');

      test_only_for_successful_compilation('const c:int8 = 9; var i:int8; begin i := c end.');
      test_only_for_successful_compilation('const c:real = 9; var r: real; begin r := c end.');
      test_only_for_successful_compilation('type str4=string[4]; const c:str4=''ab''; var s: str4; begin s := c end.');
      test_only_for_successful_compilation('type e=(e1,e2,e3); const c:e=e2; var v: e; begin v := c end.');
      test_only_for_successful_compilation('const c: boolean=false; var v: boolean; begin v := c end.');
      test_only_for_successful_compilation('type ts=set of 5..10; const c:ts=[5,9]; var v:ts; begin v := c end.');

      test_compile_error_generation('type tc=class public property i: int8; begin end; var c: tc; begin c.i := 5 end.', err_property_has_no_setter, 'i := 5 end.');
      test_compile_error_generation('type tc=class public property i: int8; begin end; var c: tc; begin with c do i := 5 end.', err_property_has_no_setter, 'i := 5 end.');
      test_compile_error_generation('type t=int8; function f1 (var i: int8): int8; begin end; procedure p2; var i: int8; begin i := f1(t) end; begin end.', err_colon_expected_for_anonymous_structured_constant, ') end; begin end.');
      test_compile_error_generation('type tm=monitor public procedure p; begin end; begin end; var m: monitor (mm: tm); begin init m end; begin end.', err_incomplete_type_definition_for_variable, 'm end; begin end.');


      // test overlay anonymous array access
      test_only_for_successful_compilation('var o: overlay array [1..2] of int8; array [''a''..''b''] of real end; i: int8; begin i := o[1] end.');
      test_compile_error_generation('var o: overlay array [1..2] of int8; array [''a''..''b'', ''c''..''d''] of real end; i: int8; begin i := o[''a'',''c''] end.', err_integer_expression_expected, 'o[''a'',''c''] end.');
      test_compile_error_generation('var o: overlay array [1..2] of int8; array [false..true,1..2] of real end; i: int8; begin i := o[''a''] end.', err_index_signature_does_not_match_any_overlay_variable_anonymous_signature, '] end.');

      // testing overlay variable access
      test_only_for_successful_compilation('type tov=overlay i: int8; record j: int8 end end; var r: record o: tov; i: int8 end; i: int8; begin  i := r.o.i end.');
      test_only_for_successful_compilation('type tov=overlay i: int8; record j: int8 end end; var r: record o: tov; i: int8 end; i: int8; begin  i := r.o.j end.');
      test_only_for_successful_compilation('type tov=record i: int8; j: int8 end; var r: overlay tov; ii: int8 end; i: int8; begin i := r.j end.');
      test_only_for_successful_compilation('type tov=overlay i: int8; packed record j: int8 end end; var r: record o: tov; i: int8 end; i: int8; begin  i := r.o.i end.');
      test_only_for_successful_compilation('type tov=overlay i: int8; packed record j: int8 end end; var r: record o: tov; i: int8 end; i: int8; begin  i := r.o.j end.');
      test_only_for_successful_compilation('type tov=packed record i: int8; j: int8 end; var r: overlay tov; ii: int8 end; i: int8; begin i := r.j end.');

      test_access_node_initialization_assumption_invalid_for_access('var i: int8; begin i end', false);
      test_access_node_initialization_assumption_invalid_for_access('var o: overlay i: int8 end; begin o.i end', true);
      test_access_node_initialization_assumption_invalid_for_access('type tr=record r: real end; var o: overlay tr end; begin o.r end', true);
      test_access_node_initialization_assumption_invalid_for_access('type tov=overlay i: int8 end; var r: record o: tov end; begin r.o.i end', true);
      test_access_node_initialization_assumption_invalid_for_with_access('var r: record i: int8 end; j: int8; begin with r do j := i end', false);
      test_access_node_initialization_assumption_invalid_for_with_access('var r: overlay i: int8 end; j: int8; begin with r do j := i end', true);
      test_access_node_initialization_assumption_invalid_for_with_access('type tov=overlay i: int8 end; var r: record o: tov end; j: int8; begin with r.o do j := i end', true);
      test_access_node_initialization_assumption_invalid_for_with_access('type tov=overlay i: int8 end; var r: record o: tov end; j: int8; begin with r do j := o.i end', true);

      display('testing structured constant access');

      s := 'type ta=array[5..7] of int8; const a:ta=([5]=50,[6]=60,[7]=70); begin a[6] end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 60) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      s := 'type te=(ea,eb,ec); ta=array[te] of int8; const a:ta=([ea]=50,[eb]=60,[ec]=70); begin a[eb] end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 60) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      s := 'type ta=array[''a''..''c''] of int8; const a:ta=([''a'']=50,[''b'']=60,[''c'']=70); begin a[''b''] end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 60) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      s := 'type ta=array[false..true] of int8; const a:ta=([false]=50,[true]=60); begin a[true] end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 60) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      s := 'type tr=record i,j: int8 end; const r:tr = (i=50,j=60); begin r.j end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 60) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      s := 'type tr=record i,j: int8 end; ta=array[3..5] of tr; const r:ta = ([3]=(i=53,j=63), [4]=(i=54,j=64), [5]=(i=55,j=65)); begin r[4].j end.';
      setup_access;
      if (c.node_access_kind <> structured_constant_access)
         or
         (c.node_structured_constant.StructuredConstantKind <> scSimple)
         or
         (c.node_structured_constant.simple_constant.constant_kind <> integer_constant)
         or
         (c.node_structured_constant.simple_constant.ordinal_value.AsInteger <> 64) then
         begin
            display('bad access str const result:' + s);
            record_bad_test_result
         end;
      takedown_access;

      display('')
   end;

end.
