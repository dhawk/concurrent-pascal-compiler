unit test_block_synax_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure test_block_syntax_unit;

implementation

uses test_main_form_unit, cpc_common_unit, SysUtils;

procedure test_TRoutine;
   begin
      display('testing TRoutine');
      test_only_for_successful_compilation('procedure x; begin end; begin end.');
      test_only_for_successful_compilation('function x (b: boolean): boolean; begin end; begin end.');
      test_only_for_successful_compilation('procedure x (a,b: boolean); begin end; begin end.');
      test_only_for_successful_compilation('procedure x (a: boolean; b: boolean); begin end; begin end.');
      test_only_for_successful_compilation('procedure x (var a: boolean); begin end; begin end.');
      test_only_for_successful_compilation('procedure x (rom a: boolean); begin end; begin end.');
      test_only_for_successful_compilation('procedure x (eeprom a: boolean); begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure x (ioreg io: tio); begin end; begin end.');

      test_only_for_successful_compilation('procedure x; var a,b: boolean; begin end; begin end.');
      test_only_for_successful_compilation('procedure x; var a: boolean;var b: boolean; begin end; begin end.');
      test_only_for_successful_compilation('procedure x; type t=int8; const c=5; var a: boolean;var b: t; begin end; begin end.');
      test_only_for_successful_compilation('type tc=monitor procedure p (var q: queue); begin end; public procedure p2; begin end; begin end; begin end.');

      test_compile_error_generation('procedure x; var x: boolean; begin end; begin end.', err_identifier_already_defined, 'x: boolean; begin end; begin end.');
      test_compile_error_generation('procedure begin); begin end; begin end.', err_identifier_expected, 'begin); begin end; begin end.');
      test_compile_error_generation('procedure x (begin); begin end; begin end.', err_identifier_expected, 'begin); begin end; begin end.');
      test_compile_error_generation('procedure x (a,begin); begin end; begin end.', err_identifier_expected, 'begin); begin end; begin end.');
      test_compile_error_generation('procedure x (b begin boolean); begin end; begin end.', err_colon_expected, 'begin boolean); begin end; begin end.');
      test_compile_error_generation('procedure x (b: begin boolean); begin end; begin end.', err_type_definition_expected, 'begin boolean); begin end; begin end.');
      test_compile_error_generation('procedure x (b: boolean;); begin end; begin end.', err_semicolon_not_allowed_here_before_parenthesis, ';); begin end; begin end');
      test_compile_error_generation('procedure x (b: boolean) begin end; begin end.', err_semicolon_expected, 'begin end; begin end');
      test_compile_error_generation('procedure x (b: boolean); end; begin end.', err_begin_expected, 'end; begin end');
      test_compile_error_generation('procedure x (b: boolean); begin end begin end.', err_semicolon_expected, 'begin end.');
      test_compile_error_generation('function x (b: boolean); begin end begin end.', err_colon_expected, '; begin end begin end.');
      test_compile_error_generation('function x (b: boolean): ; begin end begin end.', err_type_definition_expected, '; begin end begin end.');
      test_compile_error_generation('function f: queue; begin end; begin end.', err_function_result_must_be_simple_type, 'queue; begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure x (var io: tio); begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure x (rom io: tio); begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure x (eeprom io: tio); begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure x (io: tio); begin end; begin end.');
      test_compile_error_generation('type tc=monitor procedure p (q: queue); begin end; public procedure p2; begin end; begin end; begin end.', err_queue_param_must_be_var, 'queue');

      test_only_for_successful_compilation('function f: int8; begin end; procedure p; var i: int8; begin i := f end; begin end.');
      test_only_for_successful_compilation('function f (var i: int8; c: char): int8; begin end; procedure p; var i: int8; begin i := f (i, ''a'') end; begin end.');
      test_only_for_successful_compilation('type ct=class public property i: int8; get: begin end; begin end; var c:ct; i: int8; begin i := c.i end.');
      test_only_for_successful_compilation('function f(var i: int8): int8; begin end; procedure p; var r: record ii: int8 end; i: int8; begin with r do i := f (ii) end; begin end.');
      test_only_for_successful_compilation('function f(var i: int8): int8; begin end; procedure p; var r: record rr: record ii: int8 end end; i: int8; begin with r do with rr do i := f (ii) end; begin end.');

      test_compile_error_generation('function f: int8; begin end; procedure p; var i: int8; begin i := f() end; begin end', err_function_has_no_parameters, '() end; begin end');
      test_compile_error_generation('type ct=class public procedure proc; begin end; begin end; function p2 (var i:int8): int8; begin end; var c: ct; i: int8; begin i := p2(c.proc) end', err_routine_cant_be_used_as_parameter_here, 'proc) end');
      test_compile_error_generation('type ct=class public procedure proc; begin end; begin end; function p2 (var i:int8): int8; begin end; var c: ct; i: int8; begin with c do i := p2(proc) end', err_routine_cant_be_used_as_parameter_here, 'proc) end');
      test_compile_error_generation('type ct=class public function f: int8; begin end; begin end; function p2 (var i:int8): int8; begin end; var c: ct; i: int8; begin i := p2(c.f) end.', err_routine_cant_be_used_as_parameter_here, 'f) end.');
      test_compile_error_generation('type ct=class public function f: int8; begin end; begin end; function p2 (var i:int8): int8; begin end; var c: ct; i: int8; begin with c do i := p2(f) end.', err_routine_cant_be_used_as_parameter_here, 'f) end.');
      test_compile_error_generation('function f (rom i: int8): int8; begin end; procedure p2; var i: int8; begin i := f (i) end; begin end.', err_rom_constant_expected, 'i) end; begin end.');
      test_compile_error_generation('function f (eeprom i: int8): int8; begin end; procedure p2; var i: int8; begin i := f (i) end; begin end.', err_eeprom_variable_expected, 'i) end; begin end.');

      test_compile_error_generation('procedure p (q: queue); begin end; begin end.', err_queue_parameters_only_allowed_in_monitor_local_routines, 'queue); begin end; begin end.');
      test_compile_error_generation('procedure p; var q: queue; begin end; begin end.', err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor, 'queue; begin end; begin end.');
   end;

procedure test_TProperty;
   begin
      display('testing TProperty');
      test_only_for_successful_compilation('type m=monitor public property i: boolean; begin end; begin end.');
      test_only_for_successful_compilation('type m=monitor public property i: boolean; set: begin end; begin end; begin end.');
      test_only_for_successful_compilation('type m=monitor public property i: boolean; get: begin end; begin end; begin end.');
      test_only_for_successful_compilation('type m=monitor public property i: boolean; set: begin end; get: begin end; begin end; begin end.');
      test_only_for_successful_compilation('type m=monitor public property i: boolean; set: begin end; get: begin end; begin end; begin end.');

      test_compile_error_generation('type m=monitor public property begin end; set: begin end; begin end; begin end.', err_identifier_expected, 'begin end; set: begin end; begin end; begin end.');
      test_compile_error_generation('type m=monitor public property i: boolean; set: begin end; set: begin end; begin end; begin end.', err_property_setter_already_defined, 'set: begin end; begin end; begin end.');
      test_compile_error_generation('type m=monitor public property i boolean; begin end; begin end.', err_colon_expected, 'boolean; begin end; begin end.');
      test_compile_error_generation('type m=monitor public property i: begin end; begin end.', err_type_definition_expected, 'begin end; begin end.');
      test_compile_error_generation('type m=monitor public property i: boolean begin end; begin end.', err_semicolon_expected, 'begin end; begin end.');
   end;

procedure test_TSystemType;
   begin
      display('testing TSystemType');
      test_only_for_successful_compilation('type c=process priority 0; begin cycle repeat end; begin end.');
      test_only_for_successful_compilation('type c=monitor type b=boolean; public procedure x; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type c=class public procedure x; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type c=monitor public procedure x; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type c1=monitor procedure p1; begin end; public procedure p2; begin end; begin end; c2=monitor procedure p1; begin end; public procedure p2; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type ta=array[1..3] of queue; type tr=record a: ta end; m=monitor var r: tr; public procedure p; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; m=monitor(ioreg io: tio); public procedure p; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type c=monitor eeprom i: int8; r: real; public procedure p; begin end; begin end; begin end.');

      test_compile_error_generation('type tc=class (i:int8); public procedure p; begin end; begin end; var c: tc; var ii: int8; begin ii := c.i end.', err_property_or_routine_identifier_expected, 'i end.');
      test_compile_error_generation('type tc=class var i:int8; public procedure p; begin end;begin end; var c: tc; var ii: int8; begin ii := c.i end.', err_system_type_variables_are_private_to_the_system_type, 'i end.');

      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; m=monitor(rom io: tio); public procedure p; begin end; begin end; begin end.');
      test_compile_error_generation('type m=monitor (m1: m);public procedure p; begin end;  begin end; begin end.', err_self_referential_type, 'm);');
      test_compile_error_generation('type tc=class procedure p; var c: tc; begin end; begin end; begin end.', err_self_referential_type, 'tc; begin end; begin end; begin end.');
      test_compile_error_generation('type tc=class (c: tc); begin end; begin end.', err_self_referential_type, 'tc); begin end; begin end.');
      test_compile_error_generation('type c=process priority 1; property x: integer; begin end; begin end.', err_properties_not_allowed_in_processes, 'property x: integer; begin end; begin end.');

      test_compile_error_generation('type c=class var q: queue; begin end; begin end.', err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor, 'queue; begin end; begin end.');
      test_compile_error_generation('type c=process priority 1; var q: queue; begin end; begin end.', err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor, 'queue; begin end; begin end.');
      test_compile_error_generation('type ta=array[1..3] of queue; type tr=record a: ta end; m=class var r: tr; begin end; begin end.', err_queue_variables_may_only_be_declared_in_the_permanent_variables_of_a_monitor, 'tr; begin end; begin end.');

      test_compile_error_generation('type p=process begin', err_priority_expected, 'begin');
      test_compile_error_generation('type p=process priority begin', err_constant_expected, 'begin');
      test_compile_error_generation('type p=process priority 3.7 begin', err_integer_expected, '3.7 begin');
      test_compile_error_generation('type p=process priority 10; begin...', err_invalid_process_priority, '10; begin...');
      test_compile_error_generation('type p=process priority 5 begin...', err_semicolon_expected, 'begin...');

      test_only_for_successful_compilation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end; p: process priority 1; begin cycle await interrupt repeat end interrupt i; begin end.');
      test_compile_error_generation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end; p: process priority 0; begin cycle await interrupt repeat end interrupt i; begin end.',
                                    err_interrupt_process_priority_not_positive,
                                    'await'
                                   );
      test_compile_error_generation('var i: interrupt priority 1; function signalled: boolean; begin end; begin end; p: process priority 1; begin cycle repeat end interrupt i; begin end.',
                                    err_positive_priority_process_must_call_await_interrupt,
                                    'process'
                                   );
      test_only_for_successful_compilation('var i: interrupt priority 4; function signalled: boolean; begin end; begin end; p: process priority 4; begin cycle await interrupt repeat end interrupt i; begin end.');
      test_compile_error_generation('var i: interrupt priority 4; function signalled: boolean; begin end; begin end; p: process priority 3; begin cycle await interrupt repeat end interrupt i; begin end.',
                                    err_process_priority_must_be_matched_to_interrupt_priority,
                                    'i; begin end.'
                                   );
   end;

procedure test_TProgram;
   begin
      display('testing TProgram');
      test_only_for_successful_compilation('begin end.');

      test_only_for_successful_compilation('type tc=record i: int8; r: real end;' +
                                           'rom c:tc = (i=5, r=3.2);' + 'var i: int8; r: real;' +
                                           'begin i := c.i; r := c.r end.');
      test_only_for_successful_compilation('type tc=record i: int8; r: real end;' +
                                           'rom c:tc = (i=5, r=3.2);' + 'procedure p;' + 'var i: int8; r: real;' +
                                           'begin i := c.i; r := c.r end;' + 'begin end.');
      test_compile_error_generation('eeprom i: int8; begin end.', err_eeprom_variables_only_allowed_as_system_type_permanent_variables, 'eeprom i: int8; begin end.');
      test_compile_error_generation('var m: monitor var q: queue; public procedure p; begin delay(q) end; begin end;' +
                                    'begin m.p end.',
                                    err_cant_call_delay_from_initial_process,
                                    'p end.'
                                   );
   end;

procedure test_TParamList;
   begin
      display('testing TParamList');
      // with and without parameter lists:
      test_only_for_successful_compilation('type mt=monitor public procedure p; begin end; begin end; var m: mt; begin init m,m end.');
      test_only_for_successful_compilation('type mt=monitor (i: int8); public procedure p; begin end; begin end; var m: mt; begin init m(5) end.');

      // all legal system type init param lists, declaration and call
      test_only_for_successful_compilation('type tc=class public procedure p; begin end; begin end; tm=class (c: tc); public procedure p; begin end; begin end; begin end.');
      test_only_for_successful_compilation('type tc=class (r: real); public procedure p; begin end; begin end; var c: tc; begin init c(4.2) end.');
      test_only_for_successful_compilation('type tc=class (r: real); public procedure p; begin end; begin end; var c: tc; r: real; begin init c(r) end.');
      test_only_for_successful_compilation('type tc=class (r: real);public procedure p; begin end;  begin end; rom r: real = 5.2; var c: tc; begin init c(r) end.');
      test_only_for_successful_compilation('type tc=class (rom r: real); public procedure p; begin end;  begin end; rom r: real = 5.2; var c: tc; begin init c(r) end.');
      test_only_for_successful_compilation('type tc1=class public procedure p; begin end; begin end; tc2=class(c:tc1); public procedure p; begin end; begin end; var c1: tc1; c2: tc2; begin init c2(c1) end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; tc=class(r:tr); public procedure p; begin end; begin end; var r: tr; c: tc; begin init c(r) end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; tc=class(rom r: tr); public procedure p; begin end; begin end; rom r: tr=(i=5,j=6); var c: tc; begin init c(r) end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; tc=class (ioreg io: tio); public procedure p; begin end; begin end; ioreg io: tio at 1500; var c: tc; begin init c (io) end.');

      // all legal routine param lists, decl and call
      test_only_for_successful_compilation('procedure p (i: int16; r: real); begin end; begin p (5, 2.3) end.');
      test_only_for_successful_compilation('procedure p (var i1: int8; rom i2: int8); begin end; rom i2:int8 = 5; var i1: int8; begin p (i1, i2) end.');
      test_only_for_successful_compilation('type tm=monitor eeprom i: int8; procedure p (eeprom i: int8); begin end; public procedure p1; begin end; begin p(i) end; begin end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; tm=monitor eeprom i: tr; procedure p (eeprom i: tr); begin end; public procedure p1; begin end; begin p(i) end; begin end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; tm=monitor var i: tr; procedure p (var i: tr); begin end; public procedure p2; begin end; begin p(i) end; begin end.');
      test_only_for_successful_compilation('type tr=record i,j: int8 end; tm=monitor rom i: tr = (i=5,j=6); procedure p (rom i: tr); begin end; public procedure p2; begin end; begin p(i) end; begin end.');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure p(ioreg io: tio); begin end; ioreg io: tio at 1500; begin p(io) end.');
      test_only_for_successful_compilation('type tp=process priority 0; begin cycle repeat end; procedure pr (p: tp); begin end; var p: tp; begin init p; pr(p) end.');

      // anonymous overlay params
      test_only_for_successful_compilation('type tr=record i: int8 end; procedure p (r: tr); begin end; var o: overlay tr end; begin p(o) end.');

      // test parameter list declaration
      test_compile_error_generation('type mt=monitor (eeprom e: int8); public procedure p; begin end;  begin end; begin end.', err_eeprom_parameters_not_allowed_here, 'eeprom e: int8');
      test_compile_error_generation('type mt=monitor (var e: int8); public procedure p; begin end;  begin end; begin end.', err_var_parameters_not_allowed_here, 'var e: int8);');
      test_compile_error_generation('procedure p (var i: int8); begin end; procedure p2 (i: int8); begin p(i) end; begin end.', err_cannot_change_constant_parameter, 'i) end; begin end.');
      test_compile_error_generation('type tc=class public procedure p; begin end; begin end; tm=monitor (c: tc); begin end; begin end.', err_class_type_not_allowed, 'tc); begin end; begin end.');
      test_compile_error_generation('type tc=class public procedure p; begin end; begin end; tm=process (c: tc) priority 1; begin end; begin end.', err_class_type_not_allowed, 'tc) priority 1; begin end; begin end.');
      test_compile_error_generation('type tc=class public procedure p; begin end; begin end; tm=process (c tc) priority 1; begin end; begin end.', err_colon_expected, 'tc) priority 1; begin end; begin end.');
      test_compile_error_generation('type tc=class(q: queue); procedure p; begin end; begin end; begin end.', err_queue_parameters_only_allowed_in_monitor_local_routines, 'queue);');
      test_compile_error_generation('type tm=monitor procedure p (q: queue); public procedure p2; begin end; begin end; begin end; begin end.', err_queue_param_must_be_var, 'queue);');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; procedure p(io: tio); begin end; begin end.');
      test_compile_error_generation('procedure p (ioreg i: int8); begin end; begin end.', err_packed_record_type_expected, 'int8); begin end; begin end.');
      test_compile_error_generation('type tc=class public procedure p; begin end; begin end; procedure p(var c: tc); begin end; begin end.', err_system_type_parameter_must_be_constant_parameter, 'tc); begin end; begin end.');

      // test parsing of parameter assembly
      test_compile_error_generation('type mt=monitor public procedure p; begin end; begin end; var m: mt; begin init m (5) end.', err_no_parameters_expected, '(5) end.');
      test_compile_error_generation('type mt=monitor (i: int8); public procedure p; begin end; begin end; var m: mt; begin init m end.', err_left_parenthesis_expected, 'end.');
      test_compile_error_generation('function f: int8; begin end; procedure p (var i: int8); begin end; begin p (f) end.', err_routine_cant_be_used_as_parameter_here, 'f) end.');
      test_compile_error_generation('type mt=monitor public property i: int8; get: begin end; begin end; procedure p (var i: int8); begin end; var m: mt; begin p(m.i) end.', err_property_cant_be_used_as_parameter_here, 'i) end.');
      test_compile_error_generation('const c:boolean=true; procedure p(var i: int8); begin end; begin p (c) end.', err_constant_cant_be_used_as_parameter_here, 'c) end.');
      test_compile_error_generation('procedure p1(var i: int8); begin end; procedure p2 (i: int8); begin p1 (i) end; begin end.', err_cannot_change_constant_parameter, 'i) end; begin end.');
      test_compile_error_generation('rom i:int8 = 5; procedure p(var j: int8); begin end; begin p(i) end.', err_cannot_change_rom_constant, 'i) end.');
      test_compile_error_generation('procedure p (var i: int8); begin end; var i: int8; begin for i := 1 to 5 do p(i) end.', err_cannot_change_for_loop_control_variable, 'i) end.');
      test_compile_error_generation('procedure p(var i: int8); begin end; type ct=class eeprom i: int8; begin p(i) end; begin end.', err_ram_variable_expected, 'i) end; begin end.');
      test_compile_error_generation('procedure p(eeprom i: int8); begin end; var i: int8; begin p(i) end.', err_eeprom_variable_expected, 'i) end.');
      test_compile_error_generation('type tio=packed record a: 0..255 end; procedure p(ioreg io: tio); begin end; var i: int8; begin p(i) end.', err_ioregister_expected, 'i) end.');
      test_compile_error_generation('procedure p (rom i: int8); begin end; var i: int8; begin p(i) end.', err_rom_constant_expected, 'i) end.');
      test_compile_error_generation('type t1=record i:int8 end; t2=record i: int8 end; procedure p(var r: t1); begin end; var r: t2; begin p(r) end.', err_wrong_type, 'r) end.');
      test_compile_error_generation('procedure p (i,j: int8); begin end; var i,j: int8; begin p (i j) end.', err_comma_expected, 'j) end.');
      test_compile_error_generation('procedure p (i: int8); begin end; var i: int8; begin p(i end.', err_right_parenthesis_expected, 'end.');
      test_compile_error_generation('type tr=record i,j: int8 end; procedure p(r: tr); begin end; type tc=class eeprom r: tr; begin p(r) end; begin end.', err_eeprom_parameter_only_allowed_with_eeprom_parameter_descriptor, 'r) end; begin end.');
      test_compile_error_generation('type tr=record i,j:int8 end; procedure p(r: tr); begin end; rom r:tr=(i=55,j=66); begin p(r) end.', err_rom_constant_only_allowed_with_rom_parameter_descriptor, 'r) end.');
   end;

procedure test_TVARList;
   begin
      display('testing TVARList');
      test_only_for_successful_compilation('var t: int8 := 5; begin end.');

      test_compile_error_generation('var t: int8 at 5; begin end.', err_at_only_allowed_for_ioregisters, 'at 5; begin end.');
      test_compile_error_generation('type mt=monitor public procedure p; begin end; begin end; type mt2=monitor eeprom m: mt; begin end; begin end.', err_system_type_variable_must_be_ram, 'mt; begin end; begin end.');
      test_compile_error_generation('type mt=monitor public procedure p; begin end; begin end; rom m: mt=...', err_illegal_type_for_rom_constant, 'mt=...');
      test_compile_error_generation('type tm=monitor public procedure p; begin end; begin end; procedure p; var m: tm; begin end; begin end.', err_monitor_variable_not_allowed_as_routine_local_variable, 'tm; begin end; begin end.');

      test_compile_error_generation('type tm=monitor eeprom q: queue; public procedure p; begin end; begin end; begin end.', err_queue_type_not_allowed_here, 'queue;');
   end;

procedure test_TIORegisterList;
   begin
      display('testing TIORegisterList');
      test_only_for_successful_compilation('type tio=packed record a: 0..255 end; ioreg t: tio at 1050; begin end.');

      test_compile_error_generation('type tio=packed record a: 0..255 end; procedure p; ioreg t: tio at 8; begin end; begin end.', err_ioregisters_must_be_declared_at_global_level, 'ioreg t: tio at 8; begin end; begin end.');
      test_compile_error_generation('type tio=packed record a: 0..255 end; ioreg t,t2: tio at 50; begin end.', err_multiple_items_not_allowed_here, ',t2: tio at 50; begin end.');
      test_compile_error_generation('type tio=packed record a: 0..255 end; ioreg t: tio; begin end.', err_at_expected, '; begin end.');
      test_compile_error_generation('type tio=packed record a: 0..255 end; ioreg t: tio at 5.5; begin end.', err_invalid_address, '5.5; begin end.');
      test_compile_error_generation('type tio=packed record a: 0..255 end; ioreg t: tio at 50; begin end.', err_invalid_ioregister_address, '50; begin end.');
      test_compile_error_generation('type tio=packed record a: 0..256 end; ioreg t: tio at 50; begin end.', err_invalid_total_ioregister_width, 'tio at 50; begin end.');
      test_compile_error_generation ('type t=overlay packed record i: int7 end end; ioreg i: t; begin end.',
                                     err_invalid_total_ioregister_width,
                                     't; begin end.'
                                    );
      test_compile_error_generation ('type t=overlay packed record i: int8 end; packed record j: int16 end end; ioreg i: t; begin end.',
                                     err_all_ioreg_overlay_type_packed_records_must_be_same_width,
                                     't; begin end.'
                                    );

      // test for overlapping iovars
      test_only_for_successful_compilation('type tio1=packed record i: 0..65535 end; tio2=packed record i: 0..16777215 end; ioreg io1: tio1 at 1500; io2: tio2 at 1502; begin end.');
      test_only_for_successful_compilation('type tio1=packed record i: 0..65535 end; tio2=packed record i: 0..16777215 end; ioreg io1: tio1 at 1503; io2: tio2 at 1500; begin end.');
   end;

procedure test_interrupt;
   begin
      display ('testing interrupt definition');

      // test interrupt syntax in TSystemType.CreateFromSourceTokens
      test_only_for_successful_compilation ('var i: interrupt priority 1; function signalled: boolean; begin end; begin end; begin end.');
      test_compile_error_generation ('var i: interrupt priority 0; function signalled: boolean; begin end; begin end; begin end.',
                                      err_interrupt_priority_must_be_greater_than_0,
                                      '0'
                                    );
      test_compile_error_generation ('type t=interrupt priority 1; eeprom',
                                     err_eeprom_variables_not_allowed_in_interrupt_definitions,
                                     'eeprom'
                                    );
      test_compile_error_generation ('type c=class ioreg',
                                     err_ioregister_not_allowed,
                                     'ioreg'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; property',
                                     err_properties_not_allowed_in_interrupt_definitions,
                                     'property'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function signalled: boolean; begin end; function x',
                                     err_only_one_routine_allowed_in_interrupt_definition,
                                     'function x'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; procedure x',
                                     err_procedures_not_allowed_in_interrupt_definitions,
                                     'procedure x'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function x',
                                     err_signalled_function_required,
                                     'x'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function signalled +',
                                     err_colon_expected,
                                     '+'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function signalled: int8; begin end; function x',
                                     err_signalled_function_result_must_be_boolean,
                                     'int8'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function signalled: boolean; begin end; x',
                                     err_begin_expected,
                                     'x'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; var x: int8; then',
                                     err_begin_expected,
                                     'then'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; begin end;',
                                     err_interrupt_definition_must_implement_signalled_function,
                                     'interrupt'
                                    );

      test_only_for_successful_compilation ('type ti=interrupt priority 1; function signalled: boolean; begin end; begin end; tp=process (i: ti); priority 1; begin cycle await interrupt repeat end; var i: ti; p: tp interrupt i; begin init p (i) end.');
      test_compile_error_generation ('type tp=process priority 1; begin cycle await interrupt repeat end; var i: interrupt priority 1; function signalled: boolean; begin end; begin end; p1: tp interrupt i; p2: tp interrupt i; begin end.',
                                     err_interrupt_variable_already_assigned_to_another_process,
                                     'i; begin end.'
                                    );
      test_compile_error_generation ('type ti=interrupt priority 1; function signalled: boolean; begin end; begin end; var i,j: ti; begin end.',
                                     err_an_instance_of_this_interrupt_type_already_exists,
                                     'j: ti; begin end.'
                                    );
   end;

procedure test_global_var_access_rules;
   begin
      display ('testing global variable access rules');
      // ioreg are accessible from interrupt type routines & init and also in program init
      test_only_for_successful_compilation ('ioreg io: packed record flag: 0..1; x: 0..127 end at 1000;'
                                               + 'type tinterrupt = interrupt priority 1;'
                                               + 'function signalled: boolean; begin result := io.flag = 1 end;'
                                               + 'begin if io.flag = 2 then end;'
                                               + 'begin if io.flag = 4 then end.'
                                           );
      // global vars are not accessible from interrupt routines
      test_compile_error_generation ('var i: int8;'
                                         + 'type tinterrupt = interrupt priority 1;'
                                         + 'function signalled: boolean; begin result := i = 1 end;',  // in routine
                                      err_global_variable_not_accessible_here,
                                     'i = 1'
                                    );
      test_compile_error_generation ('var i: int8;'
                                         + 'type tinterrupt = interrupt priority 1;'
                                         + 'function signalled: boolean; begin end;'
                                         + 'begin if i = 2 then end;',
                                      err_global_variable_not_accessible_here,
                                     'i = 2'
                                    );
      // globals are not accessible from global routines
      test_compile_error_generation ('var i: int8;'
                                         + 'procedure p; begin if i = 2 then end;',
                                      err_global_variable_not_accessible_here,
                                     'i = 2'
                                    );
   end;

procedure test_param_type_rules;
   begin
      display ('testing param type rules');
      test_only_for_successful_compilation ('type tt=record i: int8 end; tc=class public procedure p (t: tt); begin end; begin end; begin end.');
      test_compile_error_generation ('type tc=class type tt=record i: int8 end; public procedure p (t: tt);',
                                     err_local_type_not_accessible_to_caller_of_public_routine,
                                     'tt);');
      test_only_for_successful_compilation ('procedure p (s: string[10]); begin end; begin end.');
      test_compile_error_generation ('procedure p(var s: string[',
                                     err_string_dimension_not_allowed_for_var_string_parameter,
                                     '[');
      test_compile_error_generation ('procedure p(rom s: string[',
                                     err_string_dimension_not_allowed_for_rom_string_parameter,
                                     '[');
      test_compile_error_generation ('type tc=class procedure p (eeprom s: string[',
                                     err_string_dimension_not_allowed_for_eeprom_string_parameter,
                                     '[');
      test_compile_error_generation ('procedure p (ioreg s: string',
                                     err_packed_record_type_expected,
                                     'string');
      test_only_for_successful_compilation ('procedure p (s: set of 0..3); begin end; begin end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec);tc=class public procedure p (s: set of te); begin end; begin end; begin end.');
      test_compile_error_generation ('type tc=class type te=(ea,eb,ec); public procedure p (s: set of te); begin end; begin end; begin end.',
                                     err_local_type_not_accessible_to_caller_of_public_routine,
                                     'set of te');
      test_compile_error_generation ('type te=(ea,eb,ec,ed);procedure p (var s: set of te); begin end;var ss: set of te;begin p (ss)end.',
                                     err_anonymous_type_can_only_be_a_constant_parameter,
                                     'set of te');
      test_compile_error_generation ('type te=(ea,eb,ec,ed);procedure p (ioreg s: set of te); begin end;var ss: set of te;begin p (ss)end.',
                                     err_packed_record_type_expected,
                                     'set of te');
      test_only_for_successful_compilation ('procedure p (i: 10..13); begin end; begin p(10) end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec,ed,ee);procedure p (e: eb..ed); begin end; begin p(ec) end.');
      test_compile_error_generation ('type te=(ea,eb,ec,ed,ee);procedure p (var e: eb..ed); begin end; begin p(ec) end.',
                                     err_anonymous_type_can_only_be_a_constant_parameter,
                                     'eb..ed);');
      test_compile_error_generation ('type te=(ea,eb,ec,ed,ee);procedure p (ioreg e: eb..ed); begin end; begin p(ec) end.',
                                     err_packed_record_type_expected,
                                     'eb..ed);');
      test_compile_error_generation ('procedure p (s: begin',
                                     err_type_definition_expected,
                                     'begin');
      test_compile_error_generation ('type tc=class type te=(ea,eb,ec,ed); public procedure p(e: eb..ec); begin end; begin end; begin end.',
                                     err_local_type_not_accessible_to_caller_of_public_routine,
                                     'eb..ec); begin');
      test_only_for_successful_compilation ('type te=(ea,eb,ec,ed); tc=class public procedure p(e: eb..ec); begin end; begin end; begin end.');
      test_only_for_successful_compilation ('type tc=class type tr=real; public procedure p (r: tr); begin end; begin end; begin end.');
      test_only_for_successful_compilation ('type tc=class type tc=char; public procedure p (c: tc); begin end; begin end; begin end.');
      test_only_for_successful_compilation ('type tc=class type ts=string[10]; public procedure p (s: ts); begin end; begin end; begin end.');
      test_only_for_successful_compilation ('type tc=class type ts=set of 5..10; public procedure p (s: ts); begin end; begin end; var c: tc; begin c.p([6]) end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec,ed); tc=class type tee=eb..ec; public procedure p (e: set of tee); begin end; begin end; var c: tc; begin c.p([eb,ec]) end.');
      test_only_for_successful_compilation ('type te=(ea,eb,ec,ed); tc=class type tee=set of eb..ec; public procedure p (e: tee); begin end; begin end; var c: tc; begin c.p([eb,ec]) end.');
   end;

procedure test_block_syntax_unit;
   begin
      display ('=========================');
      display ('TESTING BLOCK SYNTAX UNIT');
      display ('=========================');
      test_TRoutine;
      test_TProperty;
      test_TSystemType;
      test_TProgram;
      test_TParamList;
      test_TVARList;
      test_TIORegisterList;
      test_interrupt;
      test_global_var_access_rules;
      test_param_type_rules;
      display('')
   end;

end.
