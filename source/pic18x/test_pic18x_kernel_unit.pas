UNIT test_pic18x_kernel_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   Classes, pic18x_instructions_unit;

var
   GenerateKernelTestCoverageMap: boolean;
   StartKernelCoverageTest, EndKernelCoverageTest: TInstruction;
   KernelInstructions:
      array of
         record
            instrxxx: string;
            classname: string;
            execution_count: integer
         end;

procedure construct_kernel_test (nop: TPIC18x_NOP; tokens: TStringList);
procedure do_kernel_test (nop: TPIC18x_NOP);
procedure run_kernel_tests;

IMPLEMENTATIOn

uses
  pic18x_kernel_unit, pic18x_blocks_unit, cpc_blocks_unit, SysUtils,
  pic18x_core_objects_unit, cpc_core_objects_unit, test_pic18x_simulator_unit,
  test_pic18x_compiler_main_form_unit;

procedure construct_kernel_test (nop: TPIC18x_NOP; tokens: TStringList);

   function process_pcb_addr (process_name: string): integer;
      var
         i: integer;
      begin
         assert (length(process_name) = 1);
         if process_name[1] = '@' then
            begin
               result := initial_process_pcb_addr;
               exit
            end;
         for i := 0 to TPIC18x_Program(the_program).program_vars.Length-1 do
            if LowerCase(TPIC18x_Program(the_program).program_vars[i].name) = process_name then
               begin
                  assert (TPIC18x_Program(the_program).program_vars[i].typedef.type_kind = system_type, process_name + ' is not a system type');
                  assert (TSystemType(TPIC18x_Program(the_program).program_vars[i].typedef).system_type_kind = process_system_type, 'not a process: ' + process_name);
                  result := TPIC18x_Variable(TPIC18x_Program(the_program).program_vars[i]).pcb_address;
                  exit
               end;
         assert (false, 'no such process ' + process_name);
         result := 0;  // suppress compiler warning
      end;

   function mon_var (mon_name: string): TVariable;
      var
         i: integer;
      begin
         assert (Length(mon_name) = 1);
         for i := 0 to TPIC18x_Program(the_program).program_vars.Length-1 do
            if LowerCase(TPIC18x_Program(the_program).program_vars[i].name) = mon_name then
               begin
                  assert (TPIC18x_Program(the_program).program_vars[i].typedef.type_kind = system_type, mon_name + ' is not a system type');
                  assert (TSystemType(TPIC18x_Program(the_program).program_vars[i].typedef).system_type_kind = monitor_system_type, 'not a monitor: ' + mon_name);
                  result := TPIC18x_Program(the_program).program_vars[i];
                  exit
               end;
         assert (false, 'no such monitor ' + mon_name + ' in global variables');
         result := nil  // suppress compiler warning
      end;

   procedure check_ready_queue (priority: integer; values: string);
      var
         i: integer;
      begin
         assert (length(values) > 0);
         if values[1] = '@' then
            begin
               assert (length(values) = 1);
               nop.add_check_value (PriorityMapper.ReadyQueueAddr(priority), process_pcb_addr(values[1]), format ('queue[$%2.2X].running[0] <> @%s_pcb', [priority, values[1]]));
            end
         else
            begin
               assert (CharInSet (values[1], ['!', '#', '*', '@']));
               nop.add_check_value (PriorityMapper.ReadyQueueAddr(priority), process_pcb_addr(values[2]), format ('queue[$%2.2X].running[0] <> @%s_pcb', [priority, values[2]]));
               for i := 2 to Length(values)-1 do
                  begin
                     assert (values[i] <> '@');
                     nop.add_check_value (process_pcb_addr(values[i]), process_pcb_addr(values[i+1]), format ('queue[$%2.2X].running[%d] <> @%s_pcb', [priority, i, values[i+1]]))
                  end
            end;
         nop.add_check_value (process_pcb_addr(values[Length(values)]), 0, format ('queue[$%2.2X].running[%d] <> nil', [priority, Length(values)]))
      end;

   procedure check_monitor_queue (mon_name: char; values: string);
      var
         i: integer;
         mon: TVariable;
      begin
         assert (length(values) > 0);
         mon := mon_var(mon_name);
         if length(values) = 1 then
            assert (CharInSet(values[1], ['0','1']))
         else
            assert (values[Length(values)] = '1');

         case values[1] of
            '0': nop.add_check_value (mon, 0, format('monitor %s.gate[%d] <> $%2.2X', [mon_name, 1, 0]));
            '1': nop.add_check_value (mon, $FF, format('monitor %s.gate[%d] <> $%2.2X', [mon_name, 1, $FF]));
         else
            nop.add_check_value (mon, process_pcb_addr(values[1]), format('monitor %s.gate[%d] <> @%s_pcb @$%2.2X',[mon_name, 1, values[1], process_pcb_addr(values[1])]))
         end;
         for i := 2 to Length(values) do
            case values[i] of
               '0': nop.add_check_value (process_pcb_addr(values[i-1]), 0, format('monitor %s.gate[%d] <> $%2.2X', [mon_name, i, 0]));
               '1': nop.add_check_value (process_pcb_addr(values[i-1]), $FF, format('monitor %s.gate[%d] <> $%2.2X', [mon_name, i, $FF]));
            else
               nop.add_check_value (process_pcb_addr(values[i-1]), process_pcb_addr(values[i]), format('monitor %s.gate[%d] <> @%s_pcb @$%2.2X',[mon_name, i, values[i], process_pcb_addr(values[i])]))
            end
      end;

   var
      i: integer;
      found: boolean;
   begin
      if tokens[0] = 'test_rq' then
         begin
            assert (tokens.Count = 6);
            // test (nnn, 'test_rq !abc@ 0 #def *g *h')   for prio 2, 1, 0, -1 and -2 process queues; 0 means empty ready queue, @ for init process
            //   ! = preempted
            //   # = suspended
            //   * = running
            nop.test_kind := test_rq;

            // check current_prio
            found := false;
            for i := 2 downto -2 do
               if (not found)
                  and
                  (PriorityMapper.PrioUsed(i))
               then
                  if tokens[3-i] <> '0'
                  then
                     begin
                        found := true;
                        nop.add_check_value (current_prio, PriorityMapper.ReadyQueueAddr(i), format ('current_prio <> %d', [i]))
                     end;
            if not found
            then
               nop.add_check_value (current_prio, PriorityMapper.ReadyQueueAddr(initial_process_priority), 'current_prio <> init');

            // check priority queues
            for i := 2 downto -2 do
               begin
                  if (Length(tokens[3-i]) > 1) then
                     begin
                        assert (CharInSet (tokens[3-i][1], ['!', '#', '*']));
                        nop.check_state_value (PriorityMapper.ReadyQueueAddr(i), tokens[3-i][1])
                     end;
                  if PriorityMapper.PrioUsed(i) then
                     if tokens[3-i] = '0' then
                        nop.add_check_value (PriorityMapper.ReadyQueueAddr(i), 0, format ('queue[%d].running[0] <> nil', [i]))
                     else
                        check_ready_queue (i, tokens[3-i])
                  else
                     assert (tokens[3-i] = '0')
               end;

            // check initial process ready queue
            found := false;
            for i := 2 downto -2 do
               if pos('@', tokens[3-i]) > 0 then
                  found := true;
            if found then
               nop.add_check_value (PriorityMapper.ReadyQueueAddr(initial_process_priority), 0, 'ready[init].running <> nil')
            else
               begin
                  nop.add_check_value (PriorityMapper.ReadyQueueAddr(initial_process_priority), initial_process_pcb_addr, 'queue[init].running[0] <> @initial_process_pcb_addr');
                  nop.add_check_value (initial_process_pcb_addr, 0, 'queue[init].running[1] <> nil')
               end
         end
      else if tokens[0] = 'test_mon_gate' then
         begin
            // test (nnn, 'test_mon_gate m abc')   ; 0 by itself means nil, otherwise {processes}1 where 1 is $01 at end
            nop.test_kind := test_mon_gate;
            assert (tokens.Count = 3);
            assert (Length(tokens[1]) = 1);
            check_monitor_queue (tokens[1][1], tokens[2])
         end
      else if tokens[0] = 'interrupt' then
         begin
//            nop.test_kind := interrupt_test
         end
      else
         assert (false)
   end;

procedure do_kernel_test (nop: TPIC18x_NOP);
   var
      i: integer;
      test_shown: boolean;
   begin
      test_shown := false;
      case nop.test_kind of
         test_rq,
         test_mon_gate:
            for i := 0 to Length(nop.check_values)-1 do
               if nop.check_values[i].v <> nil then
                  begin
                     if cpu.ram[TPIC18x_Variable (nop.check_values[i].v).address - $3E] <> nop.check_values[i].expected_value then
                        begin
                           if not test_shown then
                              begin
                                 MainForm.TestResultsMemo.Lines.Add (format ('failed %s', [nop.annotation]));
                                 test_shown := true
                              end;
                           MainForm.TestResultsMemo.Lines.Add (format ('   %s.gate addr $%3.3X: expected $%2.2X, found $%2.2X  %s',
                                                                       [nop.check_values[i].monitor_name,
                                                                        TPIC18x_Variable (nop.check_values[i].v).address - $3E,
                                                                        nop.check_values[i].expected_value,
                                                                        cpu.ram[TPIC18x_Variable (nop.check_values[i].v).address - $3E],
                                                                        nop.check_values[i].error_message
                                                                       ]
                                                                      )
                                                              )
                        end
                  end
               else if nop.check_values[i].state_test then
                  begin
                     if (cpu.ram[nop.check_values[i].addr+1] and $C0) <> nop.check_values[i].expected_value then
                        begin
                           if not test_shown then
                              begin
                                 MainForm.TestResultsMemo.Lines.Add (format ('failed %s', [nop.annotation]));
                                 test_shown := true
                              end;
                           MainForm.TestResultsMemo.Lines.Add (format ('   addr $%3.3X: expected state $%2.2X, found state $%2.2X %s',
                                                                       [nop.check_values[i].addr+1,
                                                                        nop.check_values[i].expected_value,
                                                                        (cpu.ram[nop.check_values[i].addr+1] and $C0),
                                                                        nop.check_values[i].error_message
                                                                       ]
                                                                      )
                                                              )
                        end
                  end
               else if cpu.ram[nop.check_values[i].addr] <> nop.check_values[i].expected_value then
                  begin
                     if not test_shown then
                        begin
                           MainForm.TestResultsMemo.Lines.Add (format ('failed %s', [nop.annotation]));
                           test_shown := true
                        end;
                     MainForm.TestResultsMemo.Lines.Add (format ('   addr $%3.3X: expected $%2.2X, found $%2.2X %s',
                                                                 [nop.check_values[i].addr,
                                                                  nop.check_values[i].expected_value,
                                                                  cpu.ram[nop.check_values[i].addr],
                                                                  nop.check_values[i].error_message
                                                                 ]
                                                                )
                                                        )
                  end;
         test_not_implemented:
            MainForm.TestResultsMemo.Lines.Add ('not implemented: ' + nop.msg);
//         interrupt_test:
//            begin
//                           MainForm.TestResultsMemo.Lines.Add (format ('subtest %d failed: %s', [nop.subtest, nop.annotation]));
//         ;      i := ord(test_shown)
//            end;
      else
         assert (false)
      end
   end;

procedure test1;   // enter/exit same priority monitor
   begin
      add ('var');
      add ('   arr: array [0..300] of int8;');
      add ('   m: monitor');
      add ('      var q: queue;');
      add ('      procedure entry p;');
      add ('         begin');
      add ('            test (101, ''test_mon_gate m 1'');');
      add ('            test (102, ''test_rq 0 0 0 0 0'')');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('begin');
      add ('   arr[5] := 5;');  // mark as reachable
      add ('   test (100, ''test_mon_gate m 0'');');
      add ('   m.p;');
      add ('   test (103, ''test_rq 0 0 0 0 0'');');
      add ('   test (104, ''test_mon_gate m 0'');');
      add ('   m.p;');
      add ('   test (105, ''test_rq 0 0 0 0 0'');');
      add ('   test (106, ''test_mon_gate m 0'')');
      add ('end.');
      start_test (1);
      conclude_test
   end;

procedure test2;   // enter/exit higher priority moniitor
   begin
      add ('type');
      add (' tm= monitor');
      add ('       procedure entry p;');
      add ('         begin');
      add ('            test (1, ''test_rq 0 0 @ 0 0'');');
      add ('            test (2, ''test_mon_gate m 1'')');
      add ('         end;');
      add ('       begin');
      add ('       end;');
      add ('var');
      add (' arr: array [0..300] of int8;');
      add (' m: tm;');
      add (' p: process (m: tm); priority 0;');
      add ('      begin cycle repeat end;');
      add ('begin');
      add ('  arr[5] := 5;');  // mark as reachable
      add ('  test (3, ''test_rq 0 0 0 0 0'');');
      add ('  test (4, ''test_mon_gate m 0'');');
      add ('  m.p;');
      add ('  test (5, ''test_rq 0 0 0 0 0'');');
      add ('  test (6, ''test_mon_gate m 0'');');
      add ('  m.p;');
      add ('  test (7, ''test_rq 0 0 0 0 0'');');
      add ('  test (8, ''test_mon_gate m 0'')');
      add ('end.');
      start_test (2);
      conclude_test
   end;

procedure test3;
   begin       // nested call, immediate return
      add ('type');
      add ('  tm1 = monitor');
      add ('          procedure entry p;');
      add ('             begin');
      add ('               test (1, ''test_rq 0 0 0 0 0'');');
      add ('               test (2, ''test_mon_gate a 1'');');
      add ('               test (3, ''test_mon_gate b 1'');');
      add ('             end;');
      add ('        begin');
      add ('        end;');
      add ('var');
      add ('  arr: array [0..300] of int8;');
      add ('  a: tm1;');
      add ('  b: monitor (m: tm1);');
      add ('        procedure entry p;');
      add ('           begin');
      add ('              test (4, ''test_rq 0 0 0 0 0'');');
      add ('              test (5, ''test_mon_gate a 0'');');
      add ('              test (6, ''test_mon_gate b 1'');');
      add ('              m.p;');
      add ('              test (7, ''test_rq 0 0 0 0 0'');');
      add ('              test (8, ''test_mon_gate a 0'');');
      add ('              test (9, ''test_mon_gate b 1'');');
      add ('           end;');
      add ('        begin end;');
      add ('begin');
      add ('  init b(a);');
      add ('  b.p');
      add ('end.');
      start_test (3);
      conclude_test
   end;

procedure test4;
   begin       // delay during process init
      add ('type');
      add ('   tm=monitor');
      add ('         var q: queue;');
      add ('         procedure entry d;');
      add ('            begin');
      add ('               test (1, ''test_rq 0 0 *p 0 0'');');
      add ('               test (2, ''test_mon_gate m 1'');');
      add ('               delay (q);');
      add ('            end;');
      add ('         begin');
      add ('         end;');
      add ('var');
      add ('   arr: array [0..300] of int8;');
      add ('   m: tm;');
      add ('   p: process (m: tm);');
      add ('         priority 0;');
      add ('         begin');
      add ('            cycle');
      add ('               m.d');
      add ('            repeat');
      add ('         end;');
      add ('begin');
      add ('   arr[5] := 5;');  // mark as reachable
      add ('   init m, p(m);');
      add ('   test (3, ''test_rq 0 0 0 0 0'');');
      add ('   test (4, ''test_mon_gate m 0'');');
      add ('end.');
      start_test (4);
      conclude_test
   end;

procedure test5;
   begin       // await interrupt during process init
      add ('var');
      add ('   arr: array [0..300] of int8;');
      add ('   i: interrupt priority 1;');
      add ('         function signalled: boolean;');
      add ('            begin end;');
      add ('         begin end;');
      add ('   p: process');
      add ('         priority 1;');
      add ('         begin');
      add ('            cycle');
      add ('               test (1, ''test_rq 0 *p 0 0 0'');');
      add ('               await interrupt');
      add ('            repeat');
      add ('         end interrupt i;');
      add ('begin');
      add ('   arr[5] := 5;');  // mark as reachable
      add ('   init p;');
      add ('   test (2, ''test_rq 0 0 0 0 0'');');
      add ('end.');
      start_test (5);
      conclude_test
   end;

procedure test6;
   begin       // call continue from with stmt with empty queue
      add ('var');
      add ('   arr: array [0..300] of int8;');
      add ('   m: monitor');
      add ('         var r: record q: queue end;');
      add ('         procedure entry p;');
      add ('            begin');
      add ('               with r');
      add ('               do continue (q)');
      add ('            end;');
      add ('         begin');
      add ('         end;');
      add ('begin');
      add ('   m.p');
      add ('end.');
      start_test (6);
      conclude_test
   end;

procedure test7;
   begin       // simple delay & continue, all same prio
      add ('type');
      add (' tm=monitor');
      add ('      var q,qx: queue;');
      add ('      procedure entry d1;');
      add ('        begin');
      add ('          test (1, ''test_rq 0 0 *a 0 0'');');
      add ('          delay (q);');
      add ('          test (2, ''test_rq 0 0 *ab 0 0'');');
      add ('        end;');
      add ('      procedure entry d2;');
      add ('        begin');
      add ('          test (3, ''test_rq 0 0 *a 0 0'');');
      add ('          delay (q);');
      add ('        end;');
      add ('      procedure entry c;');
      add ('        begin');
      add ('          test (4, ''test_rq 0 0 *b 0 0'');');
      add ('          continue (q)');
      add ('        end;');
      add ('      procedure entry dx;');
      add ('        begin');
      add ('          delay (qx)');
      add ('        end;');
      add ('    begin');
      add ('    end;');
      add ('var');
      add (' arr: array [0..300] of int8;');
      add (' a: process (m: tm); priority 0;');
      add ('       begin');
      add ('          m.d1;');
      add ('          cycle');
      add ('             m.d2');
      add ('          repeat');
      add ('       end;');
      add (' b: process (m: tm); priority 0;');
      add ('       begin');
      add ('          m.c;');
      add ('          test (5, ''test_rq 0 0 *ba 0 0'');');
      add ('          cycle');
      add ('            m.dx');
      add ('          repeat');
      add ('       end;');
      add (' m: tm;');
      add ('begin');
      add ('  arr[5] := 5;');  // mark as reachable
      add ('  init a(m);');
      add ('  init b(m)');
      add ('end.');
      start_test (7);
      conclude_test
   end;

procedure test8;
   begin       // simple delay & continue, different prio
      add ('type');
      add (' tm=monitor');
      add ('      var q: queue;');
      add ('      procedure entry d1;');
      add ('        begin');
      add ('          test (1, ''test_rq 0 *a 0 0 0'');');
      add ('          delay (q);');
      add ('          test (2, ''test_rq 0 *ab 0 0 0'');');
      add ('        end;');
      add ('      procedure entry d2;');
      add ('        begin');
      add ('          test (3, ''test_rq 0 *a 0 0 0'');');
      add ('          delay (q);');
      add ('        end;');
      add ('      procedure entry c;');
      add ('        begin');
      add ('          test (4, ''test_rq 0 *b 0 0 0'');');
      add ('          continue (q)');
      add ('        end;');
      add ('    begin');
      add ('    end;');
      add ('var');
      add (' arr: array [0..300] of int8;');
      add (' i: interrupt priority 1;');
      add ('      function signalled: boolean;');
      add ('        begin end;');
      add ('      begin end;');
      add (' a: process (m: tm); priority 0;');
      add ('       begin');
      add ('          m.d1;');
      add ('          cycle');
      add ('             m.d2');
      add ('          repeat');
      add ('       end;');
      add (' b: process (m: tm); priority 1;');
      add ('       begin');
      add ('          m.c;');
      add ('          test (5, ''test_rq 0 *b #a 0 0'');');
      add ('          cycle');
      add ('            await interrupt');
      add ('          repeat');
      add ('       end interrupt i;');
      add (' m: tm;');
      add ('begin');
      add ('  arr[5] := 5;');  // mark as reachable
      add ('  init a(m);');
      add ('  init b(m)');
      add ('end.');
      start_test (8);
      conclude_test
   end;

procedure test9;
   begin       // simple delay & continue, same as test8 except proc prios reversed
      add ('type');
      add (' tm=monitor');
      add ('      var q,qx: queue;');
      add ('      procedure entry d1;');
      add ('        begin');
      add ('          test (1, ''test_rq 0 0 *a 0 0'');');
      add ('          delay (q);');
      add ('          test (2, ''test_rq 0 0 *a #b 0'');');
      add ('        end;');
      add ('      procedure entry d2;');
      add ('        begin');
      add ('          test (3, ''test_rq 0 0 *a #b 0'');');
      add ('          delay (q);');
      add ('        end;');
      add ('      procedure entry c;');
      add ('        begin');
      add ('          test (4, ''test_rq 0 0 *b 0 0'');');
      add ('          continue (q)');
      add ('        end;');
      add ('      procedure entry x;');
      add ('        begin');
      add ('          delay(qx)');
      add ('        end;');
      add ('    begin');
      add ('    end;');
      add ('var');
      add (' arr: array [0..300] of int8;');
      add (' a: process (m: tm); priority 0;');
      add ('       begin');
      add ('          m.d1;');
      add ('          cycle');
      add ('             m.d2');
      add ('          repeat');
      add ('       end;');
      add (' b: process (m: tm); priority -1;');
      add ('       begin');
      add ('          m.c;');
      add ('          test (5, ''test_rq 0 0 0 *b 0'');');
      add ('          cycle');
      add ('            m.x');
      add ('          repeat');
      add ('       end;');
      add (' m: tm;');
      add ('begin');
      add ('  arr[5] := 5;');  // mark as reachable
      add ('  init a(m);');
      add ('  init b(m)');
      add ('end.');
      start_test (9);
      conclude_test
   end;

procedure test10;
   begin       // monitor entry calls blocked at gate
      add ('type');
      add ('  tm_inner =');
      add ('     monitor');
      add ('       var q: queue;');
      add ('         procedure entry d;');
      add ('           begin delay (q) end;');
      add ('         procedure entry c;');
      add ('           begin continue (q) end;');
      add ('       begin');
      add ('       end;');
      add ('  tm_outer =');
      add ('     monitor (inner: tm_inner);');
      add ('        procedure entry p;');
      add ('          begin inner.d end;');
      add ('        begin');
      add ('        end;');
      add ('  tm_blocker =');
      add ('    monitor');
      add ('       var q: queue;');
      add ('         procedure entry d;');
      add ('           begin delay (q) end;');
      add ('       begin');
      add ('       end;');
      add ('var');
      add ('  arr: array [0..300] of int8;');
      add ('  i: tm_inner;');
      add ('  m: tm_outer;');
      add ('  n: tm_blocker;');
      add ('  a,b,c,d:');
      add ('     process (m: tm_outer; n: tm_blocker);');
      add ('        priority 0;');
      add ('        begin');
      add ('           m.p;');
      add ('           cycle');
      add ('              n.d');
      add ('           repeat');
      add ('        end;');
      add ('begin');
      add ('  arr[5] := 5;');  // mark as reachable
      add ('  init m (i);');
      add ('  test (1, ''test_rq 0 0 0 0 0'');');
      add ('  test (2, ''test_mon_gate m 0'');');
      add ('  init a (m,n);');
      add ('  test (3, ''test_rq 0 0 0 0 0'');');
      add ('  test (4, ''test_mon_gate m 1'');');
      add ('  init b (m,n);');
      add ('  test (5, ''test_rq 0 0 0 0 0'');');
      add ('  test (6, ''test_mon_gate m b1'');');
      add ('  init c (m,n);');
      add ('  test (7, ''test_rq 0 0 0 0 0'');');
      add ('  test (8, ''test_mon_gate m bc1'');');
      add ('  init d (m,n);');
      add ('  test (9, ''test_rq 0 0 0 0 0'');');
      add ('  test (10, ''test_mon_gate m bcd1'');');
      add ('  i.c;');
      add ('  test (11, ''test_rq 0 0 0 0 0'');');
      add ('  test (12, ''test_mon_gate m cd1'');');
      add ('  i.c;');
      add ('  test (13, ''test_rq 0 0 0 0 0'');');
      add ('  test (14, ''test_mon_gate m d1'');');
      add ('  i.c;');
      add ('  test (15, ''test_rq 0 0 0 0 0'');');
      add ('  test (16, ''test_mon_gate m 1'');');
      add ('  i.c;');
      add ('  test (17, ''test_rq 0 0 0 0 0'');');
      add ('  test (18, ''test_mon_gate m 0'');');
      add ('end.');
      start_test (10);
      conclude_test
   end;

procedure test11;
   begin       // test continue nested cur_prio=old_prio
      add ('type');
      add (' tinner =');
      add ('  monitor');
      add ('   var q: queue;');
      add ('   procedure entry d;');
      add ('    begin delay (q) end;');
      add ('   procedure entry c;');
      add ('    begin continue (q) end;');
      add ('   begin end;');
      add (' touter =');
      add ('  monitor (i: tinner);');
      add ('   var qx: queue;');
      add ('   procedure entry d;');
      add ('    begin i.d end;');
      add ('   procedure entry c;');
      add ('    begin i.c end;');
      add ('   procedure entry x;');
      add ('    begin delay (qx) end;');
      add ('   begin end;');
      add ('var');
      add (' i: tinner;');
      add (' m: touter;');
      add (' p: process (i: tinner);');
      add ('      priority 0;');
      add ('      begin');
      add ('        cycle');
      add ('          i.d');
      add ('        repeat');
      add ('      end;');
      add (' q: process (m: touter);');
      add ('      priority 0;');
      add ('      begin');
      add ('         m.c;');
      add ('         cycle');
      add ('           m.x');
      add ('         repeat');
      add ('      end;');
      add ('begin');
      add (' init m (i);');
      add (' init p (i);');
      add (' test (1, ''test_rq 0 0 0 0 0'');');
      add (' test (2, ''test_mon_gate i 0'');');
      add (' test (3, ''test_mon_gate m 0'');');
      add (' init q (m);');
      add (' test (4, ''test_rq 0 0 0 0 0'');');
      add (' test (5, ''test_mon_gate i 0'');');
      add (' test (6, ''test_mon_gate m 0'');');
      add ('end.');
      start_test (11);
      conclude_test
   end;

procedure test12;
   begin
      add ('type');
      add (' tm1=monitor');
      add ('      var q: queue;');
      add ('      procedure entry d;');
      add ('        begin delay (q) end;');
      add ('      procedure entry c;');
      add ('        begin continue (q) end;');
      add ('    begin');
      add ('    end;');
      add (' tm0=monitor (m: tm1);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('       procedure entry c;');
      add ('         begin m.c end;');
      add ('     begin');
      add ('     end;');
      add ('var');
      add (' m,n,o: tm1;');
      add (' x,y,z: tm0;');
      add (' a,b,c: process (m: tm0); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                m.d');
      add ('             repeat');
      add ('          end;');
      add (' i: interrupt priority 1;');
      add ('      function signalled: boolean;');
      add ('        begin end;');
      add ('      begin end;');
      add (' d: process (m,n,o: tm1); priority 1;');
      add ('       begin');
      add ('          test (1, ''test_rq 0 *d 0 0 0'');');
      add ('          m.c;');
      add ('          test (2, ''test_rq 0 *d #a 0 0'');');
      add ('          n.c;');
      add ('          test (3, ''test_rq 0 *d #ba 0 0'');');
      add ('          o.c;');
      add ('          test (4, ''test_rq 0 *d #cba 0 0'');');
      add ('          cycle');
      add ('            await interrupt');
      add ('          repeat');
      add ('       end interrupt i;');
      add ('begin');
      add ('  init x(m);');
      add ('  init y(n);');
      add ('  init z(o);');
      add ('  init a(x);');
      add ('  init b(y);');
      add ('  init c(z);');
      add ('  init d (m,n,o)');
      add ('end.');
      start_test (12);
      conclude_test
   end;

procedure test13;
   begin
      add ('type');
      add (' tm3=monitor');
      add ('       var q: queue;');
      add ('       procedure entry d;');
      add ('         begin delay (q) end;');
      add ('       procedure entry c;');
      add ('         begin continue (q) end;');
      add ('     begin end;');
      add (' tm2=monitor (m: tm3);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('     begin end;');
      add (' tm1=monitor (m: tm2);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('     begin end;');
      add ('var');
      add ('   m: tm3;');
      add ('   n: tm2;');
      add ('   o: tm1;');
      add ('   p: process (m: tm1);');
      add ('         priority 0;');
      add ('         begin');
      add ('           cycle');
      add ('             m.d');
      add ('           repeat');
      add ('         end;');
      add ('   q: process (n: tm2);');
      add ('         priority 0;');
      add ('         begin');
      add ('           cycle');
      add ('             n.d');
      add ('           repeat');
      add ('         end;');
      add ('begin');
      add ('   init n(m);');
      add ('   init o(n);');
      add ('   init p (o);');
      add ('   init q (n);');
      add ('   test (1, ''test_rq 0 0 0 0 0'');');
      add ('   m.c;');
      add ('   test (2, ''test_rq 0 0 0 0 0'');');
      add ('   test (3, ''test_mon_gate n p1'');');
      add ('end.');
      start_test (13);
      conclude_test
   end;

procedure test14;
   begin
      add ('type');
      add (' tm3=monitor');
      add ('       var q: queue;');
      add ('       procedure entry d;');
      add ('         begin delay (q) end;');
      add ('       procedure entry c;');
      add ('         begin continue (q) end;');
      add ('     begin end;');
      add (' tm2=monitor (m: tm3);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('     begin end;');
      add (' tm1=monitor (m: tm2);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('     begin end;');
      add ('var');
      add ('   m: tm3;');
      add ('   n: tm2;');
      add ('   o: tm1;');
      add ('   p: process (m: tm1);');
      add ('         priority 0;');
      add ('         begin');
      add ('           cycle');
      add ('             m.d');
      add ('           repeat');
      add ('         end;');
      add ('   q: process (n: tm2);');
      add ('         priority 0;');
      add ('         begin');
      add ('           cycle');
      add ('             n.d');
      add ('           repeat');
      add ('         end;');
      add ('   int: interrupt priority 1;');
      add ('          function signalled: boolean;');
      add ('             begin end;');
      add ('          begin end;');
      add ('   x: process (n: tm2);');
      add ('        priority 1;');
      add ('        begin');
      add ('           cycle');
      add ('             await interrupt');
      add ('           repeat');
      add ('        end interrupt int;');
      add ('begin');
      add ('   init n(m);');
      add ('   init o(n);');
      add ('   init p (o);');
      add ('   init q (n);');
      add ('   test (1, ''test_rq 0 0 0 0 0'');');
      add ('   init x (n);');
      add ('   m.c;');
      add ('   test (2, ''test_rq 0 0 0 0 0'');');
      add ('   test (3, ''test_mon_gate n p1'');');
      add ('end.');
      start_test (14);
      conclude_test
   end;

procedure test15;
   begin
      add ('type');
      add (' tm2=monitor');
      add ('       var q: queue;');
      add ('       procedure entry d;');
      add ('          begin');
      add ('            delay (q)');
      add ('          end;');
      add ('       procedure entry c;');
      add ('          begin');
      add ('             continue (q)');
      add ('          end;');
      add ('       begin end;');
      add (' tm1=monitor (m1: tm2);');
      add ('       var q,qx: queue;');
      add ('       procedure entry p1;');
      add ('         begin');
      add ('            test (4, ''test_rq 0 0 *p 0 0'');');
      add ('            m1.d;');
      add ('            test (5, ''test_rq 0 0 *p 0 0'');');
      add ('            test (6, ''test_mon_gate m q1'');');
      add ('            delay (q)  // test this');
      add ('         end;');
      add ('       procedure entry p2;');
      add ('         begin');
      add ('         end;');
      add ('       procedure entry x;');
      add ('         begin delay (qx) end;');
      add ('       begin end;');
      add ('var');
      add (' a: array [0..300] of int8;');
      add (' m: tm1;');
      add (' m2: tm2;');
      add (' p: process (m: tm1); priority 0;');
      add ('       begin');
      add ('          cycle');
      add ('            m.p1;');
      add ('          repeat');
      add ('       end;');
      add (' q: process (m: tm1); priority 0;');
      add ('       begin');
      add ('         m.p2;');
      add ('         cycle');
      add ('           m.x');
      add ('         repeat');
      add ('       end;');
      add ('begin');
      add (' a[5] := 66;');
      add (' init m (m2);');
      add (' init p (m);');
      add (' test (1, ''test_rq 0 0 0 0 0'');');
      add (' init q (m);');
      add (' test (2, ''test_rq 0 0 0 0 0'');');
      add (' m2.c;');
      add (' test (3, ''test_rq 0 0 0 0 0'');');
      add ('end.');
      start_test (15);
      conclude_test
   end;

procedure test16;
   begin
      add ('type');
      add (' tm1=monitor');
      add ('      var q: queue;');
      add ('      procedure entry d;');
      add ('        begin delay (q) end;');
      add ('      procedure entry c;');
      add ('        begin continue (q) end;');
      add ('    begin');
      add ('    end;');
      add (' tm0=monitor (m: tm1);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('       procedure entry c;');
      add ('         begin m.c end;');
      add ('     begin');
      add ('     end;');
      add (' tm0b=monitor');
      add ('      var q: queue;');
      add ('      procedure entry d;');
      add ('        begin delay (q) end;');
      add ('      procedure entry c;');
      add ('        begin continue (q) end;');
      add ('    begin');
      add ('    end;');
      add ('var');
      add (' m,n,o: tm1;');
      add (' x,y,z: tm0;');
      add (' t: tm0b;');
      add (' a,b: process (m: tm0); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                m.d');
      add ('             repeat');
      add ('          end;');
      add (' c: process (m: tm0; t: tm0b); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                m.d;');
      add ('                t.c;  // test this');
      add ('                test (5, ''test_rq 0 0 *ce 0 0'');');
      add ('             repeat');
      add ('          end;');
      add (' e: process (t: tm0b); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                t.d;');
      add ('                test (6, ''test_rq 0 0 *e 0 0'')');
      add ('             repeat');
      add ('          end;');
      add (' i: interrupt priority 1;');
      add ('      function signalled: boolean;');
      add ('        begin end;');
      add ('      begin end;');
      add (' d: process (m,n,o: tm1); priority 1;');
      add ('       begin');
      add ('          test (1, ''test_rq 0 *d 0 0 0'');');
      add ('          m.c;');
      add ('          test (2, ''test_rq 0 *d #a 0 0'');');
      add ('          n.c;');
      add ('          test (3, ''test_rq 0 *d #ba 0 0'');');
      add ('          o.c;');
      add ('          test (4, ''test_rq 0 *d #cba 0 0'');');
      add ('          cycle');
      add ('            await interrupt');
      add ('          repeat');
      add ('       end interrupt i;');
      add ('begin');
      add ('  init x(m);');
      add ('  init y(n);');
      add ('  init z(o);');
      add ('  init a(x);');
      add ('  init b(y);');
      add ('  init c(z, t);');
      add ('  init e(t);');
      add ('  init d (m,n,o)');
      add ('end.');
      start_test (16);
      conclude_test
   end;

procedure test17;
   begin
      add ('type');
      add (' tm1=monitor');
      add ('      var q: queue;');
      add ('      procedure entry d;');
      add ('        begin delay (q) end;');
      add ('      procedure entry c;');
      add ('        begin continue (q) end;');
      add ('      procedure entry p;');
      add ('        begin end;');
      add ('    begin');
      add ('    end;');
      add (' tm0=monitor (m: tm1);');
      add ('       procedure entry d;');
      add ('         begin m.d end;');
      add ('       procedure entry c;');
      add ('         begin m.c end;');
      add ('     begin');
      add ('     end;');
      add ('var');
      add (' m,n,o: tm1;');
      add (' x,y,z: tm0;');
      add (' a,b: process (m: tm0); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                m.d');
      add ('             repeat');
      add ('          end;');
      add (' c: process (m: tm0; t: tm1); priority 0;');
      add ('          begin');
      add ('             cycle');
      add ('                m.d;');
      add ('                t.p;  // test this');
      add ('                test (5, ''test_rq 0 0 *c 0 0'');');
      add ('             repeat');
      add ('          end;');
      add (' i: interrupt priority 1;');
      add ('      function signalled: boolean;');
      add ('        begin end;');
      add ('      begin end;');
      add (' d: process (m,n,o: tm1); priority 1;');
      add ('       begin');
      add ('          test (1, ''test_rq 0 *d 0 0 0'');');
      add ('          m.c;');
      add ('          test (2, ''test_rq 0 *d #a 0 0'');');
      add ('          n.c;');
      add ('          test (3, ''test_rq 0 *d #ba 0 0'');');
      add ('          o.c;');
      add ('          test (4, ''test_rq 0 *d #cba 0 0'');');
      add ('          cycle');
      add ('            await interrupt');
      add ('          repeat');
      add ('       end interrupt i;');
      add ('begin');
      add ('  init x(m);');
      add ('  init y(n);');
      add ('  init z(o);');
      add ('  init a(x);');
      add ('  init b(y);');
      add ('  init c(z, m);');
      add ('  init d (m,n,o)');
      add ('end.');
      start_test (17);
      conclude_test
   end;

procedure run_kernel_tests;
   begin
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test10;
      test11;
      test12;
      test13;
      test14;
      test15;
      test16;
      test17;
   end;

END.
