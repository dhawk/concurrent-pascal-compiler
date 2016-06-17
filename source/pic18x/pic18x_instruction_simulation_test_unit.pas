UNIT pic18x_instruction_simulation_test_unit;

INTERFACE

procedure run_instruction_simulation_tests;

IMPLEMENTATION

uses test_pic18x_compiler_main_form_unit, SysUtils, pic18x_instructions_unit, pic18x_microprocessor_information_unit,
  test_pic18x_simulator_unit,
  Classes, pic18x_macro_instructions_unit, pic18x_cpu_unit,
  test_pic18x_kernel_unit;

var
   tests_run, errors: integer;
   current_test: string;
   current_test_num: integer;

procedure out (s: string);
   begin
      MainForm.TestResultsMemo.Lines.Add (s)
   end;

procedure start_instr_test (test: string);
   begin
      if test <> current_test then
         out ('testing ' + test);
      current_test := test
   end;

procedure start_test (test_num: integer);
   begin
{$ifdef INCLUDE_SIMULATION}
      current_test_num := test_num;
      cpu.Reset
{$endif}
   end;

procedure run_test;
   var
      changed: boolean;
      i: integer;
   begin
{$ifdef INCLUDE_SIMULATION}
      TPIC18x_SLEEP.Create;    // append end-of-test instruction
      repeat
         ProgramCode.AssignROMAddresses;
         changed := false;
         for i := 0 to Length(ProgramCode.instr_arr)-1 do
            ProgramCode.instr_arr[i].recalculate_size (changed)
      until not changed;
      ProgramCode.AssignEEPROMAddresses;
      ProgramCode.AssignConfigByteAddresses;
      cpu.pc := first_rom_addr;
      cpu.running := true;
      while cpu.running do
         ProgramCode.Execute
{$endif}
   end;

procedure check_w (expected_w: byte);
   begin
{$ifdef INCLUDE_SIMULATION}
      if expected_w <> cpu.w then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - w was  ' + byte_to_hex_string(cpu.w) + ', expected ' + byte_to_hex_string(expected_w));
            errors := errors + 1
         end
{$endif}
   end;

procedure check_ram (addr: integer; expected_value: byte);
   begin
{$ifdef INCLUDE_SIMULATION}
      if cpu.ram[addr] <> expected_value then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - ram[' + ram_addr_to_hex_string(addr) + '] was  ' + byte_to_hex_string(cpu.ram[addr]) + ', expected ' + byte_to_hex_string(expected_value));
            errors := errors + 1
         end
{$endif}
   end;

procedure check_z (expected_z: boolean);
   begin
{$ifdef INCLUDE_SIMULATION}
      if expected_z <> cpu.z then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - z was  ' + IntToStr(ord(cpu.z)) + ', expected ' + IntToStr(ord(expected_z)));
            errors := errors + 1
         end
{$endif}
   end;

procedure check_n_z (expected_n, expected_z: boolean);
   begin
{$ifdef INCLUDE_SIMULATION}
      if expected_n <> cpu.n then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - n was  ' + IntToStr(ord(cpu.n)) + ', expected ' + IntToStr(ord(expected_n)));
            errors := errors + 1
         end;
      check_z (expected_z)
{$endif}
   end;

procedure check_n_z_c (expected_n, expected_z, expected_c: boolean);
   begin
{$ifdef INCLUDE_SIMULATION}
      check_n_z (expected_n, expected_z);
      if expected_c <> cpu.c then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - c was  ' + IntToStr(ord(cpu.c)) + ', expected ' + IntToStr(ord(expected_c)));
            errors := errors + 1
         end
{$endif}
   end;

procedure check_status (expected_n, expected_ov, expected_z, expected_dc, expected_c: boolean);
   // above parameters in order of MPSIM display
   begin
{$ifdef INCLUDE_SIMULATION}
      check_n_z_c (expected_n, expected_z, expected_c);
      if expected_dc <> cpu.dc then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - dc was  ' + IntToStr(ord(cpu.dc)) + ', expected ' + IntToStr(ord(expected_dc)));
            errors := errors + 1
         end;
      if expected_ov <> cpu.ov then
         begin
            out ('   test #' + IntToStr (current_test_num) + ' failed - ov was  ' + IntToStr(ord(cpu.ov)) + ', expected ' + IntToStr(ord(expected_ov)));
            errors := errors + 1
         end
{$endif}
   end;

procedure conclude_test;
   var
      testname: string;
   begin
      testname := current_test + '_' + IntToStr(current_test_num);
      ProgramCode.WriteHexFile ('..\temp\pic18x_simulator_test_cases\hex_tests\' + testname + '.hex');
      ProgramCode.WriteAssemblySourceFile ('..\temp\pic18x_simulator_test_cases\asm_tests\' + testname + '.asm');
      ProgramCode.Clear;
      tests_run := tests_run + 1;
   end;

procedure movlw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($6e);
         run_test;
         check_w ($6e);
         conclude_test
      end;
   begin
      start_instr_test ('movlw');
      test1
   end;

procedure movlb_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLB.Create (1);
         run_test;
         check_ram (BSR, 1);
         conclude_test
      end;
   begin
      start_instr_test ('movlb');
      test1
   end;

procedure andlw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($FF);
         TPIC18x_ANDLW.Create ($00);
         run_test;
         check_w ($00);
         check_n_z (false, true);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($FF);
         TPIC18x_ANDLW.Create ($01);
         run_test;
         check_w ($01);
         check_n_z (false, false);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($FF);
         TPIC18x_ANDLW.Create ($80);
         run_test;
         check_w ($80);
         check_n_z (true, false);
         conclude_test
      end;
   begin
      start_instr_test ('andlw');
      test1;
      test2;
      test3
   end;

procedure addlw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_ADDLW.Create ($02);
         run_test;
         check_w ($03);
         check_status (false, false, false, false, false);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($81);
         TPIC18x_ADDLW.Create ($82);
         run_test;
         check_w ($03);
         check_status (false, true, false, false, true);
         conclude_test
      end;
   procedure test3;   // two large positive numbers that overflow
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($71);
         TPIC18x_ADDLW.Create ($72);
         run_test;
         check_w ($e3);
         check_status (true, true, false, false, false);
         conclude_test
      end;
   procedure test4;  // two small negative numbers that don't overflow
      begin
         start_test (4);
         TPIC18x_MOVLW.Create ($e2);
         TPIC18x_ADDLW.Create ($e3);
         run_test;
         check_w ($c5);
         check_status (true, false, false, false, true);
         conclude_test
      end;
   procedure test5;  // two large negative numbers that overflow
      begin
         start_test (5);
         TPIC18x_MOVLW.Create ($9c);
         TPIC18x_ADDLW.Create ($9c);
         run_test;
         check_w ($38);
         check_status (false, true, false, true, true);
         conclude_test
      end;
   procedure test6;
      begin
         start_test (6);
         TPIC18x_MOVLW.Create ($09);
         TPIC18x_ADDLW.Create ($09);
         run_test;
         check_w ($12);
         check_status (false, false, false, true, false);
         conclude_test
      end;
   procedure test7;
      begin
         start_test (7);
         TPIC18x_MOVLW.Create ($09);
         TPIC18x_ADDLW.Create ($f7);
         run_test;
         check_w ($00);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   begin
      start_instr_test ('addlw');
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;

   end;

procedure iorlw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_IORLW.Create ($00);
         run_test;
         check_w ($00);
         check_n_z (false, true);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_IORLW.Create ($01);
         run_test;
         check_w ($01);
         check_n_z (false, false);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_IORLW.Create ($80);
         run_test;
         check_w ($80);
         check_n_z (true, false);
         conclude_test
      end;
   begin
      start_instr_test ('iorlw');
      test1;
      test2;
      test3
   end;

procedure xorlw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_XORLW.Create ($00);
         run_test;
         check_w ($00);
         check_n_z (false, true);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_XORLW.Create ($01);
         run_test;
         check_w ($01);
         check_n_z (false, false);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_XORLW.Create ($80);
         run_test;
         check_w ($80);
         check_n_z (true, false);
         conclude_test
      end;
   procedure test4;
      begin
         start_test (4);
         TPIC18x_MOVLW.Create ($80);
         TPIC18x_XORLW.Create ($80);
         run_test;
         check_w ($00);
         check_n_z (false, true);
         conclude_test
      end;
   begin
      start_instr_test ('xorlw');
      test1;
      test2;
      test3;
      test4
   end;

procedure sublw_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_SUBLW.Create ($02);
         run_test;
         check_w ($01);
         check_status (false, false, false, true, true);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBLW.Create ($02);
         run_test;
         check_w ($00);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   procedure test3;   // two large positive numbers that overflow
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($03);
         TPIC18x_SUBLW.Create ($02);
         run_test;
         check_w ($ff);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   procedure test4;  // two small negative numbers that don't overflow
      begin
         start_test (4);
         TPIC18x_MOVLW.Create (5);
         TPIC18x_SUBLW.Create (10);
         run_test;
         check_w ($05);
         check_status (false, false, false, true, true);
         conclude_test
      end;
   procedure test5;  // two large negative numbers that overflow
      begin
         start_test (5);
         TPIC18x_MOVLW.Create (10);
         TPIC18x_SUBLW.Create (5);
         run_test;
         check_w ($fb);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   procedure test6;
      begin
         start_test (6);
         TPIC18x_MOVLW.Create (-5 and $ff);
         TPIC18x_SUBLW.Create (-10 and $ff);
         run_test;
         check_w ($fb);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   procedure test7;
      begin
         start_test (7);
         TPIC18x_MOVLW.Create (-10 and $ff);
         TPIC18x_SUBLW.Create (-5 and $ff);
         run_test;
         check_w ($05);
         check_status (false, false, false, true, true);
         conclude_test
      end;
   procedure test8;
      begin
         start_test (8);
         TPIC18x_MOVLW.Create (-5 and $ff);
         TPIC18x_SUBLW.Create (10);
         run_test;
         check_w ($0f);
         check_status (false, false, false, false, false);
         conclude_test
      end;
   procedure test9;
      begin
         start_test (9);
         TPIC18x_MOVLW.Create (10);
         TPIC18x_SUBLW.Create (-5 and $ff);
         run_test;
         check_w ($f1);
         check_status (true, false, false, true, true);
         conclude_test
      end;
   procedure test10;
      begin
         start_test (10);
         TPIC18x_MOVLW.Create (10);
         TPIC18x_SUBLW.Create (10);
         run_test;
         check_w (0);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   begin
      start_instr_test ('sublw');
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test10
   end;

procedure lfsr_test;
   begin
      start_instr_test ('lfsr');
      start_test (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_LFSR.Create (1, $456);
      TPIC18x_LFSR.Create (2, $789);
      run_test;
      check_ram (FSR0H, $01);
      check_ram (FSR0L, $23);
      check_ram (FSR1H, $04);
      check_ram (FSR1L, $56);
      check_ram (FSR2H, $07);
      check_ram (FSR2L, $89);
      conclude_test
   end;

procedure mullw_test;
   begin
      start_instr_test ('mullw');
      start_test (1);
      TPIC18x_MOVLW.Create ($e2);
      TPIC18x_MULLW.Create ($c4);
      run_test;
      check_w ($e2);
      check_ram (PRODH, $ad);
      check_ram (PRODL, $08);
      conclude_test
   end;

procedure movwf_test;
   // test all access/bank effective address modes
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($55);
         TPIC18x_LFSR.Create (2, $100);
         TPIC18x_MOVWF.Create (95, access_mode);
         run_test;
         check_ram ($100+95, $55);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($33);
         TPIC18x_MOVWF.Create (FSR0L, access_mode);
         run_test;
         check_ram (FSR0L, $33);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLB.Create (1);
         TPIC18x_MOVLW.Create ($22);
         TPIC18x_MOVWF.Create (33, bank_mode);
         run_test;
         check_ram (256+33, $22);
         conclude_test
      end;
   begin
      start_instr_test ('movwf');
      test1;
      test2;
      test3
   end;

procedure addwf_test;
   // also tests destination bit logic in base class
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLB.Create (0);
         TPIC18x_MOVLW.Create ($55);
         TPIC18x_MOVWF.Create ($02, bank_mode);
         TPIC18x_MOVLW.Create ($22);
         TPIC18x_ADDWF.Create ($02, dest_w, bank_mode);
         run_test;
         check_w ($77);
         check_ram ($02, $55);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLB.Create (0);
         TPIC18x_MOVLW.Create ($55);
         TPIC18x_MOVWF.Create ($02, bank_mode);
         TPIC18x_MOVLW.Create ($22);
         TPIC18x_ADDWF.Create ($02, dest_f, bank_mode);
         run_test;
         check_w ($22);
         check_ram ($02, $77);
         conclude_test
      end;
   begin
      start_instr_test ('addwf');
      test1;
      test2;
   end;

procedure addwfc_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($fe);
         TPIC18x_MOVWF.Create ($02, bank_mode);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_ADDWF.Create ($02, dest_f, bank_mode);
         TPIC18x_ADDWFC.Create ($02, dest_w, bank_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($ff);
         TPIC18x_MOVWF.Create ($02, bank_mode);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_ADDWF.Create ($02, dest_f, bank_mode);
         TPIC18x_ADDWFC.Create ($02, dest_w, bank_mode);
         run_test;
         check_w ($02);
         conclude_test
      end;
   begin
      start_instr_test ('addwfc');
      test1;
      test2;
   end;

procedure andwf_test;
   begin
      start_instr_test ('andwf');
      start_test (1);
      TPIC18x_MOVLW.Create ($FF);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_MOVLW.Create ($80);
      TPIC18x_ANDWF.Create ($02, dest_f, bank_mode);
      run_test;
      check_ram ($02, $80);
      check_n_z (true, false);
      conclude_test
   end;

procedure clrf_test;
   begin
      start_instr_test ('clrf');
      start_test (1);
      TPIC18x_MOVLW.Create ($FF);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_CLRF.Create ($02, bank_mode);
      run_test;
      check_ram ($02, $00);
      check_z (true);
      conclude_test
   end;

procedure comf_test;
   begin
      start_instr_test ('comf');
      start_test (1);
      TPIC18x_MOVLW.Create ($FF);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_COMF.Create ($02, dest_f, bank_mode);
      run_test;
      check_ram ($02, $00);
      check_n_z (false, true);
      conclude_test
   end;

procedure incf_test;
   begin
      start_instr_test ('incf');
      start_test (1);
      TPIC18x_MOVLW.Create ($FF);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_INCF.Create ($02, dest_f, bank_mode);
      run_test;
      check_ram ($02, $00);
      check_status (false, false, true, true, true);
      conclude_test
   end;

procedure decf_test;
   begin
      start_instr_test ('decf');
      start_test (1);
      TPIC18x_MOVLW.Create ($fe);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_DECF.Create ($02, dest_w, bank_mode);
      run_test;
      check_w ($fd);
      check_status (true, false, false, true, true);
      conclude_test
   end;

procedure subfwb_test;
   procedure test1;
      const
         reg = $01;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($03);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBFWB.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($02);
         check_ram (reg, $ff);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   procedure test2;
      const
         reg = $02;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_SUBFWB.Create (reg, dest_w, bank_mode);
         run_test;
         check_w ($03);
         check_ram (reg, $02);
         check_status (false, false, false, true, true);
         conclude_test
      end;
   procedure test3;
      const
         reg = $03;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBFWB.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($02);
         check_ram (reg, $00);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   begin
      start_instr_test ('subfwb');
      test1;
      test2;
      test3
   end;

procedure subwf_test;
   procedure test1;
      const
         reg = $01;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($03);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBWF.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($02);
         check_ram (reg, $01);
         check_status (false, false, false, true, true);
         conclude_test
      end;
   procedure test2;
      const
         reg = $02;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBWF.Create (reg, dest_w, bank_mode);
         run_test;
         check_w ($00);
         check_ram (reg, $02);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   procedure test3;
      const
         reg = $03;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_SUBWF.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($02);
         check_ram (reg, $ff);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   begin
      start_instr_test ('subwf');
      test1;
      test2;
      test3
   end;

procedure subwfb_test;
   procedure test1;
      const
         reg = $01;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($19);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($0D);
         TPIC18x_SUBWFB.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($0D);
         check_ram (reg, $0C);
         check_status (false, false, false, false, true);
         conclude_test
      end;
   procedure test2;
      const
         reg = $02;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($1b);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($1a);
         TPIC18x_SUBWFB.Create (reg, dest_w, bank_mode);
         run_test;
         check_w ($00);
         check_ram (reg, $1b);
         check_status (false, false, true, true, true);
         conclude_test
      end;
   procedure test3;
      const
         reg = $03;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($03);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($0e);
         TPIC18x_SUBWFB.Create (reg, dest_f, bank_mode);
         run_test;
         check_w ($0e);
         check_ram (reg, $f5);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   procedure test4;
      const
         reg = $03;
      begin
         start_test (4);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_MOVWF.Create (STATUS, access_mode);
         TPIC18x_MOVLW.Create ($0b);
         TPIC18x_MOVWF.Create (reg, bank_mode);
         TPIC18x_MOVLW.Create ($0b);
         TPIC18x_SUBWFB.Create (reg, dest_w, bank_mode);
         run_test;
         check_w ($ff);
         check_status (true, false, false, false, false);
         conclude_test
      end;
   begin
      start_instr_test ('subwfb');
      test1;
      test2;
      test3;
      test4
   end;

procedure negf_test;
   begin
      start_instr_test ('negf');
      start_test (1);
      TPIC18x_MOVLW.Create ($3a);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_NEGF.Create ($02, bank_mode);
      run_test;
      check_ram ($02, $c6);
      check_status (true, false, false, false, false);
      conclude_test
   end;

procedure iorwf_test;
   begin
      start_instr_test ('iorwf');
      start_test (1);
      TPIC18x_MOVLW.Create ($13);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_MOVLW.Create ($91);
      TPIC18x_IORWF.Create ($02, dest_w, bank_mode);
      run_test;
      check_w ($93);
      check_ram ($02, $13);
      check_n_z (true, false);
      conclude_test
   end;

procedure xorwf_test;
   begin
      start_instr_test ('xorwf');
      start_test (1);
      TPIC18x_MOVLW.Create ($af);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_MOVLW.Create ($b5);
      TPIC18x_XORWF.Create ($02, dest_f, bank_mode);
      run_test;
      check_w ($b5);
      check_ram ($02, $1a);
      check_n_z (false, false);
      conclude_test
   end;

procedure movf_test;
   begin
      start_instr_test ('movf');
      start_test (1);
      TPIC18x_MOVLW.Create ($22);
      TPIC18x_MOVWF.Create ($02, bank_mode);
      TPIC18x_MOVLW.Create ($ff);
      TPIC18x_MOVF.Create ($02, dest_w, bank_mode);
      run_test;
      check_w ($22);
      check_ram ($02, $22);
      check_n_z (false, false);
      conclude_test
   end;

procedure movff_test;
   const
      reg1=1;
      reg2=2;
   begin
      start_instr_test ('movff');
      start_test (1);
      TPIC18x_MOVLW.Create ($33);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_MOVLW.Create ($11);
      TPIC18x_MOVWF.Create (reg2, bank_mode);
      TPIC18x_MOVFF.Create (reg1, reg2);
      run_test;
      check_ram (reg1, $33);
      check_ram (reg2, $33);
      conclude_test
   end;

procedure mulwf_test;
   const
      reg1=1;
   begin
      start_instr_test ('mulwf');
      start_test (1);
      TPIC18x_MOVLW.Create ($B5);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_MOVLW.Create ($c4);
      TPIC18x_MULWF.Create (reg1, bank_mode);
      run_test;
      check_ram (PRODH, $8a);
      check_ram (PRODL, $94);
      conclude_test
   end;

procedure setf_test;
   const
      reg1=1;
   begin
      start_instr_test ('setf');
      start_test (1);
      TPIC18x_SETF.Create (reg1, bank_mode);
      run_test;
      check_ram (reg1, $ff);
      conclude_test
   end;

procedure swapf_test;
   const
      reg1=1;
   begin
      start_instr_test ('swapf');
      start_test (1);
      TPIC18x_MOVLW.Create ($53);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_SWAPF.Create (reg1, dest_f, bank_mode);
      run_test;
      check_ram (reg1, $35);
      conclude_test
   end;

procedure rlcf_test;
   const
      reg1=1;
   begin
      start_instr_test ('rlcf');
      start_test (1);
      TPIC18x_CLRF.Create (STATUS, access_mode);
      TPIC18x_MOVLW.Create ($e6);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_RLCF.Create (reg1, dest_w, bank_mode);
      run_test;
      check_w ($cc);
      check_ram (reg1, $e6);
      check_n_z_c (true, false, true);
      conclude_test
   end;

procedure rlncf_test;
   const
      reg1=1;
   begin
      start_instr_test ('rlncf');
      start_test (1);
      TPIC18x_MOVLW.Create ($ab);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_RLNCF.Create (reg1, dest_f, bank_mode);
      run_test;
      check_ram (reg1, $57);
      check_n_z (false, false);
      conclude_test
   end;

procedure rrcf_test;
   const
      reg1=1;
   begin
      start_instr_test ('rrcf');
      start_test (1);
      TPIC18x_CLRF.Create (STATUS, access_mode);
      TPIC18x_MOVLW.Create ($e6);
      TPIC18x_MOVWF.Create (reg1, bank_mode);
      TPIC18x_RRCF.Create (reg1, dest_w, bank_mode);
      run_test;
      check_ram (reg1, $e6);
      check_w ($73);
      check_n_z_c (false, false, false);
      conclude_test
   end;

procedure rrncf_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($d7);
         TPIC18x_MOVWF.Create (reg1, bank_mode);
         TPIC18x_RRNCF.Create (reg1, dest_f, bank_mode);
         run_test;
         check_ram (reg1, $eb);
         check_n_z (true, false);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($d7);
         TPIC18x_MOVWF.Create (reg2, bank_mode);
         TPIC18x_RRNCF.Create (reg2, dest_w, bank_mode);
         run_test;
         check_ram (reg2, $d7);
         check_w ($eb);
         check_n_z (true, false);
         conclude_test
      end;
   begin
      start_instr_test ('rrncf');
      test1;
      test2
   end;

procedure tstfsz_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($d7);
         TPIC18x_TSTFSZ.Create (WREG, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($d8);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($00);
         TPIC18x_TSTFSZ.Create (WREG, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('tstfsz');
      test1;
      test2
   end;

procedure cpfseq_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($d7);
         TPIC18x_CPFSEQ.Create (WREG, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($d7);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_MOVWF.Create (reg2, bank_mode);
         TPIC18x_INCF.Create (reg2, dest_f, bank_mode);
         TPIC18x_CPFSEQ.Create (reg2, bank_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($06);
         conclude_test
      end;
   begin
      start_instr_test ('cpfseq');
      test1;
      test2
   end;

procedure cpfsgt_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_MOVWF.Create (reg1, bank_mode);
         TPIC18x_MOVLW.Create ($07);
         TPIC18x_CPFSGT.Create (reg1, bank_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($08);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($07);
         TPIC18x_MOVWF.Create (reg2, bank_mode);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_CPFSGT.Create (reg2, bank_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($05);
         conclude_test
      end;
   begin
      start_instr_test ('cpfsgt');
      test1;
      test2
   end;

procedure cpfslt_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_MOVWF.Create (reg1, bank_mode);
         TPIC18x_MOVLW.Create ($07);
         TPIC18x_CPFSLT.Create (reg1, bank_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($07);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($07);
         TPIC18x_MOVWF.Create (reg2, bank_mode);
         TPIC18x_MOVLW.Create ($05);
         TPIC18x_CPFSLT.Create (reg2, bank_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($06);
         conclude_test
      end;
   begin
      start_instr_test ('cpfslt');
      test1;
      test2
   end;

procedure decfsz_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($02);
         conclude_test
      end;
   begin
      start_instr_test ('decfsz');
      test1;
      test2
   end;

procedure dcfsnz_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_DCFSNZ.Create (WREG, dest_w, access_mode);
         TPIC18x_DECF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($ff);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_DCFSNZ.Create (WREG, dest_w, access_mode);
         TPIC18x_DECF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('dcfsnz');
      test1;
      test2
   end;

procedure incfsz_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($ff);
         TPIC18x_INCFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($02);
         TPIC18x_INCFSZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($04);
         conclude_test
      end;
   begin
      start_instr_test ('incfsz');
      test1;
      test2
   end;

procedure infsnz_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create ($01);
         TPIC18x_INFSNZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($02);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create ($ff);
         TPIC18x_INFSNZ.Create (WREG, dest_w, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('infsnz');
      test1;
      test2
   end;

procedure bcf_test;
   const
      reg1=1;
   begin
      start_instr_test ('bcf');
      start_test (1);
      TPIC18x_SETF.Create (reg1, bank_mode);
      TPIC18x_BCF.Create (reg1, 4, bank_mode);
      run_test;
      check_ram (reg1, $EF);
      conclude_test
   end;

procedure bsf_test;
   const
      reg1=1;
   begin
      start_instr_test ('bsf');
      start_test (1);
      TPIC18x_CLRF.Create (reg1, bank_mode);
      TPIC18x_BSF.Create (reg1, 4, bank_mode);
      run_test;
      check_ram (reg1, $10);
      conclude_test
   end;

procedure btg_test;
   const
      reg1=1;
   begin
      start_instr_test ('btg');
      start_test (1);
      TPIC18x_CLRF.Create (reg1, bank_mode);
      TPIC18x_BTG.Create (reg1, 4, bank_mode);
      run_test;
      check_ram (reg1, $10);
      conclude_test
   end;

procedure btfsc_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_BTFSC.Create (WREG, 1, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_SETF.Create (WREG, access_mode);
         TPIC18x_BTFSC.Create (WREG, 1, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('btfsc');
      test1;
      test2
   end;

procedure btfss_test;
   procedure test1;
      const
         reg1=1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_BTFSS.Create (WREG, 1, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   procedure test2;
      const
         reg2=2;
      begin
         start_test (2);
         TPIC18x_SETF.Create (WREG, access_mode);
         TPIC18x_BTFSS.Create (WREG, 1, access_mode);
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($FF);
         conclude_test
      end;
   begin
      start_instr_test ('btfss');
      test1;
      test2
   end;

procedure bc_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 0, access_mode);
         TPIC18x_BC.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 0, access_mode);
         TPIC18x_BC.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test3;
      var i: TPIC18x_BC;
      begin
         start_test (3);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 0, access_mode);
         i := TPIC18x_BC.Create;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         i.dest := TAssemblyLabel.Create;
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('bc');
      test1;
      test2;
      test3
   end;

procedure bnc_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 0, access_mode);
         TPIC18x_BNC.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 0, access_mode);
         TPIC18x_BNC.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('bnc');
      test1;
      test2
   end;

procedure bn_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 4, access_mode);
         TPIC18x_BN.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 4, access_mode);
         TPIC18x_BN.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('bn');
      test1;
      test2
   end;

procedure bnn_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 4, access_mode);
         TPIC18x_BNN.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 4, access_mode);
         TPIC18x_BNN.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('bnn');
      test1;
      test2
   end;

procedure bov_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 3, access_mode);
         TPIC18x_BOV.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 3, access_mode);
         TPIC18x_BOV.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('bov');
      test1;
      test2
   end;

procedure bnov_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 3, access_mode);
         TPIC18x_BNOV.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 3, access_mode);
         TPIC18x_BNOV.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('bnov');
      test1;
      test2
   end;

procedure bz_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 2, access_mode);
         TPIC18x_BZ.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 2, access_mode);
         TPIC18x_BZ.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   begin
      start_instr_test ('bz');
      test1;
      test2
   end;

procedure bnz_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_SETF.Create (STATUS, access_mode);
         TPIC18x_BCF.Create (STATUS, 2, access_mode);
         TPIC18x_BNZ.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($00);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CLRF.Create (STATUS, access_mode);
         TPIC18x_BSF.Create (STATUS, 2, access_mode);
         TPIC18x_BNZ.Create.n_field := 1;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w ($01);
         conclude_test
      end;
   begin
      start_instr_test ('bnz');
      test1;
      test2
   end;

procedure bra_test;
   begin
      start_instr_test ('bra');
      start_test (1);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_SETF.Create (STATUS, access_mode);
      TPIC18x_BCF.Create (STATUS, 2, access_mode);
      TPIC18x_BNZ.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_BRA.Create.n_field := -2;
      run_test;
      check_w ($00);
      conclude_test
   end;

procedure rcall_test;
   begin
      start_instr_test ('rcall');
      start_test (1);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_SETF.Create (STATUS, access_mode);
      TPIC18x_BCF.Create (STATUS, 2, access_mode);
      TPIC18x_BNZ.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_RCALL.Create.n_field := -2;
      run_test;
      check_w ($00);
      check_ram (STKPTR, 1);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, first_rom_addr + 12);
      conclude_test
   end;

procedure retlw_test;
   begin
      start_instr_test ('retlw');
      start_test (1);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_RCALL.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_RETLW.Create ($55);
      run_test;
      check_w ($55);
      check_ram (STKPTR, 0);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, 0);
      conclude_test
   end;

procedure return_test;
   begin
      start_instr_test ('return');
      start_test (1);
      TPIC18x_RCALL.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_RETURN.Create;
      run_test;
      check_ram (STKPTR, 0);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, 0);
      conclude_test
   end;

procedure push_test;
   begin
      start_instr_test ('push');
      start_test (1);
      TPIC18x_PUSH.Create;
      run_test;
      check_ram (STKPTR, 1);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, first_rom_addr + 2);
      conclude_test
   end;

procedure pop_test;
   begin
      start_instr_test ('pop');
      start_test (1);
      TPIC18x_PUSH.Create;
      TPIC18x_POP.Create;
      run_test;
      check_ram (STKPTR, 0);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, 0);
      conclude_test
   end;

procedure call_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_CALL.Create.n_field := (first_rom_addr div 2) + 4;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w (0);
         check_ram (STKPTR, 1);
         check_ram (TOSU, 0);
         check_ram (TOSH, 0);
         check_ram (TOSL, first_rom_addr + 6);
         conclude_test
      end;
   procedure test2;
      var i: TPIC18x_CALL;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         i := TPIC18x_CALL.Create;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         i.dest := TAssemblyLabel.Create;
         run_test;
         check_w (0);
         check_ram (STKPTR, 1);
         check_ram (TOSU, 0);
         check_ram (TOSH, 0);
         check_ram (TOSL, first_rom_addr + 6);
         conclude_test
      end;
   begin
      start_instr_test ('call');
      test1;
      test2
   end;

procedure goto_test;
   var g: TPIC18x_GOTO;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_CLRF.Create (WREG, access_mode);
         g := TPIC18x_GOTO.Create;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         g.dest := TAssemblyLabel.Create;
         run_test;
         check_w (0);
         check_ram (STKPTR, 0);
         check_ram (TOSU, 0);
         check_ram (TOSH, 0);
         check_ram (TOSL, 0);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_CLRF.Create (WREG, access_mode);
         TPIC18x_GOTO.Create.n_field := (first_rom_addr div 2) + 4;
         TPIC18x_INCF.Create (WREG, dest_w, access_mode);
         run_test;
         check_w (0);
         check_ram (STKPTR, 0);
         check_ram (TOSU, 0);
         check_ram (TOSH, 0);
         check_ram (TOSL, 0);
         conclude_test
      end;
   begin
      start_instr_test ('goto');
      test1;
      test2
   end;

procedure tblrd_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_MOVWF.Create (TBLPTRU, access_mode);
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_MOVLW.Create (first_rom_addr + 5);
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TPIC18x_TBLRD.Create (tblrd);
         run_test;
         check_ram (TABLAT, $6E);
         check_ram (TBLPTRU, 0);
         check_ram (TBLPTRH, 0);
         check_ram (TBLPTRL, first_rom_addr + 5);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_MOVWF.Create (TBLPTRU, access_mode);
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_MOVLW.Create (first_rom_addr + 6);
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TPIC18x_TBLRD.Create (tblrd);
         run_test;
         check_ram (TABLAT, $22);
         check_ram (TBLPTRU, 0);
         check_ram (TBLPTRH, 0);
         check_ram (TBLPTRL, first_rom_addr + 6);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_MOVWF.Create (TBLPTRU, access_mode);
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_MOVLW.Create (first_rom_addr + 6);
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TPIC18x_TBLRD.Create (tblrd_post_inc);
         run_test;
         check_ram (TABLAT, $22);
         check_ram (TBLPTRU, 0);
         check_ram (TBLPTRH, 0);
         check_ram (TBLPTRL, first_rom_addr + 7);
         conclude_test
      end;
   procedure test4;
      begin
         start_test (4);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_MOVWF.Create (TBLPTRU, access_mode);
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_MOVLW.Create (first_rom_addr + 6);
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TPIC18x_TBLRD.Create (tblrd_post_dec);
         run_test;
         check_ram (TABLAT, $22);
         check_ram (TBLPTRU, 0);
         check_ram (TBLPTRH, 0);
         check_ram (TBLPTRL, first_rom_addr + 5);
         conclude_test
      end;
   procedure test5;
      begin
         start_test (5);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_MOVWF.Create (TBLPTRU, access_mode);
         TPIC18x_MOVWF.Create (TBLPTRH, access_mode);
         TPIC18x_MOVLW.Create (first_rom_addr + 4);
         TPIC18x_MOVWF.Create (TBLPTRL, access_mode);
         TPIC18x_TBLRD.Create (tblrd_pre_inc);
         run_test;
         check_ram (TABLAT, $6e);
         check_ram (TBLPTRU, 0);
         check_ram (TBLPTRH, 0);
         check_ram (TBLPTRL, first_rom_addr + 5);
         conclude_test
      end;
   begin
      start_instr_test ('tblrd');
      test1;
      test2;
      test3;
      test4;
      test5;
   end;

procedure addfsr_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR0H, access_mode);
         TPIC18x_MOVWF.Create (FSR0L, access_mode);
         TPIC18x_ADDFSR.Create (0, $37);
         run_test;
         check_ram (FSR0L, $38);
         check_ram (FSR0H, 1);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR1H, access_mode);
         TPIC18x_MOVWF.Create (FSR1L, access_mode);
         TPIC18x_ADDFSR.Create (1, $37);
         run_test;
         check_ram (FSR1L, $38);
         check_ram (FSR1H, 1);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR2H, access_mode);
         TPIC18x_MOVWF.Create (FSR2L, access_mode);
         TPIC18x_ADDFSR.Create (2, $37);
         run_test;
         check_ram (FSR2L, $38);
         check_ram (FSR2H, 1);
         conclude_test
      end;
   begin
      start_instr_test ('addfsr');
      test1;
      test2;
      test3;
   end;

procedure addulnk_test;
   begin
      start_instr_test ('addulnk');
      start_test (1);
      TPIC18x_MOVLW.Create (1);
      TPIC18x_MOVWF.Create (FSR2H, access_mode);
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_RCALL.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_ADDULNK.Create ($37);
      run_test;
      check_ram (STKPTR, 0);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, 0);
      check_ram (FSR2L, $38);
      check_ram (FSR2H, 1);
      conclude_test
   end;

procedure callw_test;
   begin
      start_instr_test ('callw');
      start_test (1);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_MOVWF.Create (PCLATU, access_mode);
      TPIC18x_MOVWF.Create (PCLATH, access_mode);
      TPIC18x_MOVLW.Create (first_rom_addr + 12);
      TPIC18x_CALLW.Create;
      TPIC18x_SETF.Create (WREG, access_mode);
      run_test;
      check_w (first_rom_addr + 12);
      check_ram (STKPTR, 1);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, first_rom_addr + 10);
      conclude_test
   end;

procedure movsf_test;
   const
      reg2 = $10;
   begin
      start_instr_test ('movsf');
      start_test (1);
      TPIC18x_LFSR.Create (2, $80);
      TPIC18x_MOVLW.Create ($33);
      TPIC18x_MOVLB.Create (0);
      TPIC18x_MOVWF.Create ($85, bank_mode);
      TPIC18x_MOVSF.Create (5, reg2);
      run_test;
      check_ram (reg2, $33);
      conclude_test
   end;

procedure movss_test;
   const
      reg2 = $10;
   begin
      start_instr_test ('movss');
      start_test (1);
      TPIC18x_LFSR.Create (2, $80);
      TPIC18x_MOVLW.Create ($33);
      TPIC18x_MOVLB.Create (0);
      TPIC18x_MOVWF.Create ($85, bank_mode);
      TPIC18x_MOVSS.Create (5, 6);
      run_test;
      check_ram ($86, $33);
      conclude_test
   end;

procedure pushl_test;
   const
      reg2 = $10;
   begin
      start_instr_test ('pushl');
      start_test (1);
      TPIC18x_LFSR.Create (2, $1ec);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_CLRF.Create ($ec, bank_mode);
      TPIC18x_PUSHL.Create (8);
      run_test;
      check_ram ($86, $33);
      check_ram (FSR2H, 1);
      check_ram (FSR2L, $eb);
      conclude_test
   end;

procedure subfsr_test;
   procedure test1;
      begin
         start_test (1);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR0H, access_mode);
         TPIC18x_MOVWF.Create (FSR0L, access_mode);
         TPIC18x_SUBFSR.Create (0, $37);
         run_test;
         check_ram (FSR0L, $ca);
         check_ram (FSR0H, 0);
         conclude_test
      end;
   procedure test2;
      begin
         start_test (2);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR1H, access_mode);
         TPIC18x_MOVWF.Create (FSR1L, access_mode);
         TPIC18x_SUBFSR.Create (1, $37);
         run_test;
         check_ram (FSR1L, $ca);
         check_ram (FSR1H, 0);
         conclude_test
      end;
   procedure test3;
      begin
         start_test (3);
         TPIC18x_MOVLW.Create (1);
         TPIC18x_MOVWF.Create (FSR2H, access_mode);
         TPIC18x_MOVWF.Create (FSR2L, access_mode);
         TPIC18x_SUBFSR.Create (2, $37);
         run_test;
         check_ram (FSR2L, $ca);
         check_ram (FSR2H, 0);
         conclude_test
      end;
   begin
      start_instr_test ('subfsr');
      test1;
      test2;
      test3;
   end;

procedure subulnk_test;
   begin
      start_instr_test ('subulnk');
      start_test (1);
      TPIC18x_MOVLW.Create (1);
      TPIC18x_MOVWF.Create (FSR2H, access_mode);
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_RCALL.Create.n_field := 1;
      TPIC18x_SLEEP.Create;
      TPIC18x_SUBULNK.Create ($37);
      run_test;
      check_ram (STKPTR, 0);
      check_ram (TOSU, 0);
      check_ram (TOSH, 0);
      check_ram (TOSL, 0);
      check_ram (FSR2L, $ca);
      check_ram (FSR2H, 0);
      conclude_test
   end;

procedure fsr_ind_test;
   begin
      start_instr_test ('fsr_ind');
      start_test (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_LFSR.Create (1, $124);
      TPIC18x_LFSR.Create (2, $125);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create (INDF0, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_MOVF.Create (INDF0, dest_w, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (INDF2, access_mode);
      TPIC18x_CLRF.Create (WREG, access_mode);
      TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);
      run_test;
      check_ram (FSR0L, $23);
      check_ram (FSR0H, 1);
      check_ram (FSR1L, $24);
      check_ram (FSR1H, 1);
      check_ram (FSR2L, $25);
      check_ram (FSR2H, 1);
      check_ram ($123, $55);
      check_ram ($124, $56);
      check_ram ($125, $57);
      conclude_test
   end;

procedure fsr_postinc_w_test;
   begin
      start_instr_test ('fsr_postinc_w');
      start_test (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_LFSR.Create (1, $124);
      TPIC18x_LFSR.Create (2, $125);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create (POSTINC0, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (POSTINC1, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (POSTINC2, access_mode);
      run_test;
      check_ram (FSR0L, $24);
      check_ram (FSR0H, 1);
      check_ram (FSR1L, $25);
      check_ram (FSR1H, 1);
      check_ram (FSR2L, $26);
      check_ram (FSR2H, 1);
      check_ram ($123, $55);
      check_ram ($124, $56);
      check_ram ($125, $57);
      conclude_test
   end;

procedure fsr_preinc_w_test;
   begin
      start_instr_test ('fsr_preinc_w');
      start_test (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_LFSR.Create (1, $124);
      TPIC18x_LFSR.Create (2, $125);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create (PREINC0, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (PREINC1, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (PREINC2, access_mode);
      run_test;
      check_ram (FSR0L, $24);
      check_ram (FSR0H, 1);
      check_ram (FSR1L, $25);
      check_ram (FSR1H, 1);
      check_ram (FSR2L, $26);
      check_ram (FSR2H, 1);
      check_ram ($124, $55);
      check_ram ($125, $56);
      check_ram ($126, $57);
      conclude_test
   end;

procedure fsr_postdec_w_test;
   begin
      start_instr_test ('fsr_postdec_w');
      start_test (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_LFSR.Create (1, $124);
      TPIC18x_LFSR.Create (2, $125);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create (POSTDEC0, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (POSTDEC1, access_mode);
      TPIC18x_INCF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      run_test;
      check_ram (FSR0L, $22);
      check_ram (FSR0H, 1);
      check_ram (FSR1L, $23);
      check_ram (FSR1H, 1);
      check_ram (FSR2L, $24);
      check_ram (FSR2H, 1);
      check_ram ($123, $55);
      check_ram ($124, $56);
      check_ram ($125, $57);
      conclude_test
   end;

procedure fsr_postinc0_r_test;
   begin
      start_instr_test ('fsr_postinc0_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      run_test;
      check_ram (FSR0L, $24);
      check_ram (FSR0H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_postinc1_r_test;
   begin
      start_instr_test ('fsr_postinc1_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (1, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      run_test;
      check_ram (FSR1L, $24);
      check_ram (FSR1H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_postinc2_r_test;
   begin
      start_instr_test ('fsr_postinc2_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (2, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTINC2, dest_w, access_mode);
      run_test;
      check_ram (FSR2L, $24);
      check_ram (FSR2H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_preinc0_r_test;
   begin
      start_instr_test ('fsr_preinc0_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($24, bank_mode);
      TPIC18x_MOVF.Create (PREINC0, dest_w, access_mode);
      run_test;
      check_ram (FSR0L, $24);
      check_ram (FSR0H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_preinc1_r_test;
   begin
      start_instr_test ('fsr_preinc1_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (1, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($24, bank_mode);
      TPIC18x_MOVF.Create (PREINC1, dest_w, access_mode);
      run_test;
      check_ram (FSR1L, $24);
      check_ram (FSR1H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_preinc2_r_test;
   begin
      start_instr_test ('fsr_preinc2_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (2, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($24, bank_mode);
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      run_test;
      check_ram (FSR2L, $24);
      check_ram (FSR2H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_postdec0_r_test;
   begin
      start_instr_test ('fsr_postdec0_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTDEC0, dest_w, access_mode);
      run_test;
      check_ram (FSR0L, $22);
      check_ram (FSR0H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_postdec1_r_test;
   begin
      start_instr_test ('fsr_postdec1_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (1, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTDEC1, dest_w, access_mode);
      run_test;
      check_ram (FSR1L, $22);
      check_ram (FSR1H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_postdec2_r_test;
   begin
      start_instr_test ('fsr_postdec2_r');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_LFSR.Create (2, $123);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create ($23, bank_mode);
      TPIC18x_MOVF.Create (POSTDEC2, dest_w, access_mode);
      run_test;
      check_ram (FSR2L, $22);
      check_ram (FSR2H, 1);
      check_w ($55);
      conclude_test
   end;

procedure fsr_plusw0_r_test;
   begin
      start_instr_test ('fsr_plusw0_r');
      start_test (1);
      TPIC18x_MOVLW.Create ($66);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_MOVWF.Create ($20, bank_mode);
      TPIC18x_LFSR.Create (0, $123);
      TPIC18x_MOVLW.Create (byte(-3));
      TPIC18x_MOVF.Create (PLUSW0, dest_w, access_mode);
      run_test;
      check_ram (FSR0L, $23);
      check_ram (FSR0H, 1);
      check_w ($66);
      conclude_test
   end;

procedure fsr_plusw1_r_test;
   begin
      start_instr_test ('fsr_plusw1_r');
      start_test (1);
      TPIC18x_MOVLW.Create ($66);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_MOVWF.Create ($20, bank_mode);
      TPIC18x_LFSR.Create (1, $123);
      TPIC18x_MOVLW.Create (byte(-3));
      TPIC18x_MOVF.Create (PLUSW1, dest_w, access_mode);
      run_test;
      check_ram (FSR1L, $23);
      check_ram (FSR1H, 1);
      check_w ($66);
      conclude_test
   end;

procedure fsr_plusw2_r_test;
   begin
      start_instr_test ('fsr_plusw2_r');
      start_test (1);
      TPIC18x_MOVLW.Create ($66);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_MOVWF.Create ($20, bank_mode);
      TPIC18x_LFSR.Create (2, $123);
      TPIC18x_MOVLW.Create (byte(-3));
      TPIC18x_MOVF.Create (PLUSW2, dest_w, access_mode);
      run_test;
      check_ram (FSR2L, $23);
      check_ram (FSR2H, 1);
      check_w ($66);
      conclude_test
   end;

procedure fsr_plusw_w_test;
   begin
      start_instr_test ('fsr_plusw_w');
      start_test (1);
      TPIC18x_MOVLB.Create (1);
      TPIC18x_CLRF.Create ($20, bank_mode);
      TPIC18x_CLRF.Create ($21, bank_mode);
      TPIC18x_CLRF.Create ($22, bank_mode);
      TPIC18x_LFSR.Create (0, $130);
      TPIC18x_LFSR.Create (1, $131);
      TPIC18x_LFSR.Create (2, $132);
      TPIC18x_MOVLW.Create (byte(-16));
      TPIC18x_MOVWF.Create (PLUSW0, access_mode);
      TPIC18x_MOVWF.Create (PLUSW1, access_mode);
      TPIC18x_MOVWF.Create (PLUSW2, access_mode);
      run_test;
      check_ram (FSR0L, $30);
      check_ram (FSR0H, 1);
      check_ram (FSR1L, $31);
      check_ram (FSR1H, 1);
      check_ram (FSR2L, $32);
      check_ram (FSR2H, 1);
      check_ram ($120, byte(-16));
      check_ram ($121, byte(-16));
      check_ram ($122, byte(-16));
      conclude_test
   end;

procedure run_instruction_simulation_tests;
   begin
      tests_run := 0;
      errors := 0;
      MainForm.TestResultsMemo.Clear;
      movlw_test;
      movlb_test;
      andlw_test;
      addlw_test;
      iorlw_test;
      xorlw_test;
      sublw_test;
      lfsr_test;
      mullw_test;
      movwf_test;
      addwf_test;
      addwfc_test;
      andwf_test;
      clrf_test;
      comf_test;
      incf_test;
      decf_test;
      subfwb_test;
      subwf_test;
      subwfb_test;
      negf_test;
      iorwf_test;
      xorwf_test;
      movf_test;
      movff_test;
      mulwf_test;
      setf_test;
      swapf_test;
      rlcf_test;
      rlncf_test;
      rrcf_test;
      rrncf_test;
      tstfsz_test;
      cpfseq_test;
      cpfsgt_test;
      cpfslt_test;
      decfsz_test;
      dcfsnz_test;
      incfsz_test;
      infsnz_test;
      bcf_test;
      bsf_test;
      btg_test;
      btfsc_test;
      btfss_test;
      bc_test;
      bnc_test;
      bn_test;
      bnn_test;
      bov_test;
      bnov_test;
      bz_test;
      bnz_test;
      bra_test;
      rcall_test;
      retlw_test;
      return_test;
      push_test;
      pop_test;
      call_test;
      goto_test;
      tblrd_test;
      addfsr_test;
      addulnk_test;
      callw_test;
      movsf_test;
      movss_test;
      pushl_test;
      subfsr_test;
      subulnk_test;
      fsr_ind_test;
      fsr_postinc_w_test;
      fsr_preinc_w_test;
      fsr_postdec_w_test;
      fsr_postinc0_r_test;
      fsr_postinc1_r_test;
      fsr_postinc2_r_test;
      fsr_preinc0_r_test;
      fsr_preinc1_r_test;
      fsr_preinc2_r_test;
      fsr_postdec0_r_test;
      fsr_postdec1_r_test;
      fsr_postdec2_r_test;
      fsr_plusw0_r_test;
      fsr_plusw1_r_test;
      fsr_plusw2_r_test;
      fsr_plusw_w_test;
      out (IntToStr (tests_run) + ' tests completed, ' + IntToStr(errors) + ' errors.')
   end;

END.
