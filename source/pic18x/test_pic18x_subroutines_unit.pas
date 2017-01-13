UNIT test_pic18x_subroutines_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

var
   number_of_tests: integer;
   number_of_errors: integer;

procedure add (s: string);
procedure start_test (testno: integer);
procedure conclude_test;
procedure test_abs_value (addr: integer; value0: byte);
   overload;
procedure test_abs_value (addr: integer; value0, value1: byte);
   overload;
procedure test_abs_value (addr: integer; value0, value1, value2: byte);
   overload;
procedure test_abs_value (addr: integer; value0, value1, value2, value3: byte);
   overload;
procedure test_abs_value (addr: integer; value0, value1, value2, value3, value4: byte);
   overload;
procedure test_sfr_value (sfr_name: string; value: byte);
procedure test_abs_int_value (addr: integer; size, value: integer);
procedure test_abs_value_non_zero (addr, count: integer);
procedure test_run_time_error_detected (addr: integer; err_msg: string);
procedure error (s: string);
procedure display_test_result (s: string);

IMPLEMENTATION

uses
{$IFNDEF CONSOLE_TEST_MODE}
   main_form_unit,
{$ENDIF}
   Classes,
   cpc_main_compiler_unit,
   cpc_multi_precision_integer_unit,
   cpc_source_analysis_unit,
   pic18x_blocks_unit,
   pic18x_instructions_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   pic18x_main_compiler_unit,
   pic18x_microprocessor_information_unit,
   pic18x_run_time_error_check_unit,
   SysUtils,
   test_pic18x_kernel_unit,
   test_pic18x_simulator_unit;

var
   src: TStringList;
   test_program: TPIC18x_Program;
   idx: integer;

procedure display_test_result (s: string);
   begin
{$IFDEF CONSOLE_TEST_MODE}
      writeln (s)
{$ELSE}
      MainForm.TestResultsMemo.Lines.Add (s)
{$ENDIF}
   end;

procedure add_instr_to_coverage_map_test (i: TInstruction);
   begin
      idx := Length (KernelInstructions);
      SetLength (KernelInstructions, idx+1);
      if i is TSourceLine then
         KernelInstructions[idx].instrxxx := '                                    &#59; ' + i.annotation
      else
         KernelInstructions[idx].instrxxx := i.assembly_line;
      KernelInstructions[idx].classname := i.ClassName;
      i.test_coverage_index := idx
   end;

procedure check_instr_in_coverage_map_test (i: TInstruction);
   begin
      idx := idx + 1;
      assert (KernelInstructions[idx].classname = i.classname, KernelInstructions[idx].classname + '|' + i.classname);
      i.test_coverage_index := idx
   end;



procedure add (s: string);
   begin
      src.Add (s)
   end;

procedure start_test (testno: integer);
   type tproc = procedure (i: TInstruction);
   procedure for_all_kernel_instructions_do (p: tproc);
      var
         i,j: integer;
         in_kernel: boolean;
      begin
         in_kernel := false;
         for i := 0 to Length(ProgramCode.instr_arr)-1 do
            if ProgramCode.instr_arr[i] = StartKernelCoverageTest then
               in_kernel := true
            else if ProgramCode.instr_arr[i] = EndKernelCoverageTest then
               in_kernel := false
            else if in_kernel then
               begin
                  if ProgramCode.instr_arr[i] is TMacroInstruction then
                     for j := 0 to Length(TMacroInstruction(ProgramCode.instr_arr[i]).instr_arr)-1 do
                        p (TMacroInstruction(ProgramCode.instr_arr[i]).instr_arr[j])
                  else
                     p (ProgramCode.instr_arr[i])
               end
      end;
   function caret
      (line_idx: integer
      ): string;
      var
         i: integer;
      begin
         result := '';
         for i := 1 to line_idx - 1 do
            result := result + ' ';
         result := result + '^'
      end;
   var
      fn: string;
      listing: TStringList;
      compilation: TCompilation;
   begin
      listing := TStringList.Create;
      display_test_result ('test ' + IntToStr(testno));
      number_of_tests := number_of_tests + 1;
      fn := format ('test_%.5d', [testno]);
      ClearRunTimeErrorLists;
      compilation := TCompilation.CreateFromStrings (src, ProgramGenerator, listing);
      test_program := TPIC18x_Program(compilation.compiled_object);
      if compilation.compilation_result <> compiled_ok then
         error ('*** failed to compile ***')
      else
         begin
            test_program := TPIC18x_Program(compilation.compiled_object);
            if test_program <> nil then
               begin
                  test_program.AddRef;

                  if GenerateKernelTestCoverageMap then
                     if Length(KernelInstructions) = 0
                     then  // build KernelInstructions array from first kernel test
                        begin
                           SetLength (KernelInstructions, 1);  // first location won't be used
                           for_all_kernel_instructions_do (add_instr_to_coverage_map_test)
                        end
                     else   // additional kernel tests, compare KernelInstructions array to first
                        begin
                           idx := 0;
                           for_all_kernel_instructions_do (check_instr_in_coverage_map_test)
                        end;

                  cpu.Reset;
                  cpu.running := true;
                  while cpu.running do
                     ProgramCode.Execute
               end
            end;
      listing.Free;
      compilation.Free
   end;

procedure conclude_test;
   begin
      CPU.deallocate_special_sfrs;
      test_program.Release;
      ProgramCode.Clear;
      pic_info_Free;
      src.Clear
   end;

procedure test_abs_value (addr: integer; value0: byte);
   overload;
   begin
      if cpu.ram[next_available_absolute_address + addr] <> value0 then
         error (format ('ram abs %d: %X expected, %X found', [addr, value0, cpu.ram [next_available_absolute_address + addr]]))
   end;

procedure test_abs_value (addr: integer; value0, value1: byte);
   overload;
   begin
      if cpu.ram[next_available_absolute_address + addr] <> value0 then
         error (format ('ram abs %d: %X expected, %X found', [addr, value0, cpu.ram [next_available_absolute_address + addr]]));
      if cpu.ram[next_available_absolute_address + addr + 1] <> value1 then
         error (format ('ram abs %d: %X expected, %X found', [addr+1, value1, cpu.ram [next_available_absolute_address + addr + 1]]))
   end;

procedure test_abs_value (addr: integer; value0, value1, value2: byte);
   overload;
   begin
      if cpu.ram[next_available_absolute_address + addr] <> value0 then
         error (format ('ram abs %d: %X expected, %X found', [addr, value0, cpu.ram [next_available_absolute_address + addr]]));
      if cpu.ram[next_available_absolute_address + addr + 1] <> value1 then
         error (format ('ram abs %d: %X expected, %X found', [addr+1, value1, cpu.ram [next_available_absolute_address + addr + 1]]));
      if cpu.ram[next_available_absolute_address + addr + 2] <> value2 then
         error (format ('ram abs %d: %X expected, %X found', [addr+2, value2, cpu.ram [next_available_absolute_address + addr + 2]]))
   end;

procedure test_abs_value (addr: integer; value0, value1, value2, value3: byte);
   overload;
   begin
      if cpu.ram[next_available_absolute_address + addr] <> value0 then
         error (format ('ram abs %d: %X expected, %X found', [addr, value0, cpu.ram [next_available_absolute_address + addr]]));
      if cpu.ram[next_available_absolute_address + addr + 1] <> value1 then
         error (format ('ram abs %d: %X expected, %X found', [addr+1, value1, cpu.ram [next_available_absolute_address + addr + 1]]));
      if cpu.ram[next_available_absolute_address + addr + 2] <> value2 then
         error (format ('ram abs %d: %X expected, %X found', [addr+2, value2, cpu.ram [next_available_absolute_address + addr + 2]]));
      if cpu.ram[next_available_absolute_address + addr + 3] <> value3 then
         error (format ('ram abs %d: %X expected, %X found', [addr+3, value3, cpu.ram [next_available_absolute_address + addr + 3]]))
   end;

procedure test_abs_value (addr: integer; value0, value1, value2, value3, value4: byte);
   overload;
   begin
      if cpu.ram[next_available_absolute_address + addr] <> value0 then
         error (format ('ram abs %d: %X expected, %X found', [addr, value0, cpu.ram [next_available_absolute_address + addr]]));
      if cpu.ram[next_available_absolute_address + addr + 1] <> value1 then
         error (format ('ram abs %d: %X expected, %X found', [addr+1, value1, cpu.ram [next_available_absolute_address + addr + 1]]));
      if cpu.ram[next_available_absolute_address + addr + 2] <> value2 then
         error (format ('ram abs %d: %X expected, %X found', [addr+2, value2, cpu.ram [next_available_absolute_address + addr + 2]]));
      if cpu.ram[next_available_absolute_address + addr + 3] <> value3 then
         error (format ('ram abs %d: %X expected, %X found', [addr+3, value3, cpu.ram [next_available_absolute_address + addr + 3]]));
      if cpu.ram[next_available_absolute_address + addr + 4] <> value4 then
         error (format ('ram abs %d: %X expected, %X found', [addr+4, value3, cpu.ram [next_available_absolute_address + addr + 4]]))
   end;

procedure test_sfr_value (sfr_name: string; value: byte);
   begin
      sfr_name := uppercase (sfr_name);
      if cpu.ram_no_adshr_check(pic_info.SFR_Address(sfr_name) and $FFF) <> value then
         error (format ('sfr %s: %X expected, %X found', [sfr_name, value, cpu.ram_no_adshr_check (pic_info.SFR_Address(sfr_name) and $FFF)]))
   end;

procedure test_abs_int_value (addr: integer; size, value: integer);
   var
      mpi: TMultiPrecisionInteger;
      byte_addr, i: integer;
   begin
      mpi := TMultiPrecisionInteger.Create (value);
      for i := 0 to size-1 do
         begin
            byte_addr := next_available_absolute_address + addr + i;
            if cpu.ram[byte_addr] <> mpi.AsByte(size-i-1) then
               error (format ('ram abs %d: %d expected, %d found', [byte_addr-next_available_absolute_address, mpi.AsByte(i), cpu.ram [byte_addr]]))
         end;
      mpi.Free
   end;

procedure test_abs_value_non_zero (addr, count: integer);
   var
      i: integer;
      ok: boolean;
   begin
      ok := false;
      for i := 0 to count-1 do
         if cpu.ram[next_available_absolute_address + addr + i] <> 0 then
            ok := true;
      if not ok then
         error (format ('ram abs %d*%d should be non-zero', [addr, count]))
   end;

procedure test_run_time_error_detected (addr: integer; err_msg: string);
   var
      pc: integer;
      msg: string;
      src_loc: TSourceLocation;
   begin
      pc := (cpu.ram[next_available_absolute_address+addr] shl 16) + (cpu.ram[next_available_absolute_address+addr+1] shl 8) + cpu.ram[next_available_absolute_address+addr+2];
      if not GetRunTimeErrorInfo (pc, msg, src_loc) then
         error ('run-time error not detected @' + IntToStr(addr) + ': "' + err_msg + '"')
      else if msg <> err_msg then
         error ('different run-time error detected: @' + IntToStr(addr) + ': "' + msg + '" instead of "' + err_msg + '"')
   end;

procedure error (s: string);
   begin
      display_test_result ('   ' + s);
      number_of_errors := number_of_errors + 1
   end;

   
INITIALIZATION
   src := TStringList.Create;

FINALIZATION
   src.Free

END.
