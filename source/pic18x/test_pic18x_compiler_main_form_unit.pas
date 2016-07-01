UNIT test_pic18x_compiler_main_form_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
{$ifdef FPC}
   LCLType, LMessages,
   LCLIntf, LResources,
   htmlview,
{$ELSE}
   Windows,
   ActiveX,
   OleCtrls,
   SHDocVw,
{$endif}
   SysUtils, Classes, ComCtrls,
   pic18x_statements_unit, pic18x_blocks_unit, cpc_definitions_unit,
   Spin, Controls, StdCtrls,
   Forms, Messages, ExtCtrls;

type
   TMainForm =
      class(TForm)
         Button3: TButton;
         SrcToClipboardForTestButton: TButton;
         SrcToClipboardButton: TButton;
         KernelTestWebbrowserBasePanel: TPanel;
         PageControl1: TPageControl;
         TabSheet1: TTabSheet;
         TabSheet2: TTabSheet;
         Memo: TMemo;
         CompileResultsMemo: TMemo;
         ClearMemoButton: TButton;
         CompileMemoButton: TButton;
         AssemblySourceMemo: TMemo;
         TabSheet4: TTabSheet;
         TestResultsMemo: TMemo;
         TestSimulatorButton: TButton;
         TestCompilerButton: TButton;
         RunButton: TButton;
         Label1: TLabel;
         TabSheet3: TTabSheet;
         TraceMemo: TMemo;
         Label2: TLabel;
         CleanupTestSrcButton: TButton;
         procedure ClearMemoButtonClick
            (Sender: TObject
            );
         procedure CompileMemoButtonClick
            (Sender: TObject
            );
         procedure TestSimulatorButtonClick(Sender: TObject);
         procedure TestCompilerButtonClick(Sender: TObject);
         procedure RunButtonClick(Sender: TObject);
         procedure CleanupTestSrcButtonClick(Sender: TObject);
         procedure Button3Click(Sender: TObject);
         procedure SrcToClipboardForTestButtonClick(Sender: TObject);
         procedure SrcToClipboardButtonClick(Sender: TObject);
         procedure FormCreate(Sender: TObject);
      public
         number_of_tests: integer;
{$IFDEF FPC}
         kernel_test_result_html_viewer: THtmlViewer;
{$ELSE}
         kernel_test_result_html_viewer: TWebBrowser;
{$ENDIF}
      end;

var
   MainForm: TMainForm;

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

IMPLEMENTATION

{$R *.dfm}

uses
   cpc_target_cpu_unit, pic18x_instruction_simulation_test_unit, cpc_main_compiler_unit, pic18x_kernel_unit,
   cpc_multi_precision_integer_unit, pic18x_cpu_unit, test_pic18x_compiler_unit, test_pic18x_kernel_unit,
   pic18x_instructions_unit, test_pic18x_simulator_unit, pic18x_macro_instructions_unit, pic18x_multiply_divide_unit,
   pic18x_microprocessor_information_unit, pic18x_run_time_error_check_unit, cpc_source_analysis_unit,
   cpc_common_unit, ClipBrd, pic18x_main_compiler_unit;


var
   test_program: TPIC18x_Program;
   number_of_errors: integer;
   src: TStringList;
   compilation_result: TCompilationResult;

procedure add (s: string);
   begin
      src.Add (s)
   end;

procedure error (s: string);
   begin
      MainForm.TestResultsMemo.Lines.Add ('   ' + s);
      number_of_errors := number_of_errors + 1
   end;

var
   idx: integer;

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
      MainForm.TestResultsMemo.Lines.Add ('test ' + IntToStr(testno));
      MainForm.number_of_tests := MainForm.number_of_tests + 1;
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

function rand (size: integer): int64;
   // size is negative for signed, positive for unsigned
   var ov: record
              case integer of
                 0: (i: int64);
                 1: (a: array [0..7] of ShortInt)
              end;
      i: integer;
   begin
      for i := 0 to abs(size)-1 do
         ov.a[i] := random (256) - 128;

      for i := abs(size) to 7 do
         if size >= 0 then
            ov.a[i] := 0
         else
            if ov.a[abs(size)-1] < 0 then
               ov.a[i] := -1
            else
               ov.a[i] := 0;
      result := ov.i
   end;

function rand_clamped_msbit (size: integer): int64;
   // size is negative for signed, positive for unsigned
   var ov: record
              case integer of
                 0: (i: int64);
                 1: (a: array [0..7] of ShortInt)
              end;
      i: integer;
   begin
      for i := 0 to abs(size)-2 do
         ov.a[i] := random (256) - 128;
      ov.a[abs(size)-1] := random (128);
      if size < 0 then
         ov.a[abs(size)-1] := ov.a[abs(size)-1] - 64;

      for i := abs(size) to 7 do
         if size >= 0 then
            ov.a[i] := 0
         else
            if ov.a[abs(size)-1] < 0 then
               ov.a[i] := -1
            else
               ov.a[i] := 0;
      result := ov.i
   end;

function all_ones (size: integer): uint64;
   var ov: record
              case integer of
                 0: (i: uint64);
                 1: (a: array [0..7] of Byte)
              end;
      i: integer;
   begin
      ov.i := 0;
      for i := 0 to abs(size)-1 do
         ov.a[i] := $ff;
      result := ov.i
   end;

procedure TMainForm.CleanupTestSrcButtonClick(Sender: TObject);
var i,j,k: integer;
 s: string;
 x: boolean;
begin
   for i := 0 to memo.Lines.Count-1
   do begin
         s := memo.Lines[i];
         j := pos('add (''', s);
         if j > 0 then
            for k := 0 to 5 do
               begin
                  s[j+k] := ' ';
               end;
         x := true;
         j := Length(s);
         while x and (j > 2) do
            begin
               if (s[j-2] = '''') and (s[j-1] = ')') and (s[j] = ';') then
                  begin
                     s[j-2] := ' ';
                     s[j-1] := ' ';
                     s[j] := ' ';
                     x := false
                  end;
               j := j-1
            end;
         j := 1;
         while j < length(s) do
               if (s[j] = '''') and (s[j+1] = '''') then
                  begin
                     for k := j+1 to length(s)-1
                        do s[k] := s[k+1];
                     setlength(s, length(s)-1)
                  end
               else
                  j := j + 1;
         if pos('            ', s) = 1 then
            s := Copy(s, 13, 9999);
         s := TrimRight(s);
         memo.Lines[i] := s
      end
end;

procedure TMainForm.Button3Click(Sender: TObject);
   var
      i: integer;
      sl: TStringList;
      untested: boolean;
      Doc: Variant;
   begin
      sl := TStringList.Create;
      GenerateKernelTestCoverageMap := true;
      TestResultsMemo.Clear;
      number_of_tests := 0;
      number_of_errors := 0;
      run_kernel_tests;
      TestResultsMemo.Lines.Add (format ('%d tests run, %d errors', [number_of_tests, number_of_errors]));
      sl.Add('<body bgcolor="#FFFFFF">');
      sl.Add('<font face="courier new">');
      for i := 1 to Length(KernelInstructions)-1
      do begin
            untested := (KernelInstructions[i].execution_count = 0)
                        and
                        not ((KernelInstructions[i].classname = 'TAssemblyLabel')
                             or
                             (KernelInstructions[i].classname = 'TAssemblySourceBlankLine')
                             or
                             (KernelInstructions[i].classname = 'TAssemblyComment')
                             or
                             (KernelInstructions[i].classname = 'TSourceLine')
                            );
            if untested
            then
               sl.Add('<font color=red>');
            sl.Add(StringReplace (KernelInstructions[i].instrxxx, ' ', '&nbsp', [rfReplaceAll]) + '<br>');
            if untested
            then
               sl.Add('</font>')
         end;
      sl.Add('</font>');
      sl.Add('</body>');
{$IFDEF FPC}
      kernel_test_result_html_viewer.LoadFromString (sl.Text);
{$ELSE}
  //    kernel_test_result_html_viewer.navigate2 ('about:blank');
      Doc := kernel_test_result_html_viewer.Document;
      Doc.Clear;
      Doc.Write (sl.Text);
      Doc.Close;
{$ENDIF}
      GenerateKernelTestCoverageMap := false;
      SetLength (KernelInstructions, 0);
      sl.Free
   end;

procedure TMainForm.SrcToClipboardForTestButtonClick(Sender: TObject);
   var
      i: integer;
      s: string;
      sl: TStringList;
   begin
      sl := TStringList.Create;
      for i := 0 to memo.Lines.Count-1 do
         begin
            s := memo.Lines[i];
            s := StringReplace(s, '''', '''''', [rfReplaceAll]);
            sl.Add ('      add (''' + s + ''');')
         end;
      clipboard.AsText := sl.Text;
      sl.Free
   end;

procedure TMainForm.SrcToClipboardButtonClick(Sender: TObject);
   var
      i: integer;
      sl: TStringList;
   begin
      sl := TStringList.Create;
      for i := 0 to memo.Lines.Count-1 do
         sl.Add(memo.Lines[i]);
      clipboard.AsText := sl.Text;
      sl.Free
   end;

procedure TMainForm.ClearMemoButtonClick
   (Sender: TObject
   );
   begin
      Memo.Clear
   end;
   
procedure TMainForm.TestSimulatorButtonClick(Sender: TObject);
   begin
      run_instruction_simulation_tests
   end;

procedure TMainForm.TestCompilerButtonClick(Sender: TObject);
{$IFNDEF FPC}
   var
      v: OleVariant;
{$ENDIF}
   begin
      TestResultsMemo.Lines.Add (format ('%d tests run, %d errors', [number_of_tests, number_of_errors]));
{$IFDEF FPC}
      kernel_test_result_html_viewer.LoadFromString ('<body bgcolor="#FFFFFF"></body>');
{$ELSE}
      v := 'about:blank';
      kernel_test_result_html_viewer.navigate2 (v);
{$ENDIF}
      TestResultsMemo.Clear;
      number_of_tests := 0;
      number_of_errors := 0;
      RunTests
   end;

procedure TMainForm.CompileMemoButtonClick
   (Sender: TObject
   );
   var
      compilation: TCompilation;
   begin
      no_sleep_on_idle := true;
      CompileResultsMemo.Clear;
      Application.ProcessMessages;
      ClearRunTimeErrorLists;
      compilation := TCompilation.CreateFromStrings (MainForm.Memo.Lines, ProgramGenerator, CompileResultsMemo.Lines);
      compilation_result := compilation.compilation_result;
      AssemblySourceMemo.Lines.Assign(ProgramCode.assembly_source_code);
      ProgramCode.Clear;
      CPU.deallocate_special_sfrs;
      if compilation_result = compile_error_in_source then
         begin
            Memo.SetFocus;
 //           Memo.SelStart := Memo.Perform (EM_LINEINDEX, compilation.compiler_error_source_location.line_no-1, 0) + compilation.compiler_error_source_location.line_idx-1;
            Memo.SelLength := 0;
            Memo.Perform (EM_SCROLLCARET, 0, 0)
         end;
      compilation.Free;
      pic_info_Free;
      no_sleep_on_idle := false
   end;

procedure TMainForm.RunButtonClick(Sender: TObject);
   var
      compilation: TCompilation;
      ok: boolean;
      test_program: TPIC18x_Program;
      error_pc: integer;
      error_message: string;
      error_src_loc: TSourceLocation;
      trace_pc: integer;
      trace_assembly, trace_status: string;
   begin
      test_program := nil;  // suppress compiler warning
      CompileResultsMemo.Clear;
      ClearRunTimeErrorLists;
      compilation := TCompilation.CreateFromStrings (MainForm.Memo.Lines, ProgramGenerator, CompileResultsMemo.Lines);
      ok := compilation.compilation_result = compiled_ok;
      if ok then
         begin
            test_program := TPIC18x_Program(compilation.compiled_object);
            test_program.AddRef
         end;
      compilation.Free;
      AssemblySourceMemo.Lines.Assign(ProgramCode.assembly_source_code);
      if not ok then
         label1.caption := 'didn''t compile'
      else
         begin
            TraceMemo.Clear;
            cpu.Reset;
            cpu.running := true;
            while cpu.running do
               begin
                  trace_pc := cpu.pc;
                  trace_assembly := ProgramCode.AssemblyCode(trace_pc);
                  while Length(trace_assembly) < 25 do
                     trace_assembly := trace_assembly + ' ';
                  ProgramCode.Execute;

                  if cpu.n then
                     trace_status := 'N '
                  else
                     trace_status := 'n ';

                  if cpu.ov then
                     trace_status := trace_status + 'OV '
                  else
                     trace_status := trace_status + 'ov ';

                  if cpu.z then
                     trace_status := trace_status + 'Z '
                  else
                     trace_status := trace_status + 'z ';

                  if cpu.dc then
                     trace_status := trace_status + 'DC '
                  else
                     trace_status := trace_status + 'dc ';

                  if cpu.c then
                     trace_status := trace_status + 'C'
                  else
                     trace_status := trace_status + 'C';

                  TraceMemo.Lines.Add (format ('%6.6X  %s  %2.2X  %s', [trace_pc, trace_assembly, cpu.w, trace_status]));
               end;
            error_pc := (cpu.ram[error_logU] shl 16) + (cpu.ram[error_logH] shl 8) + cpu.ram[error_logL];
            if error_pc = 0 then
               CompileResultsMemo.Lines.Add ('No run-time errors reported')
            else
               begin
                  GetRunTimeErrorInfo (error_pc, error_message, error_src_loc);
                  CompileResultsMemo.Lines.Add (format ('Run-time Error detected at %6.6X:', [error_pc]));
                  CompileResultsMemo.Lines.Add (error_src_loc.line);
                  CompileResultsMemo.Lines.Add (error_src_loc.caret + ' ' + error_message);
               end;
            label1.caption := error_message;

            test_program.Release;
            ProgramCode.Clear
         end;
      pic_info_Free
   end;

procedure TMainForm.FormCreate(Sender: TObject);
{$IFNDEF FPC}
   var
      v: OleVariant;
{$ENDIF}
   begin
{$IFDEF FPC}
      kernel_test_result_html_viewer := THtmlViewer.Create (self);
      kernel_test_result_html_viewer.Parent := KernelTestWebbrowserBasePanel;
      kernel_test_result_html_viewer.LoadFromString ('<body bgcolor="#FFFFFF"></body>');
{$ELSE}
      KernelTestWebbrowserBasePanel.BevelWidth := 1;
      KernelTestWebbrowserBasePanel.BorderWidth := 0;
      KernelTestWebbrowserBasePanel.BevelInner := bvNone;
      KernelTestWebbrowserBasePanel.BevelOuter := bvRaised;
      kernel_test_result_html_viewer := TWebBrowser.Create(self);
      TWinControl(kernel_test_result_html_viewer).Parent := KernelTestWebbrowserBasePanel;
      v := 'about:blank';
      kernel_test_result_html_viewer.navigate2 (v);
{$ENDIF}
      kernel_test_result_html_viewer.Align := alClient;
      PageControl1.ActivePageIndex := 0;
      SetCurrentDir (ExtractFilePath(ParamStr(0)) + 'pic18x' + PathDelim + 'compiler_test_cases')
   end;

INITIALIZATION
   src := TStringList.Create;

FINALIZATION
   src.Free

END.
