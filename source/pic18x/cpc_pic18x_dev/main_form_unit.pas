UNIT main_form_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
{$ifdef FPC}
   LCLIntf,
   LCLType,
   LMessages,
   LResources,
{$ELSE}
   ActiveX,
   OleCtrls,
   SHDocVw,
   Windows,
{$endif}
   Classes,
   ComCtrls,
   Controls,
   ExtCtrls,
   Forms,
   Menus,
   Messages,
   pic18x_blocks_unit,
   pic18x_statements_unit,
   Spin,
   StdCtrls,
   SysUtils, Dialogs;

type
   TMainForm =
      class(TForm)
         SrcToClipboardForTestButton: TButton;
         SrcToClipboardButton: TButton;
         PageControl1: TPageControl;
         TabSheet1: TTabSheet;
         TabSheet2: TTabSheet;
         SourceMemo: TMemo;
         CompileResultsMemo: TMemo;
         ClearMemoButton: TButton;
         CompileMemoButton: TButton;
         AssemblySourceMemo: TMemo;
         TabSheet4: TTabSheet;
         TestResultsMemo: TMemo;
         TestCompilerButton: TButton;
         RunButton: TButton;
         Label1: TLabel;
         TabSheet3: TTabSheet;
         TraceMemo: TMemo;
         Label2: TLabel;
         CleanupTestSrcButton: TButton;
         MainMenu1: TMainMenu;
         AboutMenuItem: TMenuItem;
         AboutTestCPCPIC18x1: TMenuItem;
    FileMenu: TMenuItem;
         OpenMenuItem: TMenuItem;
         SaveMenuItem: TMenuItem;
         SaveAsMenuItem: TMenuItem;
         ExitMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
         procedure ClearMemoButtonClick
            (Sender: TObject
            );
         procedure CompileMemoButtonClick
            (Sender: TObject
            );
         procedure TestCompilerButtonClick(Sender: TObject);
         procedure RunButtonClick(Sender: TObject);
         procedure CleanupTestSrcButtonClick(Sender: TObject);
         procedure SrcToClipboardForTestButtonClick(Sender: TObject);
         procedure SrcToClipboardButtonClick(Sender: TObject);
         procedure FormCreate(Sender: TObject);
         procedure AboutTestCPCPIC18x1Click(Sender: TObject);
         procedure ExitMenuItemClick(Sender: TObject);
         procedure OpenMenuItemClick(Sender: TObject);
         procedure SaveMenuItemClick(Sender: TObject);
         procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SourceMemoChange(Sender: TObject);
      private
         f_ide_source_file_name: string;
         f_ide_source_file_changed: boolean;
         procedure set_caption;
         procedure set_ide_source_file_name (s: string);
         procedure set_ide_source_file_changed (b: boolean);
         property ide_source_file_name: string read f_ide_source_file_name write set_ide_source_file_name;
         property ide_source_file_changed: boolean read f_ide_source_file_changed write set_ide_source_file_changed;
      end;

var
   MainForm: TMainForm;


IMPLEMENTATION

{$R *.dfm}

uses
   ClipBrd,
   cpc_common_unit,
   cpc_main_compiler_unit,
   cpc_multi_precision_integer_unit,
   cpc_source_analysis_unit,
   cpc_target_cpu_unit,
   pic18x_compiler_dev_aboutbox_unit,
   pic18x_cpu_unit,
   pic18x_instruction_simulation_test_unit,
   pic18x_instructions_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   pic18x_main_compiler_unit,
   pic18x_microprocessor_information_unit,
   pic18x_multiply_divide_unit,
   pic18x_run_time_error_check_unit,
   test_pic18x_compiler_unit,
   test_pic18x_kernel_unit,
   test_pic18x_simulator_unit,
   test_pic18x_subroutines_unit, test_temp_directory_unit;


var
   compilation_result: TCompilationResult;

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

procedure TMainForm.AboutTestCPCPIC18x1Click(Sender: TObject);
   begin
      CPCPIC18xDevAboutBox.ShowModal
   end;

procedure TMainForm.CleanupTestSrcButtonClick(Sender: TObject);
var i,j,k: integer;
 s: string;
 x: boolean;
begin
   for i := 0 to SourceMemo.Lines.Count-1
   do begin
         s := SourceMemo.Lines[i];
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
         SourceMemo.Lines[i] := s
      end
end;

procedure TMainForm.SrcToClipboardForTestButtonClick(Sender: TObject);
   var
      i: integer;
      s: string;
      sl: TStringList;
   begin
      sl := TStringList.Create;
      for i := 0 to SourceMemo.Lines.Count-1 do
         begin
            s := SourceMemo.Lines[i];
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
      for i := 0 to SourceMemo.Lines.Count-1 do
         sl.Add(SourceMemo.Lines[i]);
      clipboard.AsText := sl.Text;
      sl.Free
   end;

procedure TMainForm.ClearMemoButtonClick
   (Sender: TObject
   );
   begin
      SourceMemo.Clear
   end;

procedure TMainForm.TestCompilerButtonClick(Sender: TObject);
   begin
      TestResultsMemo.Clear;
      number_of_tests := 0;
      number_of_errors := 0;
      RunTests;
      TestResultsMemo.Lines.Add ('');
      TestResultsMemo.Lines.Add (format ('%d tests run, %d errors', [number_of_tests, number_of_errors]))
   end;

procedure TMainForm.CompileMemoButtonClick
   (Sender: TObject
   );
   var
      compilation: TCompilation;
   begin
      source_file_name := ide_source_file_name;
      SourceMemo.Lines.SaveToFile (source_file_name);
      no_sleep_on_idle := true;
      CompileResultsMemo.Clear;
      Application.ProcessMessages;
      ClearRunTimeErrorLists;
      compilation := TCompilation.CreateFromFile (source_file_name, ProgramGenerator, CompileResultsMemo.Lines);
      compilation_result := compilation.compilation_result;
      AssemblySourceMemo.Lines.Assign(ProgramCode.assembly_source_code);
      ProgramCode.Clear;
      CPU.deallocate_special_sfrs;
{$IFDEF MSWINDOWS}
      if compilation_result = compile_error_in_source then
         begin
            SourceMemo.SetFocus;
            SourceMemo.SelStart := SourceMemo.Perform (EM_LINEINDEX, compilation.compiler_error_source_location.line_no-1, 0) + compilation.compiler_error_source_location.line_idx-1;
            SourceMemo.SelLength := 0;
            SourceMemo.Perform (EM_SCROLLCARET, 0, 0)
         end;
{$ENDIF}
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
      compilation := TCompilation.CreateFromStrings (MainForm.SourceMemo.Lines, ProgramGenerator, CompileResultsMemo.Lines);
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
   begin
      PageControl1.ActivePageIndex := 0;
      ide_source_file_name := temp_dir_for_tests + 'unnamed.cp';
   end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
   begin
      if OpenDialog.Execute then
         begin
            ide_source_file_name := OpenDialog.FileName;
            SourceMemo.Lines.LoadFromFile (ide_source_file_name);
            ide_source_file_changed := false;
            SaveMenuItem.Enabled := true
         end
   end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
   begin
      SourceMemo.Lines.SaveToFile (ide_source_file_name);
      ide_source_file_changed := false
   end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
   begin
      if SaveDialog.Execute then
         begin
            ide_source_file_name := SaveDialog.FileName;
            SourceMemo.Lines.SaveToFile (ide_source_file_name);
            ide_source_file_changed := false;
            SaveMenuItem.Enabled := true
         end
   end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
   begin
      Close
   end;

procedure TMainForm.set_caption;
   begin
      Caption := 'PIC18x Concurrent Pascal Compiler Dev - ' + ExtractFileName(ide_source_file_name);
      if ide_source_file_changed then
         Caption := Caption + '*'
   end;

procedure TMainForm.set_ide_source_file_name (s: string);
   begin
      f_ide_source_file_name := s;
      if ExtractFileExt (f_ide_source_file_name) = '' then
         f_ide_source_file_name := f_ide_source_file_name + '.cp';
      set_caption
   end;

procedure TMainForm.set_ide_source_file_changed (b: boolean);
   begin
      f_ide_source_file_changed := b;
      set_caption
   end;

procedure TMainForm.SourceMemoChange(Sender: TObject);
   begin
      ide_source_file_changed := true
   end;


END.
