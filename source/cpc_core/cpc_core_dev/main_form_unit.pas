UNIT main_form_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   Classes,
   Controls,
   cpc_blocks_unit,
   cpc_definitions_unit,
   Dialogs,
   Forms,
   Graphics,
   Menus,
   StdCtrls,
   SysUtils,
   Variants;

type
  TMainForm = class(TForm)
    ClearButton: TButton;
    CompileButton: TButton;
    CompileResultsMemo: TMemo;
    Memo: TMemo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    SelectAllButton: TButton;
    RunSelectedTestsButton: TButton;
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    AboutTestCPCCoreCompiler1: TMenuItem;
    procedure ClearButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure RunSelectedTestsButtonClick(Sender: TObject);
    procedure AboutTestCPCCoreCompiler1Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

IMPLEMENTATION

{$R *.dfm}

uses
   cpc_common_unit,
   cpc_core_dev_aboutbox_unit,
   cpc_core_objects_unit,
   cpc_main_compiler_unit,
   cpc_source_analysis_unit,
   cpc_statements_unit,
   cpc_target_cpu_unit,
   test_access_syntax_unit,
   test_block_syntax_unit,
   test_constant_expression_unit,
   test_expression_syntax_unit,
   test_multi_precision_integer_unit,
   test_statement_syntax_unit,
   test_subroutines_unit,
   test_type_syntax_unit;

procedure TMainForm.AboutTestCPCCoreCompiler1Click(Sender: TObject);
   begin
      CPCCoreDevAboutBoxForm.ShowModal
   end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
   begin
      Memo.Clear
   end;

procedure TMainForm.CompileButtonClick(Sender: TObject);
   var compilation: TCompilation;
   begin
      CompileResultsMemo.Clear;
      compilation := TCompilation.CreateFromStrings (MainForm.Memo.Lines, ProgramGenerator, CompileResultsMemo.Lines);
      compilation.Free
   end;

procedure TMainForm.SelectAllButtonClick(Sender: TObject);
   begin
      CheckBox1.Checked := true;
      CheckBox2.Checked := true;
      CheckBox3.Checked := true;
      CheckBox4.Checked := true;
      CheckBox5.Checked := true;
      CheckBox6.Checked := true;
      CheckBox7.Checked := true;
      CheckBox8.Checked := true;
      CheckBox9.Checked := true;
      CheckBox10.Checked := true;
      CheckBox11.Checked := true;
      CheckBox12.Checked := true;
      CheckBox13.Checked := true;
   end;

procedure TMainForm.RunSelectedTestsButtonClick(Sender: TObject);
   begin
      Memo.Clear;
      CompileResultsMemo.Clear;
      Application.ProcessMessages;
      total_tests_run := 0;
      tests_failed := 0;
      if CheckBox1.Checked then
         test_lex_analysis;
      if CheckBox2.Checked then
         test_constant_syntax_unit;
      if CheckBox3.Checked then
         test_type_syntax;
      if CheckBox4.Checked then
         test_data_syntax_unit;
      if CheckBox5.Checked then
         test_mp_math;
      if CheckBox6.Checked then
         test_statements;
      if CheckBox7.Checked then
         test_block_syntax;
      if CheckBox8.Checked then
         test_TPrimary;
      if CheckBox10.Checked then
         test_TFactor;
      if CheckBox11.Checked then
         test_TTerm;
      if CheckBox12.Checked then
         test_TSimpleExpression;
      if CheckBox13.Checked then
         test_TRelationalExpression;
      if CheckBox9.Checked then
         test_TAccess;

      total_tests_run := total_tests_run + 1;
      if refcount_log_enabled then
         begin
            record_bad_test_result;
            display ('DEFINITION_OBJNO_TRACE still defined in cpc_common_unit.pas')
         end;

      Memo.Lines.Add (IntToStr(total_tests_run) + ' tests run, ' + IntToStr (tests_failed) + ' failed')
   end;

INITIALIZATION
{$ifndef FPC}
   ReportMemoryLeaksOnShutdown := true;
{$endif}

END.
