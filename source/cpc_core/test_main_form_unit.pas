UNIT test_main_form_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cpc_definitions_unit, cpc_blocks_unit;

type
   TTestGenerator = function: TDefinition;

type
  TMainForm = class(TForm)
    ClearButton: TButton;
    CompileButton: TButton;
    CompileResultsMemo: TMemo;
    Memo: TMemo;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox9: TCheckBox;
    SelectAllButton: TButton;
    RunSelectedTestsButton: TButton;
    CheckBox1: TCheckBox;
    procedure ClearButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure RunSelectedTestsButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

procedure display (s: string);

function test_fragment (src_fragment: string; test_generator: TTestGenerator): TDefinition;

function setup_test_no_begin (src: string): TProgram;

procedure test_compile_error_generation_for_program_fragment
   (src_fragment: string;
    error_generator: TTestGenerator;
    expected_error_message, expected_error_message_location: string
   );

procedure test_only_for_successful_compilation (src: string);
   overload;

procedure test_only_for_successful_compilation
   (src: string;
    generator: TTestGenerator
   );
   overload;

procedure test_compile_error_generation
   (src: string;
    expected_error_message, expected_error_message_location: string
   );  // generator is TProgram

procedure record_bad_test_result;

IMPLEMENTATION

{$R *.dfm}

uses cpc_main_compiler_unit, cpc_source_analysis_unit, cpc_target_cpu_unit, test_constant_expression_unit,
   cpc_statements_unit, test_statement_syntax_unit, test_access_syntax_unit, test_multi_precision_integer_unit,
   cpc_core_objects_unit, cpc_common_unit, test_type_syntax_unit, test_expression_syntax_unit, test_block_synax_unit;

//=============

type
   TFragmentVarlist =
      class
         vars: TDataItemList;
         statement_list: TStatementList;
         constructor Create;
         destructor Destroy;
            override;
      end;

constructor TFragmentVarlist.Create;
   begin
      statement_list := TStatementList.Create;
      vars := target_cpu.TDataItemList_Create;
      vars.define_as_var_list (rw_var, absolute_address_mode)
   end;

destructor TFragmentVarlist.Destroy;
   begin
      vars.Release;
      statement_list.Release
   end;

function ProgramGenerator: TDefinition;
   begin
      result := TProgram.CreateFromSourceTokens
   end;

var
   _test_gen: TTestGenerator;
   _fragment_varlist: TFragmentVarlist;
   _global_routines: array of TRoutine;
   total_tests_run, tests_failed: integer;

function create_test_fragment_from_source_tokens: TDefinition;
   var
      gr: TRoutine;
      i: integer;
      in_preamble: boolean;
   begin
      result := nil;
      _fragment_varlist := TFragmentVarlist.Create;
      assert (Length(_global_routines) = 0);
      try
         in_preamble := lex.token.in_preamble;
         while lex.token_is_reserved_word([rw_const, rw_type, rw_var, rw_procedure, rw_function]) do
            begin
               if lex.token_is_reserved_word(rw_const) then
                  process_constant_definition_part;
               if lex.token_is_reserved_word(rw_type) then
                  process_type_definition_part;
               if lex.token_is_reserved_word(rw_var) then
                  _fragment_varlist.vars.AddFromSourceTokens (nil);
               if lex.token_is_reserved_word([rw_procedure, rw_function]) then
                  begin
                     gr := target_cpu.TRoutine_CreateFromSourceTokens (nil);
                     CurrentDefinitionTable.DefineForCurrentScope(gr.routine_id_idx, gr, gr.routine_id_src_loc);
                     i := Length(_global_routines);
                     SetLength (_global_routines, i+1);
                     _global_routines[i] := gr
                  end;
               if in_preamble and not lex.token.in_preamble then
                  begin
                     in_preamble := false;
                     CurrentDefinitionTable.EnterNewScope
                  end
            end;
         if not lex.token_is_reserved_word(rw_begin) then
            raise compile_error.Create(err_begin_expected);
         lex.advance_token;

         result := _test_gen;

         if not lex.token_is_reserved_word (rw_end) then
            raise compile_error.Create(err_end_expected);
      finally
         CurrentDefinitionTable.ExitScope;
         _fragment_varlist.Free;
         for i := 0 to Length(_global_routines)-1 do
            _global_routines[i].Release;
         SetLength(_global_routines, 0)
      end
   end;

procedure display (s: string);
   begin
      MainForm.Memo.Lines.Add (s)
   end;

function test_fragment (src_fragment: string; test_generator: TTestGenerator): TDefinition;
  var
     compilation: TCompilation;
     st: TStringList;
     i: integer;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      _test_gen := test_generator;
      compilation := TCompilation.CreateFromString (src_fragment, create_test_fragment_from_source_tokens, st);
      if compilation.compilation_result <> compiled_ok then
         begin
            record_bad_test_result;
            display ('unexpected compilation error in: ' + src_fragment);
            for i := 0 to st.Count-1 do
               MainForm.Memo.Lines.Add (st[i]);
            result := nil
         end
      else
         begin
            result := compilation.compiled_object;
            result.AddRef
         end;
      compilation.Free;
      st.Free
   end;

function setup_test_no_begin (src: string): TProgram;
  var
     compilation: TCompilation;
     st: TStringList;
     i: integer;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      compilation := TCompilation.CreateFromString (src, ProgramGenerator, st);
      if compilation.compilation_result <> compiled_ok then
         begin
            record_bad_test_result;
            display ('unexpected compilation error in: ' + src);
            for i := 0 to st.Count-1 do
               MainForm.Memo.Lines.Add (st[i]);
            result := nil
         end
      else
         begin
            result := TProgram(compilation.compiled_object);
            result.AddRef
         end;
      compilation.Free;
      st.Free
   end;

procedure test_compile_error_generation_for_program_fragment
   (src_fragment: string;
    error_generator: TTestGenerator;
    expected_error_message, expected_error_message_location: string
   );
  var
     compilation: TCompilation;
     st: TStringList;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      _test_gen := error_generator;
      compilation := TCompilation.CreateFromString (src_fragment, create_test_fragment_from_source_tokens, st);
      if compilation.compilation_result <> compile_error_in_source then
         begin
            record_bad_test_result;
            display (src_fragment + ': failed to generate compile error: ' + expected_error_message)
         end
      else
         begin
            if compilation.compiler_error_message <> expected_error_message then
               begin
                  record_bad_test_result;
                  display (src_fragment + ': FAILED, expected error message was: ' + expected_error_message);
                  display (compilation.compiler_error_source_location.caret + ' actual message was: ' + compilation.compiler_error_message)
               end
            else if Pos (expected_error_message_location, src_fragment) <> compilation.compiler_error_source_location.line_idx then
               begin
                  record_bad_test_result;
                  display (src_fragment + ': wrong source location');
                  display (caret (Pos (expected_error_message_location, src_fragment)) + ' desired error location');
                  display (compilation.compiler_error_source_location.caret + ' reported error location')
               end
         end;
      compilation.Free;
      st.Free
   end;

procedure test_only_for_successful_compilation (src: string);
  var
     compilation: TCompilation;
     st: TStringList;
     i: integer;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      compilation := TCompilation.CreateFromString (src, ProgramGenerator, st);
      if compilation.compilation_result <> compiled_ok then
         begin
            record_bad_test_result;
            display ('unexpected compilation error in: ' + src);
            for i := 0 to st.Count-1 do
               MainForm.Memo.Lines.Add (st[i])
         end;
      compilation.Free;
      st.Free
   end;

procedure test_only_for_successful_compilation
   (src: string;
    generator: TTestGenerator
   );
  var
     compilation: TCompilation;
     st: TStringList;
     i: integer;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      _test_gen := generator;
      compilation := TCompilation.CreateFromString (src, create_test_fragment_from_source_tokens, st);
      if compilation.compilation_result <> compiled_ok then
         begin
            record_bad_test_result;
            display ('unexpected compilation error in: ' + src);
            for i := 0 to st.Count-1 do
               MainForm.Memo.Lines.Add (st[i])
         end;
      compilation.Free;
      st.Free
   end;

procedure test_compile_error_generation
   (src: string;
    expected_error_message, expected_error_message_location: string
   );  // generator is TProgram
  var
     compilation: TCompilation;
     st: TStringList;
   begin
      st := TStringList.Create;
      total_tests_run := total_tests_run + 1;

      compilation := TCompilation.CreateFromString (src, ProgramGenerator, st);
      if compilation.compilation_result <> compile_error_in_source then
         begin
            record_bad_test_result;
            display (src + ': failed to generate compile error: ' + expected_error_message)
         end
      else
         begin
            if compilation.compiler_error_message <> expected_error_message then
               begin
                  record_bad_test_result;
                  display (src + ': FAILED, expected error message was: ' + expected_error_message);
                  display (caret (compilation.compiler_error_source_location.line_idx) + ' actual message was: ' + compilation.compiler_error_message)
               end
            else if Pos (expected_error_message_location, src) <> compilation.compiler_error_source_location.line_idx then
               begin
                  record_bad_test_result;
                  display (src + ': wrong source location');
                  display (caret (Pos (expected_error_message_location, src)) + ' desired error location');
                  display (compilation.compiler_error_source_location.caret + ' reported error location')
               end
         end;
      compilation.Free;
      st.Free
   end;

procedure record_bad_test_result;
   begin
      display ('****************************');
      tests_failed := tests_failed + 1
   end;


//==============

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
         test_block_syntax_unit;
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
