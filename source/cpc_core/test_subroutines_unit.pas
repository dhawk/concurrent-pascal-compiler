UNIT test_subroutines_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   cpc_blocks_unit,
   cpc_definitions_unit;

type
   TTestGenerator = function: TDefinition;

var
   tests_failed: integer;
   total_tests_run: integer;
   
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
procedure display (s: string);
function ProgramGenerator: TDefinition;

IMPLEMENTATION

uses
{$IFNDEF CONSOLE_TEST_MODE}
   main_form_unit,
{$ENDIF}
   classes,
   cpc_common_unit,
   cpc_core_objects_unit,
   cpc_main_compiler_unit,
   cpc_source_analysis_unit,
   cpc_statements_unit,
   cpc_target_cpu_unit;

procedure display (s: string);
   begin
{$IFDEF CONSOLE_TEST_MODE}
      writeln (s)
{$ELSE}
      MainForm.Memo.Lines.Add (s)
{$ENDIF}
   end;

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
                     gr := target_cpu.TRoutine_CreateFromSourceTokens (nil, false);
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
               display (st[i]);
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
               display (st[i]);
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
               display (st[i])
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
               display (st[i])
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

INITIALIZATION
   tests_failed := 0;
   total_tests_run := 0;

END.
