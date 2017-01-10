PROGRAM run_cpc_core_tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

(* The Delphi IDE frequently rewrites the uses section and loses the {$IFDEF}s needed for Lazarus.
   If that happens simply replace the first few entries with the following:
--------cut here--------
uses
{$IFDEF FPC}
  Interfaces,
{$ELSE}
  FastMM4,
{$ENDIF}
  Forms,
--------cut here--------
*)

uses
{$IFDEF FPC}
  Interfaces,
{$ELSE}
  FastMM4,
{$ENDIF}
  Forms,
  SysUtils,
  Classes,
  cpc_access_unit in '..\cpc_access_unit.pas',
  cpc_blocks_unit in '..\cpc_blocks_unit.pas',
  cpc_common_unit in '..\cpc_common_unit.pas',
  cpc_constant_expression_unit in '..\cpc_constant_expression_unit.pas',
  cpc_core_objects_unit in '..\cpc_core_objects_unit.pas',
  cpc_definitions_unit in '..\cpc_definitions_unit.pas',
  cpc_expressions_unit in '..\cpc_expressions_unit.pas',
  cpc_main_compiler_unit in '..\cpc_main_compiler_unit.pas',
  cpc_multi_precision_integer_unit in '..\cpc_multi_precision_integer_unit.pas',
  cpc_simple_expression_unit in '..\cpc_simple_expression_unit.pas',
  cpc_source_analysis_unit in '..\cpc_source_analysis_unit.pas',
  cpc_statements_unit in '..\cpc_statements_unit.pas',
  cpc_target_cpu_unit in '..\cpc_target_cpu_unit.pas',
  cpc_term_expression_unit in '..\cpc_term_expression_unit.pas',
  cpc_types_unit in '..\cpc_types_unit.pas',
  test_access_syntax_unit in '..\test_access_syntax_unit.pas',
  test_block_syntax_unit in '..\test_block_syntax_unit.pas',
  test_constant_expression_unit in '..\test_constant_expression_unit.pas',
  test_cpu_unit in '..\test_cpu_unit.pas',
  test_expression_syntax_unit in '..\test_expression_syntax_unit.pas',
  test_multi_precision_integer_unit in '..\test_multi_precision_integer_unit.pas',
  test_statement_syntax_unit in '..\test_statement_syntax_unit.pas',
  test_type_syntax_unit in '..\test_type_syntax_unit.pas',
  test_subroutines_unit in '..\test_subroutines_unit.pas',
  aboutbox_unit in '..\..\common\aboutbox_unit.pas' {AboutBoxForm},
  run_cpc_core_tests_aboutbox_unit in 'run_cpc_core_tests_aboutbox_unit.pas' {RunCPCCoreTestsAboutBoxForm};

{$R *.res}

BEGIN
   if ParamCount = 0 then
      begin
         tests_failed := 0;
         try
            test_lex_analysis;
            test_constant_syntax_unit;
            test_type_syntax;
            test_data_syntax_unit;
            test_mp_math;
            test_statements;
            test_block_syntax;
            test_TPrimary;
            test_TFactor;
            test_TTerm;
            test_TSimpleExpression;
            test_TRelationalExpression;
            test_TAccess;
         except
            on E:Exception do
               begin
                  Writeln('uncaught exception ' + E.Classname, ': ', E.Message);
                  tests_failed := tests_failed + 1
               end
         end;
         ExitCode := tests_failed
      end
   else if LowerCase(ParamStr(1)) = '-about' then
      begin
         Application.Initialize;
         Application.MainFormOnTaskbar := True;
         Application.CreateForm(TRunCPCCoreTestsAboutBoxForm, RunCPCCoreTestsAboutBoxForm);
         Application.Run
      end
   else
      begin
         writeln;
         writeln ('usage:');
         writeln ('   ' + ExtractFileName(ParamStr(0)) + ' [-about]');
         writeln ('      displays aboutbox if -about is specified,');
         writeln ('      otherwise runs cpc core tests.');
         writeln
      end
END.
