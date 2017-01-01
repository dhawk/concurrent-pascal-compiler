program test_cpc_core;

uses
{$IFDEF FPC}
   Interfaces,
{$ELSE}
   Fastmm4,
{$ENDIF}
   Forms,
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
   test_main_form_unit in 'test_main_form_unit.pas' {MainForm};

{$R *.res}

begin
   Application.Initialize;
{$IFDEF MSWINDOWS}
   Application.MainFormOnTaskbar := True;
{$ENDIF}
   Application.CreateForm(TMainForm, MainForm);
   Application.Run;
end.
