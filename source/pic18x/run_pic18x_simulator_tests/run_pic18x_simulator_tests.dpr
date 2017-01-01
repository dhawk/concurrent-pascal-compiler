program run_pic18x_simulator_tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
{$IFNDEF FPC}
   FastMM4,
{$ENDIF}
   SysUtils,
   cpc_access_unit in '..\..\cpc_core\cpc_access_unit.pas',
   cpc_blocks_unit in '..\..\cpc_core\cpc_blocks_unit.pas',
   cpc_common_unit in '..\..\cpc_core\cpc_common_unit.pas',
   cpc_constant_expression_unit in '..\..\cpc_core\cpc_constant_expression_unit.pas',
   cpc_core_objects_unit in '..\..\cpc_core\cpc_core_objects_unit.pas',
   cpc_definitions_unit in '..\..\cpc_core\cpc_definitions_unit.pas',
   cpc_expressions_unit in '..\..\cpc_core\cpc_expressions_unit.pas',
   cpc_main_compiler_unit in '..\..\cpc_core\cpc_main_compiler_unit.pas',
   cpc_multi_precision_integer_unit in '..\..\cpc_core\cpc_multi_precision_integer_unit.pas',
   cpc_simple_expression_unit in '..\..\cpc_core\cpc_simple_expression_unit.pas',
   cpc_source_analysis_unit in '..\..\cpc_core\cpc_source_analysis_unit.pas',
   cpc_statements_unit in '..\..\cpc_core\cpc_statements_unit.pas',
   cpc_target_cpu_unit in '..\..\cpc_core\cpc_target_cpu_unit.pas',
   cpc_term_expression_unit in '..\..\cpc_core\cpc_term_expression_unit.pas',
   cpc_types_unit in '..\..\cpc_core\cpc_types_unit.pas',
   pic18x_access_unit in '..\pic18x_access_unit.pas',
   pic18x_assignment_statement_unit in '..\pic18x_assignment_statement_unit.pas',
   pic18x_blocks_unit in '..\pic18x_blocks_unit.pas',
   pic18x_core_objects_unit in '..\pic18x_core_objects_unit.pas',
   pic18x_cpu_unit in '..\pic18x_cpu_unit.pas',
   pic18x_expressions_unit in '..\pic18x_expressions_unit.pas',
   pic18x_floating_point_unit in '..\pic18x_floating_point_unit.pas',
   pic18x_instruction_simulation_test_unit in '..\pic18x_instruction_simulation_test_unit.pas',
   pic18x_instructions_unit in '..\pic18x_instructions_unit.pas',
   pic18x_kernel_unit in '..\pic18x_kernel_unit.pas',
   pic18x_macro_instructions_unit in '..\pic18x_macro_instructions_unit.pas',
   pic18x_main_compiler_unit in '..\pic18x_main_compiler_unit.pas',
   pic18x_microprocessor_information_unit in '..\pic18x_microprocessor_information_unit.pas',
   pic18x_multiply_divide_unit in '..\pic18x_multiply_divide_unit.pas',
   pic18x_run_time_error_check_unit in '..\pic18x_run_time_error_check_unit.pas',
   pic18x_simple_expression_unit in '..\pic18x_simple_expression_unit.pas',
   pic18x_statements_unit in '..\pic18x_statements_unit.pas',
   pic18x_string_unit in '..\pic18x_string_unit.pas',
   pic18x_term_expression_unit in '..\pic18x_term_expression_unit.pas',
   pic18x_types_unit in '..\pic18x_types_unit.pas',
   test_pic18x_compiler_unit in '..\test_pic18x_compiler_unit.pas',
   test_pic18x_subroutines_unit in '..\test_pic18x_subroutines_unit.pas',
   test_pic18x_kernel_unit in '..\test_pic18x_kernel_unit.pas',
   test_pic18x_simulator_unit in '..\test_pic18x_simulator_unit.pas',
   pic18x_ram_map_unit in '..\pic18x_ram_map_unit.pas';

{$R *.res}

begin
   try
      number_of_tests := 0;
      number_of_errors := 0;
      run_instruction_simulation_tests;
   except
      on E:Exception do
         begin
            writeln('uncaught exception: ' + E.Classname, ': ', E.Message);
            number_of_errors := number_of_errors + 1
         end
   end;

   ExitCode := number_of_errors
end.
