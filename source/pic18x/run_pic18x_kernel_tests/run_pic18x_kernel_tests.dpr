program run_pic18x_kernel_tests;

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
  Classes,
--------cut here--------
*)

uses
{$IFDEF FPC}
  Interfaces,
{$ELSE}
  FastMM4,
{$ENDIF}
  Classes,
  Forms,
  SysUtils,
  aboutbox_unit in '..\..\common\aboutbox_unit.pas' {AboutBoxForm},
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
  pic18x_aboutbox_unit in '..\common\pic18x_aboutbox_unit.pas' {PIC18xAboutBoxForm},
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
  pic18x_ram_map_unit in '..\pic18x_ram_map_unit.pas',
  pic18x_run_time_error_check_unit in '..\pic18x_run_time_error_check_unit.pas',
  pic18x_simple_expression_unit in '..\pic18x_simple_expression_unit.pas',
  pic18x_statements_unit in '..\pic18x_statements_unit.pas',
  pic18x_string_unit in '..\pic18x_string_unit.pas',
  pic18x_term_expression_unit in '..\pic18x_term_expression_unit.pas',
  pic18x_types_unit in '..\pic18x_types_unit.pas',
  run_pic18x_kernel_tests_aboutbox_unit in 'run_pic18x_kernel_tests_aboutbox_unit.pas' {RunPIC18xKernelTestsAboutBoxForm},
  test_pic18x_compiler_unit in '..\test_pic18x_compiler_unit.pas',
  test_pic18x_kernel_unit in '..\test_pic18x_kernel_unit.pas',
  test_pic18x_simulator_unit in '..\test_pic18x_simulator_unit.pas',
  test_pic18x_subroutines_unit in '..\test_pic18x_subroutines_unit.pas';

var
   i: integer;
   html_file: TextFile;
   untested: boolean;

{$R *.res}

begin
   if ParamCount = 0 then
      begin
         number_of_tests := 0;
         number_of_errors := 0;
         GenerateKernelTestCoverageMap := true;
         try
            run_kernel_tests;
            if ParamStr(1) <> '' then
               begin
                  AssignFile (html_file, ParamStr(1));
                  Rewrite (html_file);
                  writeln (html_file, '<body bgcolor="#FFFFFF">');
                  writeln (html_file, '<font face="courier new">');
                  writeln (html_file, '<H1>PIC18x Kernel Test Code Coverage</H1>');
                  writeln (html_file, 'Black Text - Instruction was executed during tests<br>');
                  writeln (html_file, '<font color=red>Red Text - Instruction was not executed during tests</font><br><br>');
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
                           writeln (html_file, '<font color=red>');
                        writeln (html_file, StringReplace (KernelInstructions[i].instrxxx, ' ', '&nbsp;', [rfReplaceAll]) + '<br>');
                        if untested
                        then
                           writeln (html_file, '</font>')
                     end;
                  writeln (html_file, '</font>');
                  writeln (html_file, '</body>');
                  CloseFile (html_file)
               end;
         except
            on E:Exception do
               begin
                  writeln('uncaught exception: ' + E.Classname, ': ', E.Message);
                  number_of_errors := number_of_errors + 1
               end
         end;
         SetLength (KernelInstructions, 0);
         ExitCode := number_of_errors
      end
   else if LowerCase(ParamStr(1)) = '-about' then
      begin
         Application.Initialize;
         Application.MainFormOnTaskbar := True;
         Application.CreateForm(TRunPIC18xKernelTestsAboutBoxForm, RunPIC18xKernelTestsAboutBoxForm);
         Application.Run
      end
   else
      begin
         writeln;
         writeln ('usage:');
         writeln ('   ' + ExtractFileName(ParamStr(0)) + ' [-about]');
         writeln ('      displays aboutbox if -about is specified,');
         writeln ('      otherwise runs pic18x kernel tests.');
         writeln
      end
end.
