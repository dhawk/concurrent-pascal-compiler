program cpc_pic18x;

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
  btypes in '..\..\lib\mparith\btypes.pas',
  cpc_access_unit in '..\..\cpc_core\cpc_access_unit.pas',
  cpc_blocks_unit in '..\..\cpc_core\cpc_blocks_unit.pas',
  cpc_common_unit in '..\..\cpc_core\cpc_common_unit.pas',
  cpc_constant_expression_unit in '..\..\cpc_core\cpc_constant_expression_unit.pas',
  cpc_core_objects_unit in '..\..\cpc_core\cpc_core_objects_unit.pas',
  cpc_definitions_unit in '..\..\cpc_core\cpc_definitions_unit.pas',
  cpc_expressions_unit in '..\..\cpc_core\cpc_expressions_unit.pas',
  cpc_main_compiler_unit in '..\..\cpc_core\cpc_main_compiler_unit.pas',
  cpc_multi_precision_integer_unit in '..\..\cpc_core\cpc_multi_precision_integer_unit.pas',
  cpc_pic18x_aboutbox_unit in 'cpc_pic18x_aboutbox_unit.pas' {CPCPIC18xAboutBoxForm},
  cpc_simple_expression_unit in '..\..\cpc_core\cpc_simple_expression_unit.pas',
  cpc_source_analysis_unit in '..\..\cpc_core\cpc_source_analysis_unit.pas',
  cpc_statements_unit in '..\..\cpc_core\cpc_statements_unit.pas',
  cpc_target_cpu_unit in '..\..\cpc_core\cpc_target_cpu_unit.pas',
  cpc_term_expression_unit in '..\..\cpc_core\cpc_term_expression_unit.pas',
  cpc_types_unit in '..\..\cpc_core\cpc_types_unit.pas',
  dijkstra_smoothsort_unit in '..\..\lib\dijkstra_smoothsort_unit.pas',
  isaac in '..\..\lib\mparith\isaac.pas',
  mp_base in '..\..\lib\mparith\mp_base.pas',
  mp_prng in '..\..\lib\mparith\mp_prng.pas',
  mp_types in '..\..\lib\mparith\mp_types.pas',
  pic18x_aboutbox_unit in '..\common\pic18x_aboutbox_unit.pas' {PIC18xAboutBoxForm},
  pic18x_access_unit in '..\pic18x_access_unit.pas',
  pic18x_assignment_statement_unit in '..\pic18x_assignment_statement_unit.pas',
  pic18x_blocks_unit in '..\pic18x_blocks_unit.pas',
  pic18x_common_unit in '..\common\pic18x_common_unit.pas',
  pic18x_core_objects_unit in '..\pic18x_core_objects_unit.pas',
  pic18x_cpu_unit in '..\pic18x_cpu_unit.pas',
  pic18x_expressions_unit in '..\pic18x_expressions_unit.pas',
  pic18x_floating_point_unit in '..\pic18x_floating_point_unit.pas',
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
  wirth_balanced_binary_tree_unit in '..\..\lib\wirth_balanced_binary_tree_unit.pas';

{$R *.res}

var
   compilation: TCompilation;
   CompileResults: TStringList;
   i: integer;
begin
   if ParamCount = 0 then
      begin
         writeln;
         writeln ('usage:');
         writeln ('   ' + ExtractFileName(ParamStr(0)) + ' [-about] [filename]');
         writeln ('      displays aboutbox if -about is specified,');
         writeln ('      otherwise compiles a .cpas source file.');
         writeln
      end
   else if LowerCase(ParamStr(1)) = '-about' then
      begin
         Application.Initialize;
         Application.MainFormOnTaskbar := True;
         Application.CreateForm(TCPCPIC18xAboutBoxForm, CPCPIC18xAboutBoxForm);
  Application.Run
      end
   else
      begin
         source_file_name := ParamStr(1);
         if Pos('.', source_file_name) = 0 then
            source_file_name := source_file_name + '.cpas'
         else if LowerCase(ExtractFileExt(source_file_name)) <> '.cpas' then
            source_file_name := ChangeFileExt(source_file_name, '.cpas');

         try
            CompileResults := TStringList.Create;
            ClearRunTimeErrorLists;
            compilation := TCompilation.CreateFromFile (ParamStr(1), ProgramGenerator, CompileResults);
            ProgramCode.Clear;
            compilation.Free;
            for i := 0 to CompileResults.Count-1 do
               writeln (CompileResults[i]);
            CompileResults.Free
         except
            on E: Exception do
               Writeln(E.ClassName, ': ', E.Message)
         end
      end
end.

