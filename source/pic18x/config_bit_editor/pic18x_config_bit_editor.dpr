program pic18x_config_bit_editor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
   pic18x_selection_dialog_unit in '..\common\pic18x_selection_dialog_unit.pas' {Pic18xSelectionDialog},
   aboutbox_unit in '..\..\common\aboutbox_unit.pas' {AboutBoxForm},
   config_bit_editor_aboutbox_unit in 'config_bit_editor_aboutbox_unit.pas' {ConfigBitEditorAboutBoxForm},
   dummy_pic18x_cpu_unit in 'dummy_pic18x_cpu_unit.pas',
   main_form_unit in 'main_form_unit.pas' {MainForm},
   pic18x_aboutbox_unit in '..\common\pic18x_aboutbox_unit.pas' {PIC18xAboutBoxForm};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF MSWINDOWS}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.Title := 'PIC18x Configuration Bits Editor';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPic18xSelectionDialog, Pic18xSelectionDialog);
  Application.CreateForm(TConfigBitEditorAboutBoxForm, ConfigBitEditorAboutBoxForm);
  Application.Run;
end.
