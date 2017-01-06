program pic18x_config_bit_editor;

uses
{$IFDEF FPC}
   Interfaces,
{$ELSE}
   FastMM4,
{$ENDIF}
   Forms,
   main_form_unit in 'main_form_unit.pas' {MainForm},
   dummy_pic18x_cpu_unit in 'dummy_pic18x_cpu_unit.pas',
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
   about_credit_frame_unit in '..\..\lib\about_credit_frame_unit.pas' {AboutCreditFrame: TFrame},
   pic18x_selection_dialog_unit in '..\common\pic18x_selection_dialog_unit.pas' {Pic18xSelectionDialog},
   about_box_unit in 'about_box_unit.pas' {AboutBox};

{$R *.res}

begin
   Application.Initialize;
{$IFDEF MSWINDOWS}
   Application.MainFormOnTaskbar := True;
{$ENDIF}
   Application.Title := 'PIC18x Configuration Bits Editor';
   Application.CreateForm(TMainForm, MainForm);
   Application.CreateForm(TPic18xSelectionDialog, Pic18xSelectionDialog);
   Application.CreateForm(TAboutBox, AboutBox);
   Application.Run;
end.
