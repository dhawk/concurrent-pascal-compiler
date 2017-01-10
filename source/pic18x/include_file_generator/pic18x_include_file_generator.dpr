program pic18x_include_file_generator;

uses
  FastMM4,
  Forms,
  main_form_unit in 'main_form_unit.pas' {MainForm},
  pic18x_typedef_unit in 'pic18x_typedef_unit.pas',
  pic_file_parser_unit in 'pic_file_parser_unit.pas',
  processing_frame_unit in 'processing_frame_unit.pas' {ProcessingFrame: TFrame},
  pic18x_information_unit in 'pic18x_information_unit.pas',
  pic18x_interrupt_variable_unit in 'pic18x_interrupt_variable_unit.pas',
  process_pic_file_unit in 'process_pic_file_unit.pas',
  pic18x_config_bits_unit in 'pic18x_config_bits_unit.pas',
  pic18x_ioreg_unit in 'pic18x_ioreg_unit.pas',
  win32_utils in '..\..\lib\win32_utils.pas',
  multi_threaded_processing_dialog_base_class_unit in 'multi_threaded_processing_dialog_base_class_unit.pas' {MultiThreadedProcessingDialogBaseClass},
  pic18x_16bit_timer_unit in 'pic18x_16bit_timer_unit.pas',
  pic18x_dataspace_unit in 'pic18x_dataspace_unit.pas',
  pic18x_selection_dialog_unit in '..\common\pic18x_selection_dialog_unit.pas' {Pic18xSelectionDialog},
  file_viewer_unit in 'file_viewer_unit.pas' {FileViewerForm},
  common_unit in 'common_unit.pas',
  combo_type_dialog_unit in 'combo_type_dialog_unit.pas' {ComboTypeDialog},
  combo_type_sfr_pattern_frame_unit in 'combo_type_sfr_pattern_frame_unit.pas' {ComboTypeSFRPatternFrame: TFrame},
  all_pic18x_sfr_field_info_unit in 'all_pic18x_sfr_field_info_unit.pas',
  combo_type_unit in 'combo_type_unit.pas',
  select_combo_type_dlg_unit in 'select_combo_type_dlg_unit.pas' {SelectComboTypeDlg},
  fieldname_fixup_frame_unit in 'fieldname_fixup_frame_unit.pas' {FieldnameFixupFrame: TFrame},
  choose_c_variable_listing_unit in 'choose_c_variable_listing_unit.pas' {ChooseCOrAssemblyTypeListingForm},
  process_all_pic_files_dlg_unit in 'process_all_pic_files_dlg_unit.pas' {ProcessAllPICFilesDlg},
  syntax_check_all_include_files_dialog_unit in 'syntax_check_all_include_files_dialog_unit.pas' {SyntaxCheckAllIncludeFilesDialog},
  add_field_frame_unit in 'add_field_frame_unit.pas' {AddFieldFrame: TFrame},
  LibXmlComps in '..\..\lib\LibXmlComps.pas',
  LibXmlParser in '..\..\lib\LibXmlParser.pas',
  view_c_declarations_unit in 'view_c_declarations_unit.pas' {ViewCDeclarationsForm},
  cpc_access_unit in '..\..\cpc_core\cpc_access_unit.pas',
  cpc_blocks_unit in '..\..\cpc_core\cpc_blocks_unit.pas',
  cpc_common_unit in '..\..\cpc_core\cpc_common_unit.pas',
  cpc_constant_expression_unit in '..\..\cpc_core\cpc_constant_expression_unit.pas',
  cpc_core_objects_unit in '..\..\cpc_core\cpc_core_objects_unit.pas',
  cpc_definitions_unit in '..\..\cpc_core\cpc_definitions_unit.pas',
  cpc_expressions_unit in '..\..\cpc_core\cpc_expressions_unit.pas',
  cpc_multi_precision_integer_unit in '..\..\cpc_core\cpc_multi_precision_integer_unit.pas',
  cpc_simple_expression_unit in '..\..\cpc_core\cpc_simple_expression_unit.pas',
  cpc_source_analysis_unit in '..\..\cpc_core\cpc_source_analysis_unit.pas',
  cpc_statements_unit in '..\..\cpc_core\cpc_statements_unit.pas',
  cpc_target_cpu_unit in '..\..\cpc_core\cpc_target_cpu_unit.pas',
  cpc_term_expression_unit in '..\..\cpc_core\cpc_term_expression_unit.pas',
  cpc_types_unit in '..\..\cpc_core\cpc_types_unit.pas',
  aboutbox_unit in '..\..\common\aboutbox_unit.pas' {AboutBoxForm},
  pic18x_include_file_generator_aboutbox_unit in 'pic18x_include_file_generator_aboutbox_unit.pas' {PIC18xIncludeFileGeneratorAboutBoxForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPic18xSelectionDialog, Pic18xSelectionDialog);
  Application.CreateForm(TComboTypeDialog, ComboTypeDialog);
  Application.CreateForm(TSelectComboTypeDlg, SelectComboTypeDlg);
  Application.CreateForm(TChooseCOrAssemblyTypeListingForm, ChooseCOrAssemblyTypeListingForm);
  Application.CreateForm(TProcessAllPICFilesDlg, ProcessAllPICFilesDlg);
  Application.CreateForm(TSyntaxCheckAllIncludeFilesDialog, SyntaxCheckAllIncludeFilesDialog);
  Application.CreateForm(TPIC18xIncludeFileGeneratorAboutBoxForm, PIC18xIncludeFileGeneratorAboutBoxForm);
  Application.Run;
end.
