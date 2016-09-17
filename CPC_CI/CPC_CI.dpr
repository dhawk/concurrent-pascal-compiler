program CPC_CI;

uses
  FastMM4,
  Vcl.Forms,
  main_form_unit in 'main_form_unit.pas' {MainForm},
  ci_step_base_class_unit in 'ci_step_base_class_unit.pas' {CIStepFrameBaseClass: TFrame},
  github_polling_unit in 'github_polling_unit.pas' {GithubPoller: TFrame},
  execute_console_command_unit in 'execute_console_command_unit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
