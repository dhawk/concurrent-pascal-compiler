unit main_form_unit;

interface

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
   Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ci_step_base_class_unit,
   github_polling_unit, Vcl.StdCtrls;

type
   TMainForm =
      class(TForm)
         GithubPoller: TGithubPoller;
         StartButton: TButton;
         StopButton: TButton;
         Label1: TLabel;
         Memo1: TMemo;
    GithubPuller: TCIStepFrameBaseClass;
    GithubPusher: TCIStepFrameBaseClass;
         procedure DoCIProcessLoop(Sender: TObject);
         procedure StopButtonClick(Sender: TObject);
      private
         function poll_for_github_push_message: boolean;
         procedure pull_ci_test_from_github;
         procedure push_to_github_master;
      public
         { Public declarations }
      end;

var
   MainForm: TMainForm;
   Stop: boolean;

implementation

uses
   execute_console_command_unit, superobject;

{$R *.dfm}

procedure wait (wait_time: integer);   // wait_time in mSec
   var
      start_time: TDateTime;
   begin
      start_time := Now;
      while (Now - start_time){days} * 24{hrs/day} * 60{min/hr} * 60{sec/min} * 1000{mSec/sec} < wait_time do
         begin
            sleep (10);
            Application.ProcessMessages
         end
   end;

procedure TMainForm.DoCIProcessLoop(Sender: TObject);
   var
      sl: TStringList;
      new_push_available_on_github: boolean;
      new_mplabx_available: boolean;
      tests_successful: boolean;
   begin
      StartButton.Enabled := false;
      StopButton.Enabled := true;
      Stop := false;
      repeat
         new_push_available_on_github := false;
         new_mplabx_available := false;
         repeat
//            new_push_available_on_github := poll_for_github_push_message;
new_push_available_on_github := true;
stop := true;
            if not new_push_available_on_github then
               begin
                  wait (5000)
               end
         until new_push_available_on_github or new_mplabx_available or Stop;
         if new_push_available_on_github then
            begin
               pull_ci_test_from_github;

               tests_successful := true;

               if tests_successful then
                  push_to_github_master
            end
      until Stop;
      Stop := false;
      StartButton.Enabled := true
   end;

procedure TMainForm.StopButtonClick(Sender: TObject);
   begin
      Stop := true;
      StopButton.Enabled := false
   end;

function TMainForm.poll_for_github_push_message: boolean;
   var
      sl: TStringList;
      github_event_info: ISuperObject;
   begin
      result := false;
      GithubPoller.step_status := step_running;
      sl := nil;
      try
         sl := ExecuteConsoleCommand ('powershell -command .\poll-sqs.ps1');
         if sl.Count > 0 then
            begin
               github_event_info := SO(sl.Text);
               result := github_event_info.s['ref'] = 'refs/heads/ci-test'
            end
      finally
         sl.Free;
      end;
      if result then
         begin
            GithubPoller.Label4.Caption := github_event_info.o['commits'].AsArray[0]['message'].AsString;
            GithubPoller.step_status := step_succeeded;
            GithubPoller.record_push_time
         end
      else
         GithubPoller.step_status := step_idle;
      GithubPoller.record_poll_time
   end;

procedure TMainForm.pull_ci_test_from_github;
   var
      sl: TStringList;
   begin
      GithubPuller.step_status := step_running;
      sl := ExecuteConsoleCommand ('.\pull-from-github-ci-test.bat');
//      Memo1.Lines.AddStrings(sl);
      sl.Free;
      GithubPuller.step_status := step_succeeded
   end;

procedure TMainForm.push_to_github_master;
   var
      sl: TStringList;
   begin
      GithubPusher.step_status := step_running;
      sl := ExecuteConsoleCommand ('.\push-to-github-master.bat');
//      Memo1.Lines.AddStrings(sl);
      sl.Free;
      GithubPusher.step_status := step_succeeded
   end;

end.


