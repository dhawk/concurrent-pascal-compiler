unit github_polling_unit;

interface

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
   Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ci_step_base_class_unit, Vcl.StdCtrls,
   Vcl.ExtCtrls;

type
   TGithubPoller =
      class(TCIStepFrameBaseClass)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
      private
         { Private declarations }
      protected
         function execute_step: boolean;
            override;
      public
         procedure record_push_time;
         procedure record_poll_time;
      end;

var
   GithubPoller: TGithubPoller;

implementation

{$R *.dfm}

function TGithubPoller.execute_step: boolean;
   begin
   end;

procedure TGithubPoller.record_push_time;
   begin
      label2.Caption := 'Latest push to ci-test: ' + FormatDateTime ('mm/dd/yy hh:mm', now)
   end;

procedure TGithubPoller.record_poll_time;
   begin
      label3.Caption := 'Last poll: ' + FormatDateTime ('mm/dd/yy hh:mm', now)
   end;

end.
