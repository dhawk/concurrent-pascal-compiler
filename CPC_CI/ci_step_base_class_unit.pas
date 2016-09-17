UNIT ci_step_base_class_unit;

INTERFACE

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
   Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
   TStepStatus = (step_idle, step_running, step_succeeded, step_failed);
   TCIStepFrameBaseClass =
      class(TFrame)
         Panel1: TPanel;
    Label1: TLabel;
      private
         f_step_status: TStepStatus;
         procedure set_step_status (st: TStepStatus);
      protected
         function execute_step: boolean;
            virtual; abstract;
      public
         property step_status: TStepStatus read f_step_status write set_step_status;
         function execute: boolean;
      end;

IMPLEMENTATION

{$R *.dfm}

procedure TCIStepFrameBaseClass.set_step_status (st: TStepStatus);
   begin
      f_step_status := st;
      case f_step_status of
         step_idle:
            Panel1.Color := clBtnFace;
         step_running:
            Panel1.Color := clWhite;
         step_succeeded:
            Panel1.Color := clGreen;
         step_failed:
            Panel1.Color := clRed;
      else
         assert (false)
      end
   end;

function TCIStepFrameBaseClass.execute: boolean;
   begin
      step_status := step_running;
      result := execute_step;
      if result then
         step_status := step_succeeded
      else
         step_status := step_failed
   end;

END.
