unit processing_frame_unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
   TProcessingFrame = class(TFrame)
      Bevel1: TBevel;
      Memo: TMemo;
      Label1: TLabel;
      Label2: TLabel;
   private
      procedure set_title (s: string);
      procedure set_status (count: integer);
   public
      property Title: string write set_title;
      property Status: integer write set_status;
      procedure Clear;
      procedure Add (s: string);
   end;

implementation

{$R *.dfm}

procedure TProcessingFrame.set_title (s: string);
   begin
      label2.Caption := s
   end;

procedure TProcessingFrame.set_status (count: integer);
   begin
      label1.Caption := 'Files Processed: ' + IntToStr(count)
   end;

procedure TProcessingFrame.Clear;
   begin
      Memo.Clear;
      Status := 0
   end;

procedure TProcessingFrame.Add (s: string);
   begin
      Memo.Lines.Add (s)
   end;

end.
