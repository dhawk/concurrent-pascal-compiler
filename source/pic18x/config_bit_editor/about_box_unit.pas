UNIT about_box_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, Graphics,
{$IFDEF FPC}
{$ELSE}
   GIFImg,
{$ENDIF}
   Forms, Controls, StdCtrls, Buttons, ExtCtrls, about_credit_frame_unit;

type
   TAboutBox = class(TForm)
      OKButton: TButton;
      ProductName: TLabel;
      WebsiteLabel: TLabel;
      AboutCreditFrame: TAboutCreditFrame;
      procedure FormCreate(Sender: TObject);
   private
      { Private declarations }
   public
      { Public declarations }
   end;

var
   AboutBox: TAboutBox;

IMPLEMENTATION

{$R *.dfm}

procedure TAboutBox.FormCreate(Sender: TObject);
   begin
      AboutCreditFrame.Init
   end;

END.

