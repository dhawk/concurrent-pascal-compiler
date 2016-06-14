UNIT about_box_unit;

INTERFACE

uses
   Windows, SysUtils, Classes, Graphics,
{$IFDEF FPC}
{$ELSE}
   Vcl.Imaging.GIFImg,
{$ENDIF}
   Forms, Controls, StdCtrls, Buttons, ExtCtrls, about_credit_frame_unit;

type
  TAboutBox = class(TForm)
    OKButton: TButton;
    ProductName: TLabel;
    WebsiteLabel: TLabel;
    AboutCreditFrame1: TAboutCreditFrame;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

IMPLEMENTATION

{$IFDEF FPC}
uses
   LazarusVersionSupport;
{$ENDIF}

{$R *.dfm}

procedure TAboutBox.FormCreate(Sender: TObject);
   begin
      AboutCreditFrame1.Init
   end;

END.

