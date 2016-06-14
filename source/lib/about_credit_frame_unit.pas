unit about_credit_frame_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
   TAboutCreditFrame =
      class(TFrame)
         Panel1: TPanel;
         BuildUsingGroupBox: TGroupBox;
         BuiltUsingImage: TImage;
         BuilderLabel1: TLabel;
         BuilderLabel2: TLabel;
         BuilderLabel3: TLabel;
         Label1: TLabel;
         Memo1: TMemo;
      private
         { Private declarations }
      public
         procedure Init;
      end;

implementation

{$R *.dfm}

{$IFDEF FPC}
uses
   LazarusVersionSupport;
{$ENDIF}

procedure TAboutCreditFrame.Init;
{$IFDEF FPC}
   procedure adj_top (c: TControl);
      begin
         c.Top := c.Top - 12
      end;
{$ENDIF}
   begin
{$IFDEF FPC}
      BuildUsingGroupBox.Caption := 'Built with Lazrus/Free Pascal';
      adj_top (BuiltUsingImage);
      BuiltUsingImage.Picture.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'images' + PathDelim + 'lazarus48x48.png');
      adj_top (BuilderLabel1);
      BuilderLabel1.caption := GetLCLVersion;
      adj_top (BuilderLabel2);
      BuilderLabel2.Caption := 'Compiled with ' + GetCompilerInfo;
      adj_top (BuilderLabel3);
      BuilderLabel3.caption := GetWidgetSet
{$ELSE}
      BuildUsingGroupBox.Caption := 'Built with Delphi';
      BuiltUsingImage.Picture.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'images' + PathDelim + 'delphi52x52.gif');
      BuilderLabel1.Caption := '';
      BuilderLabel2.Caption := format ('Compiled with Delphi %1.1f', [CompilerVersion]);
      BuilderLabel3.Caption := ''
{$ENDIF}
   end;

end.
