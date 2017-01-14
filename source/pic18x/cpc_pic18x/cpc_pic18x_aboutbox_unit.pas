unit cpc_pic18x_aboutbox_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pic18x_aboutbox_unit, StdCtrls;

type
   TCPCPIC18xAboutBoxForm =
      class(TPIC18xAboutBoxForm)
      private
         { Private declarations }
      public
         { Public declarations }
      end;

var
   CPCPIC18xAboutBoxForm: TCPCPIC18xAboutBoxForm;

implementation

{$R *.dfm}

end.
