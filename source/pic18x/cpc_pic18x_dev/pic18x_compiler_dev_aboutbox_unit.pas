unit pic18x_compiler_dev_aboutbox_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pic18x_aboutbox_unit, StdCtrls;

type
   TCPCPIC18xDevAboutBox =
      class(TPIC18xAboutBoxForm)
      private
         { Private declarations }
      public
         { Public declarations }
      end;

var
   CPCPIC18xDevAboutBox: TCPCPIC18xDevAboutBox;

implementation

{$R *.dfm}

end.
