unit run_pic18x_compiler_tests_aboutbox_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pic18x_aboutbox_unit, StdCtrls;

type
  TPIC18xAboutBoxForm1 = class(TPIC18xAboutBoxForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   PIC18xAboutBoxForm1: TPIC18xAboutBoxForm1;

implementation

{$R *.dfm}

end.
