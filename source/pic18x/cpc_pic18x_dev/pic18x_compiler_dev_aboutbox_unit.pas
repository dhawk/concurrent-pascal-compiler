unit pic18x_compiler_dev_aboutbox_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pic18x_aboutbox_unit, StdCtrls;

type
  TCPCPIC18xDevAboutBox = class(TPIC18xAboutBoxForm)
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
