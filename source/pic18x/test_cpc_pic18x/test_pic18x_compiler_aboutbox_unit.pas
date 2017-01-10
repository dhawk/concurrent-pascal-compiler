unit test_pic18x_compiler_aboutbox_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pic18x_aboutbox_unit, StdCtrls;

type
  TTestCPCPIC18xAboutBox = class(TPIC18xAboutBoxForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestCPCPIC18xAboutBox: TTestCPCPIC18xAboutBox;

implementation

{$R *.dfm}

end.
