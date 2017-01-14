unit run_pic18x_kernel_tests_aboutbox_unit;

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
   TRunPIC18xKernelTestsAboutBoxForm =
      class(TPIC18xAboutBoxForm)
      private
         { Private declarations }
      public
         { Public declarations }
      end;

var
   RunPIC18xKernelTestsAboutBoxForm: TRunPIC18xKernelTestsAboutBoxForm;

implementation

{$R *.dfm}

end.
