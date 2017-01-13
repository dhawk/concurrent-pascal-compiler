unit run_pic18x_kernel_tests_aboutbox_unit;

interface

uses
   Classes,
   Controls,
   Dialogs,
   Forms,
   Graphics,
   Messages,
   pic18x_aboutbox_unit,
   StdCtrls,
   SysUtils,
   Variants,
   Windows;

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
