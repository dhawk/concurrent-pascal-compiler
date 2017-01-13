unit cpc_pic18x_aboutbox_unit;

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
