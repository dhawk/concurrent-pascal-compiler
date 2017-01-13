unit pic18x_compiler_dev_aboutbox_unit;

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
