UNIT pic18x_aboutbox_unit;

INTERFACE

uses
   aboutbox_unit,
   Classes,
   Controls,
   Dialogs,
   Forms,
   Graphics,
   Messages,
   StdCtrls,
   SysUtils,
   Variants,
   Windows;

type
   TPIC18xAboutBoxForm =
      class(TAboutBoxForm)
         protected
            procedure AddThirdPartyCredits;
               override;
      end;

IMPLEMENTATION

{$R *.dfm}

procedure TPIC18xAboutBoxForm.AddThirdPartyCredits;
   begin
      AddCommonThirdPartyCredits;
      // todo: add other credits...
   end;

END.
