unit pic18x_simulator_tests_aboutbox_unit;

interface

uses
   aboutbox_unit,
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
   TPIC18xSimulatorTestsAboutBoxForm =
      class(TAboutBoxForm)
         protected
            procedure AddThirdPartyCredits;
               override;
      end;

var
   PIC18xSimulatorTestsAboutBoxForm: TPIC18xSimulatorTestsAboutBoxForm;

implementation

{$R *.dfm}

procedure TPIC18xSimulatorTestsAboutBoxForm.AddThirdPartyCredits;
   begin
{$IFNDEF FPC}
      AddFastMM4Credit;
{$ENDIF}
      AddWirthCredit
   end;

end.
