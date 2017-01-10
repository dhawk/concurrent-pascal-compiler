unit pic18x_simulator_tests_aboutbox_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, aboutbox_unit, StdCtrls, pic18x_aboutbox_unit;

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
      AddWirthCredit;
      AddFastMM4Credit
   end;

end.
