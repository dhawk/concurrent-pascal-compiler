unit pic18x_simulator_tests_aboutbox_unit;

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
{$IFNDEF FPC}
      AddFastMM4Credit;
{$ENDIF}
      AddWirthCredit
   end;

end.
