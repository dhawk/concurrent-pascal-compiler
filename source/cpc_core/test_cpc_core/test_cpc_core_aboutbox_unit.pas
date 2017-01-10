UNIT test_cpc_core_aboutbox_unit;

INTERFACE

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, aboutbox_unit, StdCtrls;

type
   TTestCPCCoreAboutBoxForm =
      class(TAboutBoxForm)
      protected
         procedure AddThirdPartyCredits;
            override;
      end;

var
   TestCPCCoreAboutBoxForm: TTestCPCCoreAboutBoxForm;

IMPLEMENTATION

{$R *.dfm}

procedure TTestCPCCoreAboutBoxForm.AddThirdPartyCredits;
   begin
      AddCommonThirdPartyCredits
   end;

END.
