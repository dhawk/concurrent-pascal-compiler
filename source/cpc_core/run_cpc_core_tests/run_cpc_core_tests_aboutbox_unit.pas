UNIT run_cpc_core_tests_aboutbox_unit;

INTERFACE

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, aboutbox_unit, StdCtrls;

type
   TRunCPCCoreTestsAboutBoxForm =
      class(TAboutBoxForm)
      protected
         procedure AddThirdPartyCredits;
            override;
       end;

var
   RunCPCCoreTestsAboutBoxForm: TRunCPCCoreTestsAboutBoxForm;

IMPLEMENTATION

{$R *.dfm}

procedure TRunCPCCoreTestsAboutBoxForm.AddThirdPartyCredits;
   begin
      AddCommonThirdPartyCredits
   end;

END.
