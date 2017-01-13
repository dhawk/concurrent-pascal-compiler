UNIT run_cpc_core_tests_aboutbox_unit;

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
