UNIT cpc_core_dev_aboutbox_unit;

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
   TCPCCoreDevAboutBoxForm =
      class(TAboutBoxForm)
      protected
         procedure AddThirdPartyCredits;
            override;
      end;

var
   CPCCoreDevAboutBoxForm: TCPCCoreDevAboutBoxForm;

IMPLEMENTATION

{$R *.dfm}

procedure TCPCCoreDevAboutBoxForm.AddThirdPartyCredits;
   begin
      AddCommonThirdPartyCredits
   end;

END.
