UNIT cpc_core_dev_aboutbox_unit;

{$IFDEF FPC}
   {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
{$IFDEF FPC}
   LCLIntf, LCLType, LMessages,
{$ELSE}
   Windows,
{$ENDIF}
   Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, aboutbox_unit, StdCtrls;

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
