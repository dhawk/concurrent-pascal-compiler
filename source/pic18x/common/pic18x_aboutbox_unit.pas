UNIT pic18x_aboutbox_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, aboutbox_unit, StdCtrls;

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
