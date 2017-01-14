UNIT config_bit_editor_aboutbox_unit;

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
   Dialogs, pic18x_aboutbox_unit, StdCtrls, ExtCtrls, aboutbox_unit;

type
   TConfigBitEditorAboutBoxForm =
      class(TPIC18xAboutBoxForm)
      private
         { Private declarations }
      public
         { Public declarations }
      end;

var
   ConfigBitEditorAboutBoxForm: TConfigBitEditorAboutBoxForm;

IMPLEMENTATION

{$R *.dfm}

END.
