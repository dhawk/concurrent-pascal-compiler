unit pic18x_include_file_generator_aboutbox_unit;

interface

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, aboutbox_unit, StdCtrls;

type
   TPIC18xIncludeFileGeneratorAboutBoxForm =
      class(TAboutBoxForm)
      protected
         procedure AddThirdPartyCredits;
            override;
      end;

var
   PIC18xIncludeFileGeneratorAboutBoxForm: TPIC18xIncludeFileGeneratorAboutBoxForm;

implementation

{$R *.dfm}

procedure TPIC18xIncludeFileGeneratorAboutBoxForm.AddThirdPartyCredits;
   begin
      AddWirthCredit
   end;

end.
