unit file_viewer_unit;

interface

uses
  Windows, Classes, Controls, Forms, SHDocVw, OleCtrls;

type
   TFileViewerForm =
      class(TForm)
         WebBrowser1: TWebBrowser;
    procedure FormActivate(Sender: TObject);
      private
         fFileName: string;
      public
         property FileName: string write fFileName;
      end;


implementation

uses win32_utils, SysUtils;

{$R *.dfm}

procedure TFileViewerForm.FormActivate(Sender: TObject);
   var
      xml_fn: string;
   begin
      Caption := ExtractFileName (fFileName);
      if UpperCase(ExtractFileExt(fFileName)) = '.PIC' then
         begin
            xml_fn := ChangeFileExt (fFileName, '.xml');
            CopyFile (fFileName, xml_fn);
            WebBrowser1.Navigate (xml_fn);
            DeleteFile (xml_fn)
         end
      else
         WebBrowser1.Navigate (fFileName)
   end;

end.
