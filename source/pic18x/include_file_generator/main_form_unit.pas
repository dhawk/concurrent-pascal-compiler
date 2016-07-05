UNIT main_form_unit;

// NOTE SAX LIBRARY REQUIRES RANGE CHECKING TO BE OFF!!!!

INTERFACE

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.TabNotBk, Vcl.ComCtrls,
  Vcl.ExtCtrls, processing_frame_unit, Vcl.OleCtrls, SHDocVw, Vcl.Menus,
  Data.DB, adsdata, adsfunc, adstable, adscnnct;

type
   TMainForm =
      class (TForm)
         MainMenu1: TMainMenu;
         ViewTopMenuItem: TMenuItem;
         ViewPICFileMenuItem: TMenuItem;
         ViewXMLFileMenuItem: TMenuItem;
         ViewIncFileMenuItem: TMenuItem;
         ProcessTopMenuItem: TMenuItem;
         ProcessAllPICFilesMenuItem: TMenuItem;
         Memo1: TMemo;
         Button5: TButton;
         Button4: TButton;
         ComboTypeMainMenuItem: TMenuItem;
         EditComboTypeMenuItem: TMenuItem;
         NewComboTypeMenuItem: TMenuItem;
    Button1: TButton;
    Button2: TButton;
         Label1: TLabel;
    Button3: TButton;
    SyntaxCheckIncludeFilesMenuItem: TMenuItem;
    About1: TMenuItem;
         procedure Button4Click(Sender: TObject);
         procedure Button5Click(Sender: TObject);
         procedure ViewPICFileMenuItemClick(Sender: TObject);
         procedure ViewXMLFileMenuItemClick(Sender: TObject);
         procedure ViewIncFileMenuItemClick(Sender: TObject);
         procedure ProcessAllPICFilesMenuItemClick(Sender: TObject);
         procedure ComboTypeEditMenuItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SyntaxCheckIncludeFilesMenuItemClick(Sender: TObject);
    procedure NewComboTypeMenuItemClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
      public
         procedure AppendToMemo1 (s: string);
         procedure Discard (s: string);
      end;

var
   MainForm: TMainForm;
   pic_file_directory: string;
   inc_file_directory: string;
   xml_file_directory: string;

IMPLEMENTATION

{$R *.dfm}

uses
   pic18x_typedef_unit, pic_file_parser_unit, win32_utils,
   pic18x_information_unit, process_pic_file_unit, ClipBrd, file_viewer_unit,
   pic18x_interrupt_variable_unit, pic18x_config_bits_unit, pic18x_ioreg_unit,
   process_all_pic_files_dlg_unit, pic18x_selection_dialog_unit,
   combo_type_dialog_unit, all_pic18x_sfr_field_info_unit, combo_type_unit,
   select_combo_type_dlg_unit, view_c_declarations_unit,
  syntax_check_all_include_files_dialog_unit;


//============
//  TMainForm

procedure TMainForm.Button1Click(Sender: TObject);
var x: tAllPIC18xInfo;
begin
   x := tAllPIC18xInfo.CreateFromBinaryDataFile;
   x.Free
end;

procedure TMainForm.Button3Click(Sender: TObject);
var ctl: TComboTypeList;
begin
   ctl := TComboTypeList.Create;
   ctl.Save;
   ctl.Free
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
   try
      RunProgram ('jar.exe', true);
   except
      on e: Exception do
         if Pos ('Unable to run ', e.Message) = 1
            then raise exception.create ('no jar')
   end
end;

procedure TMainForm.Button5Click(Sender: TObject);
   begin
      Clipboard.AsText := memo1.lines.text
   end;

procedure TMainForm.ProcessAllPICFilesMenuItemClick(Sender: TObject);
   begin
      ProcessAllPICFilesDlg.ShowModal
   end;

procedure TMainForm.SyntaxCheckIncludeFilesMenuItemClick(Sender: TObject);
   begin
      SyntaxCheckAllIncludeFilesDialog.ShowModal
   end;

procedure TMainForm.About1Click(Sender: TObject);
begin
   c
end;

procedure TMainForm.AppendToMemo1 (s: string);
   begin
      Memo1.Lines.Add (s)
   end;

procedure TMainForm.Discard (s: string);
   begin
   end;

procedure TMainForm.NewComboTypeMenuItemClick(Sender: TObject);
   var
      ct: TComboType;
      ctl: TComboTypeList;
   begin
      ct := TComboType.CreateNew;
      ComboTypeDialog.ComboType := ct;
      ct.Free;
      if ComboTypeDialog.ShowModal = mrOk then
         begin
            ctl := TComboTypeList.Create;
            ctl.Add (ComboTypeDialog.ComboType);
            ctl.Sort;
            ctl.Save;
            ctl.Free
         end
   end;

procedure TMainForm.ComboTypeEditMenuItemClick (Sender: TObject);
   var
      ctl: TComboTypeList;
      i, selected_idx: integer;
   begin
      ctl := TComboTypeList.Create;
      SelectComboTypeDlg.LoadControls (ctl);
      if SelectComboTypeDlg.ShowModal = mrOk then
         begin
            selected_idx := -1;
            for i := 0 to ctl.Count-1 do
               if ctl[i] = SelectComboTypeDlg.Selected then
                  selected_idx := i;
            assert (selected_idx <> -1);
            ComboTypeDialog.ComboType := ctl[selected_idx];
            case ComboTypeDialog.ShowModal of
               mrOk:
                  begin
                     ctl.Delete(selected_idx);
                     ctl.Add(ComboTypeDialog.ComboType);
                     ctl.Sort;
                     ctl.Save
                  end;
               mrAbort:   // Delete Type
                  begin
                     ctl.Delete(selected_idx);
                     ctl.Sort;
                     ctl.Save
                  end;
               mrCancel:
                  ;
            else
               assert (false)
            end
         end;
      ctl.Free
   end;

procedure TMainForm.ViewIncFileMenuItemClick(Sender: TObject);
   begin
      if Pic18xSelectionDialog.Execute (inc_file_directory, '.inc') then
         with TFileViewerForm.Create (self) do
            begin
               FileName := inc_file_directory + Pic18xSelectionDialog.SelectedPIC + '.inc';
               Show
            end
   end;

procedure TMainForm.ViewPICFileMenuItemClick(Sender: TObject);
   begin
      if Pic18xSelectionDialog.Execute (pic_file_directory, '.pic') then
         with TFileViewerForm.Create (self) do
            begin
                FileName := pic_file_directory + Pic18xSelectionDialog.SelectedPIC + '.PIC';
                Show
            end
   end;

procedure TMainForm.ViewXMLFileMenuItemClick(Sender: TObject);
   begin
      if Pic18xSelectionDialog.Execute (xml_file_directory, '.xml') then
         with TFileViewerForm.Create (self) do
            begin
               FileName := xml_file_directory + Pic18xSelectionDialog.SelectedPIC + '.xml';
               Show
            end
   end;

INITIALIZATION
   pic_file_directory := ExtractFilePath(ParamStr(0)) + 'pic18x\mplabx\18xxxx\';   // relative to bin
   inc_file_directory := ExtractFilePath(ParamStr(0)) + 'pic18x\include\';
   xml_file_directory := ExtractFilePath(ParamStr(0)) + 'pic18x\processor_definition_files\';

END.
