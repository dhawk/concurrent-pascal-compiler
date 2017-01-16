UNIT main_form_unit;

INTERFACE

uses
   Windows, Classes, Controls, Forms, StdCtrls, Menus;

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
         About2: TMenuItem;
         UnpackPICFilesfromMPLABX1: TMenuItem;
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
         procedure About2Click(Sender: TObject);
         procedure UnpackPICFilesfromMPLABX1Click(Sender: TObject);
      public
         procedure AppendToMemo1 (s: string);
         procedure Discard (s: string);
      end;

var
   MainForm: TMainForm;
   pic18x_directory: string;
   mplabx_directory: string;
   pic_file_directory: string;
   inc_file_directory: string;
   xml_file_directory: string;
   java_jdk_jar_exe_location: string;
   microchip_crownking_edc_jar_location: string;
   mplabx_version: string;

IMPLEMENTATION

{$R *.dfm}

uses
   win32_utils, SysUtils, pic18x_include_file_generator_aboutbox_unit,
   ClipBrd, file_viewer_unit,
   process_all_pic_files_dlg_unit, pic18x_selection_dialog_unit,
   combo_type_dialog_unit, all_pic18x_sfr_field_info_unit, combo_type_unit,
   select_combo_type_dlg_unit,
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

procedure TMainForm.UnpackPICFilesfromMPLABX1Click(Sender: TObject);
   var
      old_cursor: HCURSOR;
   begin
      assert (java_jdk_jar_exe_location <> '');
      assert (microchip_crownking_edc_jar_location <> '');
      old_cursor := screen.Cursor;
      screen.cursor := crHourglass;
      RunProgram ('"' + java_jdk_jar_exe_location + '" xf "' + microchip_crownking_edc_jar_location + '" content/edc/18xxxx',
                  true,
                  mplabx_directory
                 );
      screen.Cursor := old_cursor
   end;

procedure TMainForm.About2Click(Sender: TObject);
   begin
      PIC18xIncludeFileGeneratorAboutBoxForm.ShowModal
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

function locate_java_jdk_jar_exe: string;
   var
      sr: TSearchRec;
   begin
      result := 'C:\Program Files (x86)\Java\';
      if FindFirst (result + '*', faDirectory, sr) = 0 then
         begin
            repeat
               if (sr.Name <> '.')
                  and
                  (sr.Name <> '..')
                  and
                  FileExists (result + sr.Name + '\bin\jar.exe')
               then
                  begin
                     result := result + sr.Name + '\bin\jar.exe';
                     FindClose (sr);
                     exit
                  end
            until FindNext (sr) <> 0;
            FindClose (sr)
         end;
      result := ''
   end;

function locate_microchip_crownking_edc_jar: string;
   const
      default_mplabx_install_location = 'C:\Program Files (x86)\Microchip\MPLABX\';
   var
      sr: TSearchRec;
      installed_mplabx_versions: TStringList;
   begin
      mplabx_version := '';
      if FindFirst (default_mplabx_install_location + '*', faDirectory, sr) = 0 then
         begin
            installed_mplabx_versions := TStringList.Create;
            repeat
               if (sr.Name <> '.')
                  and
                  (sr.Name <> '..')
               then
                  begin
                     assert (sr.Name[1] = 'v');
                     installed_mplabx_versions.Add (sr.Name)
                  end
            until FindNext (sr) <> 0;
            installed_mplabx_versions.Sort;
            mplabx_version := installed_mplabx_versions[installed_mplabx_versions.Count-1];  // should be latest version
            installed_mplabx_versions.Free;
            result := default_mplabx_install_location + mplabx_version + '\mplab_ide\mplablibs\modules\ext\crownking.edc.jar';
            if FileExists (result) then
               exit;
            result := default_mplabx_install_location + mplabx_version + '\mplab_ipe\lib\crownking.edc.jar';
            if FileExists (result + mplabx_version + '\mplab_ipe\lib\crownking.edc.jar') then
               exit
         end;
      result := ''
    end;

INITIALIZATION
   java_jdk_jar_exe_location := locate_java_jdk_jar_exe;
   microchip_crownking_edc_jar_location := locate_microchip_crownking_edc_jar;
   pic18x_directory := ExtractFilePath(ParamStr(0)) + 'pic18x\';
   mplabx_directory := pic18x_directory + 'mplabx\';
   pic_file_directory := mplabx_directory + 'content\edc\18xxxx\';
   inc_file_directory := pic18x_directory + 'include\';
   xml_file_directory := pic18x_directory + 'processor_definition_files\';

END.
