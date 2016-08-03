UNIT process_all_pic_files_dlg_unit;

INTERFACE

uses
  Windows, Classes, Controls, multi_threaded_processing_dialog_base_class_unit, ExtCtrls,
  all_pic18x_sfr_field_info_unit, StdCtrls, Forms;

type
   TProcessAllPICFilesDlg =
      class(TMultiThreadedProcessingDialogBaseClass)
         procedure FormActivate(Sender: TObject);
      private
         extended_files_found: integer;
         all_pic_info: tAllPIC18xInfo;
      protected
         function GetPathStarAndExt: string;
            override;
         function CreateFileProcessingThread: TFileProcessingThread;
            override;
         procedure UpdateProgress;
            override;
         procedure ThreadCompleted;
            override;
      end;

var
   ProcessAllPICFilesDlg: TProcessAllPICFilesDlg;

IMPLEMENTATION

uses
   SysUtils, main_form_unit, process_pic_file_unit, combo_type_unit;

{$R *.dfm}


//=======================
//  TPICFileParserThread
//=======================

type
   TPICFileParserThread =
      class (TFileProcessingThread)
         is_extended: boolean;
         f_inc, f_xml: TextFile;
         combo_type_list: TComboTypeList;
         f_pic_sfr_field_info: tMicroControllerSFRFieldInfo;
         procedure ProcessFile (file_name: string);
            override;
         procedure syncronized_report_file_completed;
            override;
         procedure OutputSource (s: string);
         procedure OutputXML (s: string);
         constructor Create (_dlg: TMultiThreadedProcessingDialogBaseClass);
         destructor Destroy;
            override;
      end;

constructor TPICFileParserThread.Create (_dlg: TMultiThreadedProcessingDialogBaseClass);
   begin
      combo_type_list := TComboTypeList.Create;
      inherited
   end;

destructor TPICFileParserThread.Destroy;
   begin
      combo_type_list.Free;
      inherited
   end;

procedure TPICFileParserThread.ProcessFile  (file_name: string);
   begin
      AssignFile (f_inc, inc_file_directory + ChangeFileExt(file_name, '.inc'));
      Rewrite (f_inc);
      AssignFile (f_xml, xml_file_directory + ChangeFileExt(file_name, '.xml'));
      Rewrite (f_xml);
      f_pic_sfr_field_info := nil;
      process_pic_file (file_name, is_extended, OutputSource, OutputXML, OutputError, f_pic_sfr_field_info, combo_type_list);
      CloseFile (f_inc);
      CloseFile (f_xml);
      if not is_extended then
         begin
            DeleteFile (inc_file_directory + ChangeFileExt(file_name, '.inc'));
            DeleteFile (xml_file_directory + ChangeFileExt(file_name, '.xml'))
         end
   end;

procedure TPICFileParserThread.syncronized_report_file_completed;
   begin
      if is_extended then
         begin
            TProcessAllPICFilesDlg(dlg).extended_files_found := TProcessAllPICFilesDlg(dlg).extended_files_found + 1;
            TProcessAllPICFilesDlg(dlg).all_pic_info.Add(f_pic_sfr_field_info);
            f_pic_sfr_field_info := nil
         end;
      inherited
   end;

procedure TPICFileParserThread.OutputSource (s: string);
   begin
      writeln (f_inc, s)
   end;

procedure TPICFileParserThread.OutputXML (s: string);
   begin
      writeln (f_xml, s)
   end;

function TProcessAllPICFilesDlg.CreateFileProcessingThread: TFileProcessingThread;
   begin
      result := TPICFileParserThread.Create (Self)
   end;

procedure TProcessAllPICFilesDlg.UpdateProgress;
   begin
      files_processed_count := files_processed_count + 1;
      if files_processed_count < total_file_count then
         label1.Caption := format ('%d files out of %d processed, %d PIC18x microcontrollers found', [files_processed_count, total_file_count, extended_files_found])
      else
         begin
            label1.Caption := format ('%d files processed, %d PIC18x microcontrollers found', [files_processed_count, extended_files_found]);
            label4.Caption := 'DONE'
         end
   end;

procedure TProcessAllPICFilesDlg.FormActivate(Sender: TObject);
   begin
      Caption := 'Process All ' + IntToStr(file_count) + ' PIC18 Files';
      extended_files_found := 0;
      all_pic_info := tAllPIC18xInfo.CreateEmpty;
      inherited
   end;

function TProcessAllPICFilesDlg.GetPathStarAndExt: string;
   begin
      result := pic_file_directory + '*.pic'
   end;

procedure TProcessAllPICFilesDlg.ThreadCompleted;
   begin
      inherited;
      if thread_count = 0 then
         begin
            all_pic_info.OutputBinaryDataFile;
            all_pic_info.Free;
            all_pic_info := nil
         end
   end;

END.
