unit syntax_check_all_include_files_dialog_unit;

interface

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
   Vcl.Controls, Vcl.Forms, Vcl.Dialogs,  Vcl.ExtCtrls, Vcl.StdCtrls,
   multi_threaded_processing_dialog_base_class_unit;

type
   TSyntaxCheckAllIncludeFilesDialog =
      class(TMultiThreadedProcessingDialogBaseClass)
         procedure FormActivate(Sender: TObject);
      protected
         function GetPathStarAndExt: string;
            override;
         function CreateFileProcessingThread: TFileProcessingThread;
            override;
         procedure ThreadCompleted;
            override;
      end;

var
   SyntaxCheckAllIncludeFilesDialog: TSyntaxCheckAllIncludeFilesDialog;

implementation

uses
   main_form_unit, win32_utils;

{$R *.dfm}

//==============================
//  TIncFileSyntaxCheckerThread
//==============================

type
   TIncludeFileSyntaxCheckerThread =
      class (TFileProcessingThread)
         src_file_name: string;
         constructor Create (_dlg: TMultiThreadedProcessingDialogBaseClass; _src_file_name: string);
         procedure Execute;
            override;
         procedure ProcessFile (fn: string);
            override;
         procedure syncronized_report_file_completed;
            override;
      end;

constructor TIncludeFileSyntaxCheckerThread.Create (_dlg: TMultiThreadedProcessingDialogBaseClass; _src_file_name: string);
   begin
      src_file_name := _src_file_name;
      inherited Create (_dlg)
   end;

procedure TIncludeFileSyntaxCheckerThread.Execute;
   begin
      try
         inherited
      finally
         DeleteFile (src_file_name);
         DeleteFile (ChangeFileExt (src_file_name, '.asm'));
         DeleteFile (ChangeFileExt (src_file_name, '.hex'))
      end
   end;

procedure TIncludeFileSyntaxCheckerThread.ProcessFile (fn: string);
   var
      f: TextFile;
      sl: TStringList;
      i: integer;
      proc_name: string;
   begin
      proc_name := Copy (fn, 1, Pos('.', fn)-1);

      AssignFile (f, src_file_name);
      Rewrite (f);
      writeln (f, '{$processor ''' + proc_name + '''}');
      writeln (f, 'begin');
      writeln (f, 'end.');
      CloseFile (f);

      sl := GetDosOutput (ExtractShortPathName(ExtractFilePath(ParamStr(0))) + 'cpc_pic18x ' + ExtractFileName(src_file_name),
                          ExtractFileDir(src_file_name)
                         );

      assert (sl.Count >= 3);
      assert (Trim(sl[0]) = 'PIC18x Concurrent Pascal Compiler');
      assert (Trim(sl[1]) = '');
      if Trim(sl[2]) <> 'Compiled Ok' then
         begin
            error_stringlist.Clear;
            error_stringlist.Add ('Error in ' + ExtractFileName(fn));
            for i := 2 to sl.Count-1 do
               error_stringlist.Add (sl[i]);
            OutputErrorList
         end;
      sl.Free
   end;

procedure TIncludeFileSyntaxCheckerThread.syncronized_report_file_completed;
   begin
      inherited
   end;


//====================================
//  TSyntaxCheckAllIncludeFilesDialog
//====================================

procedure TSyntaxCheckAllIncludeFilesDialog.FormActivate(Sender: TObject);
   begin
      Caption := 'Syntax Check All ' + IntToStr(file_count) + ' Include Files';
      inherited
   end;

function TSyntaxCheckAllIncludeFilesDialog.GetPathStarAndExt: string;
   begin
      result := inc_file_directory + '*.inc'
   end;

function TSyntaxCheckAllIncludeFilesDialog.CreateFileProcessingThread: TFileProcessingThread;
   begin
      result := TIncludeFileSyntaxCheckerThread.Create (Self, GetTempFileName('.cp'))
   end;

procedure TSyntaxCheckAllIncludeFilesDialog.ThreadCompleted;
   begin
      inherited
   end;


end.
