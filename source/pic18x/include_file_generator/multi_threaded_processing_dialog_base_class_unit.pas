UNIT multi_threaded_processing_dialog_base_class_unit;

INTERFACE

uses Windows, SysUtils, Classes, Forms,
   Controls, StdCtrls,  ExtCtrls, processing_frame_unit;

type
   TDirectoryList =
      class
         sr: TSearchRec;
         eol: boolean;
         constructor Create (path_and_ext: string);
         procedure GetNextFile (var fn: string);
         destructor Destroy;
            override;
      end;

   TFileProcessingThread = class;

   TMultiThreadedProcessingDialogBaseClass =
      class(TForm)
         OKBtn: TButton;
         ScrollBox1: TScrollBox;
         Panel1: TPanel;
         Label1: TLabel;
         Label2: TLabel;
         Label4: TLabel;
         StopProcessingAllButton: TButton;
         Timer1: TTimer;
         ErrorMemo: TMemo;
         procedure FormCreate(Sender: TObject);
         procedure FormActivate(Sender: TObject);
         procedure StopProcessingAllButtonClick(Sender: TObject);
         procedure Timer1Timer(Sender: TObject);
         procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      private
         start_time: TTime;
         procedure update_elapsed_time (t: TTime);
      protected
         total_file_count, files_processed_count: integer;
         directory_list: TDirectoryList;
         processing_frame: array of TProcessingFrame;
         thread_count: integer;
         function file_count: integer;
         procedure UpdateProgress;
            virtual;
         function GetPathStarAndExt: string;
            virtual; abstract;
         function CreateFileProcessingThread: TFileProcessingThread;
            virtual; abstract;
         procedure ThreadCompleted;
            virtual;
         procedure GetNextFileName (var fn: string);
         procedure OutputToErrorMemo (s: string);
            overload;
         procedure OutputToErrorMemo (sl: TStringList);
            overload;
      end;

   TFileProcessingThread =
      class (TThread)
         dlg: TMultiThreadedProcessingDialogBaseClass;
         constructor Create (_dlg: TMultiThreadedProcessingDialogBaseClass);
         destructor Destroy;
            override;
         procedure syncronized_report_file_completed;
            virtual;
         procedure ProcessFile (file_name: string);
            virtual; abstract;
         procedure Execute;
            override;
      private
         processed_files_count: integer;
         error_string: string;
         frame: TProcessingFrame;
         current_file_name: string;
         procedure synchronized_output_error;
         procedure synchronized_output_errorlist;
         procedure synchronized_report_thread_completed;
         procedure synchronized_add_filename_to_frame;
      protected
         error_stringlist: TStringList;
         procedure OutputError (s: string);
         procedure OutputErrorList;
      end;

var
   stop_all_threads: boolean;

IMPLEMENTATION

uses win32_utils;

{$R *.dfm}


//=================
//  TDirectoryList

constructor TDirectoryList.Create (path_and_ext: string);
   begin
      eol := FindFirst (path_and_ext, faAnyFile, sr) <> 0
   end;

procedure TDirectoryList.GetNextFile (var fn: string);
   begin
      fn := '';
      if eol then
         exit;
      while (sr.attr and faDirectory) = faDirectory do
         if FindNext(sr) = 0 then
            exit;
      fn := sr.Name;
      eol := FindNext(sr) <> 0
   end;

destructor TDirectoryList.Destroy;
   begin
      FindClose (sr)
   end;


//========================
//  TFileProcessingThread
//========================

constructor TFileProcessingThread.Create (_dlg: TMultiThreadedProcessingDialogBaseClass);
   begin
      dlg := _dlg;
      error_stringlist := TStringList.Create;
      inherited Create (true)
   end;

destructor TFileProcessingThread.Destroy;
   begin
      error_stringlist.Free;
      inherited
   end;

procedure TFileProcessingThread.Execute;
   begin
      while (current_file_name <> '') and (not stop_all_threads) do
         begin
            Synchronize (synchronized_add_filename_to_frame);
            try
               ProcessFile (current_file_name)
            except
               on e: Exception
               do OutputError (e.Message)
            end;
            processed_files_count := processed_files_count + 1;
            Synchronize (syncronized_report_file_completed)
         end;
//      db.ActivateTables(false);
      Synchronize (synchronized_report_thread_completed)
   end;

procedure TFileProcessingThread.synchronized_add_filename_to_frame;
   begin
      frame.Add (current_file_name)
   end;

procedure TFileProcessingThread.syncronized_report_file_completed;
   begin
      frame.Status := processed_files_count;
      dlg.GetNextFileName (current_file_name)
   end;

procedure TFileProcessingThread.synchronized_report_thread_completed;
   begin
      dlg.ThreadCompleted
   end;

procedure TFileProcessingThread.OutputError (s: string);
   begin
      error_string := s;
      Synchronize (synchronized_output_error)
   end;

procedure TFileProcessingThread.OutputErrorList;
   begin
      Synchronize (synchronized_output_errorlist)
   end;

procedure TFileProcessingThread.synchronized_output_error;
   begin
      dlg.OutputToErrorMemo (error_string)
   end;

procedure TFileProcessingThread.synchronized_output_errorlist;
   begin
      dlg.OutputToErrorMemo (error_stringlist);
      error_stringlist.Clear
   end;


//==========================================
//  TMultiThreadedProcessingDialogBaseClass
//==========================================

procedure TMultiThreadedProcessingDialogBaseClass.FormCreate(Sender: TObject);
   const
      margin = 10;
   var
      i: integer;
   begin
      for i := 0 to NumberOfProcessors-1 do
         begin
            SetLength (processing_frame, i+1);
            processing_frame[i] := TProcessingFrame.Create (Self);
            processing_frame[i].Name := processing_frame[i].Name + IntToStr(i);
            processing_frame[i].Parent := Panel1;
            processing_frame[i].Top := margin;
            processing_frame[i].Left := margin + (i * (processing_frame[i].Width + margin));
            processing_frame[i].title := 'Thread ' + IntToStr(i+1);
            processing_frame[i].Clear;
            processing_frame[i].Label1.Caption := ''
         end;
      ScrollBox1.HorzScrollBar.Range := margin + (NumberOfProcessors * (processing_frame[0].Width + margin));
      Label1.Left := margin;
      Label1.Top := processing_frame[0].Height + (2 * margin);
      Label1.Caption := '';
      Label2.Top := Label1.Top;
      Label2.Caption := '';
      Label4.Top := Label2.Top + 22;
      Label4.Caption := '';
      Label4.Left := margin;
      ScrollBox1.Height := Label4.Top + 50
   end;

procedure TMultiThreadedProcessingDialogBaseClass.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
   begin
      CanClose := thread_count = 0
   end;

procedure TMultiThreadedProcessingDialogBaseClass.update_elapsed_time (t: TTime);
   begin
      Label2.Caption := format ('Elapsed Time: %1.0f seconds', [(t - start_time) * 24.0 * 60.0 * 60.0])
   end;

procedure TMultiThreadedProcessingDialogBaseClass.FormActivate(Sender: TObject);
   var
      i: integer;
   begin
      directory_list := TDirectoryList.Create (GetPathStarAndExt);
      stop_all_threads := false;
      StopProcessingAllButton.Enabled := true;
      total_file_count := file_count;
      files_processed_count := -1;
      start_time := Time;
      update_elapsed_time (start_time);
      Timer1.Enabled := true;
      ErrorMemo.Clear;
      UpdateProgress;
      Label4.Caption := '';
      thread_count := 0;
      for i := 0 to NumberOfProcessors-1 do
         begin
            processing_frame[i].Clear;
            thread_count := thread_count + 1;
            with CreateFileProcessingThread do
               begin
                  FreeOnTerminate := true;
                  directory_list.GetNextFile (current_file_name);
                  frame := processing_frame[i];
                  Start
               end
         end
   end;

procedure TMultiThreadedProcessingDialogBaseClass.StopProcessingAllButtonClick(Sender: TObject);
   begin
      stop_all_threads := true
   end;

procedure TMultiThreadedProcessingDialogBaseClass.Timer1Timer(Sender: TObject);
   begin
      update_elapsed_time (Time)
   end;

procedure TMultiThreadedProcessingDialogBaseClass.GetNextFileName (var fn: string);
   begin
      UpdateProgress;
      directory_list.GetNextFile (fn)
   end;

procedure TMultiThreadedProcessingDialogBaseClass.ThreadCompleted;
   begin
      thread_count := thread_count - 1;
      if thread_count = 0 then
         begin
            if ErrorMemo.Lines.Count = 0 then
               if stop_all_threads then
                  ErrorMemo.Lines.Add ('Stopped by user')
               else
                  ErrorMemo.Lines.Add ('No errors');
            StopProcessingAllButton.Enabled := false;
            directory_list.Free;
            directory_list := nil;
            stop_all_threads := false;
            Timer1.Enabled := false;
            update_elapsed_time (Time)
         end
   end;

procedure TMultiThreadedProcessingDialogBaseClass.UpdateProgress;
   begin
      files_processed_count := files_processed_count + 1;
      if files_processed_count < total_file_count then
         label1.Caption := format ('%d files out of %d processed', [files_processed_count, total_file_count])
      else
         begin
            label1.Caption := format ('%d files processed', [files_processed_count]);
            label4.Caption := 'DONE'
         end
   end;

procedure TMultiThreadedProcessingDialogBaseClass.OutputToErrorMemo (s: string);
   begin
      ErrorMemo.Lines.Add (s)
   end;

procedure TMultiThreadedProcessingDialogBaseClass.OutputToErrorMemo (sl: TStringList);
   var
      s: string;
   begin
      ErrorMemo.Lines.Add ('--------------------------');
      for s in sl do
         ErrorMemo.Lines.Add (s);
      ErrorMemo.Lines.Add ('--------------------------')
   end;

function TMultiThreadedProcessingDialogBaseClass.file_count: integer;
   var
      sr: TSearchRec;
   begin
      result := 0;
      if FindFirst (GetPathStarAndExt, faAnyFile, sr) = 0
      then
         begin
            repeat
               if (sr.attr and faDirectory) <> faDirectory then
                  result := result + 1
            until FindNext (sr) <> 0;
            FindClose (sr)
         end;
   end;

END.
