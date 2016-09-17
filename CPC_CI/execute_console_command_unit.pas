{ (drh) tExecuteConsoleCommand derived from:
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #8: Echoing console output to a GUI.
 *
 * $Rev: 1358 $
 * $Date: 2013-03-25 03:31:31 +0000 (Mon, 25 Mar 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

UNIT execute_console_command_unit;

INTERFACE

uses
  System.Classes;

function ExecuteConsoleCommand (cmd: string): TStringList;

IMPLEMENTATION

uses
   PJConsoleApp, PJPipe, PJPipeFilters, System.SysUtils, Vcl.Forms;

type
   tExecuteConsoleCommand =
      class
         f_cmd_result: TStringList;
         constructor Create(cmd: string; cmd_result: TStringList);
      private
         fErrFilter, fOutFilter: TPJAnsiSBCSPipeFilter;
         err_result: TStringList;
         procedure ErrLineEndHandler(Sender: TObject; const Line: AnsiString);
         procedure OutLineEndHandler(Sender: TObject; const Line: AnsiString);
         procedure WorkHandler(Sender: TObject);
         procedure CompletionHandler(Sender: TObject);
      end;

function ExecuteConsoleCommand (cmd: string): TStringList;
   var ce: tExecuteConsoleCommand;
   begin
      ce := nil;
      result := TStringList.Create;
      try
         ce := tExecuteConsoleCommand.Create (cmd, result)
      finally
         ce.Free
      end
   end;

constructor tExecuteConsoleCommand.Create(cmd: string; cmd_result: TStringList);
   var
      App: TPJConsoleApp;
      OutPipe, ErrPipe: TPJPipe;
   begin
      fOutFilter := nil;
      fErrFilter := nil;
      OutPipe := nil;
      ErrPipe := nil;
      f_cmd_result := cmd_result;
      err_result := TStringList.Create;
      try
         // Create output pipes: one each for stdout and stderr
         OutPipe := TPJPipe.Create;
         ErrPipe := TPJPipe.Create;

         // Create filter objects used to format text from output pipe into lines
         fOutFilter := TPJAnsiSBCSPipeFilter.Create(OutPipe);
         fOutFilter.OnLineEnd := OutLineEndHandler;
         fErrFilter := TPJAnsiSBCSPipeFilter.Create(ErrPipe);
         fErrFilter.OnLineEnd := ErrLineEndHandler;
         App := TPJConsoleApp.Create;
         try
            // redirect stdout/stderr to pipes
            App.StdOut := OutPipe.WriteHandle;
            App.StdErr := ErrPipe.WriteHandle;
            App.OnWork := WorkHandler;
            App.OnComplete := CompletionHandler;
            App.TimeSlice := 1;
            if not App.Execute(cmd) then
               raise Exception.CreateFmt('Error %X: %s', [App.ErrorCode, App.ErrorMessage]);
          finally
             App.Free
          end;
      finally
         FreeAndNil(fErrFilter);
         FreeAndNil(fOutFilter);
         ErrPipe.Free;
         OutPipe.Free;
         err_result.Free
      end;
   end;

procedure tExecuteConsoleCommand.CompletionHandler(Sender: TObject);
begin
  fOutFilter.Flush;
  fErrFilter.Flush;
end;

procedure tExecuteConsoleCommand.ErrLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  err_result.Add(string(Line));
end;

procedure tExecuteConsoleCommand.OutLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  f_cmd_result.Add(string(Line));
end;

procedure tExecuteConsoleCommand.WorkHandler(Sender: TObject);
begin
  fOutFilter.ReadPipe;
  fErrFilter.ReadPipe;
  Application.ProcessMessages;       // Let the memo controls update
end;


END.
