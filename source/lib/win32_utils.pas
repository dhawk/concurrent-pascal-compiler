UNIT win32_utils;

INTERFACE

uses
  Spin, Classes;

function min (a,b: integer): integer;
   overload;
function min (a,b: real): real;
   overload;
function max (a,b: integer): integer;
   overload;
function max (a,b: real): real;
   overload;

procedure CopyFile (from_file_name, to_file_name: string);

procedure RunProgram (CmdLine: String; Wait: Boolean; _WorkDir: string = 'C:\');

function NumberOfProcessors: Integer;

function GetVersionInfo (AIdent: String): String;

function GetTempFileName(const Extension: string): string;

function GetDosOutput(CommandLine: string; _WorkDir: string = 'C:\'): TStringList;

{$ifndef FPC}
{$IF CompilerVersion < 20}  // Delphi 2009
type
   TSetOfChar = set of char;
function CharInSet(c: char; s: TSetOfChar): boolean;
{$IFEND}
{$endif}

IMPLEMENTATION

uses Windows, SysUtils;

{$ifndef FPC}
{$IF CompilerVersion < 20}
function CharInSet(c: char; s: TSetOfChar): boolean;
   begin
      result := c in s
   end;
{$IFEND}
{$endif}

function min (a,b: integer): integer;
   begin
      if a < b
      then result := a
      else result := b
   end;

function min (a,b: real): real;
   begin
      if a < b
      then result := a
      else result := b
   end;

function max (a,b: integer): integer;
   begin
      if a > b
      then result := a
      else result := b
   end;

function max (a,b: real): real;
   begin
      if a > b
      then result := a
      else result := b
   end;

procedure CopyFile (from_file_name, to_file_name: string);
   var
      S, T: TFileStream;
   begin
      S := TFileStream.Create(from_file_name, fmOpenRead );
         // might throw EFOpenError if file is being modified by external editor
      try
         T := TFileStream.Create(to_file_name, fmOpenWrite or fmCreate );
         try
            T.CopyFrom(S, S.Size)
         finally
            T.Free
         end;
      finally
         S.Free
      end
   end;

procedure RunProgram (CmdLine: String; Wait: Boolean; _WorkDir: string = 'C:\');
   var
      StartInfo: TStartupInfo;
      ProcInfo: TProcessInformation;
      CopyOfCmdLine: string;
      WorkDir: string;
   begin
      CopyOfCmdLine := CmdLine + ' ';
         // a separate copy is needed to ensure param is not
         //    in read-only memory (a CreateProcessW problem)

      FillChar(StartInfo,SizeOf(TStartupInfo),#0);
      FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
      StartInfo.cb := SizeOf(TStartupInfo);
      WorkDir := _WorkDir;
      if not CreateProcess (nil,
                            PChar(CopyOfCmdLine),
                            nil,
                            nil,
                            False,
                            CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
                            nil,
                            PChar(WorkDir),
                            StartInfo,
                            ProcInfo
                           )
      then
         raise Exception.Create ('Unable to run ' + CmdLine);

      if Wait
      then
         WaitForSingleObject(ProcInfo.hProcess, INFINITE);

      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread)
   end;


function GetDosOutput (CommandLine: string; _WorkDir: string = 'C:\'): TStringList;
// Author: 	Joe Donth
// http://www.delphidabbler.com/tips/61
   var
      SecurityAttributes: TSecurityAttributes;
      StartupInfo: TStartupInfo;
      ProcessInfo: TProcessInformation;
      StdOutPipeRead, StdOutPipeWrite: THandle;
      WasOK: Boolean;
      Buffer: array[0..255] of AnsiChar;
      BytesRead: Cardinal;
      WorkDir: string;
   begin
      Result := TStringList.Create;
      FillChar (SecurityAttributes, SizeOf(TSecurityAttributes), #0);
      FillChar (StartupInfo, SizeOf(TStartupInfo), #0);
      FillChar (ProcessInfo, SizeOf(TProcessInformation), #0);
      with SecurityAttributes do
         begin
            nLength := SizeOf(SecurityAttributes);
            bInheritHandle := True;
            lpSecurityDescriptor := nil
         end;
      CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecurityAttributes, 0);
      try
         with StartupInfo do
            begin
               cb := SizeOf(StartupInfo);
               dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
               wShowWindow := SW_HIDE;
               hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
               hStdOutput := StdOutPipeWrite;
               hStdError := StdOutPipeWrite
            end;
         WorkDir := _WorkDir;
         WasOK := CreateProcess (nil,                                        // ApplicationName
                                 PChar('cmd.exe /C ' + CommandLine),         // CommandLine
                                 nil,                                        // ProcessAttributes
                                 nil,                                        // ThreadAttributes
                                 true,                                       // InheritHandles
                                 0,                                          // CreationFlags
                                 nil,                                        // Environment
                                 PChar(WorkDir),                             // CurrentDir
                                 StartupInfo,                                // StartupInfo
                                 ProcessInfo                                 // ProcessInformation
                                );
         CloseHandle(StdOutPipeWrite);
         if WasOK then
            try
               WaitForSingleObject(ProcessInfo.hProcess,  INFINITE);
               repeat
                  WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
                  if BytesRead > 0 then
                     begin
                        Buffer[BytesRead] := #0;
                        Result.Text := Result.Text + string(Buffer)
                     end;
               until not WasOK or (BytesRead = 0)
            finally
               CloseHandle(ProcessInfo.hThread);
               CloseHandle(ProcessInfo.hProcess)
            end
      finally
         CloseHandle(StdOutPipeRead);
      end
   end;

function NumberOfProcessors: Integer;
   var
       systemInfo: SYSTEM_INFO;
   begin
       GetSystemInfo(systemInfo);
       Result := systemInfo.dwNumberOfProcessors
   end;


//=================
//  GetVersionInfo
//      Author:
//         Jiri Krivanek
//      Reference:
//         http://stackoverflow.com/questions/1717844/how-to-determine-delphi-application-version
//      Usage:
//         Result := GetVersionInfo('FileDescription');
//         Result := GetVersionInfo('LegalCopyright');
//         Result := GetVersionInfo('DateOfRelease');
//         Result := GetVersionInfo('ProductVersion');
//         Result := GetVersionInfo('FileVersion');

function GetVersionInfo(AIdent: String): String;

   type
     TLang = packed record
       Lng, Page: WORD;
     end;

     TLangs = array [0 .. 10000] of TLang;

     PLangs = ^TLangs;

   var
     BLngs: PLangs;
     BLngsCnt: Cardinal;
     BLangId: String;
     RM: TMemoryStream;
     RS: TResourceStream;
     BP: PChar;
     BL: Cardinal;
     BId: String;

   begin
     // Assume error
     Result := '';

     RM := TMemoryStream.Create;
     try
       // Load the version resource into memory
       RS := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
       try
         RM.CopyFrom(RS, RS.Size);
       finally
         FreeAndNil(RS);
       end;

       // Extract the translations list
       if not VerQueryValue(RM.Memory, '\\VarFileInfo\\Translation', Pointer(BLngs), BL) then
         Exit; // Failed to parse the translations table
       BLngsCnt := BL div sizeof(TLang);
       if BLngsCnt <= 0 then
         Exit; // No translations available

       // Use the first translation from the table (in most cases will be OK)
       with BLngs[0] do
         BLangId := IntToHex(Lng, 4) + IntToHex(Page, 4);

       // Extract field by parameter
       BId := '\\StringFileInfo\\' + BLangId + '\\' + AIdent;
       if not VerQueryValue(RM.Memory, PChar(BId), Pointer(BP), BL) then
         Exit; // No such field

       // Prepare result
       Result := BP;
     finally
       FreeAndNil(RM);
     end;
   end;

// for some reason these variables need to be global rather than local???
var
   Path, FileName: array [0..MAX_PATH] of char;

function GetTempFileName (const Extension: string): string;
   label 1;
   begin
1:    FillChar (Path, MAX_PATH, 0);
      GetTempPath (SizeOf(Path), Path);
      FillChar (FileName, MAX_PATH, 0);
      Windows.GetTempFileName (Path, '~', 0, FileName);
      result := FileName;
      if LowerCase(ExtractFileExt(result)) = LowerCase(Extension) then
         exit;
      if FileExists (ChangeFileExt (result, Extension)) then
         begin
            DeleteFile (result);
            goto 1
         end;
      RenameFile (Result, ChangeFileExt(Result, Extension));
      result := ChangeFileExt (result, Extension)
   end;

END.
