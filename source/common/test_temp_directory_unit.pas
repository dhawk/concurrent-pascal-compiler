UNIT test_temp_directory_unit;

INTERFACE

var temp_dir_for_tests: string;

IMPLEMENTATION

uses
   SysUtils,
   Windows;

procedure CreateTempDirForTests;
   var
      guid: TGUID;
      bufFolder: array [0..MAX_PATH] of Char;
   begin
      GetTempPath(SizeOf(bufFolder), bufFolder);
      temp_dir_for_tests := IncludeTrailingPathDelimiter(String(bufFolder));
      CreateGUID (guid);
      temp_dir_for_tests := temp_dir_for_tests + GUIDToString (guid) + PathDelim;
      CreateDir (temp_dir_for_tests)
   end;

procedure DeleteTempFilesForTests;
   procedure DeleteDirectory (dir: string);
      var
         sr: TSearchRec;
      begin
         if FindFirst(dir + pathdelim + '*.*', faAnyfile, sr) = 0 then
            begin
               repeat
                  if (sr.Name <> '.') and (sr.Name <> '..') then
                     if sr.Attr and faDirectory = faDirectory then
                        DeleteDirectory (dir + pathdelim + sr.Name)
                     else
                        SysUtils.DeleteFile (dir + pathdelim + sr.Name)
               until FindNext (sr) <> 0;
               SysUtils.FindClose (sr)
            end;
         RmDir (dir)
      end;
   begin
      DeleteDirectory (temp_dir_for_tests);
      temp_dir_for_tests := ''
   end;

INITIALIZATION
   CreateTempDirForTests;

FINALIZATION
   DeleteTempFilesForTests;

END.
