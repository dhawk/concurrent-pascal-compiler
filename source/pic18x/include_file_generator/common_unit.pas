UNIT common_unit;

INTERFACE

type
   TOutStringProc = procedure (s: string) of object;

function sanitize_xml_strings (s: string): string;

IMPLEMENTATIOn

uses
  System.SysUtils, System.RegularExpressions;

function sanitize_xml_strings (s: string): string;
   begin
      result := StringReplace (s, '&', '&amp;', [rfReplaceAll]);
      result := StringReplace (result, '"', '&quot;', [rfReplaceAll]);
      result := StringReplace (result, '<', '&lt;', [rfReplaceAll])
      // > and ' may appear in attribute strings
   end;

END.
