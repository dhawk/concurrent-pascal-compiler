UNIT pic18x_common_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

function config_bits_constant_name (proc: string): string;
function config_bits_constant_type_name (proc: string): string;

function config_bits_field_enum_name (byte_cname, field_cname, value_cname: string): string;
function extract_value_cname_from_config_bits_field_enum_name (constant_field_enum_name: string): string;

IMPLEMENTATIOn

uses
   SysUtils;

function config_bits_constant_name (proc: string): string;
   begin
      result := UpperCase(proc) + '_configuration_bits'
   end;

function config_bits_constant_type_name (proc: string): string;
   begin
      result := 't' + config_bits_constant_name(proc)
   end;

function config_bits_field_enum_name (byte_cname, field_cname, value_cname: string): string;
   begin
      result := byte_cname + '_' + field_cname + '_' + value_cname
   end;

function extract_value_cname_from_config_bits_field_enum_name (constant_field_enum_name: string): string;
   begin
      assert (pos ('_', constant_field_enum_name) > 1);
      result := Copy (constant_field_enum_name, Pos('_', constant_field_enum_name) + 1, 99999);
      assert (pos ('_', result) > 1);
      result := Copy (result, pos('_', result) + 1, 99999)
   end;

END.
