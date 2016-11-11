UNIT pic18x_main_compiler_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
  cpc_definitions_unit;

var
   source_file_name: string;

function ProgramGenerator: TDefinition;

IMPLEMENTATION

uses
  pic18x_macro_instructions_unit, pic18x_instructions_unit, pic18x_run_time_error_check_unit,
  cpc_target_cpu_unit, pic18x_blocks_unit, SysUtils, pic18x_cpu_unit;

function ProgramGenerator: TDefinition;
   var
      i: integer;
      changed: boolean;
      last_instr: TInstruction;
   begin
      result := target_cpu.TProgram_CreateFromSourceTokens;
      ProgramCode.Clear;
      try
         target_cpu.generate_machine_code (TPIC18x_Program(result))
      except
         on e: Exception do
            begin
               result.Free;
               raise
            end;
      end;
      ProgramCode.DoPeepHoleOptimization;
      repeat
         ProgramCode.AssignROMAddresses;
         changed := false;
         for i := 0 to Length(ProgramCode.instr_arr)-1 do
            ProgramCode.instr_arr[i].recalculate_size (changed)
      until not changed;

      i := Length(ProgramCode.instr_arr)-1;
      while ProgramCode.instr_arr[i].instruction_kind = eeprom_initialization_data do
         i := i - 1;
      last_instr := ProgramCode.instr_arr[i];
      program_memory_used := last_instr.rom_addr + last_instr.size;

      ProgramCode.AssignEEPROMAddresses;
      ProgramCode.AssignConfigByteAddresses;

      ProgramCode.WriteHexFile (ChangeFileExt(source_file_name, '.hex'));
      ProgramCode.WriteAssemblySourceFile (ChangeFileExt(source_file_name, '.asm'));
      OutputRuntimeErrorInfo (ChangeFileExt(source_file_name, '.rterr'));
   end;

INITIALIZATION
   source_file_name := ExtractFilePath(ParamStr(0)) + 'test.cp'

END.
