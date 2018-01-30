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

const
   err_program_memory_exceeded = 'PIC18x program memory exceeded (%d bytes over the %d available)';

IMPLEMENTATION

uses
   cpc_target_cpu_unit,
   pic18x_blocks_unit,
   pic18x_cpu_unit,
   pic18x_instructions_unit,
   pic18x_macro_instructions_unit,
   pic18x_run_time_error_check_unit,
   pic18x_microprocessor_information_unit,
   SysUtils, cpc_common_unit, cpc_blocks_unit, cpc_source_analysis_unit;

function ProgramGenerator: TDefinition;
   var
      i: integer;
      changed: boolean;
      last_instr: TInstruction;
      err_msg: string;
      end_src_loc: TSourceLocation;
   begin
      DeleteFile (ChangeFileExt(source_file_name, '.hex'));
      DeleteFile (ChangeFileExt(source_file_name, '.asm'));
      DeleteFile (ChangeFileExt(source_file_name, '.rterr'));

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

      if program_memory_used > pic_info.available_program_memory then
         begin
            err_msg := format (err_program_memory_exceeded, [program_memory_used - pic_info.available_program_memory, pic_info.available_program_memory]);
            end_src_loc := TProgram(result).end_src_loc;
            result.Free;
            raise compile_error.Create (err_msg, end_src_loc);
         end;

      ProgramCode.AssignEEPROMAddresses;
      ProgramCode.AssignConfigByteAddresses;

      ProgramCode.WriteHexFile (ChangeFileExt(source_file_name, '.hex'));
      ProgramCode.WriteAssemblySourceFile (ChangeFileExt(source_file_name, '.asm'));
      OutputRuntimeErrorInfo (ChangeFileExt(source_file_name, '.rterr'));
      OutputLinkerFile (ChangeFileExt(source_file_name, '.lkr'))
   end;


END.
