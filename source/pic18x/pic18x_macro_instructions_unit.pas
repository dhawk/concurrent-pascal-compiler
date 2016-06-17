UNIT pic18x_macro_instructions_unit;

INTERFACE

uses
  pic18x_instructions_unit, cpc_source_analysis_unit,
  Classes;

type

// pseudo-instructions for assembly source

   TAssemblyLabel =
      class (TInstruction)
         constructor Create;
         procedure generate_assembly_code;
            override;
      end;

   TAssemblyLabel_Packed =
      class (TAssemblyLabel)
         constructor Create;
      end;

   TSourceSyncPoint =
      class (TInstruction)
         src_loc: TSourceLocation;
         constructor Create (_src_loc: TSourceLocation);
         procedure generate_assembly_code;
            override;
      end;

   TAssemblySourceBlankLine =
      class (TInstruction)
         procedure generate_assembly_code;
            override;
      end;

   TSourceLine =
      class (TInstruction)
         constructor Create (line: string);
            overload;
         procedure generate_assembly_code;
            override;
      end;

   TAssemblySourceBlankLine_Packed =
      class (TAssemblySourceBlankLine)
         constructor Create;
      end;

   TAssemblyComment =
      class (TInstruction)
         constructor Create (_annotation: string);
         procedure generate_assembly_code;
            override;
      end;

   TAssemblyComment_Packed =
      class (TAssemblyComment)
         constructor Create (_annotation: string);
      end;

   TInlineInstructionDisplay =
      class (TInstruction)
         instr: TInstruction;
         constructor Create (_instr: TInstruction);
         procedure generate_assembly_code;
            override;
      end;

// macros

   TMacroInstruction =
      class (TInstruction)
         instr_arr: array of TInstruction;
         destructor Destroy;
            override;
         procedure set_rom_addr (addr: integer);
            override;
         procedure append (instr: TInstruction);
         procedure replace (_idx: integer; new_instr: TInstruction);
         function instr_idx (rom_addr: integer): integer;
         procedure Clear;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TGOTOMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TCallMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnTOSFalseMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnTOSTrueMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnZeroStatusMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnNonZeroStatusMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnNegativeStatusMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TBranchOnNonNegativeStatusMacro =
      class (TMacroInstruction)
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         procedure recalculate_size (var changed: boolean);
            override;
      end;

   TLoadFMacro =
      class (TMacroInstruction)
         constructor Create (f: integer; a: TPIC18x_RAM_Access_Mode; literal: byte);
      end;

   TMOVLW_ROMAddr_B0 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TMOVLW_ROMAddr_B1 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TMOVLW_ROMAddr_B2 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TADDLW_ROMAddr_B0 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TADDLW_ROMAddr_B1 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TADDLW_ROMAddr_B2 =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPushLabelMacro =
      class (TMacroInstruction)
      private
         procedure set_destination;
      public
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         procedure generate_assembly_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TInstructionArray = array of TInstruction;
   PInstructionArray = ^TInstructionArray;

   TProgramCode =
      class (TMacroInstruction)
         constructor Create;
         destructor Destroy;
            override;
         procedure Clear;
            reintroduce;
         procedure AssignROMAddresses;
         procedure AssignEEPROMAddresses;
         procedure AssignConfigByteAddresses;
         function NumberOfInstructions: integer;
         function ReadByte (rom_addr: integer): byte;
         procedure Execute;
            reintroduce;
         function AssemblyCode (pc: integer): string;
         procedure WriteHexFile (fn: string);
         procedure WriteAssemblySourceFile (fn: string);
         function LastInstruction: TInstruction;
         procedure DoPeepHoleOptimization;
         procedure AppendInstruction (instr: TInstruction);
         procedure UnappendInstruction (instr: TInstruction);
         procedure StartRecordingInlineCode (var _inline_code: TInstructionArray);
         procedure StopRecordingInlineCode;
         procedure AppendInlineCode (var _inline_code: TInstructionArray);
         function Mark: integer;
         procedure RestoreToMark (mark: integer);
      private
         recording_inline_code: boolean;
         inline_code: PInstructionArray;
         DiscardedInstructions: array of TInstruction
      end;

var
   ProgramCode: TProgramCode;


procedure set_absolute (addr, value: integer);

IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
   test_pic18x_simulator_unit,
{$endif}
   pic18x_microprocessor_information_unit,
   System.SysUtils, cpc_target_cpu_unit, pic18x_cpu_unit;

procedure set_absolute (addr, value: integer);
   begin
      if addr <= 256 then
         case value of
            0: TPIC18x_CLRF.Create (addr, bank_mode);
          255: TPIC18x_SETF.Create (addr, bank_mode);
         else
            TPIC18x_MOVLW.Create (value);
            TPIC18x_MOVWF.Create (addr, bank_mode)
         end
      else
         case value of
            0: TPIC18x_CLRF.Create (addr, access_mode);
          255: TPIC18x_SETF.Create (addr, access_mode);
         else
            TPIC18x_MOVLW.Create (value);
            TPIC18x_MOVWF.Create (addr, access_mode)
         end
   end;

constructor TAssemblyLabel.Create;
   begin
      inherited Create;
      size := 0;
      assembler_label_required := true
   end;

procedure TAssemblyLabel.generate_assembly_code;
   begin
      out('')
   end;

constructor TAssemblyLabel_Packed.Create;
   begin
      inherited;
      instruction_kind := rom_constant_data
   end;

constructor TSourceSyncPoint.Create (_src_loc: TSourceLocation);
   begin
      emit_immediately_while_extracting_inline_code := true;
      inherited Create;
      src_loc := _src_loc
   end;

procedure TSourceSyncPoint.generate_assembly_code;
   begin
      sync_source (src_loc)
   end;

procedure TAssemblySourceBlankLine.generate_assembly_code;
   begin
      out ('')
   end;

constructor TSourceLine.Create (line: string);
   begin
      inherited Create;
      annotation := line
   end;

procedure TSourceLine.generate_assembly_code;
   begin
      output_source_line (assembly_source_code, '', annotation)
   end;

constructor TAssemblySourceBlankLine_Packed.Create;
   begin
      inherited Create;
      instruction_kind := rom_constant_data
   end;

constructor TAssemblyComment.Create (_annotation: string);
   begin
      inherited Create;
      annotation := _annotation
   end;

constructor TAssemblyComment_Packed.Create (_annotation: string);
   begin
      inherited Create (_annotation);
      instruction_kind := rom_constant_data
   end;

procedure TAssemblyComment.generate_assembly_code;
   begin
      assembly_source_code.Add ('; ' + annotation)
   end;

constructor TInlineInstructionDisplay.Create (_instr: TInstruction);
   begin
      emit_immediately_while_extracting_inline_code := true;
      inherited Create;
      instr := _instr
   end;

procedure TInlineInstructionDisplay.generate_assembly_code;
   begin
      if not (instr is TMacroInstruction) then
         assembly_source_code.Add (';inl ' + instr.assembly_line)
   end;

//=====================
//  Macro Instructions

destructor TMacroInstruction.Destroy;
   begin
      Clear;
      inherited
   end;

procedure TMacroInstruction.Clear;
   var i: integer;
   begin
      for i := 0 to Length(instr_arr) - 1 do
         instr_arr[i].Release;
      SetLength (instr_arr, 0)
   end;

procedure TMacroInstruction.append (instr: TInstruction);
   var
      i: integer;
   begin
      instr.UnattachFromProgramCode;  // remove from end of ProgramCode
      i := Length(instr_arr);
      SetLength (instr_arr, i+1);
      instr_arr[i] := instr;
      instr.idx := idx    // all sub-instructions have the macro idx
   end;

procedure TMacroInstruction.replace (_idx: integer; new_instr: TInstruction);
   var old_instr: TInstruction;
   begin
      old_instr := instr_arr[_idx];
      instr_arr[_idx] := new_instr;
      new_instr.UnattachFromProgramCode;  // remove from end of ProgramCode
      new_instr.idx := idx;
      new_instr.rom_addr := old_instr.rom_addr;
      new_instr.dest := old_instr.dest;
      old_instr.Release
   end;

procedure TMacroInstruction.set_rom_addr (addr: integer);
   var i: integer;
   begin
      inherited;
      for i := 0 to Length(instr_arr)-1 do
         begin
            instr_arr[i].rom_addr := addr;
            addr := addr + instr_arr[i].size
         end
   end;

function TMacroInstruction.hex_code: THexArray;
   var
      i,j,idx: integer;
      hex: THexArray;
   begin
      for i := 0 to Length(instr_arr)-1 do
         begin
            hex := instr_arr[i].hex_code;
            for j := 0 to Length(hex)-1 do
               begin
                  idx := Length(result);
                  SetLength(result, idx+1);
                  result[idx] := hex[j]
               end;
            SetLength(hex, 0)
         end;
      assert (Length(result) = size)
   end;

function TMacroInstruction.instr_idx (rom_addr: integer): integer;
   begin
      for result := 0 to Length(instr_arr)-1 do
         if (instr_arr[result].rom_addr <= rom_addr) and (rom_addr < (instr_arr[result].rom_addr + instr_arr[result].size)) then
            exit;
      result := -1;  // to suppress compiler warning
      assert (false)  // rom_addr out of range
   end;

function TMacroInstruction.assembly_code: string;
   begin
{$ifdef INCLUDE_SIMULATION}
      result := instr_arr[instr_idx(cpu.pc)].assembly_code
{$endif}
   end;

procedure TMacroInstruction.generate_assembly_code;
   var
      i: integer;
   begin
      assert (Length(instr_arr) > 0);
      instr_arr[0].annotation := annotation;
      for i := 0 to Length(instr_arr)-1 do
         instr_arr[i].generate_assembly_code
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TMacroInstruction.exec;
   begin
      instr_arr[instr_idx(cpu.pc)].execute
   end;
{$endif}

constructor TGOTOMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BRA.Create);
      size := 2
   end;

procedure TGOTOMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_BRA(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_GOTO.Create);
            instr_arr[0].dest := f_dest;
            size := 4;
            changed := true
         end
   end;

procedure TGOTOMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      instr_arr[0].dest := d
   end;

constructor TCallMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_RCALL.Create);
      size := 2;
//      NoteHWStackUsage (hw_stack_call_depth)
   end;

procedure TCallMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_RCALL(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_CALL.Create);
            instr_arr[0].dest := f_dest;
            size := 4;
            changed := true
         end
   end;

procedure TCallMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      instr_arr[0].dest := d
   end;

constructor TBranchOnTOSFalseMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BTFSS.Create(PREINC2, 0, access_mode));
      append (TPIC18x_BRA.Create);
      size := 4
   end;

procedure TBranchOnTOSFalseMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 4)
         and
         (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

procedure TBranchOnTOSFalseMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      instr_arr[1].dest := d
   end;

constructor TBranchOnTOSTrueMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BTFSC.Create(PREINC2, 0, access_mode));
      append (TPIC18x_BRA.Create);
      size := 4
   end;

procedure TBranchOnTOSTrueMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 4)
         and
         (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

procedure TBranchOnTOSTrueMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      instr_arr[1].dest := d
   end;

constructor TBranchOnNegativeStatusMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BN.Create);
      size := 2
   end;

procedure TBranchOnNegativeStatusMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      if size = 2 then
         instr_arr[0].dest := d
      else
         instr_arr[1].dest := d
   end;

procedure TBranchOnNegativeStatusMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_BN(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_BNN.Create);
            append (TPIC18x_BRA.Create);
            append (TAssemblyLabel.Create);
            instr_arr[0].dest := instr_arr[2];
            instr_arr[1].dest := f_dest;
            size := 4;
            changed := true
         end
      else if (size = 4)
           and
           (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

constructor TBranchOnNonNegativeStatusMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BNN.Create);
      size := 2
   end;

procedure TBranchOnNonNegativeStatusMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      if size = 2 then
         instr_arr[0].dest := d
      else
         instr_arr[1].dest := d
   end;

procedure TBranchOnNonNegativeStatusMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_BNN(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_BN.Create);
            append (TPIC18x_BRA.Create);
            append (TAssemblyLabel.Create);
            instr_arr[0].dest := instr_arr[2];
            instr_arr[1].dest := f_dest;
            size := 4;
            changed := true
         end
      else if (size = 4)
           and
           (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

constructor TBranchOnZeroStatusMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BZ.Create);
      size := 2
   end;

procedure TBranchOnZeroStatusMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      if size = 2 then
         instr_arr[0].dest := d
      else
         instr_arr[1].dest := d
   end;

procedure TBranchOnZeroStatusMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_BZ(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_BNZ.Create);
            append (TPIC18x_BRA.Create);
            append (TAssemblyLabel.Create);
            instr_arr[0].dest := instr_arr[2];
            instr_arr[1].dest := f_dest;
            size := 4;
            changed := true
         end
      else if (size = 4)
           and
           (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

constructor TBranchOnNonZeroStatusMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_BNZ.Create);
      size := 2
   end;

procedure TBranchOnNonZeroStatusMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true;
      if size = 2 then
         instr_arr[0].dest := d
      else  // 4 or 6
         instr_arr[1].dest := d
   end;

procedure TBranchOnNonZeroStatusMacro.recalculate_size (var changed: boolean);
   begin
      if (size = 2)
         and
         (not TPIC18x_BZ(instr_arr[0]).in_range) then
         begin
            replace (0, TPIC18x_BZ.Create);
            append (TPIC18x_BRA.Create);
            append (TAssemblyLabel.Create);
            instr_arr[0].dest := instr_arr[2];
            instr_arr[1].dest := f_dest;
            size := 4;
            changed := true
         end
      else if (size = 4)
           and
           (not TPIC18x_BRA(instr_arr[1]).in_range) then
         begin
            replace (1, TPIC18x_GOTO.Create);
            instr_arr[1].dest := f_dest;
            size := 6;
            changed := true
         end
   end;

constructor TLoadFMacro.Create (f: integer; a: TPIC18x_RAM_Access_Mode; literal: byte);
   begin
      inherited Create;
      if literal = 0 then
         begin
            append (TPIC18x_CLRF.Create (f, a));
            size := 2
         end
      else if literal = 255 then
         begin
            append (TPIC18x_SETF.Create (f, a));
            size := 2
         end
      else
         begin
            append (TPIC18x_MOVLW.Create (literal));
            append (TPIC18x_MOVWF.Create (f, a));
            size := 4
         end
   end;

constructor TMOVLW_ROMAddr_B0.Create;
   begin
      inherited Create;
      append (TPIC18x_MOVLW.Create (0));
      size := 2
   end;

procedure TMOVLW_ROMAddr_B0.set_destination;
   begin
      TPIC18x_MOVLW(instr_arr[0]).f := (dest.rom_addr and $ff)
   end;

procedure TMOVLW_ROMAddr_B0.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TMOVLW_ROMAddr_B0.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TMOVLW_ROMAddr_B0.generate_assembly_code;
   begin
      out ('MOVLW LOW(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TMOVLW_ROMAddr_B0.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TMOVLW_ROMAddr_B1.Create;
   begin
      inherited Create;
      append (TPIC18x_MOVLW.Create (0));
      size := 2
   end;

procedure TMOVLW_ROMAddr_B1.set_destination;
   begin
      TPIC18x_MOVLW(instr_arr[0]).f := (dest.rom_addr shr 8) and $ff
   end;

procedure TMOVLW_ROMAddr_B1.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TMOVLW_ROMAddr_B1.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TMOVLW_ROMAddr_B1.generate_assembly_code;
   begin
      out ('MOVLW HIGH(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TMOVLW_ROMAddr_B1.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TMOVLW_ROMAddr_B2.Create;
   begin
      inherited Create;
      append (TPIC18x_MOVLW.Create (0));
      size := 2
   end;

procedure TMOVLW_ROMAddr_B2.set_destination;
   begin
      TPIC18x_MOVLW(instr_arr[0]).f := (dest.rom_addr shr 16) and $ff
   end;

procedure TMOVLW_ROMAddr_B2.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TMOVLW_ROMAddr_B2.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TMOVLW_ROMAddr_B2.generate_assembly_code;
   begin
      out ('MOVLW UPPER(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TMOVLW_ROMAddr_B2.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TADDLW_ROMAddr_B0.Create;
   begin
      inherited Create;
      append (TPIC18x_ADDLW.Create (0));
      size := 2
   end;

procedure TADDLW_ROMAddr_B0.set_destination;
   begin
      TPIC18x_ADDLW(instr_arr[0]).f := (dest.rom_addr and $ff)
   end;

procedure TADDLW_ROMAddr_B0.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TADDLW_ROMAddr_B0.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TADDLW_ROMAddr_B0.generate_assembly_code;
   begin
      out ('ADDLW LOW(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TADDLW_ROMAddr_B0.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TADDLW_ROMAddr_B1.Create;
   begin
      inherited Create;
      append (TPIC18x_ADDLW.Create (0));
      size := 2
   end;

procedure TADDLW_ROMAddr_B1.set_destination;
   begin
      TPIC18x_ADDLW(instr_arr[0]).f := (dest.rom_addr shr 8) and $ff
   end;

procedure TADDLW_ROMAddr_B1.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TADDLW_ROMAddr_B1.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TADDLW_ROMAddr_B1.generate_assembly_code;
   begin
      out ('ADDLW HIGH(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TADDLW_ROMAddr_B1.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TADDLW_ROMAddr_B2.Create;
   begin
      inherited Create;
      append (TPIC18x_ADDLW.Create (0));
      size := 2
   end;

procedure TADDLW_ROMAddr_B2.set_destination;
   begin
      TPIC18x_ADDLW(instr_arr[0]).f := (dest.rom_addr shr 16) and $ff
   end;

procedure TADDLW_ROMAddr_B2.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TADDLW_ROMAddr_B2.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TADDLW_ROMAddr_B2.generate_assembly_code;
   begin
      out ('ADDLW UPPER(' + dest.assembler_label + ')')
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TADDLW_ROMAddr_B2.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}

constructor TPushLabelMacro.Create;
   begin
      inherited Create;
      append (TPIC18x_PUSHL.Create (0));
      append (TPIC18x_PUSHL.Create (0));
      append (TPIC18x_PUSHL.Create (0));
      size := 6
   end;

procedure TPushLabelMacro.set_destination;
   begin
      TPIC18x_PUSHL(instr_arr[0]).k := (dest.rom_addr and $ff);
      TPIC18x_PUSHL(instr_arr[1]).k := (dest.rom_addr shr 8) and $ff;
      TPIC18x_PUSHL(instr_arr[2]).k := dest.rom_addr shr 16
   end;

procedure TPushLabelMacro.set_dest (d: TInstruction);
   begin
      f_dest := d;
      d.assembler_label_required := true
   end;

function TPushLabelMacro.hex_code: THexArray;
   begin
      set_destination;
      inherited
   end;

procedure TPushLabelMacro.generate_assembly_code;
   var
      a: string;
   begin
      out ('PUSHL LOW(' + dest.assembler_label + ')');
      a := annotation;
      annotation := '';
//      assembler_label_required := false;
      out ('PUSHL HIGH(' + dest.assembler_label + ')');
      out ('PUSHL UPPER(' + dest.assembler_label + ')');
      annotation := a;
//      assembler_label_required := true
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPushLabelMacro.exec;
   begin
      set_destination;
      inherited
   end;
{$endif}


//===============
//  TProgramCode

constructor TProgramCode.Create;
   begin
      // do not call inherited Create - it will attempt to link self to ProgramCode.instr_arr
   end;

destructor TProgramCode.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(DiscardedInstructions)-1 do
         DiscardedInstructions[i].Release;
      // do not call inherited Destroy
      Clear
   end;

procedure TProgramCode.DoPeepHoleOptimization;
   var
      i, prev: integer;
   begin
      // remove redundant adjacent push/pop instructions
      i := 1;
      while i < Length(instr_arr) do
         begin
            if (  (instr_arr[i] is TPIC18x_MOVF)
                  and
                  (TPIC18x_MOVF(instr_arr[i]).f = (PREINC2 and $ff))
                  and
                  (TPIC18x_MOVF(instr_arr[i]).d = dest_w)
                  and
                  (TPIC18x_MOVF(instr_arr[i]).a = access_mode)
                  and
                  (not (instr_arr[i].assembler_label_required))
               ) then
               begin
                  prev := i - 1;
                  while (prev >= 0)
                        and
                        (instr_arr[prev] is TAssemblyComment) do
                     prev := prev - 1;

                  if (  (instr_arr[prev] is TPIC18x_MOVWF)
                        and
                        (TPIC18x_MOVWF(instr_arr[prev]).f = (POSTDEC2 and $ff))
                        and
                        (TPIC18x_MOVWF(instr_arr[prev]).a = access_mode)
                        and
                        (not (instr_arr[prev].assembler_label_required))
                     ) then
                     begin
                        instr_arr[i].remove := true;
                        instr_arr[prev].remove := true;
                     end
               end;
            i := i + 1
         end
   end;

procedure TProgramCode.AssignROMAddresses;
   var
      i, addr: integer;
   begin
      addr := first_rom_addr;

      for i := 0 to Length(instr_arr)-1 do
         if instr_arr[i].instruction_kind = rom_constant_data
         then
            begin
               instr_arr[i].rom_addr := addr;
               addr := addr + instr_arr[i].size
            end
         else
            if odd(addr) then
               addr := addr + 1;

      if odd(addr) then
         addr := addr + 1;

      for i := 0 to Length(instr_arr)-1 do
         if instr_arr[i].instruction_kind = executable_instruction then
            begin
               instr_arr[i].rom_addr := addr;
               addr := addr + instr_arr[i].size
            end;
   end;

procedure TProgramCode.AssignEEPROMAddresses;
   var
      i, addr: integer;
   begin
      addr := $F00000;
      for i := 0 to Length(instr_arr) - 1 do
         if instr_arr[i].instruction_kind = eeprom_initialization_data then
            begin
               instr_arr[i].rom_addr := addr;
               addr := addr + instr_arr[i].size
            end
   end;

procedure TProgramCode.AssignConfigByteAddresses;
   var
      i, addr: integer;
   begin
      addr := $300000;
      for i := 0 to Length(instr_arr) - 1 do
         if instr_arr[i].instruction_kind = config_byte_data then
            begin
               instr_arr[i].rom_addr := addr;
               addr := addr + instr_arr[i].size
            end
   end;

procedure TProgramCode.Clear;
   begin
      inherited Clear;
      assembly_source_code.Clear
   end;

procedure TProgramCode.AppendInstruction (instr: TInstruction);
   var i: integer;
   begin
      if (not recording_inline_code)
         or
         (instr.emit_immediately_while_extracting_inline_code)
      then
         begin
            i := Length(instr_arr);
            SetLength (instr_arr, i+1);
            instr_arr[i] := instr;
            instr.idx := i
         end
      else
         begin
            i := Length(inline_code^);
            SetLength (inline_code^, i+1);
            inline_code^[i] := instr;

            TInlineInstructionDisplay.Create (instr);
         end
   end;

procedure TProgramCode.UnappendInstruction (instr: TInstruction);
   var i: integer;
   begin
      if (not recording_inline_code)
         or
         (instr is TAssemblyComment)
      then
         begin
            i := Length(instr_arr)-1;
            assert (instr_arr[i] = instr);
            SetLength (instr_arr, i)
         end
      else
         begin
            i := Length(inline_code^)-1;
            assert (inline_code^[i] = instr);
            SetLength (inline_code^, i)
         end
   end;

procedure TProgramCode.StartRecordingInlineCode (var _inline_code: TInstructionArray);
   begin
      inline_code := @_inline_code;
      recording_inline_code := true
   end;

procedure TProgramCode.StopRecordingInlineCode;
   begin
      recording_inline_code := false
   end;

procedure TProgramCode.AppendInlineCode (var _inline_code: TInstructionArray);
   var
      i: integer;
   begin
      if _inline_code <> nil then
         for i := 0 to Length(_inline_code)-1 do
            AppendInstruction (_inline_code[i]);
      SetLength (_inline_code, 0)
   end;

function TProgramCode.NumberOfInstructions: integer;
   begin
      result := Length(instr_arr)
   end;

function TProgramCode.Mark: integer;
   begin
      result := Length(instr_arr)
   end;

procedure TProgramCode.RestoreToMark (mark: integer);
   var
      i,j: integer;
   begin
      // save discarded instructions - they may be call instructions to a subroutine
      for i := mark to Length(instr_arr)-1 do
         begin
            j := Length(DiscardedInstructions);
            SetLength (DiscardedInstructions, j+1);
            DiscardedInstructions[j] := instr_arr[i]
         end;
      SetLength (instr_arr, mark)
   end;

function TProgramCode.ReadByte (rom_addr: integer): byte;
   var
      idx: integer;
   begin
      idx := instr_idx(rom_addr);
      assert ((instr_arr[idx].rom_addr <= rom_addr) and (rom_addr < (instr_arr[idx].rom_addr + instr_arr[idx].size)));
      result := instr_arr[idx].hex_code[(rom_addr - instr_arr[idx].rom_addr)]
   end;

procedure TProgramCode.Execute;
   begin
{$ifdef INCLUDE_SIMULATION}
      instr_arr[instr_idx(cpu.pc)].execute
{$endif}
   end;

function TProgramCode.AssemblyCode (pc: integer): string;
   begin
      result := instr_arr[instr_idx(pc)].assembly_code
   end;

procedure TProgramCode.WriteHexFile (fn: string);
   var
      f: TextFile;
      i,j: integer;
      chksum: integer;
      instr_hex: THexArray;
      rom_addr, ulba, offset: cardinal;
      current_hex_file_ulba: cardinal;
   procedure write_hex_byte (b: byte);
      begin
         write (f, hex_char[b div 16]);
         write (f, hex_char[b and $f]);
         chksum := chksum + b
      end;
   procedure update_ulba;
      begin
         ulba := rom_addr shr 16;
         offset := rom_addr and $ffff;
         if ulba <> current_hex_file_ulba then
            begin
               chksum := 0;
               write (f, ':');
               write_hex_byte (2);
               write_hex_byte (0);
               write_hex_byte (0);
               write_hex_byte (4);
               write_hex_byte (ulba shr 8);
               write_hex_byte (ulba and $ff);
               write_hex_byte ((-chksum) and $ff);
               writeln (f);
               current_hex_file_ulba := ulba
            end
      end;
   begin
      AssignFile (f, fn);
      Rewrite (f);
      current_hex_file_ulba := 0;
      for i := 0 to Length(instr_arr) - 1 do
         begin
            instr_hex := instr_arr[i].hex_code;
            if Length(instr_hex) > 0 then
               begin
                  rom_addr := instr_arr[i].rom_addr;
                  while Length(instr_hex) >= 255 do
                     begin
                        update_ulba;
                        chksum := 0;
                        write (f, ':');
                        write_hex_byte (255);
                        write_hex_byte (offset shr 8);
                        write_hex_byte (offset and $ff);
                        write_hex_byte (0);  // data tag
                        for j := 0 to 254 do
                           write_hex_byte(instr_hex[j]);
                        write_hex_byte ((-chksum) and $ff);
                        writeln (f);
                        rom_addr := rom_addr + 255;
                        instr_hex := Copy (instr_hex, 255, maxint)
                     end;
                  update_ulba;
                  chksum := 0;
                  write (f, ':');
                  write_hex_byte (Length(instr_hex));
                  write_hex_byte (offset shr 8);
                  write_hex_byte (offset and $ff);
                  write_hex_byte (0);  // data tag
                  for j := 0 to Length(instr_hex) - 1 do
                     write_hex_byte(instr_hex[j]);
                  write_hex_byte ((-chksum) and $ff);
                  writeln (f);
                  SetLength (instr_hex, 0)
               end
         end;
      writeln (f, ':00000001FF');
      CloseFile (f)
   end;

procedure TProgramCode.WriteAssemblySourceFile (fn: string);
   var
      f: TextFile;
      i, j, label_number: integer;
      last_instruction_kind: TPIC18x_Instruction_Kind;
   begin
      label_number := 1;
      for i := 0 to Length(instr_arr)-1 do
         if instr_arr[i] is TMacroInstruction then
            begin
               if TMacroInstruction(instr_arr[i]).assembler_label_required then
                  begin
                     TMacroInstruction(instr_arr[i]).instr_arr[0].assembler_label_required := true;
                     TMacroInstruction(instr_arr[i]).assembler_label_number := label_number
                  end;
               with TMacroInstruction(instr_arr[i]) do
                  for j := 0 to Length(instr_arr)-1 do
                     if instr_arr[j].assembler_label_required then
                        begin
                           instr_arr[j].assembler_label_number := label_number;
                           label_number := label_number + 1
                        end
            end
         else
            begin
               if instr_arr[i].assembler_label_required then
                  begin
                     instr_arr[i].assembler_label_number := label_number;
                     label_number := label_number + 1
                  end
            end;

      current_source_idx := -1;
      if pic_info.microprocessor = upspecified_microprocessor then
         assembly_source_code.Add ('#INCLUDE P18F2520.INC')
      else
         assembly_source_code.Add ('#INCLUDE P' + UpperCase(copy(pic_info.microprocessor, 4, 999)) + '.INC');

      // config bytes must be first in source after above INCLUDE (MPASM rule)
      i := 0;
      if instr_arr[0].instruction_kind = config_byte_data then
         begin
            assembly_source_code.Add ('');
            assembly_source_code.Add ('; ====================');
            assembly_source_code.Add (';  Configuration Bits ');
            assembly_source_code.Add ('; ====================');
            assembly_source_code.Add ('');
            repeat
               if TPIC18x_CONFIG(instr_arr[i]).IncludeInSourceFile then
                  instr_arr[i].generate_assembly_code;
               i := i + 1
            until instr_arr[i].instruction_kind <> config_byte_data
         end;
      last_instruction_kind := config_byte_data;

      label_number := 1;
      for i := i to Length(instr_arr)-1 do
         begin
            if last_instruction_kind <> instr_arr[i].instruction_kind then
               begin
                  TInstruction.blank_line;
                  case instr_arr[i].instruction_kind of
                      executable_instruction,
                      reset_vector_instruction,
                      hi_pri_interrupt_vector_instruction,
                      kernel_instruction:
                         TInstruction.output_assembly_with_label ('R', label_number, 'CODE ' + rom_addr_to_hex_string(instr_arr[i].rom_addr));
                      rom_constant_data:
                         TInstruction.output_assembly_with_label ('R', label_number, 'CODE_PACK ' + rom_addr_to_hex_string(instr_arr[i].rom_addr));
                      eeprom_initialization_data:
                         TInstruction.output_assembly_with_label ('E', 1, 'CODE_PACK 0xF00000');
                      config_byte_data:
                         assert (false);
                  else
                     assert (false)
                  end;
                  label_number := label_number + 1;
                  TInstruction.blank_line
               end;
            instr_arr[i].generate_assembly_code;
            last_instruction_kind := instr_arr[i].instruction_kind
         end;

      blank_line;
      append_remaining_source (assembly_source_code);
      blank_line;

      output_assembly ('END');

      AssignFile (f, fn);
      Rewrite (f);
      for i := 0 to assembly_source_code.Count - 1 do
         writeln (f, assembly_source_code[i]);
      CloseFile (f)
   end;

function TProgramCode.LastInstruction: TInstruction;
   begin
      result := instr_arr[Length(instr_arr)-1]
   end;


INITIALIZATION
   ProgramCode := TProgramCode.Create;


FINALIZATION
   ProgramCode.Free;   // use Free instead of Release here since constructor did not call TDefinition.Create

END.
