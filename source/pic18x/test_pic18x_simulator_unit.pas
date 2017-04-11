UNIT test_pic18x_simulator_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   cpc_common_unit,
   pic18x_instructions_unit,
   pic18x_microprocessor_information_unit;

type
   TFSR =
      class (TReferenceCountedObject)
      private
         f_value: byte;
         function get_value: byte;
            virtual;
         procedure set_value (b: byte);
            virtual;
      public
         name: string;
         property value: byte read get_value write set_value;
         constructor Create;
            overload;
         constructor Create (_name: string);
            overload;
      end;

   TPCL_FSR =
      class (TFSR)
         procedure set_value (b: byte);
            override;
         function get_value: byte;
            override;
      end;

   TIndirect_FSR_Kind = (indirect, postdec, postinc, preinc, plusw);
   TGetFSRFunction = function: TDataMemoryAddress of object;
   TSetFSRProcedure = procedure (v: TDataMemoryAddress) of object;
   TIndirect_FSR =
      class (TFSR)
         kind: TIndirect_FSR_Kind;
         fsr_getfunc: TGetFSRFunction;
         fsr_setproc: TSetFSRProcedure;
         constructor Create (_name: string; _kind: TIndirect_FSR_Kind; _getfunc: TGetFSRFunction; _setproc: TSetFSRProcedure);
         procedure set_value (b: byte);
            override;
         function get_value: byte;
            override;
      end;

   TEECON1 =
      class (TFSR)
         procedure set_value (b: byte);
            override;
         function get_value: byte;
            override;
      end;

   TPIC18x_Simulated_CPU =
      class (TReferenceCountedObject)
      private
         f_ram: array [0..$FFF] of TFSR;
         f_refocon_ram: TFSR;
         f_pc: integer;
         stk_u, stk_h, stk_l: array [0..31] of byte;
         function get_FSR (idx: TDataMemoryAddress): byte;
         procedure set_FSR (idx: TDataMemoryAddress; value: byte);
         function get_w: byte;
         procedure set_w (value: byte);
         function get_pc: integer;
         procedure set_pc (value: integer);
         function get_n: boolean;
         function get_ov: boolean;
         function get_c: boolean;
         function get_dc: boolean;
         function get_z: boolean;
         procedure set_n (b: boolean);
         procedure set_ov (b: boolean);
         procedure set_c (b: boolean);
         procedure set_dc (b: boolean);
         procedure set_z (b: boolean);
         function get_fsr0: TDataMemoryAddress;
         function get_fsr1: TDataMemoryAddress;
         function get_fsr2: TDataMemoryAddress;
         procedure set_fsr0 (v: TDataMemoryAddress);
         procedure set_fsr1 (v: TDataMemoryAddress);
         procedure set_fsr2 (v: TDataMemoryAddress);
         function get_tblptr: TROMAddress;
         procedure set_tblptr (p: TROMAddress);
     public
         running: boolean;
         eeprom: array [0..255] of byte;
         property ram [i: TDataMemoryAddress]: byte read get_FSR write set_FSR;
         property w: byte read get_w write set_w;
         property pc: integer read get_pc write set_pc;
         property n: boolean read get_n write set_n;
         property ov: boolean read get_ov write set_ov;
         property c: boolean read get_c write set_c;
         property dc: boolean read get_dc write set_dc;
         property z: boolean read get_z write set_z;
         property fsr0: TDataMemoryAddress read get_fsr0 write set_fsr0;
         property fsr1: TDataMemoryAddress read get_fsr1 write set_fsr1;
         property fsr2: TDataMemoryAddress read get_fsr2 write set_fsr2;
         property tblptr: TROMAddress read get_tblptr write set_tblptr;
         constructor Create;
         destructor Destroy;
            override;
         function refocon: byte;
         procedure push_return_address (return_addr: integer);
         function pop_return_address: integer;
         function ram_no_adshr_check (i: TDataMemoryAddress): byte;
         procedure allocate_special_sfrs;
         procedure deallocate_special_sfrs;
         procedure Reset;
      end;

var
   CPU: TPIC18x_Simulated_CPU;

IMPLEMENTATION

uses
   cpc_target_cpu_unit,
   pic18x_cpu_unit,
   pic18x_macro_instructions_unit,
   SysUtils,
   test_pic18x_subroutines_unit;


constructor TFSR.Create;
   begin
      inherited Create
   end;

constructor TFSR.Create (_name: string);
   begin
      inherited Create;
      name := _name
   end;

function TFSR.get_value: byte;
   begin
      result := f_value
   end;

procedure TFSR.set_value (b: byte);
   begin
      f_value := b;
   end;

procedure TPCL_FSR.set_value (b: byte);
   begin
      inherited set_value(b);
      cpu.pc := (cpu.ram[PCLATU] shl 16) + (cpu.ram[PCLATH] shl 8) + b
   end;

function TPCL_FSR.get_value: byte;
   begin
      cpu.ram[PCLATU] := usb(cpu.pc);
      cpu.ram[PCLATH] := msb(cpu.pc);
      result := cpu.pc and $ff
   end;

constructor TIndirect_FSR.Create (_name: string; _kind: TIndirect_FSR_Kind; _getfunc: TGetFSRFunction; _setproc: TSetFSRProcedure);
   begin
      inherited Create (_name);
      kind := _kind;
      fsr_getfunc := _getfunc;
      fsr_setproc := _setproc
   end;

procedure TIndirect_FSR.set_value (b: byte);
   begin
      if kind = plusw then
         cpu.ram[(integer(fsr_getfunc) + ShortInt(cpu.w)) and $FFF] := b
      else
         cpu.ram[integer(fsr_getfunc) and $FFF] := b
   end;

function TIndirect_FSR.get_value: byte;
   begin
      if kind = plusw then
         result := cpu.ram[(integer(fsr_getfunc) + ShortInt(cpu.w)) and $FFF]
      else
         result := cpu.ram[integer(fsr_getfunc) and $FFF]
   end;

procedure TEECON1.set_value (b: byte);
   begin
      if b and $02 = $02 then  // write
         CPU.eeprom[CPU.f_ram[pic_info.EEADR].value] := CPU.f_ram[pic_info.EEDATA].value;
      if b and $01 = $01 then  // read
         CPU.f_ram[pic_info.EEDATA].value := CPU.eeprom[CPU.f_ram[pic_info.EEADR].value]
   end;

function TEECON1.get_value: byte;
   begin
      result := 0
   end;

function TPIC18x_Simulated_CPU.ram_no_adshr_check (i: TDataMemoryAddress): byte;
   begin
      result := f_ram[i].get_value
   end;

function TPIC18x_Simulated_CPU.get_FSR (idx: TDataMemoryAddress): byte;
   begin
      if (pic_info.REFOCON > 0)
         and
         (idx = (pic_info.REFOCON and $FFF))
         and
         (pic_info.WDTCON > 0)
         and
         (ram[pic_info.WDTCON] and $10 = $10)
      then
         result := f_refocon_ram.get_value
      else
         result := f_ram[idx].get_value
   end;

procedure TPIC18x_Simulated_CPU.set_FSR (idx: TDataMemoryAddress; value: byte);
   begin
      if (pic_info.REFOCON > 0)
         and
         (idx = (pic_info.REFOCON and $FFF))
         and
         (pic_info.WDTCON > 0)
         and
         (ram[pic_info.WDTCON] and $10 = $10)
      then
         f_refocon_ram.set_value (value)
      else
         f_ram[idx].set_value (value)
   end;

function TPIC18x_Simulated_CPU.get_pc: integer;
   begin
      result := f_pc
   end;

procedure TPIC18x_Simulated_CPU.set_pc (value: integer);
   begin
      assert (not odd (value));
      f_pc := value
   end;

function TPIC18x_Simulated_CPU.get_w: byte;
   begin
      result := ram[WREG]
   end;

procedure TPIC18x_Simulated_CPU.set_w (value: byte);
   begin
      ram[WREG] := value
   end;

function TPIC18x_Simulated_CPU.get_n: boolean;
   begin
      result := ((ram[STATUS] and status_n_bit_mask) = status_n_bit_mask)
   end;

function TPIC18x_Simulated_CPU.get_ov: boolean;
   begin
      result := ((ram[STATUS] and status_ov_bit_mask) = status_ov_bit_mask)
   end;

function TPIC18x_Simulated_CPU.get_c: boolean;
   begin
      result := ((ram[STATUS] and status_c_bit_mask) = status_c_bit_mask)
   end;

function TPIC18x_Simulated_CPU.get_dc: boolean;
   begin
      result := ((ram[STATUS] and status_dc_bit_mask) = status_dc_bit_mask)
   end;

function TPIC18x_Simulated_CPU.get_z: boolean;
   begin
      result := ((ram[STATUS] and status_z_bit_mask) = status_z_bit_mask)
   end;

procedure TPIC18x_Simulated_CPU.set_n (b: boolean);
   begin
      if b then
         ram[STATUS] := ram[STATUS] or status_n_bit_mask
      else
         ram[STATUS] := ram[STATUS] and (not status_n_bit_mask)
   end;

procedure TPIC18x_Simulated_CPU.set_ov (b: boolean);
   begin
      if b then
         ram[STATUS] := ram[STATUS] or status_ov_bit_mask
      else
         ram[STATUS] := ram[STATUS] and (not status_ov_bit_mask)

   end;

procedure TPIC18x_Simulated_CPU.set_c (b: boolean);
   begin
      if b then
         ram[STATUS] := ram[STATUS] or status_c_bit_mask
      else
         ram[STATUS] := ram[STATUS] and (not status_c_bit_mask)

   end;

procedure TPIC18x_Simulated_CPU.set_dc (b: boolean);
   begin
      if b then
         ram[STATUS] := ram[STATUS] or status_dc_bit_mask
      else
         ram[STATUS] := ram[STATUS] and (not status_dc_bit_mask)

   end;

procedure TPIC18x_Simulated_CPU.set_z (b: boolean);
   begin
      if b then
         ram[STATUS] := ram[STATUS] or status_z_bit_mask
      else
         ram[STATUS] := ram[STATUS] and (not status_z_bit_mask)

   end;

constructor TPIC18x_Simulated_CPU.Create;
   var
      i: integer;
   begin
      inherited Create;
      for i := 0 to  $7FF do
         f_ram[i] := TFSR.Create;
      f_ram[BSR] := TFSR.Create('BSR');
      f_ram[FSR0L] := TFSR.Create ('FSR0L');
      f_ram[FSR0H] := TFSR.Create ('FSR0H');
      f_ram[FSR1L] := TFSR.Create ('FSR1L');
      f_ram[FSR1H] := TFSR.Create ('FSR1H');
      f_ram[FSR2L] := TFSR.Create ('FSR2L');
      f_ram[FSR2H] := TFSR.Create ('FSR2H');
      f_ram[INTCON] := TFSR.Create ('INTCON');
      f_ram[PCL] := TPCL_FSR.Create ('PCL');
      f_ram[PCLATH] := TFSR.Create ('PCLATH');
      f_ram[PCLATU] := TFSR.Create ('PCLATU');
      f_ram[PRODH] := TFSR.Create('PRODH');
      f_ram[PRODL] := TFSR.Create('PRODL');
      if pic_info.RCON > 0 then
         f_ram[pic_info.RCON] := TFSR.Create('RCON');
      f_ram[STATUS] := TFSR.Create('STATUS');
      f_ram[STKPTR] := TFSR.Create('STKPTR');
      f_ram[TABLAT] := TFSR.Create ('TABLAT');
      f_ram[TBLPTRL] := TFSR.Create('TBLPTRL');
      f_ram[TBLPTRH] := TFSR.Create('TBLPTRH');
      f_ram[TBLPTRU] := TFSR.Create('TBLPTRU');
      f_ram[TOSU] := TFSR.Create('TOSU');
      f_ram[TOSH] := TFSR.Create('TOSH');
      f_ram[TOSL] := TFSR.Create('TOSL');
      f_ram[WREG] := TFSR.Create('WREG');

      f_ram[INDF0] := TIndirect_FSR.Create ('INDF0', indirect, get_fsr0, set_fsr0);
      f_ram[POSTDEC0] := TIndirect_FSR.Create ('POSTDEC0', postdec, get_fsr0, set_fsr0);
      f_ram[POSTINC0] := TIndirect_FSR.Create ('POSTINC0', postinc, get_fsr0, set_fsr0);
      f_ram[PREINC0] := TIndirect_FSR.Create ('PREINC0', preinc, get_fsr0, set_fsr0);
      f_ram[PLUSW0] := TIndirect_FSR.Create ('PLUSW0', plusw, get_fsr0, set_fsr0);

      f_ram[INDF1] := TIndirect_FSR.Create ('INDF1', indirect, get_fsr1, set_fsr1);
      f_ram[POSTDEC1] := TIndirect_FSR.Create ('POSTDEC1', postdec, get_fsr1, set_fsr1);
      f_ram[POSTINC1] := TIndirect_FSR.Create ('POSTINC1', postinc, get_fsr1, set_fsr1);
      f_ram[PREINC1] := TIndirect_FSR.Create ('PREINC1', preinc, get_fsr1, set_fsr1);
      f_ram[PLUSW1] := TIndirect_FSR.Create ('PLUSW1', plusw, get_fsr1, set_fsr1);

      f_ram[INDF2] := TIndirect_FSR.Create ('INDF2', indirect, get_fsr2, set_fsr2);
      f_ram[POSTDEC2] := TIndirect_FSR.Create ('POSTDEC2', postdec, get_fsr2, set_fsr2);
      f_ram[POSTINC2] := TIndirect_FSR.Create ('POSTINC2', postinc, get_fsr2, set_fsr2);
      f_ram[PREINC2] := TIndirect_FSR.Create ('PREINC2', preinc, get_fsr2, set_fsr2);
      f_ram[PLUSW2] := TIndirect_FSR.Create ('PLUSW2', plusw, get_fsr2, set_fsr2)
   end;

procedure TPIC18x_Simulated_CPU.allocate_special_sfrs;
   begin
      if pic_info.available_eeprom_memory > 0 then
         begin
            f_ram[pic_info.EEDATA] := TFSR.Create ('EEDATA');
            f_ram[pic_info.EEADR] := TFSR.Create ('EEADR');
            f_ram[pic_info.EECON1] := TEECON1.Create ('EECON1');
            f_ram[pic_info.EECON2] := TFSR.Create ('EECON2');
            if pic_info.available_eeprom_memory > 256 then
               f_ram [pic_info.EEADRH] := TFSR.Create ('EEADRH');
         end;
      if pic_info.TMR3L > 0 then
         begin
            f_ram[pic_info.TMR3L] := TFSR.Create ('TMR3L');
            f_ram[pic_info.TMR3H] := TFSR.Create ('TMR3H');
            f_ram[pic_info.T3CON] := TFSR.Create ('T3CON')
         end;
      if pic_info.UFRML > 0 then
         begin
            f_ram[pic_info.UFRML] := TFSR.Create ('UFRML');
            f_ram[pic_info.UFRMH] := TFSR.Create ('UFRMH')
         end;
      if pic_info.OSCCON > 0 then
         f_ram[pic_info.OSCCON] := TFSR.Create ('OSCCON');
      if pic_info.WDTCON > 0 then
         f_ram[pic_info.WDTCON] := TFSR.Create ('WDTCON');
      if pic_info.REFOCON > 0 then
         f_refocon_ram := TFSR.Create ('REFOCON')
   end;

procedure TPIC18x_Simulated_CPU.deallocate_special_sfrs;
   begin
      if pic_info.available_eeprom_memory > 0 then
         begin
            f_ram[pic_info.EEDATA].Free;
            f_ram[pic_info.EEADR].Free;
            f_ram[pic_info.EECON1].Free;
            f_ram[pic_info.EECON2].Free;
            f_ram[pic_info.EEDATA] := nil;
            f_ram[pic_info.EEADR] := nil;
            f_ram[pic_info.EECON1] := nil;
            f_ram[pic_info.EECON2] := nil;
            if pic_info.available_eeprom_memory > 256 then
               begin
                  f_ram [pic_info.EEADRH].Free;
                  f_ram [pic_info.EEADRH] := nil
               end
         end;
      pic_info.EEADR    := -1;
      pic_info.EEADRH   := -1;
      pic_info.EECON1   := -1;
      pic_info.EECON2   := -1;
      pic_info.EEDATA   := -1;

      if pic_info.TMR3L > 0 then
         begin
            f_ram[pic_info.TMR3L].Free;
            f_ram[pic_info.TMR3H].Free;
            f_ram[pic_info.T3CON].Free;
            f_ram[pic_info.TMR3L] := nil;
            f_ram[pic_info.TMR3H] := nil;
            f_ram[pic_info.T3CON] := nil
         end;
      pic_info.TMR3L := -1;
      pic_info.TMR3H := -1;
      pic_info.T3CON := -1;

      if pic_info.UFRML > 0 then
         begin
            f_ram[pic_info.UFRML].Free;
            f_ram[pic_info.UFRMH].Free;
            f_ram[pic_info.UFRML] := nil;
            f_ram[pic_info.UFRMH] := nil
         end;
      pic_info.UFRML := -1;
      pic_info.UFRMH := -1;
      if pic_info.WDTCON > 0 then
         begin
            f_ram[pic_info.WDTCON].Free;
            f_ram[pic_info.WDTCON] := nil
         end;
      pic_info.WDTCON := -1;
      if pic_info.OSCCON > 0 then
         begin
            f_ram[pic_info.OSCCON].Free;
            f_ram[pic_info.OSCCON] := nil
         end;
      pic_info.OSCCON := -1;
      if pic_info.REFOCON > 0 then
         begin
            f_refocon_ram.Free;
            f_refocon_ram := nil
         end;
      pic_info.REFOCON := -1
   end;

destructor TPIC18x_Simulated_CPU.Destroy;
   var
      i: integer;
   begin
      for i := 0 to max_ram_addr do
         f_ram[i].Release;
      inherited
   end;

function TPIC18x_Simulated_CPU.get_fsr0: TDataMemoryAddress;
   begin
      result := ((ram[FSR0H] and $F) shl 8) + ram[FSR0L]
   end;

function TPIC18x_Simulated_CPU.get_fsr1: TDataMemoryAddress;
   begin
      result := ((ram[FSR1H] and $F) shl 8) + ram[FSR1L]
   end;

function TPIC18x_Simulated_CPU.get_fsr2: TDataMemoryAddress;
   begin
      result := ((ram[FSR2H] and $F) shl 8) + ram[FSR2L]
   end;

procedure TPIC18x_Simulated_CPU.set_fsr0 (v: TDataMemoryAddress);
   begin
      ram[FSR0H] := v shr 8;
      ram[FSR0L] := v and $ff
   end;

procedure TPIC18x_Simulated_CPU.set_fsr1 (v: TDataMemoryAddress);
   begin
      ram[FSR1H] := v shr 8;
      ram[FSR1L] := v and $ff
   end;

procedure TPIC18x_Simulated_CPU.set_fsr2 (v: TDataMemoryAddress);
   begin
      ram[FSR2H] := v shr 8;
      ram[FSR2L] := v and $ff
   end;

function TPIC18x_Simulated_CPU.refocon: byte;
   begin
      result := f_refocon_ram.get_value
   end;

procedure TPIC18x_Simulated_CPU.push_return_address (return_addr: integer);
   begin
      stk_u[ram[STKPTR]] := ram[TOSU];
      stk_h[ram[STKPTR]] := ram[TOSH];
      stk_l[ram[STKPTR]] := ram[TOSL];
      ram[STKPTR] := ram[STKPTR] + 1;
      ram[TOSU] := return_addr shr 16;
      ram[TOSH] := (return_addr shr 8) and $ff;
      ram[TOSL] := return_addr and $ff
   end;

function TPIC18x_Simulated_CPU.pop_return_address: integer;
   begin
      result := (ram[TOSU] shl 16) + (ram[TOSH] shl 8) + ram[TOSL];
      ram[STKPTR] := ram[STKPTR] - 1;
      ram[TOSU] := stk_u[ram[STKPTR]];
      ram[TOSH] := stk_h[ram[STKPTR]];
      ram[TOSL] := stk_l[ram[STKPTR]]
   end;

procedure TPIC18x_Simulated_CPU.Reset;
   var
      i,j: integer;
   begin
      pc := 0;
      ram[STKPTR] := 0;
      ram[TOSU] := 0;
      ram[TOSH] := 0;
      ram[TOSL] := 0;
      ram[FSR0H] := 0;
      ram[FSR1H] := 0;
      ram[FSR2H] := 0;
      for i := 0 to 255 do
         eeprom[i] := $55;
      for i := 0 to Length(ProgramCode.instr_arr)-1 do
         if ProgramCode.instr_arr[i].instruction_kind = eeprom_initialization_data then
            for j := 0 to Length(ProgramCode.instr_arr[i].hex_code)-1 do
               eeprom[(ProgramCode.instr_arr[i].rom_addr and $ff) + j] := ProgramCode.instr_arr[i].hex_code[j]
   end;

function TPIC18x_Simulated_CPU.get_tblptr: TROMAddress;
   begin
      result := (ram[TBLPTRU] shl 16) + (ram[TBLPTRH] shl 8) + (ram[TBLPTRL])
   end;

procedure TPIC18x_Simulated_CPU.set_tblptr (p: TROMAddress);
   begin
      ram[TBLPTRU] := p shr 16;
      ram[TBLPTRH] := (p shr 8) and $ff;
      ram[TBLPTRL] := p and $ff
   end;


INITIALIZATION
   CPU := TPIC18x_Simulated_CPU.Create;

FINALIZATION
   CPU.Release

END.
