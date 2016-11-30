UNIT pic18x_kernel_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_expressions_unit, cpc_statements_unit, cpc_core_objects_unit, pic18x_microprocessor_information_unit,
   pic18x_core_objects_unit, pic18x_instructions_unit, cpc_blocks_unit,
   pic18x_blocks_unit, Classes, pic18x_cpu_unit;

const
   initial_process_stk_init = ord('S');
   process_stk_init_value = ord('s');
   systyp_param_init = ord ('p');

const
   high_priority_interrupt = 2;
   low_priority_interrupt = 1;
   initial_process_priority = -7;
type
   TPriorityRange = initial_process_priority .. high_priority_interrupt;
   TInterruptPriorityRange = low_priority_interrupt .. high_priority_interrupt;

const
   queue_variable_size = 1;
   kernel_interrupt_handler_stack_allowance = 2;

// kernel variables
const
   class_eeprom_base_addr = 0;

   monitor_eeprom_base_addr = 0;

   process_eeprom_base_addr = 0;

type
   TPIC18x_AwaitInterruptStatement =
      class (TAwaitInterruptStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_ContinueStatement =
      class (TContinueStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_DelayStatement =
      class (TDelayStatement)
         function Generate (param1, param2: integer): integer;
            override;
      end;
   TPIC18x_EmptyFunctionPrimary =
      class (TEmptyFunctionPrimary)
         function Generate (param1, param2: integer): integer;
            override;
      end;

type
   TInitProcessSubroutine =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      end;

   TPriorityInfo =
      record
         prio: integer;        // value in source
         ready_addr: integer;  // encoded value
      end;
   TPriorityMapper =
      record
         priorities: array of TPriorityInfo;
         number_of_processes: integer;
         procedure Init;
         procedure NotePriorityLevelUse (prio: TPriorityRange);
         function PrioUsed (prio: integer): boolean;
         function ReadyQueueAddr (prio: TPriorityRange): integer;
         function AnyHiPriorityInterruptProcesses: boolean;
         function AnyLowPriorityInterruptProcesses: boolean;
         function Interruptable (prio: TPriorityRange): boolean;
      end;

   TGetEEPROMByte =
      // in: addr in FSR1L
      // out: data in W
      //      FSR1--
      class (TSubroutine)
{$ifdef INCLUDE_SIMULATION}
      protected
         procedure report_stack_sizes;
            override;
{$endif}
      public
         const
            push_count     = 1;
            pop_count      = 1;
            hw_stack_usage = 1;
         constructor Create;
         procedure generate_subroutine_code;
            override;
      end;

   TSetEEPROMByte =
      // in: data in W
      //     addr in FSR0L
      // out: FSR0++
      class (TSubroutine)
{$ifdef INCLUDE_SIMULATION}
      protected
         procedure report_stack_sizes;
            override;
{$endif}
      public
         const
            push_count     = 1;
            pop_count      = 1;
            hw_stack_usage = 1;
         constructor Create;
         procedure generate_subroutine_code;
            override;
      end;


// process control block layout
const
   pcb_next_offset      = 0;
   pcb_stk_saveH_offset = 1;
   pcb_stk_saveL_offset = 2;
   pcb_nesting_offset   = 3;
   pcb_size             = 4;

// RAM kernel variables
var
   next_available_absolute_address,
   error_logU,
   error_logH,
   error_logL,
   this_ptrH,
   this_ptrL,
   current_prio,
   kernel_exit_flags,
   kernel_temp1,
   kernel_temp2,
   initial_process_pcb_addr: integer;

// bit assignments for kernel_exit_flags byte
const
   kef_system_initializing_bit = 7;   // bit 7 is assumed in $80 value in init_kernel
   kef_running_at_high_priority_bit = 6;

var
   reset_vector_goto,
   hi_priority_interrupt_vector_goto,
   low_priority_interrupt_vector_goto: TInstruction;
   PriorityMapper: TPriorityMapper;
   kernel_stack_addr: integer;
   kernel_stack_size: integer;
   kernel_stack_base: integer;
{$ifdef INCLUDE_SIMULATION}
   no_sleep_on_idle: boolean;
{$endif}

procedure allocate_hw_stack (prog: TPIC18x_Program);
procedure init_kernel (proc: TByteParamProcedureOfObject);
procedure init_class_control_block (class_var: TVariable; proc: TByteParamProcedureOfObject);
procedure init_monitor_control_block (monitor_var: TVariable; proc: TByteParamProcedureOfObject);
procedure init_process_control_block (process_var: TVariable; proc: TByteParamProcedureOfObject);
procedure init_initial_process (initial_statement_stack_size: integer; proc: TByteParamProcedureOfObject);
procedure AssignKernelVariableAddresses (prog: TProgram);
procedure GenerateKernel (prog: TProgram);
procedure GenerateKernelStartupCode;
procedure EnterMonitor (monitor_prio: integer);
procedure LeaveMonitor (monitor_prio, pop_size: integer);

function ExitKernel: TInstruction;
function TurnInterruptsOn: TInstruction;
function TurnInterruptsOff: TInstruction;

var
   InitProcessSubroutine: TInitProcessSubroutine;
   CriticalAddress: TInstruction;
   kernel_dispatch: TBranchTarget;
   kernel_await_interrupt: TBranchTarget;
   kernel_entermon: TBranchTarget;
   kernel_leavemon: TBranchTarget;
   kernel_delay: TBranchTarget;
   kernel_continue: TBranchTarget;
   kernel_idle: TBranchTarget;
   GetEEPROMByte: TGetEEPROMByte;
   SetEEPROMByte: TSetEEPROMByte;

type
   TProcessStateSFRs =
      (psFSR1H,
       psFSR1L,
       psPCLATU,
       psPCLATH,
       psPRODH,
       psPRODL,
       psTABLAT,
       psTBLPTRH,
       psTBLPTRL,
       psthis_ptrH,
       psthis_ptrL,
       psTOSU,
       psTOSH,
       psTOSL,
       psWREG
      );
const
   ProcessStateAreaSize = ord(High(TProcessStateSFRs)) + 1;
var
   ProcessStateSFRAddress:
      array [TProcessStateSFRs] of
         record
            addr: TDataMemoryAddress;
            name: string
         end;

IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
   test_pic18x_simulator_unit,
   test_pic18x_subroutines_unit,
   test_pic18x_kernel_unit,
{$endif}
   SysUtils, pic18x_access_unit,
   pic18x_macro_instructions_unit, cpc_source_analysis_unit,
   pic18x_run_time_error_check_unit, cpc_target_cpu_unit;

const
   ready_state_offset = 1;
   ready_state_running_bit = 7;
   ready_state_suspended_bit = 6;

function ExitKernel: TInstruction;
   begin
      result := TPIC18x_TSTFSZ.Create (kernel_exit_flags, bank_mode);
      TPIC18x_RETURN.Create.annotation := 'return and leave interrupts off';
      TPIC18x_RETFIE.Create.annotation := 'return and enable interrupts'
   end;

function TurnInterruptsOn: TInstruction;
   begin
      result := TPIC18x_BSF.Create (INTCON, intcon_gieh, access_mode);
      result.annotation := 'turn interrupts on'
   end;

function TurnInterruptsOff: TInstruction;
   begin
      result := TPIC18x_BCF.Create (INTCON, intcon_gieh, access_mode);
      result.annotation := 'turn interrupts off'
   end;

procedure allocate_hw_stack (prog: TPIC18x_Program);
   begin
      hw_stack_used := prog.initial_statement_hw_stack_usage;
   end;

procedure TPriorityMapper.Init;
   begin
      SetLength(priorities, 0);
      NotePriorityLevelUse (initial_process_priority);
      number_of_processes := 1   // initial process
   end;

procedure TPriorityMapper.NotePriorityLevelUse (prio: TPriorityRange);
   var
      i: integer;
   begin
      number_of_processes := number_of_processes + 1;
      for i := 0 to High(priorities) do
         if priorities[i].prio = prio then
            exit;
      i := Length(priorities);
      SetLength(priorities, i+1);
      if i > 0
      then
         while priorities[i-1].prio > prio
         do begin
               priorities[i].prio := priorities[i-1].prio;
               i := i - 1
            end;
      priorities[i].prio := prio
   end;

function TPriorityMapper.PrioUsed (prio: integer): boolean;
   var
      i: integer;
   begin
      result := false;
      for i := 0 to High(priorities) do
         if priorities[i].prio = prio then
            begin
               result := true;
               exit
            end
   end;

function TPriorityMapper.ReadyQueueAddr (prio: TPriorityRange): integer;
   var
      i: integer;
   begin
      result := 0;  // suppress compiler warning
      for i := 0 to High(priorities) do
         if priorities[i].prio = prio then
            begin
               result := priorities[i].ready_addr;
               exit
            end;
      assert (false)
   end;

function TPriorityMapper.AnyHiPriorityInterruptProcesses: boolean;
   begin
      result := PrioUsed (2)
   end;

function TPriorityMapper.AnyLowPriorityInterruptProcesses: boolean;
   begin
      result := PrioUsed (1)
   end;

function TPriorityMapper.Interruptable (prio: TPriorityRange): boolean;
   begin
      case prio of
         2: result := false;
         1: result := AnyHiPriorityInterruptProcesses;
         initial_process_priority: result := false;
      else
         result := AnyHiPriorityInterruptProcesses or AnyLowPriorityInterruptProcesses
      end
   end;

procedure InitProcessStateSFRAddresses;
   procedure init (idx: TProcessStateSFRs; addr: TDataMemoryAddress; name: string);
      begin
         ProcessStateSFRAddress[idx].addr := addr;
         ProcessStateSFRAddress[idx].name := name
      end;
   var
      idx: TProcessStateSFRs;
   begin
      for idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
         ProcessStateSFRAddress[idx].addr := 0;

      init (psFSR1H, FSR1H, 'FSR1H');
      init (psFSR1L, FSR1L, 'FSR1L');
      init (psPCLATH, PCLATH, 'PCLATH');
      init (psPCLATU, PCLATU, 'PCLATU');
      init (psPRODH, PRODH, 'PRODH');
      init (psPRODL, PRODL, 'PRODL');
      init (psTABLAT, TABLAT, 'TABLAT');
      init (psTBLPTRL, TBLPTRL, 'TBLPTRL');
      init (psTBLPTRH, TBLPTRH, 'TBLPTRH');
      init (psthis_ptrH, this_ptrH, 'thisH');
      init (psthis_ptrL, this_ptrL, 'thisL');
      init (psTOSU, TOSU, 'TOSU');
      init (psTOSH, TOSH, 'TOSH');
      init (psTOSL, TOSL, 'TOSL');
      init (psWREG, WREG, 'WREG');

      for idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
         assert (ProcessStateSFRAddress[idx].addr <> 0)
   end;

procedure AssignKernelVariableAddresses (prog: TProgram);

   procedure assign (var addr: integer; size: integer);
      begin
         assert (addr < $100);  // must be bank 0 addressable
         addr := next_available_absolute_address;
         next_available_absolute_address := next_available_absolute_address + size
      end;

   var
      i: integer;
   begin
      next_available_absolute_address := 0;

      assign (current_prio, 1);
      assign (kernel_exit_flags, 1);
      assign (this_ptrH, 1);
      assign (this_ptrL, 1);
      assign (error_logU, 1);
      assign (error_logH, 1);
      assign (error_logL, 1);
      assign (kernel_temp1, 1);
      assign (kernel_temp2, 1);

      // assign addresses of ready "array" entries
      for i := low(PriorityMapper.priorities) to high(PriorityMapper.priorities) do
         if PriorityMapper.Interruptable (PriorityMapper.priorities[i].prio)
         then
            assign (PriorityMapper.priorities[i].ready_addr, 2 + ProcessStateAreaSize)
         else
            assign (PriorityMapper.priorities[i].ready_addr, 2);  // can't be interrupted
//      assert (PriorityMapper.priorities[0].ready_addr = $00);

      for i := 0 to prog.program_vars.Length-1 do
         if prog.program_vars[i].typedef.IsProcessSystemType
//            and
//            prog.program_vars[i].reachable
         then
            assign (TPIC18x_Variable(prog.program_vars[i]).pcb_address, pcb_size);
      assign (initial_process_pcb_addr, pcb_size);
      assert (initial_process_pcb_addr <= $FC);  // pcb_addr+3 needs to fit in 8 bits without overflow

      InitProcessStateSFRAddresses
   end;

procedure EnterMonitor (monitor_prio: integer);
   var
      lbl: TInstruction;
   begin
      TPIC18x_LFSR.Create (0, PriorityMapper.ReadyQueueAddr(monitor_prio)).annotation := 'fsr0 := @ready[monitor_prio];';
      TurnInterruptsOff;
      case monitor_prio of
         high_priority_interrupt:
            TPIC18x_BSF.Create (kernel_exit_flags, kef_running_at_high_priority_bit, bank_mode).annotation := 'running_at_high_priority := true';
         low_priority_interrupt:
            TPIC18x_BCF.Create (INTCON, intcon_giel, access_mode).annotation := 'GIEL := false';
      else
      end;
      lbl := TCALLMacro.Create;
      kernel_entermon.ComeFrom (lbl);
      lbl.annotation := 'enter monitor';
      StackUsageCounter.PushPop (5)    // resume addr(3) and this ptr(2)
   end;


procedure LeaveMonitor (monitor_prio, pop_size: integer);
   var
      lbl: TInstruction;
   begin
      TurnInterruptsOff;
      lbl := TCALLMacro.Create;
      kernel_leavemon.ComeFrom (lbl);
      lbl.annotation := 'leave monitor';
//      StackUsageCounter.Pop (1)      //  pop old prio
   end;


function TPIC18x_AwaitInterruptStatement.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               // fsr1 := addr of process priority queue
               TPIC18x_LFSR.Create (1, PriorityMapper.ReadyQueueAddr(containing_process.priority)).annotation := format ('fsr1 := @ready[%d]', [containing_process.priority]);
               TurnInterruptsOff;
               kernel_await_interrupt.ComeFrom (TCALLMacro.Create);
               StackUsageCounter.PushPop (5)    // resume addr(3) and this ptr(2)
            end;
      else
         assert (false, 'TPIC18x_AwaitStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_ContinueStatement.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               TPIC18x_Access(queue_access).Generate_Load_Ptr2_Code (pFSR0, 0);
               if (StackUsageCounter.Current + TPIC18x_ParamList(containing_routine.parameter_definitions).Size) > 0 then
                  TPIC18x_ADDFSR.Create (2, StackUsageCounter.Current + TPIC18x_ParamList(containing_routine.parameter_definitions).Size);  // don't count stack changes here, continue will exit but it looks like a normal statement
               // tos is old_prio
               TurnInterruptsOff;
               kernel_continue.ComeFrom (TCALLMacro.Create)
            end;
      else
         assert (false, 'TPIC18x_ContinueStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_DelayStatement.Generate (param1, param2: integer): integer;
   var
      lbl: TInstruction;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TSourceSyncPoint.Create (src_loc);
               TPIC18x_Access(queue_access).Generate_Load_Ptr2_Code (pFSR0, 0);
               TurnInterruptsOff;
               lbl := TCALLMacro.Create;
               kernel_delay.ComeFrom (lbl);
               lbl.annotation := 'execute delay';
               StackUsageCounter.PushPop (5)    // resume addr(3) and this(2)
            end;
      else
         assert (false, 'TPIC18x_DelayStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

function TPIC18x_EmptyFunctionPrimary.Generate (param1, param2: integer): integer;
   begin
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               TPIC18x_Access(access).Generate_Load_Ptr2_Code (pFSR0, 0);
               TPIC18x_PUSHL.Create (0);
               StackUsageCounter.Push(1);
               TPIC18x_TSTFSZ.Create (INDF0, access_mode);
               TPIC18x_INCF.Create (1, dest_f, access_mode)
            end;
      else
         assert (false, 'TPIC18x_EmptyFunctionPrimary.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;

procedure TInitProcessSubroutine.generate_subroutine_code;
   var
      instr: TInstruction;
      bz: TPIC18x_BZ;
      push_return_address_macro: TPushLabelMacro;
   begin
      // note: this subroutine is only called before interrupts are turned on
      TAssemblyComment.Create ('init_process (proc_fsr1: ^process; process_prio: prio_range; pcb: ^pcb; stk_base_adj: integer; init_stmt: rom_addr);');

      // get pointer to parameters
      TPIC18x_MOVFF.Create (TOSU, TBLPTRU).annotation := 'tblptr := @parameters;';
      TPIC18x_MOVFF.Create (TOSH, TBLPTRH);
      TPIC18x_MOVFF.Create (TOSL, TBLPTRL);

      // set this pointer to process
      TPIC18x_MOVFF.Create (FSR1L, this_ptrL).annotation := 'this := proc_fsr1;';
      TPIC18x_MOVFF.Create (FSR1H, this_ptrH);

      TPIC18x_TBLRD.Create (tblrd_post_inc);     // read process_prio
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (current_prio, bank_mode);
      TPIC18x_CLRF.Create (FSR0H, access_mode);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);

      TPIC18x_TBLRD.Create (tblrd_post_inc);     // read pcb_address
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (INDF0, access_mode);

      TPIC18x_SETF.Create (PREINC0, access_mode);  // set state to running

      // copy parameters
      adjust_fsr (pFSR1, -$3E).annotation := 'copy parameters';
      TPIC18x_TBLRD.Create (tblrd_post_inc);     // read parameter block size
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      bz := TPIC18x_BZ.Create;
      instr := TPIC18x_MOVFF.Create (PREINC2, POSTINC1);
      TPIC18x_DECFSZ.Create (WREG, dest_w, access_mode);
      TPIC18x_BRA.Create.dest := instr;

      bz.dest := TAssemblyLabel.Create;
      push_return_address_macro := TPushLabelMacro.Create;
      push_return_address_macro.annotation := 'push return address';
      TPIC18x_SUBFSR.Create (2, 2);  // push garbage this_ptr (ptr in program initial process not used)
      // save caller's (the initial process) stack pointer
      TPIC18x_MOVFF.Create (FSR2H, initial_process_pcb_addr + pcb_stk_saveH_offset).annotation := 'save initial process stack pointer for return via dispatch';
      TPIC18x_MOVFF.Create (FSR2L, initial_process_pcb_addr + pcb_stk_saveL_offset);

      // set process initial stack ptr
      TPIC18x_TBLRD.Create (tblrd_post_inc).annotation := 'fsr2 := stk_base;';
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (FSR2H, access_mode);

      // call process initial statement
      TPIC18x_TBLRD.Create (tblrd_post_inc).annotation := 'pclat := @process initial statement';
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (PCLATU, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (PCLATH, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);

      TPIC18x_MOVF.Create (TBLPTRU, dest_w, access_mode).annotation := 'update return address';
      TPIC18x_MOVWF.Create (TOSU, access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSH, access_mode);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSL, access_mode);
      TPIC18x_CLRF.Create (TBLPTRU, access_mode);   // will be assumed 0 after interrupts are turned on

      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode).annotation := 'call process initial statement';
      TPIC18x_MOVWF.Create (PCL, access_mode);
      push_return_address_macro.dest := TAssemblyLabel.Create;

      // update return address and return
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TInitProcessSubroutine.report_stack_sizes;
   begin
      check_stack_sizes (5, 5, 1)
   end;
{$endif}

procedure init_kernel (proc: TByteParamProcedureOfObject);
   var
      i, j: integer;
      found: boolean;
   begin
      for i := 0 to next_available_absolute_address-1 do
         begin
            found := false;
            for j := 0 to High(PriorityMapper.priorities) do
               if i = PriorityMapper.priorities[j].ready_addr + ready_state_offset then
                  found := true;
            if found then
               proc ($40, 0, '', '', false)  // set state to suspended
            else if i = current_prio then
               proc (PriorityMapper.ReadyQueueAddr(initial_process_priority), 0, '', '', false)
            else if i = PriorityMapper.ReadyQueueAddr(initial_process_priority) then
               proc (initial_process_pcb_addr, 0, '', '', false)
            else if i = kernel_exit_flags then
               proc ($80, 0, '', '', false)   // set initializing bit
            else
               proc (0, 0, '', '', false)
         end
   end;

procedure init_class_control_block (class_var: TVariable; proc: TByteParamProcedureOfObject);
   begin
      if TSystemType(class_var.typedef).permanent_eeprom_vars.Length > 0 then
         begin
            proc (eeprom_used, 0, '', '', false);
            eeprom_used := eeprom_used + TPIC18x_DataItemList (TSystemType(class_var.typedef).permanent_eeprom_vars).Size
         end
   end;

procedure init_monitor_control_block (monitor_var: TVariable; proc: TByteParamProcedureOfObject);
   begin
      if TSystemType(monitor_var.typedef).permanent_eeprom_vars.Length > 0 then
         begin
            proc (eeprom_used, 0, '', '', false);
            eeprom_used := eeprom_used + TPIC18x_DataItemList (TSystemType(monitor_var.typedef).permanent_eeprom_vars).Size
         end;
      proc (0, 0, '', '', false);  // gate
   end;

procedure init_process_control_block (process_var: TVariable; proc: TByteParamProcedureOfObject);
   begin
      if TSystemType(process_var.typedef).permanent_eeprom_vars.Length > 0 then
         begin
            proc (eeprom_used, 0, '', '', false);
            eeprom_used := eeprom_used + TPIC18x_DataItemList (TSystemType(process_var.typedef).permanent_eeprom_vars).Size
         end
   end;

procedure init_initial_process (initial_statement_stack_size: integer; proc: TByteParamProcedureOfObject);
   var
      i: integer;
   begin
      for i := 1 to initial_statement_stack_size-1 do    // do one less than #bytes so last POSTINC leaves FSR2 in right place
         proc (initial_process_stk_init, 0, '', '', false)      //    this will mean first use of stk by initial process will appear
   end;

procedure GenerateKernelStartupCode;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('system_initializing := false;');
      TPIC18x_BCF.Create (kernel_exit_flags, kef_system_initializing_bit, bank_mode);
      TSourceLine.Create ('goto kernel_idle;');
      kernel_idle.ComeFrom (TGOTOMacro.Create)
   end;

procedure generate_await_interrupt_code;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('==========================');
      TSourceLine.Create (' Kernel - Await Interrupt ');
      TSourceLine.Create ('==========================');
      TAssemblySourceBlankLine.Create;
      // fsr1 -> ready[current_prio]
      kernel_await_interrupt.target_label := TAssemblyLabel.Create;
      TAssemblyComment.Create ('@ready[current_prio] is in FSR1');
      TSourceLine.Create.annotation := 'push return address;';
      TPIC18x_MOVFF.Create (TOSL, POSTDEC2);
      TPIC18x_MOVFF.Create (TOSH, POSTDEC2);
      TPIC18x_MOVFF.Create (TOSU, POSTDEC2);
      TSourceLine.Create.annotation := 'push this;';
      TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2);
      TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2);
      TSourceLine.Create.annotation := 'pcb := ready[current_prio].queue;';
      TPIC18x_CLRF.Create (FSR0H, access_mode);
      TPIC18x_MOVFF.Create (INDF1, FSR0L);
      TSourceLine.Create.annotation := 'ready[current_prio] := pcb^.next;';
      TPIC18x_MOVFF.Create (INDF0, INDF1);
      TSourceLine.Create.annotation := 'pcb^.next := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);
      TSourceLine.Create.annotation := 'pcb.stkptr := stkptr;';
      TPIC18x_MOVFF.Create (FSR2H, PREINC0);
      TPIC18x_MOVFF.Create (FSR2L, PREINC0);
      TSourceLine.Create.annotation := 'pcb.nesting := awaiting_interrupt;';
      // assert pcb.nesting = 0 (since await interrupt cannot be called from a monitor )
      TPIC18x_SETF.Create (PREINC0, access_mode);
      TSourceLine.Create.annotation := 'ready[current_prio].state := suspended;';
      TPIC18x_BCF.Create (PREINC1, ready_state_running_bit, access_mode);
      TSourceLine.Create.annotation := 'goto dispatch;';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create)
   end;

procedure generate_dispatch_code;
   var
      i: integer;
      dispatch_preempted_process, dispatch_suspended_process: TBranchTarget;
      lbl: TInstruction;
      idx: TProcessStateSFRs;
      bz: TPIC18x_BZ;
      bra: TPIC18x_BRA;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('===================');
      TSourceLine.Create (' Kernel - Dispatch ');
      TSourceLine.Create ('===================');
      TAssemblySourceBlankLine.Create;

      dispatch_preempted_process := TBranchTarget.Create;
      dispatch_suspended_process := TBranchTarget.Create;

      TSourceLine.Create ('dispatch:');
      kernel_dispatch.target_label := TAssemblyLabel.Create;

      TPIC18x_CLRF.Create (FSR0H, access_mode);  // not necessary if all branches here have fsr0h already cleared

      for i := High(PriorityMapper.priorities) downto Low(PriorityMapper.priorities)+1 do
         begin
            case PriorityMapper.priorities[i].prio of
               high_priority_interrupt:
                  begin
                     TSourceLine.Create ('   running_at_high_priority := true;');
                     TPIC18x_BSF.Create (kernel_exit_flags, kef_running_at_high_priority_bit, bank_mode)
                  end;
               low_priority_interrupt:
                  begin
                     TSourceLine.Create ('   INTCON.GIEL := false;');
                     TPIC18x_BCF.Create (INTCON, intcon_giel, access_mode)
                  end;
            else
            end;

            if PriorityMapper.Interruptable(PriorityMapper.priorities[i].prio)
            then
               begin
                  TSourceLine.Create (format('   if ready[%d].queue <> nil', [PriorityMapper.priorities[i].prio]));
                  TPIC18x_MOVF.Create (PriorityMapper.priorities[i].ready_addr, dest_f, bank_mode);   // set status
                  bz := TPIC18x_BZ.Create;
                  TSourceLine.Create ('   then');
                  TPIC18x_MOVLW.Create (PriorityMapper.priorities[i].ready_addr);
                  TSourceLine.Create (format('      if ready[%d].state = preempted', [PriorityMapper.priorities[i].prio]));
                  TPIC18x_BTFSS.Create (PriorityMapper.priorities[i].ready_addr + ready_state_offset, ready_state_suspended_bit, bank_mode);
                  TSourceLine.Create ('      then');
                  TSourceLine.Create (format ('         goto dispatch_preempted_process (ready[%d])', [PriorityMapper.priorities[i].prio]));
                  dispatch_preempted_process.ComeFrom (TGOTOMacro.Create);
                  TSourceLine.Create ('      else');
                  TSourceLine.Create (format ('         goto dispatch_suspended_process (ready[%d]);', [PriorityMapper.priorities[i].prio]));
                  dispatch_suspended_process.ComeFrom (TGOTOMacro.Create);
                  bz.dest := TAssemblyLabel.Create
               end
            else  // not interruptable
               begin
                  TSourceLine.Create (format('   if ready[%d].queue <> nil', [PriorityMapper.priorities[i].prio]));
                  TPIC18x_MOVF.Create (PriorityMapper.priorities[i].ready_addr, dest_f, bank_mode);  // set status
                  TSourceLine.Create ('   then');
                  TPIC18x_MOVLW.Create (PriorityMapper.priorities[i].ready_addr);
                  TSourceLine.Create (format ('      goto dispatch_suspended_process (ready[%d]);', [PriorityMapper.priorities[i].prio]));
                  dispatch_suspended_process.ComeFrom (TPIC18x_BNZ.Create)
               end;

            case PriorityMapper.priorities[i].prio of
               high_priority_interrupt:
                  begin
                     TSourceLine.Create ('   running_at_high_priority := false;');
                     TPIC18x_BCF.Create (kernel_exit_flags, kef_running_at_high_priority_bit, bank_mode)
                  end;
               low_priority_interrupt:
                  begin
                     TSourceLine.Create ('   INTCON.GIEL := true;');
                     TPIC18x_BSF.Create (INTCON, intcon_giel, access_mode)
                  end;
            else
            end
         end;
      TSourceLine.Create ('   if not system_initializing');
      TPIC18x_BTFSC.Create (kernel_exit_flags, kef_system_initializing_bit, bank_mode);
      bra := TPIC18x_BRA.Create;

      TSourceLine.Create ('   then');

      TSourceLine.Create ('      begin');

      TPIC18x_POP.Create;   // pop hw stack (special case - all other exits of kernel are via RETURN/RETFIE).

      TSourceLine.Create ('kernel_idle:');
      kernel_idle.target_label := TAssemblyLabel.Create;

      TSourceLine.Create ('         switch to kernel stack;');
      TPIC18x_LFSR.Create (2, kernel_stack_base);

      TSourceLine.Create ('         current_prio := init;');
      TPIC18x_MOVLW.Create (PriorityMapper.priorities[0].ready_addr);
      TPIC18x_MOVWF.Create (current_prio, bank_mode);

      TSourceLine.Create ('         Ready[init].state := suspended;');
      TPIC18x_BCF.Create (PriorityMapper.priorities[0].ready_addr + ready_state_offset, ready_state_running_bit, bank_mode);

      if PriorityMapper.AnyLowPriorityInterruptProcesses then
         begin
            TSourceLine.Create ('         INTCONT.GIEL := true;');
            TPIC18x_BSF.Create (INTCON, intcon_giel, access_mode)
         end;
      TSourceLine.Create ('         INTCONT.GIEH := true;');
      TPIC18x_BSF.Create (INTCON, intcon_gieh, access_mode);
{$ifdef INCLUDE_SIMULATION}
      if no_sleep_on_idle then
         begin
            lbl := TAssemblyLabel.Create;
            TSourceLine.Create ('goto *')
         end
      else
         begin
            TSourceLine.Create ('         sleep');     // sleep needed here since it is used by internal simulation to mark end of test
            lbl := TPIC18x_SLEEP.Create       // WANT SIMULATOR TO STOP????  HOW ABOUT TESTING STUFF AFTER INIT DONE???
         end;
      TPIC18x_BRA.Create.dest := lbl;    // this is only needed for the MPLAB simulator which steps out of SLEEP
{$else}
      if prog.FinalStatementIsEmptyCycleStatement then
         begin
            TSourceLine.Create ('         goto *');
            lbl := TPIC18x_BRA.Create;
            lbl.dest := lbl
         end
      else
         begin
            TSourceLine.Create ('         sleep');
            lbl := TPIC18x_SLEEP.Create;
            TPIC18x_BRA.Create.dest := lbl     // this is only needed for the  MPLAB simulator which steps out of SLEEP
         end;
{$endif}
      TSourceLine.Create ('      end');

      TSourceLine.Create ('   else  // current_prio = init');

      bra.dest := TAssemblyLabel.Create;

      TSourceLine.Create ('      goto dispatch_suspended_process (ready[init].queue);');
      TPIC18x_MOVLW.Create (PriorityMapper.priorities[0].ready_addr);
//      dispatch_suspended_process.ComeFrom (TPIC18x_BRA.Create);   FALL THROUGH TO DISPATCH_SUSPENDED_PROCESS NEXT

      TSourceLine.Create ('dispatch_suspended_process:');
      dispatch_suspended_process.target_label := TAssemblyLabel.Create;
      TAssemblyComment.Create ('priority is in W');

      TSourceLine.Create.annotation := '   current_prio := priority;';
      TPIC18x_MOVWF.Create (current_prio, bank_mode);

      TSourceLine.Create.annotation := '   ready[current_prio].state := running;';
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_SETF.Create (PREINC0, access_mode);
      TPIC18x_SUBFSR.Create (0, 1);

      TSourceLine.Create.annotation := '   pcb := ready[current_prio].queue;';
      TPIC18x_MOVFF.Create (INDF0, FSR0L);

      TSourceLine.Create.annotation := '   stkptr := pcb.stk_ptr;';
      TPIC18x_MOVFF.Create (PREINC0, FSR2H);
      TPIC18x_MOVFF.Create (PREINC0, FSR2L);

      TSourceLine.Create.annotation :='   pop this;';
      TPIC18x_MOVFF.Create (PREINC2, this_ptrH);
      TPIC18x_MOVFF.Create (PREINC2, this_ptrL);

      TSourceLine.Create.annotation := '   pop return address;';
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSU, access_mode);
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSH, access_mode);
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSL, access_mode);

      TSourceLine.Create.annotation :='   return;';
      ExitKernel;

      TSourceLine.Create ('dispatch_preempted_process:');
      dispatch_preempted_process.target_label := TAssemblyLabel.Create;
      TAssemblyComment.Create ('priority is in W');

      TSourceLine.Create.annotation := '   current_prio := priority;';
      TPIC18x_MOVWF.Create (current_prio, bank_mode);

      TSourceLine.Create.annotation := '   FSR2 := ready[current_prio].queue.stkptr;';
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_MOVFF.Create (INDF0, FSR0L);
      TPIC18x_MOVFF.Create (PREINC0, FSR2H);
      TPIC18x_MOVFF.Create (PREINC0, FSR2L);

      TSourceLine.Create.annotation := '   STATUS := ready[current_prio].status_save;';
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_MOVFF.Create (PREINC0, STATUS);

      TSourceLine.Create.annotation := '   ready.state := running;';
      TPIC18x_SETF.Create (INDF0, access_mode);

      // restore all other SFRs
      for idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
         begin
            TSourceLine.Create.annotation := '   ' + ProcessStateSFRAddress[idx].name + ' := save_' + ProcessStateSFRAddress[idx].name + ';';
            case idx of
               psTOSU:
                  begin
                     assert (idx < psWREG);  // trashes WREG, must be done before restore WREG
                     TPIC18x_MOVF.Create (PREINC0, dest_w, access_mode);
                     TPIC18x_MOVWF.Create (TOSU, access_mode)
                  end;
               psTOSH:
                  begin
                     assert (idx < psWREG);  // trashes WREG, must be done before restore WREG
                     TPIC18x_MOVF.Create (PREINC0, dest_w, access_mode);
                     TPIC18x_MOVWF.Create (TOSH, access_mode)
                  end;
               psTOSL:
                  begin
                     assert (idx < psWREG);  // trashes WREG, must be done before restore WREG
                     TPIC18x_MOVF.Create (PREINC0, dest_w, access_mode);
                     TPIC18x_MOVWF.Create (TOSL, access_mode)
                  end;
            else
               TPIC18x_MOVFF.Create (PREINC0, ProcessStateSFRAddress[idx].addr)
            end
         end;

      TSourceLine.Create.annotation := '   pop FSR0;';
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      TPIC18x_MOVFF.Create (PREINC2, FSR0H);

      TSourceLine.Create.annotation := '   return;';
      ExitKernel;

      dispatch_preempted_process.set_client_destinations;
      dispatch_suspended_process.set_client_destinations;

      dispatch_preempted_process.Free;
      dispatch_suspended_process.Free
  end;

procedure generate_enter_monitor_code;
   var
      bra1, bra2, bra3: TPIC18x_BRA;
      bnz4: TPIC18x_BNZ;
      bz5: TPIC18x_BZ;
      lbl: TInstruction;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('========================');
      TSourceLine.Create (' Kernel - Enter Monitor ');
      TSourceLine.Create ('========================');
      TAssemblySourceBlankLine.Create;

      kernel_entermon.target_label := TAssemblyLabel.Create;
      // when called FSR0 -> @ready[monitor_prio] & FSR0L = monitor_prio

      TSourceLine.Create.annotation := 'if gate = nil';
      TPIC18x_MOVFF.Create (this_ptrH, FSR1H);   // fsr1 := @this.gate
      TPIC18x_MOVFF.Create (this_ptrL, FSR1L);
      TPIC18x_SUBFSR.Create (1, $3E);
      TPIC18x_TSTFSZ.Create (INDF1, access_mode);
      bra1 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := 'then  // monitor is unoccupied';

      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      gate := ppcb($FF);';
      TPIC18x_SETF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := '      ready[current_prio].state := suspended;';
      TPIC18x_CLRF.Create (FSR1H, access_mode);    // fsr1 := @ready[current_prio)
      TPIC18x_MOVF.Create (current_prio, dest_w, bank_mode);  // W now contains current_prio
      TPIC18x_MOVWF.Create (FSR1L, access_mode);
      TPIC18x_BCF.Create (PREINC1, ready_state_running_bit, access_mode);
      TPIC18x_SUBFSR.Create (1, 1);

      TSourceLine.Create.annotation := '      ready[monitor_prio].state := running;';
      TPIC18x_SETF.Create (PREINC0, access_mode);
      TPIC18x_SUBFSR.Create (0, 1);

      TSourceLine.Create.annotation := '      this_proc_pcb := ready[current_prio].queue;';
      TPIC18x_MOVFF.Create (INDF1, FSR1L);

      TSourceLine.Create.annotation := '      if current_prio < monitor_prio';
      TPIC18x_CPFSGT.Create (FSR0L, access_mode);
      bra2 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '      then  // boost process prio to monitor prio';

      TSourceLine.Create.annotation := '         begin';

      TSourceLine.Create.annotation := '            ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_MOVFF.Create (FSR0L, kernel_temp1);  // save monitor_prio
      TPIC18x_MOVWF.Create (FSR0L, access_mode);   // fsr0 := @ready[current_prio]
      TPIC18x_MOVFF.Create (INDF1, INDF0);

      TSourceLine.Create.annotation := '            ready[monitor_prio].queue := this_proc_pcb;';
      TPIC18x_MOVF.Create (kernel_temp1, dest_w, bank_mode);  // w := monitor_prio
      TPIC18x_MOVWF.Create (FSR0L, access_mode);  // fsr0 := @ready[monitor_prio]
      TPIC18x_MOVFF.Create (FSR1L, INDF0);

      TSourceLine.Create.annotation := '            current_prio := monitor_prio;';
      TPIC18x_MOVWF.Create (current_prio, bank_mode);

      TSourceLine.Create.annotation := '            this_proc_pcb^.next := nil';
      TPIC18x_CLRF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := '         end;';
      bra2.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      this_proc_pcb^.nesting := this_proc_pcb^.nesting + 1';
      TPIC18x_MOVLW.Create (3);
      TPIC18x_INCF.Create (PLUSW1, dest_f, access_mode);

      TSourceLine.Create.annotation := '      return';
      ExitKernel;

      TSourceLine.Create.annotation := '   end';

      TSourceLine.Create.annotation := 'else  // monitor is occupied, block this process until monitor frees up';
      bra1.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '   begin';

      TPIC18x_MOVFF.Create (TOSL, POSTDEC2).annotation := 'save return addr';
      TPIC18x_MOVFF.Create (TOSH, POSTDEC2);
      TPIC18x_MOVFF.Create (TOSU, POSTDEC2);

      TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2).annotation := 'save this';
      TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2);

      TSourceLine.Create.annotation := '      ready[current_prio].state := suspended;';
      TPIC18x_MOVFF.Create (current_prio, FSR0L);   // fsr0 := @ready[current_prio]
      TPIC18x_BCF.Create (PREINC0, ready_state_running_bit, access_mode);
      TPIC18x_SUBFSR.Create (0, 1);

      TSourceLine.Create.annotation := '      this_proc_pcb := ready[current_prio].queue;';
      TPIC18x_MOVFF.Create (INDF0, FSR0L);

      TPIC18x_MOVFF.Create (FSR2H, PREINC0);  // save fsr2 in PCB
      TPIC18x_MOVFF.Create (FSR2L, PREINC0);
      TSourceLine.Create.annotation := '      this_proc_pcb^.nesting := this_proc_pcb^.nesting + 1;';
      TPIC18x_INCF.Create (PREINC0, dest_f, access_mode);
      TPIC18x_SUBFSR.Create (0, 3);  // put fsr0 back to @this_pcb

      TSourceLine.Create.annotation := '      if gate = ppcb($FF)';
      TPIC18x_INCF.Create (INDF1, dest_w, access_mode);
      bnz4 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := '      then';
      TSourceLine.Create.annotation := '         gate := this_proc_pcb';
      TPIC18x_MOVFF.Create (FSR0L, INDF1);

      bra3 := TPIC18x_BRA.Create;
      TSourceLine.Create.annotation := '      else';
      bnz4.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '         begin';

      TSourceLine.Create.annotation := '            // place this process at end of monitor queue';

      TSourceLine.Create.annotation := '            pcb := gate;';
      TPIC18x_MOVFF.Create (INDF1, FSR1L);  // pcb is fsr1
      TPIC18x_CLRF.Create (FSR1H, access_mode);

      TSourceLine.Create.annotation := '            while pcb^.next <> ppcb($FF)';
      lbl := TPIC18x_INCF.Create (INDF1, dest_w, access_mode);
      bz5 := TPIC18x_BZ.Create;

      TSourceLine.Create.annotation := '            do pcb := pcb^.next;';
      TPIC18x_MOVFF.Create (INDF1, FSR1L);
      TPIC18x_BRA.Create.dest := lbl;
      bz5.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '            pcb^.next := this_proc_pcb';
      TPIC18x_MOVFF.Create (FSR0L, INDF1);

      TSourceLine.Create.annotation := '         end;';
      bra3.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_CLRF.Create (FSR1H, access_mode);
      TPIC18x_MOVFF.Create (current_prio, FSR1L);
      TPIC18x_MOVFF.Create (INDF0, INDF1);

      TSourceLine.Create.annotation := '      this_proc_pcb^.next := ppcb($FF);';
      TPIC18x_SETF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '   end;'
   end;

procedure generate_leave_monitor_code;
   var
      bra7: TPIC18x_BRA;
      lbl: TInstruction;
      bra3: TPIC18x_BRA;
      bnz1, bnz2, bnz3: TPIC18x_BNZ;
      bz8: TPIC18x_BZ;
      br5, br6, br10, br11, br12, br13: TPIC18x_relative_transfer;
      then1, endif1: TBranchTarget;
      old_prio: integer;
      next_proc_pcb: integer;
   begin
      then1 := TBranchTarget.Create;
      endif1 := TBranchTarget.Create;
      old_prio := kernel_temp1;
      next_proc_pcb := kernel_temp2;

      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('========================');
      TSourceLine.Create (' Kernel - Leave Monitor ');
      TSourceLine.Create ('========================');
      TAssemblySourceBlankLine.Create;

      kernel_leavemon.target_label := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := 'this_proc_pcb := ready[current_prio].queue;';
      TPIC18x_CLRF.Create (FSR0H, access_mode);
      TPIC18x_MOVFF.Create (current_prio, FSR0L);
      TPIC18x_MOVFF.Create (INDF0, FSR0L);

      TSourceLine.Create.annotation := 'this_proc_pcb^.nesting := this_proc_pcb^.nesting - 1;';
      TPIC18x_MOVLW.Create (pcb_nesting_offset);
      TPIC18x_DECF.Create (PLUSW0, dest_f, access_mode);

      TSourceLine.Create.annotation := 'old_prio := stack.pop;';
      TPIC18x_MOVFF.Create (PREINC2, old_prio);

      TSourceLine.Create.annotation := '// exit via return if no task switch necessary';

      TSourceLine.Create.annotation := 'if (gate = ppcb($FF))';
      TPIC18x_MOVFF.Create (this_ptrH, FSR1H);    // fsr1 := @gate
      TPIC18x_MOVFF.Create (this_ptrL, FSR1L);
      TPIC18x_SUBFSR.Create (1, $3E);
      TPIC18x_INCF.Create (INDF1, dest_w, access_mode);
      endif1.ComeFrom (TPIC18x_BNZ.Create);

      TSourceLine.Create.annotation := '   and';

      TSourceLine.Create.annotation := '   (current_prio = old_prio)';
      TPIC18x_MOVF.Create (old_prio, dest_w, bank_mode);
      TPIC18x_CPFSEQ.Create (current_prio, bank_mode);  // w has old_prio
      endif1.ComeFrom (TPIC18x_BRA.Create);

      TSourceLine.Create.annotation := 'then';
      TSourceLine.Create.annotation := '   if (this_proc_pcb^.nesting > 0)';
      TPIC18x_MOVLW.Create (pcb_nesting_offset);
      TPIC18x_MOVF.Create (PLUSW0, dest_f, access_mode);
      then1.ComeFrom (TPIC18x_BNZ.Create);

      TSourceLine.Create.annotation := '      or';

      TSourceLine.Create.annotation := '      (this_proc_pcb^.next = nil)';
      TPIC18x_MOVF.Create (INDF0, dest_f, access_mode);
      endif1.ComeFrom (TPIC18x_BNZ.Create);

      TSourceLine.Create.annotation := '   then';
      then1.target_label := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      begin';

      TSourceLine.Create.annotation := '         gate := nil;';
      TPIC18x_CLRF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := '         this := stack.pop;';
      TPIC18x_MOVFF.Create (PREINC2, this_ptrH);
      TPIC18x_MOVFF.Create (PREINC2, this_ptrL);
      TSourceLine.Create.annotation := '         return';
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSU, access_mode);
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSH, access_mode);
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (TOSL, access_mode);
      ExitKernel;

      TSourceLine.Create.annotation := '      end;';
      endif1.target_label := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '';
      TSourceLine.Create.annotation := '// a task switch may be necessary, exit will be via dispatch';

      TPIC18x_MOVFF.Create (FSR2H, PREINC0);
      TPIC18x_MOVFF.Create (FSR2L, PREINC0);
      // at this point this_pcb is ready to be dispatched & fsr2 is free

      TSourceLine.Create.annotation := 'ready[current_prio].state := suspended;';
      TPIC18x_CLRF.Create (FSR2H, access_mode);
      TPIC18x_MOVFF.Create (current_prio, FSR2L);
      TPIC18x_BCF.Create (PREINC2, ready_state_running_bit, access_mode);
      TPIC18x_SUBFSR.Create (2, 1);

      TSourceLine.Create.annotation := 'if this_proc_pcb^.nesting = 0';
      TPIC18x_MOVF.Create (PREINC0, dest_f, access_mode);
      TPIC18x_SUBFSR.Create (0, pcb_nesting_offset);    // restore fsr0 to @this_proc_pcb
      bnz1 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := 'then';

      TSourceLine.Create.annotation := '   if gate = ppcb($FF)';
      TPIC18x_INCF.Create (INDF1, dest_w, access_mode);
      bnz2 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := '   then  // no other process at gate to get into monitor';
      TSourceLine.Create.annotation := '      begin';

      TSourceLine.Create.annotation := '         gate := nil;';
      TPIC18x_CLRF.Create (INDF1, access_mode);

      TPIC18x_CLRF.Create (FSR1H, access_mode);  // done with gate, will only need 8 bit ptr from here on

      TSourceLine.Create.annotation := '         if current_prio = old_prio';
      TPIC18x_MOVF.Create (current_prio, dest_w, bank_mode);     // w := current_prio
      TPIC18x_CPFSEQ.Create (old_prio, bank_mode);
      bra3 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '         then';

      TSourceLine.Create.annotation := '            begin  // move this process to back of ready queue';

      TPIC18x_MOVWF.Create (FSR1L, access_mode);  // fsr1 := @ready[current_prio]

      TSourceLine.Create.annotation := '               pcb := this_proc_pcb^.next;';
      TPIC18x_MOVF.Create (INDF0, dest_w, access_mode);     // w := this_proc_pcb^.next
      TPIC18x_MOVWF.Create (FSR2L, access_mode);            // fsr2 is pcb from now on

      TSourceLine.Create.annotation := '               assert (pcb <> nil);';   // should have returned above

      TSourceLine.Create.annotation := '               ready[current_prio].queue := pcb;';
      TPIC18x_MOVWF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := '               this_proc_pcb^.next := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := '               while pcb^.next <> nil';
      lbl := TPIC18x_MOVF.Create (INDF2, dest_f, access_mode);   // set status
      bz8 := TPIC18x_BZ.Create;

      TSourceLine.Create.annotation := '               do pcb := pcb^.next;';
      TPIC18x_MOVFF.Create (INDF2, FSR2L);
      TPIC18x_BRA.Create.dest := lbl;
      bz8.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '               pcb^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '               goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '            end';

      TSourceLine.Create.annotation := '         else  // current_prio > old_prio';
      bra3.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '            begin';

      TSourceLine.Create.annotation := '               ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_MOVWF.Create (FSR1L, access_mode);
      TPIC18x_MOVFF.Create (INDF0, INDF1);

      TSourceLine.Create.annotation := '               this_proc_pcb^.next := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := '               pcb := ready[old_prio].queue;';
      TPIC18x_CLRF.Create (FSR2H, access_mode);
      TPIC18x_MOVFF.Create (old_prio, FSR2L);
      TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);

      TSourceLine.Create.annotation := '               if pcb = nil';
      bnz3 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := '               then';

      TSourceLine.Create.annotation := '                  begin';

      TSourceLine.Create.annotation := '                     ready[old_prio].queue := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '                     goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '                  end';

      TSourceLine.Create.annotation := '               else';
      bnz3.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '                  begin';

      TSourceLine.Create.annotation := '                     while pcb^.next <> nil';
      TSourceLine.Create.annotation := '                     do pcb := pcb^.next;';
      lbl := TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);
      TPIC18x_BNZ.Create.dest := lbl;

      TSourceLine.Create.annotation := '                     pcb^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '                     goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '                  end';

      TSourceLine.Create.annotation := '            end';

      TSourceLine.Create.annotation := '      end';

      TSourceLine.Create.annotation := '   else  // gate <> ppcb($FF): at least one other processes is at gate to get into monitor';
      bnz2.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      begin';

      TSourceLine.Create.annotation := '         next_proc_pcb := gate;';
      TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (next_proc_pcb, bank_mode);

      TSourceLine.Create.annotation := '         gate := next_proc_pcb^.next;';
//      TPIC18x_CLRF.Create (FSR2H, access_mode);
      TPIC18x_MOVWF.Create (FSR2L, access_mode);   // fsr2 := @next_proc_pcb
      TPIC18x_MOVFF.Create (INDF2, INDF1);

      TSourceLine.Create.annotation := '         ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_MOVF.Create (current_prio, dest_w, bank_mode);  // fsr2 := @ready[current_prio]
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_MOVFF.Create (INDF0, INDF2);

      TSourceLine.Create.annotation := '         this_proc_pcb^.next := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := '         if ready[old_prio].queue = nil';
      TPIC18x_MOVFF.Create (old_prio, FSR2L);  // fsr2 := @ready[old_prio]
      TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);
      br6 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := '         then';

      TSourceLine.Create.annotation := '            ready[old_prio].queue := this_proc_pcb';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      bra7 := TPIC18x_BRA.Create;
      TSourceLine.Create.annotation := '         else';
      br6.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '            begin';

      TSourceLine.Create.annotation := '               pcb := ready[old_prio].queue;';
      TPIC18x_MOVWF.Create (FSR2L, access_mode);

      TSourceLine.Create.annotation := '               while pcb^.next <> nil';
      lbl := TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);
      br5 := TPIC18x_BZ.Create;

      TSourceLine.Create.annotation := '               do pcb := pcb^.next;';
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_BRA.Create.dest := lbl;

      br5.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '               pcb^.next := this_proc_pcb';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '            end;';
      bra7.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '         next_proc_pcb^.next := ready[current_prio].queue;';
      TPIC18x_MOVFF.Create (current_prio, FSR2L);  // fsr2 := @ready[current_prio]
      TPIC18x_CLRF.Create (FSR1H, access_mode);     // fsr1 := @next_proc_pcb
      TPIC18x_MOVF.Create (next_proc_pcb, dest_w, bank_mode);
      TPIC18x_MOVWF.Create (FSR1L, access_mode);
      TPIC18x_MOVFF.Create (INDF2, INDF1);

      TSourceLine.Create.annotation := '         ready[current_prio].queue := next_proc_pcb;';
      TPIC18x_MOVWF.Create (INDF2, access_mode);

      TSourceLine.Create.annotation := '         goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '      end';

      TSourceLine.Create.annotation := 'else  // nesting > 0';
      bnz1.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      if gate = ppcb($FF)';
      TPIC18x_INCF.Create (INDF1, dest_w, access_mode);
      br10 := TPIC18x_BNZ.Create;

      TSourceLine.Create.annotation := '      then  // no other process is at gate to get into monitor';

      TSourceLine.Create.annotation := '         begin';

      TSourceLine.Create.annotation := '            gate := nil;';
      TPIC18x_CLRF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := '            assert (current_prio <> old_prio);';

      TSourceLine.Create.annotation := '            ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_CLRF.Create (FSR1H, access_mode);     // fsr1 := @ready[current_prio]
      TPIC18x_MOVFF.Create (current_prio, FSR1L);
      TPIC18x_MOVFF.Create (INDF0, INDF1);

      TSourceLine.Create.annotation := '         end';

      br11 := TPIC18x_BRA.Create;
      TSourceLine.Create.annotation := '      else  // gate <> ppcb($FF): at least one other processes is at gate to get into monitor';
      br10.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '         begin';

      TSourceLine.Create.annotation := '            ready[current_prio].queue := gate;';
      TPIC18x_CLRF.Create (FSR2H, access_mode);    // fsr2 := @ready[current_prio]
      TPIC18x_MOVF.Create (current_prio, dest_w, bank_mode);  // w := current_prio
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_MOVFF.Create (INDF1, INDF2);

      TSourceLine.Create.annotation := '            gate := gate^.next;';
      TPIC18x_MOVFF.Create (INDF1, FSR2L);    // fsr2 := @gate^.next
      TPIC18x_MOVFF.Create (INDF2, INDF1);

      TPIC18x_MOVWF.Create (FSR2L, access_mode);  // fsr2 := @ready[current_prio]
      TPIC18x_MOVFF.Create (INDF2, FSR2L);   // fsr2 := @ready[current_prio].queue

      TSourceLine.Create.annotation := '            if current_prio = old_prio';
      TPIC18x_CPFSEQ.Create (old_prio, bank_mode);
      br13 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '            then';

      TSourceLine.Create.annotation := '               begin';

      TSourceLine.Create.annotation := '                  ready[current_prio].queue^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '                  goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '               end;';
      br13.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '            // current_prio > old_prio';

      TSourceLine.Create.annotation := '            ready[current_prio].queue^.next := this_proc_pcb^.next;';
      TPIC18x_MOVFF.Create (INDF0, INDF2);

      TSourceLine.Create.annotation := '         end;';
      br11.dest := TAssemblyLabel.Create;

      TPIC18x_MOVFF.Create (old_prio, FSR2L);  // fsr2 := @ready[old_prio]

      TSourceLine.Create.annotation := '      if ready[old_prio].state = preempted';
      TPIC18x_MOVLW.Create (ready_state_offset);
      TPIC18x_BTFSC.Create (PLUSW2, ready_state_suspended_bit, access_mode);
      br12 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '      then';

      TSourceLine.Create.annotation := '         begin  // run this proc after interrupted process at old prio';

      TSourceLine.Create.annotation := '            this_proc_pcb^.next := ready[old_prio].queue^.next;';
      TPIC18x_MOVFF.Create (INDF2, FSR1L);  // fsr1 := @ready[old_prio].queue
      TPIC18x_MOVFF.Create (INDF1, INDF0);

      TSourceLine.Create.annotation := '            ready[old_prio].queue^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF1);

      TSourceLine.Create.annotation := '            goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '         end';

      TSourceLine.Create.annotation := '      else';
      br12.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '         begin  // run this proc first when old prio reached';

      TSourceLine.Create.annotation := '            this_proc_pcb^.next := ready[old_prio].queue;';
      TPIC18x_MOVFF.Create (INDF2, INDF0);

      TSourceLine.Create.annotation := '            ready[old_prio].queue := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '            goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '         end';
      TSourceLine.Create.annotation := '   end;';

      then1.set_client_destinations;
      then1.Free;
      endif1.set_client_destinations;
      endif1.Free
   end;

procedure generate_delay_code;
   var
      bnz1: TPIC18x_BNZ;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('================');
      TSourceLine.Create (' Kernel - Delay ');
      TSourceLine.Create ('================');
      TAssemblySourceBlankLine.Create;

      kernel_delay.target_label := TAssemblyLabel.Create;
      TAssemblyComment.Create ('FSR0 -> q');

      TSourceLine.Create.annotation := 'push return address;';
      TPIC18x_MOVFF.Create (TOSL, POSTDEC2);
      TPIC18x_MOVFF.Create (TOSH, POSTDEC2);
      TPIC18x_MOVFF.Create (TOSU, POSTDEC2);

      TSourceLine.Create.annotation := 'push this;';
      TPIC18x_MOVFF.Create (this_ptrL, POSTDEC2);
      TPIC18x_MOVFF.Create (this_ptrH, POSTDEC2);

      TSourceLine.Create.annotation := 'ready[current_prio].state := suspended;';
      TPIC18x_CLRF.Create (FSR1H, access_mode);
      TPIC18x_MOVFF.Create (current_prio, FSR1L);
      TPIC18x_BCF.Create (PREINC1, ready_state_running_bit, access_mode);
      TPIC18x_SUBFSR.Create (1, 1);

      TSourceLine.Create.annotation := 'this_proc_pcb := ready[current_prio].queue;';
      TPIC18x_MOVFF.Create (INDF1, FSR1L);
      // save FSR2 in PCB
      TPIC18x_MOVFF.Create (FSR2H, PREINC1);
      TPIC18x_MOVFF.Create (FSR2L, PREINC1);
      // put FSR1 back to @this_pcb
      TPIC18x_SUBFSR.Create (1, 2);
      TAssemblySourceBlankLine.Create.annotation := 'q.delayed := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR1L, INDF0);
      // we will leave q.delayed^.next to be garbage, it will be properly set when continue'd
      // set FSR0 -> ready[current_prio]
      TPIC18x_CLRF.Create (FSR0H, access_mode);
      TPIC18x_MOVFF.Create (current_prio, FSR0L);
      TSourceLine.Create.annotation := 'if gate = ppcb($FF)';
      TPIC18x_MOVFF.Create (this_ptrH, FSR2H);
      TPIC18x_MOVFF.Create (this_ptrL, FSR2L);
      // FSR2 now contains this pointer
      TPIC18x_SUBFSR.Create (2, $3E);
      // FSR2 now points to gate
      TPIC18x_INCF.Create (INDF2, dest_w, access_mode);
      bnz1 := TPIC18x_BNZ.Create;
      TSourceLine.Create.annotation := 'then  // no other processes at gate to get into this monitor';
      TSourceLine.Create.annotation := '   begin';
      TSourceLine.Create.annotation := '      gate := nil;';
      TPIC18x_CLRF.Create (INDF2, access_mode);
      TSourceLine.Create.annotation := '      ready[current_prio].queue := this_proc_pcb^.next;';
      TPIC18x_MOVFF.Create (INDF1, INDF0);
      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);
      TSourceLine.Create.annotation := '   end';
      TSourceLine.Create.annotation := 'else  // one or more other processes are at gate to get into this monitor';
      bnz1.dest := TAssemblyLabel.Create;
      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      this_proc_pcb_next := this_proc_pcb^.next;';
      TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);  // w := this_proc_pcb^.next

      TSourceLine.Create.annotation := '      ready[current_prio].queue := gate;';
      TPIC18x_MOVFF.Create (INDF2, INDF0);

      TSourceLine.Create.annotation := '      gate := gate^.next;';
      TPIC18x_MOVFF.Create (INDF2, FSR1L);
      TPIC18x_MOVFF.Create (INDF1, INDF2);

      TSourceLine.Create.annotation := '      ready[current_prio].queue^.next := this_proc_pcb_next;';
      TPIC18x_MOVFF.Create (INDF0, FSR0L);
      TPIC18x_MOVWF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);
      TSourceLine.Create.annotation := '   end;'
   end;

procedure generate_continue_code;
   var
      br1, br2, br3, br4, br5: TPIC18x_relative_transfer;
      lbl: TAssemblyLabel;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('===================');
      TSourceLine.Create (' Kernel - Continue ');
      TSourceLine.Create ('===================');
      TAssemblySourceBlankLine.Create;

      kernel_continue.target_label := TAssemblyLabel.Create;
      TAssemblyComment.Create ('fsr0 -> q');
      TAssemblyComment.Create ('old_prio is on top of stack');

      TSourceLine.Create.annotation := 'next_proc_pcb := q;';
      TPIC18x_MOVF.Create (INDF0, dest_w, access_mode);  // sets status

      TSourceLine.Create.annotation := 'if q = nil';
      TSourceLine.Create.annotation := 'then';
      TSourceLine.Create.annotation := '   goto leave;';
      kernel_leavemon.ComeFrom (TBranchOnZeroStatusMacro.Create);

      TSourceLine.Create.annotation := 'q := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := 'ready[current_prio].state := suspended;';
      TPIC18x_CLRF.Create (FSR1H, access_mode);    // fsr1 := @ready[current_prio]
      TPIC18x_MOVFF.Create (current_prio, FSR1L);
      TPIC18x_BCF.Create (PREINC1, ready_state_running_bit, access_mode);
      TPIC18x_SUBFSR.Create (1, 1);

      TSourceLine.Create.annotation := 'this_proc_pcb := ready[current_prio].queue;';
      TPIC18x_CLRF.Create (FSR0H, access_mode);    // fsr0 := @this_proc_pcb
      TPIC18x_MOVFF.Create (INDF1, FSR0L);

      TSourceLine.Create.annotation := 'ready[current_prio].queue := next_proc_pcb;';
      TPIC18x_MOVWF.Create (INDF1, access_mode);

      TSourceLine.Create.annotation := 'next_proc_pcb^.next := this_proc_pcb^.next;';
      TPIC18x_MOVWF.Create (FSR1L, access_mode);
      TPIC18x_MOVFF.Create (INDF0, INDF1);

      TSourceLine.Create.annotation := 'this_proc_pcb^.next := nil;';
      TPIC18x_CLRF.Create (INDF0, access_mode);

      TSourceLine.Create.annotation := 'old_prio := stack.pop;';
      TPIC18x_ADDFSR.Create (2, 1);  // set fsr2 to point directly to old_prio

      TSourceLine.Create.annotation := 'this_proc_pcb^.stk_ptr_save := stack.stk_ptr;';
      TPIC18x_MOVFF.Create (FSR2H, PREINC0);      // note this_proc_pcb^.stk_ptr = @old_prio
      TPIC18x_MOVFF.Create (FSR2L, PREINC0);
      // at this point this_pcb is ready to be dispatched
      TPIC18x_MOVFF.Create (INDF2, FSR2L);  // set fsr2 -> @ready[old_prio]
      TPIC18x_CLRF.Create (FSR2H, access_mode);

      TSourceLine.Create.annotation := 'this_proc_pcb^.nesting := this_proc_pcb^.nesting - 1;';
      TPIC18x_DECF.Create (PREINC0, dest_f, access_mode);  // sets status
      TPIC18x_SUBFSR.Create (0, 3);   // reset fsr0 to beginning of this_pcb

      // fsr0 -> this_pcb
      // fsr2 -> ready[old_prio]

      TSourceLine.Create.annotation := 'if ready[old_prio].queue = nil then';
      TPIC18x_TSTFSZ.Create (INDF2, access_mode);
      br1 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      ready[old_prio].queue := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '   end';

      TSourceLine.Create.annotation := 'else if this_proc_pcb^.nesting = 0 then';
      br1.dest := TPIC18x_MOVLW.Create (pcb_nesting_offset);
      TPIC18x_TSTFSZ.Create (PLUSW0, access_mode);
      br2 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      pcb := ready[old_prio].queue;';
      TPIC18x_MOVFF.Create (INDF2, FSR2L);

      TSourceLine.Create.annotation := '      while pcb^.next <> nil';
      lbl := TAssemblyLabel.Create;
      TPIC18x_MOVF.Create (INDF2, dest_w, access_mode);
      br3 := TPIC18x_BZ.Create;

      TSourceLine.Create.annotation := '      do pcb := pcb^.next;';
      TPIC18x_MOVWF.Create (FSR2L, access_mode);
      TPIC18x_BRA.Create.dest := lbl;
      br3.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      pcb^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '   end';

      TSourceLine.Create.annotation := 'else if (old_prio = current_prio)';
      br2.dest := TAssemblyLabel.Create;
      TPIC18x_MOVF.Create (current_prio, dest_w, bank_mode);
      TPIC18x_SUBWF.Create (FSR2L, dest_w, access_mode);
      br4 := TPIC18x_BZ.Create;

      TSourceLine.Create.annotation := '        or';

      TSourceLine.Create.annotation := '        (ready[old_prio].state = preempted) then';
      TPIC18x_BTFSS.Create (PREINC2, ready_state_suspended_bit, access_mode);
      br5 := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := '   begin  // put this_proc after head of ready[old_prio].queue';
      br4.dest := TAssemblyLabel.Create;

      TSourceLine.Create.annotation := '      this_proc_pcb^.next := ready[old_prio].queue^.next;';
      TPIC18x_MOVFF.Create (INDF1, INDF0);

      TSourceLine.Create.annotation := '      ready[old_prio].queue^.next := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF1);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '   end';

      TSourceLine.Create.annotation := 'else';
      br5.dest := TAssemblyLabel.Create;

      TPIC18x_SUBFSR.Create (2, 1);  // restore FSR2 from above PREINC2

      TSourceLine.Create.annotation := '   begin  // put this_proc at head of ready[old_prio].queue';

      TSourceLine.Create.annotation := '      this_proc_pcb^.next := ready[old_prio].queue;';
      TPIC18x_MOVFF.Create (FSR1L, INDF0);

      TSourceLine.Create.annotation := '      ready[old_prio].queue := this_proc_pcb;';
      TPIC18x_MOVFF.Create (FSR0L, INDF2);

      TSourceLine.Create.annotation := '      goto dispatch';
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      TSourceLine.Create.annotation := '   end;';
   end;

function generate_interrupt_handler_code (prog: TProgram; prio: TInterruptPriorityRange): TInstruction;

   function dropped_interrupt_error_message (proc: TPIC18x_Variable): string;
      begin
         result := format (rterr_dropped_interrupt, [proc.interrupt.name])
      end;

   var
      i: integer;
      process: TPIC18x_Variable;
      lbl, wh1: TInstruction;
      bz: TPIC18x_BZ;
      first: boolean;
      idx: TProcessStateSFRs;
      bra: TPIC18x_BRA;
   begin
      result := TAssemblyLabel.Create;
      // The following will save FSR0 on the current stack.  If the system is
      // idle then the current stack will be the kernel stack in which case
      // the unneeded saved values will be discarded when the kernel stack base
      // is re-loaded before the interrupt flag polling.
      TPIC18x_MOVFF.Create (FSR0H, POSTDEC2).annotation := 'push FSR0;';
      TPIC18x_MOVFF.Create (FSR0L, POSTDEC2);

      TSourceLine.Create.annotation := 'if ready[current_prio].state = running';
      TPIC18x_MOVFF.Create (BSR, FSR0H);  // BSR is always 0 - CLRF would trash status
      TPIC18x_MOVFF.Create (current_prio, FSR0L);
      TPIC18x_BTFSS.Create (PREINC0, ready_state_running_bit, access_mode);
      bra := TPIC18x_BRA.Create;

      TSourceLine.Create.annotation := 'then';

      TSourceLine.Create.annotation := '   begin';

      TSourceLine.Create.annotation := '      ready[current_prio].state := preempted;';
      TSourceLine.Create.annotation := '      ready[current_prio].save.STATUS := STATUS;';
      TPIC18x_MOVFF.Create (STATUS, INDF0);   // also sets state to preempted 00-- ----

      // save all other SFRs
      for idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
         begin
            TSourceLine.Create.annotation := '      ready[current_prio].save.' + ProcessStateSFRAddress[idx].name + ' := ' + ProcessStateSFRAddress[idx].name + ';';
            case ProcessStateSFRAddress[idx].addr of
               WREG:
                  TPIC18x_MOVWF.Create (PREINC0, access_mode);
            else
               TPIC18x_MOVFF.Create (ProcessStateSFRAddress[idx].addr, PREINC0)
            end
         end;

      TSourceLine.Create.annotation := '      ready[current_prio].queue.stkptr := FSR2';
      TPIC18x_SUBFSR.Create (0, ProcessStateAreaSize + 1);
      TPIC18x_MOVFF.Create (INDF0, FSR0L);
      TPIC18x_MOVFF.Create (FSR2H, PREINC0);
      TPIC18x_MOVFF.Create (FSR2L, PREINC0);

      TSourceLine.Create.annotation := '   end;';
      bra.dest := TAssemblyLabel.Create;

      TSourceLine.Create ('switch to kernel stack;');
      TPIC18x_LFSR.Create (2, kernel_stack_base);

      // poll interrupt flags
      for i := 0 to prog.program_vars.Length-1 do
         if prog.program_vars[i].typedef.IsProcessSystemType
//            and
//            prog.program_vars[i].reachable
            and
            (TSystemType(prog.program_vars[i].typedef).priority = prio)
         then
            begin
               process := TPIC18x_Variable(prog.program_vars[i]);
               TSourceLine.Create ('if ' + process.interrupt.name + '.signaled');
               TPIC18x_PUSHL.Create (0);  // initialize result var
               assert (LowerCase(lex.identifiers [TPIC18x_SystemType(process.interrupt.typedef).routines[0].routine_id_idx]) = 'signaled');
               ProgramCode.AppendInlineCode (TPIC18x_Routine(TSystemType(process.interrupt.typedef).routines[0]).inline_code);
               TPIC18x_DECFSZ.Create (PREINC2, dest_w, access_mode);
               lbl := TGOTOMAcro.Create;

               TSourceLine.Create ('then');

               TSourceLine.Create ('   if ' + process.name + '.pcb.nesting <> $FF');
               TPIC18x_INCFSZ.Create (process.pcb_address + pcb_nesting_offset, dest_f, bank_mode);

               TSourceLine.Create ('   then');

               TSourceLine.Create ('      SetError (''' + dropped_interrupt_error_message(process) + ''')');
               process.dropped_interrupt_br := TGOTOMacro.Create;

               TSourceLine.Create ('   else');

               TSourceLine.Create ('      begin');

               TSourceLine.Create ('         ' + process.name + '.pcb.nesting := $00;');   //  accomplished by above INCFSZ

               TSourceLine.Create (format ('         pcb := @ready[%d].queue;', [prio]));
               TPIC18x_LFSR.Create (0, PriorityMapper.ReadyQueueAddr(prio));

               TSourceLine.Create ('         while pcb^ <> nil');
               wh1 := TPIC18x_MOVF.Create (INDF0, dest_w, access_mode);
               bz := TPIC18x_BZ.Create;

               TSourceLine.Create ('         do pcb := pcb^;');
               TPIC18x_MOVWF.Create (INDF0, access_mode);
               TPIC18x_BRA.Create.dest := wh1;

               TSourceLine.Create ('         pcb^ := @' + process.name + '.pcb');
               bz.dest := TPIC18x_MOVLW.Create (process.pcb_address);
               TPIC18x_MOVWF.Create (INDF0, access_mode);

               // assert - state will always be suspended here since this priority interrupts are off while running or preempted

               TSourceLine.Create ('      end;');
               lbl.dest := TAssemblyLabel.Create;
               process.dropped_interrupt_endif := lbl.dest
            end;

      TSourceLine.Create ('goto dispatch;');
      kernel_dispatch.ComeFrom (TGOTOMacro.Create);

      // generate out-of-line code to handle hopefully rare case of dropped interrupts
      TAssemblySourceBlankLine.Create;
      first := true;
      for i := 0 to prog.program_vars.Length-1 do
         if prog.program_vars[i].typedef.IsProcessSystemType
//            and
//            prog.program_vars[i].reachable
            and
            (TSystemType(prog.program_vars[i].typedef).priority = prio)
         then
            begin
               process := TPIC18x_Variable(prog.program_vars[i]);

               lbl := TPIC18x_DECF.Create (process.pcb_address + pcb_nesting_offset, dest_f, bank_mode);
                  // the INCFSZ test altered the nesting value, restore it.
               process.dropped_interrupt_br.dest := lbl;
               if first then
                  lbl.annotation := 'handle dropped interrupts';
               first := false;

               TPIC18x_BSF.Create (kernel_exit_flags, kef_running_at_high_priority_bit, bank_mode);
                  // this ensures that set_errorcode will RETURN rather than RETFIE
                  // this could be done once at beginning of interrupt handler, however it would
                  // be an unnecessary extra cycle most of the time.  Putting it here costs a little
                  // extra code.
               lbl := set_errorcode_routine.Call;
               lbl.annotation := '';
               RecordRunTimeErrorLocation (lbl, dropped_interrupt_error_message(process), NonExistantSourceLocation);

               TGotoMacro.Create.dest := process.dropped_interrupt_endif
            end;
   end;

procedure list_kernel_variables (prog: TProgram);
   var
      i: integer;
      idx: TProcessStateSFRs;
   begin
      TAssemblySourceBlankLine.Create;
      TSourceLine.Create ('==================');
      TSourceLine.Create (' Kernel Variables ');
      TSourceLine.Create ('==================');
      TAssemblySourceBlankLine.Create;

      TSourceLine.Create ('     type');
      TSourceLine.Create ('        prio_range =');
      if Length(PriorityMapper.priorities) = 1 then
         TSourceLine.Create (format ('           (init  {encoded $%2.2x}  // initialization', [PriorityMapper.priorities[0].ready_addr]))
      else
         TSourceLine.Create (format ('           (init, {encoded $%2.2x}  // initialization', [PriorityMapper.priorities[0].ready_addr]));
      for i := Low(PriorityMapper.priorities)+1 to High(PriorityMapper.priorities) do
         if i < High(PriorityMapper.priorities) then
            TSourceLine.Create (format ('            %4d, {encoded $%2.2x}', [PriorityMapper.priorities[i].prio, PriorityMapper.priorities[i].ready_addr]))
         else
            TSourceLine.Create (format ('            %4d  {encoded $%2.2x}', [PriorityMapper.priorities[i].prio, PriorityMapper.priorities[i].ready_addr]));
      TSourceLine.Create ('           );');

      TSourceLine.Create ('        ppcb = ^pcb;');
      TSourceLine.Create ('           { one byte pointer:         }');
      TSourceLine.Create ('           {    nil = $00              }');
      TSourceLine.Create ('           {    actual pcb''s $01..$FC  }');
      TSourceLine.Create ('           {    monitor occupied = $FF }');
      TSourceLine.Create ('        pcb =');
      TSourceLine.Create ('           record');
      TSourceLine.Create ('              next: ppcb;       { offset 0 }');
      TSourceLine.Create ('              fsr2_save: SFR; { offset H=1,L=2 }');
      TSourceLine.Create ('              nesting: 0..255   { offset 3 }');
      TSourceLine.Create ('           end;');
      TSourceLine.Create ('        process_state =');
      TSourceLine.Create ('           (suspended,  {encoded 01-- ----}');
      TSourceLine.Create ('            preempted,  {encoded 000s ssss}');
      TSourceLine.Create ('            running     {encoded 1111 1111}');
      TSourceLine.Create ('           );');
      TSourceLine.Create ('        SFR_save =');
      TSourceLine.Create ('           record');
      TSourceLine.Create ('              STATUS: uint5;');
      for idx := Low(TProcessStateSFRs) to High(TProcessStateSFRs) do
         if idx < High(TProcessStateSFRs) then
            TSourceLine.Create ('              ' + ProcessStateSFRAddress[idx].name + ': uint8;')
         else
            TSourceLine.Create ('              ' + ProcessStateSFRAddress[High(TProcessStateSFRs)].name + ': uint8');
      TSourceLine.Create ('           end;');
      TSourceLine.Create ('     var');
      TSourceLine.Create (format ('%3.3x     current_prio: prio_range;', [current_prio]));
      TSourceLine.Create (format ('%3.3x:%d:1 system_initializing: boolean;', [kernel_exit_flags, kef_system_initializing_bit]));
      TSourceLine.Create (format ('%3.3x:%d:1 running_at_high_priority: boolean;', [kernel_exit_flags, kef_running_at_high_priority_bit]));
      TSourceLine.Create (format ('%3.3x     this: ram pointer;', [this_ptrH]));
      TSourceLine.Create (format ('%3.3x     error_log: uint24;', [error_logU]));
      TSourceLine.Create ('        ready:  { an "array" of unequally sized elements }');
      TSourceLine.Create ('           array [prio_range] of');
      TSourceLine.Create (format ('%3.3x           [init] record', [PriorityMapper.ReadyQueueAddr(initial_process_priority)]));
      TSourceLine.Create ('                        queue: ppcb;');
      TSourceLine.Create ('                        state: process_state');
      TSourceLine.Create ('                     end;');
      for i := Low(PriorityMapper.priorities)+1 to High(PriorityMapper.priorities) do
         begin
            TSourceLine.Create (format ('%3.3x           [%4d] record', [PriorityMapper.priorities[i].ready_addr, PriorityMapper.priorities[i].prio]));
            if PriorityMapper.Interruptable (PriorityMapper.priorities[i].prio) then
               begin
                  TSourceLine.Create ('                        queue: ppcb;');
                  TSourceLine.Create ('                        state: process_state;');
                  TSourceLine.Create ('                        save: SFR_save');
               end
            else
               begin
                  TSourceLine.Create ('                        queue: ppcb;');
                  TSourceLine.Create ('                        state: process_state');
               end;
            if i < High(PriorityMapper.priorities) then
               TSourceLine.Create ('                     end;')
            else
               TSourceLine.Create ('                     end');
         end;
      TSourceLine.Create ('           end;');
      for i := 0 to prog.program_vars.Length-1 do
         if prog.program_vars[i].typedef.IsProcessSystemType
//            and
//            prog.program_vars[i].reachable
         then
            TSourceLine.Create (format ('%3.3x     %s_pcb: pcb;', [TPIC18x_Variable(prog.program_vars[i]).pcb_address, prog.program_vars[i].name]));
      TSourceLine.Create (format ('%3.3x     initial_process_pcb: pcb;', [initial_process_pcb_addr]))
   end;

procedure GenerateKernel (prog: TProgram);
   begin
      list_kernel_variables (prog);

      if PriorityMapper.AnyHiPriorityInterruptProcesses then
         begin
            TAssemblySourceBlankLine.Create;
            TSourceLine.Create ('==========================================');
            TSourceLine.Create (' Kernel - Handle High Priority Interrupts ');
            TSourceLine.Create ('==========================================');
            TAssemblySourceBlankLine.Create;
            hi_priority_interrupt_vector_goto.dest := generate_interrupt_handler_code (prog, high_priority_interrupt)
         end;

      if PriorityMapper.AnyLowPriorityInterruptProcesses then
         begin
            TAssemblySourceBlankLine.Create;
            TSourceLine.Create ('=========================================');
            TSourceLine.Create (' Kernel - Handle Low Priority Interrupts ');
            TSourceLine.Create ('=========================================');
            TAssemblySourceBlankLine.Create;
            low_priority_interrupt_vector_goto.dest := generate_interrupt_handler_code (prog, low_priority_interrupt)
         end;

      generate_await_interrupt_code;
      generate_dispatch_code;

{$ifdef INCLUDE_SIMULATION}
      StartKernelCoverageTest := TAssemblyLabel.Create;
{$endif}
      generate_enter_monitor_code;
      generate_leave_monitor_code;
      generate_delay_code;
      generate_continue_code;
{$ifdef INCLUDE_SIMULATION}
      EndKernelCoverageTest := TAssemblyLabel.Create
{$endif}
   end;

type
   TEnsureEEPROMWriteCompleted =
      class (TSubroutine)
      protected
         function generate_call_code: TInstruction;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         const
            push_count     = 1;
            pop_count      = 1;
            hw_stack_usage = 1;
         constructor Create;
         procedure generate_subroutine_code;
            override;
      end;

constructor TEnsureEEPROMWriteCompleted.Create;
   begin
      inherited Create (0, 0, 'ensure eeprom write completed')
   end;

function TEnsureEEPROMWriteCompleted.generate_call_code: TInstruction;
   begin
      result := TurnInterruptsOff;
      call_instruction := TCallMacro.Create;
      TPIC18x_BNZ.Create.dest := result
   end;

procedure TEnsureEEPROMWriteCompleted.generate_subroutine_code;
   begin
      // called with interrupts off, returns Z if no write in progress, z if write in progress
      TPIC18x_BSF.Create (STATUS, status_z, access_mode);  // set Z result (provisional)
      // test to see if an earlier eeprom write is still in-progress
      TPIC18x_BTFSS.Create (pic_info.EECON1, eecon1_wr, access_mode);
      TPIC18x_RETURN.Create;  // return reporting no write in progress
      // write is still in progress
      TPIC18x_BCF.Create (STATUS, status_z, access_mode);  // set z result
      ExitKernel
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TEnsureEEPROMWriteCompleted.report_stack_sizes;
   begin
      check_stack_sizes (0, 0, 1)
   end;
{$endif}

var
   EnsureEEPROMWriteCompleted: TEnsureEEPROMWriteCompleted;

// ================
//  TGetEEPROMByte

constructor TGetEEPROMByte.Create;
   begin
      inherited Create (push_count, pop_count, 'read byte from eeprom')
   end;

procedure TGetEEPROMByte.generate_subroutine_code;
   begin
      EnsureEEPROMWriteCompleted.Call;
      // at this point interrupts are off and no write is in progress
      TPIC18x_MOVFF.Create (FSR1L, pic_info.EEADR);
      TPIC18x_BCF.Create (pic_info.EECON1, eecon1_eepgd, access_mode);
      TPIC18x_BCF.Create (pic_info.EECON1, eecon1_cfgs, access_mode);
      TPIC18x_BSF.Create (pic_info.EECON1, eecon1_rd, access_mode);
      TPIC18x_MOVF.Create (pic_info.EEDATA, dest_w, access_mode);    // sets N,Z status
      TPIC18x_SUBFSR.Create (1, 1);
      ExitKernel
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TGetEEPROMByte.report_stack_sizes;
   begin
      check_stack_sizes (1, 1, 1)
   end;
{$endif}


// ================
//  TSetEEPROMByte

constructor TSetEEPROMByte.Create;
   begin
      inherited Create (push_count, pop_count, 'write byte to eeprom')
   end;

procedure TSetEEPROMByte.generate_subroutine_code;
   begin
      EnsureEEPROMWriteCompleted.Call;
      // at this point interrupts are off and no write is in progress
      TPIC18x_MOVFF.Create (FSR0L, pic_info.EEADR);
      TPIC18x_MOVWF.Create (pic_info.EEDATA, access_mode);
      TPIC18x_BCF.Create (pic_info.EECON1, eecon1_eepgd, access_mode);
      TPIC18x_BCF.Create (pic_info.EECON1, eecon1_cfgs, access_mode);
      TPIC18x_BSF.Create (pic_info.EECON1, eecon1_wren, access_mode);
      TPIC18x_MOVLW.Create ($55);
      TPIC18x_MOVWF.Create (pic_info.EECON2, access_mode);
      TPIC18x_MOVLW.Create ($AA);
      TPIC18x_MOVWF.Create (pic_info.EECON2, access_mode);
      TPIC18x_BSF.Create (pic_info.EECON1, eecon1_wr, access_mode);
      TPIC18x_BCF.Create (pic_info.EECON1, eecon1_wren, access_mode);
      TPIC18x_ADDFSR.Create (0, 1);
      ExitKernel
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TSetEEPROMByte.report_stack_sizes;
   begin
      check_stack_sizes (1, 1, 1)
   end;
{$endif}

INITIALIZATION
   InitProcessSubroutine := TInitProcessSubroutine.Create (5, 5, 'initialize a process');
   GetEEPROMByte := TGetEEPROMByte.Create;
   SetEEPROMByte := TSetEEPROMByte.Create;
   EnsureEEPROMWriteCompleted := TEnsureEEPROMWriteCompleted.Create;

FINALIZATION
   InitProcessSubroutine.Free;
   GetEEPROMByte.Free;
   SetEEPROMByte.Free;
   EnsureEEPROMWriteCompleted.Free;

END.

