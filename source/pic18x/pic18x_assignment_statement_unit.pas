UNIT pic18x_assignment_statement_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   cpc_access_unit,
   cpc_core_objects_unit,
   cpc_source_analysis_unit,
   cpc_statements_unit;

type
   TPIC18x_AssignmentStatement =
      class (TAssignmentStatement)
      private
         bytes: array of byte;
         procedure add_constant_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
      public
         constructor Create (_assignee: TAccess; _expression: TExpression; _src_loc: TSourceLocation);
         function Generate (param1, param2: integer): integer;
            override;
       end;

IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
   test_pic18x_subroutines_unit,
{$endif}
   cpc_definitions_unit,
   cpc_expressions_unit,
   cpc_multi_precision_integer_unit,
   cpc_target_cpu_unit,
   Math,
   pic18x_access_unit,
   pic18x_blocks_unit,
   pic18x_core_objects_unit,
   pic18x_cpu_unit,
   pic18x_expressions_unit,
   pic18x_floating_point_unit,
   pic18x_instructions_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   pic18x_microprocessor_information_unit,
   pic18x_run_time_error_check_unit,
   pic18x_statements_unit,
   pic18x_types_unit,
   SysUtils;

type
   Tset_ioreg_1bit_param_Subroutine =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      end;
   Tclear_ioreg_1bit_param_Subroutine =
      class (TSubroutine)
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      end;
   Tassign_ioreg_1bit_param_Subroutine =
      class (TSubroutine)
      private
         const
            // locations on stack at call
            ptrH = 1;
            ptrL = 2;
            exp_result = 3;                           
            pop_stk_size = 3;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      end;

procedure Tset_ioreg_1bit_param_Subroutine.generate_subroutine_code;
   begin
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (FSR0H, access_mode);
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      TPIC18x_SWAPF.Create (WREG, dest_f, access_mode);
      TCallMacro.Create.dest := get_bit_mask_routine;
      TPIC18x_IORWF.Create (INDF0, dest_f, access_mode);
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tset_ioreg_1bit_param_Subroutine.report_stack_sizes;
   begin
      check_stack_sizes (0, 2, 2)
   end;
{$endif}

procedure Tclear_ioreg_1bit_param_Subroutine.generate_subroutine_code;
   begin
      TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_MOVWF.Create (FSR0H, access_mode);
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      TPIC18x_SWAPF.Create (WREG, dest_f, access_mode);
      TCallMacro.Create.dest := get_bit_mask_routine;
      TPIC18x_COMF.Create (WREG, dest_w, access_mode);
      TPIC18x_ANDWF.Create (INDF0, dest_f, access_mode);
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tclear_ioreg_1bit_param_Subroutine.report_stack_sizes;
   begin
      check_stack_sizes (0, 2, 2)
   end;
{$endif}

procedure Tassign_ioreg_1bit_param_Subroutine.generate_subroutine_code;
   var
      bra: TPIC18x_BRA;
   begin
      TPIC18x_MOVSF.Create (ptrH, FSR0H);
      TPIC18x_MOVSF.Create (ptrL, FSR0L);
      TPIC18x_SWAPF.Create (ptrH, dest_w, access_mode);
      TCallMacro.Create.dest := get_bit_mask_routine;
      TPIC18x_TSTFSZ.Create (exp_result, access_mode);
      bra := TPIC18x_BRA.Create;
      TPIC18x_COMF.Create (WREG, dest_w, access_mode);
      TPIC18x_ANDWF.Create (INDF0, dest_f, access_mode);
      TPIC18x_ADDULNK.Create (pop_stk_size);
      bra.dest := TPIC18x_IORWF.Create (INDF0, dest_f, access_mode);
      TPIC18x_ADDULNK.Create (pop_stk_size)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure Tassign_ioreg_1bit_param_Subroutine.report_stack_sizes;
   begin
      check_stack_sizes (0, pop_stk_size, 2)
   end;
{$endif}

var
   set_ioreg_1bit_param_Subroutine: Tset_ioreg_1bit_param_Subroutine;
   clear_ioreg_1bit_param_Subroutine: Tclear_ioreg_1bit_param_Subroutine;
   assign_ioreg_1bit_param_Subroutine: Tassign_ioreg_1bit_param_Subroutine;

constructor TPIC18x_AssignmentStatement.Create (_assignee: TAccess; _expression: TExpression; _src_loc: TSourceLocation);
   begin
      inherited Create(assignment_statement);
      assignee := _assignee;
      _assignee.AddRef;
      expression := _expression;
      _expression.AddRef;
      src_loc := _src_loc
   end;

// The following global variables are used exclusively during the invocation of TPIC18x_AssignmentStatement.Generate
// TPIC18x_AssignmentStatement.Generate is not recursive (assignment statements are not nested) and so these variables
// may be global (rather than stack).

type
   tsrc_mode = (srcStack,     // pop from TOS
                srcGlobal,    // absolute address
                srcLocal,     // TOS relative address
                srcConstant,
                srcROM,
                srcEEPROM_viaFSR1,
                srcFSR0,
                srcFSR1
               );
   tdest_mode = (destGlobal,
                 destFSR0,
                 destFSR1,
                 destLocal,
                 destEEPROM_viaFSR0
                );

var
   src_mode: tsrc_mode;
   dest_mode: tdest_mode;
   assignee: TAccess;       // copied from base class
   expression: TExpression; // copied from base class
   src_addr, dest_addr: integer;
   assignee_size, expression_result_size: integer;

procedure copy_from_base_class (_assignee: TAccess; _expression: TExpression);
   begin
      assignee := _assignee;
      expression := _expression
   end;

procedure TPIC18x_AssignmentStatement.add_constant_byte (b: byte; byte_no: integer; path, value: string; initialization_unnecessary: boolean);
   var i: integer;
   begin
      i := Length(bytes);
      SetLength (bytes, i+1);
      bytes[i] := b
   end;

function packed_field_byte_mask (node_packed_record_field_info: TPIC18x_PackedRecordFieldInfo; byte_no: integer {1..Span}): byte;

   procedure set_bit_in_mask (bit: integer);
      begin
         case bit of
            0: result := result or $01;
            1: result := result or $02;
            2: result := result or $04;
            3: result := result or $08;
            4: result := result or $10;
            5: result := result or $20;
            6: result := result or $40;
            7: result := result or $80
         end;
      end;

   var
      i: integer;
   begin   // mask
      if node_packed_record_field_info.Span = 1 then
         begin
            // mask out bits above and below field
            result := 0;
            for i := 7 downto 0 do
               if (node_packed_record_field_info.Position-node_packed_record_field_info.Width+1 <= i) and (i <= node_packed_record_field_info.Position) then
                  set_bit_in_mask (i)
         end
      else  // Span > 1
         if byte_no = node_packed_record_field_info.Span then
            begin  // mask out bits above field
               result := 0;
               for i := 7 downto 0 do
                  if i <= node_packed_record_field_info.Position then
                     set_bit_in_mask (i)
            end
         else if byte_no = 1 then
            begin  // mask out bits below field
               result := 0;
               for i := 7 downto 0 do
                  if ((node_packed_record_field_info.Position-node_packed_record_field_info.Width+1) and $07) <= i then
                     set_bit_in_mask (i)
            end
         else
            result := $FF
   end;   // mask

function clear_bit (Position: integer): TInstruction;
   begin
      result := nil;  // supress warning
      case dest_mode of
         destGlobal:
            if dest_addr <= 255 then
               result := TPIC18x_BCF.Create (dest_addr, Position, bank_mode)
            else
               result := TPIC18x_BCF.Create (dest_addr, Position, access_mode);
         destLocal:
            result := TPIC18x_BCF.Create (dest_addr + StackUsageCounter.Current, Position, access_mode);
         destFSR0:
            result := TPIC18x_BCF.Create (INDF0, Position, access_mode);
         destFSR1:
            result := TPIC18x_BCF.Create (INDF1, Position, access_mode);
      else
         assert (false)
      end
   end;

function set_bit (Position: integer): TInstruction;
   begin
      result := nil;   // suppress warning
      case dest_mode of
         destGlobal:
            if dest_addr <= 255 then
               result := TPIC18x_BSF.Create (dest_addr, Position, bank_mode)
            else // ioreg
               result := TPIC18x_BSF.Create (dest_addr, Position, access_mode);
         destLocal:
            result := TPIC18x_BSF.Create (dest_addr + StackUsageCounter.Current, Position, access_mode);
         destFSR0:
            result := TPIC18x_BSF.Create (INDF0, Position, access_mode);
         destFSR1:
            result := TPIC18x_BSF.Create (INDF1, Position, access_mode);
      else
         assert (false)
      end
   end;

function put_wreg_then_incr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_MOVWF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_MOVWF.Create (dest_addr, access_mode);
               dest_addr := dest_addr + 1
            end;
         destLocal:
            begin
               result := TPIC18x_MOVWF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr + 1
            end;
         destEEPROM_viaFSR0:
            result := SetEEPROMByte.Call;   // increments FSR0
         destFSR0:
            result := TPIC18x_MOVWF.Create (POSTINC0, access_mode);
         destFSR1:
            result := TPIC18x_MOVWF.Create (POSTINC1, access_mode);
      else
         assert (false)
      end
   end;

function put_wreg_then_decr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_MOVWF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_MOVWF.Create (dest_addr, access_mode);
               dest_addr := dest_addr - 1
            end;
         destLocal:
            begin
               result := TPIC18x_MOVWF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr - 1
            end;
         destEEPROM_viaFSR0:
            begin
               result := SetEEPROMByte.Call;   // increments FSR0
               TPIC18x_SUBFSR.Create (0, 2)
            end;
         destFSR0:
            result := TPIC18x_MOVWF.Create (POSTDEC0, access_mode);
         destFSR1:
            result := TPIC18x_MOVWF.Create (POSTDEC1, access_mode);
      else
         assert (false)
      end
   end;

function clrf_dest_then_decr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_CLRF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_CLRF.Create (dest_addr, access_mode);
               dest_addr := dest_addr - 1
            end;
         destLocal:
            begin
               result := TPIC18x_CLRF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr - 1
            end;
         destEEPROM_viaFSR0:
            begin
               TPIC18x_MOVLW.Create (0);
               result := SetEEPROMByte.Call;   // increments FSR0
               TPIC18x_SUBFSR.Create (0, 2)
            end;
         destFSR0:
            result := TPIC18x_CLRF.Create (POSTDEC0, access_mode);
         destFSR1:
            result := TPIC18x_CLRF.Create (POSTDEC1, access_mode);
      else
         assert (false)
      end
   end;

function clrf_dest_then_incr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_CLRF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_CLRF.Create (dest_addr, access_mode);
               dest_addr := dest_addr + 1
            end;
         destLocal:
            begin
               result := TPIC18x_CLRF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr + 1
            end;
         destEEPROM_viaFSR0:
            begin
               TPIC18x_MOVLW.Create (0);
               result := SetEEPROMByte.Call   // increments FSR0
            end;
         destFSR0:
            result := TPIC18x_CLRF.Create (POSTINC0, access_mode);
         destFSR1:
            result := TPIC18x_CLRF.Create (POSTINC1, access_mode);
      else
         assert (false)
      end
   end;

function setf_dest_then_decr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_SETF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_SETF.Create (dest_addr, access_mode);
               dest_addr := dest_addr - 1
            end;
         destLocal:
            begin
               result := TPIC18x_SETF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr - 1
            end;
         destEEPROM_viaFSR0:
            begin
               TPIC18x_MOVLW.Create ($FF);
               result := SetEEPROMByte.Call;   // increments FSR0
               TPIC18x_SUBFSR.Create (0, 2)
            end;
         destFSR0:
            result := TPIC18x_SETF.Create (POSTDEC0, access_mode);
         destFSR1:
            result := TPIC18x_SETF.Create (POSTDEC1, access_mode);
      else
         assert (false)
      end
   end;

function setf_dest_then_incr_address: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            begin
               if dest_addr <= 255 then
                  result := TPIC18x_SETF.Create (dest_addr, bank_mode)
               else
                  result := TPIC18x_SETF.Create (dest_addr, access_mode);
               dest_addr := dest_addr + 1
            end;
         destLocal:
            begin
               result := TPIC18x_SETF.Create (dest_addr + StackUsageCounter.Current, access_mode);
               dest_addr := dest_addr + 1
            end;
         destEEPROM_viaFSR0:
            begin
               TPIC18x_MOVLW.Create ($FF);
               result := SetEEPROMByte.Call   // increments FSR0
            end;
         destFSR0:
            result := TPIC18x_SETF.Create (POSTINC0, access_mode);
         destFSR1:
            result := TPIC18x_SETF.Create (POSTINC1, access_mode);
      else
         assert (false)
      end
   end;

function read_destination_byte: TInstruction;
   begin
      result := nil;  // to suppress compiler warning
      case dest_mode of
         destGlobal:
            if dest_addr <= 255 then
               result := TPIC18x_MOVF.Create (dest_addr, dest_w, bank_mode)
            else
               result := TPIC18x_MOVF.Create (dest_addr, dest_w, access_mode);
         destLocal:
            result := TPIC18x_MOVF.Create (dest_addr + StackUsageCounter.Current, dest_w, access_mode);
         destEEPROM_viaFSR0:
            begin
               result := TPIC18x_MOVFF.Create (FSR0L, FSR1L);
               GetEEPROMByte.Call   // increments FSR1, but so what
            end;
         destFSR0:
            result := TPIC18x_MOVF.Create (INDF0, dest_w, access_mode);
         destFSR1:
            result := TPIC18x_MOVF.Create (INDF1, dest_w, access_mode);
      else
         assert (false)
      end
   end;


//===============================================
//  TSetSingleBitFromConstantInPackedRecordField
//===============================================

type
   TSetSingleBitFromConstantInPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         destination_mode: tdest_mode;
         destination_address: integer;
         Value: integer;
         Position: integer;
         constructor Create (_Access: TAccess; _Value: integer; _Position: integer);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TSetSingleBitFromConstantInPackedRecordField.Create (_Access: TAccess; _Value: integer; _Position: integer);
   begin
      destination_mode := dest_mode;
      destination_address := dest_addr;
      Value := _Value;
      Position := _Position;
      inherited Create (_Access, true, 0, 0, 'assign single bit from constant')
   end;

function TSetSingleBitFromConstantInPackedRecordField.generate_code_segment: TInstruction;
   begin
      assignee := TAccess(access);   // reset global variable for when called at end of block instead of inline
      dest_mode := destination_mode;
      dest_addr := destination_address;
      result := nil;  // suppress warning
      case Value of
         0: result := clear_bit (Position);
         1: result := set_bit (Position);
      else
         assert (false)
      end
   end;


//==========================================
//  TSetSingleBitFromTOSInPackedRecordField
//==========================================

type
   TSetSingleBitFromTOSInPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         destination_mode: tdest_mode;
         destination_address: integer;
         Position: integer;
         constructor Create (_Access: TAccess; _Position: integer);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TSetSingleBitFromTOSInPackedRecordField.Create (_Access: TAccess; _Position: integer);
   begin
      destination_mode := dest_mode;
      destination_address := dest_addr;
      Position := _Position;
      inherited Create (_Access, true, 0, 1, 'assign single bit from constant')
   end;

function TSetSingleBitFromTOSInPackedRecordField.generate_code_segment: TInstruction;
   begin
      assignee := TAccess(access);   // reset global variable for when called at end of block instead of inline
      dest_mode := destination_mode;
      dest_addr := destination_address;
      result := TPIC18x_BTFSS.Create (1, 0, access_mode);
      clear_bit (Position);
      TPIC18x_BTFSC.Create (PREINC2, 0, access_mode);
      set_bit (Position)
   end;


//===================================================
//  TSetMultiBitFieldFromConstantInPackedRecordField
//===================================================

type
   TSetMultiBitFieldFromConstantInPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         destination_mode: tdest_mode;
         destination_address: integer;
         Span, Position, Width, Value: integer;
         constructor Create (_Access: TAccess; _Span, _Position, _Width, _Value: integer);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TSetMultiBitFieldFromConstantInPackedRecordField.Create (_Access: TAccess; _Span, _Position, _Width, _Value: integer);
   begin
      destination_mode := dest_mode;
      destination_address := dest_addr;
      Span := _Span;
      Position := _Position;
      Width := _Width;
      Value := _Value;
      inherited Create (_Access, false, 0, 0, 'assign multi bits from constant')
   end;

function TSetMultiBitFieldFromConstantInPackedRecordField.generate_code_segment: TInstruction;
   var
      mpi: TMultiPrecisionInteger;
   function f (i: integer): TInstruction;
      begin
         if packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i) = $FF then
            case mpi.AsByte(i-1) of
               0: if TPIC18x_PackedRecordFieldInfo(assignee.node_packed_record_field.info).reversed_byte_order then
                     result := clrf_dest_then_decr_address
                  else
                     result := clrf_dest_then_incr_address;
             $FF: if TPIC18x_PackedRecordFieldInfo(assignee.node_packed_record_field.info).reversed_byte_order then
                     result := setf_dest_then_decr_address
                  else
                     result := setf_dest_then_incr_address;
            else
               begin
                  result := TPIC18x_MOVLW.Create (mpi.AsByte(i-1));
                  if TPIC18x_PackedRecordFieldInfo(assignee.node_packed_record_field.info).reversed_byte_order then
                     put_wreg_then_decr_address
                  else
                     put_wreg_then_incr_address
               end
            end
         else   // mask(i) <> $FF
            begin
               result := read_destination_byte;
               TPIC18x_ANDLW.Create (not packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i));
               if (mpi.AsByte(i-1) and packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i)) <> $00 then   // ORing $00 is a NOP
                  TPIC18x_IORLW.Create (mpi.AsByte(i-1) and packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i));
               if TPIC18x_PackedRecordFieldInfo(assignee.node_packed_record_field.info).reversed_byte_order then
                  put_wreg_then_decr_address
               else
                  put_wreg_then_incr_address
            end
      end;
   var
      i: integer;
   begin
      assignee := TAccess(access);   // reset global variable for when called at end of block instead of inline
      dest_mode := destination_mode;
      dest_addr := destination_address;
      mpi := TMultiPrecisionInteger.Create (Value);
      mpi.shift_left((Position - Width + 1) and $07);
      result := f(Span);
      for i := Span-1 downto 1 do
         f(i);
      mpi.Free
   end;


//==============================================
//  TSetMultiBitFieldFromTOSInPackedRecordField
//==============================================

type
   TSetMultiBitFieldFromTOSInPackedRecordField =
      class (TPossibleIORegOperationCodeSegment)
         destination_mode: tdest_mode;
         destination_address: integer;
         Span: integer;
         constructor Create (_Access: TAccess; _Span: integer);
         function generate_code_segment: TInstruction;
            override;
      end;

constructor TSetMultiBitFieldFromTOSInPackedRecordField.Create (_Access: TAccess; _Span: integer);
   begin
      destination_mode := dest_mode;
      destination_address := dest_addr;
      Span := _Span;
      inherited Create (_Access, false, 0, Span, 'assign multi bits from tos')
   end;

function TSetMultiBitFieldFromTOSInPackedRecordField.generate_code_segment: TInstruction;
   function f (i: integer): TInstruction;
      begin
         if packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i) = $FF then
            result := TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode)
         else
            begin
               result := TPIC18x_MOVLW.Create (packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i));
               TPIC18x_ANDWF.Create (1, dest_f, access_mode);
               read_destination_byte;
               TPIC18x_ANDLW.Create (not packed_field_byte_mask(TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info), i));
               TPIC18x_IORWF.Create (PREINC2, dest_w, access_mode)
            end;
         if TPIC18x_PackedRecordFieldInfo(assignee.node_packed_record_field.info).reversed_byte_order then
            put_wreg_then_decr_address
         else
            put_wreg_then_incr_address
      end;
   var
      i: integer;
   begin
      assignee := TAccess(access);   // reset global variable for when called at end of block instead of inline
      dest_mode := destination_mode;
      dest_addr := destination_address;
      result := f(Span);
      for i := Span-1 downto 1 do
         f(i)
   end;


//=======================================
//  TPIC18x_AssignmentStatement.Generate
//=======================================

function TPIC18x_AssignmentStatement.Generate (param1, param2: integer): integer;

   procedure load_dest (offset: integer);
      begin
         case dest_mode of
            destGlobal,
            destLocal:
               dest_addr := TPIC18x_Access(assignee).base_variable.address + TPIC18x_Access(assignee).total_fixed_offsets + offset;
            destFSR0:
               TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR0, offset);
            destFSR1:
               TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR1, offset);
            destEEPROM_viaFSR0:
               TPIC18x_Access(assignee).Generate_Load_Ptr1_Code (pFSR0, offset);
         else
            assert (false)
         end
      end;

   procedure generate_packed_field_assignment_code;
      var
         i,j: integer;
      begin    // generate_packed_field_assignment_code
         assert (assignee.node_typedef.type_kind = basic_data_type);
         assert (TBasicDataType(assignee.node_typedef).basic_data_type_kind = ordinal_data_type);
         assert (src_mode = srcStack);

         with TPIC18x_PackedRecordFieldInfo (TPIC18x_Access(assignee).node_packed_record_field.info) do
            if TPIC18x_Access(assignee).node_is_single_bit_data_packed_field then
               if expression.contains_constant then
                  begin
                     load_dest (Offset);
                     TSetSingleBitFromConstantInPackedRecordField.Create (assignee, expression.constant.AsOrdinal, Position)
                  end
               else  // single bit, not a constant
                  begin
                     expression_result_size := TPIC18x_TypeInfo (expression.info).Size;
                     expression.Generate (GenerateCode, expression_result_size);
                     GenerateRangeCheckCode (TOrdinalDataType(assignee.node_typedef), expression_result_size, expression.info, assignment_operator_src_loc, rterr_assignment_of_out_of_range_value);
                     generate_stack_fix_and_sign_extend_code (expression_result_size, 0, 1, expression.info.IntegerRange);
                     load_dest (Offset);
                     TSetSingleBitFromTOSInPackedRecordField.Create (assignee, Position)
                  end
            else  // multi-bit field
               if expression.contains_constant then
                  begin  // multi-bit field expression is constant
                     load_dest (Offset);
                     TSetMultiBitFieldFromConstantInPackedRecordField.Create (assignee, Span, Position, Width, expression.constant.AsOrdinal);
                  end    // multi-bit field expression is constant
               else
                  begin  // multi-bit field expression is non-constant
                     // stack right-normalized expression in Span bytes
                     expression_result_size := TPIC18x_TypeInfo (expression.info).Size;
                     expression.Generate (GenerateCode, expression_result_size);
                     GenerateRangeCheckCode (TOrdinalDataType(assignee.node_typedef), expression_result_size, expression.info, assignment_operator_src_loc, rterr_assignment_of_out_of_range_value);
                     generate_stack_fix_and_sign_extend_code (expression_result_size, 0, Span, expression.info.IntegerRange);

                     // left shift bits on TOS to align with packed field position
                     //    note: the following will shift garbage into lower bits
                     if (Span = 1)
                        and
                        (Position >= 4)
                        and
                        (Position - Width >= 3)
                     then
                        begin
                           TPIC18x_SWAPF.Create (1, dest_f, access_mode);
                           for i := 1 to Position - Width - 3 do
                              TPIC18x_RLCF.Create (1, dest_f, access_mode)
                        end
                     else
                        for i := 1 to (Position - (Width and $07) + 1) and $07 do
                           for j := Span downto 1 do
                              TPIC18x_RLCF.Create (j, dest_f, access_mode);

                     load_dest (Offset);
                     TSetMultiBitFieldFromTOSInPackedRecordField.Create (assignee, Span)
                  end    // multi-bit field expression is non-constant
      end;  // generate_packed_field_assignment_code

   function get_src_byte_into_wreg (postinc_needed: boolean): TInstruction;
      begin
         result := nil;  // to suppress compiler warning
         case src_mode of
            srcStack:
               begin
                  result := TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
                  StackUsageCounter.Pop (1)
               end;
            srcGlobal:
               begin
                  if src_addr <= 255 then
                     result := TPIC18x_MOVF.Create (src_addr, dest_w, bank_mode)
                  else
                     result := TPIC18x_MOVF.Create (src_addr, dest_w, access_mode);
                  src_addr := src_addr + 1
               end;
            srcLocal:
               begin
                  assert ((1 <= src_addr + StackUsageCounter.Current) and (src_addr + StackUsageCounter.Current <= $5F));
                  result := TPIC18x_MOVF.Create (src_addr + StackUsageCounter.Current, dest_w, access_mode);
                  src_addr := src_addr + 1
               end;
            srcConstant:
               assert (false);
            srcROM:
               begin
                  result := TPIC18x_TBLRD.Create (tblrd_post_inc);
                  TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode)
               end;
            srcEEPROM_viaFSR1:
               begin
                  result := GetEEPROMByte.Call;    // decrements FSR1
                  if postinc_needed then
                     TPIC18x_ADDFSR.Create (1, 2)     // net result is increment FSR1
               end;
            srcFSR0:
               result := TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
            srcFSR1:
               result := TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
         else
            assert (false)
         end
         // assert: N status is set
      end;

   function get_decremented_src_byte_into_wreg: TInstruction;
      begin
         result := nil;  // to suppress compiler warning
         case src_mode of
            srcStack:
               begin
                  result := TPIC18x_DECF.Create (PREINC2, dest_w, access_mode);
                  StackUsageCounter.Pop (1)
               end;
            srcGlobal:
               begin
                  if src_addr <= 255 then
                     result := TPIC18x_DECF.Create (src_addr, dest_w, bank_mode)
                  else
                     result := TPIC18x_DECF.Create (src_addr, dest_w, access_mode);
                  src_addr := src_addr + 1
               end;
            srcLocal:
               begin
                  assert ((1 <= src_addr + StackUsageCounter.Current) and (src_addr + StackUsageCounter.Current <= $5F));
                  result := TPIC18x_DECF.Create (src_addr + StackUsageCounter.Current, dest_w, access_mode);
                  src_addr := src_addr + 1
               end;
            srcConstant:
               assert (false);
            srcROM:
               begin
                  result := TPIC18x_TBLRD.Create (tblrd_post_inc);
                  TPIC18x_DECF.Create (TABLAT, dest_w, access_mode)
               end;
            srcEEPROM_viaFSR1:
               begin
                  result := GetEEPROMByte.Call;    // decrements FSR1
                  TPIC18x_DECF.Create (WREG, dest_w, access_mode)
               end;
            srcFSR0:
               result := TPIC18x_DECF.Create (POSTINC0, dest_w, access_mode);
            srcFSR1:
               result := TPIC18x_DECF.Create (POSTINC1, dest_w, access_mode);
         else
            assert (false)
         end
         // assert: N status is set
      end;

   function get_and_discard (count: integer): TInstruction;
      begin
         result := nil;  // to suppress compiler warning
         case src_mode of
            srcStack:
               begin
                  result := TPIC18x_ADDFSR.Create (2, count);
                  StackUsageCounter.Pop (count)
               end;
            srcGlobal,
            srcLocal,
            srcROM:
               src_addr := src_addr + count;
            srcFSR0:
               result := TPIC18x_ADDFSR.Create (0, count);
            srcFSR1,
            srcEEPROM_viaFSR1:
               result := TPIC18x_ADDFSR.Create (1, count);
         else
            assert (false)
         end
      end;

   function put_constant_byte (b: byte): TInstruction;
      begin
         result := nil;  // to suppress compiler warning
         case b of
            0: case dest_mode of
                  destGlobal:
                     begin
                        if dest_addr <= 255 then
                           result := TPIC18x_CLRF.Create (dest_addr, bank_mode)
                        else
                           result := TPIC18x_CLRF.Create (dest_addr, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destLocal:
                     begin
                        result := TPIC18x_CLRF.Create (dest_addr + StackUsageCounter.Current, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destEEPROM_viaFSR0:
                     assert (false);
                  destFSR0:
                     result := TPIC18x_CLRF.Create (POSTINC0, access_mode);
                  destFSR1:
                     result := TPIC18x_CLRF.Create (POSTINC1, access_mode);
               else
                  assert (false)
               end;
          255: case dest_mode of
                  destGlobal:
                     begin
                        if dest_addr <= 255 then
                           result := TPIC18x_SETF.Create (dest_addr, bank_mode)
                        else
                           result := TPIC18x_SETF.Create (dest_addr, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destLocal:
                     begin
                        result := TPIC18x_SETF.Create (dest_addr + StackUsageCounter.Current, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destEEPROM_viaFSR0:
                     assert (false);
                  destFSR0:
                     result := TPIC18x_SETF.Create (POSTINC0, access_mode);
                  destFSR1:
                     result := TPIC18x_SETF.Create (POSTINC1, access_mode);
               else
                  assert (false)
               end;
         else
            begin
               TPIC18x_MOVLW.Create (b);
               case dest_mode of
                  destGlobal:
                     begin
                        if dest_addr <= 255 then
                           result := TPIC18x_MOVWF.Create (dest_addr, bank_mode)
                        else
                           result := TPIC18x_MOVWF.Create (dest_addr, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destLocal:
                     begin
                        result := TPIC18x_MOVWF.Create (dest_addr + StackUsageCounter.Current, access_mode);
                        dest_addr := dest_addr + 1
                     end;
                  destEEPROM_viaFSR0:
                     SetEEPROMByte.Call;
                  destFSR0:
                     result := TPIC18x_MOVWF.Create (POSTINC0, access_mode);
                  destFSR1:
                     result := TPIC18x_MOVWF.Create (POSTINC1, access_mode);
               else
                  assert (false)
               end
            end
         end
      end;

   procedure generate_ordinal_assignment_code;
      var
         i, temp: integer;
         annotation: string;
         bn_instruction: TPIC18x_BN;
         bra_instruction: TPIC18x_BRA;
      begin
         annotation := 'do ordinal assignment';
         if expression_result_size > assignee_size then
            begin
               get_and_discard (expression_result_size-assignee_size);
               for i := 1 to assignee_size do
                  begin
                     get_src_byte_into_wreg (i < assignee_size).annotation := annotation;
                     annotation := '';
                     put_wreg_then_incr_address
                  end
            end
         else if expression_result_size = assignee_size then
            if (expression is TVariableAccessPrimary)
               and
               (TVariableAccessPrimary(expression).access.is_maxstrlen_attribute)
            then
               begin
                  assert (expression_result_size = 1);
                  get_decremented_src_byte_into_wreg.annotation := annotation;   // maxstrlen := size-1
                  annotation := '';
                  put_wreg_then_incr_address
               end
            else
               begin
                  for i := 1 to expression_result_size do
                     begin
                        get_src_byte_into_wreg (i < expression_result_size).annotation := annotation;
                        annotation := '';
                        put_wreg_then_incr_address
                     end
               end
         else  // expression_result_size < assignee_size
            case expression.info.IntegerRange of
               irAlwaysNegative:
                  begin
                     // sign extend: set upper bytes of result
                     for i := expression_result_size+1 to assignee_size do
                        begin
                           put_constant_byte (255).annotation := annotation;
                           annotation := ''
                        end;
                     for i := 1 to expression_result_size do
                        begin
                           get_src_byte_into_wreg (i < expression_result_size).annotation := annotation;
                           annotation := '';
                           put_wreg_then_incr_address
                        end
                  end;
               irNegativeOrPositive:
                  begin
                     get_src_byte_into_wreg (true).annotation := annotation;   // load expression.msb (contains sign, sets N)
                     // fill remaining (upper) bytes of result
                     // status was set by MOVF PREINC2,W,A instruction above
                     bn_instruction := TPIC18x_BN.Create;
                     temp := dest_addr;
                     for i := expression_result_size+1 to assignee_size do
                        put_constant_byte (0);
                     bra_instruction := TPIC18x_BRA.Create;
                     dest_addr := temp;
                     bn_instruction.dest := put_constant_byte (255);
                     for i := expression_result_size+2 to assignee_size do
                        put_constant_byte (255);
                     bra_instruction.dest := put_wreg_then_incr_address;  // put expression.msb
                     for i := 2 to expression_result_size do
                        begin
                           get_src_byte_into_wreg (i < expression_result_size);
                           put_wreg_then_incr_address
                        end
                  end;
               irAlwaysNonNegative:
                  begin
                     // sign extend: zero upper bytes of result
                     for i := expression_result_size+1 to assignee_size do
                        begin
                           put_constant_byte(0).annotation := annotation;
                           annotation := ''
                        end;
                     if (expression is TVariableAccessPrimary)
                        and
                        (TVariableAccessPrimary(expression).access.is_maxstrlen_attribute)
                     then
                        begin
                           assert (expression_result_size = 1);
                           get_decremented_src_byte_into_wreg.annotation := annotation;   // maxstrlen := size-1
                           annotation := '';
                           put_wreg_then_incr_address
                        end
                     else
                        for i := 1 to expression_result_size do
                           begin
                              get_src_byte_into_wreg (i < expression_result_size).annotation := annotation;
                              annotation := '';
                              put_wreg_then_incr_address
                           end
                  end;
            else
               assert (false)
            end
      end;

   procedure generate_real_assignment_code;
      var
         annotation: string;
         i: integer;
      begin
         annotation := 'do float assignment';
         for i := 1 to real_size do
            begin
               get_src_byte_into_wreg (i < real_size).annotation := annotation;
               annotation := '';
               put_wreg_then_incr_address
            end
      end;

   procedure generate_set_assignment_code;
      var
         i: integer;
         annotation: string;
      begin
         annotation := 'do set assignment';
         for i := expression_result_size+1 to assignee_size do
            begin
               put_constant_byte (0).annotation := annotation;
               annotation := ''
            end;
         for i := 1 to expression_result_size do
            begin
               get_src_byte_into_wreg (i < expression_result_size).annotation := annotation;
               annotation := '';
               put_wreg_then_incr_address
            end
      end;

   procedure generate_string_assignment_code;
      begin
         case expression.expression_kind of
            char_expression:
               begin
                  expression.Generate (GenerateCode, 1);
                  case assignee.base_variable.descriptor of
                     rw_var:
                        begin
                           TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR1, 0);
                           // set strlen to 1
                           TPIC18x_MOVLW.Create (1);
                           TPIC18x_MOVWF.Create (POSTINC1, access_mode);
                           // set s[1] to expr result
                           TPIC18x_MOVFF.Create (PREINC2, POSTINC1);
                           StackUsageCounter.Pop (1)
                        end;
                     rw_eeprom:
                        begin
                           TPIC18x_Access(assignee).Generate_Load_Ptr1_Code (pFSR0, 0);
                           // set strlen to 1
                           TPIC18x_MOVLW.Create (1);
                           SetEEPROMByte.Call;
                           // set s[1] to expr result
                           TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode);
                           StackUsageCounter.Pop (1);
                           SetEEPROMByte.Call
                        end;
                  else
                     assert (false)
                  end
               end;
            string_expression:
               case assignee.base_variable.descriptor of
                  rw_var:
                     if expression is TPIC18x_FunctionAccessPrimary then
                        begin
                           TPIC18x_Access(assignee).Generate_Push_Address2_Code (0, true);
                           expression.Generate (GenerateCode, 0);
                           TPIC18x_ADDFSR.Create (2, 3);
                           StackUsageCounter.Pop (3)
                        end
                     else
                        begin
                           TPIC18x_Access(assignee).Generate_Push_Address2_Code (0, true);
                           expression.Generate (GenerateCodeToCopyToRAMString, 0)
                        end;
                  rw_eeprom:
                     begin
                        assert (not (expression is TPIC18x_FunctionAccessPrimary));
                        // push base address
                        TPIC18x_Access(assignee).Generate_Push_Address1_Code (0, true);
                        TPIC18x_MOVF.Create (2, dest_w, access_mode);   // convert size to limit
                        TPIC18x_ADDWF.Create (1, dest_f, access_mode);  //
                        expression.Generate (GenerateCodeToCopyToEEPROMString, 0)
                     end;
               else
                  assert (false)
               end;
         else
            assert (false)
         end
      end;

   procedure generate_ioreg_1bit_param_assignment_code;
      begin
         if expression.contains_constant then
            begin
               TPIC18x_Access(assignee).Generate_Push_Address2_Code (0, false);
               if expression.ordinal_constant_value = 0 then
                  clear_ioreg_1bit_param_Subroutine.Call.annotation := 'clear ioreg bit'
               else  // const value = 1 or -1
                  set_ioreg_1bit_param_Subroutine.Call.annotation := 'set ioreg bit'
            end
         else  // non-constant expression
            begin
               expression.Generate (GenerateCode, 1);
               TPIC18x_Access(assignee).Generate_Push_Address2_Code (0, false);
               assign_ioreg_1bit_param_Subroutine.Call.annotation := 'assign ioreg bit'
            end
      end;

   procedure generate_constant_assignment_code;

      procedure put_real_constant_bytes (r: real);
         var
            pic_real: TPIC18x_Real;
            i: integer;
         begin
            pic_real.r := r;
            if assignee.node_typedef = target_cpu.get_supported_data_type ('real') then
               for i := 3 downto 0 do
                  put_constant_byte (pic_real.pic_real_AsByte[i])
            else if assignee.node_typedef = target_cpu.get_supported_data_type (ieee_single_type_name) then
               for i := 3 downto 0 do
                  put_constant_byte (pic_real.ieee_single_AsByte[i])
            else
               assert (false)
         end;

      var
         i: integer;
      begin  // generate_constant_assignment_code
         load_dest (0);
         if (assignee.node_typedef = target_cpu.get_supported_data_type ('real'))
            or
            (assignee.node_typedef = target_cpu.get_supported_data_type ('ieee_single'))
         then
            case TConstantPrimary(expression).the_constant.definition_kind of
               constant_definition:
                  put_real_constant_bytes (TConstant(TConstantPrimary(expression).the_constant).real_value);
               structured_constant_definition:
                  put_real_constant_bytes (TStructuredConstant(TConstantPrimary(expression).the_constant).simple_constant.real_value);
            else
               assert (false)
            end
         else
            case TConstantPrimary(expression).the_constant.definition_kind of
               constant_definition:
                  if (dest_mode in [destGlobal, destLocal, destFSR0, destFSR1])
                     and
                     (assignee.node_typedef.type_kind = basic_data_type)
                     and
                     (TBasicDataType(assignee.node_typedef).basic_data_type_kind = ordinal_data_type)
                     and
                     (TConstantPrimary(expression).ordinal_constant_value = 1)
                     and
                     (assignee.node_typedef.info.min_value.AsInteger = 0)
                     and
                     (assignee.node_typedef.info.max_value.AsInteger = 1)
                     and
                     (not assignee.node_initialization_assumption_invalid)   // not part of an overlay variable
                  then  // set single LSB bit optimization possible (note: clear single LSB bit already handled by single CLRF below)
                     case dest_mode of
                        destGlobal:
                           if dest_addr <= 255 then
                              TPIC18x_BSF.Create (dest_addr, 0, bank_mode)
                           else
                              TPIC18x_BSF.Create (dest_addr, 0, access_mode);
                        destLocal:
                           TPIC18x_BSF.Create (dest_addr + StackUsageCounter.Current, 0, access_mode);
                        destFSR0:
                           TPIC18x_BSF.Create (INDF0, 0, access_mode);
                        destFSR1:
                           TPIC18x_BSF.Create (INDF1, 0, access_mode);
                     else
                        assert (false)
                     end
                  else
                     for i := TPIC18x_TypeInfo(assignee.node_typedef.info).Size-1 downto 0 do
                        put_constant_byte (TConstant(TConstantPrimary(expression).the_constant).AsByte(i));
               structured_constant_definition:
                  begin
                     TPIC18x_TypeDef_TypeInfo(assignee.node_typedef.info).enumerate_constant_bytes ('', TConstantPrimary(expression).the_constant, add_constant_byte);
                     for i := 0 to Length(bytes)-1 do
                        put_constant_byte (bytes[i]);
                     SetLength (bytes, 0)
                  end;
            else
               assert (false)
            end
      end;   // generate_constant_assignment_code

   procedure generate_stack_evaluated_assignment_code;
      var
         i: integer;
         bz: TPIC18x_BZ;
         bnz: TPIC18x_BNZ;
         bnc: TPIC18x_BNC;
         lbl: TInstruction;
      begin
         case assignee.node_typedef.type_kind of
            basic_data_type:
               case TBasicDataType(assignee.node_typedef).basic_data_type_kind of
                  ordinal_data_type:
                     begin
                        expression_result_size := TPIC18x_TypeInfo (expression.info).Size;
                        assignee_size := TPIC18x_TypeInfo(assignee.node_typedef.info).Size;
                        expression.Generate (GenerateCode, expression_result_size);
                        if assignee.is_strlen_attribute
                        then
                           if assignee.base_variable.address_mode = local_indirect_address_mode
                           then
                              begin
                                 bnz := nil;
                                 if expression_result_size > 1 then
                                    begin
                                       TPIC18x_MOVF.Create (1, dest_w, access_mode);
                                       for i := 2 to expression_result_size-1 do
                                          TPIC18x_IORWF.Create (i, dest_w, access_mode);
                                       bnz := TPIC18x_BNZ.Create
                                    end;
                                 if assignee.base_variable.address+StackUsageCounter.Current-1 <= $5F then
                                    TPIC18x_MOVF.Create (assignee.base_variable.address+StackUsageCounter.Current-1, dest_w, access_mode)
                                 else
                                    begin
                                       load_ptr (pFSR2, assignee.base_variable.address+StackUsageCounter.Current-1, pFSR1);
                                       TPIC18x_MOVF.Create (INDF1, dest_w, access_mode)
                                    end;
                                 bz := TPIC18x_BZ.Create;    // br if size is 256 (i.e. maxstrlen is 255): any value of stk[expression_size] is ok
                                 TPIC18x_SUBWF.Create (expression_result_size, dest_w, access_mode);
                                 bnc := TPIC18x_BNC.Create;
                                 lbl := set_errorcode_routine.Call;
                                 if bnz <> nil then
                                    bnz.dest := lbl;
                                 RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_invalid_strlen, expression.src_loc);
                                 TPIC18x_CLRF.Create (expression_result_size, access_mode);
                                 bz.dest := TAssemblyLabel.Create;
                                 bnc.dest := bz.dest
                              end
                           else
                              GenerateRangeCheckCode (TOrdinalDataType(assignee.node_typedef), expression_result_size, expression.info, assignment_operator_src_loc, rterr_invalid_strlen)
                        else
                           GenerateRangeCheckCode (TOrdinalDataType(assignee.node_typedef), expression_result_size, expression.info, assignment_operator_src_loc, rterr_assignment_of_out_of_range_value)
                     end;
                  floating_point_data_type:
                     case expression.expression_kind of
                        integer_expression:
                           begin
                              generate_integer_expression_to_real_code (expression);
                              // tos is now pic real
                              if assignee.node_typedef = target_cpu.get_supported_data_type (ieee_single_type_name) then
                                 convert_tos_from_pic_to_ieee
                           end;
                        real_expression:
                           begin
                              expression.Generate (GenerateCode, real_size);
                              if (assignee.node_typedef = target_cpu.get_supported_data_type (ieee_single_type_name))
                                 and
                                 (not TPIC18x_Expression_TypeInfo(expression.info).is_ieee_single)
                              then
                                 convert_tos_from_pic_to_ieee
                              else if (assignee.node_typedef = target_cpu.get_supported_data_type ('real'))
                                      and
                                      (TPIC18x_Expression_TypeInfo(expression.info).is_ieee_single) then
                                 convert_tos_from_ieee_to_pic
                           end
                     else
                        assert (false)
                     end;
               else
                  assert (false)
               end;
            set_type:
               begin
                  assignee_size := TPIC18x_TypeInfo(assignee.node_typedef.info).Size;
                  expression_result_size := min (assignee_size, TPIC18x_TypeInfo (expression.info).Size);
                  expression.Generate (GenerateCode, expression_result_size)
               end;
            string_type:
               begin
                  assert (false)
               end;
         else
            assert (false)
         end;

         load_dest (0);
         // move src to destination
         case assignee.node_typedef.type_kind of
            basic_data_type:
               case TBasicDataType(assignee.node_typedef).basic_data_type_kind of
                  ordinal_data_type:
                     generate_ordinal_assignment_code;
                  floating_point_data_type:
                     generate_real_assignment_code;
               else
                  assert (false)
               end;
            set_type:
               generate_set_assignment_code;
            string_type:
               assert (false)
         else
            assert (false)
         end
      end;

   procedure generate_copy_assignment_code;
      var
         annotation: string;
         i: integer;
      begin
         if src_mode in [srcGlobal, srcLocal] then
            begin
               if TVariableAccessPrimary(expression).access.is_maxstrlen_attribute then
                  src_addr := TPIC18x_Access(TVariableAccessPrimary(expression).access).base_variable.address - 1
               else
                  src_addr := TPIC18x_Access(TVariableAccessPrimary(expression).access).base_variable.address + TPIC18x_Access(TVariableAccessPrimary(expression).access).total_fixed_offsets;

               case dest_mode of
                  destGlobal,
                  destLocal:
                     dest_addr := TPIC18x_Access(assignee).base_variable.address + TPIC18x_Access(assignee).total_fixed_offsets;
                  destFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR0, 0);
                  destFSR1:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR1, 0);
                  destEEPROM_viaFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr1_Code (pFSR0, 0);
               else
                  assert (false)
               end
            end
         else if dest_mode in [destGlobal, destLocal] then
            begin
               case src_mode of
                  srcROM:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                  srcFSR0:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pFSR0, 0);
                  srcFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pFSR1, 0);
                  srcEEPROM_viaFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr1_Code (pFSR1, 0);
               else
                  assert (false)
               end;

               dest_addr := TPIC18x_Access(assignee).base_variable.address + TPIC18x_Access(assignee).total_fixed_offsets
            end
         else if TPIC18x_Access(TVariableAccessPrimary(expression).access).indirectly_addressable_absolute_address then
            begin
               // dest ptr calc may trash fsr's
               case dest_mode of
                  destFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR0, 0);
                  destFSR1:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR1, 0);
                  destEEPROM_viaFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr1_Code (pFSR0, 0);
               else
                  assert (false)
               end;

               case src_mode of
                  srcFSR0:
                     TPIC18x_LFSR.Create (0, TPIC18x_Access(TVariableAccessPrimary(expression).access).absolute_address(0));
                  srcFSR1:
                     TPIC18x_LFSR.Create (1, TPIC18x_Access(TVariableAccessPrimary(expression).access).absolute_address(0));
               else
                  assert (false)
               end
            end
         else if TPIC18x_Access(assignee).indirectly_addressable_absolute_address then
            begin
               // src ptr calc may trash both fsr's
               case src_mode of
                  srcROM:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pTBLPTR, 0);
                  srcFSR0:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pFSR0, 0);
                  srcFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr2_Code (pFSR1, 0);
                  srcEEPROM_viaFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Load_Ptr1_Code (pFSR1, 0);
               else
                  assert (false)
               end;

               case dest_mode of
                  destFSR0:
                     TPIC18x_LFSR.Create (0, TPIC18x_Access(assignee).absolute_address(0));
                  destFSR1:
                     TPIC18x_LFSR.Create (1, TPIC18x_Access(assignee).absolute_address(0));
               else
                  assert (false)
               end
            end
         else  // assume one pointer calculation trashes the other, so use stack to save first calculation
            begin
               case src_mode of
                  srcROM,
                  srcFSR0,
                  srcFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Push_Address2_Code(0, false);
                  srcEEPROM_viaFSR1:
                     TPIC18x_Access(TVariableAccessPrimary(expression).access).Generate_Push_Address1_Code(0, false);
               else
                  assert (false)
               end;

               case dest_mode of
                  destFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR0, 0);
                  destFSR1:
                     TPIC18x_Access(assignee).Generate_Load_Ptr2_Code (pFSR1, 0);
                  destEEPROM_viaFSR0:
                     TPIC18x_Access(assignee).Generate_Load_Ptr1_Code (pFSR0, 0);
               else
                  assert (false)
               end;

               annotation := 'pop @' + TPIC18x_Access(TVariableAccessPrimary(expression).access).path_src + ' into ';
               case src_mode of
                  srcROM:
                     begin
                        TPIC18x_MOVFF.Create (PREINC2, TBLPTRH).annotation := annotation + 'TBLPTR';
                        TPIC18x_MOVFF.Create (PREINC2, TBLPTRL);
                        StackUsageCounter.Pop (2)
                     end;
                  srcEEPROM_viaFSR1:
                     begin
                        TPIC18x_MOVFF.Create (PREINC2, FSR1L).annotation := annotation + 'FSR1';
                        StackUsageCounter.Pop (1)
                     end;
                  srcFSR0:
                     begin
                        TPIC18x_MOVFF.Create (PREINC2, FSR0H).annotation := annotation + 'FSR0';
                        TPIC18x_MOVFF.Create (PREINC2, FSR0L);
                        StackUsageCounter.Pop (2)
                     end;
                  srcFSR1:
                     begin
                        TPIC18x_MOVFF.Create (PREINC2, FSR1H).annotation := annotation + 'FSR1';
                        TPIC18x_MOVFF.Create (PREINC2, FSR1L);
                        StackUsageCounter.Pop (2)
                     end;
               else
                  assert (false)
               end
            end;

         case assignee.node_typedef.type_kind of
            basic_data_type:
               begin
                  assignee_size := TPIC18x_TypeInfo(assignee.node_typedef.info).Size;
                  expression_result_size := TPIC18x_TypeInfo (expression.info).Size;
                  generate_ordinal_assignment_code
               end;
            set_type:
               begin
                  assignee_size := TPIC18x_TypeInfo(assignee.node_typedef.info).Size;
                  expression_result_size := min (assignee_size, TPIC18x_TypeInfo (expression.info).Size);
                  generate_set_assignment_code
               end;
            record_type,
            array_type,
            packed_record_type,
            overlay_type:
               begin
                  annotation := 'do copy assignment';
                  expression_result_size := TPIC18x_TypeInfo (expression.info).Size;
                  for i := 1 to expression_result_size do
                     begin
                        get_src_byte_into_wreg (i < expression_result_size).annotation := annotation;
                        annotation := '';
                        put_wreg_then_incr_address
                     end
               end;
         else
            assert (false)
         end
      end;    // generate_copy_assignment_code

   procedure generate_assignment_code;
      begin
         // tentatively assign FSR1 for both src and dest ptrs if a ptr is needed, if both need a ptr then resolve at end

         // determine src mode
         if (assignee.is_strlen_attribute)
            and
            (assignee.base_variable.address_mode = local_indirect_address_mode)
         then
            src_mode := srcStack
         else if assignee.node_is_packed_field then
            src_mode := srcStack
         else if expression.contains_constant then
            begin
               assert (expression is TConstantPrimary);
               src_mode := srcConstant
            end
         else if expression is TVariableAccessPrimary then
            begin
               if TVariableAccessPrimary(expression).access.node_is_packed_field then
                  src_mode := srcStack
               else if TPIC18x_Access(TVariableAccessPrimary(expression).access).is_overlay_variable_needing_range_check then
                  src_mode := srcStack
               else
                  case TVariableAccessPrimary(expression).access.path_start.definition_kind of
                     variable_definition:
                        if (expression.expression_kind in ordinal_expression_kinds)
                           and
                           ((assignee.node_typedef.type_kind = basic_data_type)    // should be!
                            and
                            (TBasicDataType(assignee.node_typedef).basic_data_type_kind = floating_point_data_type)
                           )
                        then  // assign int to real - will need on-stack conversion
                           src_mode := srcStack
                        else if (expression.expression_kind in ordinal_expression_kinds)
                           and
                           ((expression.info.max_value.gt (assignee.node_typedef.info.max_value))
                            or
                            (expression.info.min_value.lt (assignee.node_typedef.info.min_value))
                           )
                        then   // will need range check
                           src_mode := srcStack
                        else if (expression.expression_kind = real_expression)
                                and
                                (assignee.node_typedef <> TVariableAccessPrimary(expression).access.node_typedef) then
                           begin  // will need conversion between pic flt & ieee flt
                              src_mode := srcStack
                           end
                        else   // is not an expression, or is an expression that will not need range check, so we can simply copy value from src to dest
                           case TVariableAccessPrimary(expression).access.base_variable.descriptor of
                              rw_const,
                              rw_for,
                              rw_ioreg,
                              rw_var:
                                 case TVariableAccessPrimary(expression).access.base_variable.address_mode of
                                    absolute_address_mode:
                                       if TPIC18x_Access(TVariableAccessPrimary(expression).access).directly_addressable_absolute_address then
                                          src_mode := srcGlobal
                                       else
                                          src_mode := srcFSR1;
                                    system_type_address_mode:
                                       src_mode := srcFSR1;
                                    system_type_indirect_address_mode:
                                       src_mode := srcFSR1;
                                    local_address_mode:
                                       if TVariableAccessPrimary(expression).access.indexed then
                                          src_mode := srcFSR1
                                       else
                                          if  TPIC18x_Access(TVariableAccessPrimary(expression).access).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(assignee.node_typedef.info).Size-1) then
                                             src_mode := srcLocal
                                          else
                                             src_mode := srcFSR1;
                                    local_indirect_address_mode:
                                       if TVariableAccessPrimary(expression).access.is_maxstrlen_attribute
                                       then   // hidden maxstrlen parameter
                                          if (not TPIC18x_Access(TVariableAccessPrimary(expression).access).indexed)
                                             and
                                             (TPIC18x_Access(TVariableAccessPrimary(expression).access).base_variable.address + TPIC18x_Access(TVariableAccessPrimary(expression).access).total_fixed_offsets - 1 + StackUsageCounter.Current <= $5F)
                                          then
                                             src_mode := srcLocal
                                          else
                                             src_mode := srcFSR1
                                       else
                                          src_mode := srcFSR1;      // NEED TO DUPE ABOVE LOGIC
                                 else
                                    assert (false)
                                 end;
                              rw_eeprom:
                                 if (TVariableAccessPrimary(expression).access.is_maxstrlen_attribute)
                                    and
                                    (TVariableAccessPrimary(expression).access.base_variable.address_mode = local_indirect_address_mode)
                                 then  // hidden maxstrlen parameter
                                    if (not TPIC18x_Access(TVariableAccessPrimary(expression).access).indexed)
                                       and
                                       (TPIC18x_Access(TVariableAccessPrimary(expression).access).base_variable.address + TPIC18x_Access(TVariableAccessPrimary(expression).access).total_fixed_offsets - 1 + StackUsageCounter.Current <= $5F)
                                    then
                                       src_mode := srcLocal
                                    else
                                       src_mode := srcFSR1
                                 else
                                    src_mode := srcEEPROM_viaFSR1;
                              rw_rom:
                                 src_mode := srcROM;
                           else
                              assert (false)
                           end;
                     with_property_definition:
                        assert (false);
                     with_routine_definition:
                        assert (false);
                     with_variable_definition:
                        case TVariableAccessPrimary(expression).access.base_variable.descriptor of
                           rw_const,
                           rw_for,
                           rw_ioreg,
                           rw_var:
                              src_mode := srcFSR1;
                           rw_eeprom:
                              src_mode := srcEEPROM_viaFSR1;
                           rw_rom:
                              src_mode := srcROM;
                        else
                           assert (false)
                        end
                  else
                     assert (false)
                  end
            end  // expression is TVariableAccessPrimary
         else
            src_mode := srcStack;

         // determine dest mode
         case assignee.path_start.definition_kind of
            variable_definition:
               case assignee.base_variable.descriptor of
                  rw_const,
                  rw_ioreg,
                  rw_var:
                     case assignee.base_variable.address_mode of
                        absolute_address_mode:
                           if TPIC18x_Access(assignee).directly_addressable_absolute_address then
                              dest_mode := destGlobal
                           else
                              dest_mode := destFSR1;
                        system_type_address_mode:
                           dest_mode := destFSR1;
                        system_type_indirect_address_mode:
                           dest_mode := destFSR1;
                        local_address_mode:
                           if assignee.indexed then
                              dest_mode := destFSR1
                           else
                              if  TPIC18x_Access(assignee).near_stack_address_with_no_indexing_required_mode6(TPIC18x_TypeInfo(assignee.node_typedef.info).Size-1) then
                                 dest_mode := destLocal
                              else
                                 dest_mode := destFSR1;
                        local_indirect_address_mode:
                           dest_mode := destFSR1;
                     else
                        assert (false)
                     end;
                  rw_eeprom:
                     dest_mode := destEEPROM_viaFSR0;
               else
                  assert (false)
               end;
            with_variable_definition:
               case assignee.base_variable.descriptor of
                  rw_const,
                  rw_ioreg,
                  rw_var:
                     dest_mode := destFSR1;
                  rw_eeprom:
                     dest_mode := destEEPROM_viaFSR0;
               else
                  assert (false)
               end;
         else
            assert (false)
         end;

         // if both src and dest need a RAM ptr, resolve since both can't use FSR1
         if (src_mode in [srcFSR1, srcEEPROM_viaFSR1]) and (dest_mode = destFSR1) then
            dest_mode := destFSR0;

         if src_mode = srcConstant then
            generate_constant_assignment_code
         else if assignee.node_is_packed_field then
            generate_packed_field_assignment_code
         else if src_mode = srcStack then
            generate_stack_evaluated_assignment_code
         else
            generate_copy_assignment_code
      end;   // generate_assignement_code

   begin   // TAssignmentStatement.Generate
      result := 0;  // to suppress compiler warning
      case param1 of
         GenerateCode:
            begin
               copy_from_base_class (assignee, expression);
               TSourceSyncPoint.Create (last_token_src_loc);

               if assignee.node_property <> nil then
                  property_setter_routine_call.Generate (GenerateCode, 0)
               else if assignee.node_typedef.type_kind = string_type then
                  generate_string_assignment_code
               else if assignee.base_variable.is_ioreg_1bit_param then
                  generate_ioreg_1bit_param_assignment_code
               else
                  generate_assignment_code
            end;
      else
         assert (false, 'TPIC18x_AssignmentStatement.Generate(' + IntToStr(param1) + ') not implemented')
      end
   end;   // TAssignmentStatement.Generate

INITIALIZATION
   set_ioreg_1bit_param_Subroutine := Tset_ioreg_1bit_param_Subroutine.Create (0, 2, 'set ioreg 1-bit parameter');
   clear_ioreg_1bit_param_Subroutine := Tclear_ioreg_1bit_param_Subroutine.Create (0, 2, 'clear ioreg 1-bit parameter');
   assign_ioreg_1bit_param_Subroutine := Tassign_ioreg_1bit_param_Subroutine.Create (0, Tassign_ioreg_1bit_param_Subroutine.pop_stk_size, 'asssign ioreg 1-bit parameter');

FINALIZATION
   set_ioreg_1bit_param_Subroutine.Free;
   clear_ioreg_1bit_param_Subroutine.Free;
   assign_ioreg_1bit_param_Subroutine.Free;

END.
