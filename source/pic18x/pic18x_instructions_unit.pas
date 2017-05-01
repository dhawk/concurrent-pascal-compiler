UNIT pic18x_instructions_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   Classes,
   cpc_common_unit,
   cpc_core_objects_unit,
   cpc_definitions_unit,
   cpc_source_analysis_unit,
   pic18x_microprocessor_information_unit;

type
   THexArray = array of byte;
   TROMAddress = 0..$fffff;
   TFSR = 0..2;
   TBitNum = 0..7;
   TLargeRelativeBranchRange = -1024..1023;
   TUInt6 = 0..$3f;
   TUInt7 = 0..$7f;

   TPIC18x_Destination_Selection_Bit = (dest_f, dest_w);
   TPIC18x_RAM_Access_Mode = (access_mode, bank_mode);
   TPIC18x_Instruction_Kind = (executable_instruction,   // default - must be first (0)
                               reset_vector_instruction,
                               hi_pri_interrupt_vector_instruction,
                               kernel_instruction,
                               rom_constant_data,
                               eeprom_initialization_data,
                               config_byte_data
                              );
   TInstruction =
      class (TReferenceCountedObject)
      protected
         f_rom_addr: integer;    // address in ROM
         f_remove: boolean;
         f_dest: TInstruction;
         annotation_indent: string;
         procedure set_rom_addr (addr: integer);
            virtual;
         procedure set_remove (b: boolean);
            virtual;
         procedure sync_source (src_loc: TSourceLocation);
      public
         idx: integer;     // index in instruction_array
         size: integer;    // in bytes
         assembler_label_number: integer;
         annotation: string;
         instruction_kind: TPIC18x_Instruction_Kind;
         emit_immediately_while_extracting_inline_code: boolean;
         assembler_label_required: boolean;
{$ifdef INCLUDE_SIMULATION}
         test_coverage_index: integer;
{$endif}
         class var assembly_source_code: TStringList;
         property rom_addr: integer read f_rom_addr write set_rom_addr;
         property remove: boolean read f_remove write set_remove;
         procedure set_dest (d: TInstruction);
            virtual;
         property dest: TInstruction read f_dest write set_dest;
         constructor Create;
         constructor CreateNoLink;
         procedure UnattachFromProgramCode;
         function assembler_label: string;
         function loads_w_and_z: boolean;
            virtual;
         function hex_code: THexArray;
            virtual;
         procedure generate_assembly_code;
            virtual;
         function assembly_code: string;
            virtual;
         function assembly_line: string;
         procedure recalculate_size (var changed: boolean);
            virtual;
         class procedure output_assembly (s: string);
         class procedure output_assembly_with_label (label_char: char; label_num: integer; s: string);
         class procedure blank_line;
{$ifdef INCLUDE_SIMULATION}
         procedure execute;
         procedure pre_exec;
            virtual;
         procedure exec;
            virtual;
         procedure post_exec;
            virtual;
         procedure handle_pre (addr: TROMAddress);
         procedure handle_post (addr: TROMAddress);
{$endif}
      protected
         procedure out (s: string);
{$ifdef INCLUDE_SIMULATION}
         function do_alu_addition (op1, op2: byte; carry: boolean): byte;
         function do_alu_subtraction (op1, op2: integer; borrow: boolean): byte;
         procedure set_n_z_status (result: byte);
{$endif}
      private const
         annotation_column = 25;
      end;

   TBranchTargetBaseClass =
      class
      protected
         ftarget_label: TInstruction;
         client_list: array of TInstruction;
         procedure set_target_label (lbl: TInstruction);
      public
         function ComeFrom (instr: TInstruction): TInstruction;
            virtual;
         function has_clients: boolean;
         procedure set_client_destinations;
      end;

   TBranchTarget =
      class (TBranchTargetBaseClass)
         property target_label: TInstruction read ftarget_label write set_target_label;
      end;

   TSubroutine =
      class (TBranchTargetBaseClass)
      private
         class var compilation_counter: integer;
            // incremented each time compiler runs (needed for testing when compiler is run several
            //    times within same process instance)
         class var subroutine_list: array of TSubroutine;
      private
         compilation_counter_when_last_generated: integer;
         header: string;
         function generated: boolean;
      protected
         constructor Create (_bytes_pushed, _bytes_popped: integer; _header: string);
         function calls (subr: TSubroutine): boolean;
         function generate_call_code: TInstruction;
            virtual;
         procedure generate_subroutine_code;
            virtual; abstract;
      private
         called: boolean;
         called_subr: TSubroutine;
         additional_stack_use_evaluated: boolean;
         additional_stack_use: integer;
         __bytes_pushed, __bytes_popped, __hw_stack_use: integer;
         procedure evaluate_additional_stack_use;
      protected
         call_instruction: TInstruction;
         type Tinitial_stack_level_range = -999..0;
         procedure enumerate_sub_subroutine_usage;
            virtual;
         procedure evaluate_called_subroutine_stack_use (subroutine: TSubroutine; initial_stack_level: Tinitial_stack_level_range);
         procedure evaluate_gotoed_subroutine_stack_use (subroutine: TSubroutine; initial_stack_level: Tinitial_stack_level_range);
         function bytes_pushed: integer;
         function hw_stack_use: integer;
{$ifdef INCLUDE_SIMULATION}
         procedure check_stack_sizes (sw_push_count, sw_pop_count, hw_count: integer);
         procedure report_stack_sizes;
            virtual;
{$endif}
      public
         class procedure increment_compilation_counter;
         function ComeFrom (instr: TInstruction): TInstruction;
            override;
         function Call: TInstruction;
         class procedure generate_subroutines;
{$ifdef INCLUDE_SIMULATION}
         class procedure check_stack_calculations;
{$endif}
      end;

   TDataByteArray = array of byte;

   TPIC18x_DB_Packed =
      class (TInstruction)
         bytes: TDataByteArray;
         constructor Create (_byte: byte);
            overload;
         constructor Create (_bytes: TDataByteArray);
            overload;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_DB =
      class (TPIC18x_DB_Packed)
         constructor Create (_byte: byte);
            overload;
         constructor Create (_bytes: TDataByteArray);
            overload;
         function hex_code: THexArray;
            override;
      end;

   TPIC18x_DE =
      class (TPIC18x_DB_Packed)
         constructor Create (_byte: byte);
            overload;
         constructor Create (_bytes: TDataByteArray);
            overload;
      end;

   TPIC18x_CONFIG =
      class (TInstruction)
      private const
         src_base = 'CONFIG ';
      public
         b: byte;
         sc: TStructuredConstant;
         constructor Create (_b: byte; _sc: TStructuredConstant);
            overload;
         destructor Destroy;
            override;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
         function IncludeInSourceFile: boolean;
      end;


   TPIC18x_DW =
      class (TInstruction)
      private
         type t_byte_src =
            record
               from_labelL,
               from_labelH,
               from_labelU: TInstruction
            end;
         var
            w: word;
            msb_src, lsb_src: t_byte_src;
            db_mode: boolean;
         procedure set_msb (b: byte);
         procedure set_msb_from_labelL (instr: TInstruction);
         procedure set_msb_from_labelH (instr: TInstruction);
         procedure set_msb_from_labelU (instr: TInstruction);
         procedure set_lsb (b: byte);
         procedure set_lsb_from_labelL (instr: TInstruction);
         procedure set_lsb_from_labelH (instr: TInstruction);
         procedure set_lsb_from_labelU (instr: TInstruction);
      public
         property msb: byte write set_msb;
         property msb_from_labelL: TInstruction write set_msb_from_labelL;
         property msb_from_labelH: TInstruction write set_msb_from_labelH;
         property msb_from_labelU: TInstruction write set_msb_from_labelU;
         property lsb: byte write set_lsb;
         property lsb_from_labelL: TInstruction write set_lsb_from_labelL;
         property lsb_from_labelH: TInstruction write set_lsb_from_labelH;
         property lsb_from_labelU: TInstruction write set_lsb_from_labelU;
         constructor Create (_word: word);
            overload;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
         procedure set_dest;  // to hide ancestor
            reintroduce;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_DW_Packed =
      class (TPIC18x_DW)
         constructor Create (_word: word);
            overload;
      end;

   TPIC18x_Access_Instruction =
      class (TInstruction)
         mnemonic: string;
         opcode: byte;
         f: byte;
         sfr_addr: integer;
         a: TPIC18x_RAM_Access_Mode;
         constructor Create (_mnemonic: string; _opcode: byte; _f: integer; _a: TPIC18x_RAM_Access_Mode);
         function assembly_code: string;
            override;
         function hex_code: THexArray;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure pre_exec;
            override;
         procedure post_exec;
            override;
{$endif}
      protected
{$ifdef INCLUDE_SIMULATION}
         function effective_addr: integer;
         procedure put_result (b: byte);
            virtual;
{$endif}
      end;

   TPIC18x_byte_oriented_operations_fda =
      class (TPIC18x_Access_Instruction)
         d: TPIC18x_Destination_Selection_Bit;
         sets_z_status: boolean;
         constructor Create (_mnemonic: string; _opcode: byte; _f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode; _sets_z_status: boolean);
         function loads_w_and_z: boolean;
            override;
         function assembly_code: string;
            override;
         function hex_code: THexArray;
            override;
      protected
{$ifdef INCLUDE_SIMULATION}
         procedure put_result (b: byte);
            override;
{$endif}
      end;

   TPIC18x_literal_operations =
      class (TInstruction)
         mnemonic: string;
         opcode: byte;
         f: byte;
         sets_w_and_z_status: boolean;
         constructor Create (_mnemonic: string; _opcode: byte; _f: byte; _sets_w_and_z_status: boolean);
         function assembly_code: string;
            override;
         function loads_w_and_z: boolean;
            override;
         function hex_code: THexArray;
            override;
      end;

   TPIC18x_relative_transfer =
      class (TInstruction)
         mnemonic: string;
         opcode: byte;
         // fill in either dest or n_field (dest overrides n_field)
         n_field: integer;
         low_n, hi_n: integer;
         constructor Create (_mnemonic: string; _opcode: byte; _low_n, _hi_n: integer);
         procedure set_dest (d: TInstruction);
            override;
         function assembly_code: string;
            override;
         function hex_code: THexArray;
            override;
         function calculate_n: integer;
         function in_range: boolean;
      end;

   TPIC18x_conditional_branch =
      class (TPIC18x_relative_transfer)
         constructor Create (_mnemonic: string; _opcode: byte);
      end;

   TPIC18x_bit_oriented_operations =
      class (TPIC18x_Access_Instruction)
         bit_num: TBitNum;
         constructor Create (_mnemonic: string; _opcode: byte; _f: integer; _bit_num: TBitNum; _a: TPIC18x_RAM_Access_Mode);
         function assembly_code: string;
            override;
         function hex_code: THexArray;
            override;
         function bit: byte;
      end;

   TPIC18x_ADDWF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_ADDWFC =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_ANDWF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_COMF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_DECF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_DECFSZ =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_DCFSNZ =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_INCF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_INCFSZ =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_INFSNZ =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_IORWF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RLCF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RLNCF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RRCF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RRNCF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBFWB =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBWF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBWFB =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SWAPF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_XORWF =
      class (TPIC18x_byte_oriented_operations_fda)
         constructor Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CLRF =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CPFSEQ =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CPFSGT =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CPFSLT =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVWF =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MULWF =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_NEGF =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SETF =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_TSTFSZ =
      class (TPIC18x_Access_Instruction)
         constructor Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVFF =
      class (TInstruction)
         fs, fd: TDataMemoryAddress;
         constructor Create (_fs, _fd: TDataMemoryAddress);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
         procedure pre_exec;
            override;
         procedure post_exec;
            override;
{$endif}
      end;

   TPIC18x_ADDLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_ANDLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_IORLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVLB =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MULLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RETLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_XORLW =
      class (TPIC18x_literal_operations)
         constructor Create (_f: byte);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_LFSR =
      class (TInstruction)
         fsr: TFSR;
         addr: TDataMemoryAddress;
         constructor Create (_fsr: TFSR; _addr: TDataMemoryAddress);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BCF =
      class (TPIC18x_bit_oriented_operations)
         constructor Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BSF =
      class (TPIC18x_bit_oriented_operations)
         constructor Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BTFSC =
      class (TPIC18x_bit_oriented_operations)
         constructor Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BTFSS =
      class (TPIC18x_bit_oriented_operations)
         constructor Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BTG =
      class (TPIC18x_bit_oriented_operations)
         constructor Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BC =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BN =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BNC =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BNN =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BNOV =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BNZ =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BOV =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BZ =
      class (TPIC18x_conditional_branch)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_BRA =
      class (TPIC18x_relative_transfer)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RCALL =
      class (TPIC18x_relative_transfer)
         constructor Create;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SLEEP =
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CLRWDT =
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_RETURN =           // note S bit not implemented
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_NOP =
      class (TInstruction)
{$ifdef INCLUDE_SIMULATION}
         subtest: integer;
         test_kind: (test_rq, test_mon_gate, test_not_implemented);
         msg: string;
         check_values:
            array of
               record
                  addr, expected_value: integer;
                  error_message: string;
                  v: TVariable;
                  state_test: boolean;
                  monitor_name: string
               end;
{$endif}
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
         procedure add_check_value (addr, expected_value: integer; error_msg: string);
            overload;
         procedure add_check_value (v: TVariable; expected_value: integer; error_msg: string);
            overload;
         procedure check_state_value (addr: integer; state: char);
         destructor Destroy;
            override;
{$endif}
      end;

   TPIC18x_RETFIE =           // note S bit not implemented
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_PUSH =
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_POP =
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CALL =       // S bit not implemented
      class (TInstruction)
         n_field: integer;
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_GOTO =
      class (TInstruction)
         n_field: integer;
         constructor Create;
         procedure set_dest (d: TInstruction);
            override;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_TBLRD_Mode = (tblrd, tblrd_post_inc, tblrd_post_dec, tblrd_pre_inc);
   TPIC18x_TBLRD =
      class (TInstruction)
         mode: TPIC18x_TBLRD_Mode;
         constructor Create (_mode: TPIC18x_TBLRD_Mode);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_ADDFSR =
      class (TInstruction)
         f: TFSR;
         k: TUint6;
         constructor Create (_f: TFSR; _k: TUint6);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_ADDULNK =
      class (TInstruction)
         k: TUint6;
         constructor Create (_k: TUint6);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_CALLW =
      class (TInstruction)
         constructor Create;
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_MOVSF =
      class (TInstruction)
         zs: TUInt7;
         fd: TDataMemoryAddress;
         constructor Create (_zs: TUInt7; _fd: TDataMemoryAddress);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
         procedure pre_exec;
            override;
         procedure post_exec;
            override;
{$endif}
      end;

   TPIC18x_MOVSS =
      class (TInstruction)
         zs, zd: TUInt7;
         constructor Create (_zs, _zd: TUInt7);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_PUSHL =
      class (TInstruction)
         k: byte;
         constructor Create (_k: byte);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBFSR =
      class (TInstruction)
         f: TFSR;
         k: TUInt6;
         constructor Create (_f: TFSR; _k: TUInt6);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_SUBULNK =
      class (TInstruction)
         k: TUInt6;
         constructor Create (_k: TUInt6);
         function hex_code: THexArray;
            override;
         function assembly_code: string;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure exec;
            override;
{$endif}
      end;

   TPIC18x_Data =
      class (TInstruction)
      protected
         define_data_operator: string;
         assign_operator: string;
         identifier: string;
         structured_constant: TStructuredConstant;
         decl_end_src_loc: TSourceLocation;
         constructor Create (_define_data_operator, _assign_operator: string; _identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
      public
         destructor Destroy;
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

   TPIC18x_ROM_Data =
      class (TPIC18x_Data)
         constructor Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
      end;

   TPIC18x_Data_Initialization =
      class (TPIC18x_Data)
         constructor Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
      end;

   TPIC18x_EEPROM_Data =
      class (TPIC18x_Data)
         constructor Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
      end;

   TInterruptsOffCodeSegment =
      class
      private
         annotation: string;
         call_instruction: TInstruction;
      protected
         function do_code_generation: TInstruction;
            virtual;
      public
         constructor Create (stack_push_count, stack_pop_count: integer;
                             _annotation: string
                            );
         function generate_code_segment: TInstruction;
            virtual; abstract;
         function always_inline_regardless_of_priority: boolean;
            virtual;
      end;

   TPossibleIORegOperationCodeSegment =
      class (TInterruptsOffCodeSegment)
      protected
         access: TDefinition;
         function do_code_generation: TInstruction;
            override;
      public
         access_is_via_a_single_instruction: boolean;
         constructor Create (_access: TDefinition;
                             _is_single_instruction: boolean;
                             stack_push_count: integer;
                             stack_pop_count: integer;
                             _annotation: string
                            );
         function always_inline_regardless_of_priority: boolean;
            override;
         destructor Destroy;
            override;
      end;

procedure GenerateInterruptsOffCodeSegmentsAsSubroutines;
function MarkInterruptsOffCodeSegments: integer;
procedure CutbackInterruptsOffCodeSegments (mark: integer);

const
   hex_char: array [0..15] of char = '0123456789ABCDEF';

var
   current_source_idx: integer;

function byte_to_hex_string (b: byte): string;
function word_to_hex_string (w: word): string;
function ram_addr_to_hex_string (addr: TDataMemoryAddress): string;
function rom_addr_to_hex_string (addr: integer): string;

type
   TPointer = (pFSR0, pFSR1, pFSR2, pTBLPTR, pTHIS, pTOS);

function ram_access_mode (ptr: TPointer): TPIC18x_RAM_Access_Mode;
function ptrL (ptr: TPointer): integer;
function ptrH (ptr: TPointer): integer;
function fsr (ptr: TPointer): integer;
function indf (ptr: TPointer): integer;
function plusw (ptr: TPointer): integer;
function postinc (ptr: TPointer): integer;
function load_ptr (src_ptr: TPointer; offset: integer; dest_ptr: TPointer): TInstruction;
function adjust_fsr (ptr: TPointer; offset: integer): TInstruction;

function usb (w: integer): integer;
function msb (w: integer): integer;
function lsb (w: integer): integer;

IMPLEMENTATION

uses
{$ifdef INCLUDE_SIMULATION}
   test_pic18x_kernel_unit,
   test_pic18x_simulator_unit,
   test_pic18x_subroutines_unit,
{$endif}
   cpc_blocks_unit,
   cpc_target_cpu_unit,
   cpc_types_unit,
   Math,
   pic18x_access_unit,
   pic18x_blocks_unit,
   pic18x_common_unit,
   pic18x_core_objects_unit,
   pic18x_cpu_unit,
   pic18x_floating_point_unit,
   pic18x_kernel_unit,
   pic18x_macro_instructions_unit,
   SysUtils;

var
   InterruptsOffCodeSegments: array of TInterruptsOffCodeSegment;

constructor TInterruptsOffCodeSegment.Create (stack_push_count, stack_pop_count: integer; _annotation: string);
   var
      i: integer;
      prio2: boolean;
   begin
      i := Length(InterruptsOffCodeSegments);
      SetLength (InterruptsOffCodeSegments, i+1);
      InterruptsOffCodeSegments[i] := self;

      prio2 := false;  // suppress compiler warning
      case current_block.definition_kind of
         program_definition:
            prio2 := true;   // initial process initialization is run with interrupts off
         routine_definition:
            case TRoutine(current_block).context.definition_kind of
               program_definition:
                  prio2 := false;  // free-standing proc/func called from anywhere
               type_definition:
                  begin
                     assert (TTypeDef(TRoutine(current_block).context).type_kind = system_type);
                     case TSystemType(TRoutine(current_block).context).system_type_kind of
                        class_system_type:
                           prio2 := false;   // class can be called from anywhere
                        monitor_system_type,
                        process_system_type,
                        interrupt_system_type:
                           prio2 := TSystemType(TRoutine(current_block).context).priority = 2;
                     else
                        assert (false)
                     end
                  end;
            else
               assert (false)
            end;
         type_definition:
            begin
               assert (TTypeDef(current_block).type_kind = system_type);
               case TSystemType(current_block).system_type_kind of
                  class_system_type:
                     prio2 := false;     // class can be initialized from anywhere
                  monitor_system_type,
                  interrupt_system_type:
                     prio2 := true;    // initial statements are executed with interrupts off
                  process_system_type:
                     prio2 := TSystemType(current_block).priority = 2;
               else
                  assert (false)
               end
            end;
      else
         assert (false)
      end;

      if (prio2)   // interrupts are already off,
         or
         always_inline_regardless_of_priority
      then
         do_code_generation   // generate code in-line
      else
         begin
            annotation := _annotation;
            TurnInterruptsOff;
            call_instruction := TCallMacro.Create
         end;

      StackUsageCounter.Push (stack_push_count);
      StackUsageCounter.Pop (stack_pop_count)
   end;

function TInterruptsOffCodeSegment.do_code_generation: TInstruction;
   begin
      result := generate_code_segment
   end;

function TInterruptsOffCodeSegment.always_inline_regardless_of_priority: boolean;
   begin
      result := false
   end;

constructor TPossibleIORegOperationCodeSegment.Create (_access: TDefinition;
                                                       _is_single_instruction: boolean;
                                                       stack_push_count: integer;
                                                       stack_pop_count: integer;
                                                       _annotation: string
                                                      );
   begin
      access := _access;
      access.AddRef;
      access_is_via_a_single_instruction := _is_single_instruction;
      inherited Create (stack_push_count, stack_pop_count, _annotation)
   end;

function TPossibleIORegOperationCodeSegment.do_code_generation: TInstruction;
   begin
      if TPIC18x_TypeInfo(TPIC18x_Access(access).base_variable.typedef.info).is_in_alternate_shared_address_space then
         begin
            result := TPIC18x_BSF.Create (pic_info.SFR_Address('WDTCON'), 4, access_mode);
            generate_code_segment;
            TPIC18x_BCF.Create (pic_info.SFR_Address('WDTCON'), 4, access_mode)
         end
      else
         result := generate_code_segment
   end;

function TPossibleIORegOperationCodeSegment.always_inline_regardless_of_priority: boolean;
   begin
      if TPIC18x_Access(access).base_variable.descriptor <> rw_ioreg then
         result := true
      else
         result := access_is_via_a_single_instruction     // single instructions are non-interruptable
                   and
                   (not TPIC18x_TypeInfo(TPIC18x_Access(access).base_variable.typedef.info).is_in_alternate_shared_address_space)
   end;

destructor TPossibleIORegOperationCodeSegment.Destroy;
   begin
      access.Release;
      inherited
   end;

procedure GenerateInterruptsOffCodeSegmentsAsSubroutines;
   var
      i: integer;
      instr: TInstruction;
   begin
      if Length(InterruptsOffCodeSegments) > 0 then
         TAssemblySourceBlankLine.Create;
      for i := 0 to Length(InterruptsOffCodeSegments)-1 do
         begin
            if InterruptsOffCodeSegments[i].call_instruction <> nil then
               begin
                  instr := InterruptsOffCodeSegments[i].do_code_generation;
                  InterruptsOffCodeSegments[i].call_instruction.dest := instr;
                  instr.annotation := InterruptsOffCodeSegments[i].annotation;
                  ExitKernel;
                  TAssemblySourceBlankLine.Create
               end;
            InterruptsOffCodeSegments[i].Free
         end;
      SetLength (InterruptsOffCodeSegments, 0)
   end;

function MarkInterruptsOffCodeSegments: integer;
   begin
      result := Length(InterruptsOffCodeSegments)
   end;

procedure CutbackInterruptsOffCodeSegments (mark: integer);
   var
      i: integer;
   begin
      for i := mark to Length(InterruptsOffCodeSegments)-1
         do InterruptsOffCodeSegments[i].Free;
      SetLength (InterruptsOffCodeSegments, mark)
   end;


function usb (w: integer): integer;
   begin
      result := (w and $ff0000) shr 16
   end;

function msb (w: integer): integer;
   begin
      result := (w and $ff00) shr 8
   end;

function lsb (w: integer): integer;
   begin
      result := w and $ff
   end;

procedure TInstruction.sync_source (src_loc: TSourceLocation);
   begin
      append_source_up_to (assembly_source_code, src_loc)
   end;

function byte_to_hex_string (b: byte): string;
   begin
      result := '0??h';
      result[2] := hex_char[b div 16];
      result[3] := hex_char[b and $f]
   end;

function word_to_hex_string (w: word): string;
   begin
      result := '0????h';
      result[2] := hex_char[w shr 12];
      result[3] := hex_char[(w shr 8) and $f];
      result[4] := hex_char[(w shr 4) and $f];
      result[5] := hex_char[w and $f]
   end;

function ram_addr_to_hex_string (addr: TDataMemoryAddress): string;
   begin
      result := pic_info.SFR_Name (addr)
   end;

function rom_addr_to_hex_string (addr: integer): string;
   begin
      result := '0?????h';
      result[2] := hex_char[(addr div 65536) and $f];
      result[3] := hex_char[(addr div 4096) and $f];
      result[4] := hex_char[(addr div 256) and $f];
      result[5] := hex_char[(addr div 16) and $f];
      result[6] := hex_char[addr and $f]
   end;

function ls_nibble (b: byte): byte;
   begin
      result := b and $0f
   end;

function ram_access_mode (ptr: TPointer): TPIC18x_RAM_Access_Mode;
   begin
      result := access_mode;  // suppress compiler warning
      case ptr of
         pFSR0,
         pFSR1,
         pFSR2,
         pTBLPTR,
         pTOS:
            result := access_mode;
         pTHIS:
            result := bank_mode;
      else
         assert (false)
      end
   end;

function ptrL (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := FSR0L;
         pFSR1:
            result := FSR1L;
         pFSR2:
            result := FSR2L;
         pTBLPTR:
            result := TBLPTRL;
         pTHIS:
            result := this_ptrL;
         pTOS:
            result := 2;
      else
         assert (false)
      end
   end;

function ptrH (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := FSR0H;
         pFSR1:
            result := FSR1H;
         pFSR2:
            result := FSR2H;
         pTBLPTR:
            result := TBLPTRH;
         pTHIS:
            result := this_ptrH;
         pTOS:
            result := 1;
      else
         assert (false)
      end
   end;

function fsr (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := 0;
         pFSR1:
            result := 1;
         pFSR2:
            result := 2;
      else
         assert (false)
      end
   end;

function indf (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := INDF0;
         pFSR1:
            result := INDF1;
         pFSR2:
            result := INDF2;
      else
         assert (false)
      end
   end;

function plusw (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := PLUSW0;
         pFSR1:
            result := PLUSW1;
         pFSR2:
            result := PLUSW2;
      else
         assert (false)
      end
   end;

function postinc (ptr: TPointer): integer;
   begin
      result := -1;  // suppress compiler warning
      case ptr of
         pFSR0:
            result := POSTINC0;
         pFSR1:
            result := POSTINC1;
         pFSR2:
            result := POSTINC2;
      else
         assert (false)
      end
   end;

function load_ptr (src_ptr: TPointer; offset: integer; dest_ptr: TPointer): TInstruction;
   begin
      if offset = 0 then
         begin  // can be done in 4 instruction cycles
            result := TPIC18x_MOVFF.Create (ptrL(src_ptr), ptrL(dest_ptr));
            TPIC18x_MOVFF.Create (ptrH(src_ptr), ptrH(dest_ptr))
         end
      else if (dest_ptr in [pFSR0, pFSR1, pFSR2])
              and
              (-$3F <= offset) and (offset <= $3F)
           then
              begin  // can be done in 5 instruction cycles
                 result := TPIC18x_MOVFF.Create (ptrL(src_ptr), ptrL(dest_ptr));
                 TPIC18x_MOVFF.Create (ptrH(src_ptr), ptrH(dest_ptr));
                 if offset < 0 then
                    TPIC18x_SUBFSR.Create (fsr(dest_ptr), -offset)
                 else if offset > 0 then
                    TPIC18x_ADDFSR.Create (fsr(dest_ptr), offset)
              end
      else
         begin  // // can be done in 6 instruction cycles
            result := TPIC18x_MOVLW.Create (lsb(offset));
            TPIC18x_ADDWF.Create (ptrL(src_ptr), dest_w, ram_access_mode(src_ptr));
            TPIC18x_MOVWF.Create (ptrL(dest_ptr), access_mode);
            TPIC18x_MOVLW.Create (msb(offset));
            TPIC18x_ADDWFC.Create (ptrH(src_ptr), dest_w, ram_access_mode(src_ptr));
            TPIC18x_MOVWF.Create (ptrH(dest_ptr), access_mode)
         end
   end;

function adjust_fsr (ptr: TPointer; offset: integer): TInstruction;
   var i: TInstruction;
   begin
      result := nil;
      if offset > $3F * 4 then
         begin
            result := TPIC18x_MOVLW.Create (lsb(offset));
            TPIC18x_ADDWF.Create (ptrL(ptr), dest_f, access_mode);
            TPIC18x_MOVLW.Create (msb(offset));
            TPIC18x_ADDWFC.Create (ptrH(ptr), dest_f, access_mode)
         end
      else if offset > 0 then
         repeat
            i := TPIC18x_ADDFSR.Create (fsr(ptr), min($3F,offset));
            if result = nil then
               result := i;
            offset := offset - min($3F,offset)
         until offset = 0
      else if offset < -$3f * 4 then
         begin
            result := TPIC18x_MOVLW.Create (lsb(-offset));
            TPIC18x_SUBWF.Create (ptrL(ptr), dest_f, access_mode);
            TPIC18x_MOVLW.Create (msb(-offset));
            TPIC18x_SUBWFB.Create (ptrH(ptr), dest_f, access_mode)
         end
      else if offset < 0 then
         repeat
            i := TPIC18x_SUBFSR.Create (fsr(ptr), -max(-$3F,offset));
            if result = nil then
               result := i;
            offset := offset - max(-$3F,offset)
         until offset = 0
   end;


//=============
// Instruction

constructor TInstruction.Create;
   begin
      inherited Create;
      ProgramCode.AppendInstruction (self)
   end;

constructor TInstruction.CreateNoLink;
   begin
      inherited Create
   end;

procedure TInstruction.UnattachFromProgramCode;
   begin
      ProgramCode.UnappendInstruction (self)
   end;

function TInstruction.loads_w_and_z: boolean;
   begin
      result := false
   end;

procedure TInstruction.set_remove (b: boolean);
   begin
      f_remove := true;
      size := 0
   end;

procedure TInstruction.set_dest (d: TInstruction);
   begin
      assert (false)
   end;

procedure TInstruction.generate_assembly_code;
   begin
      out (assembly_code)
   end;

function TInstruction.assembly_code: string;
   begin
      result := ''
   end;

function TInstruction.assembler_label: string;
   begin
      result := 'L' + format('%6.6d', [assembler_label_number])
   end;

// label column constants (all should be same width)
const
   label_fmt_string = '%6.6d ';
   blank_label_column   = '        ';
   removed_label_column = ';opt    ';

class procedure TInstruction.output_assembly (s: string);
   begin
      assembly_source_code.Add (blank_label_column + s)
   end;

class procedure TInstruction.output_assembly_with_label (label_char: char; label_num: integer; s: string);
   begin
      assembly_source_code.Add (label_char + format(label_fmt_string, [label_num]) + s)
   end;

class procedure TInstruction.blank_line;
   begin
      if assembly_source_code.Strings[assembly_source_code.Count-1] <> '' then
         assembly_source_code.Add('')
   end;

function TInstruction.assembly_line: string;
   var
      s: string;
   begin
      s := assembly_code;
      if assembler_label_required then
         begin
            assert (not f_remove);
            result := 'L' + format(label_fmt_string, [assembler_label_number]) + s
         end
      else
         if f_remove
         then
            result := removed_label_column + s
         else
            result := blank_label_column + s
   end;

procedure TInstruction.out (s: string);
   begin
      if annotation <> '' then
         assembly_source_code.Add ('; ' + annotation);
      if assembler_label_required then
         begin
            assert (not f_remove);
            output_assembly_with_label ('L', assembler_label_number, s)
         end
      else
         if f_remove
         then
            assembly_source_code.Add (removed_label_column + s)
         else
            output_assembly (s)
   end;

procedure TInstruction.recalculate_size (var changed: boolean);
   begin
      // default is no size change
   end;

function TInstruction.hex_code: THexArray;
   begin
      SetLength (result, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TInstruction.execute;
   begin
      if test_coverage_index > 0 then
         KernelInstructions[test_coverage_index].execution_count := KernelInstructions[test_coverage_index].execution_count + 1;
      if cpu.pc = rom_addr then
         pre_exec;
      exec;
      if cpu.pc = rom_addr + size then
         post_exec
   end;

procedure TInstruction.pre_exec;
   begin
   end;

procedure TInstruction.exec;
   begin
      assert (false)  // should have been overriden in base class
   end;

procedure TInstruction.post_exec;
   begin
   end;

procedure TInstruction.handle_pre (addr: TROMAddress);
   begin
      case addr of
         PREINC0:
            cpu.fsr0 := cpu.fsr0 + 1;
         PREINC1:
            cpu.fsr1 := cpu.fsr1 + 1;
         PREINC2:
            cpu.fsr2 := cpu.fsr2 + 1;
      else
      end
   end;

procedure TInstruction.handle_post (addr: TROMAddress);
   begin
      case addr of
         POSTDEC0:
            cpu.fsr0 := cpu.fsr0 - 1;
         POSTINC0:
            cpu.fsr0 := cpu.fsr0 + 1;
         POSTDEC1:
            cpu.fsr1 := cpu.fsr1 - 1;
         POSTINC1:
            cpu.fsr1 := cpu.fsr1 + 1;
         POSTDEC2:
            cpu.fsr2 := cpu.fsr2 - 1;
         POSTINC2:
            cpu.fsr2 := cpu.fsr2 + 1;
      else
      end
   end;

function TInstruction.do_alu_addition (op1, op2: byte; carry: boolean): byte;
   var
      signed_sum: integer;
      sum: integer;
   begin
      signed_sum := ShortInt(op1) + ShortInt(op2) + ord(carry);
      sum := op1 + op2 + ord(carry);

      cpu.n := (sum and $80) = $80;
      cpu.ov := (signed_sum < low(ShortInt)) or (high(ShortInt) < signed_sum);
      cpu.z := (sum and $ff) = 0;
      cpu.c := sum > $FF;
      cpu.dc := ls_nibble(op1) + ls_nibble(op2) + ord(carry) > $F;

      result := sum and $0ff
   end;

function TInstruction.do_alu_subtraction (op1, op2: integer; borrow: boolean): byte;
   var
      full_difference: integer;
   begin
      full_difference := op1 - op2 - ord(borrow);
      result := full_difference and $ff;
      cpu.n := (full_difference and $80) = $80;
      cpu.z := result = 0;
      cpu.c := (full_difference and $100) = 0;
      cpu.dc := ((full_difference xor op1 xor op2) and $10) = 0
   end;

procedure TInstruction.set_n_z_status (result: byte);
   begin
      cpu.n := (result and $80) <> 0;   // sign bit negative
      cpu.z := result = 0
   end;
{$endif}

procedure TInstruction.set_rom_addr (addr: integer);
   begin
      f_rom_addr := addr
   end;


// ========================
//  TBranchTargetBaseClass

procedure TBranchTargetBaseClass.set_target_label (lbl: TInstruction);
   begin
      ftarget_label := lbl
   end;

function TBranchTargetBaseClass.has_clients: boolean;
   begin
      result := Length (client_list) > 0
   end;

procedure TBranchTargetBaseClass.set_client_destinations;
   var i: integer;
   begin
      if has_clients then
         assert (ftarget_label <> nil);
      for i := 0 to Length(client_list)-1 do
         client_list[i].dest := ftarget_label;
      SetLength (client_list, 0)
   end;

function TBranchTargetBaseClass.ComeFrom (instr: TInstruction): TInstruction;
   var i: integer;
   begin
      assert (instr <> nil);
      i := Length(client_list);
      SetLength (client_list, i+1);
      client_list [i] := instr;
      result := instr
   end;

//  TSubroutine

constructor TSubroutine.Create (_bytes_pushed, _bytes_popped: integer; _header: string);
   var
      i: integer;
   begin
      __bytes_pushed := _bytes_pushed;
      __bytes_popped := _bytes_popped;
      __hw_stack_use := 1;
      header := _header;
      i := Length (subroutine_list);
      SetLength (subroutine_list, i+1);
      subroutine_list[i] := Self
   end;

procedure TSubroutine.evaluate_additional_stack_use;
   begin
      enumerate_sub_subroutine_usage;
      additional_stack_use_evaluated := true
   end;

function TSubroutine.bytes_pushed: integer;
   begin
      evaluate_additional_stack_use;
      result := __bytes_pushed
   end;

function TSubroutine.hw_stack_use: integer;
   begin
      evaluate_additional_stack_use;
      result := __hw_stack_use
   end;

procedure TSubroutine.enumerate_sub_subroutine_usage;
   begin
      // base class default is no additional subroutines called
   end;

procedure TSubroutine.evaluate_called_subroutine_stack_use (subroutine: TSubroutine; initial_stack_level: Tinitial_stack_level_range);
   begin
      if called_subr = subroutine then
         called := true;
      additional_stack_use := max (additional_stack_use, initial_stack_level + subroutine.bytes_pushed);
      __hw_stack_use := max (__hw_stack_use, 1 + subroutine.hw_stack_use)
   end;

procedure TSubroutine.evaluate_gotoed_subroutine_stack_use (subroutine: TSubroutine; initial_stack_level: Tinitial_stack_level_range);
   begin
      if called_subr = subroutine then
         called := true;
      additional_stack_use := max (additional_stack_use, initial_stack_level + subroutine.bytes_pushed);
      __hw_stack_use := max (__hw_stack_use, subroutine.hw_stack_use)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TSubroutine.check_stack_sizes (sw_push_count, sw_pop_count, hw_count: integer);
   procedure check (calc, expected: integer; n: string);
      begin
         if calc <> expected then
            error ('Stack Calc Error in ' + ClassName + format (' %s: %d calc, %d expected', [n, calc, expected]))
      end;
   begin
      evaluate_additional_stack_use;
      check (__bytes_pushed, sw_push_count, 'push_count');
      check (__bytes_popped, sw_pop_count, 'pop count');
      check (__hw_stack_use, hw_count, 'hw stack')
   end;

procedure TSubroutine.report_stack_sizes;
   begin
      error ('   ' + ClassName + '.report_stack_sizes not implemented')
   end;

class procedure TSubroutine.check_stack_calculations;
   var
      i: integer;
   begin
      for i := 0 to Length(subroutine_list)-1 do
         subroutine_list[i].report_stack_sizes
   end;
{$endif}

class procedure TSubroutine.increment_compilation_counter;
   begin
      compilation_counter := compilation_counter + 1
   end;

function TSubroutine.generated: boolean;
   begin
      result := compilation_counter_when_last_generated = compilation_counter
   end;

function TSubroutine.ComeFrom (instr: TInstruction): TInstruction;
   begin
      assert (not generated, 'come from''s must be noted before subroutine is generated');
      evaluate_additional_stack_use;
      result := inherited ComeFrom (instr)
   end;

function TSubroutine.generate_call_code: TInstruction;
   begin
      result := TCallMacro.Create;
      call_instruction := result;
      result.annotation := header
   end;

function TSubroutine.Call: TInstruction;
   begin
      assert (not generated, 'calls must be noted before subroutine is generated');
      evaluate_additional_stack_use;
      result := generate_call_code;
      ComeFrom (call_instruction);
      StackUsageCounter.Push (__bytes_pushed + additional_stack_use);
      StackUsageCounter.Pop (__bytes_popped + additional_stack_use);
      NoteHWStackUsage (__hw_stack_use)
   end;

function TSubroutine.calls (subr: TSubroutine): boolean;
   begin
      called := false;
      called_subr := subr;
      enumerate_sub_subroutine_usage;
      result := called
   end;

class procedure TSubroutine.generate_subroutines;
   var
      all_subroutines_generated, all_callers_generated: boolean;
      i, j: integer;
   begin
      repeat
         for i := 0 to Length(subroutine_list)-1 do
            if subroutine_list[i].has_clients
            then
               begin
                  all_callers_generated := true;
                  for j := 0 to Length(subroutine_list)-1 do
                     if (i <> j)
                        and
                        (subroutine_list[j].has_clients)
                        and
                        (subroutine_list[j].calls (subroutine_list[i]))
                        and
                        (not subroutine_list[j].generated)
                     then
                        all_callers_generated := false;
                  if all_callers_generated then
                     begin
                        TAssemblySourceBlankLine.Create;
                        subroutine_list[i].ftarget_label := TAssemblyLabel.Create;
                        subroutine_list[i].ftarget_label.annotation := 'subroutine to ' + subroutine_list[i].header;
                        subroutine_list[i].generate_subroutine_code;
                        subroutine_list[i].set_client_destinations
                     end
               end;
         all_subroutines_generated := true;
         for i := 0 to Length(subroutine_list)-1 do
            if (subroutine_list[i].has_clients)
               and
               (not subroutine_list[i].generated)
            then
               all_subroutines_generated := false
      until all_subroutines_generated
   end;


//======================================
//  TPIC18x_byte_oriented_operations_fa

constructor TPIC18x_Access_Instruction.Create (_mnemonic: string; _opcode: byte; _f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create;
      assert ((0 <= _f) and (_f <= $1FFF));
      size := 2;
      mnemonic := _mnemonic;
      opcode := _opcode;
      a := _a;
      sfr_addr := -1;
      case a of
         access_mode:
            if _f < 96 then
               // stack relative address
            else if (96 <= _f) and (_f < pic_info.first_access_bank_absolute_address) then
               assert (false)
            else  // _f >= first_access_bank_absolute_address
               sfr_addr := _f;
         bank_mode:
            assert ((0 <= _f) and (_f <= 255))   // bank 0
      end;
      f := _f and $ff
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_Access_Instruction.pre_exec;
   begin
      handle_pre (effective_addr)
   end;

procedure TPIC18x_Access_Instruction.post_exec;
   begin
      handle_post (effective_addr)
   end;

function TPIC18x_Access_Instruction.effective_addr: integer;
   begin
      result := 0;  // suppress compiler warning
      case a of
         access_mode:
            if f <= $5F then
               result := cpu.fsr2 + f
            else
               begin
                  assert ($F00 + f >= pic_info.first_access_bank_absolute_address);
                  result := $F00 + f
               end;
         bank_mode:
            result := (cpu.ram[BSR] shl 8) + f
      end;
   end;
{$endif}

function TPIC18x_Access_Instruction.assembly_code: string;
   var
      s: string;
   begin
      s := mnemonic + ' ';
      if a = access_mode then
         if f <= 95 then
            s := s + '[' + byte_to_hex_string (f) + ']'
         else
            s := s + pic_info.SFR_Name (sfr_addr)
      else
         s := s + byte_to_hex_string (f) + ',BANKED';
      result := s
   end;

function TPIC18x_Access_Instruction.hex_code: THexArray;
   begin
      if not f_remove then
         begin
            SetLength (result, 2);
            result[1] := opcode;
            if a = bank_mode then
               result[1] := result[1] or $01;
            result[0] := f
         end
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_Access_Instruction.put_result (b: byte);
   begin
      cpu.ram[effective_addr] := b
   end;
{$endif}

//======================================
// TPIC18x_byte_oriented_operations_fda

constructor TPIC18x_byte_oriented_operations_fda.Create (_mnemonic: string; _opcode: byte; _f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode; _sets_z_status: boolean);
   begin
      inherited Create (_mnemonic, _opcode, _f, _a);
      assert (_opcode and $03 = 0);
      d := _d;
      sets_z_status := _sets_z_status
   end;

function TPIC18x_byte_oriented_operations_fda.loads_w_and_z: boolean;
   begin
      result := (sets_z_status)
                and
                (  (d = dest_w)
                   or
                   ((f = (WREG and $ff)) and (a = access_mode))
                )
   end;

function TPIC18x_byte_oriented_operations_fda.assembly_code: string;
   var
      s: string;
   begin
      s := mnemonic + ' ';

      if a = bank_mode then
         s := s + byte_to_hex_string (f)
      else  // access
         if f <= 95 then
            s := s + '[' + byte_to_hex_string (f) + ']'
         else if f <= 127 then
            s := s + byte_to_hex_string (f)
         else
            s := s + pic_info.SFR_Name (sfr_addr);

      case d of
         dest_f: s := s + ',F';
         dest_w: s := s + ',W'
      end;

      if a = bank_mode then
         s := s + ',BANKED'
      else if (f > 95) and (f <= 127) then
         s := s + ',ACCESS';

      result := s
   end;

function TPIC18x_byte_oriented_operations_fda.hex_code: THexArray;
   begin
      result := inherited hex_code;
      if not f_remove then
         if d = dest_f then
            result[1] := result[1] or $02
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_byte_oriented_operations_fda.put_result (b: byte);
   begin
      case d of
         dest_f:
            cpu.ram[effective_addr] := b;
         dest_w:
            cpu.w := b
      end
   end;
{$endif}


//=============================
//  TPIC18x_literal_operations

constructor TPIC18x_literal_operations.Create (_mnemonic: string; _opcode: byte; _f: byte; _sets_w_and_z_status: boolean);
   begin
      inherited Create;
      f := _f;
      size := 2;
      mnemonic := _mnemonic;
      opcode := _opcode;
      sets_w_and_z_status := _sets_w_and_z_status
   end;

function TPIC18x_literal_operations.loads_w_and_z: boolean;
   begin
      result := sets_w_and_z_status
   end;

function TPIC18x_literal_operations.assembly_code: string;
   begin
      result := mnemonic + ' ' + byte_to_hex_string (f)
   end;

function TPIC18x_literal_operations.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := opcode;
      result[0] := f
   end;


//===========================
// TPIC18x_relative_transfer

constructor TPIC18x_relative_transfer.Create (_mnemonic: string; _opcode: byte; _low_n, _hi_n: integer);
   begin
      inherited Create;
      size := 2;
      mnemonic := _mnemonic;
      opcode := _opcode;
      low_n := _low_n;
      hi_n := _hi_n
   end;

function TPIC18x_relative_transfer.assembly_code: string;
   var
      rel_addr: integer;
   begin
      assert (in_range);
      if dest <> nil then
         result := mnemonic + ' ' + dest.assembler_label
      else
         begin
            rel_addr := (n_field*2) + 2;
            if  rel_addr >= 0 then
               result := mnemonic + ' $+.' + IntToStr (rel_addr)
            else
               result := mnemonic + ' $-.' + IntToStr (-rel_addr)
         end
   end;

procedure TPIC18x_relative_transfer.set_dest (d: TInstruction);
   begin
      f_dest := d;
      if d <> nil then
         d.assembler_label_required := true
   end;

function TPIC18x_relative_transfer.hex_code: THexArray;
   var
      n: integer;
   begin
      assert (in_range);
      n := calculate_n;
      SetLength (result, 2);
      result[1] := opcode + ((n and $7ff) shr 8);
      result[0] := n and $ff
   end;

function TPIC18x_relative_transfer.calculate_n: integer;
   begin
      if dest <> nil then
         result := (integer(dest.rom_addr) - integer(rom_addr) - 2) div 2
      else
         result := n_field
   end;

function TPIC18x_relative_transfer.in_range: boolean;
   begin
      result := (low_n <= calculate_n) and (calculate_n <= hi_n)
   end;


//=============================
//  TPIC18x_conditional_branch

constructor TPIC18x_conditional_branch.Create (_mnemonic: string; _opcode: byte);
   begin
      inherited Create (_mnemonic, _opcode, Low(ShortInt), High(ShortInt))
   end;


//=================================
// TPIC18x_bit_oriented_operations

constructor TPIC18x_bit_oriented_operations.Create (_mnemonic: string; _opcode: byte; _f: integer; _bit_num: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create (_mnemonic, _opcode, _f, _a);
      assert (_opcode and $0F = 0);
      bit_num := _bit_num
   end;

function TPIC18x_bit_oriented_operations.assembly_code: string;
   var
      s: string;
   begin
      s := mnemonic + ' ';

      if (a = access_mode) and (f <= 95) then
         s := s + '[' + byte_to_hex_string (f) + ']'
      else if (a = access_mode) and (f > 127) then
         s := s + pic_info.SFR_Name (sfr_addr)
      else
         s := s + byte_to_hex_string (f);

      s := s + ',' + IntToStr(bit_num);

      if a = bank_mode then
         s := s + ',BANKED'
      else if f > 95 then
         s := s + ',ACCESS';

      result := s
   end;

function TPIC18x_bit_oriented_operations.hex_code: THexArray;
   begin
      result := inherited hex_code;
      result[1] := result[1] or (bit_num shl 1)
   end;

function TPIC18x_bit_oriented_operations.bit: byte;
   begin
      result := 0;  // suppress compier warning
      case bit_num of
         0: result := $01;
         1: result := $02;
         2: result := $04;
         3: result := $08;
         4: result := $10;
         5: result := $20;
         6: result := $40;
         7: result := $80
      end;
   end;


//=====================
// Actual Instructions

constructor TPIC18x_ADDWF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('ADDWF', $24, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ADDWF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_addition (cpu.ram[WREG], cpu.ram[effective_addr], false))
   end;
{$endif}

constructor TPIC18x_ADDWFC.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('ADDWFC', $20, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ADDWFC.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_addition (cpu.ram[WREG], cpu.ram[effective_addr], cpu.c))
   end;
{$endif}

constructor TPIC18x_ANDWF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('ANDWF', $14, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ANDWF.exec;
   var result: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.w and cpu.ram[effective_addr];
      put_result (result);
      set_n_z_status (result)
   end;
{$endif}

constructor TPIC18x_COMF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('COMF', $1c, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_COMF.exec;
   var result: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := not cpu.ram[effective_addr];
      put_result (result);
      set_n_z_status (result)
   end;
{$endif}

constructor TPIC18x_DECF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('DECF', $04, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_DECF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_subtraction (cpu.ram[effective_addr], 1, false))
   end;
{$endif}

constructor TPIC18x_DECFSZ.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('DECFSZ', $2c, _f, _d, _a, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_DECFSZ.exec;
   var result: integer;
   begin
      assert (cpu.pc = rom_addr);
      result := (cpu.ram[effective_addr] - 1) and $ff;
      if result = 0 then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2;
      put_result (result)
   end;
{$endif}

constructor TPIC18x_DCFSNZ.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('DCFSNZ', $4c, _f, _d, _a, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_DCFSNZ.exec;
   var result: integer;
   begin
      assert (cpu.pc = rom_addr);
      result := (cpu.ram[effective_addr] - 1) and $ff;
      if result = 0 then
         cpu.pc := cpu.pc + 2
      else
         cpu.pc := cpu.pc + 4;
      put_result (result)
   end;
{$endif}

constructor TPIC18x_INCF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('INCF', $28, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_INCF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_addition (cpu.ram[effective_addr], 1, false))
   end;
{$endif}

constructor TPIC18x_INCFSZ.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('INCFSZ', $3c, _f, _d, _a, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_INCFSZ.exec;
   var result: integer;
   begin
      assert (cpu.pc = rom_addr);
      result := (cpu.ram[effective_addr] + 1) and $ff;
      if result = 0 then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2;
      put_result (result)
   end;
{$endif}

constructor TPIC18x_INFSNZ.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('INFSNZ', $48, _f, _d, _a, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_INFSNZ.exec;
   var result: integer;
   begin
      assert (cpu.pc = rom_addr);
      result := (cpu.ram[effective_addr] + 1) and $ff;
      if result = 0 then
         cpu.pc := cpu.pc + 2
      else
         cpu.pc := cpu.pc + 4;
      put_result (result)
   end;
{$endif}

constructor TPIC18x_IORWF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('IORWF', $10, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_IORWF.exec;
   var result: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.w or cpu.ram[effective_addr];
      put_result (result);
      set_n_z_status (result)
   end;
{$endif}

constructor TPIC18x_MOVF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('MOVF', $50, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVF.exec;
   var result: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.ram[effective_addr];
      put_result (result);
      set_n_z_status (result)
   end;
{$endif}

constructor TPIC18x_RLCF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('RLCF', $34, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RLCF.exec;
   var w: integer;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      w := cpu.ram[effective_addr] shl 1;
      if cpu.c then
         w := w or $01;
      cpu.c := (w and $100) = $100;   // w > $ff;
      cpu.z := (w and $ff) = 0;
      cpu.n := (w and $80) = $80;
      put_result (w and $ff)
   end;
{$endif}

constructor TPIC18x_RLNCF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('RLNCF', $44, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RLNCF.exec;
   var w: integer;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      w := integer(cpu.ram[effective_addr]) shl 1;
      if w > $ff then
         w := (w + 1) and $ff;
      cpu.z := w = 0;
      cpu.n := w >= $80;
      put_result (w)
   end;
{$endif}

constructor TPIC18x_RRCF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('RRCF', $30, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RRCF.exec;
   var
      w: integer;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      w := cpu.ram[effective_addr];
      if cpu.c then
         w := w + $100;
      cpu.c := odd (w);
      w := w shr 1;
      put_result (w);
      cpu.z := w = 0;
      cpu.n := (w and $80) = $80
   end;
{$endif}

constructor TPIC18x_RRNCF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('RRNCF', $40, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RRNCF.exec;
   var
      b: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      b := cpu.ram[effective_addr];
      if odd (b) then
         b := (b shr 1) + $80
      else
         b := b shr 1;
      put_result (b);
      cpu.z := b = 0;
      cpu.n := (b and $80) = $80
   end;
{$endif}

constructor TPIC18x_SUBFWB.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('SUBFWB', $54, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBFWB.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_subtraction (cpu.w, cpu.ram[effective_addr], not cpu.c))
   end;
{$endif}

constructor TPIC18x_SUBWF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('SUBWF', $5c, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBWF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_subtraction (cpu.ram[effective_addr], cpu.w, false))
   end;
{$endif}

constructor TPIC18x_SUBWFB.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('SUBWFB', $58, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBWFB.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_subtraction (cpu.ram[effective_addr], cpu.w, not cpu.c))
   end;
{$endif}

constructor TPIC18x_SWAPF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('SWAPF', $38, _f, _d, _a, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SWAPF.exec;
   var b: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      b := cpu.ram[effective_addr];
      put_result (((b and $0f) shl 4) + ((b and $f0) shr 4))
   end;
{$endif}

constructor TPIC18x_XORWF.Create (_f: integer; _d: TPIC18x_Destination_Selection_Bit; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('XORWF', $18, _f, _d, _a, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_XORWF.exec;
   var result: byte;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.w xor cpu.ram[effective_addr];
      put_result (result);
      set_n_z_status (result)
   end;
{$endif}

constructor TPIC18x_CLRF.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('CLRF', $6a, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CLRF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (0);
      cpu.z := true
   end;
{$endif}

constructor TPIC18x_CPFSEQ.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('CPFSEQ', $62, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CPFSEQ.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.ram[effective_addr] = cpu.w then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_CPFSGT.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('CPFSGT', $64, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CPFSGT.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.ram[effective_addr] > cpu.w then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_CPFSLT.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('CPFSLT', $60, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CPFSLT.exec;
   begin
      assert (cpu.pc = rom_addr);
      if  cpu.ram[effective_addr] < cpu.w then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_MOVWF.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('MOVWF', $6e, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVWF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (cpu.w)
   end;
{$endif}

constructor TPIC18x_MULWF.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('MULWF', $02, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MULWF.exec;
   var result: integer;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.w * cpu.ram[effective_addr];
      cpu.ram[PRODH] := result shr 8;
      cpu.ram[PRODL] := result and $ff
   end;
{$endif}

constructor TPIC18x_NEGF.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('NEGF', $6c, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_NEGF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (do_alu_subtraction (0, cpu.ram[effective_addr], false))
   end;
{$endif}

constructor TPIC18x_SETF.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('SETF', $68, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SETF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result ($ff)
   end;
{$endif}

constructor TPIC18x_TSTFSZ.Create (_f: integer; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('TSTFSZ', $66, _f, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_TSTFSZ.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.ram[effective_addr] = 0 then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_MOVFF.Create (_fs, _fd: TDataMemoryAddress);
   begin
      inherited Create;
      fs := _fs;
      fd := _fd;
      size := 4;
      assert (fd <> PCL);
      assert (fd <> TOSU);
      assert (fd <> TOSH);
      assert (fd <> TOSL)
   end;

function TPIC18x_MOVFF.hex_code: THexArray;
   begin
      SetLength(result, 4);
      result [1] := $c0 + (fs shr 8);
      result [0] := fs and $0ff;
      result [3] := $f0 + (fd shr 8);
      result [2] := fd and $0ff
   end;

function TPIC18x_MOVFF.assembly_code: string;
   begin
      result := 'MOVFF ' + pic_info.SFR_Name(fs) + ',' + pic_info.SFR_Name(fd)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVFF.exec;
   begin
      case cpu.pc - rom_addr of
         0: begin
               cpu.pc := cpu.pc + 4;
               cpu.ram[fd] := cpu.ram[fs]
            end;
         2: cpu.pc := cpu.pc + 2;
      else
         assert (false)
      end;
   end;

procedure TPIC18x_MOVFF.pre_exec;
   begin
      handle_pre (fd);
      handle_pre (fs)
   end;

procedure TPIC18x_MOVFF.post_exec;
   begin
      handle_post (fd);
      handle_post (fs)
   end;
{$endif}

constructor TPIC18x_ADDLW.Create (_f: byte);
   begin
      inherited Create ('ADDLW', $0f, _f, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ADDLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := do_alu_addition (cpu.w, f, false)
   end;
{$endif}

constructor TPIC18x_ANDLW.Create (_f: byte);
   begin
      inherited Create ('ANDLW', $0b, _f, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ANDLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := cpu.w and f;
      set_n_z_status (cpu.w)
   end;
{$endif}

constructor TPIC18x_IORLW.Create (_f: byte);
   begin
      inherited Create ('IORLW', $09, _f, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_IORLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := cpu.w or f;
      set_n_z_status (cpu.w)
   end;
{$endif}

constructor TPIC18x_MOVLB.Create (_f: byte);
   begin
      inherited Create ('MOVLB', $01, _f, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVLB.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.ram[BSR] := f and $0F
   end;
{$endif}

constructor TPIC18x_MOVLW.Create (_f: byte);
   begin
      inherited Create ('MOVLW', $0e, _f, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := f
   end;
{$endif}

constructor TPIC18x_MULLW.Create (_f: byte);
   begin
      inherited Create ('MULLW', $0d, _f, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MULLW.exec;
   var
      result: integer;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      result := cpu.w * f;
      cpu.ram[PRODH] := result shr 8;
      cpu.ram[PRODL] := result and $ff
   end;
{$endif}

constructor TPIC18x_RETLW.Create (_f: byte);
   begin
      inherited Create ('RETLW', $0c, _f, false)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RETLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.w := f;
      cpu.pc := cpu.pop_return_address
   end;
{$endif}

constructor TPIC18x_SUBLW.Create (_f: byte);
   begin
      inherited Create ('SUBLW', $08, _f, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := do_alu_subtraction (f, cpu.w, false)
   end;
{$endif}

constructor TPIC18x_XORLW.Create (_f: byte);
   begin
      inherited Create ('XORLW', $0a, _f, true)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_XORLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.w := cpu.w xor f;
      set_n_z_status (cpu.w)
   end;
{$endif}


//===============
// TPIC18x_SLEEP

constructor TPIC18x_SLEEP.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_SLEEP.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $03
   end;

function TPIC18x_SLEEP.assembly_code: string;
   begin
      result := 'SLEEP'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SLEEP.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.running := false
   end;
{$endif}

constructor TPIC18x_CLRWDT.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_CLRWDT.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $04
   end;

function TPIC18x_CLRWDT.assembly_code: string;
   begin
      result := 'CLRWDT'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CLRWDT.exec;
   begin
      assert (cpu.pc = rom_addr);
      // nop - watchdog timer not simulated
      cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_RETURN.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_RETURN.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $12
   end;

function TPIC18x_RETURN.assembly_code: string;
   begin
      result := 'RETURN'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RETURN.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pop_return_address
   end;
{$endif}


//=================
//  TPIC18x_RETFIE

constructor TPIC18x_RETFIE.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_RETFIE.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $80
   end;

function TPIC18x_RETFIE.assembly_code: string;
   begin
      result := 'RETFIE'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RETFIE.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pop_return_address;
      // from simulator:
      //    $00 -> $80
      //    $40 -> $C0
      //    $80 -> $C0
      //    $C0 -> $C0
      if cpu.ram[INTCON] and $C0 = $00 then
         cpu.ram[INTCON] := cpu.ram[INTCON] and $BF
      else
         cpu.ram[INTCON] := cpu.ram[INTCON] or $40;
      cpu.ram[INTCON] := cpu.ram[INTCON] or $80;
   end;
{$endif}


//==============
// TPIC18x_LFSR

constructor TPIC18x_LFSR.Create (_fsr: TFSR; _addr: TDataMemoryAddress);
   begin
      inherited Create;
      size := 4;
      fsr := _fsr;
      addr := _addr
   end;

function TPIC18x_LFSR.hex_code: THexArray;
   begin
      SetLength (result, 4);
      result[1] := $ee;
      result[0] := (fsr shl 4) or (addr shr 8);
      result[3] := $f0;
      result[2] := addr and $ff
   end;

function TPIC18x_LFSR.assembly_code: string;
   begin
      result := 'LFSR ' + IntToStr(fsr) +',' + ram_addr_to_hex_string (addr)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_LFSR.exec;
   begin
      case cpu.pc - rom_addr of
         0: begin
               cpu.pc := cpu.pc + 4;
               case fsr of
                  0: cpu.fsr0 := addr;
                  1: cpu.fsr1 := addr;
                  2: cpu.fsr2 := addr;
               else
                  assert (false)
               end
            end;
         2: cpu.pc := cpu.pc + 2;  // nop
      else
         assert (false)
      end;
   end;
{$endif}

constructor TPIC18x_BCF.Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('BCF', $90, _f, _bit, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BCF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (cpu.ram[effective_addr] and (not bit))
   end;
{$endif}

constructor TPIC18x_BSF.Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('BSF', $80, _f, _bit, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BSF.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (cpu.ram[effective_addr] or bit)
   end;
{$endif}

constructor TPIC18x_BTFSC.Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('BTFSC', $b0, _f, _bit, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BTFSC.exec;
   begin
      assert (cpu.pc = rom_addr);
      if (cpu.ram[effective_addr] and bit) = 0 then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BTFSS.Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('BTFSS', $a0, _f, _bit, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BTFSS.exec;
   begin
      assert (cpu.pc = rom_addr);
      if (cpu.ram[effective_addr] and bit) <> 0 then
         cpu.pc := cpu.pc + 4
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BTG.Create (_f: integer; _bit: TBitNum; _a: TPIC18x_RAM_Access_Mode);
   begin
      inherited Create ('BTG', $70, _f, _bit, _a)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BTG.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      put_result (cpu.ram[effective_addr] xor bit)
   end;
{$endif}

constructor TPIC18x_BC.Create;
   begin
      inherited Create ('BC', $e2)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BC.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.c then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BN.Create;
   begin
      inherited Create ('BN', $e6)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BN.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.n then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BNC.Create;
   begin
      inherited Create ('BNC', $e3)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BNC.exec;
   begin
      assert (cpu.pc = rom_addr);
      if not cpu.c then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BNN.Create;
   begin
      inherited Create ('BNN', $e7)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BNN.exec;
   begin
      assert (cpu.pc = rom_addr);
      if not cpu.n then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BNOV.Create;
   begin
      inherited Create ('BNOV', $e5)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BNOV.exec;
   begin
      assert (cpu.pc = rom_addr);
      if not cpu.ov then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BNZ.Create;
   begin
      inherited Create ('BNZ', $e1)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BNZ.exec;
   begin
      assert (cpu.pc = rom_addr);
      if not cpu.z then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BOV.Create;
   begin
      inherited Create ('BOV', $e4)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BOV.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.ov then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BZ.Create;
   begin
      inherited Create ('BZ', $e0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BZ.exec;
   begin
      assert (cpu.pc = rom_addr);
      if cpu.z then
         cpu.pc := cpu.pc + 2 + (2*calculate_n)
      else
         cpu.pc := cpu.pc + 2
   end;
{$endif}

constructor TPIC18x_BRA.Create;
   begin
      inherited Create ('BRA', $d0, Low(TLargeRelativeBranchRange), High(TLargeRelativeBranchRange))
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_BRA.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + (2 * (calculate_n+1))
   end;
{$endif}

constructor TPIC18x_RCALL.Create;
   begin
      inherited Create ('RCALL', $d8, Low(TLargeRelativeBranchRange), High(TLargeRelativeBranchRange))
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_RCALL.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.push_return_address (cpu.pc+2);
      cpu.pc := cpu.pc + (2 * (calculate_n+1))
   end;
{$endif}

constructor TPIC18x_PUSH.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_PUSH.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $05
   end;

function TPIC18x_PUSH.assembly_code: string;
   begin
      result := 'PUSH'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_PUSH.exec;
   begin
      cpu.pc := cpu.pc + 2;
      cpu.push_return_address (cpu.pc)
   end;
{$endif}

constructor TPIC18x_POP.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_POP.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $06
   end;

function TPIC18x_POP.assembly_code: string;
   begin
      result := 'POP'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_POP.exec;
   begin
      cpu.pc := cpu.pc + 2;
      cpu.pop_return_address
   end;
{$endif}

constructor TPIC18x_CALL.Create;
   begin
      inherited Create;
      size := 4
   end;

function TPIC18x_CALL.hex_code: THexArray;
   begin
      if dest <> nil then
         n_field := dest.rom_addr shr 1;
      SetLength(result, 4);
      result [1] := $ec;
      result [0] := n_field and $0ff;
      result [3] := $f0 + (n_field shr 16);
      result [2] := (n_field shr 8) and $ff
   end;

procedure TPIC18x_CALL.set_dest (d: TInstruction);
   begin
      f_dest := d;
      if d <> nil then
         d.assembler_label_required := true
   end;

function TPIC18x_CALL.assembly_code: string;
   begin
      if dest <> nil then
         result := 'CALL ' + dest.assembler_label
      else
         result := 'CALL ' + rom_addr_to_hex_string(n_field * 2)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CALL.exec;
   begin
      if dest <> nil then
         n_field := dest.rom_addr div 2;
      case cpu.pc - rom_addr of
         0: begin
               cpu.push_return_address (cpu.pc+4);
               if dest <> nil then
                  cpu.pc := dest.rom_addr
               else
                  cpu.pc := n_field * 2;
            end;
         2: cpu.pc := cpu.pc + 2;
      else
         assert (false)
      end;
   end;
{$endif}

constructor TPIC18x_GOTO.Create;
   begin
      inherited Create;
      size := 4
   end;

function TPIC18x_GOTO.hex_code: THexArray;
   begin
      if dest <> nil then
         n_field := dest.rom_addr shr 1;
      SetLength(result, 4);
      result [1] := $ef;
      result [0] := n_field and $ff;
      result [3] := $f0 + (n_field shr 16);
      result [2] := (n_field shr 8) and $0ff
   end;

procedure TPIC18x_GOTO.set_dest (d: TInstruction);
   begin
      f_dest := d;
      if f_dest <> nil then
         d.assembler_label_required := true
   end;

function TPIC18x_GOTO.assembly_code: string;
   begin
      if dest <> nil then
         result := 'GOTO ' + dest.assembler_label
      else
         result := 'GOTO ' + rom_addr_to_hex_string (n_field * 2)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_GOTO.exec;
   begin
      case cpu.pc - rom_addr of
         0: if dest <> nil then
               cpu.pc := dest.rom_addr
            else
               cpu.pc := n_field * 2;
         2: cpu.pc := cpu.pc + 2;
      else
         assert (false)
      end;
   end;
{$endif}

constructor TPIC18x_TBLRD.Create (_mode: TPIC18x_TBLRD_Mode);
   begin
      inherited Create;
      mode := _mode;
      size := 2
   end;

function TPIC18x_TBLRD.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result [1] := $00;
      case mode of
         tblrd:
            result [0] := $08;
         tblrd_post_inc:
            result [0] := $09;
         tblrd_post_dec:
            result [0] := $0a;
         tblrd_pre_inc:
            result [0] := $0b
      end
   end;

function TPIC18x_TBLRD.assembly_code: string;
   begin
      case mode of
         tblrd:
            result := 'TBLRD *';
         tblrd_post_inc:
            result := 'TBLRD *+';
         tblrd_post_dec:
            result := 'TBLRD *-';
         tblrd_pre_inc:
            result := 'TBLRD +*'
      end
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_TBLRD.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      case mode of
         tblrd:
            cpu.ram[TABLAT] := ProgramCode.ReadByte (cpu.tblptr);
         tblrd_post_inc:
            begin
               cpu.ram[TABLAT] := ProgramCode.ReadByte (cpu.tblptr);
               cpu.tblptr := cpu.tblptr + 1
            end;
         tblrd_post_dec:
            begin
               cpu.ram[TABLAT] := ProgramCode.ReadByte (cpu.tblptr);
               cpu.tblptr := cpu.tblptr - 1
            end;
         tblrd_pre_inc:
            begin
               cpu.tblptr := cpu.tblptr + 1;
               cpu.ram[TABLAT] := ProgramCode.ReadByte (cpu.tblptr)
            end
      end
   end;
{$endif}

constructor TPIC18x_ADDFSR.Create (_f: TFSR; _k: TUint6);
   begin
      inherited Create;
      f := _f;
      k := _k;
      size := 2
   end;

function TPIC18x_ADDFSR.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $e8;
      result[0] := (f shl 6) + k
   end;

function TPIC18x_ADDFSR.assembly_code: string;
   begin
      result := 'ADDFSR ' + IntToStr(f) + ',' + byte_to_hex_string(k)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ADDFSR.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      case f of
         0: cpu.fsr0 := (cpu.fsr0 + k) and $0FFF;
         1: cpu.fsr1 := (cpu.fsr1 + k) and $0FFF;
         2: cpu.fsr2 := (cpu.fsr2 + k) and $0FFF;
      else
         assert (false)
      end;
   end;
{$endif}

constructor TPIC18x_ADDULNK.Create (_k: TUint6);
   begin
      inherited Create;
      k := _k;
      size := 2
   end;

function TPIC18x_ADDULNK.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $e8;
      result[0] := $c0 + k
   end;

function TPIC18x_ADDULNK.assembly_code: string;
   begin
      result := 'ADDULNK ' + byte_to_hex_string(k)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_ADDULNK.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.fsr2 := cpu.fsr2 + k;
      cpu.pc := cpu.pop_return_address
   end;
{$endif}

constructor TPIC18x_CALLW.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_CALLW.hex_code: THexArray;
   begin
      SetLength(result, 2);
      result[1] := $00;
      result[0] := $14
   end;

function TPIC18x_CALLW.assembly_code: string;
   begin
      result := 'CALLW'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CALLW.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.push_return_address (cpu.pc + 2);
      cpu.ram[PCL] := cpu.w
   end;
{$endif}

constructor TPIC18x_MOVSF.Create (_zs: TUInt7; _fd: TDataMemoryAddress);
   begin
      inherited Create;
      assert (_fd <> POSTDEC2);   // this doesn't work as expected in Microchip simulator
      assert (_fd <> PREINC2);
      assert (_fd <> POSTINC2);
      zs := _zs;
      fd := _fd;
      size := 4;
      assert (fd <> PCL);
      assert (fd <> TOSU);
      assert (fd <> TOSH);
      assert (fd <> TOSL)
   end;

function TPIC18x_MOVSF.hex_code: THexArray;
   begin
      SetLength (result, 4);
      result[1] := $eb;
      result[0] := zs;
      result[3] := $f0 + (fd shr 8);
      result[2] := fd and $ff
   end;

function TPIC18x_MOVSF.assembly_code: string;
   begin
      result := 'MOVSF [' + byte_to_hex_string(zs) + '],' + ram_addr_to_hex_string(fd)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVSF.exec;
   begin
      case cpu.pc - rom_addr of
         0: begin
               cpu.pc := cpu.pc + 4;
               cpu.ram[fd] := cpu.ram[cpu.fsr2 + zs]
            end;
         2: cpu.pc := cpu.pc + 2;
      else
         assert (false)
      end;
   end;

procedure TPIC18x_MOVSF.pre_exec;
   begin
      handle_pre (fd)
   end;

procedure TPIC18x_MOVSF.post_exec;
   begin
      handle_post (fd)
   end;
{$endif}

constructor TPIC18x_MOVSS.Create (_zs, _zd: TUInt7);
   begin
      inherited Create;
      zs := _zs;
      zd := _zd;
      size := 4
   end;

function TPIC18x_MOVSS.hex_code: THexArray;
   begin
      SetLength (result, 4);
      result[1] := $eb;
      result[0] := $80 + zs;
      result[3] := $f0;
      result[2] := zd
   end;

function TPIC18x_MOVSS.assembly_code: string;
   begin
      result := 'MOVSS [' + byte_to_hex_string(zs) + '],[' + byte_to_hex_string(zd) + ']'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_MOVSS.exec;
   begin
      case cpu.pc - rom_addr of
         0: begin
               cpu.pc := cpu.pc + 4;
               cpu.ram[cpu.fsr2 + zd] := cpu.ram[cpu.fsr2 + zs]
            end;
         2: cpu.pc := cpu.pc + 2;
      else
         assert (false)
      end;
   end;
{$endif}

constructor TPIC18x_PUSHL.Create (_k: byte);
   begin
      inherited Create;
      k := _k;
      size := 2
   end;

function TPIC18x_PUSHL.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $ea;
      result[0] := k
   end;

function TPIC18x_PUSHL.assembly_code: string;
   begin
      result := 'PUSHL ' + byte_to_hex_string (k)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_PUSHL.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      cpu.ram[cpu.fsr2] := k;
      cpu.fsr2 := cpu.fsr2 - 1
   end;
{$endif}

constructor TPIC18x_SUBFSR.Create (_f: TFSR; _k: TUInt6);
   begin
      inherited Create;
      f := _f;
      k := _k;
      size := 2
   end;

function TPIC18x_SUBFSR.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $e9;
      result[0] := (f shl 6) + k
   end;

function TPIC18x_SUBFSR.assembly_code: string;
   begin
      result := 'SUBFSR ' + IntToStr(f) + ',' + byte_to_hex_string(k)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBFSR.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.pc := cpu.pc + 2;
      case f of
         0: cpu.fsr0 := (cpu.fsr0 - k) and $0FFF;
         1: cpu.fsr1 := (cpu.fsr1 - k) and $0FFF;
         2: cpu.fsr2 := (cpu.fsr2 - k) and $0FFF
      end;
   end;
{$endif}

constructor TPIC18x_SUBULNK.Create (_k: TUInt6);
   begin
      inherited Create;
      k := _k;
      size := 2
   end;

function TPIC18x_SUBULNK.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $e9;
      result[0] := $c0 + k
   end;

function TPIC18x_SUBULNK.assembly_code: string;
   begin
      result := 'SUBULNK ' + byte_to_hex_string(k)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_SUBULNK.exec;
   begin
      assert (cpu.pc = rom_addr);
      cpu.fsr2 := cpu.fsr2 - k;
      cpu.pc := cpu.pop_return_address
   end;
{$endif}

//=================
// TPIC18x_ROMData

constructor TPIC18x_Data.Create (_define_data_operator, _assign_operator: string; _identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
   var
      h: THexArray;
   begin
      inherited Create;
      define_data_operator := _define_data_operator + ' ';
      identifier := _identifier;
      assign_operator := _assign_operator;
      structured_constant := _structured_constant;
      structured_constant.AddRef;
      decl_end_src_loc := _decl_end_src_loc;
      h := hex_code;
      size := Length(h);
      SetLength(h, 0)
   end;

destructor TPIC18x_Data.Destroy;
   begin
      structured_constant.Release;
      inherited
   end;

function overlay_padding_needed (sc: TStructuredConstant): integer;
   begin
      if sc.StructuredConstantKind = scOverlay then
         result := TPIC18x_Typedef_TypeInfo(sc.typedef.info).Size - TPIC18x_Typedef_TypeInfo(sc.overlay_constant.typedef.info).Size
      else
         result := 0
   end;

function TPIC18x_Data.hex_code: THexArray;
   procedure output_byte (b: byte);
      var i: integer;
      begin
         i := Length(result);
         SetLength(result, i+1);
         result[i] := b
      end;
   procedure output_simple_constant (typedef: TTypeDef; constant: TConstant);
      var
         i: integer;
         pic_real: TPIC18x_Real;
      begin
         case typedef.type_kind of
            basic_data_type:
               case TBasicDataType(typedef).basic_data_type_kind of
                  ordinal_data_type:
                     for i := TPIC18x_TypeInfo(typedef.info).Size-1 downto 0 do
                        output_byte (constant.ordinal_value.AsByte(i));
                  floating_point_data_type:
                     begin
                        pic_real.r := TConstant(constant).real_value;
                        if typedef = target_cpu.get_supported_data_type ('real') then
                           for i := 3 downto 0 do
                              output_byte (pic_real.pic_real_AsByte[i])
                        else if typedef = target_cpu.get_supported_data_type (ieee_single_type_name) then
                           for i := 3 downto 0 do
                              output_byte (pic_real.ieee_single_AsByte[i])
                        else
                           assert (false)
                     end;
               else
                  assert (false)
               end;
            set_type:
               for i := TPIC18x_TypeInfo(typedef.info).Size-1 downto 0 do
                  output_byte (setbyte (constant, i));
            string_type:
               begin
                  output_byte (Length(constant.s));
                  for i := 1 to TPIC18x_TypeInfo(typedef.info).Size-1 do
                      if i <= Length(constant.s) then
                         output_byte (ord(constant.s[i]))
                      else
                         output_byte (0)
               end;
         else
            assert (false)
         end
      end;

   var
      i,j: integer;
      bytes: TDataByteArray;
   begin
      SetLength (result, 0);
      for i := 0 to structured_constant.NumberOfElements-1 do
         if structured_constant[i].variant_typedef <> nil then   // overlay padding
            for j := TPIC18x_TypeInfo(structured_constant[i].variant_typedef.info).Size+1 to TPIC18x_TypeInfo(structured_constant[i].typedef.info).Size do
               output_byte (0)
         else if structured_constant[i].constant = nil then
            for j := 1 to TPIC18x_TypeInfo(structured_constant[i].typedef.info).Size do
               output_byte (0)
         else
            case structured_constant[i].constant.definition_kind of
               constant_definition:
                  output_simple_constant (structured_constant[i].typedef, TConstant(structured_constant[i].constant));
               structured_constant_definition:
                  begin
                     assert (structured_constant[i].typedef.type_kind = packed_record_type);
                     bytes := TPIC18x_PackedRecord_TypeInfo(structured_constant[i].typedef.info).get_constant_bytes_array (TStructuredConstant (structured_constant[i].constant));
                     for j := 0 to Length(bytes)-1 do
                        output_byte (bytes[j]);
                     for j := 1 to overlay_padding_needed (TStructuredConstant (structured_constant[i].constant)) do
                        output_byte (0)
                  end;
            else
               assert (false)
            end
   end;

procedure TPIC18x_Data.generate_assembly_code;
   var
      s: string;

   procedure output_simple_constant (typedef: TTypeDef; constant: TConstant);
      var
         i: integer;
         pic_real: TPIC18x_Real;
      begin
         case typedef.type_kind of
            basic_data_type:
               case TBasicDataType(typedef).basic_data_type_kind of
                  ordinal_data_type:
                     begin
                        s := s + byte_to_hex_string(constant.ordinal_value.AsByte(TPIC18x_TypeInfo(typedef.info).Size-1));
                        for i := TPIC18x_TypeInfo(typedef.info).Size-2 downto 0 do
                           s := s + ',' + byte_to_hex_string(constant.ordinal_value.AsByte(i))
                     end;
                  floating_point_data_type:
                     begin
                        pic_real.r := TConstant(constant).real_value;
                        if typedef = target_cpu.get_supported_data_type ('real') then
                           begin
                              s := s + byte_to_hex_string(pic_real.pic_real_AsByte[3]);
                              for i := 2 downto 0 do
                                 s := s + ',' + byte_to_hex_string(pic_real.pic_real_AsByte[i])
                           end
                        else if typedef = target_cpu.get_supported_data_type (ieee_single_type_name) then
                           begin
                              s := s + byte_to_hex_string(pic_real.ieee_single_AsByte[3]);
                              for i := 2 downto 0 do
                                 s := s + ',' + byte_to_hex_string(pic_real.ieee_single_AsByte[i])
                           end
                        else
                           assert (false)
                     end;
               else
                  assert (false)
               end;
            set_type:
               begin
                  s := s + byte_to_hex_string(TPIC18x_TypeInfo(typedef.info).Size-2) + ',' + byte_to_hex_string(Length(constant.s));
                  for i := 1 to TPIC18x_TypeInfo(typedef.info).Size-2 do
                      if i <= Length(constant.s) then
                         s := s + ',' + byte_to_hex_string(ord(constant.s[i]))
                      else
                         s := s + ',000h'
               end;
            string_type:
               begin
                  s := s + byte_to_hex_string(Length(constant.s));
                  for i := 1 to TPIC18x_TypeInfo(typedef.info).Size-1 do
                      if i <= Length(constant.s) then
                         s := s + ',' + byte_to_hex_string(ord(constant.s[i]))
                      else
                         s := s + ',000h'
               end;
            overlay_type,
            system_type,
            queue_type:
               begin
                  s := s + byte_to_hex_string(0);
                  for i := TPIC18x_TypeInfo(typedef.info).Size-2 downto 0 do
                     s := s + ',' + byte_to_hex_string(0)
               end;
         else
            assert (false)
         end
      end;  // output_simple_constant

   function output_overlay_padding (overlay_typedef, variant_typedef: TTypeDef): boolean;
      var
         i: integer;
         n: integer;
      begin
         n := TPIC18x_TypeInfo(overlay_typedef.info).Size - TPIC18x_TypeInfo(variant_typedef.info).Size;
         if n = 0 then
            result := true
         else
            begin
               result := false;
               s := s + byte_to_hex_string(0);
               for i := 2 to n do
                  s := s + ',' + byte_to_hex_string(0)
            end
      end;

   var
      i,j: integer;
      bytes: TDataByteArray;
      suppress: boolean;
   begin   // TPIC18x_Data.generate_assembly_code
      sync_source (decl_end_src_loc);
      for i := 0 to structured_constant.NumberOfElements-1 do
         begin
            suppress := false;
            s := define_data_operator;
            if structured_constant[i].variant_typedef <> nil then
               suppress := output_overlay_padding (structured_constant[i].typedef, structured_constant[i].variant_typedef)
            else if structured_constant[i].constant = nil then
               output_simple_constant (structured_constant[i].typedef, nil)
            else
               begin
                  sync_source (structured_constant[i].constant.src_loc);
                  case structured_constant[i].constant.definition_kind of
                     constant_definition:
                        output_simple_constant (structured_constant[i].typedef, TConstant(structured_constant[i].constant));
                     structured_constant_definition:
                        begin
                           assert (structured_constant[i].typedef.type_kind = packed_record_type);
                           bytes := TPIC18x_PackedRecord_TypeInfo(structured_constant[i].typedef.info).get_constant_bytes_array (TStructuredConstant (structured_constant[i].constant));
                           s := s + byte_to_hex_string(bytes[0]);
                           for j := 1 to Length(bytes)-1 do
                              s := s + ',' + byte_to_hex_string(bytes[j]);
                           for j := 1 to overlay_padding_needed (TStructuredConstant (structured_constant[i].constant)) do
                              s := s + ',' + byte_to_hex_string(0)
                        end;
                     else
                        assert (false)
                  end
               end;
            annotation := identifier + structured_constant[i].path + ' ' + assign_operator + ' ' + structured_constant[i].value;
            if not suppress then
               out (s);
            annotation := ''
         end
   end;   // TPIC18x_Data.generate_assembly_code

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_Data.exec;
   begin
      assert (false, 'can''t execute ROM data!')
   end;
{$endif}

constructor TPIC18x_ROM_Data.Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
   begin
      inherited Create ('DB', '=', _identifier, _structured_constant, _decl_end_src_loc);
      instruction_kind := rom_constant_data
   end;

constructor TPIC18x_Data_Initialization.Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
   begin
      inherited Create ('DB', ':=', _identifier, _structured_constant, _decl_end_src_loc);
      instruction_kind := rom_constant_data
   end;

constructor TPIC18x_EEPROM_Data.Create (_identifier: string; _structured_constant: TStructuredConstant; _decl_end_src_loc: TSourceLocation);
   begin
      inherited Create ('DE', ':=', _identifier, _structured_constant, _decl_end_src_loc);
      instruction_kind := eeprom_initialization_data
   end;


// DB for PACKED CODE segments

constructor TPIC18x_DB_Packed.Create (_byte: byte);
   begin
      inherited Create;
      SetLength(bytes, 1);
      bytes[0] := _byte;
      size := 1;
      instruction_kind := rom_constant_data
   end;

constructor TPIC18x_DB_Packed.Create (_bytes: TDataByteArray);
   begin
      inherited Create;
      assert (Length (_bytes) > 0);
      bytes := _bytes;
      size := Length(bytes);
      instruction_kind := rom_constant_data
   end;

function TPIC18x_DB_Packed.hex_code: THexArray;
   var
      i: integer;
   begin
      SetLength (result, Length(bytes));
      for i := 0 to Length(bytes)-1 do
         result[i] := bytes[i]
   end;

function TPIC18x_DB_Packed.assembly_code: string;
   var
      s: string;
      i: integer;
   begin
      s := 'DB ' + byte_to_hex_string(bytes[0]);
      for i := 1 to Length(bytes)-1 do
         s := s + ',' + byte_to_hex_string(bytes[i]);
      result := s
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_DB_Packed.exec;
   begin
      assert (false, 'can''t execute data!')
   end;
{$endif}

// DB for unpacked CODE segments

constructor TPIC18x_DB.Create (_byte: byte);
   begin
      inherited;
      size := 2;
      instruction_kind := executable_instruction
   end;

constructor TPIC18x_DB.Create (_bytes: TDataByteArray);
   begin
      inherited;
      if Odd (size) then
         size := size + 1;
      instruction_kind := executable_instruction
   end;

function TPIC18x_DB.hex_code: THexArray;
   begin
      result := inherited hex_code;
      if Odd (Length (result)) then
         begin
            SetLength (result, Length(result)+1);
            result [Length(result)-1] := 0
         end
   end;

constructor TPIC18x_DE.Create (_byte: byte);
   begin
      inherited;
      instruction_kind := eeprom_initialization_data
   end;

constructor TPIC18x_DE.Create (_bytes: TDataByteArray);
   begin
      inherited;
      instruction_kind := eeprom_initialization_data
   end;

constructor TPIC18x_CONFIG.Create (_b: byte; _sc: TStructuredConstant);
   begin
      inherited Create;
      b := _b;
      sc := _sc;
      sc.AddRef;
      size := 1;
      instruction_kind := config_byte_data
   end;

destructor TPIC18x_CONFIG.Destroy;
   begin
      sc.Release;
      inherited
   end;

function TPIC18x_CONFIG.hex_code: THexArray;
   begin
      SetLength (result, 1);
      result[0] := b
   end;

function TPIC18x_CONFIG.assembly_code: string;
   var
      i, j, last, ev: integer;
      et: TEnumType;
   begin
      result := src_base;
      assert (sc.StructuredConstantKind = scPackedRecord);

      last := 0;
      for i := 0 to Length(sc.packed_record_fields)-1 do
         if (TPackedRecordType(sc.typedef).fields[i].kind = normal_packed_field)
            and
            (TPackedRecordType(sc.typedef).fields[i].name <> 'nop')
         then
            last := i;

      for i := 0 to Length(sc.packed_record_fields)-1 do
         if (TPackedRecordType(sc.typedef).fields[i].kind = normal_packed_field)
            and
            (TPackedRecordType(sc.typedef).fields[i].name <> 'nop')
         then
            begin
               assert (sc.packed_record_fields[i].ctyp.ordinal_kind = ordinal_base_is_enum);
               result := result + lex.identifiers[TPackedRecordType(sc.typedef).fields[i].identifier_idx] + '=';
               ev := sc.packed_record_fields[i].c.ordinal_value.AsInteger;
               et := TEnumType(sc.packed_record_fields[i].ctyp);
               for j := 0 to Length(et.enums)-1 do
                  if et.enums[j].value = ev then
                     result := result + extract_value_cname_from_config_bits_field_enum_name(lex.identifiers[et.enums[j].identifier_idx]);
               if i < last then
                  result := result + ', '
            end
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_CONFIG.exec;
   begin
      assert (false, 'can''t execute data!')
   end;
{$endif}

function TPIC18x_CONFIG.IncludeInSourceFile: boolean;
   begin
      result :=
         (sc.StructuredConstantKind = scPackedRecord)
         and
         (assembly_code <> src_base)
   end;

// DW for unpacked CODE segments

constructor TPIC18x_DW.Create (_word: word);
   begin
      inherited Create;
      w := _word;
      size := 2
   end;

procedure TPIC18x_DW.set_msb (b: byte);
   begin
      w := (w and $ff) + (b shl 8);
      db_mode := true
   end;

procedure TPIC18x_DW.set_msb_from_labelL (instr: TInstruction);
   begin
      assert (msb_src.from_labelH = nil);
      assert (msb_src.from_labelU = nil);
      msb_src.from_labelL := instr;
      db_mode := true
   end;

procedure TPIC18x_DW.set_msb_from_labelH (instr: TInstruction);
   begin
      assert (msb_src.from_labelL = nil);
      assert (msb_src.from_labelU = nil);
      msb_src.from_labelH := instr;
      db_mode := true
   end;

procedure TPIC18x_DW.set_msb_from_labelU (instr: TInstruction);
   begin
      assert (msb_src.from_labelL = nil);
      assert (msb_src.from_labelH = nil);
      msb_src.from_labelU := instr;
      db_mode := true
   end;

procedure TPIC18x_DW.set_lsb (b: byte);
   begin
      w := (w and $ff00) + b;
      db_mode := true
   end;

procedure TPIC18x_DW.set_lsb_from_labelL (instr: TInstruction);
   begin
      assert (lsb_src.from_labelH = nil);
      assert (lsb_src.from_labelU = nil);
      lsb_src.from_labelL := instr;
      db_mode := true
   end;

procedure TPIC18x_DW.set_lsb_from_labelH (instr: TInstruction);
   begin
      assert (lsb_src.from_labelL = nil);
      assert (lsb_src.from_labelU = nil);
      lsb_src.from_labelH := instr;
      db_mode := true
   end;

procedure TPIC18x_DW.set_lsb_from_labelU (instr: TInstruction);
   begin
      assert (lsb_src.from_labelL = nil);
      assert (lsb_src.from_labelH = nil);
      lsb_src.from_labelU := instr;
      db_mode := true
   end;

function TPIC18x_DW.hex_code: THexArray;
   procedure set_byte_from_label (var value: integer; x: t_byte_src);
      begin
         if x.from_labelL <> nil then
            value := (x.from_labelL.rom_addr and $0000ff)
         else if x.from_labelH <> nil then
            value := (x.from_labelH.rom_addr and $00ff00) shr 8
         else if x.from_labelU <> nil then
            value := (x.from_labelU.rom_addr and $ff0000) shr 16
      end;
   var
      msb_value, lsb_value: integer;
   begin
      msb_value := (w shr 8) and $ff;
      lsb_value := w and $ff;
      set_byte_from_label (msb_value, msb_src);
      set_byte_from_label (lsb_value, lsb_src);
      SetLength (result, 2);
      result[1] := msb_value;
      result[0] := lsb_value
   end;

function TPIC18x_DW.assembly_code: string;
   procedure get_x (var s: string; src: t_byte_src; b: byte);
      begin
         if src.from_labelL <> nil then
            s := 'LOW(' + src.from_labelL.assembler_label + ')'
         else if src.from_labelH <> nil then
            s := 'HIGH(' + src.from_labelH.assembler_label + ')'
         else if src.from_labelU <> nil then
            s := 'UPPER(' + src.from_labelU.assembler_label + ')'
         else
            s := byte_to_hex_string (b)
      end;
   var
      msb_string, lsb_string: string;
   begin
      if db_mode then
         begin
            get_x (lsb_string, lsb_src, w and $ff);
            get_x (msb_string, msb_src, (w and $ff00) shr 8);
            result := 'DB ' + lsb_string + ', ' + msb_string
         end
      else
         result := 'DW ' + word_to_hex_string(w)
   end;

procedure TPIC18x_DW.set_dest;
   begin
      assert (false)
   end;


{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_DW.exec;
   begin
      assert (false, 'can''t execute data!')
   end;
{$endif}

// DW for PACKED CODE segments

constructor TPIC18x_DW_Packed.Create (_word: word);
   begin
      inherited;
      instruction_kind := rom_constant_data
   end;

//======
//  NOP

constructor TPIC18x_NOP.Create;
   begin
      inherited Create;
      size := 2
   end;

function TPIC18x_NOP.hex_code: THexArray;
   begin
      SetLength (result, 2);
      result[1] := $00;
      result[0] := $00
   end;

function TPIC18x_NOP.assembly_code: string;
   begin
      result := 'NOP'
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TPIC18x_NOP.exec;
   begin
      cpu.pc := cpu.pc + 2;
      do_kernel_test (self)
   end;

procedure TPIC18x_NOP.add_check_value (addr, expected_value: integer; error_msg: string);
   var i: integer;
   begin
      i := Length(check_values);
      SetLength (check_values, i+1);
      check_values[i].addr := addr;
      check_values[i].expected_value := expected_value;
      check_values[i].error_message := error_msg
   end;

procedure TPIC18x_NOP.add_check_value (v: TVariable; expected_value: integer; error_msg: string);
   var i: integer;
   begin
      i := Length(check_values);
      SetLength (check_values, i+1);
      check_values[i].v := v;
      check_values[i].v.AddRef;
      check_values[i].monitor_name := v.name;
      check_values[i].expected_value := expected_value;
      check_values[i].error_message := error_msg
   end;

procedure TPIC18x_NOP.check_state_value (addr: integer; state: char);
   var i: integer;
   begin
      i := Length(check_values);
      SetLength (check_values, i+1);
      check_values[i].state_test := true;
      check_values[i].addr := addr;
      case state of
         '*': check_values[i].expected_value := $C0;     // running
         '#': check_values[i].expected_value := $40;     // suspended
         '!': check_values[i].expected_value := $00;     // preempted
      else
         assert (false)
      end;
   end;

destructor TPIC18x_NOP.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(check_values)-1
      do check_values[i].v.Release;
      inherited
   end;
{$endif}

END.
