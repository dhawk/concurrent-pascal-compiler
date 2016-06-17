UNIT pic18x_string_unit;

INTERFACE

uses cpc_source_analysis_unit, pic18x_instructions_unit;


//============================
//  String Assign Subroutines
//============================

type
   TAssignRAMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAssignEEPROMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAssignROMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAssignRAMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 3;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAssignROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 3;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAssignEEPROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 3;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
var
   AssignEEPROMStrToEEPROMStr: TAssignEEPROMStrToEEPROMStr;
   AssignEEPROMStrToRAMStr: TAssignEEPROMStrToRAMStr;
   AssignRAMStrToEEPROMStr: TAssignRAMStrToEEPROMStr;
   AssignRAMStrToRAMStr: TAssignRAMStrToRAMStr;
   AssignROMStrToEEPROMStr: TAssignROMStrToEEPROMStr;
   AssignROMStrToRAMStr: TAssignROMStrToRAMStr;


//============================
//  String Append Subroutines
//============================

type
   TAppendRAMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendROMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendCharToRAMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 4;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendEEPROMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 4;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendRAMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;  // push count
         pop_count   = 3;   // pop count, limit & base
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 3;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendEEPROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 3;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;
   TAppendCharToEEPROMString =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 3;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
         function Call (src_loc: TSourceLocation): TInstruction;
            reintroduce;
      end;

var
   AppendCharToEEPROMString: TAppendCharToEEPROMString;
   AppendCharToRAMStr: TAppendCharToRAMStr;
   AppendEEPROMStrToEEPROMStr: TAppendEEPROMStrToEEPROMStr;
   AppendEEPROMStrToRAMStr: TAppendEEPROMStrToRAMStr;
   AppendRAMStrToEEPROMStr: TAppendRAMStrToEEPROMStr;
   AppendRAMStrToRAMStr: TAppendRAMStrToRAMStr;
   AppendROMStrToEEPROMStr: TAppendROMStrToEEPROMStr;
   AppendROMStrToRAMStr: TAppendROMStrToRAMStr;


type
   TCompareRAMStrToRAMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareRAMStrToROMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareROMStrToROMStr =
      class (TSubroutine)
      private const
         push_count = 3;
         pop_count  = 6;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareRAMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareEEPROMStrToEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 3;
         pop_count  = 4;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareRAMStrToChar =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareROMStrToChar =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 1;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;
   TCompareEEPROMStrToChar =
      class (TSubroutine)
      private const
         push_count = 1;
         pop_count  = 1;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

var
   CompareRAMStrToRAMStr: TCompareRAMStrToRAMStr;
   CompareRAMStrToROMStr: TCompareRAMStrToROMStr;
   CompareROMStrToROMStr: TCompareROMStrToROMStr;
   CompareRAMStrToEEPROMStr: TCompareRAMStrToEEPROMStr;
   CompareRAMStrToChar: TCompareRAMStrToChar;
   CompareROMStrToChar: TCompareROMStrToChar;
   CompareEEPROMStrToChar: TCompareEEPROMStrToChar;
   CompareROMStrToEEPROMStr: TCompareROMStrToEEPROMStr;
   CompareEEPROMStrToEEPROMStr: TCompareEEPROMStrToEEPROMStr;

type
   TStrPosOfRAMStrInRAMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 6;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfROMStrInRAMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 6;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfEEPROMStrInRAMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 5;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfRAMStrInROMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 6;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfEEPROMStrInROMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 5;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfROMStrInROMStr =
      class (TSubroutine)
      private const
         push_count = 6;
         pop_count  = 10;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfRAMStrInEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 5;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfROMStrInEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 2;
         pop_count  = 5;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfEEPROMStrInEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 5;
         pop_count  = 7;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfCharInRAMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 3;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfCharInROMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 3;
         hw_stack_usage = 1;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

   TStrPosOfCharInEEPROMStr =
      class (TSubroutine)
      private const
         push_count = 0;
         pop_count  = 2;
         hw_stack_usage = 2;
      protected
         procedure generate_subroutine_code;
            override;
         procedure enumerate_sub_subroutine_usage;
            override;
{$ifdef INCLUDE_SIMULATION}
         procedure report_stack_sizes;
            override;
{$endif}
      public
         constructor Create;
      end;

var
   StrPosOfRAMStrInRAMStr: TStrPosOfRAMStrInRAMStr;
   StrPosOfROMStrInRAMStr: TStrPosOfROMStrInRAMStr;
   StrPosOfEEPROMStrInRAMStr: TStrPosOfEEPROMStrInRAMStr;
   StrPosOfRAMStrInROMStr: TStrPosOfRAMStrInROMStr;
   StrPosOfROMStrInROMStr: TStrPosOfROMStrInROMStr;
   StrPosOfEEPROMStrInROMStr: TStrPosOfEEPROMStrInROMStr;
   StrPosOfRAMStrInEEPROMStr: TStrPosOfRAMStrInEEPROMStr;
   StrPosOfROMStrInEEPROMStr: TStrPosOfROMStrInEEPROMStr;
   StrPosOfEEPROMStrInEEPROMStr: TStrPosOfEEPROMStrInEEPROMStr;
   StrPosOfCharInRAMStr: TStrPosOfCharInRAMStr;
   StrPosOfCharInROMStr: TStrPosOfCharInROMStr;
   StrPosOfCharInEEPROMStr: TStrPosOfCharInEEPROMStr;


IMPLEMENTATION

// String operations involving eeprom variables are implemented in-line rather than as subroutines
// in order to keep the hardware stack usage at one level max in any process (the eeprom access
// subroutines use one level themselves, so calling them from a subroutine would result in two
// levels being used).  This hopefully won't be much of a problem since eeprom string variable is
// expected to be only lightly used in most applications.

uses
   pic18x_cpu_unit, math, pic18x_kernel_unit, pic18x_microprocessor_information_unit,
   pic18x_macro_instructions_unit, pic18x_run_time_error_check_unit;


//----------------------
//  TCopyRAMStrToRAMStr

constructor TAssignRAMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy ram string to ram string')
   end;

procedure TAssignRAMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);             // load FSR0 with dest_ptr
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      TPIC18x_ADDFSR.Create (0, 1);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_MOVFF.Create (POSTINC1, POSTINC0);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry?
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignRAMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignRAMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

function TAssignRAMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//---------------------------
//  TAssignEEPROMStrToRAMStr

constructor TAssignEEPROMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy eeprom string to ram string')
   end;

procedure TAssignEEPROMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1L
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
      loop: TInstruction;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);   // convert size to limit
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);            // load FSR0 with dest_ptr
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);
      TPIC18x_ADDFSR.Create (0, 1);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := GetEEPROMByte.Call;    // decrements FSR1
      TPIC18x_ADDFSR.Create (1, 2);     // net result is increment FSR1
      TPIC18x_MOVWF.Create (POSTINC0, access_mode);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF set borrow (cleared carry)
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      // clean up stack
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignEEPROMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignEEPROMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAssignEEPROMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//===========================
//  TAssignRAMStrToEEPROMStr

constructor TAssignRAMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy ram string to eeprom string')
   end;

procedure TAssignRAMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
      pop_size    = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      TPIC18x_INCF.Create (FSR0L, dest_f, access_mode);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_MOVF.Create(POSTINC1, dest_w, access_mode);
      SetEEPROMByte.Call;
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignRAMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (SetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignRAMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAssignRAMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//---------------------------
//  TAssignROMStrToEEPROMStr

constructor TAssignROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy rom string to eeprom string')
   end;

procedure TAssignROMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in TBLPTR
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
      pop_size    = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);             // load FSR0 with dest_ptr
      TPIC18x_INCF.Create (FSR0L, dest_f, access_mode);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      SetEEPROMByte.Call;
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (SetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAssignROMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;




//---------------------------
//  TAssignEEPROMStrToEEPROMStr

constructor TAssignEEPROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy eeprom string to eeprom string')
   end;

procedure TAssignEEPROMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
      pop_size    = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      TPIC18x_INCF.Create (FSR0L, dest_f, access_mode);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := GetEEPROMByte.Call;    // decrements FSR1
      TPIC18x_ADDFSR.Create (1, 2);     // net result is increment FSR1
      SetEEPROMByte.Call;
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignEEPROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_called_subroutine_stack_use (SetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignEEPROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAssignEEPROMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//----------------------
//  TAppendRAMStrToRAMStr

constructor TAppendRAMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'append ram string to ram string')
   end;

procedure TAppendRAMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;   // size passed, converted to limit
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;  // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (FSR0H, dest_f, access_mode);

      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);  // pop count
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_MOVFF.Create (POSTINC1, POSTINC0);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry?
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendRAMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendRAMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

function TAppendRAMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//----------------------
//  TCopyROMStrToRAMStr

constructor TAssignROMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'copy rom string to ram string')
   end;

procedure TAssignROMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in TBLPTR
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);             // load FSR0 with dest_ptr
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      TPIC18x_ADDFSR.Create (0, 1);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVFF.Create (TABLAT, POSTINC0);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      bz.dest := TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      // clean up stack & return
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAssignROMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAssignROMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

function TAssignROMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//----------------------
//  TAppendROMStrToRAMStr

constructor TAppendROMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'append rom string to ram string')
   end;

procedure TAppendROMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in TBLPTR
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (FSR0H, dest_f, access_mode);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVFF.Create (TABLAT, POSTINC0);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      // clean up stack & return
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendROMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendROMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

function TAppendROMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//----------------------
//  TAppendCharToRAMStr

constructor TAppendCharToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'append char to ram string')
   end;

procedure TAppendCharToRAMStr.generate_subroutine_code;
   const
      data_byte   = 1;
      dest_limitL = 2;
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);  // convert size to limit
      TPIC18x_MOVWF.Create (FSR1L, access_mode);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      TPIC18x_ADDWF.Create (FSR1L, dest_f, access_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (FSR1H, dest_f, access_mode);
      TPIC18x_MOVF.Create (FSR1L, dest_w, access_mode);
      TPIC18x_SUBWF.Create (dest_limitL, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_MOVSF.Create (data_byte, INDF1);
      TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_INCF.Create (INDF1, dest_f, access_mode);
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendCharToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendCharToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

function TAppendCharToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//===========================
//  TAppendEEPROMStrToRAMStr

constructor TAppendEEPROMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'append eeprom to ram string')
   end;

procedure TAppendEEPROMStrToRAMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1L
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;   // size passed, converted to limit
      dest_baseH  = 3;
      dest_baseL  = 4;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;  // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (FSR0H, dest_f, access_mode);
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := GetEEPROMByte.Call;  // decrements FSR1
      TPIC18x_ADDFSR.Create (1, 2);    // net result is FSR1++
      TPIC18x_MOVWF.Create (POSTINC0, access_mode);
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // above DECF cleared carry?
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseH, FSR1H);
      TPIC18x_MOVSF.Create (dest_baseL, FSR1L);
      TPIC18x_MOVWF.Create (INDF1, access_mode);
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendEEPROMStrToRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendEEPROMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAppendEEPROMStrToRAMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//===========================
//  TAppendRAMStrToEEPROMStr

constructor TAppendRAMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count+2, pop_count+2, 'append ram to eeprom string')
   end;

procedure TAppendRAMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]
      TPIC18x_MOVFF.Create (FSR1L, POSTDEC2);          // save FSR1 - USES TWO MORE STACK
      TPIC18x_MOVFF.Create (FSR1H, POSTDEC2);
      StackUsageCounter.Push (2);
      TPIC18x_MOVWF.Create (FSR1L, access_mode);           // fsr1L := @dest.strlen
      TPIC18x_ADDLW.Create (1);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_MOVFF.Create (PREINC2, FSR1H);            // restore FSR1
      TPIC18x_MOVFF.Create (PREINC2, FSR1L);
      StackUsageCounter.Pop (2);
      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);   // fsr0L := @dest[dest.strlen+1]
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      SetEEPROMByte.Call;  // increments FSR0
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      // clean up stack
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendRAMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, -2);
      evaluate_called_subroutine_stack_use (SetEEPROMByte, -2);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -2-pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendRAMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count+2, pop_count+2, 2)
   end;
{$endif}

function TAppendRAMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;

//===========================
//  TAppendROMStrToEEPROMStr

constructor TAppendROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'append rom to eeprom string')
   end;

procedure TAppendROMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in TBLPTR
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
      pop_count   = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]
      TPIC18x_MOVWF.Create (FSR1L, access_mode);           // fsr1L := @dest.strlen
      TPIC18x_ADDLW.Create (1);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);   // fsr0L := @dest[dest.strlen+1]
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      SetEEPROMByte.Call;  // increments FSR0
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      // clean up stack
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_called_subroutine_stack_use (SetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAppendROMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//==============================
//  TAppendEEPROMStrToEEPROMStr

constructor TAppendEEPROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count+1, pop_count+1, 'append eeprom to eeprom string')
   end;

procedure TAppendEEPROMStrToEEPROMStr.generate_subroutine_code;
   const
      // src_ptr in FSR1
      // src count in W
      //    Z set if src count = 0
      count       = 1;
      dest_limitL = 2;
      dest_baseL  = 3;
      pop_count   = 3;
   var
      loop: TInstruction;
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);        // push src count
      bz := TPIC18x_BZ.Create;                             // nothing to do if count was 0
      // convert size to limit
      TPIC18x_MOVF.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_ADDWF.Create (dest_limitL, dest_f, access_mode);
      // load FSR0 with @dest[dest.strlen+1]

      TPIC18x_MOVFF.Create (FSR1L, POSTDEC2);
      StackUsageCounter.Push (1);
      TPIC18x_MOVWF.Create (FSR1L, access_mode);           // fsr1L := @dest.strlen
      TPIC18x_ADDLW.Create (1);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_MOVFF.Create (PREINC2, FSR1L);
      StackUsageCounter.Pop (1);

      TPIC18x_ADDWF.Create (FSR0L, dest_f, access_mode);   // fsr0L := @dest[dest.strlen+1]
      loop := TPIC18x_MOVF.Create (dest_limitL, dest_w, access_mode);  // test limit
      TPIC18x_SUBWF.Create (FSR0L, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      bnz.dest := GetEEPROMBYte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      SetEEPROMByte.Call;  // increments FSR0
      TPIC18x_DECF.Create (count, dest_f, access_mode);
      TPIC18x_BNZ.Create.dest := loop;
      // set new dest.strlen
      // above DECF cleared carry
      TPIC18x_MOVF.Create (FSR0L, dest_w, access_mode);
      TPIC18x_SUBFWB.Create (dest_baseL, dest_w, access_mode);
      TPIC18x_DECF.Create (WREG, dest_w, access_mode);
      TPIC18x_MOVSF.Create (dest_baseL, FSR0L);
      SetEEPROMByte.Call;
      // clean up stack
      bz.dest := TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendEEPROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_called_subroutine_stack_use (GetEEPROMByte, -1);
      evaluate_called_subroutine_stack_use (SetEEPROMByte, -1);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -1-pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendEEPROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count+1, pop_count+1, 2)
   end;
{$endif}

function TAppendEEPROMStrToEEPROMStr.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//============================
//  TAppendCharToEEPROMString

constructor TAppendCharToEEPROMString.Create;
   begin
      inherited Create (push_count, pop_count, 'append char to eeprom string')
   end;

procedure TAppendCharToEEPROMString.generate_subroutine_code;
   const
      data_char  = 1;
      limit      = 2;
      dest_base  = 3;
   var
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVF.Create (dest_base, dest_w, access_mode);    // load base
      TPIC18x_ADDWF.Create (limit, dest_f, access_mode);   // set limit
      TPIC18x_MOVSF.Create (dest_base, FSR1L);
      GetEEPROMByte.Call;             // get base.strlen; decrements FSR1
      TPIC18x_ADDFSR.Create (1, 2);   // net result: FSR1++
      TPIC18x_ADDWF.Create (FSR1L, dest_w, access_mode);
      TPIC18x_MOVWF.Create (FSR0L, access_mode);    // set addr for SetEEPROMByte below
      TPIC18x_SUBWF.Create (limit, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_ADDFSR.Create (2, pop_count);
      set_errorcode_routine.ComeFrom (TGOTOMacro.Create);   // report string overflow
      // write data_char
      bnz.dest := TPIC18x_MOVF.Create (data_char, dest_w, access_mode);
      SetEEPROMByte.Call;
      // update length
      TPIC18x_MOVSF.Create (dest_base, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDLW.Create (1);
      TPIC18x_MOVSF.Create (dest_base, FSR0L);
      SetEEPROMByte.Call;
      // fix up stack
      TPIC18x_ADDULNK.Create (pop_count)
   end;

procedure TAppendCharToEEPROMString.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0);
      evaluate_called_subroutine_stack_use (SetEEPROMByte, 0);
      evaluate_gotoed_subroutine_stack_use (set_errorcode_routine, -pop_count)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TAppendCharToEEPROMString.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}

function TAppendCharToEEPROMString.Call (src_loc: TSourceLocation): TInstruction;
   begin
      result := inherited Call;
      RecordRunTimeErrorLocation (TAssemblyLabel.Create, rterr_string_overflow, src_loc)
   end;


//===========================
//  String Compare Functions
//===========================

//-------------------------
//  TCompareRAMStrToRAMStr

constructor TCompareRAMStrToRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'compare ram string to ram string')
   end;
procedure TCompareRAMStrToRAMStr.generate_subroutine_code;
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      // pop @a into FSR0
      TPIC18x_MOVFF.Create (PREINC2, FSR0H);
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      // push a.len; fsr0++
      TPIC18x_MOVFF.Create (POSTINC0, POSTDEC2);
      // push b.len; fsr1++
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);  // put b.len in WREG
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);    // push it
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 2);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareRAMStrToRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

//-------------------------
//  TCompareRAMStrToROMStr

constructor TCompareRAMStrToROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'compare ram string to rom string')
   end;

procedure TCompareRAMStrToROMStr.generate_subroutine_code;
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      // pop @a into FSR0
      TPIC18x_MOVFF.Create (PREINC2, FSR0H);
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      // push a.len; fsr0++
      TPIC18x_MOVFF.Create (POSTINC0, POSTDEC2);
      // push b.len; fsr1++
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);   // put b.len in WREG
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);    // push it
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 2);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareRAMStrToROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

//-------------------------
//  TCompareRAMStrToROMStr

constructor TCompareROMStrToROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'compare rom string to rom string')
   end;

procedure TCompareROMStrToROMStr.generate_subroutine_code;
   const
      eos_status = 1;
      count      = 2;
      baddrH     = 3;
      baddrL     = 4;
      aaddrH     = 5;
      aaddrL     = 6;
   procedure reada (adj: integer);
      begin
         TPIC18x_MOVSF.Create (aaddrH+adj, TBLPTRH);
         TPIC18x_MOVSF.Create (aaddrL+adj, TBLPTRL);
         TPIC18x_TBLRD.Create (tblrd_post_inc);
         TPIC18x_INCF.Create (aaddrL+adj, dest_f, access_mode);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_ADDWFC.Create (aaddrH+adj, dest_f, access_mode);
         TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode)
      end;
   procedure readb (adj: integer);
      begin
         TPIC18x_MOVSF.Create (baddrH+adj, TBLPTRH);
         TPIC18x_MOVSF.Create (baddrL+adj, TBLPTRL);
         TPIC18x_TBLRD.Create (tblrd_post_inc);
         TPIC18x_INCF.Create (baddrL+adj, dest_f, access_mode);
         TPIC18x_MOVLW.Create (0);
         TPIC18x_ADDWFC.Create (baddrH+adj, dest_f, access_mode);
         TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode)
      end;
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      reada (-2);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      readb (-1);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      reada (0);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      readb (1);
      TPIC18x_SUBWF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 6);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareROMStrToROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

//----------------------------
//  TCompareRAMStrToEEPROMStr

//      class (TSubroutine)
//      private const
//         push_count = 0;
//         pop_count  = 1;
//         hw_stack_usage = 1;
constructor TCompareRAMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count+1, pop_count+1, 'compare ram string to eeprom string')
   end;

procedure TCompareRAMStrToEEPROMStr.generate_subroutine_code;
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      // pop @a into FSR0
      TPIC18x_MOVFF.Create (PREINC2, FSR0H);
      TPIC18x_MOVFF.Create (PREINC2, FSR0L);
      // push a.len; fsr0++
      TPIC18x_MOVFF.Create (POSTINC0, POSTDEC2);
      // push b.len; fsr1++
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);    // push it
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 2);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

procedure TCompareRAMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareRAMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count+1, pop_count+1, 2)
   end;
{$endif}

//----------------------------
//  TCompareROMStrToEEPROMStr

//      class (TSubroutine)
//      private const
//         push_count = 0;
//         pop_count  = 1;
//         hw_stack_usage = 1;
constructor TCompareROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count+1, pop_count+1, 'compare ram string to eeprom string')
   end;

procedure TCompareROMStrToEEPROMStr.generate_subroutine_code;
   // @a on stk
   // @b in FSR1L
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      // pop @a into FSR0
      TPIC18x_MOVFF.Create (PREINC2, TBLPTRH);    // was fsr0
      TPIC18x_MOVFF.Create (PREINC2, TBLPTRL);
      // push a.len; fsr0++
//?      TPIC18x_MOVFF.Create (POSTINC0, POSTDEC2);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVFF.Create (TABLAT, POSTDEC2);
      // push b.len; fsr1++
      GetEEPROMByte.Call;    // put b.len in WREG
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);    // push it
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 2);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

procedure TCompareROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count+1, pop_count+1, 2)
   end;
{$endif}


//-----------------------
//  TCompareRAMStrToChar

constructor TCompareRAMStrToChar.Create;
   begin
      inherited Create (push_count, pop_count, 'compare ram string to char')
   end;

procedure TCompareRAMStrToChar.generate_subroutine_code;
   // in:  FSR1 -> a
   //      [1] = b
   // out: [1] is 0
   //      status is set
   var
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVF.Create (1, dest_w, access_mode);  // push b
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      TPIC18x_CLRF.Create (2, access_mode);   // clear result tos
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);  // read a.strlen; Z means strlen is 0; set fsr to @a[1]
      bz := TPIC18x_BZ.Create;   // branch if a.strlen=0; will want status result N since a < b ('' < b)
      // a.strlen >= 1 known
      TPIC18x_MOVF.Create (1, dest_w, access_mode);  // read b
      TPIC18x_SUBWF.Create (POSTDEC1, dest_w, access_mode);  // w:=a[1]-b;  fsr1 := @a.strlen
      bnz := TPIC18x_BNZ.Create;  // branch if a[1] <> b; result is < if (a < b) or > if (a > b)
      // a[1]=b known
      TPIC18x_DECF.Create (INDF1, dest_w, access_mode);  // w:=a.strlen-1; Z means a.strlen is 1 and a=b - otherwise a.strlen > 1 and a > b
      TPIC18x_BCF.Create (STATUS, status_n, access_mode);  // clear N bit, leave Z
      bnz.dest := TPIC18x_ADDULNK.Create (1);
      bz.dest := TPIC18x_DECF.Create (WREG, dest_w, access_mode);  // w := 0-1; sets Nz for '' < b
      TPIC18x_ADDULNK.Create (1)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareRAMStrToChar.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}

// ----------------------
//  TCompareROMStrToChar
constructor TCompareROMStrToChar.Create;
   begin
      inherited Create (push_count, pop_count, 'compare ram string to char')
   end;
  //   TPIC18x_TBLRD_Mode = (tblrd, tblrd_post_inc, tblrd_post_dec, tblrd_pre_inc);

procedure TCompareROMStrToChar.generate_subroutine_code;
   // in:  FSR1 -> a
   //      [1] = b
   // out: [1] is 0
   //      status is set
   var
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVF.Create (1, dest_w, access_mode);  // push b
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      TPIC18x_CLRF.Create (2, access_mode);   // clear result tos

      TPIC18x_TBLRD.Create (tblrd_post_inc);  // read a.strlen;
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);  // Z means strlen is 0; set tblptr to @a[1]

      bz := TPIC18x_BZ.Create;   // branch if a.strlen=0; will want status result N since a < b ('' < b)
      // a.strlen >= 1 known
      TPIC18x_MOVF.Create (1, dest_w, access_mode);  // read b

      TPIC18x_TBLRD.Create (tblrd_post_dec);  // tblptr := @a.strlen
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);  // w:=a[1]-b

      bnz := TPIC18x_BNZ.Create;  // branch if a[1] <> b; result is < if (a < b) or > if (a > b)
      // a[1]=b known
      TPIC18x_TBLRD.Create (tblrd);  // read a.strlen
      TPIC18x_DECF.Create (TABLAT, dest_w, access_mode);  // w:=a.strlen-1; Z means a.strlen is 1 and a=b - otherwise a.strlen > 1 and a > b
      TPIC18x_BCF.Create (STATUS, status_n, access_mode);  // clear N bit, leave Z as is
      bnz.dest := TPIC18x_ADDULNK.Create (1);
      bz.dest := TPIC18x_DECF.Create (WREG, dest_w, access_mode);  // w := 0-1; sets Nz for '' < b
      TPIC18x_ADDULNK.Create (1)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareROMStrToChar.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 1)
   end;
{$endif}


//-----------------------
//  TCompareEEPROMStrToChar

constructor TCompareEEPROMStrToChar.Create;
   begin
      inherited Create (push_count, pop_count, 'compare eeprom string to char')
   end;

procedure TCompareEEPROMStrToChar.generate_subroutine_code;
   // in:  FSR1 -> a
   //      [1] = b
   // out: [1] is 0
   //      status is set
   var
      bz: TPIC18x_BZ;
      bnz: TPIC18x_BNZ;
   begin
      TPIC18x_MOVF.Create (1, dest_w, access_mode);  // push b
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      TPIC18x_CLRF.Create (2, access_mode);   // clear result tos
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      bz := TPIC18x_BZ.Create;   // branch if a.strlen=0; will want status result N since a < b ('' < b)
      // a.strlen >= 1 known
      GetEEPROMByte.Call;
      TPIC18x_SUBWF.Create (1, dest_w, access_mode);
      TPIC18x_NEGF.Create (WREG, access_mode);
      bnz := TPIC18x_BNZ.Create;  // branch if a[1] <> b; result is < if (a < b) or > if (a > b)
      // a[1]=b known
      GetEEPROMByte.Call;  // w:=a.strlen-1; Z means a.strlen is 1 and a=b - otherwise a.strlen > 1 and a > b
      TPIC18x_SUBLW.Create (1);
      TPIC18x_BCF.Create (STATUS, status_n, access_mode);  // clear N bit, leave Z
      bnz.dest := TPIC18x_ADDULNK.Create (1);
      bz.dest := TPIC18x_DECF.Create (WREG, dest_w, access_mode);  // w := 0-1; sets Nz for '' < b
      TPIC18x_ADDULNK.Create (1)
   end;

procedure TCompareEEPROMStrToChar.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareEEPROMStrToChar.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, 2)
   end;
{$endif}


//-------------------------------
//  TCompareEEPROMStrToEEPROMStr

constructor TCompareEEPROMStrToEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'compare eeprom string to eeprom string')
   end;

procedure TCompareEEPROMStrToEEPROMStr.generate_subroutine_code;
   const
      b_ptr = 3;       // was fsr1
      a_ptr = 4;       // was fsr0
   var
      bn: TPIC18x_BN;
      bz: TPIC18x_BZ;
      bra1, bra2: TPIC18x_BRA;
      loop: TInstruction;
   begin
      // push a.len; a_ptr++
      TPIC18x_MOVSF.Create (a_ptr-2, FSR1L);
      TPIC18x_INCF.Create (a_ptr-2, dest_f, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
      // push b.len; b_ptr++
      TPIC18x_MOVSF.Create (b_ptr-1, FSR1L);
      TPIC18x_INCF.Create (b_ptr-1, dest_f, access_mode);
      GetEEPROMByte.Call;  // put b.len in WREG
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);    // push it
      // at this point [1] = b.len
      //               [2] = a.len
      // compare a.len to b.len      (a.len - b.len)
      TPIC18x_SUBWF.Create (2, dest_w, access_mode);
      bn := TPIC18x_BN.Create;
      bz := TPIC18x_BZ.Create;
      // handle a.len > b.len
      TPIC18x_MOVSS.Create (1, 2);  // count := b.len
      TPIC18x_MOVLW.Create (status_greater_than);
      bra1 := TPIC18x_BRA.Create;
      // handle a.len < b.len
      // count is already a.len
      bn.dest := TPIC18x_MOVLW.Create (status_less_than);
      bra2 := TPIC18x_BRA.Create;
      // handle = a.len = b.len
      // count is already a.len
      bz.dest := TPIC18x_MOVLW.Create (status_equals);
      bra1.dest := TPIC18x_MOVWF.Create (1, access_mode);  // store eos_status
      bra2.dest := bra1.dest;
      // at this point [1] = eos_status
      //               [2] = count
      // increment count
      TPIC18x_INCF.Create (2, dest_f, access_mode);
      loop := TPIC18x_DCFSNZ.Create (2, dest_f, access_mode);
      bra1 := TPIC18x_BRA.Create;
      TPIC18x_MOVSF.Create (a_ptr, FSR1L);
      TPIC18x_INCF.Create (a_ptr, dest_f, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (POSTDEC2, access_mode);  // push a[x]
      TPIC18x_MOVSF.Create (b_ptr+1, FSR1L);
      TPIC18x_INCF.Create (b_ptr+1, dest_f, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_SUBWF.Create (PREINC2, dest_w, access_mode);
      TPIC18x_BZ.Create.dest := loop;
      // a comparison was <>
      bra2 := TPIC18x_BRA.Create;
      // all count chars were equal
      bra1.dest := TPIC18x_MOVSF.Create (1, STATUS);
      bra2.dest := TPIC18x_ADDFSR.Create (2, 4);
      TPIC18x_PUSHL.Create (0);
      TPIC18x_RETURN.Create
   end;

procedure TCompareEEPROMStrToEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TCompareEEPROMStrToEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}

//   Pseudo-code for strpos algorithm:
//      function strpos (str, substr: pansichar): integer;
//         label 9;
//         var
//            p_str, p_substr: pansichar;
//            iter, i: integer;
//         begin
//            result := 0;
//            if ord(str^) = 0 then exit;   // test str.len
//            if ord(substr^) = 0 then exit;  // test substr.len
//            iter := ord(str^) - ord(substr^) + 1;
//            if iter <= 0 then exit;
//            repeat
//               result := result + 1;
//               str := str + 1;
//               p_str := str;
//               p_substr := substr;
//               i := ord(p_substr^); p_substr := p_substr + 1;
//               repeat
//                  if p_str^ <> p_substr^ then goto 9;
//                  p_str := p_str + 1;
//                  p_substr := p_substr + 1;
//                  i := i - 1
//               until i = 0;
//               exit;  // success: substr location found in str
//         9:    iter := iter - 1;
//            until iter = 0;
//            result := 0
//         end;


//==========================
//  TStrPosOfRAMStrInRAMStr

constructor TStrPosOfRAMStrInRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of RAM string in RAM string')
   end;

procedure TStrPosOfRAMStrInRAMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrH = 3;
      substrL = 4;
      strH = 5;
      strL = 6;
      result = 7;
      pop_count = 6;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      TPIC18x_MOVSF.Create (substrH, FSR1H);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      TPIC18x_MOVSF.Create (substrH, FSR1H);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (POSTINC1, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfRAMStrInRAMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfRAMStrInRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//==========================
//  TStrPosOfROMStrInRAMStr

constructor TStrPosOfROMStrInRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of ROM string in RAM string')
   end;

procedure TStrPosOfROMStrInRAMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrH = 3;
      substrL = 4;
      strH = 5;
      strL = 6;
      result = 7;
      pop_count = 6;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfROMStrInRAMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfROMStrInRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//=============================
//  TStrPosOfEEPROMStrInRAMStr

constructor TStrPosOfEEPROMStrInRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of EEPROM string in RAM string')
   end;

procedure TStrPosOfEEPROMStrInRAMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrL = 3;
      strH = 4;
      strL = 5;
      result = 6;
      pop_count = 5;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfEEPROMStrInRAMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfEEPROMStrInRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}

//==========================
//  TStrPosOfRAMStrInROMStr

constructor TStrPosOfRAMStrInROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of RAM string in ROM string')
   end;

procedure TStrPosOfRAMStrInROMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrH = 3;
      substrL = 4;
      strH = 5;
      strL = 6;
      result = 7;
      pop_count = 6;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      TPIC18x_MOVSF.Create (substrH, FSR1H);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      TPIC18x_MOVSF.Create (substrH, FSR1H);
      TPIC18x_MOVF.Create (POSTINC1, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_SUBWF.Create (POSTINC1, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfRAMStrInROMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfRAMStrInROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//=============================
//  TStrPosOfEEPROMStrInROMStr

constructor TStrPosOfEEPROMStrInROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of EEPROM string in RAM string')
   end;

procedure TStrPosOfEEPROMStrInROMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrL = 3;
      strH = 4;
      strL = 5;
      result = 6;
      pop_count = 5;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfEEPROMStrInROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfEEPROMStrInROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//==========================
//  TStrPosOfROMStrInROMStr

constructor TStrPosOfROMStrInROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of ROM string in ROM string')
   end;

procedure TStrPosOfROMStrInROMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      p_substrH = 3;
      p_substrL = 4;
      p_strH = 5;
      p_strL = 6;
      substrH = 7;
      substrL = 8;
      strH = 9;
      strL = 10;
      result = 11;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      TPIC18x_MOVSS.Create (substrL, p_substrL);
      TPIC18x_MOVSS.Create (substrH, p_substrH);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSS.Create (strL, p_strL);
      TPIC18x_MOVLW.Create (0);
      TPIC18x_ADDWFC.Create (strH, dest_f, access_mode);
      TPIC18x_MOVSS.Create (strH, p_strH);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_substrL, access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_substrH, access_mode);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVSF.Create (p_strL, TBLPTRL);
      TPIC18x_MOVSF.Create (p_strH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_MOVF.Create (TBLPTRL, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_strL, access_mode);
      TPIC18x_MOVF.Create (TBLPTRH, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_strH, access_mode);
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfROMStrInROMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfROMStrInROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//=============================
//  TStrPosOfRAMStrInEEPROMStr

constructor TStrPosOfRAMStrInEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of RAM string in EEPROM string')
   end;

procedure TStrPosOfRAMStrInEEPROMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrH = 3;
      substrL = 4;
      strL = 5;
      result = 6;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR0L);
      TPIC18x_MOVSF.Create (substrH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, FSR1L);
      TPIC18x_MOVSF.Create (substrL, FSR0L);
      TPIC18x_MOVSF.Create (substrH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TAssemblyLabel.Create;
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_SUBWF.Create (POSTINC0, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfRAMStrInEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfRAMStrInEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//=============================
//  TStrPosOfROMStrInEEPROMStr

constructor TStrPosOfROMStrInEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of ROM string in EEPROM string')
   end;

procedure TStrPosOfROMStrInEEPROMStr.generate_subroutine_code;
   const
      i = 1;
      iter = 2;
      substrH = 3;
      substrL = 4;
      strL = 5;
      result = 6;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSF.Create (strL, FSR1L);
      TPIC18x_MOVSF.Create (substrL, TBLPTRL);
      TPIC18x_MOVSF.Create (substrH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TAssemblyLabel.Create;
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_SUBWF.Create (TABLAT, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfROMStrInEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfROMStrInEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//================================
//  TStrPosOfEEPROMStrInEEPROMStr

constructor TStrPosOfEEPROMStrInEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of EEPROM string in EEPROM string')
   end;

procedure TStrPosOfEEPROMStrInEEPROMStr.generate_subroutine_code;
   const
      temp = 1;
      i = 2;
      iter = 3;
      p_substrL = 4;
      p_strL = 5;
      substrL = 6;
      strL = 7;
      result = 8;
   var
      exit_label: TBranchTarget;
      iter_loop: TInstruction;
      substr_loop: TInstruction;
      bnz: TPIC18x_BNZ;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_SUBFSR.Create (2, push_count);
      TPIC18x_MOVSF.Create (strL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (iter, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_SUBWF.Create (iter, dest_f, access_mode);
      exit_label.ComeFrom (TPIC18x_BN.Create);
      TPIC18x_INCF.Create (iter, dest_f, access_mode);
      TPIC18x_MOVSS.Create (substrL, p_substrL);
      iter_loop := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_INCF.Create (strL, dest_f, access_mode);
      TPIC18x_MOVSS.Create (strL, p_strL);
      TPIC18x_MOVSF.Create (substrL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVWF.Create (i, access_mode);
      substr_loop := TAssemblyLabel.Create;
      GetEEPROMByte.Call;
      TPIC18x_MOVWF.Create (temp, access_mode);
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_MOVF.Create (FSR1L, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_substrL, access_mode);
      TPIC18x_MOVSF.Create (p_strL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1,2);
      TPIC18x_SUBWF.Create (temp, dest_w, access_mode);
      bnz := TPIC18x_BNZ.Create;
      TPIC18x_MOVF.Create (FSR1L, dest_w, access_mode);
      TPIC18x_MOVWF.Create (p_strL, access_mode);
      TPIC18x_DECFSZ.Create (i, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := substr_loop;
      TPIC18x_ADDULNK.Create (pop_count);
      bnz.dest := TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := iter_loop;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfEEPROMStrInEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfEEPROMStrInEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//========================
//  TStrPosOfCharInRAMStr

constructor TStrPosOfCharInRAMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of char in RAM string')
   end;

procedure TStrPosOfCharInRAMStr.generate_subroutine_code;
   const
      strH = 1;
      iter = 1;
      strL = 2;
      substrchar = 3;
      result = 4;
   var
      exit_label: TBranchTarget;
      lbl: TInstruction;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_MOVSF.Create (strL, FSR0L);
      TPIC18x_MOVSF.Create (strH, FSR0H);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVWF.Create (iter, access_mode);
      lbl := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_MOVF.Create (POSTINC0, dest_w, access_mode);
      TPIC18x_SUBWF.Create (substrchar, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := lbl;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfCharInRAMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfCharInRAMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//========================
//  TStrPosOfCharInROMStr

constructor TStrPosOfCharInROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of char in ROM string')
   end;

procedure TStrPosOfCharInROMStr.generate_subroutine_code;
   const
      strH = 1;
      iter = 1;
      strL = 2;
      substrchar = 3;
      result = 4;
   var
      exit_label: TBranchTarget;
      lbl: TInstruction;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_MOVSF.Create (strL, TBLPTRL);
      TPIC18x_MOVSF.Create (strH, TBLPTRH);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVWF.Create (iter, access_mode);
      lbl := TPIC18x_INCF.Create (result, dest_f, access_mode);
      TPIC18x_TBLRD.Create (tblrd_post_inc);
      TPIC18x_MOVF.Create (TABLAT, dest_w, access_mode);
      TPIC18x_SUBWF.Create (substrchar, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := lbl;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfCharInROMStr.enumerate_sub_subroutine_usage;
   begin
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfCharInROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


//===========================
//  TStrPosOfCharInEEPROMStr

constructor TStrPosOfCharInEEPROMStr.Create;
   begin
      inherited Create (push_count, pop_count, 'find StrPos of char in EEPROM string')
   end;

procedure TStrPosOfCharInEEPROMStr.generate_subroutine_code;
   const
      iter = 1;
      strL = 1;
      substrchar = 2;
      result = 3;
   var
      exit_label: TBranchTarget;
      lbl: TInstruction;
   begin
      exit_label := TBranchTarget.Create;
      TPIC18x_MOVSF.Create (strL, FSR1L);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_MOVWF.Create (iter, access_mode);
      lbl := TPIC18x_INCF.Create (result, dest_f, access_mode);
      GetEEPROMByte.Call;
      TPIC18x_ADDFSR.Create (1, 2);
      TPIC18x_SUBWF.Create (substrchar, dest_w, access_mode);
      exit_label.ComeFrom (TPIC18x_BZ.Create);
      TPIC18x_DECFSZ.Create (iter, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := lbl;
      TPIC18x_CLRF.Create (result, access_mode);
      exit_label.target_label := TPIC18x_ADDULNK.Create (pop_count);
      exit_label.set_client_destinations;
      exit_label.Free
   end;

procedure TStrPosOfCharInEEPROMStr.enumerate_sub_subroutine_usage;
   begin
      evaluate_called_subroutine_stack_use (GetEEPROMByte, 0)
   end;

{$ifdef INCLUDE_SIMULATION}
procedure TStrPosOfCharInEEPROMStr.report_stack_sizes;
   begin
      check_stack_sizes (push_count, pop_count, hw_stack_usage)
   end;
{$endif}


INITIALIZATION
   AppendCharToEEPROMString := TAppendCharToEEPROMString.Create;
   AppendCharToRAMStr := TAppendCharToRAMStr.Create;
   AppendEEPROMStrToEEPROMStr := TAppendEEPROMStrToEEPROMStr.Create;
   AppendEEPROMStrToRAMStr := TAppendEEPROMStrToRAMStr.Create;
   AppendRAMStrToEEPROMStr := TAppendRAMStrToEEPROMStr.Create;
   AppendRAMStrToRAMStr := TAppendRAMStrToRAMStr.Create;
   AppendROMStrToEEPROMStr := TAppendROMStrToEEPROMStr.Create;
   AppendROMStrToRAMStr := TAppendROMStrToRAMStr.Create;
   AssignEEPROMStrToEEPROMStr := TAssignEEPROMStrToEEPROMStr.Create;
   AssignEEPROMStrToRAMStr := TAssignEEPROMStrToRAMStr.Create;
   AssignRAMStrToEEPROMStr := TAssignRAMStrToEEPROMStr.Create;
   AssignRAMStrToRAMStr := TAssignRAMStrToRAMStr.Create;
   AssignROMStrToEEPROMStr := TAssignROMStrToEEPROMStr.Create;
   AssignROMStrToRAMStr := TAssignROMStrToRAMStr.Create;
   CompareEEPROMStrToChar := TCompareEEPROMStrToChar.Create;
   CompareRAMStrToChar := TCompareRAMStrToChar.Create;
   CompareRAMStrToEEPROMStr := TCompareRAMStrToEEPROMStr.Create;
   CompareRAMStrToRAMStr := TCompareRAMStrToRAMStr.Create;
   CompareRAMStrToROMStr := TCompareRAMStrToROMStr.Create;
   CompareROMStrToChar := TCompareROMStrToChar.Create;
   CompareROMStrToROMStr := TCompareROMStrToROMStr.Create;
   CompareROMStrToEEPROMStr := TCompareROMStrToEEPROMStr.Create;
   CompareEEPROMStrToEEPROMStr := TCompareEEPROMStrToEEPROMStr.Create;
   StrPosOfRAMStrInRAMStr := TStrPosOfRAMStrInRAMStr.Create;
   StrPosOfROMStrInRAMStr := TStrPosOfROMStrInRAMStr.Create;
   StrPosOfEEPROMStrInRAMStr := TStrPosOfEEPROMStrInRAMStr.Create;
   StrPosOfRAMStrInROMStr := TStrPosOfRAMStrInROMStr.Create;
   StrPosOfROMStrInROMStr := TStrPosOfROMStrInROMStr.Create;
   StrPosOfEEPROMStrInROMStr := TStrPosOfEEPROMStrInROMStr.Create;
   StrPosOfRAMStrInEEPROMStr := TStrPosOfRAMStrInEEPROMStr.Create;
   StrPosOfROMStrInEEPROMStr := TStrPosOfROMStrInEEPROMStr.Create;
   StrPosOfEEPROMStrInEEPROMStr := TStrPosOfEEPROMStrInEEPROMStr.Create;
   StrPosOfCharInRAMStr := TStrPosOfCharInRAMStr.Create;
   StrPosOfCharInROMStr := TStrPosOfCharInROMStr.Create;
   StrPosOfCharInEEPROMStr := TStrPosOfCharInEEPROMStr.Create;

FINALIZATION
   AppendCharToEEPROMString.Free;
   AppendCharToRAMStr.Free;
   AppendEEPROMStrToEEPROMStr.Free;
   AppendEEPROMStrToRAMStr.Free;
   AppendRAMStrToEEPROMStr.Free;
   AppendRAMStrToRAMStr.Free;
   AppendROMStrToEEPROMStr.Free;
   AppendROMStrToRAMStr.Free;
   AssignEEPROMStrToEEPROMStr.Free;
   AssignEEPROMStrToRAMStr.Free;
   AssignRAMStrToEEPROMStr.Free;
   AssignRAMStrToRAMStr.Free;
   AssignROMStrToEEPROMStr.Free;
   AssignROMStrToRAMStr.Free;
   CompareEEPROMStrToChar.Free;
   CompareRAMStrToChar.Free;
   CompareRAMStrToEEPROMStr.Free;
   CompareRAMStrToRAMStr.Free;
   CompareRAMStrToROMStr.Free;
   CompareROMStrToChar.Free;
   CompareROMStrToROMStr.Free;
   CompareROMStrToEEPROMStr.Free;
   CompareEEPROMStrToEEPROMStr.Free;
   StrPosOfRAMStrInRAMStr.Free;
   StrPosOfROMStrInRAMStr.Free;
   StrPosOfEEPROMStrInRAMStr.Free;
   StrPosOfRAMStrInROMStr.Free;
   StrPosOfROMStrInROMStr.Free;
   StrPosOfEEPROMStrInROMStr.Free;
   StrPosOfRAMStrInEEPROMStr.Free;
   StrPosOfROMStrInEEPROMStr.Free;
   StrPosOfEEPROMStrInEEPROMStr.Free;
   StrPosOfCharInRAMStr.Free;
   StrPosOfCharInROMStr.Free;
   StrPosOfCharInEEPROMStr.Free;

END.


