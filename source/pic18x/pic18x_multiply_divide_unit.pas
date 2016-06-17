UNIT pic18x_multiply_divide_unit;

INTERFACE

uses SysUtils, cpc_common_unit, pic18x_core_objects_unit;

type
   TIntegerInfo =
      class (TReferenceCountedObject)
         Size: integer;
         Signed: boolean;
         procedure init (info: TPIC18x_TypeInfo);
         constructor Create (info: TIntegerInfo);
            overload;
         constructor Create (info: TPIC18x_TypeInfo);
            overload;
      end;

procedure GenerateMultiplyCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);
procedure GenerateDivideCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);
procedure GenerateRemainderCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);

procedure GenerateMathRoutines;

IMPLEMENTATION

uses pic18x_macro_instructions_unit, pic18x_microprocessor_information_unit, cpc_multi_precision_integer_unit,
   pic18x_cpu_unit, pic18x_instructions_unit;

constructor TIntegerInfo.Create (info: TIntegerInfo);
   begin
      inherited Create;
      Size := info.Size;
      Signed := info.Signed
   end;

constructor TIntegerInfo.Create (info: TPIC18x_TypeInfo);
   begin
      inherited Create;
      Size := info.Size;
      Signed := info.Signed
   end;

procedure TIntegerInfo.init (info: TPIC18x_TypeInfo);
   begin
      Size := info.Size;
      Signed := info.Signed
   end;

type
   TMultiplyRoutine =
      class
         result_size: integer;
         a_info, b_info: TIntegerInfo;
         callers: array of TInstruction;
         constructor Create (_result_size: integer; _a_info, _b_info: TIntegerInfo);
         destructor Destroy;
            override;
      end;

type
   TDivideRoutine =
      class
         a_size_in_bits, b_size_in_bits: integer;  // negative if signed, positive if unsigned
         result_size: integer;
         callers: array of TInstruction;
         constructor Create (_a_size_in_bits, _b_size_in_bits, _result_size: integer);
      end;

type
   TRemainderRoutine =
      class
         a_size_in_bits, b_size_in_bits: integer;  // negative if signed, positive if unsigned
         result_size: integer;
         callers: array of TInstruction;
         constructor Create (_a_size_in_bits, _b_size_in_bits, _result_size: integer);
      end;

var
   MultiplyRoutines: array of TMultiplyRoutine;
   DivideRoutines: array of TDivideRoutine;
   RemainderRoutines: array of TRemainderRoutine;

function mult_annotation (result_size: integer; a_info, b_info: TIntegerInfo): string;
   begin
      result := 'tos*' + IntToStr(result_size) + ' := ';

      if a_info.signed then
         result := result + 'tos*s'
      else
         result := result + 'tos*u';
      result := result + IntToStr(abs(a_info.size)) + '@[' + IntToStr(b_info.size + 1 + result_size - a_info.size) + '] ';

      result := result + '* ';

      if b_info.signed then
         result := result + 'tos*s'
      else
         result := result + 'tos*u';
      result := result + IntToStr(b_info.size) + '@[1]'
   end;

function div_annotation (operator: string; result_size, abits, a_addr, bbits: integer): string;
   var
      a_size, b_size: integer;
   begin
      a_size := (abs(abits) + 7) div 8;
      b_size := (abs(bbits) + 7) div 8;

      result := 'tos*' + IntToStr(result_size) + ' := ';

      if abits < 0 then
         result := result + 'tos*s'
      else
         result := result + 'tos*u';
      result := result + format ('%d:%d@[%d] %s ', [a_size, abs(abits), a_addr, operator]);

      if bbits < 0 then
         result := result + 'tos*s'
      else
         result := result + 'tos*u';
      result := result + format ('%d:%d@[1]', [b_size, abs(bbits)])
   end;

function generate_multiply (result_size: integer; a_info, b_info: TIntegerInfo): TInstruction;
   // stack before call:
   //    b: b_info.size bytes
   //    uninitialized: result_size - a_info.size bytes
   //    a: a_info.size bytes
   // during execution
   //    a: a_info.size bytes (copied from result area)
   //    b: b_info.size bytes
   //    result: result_size
   // stack after call:
   //    result: result_size

   var
      result_addr, a_addr, b_addr: integer;

   function requested_result_byte (offset: integer): boolean;
      begin
         result := offset < result_size
      end;

   function calc_horizontally: boolean;
      begin
         result := a_info.size >= b_info.size
      end;

   procedure gen_init_multiply_chain (a, b: integer);
      begin
         // assert: first byte of operand a (a_addr+a_info.size-1-a) to multiply is alreay in wreg
         TPIC18x_MULWF.Create (b_addr + b_info.size - 1 - b, access_mode);
         TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
         TPIC18x_MOVWF.Create (result_addr + result_size - 1 - (a+b), access_mode);
         if requested_result_byte (a+b+1) then
            begin
               TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
               TPIC18x_MOVWF.Create (result_addr+result_size-1-(a+b+1), access_mode)
            end;
         if calc_horizontally then
            a := a + 2
         else
            b := b + 2;
         while (a < a_info.size) and (b < b_info.size) do
            begin
               TPIC18x_MOVF.Create (a_addr+a_info.size-1-a, dest_w, access_mode);
               TPIC18x_MULWF.Create (b_addr+b_info.size-1-b, access_mode);
               TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
               TPIC18x_MOVWF.Create (result_addr+result_size-1-(a+b), access_mode);
               if requested_result_byte (a+b+1) then
                  begin
                     TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                     TPIC18x_MOVWF.Create (result_addr+result_size-1-(a+b+1), access_mode)
                  end;
               if calc_horizontally then
                  a := a + 2
               else
                  b := b + 2
            end;
         if calc_horizontally then
            while a < result_size do
               begin
                  TPIC18x_CLRF.Create (result_addr+result_size-1-a, access_mode);
                  a := a + 1
               end
         else
            while b < result_size do
               begin
                  TPIC18x_CLRF.Create (result_addr+result_size-1-b, access_mode);
                  b := b + 1
               end
      end;

   procedure gen_multiply_carry_chain (a, b: integer; handle_final_carry: boolean);
      begin
         TPIC18x_MOVF.Create (a_addr + a_info.size - 1 - a, dest_w, access_mode);
         TPIC18x_MULWF.Create (b_addr + b_info.size - 1 - b, access_mode);
         TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
         if (a=0) and (b=0) then
            begin
               TPIC18x_MOVWF.Create (result_addr + result_size - 1, access_mode);
               if requested_result_byte (1) then
                  begin
                     TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                     TPIC18x_ADDWF.Create (result_addr + result_size - 2, dest_f, access_mode)
                  end
            end
         else
            begin
               TPIC18x_ADDWF.Create (result_addr+result_size-1-(a+b), dest_f, access_mode);
               if requested_result_byte (a+b+1) then
                  begin
                     TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                     TPIC18x_ADDWFC.Create (result_addr+result_size-1-(a+b+1), dest_f, access_mode)
                  end
            end;
         if calc_horizontally then
            a := a + 2
         else
            b := b + 2;
         while (a < a_info.size) and (b < b_info.size) do
            begin
               if requested_result_byte (a+b) then
                  begin
                     TPIC18x_MOVF.Create (a_addr+a_info.size-1-a, dest_w, access_mode);
                     TPIC18x_MULWF.Create (b_addr+b_info.size-1-b, access_mode);
                     TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
                     TPIC18x_ADDWFC.Create (result_addr+result_size-1-(a+b), dest_f, access_mode);
                     if requested_result_byte (a+b+1) then
                        begin
                           TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                           TPIC18x_ADDWFC.Create (result_addr+result_size-1-(a+b+1), dest_f, access_mode)
                        end
                  end;
               if calc_horizontally then
                  a := a + 2
               else
                  b := b + 2
            end;
         if handle_final_carry and requested_result_byte (a+b) then
            begin
               TPIC18x_CLRF.Create (WREG, access_mode);
               TPIC18x_ADDWFC.Create (result_addr + result_size - 1 - (a+b), dest_f, access_mode)
            end
      end;

   var
      i: integer;
      bra: TPIC18x_BRA;

   begin
      TAssemblySourceBlankLine.Create;
      result := TAssemblyLabel.Create;
      result.annotation := 'subroutine to multiply ' + mult_annotation (result_size, a_info, b_info);

      TPIC18x_SUBFSR.Create (2, a_info.size);

      a_addr := 1;
      b_addr := a_addr + a_info.size;
      result_addr := b_addr + b_info.size;

      // copy a to temp
      if odd(a_info.size) and (not a_info.signed) then
         begin
            for i := a_info.size-1 downto 2 do
               begin
                  TPIC18x_MOVF.Create (result_addr + result_size - 1 - i, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (a_addr + a_info.size - 1 - i, access_mode)
               end;
            TPIC18x_MOVF.Create (result_addr + result_size - 1, dest_w, access_mode);
            TPIC18x_MOVWF.Create (a_addr + a_info.size - 1, access_mode);
            TPIC18x_MOVF.Create (result_addr + result_size - 2, dest_w, access_mode);
            TPIC18x_MOVWF.Create (a_addr+a_info.size - 2, access_mode)
         end
      else
         for i := a_info.size-1 downto 0 do
            begin
               TPIC18x_MOVF.Create (result_addr+result_size - 1 - i, dest_w, access_mode);
               TPIC18x_MOVWF.Create (a_addr+a_info.size - 1 - i, access_mode)
            end;
      // wreg contains first byte of a to be used in gen_init_multiply_chain

      if calc_horizontally then
         begin
            if a_info.size = 1 then
               begin
                  TPIC18x_MULWF.Create (b_addr + b_info.size - 1, access_mode);
                  TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (result_addr + result_size - 1, access_mode);
                  if requested_result_byte (1) then
                     begin
                        TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
                        TPIC18x_MOVWF.Create (result_addr + result_size - 2, access_mode)
                     end
               end
            else
               begin
                  if odd (a_info.size) then
                     begin
                        gen_init_multiply_chain (1, 0);
                        gen_multiply_carry_chain (0, 0, false)
                     end
                  else
                     begin
                        gen_init_multiply_chain (0, 0);
                        gen_multiply_carry_chain (1, 0, false)
                     end;
                  for i := 1 to b_info.size-1 do
                     if odd(a_info.size) then
                        begin
                           gen_multiply_carry_chain (1, i, true);
                           gen_multiply_carry_chain (0, i, false)
                        end
                     else
                        begin
                           gen_multiply_carry_chain (0, i, true);
                           gen_multiply_carry_chain (1, i, false)
                        end
               end
         end
      else
         begin
            if odd (b_info.size) then
               begin
                  gen_init_multiply_chain (0, 1);
                  gen_multiply_carry_chain (0, 0, false)
               end
            else
               begin
                  gen_init_multiply_chain (0, 0);
                  gen_multiply_carry_chain (0, 1, false)
               end;
            for i := 1 to a_info.size-1 do
               if odd(b_info.size) then
                  begin
                     gen_multiply_carry_chain (i, 1, true);
                     gen_multiply_carry_chain (i, 0, false)
                  end
               else
                  begin
                     gen_multiply_carry_chain (i, 0, true);
                     gen_multiply_carry_chain (i, 1, false)
                  end
         end;

      if (a_info.signed) and (requested_result_byte (a_info.size)) then
         begin
            TPIC18x_BTFSS.Create (a_addr, 7, access_mode);
            bra := TPIC18x_BRA.Create;
            TPIC18x_MOVF.Create (b_addr+b_info.size-1, dest_w, access_mode);
            TPIC18x_SUBWF.Create (result_addr+result_size-1-a_info.size, dest_f, access_mode);
            for i := a_info.size+1 to result_size-1 do
               begin
                  TPIC18x_MOVF.Create (b_addr+b_info.size-1-(i-a_info.size), dest_w, access_mode);
                  TPIC18x_SUBWFB.Create (result_addr+result_size-1-i, dest_f, access_mode)
               end;
            bra.dest := TAssemblyLabel.Create
         end;

      if (b_info.signed) and (requested_result_byte (b_info.size)) then
         begin
            TPIC18x_BTFSS.Create (b_addr, 7, access_mode);
            bra := TPIC18x_BRA.Create;
            TPIC18x_MOVF.Create (a_addr + a_info.size - 1, dest_w, access_mode);
            TPIC18x_SUBWF.Create (result_addr + result_size - 1 - b_info.size, dest_f, access_mode);
            for i := b_info.size+1 to result_size-1 do
               begin
                  TPIC18x_MOVF.Create (a_addr + a_info.size - 1 - (i - b_info.size), dest_w, access_mode);
                  TPIC18x_SUBWFB.Create (result_addr + result_size - 1 - i, dest_f, access_mode)
               end;
            bra.dest := TAssemblyLabel.Create
         end;

      TPIC18x_ADDULNK.Create (a_info.size + b_info.size)
   end;

//   ; EXAMPLE:
//   ;
//   ; include "18fdiv.inc"
//   ; cblock 0x80 ; each arg must be contiguous,
//   ;             ; but the args don't have to be adjacent
//   ;  ARGA:4 ; dividend, MSB in ARGA, LSB in ARGA+n
//   ;  ARGB:2 ; divisor
//   ;  REM:2 ; remainder (output, same size as divisor)
//   ;  LOOP
//   ; endc
//   ; mkdiv foo, ARGA, ARGB, REM, LOOP, d'32', d'16', UNSIGNED
//   ;
//   ; Makes a function foo32x16u.  If you don't need all 8*n bits
//   ; of the divisor, you can reduce it by one to get a slightly
//   ; faster function:
//   ;
//   ; mkdiv foo, ARGA, ARGB, REM, LOOP, d'32', d'15', UNSIGNED
//   ;
//   ; (No other shortening of the bits helps or is allowed.)
//   ;
//   ; If you need a signed divider, change the last argument:
//   ;
//   ; mkdiv foo, ARGA, ARGB, REM, LOOP, d'24', d'7', SIGNED
//   ;
//   ; This produces foo24x7s.  Notice you can re-use the blocks
//   ; of memory as long as they are at least long enough to fit.
//   ; With signed divide it makes a lot of sense to drop a bit on
//   ; the divisor, since only -2^n-1 doesn't fit once the divider
//   ; takes the absolute value.
//   ;

procedure generate_negation_code (addr, size: integer);
   overload;
   var
      i: integer;
   begin
      // assert: WREG must be clear if size > 1
      i := 0;
      while i < size-1 do
         begin
            TPIC18x_COMF.Create (addr+i, dest_f, access_mode);
            i := i + 1
         end;
      TPIC18x_NEGF.Create (addr+i, access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_ADDWFC.Create (addr+i, dest_f, access_mode)
         end
   end;

procedure generate_negation_code (addr: array of integer; size: integer);
   overload;
   var
      i: integer;
   begin
      // assert: WREG must be clear if size > 1
      i := 0;
      while i < size-1 do
         begin
            TPIC18x_COMF.Create (addr[i], dest_f, access_mode);
            i := i + 1
         end;
      TPIC18x_NEGF.Create (addr[i], access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_ADDWFC.Create (addr[i], dest_f, access_mode)
         end
   end;


//   Division/remainder code based on Assembly Macros by:
//      Copyright © 2004 Ben J. Jackson <ben@ben.com>
//      May be freely used, modified and distributed,
//      provided this notice remains intact.  This software
//      is provided "as is" without express or implied warranty.

function generate_div (abits, bbits, result_size: integer): TInstruction;
   // stack before call:
   //    b: b_size bytes
   //    a: a_size bytes
   // during execution
   //    loop counter: 1 byte
   //    remainder: b_size bytes
   //    b: b_size bytes
   //    result: a_size bytes
   // stack after call:
   //    result: result_size bytes
   var
      a_size, b_size: integer;
      a_addr, b_addr, rem_addr, loop_counter_addr: integer;
      bc1, bc2: TPIC18x_BC;
      bra: TPIC18x_BRA;
      loop_label: TAssemblyLabel;
      i: integer;
      wreg_clear: boolean;
   begin
      bc2 := nil;
      a_size := result_size;
      b_size := (abs(bbits) + 7) div 8;
      assert (not (((abs(abits) > 127) and (abs(bbits) = b_size * 8)) or (abs(abits) > 256)));
         //  If you shorten the divisor by a bit you get a wider dividend
         //  because LOOP.7 is freed up
         //  error "18fdiv has no optimizations for odd bit length dividends."
         //  This could be done by clearing carry and pre-rotating by
         //  (awords * 8 - abits) and then doing the normal divide.
      assert (abs(bbits) >= (b_size * 8) - 1);
          // 	messg "18fdiv has no optimizations for divisors any shorter than 8n-1."
          //   The loop is simplified by not having the MSB of the divisor count,
          //   but nothing any shorter than that triggers any optimizations.

      TAssemblySourceBlankLine.Create;
      TAssemblyComment.Create ('Divide ' + div_annotation ('div', result_size, abits, b_size+1, bbits));
      result := TAssemblyLabel.Create;

      loop_counter_addr := 1;
      rem_addr := 2;
      b_addr := rem_addr + b_size;
      a_addr := b_addr + b_size;

      // clear remainder
      for i := 0 to b_size-1 do
         TPIC18x_CLRF.Create (POSTDEC2, access_mode);
      // initialize loop counter
      TPIC18x_PUSHL.Create (result_size*8); //(abs(abits));

      if (abits < 0) or (bbits < 0) then
         begin
            // signed div will find the sign of the result, stash
            // it away, then take the absolute values of the terms,
            // do the divide, and restore the sign to the result and
            // remainder.
            if (abits < 0) and (bbits < 0) then
               begin
                  TPIC18x_MOVF.Create (b_addr, dest_w, access_mode);
                  TPIC18x_XORWF.Create (a_addr, dest_w, access_mode)
               end
            else if abits < 0 then
               TPIC18x_MOVF.Create (a_addr, dest_w, access_mode)
            else  // bbits < 0
               TPIC18x_MOVF.Create (b_addr, dest_w, access_mode);
            TPIC18x_MOVWF.Create (rem_addr, access_mode);  // sign in bit 7

            wreg_clear := false;
            if abits < 0 then
               begin  // a may be negative
                  if a_size > 1 then
                     begin
                        TPIC18x_CLRF.Create (WREG, access_mode);
                        wreg_clear := true;
                        TPIC18x_BTFSS.Create (a_addr, 7, access_mode);
                        bra := TPIC18x_BRA.Create
                     end
                  else
                     begin
                        TPIC18x_BTFSC.Create (a_addr, 7, access_mode);
                        bra := nil
                     end;
                  generate_negation_code (a_addr, a_size);
                  if bra <> nil then
                     bra.dest := TAssemblyLabel.Create
               end;

            if bbits < 0 then
               begin  // b may be negative
                  if b_size > 1 then
                     begin
                        if not wreg_clear then
                           TPIC18x_CLRF.Create (WREG, access_mode);
                        TPIC18x_BTFSS.Create (b_addr, 7, access_mode);
                        bra := TPIC18x_BRA.Create
                     end
                  else
                     begin
                        TPIC18x_BTFSC.Create (b_addr, 7, access_mode);  // it will only be 1 instr
                        bra := nil
                     end;
                  generate_negation_code (b_addr, b_size);
                  if bra <> nil then
                     bra.dest := TAssemblyLabel.Create
               end;

            TPIC18x_RLCF.Create (rem_addr, dest_w, access_mode);
            TPIC18x_CLRF.Create (rem_addr, access_mode)
         end;
      loop_label := TAssemblyLabel.Create;
      i := a_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_RLCF.Create (a_addr+i, dest_f, access_mode)
         end;
      i := b_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_RLCF.Create (rem_addr+i, dest_f, access_mode)
         end;
       // if we're using all 2^n bits, use loop.7 to save extra bit
      if abs(bbits) = b_size * 8 then
         begin
            TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
            TPIC18x_BSF.Create (loop_counter_addr, 7, access_mode)
         end;
      i := b_size - 1;
      TPIC18x_MOVF.Create (b_addr+i, dest_w, access_mode);
      TPIC18x_SUBWF.Create (rem_addr+i, dest_f, access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_MOVF.Create (b_addr+i, dest_w, access_mode);
            TPIC18x_SUBWFB.Create (rem_addr+i, dest_f, access_mode)
         end;
      // if no borrow, shortcut everything and get that C into answer
      bc1 := TPIC18x_BC.Create;
      if abs(bbits) = b_size * 8 then
         begin
            // if a saved bit, try the rescue
            TPIC18x_RLCF.Create (loop_counter_addr, dest_w, access_mode);  // must get it in C for branch
            bc2 := TPIC18x_BC.Create;
         end;
      // restore
      i := b_size - 1;
      if (i > 0) or (abs(bbits) = b_size * 8) then
          TPIC18x_MOVF.Create (b_addr+i, dest_w, access_mode);
      TPIC18x_ADDWF.Create (rem_addr+i, dest_f, access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_MOVF.Create (b_addr+i, dest_w, access_mode);
            TPIC18x_ADDWFC.Create (rem_addr+i, dest_f, access_mode)
         end;
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      bc1.dest := TAssemblyLabel.Create;
      if bc2 <> nil then
         bc2.dest := bc1.dest;
      if abs(bbits) = b_size * 8 then
         TPIC18x_BCF.Create (loop_counter_addr, 7, access_mode);
      TPIC18x_DECFSZ.Create (loop_counter_addr, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := loop_label;
      // rotate in the final result bit
      i := a_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_RLCF.Create (a_addr+i, dest_f, access_mode)
         end;
      if (abits < 0) or (bbits < 0) then
         begin
            // we stuffed the sign into the result and just rolled it off
            TPIC18x_BTFSS.Create (STATUS, status_c, access_mode);
            bra := TPIC18x_BRA.Create;
            if a_size > 1 then
               TPIC18x_CLRF.Create (WREG, access_mode);
            generate_negation_code (a_addr, a_size);
            bra.dest := TAssemblyLabel.Create
         end;
      TPIC18x_ADDULNK.Create (1+(2*b_size)+(a_size-result_size))
   end;

function generate_remainder (abits, bbits: integer; result_size: integer): TInstruction;
   // stack before call:
   //    b: b_size bytes
   //    a: a_size bytes
   // during execution
   //    loop counter: 1 byte
   //    remainder: b_size bytes
   //    b: b_size bytes
   //    division result: a_size bytes
   // stack after call:
   //    remainder: b_size bytes
   var
      a_size, b_size: integer;
      a_addr, b_addr: array of integer;
      rem_addr, loop_counter_addr, remainder_sign_addr, addr: integer;
      bra: TPIC18x_BRA;
      bc1, bc2: TPIC18x_BC;
      loop_label: TAssemblyLabel;
      i: integer;
      wreg_clear: boolean;
   begin
      a_size := (abs(abits) + 7) div 8;
      b_size := (abs(bbits) + 7) div 8;
      assert (not (((abs(abits) > 127) and (abs(bbits) = b_size * 8)) or (abs(abits) > 256)));
        //  If you shorten the divisor by a bit you get a wider dividend
        //  because LOOP.7 is freed up
      assert (abs(abits) = a_size * 8);
        //  error "18fdiv has no optimizations for odd bit length dividends."
        //  This could be done by clearing carry and pre-rotating by
        //  (awords * 8 - abits) and then doing the normal divide.
      assert (abs(bbits) >= (b_size * 8) - 1);
         // 	messg "18fdiv has no optimizations for divisors any shorter than 8n-1."
         //   The loop is simplified by not having the MSB of the divisor count,
         //   but nothing any shorter than that triggers any optimizations.

      SetLength (a_addr, a_size);
      SetLength (b_addr, b_size);

      TAssemblySourceBlankLine.Create;
      TAssemblyComment.Create ('Remainder ' + div_annotation ('mod', result_size, abits, b_size+1, bbits));
      result := TAssemblyLabel.Create;
      if a_size = b_size then
         begin
            for i := 0 to a_size-1 do   // copy a
               begin
                  TPIC18x_MOVF.Create (a_size+b_size, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
               end;
            TPIC18x_PUSHL.Create (abs(abits));    // initialize loop counter
            if (abits < 0) and (bbits < 0) then
               begin
                  TPIC18x_MOVF.Create (2, dest_w, access_mode);  // pick up MSB of a
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                  remainder_sign_addr := 1;
                  loop_counter_addr := 2
               end
            else
               begin
                  remainder_sign_addr := 666;
                  loop_counter_addr := 1
               end;
            addr := loop_counter_addr + 1;
            for i := 0 to a_size-1 do
               begin
                  a_addr[i] := addr;
                  addr := addr + 1
               end;
            for i := 0 to b_size-1  do
               begin
                  b_addr[i] := addr;
                  addr := addr + 1
               end;
            rem_addr := addr
         end
      else if a_size > b_size then
         begin
            for i := 0 to b_size-1 do   // copy part of a to be occupied by result
               begin
                  TPIC18x_MOVF.Create (a_size+b_size, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
               end;
            TPIC18x_PUSHL.Create (abs(abits));    // initialize loop counter
            if (abits < 0) and (bbits < 0) then
               begin
                  TPIC18x_MOVF.Create ((2*b_size)+2, dest_w, access_mode);  // pick up MSB of a
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                  remainder_sign_addr := 1;
                  loop_counter_addr := 2
               end
            else
               begin
                  remainder_sign_addr := 666;
                  loop_counter_addr := 1
               end;
            addr := loop_counter_addr + 1;
            for i := 0 to b_size-1 do
               begin
                  a_addr[i+a_size-b_size] := addr;
                  addr := addr + 1
               end;
            for i := 0 to b_size-1  do
               begin
                  b_addr[i] := addr;
                  addr := addr + 1
               end;
            for i := 0 to a_size-b_size-1 do
               begin
                  a_addr[i] := addr;
                  addr := addr + 1
               end;
            rem_addr := addr
         end
      else   // a_size < b_size
         begin
            for i := 0 to b_size-1 do   // copy a and lowest bytes of b
               begin
                  TPIC18x_MOVF.Create (a_size+b_size, dest_w, access_mode);
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode)
               end;
            TPIC18x_PUSHL.Create (abs(abits));    // initialize loop counter
            if (abits < 0) and (bbits < 0) then
               begin
                  TPIC18x_MOVF.Create (2, dest_w, access_mode);  // pick up MSB of a
                  TPIC18x_MOVWF.Create (POSTDEC2, access_mode);
                  remainder_sign_addr := 1;
                  loop_counter_addr := 2
               end
            else
               begin
                  remainder_sign_addr := 666;
                  loop_counter_addr := 1
               end;
            addr := loop_counter_addr + 1;
            for i := 0 to b_size-a_size-1 do
               begin
                  b_addr[i] := addr;
                  addr := addr + 1
               end;
            for i := 0 to a_size-1 do
               begin
                  a_addr[i] := addr;
                  addr := addr + 1
               end;
            for i := b_size-a_size to b_size-1 do
               begin
                  b_addr[i] := addr;
                  addr := addr + 1
               end;
            rem_addr := addr
         end;

      if (abits < 0) or (bbits < 0) then
         begin
            // signed div will find the sign of the result, stash
            // it away, then take the absolute values of the terms,
            // do the divide, and restore the sign to the result and
            // remainder.
            if (abits < 0) and (bbits < 0) then
               begin
                  TPIC18x_MOVF.Create (a_addr[0], dest_w, access_mode);
                  TPIC18x_MOVWF.Create (remainder_sign_addr, access_mode);
                  TPIC18x_XORWF.Create (b_addr[0], dest_w, access_mode);
                  TPIC18x_MOVWF.Create (rem_addr, access_mode)  // sign in bit 7
               end
            else if abits < 0 then
               begin
                  TPIC18x_MOVF.Create (a_addr[0], dest_w, access_mode);
                  TPIC18x_MOVWF.Create (rem_addr, access_mode)  // sign in bit 7
               end
            else  // bbits < 0
               begin
                  TPIC18x_MOVF.Create (b_addr[0], dest_w, access_mode);
                  TPIC18x_MOVWF.Create (rem_addr, access_mode)  // sign in bit 7
               end;

            wreg_clear := false;
            if abits < 0 then
               begin  // a may be negative
                  bra := nil;
                  if a_size > 1 then
                     begin
                        TPIC18x_CLRF.Create (WREG, access_mode);
                        wreg_clear := true;
                        TPIC18x_BTFSS.Create (a_addr[0], 7, access_mode);
                        bra := TPIC18x_BRA.Create
                     end
                  else
                     TPIC18x_BTFSC.Create (a_addr[0], 7, access_mode);  // it will only be 1 instr
                  generate_negation_code (a_addr[0], a_size);
                  if bra <> nil then
                     bra.dest := TAssemblyLabel.Create
               end;

            if bbits < 0 then
               begin  // b may be negative
                  bra := nil;
                  if b_size > 1 then
                     begin
                        if not wreg_clear then
                           TPIC18x_CLRF.Create (WREG, access_mode);
                        TPIC18x_BTFSS.Create (b_addr[0], 7, access_mode);
                        bra := TPIC18x_BRA.Create
                     end
                  else
                     TPIC18x_BTFSC.Create (b_addr[0], 7, access_mode);  // it will only be 1 instr
                  generate_negation_code (b_addr, b_size);
                  if bra <> nil then
                     bra.dest := TAssemblyLabel.Create
               end;

            TPIC18x_RLCF.Create (rem_addr, dest_w, access_mode)
         end;
      i := b_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_CLRF.Create (rem_addr+i, access_mode)
         end;
      loop_label := TAssemblyLabel.Create;
      i := a_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_RLCF.Create (a_addr[i], dest_f, access_mode)
         end;
      i := b_size;
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_RLCF.Create (rem_addr+i, dest_f, access_mode)
         end;
       // if we're using all 2^n bits, use loop.7 to save extra bit
      if abs(bbits) = b_size * 8 then
         begin
            TPIC18x_BTFSC.Create (STATUS, status_c, access_mode);
            TPIC18x_BSF.Create (loop_counter_addr, 7, access_mode)
         end;
      i := b_size - 1;
      TPIC18x_MOVF.Create (b_addr[i], dest_w, access_mode);
      TPIC18x_SUBWF.Create (rem_addr+i, dest_f, access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_MOVF.Create (b_addr[i], dest_w, access_mode);
            TPIC18x_SUBWFB.Create (rem_addr+i, dest_f, access_mode)
         end;
      // if no borrow, shortcut everything and get that C into answer
      bc1 := TPIC18x_BC.Create;
      if abs(bbits) = b_size * 8 then
         begin
            // if a saved bit, try the rescue
            TPIC18x_RLCF.Create (loop_counter_addr, dest_w, access_mode);  // must get it in C for branch
            bc2 := TPIC18x_BC.Create;
         end
      else
         bc2 := nil;
      // restore
      i := b_size - 1;
      if (i > 0) or (abs(bbits) = b_size * 8) then
          TPIC18x_MOVF.Create (b_addr[i], dest_w, access_mode);
      TPIC18x_ADDWF.Create (rem_addr+i, dest_f, access_mode);
      while i > 0 do
         begin
            i := i - 1;
            TPIC18x_MOVF.Create (b_addr[i], dest_w, access_mode);
            TPIC18x_ADDWFC.Create (rem_addr+i, dest_f, access_mode)
         end;
      TPIC18x_BCF.Create (STATUS, status_c, access_mode);
      bc1.dest := TAssemblyLabel.Create;
      if bc2 <> nil then
         bc2.dest := bc1.dest;
      if abs(bbits) = b_size * 8 then
         TPIC18x_BCF.Create (loop_counter_addr, 7, access_mode);
      TPIC18x_DECFSZ.Create (loop_counter_addr, dest_f, access_mode);
      TPIC18x_BRA.Create.dest := loop_label;
      bra := nil;
      if abits < 0 then
         begin
            if bbits > 0 then
               TPIC18x_BTFSS.Create (a_addr[0], 7, access_mode)
            else // bbits < 0
               TPIC18x_BTFSS.Create (remainder_sign_addr, 7, access_mode);
            TPIC18x_BRA.Create;
            if b_size > 1 then
               TPIC18x_CLRF.Create (WREG, access_mode);
            generate_negation_code (rem_addr, b_size)
         end;
      if bra <> nil then
         bra.dest := TAssemblyLabel.Create;
      if (abits < 0) and (bbits < 0) then
         TPIC18x_ADDULNK.Create (a_size + (2*b_size) - result_size + 2)
      else
         TPIC18x_ADDULNK.Create (a_size + (2*b_size) - result_size + 1)
   end;

constructor TMultiplyRoutine.Create (_result_size: integer; _a_info, _b_info: TIntegerInfo);
   begin
      result_size := _result_size;
      a_info := _a_info;
      b_info := _b_info
   end;

destructor TMultiplyRoutine.Destroy;
   begin
      a_info.Release;
      b_info.Release
   end;

constructor TDivideRoutine.Create (_a_size_in_bits, _b_size_in_bits, _result_size: integer);
   begin
      a_size_in_bits := _a_size_in_bits;
      b_size_in_bits := _b_size_in_bits;
      result_size := _result_size
   end;

constructor TRemainderRoutine.Create ( _a_size_in_bits, _b_size_in_bits, _result_size: integer);
   begin
      a_size_in_bits := _a_size_in_bits;
      b_size_in_bits := _b_size_in_bits;
      result_size := _result_size
   end;

procedure GenerateMultiplyCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);
   var
      i: integer;
      caller: TInstruction;
      b_integer_info: TIntegerInfo;
   procedure add_to_list;
      var idx: integer;
      begin
         idx := Length(MultiplyRoutines[i].callers);
         SetLength (MultiplyRoutines[i].callers, idx+1);
         MultiplyRoutines[i].callers[idx] := caller
      end;
   begin
      b_integer_info := TIntegerInfo.Create (b_info);
      if (a_info.size = 1)
         and
         (b_integer_info.size = 1)
         and
         (result_size = 1) then
         begin
            // the following will work for signed or unsigned operands
            TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := mult_annotation (result_size, a_info, b_integer_info);
            StackUsageCounter.Pop(1);
            TPIC18x_MULWF.Create (1, access_mode);
            TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
            TPIC18x_MOVWF.Create (1, access_mode);
            b_integer_info.Release
         end
      else if (a_info.size = 1)
              and
              (not a_info.Signed)
              and
              (b_integer_info.size = 1)
              and
              (not b_info.Signed)
              and
              (result_size = 2) then
         begin
            // the following will work only for unsigned operands
            TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := mult_annotation (result_size, a_info, b_integer_info);
            StackUsageCounter.Pop(1);
            TPIC18x_MULWF.Create (2, access_mode);
            TPIC18x_MOVF.Create (PRODL, dest_w, access_mode);
            TPIC18x_MOVWF.Create (2, access_mode);
            TPIC18x_MOVF.Create (PRODH, dest_w, access_mode);
            TPIC18x_MOVWF.Create (1, access_mode);
            b_integer_info.Release
         end
      else
         begin
            caller := TCallMacro.Create;
            caller.annotation := mult_annotation (result_size, a_info, b_integer_info);
            StackUsageCounter.Push (a_info.Size);
            StackUsageCounter.Pop (a_info.Size + b_info.Size);

            for i := 0 to Length(MultiplyRoutines)-1 do
               if (MultiplyRoutines[i].result_size = result_size)
                  and
                  (MultiplyRoutines[i].a_info.size = a_info.size)
                  and
                  (MultiplyRoutines[i].b_info.size = b_info.size)
                  and
                  (MultiplyRoutines[i].a_info.signed = a_info.signed)
                  and
                  (MultiplyRoutines[i].b_info.signed = b_info.signed) then
                  begin
                     add_to_list;
                     b_integer_info.Release;
                     exit
                  end;
            i := Length(MultiplyRoutines);
            SetLength (MultiplyRoutines, i+1);
            MultiplyRoutines[i] := TMultiplyRoutine.Create (result_size, TIntegerInfo.Create(a_info), b_integer_info);
            add_to_list
         end
   end;

procedure GenerateDivideCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);
   var
      i: integer;
      caller: TInstruction;
   procedure add_to_list;
      var idx: integer;
      begin
         idx := Length(DivideRoutines[i].callers);
         SetLength (DivideRoutines[i].callers, idx+1);
         DivideRoutines[i].callers[idx] := caller
      end;
   var
      abits, bbits: integer;
   begin
      if a_info.Signed then
         abits := -a_info.Size * 8
      else
         abits := a_info.Size * 8;

      if b_info.Signed then
         bbits := -((b_info.Size * 8) - 1)
      else  // unsigned
         if b_info.max_value.BitsRequiredForUnsignedValue mod 8 = 0 then
            bbits := b_info.Size * 8
         else
            bbits := (b_info.Size * 8) - 1;

      caller := TCallMacro.Create;
      caller.annotation := div_annotation ('div', result_size, abits, b_info.Size+1, bbits);
      StackUsageCounter.Push (b_info.Size + 1);     // space for remainder + loop count
      StackUsageCounter.Pop ((2*b_info.Size) + 1);

      for i := 0 to Length(DivideRoutines)-1 do
         if (DivideRoutines[i].a_size_in_bits = abits)
            and
            (DivideRoutines[i].b_size_in_bits = bbits)
            and
            (DivideRoutines[i].result_size = result_size) then
            begin
               add_to_list;
               exit
            end;
      i := Length(DivideRoutines);
      SetLength (DivideRoutines, i+1);
      DivideRoutines[i] := TDivideRoutine.Create (abits, bbits, result_size);
      add_to_list
   end;

procedure GenerateRemainderCode (result_size: integer; a_info: TIntegerInfo; b_info: TPIC18x_TypeInfo);
   var
      i: integer;
      caller: TInstruction;
   procedure add_to_list;
      var idx: integer;
      begin
         idx := Length(RemainderRoutines[i].callers);
         SetLength (RemainderRoutines[i].callers, idx+1);
         RemainderRoutines[i].callers[idx] := caller
      end;
   var
      abits, bbits: integer;
   begin
      if a_info.Signed then
         abits := -a_info.Size * 8
      else
         abits := a_info.Size * 8;

      if b_info.Signed then
         bbits := -((b_info.Size * 8) - 1)
      else  // unsigned
         if b_info.max_value.BitsRequiredForUnsignedValue mod 8 = 0 then
            bbits := b_info.Size * 8
         else
            bbits := (b_info.Size * 8) - 1;

      caller := TCallMacro.Create;
      caller.annotation := div_annotation ('mod', result_size, abits, b_info.Size+1, bbits);
      if a_info.Signed and b_info.Signed then
         begin
            StackUsageCounter.Push (b_info.Size + 2);     // space for b, loop counter and result sign
            StackUsageCounter.Pop (a_info.Size + (2*b_info.Size) - result_size + 2)
         end
      else
         begin
            StackUsageCounter.Push (b_info.Size + 1);     // space for b and loop counter
            StackUsageCounter.Pop (a_info.Size + (2*b_info.Size) - result_size + 1)
         end;

      for i := 0 to Length(RemainderRoutines)-1 do
         if (RemainderRoutines[i].a_size_in_bits = abits)
            and
            (RemainderRoutines[i].b_size_in_bits = bbits)
            and
            (RemainderRoutines[i].result_size = result_size) then
            begin
               add_to_list;
               exit
            end;
      i := Length(RemainderRoutines);
      SetLength (RemainderRoutines, i+1);
      RemainderRoutines[i] := TRemainderRoutine.Create (abits, bbits, result_size);
      add_to_list
   end;

procedure GenerateMathRoutines;
   var
      i,j: integer;
      lbl: TInstruction;
   begin
      if (Length(MultiplyRoutines) > 0) then
         begin
            for i := 0 to Length(MultiplyRoutines)-1 do
               begin
                  lbl := generate_multiply (MultiplyRoutines[i].result_size, MultiplyRoutines[i].a_info, MultiplyRoutines[i].b_info);
                  for j := 0 to Length (MultiplyRoutines[i].callers)-1 do
                      MultiplyRoutines[i].callers[j].dest := lbl;
                  MultiplyRoutines[i].Free
               end;
            SetLength (MultiplyRoutines, 0)
         end;

      if (Length(DivideRoutines) > 0) or (Length(RemainderRoutines) > 0) then
         begin
            TAssemblySourceBlankLine.Create;
            TAssemblyComment.Create ('------------------------------------------------------------------------------------------');
            TAssemblyComment.Create ('The following integer division/remainder subroutines were derived from assembly macros by:');
            TAssemblyComment.Create ('   Copyright © 2004 Ben J. Jackson <ben@ben.com>');
            TAssemblyComment.Create ('   May be freely used, modified and distributed, provided this notice remains intact.');
            TAssemblyComment.Create ('   This software is provided "as is" without express or implied warranty.');
            TAssemblyComment.Create ('------------------------------------------------------------------------------------------');
            TAssemblySourceBlankLine.Create
         end;

      if (Length(DivideRoutines) > 0) then
         begin
            for i := 0 to Length(DivideRoutines)-1 do
               begin
                  lbl := generate_div (DivideRoutines[i].a_size_in_bits, DivideRoutines[i].b_size_in_bits, DivideRoutines[i].result_size);
                  for j := 0 to Length (DivideRoutines[i].callers)-1 do
                      DivideRoutines[i].callers[j].dest := lbl;
                  DivideRoutines[i].Free
               end;
            SetLength (DivideRoutines, 0)
         end;

      if (Length(RemainderRoutines) > 0) then
         begin
            for i := 0 to Length(RemainderRoutines)-1 do
               begin
                  lbl := generate_remainder (RemainderRoutines[i].a_size_in_bits, RemainderRoutines[i].b_size_in_bits, RemainderRoutines[i].result_size);
                  for j := 0 to Length (RemainderRoutines[i].callers)-1 do
                      RemainderRoutines[i].callers[j].dest := lbl;
                  RemainderRoutines[i].Free
               end;
            SetLength (RemainderRoutines, 0)
         end
   end;

END.
