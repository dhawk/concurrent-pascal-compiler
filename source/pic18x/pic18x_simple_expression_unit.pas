UNIT pic18x_simple_expression_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

INTERFACE

uses
   cpc_simple_expression_unit,
   pic18x_cpu_unit;

type
   TPIC18x_SimpleExpression =
      class (TSimpleExpression, IGenerateCode)
         procedure GenerateCode (result_stk_size: integer);
      end;

IMPLEMENTATION

uses
   cpc_core_objects_unit,
   Math,
   pic18x_core_objects_unit,
   pic18x_expressions_unit,
   pic18x_floating_point_unit,
   pic18x_instructions_unit,
   pic18x_macro_instructions_unit,
   pic18x_microprocessor_information_unit,
   SysUtils;


procedure TPIC18x_SimpleExpression.GenerateCode (result_stk_size: integer);

   procedure generate_simple_numeric_expression_code;

      var
         result_addr, b_addr, i: integer;
         term_idx: integer;
         intermediate_integer_calculation_info:
            array of   // [0] is info for first_term+additional_term[0] calculation;
                       // [n] is for previous+additional_term[n] calculation; ...
               record   // note only fields for integer calculation portion of expression are filled in, real calculation parts are 0
                  a_size: integer;
                  b_size: integer;     // b is additional_term[n]
                  calculation_result_size: integer;
                  adjusted_result_size: integer
               end;

      procedure calculate_intermediate_integer_calculation_info;
         var
            term_idx, i: integer;
         begin
            SetLength (intermediate_integer_calculation_info, Length(additional_terms));

            for term_idx := 0 to Length(additional_terms)-1 do
               with additional_terms[term_idx], intermediate_integer_calculation_info[term_idx] do
                  case additional_terms[term_idx].addop of
                     addop_add_int_to_int,
                     addop_subtract_int_from_int:
                        begin
                           b_size := TPIC18x_TypeInfo(right_term.info).Size;
                           calculation_result_size := TPIC18x_TypeInfo(result_info).Size
                        end;
                     addop_add_flt_to_int,
                     addop_add_flt_to_flt,
                     addop_subtract_flt_from_flt,
                     addop_subtract_flt_from_int:
                        ;
                  else
                     assert (false)
                  end;

            // eliminate unneeded higher-order byte calculations
            //    this is occassionally useful when you have a situation like this:
            //       type t = 10_000..10_010;
            //       var v1,v2: t;
            //       calc v1-v2
            //    even though v1 and v2 require 2 bytes storage each,
            //    the difference will always fit in one signed byte, and
            //    there is no point in subtracting the higher order byte
            //    since the result will always be $00 or $ff.
            for term_idx := 0 to Length(additional_terms)-1 do
               with intermediate_integer_calculation_info[term_idx] do
                  for i := term_idx to Length(additional_terms)-1 do
                     case additional_terms[i].addop of
                        addop_add_int_to_int,
                        addop_subtract_int_from_int:
                           begin
                              b_size :=
                                 min (b_size,
                                      intermediate_integer_calculation_info[i].calculation_result_size
                                     );
                              calculation_result_size :=
                                 min (calculation_result_size,
                                      intermediate_integer_calculation_info[i].calculation_result_size
                                     )
                           end;
                        addop_add_flt_to_int,
                        addop_add_flt_to_flt,
                        addop_subtract_flt_from_flt,
                        addop_subtract_flt_from_int:
                           {nop};
                     else
                        assert (false)
                     end;
            if expression_kind = integer_expression then
               for term_idx := 0 to Length(additional_terms)-1 do
                  with intermediate_integer_calculation_info[term_idx] do
                     begin
                        b_size := min (b_size, result_stk_size);
                        calculation_result_size := min (calculation_result_size, result_stk_size)
                     end;

            // for all but last term...
            for term_idx := 0 to Length(additional_terms)-2 do
               with additional_terms[term_idx], intermediate_integer_calculation_info[term_idx] do
                  case additional_terms[term_idx+1].addop of
                     addop_add_int_to_int,
                     addop_subtract_int_from_int:
                        adjusted_result_size := intermediate_integer_calculation_info[term_idx+1].calculation_result_size;
                     addop_add_flt_to_int,
                     addop_subtract_flt_from_int:
                        adjusted_result_size := optimal_int_size_before_float (result_info);
                     addop_add_flt_to_flt,
                     addop_subtract_flt_from_flt:
                        {nop};
                  else
                     assert (false)
                  end;
            // for last term...
            term_idx := Length(additional_terms)-1;
            with additional_terms[term_idx], intermediate_integer_calculation_info[term_idx] do
               case addop of
                  addop_add_int_to_int,
                  addop_subtract_int_from_int:
                     adjusted_result_size := result_stk_size;
                  addop_add_flt_to_int,
                  addop_subtract_flt_from_int,
                  addop_add_flt_to_flt,
                  addop_subtract_flt_from_flt:
                     {nop};
               else
                  assert (false)
               end
         end;    // calculate_intermediate_integer_calculation_info

      function annotation (operator: char): string;
         begin
            with intermediate_integer_calculation_info[term_idx] do
               result :=
                  format ('tos*%d@[%d] := tos*%d@[%d] %s tos*%d@[%d]',
                          [calculation_result_size,
                           result_addr,
                           calculation_result_size,
                           result_addr,
                           operator,
                           b_size,
                           b_addr
                          ]
                         )
         end;

      procedure load_wreg_with_right_term_sign_extension;
         begin
            case TPIC18x_TypeInfo(additional_terms[term_idx].right_term.info).IntegerRange of
               irAlwaysNonNegative:
                  TPIC18x_MOVLW.Create (0);
               irNegativeOrPositive:
                  begin
                     TPIC18x_MOVLW.Create (0);
                     TPIC18x_BTFSC.Create (1, 7, access_mode);    // test tos sign bit
                     TPIC18x_MOVLW.Create ($FF)
                  end;
               irAlwaysNegative:
                  TPIC18x_MOVLW.Create ($FF);
            else
               assert (false)
            end
         end;

      begin    // generate_simple_numeric_expression_code
         calculate_intermediate_integer_calculation_info;

         // push first term
         case additional_terms[0].addop of
            addop_add_int_to_int,
            addop_subtract_int_from_int:
               (first_term as IGenerateCode).GenerateCode (intermediate_integer_calculation_info[0].calculation_result_size);
            addop_add_flt_to_int:
               begin
                  PushRealExpression (first_term);
                  additional_terms[0].addop := addop_add_flt_to_flt
               end;
            addop_subtract_flt_from_int:
               begin
                  PushRealExpression (first_term);
                  additional_terms[0].addop := addop_subtract_flt_from_flt
               end;
            addop_add_flt_to_flt,
            addop_subtract_flt_from_flt:
               PushRealExpression (first_term);
         else
            assert (false)
         end;
         // note: 0th addop will not be add_flt_to_int or subtract_flt_from_int due to above removal

         for term_idx := 0 to Length(additional_terms)-1 do
            with additional_terms[term_idx], intermediate_integer_calculation_info[term_idx] do
               begin
                  result_addr := 666;  // to suppress compiler warning
                  b_addr := 666;  // to suppress compiler warning
                  case addop of
                     addop_add_int_to_int,
                     addop_subtract_int_from_int:
                        begin
                           (right_term as IGenerateCode).GenerateCode (b_size);
                           result_addr := calculation_result_size + b_size;
                           b_addr := b_size
                        end;
                     addop_add_flt_to_int,
                     addop_subtract_flt_from_int:
                        begin
                           assert (term_idx > 0);  // due to above removal
                           convert_tos_integer_to_real (intermediate_integer_calculation_info[term_idx-1].adjusted_result_size, TPIC18x_TypeInfo(left_info), src_loc);
                           PushRealExpression (right_term)
                        end;
                     addop_add_flt_to_flt,
                     addop_subtract_flt_from_flt:
                        PushRealExpression (right_term);
                  else
                     assert (false)
                  end;

                  case addop of
                     addop_add_int_to_int:
                        begin
                           if (b_size = 1) and (calculation_result_size = 1) then
                              begin
                                 TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation ('+');
                                 StackUsageCounter.Pop (1);
                                 b_size := 0;
                                 b_addr := b_addr - 1;  // 0
                                 result_addr := result_addr - 1
                              end
                           else  // b_size > 1
                              TPIC18x_MOVF.Create (b_addr, dest_w, access_mode).annotation := annotation ('+');
                           TPIC18x_ADDWF.Create (result_addr, dest_f, access_mode);    // sets N status
                           i := 1;
                           while i < b_size do
                              begin
                                 TPIC18x_MOVF.Create (b_addr-i, dest_w, access_mode);
                                 TPIC18x_ADDWFC.Create (result_addr-i, dest_f, access_mode);    // sets N status
                                 i := i + 1
                              end;
                           if i < calculation_result_size then
                              begin
                                 load_wreg_with_right_term_sign_extension;
                                 while i < calculation_result_size do
                                    begin
                                       TPIC18x_ADDWFC.Create (result_addr-i, dest_f, access_mode);    // sets N status
                                       i := i + 1
                                    end
                              end;
                           generate_stack_fix_and_sign_extend_code (calculation_result_size,
                                                                    b_size,
                                                                    adjusted_result_size,
                                                                    TPIC18x_TypeInfo(result_info).IntegerRange
                                                                   )
                        end;
                     addop_subtract_int_from_int:
                        begin
                           if (b_size = 1) and (calculation_result_size = 1) then
                              begin
                                 TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation ('-');
                                 StackUsageCounter.Pop (1);
                                 b_size := 0;
                                 b_addr := b_addr - 1;  // 0
                                 result_addr := result_addr - 1
                              end
                           else  // b_size > 1
                              TPIC18x_MOVF.Create (b_addr, dest_w, access_mode).annotation := annotation ('-');
                           TPIC18x_SUBWF.Create (result_addr, dest_f, access_mode);    // sets N status
                           i := 1;
                           while i < b_size do
                              begin
                                 TPIC18x_MOVF.Create (b_addr-i, dest_w, access_mode);
                                 TPIC18x_SUBWFB.Create (result_addr-i, dest_f, access_mode);    // sets N status
                                 i := i + 1
                              end;
                           if i < calculation_result_size then
                              begin
                                 load_wreg_with_right_term_sign_extension;
                                 while i < calculation_result_size do
                                    begin
                                       TPIC18x_SUBWFB.Create (result_addr-i, dest_f, access_mode);    // sets N status
                                       i := i + 1
                                    end
                              end;
                           generate_stack_fix_and_sign_extend_code (calculation_result_size,
                                                                    b_size,
                                                                    adjusted_result_size,
                                                                    TPIC18x_TypeInfo(result_info).IntegerRange
                                                                   )
                        end;
                     addop_add_flt_to_int,
                     addop_add_flt_to_flt:
                        FPA32.Call (addop_src_loc);
                     addop_subtract_flt_from_int,
                     addop_subtract_flt_from_flt:
                        FPS32.Call (addop_src_loc);
                  else
                     assert (false)
                  end
               end
      end;    // generate_simple_numeric_expression_code

   procedure generate_simple_boolean_expression_code;
      var
         bras: array of TPIC18x_BRA;
         dummy: boolean;

      procedure add_bra;
         var i: integer;
         begin
            i := Length(bras);
            SetLength (bras, i+1);
            bras[i] := TPIC18x_BRA.Create
         end;

      procedure generate_term_code (term: TExpression; skip_sense: boolean);
         begin
            if expression_can_be_evaluated_with_simple_bit_test (term, dummy) <> nil then
               GenerateCodeForConditionalSkip (term, skip_sense)
            else
               begin
                  (term as IGenerateCode).GenerateCode (1);
                  if skip_sense then
                     TPIC18x_BTFSS.Create (PREINC2, 0, access_mode)
                  else
                     TPIC18x_BTFSC.Create (PREINC2, 0, access_mode);
                  StackUsageCounter.Pop (1)
               end
         end;

      var
         idx: integer;
         lbl: TInstruction;
      begin   // generate_simple_boolean_expression_code
         TPIC18x_PUSHL.Create (1);
         StackUsageCounter.Push(1);
         generate_term_code (first_term, false);
         add_bra;
         for idx := 0 to Length(additional_terms)-2 do
            begin
               generate_term_code (additional_terms[idx].right_term, false);
               add_bra
          end;
         generate_term_code (additional_terms[Length(additional_terms)-1].right_term, true);
         TPIC18x_CLRF.Create (1, access_mode);
         lbl := TAssemblyLabel.Create;
         for idx := 0 to Length(bras)-1 do
            bras[idx].dest := lbl
      end;    // generate_simple_boolean_expression_code

   procedure generate_simple_set_expression_code;
      var
         idx,i: integer;
         annotation: string;
      begin   // generate_simple_set_expression_code
         (first_term as IGenerateCode).GenerateCode (result_stk_size);
         for idx := 0 to Length(additional_terms)-1 do
            begin
               (additional_terms[idx].right_term as IGenerateCode).GenerateCode (result_stk_size);
               case additional_terms[idx].addop of
                  addop_set_union:
                     begin
                        annotation := 'set union(+) of tos*' + IntToStr(result_stk_size) + ' with (tos-1)*' + IntToStr(result_stk_size) + ', pop tos*' + IntToStr(result_stk_size);
                        for i := 1 to result_stk_size do
                           begin
                              TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation;
                              annotation := '';
                              TPIC18x_IORWF.Create (result_stk_size, dest_f, access_mode)
                           end;
                        StackUsageCounter.Pop (result_stk_size)
                     end;
                  addop_set_difference:
                     begin
                        annotation := 'set difference(-) of tos*' + IntToStr(result_stk_size) + ' with (tos-1)*' + IntToStr(result_stk_size) + ', pop tos*' + IntToStr(result_stk_size);
                        for i := 1 to result_stk_size do
                           begin
                              TPIC18x_MOVF.Create (PREINC2, dest_w, access_mode).annotation := annotation;
                              annotation := '';
                              TPIC18x_COMF.Create (WREG, dest_w, access_mode);
                              TPIC18x_ANDWF.Create (result_stk_size, dest_f, access_mode)
                           end;
                        StackUsageCounter.Pop (result_stk_size)
                     end;
               else
                  assert (false)
               end
            end
      end;    // generate_simple_set_expression_code

   begin
      case expression_kind of
         integer_expression,
         real_expression:
            generate_simple_numeric_expression_code;
         boolean_expression:
            generate_simple_boolean_expression_code;
         set_expression:
            generate_simple_set_expression_code;
      else
         assert (false)
      end
   end;

END.
