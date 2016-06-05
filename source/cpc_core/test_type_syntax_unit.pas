unit test_type_syntax_unit;

interface

uses
  cpc_core_objects_unit, cpc_definitions_unit;

type
   Told_TROMConstant =
      class (TVariable)
         constructor CreateFromSourceTokens;
      end;

procedure test_type_syntax;
function create_TROMConstant: TDefinition;

implementation

uses test_main_form_unit,
  cpc_common_unit, cpc_source_analysis_unit, SysUtils, cpc_types_unit;

constructor Told_TROMConstant.CreateFromSourceTokens;
   begin
      if not lex.token_is_identifier then
         raise compile_error.Create(err_identifier_expected);

      src_loc := lex.token.src_loc;
      CreateForLaterDefinition(lex.token.identifier_idx, nil);
      lex.advance_token;

      if not lex.token_is_symbol(sym_colon) then
         raise compile_error.Create(err_colon_expected);
      lex.advance_token;

      typedef_src_loc := lex.token.src_loc;
      typedef := CreateTypeDenoterFromSourceTokens;
      if (typedef.type_kind = string_type)
         and
         (TStringType(typedef).max_length = -1) then
         raise compile_error.Create(err_left_bracket_expected);
      descriptor := rw_rom;
      address_mode := absolute_address_mode;

      if typedef.ContainsQueueVariables then
         raise compile_error.Create(err_queue_type_not_allowed_here, typedef_src_loc);
      if typedef.ContainsSystemType then
         raise compile_error.Create (err_illegal_type_for_rom_constant, typedef_src_loc);

      if not lex.token_is_symbol(sym_equals) then
         raise compile_error.Create(err_equals_expected);
      lex.advance_token;

      initial_value := TStructuredConstant.CreateFromSourceTokens(typedef, typedef_src_loc);

      if not lex.token_is_symbol(sym_semicolon) then
         compile_error.Create(err_semicolon_expected);
      lex.advance_token;
   end;

function create_TROMConstant: TDefinition;
   begin
      result := Told_TROMConstant.CreateFromSourceTokens
   end;

procedure test_integer_type;
   procedure test1;
      const
         s = 'begin a:int8=-128; end';
      begin
         test_fragment(s, create_TROMConstant).Release
      end;
   begin
      display('testing TIntegerDataType');
      test1;
      test_compile_error_generation_for_program_fragment('type t=array [3..5] of int8; begin a:t = ([3]=1.3,[4]=22,[5]=77); end', create_TROMConstant, err_integer_expected, '1.3,[4]=22,[5]=77); end');
      test_compile_error_generation_for_program_fragment('begin a:int8=256; end', create_TROMConstant, err_value_outside_legal_range, '256; end');
      test_compile_error_generation_for_program_fragment('begin a:int8=-129; end', create_TROMConstant, err_value_outside_legal_range, '-129; end');

      // test expression assignment compat
      test_only_for_successful_compilation('procedure x; var i: int8; begin i := -128 end; begin end.');
      test_only_for_successful_compilation('type t1 = class procedure entry x; begin end; begin end; var c: t1; begin c.x end.');
      test_only_for_successful_compilation('procedure x; var i: int8; begin i := 127 end; begin end.');
      test_compile_error_generation('procedure x; var i: int8; begin i := true end; begin end.', err_integer_expression_expected, 'true end; begin end.');
      test_compile_error_generation('procedure x; var i: int8; begin i := -129 end; begin end.', err_expression_value_outside_legal_range, '-129 end; begin end.');
      test_compile_error_generation('procedure x; var i: int8; begin i := 128 end; begin end.', err_expression_value_outside_legal_range, '128 end; begin end.')
   end;

procedure test_TCharDataType;
   procedure test1;
      const
         s = 'begin a:char=''a''; end';
      begin
         test_fragment(s, create_TROMConstant).Release
      end;
   begin
      display('test_TCharDataType');
      test1;
      test_compile_error_generation_for_program_fragment('begin a:char=10; end', create_TROMConstant, err_char_expected, '10; end');
      test_compile_error_generation_for_program_fragment('begin a:char=''ab''; end', create_TROMConstant, err_char_expected, '''ab''; end');

      // test expression assignment compat
      test_only_for_successful_compilation('procedure x; var i: char; begin i := ''a'' end; begin end.');
      test_compile_error_generation('procedure x; var i: char; begin i := 123 end; begin end.', err_char_expression_expected, '123 end; begin end.');
   end;

procedure test_TBooleanDataType;
   procedure test1;
      const
         s = 'begin a:boolean=true; end';
      begin
         test_fragment(s, create_TROMConstant).Release
      end;
   begin
      display('test_TBooleanDataType');
      test1;
      test_compile_error_generation_for_program_fragment('begin a:boolean=10; end',
      create_TROMConstant, err_boolean_expected, '10; end');
      // test expression assignment compat
      test_only_for_successful_compilation('procedure x; var b: boolean; begin b := true end; begin end.');
      test_only_for_successful_compilation('procedure x; var b: boolean; begin b := false end; begin end.');
      test_compile_error_generation('procedure x; var b: boolean; begin b := 1 end; begin end.', err_boolean_expression_expected, '1 end; begin end.');
   end;

function create_TSubRangeType: TDefinition;
   begin
      result := TSubRangeType.CreateFromSourceTokens
   end;

procedure test_subrange_type;
   procedure test_int_subrange;
      const
         s = 'begin 3..5 end';
      var
         sr: TSubRangeType;
      begin
         sr := TSubRangeType (test_fragment (s, create_TSubRangeType));
         if (sr = nil) or (not sr.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sr.ordinal_kind <> ordinal_base_is_integer then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_integer expected')
                  end;
               if sr.info.min_value.ne (3) then
                  begin
                     record_bad_test_result;
                     display(s + ': first=3 expected')
                  end;
               if sr.info.max_value.ne(5) then
                  begin
                     record_bad_test_result;
                     display(s + ': last=5 expected')
                  end
            end;
         sr.Release
      end;
   procedure test_char_subrange;
      const
         s = 'begin ''a''..''d'' end';
      var
         sr: TSubRangeType;
      begin
         sr := TSubRangeType (test_fragment (s, create_TSubRangeType));
         if (sr = nil) or (not sr.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sr.info.min_value.ne(ord('a')) then
                  begin
                     record_bad_test_result;
                     display(s + ': first=ord(''a'') expected')
                  end;
               if sr.info.max_value.ne(ord('d')) then
                  begin
                     record_bad_test_result;
                     display(s + ': last=ord(''d'') expected')
                  end;
               if sr.ordinal_kind <> ordinal_base_is_char then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_char expected')
                  end
            end;
         sr.Release
      end;
   procedure test_enum_subrange;
      const
         s = 'type x=(a,b,c); begin b..c end';
      var
         sr: TSubRangeType;
      begin
         sr := TSubRangeType (test_fragment (s, create_TSubRangeType));
         if (sr = nil) or (not sr.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sr.info.min_value.ne(1) then
                  begin
                     record_bad_test_result;
                     display(s + ': first=1 expected')
                  end;
               if sr.info.max_value.ne(2) then
                  begin
                     record_bad_test_result;
                     display(s + ': last=2 expected')
                  end;
               if sr.ordinal_kind <> ordinal_base_is_enum then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_enum expected')
                  end
            end;
         sr.Release
      end;
   procedure test_bool_subrange;
      const
         s = 'begin false..true end';
      var
         sr: TSubRangeType;
      begin
         sr := TSubRangeType (test_fragment (s, create_TSubRangeType));
         if (sr = nil) or (not sr.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sr.info.min_value.ne(0) then
                  begin
                     record_bad_test_result;
                     display(s + ': first=0 expected')
                  end;
               if sr.info.max_value.ne(1) then
                  begin
                     record_bad_test_result;
                     display(s + ': last=1 expected')
                  end;
               if sr.ordinal_kind <> ordinal_base_is_bool then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_bool expected')
                  end
            end;
         sr.Release
      end;
   begin
      display('testing TSubRangeType');
      test_int_subrange;
      test_char_subrange;
      test_enum_subrange;
      test_bool_subrange;

      test_compile_error_generation_for_program_fragment('begin 1..''b'' end', create_TSubRangeType, err_subrange_first_and_last_must_be_of_same_type, '..''b'' end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f); begin a..f end', create_TSubRangeType, err_subrange_first_and_last_must_be_of_same_type, '..f end');
      test_compile_error_generation_for_program_fragment('begin ''ab''..''f'' end', create_TSubRangeType, err_subrange_must_be_single_char, '''ab''..''f'' end');
      test_compile_error_generation_for_program_fragment('begin ''a''..''f2'' end', create_TSubRangeType, err_subrange_must_be_single_char, '''f2'' end');
      test_compile_error_generation_for_program_fragment('begin 1.7..2.3 end', create_TSubRangeType, err_subrange_types_must_be_ordinal, '1.7..2.3 end');
      test_compile_error_generation_for_program_fragment('begin 5..2 end', create_TSubRangeType, err_first_subrange_value_greater_than_last_subrange_value, '..2 end');

      // test CheckAssignmentCompatability
      test_only_for_successful_compilation('type x=(a,b,c);xs=b..c;begin aa:xs=b; end', create_TROMConstant);
      test_only_for_successful_compilation('type x=''a''..''b'';begin aa:x=''b''; end', create_TROMConstant);
      test_only_for_successful_compilation('type x=true..true;begin aa:x=true; end', create_TROMConstant);
      test_only_for_successful_compilation('type x=3..5;begin aa:x=4; end', create_TROMConstant);
      test_compile_error_generation_for_program_fragment('type x=1..5;begin a:x=true; end', create_TROMConstant, err_integer_expected, 'true; end');
      test_compile_error_generation_for_program_fragment('type x=1..5;begin a:x=0; end', create_TROMConstant, err_constant_value_outside_subrange, '0; end');
      test_compile_error_generation_for_program_fragment('type x=1..5;begin a:x=6; end', create_TROMConstant, err_constant_value_outside_subrange, '6; end');

      // test expression assignment compat
      test_only_for_successful_compilation('procedure x; var i: 3..5 := 4; begin i := 3 end; begin end.');
      test_only_for_successful_compilation('procedure x; var i: 3..5 := 4; begin i := 5 end; begin end.');
      test_compile_error_generation('procedure x; var i: 3..5 := 4; begin i := true end; begin end.', err_integer_expression_expected, 'true end; begin end.');
      test_compile_error_generation('procedure x; var i: 3..5 := 4; begin i := 2 end; begin end.', err_expression_value_outside_legal_range, '2 end; begin end.');
      test_compile_error_generation('procedure x; var i: 3..5 := 4; begin i := 6 end; begin end.', err_expression_value_outside_legal_range, '6 end; begin end.');

      test_only_for_successful_compilation('procedure x; var i: true..true := true; begin i := true end; begin end.');
      test_only_for_successful_compilation('procedure x; var i: false..false; begin i := false end; begin end.');
      test_compile_error_generation('procedure x; var i: true..true := true; begin i := 5 end; begin end.', err_boolean_expression_expected, '5 end; begin end.');
      test_compile_error_generation('procedure x; var i: true..true := true; begin i := false end; begin end.', err_expression_value_outside_legal_range, 'false end; begin end.');
      test_compile_error_generation('procedure x; var i: false..false; begin i := true end; begin end.', err_expression_value_outside_legal_range, 'true end; begin end.');

      test_only_for_successful_compilation('procedure x; var c: ''3''..''5'' := ''4''; begin c := ''3'' end; begin end.');
      test_only_for_successful_compilation('procedure x; var c: ''3''..''5'' := ''4''; begin c := ''5'' end; begin end.');
      test_compile_error_generation('procedure x; var c: ''3''..''5'' := ''4''; begin c := true end; begin end.', err_char_expression_expected, 'true end; begin end.');
      test_compile_error_generation('procedure x; var c: ''3''..''5'' := ''4''; begin c := ''2'' end; begin end.', err_expression_value_outside_legal_range, '''2'' end; begin end.');
      test_compile_error_generation('procedure x; var c: ''3''..''5'' := ''4''; begin c := ''6'' end; begin end.', err_expression_value_outside_legal_range, '''6'' end; begin end.');

      test_only_for_successful_compilation('type e=(e1,e2,e3,e4,e5); procedure x; var i: e3..e4 := e3; begin i := e3 end; begin end.');
      test_only_for_successful_compilation('type e=(e1,e2,e3,e4,e5); procedure x; var i: e3..e4 := e3; begin i := e4 end; begin end.');
      test_compile_error_generation('type e=(e1,e2,e3,e4,e5); procedure x; var i: e3..e4 := e3; begin i := true end; begin end.', err_enum_expression_expected, 'true end; begin end.');
      test_compile_error_generation('type e=(e1,e2,e3,e4,e5); procedure x; var i: e3..e4 := e3; begin i := e2 end; begin end.', err_expression_value_outside_legal_range, 'e2 end; begin end.');
      test_compile_error_generation('type e=(e1,e2,e3,e4,e5); procedure x; var i: e3..e4 := e3; begin i := e5 end; begin end.', err_expression_value_outside_legal_range, 'e5 end; begin end.');
   end;

function create_TEnumType: TDefinition;
   begin
      result := TEnumType.CreateFromSourceTokens
   end;

procedure test_TEnumType;
   procedure test1;
      const
         s = 'begin (a,b,c) end';
      var
         et: TEnumType;
         i: integer;
      begin
         et := TEnumType (test_fragment (s, create_TEnumType));
         if (et = nil) or (not et.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if et.basic_data_type_kind <> ordinal_data_type then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal type expected')
                  end;
               if et.ordinal_kind <> ordinal_base_is_enum then
                  begin
                     record_bad_test_result;
                     display(s + ': enum_type expected')
                  end;
               if Length(et.enums) <> 3 then
                  begin
                     record_bad_test_result;
                     display(s + ': 3 enums expected')
                  end;
               for i := 0 to Length(et.enums) - 1 do
                  if et.enums[i].value <> i then
                     begin
                        record_bad_test_result;
                        display(s + ': enum[' + IntToStr(i) + '] ord incorrect')
                     end
            end;
         et.Release
      end;
   procedure test2;
      const
         s = 'type x=(a,b,c);begin aa:x=b; end';
      begin
         test_fragment(s, create_TROMConstant).Release
      end;
   begin
      display('testing TDefEnumType');
      test1;
      test2;
      test_compile_error_generation_for_program_fragment('begin (a,1) end', create_TEnumType, err_identifier_expected, '1) end');
      test_compile_error_generation_for_program_fragment('begin (a,b end', create_TEnumType, err_comma_expected, 'end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);begin a:x=5; end', create_TROMConstant, err_enum_value_expected, '5; end');
      test_compile_error_generation_for_program_fragment('type x=(a,b,c);y=(d,e,f);begin a:x=f; end', create_TROMConstant, err_wrong_enum_type, 'f; end');

      // test expression assignment compat
      test_only_for_successful_compilation('procedure x; var e1: (a,b,c); begin e1 := a end; begin end.');
      test_compile_error_generation('procedure x; var e1: (a,b,c); begin e1 := 5 end; begin end.', err_enum_expression_expected, '5 end; begin end.');
      test_compile_error_generation('procedure x; var e1: (a,b,c); e2: (d,e,f); begin e1 := d end; begin end.', err_wrong_enum_type, 'd end; begin end.');
   end;

function create_TRecordType: TDefinition;
   begin
      result := TRecordType.CreateFromSourceTokens
   end;

procedure test_TRecordType;
   procedure test1;
      const
         s = 'begin record end end';
      var
         r: TRecordType;
      begin
         r := TRecordType (test_fragment (s, create_TRecordType));
         if (r = nil) or (not r.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if r.type_kind <> record_type then
                  begin
                     record_bad_test_result;
                     display(s + ': record kind expected')
                  end;
               if Length(r.fields) <> 0 then
                  begin
                     record_bad_test_result;
                     display(s + ': no fields expected')
                  end
            end;
         r.Release
      end;
   procedure test2;
      const
         s = 'begin record b: boolean end end';
      var
         r: TRecordType;
      begin
         r := TRecordType (test_fragment (s, create_TRecordType));
         if (r = nil) or (not r.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if r.type_kind <> record_type then
                  begin
                     record_bad_test_result;
                     display(s + ': record kind expected')
                  end;
               if Length(r.fields) <> 1 then
                  begin
                     record_bad_test_result;
                     display(s + ': one field expected')
                  end
            end;
         r.Release
      end;
   procedure test3;
      const
         s = 'begin record b: boolean; i: int16 end end';
      var
         r: TRecordType;
      begin
         r := TRecordType (test_fragment (s, create_TRecordType));
         if (r = nil) or (not r.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if r.type_kind <> record_type then
                  begin
                     record_bad_test_result;
                     display(s + ': record kind expected')
                  end;
               if Length(r.fields) <> 2 then
                  begin
                     record_bad_test_result;
                     display(s + ': two fields expected')
                  end
            end;
         r.Release
      end;
   procedure test4;
      const
         s = 'begin record b1,b2,b3: boolean; i: int16 end end';
      var
         r: TRecordType;
      begin
         r := TRecordType (test_fragment (s, create_TRecordType));
         if (r = nil) or (not r.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if r.type_kind <> record_type then
                  begin
                     record_bad_test_result;
                     display(s + ': record kind expected')
                  end;
               if Length(r.fields) <> 4 then
                  begin
                     record_bad_test_result;
                     display(s + ': four fields expected')
                  end;
               if (r.fields[0].typedef <> r.fields[1].typedef) or (r.fields[1].typedef <>
               r.fields[2].typedef) then
                  begin
                     record_bad_test_result;
                     display(s + ': three bool fields have different type')
                  end;
               if r.fields[2].typedef = r.fields[3].typedef then
                  begin
                     record_bad_test_result;
                     display(s + ': bool and int16 fields have same type')
                  end
            end;
         r.Release
      end;
   begin
      display('testing TRecordType');
      test1;
      test2;
      test3;
      test4;
      test_compile_error_generation_for_program_fragment('type tint = -128..127;begin record 5: tint end end', create_TRecordType, err_identifier_expected, '5: tint end end');
      test_compile_error_generation_for_program_fragment('type tint = -128..127;begin record i; tint end end', create_TRecordType, err_colon_expected, '; tint end end');
      test_compile_error_generation_for_program_fragment('type tint = -128..127;begin record i: tint b: boolean end end', create_TRecordType, err_semicolon_or_end_expected, 'b: boolean end end');
      test_compile_error_generation('type r=record a,a: int8 end; begin end.', err_duplicate_field_name, 'a: int8 end; begin end.');
      test_only_for_successful_compilation('type tr=record i: int8 end; var r: tr; begin r := r end.');
      test_only_for_successful_compilation('type tr=record i: int8 end; var o: overlay tr end; r: tr; begin r := o end.');
   end;

procedure test_TOverlayType;
   begin
      display('testing TOverlayType');

      test_only_for_successful_compilation('type tov=overlay a: int8; b: boolean; end; begin end.');
      test_compile_error_generation('type tov=overlay a: int8; q: queue; end; begin end.', err_overlay_may_not_contain_queue_variables, 'queue; end; begin end.');
      test_compile_error_generation('type tov=overlay a: int8; c: class begin end end; begin end.', err_overlay_may_not_contain_class_variables, 'class begin end end; begin end.');
      test_compile_error_generation('type tov=overlay a: int8; m: monitor begin end begin end.', err_overlay_may_not_contain_monitor_variables, 'monitor begin end begin end.');
      test_compile_error_generation('type tov=overlay a: int8; p: process priority 0; begin loop repeat end end; begin end.', err_overlay_may_not_contain_process_variables, 'process priority 0; begin loop repeat end end; begin end.');
      test_compile_error_generation('type t=overlay packed record X: uint8; end; packed record X: uint8; end end;begin end.', format (err_duplicate_overlay_name, ['X']), 'packed record X: uint8; end end;begin end.');
      test_compile_error_generation('ioreg x: overlay i: int8 end;', err_ioreg_overlaid_variables_must_all_be_packed_records, 'overlay i: int8 end;');
      test_compile_error_generation('ioreg t: overlay packed record x: uint8 end; packed record y: uint1 end; end at 1000;begin end.', err_invalid_total_ioregister_width, 'overlay');
      test_compile_error_generation('var o: overlay i,j: int8 end; begin end.', err_comma_not_allowed, ',j: int8 end; begin end.');

      // test anonymous array variables
      test_only_for_successful_compilation('type tov=overlay record a,b: int8 end; r: real end; begin end.');
      test_only_for_successful_compilation('type te1=(a,b,c); te2=(d,e,f); tov=overlay array[d..f] of int8; array [a..c] of real end; begin end.');
      test_only_for_successful_compilation('type tov=overlay array [1..2, 2..3] of int8; array [false..true] of boolean end; begin end.');
      test_only_for_successful_compilation('type tov=overlay array [''a''..''c''] of int8; array [10..12] of array [5..6, 2..3] of boolean end; begin end.');
      test_compile_error_generation('type tov=overlay array[1..3] of int8; array [2..5] of real end; begin end.', err_ambiguous_first_index_base_type_for_anonymous_array, 'array [2..5] of real end; begin end.');
      test_compile_error_generation('type te1=(a,b,c,d); tov=overlay array[a..b] of int8; array [c..d] of real end; begin end.', err_ambiguous_first_index_base_type_for_anonymous_array, 'array [c..d] of real end; begin end.');
      test_compile_error_generation('type tov=overlay array [1..2, 2..3] of int8; array [10..12] of array [5..6] of boolean end; begin end.', err_ambiguous_first_index_base_type_for_anonymous_array, 'array [10..12] of array [5..6] of boolean end; begin end.');

      // anonymous string
      test_only_for_successful_compilation('type tov=overlay string[10]; r: array [''a''..''b''] of real end; begin end.');
      test_only_for_successful_compilation('type tov=overlay array [''a''..''b''] of real; string[10] end; begin end.');
      test_compile_error_generation('type tov=overlay string[10]; array [1..3] of real end; begin end.', err_ambiguous_first_index_base_type_for_anonymous_array, 'array [1..3] of real end; begin end.');
      test_compile_error_generation('type tov=overlay array [1..3] of real; string[10] end; begin end.', err_ambiguous_first_index_base_type_for_anonymous_array, 'string[10] end; begin end.');

      test_only_for_successful_compilation('type tr=record x,y: real end; tov=overlay i: int8; tr end; var ov: tov; r: tr; begin ov := r end.');
   end;

function create_TSetType: TDefinition;
   begin
      result := TSetType.CreateFromSourceTokens
   end;

procedure test_TSetType;
   procedure test1;
      const
         s = 'begin set of 5..10 end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> 5 then
                  begin
                     record_bad_test_result;
                     display(s + ': first value 5 expected')
                  end;
               if sett.info.max_value.AsInteger <> 10 then
                  begin
                     record_bad_test_result;
                     display(s + ': last value 10 expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_integer then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_integer expected')
                  end
            end;
         sett.Release
      end;
   procedure test2;
      const
         s = 'begin set of ''a''..''z'' end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> ord('a') then
                  begin
                     record_bad_test_result;
                     display(s + ': first value ''a'' expected')
                  end;
               if sett.info.max_value.AsInteger <> ord('z') then
                  begin
                     record_bad_test_result;
                     display(s + ': last value ''z'' expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_char then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_char expected')
                  end
            end;
         sett.Release
      end;
   procedure test3;
      const
         s = 'begin set of char end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_char then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_char expected')
                  end
            end;
         sett.Release
      end;
   procedure test4;
      const
         s = 'begin set of false..true end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> 0 then
                  begin
                     record_bad_test_result;
                     display(s + ': first value 0 expected')
                  end;
               if sett.info.max_value.AsInteger <> 1 then
                  begin
                     record_bad_test_result;
                     display(s + ': last value 1 expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_bool then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_bool expected')
                  end
            end;
         sett.Release
      end;
   procedure test5;
      const
         s = 'begin set of boolean end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> 0 then
                  begin
                     record_bad_test_result;
                     display(s + ': first value 0 expected')
                  end;
               if sett.info.max_value.AsInteger <> 1 then
                  begin
                     record_bad_test_result;
                     display(s + ': last value 1 expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_bool then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_bool expected')
                  end
            end;
         sett.Release
      end;
   procedure test6;
      const
         s = 'type x=(a,b,c); begin set of x end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> 0 then
                  begin
                     record_bad_test_result;
                     display(s + ': first value 0 expected')
                  end;
               if sett.info.max_value.AsInteger <> 2 then
                  begin
                     record_bad_test_result;
                     display(s + ': last value 2 expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_enum then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_bool expected')
                  end
            end;
         sett.Release
      end;
   procedure test7;
      const
         s = 'type x=(a,b,c); begin set of b..c end';
      var
         sett: TSetType;
      begin
         sett := TSetType (test_fragment (s, create_TSetType));
         if (sett = nil) or (not sett.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if sett.type_kind <> set_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if sett.info.min_value.AsInteger <> 1 then
                  begin
                     record_bad_test_result;
                     display(s + ': first value 1 expected')
                  end;
               if sett.info.max_value.AsInteger <> 2 then
                  begin
                     record_bad_test_result;
                     display(s + ': last value 2 expected')
                  end;
               if sett.ordinal_kind <> ordinal_base_is_enum then
                  begin
                     record_bad_test_result;
                     display(s + ': ordinal_base_is_bool expected')
                  end
            end;
         sett.Release
      end;
   begin
      display('testing TSetType');
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test_compile_error_generation_for_program_fragment('begin set 1..3 end', create_TSetType, err_of_expected, '1..3 end');
      test_compile_error_generation_for_program_fragment('begin set of real end', create_TSetType, err_ordinal_type_expected, 'real end');
      test_compile_error_generation_for_program_fragment('begin set of -1..255 end', create_TSetType, err_invalid_low_set_range_value, '-1..255 end');
      test_compile_error_generation_for_program_fragment('begin set of 0..256 end', create_TSetType, err_invalid_high_set_range_value, '0..256 end');

      // test CheckAssignmentCompatability
      test_only_for_successful_compilation('type x=set of 3..5;begin aa:x=[3..5]; end', create_TROMConstant);
      test_compile_error_generation_for_program_fragment('type x=set of 3..5;begin aa:x=5; end', create_TROMConstant, err_set_expected, '5; end');
      test_compile_error_generation_for_program_fragment('type x=set of 3..5;begin aa:x=[false..true]; end', create_TROMConstant, err_set_constant_is_of_wrong_type, '[false..true]; end');
      test_compile_error_generation_for_program_fragment('type e=(a,b,c);x=set of(d,e2,f);begin aa:x=[b]; end', create_TROMConstant, err_set_constant_is_of_wrong_type, '[b]; end');
      test_compile_error_generation_for_program_fragment('type x=set of 3..5;begin aa:x=[1..5]; end', create_TROMConstant, err_set_constant_has_members_outside_specified_range, '[1..5]; end');
   end;

function create_TArrayType: TDefinition;
   begin
      result := TArrayType.CreateFromSourceTokens
   end;

procedure test_TArrayType;
   procedure test1;
      const
         s = 'begin array [1..3] of boolean end';
      var
         a: TArrayType;
      begin
         a := TArrayType (test_fragment (s, create_TArrayType));
         if (a = nil) or (not a.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if a.type_kind <> array_type then
                  begin
                     record_bad_test_result;
                     display(s + ': array kind expected')
                  end;
               if not a.index_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': index is not a type')
                  end;
               if a.index_typedef.info.min_value.ne(1) or a.index_typedef.info.max_value.ne(3)
               then
                  begin
                     record_bad_test_result;
                     display(s + ': index bounds not 1..3')
                  end
            end;
         a.Release
      end;
   procedure test2;
      const
         s = 'begin array [1..3, 4..5] of boolean end';
      var
         a, suba: TArrayType;
      begin
         a := TArrayType (test_fragment (s, create_TArrayType));
         if (a = nil) or (not a.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if a.type_kind <> array_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if not a.index_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': index is not a type')
                  end;
               if a.index_typedef.info.min_value.ne(1) or a.index_typedef.info.max_value.ne(3)
               then
                  begin
                     record_bad_test_result;
                     display(s + ': index bounds not 1..3')
                  end;
               if not a.element_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': element is not a type')
                  end;
               if a.element_typedef.type_kind <> array_type then
                  begin
                     record_bad_test_result;
                     display(s + ': elements are not array')
                  end;
               suba := TArrayType(a.element_typedef);
               if not suba.element_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': second element is not a type')
                  end;
            end;
         a.Release
      end;
   procedure test3;
      const
         s = 'begin array [1..3] of array [4..5] of boolean end';
      var
         a: TArrayType;
      begin
         a := TArrayType (test_fragment (s, create_TArrayType));
         if (a = nil) or (not a.IsTypeDefinition) then
            begin
               record_bad_test_result;
               display(s + ': def type expected')
            end
         else
            begin
               if a.type_kind <> array_type then
                  begin
                     record_bad_test_result;
                     display(s + ': set kind expected')
                  end;
               if not a.index_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': index is not a type')
                  end;
               if a.index_typedef.info.min_value.ne(1) or a.index_typedef.info.max_value.ne(3)
               then
                  begin
                     record_bad_test_result;
                     display(s + ': index bounds not 1..3')
                  end;
               if not a.element_typedef.IsTypeDefinition then
                  begin
                     record_bad_test_result;
                     display(s + ': element is not a type')
                  end;
               if a.element_typedef.type_kind <> array_type then
                  begin
                     record_bad_test_result;
                     display(s + ': elements are not array')
                  end
            end;
         a.Release
      end;
   begin
      display('testing TArrayType');
      test1;
      test2;
      test3;
      test_compile_error_generation_for_program_fragment('type tint=-128..127;begin array 1..3] of tint end', create_TArrayType, err_left_bracket_expected, '1..3] of tint end');
      test_compile_error_generation_for_program_fragment('type tint=-128..127;begin array [1..3 of tint end', create_TArrayType, err_right_bracket_expected, 'of tint end');
      test_compile_error_generation_for_program_fragment('type tint=-128..127;begin array [1..3] tint end', create_TArrayType, err_of_expected, 'tint end');

      test_only_for_successful_compilation('type te=(a,b,c); var arr: array [te] of int8; begin end.');
      test_only_for_successful_compilation('type tx=array [1..2] of int8; var x: tx; begin x := x end.');
      test_only_for_successful_compilation('type tx=array [1..2] of int8; var o: overlay tx end; x: tx; begin x := o end.');
   end;

procedure test_TStringType;
   begin
      display('testing TStringType');
      test_only_for_successful_compilation('procedure p; var s: string[10]; begin end; begin end.');
      test_only_for_successful_compilation('const c: char = ''x''; var s: string [5]; begin s := c end.');
      test_compile_error_generation('procedure p; var s: string(10]; begin end; begin end.', err_left_bracket_expected, '(10]; begin end; begin end.');
      test_compile_error_generation('procedure p; var s: string[10.2]; begin end; begin end.', err_integer_expression_expected, '10.2]; begin end; begin end.');
      test_compile_error_generation('procedure p; var s: string[10); begin end; begin end.', err_right_bracket_expected, '); begin end; begin end.');
      test_compile_error_generation('type str0 = string[-1]; begin end.', err_string_length_must_be_non_negative, '-1]; begin end.');
      test_compile_error_generation('type str0 = string[256]; begin end.', err_string_length_must_be_less_than_256, '256]; begin end.');
      test_only_for_successful_compilation('type tx=string[10]; var x: tx; begin x := x end.');
      test_only_for_successful_compilation('type tx=string[10]; var o: overlay tx end; x: tx; begin x := o end.');
   end;

procedure test_TFloatingPointDataType;
   begin
      display('testing TFloatingPointDataType');

      // test constant assignement compatability
      test_only_for_successful_compilation('begin aa:real=1.2; end', create_TROMConstant);
      test_only_for_successful_compilation('begin aa:real=1; end', create_TROMConstant);
      test_compile_error_generation_for_program_fragment('begin a:real=true; end', create_TROMConstant, err_number_expected, 'true; end');

      // test expression assignment compatability
      test_only_for_successful_compilation('procedure x; var r: real; begin r := 5.3 end; begin end.');
      test_only_for_successful_compilation('procedure x; var r: real; begin r := 5 end; begin end.');
      test_compile_error_generation('procedure x; var r: real; begin r := true end; begin end.', err_numeric_expression_expected, 'true end; begin end.')
   end;

procedure test_TPackedRecordType;
   var
      overlay:
         record
            case integer of
               0:
                  (r: real
                  );
               1:
                  (i: int64
                  )
         end;
   begin
      display('testing TPackedRecordType');
      test_only_for_successful_compilation('type io=packed record a: (x=5) end; begin end.');

      test_only_for_successful_compilation('type io=packed record a: (x=3) end; begin end.');
      test_only_for_successful_compilation('type tx=packed record i: int8 end; var x: tx; begin x := x end.');
      test_only_for_successful_compilation('type tx=packed record i: int8 end; var o: overlay tx end; x: tx; begin x := o end.');

      overlay.i := 0;  // suppress compiler warning
      assert(sizeof(overlay.r) = 8);
      assert(sizeof(overlay.i) = 8);
   end;

procedure test_type_syntax;
   begin
      display('=========================');
      display('TESTING TYPE_SYNTAX_UNIT');
      display('========================');
      test_integer_type;
      test_TCharDataType;
      test_TBooleanDataType;
      test_subrange_type;
      test_TEnumType;
      test_TRecordType;
      test_TOverlayType;
      test_TSetType;
      test_TArrayType;
      test_TStringType;
      test_TFloatingPointDataType;
      test_TPackedRecordType;
      display('')
   end;

end.
