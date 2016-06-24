UNIT pic18x_ioreg_unit;

INTERFACE

uses
   pic18x_information_unit, Generics.Collections, Generics.Defaults,
   pic18x_typedef_unit, common_unit, all_pic18x_sfr_field_info_unit,
   combo_type_unit;

type
   tIoreg =
      class
         sfr_name: string;
         sfr_addr: integer;
         typedef: tTypeDef;
         constructor Create (_sfr_name: string; _sfr_addr: integer; _typedef: tTypeDef);
      end;
   tIoregList =
      class (TObjectDictionary<string,tIoreg>)
         constructor Create (pic_info: TPICInfo; pic_sfr_field_info: tMicroControllerSFRFieldInfo; typedefs: tTypeDefList; combo_type_list: TComboTypeList);
         procedure AppendIncludeFileSource (out: TOutStringProc);
      end;

IMPLEMENTATION

uses System.Classes, System.SysUtils, System.RegularExpressions;

constructor tIoreg.Create (_sfr_name: string; _sfr_addr: integer; _typedef: tTypeDef);
   begin
      sfr_name := _sfr_name;
      sfr_addr := _sfr_addr;
      typedef := _typedef
   end;

constructor tIoregList.Create (pic_info: TPICInfo; pic_sfr_field_info: tMicroControllerSFRFieldInfo; typedefs: tTypeDefList; combo_type_list: TComboTypeList);

   procedure process_uninteresting_sfrs;
      var
         sfr: TSFR;
         msfrH, msfrL: TMuxdSFR;
         jsfr: TJoinedSFR;
         i: integer;
      begin
         for sfr in pic_info.sfrs do
            if (sfr.name = '') or (sfr.isCPU_SFR) or (sfr.ishidden) then
               begin
                  sfr.processed := true;
                  case sfr.kind of
                     simple_sfr:
                        ;
                     joined_sfr:
                        begin
                           jsfr := TJoinedSFR(sfr);
                           case jsfr.sfrL.kind of
                              simple_sfr:
                                 begin
                                    jsfr.sfrL.processed := true;
                                    jsfr.sfrH.processed := true;
                                    if jsfr.sfrU <> nil then
                                       jsfr.sfrU.processed := true
                                 end;
                              muxd_sfr:
                                 begin
                                    msfrH := TMuxdSFR(jsfr.sfrH);
                                    msfrL := TMuxdSFR(jsfr.sfrL);
                                    assert (jsfr.sfrU = nil);
                                    assert (Length(msfrH.sfrs) = Length(msfrL.sfrs));
                                    for i := 0 to Length(msfrH.sfrs)-1 do
                                       begin
                                         msfrH.sfrs[i].processed := true;
                                         msfrL.sfrs[i].processed := true
                                       end
                                 end;
                           else
                              assert (false)
                           end;
                        end;
                     muxd_sfr:
                        begin
                           msfrL := TMuxdSFR(sfr);
                           for i := 0 to Length(msfrL.sfrs)-1 do
                              msfrL.sfrs[i].processed := true
                        end;
                  else
                     assert (false)
                  end
               end
      end;

   procedure process_single_sfr (sfr: TSFRDef);
      var
         typedef: tTypeDef;
      begin
         if sfr.processed then
            exit;

         assert (sfr.kind = simple_sfr);
         typedef := typedefs.GetTypeDef (sfr.name, 1, false);
         typedef.AddSFRFields (sfr, 0);
         if sfr.IsAlternateSharedAddressSFR then
            Add (sfr.name, tIoreg.Create (sfr.name, $1000 + sfr.addr, typedef))
         else
            Add (sfr.name, tIoreg.Create (sfr.name, sfr.addr, typedef));
         sfr.processed := true
      end;

   procedure process_sfr_pair (sfrH, sfrL: TSFRDef);
      function min (a, b: integer): integer;
         begin
            if a < b then
               result := a
            else
               result := b
         end;
      var
         name: string;
         typedef: tTypeDef;
      begin
         assert (sfrH.processed = sfrL.processed);
         if sfrH.processed then
            exit;

         assert (sfrH.kind = simple_sfr);
         assert (sfrL.kind = simple_sfr);

         name := Copy (sfrH.name, 1, Length(sfrH.name)-1);
         if not ((sfrH.name = 'ODCON1') and (sfrL.name = 'ODCON2')) then
            begin
               assert (sfrH.name[Length(sfrH.name)] = 'H', 'nameH:' + sfrH.name + ' nameL:' + sfrL.name);
               assert (sfrL.name = name + 'L', 'nameL is ' + sfrL.name)
            end;
         typedef := typedefs.GetTypeDef (name, 2, sfrH.addr > sfrL.addr);
         typedef.AddSFRFields (sfrH, 0);
         typedef.AddSFRFields (sfrL, 1);
         assert (sfrH.IsAlternateSharedAddressSFR = sfrL.IsAlternateSharedAddressSFR);
         if sfrH.IsAlternateSharedAddressSFR then
            Add (name, tIoreg.Create (name, $1000 + min(sfrH.addr, sfrL.addr), typedef))
         else
            Add (name, tIoreg.Create (name, min(sfrH.addr, sfrL.addr), typedef));
         sfrH.processed := true;
         sfrL.processed := true
      end;

   procedure process_combo_sfrs;
      var
         combo_type: TComboType;
         var_name: string;
         typedef: tTypeDef;
         addr_slot_idx: integer;

      function sufficient_contiguous_slots: boolean;
         var
            i: integer;
         begin
            result := false;
            for i := 1 to combo_type.SFRPatterns.Count-1 do
               if pic_sfr_field_info.sfr_addr_slots[addr_slot_idx+i].addr <> pic_sfr_field_info.sfr_addr_slots[addr_slot_idx].addr + i then
                  exit;
            result := true
         end;

      function sfr_patterns_match: boolean;
         var
            x: integer;
            group_1_match_value: string;
            expected_group_match_count: integer;

         function sfr_column_matches: boolean;
            var
               y: integer;

            function match_in_addr_slot: boolean;
               var
                  i: integer;
                  m: TMatch;
               begin  // match_in_addr_slot
                  result := true;
                  for i := 0 to Length(pic_sfr_field_info.sfr_addr_slots[addr_slot_idx+y].sfrs)-1 do
                     begin
                        m := combo_type.SFRPatterns[x].sfrs[y].regex.Match (String(pic_sfr_field_info.sfr_addr_slots[addr_slot_idx+y].sfrs[i].sfr_name));
                        if m.Success then
                           try
                              case m.Groups.Count of
                                 1: begin
                                       if y = 0 then
                                          begin
                                             expected_group_match_count := 1;
                                             group_1_match_value := '';
                                             if Pos ('%s', LowerCase(combo_type.VarNameFormatString)) <> 0 then
                                                raise EConvertError.Create ('extranesous "%s" in var format string but no parentesized group in pattern');
                                          end
                                       else
                                          if expected_group_match_count <> 1 then
                                             raise EConvertError.Create ('first SFR pattern had parenthesized group but this pattern does not');
                                       exit
                                    end;
                                 2: begin
                                       if y = 0 then
                                          begin
                                             expected_group_match_count := 2;
                                             group_1_match_value := m.groups[1].value;
                                             if Pos ('%s', LowerCase(combo_type.VarNameFormatString)) = 0 then
                                                raise EConvertError.Create ('parenthesized group in pattern but no "%s" in var format string');
                                             exit
                                          end;
                                       if group_1_match_value = m.groups[1].value then
                                          exit;
                                       if expected_group_match_count <> 2 then
                                          raise EConvertError.Create ('first SFR pattern had no parenthesized group but this one does')
                                    end;
                              else
                                 raise EConvertError.Create ('only one parenthesized group allowed in pattern')
                              end
                           except
                              on e: EConvertError do
                                 raise EConvertError.Create ('SFRPattern=' + combo_type.SFRPatterns[x].sfrs[y].regex_pattern + ' ' + e.Message)
                           end
                     end;
                  result := false
               end;   // match_in_addr_slot

            begin  // sfr_column_matches
               try
                  result := false;
                  for y := 0 to Length(combo_type.SFRPatterns[x].sfrs)-1 do
                     if not match_in_addr_slot then
                        exit;
                  var_name := format (combo_type.VarNameFormatString, [group_1_match_value]);
                  result := true
               except
                  on e: EConvertError do
                     raise EConvertError.Create ('Combotype SFR Pattern ComboType=' + combo_type.TypeName + ' ' + e.Message)
               end
            end;   // sfr_column_matches

         var
            y: integer;
            offset: integer;

         procedure record_field (slot: tSFRAddressSlot);

            procedure add_sfr_fields (sfr: TSFRDef);
               var
                  m: TSFRMode;
                  f: TSFRField;
                  bitno: integer;
                  field_name: string;
               begin  // add_sfr_fields
                  for m in sfr.modes do
                     begin
                        bitno := -1;
                        for f in m.fields do
                           begin
                              field_name := typedef.field_name_filter (f.fieldname);
                              field_name := combo_type.FieldNameFixups.FixFieldname (field_name, group_1_match_value, GetNew_TComboType_t_field_name_fixup_Object);
                              typedef.AddField (m.id, field_name, ((combo_type.Size-offset-1)*8) + bitno + f.width, f.width);
                              bitno := bitno + f.width
                           end
                     end
               end;   // add_sfr_fields

            var
               i: integer;
               m: TMatch;
            begin  // record_field
               for i := 0 to Length(slot.sfrs)-1 do
                  begin
                     m := combo_type.SFRPatterns[x].sfrs[y].regex.Match(String(slot.sfrs[i].sfr_name));
                     if m.Success then
                        begin
                           add_sfr_fields (slot.sfrs[i].sfr);
                           slot.sfrs[i].sfr.processed := true;
                           exit
                        end
                  end
            end;   // record_field
         var
            add_field: TAddField;
         begin  // sfr_patterns_match
            result := true;
            for x := 0 to combo_type.SFRPatterns.Count-1 do
               if sfr_column_matches then
                  begin
                     var_name := format (combo_type.VarNameFormatString, [group_1_match_value]);
                     assert (combo_type.TypeName[1] = 't');
                     typedef := typedefs.GetTypeDef (Copy (combo_type.TypeName, 2, 9999), combo_type.Size, combo_type.Reversed);
                     for add_field in combo_type.AddFields do
                        typedef.AddField ('', add_field.FieldName, add_field.Bitno, add_field.Width);
                     for y := 0 to combo_type.Size-1 do
                        begin
                           if combo_type.Reversed then
                              offset := combo_type.Size-1 - y
                           else
                              offset := y;
                           record_field (pic_sfr_field_info.sfr_addr_slots[addr_slot_idx+y])
                        end;
                     exit
                  end;
            result := false
         end;   // sfr_patterns_match

      begin  // process_combo_sfrs
         for combo_type in combo_type_list do
            try
               for addr_slot_idx := 0 to pic_sfr_field_info.sfr_addr_slots.Count - combo_type.SFRPatterns.Count do
                  if sufficient_contiguous_slots
                     and
                     sfr_patterns_match
                  then
                     Add (var_name, tIoreg.Create (var_name, pic_sfr_field_info.sfr_addr_slots[addr_slot_idx].addr, typedef))
            except
               on e: EConvertError do
                  raise EConvertError.Create ('ComboType Field Name Fixup Error: ComboType=' + combo_type.TypeName + ', ' + e.Message)
            end
      end;   // process_combo_sfrs

   procedure process_joined_and_muxd_sfrs;
      var
         i,j: integer;
         msfrH, msfrL: TMuxdSFR;
         jsfr: TJoinedSFR;
      begin  // process_joined_and_muxd_sfrs
         for i := 0 to Length(pic_info.sfrs)-1 do
            case pic_info.sfrs[i].kind of
               muxd_sfr:
                  begin
                     msfrL := TMuxdSFR(pic_info.sfrs[i]);
                     for j := 0 to Length(msfrL.sfrs)-1 do
                        process_single_sfr (msfrL.sfrs[j]);
                     msfrL.processed := true
                  end;
               joined_sfr:
                  begin
                     jsfr := TJoinedSFR(pic_info.sfrs[i]);
                     if jsfr.sfrU <> nil then
                        assert ((jsfr.sfrU.name = '') or (jsfr.sfrU.isCPU_SFR) or (jsfr.sfrU.ishidden));
                     assert (jsfr.sfrH.kind = jsfr.sfrL.kind);
                     case jsfr.sfrL.kind of
                        simple_sfr:
                           process_sfr_pair (TSFRDef(jsfr.sfrH), TSFRDef(jsfr.sfrL));
                        muxd_sfr:
                           begin
                              msfrH := TMuxdSFR(jsfr.sfrH);
                              msfrL := TMuxdSFR(jsfr.sfrL);
                              assert (Length(msfrH.sfrs) = Length(msfrL.sfrs));
                              for j := 0 to Length(msfrH.sfrs)-1 do
                                 process_sfr_pair (msfrH.sfrs[j], msfrL.sfrs[j])
                           end
                     else
                        assert (false)
                     end;
                     jsfr.processed := true
                  end;
            else
            end
      end;   // process_joined_and_muxd_sfrs

   procedure process_adjacent_H_L_pairs;
      var
         i: integer;
         sfr1, sfr2: TSFR;
      begin  // combine_adjacent_H_L_pairs
         for i := 0 to Length(pic_info.sfrs)-2 do
            begin
               sfr1 := pic_info.sfrs[i];
               sfr2 := pic_info.sfrs[i+1];
               if Copy(sfr1.name, 1, Length(sfr1.name)-1) = Copy(sfr2.name, 1, Length(sfr2.name)-1) then
                  if (sfr1.name[Length(sfr1.name)] = 'H') and (sfr2.name[Length(sfr2.name)] = 'L') then
                     process_sfr_pair (TSFRDef(sfr1), TSFRDef(sfr2))
                  else if (sfr1.name[Length(sfr1.name)] = 'L') and (sfr2.name[Length(sfr2.name)] = 'H') then
                     process_sfr_pair (TSFRDef(sfr2), TSFRDef(sfr1))
            end
      end;   // combine_adjacent_H_L_pairs

   procedure process_remaining_single_sfrs;
      var
         i: integer;
      begin  // process_remaining_single_sfrs
         for i := 0 to Length(pic_info.sfrs)-1 do
            if not pic_info.sfrs[i].processed then
               begin
                  assert (pic_info.sfrs[i].kind = simple_sfr);
                  process_single_sfr (TSFRDef (pic_info.sfrs[i]))
               end
      end;   // process_remaining_single_sfrs

   begin   // tIoregList.Create
      inherited Create ([doOwnsValues]);

      process_uninteresting_sfrs;
      process_combo_sfrs;
      process_joined_and_muxd_sfrs;
      process_adjacent_H_L_pairs;
      process_remaining_single_sfrs
   end;   // tIoregList.Create

procedure tIoregList.AppendIncludeFileSource (out: TOutStringProc);
   var
      ioreg_name: string;
      keyArray: TArray<string>;
      keyCollection: TObjectDictionary<string,tIoreg>.TKeyCollection;
   begin
      keyCollection := nil;
      try
         if Count > 0 then
            begin
               // output alphabetically
               keyCollection := TObjectDictionary<string,tIoreg>.TKeyCollection.Create (Self);
               keyArray := keyCollection.ToArray;
               TArray.Sort<string> (keyArray,
                                    TComparer<string>.Construct
                                       (function (const L, R: string): integer
                                           begin
                                              result := CompareText (L, R)
                                           end
                                       )
                                    );

               out ('ioreg');
               for ioreg_name in keyArray do
                  out ('   ' + ioreg_name + ': t' + Self[ioreg_name].typedef.typename + ' at $' + format ('%3.3X', [Self[ioreg_name].sfr_addr]) + ';');
               out ('')
            end
      finally
         keyCollection.Free
      end
   end;

END.


