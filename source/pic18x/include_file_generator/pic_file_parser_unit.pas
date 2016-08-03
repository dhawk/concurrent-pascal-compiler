UNIT pic_file_parser_unit;

INTERFACE

uses pic18x_information_unit;

function parse_pic_file (fn: string): TPICInfo;

IMPLEMENTATION

uses
   Classes, SysUtils, wirth_balanced_binary_tree_unit, LibXmlComps, LibXmlParser;

type
   TPICFileParser = class;

   TFieldInfo =
      class
         mode, fieldname: string;
         bitno, width: integer;    // bits are numbered left to right over all bytes of type, e.g. 15..0 for 2-byte type
         constructor Create (_mode, _name: string; _bit_no, _width: integer);
      end;

   TSelectSFR =
      class
         when: string
      end;

   TTag =
      (other_tag,
       AdjustPoint,
       CodeSector,
       ConfigFuseSector,
       DataSpace,
       DCRDef,
       DCRMode,
       DCRFieldDef,
       DCRFieldSemantic,
       DPRDataSector,
       EEDataSector,
       EmulatorZone,
       ExtendedModeOnly,
       GPRDataSector,
       JoinedSFRDef,
       MuxedSFRDef,
       NMMRPlace,
       PIC,
       ProgramSpace,
       RegardlessOfMode,
       SelectSFR,
       SFRDataSector,
       SFRDef,
       SFRFieldDef,
       SFRMode,
       SFRModeList,
       WORMHoleSector
      );

   TPICFileParser =
      class (TPICInfo)
      public
         stk:
            array [0 .. 100] of
               record tag: TTag;
               ptr: TObject;
               save: boolean;
            end;
         in_relevant_sfr_data_sector: boolean;
         in_fuse_section: boolean;
         current_fuse_byte: TConfigByte;
         current_fuse_byte_field: TConfigByteField;
         fuse_byte_bitno: integer;
         fuse_offset: integer;
         level: integer;
         is_pic18_with_extended_instruction_set: boolean;
         constructor Create;
            reintroduce;
         procedure XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);
         procedure XmlScannerEndTag(Sender: TObject; TagName: String);
         procedure XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
         procedure parse (FileName: string);
      end;

type
   TTagInfo =
      class(TBalancedTreeEntry)
         id: string;
         tag: TTag;
         constructor Create (_tag: TTag; _id: string);
         function compare (a: TBalancedTreeEntry): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy (ToA: TBalancedTreeEntry); // data
            override;
      end;

var
   tags: TBalancedBinaryTree;

function CompareFieldsByPositionAndWidth (v1, v2: TObject): boolean;
   //   result := v1 <= v2
   begin
      if TFieldInfo(v1).bitno > TFieldInfo(v2).bitno then
         result := false
      else if TFieldInfo(v1).bitno < TFieldInfo(v2).bitno then
         result := true
      else
         result := TFieldInfo(v1).width <= TFieldInfo(v2).width
   end;

function CompareFieldsByNamePositionWidthAndMode (v1, v2: TObject): boolean;
   //   result := v1 <= v2
   begin
      if TFieldInfo(v1).fieldname > TFieldInfo(v2).fieldname then
         result := false
      else if TFieldInfo(v1).fieldname < TFieldInfo(v2).fieldname then
         result := true
      else
         if TFieldInfo(v1).bitno > TFieldInfo(v2).bitno then
            result := false
         else if TFieldInfo(v1).bitno < TFieldInfo(v2).bitno then
            result := true
         else
            if TFieldInfo(v1).width > TFieldInfo(v2).width then
               result := false
            else if TFieldInfo(v1).width < TFieldInfo(v2).width then
               result := true
            else
               result := TFieldInfo(v1).mode <= TFieldInfo(v2).mode
   end;


//=============
//  TFieldInfo

constructor TFieldInfo.Create (_mode, _name: string; _bit_no, _width: integer);
   begin
      mode := _mode;
      fieldname := _name;
      bitno := _bit_no;
      width := _width
   end;


function CompareFieldsByModeAndPosition (v1,v2: TObject): boolean;
   //   result := v1 <= v2
   begin
      if TFieldInfo(v1).mode > TFieldInfo(v2).mode then
         result := false
      else if TFieldInfo(v1).mode < TFieldInfo(v2).mode then
         result := true
      else  // TFieldInfo(v1).mode = TFieldInfo(v2).mode
         result := TFieldInfo(v1).bitno >= TFieldInfo(v2).bitno
   end;

constructor TTagInfo.Create (_tag: TTag; _id: string);
   begin
      tag := _tag;
      id := _id;
      inherited Create
   end;

function TTagInfo.compare (a: TBalancedTreeEntry): Shortint;
   // a < self :-1  a=self :0  a > self :+1
   begin
      if TTagInfo (a).id < id
      then
         result := -1
      else if TTagInfo (a).id = id
      then
         result := 0
      else
         result := +1
   end;

procedure TTagInfo.copy (ToA: TBalancedTreeEntry); // data
   begin
      TTagInfo (ToA).id := id;
      TTagInfo (ToA).tag := tag
   end;

function identify_tag (s: string): TTag;
   function r (t: TTagInfo): TTag;
      begin
         if t = nil then
            result := other_tag
         else if s < t.id then
            result := r (TTagInfo(t.lesser_values))
         else if s = t.id then
            result := t.tag
         else
            result := r (TTagInfo(t.greater_values))
      end;
   begin
      result := r (TTagInfo(tags.root))
   end;

constructor TPICFileParser.Create;
   begin
   end;

procedure TPICFileParser.parse (FileName: string);
   var
      xmlscanner:  TXmlScanner;
   begin
      is_pic18_with_extended_instruction_set := true;
      in_relevant_sfr_data_sector := false;
      level := -1;
      xmlscanner := TXmlScanner.Create (self);
      xmlscanner.OnStartTag := XmlScannerStartTag;
      xmlscanner.OnEndTag := XmlScannerEndTag;
      xmlscanner.OnEmptyTag := XmlScannerEmptyTag;
      XmlScanner.Filename := FileName;
      XmlScanner.Execute;
      xmlscanner.free;
      assert (level = -1)
   end;

procedure TPICFileParser.XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);

   function get_attrs (attr_name: string): string;
      var i: integer;
      begin
         for i := 0 to Attributes.Count-1 do
            if String(Attributes.Name(i)) = attr_name then
               begin
                  result := String(Attributes.Value(i));
                  exit
               end;
         result := ''
      end;

   function get_attri (attr_name: string): integer;
      var
         v: string;
         i: integer;
      begin
         result := 0;
         v := get_attrs (attr_name);
         if (Length(v) > 2) and (v[1] = '0') and (v[2] = 'x')
         then
            for i := 3 to Length(v) do
               case v[i] of
                  '0' .. '9':
                     result := (result * 16) + ord (v[i]) - ord ('0');
                  'A' .. 'F':
                     result := (result * 16) + ord (v[i]) - ord ('A') + 10;
                  'a' .. 'f':
                     result := (result * 16) + ord (v[i]) - ord ('a') + 10;
               else
                  assert (false)
               end
         else
            for i := 1 to Length(v) do
               begin
                  assert (CharInSet (v[i], ['0' .. '9']));
                  result := (result * 10) + ord (v[i]) - ord ('0')
               end
      end;

   function hex_value (s: string; var i: integer): integer;
      const
         hex_chars = ['0'..'9', 'a'..'f', 'A'..'F'];
      begin
         assert ((s[i] = '0') and (s[i+1]='x'));
         i := i + 2;
         assert (CharInSet (s[i], hex_chars));
         result := 0;
         repeat
            result := result shl 4;
            if CharInSet (s[i], ['0'..'9']) then
               result := result + ord(s[i]) - ord('0')
            else if CharInSet (s[i], ['A'..'F']) then
               result := result + 10 + ord(s[i]) - ord('A')
            else
               result := result + 10 + ord(s[i]) - ord('a');
            i := i + 1
         until (i > Length(s)) or (not CharInSet (s[i], hex_chars))
      end;

   var
      i,j: integer;
      field: TSFRField;
      jsfr: TJoinedSFR;
      sfr: TSFRDef;
      mode: TSFRMode;
      msfr: TMuxdSFR;
      select: TSelectSFR;
      when: string;
   begin
      level := level + 1;
      stk[level].tag := identify_tag (TagName);
      stk[level].ptr := nil;
      stk[level].save := false;
      if is_pic18_with_extended_instruction_set
      then
         case stk[level].tag of
            other_tag:
               begin
                  if (stk[level-1].tag in [RegardlessOfMode, ExtendedModeOnly])
                     and
                     (TagName <> 'edc:NMMRPlace')
                     and
                     (TagName <> 'edc:EmulatorZone')
                  then
                     assert (false, 'unexpected element in DataSpace???? ' + TagName);
               end;
            AdjustPoint:
               if in_relevant_sfr_data_sector
               then
                  case stk[level-1].tag of
                     SFRDataSector:
                        ;
                     SFRMode:
                        begin
                           field := TSFRField.Create;
                           // leave field.name blank
                           field.width := get_attri ('edc:offset');
                           mode := TSFRMode (stk[level-1].ptr);
                           i := Length (mode.fields);
                           SetLength (mode.fields, i + 1);
                           mode.fields[i] := field
                        end;
                  else
                     assert (false);
                  end
               else if in_fuse_section then
                  case stk[level-1].tag of
                     ConfigFuseSector,
                     WORMHoleSector:
                        begin
                           i := Length(config_bytes);
                           SetLength(config_bytes, i+1);
                           config_bytes[i] := TConfigByte.Create
                        end;
                     DCRMode:
                        begin
                           current_fuse_byte_field := TConfigByteField.Create;
                           current_fuse_byte_field.name := '-';
                           fuse_byte_bitno := fuse_byte_bitno + get_attri ('edc:offset');
                           current_fuse_byte_field.bitno := fuse_byte_bitno;
                           current_fuse_byte_field.width := get_attri ('edc:offset');
                           current_fuse_byte_field.default_value := 0;
                           i := Length(current_fuse_byte.fields);
                           SetLength (current_fuse_byte.fields, i+1);
                           current_fuse_byte.fields[i] := current_fuse_byte_field
                        end;
                  else
                     assert (false)
                  end;
            CodeSector:
               begin
                  assert ((level = 2) and (stk[level-1].tag = ProgramSpace));
                  rom_size := get_attri ('edc:endaddr') - get_attri ('edc:beginaddr')
               end;
            ConfigFuseSector,
            WORMHoleSector:
               begin
                  assert ((level = 2) and (stk[level-1].tag = ProgramSpace));
                  in_fuse_section := true;
                  fuse_offset := 0
               end;
            DataSpace:
               assert (level = 1);
            DCRDef:
               begin
                  current_fuse_byte := TConfigByte.Create;
                  current_fuse_byte.name := get_attrs ('edc:cname');
                  current_fuse_byte.default_value := get_attri('edc:default');
                  fuse_byte_bitno := -1;
                  assert (get_attri('edc:nzwidth') = 8);   // only width ever seen
                  i := Length(config_bytes);
                  SetLength(config_bytes, i+1);
                  config_bytes[i] := current_fuse_byte
               end;
            DCRMode:
               begin
                  current_fuse_byte.mode_count := current_fuse_byte.mode_count + 1;
                  assert (current_fuse_byte.mode_count = 1)   // have never seen multiple modes in a fuse byte
               end;
            DCRFieldDef:
               begin
                  current_fuse_byte_field := TConfigByteField.Create;
                  current_fuse_byte_field.name := get_attrs ('edc:cname');
                  current_fuse_byte_field.desc := get_attrs ('edc:desc');
                  current_fuse_byte_field.ishidden := LowerCase(get_attrs ('edc:ishidden')) = 'true';
                  fuse_byte_bitno := fuse_byte_bitno + get_attri ('edc:nzwidth');
                  current_fuse_byte_field.bitno := fuse_byte_bitno;
                  current_fuse_byte_field.width := get_attri ('edc:nzwidth');
                  current_fuse_byte_field.nop_marker :=
                     (stk[level-4].tag = WORMHoleSector)
                     and
                     (current_fuse_byte.name[Length(current_fuse_byte.name)] = 'H')
                     and
                     (current_fuse_byte_field.bitno = 7)
                     and
                     (current_fuse_byte_field.width = 4)
                     and
                     (current_fuse_byte_field.ishidden);
                  current_fuse_byte_field.default_value := (current_fuse_byte.default_value shr (current_fuse_byte_field.bitno + 1 - current_fuse_byte_field.width)) and get_attri ('edc:mask');
                  i := Length(current_fuse_byte.fields);
                  SetLength (current_fuse_byte.fields, i+1);
                  current_fuse_byte.fields[i] := current_fuse_byte_field
               end;
            DCRFieldSemantic:
               if UpperCase(get_attrs('edc:cname')) <> 'RESERVED' then
                  begin
                     i := Length(current_fuse_byte_field.values);
                     SetLength(current_fuse_byte_field.values, i+1);
                     current_fuse_byte_field.values[i].cname := get_attrs('edc:cname');
                     current_fuse_byte_field.values[i].islanghidden := get_attrs('edc:islanghidden') = 'true';
                     current_fuse_byte_field.values[i].desc := get_attrs('edc:desc');

                     when := get_attrs('edc:when');
                     j := Pos('field & 0x', when);
                     assert (j>0);
                     j := j + Length('field & ');
                     hex_value (when, j);
                     assert (j = Pos(') == 0x', when));
                     j := j + Length(') == ');
                     current_fuse_byte_field.values[i].value := hex_value(when,j)
                  end;
            DPRDataSector:
               begin
                  assert (level = 3);
                  i := Length(data_space_sectors);
                  SetLength (data_space_sectors, i + 1);
                  data_space_sectors[i] := TDataSpaceSector.Create (dpr_dataspace_sector, get_attri ('edc:beginaddr'), get_attri ('edc:endaddr'))
               end;
            EEDataSector:
               begin
                  assert ((level = 2) and (stk[level-1].tag = ProgramSpace));
                  eeprom_size := get_attri ('edc:endaddr') - get_attri ('edc:beginaddr');
               end;
            EmulatorZone:
               assert (level = 3);
            ExtendedModeOnly:
               assert (level = 2);
            GPRDataSector:
               begin
                  assert (level = 3);
                  if stk[level-1].tag in [RegardlessOfMode, ExtendedModeOnly] then
                     begin
                        i := Length(data_space_sectors);
                        SetLength (data_space_sectors, i + 1);
                        data_space_sectors[i] := TDataSpaceSector.Create (gpr_dataspace_sector, get_attri ('edc:beginaddr'), get_attri ('edc:endaddr'))
                     end
                end;
            JoinedSFRDef:
               if in_relevant_sfr_data_sector
               then
                  begin
                     jsfr := TJoinedSFR.Create (Self, true);   // if declared as joined in .pic file for C compiler purposes, then should be reversed for CPC
                     jsfr.name := get_attrs ('edc:cname');
                     jsfr.addr := get_attri ('edc:_addr');
                     jsfr.offset := get_attri ('edc:nzwidth');
                     stk[level].ptr := jsfr;
                     assert (stk[level-1].tag = SFRDataSector);
                     i := Length (sfrs);
                     SetLength (sfrs, i + 1);
                     sfrs[i] := jsfr;
                     stk[level].save := true
                  end;
            MuxedSFRDef:
               if in_relevant_sfr_data_sector
               then
                  begin
                     msfr := TMuxdSFR.Create (Self);
                     msfr.addr := get_attri ('edc:_addr');
                     msfr.offset := get_attri ('edc:nzwidth');
                     stk[level].ptr := msfr;
                     stk[level].save := true;
                     case stk[level-1].tag of
                        JoinedSFRDef:
                           begin
                              jsfr := TJoinedSFR (stk[level-1].ptr);
                              if jsfr.sfrL = nil then
                                 jsfr.sfrL := msfr
                              else if jsfr.sfrH = nil then
                                 jsfr.sfrH := msfr
                              else
                                 assert (false);
                              assert (stk[level-2].tag = SFRDataSector)
                           end;
                        SFRDataSector:
                           begin
                              i := Length (sfrs);
                              SetLength (sfrs, i + 1);
                              sfrs[i] := msfr;
                           end;
                     else
                        assert (false)
                     end
                  end;
            NMMRPlace:
               assert (level = 3);
            PIC:
               begin
                  assert (level = 0);
                  chip_name := get_attrs ('edc:name');
                  if get_attrs ('edc:isextended') = 'true'
                  then
                     is_extended := true
                  else
                     is_pic18_with_extended_instruction_set := false
               end;
            ProgramSpace:
               assert (level = 1);
            RegardlessOfMode:
               assert (level = 2);
            SelectSFR:
               begin
                  assert (stk[level-1].tag = MuxedSFRDef);
                  select := TSelectSFR.Create;
                  select.when := get_attrs ('edc:when');
                  stk[level].ptr := select
               end;
            SFRDataSector:
               begin
                  assert (level = 3);
                  if stk[level-1].tag = RegardlessOfMode
                  then
                     begin
                        if get_attrs('edc:regionid') = 'accesssfr'
                        then
                           first_access_bank_absolute_address := get_attri('edc:beginaddr');
                        in_relevant_sfr_data_sector := true;
                        i := Length(data_space_sectors);
                        SetLength (data_space_sectors, i + 1);
                        data_space_sectors[i] := TDataSpaceSector.Create (sfr_dataspace_sector, get_attri ('edc:beginaddr'), get_attri ('edc:endaddr'))
                     end
               end;
            SFRDef:
               if in_relevant_sfr_data_sector
               then
                  begin
                     sfr := TSFRDef.Create (Self);
                     sfr.name := get_attrs ('edc:cname');
                     sfr.addr := get_attri ('edc:_addr');

                     if sfr.name = 'EEADR' then
                         eeadr := sfr.addr
                     else if sfr.name = 'EEADRH' then
                         eeadrh := sfr.addr
                     else if sfr.name = 'EECON1' then
                         eecon1 := sfr.addr
                     else if sfr.name = 'EECON2' then
                         eecon2 := sfr.addr
                     else if sfr.name = 'EEDATA' then
                         eedata := sfr.addr
                     else if sfr.name = 'WDTCON' then
                         wdtcon := sfr.addr;

                     sfr.offset := get_attri ('edc:nzwidth');
                     sfr.ishidden := (get_attrs ('edc:ishidden') = 'true');
                     stk[level].ptr := sfr;
                     case stk[level-1].tag of
                        JoinedSFRDef:
                           begin
                              jsfr := TJoinedSFR (stk[level-1].ptr);
                              if jsfr.sfrL = nil then
                                 jsfr.sfrL := sfr
                              else if jsfr.sfrH = nil then
                                 jsfr.sfrH := sfr
                              else if jsfr.sfrU = nil then
                                 jsfr.sfrU := sfr
                              else
                                 assert (false);
                              stk[level].save := true
                           end;
                        NMMRPlace:
                           { will be ignored };
                        SelectSFR:
                           begin
                              select := TSelectSFR (stk[level-1].ptr);
                              sfr.when := select.when;

                              assert (stk[level-2].tag = MuxedSFRDef);
                              msfr := TMuxdSFR (stk[level-2].ptr);
                              i := Length (msfr.sfrs);
                              SetLength (msfr.sfrs, i+1);
                              msfr.sfrs[i] := sfr;

                              if msfr.name = '' then
                                 msfr.name := sfr.name
                              else
                                 begin
                                    assert (msfr.name2 = '');
                                    msfr.name2 := sfr.name
                                 end;

                              stk[level].save := true
                           end;
                        SFRDataSector:
                           begin
                              i := Length (sfrs);
                              SetLength (sfrs, i + 1);
                              sfrs[i] := sfr;
                              stk[level].save := true
                           end;
                     else
                        assert (false)
                     end;
                  end;
            SFRFieldDef:
               if in_relevant_sfr_data_sector
               then
                  begin
                     assert (stk[level-1].tag = SFRMode);
                     assert (stk[level-2].tag = SFRModeList);
                     case stk[level-3].tag of
                        JoinedSFRDef:
                           ;
                        SFRDef:
                           begin
                              field := TSFRField.Create;
                              field.fieldname := get_attrs ('edc:cname');
                              field.width := get_attri ('edc:nzwidth');
                              mode := TSFRMode (stk[level-1].ptr);
                              i := Length (mode.fields);
                              SetLength (mode.fields, i + 1);
                              mode.fields[i] := field
                           end;
                     else
                        assert (false)
                     end
                  end;
            SFRMode:
               if in_relevant_sfr_data_sector
               then
                  begin
                     assert (stk[level-1].tag = SFRModeList);
                     case stk[level-2].tag of
                        JoinedSFRDef:
                           ;
                        SFRDef:
                           begin
                              sfr := TSFRDef (stk[level-2].ptr);
                              mode := TSFRMode.Create;
                              mode.id := get_attrs ('edc:id');
                              stk[level].ptr := mode;
                              i := Length (sfr.modes);
                              SetLength (sfr.modes, i + 1);
                              sfr.modes[i] := mode;
                              stk[level].save := true
                           end;
                     else
                        assert (false)
                     end
                  end;
            SFRModeList:
               if in_relevant_sfr_data_sector
               then
                  case stk[level-1].tag of
                     JoinedSFRDef:
                        ;
                     SFRDef:
                        begin
                           sfr := TSFRDef (stk[level-1].ptr);
                           assert (Length(sfr.modes) = 0)
                        end;
                  else
                     assert (false)
                  end;
         else
            assert (false)
         end
   end;

procedure TPICFileParser.XmlScannerEndTag(Sender: TObject; TagName: String);
   var
      joined_sfr_info: TJoinedSFR;
      sfr0, sfr1: TSFRDef;
      t: TSFR;
   begin
      assert (stk[level].tag = identify_tag(TagName));
      case stk[level].tag of
         JoinedSFRDef:
            begin
               joined_sfr_info := TJoinedSFR(stk[level].ptr);
               if (joined_sfr_info <> nil)
                  and
                  (joined_sfr_info.sfrL.kind = simple_sfr)
                  and
                  (joined_sfr_info.sfrH.kind = simple_sfr)
               then
                  begin
                     sfr0 := TSFRDef(joined_sfr_info.sfrL);
                     sfr1 := TSFRDef(joined_sfr_info.sfrH);
                     if (joined_sfr_info.name = Copy(sfr0.name, 1, Length(sfr0.name)-1))
                        and
                        (joined_sfr_info.name = Copy(sfr1.name, 1, Length(sfr1.name)-1))
                        and
                        (sfr0.name[Length(sfr0.name)] = 'H')
                        and
                        (sfr1.name[Length(sfr1.name)] = 'L')
                     then   // this quirk has only been seen in the UFRM joined sfr
                        begin
                           joined_sfr_info.reversed := false;
                           t := joined_sfr_info.sfrL;
                           joined_sfr_info.sfrL := joined_sfr_info.sfrH;
                           joined_sfr_info.sfrH := t
                        end
                  end
            end;
         SFRDataSector:
            in_relevant_sfr_data_sector := false;
         ConfigFuseSector:
            in_fuse_section := false;
         DCRDef:
            current_fuse_byte := nil;
      else
         {nop}
      end;
      if not stk[level].save
      then
         stk[level].ptr.Free;
      level := level - 1
   end;

procedure TPICFileParser.XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
   begin
      XmlScannerStartTag(Sender, TagName, Attributes);
      XmlScannerEndTag(Sender, TagName)
   end;

procedure init_tag (tag: TTag; s: string);
   begin
      tags.Add (TTagInfo.Create(tag, s))
   end;

function parse_pic_file (fn: string): TPICInfo;
   begin
      result := TPICFileParser.Create;
      TPICFileParser(result).parse (fn)
   end;


INITIALIZATION
   tags := TBalancedBinaryTree.Create;
   init_tag (PIC, 'edc:PIC');
   init_tag (GPRDataSector, 'edc:GPRDataSector');
   init_tag (EEDataSector, 'edc:EEDataSector');
   init_tag (CodeSector, 'edc:CodeSector');
   init_tag (ConfigFuseSector, 'edc:ConfigFuseSector');
   init_tag (SFRDataSector, 'edc:SFRDataSector');
   init_tag (JoinedSFRDef, 'edc:JoinedSFRDef');
   init_tag (SFRDef, 'edc:SFRDef');
   init_tag (SFRMode, 'edc:SFRMode');
   init_tag (SFRFieldDef, 'edc:SFRFieldDef');
   init_tag (AdjustPoint, 'edc:AdjustPoint');
   init_tag (DataSpace, 'edc:DataSpace');
   init_tag (DCRDef, 'edc:DCRDef');
   init_tag (DCRMode, 'edc:DCRMode');
   init_tag (DCRFieldDef, 'edc:DCRFieldDef');
   init_tag (DCRFieldSemantic, 'edc:DCRFieldSemantic');
   init_tag (RegardlessOfMode, 'edc:RegardlessOfMode');
   init_tag (DPRDataSector, 'edc:DPRDataSector');
   init_tag (ExtendedModeOnly, 'edc:ExtendedModeOnly');
   init_tag (ProgramSpace, 'edc:ProgramSpace');
   init_tag (NMMRPlace, 'edc:NMMRPlace');
   init_tag (EmulatorZone, 'edc:EmulatorZone');
   init_tag (SFRModeList, 'edc:SFRModeList');
   init_tag (MuxedSFRDef, 'edc:MuxedSFRDef');
   init_tag (SelectSFR, 'edc:SelectSFR');
   init_tag (WORMHoleSector, 'edc:WORMHoleSector');

FINALIZATION
   tags.Free;

END.
