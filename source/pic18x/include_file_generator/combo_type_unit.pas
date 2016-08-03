UNIT combo_type_unit;

INTERFACe

uses
  RegularExpressions, Generics.Collections, LibXmlParser;

type
   TComboTypeSFRPattern =
      class
         sfrs:
            array of
               record
                  regex: TRegEx;
                  regex_pattern: string
               end;
         constructor CreateCopy (orig: TComboTypeSFRPattern);
      end;
   TComboTypeSFRPatternList = TObjectList<TComboTypeSFRPattern>;

   TFieldNameFixup =
      class
      private
         regex: TRegEx;
      protected
         function get_regex_pattern: string;
            virtual; abstract;
         procedure set_regex_pattern (pattern: string);
            virtual;
         function get_replacement_format_string: string;
            virtual; abstract;
         procedure set_replacement_format_string (fmt: string);
            virtual; abstract;
      public
         property RegexPattern: string read get_regex_pattern write set_regex_pattern;
         property ReplacementFormatString: string read get_replacement_format_string write set_replacement_format_string;
         constructor CreateCopy (orig: TFieldNameFixup);
            virtual;
         function Match (var s: string): boolean;
      end;
   TGetNewTFieldNameFixupObject = function (regex, fmt: string): TFieldNameFixup;
   TFieldNameFixupList =
      class (TObjectList<TFieldNameFixup>)
         constructor CreateCopy (orig: TFieldNameFixupList; get_new_fieldname_fixup_object: TGetNewTFieldNameFixupObject);
         function FixFieldname (fn, varno: string; get_new_fieldname_fixup_object: TGetNewTFieldNameFixupObject): string;
      end;

   TAddField =
      class
      private
         f_Fieldname: string;
         f_Bitno: integer;
         f_Width: integer;
      protected
         procedure set_FieldName (s: string);
            virtual;
         procedure set_Bitno (b: integer);
            virtual;
         procedure set_Width (w: integer);
            virtual;
         function get_FieldName: string;
            virtual;
         function get_Bitno: integer;
            virtual;
         function get_Width: integer;
            virtual;
      public
         property FieldName: string read get_FieldName write set_FieldName;
         property Bitno: integer read get_Bitno write set_Bitno;
         property Width: integer read get_Width write set_Width;
         constructor CreateCopy (orig: TAddField);
            virtual;
      end;
   TGetNewTAddFieldObject = function (_fieldname: string; _bitno, _width: integer): TAddField;
   TAddFieldList =
      class (TObjectList<TAddField>)
         constructor CreateCopy (orig: TAddFieldList; get_new_add_field_object: TGetNewTAddFieldObject);
      end;

   TComboType =
      class
      type
         t_field_name_fixup =
            class (TFieldNameFixup)
            protected
               regex_pattern: string;
               replacement_format_string: string;
               function get_regex_pattern: string;
                  override;
               procedure set_regex_pattern (pattern: string);
                  override;
               function get_replacement_format_string: string;
                  override;
               procedure set_replacement_format_string (fmt: string);
                  override;
            end;
      var
         TypeName: string;
         VarNameFormatString: string;
         Reversed: boolean;
         SFRPatterns: TComboTypeSFRPatternList;
         FieldNameFixups: TFieldNameFixupList;
         AddFields: TAddFieldList;
         constructor Create;
         constructor CreateNew;
         function Size: integer;
         destructor Destroy;
            override;
      end;
   TComboTypeList =
      class (TObjectList<TComboType>)
         constructor Create;   // load from combo_types.xml
            reintroduce;
         procedure SortIntoStandardOrder;
         procedure Save;       // save to combo_types.xml
      private
         combo_type: TComboType;
         sfr_pattern: TComboTypeSFRPattern;
         procedure XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);
         procedure XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
      end;

var
   NewFieldFixupFound: boolean;

function GetNew_TComboType_t_field_name_fixup_Object (regex, fmt: string): TFieldNameFixup;
function GetNew_TAddFieldObject (fieldname: string; bitno, width: integer): TAddField;

IMPLEMENTATION

uses
  SysUtils, LibXmlComps, Classes, Generics.Defaults;

var
   combo_type_file_name: string;


//==================
//  TFieldNameFixup
//==================

function GetNew_TComboType_t_field_name_fixup_Object (regex, fmt: string): TFieldNameFixup;
   begin
      result := TComboType.t_field_name_fixup.Create;
      result.RegexPattern := regex;
      result.ReplacementFormatString := fmt
   end;

procedure TFieldNameFixup.set_regex_pattern (pattern: string);
   begin
      regex := TRegEx.Create (pattern)
   end;

constructor TFieldNameFixupList.CreateCopy (orig: TFieldNameFixupList; get_new_fieldname_fixup_object: TGetNewTFieldNameFixupObject);
   var
      f: TFieldNameFixup;
   begin
      Create;
      for f in orig do
         Add (get_new_fieldname_fixup_object (f.RegexPattern, f.ReplacementFormatString))
   end;

function TFieldNameFixupList.FixFieldname (fn, varno: string; get_new_fieldname_fixup_object: TGetNewTFieldNameFixupObject): string;
   var
      fixup: TFieldNameFixup;
      AAAnBBB_regex: TRegEx;
      m: TMatch;
   begin
      result := fn;
      for fixup in self do
         if fixup.Match (fn) then
            begin
               result := fn;
               exit
            end;
      if (fn = '-') or (varno = '') then
         exit;
      AAAnBBB_regex := TRegEx.Create ('^([A-Za-z]+)([0-9]+)([A-Z]+[0-9]*)$');
      m := AAAnBBB_regex.Match (fn);
      if m.Success then
         begin   // fn has a possible varno embedded in it
            if m.Groups[2].Value <> varno then
               begin
                  Add (get_new_fieldname_fixup_object ('^' + fn + '()$', fn));
                  NewFieldFixupFound := true
               end
            else  // replace varno with 'x'
               result := format ('%sx%s', [m.Groups[1].Value, m.Groups[3].Value])
         end
   end;

constructor TFieldNameFixup.CreateCopy (orig: TFieldNameFixup);
   begin
      RegexPattern := orig.RegexPattern;
      ReplacementFormatString := orig.ReplacementFormatString
   end;

function TFieldNameFixup.Match (var s: string): boolean;
   var
      m: TMatch;
   begin
      try
         m := regex.Match (s);
         result := m.Success;
         if result then
            case m.Groups.Count of
               1: begin
                     if Pos ('%s', LowerCase(ReplacementFormatString)) <> 0 then
                        raise EConvertError.Create ('extranesous "%s" in replacement format string but no parentesized group in pattern');
                     s := ReplacementFormatString
                  end;
               2: begin
                     if Pos('%s', LowerCase(ReplacementFormatString)) = 0 then
                        raise EConvertError.Create ('parentesized group in pattern but no "%s" in replacement format string');
                     s := format (ReplacementFormatString, [m.Groups[1].Value])
                  end
            else
               raise EConvertError.Create ('only one parentesized group allowed in pattern')
            end
      except
         on e: EConvertError do
            raise EConvertError.Create ('Pattern ''' + RegexPattern + ''', ' + e.Message)
      end
   end;


//============
//  TAddField
//============

function GetNew_TAddFieldObject (fieldname: string; bitno, width: integer): TAddField;
   begin
      result := TAddField.Create;
      result.FieldName := fieldname;
      result.Bitno := bitno;
      result.Width := width
   end;

procedure TAddField.set_FieldName (s: string);
   begin
      f_FieldName := s
   end;

procedure TAddField.set_Bitno (b: integer);
   begin
      f_Bitno := b
   end;

procedure TAddField.set_Width (w: integer);
   begin
      f_Width := w
   end;

function TAddField.get_FieldName: string;
   begin
      result := f_FieldName
   end;

function TAddField.get_Bitno: integer;
   begin
      result := f_Bitno
   end;

function TAddField.get_Width: integer;
   begin
      result := f_Width
   end;

constructor TAddField.CreateCopy (orig: TAddField);
   begin
      FieldName := orig.FieldName;
      Bitno := orig.Bitno;
      Width := orig.Width
   end;

constructor TAddFieldList.CreateCopy (orig: TAddFieldList; get_new_add_field_object: TGetNewTAddFieldObject);
   var
      f: TAddField;
   begin
      Create;
      for f in orig do
         Add (get_new_add_field_object (f.FieldName, f.Bitno, f.Width))
   end;


//=============
//  TComboType
//=============

function TComboType.t_field_name_fixup.get_regex_pattern: string;
   begin
      result := regex_pattern
   end;

procedure TComboType.t_field_name_fixup.set_regex_pattern (pattern: string);
   begin
      regex_pattern := pattern;
      inherited
   end;

function TComboType.t_field_name_fixup.get_replacement_format_string: string;
   begin
      result := replacement_format_string
   end;

procedure TComboType.t_field_name_fixup.set_replacement_format_string (fmt: string);
   begin
      replacement_format_string := fmt
   end;

constructor TComboTypeSFRPattern.CreateCopy (orig: TComboTypeSFRPattern);
   var i: integer;
   begin
      SetLength (sfrs, Length(orig.sfrs));
      for i := 0 to Length(orig.sfrs)-1 do
         begin
            sfrs[i].regex := TRegEx.Create (orig.sfrs[i].regex_pattern);
            sfrs[i].regex_pattern := orig.sfrs[i].regex_pattern
         end
   end;

constructor TComboType.Create;
   begin
      SFRPatterns := TComboTypeSFRPatternList.Create (true);
      FieldNameFixups := TFieldNameFixupList.Create;
      AddFields := TAddFieldList.Create (true)
   end;

constructor TComboType.CreateNew;
   begin
      Create;
      SFRPatterns.Add (TComboTypeSFRPattern.Create);
      SetLength (SFRPatterns[0].sfrs,1);
      SFRPatterns[0].sfrs[0].regex := TRegEx.Create ('');
      SFRPatterns[0].sfrs[0].regex_pattern := ''
   end;

function TComboType.Size: integer;
   begin
      result := Length(SFRPatterns[0].sfrs)
   end;

destructor TComboType.Destroy;
   begin
      SFRPatterns.Free;
      FieldNameFixups.Free;
      AddFields.Free;
      inherited
   end;

constructor TComboTypeList.Create;
   var
      xmlscanner:  TXmlScanner;
   begin
      inherited;
      OwnsObjects := true;
      xmlscanner := TXmlScanner.Create (nil);
      xmlscanner.OnStartTag := XmlScannerStartTag;
      xmlscanner.OnEmptyTag := XmlScannerEmptyTag;
      XmlScanner.Filename := combo_type_file_name;
      XmlScanner.Execute;
      xmlscanner.free
   end;

procedure TComboTypeList.SortIntoStandardOrder;
   var
      ct: TComboType;
   begin
      for ct in self do
         begin
            ct.SFRPatterns.Sort (TComparer<TComboTypeSFRPattern>.Construct
                                    (function (const L, R: TComboTypeSFRPattern): integer
                                        var i: integer;
                                        begin
                                           assert (Length(L.sfrs) = Length(R.sfrs));
                                           for i := 0 to Length(L.sfrs)-1 do
                                              begin
                                                 result := CompareText (L.sfrs[i].regex_pattern, R.sfrs[i].regex_pattern);
                                                 if result <> 0 then
                                                    exit
                                              end;
                                           result := 0
                                        end
                                    )
                                );
            ct.FieldNameFixups.Sort (TComparer<TFieldNameFixup>.Construct
                                        (function (const L, R: TFieldNameFixup): integer
                                            begin
                                               result := CompareText (L.RegexPattern, R.RegexPattern)
                                            end
                                        )
                                    );
            ct.AddFields.Sort (TComparer<TAddField>.Construct
                                  (function (const L, R: TAddField): integer
                                      begin
                                         result := CompareText (L.FieldName, R.FieldName)
                                      end
                                  )
                              )
         end;
      Sort (TComparer<TComboType>.Construct
              (function (const L, R: TComboType): integer
                  var
                     i: integer;
                  begin
                     result := CompareText (L.TypeName, R.TypeName);
                     if result <> 0 then
                        exit;

                     if L.Reversed < R.Reversed then
                        result := -1
                     else if L.Reversed > R.Reversed then
                        result := 1;
                     if result <> 0 then
                        exit;

                     if Length(L.SFRPatterns[0].sfrs) < Length(R.SFRPatterns[0].sfrs) then
                        result := -1
                     else if Length(L.SFRPatterns[0].sfrs) > Length(R.SFRPatterns[0].sfrs) then
                        result := 1;
                     if result <> 0 then
                        exit;

                     for i := 0 to Length(L.SFRPatterns[0].sfrs)-1 do
                        begin
                           result := CompareText(L.SFRPatterns[0].sfrs[i].regex_pattern, R.SFRPatterns[0].sfrs[i].regex_pattern);
                           if result <> 0 then
                              exit
                        end;
                  end
              )
           )
   end;

procedure TComboTypeList.Save;
   function bool_val (b: boolean): string;
      begin
         if b then
            result := 'true'
         else
            result := 'false'
      end;
   var
      f: TextFile;
      ct: TComboType;
      sfr_pattern: TComboTypeSFRPattern;
      fixup: TFieldNameFixup;
      addfield: TAddField;
      i: integer;
   begin
      SortIntoStandardOrder;
      AssignFile (f, combo_type_file_name);
      Rewrite (f);
      writeln (f, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
      writeln (f, '<ComboTypes>');
      for ct in self do
         begin
            writeln (f, format ('   <ComboType TypeName="%s" VarNameFormatString="%s" Reversed="%s">',
                                [ct.TypeName, ct.VarNameFormatString, bool_val(ct.Reversed)]
                               )
                    );
// writeln (f, format ('      <ComboTypeGroup Group="%d" Reversed="%s">', [1, bool_val(ct.Reversed)]));
            for sfr_pattern in ct.SFRPatterns do
               begin
                  writeln (f, '         <SFRPattern>');
                  for i := 0 to Length(sfr_pattern.sfrs)-1 do
                     writeln (f, format ('            <SFR regex="%s"/>', [sfr_pattern.sfrs[i].regex_pattern]));
                  writeln (f, '         </SFRPattern>')
               end;

            for fixup in ct.FieldNameFixups do
               writeln (f, format ('         <FieldNameFixup regex="%s" replacement="%s"/>',
                                   [fixup.RegexPattern, fixup.ReplacementFormatString]
                                  )
                       );

            for addfield in ct.AddFields do
               writeln (f, format ('         <AddField name="%s" bitno="%d" width="%d"/>',
                                   [addfield.FieldName, addfield.Bitno, addfield.Width]
                                  )
                       );
//writeln (f, '      </ComboTypeGroup>');
            writeln (f, '   </ComboType>')
         end;
      writeln (f, '</ComboTypes>');
      CloseFile (f)
   end;

procedure TComboTypeList.XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);

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

   function get_attrb (attr_name: string): boolean;
      begin
         result := false;
         if get_attrs(attr_name) = 'true' then
            result := true
         else if LowerCase(get_attrs(attr_name)) = 'false' then
            result := false
         else
            assert (false)
      end;

   begin
      if TagName = 'ComboType' then
         begin
            combo_type := TComboType.Create;
            Add (combo_type);
            with combo_type do
               begin
                  TypeName := get_attrs('TypeName');
                  VarNameFormatString := get_attrs('VarNameFormatString');
                  Reversed := get_attrb('Reversed')
               end
         end
      else if TagName = 'SFRPattern' then
         begin
            sfr_pattern := TComboTypeSFRPattern.Create;
            combo_type.SFRPatterns.Add(sfr_pattern)
         end
   end;

procedure TComboTypeList.XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);

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

   function get_attrb (attr_name: string): boolean;
      begin
         result := false;
         if get_attrs(attr_name) = 'true' then
            result := true
         else if LowerCase(get_attrs(attr_name)) = 'false' then
            result := false
         else
            assert (false)
      end;

   var
      i: integer;
      fixup: TComboType.t_field_name_fixup;
      addfield: TAddField;
   begin
      if TagName = 'SFR' then
         begin
            i := Length(sfr_pattern.sfrs);
            SetLength (sfr_pattern.sfrs, i+1);
            sfr_pattern.sfrs[i].regex := TRegEx.Create (get_attrs('regex'));
            sfr_pattern.sfrs[i].regex_pattern := get_attrs('regex')
         end
      else if TagName = 'FieldNameFixup' then
         begin
            fixup := TComboType.t_field_name_fixup.Create;
            combo_type.FieldNameFixups.Add(fixup);
            fixup.regex := TRegEx.Create (get_attrs('regex'));
            fixup.regex_pattern := get_attrs('regex');
            fixup.ReplacementFormatString := get_attrs('replacement')
         end
      else if TagName = 'AddField' then
         begin
            addfield := TAddField.Create;
            combo_type.AddFields.Add(addfield);
            addfield.FieldName := get_attrs('name');
            addfield.Bitno := get_attri('bitno');
            addfield.Width := get_attri('width')
         end
   end;


INITIALIZATION
   combo_type_file_name := ExtractFilePath(ParamStr(0)) + 'pic18x\combo_types.xml';

END.

