UNIT combo_type_dialog_unit;

INTERFACE

uses
   Windows, Classes, Controls, Forms, StdCtrls, ComCtrls, TabNotBk,
   combo_type_sfr_pattern_frame_unit, Spin, combo_type_unit, ExtCtrls;

type
   t_var_sfr_list =
      record
         var_name: string;
         sfr_names: array of string
      end;
   p_var_sfr_list = ^t_var_sfr_list;

   t_all_pics_var_sfr_list =
      record
      private
         pics:
            array of
               record
                  pic_name: string;
                  vars: array of t_var_sfr_list
               end;
         current_pic_idx: integer;
         current_var_idx: integer;
      public
         procedure init;
         procedure start_new_pic (pic_name: string);
         procedure start_new_pic_var (var_name: string);
         procedure record_pic_var_sfr (sfr_name: string);
         function get_var_sfr_list (_pic_name, _var_name: string): p_var_sfr_list;
      end;

  TPICVarSFRList =
     class
        pic_name: string;
        var_list: array of p_var_sfr_list;
        procedure add_var_list (var_sfr_list: p_var_sfr_list);
     end;

   TComboTypeDialog =
      class(TForm)
         TabbedNotebook: TTabbedNotebook;
         NumberOfPatternsSpinEdit: TSpinEdit;
         Label1: TLabel;
         Label2: TLabel;
         NumberOfSFRsSpinEdit: TSpinEdit;
         CancelButton: TButton;
         ReversedCheckBox: TCheckBox;
         SFRPatternsScrollBox: TScrollBox;
         Offset0Label: TLabel;
         TestSFRPatternPanel: TPanel;
         TestSFRPatternButton: TButton;
         StatusBar: TStatusBar;
         Label3: TLabel;
         VariableNameFormatStringEdit: TEdit;
         ComboTypeUsageReportMemo: TMemo;
         FieldNameFixupsScrollBox: TScrollBox;
         AddFieldNameFixupButton: TButton;
         Label5: TLabel;
         Label6: TLabel;
         Label7: TLabel;
         Label9: TLabel;
         ScrollBox1: TScrollBox;
         FieldUsageReportTreeView: TTreeView;
         Label11: TLabel;
         ScrollBox2: TScrollBox;
         FieldDiscrepanciesTreeView: TTreeView;
         TestFieldNameFixupsEdit: TEdit;
         TestFieldNameFixupsButton: TButton;
         AddAddFieldsButton: TButton;
         AddFieldsScrollBox: TScrollBox;
         SaveButtonHintPanel: TPanel;
         SaveButton: TButton;
         Label12: TLabel;
         TypeNameEdit: TEdit;
         DeleteTypeButton: TButton;
         procedure NumberOfPatternsSpinEditChange(Sender: TObject);
         procedure NumberOfSFRsSpinEditChange(Sender: TObject);
         procedure TestSFRPatternButtonClick(Sender: TObject);
         procedure ValidateOnChange(Sender: TObject);
         procedure FormCreate(Sender: TObject);
         procedure TabbedNotebookChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
         procedure AddFieldNameFixupButtonClick(Sender: TObject);
         procedure FormClose(Sender: TObject; var Action: TCloseAction);
         procedure FieldUsageReportTreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
         procedure FieldUsageReportTreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure AddAddFieldsButtonClick(Sender: TObject);
         procedure DeleteTypeButtonClick(Sender: TObject);
         procedure TestFieldNameFixupsButtonClick(Sender: TObject);
         procedure ExitDialog(Sender: TObject);
      private
         offset_labels: array {sfr#} of TLabel;  // offset_labels[0] not used
         varname_labels: array {pattern#} of TLabel;
         test_patterns: array {sfr#} of TEdit;
         sfr_patterns: array {pattern#} of array {sfr#} of TComboTypeSFRPatternFrame;
         sfr_pattern_frame_width: integer;
         suppress_validation: boolean;
         sfr_patterns_valid: boolean;
         add_fields_valid: boolean;
         pic_var_sfr_list: t_all_pics_var_sfr_list;
         std_field_fixup: TFrame;  // actually TFieldnameFixupFrame
         procedure set_combo_type (ct: TComboType);
         function get_combo_type: TComboType;
         procedure set_number_of_patterns (n: integer);
         function get_number_of_patterns: integer;
         procedure set_number_of_sfrs (n: integer);
         function get_number_of_sfrs: integer;
         procedure create_sfr_pattern_frame (pattern: string; x,y: integer);
         function sfr_pattern_left (x: integer): integer;
         procedure generate_combo_type_usage_report;
      private
         property number_of_patterns: integer read get_number_of_patterns write set_number_of_patterns;
         procedure DisplayHint(Sender: TObject);
      public
         FieldNameFixups: TFieldNameFixupList;
         AddFields: TAddFieldList;
         property NumberOfSFRs: integer read get_number_of_sfrs write set_number_of_sfrs;
         property ComboType: TComboType read get_combo_type write set_combo_type;
         function Regex_Left: integer;
         function Replacement_Left: integer;
         function Test_Result_Left: integer;
         procedure PositionFieldnameFixupFrames;
         procedure PositionAddFieldFrames;
         function GetCurrentTypeName: string;
         procedure Validate;
      end;

var
   ComboTypeDialog: TComboTypeDialog;

IMPLEMENTATION

{$R *.dfm}

uses
   all_pic18x_sfr_field_info_unit, main_form_unit, Menus, Graphics, RegularExpressions,
   fieldname_fixup_frame_unit, pic18x_selection_dialog_unit, Generics.Collections, Generics.Defaults,
   choose_c_variable_listing_unit, view_c_declarations_unit, Dialogs,
   add_field_frame_unit, SysUtils;

const
   SFR_Patterns_Tab      = 0;
   Field_Name_Fixups_Tab = 1;
   Add_Fields_Tab        = 2;
   Combo_Type_Usage_Tab  = 3;
   Field_Usage_Tab       = 4;
   Field_Discrepancy_Tab = 5;

type
   TTreeNodeWithDestructor =
      class (TTreeNode)
         destructor Destroy;
            override;
      end;

destructor TTreeNodeWithDestructor.Destroy;
   begin
      TObject(data).Free
   end;

type
   TComboVariable =
      class
         pic: string;
         variable: string;
         constructor Create (_pic, _variable: string);
      end;
   TComboVariableList =
      class (TObjectList<TComboVariable>)
         procedure Sort;
            reintroduce;
      end;
   TField =
      class
         offset: integer;
         name: string;
         bitno: integer;
         width: integer;
         usage: TComboVariableList;
         constructor Create (_offset: integer; _name: string; _bitno, _width: integer);
         procedure RecordUsage (pic, variable: string);
         destructor Destroy;
            override;
      end;
   TFieldList =
      class (TObjectList<TField>)
         function GetField (_offset: integer; _name: string; _bitno, _width: integer): TField;
         procedure Sort;
            reintroduce;
      end;

constructor TComboVariable.Create (_pic, _variable: string);
   begin
      pic := _pic;
      variable := _variable
   end;

procedure TComboVariableList.Sort;
   begin
      inherited Sort (TComparer<TComboVariable>.Construct
                        (function (const L, R: TComboVariable): integer
                            begin
                               result := ComparePICNames (L.pic, R.pic);
                               if result <> 0 then
                                  exit;

                               result := CompareText (L.variable, R.variable)
                            end
                        )
                     )
   end;

constructor TField.Create (_offset: integer; _name: string; _bitno, _width: integer);
   begin
      offset := _offset;
      name := _name;
      bitno := _bitno;
      width := _width;
      usage := TComboVariableList.Create
   end;

procedure TField.RecordUsage (pic, variable: string);
   var
      v: TComboVariable;
   begin
      for v in usage do                                      // wny needed?
         if (v.pic = pic) and (v.variable = variable) then
            exit;
      usage.Add (TComboVariable.Create (pic, variable))
   end;

destructor TField.Destroy;
   begin
      usage.Free;
      inherited
   end;

procedure TComboTypeDialog.FormClose(Sender: TObject; var Action: TCloseAction);
   begin
      if ModalResult <> mrOk then
         begin
            FieldNameFixups.Free;
            FieldNameFixups := nil;
            AddFields.Free;
            AddFields := nil
         end
   end;

type
   tSpecialFieldnameFixupFrame =
      class (TFieldnameFixupFrame)
         constructor Create (AOwner: TComponent);
            override;
         procedure Test (s: string);
            override;
      end;

constructor tSpecialFieldnameFixupFrame.Create (AOwner: TComponent);
   begin
      inherited;
      BevelKind := bkTile;
      BevelOuter := bvRaised;
      BevelInner := bvNone;
      BevelWidth := 1;
      Anchors := [akLeft, akBottom];
      regex := TRegex.Create (RegexPatternEdit.Text)
   end;

procedure tSpecialFieldnameFixupFrame.Test (s: string);
   var
      m: TMatch;
   begin
      m := regex.Match (s);
      if m.Success then
         begin
            RegexPatternEdit.Color := clGreen;
            Label1.Caption := format ('%sx%s', [m.Groups[1].Value, m.Groups[3].Value])
         end
      else
         begin
            RegexPatternEdit.Color := clWhite;
            Label1.Caption := ''
         end
   end;

procedure TComboTypeDialog.FormCreate(Sender: TObject);
   var
      fr: TFieldnameFixupFrame;
   begin
      Application.OnHint := DisplayHint;

      suppress_validation := true;
      fr := tSpecialFieldnameFixupFrame.Create (self);
      fr.left := FieldNameFixupsScrollbox.Left + 12;
      fr.top := FieldNameFixupsScrollBox.Top + FieldNameFixupsScrollBox.Height + 20;
      fr.position_elements (FieldNameFixupsScrollbox.Left);
      fr.RegexPatternEdit.Text := '^([A-Z]+)([0-9]+)([A-Z]+[0-9]*)$';
      fr.RegexPatternEdit.ReadOnly := true;
      fr.ReplacementFormatStringEdit.Text := 'special /1x/3';
      fr.ReplacementFormatStringEdit.ReadOnly := true;
      fr.Label1.Caption := '';
      fr.DeleteButton.Visible := false;
      fr.Width := fr.Width + 2;
      fr.Parent := TWinControl(TabbedNotebook.Pages.Objects[Field_Name_Fixups_Tab]);
      fr.Height := fr.Panel1.Height;
      std_field_fixup := fr;
      suppress_validation := false
   end;

function TFieldList.GetField (_offset: integer; _name: string; _bitno, _width: integer): TField;
   begin
      for result in self do
         if (result.offset = _offset)
            and
            (result.name = _name)
            and
            (result.bitno = _bitno)
            and
            (result.width = _width)
         then
            exit;
      result := TField.Create (_offset, _name, _bitno, _width);
      Add (result)
   end;

procedure TFieldList.Sort;
   begin
      inherited Sort (TComparer<TField>.Construct
                         (function (const L, R: TField): integer
                             begin
                                result := 0;

                                // offset in ascending order
                                if L.offset > R.offset then
                                   result := 1
                                else if L.offset < R.offset then
                                   result := -1;
                                if result <> 0 then
                                   exit;

                                // bitno in descending order
                                if L.bitno < R.bitno then
                                   result := 1
                                else if L.bitno > R.bitno then
                                   result := -1;
                                if result <> 0 then
                                   exit;

                                // name in ascending order
                                result := CompareText (String(L.name), String(R.name));
                                if result <> 0 then
                                   exit;

                                // width in ascending order
                                if L.width > R.width then
                                   result := 1
                                else if L.width < R.width then
                                   result := -1
                             end
                         )
                     )
   end;

procedure TComboTypeDialog.set_combo_type (ct: TComboType);
   var
      x, y: integer;
      old_suppress_validation: boolean;
   begin
      old_suppress_validation := suppress_validation;
      suppress_validation := true;
      TypeNameEdit.Text := ct.TypeName;
      VariableNameFormatStringEdit.Text := ct.VarNameFormatString;
      ReversedCheckBox.Checked := ct.Reversed;
      number_of_patterns := ct.SFRPatterns.Count;
      NumberOfSFRs := Length(ct.SFRPatterns[0].sfrs);
      for x := 0 to ct.SFRPatterns.Count-1 do
         for y := 0 to Length(ct.SFRPatterns[0].sfrs)-1
            do sfr_patterns[x,y].pattern := ct.SFRPatterns[x].sfrs[y].regex_pattern;
      NumberOfPatternsSpinEdit.Value := number_of_patterns;
      NumberOfSFRsSpinEdit.Value := NumberOfSFRs;

      assert (FieldNameFixups = nil);
      FieldNameFixups := TFieldNameFixupList.CreateCopy (ct.FieldNameFixups, GetNewTVisibleFieldNameFixupObject);
      PositionFieldnameFixupFrames;

      assert (AddFields = nil);
      AddFields := TAddFieldList.CreateCopy (ct.AddFields, GetNewTAddFieldObject);
      PositionAddFieldFrames;

      suppress_validation := old_suppress_validation;
      Validate;
      TabbedNotebook.PageIndex := SFR_Patterns_Tab
   end;

procedure TComboTypeDialog.PositionFieldnameFixupFrames;
   var y: integer;
   begin
      for y := 0 to FieldNameFixups.Count-1 do
         with TVisibleFieldNameFixup(FieldNameFixups[y]) do
            Top := 10 + (y*(field_name_fixup_frame.Height+10))
   end;

procedure TComboTypeDialog.PositionAddFieldFrames;
   var y: integer;
   begin
      for y := 0 to AddFields.Count-1 do
         with TVisibleAddField(AddFields[y]) do
            Top := 10 + (y*(add_field_frame.Height+10))
   end;

function TComboTypeDialog.GetCurrentTypeName: string;
   begin
      result := TypeNameEdit.Text
   end;

function TComboTypeDialog.get_combo_type: TComboType;
   var
      x, y: integer;
      p: TComboTypeSFRPattern;
   begin
      result := TComboType.Create;
      result.TypeName := GetCurrentTypeName;
      result.VarNameFormatString := VariableNameFormatStringEdit.Text;
      result.Reversed := ReversedCheckBox.Checked;

      for x := 0 to number_of_patterns-1 do
         begin
            p := TComboTypeSFRPattern.Create;
            result.SFRPatterns.Add(p);
            SetLength (p.sfrs, NumberOfSFRs);
            for y := 0 to NumberOfSFRs-1
               do begin
                     p.sfrs[y].regex := TRegEx.Create (sfr_patterns[x,y].pattern);
                     p.sfrs[y].regex_pattern := sfr_patterns[x,y].pattern
                  end
         end;
      SetLength(sfr_patterns, 0);

      result.FieldNameFixups.Free;
      result.FieldNameFixups := TFieldNameFixupList.CreateCopy (FieldNameFixups, GetNew_TComboType_t_field_name_fixup_Object);
      FieldNameFixups.Free;
      FieldNameFixups := nil;

      result.AddFields.Free;
      result.AddFields := TAddFieldList.CreateCopy (AddFields, GetNew_TAddFieldObject);
      AddFields.Free;
      AddFields := nil
   end;

procedure TComboTypeDialog.DeleteTypeButtonClick(Sender: TObject);
   begin
      if MessageDlg ('Are you sure you want to permanently delete ' + TypeNameEdit.Text + '?', mtConfirmation, [mbYes,mbNo], 0) = mrNo then
         ModalResult := 0
      else
         ExitDialog(Sender)
   end;

procedure TComboTypeDialog.DisplayHint(Sender: TObject);
   begin
      StatusBar.SimpleText := GetLongHint(Application.Hint)
   end;

procedure TComboTypeDialog.Validate;
   var
      x,y: integer;
      fieldname_fixup_patterns_valid: boolean;
   begin
      if suppress_validation then
         exit;

      TypeNameEdit.Hint := '';
      if TypeNameEdit.Text = '' then
         TypeNameEdit.Hint := 'Can''t be blank'
      else if TypeNameEdit.Text[1] <> 't' then
         TypeNameEdit.Hint := 'typename must start with "t"'
      else if Length(TypeNameEdit.Text) = 1 then
         TypeNameEdit.Hint := 'incomplete typename';
      if TypeNameEdit.Hint = '' then
         TypeNameEdit.Color := clWhite
      else
         TypeNameEdit.Color := clRed;

      sfr_patterns_valid := true;
      if VariableNameFormatStringEdit.Text = '' then
         begin
            sfr_patterns_valid := false;
            VariableNameFormatStringEdit.Color := clRed;
            VariableNameFormatStringEdit.Hint := 'Can''t be blank'
         end
      else
         begin
            VariableNameFormatStringEdit.Color := clWhite;
            VariableNameFormatStringEdit.Hint := ''
         end;
      for x := 0 to number_of_patterns-1 do
         begin
            varname_labels[x].Caption := '';
            for y := 0 to NumberOfSFRs-1 do
               if not sfr_patterns[x,y].Valid then
                  sfr_patterns_valid := false
         end;
      TestSFRPatternButton.Enabled := sfr_patterns_valid;

      fieldname_fixup_patterns_valid := true;
      for x := 0 to FieldNameFixups.Count-1 do
         begin
            if not TVisibleFieldNameFixup(FieldNameFixups[x]).field_name_fixup_frame.Valid then
               fieldname_fixup_patterns_valid := false
         end;
      with std_field_fixup as TFieldnameFixupFrame do
         begin
            RegexPatternEdit.Color := clWhite;
            Label1.Caption := ''
         end;

      add_fields_valid := true;
      for x := 0 to AddFields.Count-1 do
         if not TVisibleAddField(AddFields[x]).add_field_frame.Valid then
            add_fields_valid := false;

      if (TypeNameEdit.Hint = '') and sfr_patterns_valid and fieldname_fixup_patterns_valid and add_fields_valid then
         begin
            SaveButton.Enabled := true;
            SaveButtonHintPanel.Hint := ''
         end
      else
         begin
            SaveButton.Enabled := false;
            if sfr_patterns_valid and fieldname_fixup_patterns_valid and add_fields_valid then
               SaveButtonHintPanel.Hint := 'Fix TypeName problem'
            else
               begin
                  SaveButtonHintPanel.Hint := 'Can''t Save until problems on the following tabs are fixed:';
                  if not sfr_patterns_valid then
                     SaveButtonHintPanel.Hint := SaveButtonHintPanel.Hint + ' [SFR Pattern(s)]';
                  if not fieldname_fixup_patterns_valid then
                     SaveButtonHintPanel.Hint := SaveButtonHintPanel.Hint + ' [Field Name Fixups]';
                  if not add_fields_valid then
                     SaveButtonHintPanel.Hint := SaveButtonHintPanel.Hint + ' [Add Fields]';
               end
         end
   end;

procedure TComboTypeDialog.ValidateOnChange(Sender: TObject);
   begin
      Validate
   end;

procedure TComboTypeDialog.TabbedNotebookChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
   var
      old_cursor: TCursor;
   begin
      AllowChange := true;
      case NewTab of
         SFR_Patterns_Tab,
         Field_Name_Fixups_Tab:
            Validate;
         Combo_Type_Usage_Tab,
         Field_Usage_Tab,
         Field_Discrepancy_Tab:
            if not (TabbedNotebook.PageIndex in [Combo_Type_Usage_Tab, Field_Usage_Tab, Field_Discrepancy_Tab]) then
               if not sfr_patterns_valid then
                  begin
                     ShowMessage ('Fix SFR Patterns Problems First');
                     TabbedNotebook.PageIndex := SFR_Patterns_Tab;
                     AllowChange := false
                  end
               else
                  begin
                     old_cursor := Screen.Cursor;

                     Screen.Cursor := crHourGlass;
                     Application.ProcessMessages;

                     generate_combo_type_usage_report;  // lengthy

                     Screen.Cursor := old_cursor;
                     Application.ProcessMessages;

                     PositionFieldnameFixupFrames
                  end;
      else
      end
   end;


//=================
// SFR Pattern Tab
//=================

function TComboTypeDialog.sfr_pattern_left (x: integer): integer;
   var
      fr: TComboTypeSFRPatternFrame;
   begin
      if sfr_pattern_frame_width = 0 then
         begin
            fr := TComboTypeSFRPatternFrame.Create(nil);
            sfr_pattern_frame_width := fr.Width;
            fr.Free
         end;
      result := TestSFRPatternPanel.Left + TestSFRPatternPanel.Width + 20 + (x * sfr_pattern_frame_width)
   end;

procedure TComboTypeDialog.create_sfr_pattern_frame (pattern: string; x, y: integer);
   var
      fr: TComboTypeSFRPatternFrame;
      offset0_cl: integer;
   begin
      offset0_cl := Offset0Label.Top + (Offset0Label.Height div 2);
      fr := TComboTypeSFRPatternFrame.Create(nil);
      fr.Validate := Validate;
      fr.pattern := pattern;
      sfr_patterns[x,y] := fr;
      fr.Name := 'Frame_' + IntToStr(x) + '_' + IntToStr(y);
      fr.parent := SFRPatternsScrollBox;
      fr.Top := offset0_cl - (fr.Edit.Height div 2) + (y * fr.Height);
      fr.Left := sfr_pattern_left(x);
      sfr_pattern_frame_width := fr.Width
   end;

procedure TComboTypeDialog.set_number_of_patterns (n: integer);
   var
      x, y: integer;
      current_number_of_patterns: integer;
      lbl: TLabel;
      old_supress_validation: boolean;
   begin
      old_supress_validation := suppress_validation;
      suppress_validation := true;
      current_number_of_patterns := get_number_of_patterns;
      if n > current_number_of_patterns then
         begin
            SetLength (sfr_patterns, n);
            SetLength (varname_labels, n);
            for x := current_number_of_patterns to n-1 do
               begin
                  SetLength(sfr_patterns[x], get_number_of_sfrs);
                  for y := 0 to get_number_of_sfrs-1 do
                     create_sfr_pattern_frame ('', x, y);

                  lbl := TLabel.Create (nil);
                  varname_labels[x] := lbl;
                  lbl.Name := 'Label_v' + IntToStr(x);
                  lbl.Caption := '';
                  lbl.Parent := SFRPatternsScrollBox;
                  lbl.Top := TestSFRPatternPanel.Top + TestSFRPatternButton.Top + (TestSFRPatternButton.Height div 2) - (lbl.Height div 2);
                  lbl.Left := sfr_pattern_left(x)
               end
         end
      else if n < current_number_of_patterns then
         begin
            for x := current_number_of_patterns-1 downto n do
               begin
                  varname_labels[x].Free;
                  for y := 0 to get_number_of_sfrs-1 do
                     sfr_patterns[x,y].Free;
               end;
            SetLength (varname_labels, n);
            SetLength (sfr_patterns, n)
         end;
      suppress_validation := old_supress_validation;
      Validate
   end;

function TComboTypeDialog.get_number_of_patterns: integer;
   begin
      result := Length(sfr_patterns)
   end;

procedure TComboTypeDialog.set_number_of_sfrs (n: integer);
   var
      current_number_of_sfrs: integer;
      x, y: integer;
      lbl: TLabel;
      edit: TEdit;
      fr_height: integer;
      old_suppress_validation: boolean;
   begin
      old_suppress_validation := suppress_validation;
      suppress_validation := true;
      current_number_of_sfrs := get_number_of_sfrs;
      if n > current_number_of_sfrs then
         begin
            for x := 0 to number_of_patterns-1 do
               begin
                  SetLength (sfr_patterns[x], n);
                  for y := current_number_of_sfrs to n-1 do
                     create_sfr_pattern_frame ('', x, y);
               end;
            fr_height := sfr_patterns[0,0].Height;
            SetLength (offset_labels, n);
            SetLength (test_patterns, n);
            TestSFRPatternPanel.Height := TestSFRPatternButton.Top + TestSFRPatternButton.Height + (n * fr_height) + 20;
            for y := current_number_of_sfrs to n-1 do
               begin
                  lbl := TLabel.Create (nil);
                  offset_labels[y] := lbl;
                  lbl.Caption := IntToStr(y) + ':';
                  lbl.Name := 'Label_' + IntToStr(y);
                  lbl.Parent := SFRPatternsScrollBox;
                  lbl.Top := Offset0Label.Top + (y * fr_height);
                  lbl.Left := Offset0Label.Left;

                  edit := TEdit.Create (nil);
                  test_patterns[y] := edit;
                  edit.Name := 'Edit_' + IntToStr(y);
                  edit.Text := '';
                  edit.Parent := TestSFRPatternPanel;
                  edit.Top := Offset0Label.Top + (Offset0Label.Height div 2) - TestSFRPatternPanel.Top + (y * fr_height) - (edit.Height div 2) - 2;
                  edit.Left := 10;
                  edit.Width := TestSFRPatternPanel.Width - 24;
                  edit.OnChange := ValidateOnChange
               end
            end
      else if n < current_number_of_sfrs then
         begin
            for y := current_number_of_sfrs-1 downto n do
               begin
                  offset_labels[y].Free;
                  test_patterns[y].Free
               end;
            SetLength (offset_labels, n);
            SetLength (test_patterns, n);
            for x := 0 to number_of_patterns-1 do
               begin
                  for y := current_number_of_sfrs-1 downto n do
                     sfr_patterns[x,y].Free;
                  SetLength (sfr_patterns[x], n)
               end;
            fr_height := sfr_patterns[0,0].Height;
            TestSFRPatternPanel.Height := TestSFRPatternButton.Top + TestSFRPatternButton.Height + (n * fr_height) + 20;
         end;
      suppress_validation := old_suppress_validation;
      Validate
   end;

function TComboTypeDialog.get_number_of_sfrs: integer;
   begin
      result := Length(sfr_patterns[0])
   end;

procedure TComboTypeDialog.NumberOfPatternsSpinEditChange(Sender: TObject);
   var
      confirmation_needed: boolean;
      x,y: integer;
   begin
      if NumberOfPatternsSpinEdit.Value < number_of_patterns then
         begin
            confirmation_needed := false;
            for x := NumberOfPatternsSpinEdit.Value to number_of_patterns-1 do
               for y := 0 to NumberOfSFRs-1 do
                  if sfr_patterns[x,y].pattern <> '' then
                     begin
                        confirmation_needed := true;
                        sfr_patterns[x,y].Edit.Color := clYellow
                     end
                  else
                     sfr_patterns[x,y].Edit.Color := clWhite;
            if confirmation_needed
               and
               (MessageDlg ('Delete non-empty patterns?', mtConfirmation, [mbYes,mbCancel], 0) = mrCancel)
            then
               begin
                  NumberOfPatternsSpinEdit.Value := number_of_patterns;
                  exit
               end
         end;
      number_of_patterns := NumberOfPatternsSpinEdit.Value
   end;

procedure TComboTypeDialog.NumberOfSFRsSpinEditChange(Sender: TObject);
   var
      confirmation_needed: boolean;
      x,y: integer;
   begin
      if NumberOfSFRsSpinEdit.Value < NumberOfSFRs then
         begin
            confirmation_needed := false;
            for x := 0 to number_of_patterns-1 do
               for y := NumberOfSFRsSpinEdit.Value to NumberOfSFRs-1 do
                  if sfr_patterns[x,y].pattern <> '' then
                     begin
                        confirmation_needed := true;
                        sfr_patterns[x,y].Edit.Color := clYellow
                     end
                  else
                     sfr_patterns[x,y].Edit.Color := clWhite;
            if confirmation_needed
               and
               (MessageDlg ('Delete non-empty patterns?', mtConfirmation, [mbYes,mbCancel], 0) = mrCancel)
            then
               begin
                  NumberOfSFRsSpinEdit.Value := NumberOfSFRs;
                  exit
               end
         end;
      NumberOfSFRs := NumberOfSFRsSpinEdit.Value
   end;

procedure TComboTypeDialog.TestFieldNameFixupsButtonClick(Sender: TObject);
   var
      f: TFieldNameFixup;
   begin
      TSpecialFieldnameFixupFrame(std_field_fixup).Test(TestFieldNameFixupsEdit.Text);
      for f in FieldNameFixups do
         TVisibleFieldNameFixup(f).field_name_fixup_frame.Test(TestFieldNameFixupsEdit.Text)
   end;

procedure TComboTypeDialog.TestSFRPatternButtonClick(Sender: TObject);
   var
      x, y: integer;
      success: boolean;
      group_match: string;
   begin
      for x := 0 to number_of_patterns-1 do
         begin
            success := true;
            for y := 0 to NumberOfSFRs-1 do
               if not sfr_patterns[x,y].Test(test_patterns[y].Text) then
                  success := false;
            group_match := sfr_patterns[x,0].Group1MatchValue;
            for y := 1 to NumberOfSFRs-1 do
               if group_match <> sfr_patterns[x,0].Group1MatchValue then
                  success := false;
            if success then
               varname_labels[x].Caption := format (VariableNameFormatStringEdit.Text, [group_match])
         end;
   end;


//=======================
//  Field Name Fixup Tab
//=======================

procedure TComboTypeDialog.AddAddFieldsButtonClick(Sender: TObject);
   var
      f: TVisibleAddField;
   begin
      f := TVisibleAddField.Create;
      f.top := 10;
      AddFields.Add(f);
      PositionAddFieldFrames;
      Validate
   end;

procedure TComboTypeDialog.AddFieldNameFixupButtonClick(Sender: TObject);
   var
      f: TVisibleFieldNameFixup;
   begin
      f := TVisibleFieldNameFixup.Create;
      f.top := 10;
      FieldNameFixups.Add(f);
      PositionFieldnameFixupFrames
   end;


//==============================
//  Combo Type Usage Report Tab
//==============================

type
  t_variable_appender =
      record
      private
         current_pic: string;
         node: TTreeNode;
         s: string;
         tree_view: TTreeView;
         pic_sfr_var_list: t_all_pics_var_sfr_list;
         treenode_pic_var_sfr_list: TPicVarSFRList;
      public
         procedure init (_tree_view: TTreeView; _node: TTreeNode; _pic_var_sfr_list: t_all_pics_var_sfr_list);
         procedure add (pic, variable: string);
         procedure finish;
      end;

procedure TPicVarSFRList.add_var_list (var_sfr_list: p_var_sfr_list);
   var i: integer;
   begin
      i := Length(var_list);
      SetLength(var_list, i+1);
      var_list[i] := var_sfr_list
   end;

procedure t_variable_appender.init (_tree_view: TTreeView; _node: TTreeNode; _pic_var_sfr_list: t_all_pics_var_sfr_list);
   begin
      tree_view := _tree_view;
      pic_sfr_var_list := _pic_var_sfr_list;
      node := _node;
      current_pic := '';
      s := ''
   end;

procedure t_variable_appender.add (pic, variable: string);
   begin
      if pic = current_pic then
         s := s + ' ' + variable
      else
         begin
            finish;
            treenode_pic_var_sfr_list := TPicVarSFRList.Create;
            treenode_pic_var_sfr_list.pic_name := pic;
            s := pic + ' ' + variable;
            current_pic := pic
         end;
      treenode_pic_var_sfr_list.add_var_list (pic_sfr_var_list.get_var_sfr_list (pic, variable))
   end;

procedure t_variable_appender.finish;
   begin
      if s <> '' then
         with tree_view.Items.AddChild (node, s) do
            data := treenode_pic_var_sfr_list
   end;

procedure TComboTypeDialog.FieldUsageReportTreeViewCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
   begin
      NodeClass := TTreeNodeWithDestructor
   end;

procedure TComboTypeDialog.FieldUsageReportTreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   var
      tree_node: TTreeNode;
      pic_var_sfr_list: TPicVarSFRList;
   begin
      if Button <> mbRight then
         exit;

      tree_node := FieldUsageReportTreeView.GetNodeAt(X, Y);
      if tree_node = nil then
         exit;

      pic_var_sfr_list := TPicVarSFRList(tree_node.Data);
      if pic_var_sfr_list = nil then
         exit;

      ChooseCOrAssemblyTypeListingForm.PicVarSFRList := pic_var_sfr_list;
      if ChooseCOrAssemblyTypeListingForm.ShowModal = mrCancel then
         exit;

      with TViewCDeclarationsForm.Create(Self) do
         begin
            PICName := pic_var_sfr_list.pic_name;
            Variable := ChooseCOrAssemblyTypeListingForm.Variable;
            Show
         end
   end;

procedure t_all_pics_var_sfr_list.init;
   begin
      SetLength(pics, 0);
   end;

procedure t_all_pics_var_sfr_list.start_new_pic (pic_name: string);
   begin
      current_pic_idx := Length (pics);
      SetLength (pics, current_pic_idx + 1);
      pics[current_pic_idx].pic_name := pic_name
   end;

procedure t_all_pics_var_sfr_list.start_new_pic_var (var_name: string);
   begin
      with pics[current_pic_idx] do
         begin
            current_var_idx := Length (vars);
            SetLength (vars, current_var_idx+1);
            vars[current_var_idx].var_name := var_name
         end
   end;

procedure t_all_pics_var_sfr_list.record_pic_var_sfr (sfr_name: string);
   var
      current_sfr_idx: integer;
   begin
      with pics[current_pic_idx].vars[current_var_idx] do
         begin
            current_sfr_idx := Length(sfr_names);
            SetLength (sfr_names, current_sfr_idx+1);
            sfr_names[current_sfr_idx] := sfr_name
         end
   end;

function t_all_pics_var_sfr_list.get_var_sfr_list (_pic_name, _var_name: string): p_var_sfr_list;
   var
      pic_idx, var_idx: integer;
   begin
      for pic_idx := 0 to Length(pics)-1 do
         with pics[pic_idx] do
            if pic_name = _pic_name then
               for var_idx := 0 to Length(vars)-1 do
                  if vars[var_idx].var_name = _var_name then
                     begin
                        result := @pics[pic_idx].vars[var_idx];
                        exit
                     end;
      result := nil;  // suppress warning
      assert (false)
   end;

procedure TComboTypeDialog.generate_combo_type_usage_report;

   function menu_item_caption (mi: TMenuItem): string;
      begin
         result := StringReplace (mi.Caption, '&', '', [rfReplaceAll])
      end;

   var
      group_1_match_value: string;
      pic_info: tMicroControllerSFRFieldInfo;
      addr_slot_idx: integer;
      var_name: string;
      field_list: TFieldList;
      all_vars: TComboVariableList;

   function sufficient_contiguous_slots: boolean;
      var
         i: integer;
      begin
         result := false;
         for i := 1 to NumberOfSFRs-1 do
            if pic_info.sfr_addr_slots[addr_slot_idx+i].addr <> pic_info.sfr_addr_slots[addr_slot_idx].addr + i then
               exit;
         result := true
      end;

   function sfr_patterns_match: boolean;
      var
         x: integer;

      function sfr_column_matches: boolean;
         var
            y: integer;

         function match_in_addr_slot: boolean;
            var i: integer;
            begin  // match_in_addr_slot
               result := true;
               for i := 0 to Length(pic_info.sfr_addr_slots[addr_slot_idx+y].sfrs)-1 do
                  if sfr_patterns[x,y].Match (String(pic_info.sfr_addr_slots[addr_slot_idx+y].sfrs[i].sfr_name)) then
                     if y = 0 then
                        begin
                           group_1_match_value := sfr_patterns[x,y].Group1MatchValue;
                           exit
                        end
                     else
                        if group_1_match_value = sfr_patterns[x,y].Group1MatchValue then
                           exit;
               result := false
            end;   // match_in_addr_slot

         begin  // sfr_column_matches
            result := false;
            for y := 0 to NumberOfSFRs-1 do
               if not match_in_addr_slot then
                  exit;
            result := true
         end;   // sfr_column_matches

      procedure record_fields;
         var
            y: integer;

         procedure record_sfr_fields (sfr: TSFRInfo);
            var
               fi: TFieldInfo;
               f: TField;
               fixed_field_name: string;
            begin  // record_sfr_fields
               for fi in sfr.fields do
                  begin
                     fixed_field_name := FieldNameFixups.FixFieldname (String(fi.field_name), group_1_match_value, GetNewTVisibleFieldNameFixupObject);
                     f := field_list.GetField(y, fixed_field_name, fi.bitno, fi.width);
                     f.RecordUsage (String(pic_info.microcontroller_name), var_name)
                  end;
            end;   // record_sfr_fields

         procedure record_fields_in_addr_slot (slot: tSFRAddressSlot);
            var
               i: integer;
            begin  // record_fields_in_addr_slot
               for i := 0 to Length(slot.sfrs)-1 do
                  if sfr_patterns[x,y].Match (String(slot.sfrs[i].sfr_name)) then
                     begin
                        record_sfr_fields (slot.sfrs[i]);
                        pic_var_sfr_list.record_pic_var_sfr (String(slot.sfrs[i].sfr_name));
                        exit
                     end
            end;   // record_fields_in_addr_slot

         begin  // record_fields
            for y := 0 to NumberOfSFRs-1 do
               record_fields_in_addr_slot(pic_info.sfr_addr_slots[addr_slot_idx+y])
         end;   // record_fields

      begin  // sfr_patterns_match
         result := true;
         for x := 0 to number_of_patterns-1 do
            if sfr_column_matches then
               begin
                  var_name := format (VariableNameFormatStringEdit.Text, [group_1_match_value]);
                  pic_var_sfr_list.start_new_pic_var (var_name);
                  record_fields;
                  exit
               end;
         result := false
      end;   // sfr_patterns_match

   var
      info: tAllPIC18xInfo;
      field: TField;
      current_offset: integer;
      field_node, discrepancy_field_node: TTreeNode;
      usage: TComboVariable;
      s: string;
      i: integer;
      variable_appender: t_variable_appender;
      uidx, aidx: integer;
      current_pic, variables_containing_field, variables_not_containing_field: string;
   begin  // generate_combo_type_usage_report
      ComboTypeUsageReportMemo.Clear;
      FieldUsageReportTreeView.Items.Clear;
      FieldDiscrepanciesTreeView.Items.Clear;

      try
         info := tAllPIC18xInfo.CreateFromBinaryDataFile
      except
         on e: eInvalidBinaryDateFile do
            begin
               ComboTypeUsageReportMemo.Lines.Add ('*** Invalid Binary Data File *** ' + e.Message);
               ComboTypeUsageReportMemo.Lines.Add ('');
               ComboTypeUsageReportMemo.Lines.Add ('Regenerate from Main Form menu using: ' + menu_item_caption(MainForm.ProcessTopMenuItem) + ' / ' + menu_item_caption(MainForm.ProcessAllPICFilesMenuItem));
               exit
            end
      end;

      field_list := TFieldList.Create;
      all_vars := TComboVariableList.Create;
      pic_var_sfr_list.init;
      for pic_info in info do
         begin
            pic_var_sfr_list.start_new_pic (string(pic_info.microcontroller_name));
            s := String(pic_info.microcontroller_name) + ':';
            for i := Length(s) to max_picname_len + 3 do
               s := s + ' ';
            for addr_slot_idx := 0 to pic_info.sfr_addr_slots.Count - NumberOfSFRs do
               if sufficient_contiguous_slots
                  and
                  sfr_patterns_match
               then
                  begin
                     s := s + ' ' + var_name;
                     all_vars.Add (TComboVariable.Create (String(pic_info.microcontroller_name), var_name))
                  end;
            ComboTypeUsageReportMemo.Lines.Add (s)
         end;
      field_list.Sort;
      all_vars.Sort;

      current_offset := -1;
      FieldUsageReportTreeView.Items.Add (nil, format ('List of PIC18s combo variables of type %s', [GetCurrentTypeName]));
      for field in field_list do
         if field.usage.Count < all_vars.Count then
            begin
               field.usage.Sort;
               if field.offset <> current_offset then
                   begin
                      current_offset := field.offset;
                      s := '------------------Offset ' + IntToStr(current_offset) + '------------------';
                      FieldUsageReportTreeView.Items.Add (nil, s);
                      FieldDiscrepanciesTreeView.Items.Add (nil, s)
                   end;

               field_node := FieldUsageReportTreeView.Items.Add (nil, format ('%s:%d:%d  %d/%d', [field.name, field.bitno, field.width, field.usage.Count, all_vars.Count]));

               // report combo variables containing the field
               variable_appender.init (FieldUsageReportTreeView,
                                       FieldUsageReportTreeView.Items.AddChild (field_node, 'PIC18x combo variables containing the field'),
                                       pic_var_sfr_list
                                      );
               for usage in field.usage do
                  variable_appender.add (usage.pic, usage.variable);
               variable_appender.finish;

               // report combo variables not containing the field
               variable_appender.init (FieldUsageReportTreeView,
                                       FieldUsageReportTreeView.Items.AddChild (field_node, 'PIC18x combo variables not containing the field'),
                                       pic_var_sfr_list
                                      );
               i := 0;
               for usage in field.usage do
                  begin
                     while (all_vars[i].pic <> usage.pic) and (all_vars[i].variable <> usage.variable)
                     do begin
                           variable_appender.add (all_vars[i].pic, all_vars[i].variable);
                           i := i + 1
                        end;
                     i := i + 1
                  end;
               while i < all_vars.Count do
                  begin
                     variable_appender.add (all_vars[i].pic, all_vars[i].variable);
                     i := i + 1
                  end;
               variable_appender.finish;

               // report PICs that have combo variables, some containing the field and some not
               discrepancy_field_node := nil;
               aidx := 0;
               uidx := 0;
               while uidx < field.usage.Count do
                  begin  // loop once per pic that uses this field
                     current_pic := field.usage[uidx].pic;
                     while all_vars[aidx].pic <> current_pic do
                        aidx := aidx+1;
                     variables_containing_field := '';
                     variables_not_containing_field := '';
                     repeat  // once per pic variable
                        if all_vars[aidx].variable <> field.usage[uidx].variable then
                           begin
                              variables_not_containing_field := variables_not_containing_field + ' ' + all_vars[aidx].variable;
                              aidx := aidx + 1
                           end
                        else
                           begin
                              variables_containing_field := variables_containing_field + ' ' + all_vars[aidx].variable;
                              aidx := aidx + 1;
                              uidx := uidx + 1
                           end
                     until (uidx = field.usage.Count)
                           or
                           (field.usage[uidx].pic <> current_pic);
                     if (variables_containing_field <> '') and (variables_not_containing_field <> '') then
                        begin
                           if discrepancy_field_node = nil then
                              discrepancy_field_node := FieldDiscrepanciesTreeView.Items.Add (nil, field_node.Text);
                           FieldDiscrepanciesTreeView.Items.AddChild (discrepancy_field_node, current_pic + ': field used in' + variables_containing_field + ' but not in' + variables_not_containing_field)
                        end
                  end
            end;

      field_list.Free;
      all_vars.Free;
      info.Free
   end;   // generate_combo_type_usage_report

function TComboTypeDialog.Regex_Left: integer;
   begin
      result := Label5.Left - FieldNameFixupsScrollbox.Left - 2
   end;

function TComboTypeDialog.Replacement_Left: integer;
   begin
      result := Label6.Left - FieldNameFixupsScrollbox.Left - 2
   end;

function TComboTypeDialog.Test_Result_Left: integer;
   begin
      result := Label9.Left - FieldNameFixupsScrollbox.Left - 2
   end;

procedure TComboTypeDialog.ExitDialog(Sender: TObject);
   begin
      set_number_of_patterns(1);
      set_number_of_sfrs(1)
   end;

END.
