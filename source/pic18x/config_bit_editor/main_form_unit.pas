UNIT main_form_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   Classes,
   SysUtils,
   Comctrls,
   Dialogs,
   Forms,
   StdCtrls,
   Menus,
   Controls,
   LibXmlParser;

type
   TRadioButtonWithValue =
      class (TRadioButton)
      public
         value: integer;
      end;

type
   TMainForm =
      class(TForm)
         PageControl: TPageControl;
         Main: TTabSheet;
         Memo: TMemo;
         SaveDialog: TSaveDialog;
         OpenDialog: TOpenDialog;
         MainMenu: TMainMenu;
         FileMainMenu: TMenuItem;
         FileNewMenuItem: TMenuItem;
         FileOpenMenuItem: TMenuItem;
         FileSaveAsMenuItem: TMenuItem;
         FileSaveMenuItem: TMenuItem;
         FileMenuSeparator: TMenuItem;
         ExitMenuItem: TMenuItem;
         AboutMainMenuItem: TMenuItem;
         procedure UpdateConfigConstantDisplay(Sender: TObject);
         procedure FileOpenMenuItemClick(Sender: TObject);
         procedure FileSaveAsMenuItemClick(Sender: TObject);
         procedure FileNewMenuItemClick(Sender: TObject);
         procedure FileSaveMenuItemClick(Sender: TObject);
         procedure ExitMenuItemClick(Sender: TObject);
         procedure AboutMainMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      private
         pic: string;
         include_file_name: string;
         dirty: boolean;
         tabsheet: TTabSheet;
         groupbox: TGroupBox;
         y: integer;
         field_idx: integer;
         scrollbox: TScrollBox;
         radiobutton: TRadioButtonWithValue;
         radiobutton_count: integer;
         byte_idx: integer;
         config_field_name: string;
         config_field_width: integer;
         config_field_default_value: integer;
         config_field_ishidden: boolean;
         config_field_nop_marker: boolean;
         config_field_description: string;
         function show_config_field_in_dialog: boolean;
         function list_config_field_in_include_file: boolean;
         procedure load_pic_configbits_info (pic: string);
         procedure XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);
         procedure XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
         procedure set_caption;
      end;

var
  MainForm: TMainForm;

IMPLEMENTATION

uses
   Graphics, regular_expression_unit,
   pic18x_common_unit, pic18x_selection_dialog_unit, LibXmlComps,
   cpc_main_compiler_unit, cpc_definitions_unit, cpc_blocks_unit,
   cpc_statements_unit, cpc_core_objects_unit, filectrl, about_box_unit;

{$R *.dfm}

const
   std_caption = 'PIC18x Configuration Bits Editor';

var
   ControlNo: integer;
   bytes:
      array of
         record
            bytename: string;
            fields:
               array of
                  record
                     fieldname: string;
                     fielddsc: string;
                     nop_field: boolean;
                     default_value: integer;
                     values:
                        array of
                           record
                              fieldvaluecname: string;
                              fieldvaluedesc: string;
                              fieldvalue: integer;
                              radiobutton: TRadioButton
                           end
                  end
         end;
   loading_file: boolean;
   xml_file_directory: string;

function IntValue (s: string): integer;
   var i: integer;
   begin
      result := 0;
      for i := 1 to Length(s) do
         begin
            assert (s[i] in ['0'..'9']);
            result := (result * 10) + ord(s[i]) - ord('0')
         end
   end;

function BoolValue (s: string): boolean;
   begin
      if Lowercase(s) = 'true' then
         result := true
      else if LowerCase(s) = 'false' then
         result := false
      else
         assert (false)
   end;

function ProgramGenerator: TDefinition;
   begin
      result := TProgram.CreateFromSourceTokens
   end;

function TMainForm.show_config_field_in_dialog: boolean;
   begin
      result := (not config_field_ishidden)
                and
                (not config_field_nop_marker)
   end;

function TMainForm.list_config_field_in_include_file: boolean;
   begin
      if config_field_name = '-' then
         result := false
      else
         result := (not config_field_ishidden)
                   or
                   (config_field_nop_marker)
   end;

procedure TMainForm.XmlScannerStartTag(Sender: TObject; TagName: String; Attributes: TAttrList);
   var
      config_byte_name: string;
   begin
      if TagName = 'ConfigByte' then
         begin
            config_byte_name := Attributes.Value('name');
            if (tabsheet = nil)
               or
               (tabsheet.Caption <> config_byte_name)
            then
               begin
                  tabsheet := TTabSheet.Create(Self);
                  tabsheet.Name := 'x' + IntToStr (ControlNo);
                  ControlNo := ControlNo + 1;
                  tabsheet.Caption := config_byte_name;
                  tabsheet.PageControl := PageControl;
                  scrollbox := TScrollBox.Create(Self);
                  with scrollbox do
                     begin
                        Parent := tabsheet;
                        Name := 'x' + IntToStr (ControlNo);
                        ControlNo := ControlNo + 1;
                        Align := alClient;
                        AutoScroll := true
                     end;
                  groupbox := nil;
                  y := 16;
                  byte_idx := Length(bytes);
                  SetLength (bytes, byte_idx+1);
                  bytes[byte_idx].bytename := config_byte_name;
                  field_idx := -1
               end
         end
      else if TagName = 'ConfigField' then
         begin
            config_field_name := Attributes.Value('name');
            config_field_description := Attributes.Value('desc');
            config_field_width := IntValue(Attributes.Value('width'));
            config_field_default_value := IntValue(Attributes.Value('default'));
            config_field_ishidden := BoolValue(Attributes.Value('ishidden'));
            config_field_nop_marker := BoolValue(Attributes.Value('nop_marker'));

            if show_config_field_in_dialog then
               begin
                  groupbox := TGroupBox.Create(Self);
                  with groupbox do
                     begin
                        Parent := scrollbox;
                        Name := 'x' + IntToStr (ControlNo);
                        ControlNo := ControlNo + 1;
                        Left := 16;
                        Top := y;
                        Width := scrollbox.Width - 32;
                        Height := 33;
                        Anchors := [akTop, akLeft, akRight];
                        Caption := config_field_name + ' - ' + config_field_description;
                     end;
                  radiobutton_count := 0
               end;

            if list_config_field_in_include_file then
               begin
                  field_idx := Length(bytes[byte_idx].fields);
                  SetLength(bytes[byte_idx].fields, field_idx+1);
                  bytes[byte_idx].fields[field_idx].fieldname := config_field_name;
                  bytes[byte_idx].fields[field_idx].fielddsc := config_field_description;
                  bytes[byte_idx].fields[field_idx].nop_field := config_field_nop_marker;
                  bytes[byte_idx].fields[field_idx].default_value := config_field_default_value
               end
         end
   end;

procedure TMainForm.XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
   var
      value_idx: integer;
      config_field_value_name: string;
      config_field_value_desc: string;
      config_field_value_value: integer;
   begin
      if TagName = 'ConfigField' then
         begin
            config_field_name := Attributes.Value('name');
            config_field_description := Attributes.Value('desc');
            config_field_width := IntValue(Attributes.Value('width'));
            config_field_default_value := IntValue(Attributes.Value('default'));
            config_field_ishidden := BoolValue(Attributes.Value('ishidden'));
            config_field_nop_marker := BoolValue(Attributes.Value('nop_marker'));
            if list_config_field_in_include_file then
               begin
                  field_idx := Length(bytes[byte_idx].fields);
                  SetLength(bytes[byte_idx].fields, field_idx+1);
                  bytes[byte_idx].fields[field_idx].fieldname := config_field_name;
                  bytes[byte_idx].fields[field_idx].fielddsc := config_field_description;
                  bytes[byte_idx].fields[field_idx].nop_field := config_field_nop_marker;
                  bytes[byte_idx].fields[field_idx].default_value := config_field_default_value
               end
         end
      else if TagName = 'ConfigFieldValue' then
         begin
            config_field_value_name := Attributes.Value('name');
            config_field_value_desc := Attributes.Value('desc');
            config_field_value_value := IntValue(Attributes.Value('value'));

            if show_config_field_in_dialog then
               begin
                  radiobutton := TRadioButtonWithValue.Create(Self);
                  radiobutton.Value := config_field_value_value;
                  with radiobutton do
                     begin
                        Parent := groupbox;
                        Name := 'x' + IntToStr (ControlNo);
                        ControlNo := ControlNo + 1;
                        Left := 16;
{$IFDEF FPC}
                        Top := 4 + (radiobutton_count * 20);
{$ELSE}
                        Top := 20 + (radiobutton_count * 20);
{$ENDIF}
                        radiobutton_count := radiobutton_count + 1;
                        Width := groupbox.Width - 32;
                        Height := 17;
                        Anchors := [akTop, akLeft, akRight];
                        OnClick := UpdateConfigConstantDisplay;
                        Caption := config_field_value_name + ' | ' + config_field_value_desc;
                        if config_field_name = 'XINST' then
                           if config_field_value_value = 1 then
                              begin
                                 Caption := Caption + ' (required for Concurrent Pascal)';
                                 Font.Style := Font.Style + [fsBold];
                                 Checked := true
                              end
                           else
                              Enabled := false
                        else
                           if config_field_default_value = config_field_value_value then
                              begin
                                 Font.Style := Font.Style + [fsBold];
                                 Checked := true
                              end
                     end;

                    groupbox.height := 30 + (radiobutton_count * 20);
                    y := groupbox.Top + groupbox.Height + 16
               end;

            if list_config_field_in_include_file then
               begin
                  value_idx := Length(bytes[byte_idx].fields[field_idx].values);
                  SetLength (bytes[byte_idx].fields[field_idx].values, value_idx+1);
                  bytes[byte_idx].fields[field_idx].values[value_idx].fieldvaluecname := config_field_value_name;
                  bytes[byte_idx].fields[field_idx].values[value_idx].fieldvaluedesc := config_field_value_desc;
                  bytes[byte_idx].fields[field_idx].values[value_idx].fieldvalue := config_field_value_value;
                  if show_config_field_in_dialog then
                     bytes[byte_idx].fields[field_idx].values[value_idx].radiobutton := radiobutton
                  else
                     bytes[byte_idx].fields[field_idx].values[value_idx].radiobutton := nil
               end
        end
   end;

procedure TMainForm.set_caption;
   begin
      Caption := std_caption + ' - ' + ExtractFileName (include_file_name);
      if dirty then
         Caption := Caption + '*'
   end;

procedure TMainForm.UpdateConfigConstantDisplay(Sender: TObject);

   procedure out (s: string);
      begin
         Memo.Lines.Add (s)
      end;

   var
      s: string;
      byte_idx, field_idx, value_idx: integer;
   begin
      if loading_file
      then exit;

      Memo.Clear;
      out ('// ------------------------------------------------------------------');
      out ('//  DO NOT EDIT!  This file is generated by pic18x_config_bit_editor');
      out ('// ------------------------------------------------------------------');
      out ('');
      out ('const');
      out ('   ' + config_bits_constant_name(pic) + ': ' + config_bits_constant_type_name(pic) + ' =');

      for byte_idx := 0 to Length(bytes)-1 do
         begin
            if byte_idx = 0 then
               out ('      (' + bytes[byte_idx].bytename + ' =')
            else
               out ('       ' + bytes[byte_idx].bytename + ' =');
            for field_idx := Length(bytes[byte_idx].fields)-1 downto 0 do
               begin
                  if field_idx = Length(bytes[byte_idx].fields)-1 then
                     s := '          ('
                  else
                     s := '           ';
                  if bytes[byte_idx].fields[field_idx].nop_field then
                     begin
                        assert (bytes[byte_idx].fields[field_idx].default_value = $F);
                        s := s + 'nop = ' + bytes[byte_idx].bytename + '_nop';
                        if field_idx > 0 then
                           s := s + ',';
                        out (s);
                        out ('              // ensures that this FCW will execute as a NOP in the remote');
                        out ('              //    event that this location is ever executed by accident.');
                     end
                  else
                     begin
                        s := s + bytes[byte_idx].fields[field_idx].fieldname + ' = ';
                        for value_idx := 0 to Length(bytes[byte_idx].fields[field_idx].values)-1 do
                           if bytes[byte_idx].fields[field_idx].values[value_idx].radiobutton.Checked then
                              s := s + config_bits_field_enum_name(bytes[byte_idx].bytename, bytes[byte_idx].fields[field_idx].fieldname, bytes[byte_idx].fields[field_idx].values[value_idx].fieldvaluecname);
                        if field_idx > 0 then
                           s := s + ',';
                        out (s);

                        out ('              // ' + bytes[byte_idx].fields[field_idx].fielddsc + ':');

                        for value_idx := 0 to Length(bytes[byte_idx].fields[field_idx].values)-1 do
                           if bytes[byte_idx].fields[field_idx].values[value_idx].radiobutton.Checked then
                              begin
                                 s := '              //    ' + bytes[byte_idx].fields[field_idx].values[value_idx].fieldvaluecname + ' = ' + bytes[byte_idx].fields[field_idx].values[value_idx].fieldvaluedesc;
                                 out (s)
                              end;

                        if field_idx = 0 then
                           begin
                              s := '          )';
                              if byte_idx < Length(bytes)-1 then
                                 s := s + ',';
                              out (s)
                           end
                     end
               end;
            if byte_idx = Length(bytes)-1 then
               out ('      );')
         end;
      dirty := true;
      set_caption
   end;

procedure TMainForm.load_pic_configbits_info (pic: string);
   var
      xmlscanner:  TXmlScanner;
   begin
      SetLength (bytes, 0);
      byte_idx := -1;
      field_idx := -1;
      scrollbox := nil;
      groupbox := nil;
      tabsheet := nil;
      radiobutton_count := -1;
      y := 0;

      loading_file := true;
      while PageControl.PageCount > 1 do
         PageControl.Pages[1].Free;
      xmlscanner := TXmlScanner.Create (self);
      xmlscanner.OnStartTag := XmlScannerStartTag;
      xmlscanner.OnEmptyTag := XmlScannerEmptyTag;
      XmlScanner.Filename := xml_file_directory + pic + '.xml';
      XmlScanner.Execute;
      xmlscanner.free;
      loading_file := false
   end;

procedure TMainForm.FileNewMenuItemClick(Sender: TObject);
   begin
      if Pic18xSelectionDialog.Execute (xml_file_directory, '.xml')  then
         begin
            pic := Pic18xSelectionDialog.SelectedPIC;
            load_pic_configbits_info (Pic18xSelectionDialog.SelectedPIC);
            UpdateConfigConstantDisplay (nil);
            dirty := false;
            include_file_name := config_bits_constant_name(pic) + '.inc';   // a suggested name...
            set_caption;
            FileSaveAsMenuItem.Enabled := true
         end
   end;

procedure TMainForm.FileOpenMenuItemClick(Sender: TObject);
   var
      src, results: TStringList;
      compilation: TCompilation;
      i, j, k: integer;
      tabsheet: TTabSheet;
      scrollbox: TScrollBox;
      field_name: string;
      stmt_idx: integer;
      prog: TProgram;
      stmts: TStatement;
      stmt_list: TStatementList;
      assignment_stmt: TAssignmentStatement;
      const_expr_value: integer;
      f: TextFile;
      s: string;
      regex: t_regular_expression;
   begin
      if OpenDialog.Execute then
         begin
            // quick check - this file should contain a pic config bit constant generated by this program
            AssignFile (f, OpenDialog.FileName);
            Reset (f);
            pic := '';
            regex := t_regular_expression.Create (' ([^ _]+)_configuration_bits: ');
            while not eof (f) do
               begin
                  readln (f, s);
                  if regex.Matches(s) then
                     pic := UpperCase(regex.Match(1))
               end;
            regex.Free;
            regex := nil;
            CloseFile(f);
            if pic = '' then
               begin
                  ShowMessage ('Invalid config bits include file');
                  exit
               end;
            if not FileExists (xml_file_directory + pic + '.xml') then
               begin
                  ShowMessage (pic + ' is unsupported');
                  exit
               end;

            include_file_name := OpenDialog.FileName;
            FileSaveMenuItem.Enabled := true;
            FileSaveAsMenuItem.Enabled := true;

            load_pic_configbits_info (pic);

            src := TStringList.Create;
            results := TStringList.Create;
            src.Add ('{$processor ''' + pic + '''}');
            src.Add ('{$include ''' + include_file_name + '''}');
            src.Add ('var i: uint8;');
            src.Add ('begin');
            // the following nested loop processing order must be same as similar loop below
            for i := 1 to PageControl.PageCount-1 do
               begin
                  tabsheet := PageControl.Pages[i];
                  assert ((tabsheet.ControlCount = 1) and (tabsheet.Controls[0] is TScrollBox));
                  scrollbox := TScrollBox(tabsheet.Controls[0]);
                  for j := 0 to scrollbox.ControlCount-1 do
                     begin
                        assert (scrollbox.Controls[j] is TGroupBox);
                        groupbox := TGroupBox(scrollbox.Controls[j]);
                        field_name := groupbox.Caption;
                        assert (Pos(' - ', field_name) > 0);
                        field_name := Copy (field_name, 1, Pos(' - ',field_name)-1);
                        src.Add (format ('   i := ord(%s_configuration_bits.%s.%s);', [pic, tabsheet.Caption, field_name]))
                     end;
               end;
            src.Add ('end.');

            compilation := TCompilation.CreateFromStrings (src, ProgramGenerator, results);
            assert (compilation.compiled_object.definition_kind = program_definition);
            prog := TProgram(compilation.compiled_object);
            assert (prog.initial_statement.definition_kind = statement_definition);
            stmts := TStatement(prog.initial_statement);
            assert (stmts.statement_kind = statement_list);
            stmt_list := TStatementList(stmts);

            assert (stmt_list[0].statement_kind = assignment_statement);
            assignment_stmt := TAssignmentStatement(stmt_list[0]);
            assert (assignment_stmt.expression.contains_integer_constant);
            const_expr_value := assignment_stmt.expression.ordinal_constant_value;

            stmt_idx := 0;
            // the following nested loop processing order must be same as similar loop above
            for i := 1 to PageControl.PageCount-1 do
               begin
                  tabsheet := PageControl.Pages[i];
                  assert ((tabsheet.ControlCount = 1) and (tabsheet.Controls[0] is TScrollBox));
                  scrollbox := TScrollBox(tabsheet.Controls[0]);
                  for j := 0 to scrollbox.ControlCount-1 do
                     begin
                        assert (scrollbox.Controls[j] is TGroupBox);
                        groupbox := TGroupBox (scrollbox.Controls[j]);

                        assert (stmt_list[stmt_idx].statement_kind = assignment_statement);
                        assignment_stmt := TAssignmentStatement(stmt_list[stmt_idx]);
                        assert (assignment_stmt.expression.contains_integer_constant);
                        const_expr_value := assignment_stmt.expression.ordinal_constant_value;

                        for k := 0 to groupbox.ControlCount-1 do
                           begin
                              assert (groupbox.Controls[k] is TRadioButtonWithValue);
                              radiobutton := TRadioButtonWithValue(groupbox.Controls[k]);
                              if radiobutton.value = const_expr_value then
                                 radiobutton.Checked := true
                           end;

                        stmt_idx := stmt_idx + 1
                     end;
               end;

            compilation.Free;
            results.Free;
            src.Free;

            dirty := false;
            set_caption
         end
   end;

procedure TMainForm.FileSaveMenuItemClick(Sender: TObject);
   begin
      Memo.Lines.SaveToFile (include_file_name);
      dirty := false;
      set_caption
   end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
   begin
      if dirty then
         if MessageDlg ('File has been changed - discard changes and close?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
            CanClose := false
   end;

procedure TMainForm.FileSaveAsMenuItemClick(Sender: TObject);
   begin
      SaveDialog.FileName := include_file_name;
      if SaveDialog.Execute then
         begin
            include_file_name := SaveDialog.FileName;
            FileSaveMenuItem.Enabled := true;
            Memo.Lines.SaveToFile (include_file_name);
            dirty := false;
            set_caption
         end
   end;

procedure TMainForm.AboutMainMenuItemClick(Sender: TObject);
   begin
      AboutBox.ShowModal
   end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
   begin
      if not dirty then
         Close
      else if MessageDlg ('Include File Has Been Altered, Exit Anyway?', mtConfirmation, [mbOK, mbCancel], 0) = mrOk then
         Close
   end;

INITIALIZATION
   xml_file_directory := ExtractFilePath(ParamStr(0)) + 'pic18x' + PathDelim + 'processor_definition_files' + PathDelim;

END.
