UNIT fieldname_fixup_frame_unit;

INTERFACE

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
   Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
   System.RegularExpressions, Vcl.ExtCtrls, combo_type_unit;

type
   TFieldnameFixupFrame =
      class(TFrame)
         Panel1: TPanel;
         RegexPatternEdit: TEdit;
         ReplacementFormatStringEdit: TEdit;
         Label1: TLabel;
         DeleteButton: TButton;
         procedure DeleteButtonClick(Sender: TObject);
         procedure RegexPatternEditChange(Sender: TObject);
      public
         regex: TRegEx;
         procedure position_elements (offset: integer);
         function Valid: boolean;
         procedure Test (s: string);
            virtual;
      end;

   TVisibleFieldNameFixup =
      class(TFieldNameFixup)
      private
         procedure set_Top (i: integer);
         function get_Top: integer;
      protected
         function get_regex_pattern: string;
            override;
         procedure set_regex_pattern (pattern: string);
            override;
         function get_replacement_format_string: string;
            override;
         procedure set_replacement_format_string (fmt: string);
            override;
      public
         field_name_fixup_frame: TFieldnameFixupFrame;
         property Top: integer read get_Top write set_Top;
         constructor Create;
         constructor CreateCopy (orig: TFieldNameFixup);
            override;
         destructor Destroy;
            override;
      end;

function GetNewTVisibleFieldNameFixupObject (regex, fmt: string): TFieldNameFixup;
function GetNewTAddFieldObject (_fieldname: string; _bitno, _width: integer): TAddField;

IMPLEMENTATION

uses
   combo_type_dialog_unit, add_field_frame_unit;

{$R *.dfm}


//=======================
//  TFieldnameFixupFrame
//=======================

procedure TFieldnameFixupFrame.DeleteButtonClick(Sender: TObject);
   var
      i: integer;
   begin
      for i := 0 to ComboTypeDialog.FieldNameFixups.Count-1 do
         if TVisibleFieldNameFixup(ComboTypeDialog.FieldNameFixups[i]).Top = Top then
             begin
                ComboTypeDialog.FieldNameFixups.Delete(i);
                ComboTypeDialog.PositionFieldnameFixupFrames;
                exit
             end
   end;

procedure TFieldnameFixupFrame.position_elements (offset: integer);
   begin
      RegExPatternEdit.Left := offset + ComboTypeDialog.Regex_Left - Left - Panel1.Left - 3;
      ReplacementFormatStringEdit.Left := offset + ComboTypeDialog.Replacement_Left - Left - Panel1.Left - 3;
      Label1.Left := offset + ComboTypeDialog.Test_Result_Left - Left - Panel1.Left;
      RegExPatternEdit.Width := ReplacementFormatStringEdit.Left - RegExPatternEdit.Left - 16;
      ReplacementFormatStringEdit.Width := Label1.Left - ReplacementFormatStringEdit.Left - 16;
      DeleteButton.Left := Label1.Left + Label1.Width + 16;
      Panel1.Width := DeleteButton.Left + DeleteButton.Width + 16;
      Width := Panel1.Width
   end;

procedure TFieldnameFixupFrame.RegexPatternEditChange(Sender: TObject);
   begin
      regex := TRegEx.Create (RegexPatternEdit.Text);
      ComboTypeDialog.Validate
   end;

procedure TFieldnameFixupFrame.Test (s: string);
   var
      m: TMatch;
   begin
      m := regex.Match (s);
      if m.Success then
         begin
            RegexPatternEdit.Color := clGreen;
            if Pos('%s', ReplacementFormatStringEdit.Text) > 0 then
               Label1.Caption := format (ReplacementFormatStringEdit.Text, [m.Groups[1].Value])
            else
               Label1.Caption := ReplacementFormatStringEdit.Text
         end
      else
         begin
            RegexPatternEdit.Color := clWhite;
            Label1.Caption := ''
         end
   end;

function TFieldnameFixupFrame.Valid: boolean;
   begin
      if (Length(RegexPatternEdit.Text) = 0)
         or
         (RegexPatternEdit.Text[1] <> '^')
      then
         RegexPatternEdit.Hint := 'Regex must start with ^'
      else if RegexPatternEdit.Text[Length(RegexPatternEdit.Text)] <> '$' then
         RegexPatternEdit.Hint := 'Regex must end with $'
      else
         RegexPatternEdit.Hint := '';
      result := RegexPatternEdit.Hint = '';
      if result then
         RegexPatternEdit.Color := clWhite
      else
         RegexPatternEdit.Color := clRed;
      Label1.Caption := ''
   end;


//=========================
//  TVisibleFieldNameFixup
//=========================

function TVisibleFieldNameFixup.get_regex_pattern: string;
   begin
      result := field_name_fixup_frame.RegexPatternEdit.Text
   end;

procedure TVisibleFieldNameFixup.set_regex_pattern (pattern: string);
   begin
      field_name_fixup_frame.RegexPatternEdit.Text := pattern;
      inherited
   end;

function TVisibleFieldNameFixup.get_replacement_format_string: string;
   begin
      result := field_name_fixup_frame.ReplacementFormatStringEdit.Text
   end;

procedure TVisibleFieldNameFixup.set_replacement_format_string (fmt: string);
   begin
      field_name_fixup_frame.ReplacementFormatStringEdit.Text := fmt
   end;

procedure TVisibleFieldNameFixup.set_Top (i: integer);
   begin
      field_name_fixup_frame.Top := i
   end;

function TVisibleFieldNameFixup.get_Top: integer;
   begin
      result := field_name_fixup_frame.Top
   end;

constructor TVisibleFieldNameFixup.Create;
   begin
      field_name_fixup_frame := TFieldnameFixupFrame.Create (nil);
      field_name_fixup_frame.Parent := ComboTypeDialog.FieldNameFixupsScrollBox;
      field_name_fixup_frame.left := 10;
      field_name_fixup_frame.top := 10;
      field_name_fixup_frame.position_elements (0);
      field_name_fixup_frame.RegexPatternEdit.Text := '';
      field_name_fixup_frame.ReplacementFormatStringEdit.Text := '';
      field_name_fixup_frame.Label1.Caption := ''
   end;

constructor TVisibleFieldNameFixup.CreateCopy (orig: TFieldNameFixup);
   begin
      Create;
      inherited
   end;

destructor TVisibleFieldNameFixup.Destroy;
   begin
      field_name_fixup_frame.Free;
      inherited
   end;

function GetNewTVisibleFieldNameFixupObject (regex, fmt: string): TFieldNameFixup;
   begin
      result := TVisibleFieldNameFixup.Create;
      result.RegexPattern := regex;
      result.ReplacementFormatString := fmt
   end;

function GetNewTAddFieldObject (_fieldname: string; _bitno, _width: integer): TAddField;
   begin
      result := TVisibleAddField.Create;
      result.FieldName := _fieldname;
      result.Bitno := _bitno;
      result.Width := _width
   end;

END.
