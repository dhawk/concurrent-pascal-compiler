UNIT combo_type_sfr_pattern_frame_unit;

INTERFACE

uses
  Windows, Forms, StdCtrls, RegularExpressions, Classes, Controls;

type
   t_proc_of_object = procedure of object;
   TComboTypeSFRPatternFrame =
      class( TFrame)
         Edit: TEdit;
         Group1MatchValueLabel: TLabel;
         procedure EditChange(Sender: TObject);
      private
         procedure set_pattern (s: string);
         function get_pattern: string;
      public
         regex: TRegEx;
         Validate: t_proc_of_object;
         property pattern: string read get_pattern write set_pattern;
         function Valid: boolean;
         function Test (s: string): boolean;
         function Match (s: string): boolean;
         function Group1MatchValue: string;
      end;

IMPLEMENTATION

uses
   Graphics;

{$R *.dfm}

procedure TComboTypeSFRPatternFrame.set_pattern (s: string);
   begin
      Edit.Text := s  // EditChange will set regex
   end;

function TComboTypeSFRPatternFrame.Valid: boolean;
   begin
      if (Length(Edit.Text) = 0)
         or
         (Edit.Text[1] <> '^')
      then
         Edit.Hint := 'Regex must start with ^'
      else if Edit.Text[Length(Edit.Text)] <> '$' then
         Edit.Hint := 'Regex must end with $'
      else
         Edit.Hint := '';
      result := Edit.Hint = '';
      if result then
         Edit.Color := clWhite
      else
         Edit.Color := clRed;
      Group1MatchValueLabel.Caption := ''
   end;

procedure TComboTypeSFRPatternFrame.EditChange(Sender: TObject);
   begin
      regex := TRegEx.Create (Edit.Text);
      Validate
   end;

function TComboTypeSFRPatternFrame.get_pattern: string;
   begin
      result := Edit.Text
   end;

function TComboTypeSFRPatternFrame.Test (s: string): boolean;
   var
      m: TMatch;
   begin
      m := Regex.Match(s);
      result := m.Success;
      if result then
         begin
            Edit.Color := clGreen;
            if m.Groups.Count = 2 then
               Group1MatchValueLabel.Caption := m.Groups[1].Value
            else
               Group1MatchValueLabel.Caption := ''
         end
      else
         Edit.Color := clRed
   end;

function TComboTypeSFRPatternFrame.Match (s: string): boolean;
   var
      m: TMatch;
   begin
      m := Regex.Match(s);
      result := m.Success
   end;

function TComboTypeSFRPatternFrame.Group1MatchValue: string;
   begin
      result := Group1MatchValueLabel.Caption
   end;

END.
