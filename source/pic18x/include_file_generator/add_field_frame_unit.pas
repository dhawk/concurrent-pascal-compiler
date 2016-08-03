UNIT add_field_frame_unit;

INTERFACE

uses
   Windows, Forms, Spin,
   StdCtrls, ExtCtrls, combo_type_unit, Controls, Classes;

type
   TAddFieldFrame =
      class(TFrame)
         Panel1: TPanel;
         FieldNameEdit: TEdit;
         DeleteButton: TButton;
         BitnoSpinEdit: TSpinEdit;
         WidthSpinEdit: TSpinEdit;
         procedure DeleteButtonClick(Sender: TObject);
         procedure ValidateOnChange(Sender: TObject);
      private
         { Private declarations }
      public
         delete: boolean;
         function Valid: boolean;
      end;
   TVisibleAddField =
      class (TAddField)
      private
         procedure set_Top (i: integer);
         function get_Top: integer;
      protected
         procedure set_FieldName (s: string);
            override;
         procedure set_Bitno (b: integer);
            override;
         procedure set_Width (w: integer);
            override;
         function get_FieldName: string;
            override;
         function get_Bitno: integer;
            override;
         function get_Width: integer;
            override;
      public
         add_field_frame: TAddFieldFrame;
         property Top: integer read get_Top write set_Top;
         constructor Create;
         constructor CreateCopy (orig: TAddField);
            override;
         destructor Destroy;
            override;
      end;

IMPLEMENTATION

uses
   combo_type_dialog_unit, Math, SysUtils, Graphics;

{$R *.dfm}

procedure TAddFieldFrame.ValidateOnChange(Sender: TObject);
   begin
      ComboTypeDialog.Validate
   end;

procedure TAddFieldFrame.DeleteButtonClick(Sender: TObject);
   var
      i: integer;
   begin
      for i := 0 to ComboTypeDialog.FieldNameFixups.Count-1 do
         if TVisibleAddField(ComboTypeDialog.AddFields[i]).Top = Top then
             begin
                ComboTypeDialog.AddFields.Delete(i);
                ComboTypeDialog.PositionAddFieldFrames;
                ComboTypeDialog.Validate;
                exit
             end
   end;

function TAddFieldFrame.Valid: boolean;
   var
      ct_width: integer;  // in bits
   begin
      ct_width := ComboTypeDialog.NumberOfSFRs * 8;
      result := true;

      if FieldNameEdit.Text = '' then
         begin
            FieldNameEdit.Color := clRed;
            FieldNameEdit.Hint := 'Can''t be blank';
            result := false
         end
      else
         begin
            FieldNameEdit.Color := clWhite;
            FieldNameEdit.Hint := ''
         end;

      if BitNoSpinEdit.Value >= ct_width then
         begin
            BitNoSpinEdit.Color := clRed;
            BitNoSpinEdit.Hint := format ('BitNo must be 0..%d', [ct_width-1]);
            result := false
         end
      else
         begin
            BitNoSpinEdit.Color := clWhite;
            BitNoSpinEdit.Hint := ''
         end;

      if (WidthSpinEdit.Value > ct_width)
         or
         (WidthSpinEdit.Value > BitNoSpinEdit.Value+1)
      then
         begin
            WidthSpinEdit.Color := clRed;
            WidthSpinEdit.Hint := format ('Width must be 1..%d', [min(ct_width, BitNoSpinEdit.Value+1)]);
            result := false
         end
      else
         begin
            WidthSpinEdit.Color := clWhite;
            WidthSpinEdit.Hint := ''
         end
   end;

procedure TVisibleAddField.set_Top (i: integer);
   begin
      add_field_frame.Top := i
   end;

function TVisibleAddField.get_Top: integer;
   begin
      result := add_field_frame.Top
   end;

constructor TVisibleAddField.Create;
   begin
      add_field_frame := TAddFieldFrame.Create (nil);
      add_field_frame.Parent := ComboTypeDialog.AddFieldsScrollBox;
      add_field_frame.left := 10;
      add_field_frame.top := 10
   end;

procedure TVisibleAddField.set_FieldName (s: string);
   begin
      add_field_frame.FieldNameEdit.Text := s
   end;

procedure TVisibleAddField.set_Bitno (b: integer);
   begin
      add_field_frame.BitnoSpinEdit.Value := b
   end;

procedure TVisibleAddField.set_Width (w: integer);
   begin
      add_field_frame.WidthSpinEdit.Value := w
   end;

function TVisibleAddField.get_FieldName: string;
   begin
      result := add_field_frame.FieldNameEdit.Text
   end;

function TVisibleAddField.get_Bitno: integer;
   begin
      result := add_field_frame.BitnoSpinEdit.Value
   end;

function TVisibleAddField.get_Width: integer;
   begin
      result := add_field_frame.WidthSpinEdit.Value
   end;

constructor TVisibleAddField.CreateCopy (orig: TAddField);
   begin
      FieldName := orig.FieldName;
      Bitno := orig.Bitno;
      Width := orig.Width
   end;

destructor TVisibleAddField.Destroy;
   begin
      add_field_frame.Free
   end;


END.
