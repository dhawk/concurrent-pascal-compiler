UNIT choose_c_variable_listing_unit;

INTERFACE

uses
  Windows, Forms, StdCtrls, combo_type_dialog_unit, Controls, Classes;

type
   TChooseCOrAssemblyTypeListingForm =
      class(TForm)
         CancelButton: TButton;
         Label1: TLabel;
         Button1: TButton;
         procedure FormActivate(Sender: TObject);
         procedure FormCreate(Sender: TObject);
         procedure ButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
      private
         created_buttons: array of TButton;
         first_button_top: integer;
         cancel_button_top: integer;
         procedure free_created_buttons;
      public  // dialog paramter - set before ShowModal
         PicVarSFRList: TPicVarSFRList;
      public  // dialog result - avail after ShowModal
         Variable: p_var_sfr_list;
      end;

var
   ChooseCOrAssemblyTypeListingForm: TChooseCOrAssemblyTypeListingForm;

IMPLEMENTATION

{$R *.dfm}

procedure TChooseCOrAssemblyTypeListingForm.ButtonClick(Sender: TObject);
   var i: integer;
   begin
      for i := 0 to Length(PicVarSFRList.var_list)-1 do
         if PicVarSFRList.var_list[i]^.var_name = TButton(Sender).Caption then
            Variable := PicVarSFRList.var_list[i];
      free_created_buttons
   end;

procedure TChooseCOrAssemblyTypeListingForm.CancelButtonClick(Sender: TObject);
   begin
      free_created_buttons
   end;

procedure TChooseCOrAssemblyTypeListingForm.FormActivate(Sender: TObject);
   procedure create_button (caption: string; left, top: integer);
      var
         b: TButton;
         i: integer;
      begin
         b := TButton.Create (Self);
         b.Parent := Self;
         b.Top := top;
         b.Left := left;
         b.Width := Button1.Width;
         b.Caption := caption;
         b.OnClick := ButtonClick;
         b.ModalResult := mrOk;
         i := Length(created_buttons);
         SetLength (created_buttons, i+1);
         created_buttons[i] := b
      end;
   var
      i: integer;
   begin
      Label1.Caption := PicVarSFRList.pic_name;
      Button1.Caption := PicVarSFRList.var_list[0]^.var_name;
      for i := 1 to Length (PicVarSFRList.var_list)-1 do
         begin
            create_button (PicVarSFRList.var_list[i]^.var_name,
                           Button1.Left,
                           Button1.Top + (i * (Button1.Height + 16))
                          )
         end;
      CancelButton.Top := cancel_button_top + (Length(PicVarSFRList.var_list)-1) * (Button1.Height + 16);
      ClientHeight := CancelButton.Top + CancelButton.Height + 16
   end;

procedure TChooseCOrAssemblyTypeListingForm.FormCreate(Sender: TObject);
   begin
      first_button_top := Button1.Top;
      cancel_button_top := CancelButton.Top
   end;

procedure TChooseCOrAssemblyTypeListingForm.free_created_buttons;
   var
      i: integer;
   begin
      for i := 0 to Length(created_buttons)-1 do
         created_buttons[i].Free;
      SetLength(created_buttons, 0)
   end;
END.
