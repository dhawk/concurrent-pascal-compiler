unit select_combo_type_dlg_unit;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, combo_type_unit;

type
   TSelectComboTypeDlg =
      class(TForm)
         OKBtn: TButton;
         CancelBtn: TButton;
         ListBox: TListBox;
         procedure FormActivate(Sender: TObject);
         procedure ListBoxClick(Sender: TObject);
      private
         { Private declarations }
      public
         procedure LoadControls (combo_type_list: TComboTypeList);
         function Selected: TComboType;
      end;

var
  SelectComboTypeDlg: TSelectComboTypeDlg;

implementation

{$R *.dfm}

procedure TSelectComboTypeDlg.LoadControls (combo_type_list: TComboTypeList);
   var
      ct: TComboType;
   begin
      ListBox.Clear;
      for ct in combo_type_list do
         ListBox.Items.AddObject (ct.TypeName, ct)
   end;

procedure TSelectComboTypeDlg.FormActivate(Sender: TObject);
   begin
      OkBtn.Enabled := false;
   end;

procedure TSelectComboTypeDlg.ListBoxClick(Sender: TObject);
   begin
      OkBtn.Enabled := true
   end;

function TSelectComboTypeDlg.Selected: TComboType;
   begin
      result := ListBox.Items.Objects[ListBox.ItemIndex] as TComboType
   end;

end.
