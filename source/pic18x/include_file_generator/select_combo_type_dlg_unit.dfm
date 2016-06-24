object SelectComboTypeDlg: TSelectComboTypeDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 364
  ClientWidth = 228
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 140
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 140
    Top = 326
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ListBox: TListBox
    Left = 8
    Top = 8
    Width = 113
    Height = 345
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListBoxClick
  end
end
