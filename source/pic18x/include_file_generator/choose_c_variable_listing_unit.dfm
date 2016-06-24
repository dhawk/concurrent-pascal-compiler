object ChooseCOrAssemblyTypeListingForm: TChooseCOrAssemblyTypeListingForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'View C  Declarations for Variable SFRs'
  ClientHeight = 122
  ClientWidth = 211
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object CancelButton: TButton
    Left = 68
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object Button1: TButton
    Left = 45
    Top = 32
    Width = 121
    Height = 25
    Caption = 'Button1'
    ModalResult = 1
    TabOrder = 1
    OnClick = ButtonClick
  end
end
