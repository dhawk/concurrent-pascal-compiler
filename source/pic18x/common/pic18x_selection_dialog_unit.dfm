object Pic18xSelectionDialog: TPic18xSelectionDialog
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Select PIC18x'
  ClientHeight = 461
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = InitializeSelection
  OnDeactivate = FormDeactivate
  DesignSize = (
    194
    461)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 179
    Height = 384
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Label1: TLabel
    Left = 16
    Top = 192
    Width = 161
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Label1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object OkButton: TButton
    Left = 8
    Top = 415
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 112
    Top = 415
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 16
    Top = 16
    Width = 161
    Height = 337
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14'
      '15'
      '16'
      '17'
      '18'
      '19'
      '20'
      '21'
      '22'
      '23'
      '24'
      '25'
      '26'
      '27'
      '28'
      '29'
      '30')
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object ResetButton: TButton
    Left = 59
    Top = 359
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Reset'
    TabOrder = 3
    OnClick = InitializeSelection
  end
end
