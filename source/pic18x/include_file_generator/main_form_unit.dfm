object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 641
  ClientWidth = 871
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    871
    641)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 576
    Top = 256
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 552
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Memo1: TMemo
    Left = 18
    Top = 14
    Width = 423
    Height = 179
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button5: TButton
    Left = 576
    Top = 89
    Width = 75
    Height = 25
    Caption = 'to clipboard'
    TabOrder = 1
    OnClick = Button5Click
  end
  object RunSingleThreadCheckBox: TCheckBox
    Left = 120
    Top = 384
    Width = 105
    Height = 17
    Caption = 'Run Single Thread'
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object ProcessTopMenuItem: TMenuItem
      Caption = 'Process'
      object UnpackPICFilesfromMPLABX1: TMenuItem
        Caption = 'Unpack PIC Files from MPLABX'
        OnClick = UnpackPICFilesfromMPLABX1Click
      end
      object ProcessAllPICFilesMenuItem: TMenuItem
        Caption = 'all PIC Files -> Include Files'
        OnClick = ProcessAllPICFilesMenuItemClick
      end
      object SyntaxCheckIncludeFilesMenuItem: TMenuItem
        Caption = 'Syntax Check Include Files'
        OnClick = SyntaxCheckIncludeFilesMenuItemClick
      end
    end
    object ComboTypeMainMenuItem: TMenuItem
      Caption = 'ComboType'
      object NewComboTypeMenuItem: TMenuItem
        Caption = 'New...'
        OnClick = NewComboTypeMenuItemClick
      end
      object EditComboTypeMenuItem: TMenuItem
        Caption = 'Edit...'
        OnClick = ComboTypeEditMenuItemClick
      end
    end
    object ViewTopMenuItem: TMenuItem
      Caption = 'View'
      object ViewPICFileMenuItem: TMenuItem
        Caption = 'PIC File'
        OnClick = ViewPICFileMenuItemClick
      end
      object ViewXMLFileMenuItem: TMenuItem
        Caption = 'XML File'
        OnClick = ViewXMLFileMenuItemClick
      end
      object ViewIncFileMenuItem: TMenuItem
        Caption = 'INC File'
        OnClick = ViewIncFileMenuItemClick
      end
    end
    object About1: TMenuItem
      Caption = 'Help'
      object About2: TMenuItem
        Caption = 'About'
        OnClick = About2Click
      end
    end
  end
end
