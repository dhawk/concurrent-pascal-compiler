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
  object Button4: TButton
    Left = 696
    Top = 89
    Width = 75
    Height = 25
    Caption = 'run jar.exe'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 568
    Top = 192
    Width = 75
    Height = 25
    Caption = 'load from xml'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 464
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
  object Button3: TButton
    Left = 176
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object ProcessTopMenuItem: TMenuItem
      Caption = 'Process'
      object ProcessAllPICFilesMenuItem: TMenuItem
        Caption = 'all PIC Files'
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
      Caption = 'About'
      OnClick = About1Click
    end
  end
end
