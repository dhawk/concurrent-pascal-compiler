object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'PIC18x Configuration Bits Editor'
  ClientHeight = 493
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    1028
    493)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 1012
    Height = 476
    ActivePage = Main
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Main: TTabSheet
      Caption = 'Main'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        1004
        448)
      object Memo: TMemo
        Left = 16
        Top = 16
        Width = 972
        Height = 420
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object SaveDialog: TSaveDialog
    Left = 248
  end
  object OpenDialog: TOpenDialog
    Filter = 'Include files (*.inc)|*.inc'
    Title = 'Open PIC18x Configuration Bits Include File'
    Left = 184
  end
  object MainMenu: TMainMenu
    Left = 120
    object FileMainMenu: TMenuItem
      Caption = 'File'
      object FileNewMenuItem: TMenuItem
        Caption = 'New'
        OnClick = FileNewMenuItemClick
      end
      object FileOpenMenuItem: TMenuItem
        Caption = 'Open...'
        OnClick = FileOpenMenuItemClick
      end
      object FileSaveMenuItem: TMenuItem
        Caption = 'Save'
        Enabled = False
        OnClick = FileSaveMenuItemClick
      end
      object FileSaveAsMenuItem: TMenuItem
        Caption = 'Save As...'
        Enabled = False
        OnClick = FileSaveAsMenuItemClick
      end
      object FileMenuSeparator: TMenuItem
        Caption = '-'
      end
      object ExitMenuItem: TMenuItem
        Caption = 'Exit'
        OnClick = ExitMenuItemClick
      end
    end
    object AboutMainMenuItem: TMenuItem
      Caption = 'About'
      OnClick = AboutMainMenuItemClick
    end
  end
end
