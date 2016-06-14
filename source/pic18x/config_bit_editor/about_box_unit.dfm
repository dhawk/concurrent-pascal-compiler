object AboutBox: TAboutBox
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 349
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ProductName: TLabel
    Left = 29
    Top = 8
    Width = 348
    Height = 16
    Caption = 'Concurrent Pascal PIC18x Configuration Bits Editor'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object WebsiteLabel: TLabel
    Left = 59
    Top = 30
    Width = 288
    Height = 13
    Caption = 'http://dhawk.github.io/concurrent-pascal-compiler'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object OKButton: TButton
    Left = 166
    Top = 306
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  inline AboutCreditFrame1: TAboutCreditFrame
    Left = 68
    Top = 65
    Width = 272
    Height = 227
    Color = clWhite
    ParentColor = False
    TabOrder = 1
    ExplicitLeft = 68
    ExplicitTop = 65
    inherited BuildUsingGroupBox: TGroupBox
      inherited BuilderLabel1: TLabel
        Width = 64
        ExplicitWidth = 64
      end
      inherited BuilderLabel2: TLabel
        Width = 64
        ExplicitWidth = 64
      end
      inherited BuilderLabel3: TLabel
        Width = 64
        ExplicitWidth = 64
      end
    end
  end
end
