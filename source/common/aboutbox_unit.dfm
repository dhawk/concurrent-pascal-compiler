object AboutBoxForm: TAboutBoxForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'About'
  ClientHeight = 530
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    325
    530)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 158
    Top = 35
    Width = 142
    Height = 32
    Caption = 'Concurrent'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -29
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 81
    Width = 78
    Height = 32
    Caption = 'Pascal'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -29
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ProductName: TLabel
    Left = 8
    Top = 159
    Width = 309
    Height = 19
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '**program name here**'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    WordWrap = True
  end
  object WebsiteLabel: TLabel
    Left = 18
    Top = 205
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
  object Label3: TLabel
    Left = 28
    Top = 335
    Width = 209
    Height = 13
    Caption = 'Includes the following third-party software:'
  end
  object OKButton: TButton
    Left = 125
    Top = 490
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object BuiltUsingGroupBox: TGroupBox
    Left = 28
    Top = 232
    Width = 269
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'BuiltUsingGroupBox'
    Color = clWhite
    ParentColor = False
    TabOrder = 1
    object BuilderLabel1: TLabel
      Left = 87
      Top = 20
      Width = 63
      Height = 13
      Caption = 'BuilderLabel1'
      Color = clWhite
      ParentColor = False
      Transparent = False
    end
    object BuilderLabel2: TLabel
      Left = 87
      Top = 39
      Width = 63
      Height = 13
      Caption = 'BuilderLabel2'
      Color = clWhite
      ParentColor = False
      Transparent = False
    end
    object BuilderLabel3: TLabel
      Left = 87
      Top = 58
      Width = 63
      Height = 13
      Caption = 'BuilderLabel3'
      Color = clWhite
      ParentColor = False
      Transparent = False
    end
  end
  object Memo1: TMemo
    Left = 28
    Top = 354
    Width = 269
    Height = 124
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = []
    BevelInner = bvNone
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
