object MainForm: TMainForm
  Left = 174
  Top = 210
  Caption = 'Concurrent Pascal Compiler Core Syntax Analysis Test'
  ClientHeight = 511
  ClientWidth = 1039
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  DesignSize = (
    1039
    511)
  PixelsPerInch = 96
  TextHeight = 13
  object ClearButton: TButton
    Left = 8
    Top = 478
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 0
    OnClick = ClearButtonClick
  end
  object CompileButton: TButton
    Left = 96
    Top = 478
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Compile'
    TabOrder = 1
    OnClick = CompileButtonClick
  end
  object CompileResultsMemo: TMemo
    Left = 8
    Top = 348
    Width = 873
    Height = 117
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 873
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 904
    Top = 55
    Width = 69
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Constants'
    TabOrder = 4
  end
  object CheckBox3: TCheckBox
    Left = 904
    Top = 78
    Width = 49
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Types'
    TabOrder = 5
  end
  object CheckBox4: TCheckBox
    Left = 904
    Top = 101
    Width = 43
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Data'
    TabOrder = 6
  end
  object CheckBox5: TCheckBox
    Left = 904
    Top = 123
    Width = 61
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'MP Math'
    TabOrder = 7
  end
  object CheckBox6: TCheckBox
    Left = 904
    Top = 146
    Width = 75
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Statements'
    TabOrder = 8
  end
  object CheckBox7: TCheckBox
    Left = 904
    Top = 169
    Width = 60
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Program'
    TabOrder = 9
  end
  object CheckBox8: TCheckBox
    Left = 904
    Top = 192
    Width = 116
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Primary Expressions'
    TabOrder = 10
  end
  object CheckBox10: TCheckBox
    Left = 904
    Top = 215
    Width = 111
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Factor Expressions'
    TabOrder = 11
  end
  object CheckBox11: TCheckBox
    Left = 904
    Top = 238
    Width = 104
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Term Expressions'
    TabOrder = 12
  end
  object CheckBox12: TCheckBox
    Left = 904
    Top = 261
    Width = 110
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Simple Expressions'
    TabOrder = 13
  end
  object CheckBox13: TCheckBox
    Left = 904
    Top = 284
    Width = 127
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Relational Expressions'
    TabOrder = 14
  end
  object CheckBox9: TCheckBox
    Left = 904
    Top = 307
    Width = 53
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Access'
    TabOrder = 15
  end
  object SelectAllButton: TButton
    Left = 906
    Top = 408
    Width = 65
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Select All'
    TabOrder = 16
    OnClick = SelectAllButtonClick
  end
  object RunSelectedTestsButton: TButton
    Left = 906
    Top = 448
    Width = 104
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Run Selected Tests'
    TabOrder = 17
    OnClick = RunSelectedTestsButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 904
    Top = 32
    Width = 37
    Height = 19
    Caption = 'Lex'
    TabOrder = 18
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 8
    object About1: TMenuItem
      Caption = 'About'
      object AboutTestCPCCoreCompiler1: TMenuItem
        Caption = 'About Test CPC Core Compiler'
        OnClick = AboutTestCPCCoreCompiler1Click
      end
    end
  end
end
