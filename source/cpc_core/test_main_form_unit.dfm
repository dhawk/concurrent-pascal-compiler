object MainForm: TMainForm
  Left = 174
  Height = 511
  Top = 210
  Width = 1039
  Caption = 'Concurrent Pascal Compiler Core Syntax Analysis Test'
  ClientHeight = 511
  ClientWidth = 1039
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  LCLVersion = '1.6.0.4'
  object ClearButton: TButton
    Left = 8
    Height = 25
    Top = 478
    Width = 75
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    OnClick = ClearButtonClick
    TabOrder = 0
  end
  object CompileButton: TButton
    Left = 96
    Height = 25
    Top = 478
    Width = 75
    Anchors = [akLeft, akBottom]
    Caption = 'Compile'
    OnClick = CompileButtonClick
    TabOrder = 1
  end
  object CompileResultsMemo: TMemo
    Left = 8
    Height = 117
    Top = 348
    Width = 873
    Anchors = [akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Pitch = fpFixed
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo: TMemo
    Left = 8
    Height = 321
    Top = 8
    Width = 873
    Anchors = [akTop, akLeft, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Pitch = fpFixed
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 904
    Height = 19
    Top = 55
    Width = 69
    Anchors = [akTop, akRight]
    Caption = 'Constants'
    TabOrder = 4
  end
  object CheckBox3: TCheckBox
    Left = 904
    Height = 19
    Top = 78
    Width = 49
    Anchors = [akTop, akRight]
    Caption = 'Types'
    TabOrder = 5
  end
  object CheckBox4: TCheckBox
    Left = 904
    Height = 19
    Top = 101
    Width = 43
    Anchors = [akTop, akRight]
    Caption = 'Data'
    TabOrder = 6
  end
  object CheckBox5: TCheckBox
    Left = 904
    Height = 19
    Top = 123
    Width = 61
    Anchors = [akTop, akRight]
    Caption = 'MP Math'
    TabOrder = 7
  end
  object CheckBox6: TCheckBox
    Left = 904
    Height = 19
    Top = 146
    Width = 75
    Anchors = [akTop, akRight]
    Caption = 'Statements'
    TabOrder = 8
  end
  object CheckBox7: TCheckBox
    Left = 904
    Height = 19
    Top = 169
    Width = 60
    Anchors = [akTop, akRight]
    Caption = 'Program'
    TabOrder = 9
  end
  object CheckBox8: TCheckBox
    Left = 904
    Height = 19
    Top = 192
    Width = 116
    Anchors = [akTop, akRight]
    Caption = 'Primary Expressions'
    TabOrder = 10
  end
  object CheckBox10: TCheckBox
    Left = 904
    Height = 19
    Top = 215
    Width = 111
    Anchors = [akTop, akRight]
    Caption = 'Factor Expressions'
    TabOrder = 11
  end
  object CheckBox11: TCheckBox
    Left = 904
    Height = 19
    Top = 238
    Width = 104
    Anchors = [akTop, akRight]
    Caption = 'Term Expressions'
    TabOrder = 12
  end
  object CheckBox12: TCheckBox
    Left = 904
    Height = 19
    Top = 261
    Width = 110
    Anchors = [akTop, akRight]
    Caption = 'Simple Expressions'
    TabOrder = 13
  end
  object CheckBox13: TCheckBox
    Left = 904
    Height = 19
    Top = 284
    Width = 127
    Anchors = [akTop, akRight]
    Caption = 'Relational Expressions'
    TabOrder = 14
  end
  object CheckBox9: TCheckBox
    Left = 904
    Height = 19
    Top = 307
    Width = 53
    Anchors = [akTop, akRight]
    Caption = 'Access'
    TabOrder = 15
  end
  object SelectAllButton: TButton
    Left = 906
    Height = 17
    Top = 408
    Width = 65
    Anchors = [akTop, akRight]
    Caption = 'Select All'
    OnClick = SelectAllButtonClick
    TabOrder = 16
  end
  object RunSelectedTestsButton: TButton
    Left = 906
    Height = 25
    Top = 448
    Width = 104
    Anchors = [akTop, akRight]
    Caption = 'Run Selected Tests'
    OnClick = RunSelectedTestsButtonClick
    TabOrder = 17
  end
  object CheckBox1: TCheckBox
    Left = 904
    Height = 19
    Top = 32
    Width = 37
    Caption = 'Lex'
    TabOrder = 18
  end
end
