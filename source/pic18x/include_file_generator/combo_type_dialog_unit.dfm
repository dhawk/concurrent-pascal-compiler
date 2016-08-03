object ComboTypeDialog: TComboTypeDialog
  Left = 0
  Top = 0
  Caption = 'Combo Type Editor'
  ClientHeight = 532
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    823
    532)
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 8
    Top = 480
    Width = 61
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Type Name: '
    ExplicitTop = 472
  end
  object TabbedNotebook: TTabbedNotebook
    Left = 8
    Top = 8
    Width = 807
    Height = 453
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'Tahoma'
    TabFont.Style = []
    TabOrder = 0
    OnChange = TabbedNotebookChange
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'SFR Pattern(s)'
      DesignSize = (
        799
        425)
      object Label1: TLabel
        Left = 532
        Top = 23
        Width = 94
        Height = 13
        Caption = 'Number of Patterns'
      end
      object Label2: TLabel
        Left = 699
        Top = 23
        Width = 77
        Height = 13
        Caption = 'Number of SFRs'
      end
      object Label3: TLabel
        Left = 8
        Top = 23
        Width = 136
        Height = 13
        Caption = 'Variable Name Format String'
      end
      object NumberOfPatternsSpinEdit: TSpinEdit
        Left = 481
        Top = 20
        Width = 45
        Height = 22
        MaxValue = 4
        MinValue = 1
        TabOrder = 0
        Value = 1
        OnChange = NumberOfPatternsSpinEditChange
      end
      object NumberOfSFRsSpinEdit: TSpinEdit
        Left = 647
        Top = 20
        Width = 46
        Height = 22
        MaxValue = 50
        MinValue = 1
        TabOrder = 1
        Value = 1
        OnChange = NumberOfSFRsSpinEditChange
      end
      object ReversedCheckBox: TCheckBox
        Left = 294
        Top = 22
        Width = 67
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Reversed'
        TabOrder = 2
      end
      object SFRPatternsScrollBox: TScrollBox
        Left = 8
        Top = 58
        Width = 782
        Height = 287
        HorzScrollBar.Tracking = True
        VertScrollBar.Tracking = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 3
        object Offset0Label: TLabel
          Left = 10
          Top = 64
          Width = 10
          Height = 13
          Caption = '0:'
        end
        object TestSFRPatternPanel: TPanel
          Left = 36
          Top = 14
          Width = 113
          Height = 281
          BorderStyle = bsSingle
          TabOrder = 0
          object TestSFRPatternButton: TButton
            Left = 16
            Top = 9
            Width = 75
            Height = 25
            Caption = 'Test'
            TabOrder = 0
            OnClick = TestSFRPatternButtonClick
          end
        end
      end
      object VariableNameFormatStringEdit: TEdit
        Left = 150
        Top = 20
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'VariableNameFormatStringEdit'
        OnChange = ValidateOnChange
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Field Name Fixups'
      DesignSize = (
        799
        425)
      object Label5: TLabel
        Left = 140
        Top = 15
        Width = 31
        Height = 13
        Caption = 'RegEx'
      end
      object Label6: TLabel
        Left = 346
        Top = 7
        Width = 62
        Height = 13
        Caption = 'Replacement'
      end
      object Label7: TLabel
        Left = 346
        Top = 26
        Width = 65
        Height = 13
        Caption = 'Format String'
      end
      object Label9: TLabel
        Left = 520
        Top = 16
        Width = 54
        Height = 13
        Caption = 'Test Result'
      end
      object FieldNameFixupsScrollBox: TScrollBox
        Left = 112
        Top = 45
        Width = 676
        Height = 228
        HorzScrollBar.Smooth = True
        HorzScrollBar.Tracking = True
        HorzScrollBar.Visible = False
        VertScrollBar.Smooth = True
        VertScrollBar.Tracking = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
      object AddFieldNameFixupButton: TButton
        Left = 10
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = AddFieldNameFixupButtonClick
      end
      object TestFieldNameFixupsEdit: TEdit
        Left = 10
        Top = 274
        Width = 87
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 2
      end
      object TestFieldNameFixupsButton: TButton
        Left = 18
        Top = 316
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Test'
        TabOrder = 3
        OnClick = TestFieldNameFixupsButtonClick
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Add Fields'
      DesignSize = (
        799
        425)
      object AddAddFieldsButton: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 0
        OnClick = AddAddFieldsButtonClick
      end
      object AddFieldsScrollBox: TScrollBox
        Left = 103
        Top = 11
        Width = 685
        Height = 316
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Combo Type Usage'
      object ComboTypeUsageReportMemo: TMemo
        Left = 0
        Top = 0
        Width = 799
        Height = 425
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        Lines.Strings = (
          'ComboTypeUsageReportMemo')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Field Usage'
      object Label11: TLabel
        Left = 2
        Top = 399
        Width = 220
        Height = 13
        Caption = 'Note: Fields present in all variables not shown'
      end
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 799
        Height = 149
        VertScrollBar.Tracking = True
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object FieldUsageReportTreeView: TTreeView
          Left = 0
          Top = 0
          Width = 795
          Height = 145
          Align = alClient
          Indent = 19
          TabOrder = 0
          OnCreateNodeClass = FieldUsageReportTreeViewCreateNodeClass
          OnMouseDown = FieldUsageReportTreeViewMouseDown
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Field Discrepancies'
      object ScrollBox2: TScrollBox
        Left = 0
        Top = 0
        Width = 799
        Height = 425
        Align = alClient
        TabOrder = 0
        object FieldDiscrepanciesTreeView: TTreeView
          Left = 0
          Top = 0
          Width = 795
          Height = 421
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
    end
  end
  object CancelButton: TButton
    Left = 736
    Top = 475
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 513
    Width = 823
    Height = 19
    AutoHint = True
    Panels = <>
    SimplePanel = True
  end
  object SaveButtonHintPanel: TPanel
    Left = 642
    Top = 475
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 3
    object SaveButton: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Align = alClient
      Caption = 'Save'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object TypeNameEdit: TEdit
    Left = 75
    Top = 477
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    OnChange = ValidateOnChange
  end
  object DeleteTypeButton: TButton
    Left = 546
    Top = 475
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Delete Type'
    ModalResult = 3
    TabOrder = 5
    OnClick = DeleteTypeButtonClick
  end
end
