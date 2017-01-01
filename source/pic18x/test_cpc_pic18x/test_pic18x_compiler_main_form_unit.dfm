object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'PIC18x Concurrent Pascal Compiler Test'
  ClientHeight = 775
  ClientWidth = 1212
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1212
    775)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 1196
    Height = 759
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Source'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1172
      ExplicitHeight = 717
      DesignSize = (
        1188
        731)
      object Label1: TLabel
        Left = 648
        Top = 696
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Memo: TMemo
        Left = 16
        Top = 16
        Width = 1145
        Height = 495
        Anchors = [akLeft, akTop, akBottom]
        BevelEdges = []
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitHeight = 481
      end
      object CompileResultsMemo: TMemo
        Left = 16
        Top = 534
        Width = 1161
        Height = 149
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        ExplicitTop = 520
        ExplicitWidth = 1145
      end
      object ClearMemoButton: TButton
        Left = 16
        Top = 703
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Clear'
        TabOrder = 2
        OnClick = ClearMemoButtonClick
        ExplicitTop = 689
      end
      object CompileMemoButton: TButton
        Left = 112
        Top = 703
        Width = 97
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Compile Memo'
        TabOrder = 3
        OnClick = CompileMemoButtonClick
        ExplicitTop = 689
      end
      object RunButton: TButton
        Left = 528
        Top = 702
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Run'
        TabOrder = 4
        OnClick = RunButtonClick
        ExplicitTop = 688
      end
      object CleanupTestSrcButton: TButton
        Left = 721
        Top = 702
        Width = 97
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'cleanup test src'
        TabOrder = 5
        OnClick = CleanupTestSrcButtonClick
        ExplicitLeft = 705
        ExplicitTop = 688
      end
      object SrcToClipboardForTestButton: TButton
        Left = 911
        Top = 703
        Width = 115
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Src->Clipboard 4 test'
        TabOrder = 6
        OnClick = SrcToClipboardForTestButtonClick
        ExplicitLeft = 895
        ExplicitTop = 689
      end
      object SrcToClipboardButton: TButton
        Left = 824
        Top = 702
        Width = 81
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Src->Clipboard'
        TabOrder = 7
        OnClick = SrcToClipboardButtonClick
        ExplicitLeft = 808
        ExplicitTop = 688
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Assembly'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1172
      ExplicitHeight = 717
      DesignSize = (
        1188
        731)
      object AssemblySourceMemo: TMemo
        Left = 24
        Top = 24
        Width = 1161
        Height = 655
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 1145
        ExplicitHeight = 641
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Trace'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1172
      ExplicitHeight = 717
      object Label2: TLabel
        Left = 27
        Top = 27
        Width = 360
        Height = 15
        Caption = 'PC      Instruction                W   Status'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
      end
      object TraceMemo: TMemo
        Left = 24
        Top = 48
        Width = 1129
        Height = 609
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'System'
        Font.Pitch = fpFixed
        Font.Style = []
        Lines.Strings = (
          'TraceMemo')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Compiler Tests'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1172
      ExplicitHeight = 717
      DesignSize = (
        1188
        731)
      object TestResultsMemo: TMemo
        Left = 32
        Top = 32
        Width = 1137
        Height = 609
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
        ExplicitWidth = 1121
      end
      object TestCompilerButton: TButton
        Left = 24
        Top = 686
        Width = 89
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Test Compiler'
        TabOrder = 1
        OnClick = TestCompilerButtonClick
        ExplicitTop = 672
      end
    end
  end
end
