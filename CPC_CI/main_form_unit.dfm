object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 448
  ClientWidth = 916
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = DoCIProcessLoop
  DesignSize = (
    916
    448)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 296
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  inline GithubPoller: TGithubPoller
    Left = 8
    Top = 40
    Width = 357
    Height = 140
    Anchors = []
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 40
  end
  object StartButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = DoCIProcessLoop
  end
  object StopButton: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object Memo1: TMemo
    Left = 432
    Top = 232
    Width = 265
    Height = 184
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  inline GithubPuller: TCIStepFrameBaseClass
    Left = 380
    Top = 40
    Width = 357
    Height = 140
    Anchors = []
    TabOrder = 4
    ExplicitLeft = 380
    ExplicitTop = 40
    inherited Panel1: TPanel
      ExplicitLeft = 16
      ExplicitTop = -48
      inherited Label1: TLabel
        Width = 190
        Caption = 'Pull ci-test from Github'
        ExplicitWidth = 190
      end
    end
  end
  inline GithubPusher: TCIStepFrameBaseClass
    Left = 8
    Top = 248
    Width = 357
    Height = 140
    Anchors = []
    TabOrder = 5
    ExplicitLeft = 8
    ExplicitTop = 248
  end
end
