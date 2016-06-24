object ProcessingFrame: TProcessingFrame
  Left = 0
  Top = 0
  Width = 218
  Height = 170
  Align = alCustom
  TabOrder = 0
  object Bevel1: TBevel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 212
    Height = 164
    Align = alClient
    Anchors = []
    Shape = bsFrame
    Style = bsRaised
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 240
    ExplicitHeight = 170
  end
  object Label1: TLabel
    Left = 16
    Top = 144
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 16
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Memo: TMemo
    Left = 16
    Top = 41
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
end
