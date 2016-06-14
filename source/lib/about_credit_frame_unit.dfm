object AboutCreditFrame: TAboutCreditFrame
  Left = 0
  Top = 0
  Width = 287
  Height = 235
  Color = clWhite
  ParentColor = False
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 235
    Align = alClient
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      287
      235)
    object Label1: TLabel
      Left = 7
      Top = 95
      Width = 51
      Height = 13
      Caption = 'Thanks to:'
    end
    object BuildUsingGroupBox: TGroupBox
      Left = 8
      Top = 2
      Width = 269
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Caption = 'BuildUsingGroupBox'
      Color = clWhite
      ParentColor = False
      TabOrder = 0
      object BuiltUsingImage: TImage
        Left = 13
        Top = 20
        Width = 60
        Height = 60
      end
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
      Left = 8
      Top = 114
      Width = 269
      Height = 112
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelInner = bvNone
      Lines.Strings = (
        'Memo1'
        'and more..'
        'a'
        'sdf'
        'adsf'
        'asdf'
        'asdf'
        'asdf'
        'asdf'
        'asdf'
        'asdf'
        'asdf'
        'asdf'
        'xy')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
