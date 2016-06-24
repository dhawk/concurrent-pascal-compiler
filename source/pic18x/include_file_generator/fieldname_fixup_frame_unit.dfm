object FieldnameFixupFrame: TFieldnameFixupFrame
  Left = 0
  Top = 0
  Width = 617
  Height = 49
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 41
    Align = alCustom
    TabOrder = 0
    object Label1: TLabel
      Left = 280
      Top = 11
      Width = 114
      Height = 13
      Caption = '1234567890123456789'
    end
    object RegexPatternEdit: TEdit
      Left = 8
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'RegexPatternEdit'
      OnChange = RegexPatternEditChange
    end
    object ReplacementFormatStringEdit: TEdit
      Left = 144
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'ReplacementFormatStringEdit'
    end
    object DeleteButton: TButton
      Left = 344
      Top = 7
      Width = 49
      Height = 23
      Caption = 'Delete'
      TabOrder = 2
      OnClick = DeleteButtonClick
    end
  end
end
