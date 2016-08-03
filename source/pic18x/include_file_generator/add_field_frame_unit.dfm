object AddFieldFrame: TAddFieldFrame
  Left = 0
  Top = 0
  Width = 303
  Height = 49
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 41
    Align = alCustom
    TabOrder = 0
    object FieldNameEdit: TEdit
      Left = 8
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = ValidateOnChange
    end
    object DeleteButton: TButton
      Left = 231
      Top = 7
      Width = 49
      Height = 23
      Caption = 'Delete'
      TabOrder = 1
      OnClick = DeleteButtonClick
    end
    object BitnoSpinEdit: TSpinEdit
      Left = 135
      Top = 8
      Width = 42
      Height = 22
      MaxValue = 999
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = ValidateOnChange
    end
    object WidthSpinEdit: TSpinEdit
      Left = 183
      Top = 8
      Width = 42
      Height = 22
      MaxValue = 999
      MinValue = 1
      TabOrder = 3
      Value = 1
      OnChange = ValidateOnChange
    end
  end
end
