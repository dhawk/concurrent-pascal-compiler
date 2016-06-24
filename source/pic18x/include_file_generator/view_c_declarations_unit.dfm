object ViewCDeclarationsForm: TViewCDeclarationsForm
  Left = 0
  Top = 0
  Caption = 'ViewCDeclarationsForm'
  ClientHeight = 426
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object TabbedNotebook: TTabbedNotebook
    Left = 8
    Top = 8
    Width = 737
    Height = 409
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'Tahoma'
    TabFont.Style = []
    TabOrder = 0
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Default'
      ExplicitHeight = 173
    end
  end
end
