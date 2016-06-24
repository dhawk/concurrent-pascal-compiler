inherited ProcessAllPICFilesDlg: TProcessAllPICFilesDlg
  Caption = 'Process All PIC Files'
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited OKBtn: TButton
    ExplicitTop = 520
  end
  inherited ScrollBox1: TScrollBox
    inherited Panel1: TPanel
      ExplicitLeft = 0
      ExplicitTop = 0
    end
  end
  inherited StopProcessingAllButton: TButton
    ExplicitTop = 520
  end
  inherited ErrorMemo: TMemo
    ExplicitHeight = 210
  end
end
