object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pascal_FC'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  TextHeight = 15
  object Memo1: TMemo
    Left = 24
    Top = 112
    Width = 257
    Height = 297
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 328
    Top = 112
    Width = 257
    Height = 297
    Lines.Strings = (
      '')
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    FileName = 'C:\Users\majka\MOJE\Moje programy\PASCAL_FC\gardens4.pfc'
    Filter = 'pfc files (*.pfc)|*.pfc|Any file (*.*)|*.*'#39
    Left = 104
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        OnClick = SaveAs1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        OnClick = Print1Click
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
        OnClick = PrintSetup1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Undo1: TMenuItem
        Caption = '&Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
        Enabled = False
        ShortCut = 16472
        OnClick = Cut1Click
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        Enabled = False
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
      object PasteSpecial1: TMenuItem
        Caption = 'Paste &Special...'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Caption = '&Find...'
        OnClick = Find1Click
      end
      object Replace1: TMenuItem
        Caption = 'R&eplace...'
        OnClick = Replace1Click
      end
      object GoTo1: TMenuItem
        Caption = '&Go To...'
        OnClick = GoTo1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
    end
    object ranslate1: TMenuItem
      Caption = 'Compile'
      object ranslate2: TMenuItem
        Caption = 'Compile'
        Enabled = False
        OnClick = ranslate2Click
      end
    end
    object Run1: TMenuItem
      Caption = 'Run'
      object Run2: TMenuItem
        Caption = 'Run'
        Enabled = False
        OnClick = Run2Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
      end
      object Index1: TMenuItem
        Caption = '&Index'
      end
      object Commands1: TMenuItem
        Caption = 'Co&mmands'
      end
      object Procedures1: TMenuItem
        Caption = '&Procedures'
      end
      object Keyboard1: TMenuItem
        Caption = '&Keyboard'
      end
      object SearchforHelpOn1: TMenuItem
        Caption = '&Search for Help On...'
      end
      object Tutorial1: TMenuItem
        Caption = '&Tutorial'
      end
      object HowtoUseHelp1: TMenuItem
        Caption = '&How to Use Help'
      end
      object About1: TMenuItem
        Caption = '&About...'
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 200
    Top = 16
  end
  object FindDialog1: TFindDialog
    OnFind = FindDialogFind
    Left = 280
    Top = 8
  end
  object ReplaceDialog1: TReplaceDialog
    Options = [frDown, frReplace, frReplaceAll]
    OnReplace = ReplaceDialog1Replace
    Left = 392
    Top = 8
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 496
    Top = 8
  end
  object PrintDialog1: TPrintDialog
    Left = 560
    Top = 8
  end
end
