object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Give number of row'
  ClientHeight = 88
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 84
    Height = 15
    Caption = 'Number of row:'
  end
  object Edit1: TEdit
    Left = 24
    Top = 40
    Width = 281
    Height = 23
    TabOrder = 0
  end
  object Button1: TButton
    Left = 344
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Done'
    TabOrder = 1
    OnClick = Button1Click
  end
end
