object ProgressWindow: TProgressWindow
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Logging In'
  ClientHeight = 71
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Logging In...'
  end
  object lbCounter: TLabel
    Left = 126
    Top = 44
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Button1: TButton
    Left = 232
    Top = 11
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Stop'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 178
    Top = 36
  end
end
