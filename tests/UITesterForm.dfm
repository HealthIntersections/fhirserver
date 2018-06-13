object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 295
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 295
    Width = 635
    Height = 41
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Mark'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 97
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Data'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 178
      Top = 6
      Width = 75
      Height = 25
      Caption = 'More'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 259
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Function'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
end
