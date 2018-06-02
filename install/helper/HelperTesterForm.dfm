object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 389
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 105
    Top = 359
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Unload'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Packages'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 104
    Top = 51
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '3.0.1'
  end
  object Button4: TButton
    Left = 552
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 4
    OnClick = Button4Click
  end
  object CheckListBox1: TCheckListBox
    Left = 24
    Top = 78
    Width = 521
    Height = 243
    ItemHeight = 13
    TabOrder = 5
  end
  object Button5: TButton
    Left = 24
    Top = 328
    Width = 75
    Height = 33
    Caption = 'Install'
    TabOrder = 6
    OnClick = Button5Click
  end
  object ProgressBar1: TProgressBar
    Left = 105
    Top = 336
    Width = 440
    Height = 17
    TabOrder = 7
  end
end
