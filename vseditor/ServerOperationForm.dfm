object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Form2'
  ClientHeight = 131
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 131
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 5
    TabOrder = 0
    DesignSize = (
      363
      131)
    object lblOpName: TLabel
      Left = 25
      Top = 23
      Width = 316
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'lblOpName'
      ExplicitWidth = 150
    end
    object lblProgress: TLabel
      Left = 25
      Top = 63
      Width = 316
      Height = 13
      AutoSize = False
    end
    object DProgressBar1: TDProgressBar
      Left = 25
      Top = 42
      Width = 316
      Height = 15
      Animate = True
      Min = 0
      Max = 100
      Position = 100
      Step = 100
      Speed = 50
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 266
      Top = 87
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      Enabled = False
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
      ExplicitLeft = 100
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 20
    Top = 64
  end
end
