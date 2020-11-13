object TextPresentationDialog: TTextPresentationDialog
  Left = 0
  Top = 0
  Caption = 'TextPresentationDialog'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 296
    Width = 635
    Height = 40
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      635
      40)
    object btnCancel: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 296
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    Caption = 'Panel2'
    TabOrder = 1
    object Memo1: TMemo
      Left = 6
      Top = 6
      Width = 623
      Height = 284
      Align = alClient
      TabOrder = 0
    end
  end
end
