object TransformerOptionsForm: TTransformerOptionsForm
  Left = 0
  Top = 0
  Caption = 'TransformerOptionsForm'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 635
    Height = 40
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      635
      40)
    object btnok: TButton
      Left = 471
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnokClick
    end
    object btnCancel: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object cbAutosave: TCheckBox
    Left = 8
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Autosave files'
    TabOrder = 1
  end
end
