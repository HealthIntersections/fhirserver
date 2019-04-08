object TransformerExecConfigForm: TTransformerExecConfigForm
  Left = 0
  Top = 0
  Caption = 'Edit Execution Configuration'
  ClientHeight = 347
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    704
    347)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 53
    Height = 13
    Caption = 'Javascript:'
  end
  object Label2: TLabel
    Left = 16
    Top = 51
    Width = 67
    Height = 13
    Caption = 'Focus Object:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 307
    Width = 704
    Height = 40
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 259
    ExplicitWidth = 635
    DesignSize = (
      704
      40)
    object btnok: TButton
      Left = 540
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnokClick
      ExplicitLeft = 471
    end
    object btnCancel: TButton
      Left = 621
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 552
    end
  end
  object cbxScripts: TComboBox
    Left = 96
    Top = 21
    Width = 577
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 601
  end
  object cbxFocus: TComboBox
    Left = 96
    Top = 48
    Width = 577
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 601
  end
end
