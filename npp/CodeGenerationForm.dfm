object CodeGeneratorForm: TCodeGeneratorForm
  Left = 0
  Top = 0
  Caption = 'Code Generation'
  ClientHeight = 742
  ClientWidth = 861
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 861
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      861
      41)
    object Label1: TLabel
      Left = 6
      Top = 15
      Width = 161
      Height = 13
      Caption = 'Generated Code for this resource'
    end
    object Label2: TLabel
      Left = 599
      Top = 15
      Width = 51
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Language:'
    end
    object cbxLanguage: TComboBox
      Left = 656
      Top = 11
      Width = 199
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 2
      TabOrder = 0
      Text = 'Pascal'
      OnChange = cbxLanguageChange
      Items.Strings = (
        'Java (Reference Implementation)'
        'Java (HAPI)'
        'Pascal'
        'DotNet')
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 701
    Width = 861
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      861
      41)
    object Button1: TButton
      Left = 778
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 6
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Copy'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 861
    Height = 660
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    Caption = 'Panel3'
    TabOrder = 2
    object Memo1: TMemo
      Left = 6
      Top = 6
      Width = 849
      Height = 648
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
end
