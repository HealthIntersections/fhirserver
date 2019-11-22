object ResourceNewForm: TResourceNewForm
  Left = 0
  Top = 0
  Caption = 'Create New Resource'
  ClientHeight = 233
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    440
    233)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 88
    Width = 45
    Height = 13
    Caption = 'Structure'
  end
  object Panel1: TPanel
    Left = 0
    Top = 192
    Width = 440
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      440
      41)
    object Label1: TLabel
      Left = 10
      Top = 14
      Width = 28
      Height = 13
      Caption = 'Filter:'
    end
    object edtFilter: TEdit
      Left = 64
      Top = 10
      Width = 74
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnCreate: TButton
      Left = 272
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Create'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 1
      OnClick = btnCreateClick
    end
    object Button2: TButton
      Left = 353
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object rbXml: TRadioButton
      Left = 150
      Top = 12
      Width = 45
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'XML'
      TabOrder = 3
    end
    object rbJson: TRadioButton
      Left = 201
      Top = 12
      Width = 55
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'JSON'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 440
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      440
      73)
    object Label2: TLabel
      Left = 10
      Top = 16
      Width = 35
      Height = 13
      Caption = 'Version'
    end
    object Label3: TLabel
      Left = 10
      Top = 43
      Width = 40
      Height = 13
      Caption = 'Package'
    end
    object cbxVersion: TComboBox
      Left = 64
      Top = 13
      Width = 368
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = cbxVersionChange
    end
    object cbxPackage: TComboBox
      Left = 64
      Top = 40
      Width = 368
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = cbxPackageChange
    end
  end
  object lbProfiles: TListBox
    Left = 64
    Top = 79
    Width = 368
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    OnClick = lbProfilesClick
    OnDblClick = btnCreateClick
  end
end
