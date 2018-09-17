object IIEditForm: TIIEditForm
  Left = 0
  Top = 0
  Caption = 'IIEditForm'
  ClientHeight = 263
  ClientWidth = 470
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
  object Root: TLabel
    Left = 16
    Top = 11
    Width = 23
    Height = 13
    Caption = 'Root'
  end
  object Extension: TLabel
    Left = 16
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Extension'
  end
  object Label1: TLabel
    Left = 16
    Top = 91
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 73
    Height = 13
    Caption = 'Defined OIDs'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 144
    Width = 449
    Height = 9
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 21
    Top = 186
    Width = 28
    Height = 13
    Caption = 'Filter:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 222
    Width = 470
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      470
      41)
    object Button1: TButton
      Left = 383
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object Button2: TButton
      Left = 302
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object cbxOIDs: TComboBox
    Left = 21
    Top = 159
    Width = 441
    Height = 21
    TabOrder = 1
    Text = 'Choose an OID...'
    OnChange = cbxOIDsChange
  end
  object Edit1: TEdit
    Left = 64
    Top = 186
    Width = 121
    Height = 21
    Hint = 'Filter'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnChange = FilterOids
  end
  object edtExtension: TEdit
    Left = 72
    Top = 48
    Width = 385
    Height = 21
    TabOrder = 3
    Text = 'edtExtension'
    OnChange = edtExtensionChange
  end
  object edtName: TEdit
    Left = 72
    Top = 88
    Width = 385
    Height = 21
    TabOrder = 4
    Text = 'edtName'
    OnChange = edtNameChange
  end
  object edtRoot: TEdit
    Left = 72
    Top = 8
    Width = 385
    Height = 21
    TabOrder = 5
    Text = 'edtExtension'
    OnChange = edtRootChange
  end
end
