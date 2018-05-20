object OidFetcherForm: TOidFetcherForm
  Left = 0
  Top = 0
  Caption = 'Update OID Information'
  ClientHeight = 116
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 265
    Height = 16
    Caption = 'Fetch OID information from HL7  Registry'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 75
    Height = 13
    Caption = 'Current Status:'
  end
  object lblStatus: TLabel
    Left = 105
    Top = 48
    Width = 93
    Height = 13
    Caption = 'Waiting for user....'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnFetch: TButton
    Left = 184
    Top = 82
    Width = 75
    Height = 25
    Caption = 'Fetch'
    Default = True
    TabOrder = 0
    OnClick = btnFetchClick
  end
  object btnCancel: TButton
    Left = 265
    Top = 82
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
