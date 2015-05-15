object frmRegisterServer: TfrmRegisterServer
  Left = 0
  Top = 0
  Caption = 'Register FHIR Server'
  ClientHeight = 139
  ClientWidth = 627
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
    Top = 0
    Width = 627
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 632
    DesignSize = (
      627
      37)
    object Label1: TLabel
      Left = 16
      Top = 10
      Width = 35
      Height = 13
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtName: TEdit
      Left = 76
      Top = 7
      Width = 539
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 37
    Width = 627
    Height = 44
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 632
    DesignSize = (
      627
      44)
    object Label2: TLabel
      Left = 15
      Top = 12
      Width = 49
      Height = 13
      Caption = 'Address:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtAddress: TEdit
      Left = 76
      Top = 7
      Width = 539
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object btnOpenFile: TButton
    Left = 535
    Top = 96
    Width = 80
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
    WordWrap = True
  end
  object Button1: TButton
    Left = 436
    Top = 96
    Width = 93
    Height = 33
    Caption = 'Add'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
    WordWrap = True
  end
end
