object frmNewServer: TfrmNewServer
  Left = 0
  Top = 0
  Caption = 'frmNewServer'
  ClientHeight = 129
  ClientWidth = 542
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
    Top = 88
    Width = 542
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 248
    ExplicitTop = 150
    ExplicitWidth = 185
    DesignSize = (
      542
      41)
    object Button1: TButton
      Left = 377
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 465
    end
    object Button2: TButton
      Left = 458
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 546
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 88
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 196
    ExplicitTop = 86
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Label1: TLabel
      Left = 26
      Top = 22
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object Label2: TLabel
      Left = 26
      Top = 58
      Width = 43
      Height = 13
      Caption = 'Address:'
    end
    object edtName: TEdit
      Left = 84
      Top = 20
      Width = 445
      Height = 21
      TabOrder = 0
    end
    object edtAddress: TEdit
      Left = 84
      Top = 55
      Width = 445
      Height = 21
      TabOrder = 1
    end
  end
end
