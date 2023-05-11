object SaveOnServerDialog: TSaveOnServerDialog
  Left = 0
  Top = 0
  Caption = 'Send Resource to Server'
  ClientHeight = 175
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 427
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      427
      41)
    object Label4: TLabel
      Left = 16
      Top = 8
      Width = 36
      Height = 13
      Caption = 'Server:'
    end
    object cbxServers: TComboBox
      Left = 58
      Top = 6
      Width = 280
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnSerevrs: TButton
      Left = 344
      Top = 4
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Manage'
      TabOrder = 1
      OnClick = btnSerevrsClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 130
    Width = 427
    Height = 45
    Align = alBottom
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      427
      45)
    object btnCancel: TButton
      Left = 342
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnSend: TButton
      Left = 258
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Send'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 1
      OnClick = btnSendClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 427
    Height = 89
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object rbPost: TRadioButton
      Left = 16
      Top = 6
      Width = 473
      Height = 17
      Caption = 'Server Assgned ID (ignore existing id)'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbPut: TRadioButton
      Left = 16
      Top = 29
      Width = 473
      Height = 17
      Caption = 'Use Existing ID  (server may reject this)'
      TabOrder = 1
    end
    object cbUpdate: TCheckBox
      Left = 16
      Top = 64
      Width = 473
      Height = 17
      Caption = 'Update resource being edited with response from server'
      TabOrder = 2
    end
  end
end
