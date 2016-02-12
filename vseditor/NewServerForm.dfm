object frmNewServer: TfrmNewServer
  Left = 0
  Top = 0
  Caption = 'frmNewServer'
  ClientHeight = 192
  ClientWidth = 540
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 151
    Width = 540
    Height = 41
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      540
      41)
    object Button1: TButton
      Left = 375
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 456
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 151
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      540
      151)
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
    object Label3: TLabel
      Left = 26
      Top = 92
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object Label4: TLabel
      Left = 26
      Top = 128
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtName: TEdit
      Left = 84
      Top = 20
      Width = 445
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edtAddress: TEdit
      Left = 84
      Top = 55
      Width = 445
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edtUsername: TEdit
      Left = 84
      Top = 89
      Width = 445
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object edtPassword: TEdit
      Left = 84
      Top = 125
      Width = 393
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 3
    end
    object CheckBox1: TCheckBox
      Left = 483
      Top = 128
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Show'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
  end
end
