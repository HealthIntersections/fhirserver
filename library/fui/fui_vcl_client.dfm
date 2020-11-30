object RegisterClientForm: TRegisterClientForm
  Left = 0
  Top = 0
  Caption = 'RegisterClientForm'
  ClientHeight = 619
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 39
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 67
    Width = 45
    Height = 13
    Caption = 'Logo URL'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 86
    Width = 552
    Height = 17
    Shape = bsBottomLine
  end
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 120
    Height = 13
    Caption = 'Client OAuth Settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 152
    Width = 30
    Height = 13
    Caption = 'Mode:'
  end
  object Label6: TLabel
    Left = 8
    Top = 183
    Width = 49
    Height = 13
    Caption = 'Redirects:'
  end
  object Label7: TLabel
    Left = 72
    Top = 275
    Width = 473
    Height = 70
    AutoSize = False
    Caption = 
      'Each line must have the format http(s)://[local]:port/done where' +
      ' [local] is your selected name for your own PC (usually '#39'localho' +
      'st'#39'), and Port is a number you'#39'll choose between 1024 and 65534'
    WordWrap = True
  end
  object edtName: TEdit
    Left = 72
    Top = 8
    Width = 481
    Height = 21
    TabOrder = 0
    Text = 'edtName'
  end
  object edtUrl: TEdit
    Left = 72
    Top = 36
    Width = 481
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object edtLogo: TEdit
    Left = 72
    Top = 64
    Width = 481
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object memRedirects: TMemo
    Left = 72
    Top = 180
    Width = 481
    Height = 89
    Lines.Strings = (
      'memRedirects')
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 578
    Width = 568
    Height = 41
    Align = alBottom
    TabOrder = 4
    object btnOk: TButton
      Left = 400
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOkClick
    end
    object Button2: TButton
      Left = 481
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 1
    end
  end
  object cbxConfidential: TComboBox
    Left = 72
    Top = 148
    Width = 481
    Height = 21
    TabOrder = 5
    Text = 'cbxConfidential'
    Items.Strings = (
      'Confidential Client'
      'Public Client (no secret)')
  end
end
