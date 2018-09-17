object ServerLoginForm: TServerLoginForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Server Login'
  ClientHeight = 223
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 182
    Width = 467
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      467
      41)
    object btnLogin: TButton
      Left = 303
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Login'
      Default = True
      TabOrder = 0
      OnClick = btnLoginClick
    end
    object btnCancel: TButton
      Left = 384
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object chkInProgress: TCheckBox
      Left = 8
      Top = 14
      Width = 97
      Height = 17
      Caption = 'In Process'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 467
    Height = 182
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      467
      182)
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Server:'
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 44
      Height = 13
      Caption = 'Client Id:'
    end
    object Label6: TLabel
      Left = 8
      Top = 80
      Width = 67
      Height = 13
      Caption = 'Redirect Port:'
    end
    object Label1: TLabel
      Left = 8
      Top = 116
      Width = 51
      Height = 13
      Caption = 'Patient Id:'
    end
    object Label3: TLabel
      Left = 7
      Top = 152
      Width = 100
      Height = 13
      Caption = 'Authorization Scope:'
    end
    object cbxServer: TComboBox
      Left = 80
      Top = 13
      Width = 380
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 
        'https://fhir-ehr.sandboxcerner.com/dstu2/0b8a0111-e8e6-4c26-a91c' +
        '-5069cbc6b1ca'
      Items.Strings = (
        
          'https://fhir-ehr.sandboxcerner.com/dstu2/0b8a0111-e8e6-4c26-a91c' +
          '-5069cbc6b1ca'
        'https://test.fhir.org/r2')
    end
    object edtRedirectPort: TEdit
      Left = 81
      Top = 77
      Width = 276
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '32411'
    end
    object edtPatientId: TEdit
      Left = 80
      Top = 113
      Width = 380
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = '4342008'
    end
    object CheckBox1: TCheckBox
      Left = 365
      Top = 76
      Width = 95
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Web Redirector'
      TabOrder = 3
    end
    object cbAuthScope: TComboBox
      Left = 120
      Top = 149
      Width = 145
      Height = 21
      ItemIndex = 1
      TabOrder = 4
      Text = 'User'
      Items.Strings = (
        'System'
        'User')
    end
    object cbClientId: TComboBox
      Left = 81
      Top = 45
      Width = 379
      Height = 21
      ItemIndex = 0
      TabOrder = 5
      Text = 'c24732fd-9717-4972-8d63-a1a74986f420'
      Items.Strings = (
        'c24732fd-9717-4972-8d63-a1a74986f420'
        '1db72b5d-5233-494c-aa1e-452a9a56321e')
    end
  end
end
