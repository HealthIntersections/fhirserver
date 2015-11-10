object RegisterServerForm: TRegisterServerForm
  Left = 0
  Top = 0
  Caption = 'Register a new Server'
  ClientHeight = 456
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    424
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 46
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object Label2: TLabel
    Left = 52
    Top = 70
    Width = 207
    Height = 13
    Caption = 'The URL must be a FHIR server [base] URL'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 14
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object btnOk: TButton
    Left = 255
    Top = 421
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
  end
  object Button2: TButton
    Left = 336
    Top = 421
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtServer: TEdit
    Left = 52
    Top = 43
    Width = 359
    Height = 21
    TabOrder = 2
    OnChange = edtNameChange
  end
  object edtName: TEdit
    Left = 52
    Top = 13
    Width = 359
    Height = 21
    TabOrder = 3
    OnChange = edtNameChange
  end
  object GroupBox1: TGroupBox
    Left = 14
    Top = 93
    Width = 397
    Height = 314
    Caption = 'SMART on FHIR'
    TabOrder = 4
    object Label4: TLabel
      Left = 12
      Top = 45
      Width = 44
      Height = 13
      Caption = 'Client Id:'
    end
    object Label5: TLabel
      Left = 12
      Top = 23
      Width = 139
      Height = 13
      Caption = 'From your client registration:'
    end
    object Label6: TLabel
      Left = 12
      Top = 77
      Width = 65
      Height = 13
      Caption = 'Client Secret:'
    end
    object Label7: TLabel
      Left = 86
      Top = 95
      Width = 284
      Height = 26
      Caption = 
        'Note: the Notepad++ plug-in is not a confidential app, but this ' +
        'can be provided to help test SMART on FHIR Servers'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 12
      Top = 131
      Width = 67
      Height = 13
      Caption = 'Redirect Port:'
    end
    object Label9: TLabel
      Left = 86
      Top = 151
      Width = 258
      Height = 13
      Caption = 'The redirect URL must be http://localhost:[port]/done'
    end
    object Bevel1: TBevel
      Left = 12
      Top = 184
      Width = 377
      Height = 11
      Shape = bsTopLine
    end
    object Label10: TLabel
      Left = 12
      Top = 197
      Width = 164
      Height = 13
      Caption = 'From the conformance statement:'
    end
    object Label11: TLabel
      Left = 12
      Top = 219
      Width = 50
      Height = 13
      Caption = 'Authorize:'
    end
    object Label12: TLabel
      Left = 12
      Top = 246
      Width = 33
      Height = 13
      Caption = 'Token:'
    end
    object Label13: TLabel
      Left = 12
      Top = 278
      Width = 369
      Height = 29
      AutoSize = False
      Caption = 
        'Note: Other than the reference server, you must register the plu' +
        'g-in with each SMART-ON-FHIR server yourself'
      WordWrap = True
    end
    object edtClientId: TEdit
      Left = 86
      Top = 42
      Width = 303
      Height = 21
      TabOrder = 0
      OnChange = edtNameChange
    end
    object edtClientSecret: TEdit
      Left = 86
      Top = 74
      Width = 303
      Height = 21
      TabOrder = 1
      OnChange = edtNameChange
    end
    object edtRedirect: TEdit
      Left = 86
      Top = 128
      Width = 55
      Height = 21
      TabOrder = 2
      OnChange = edtNameChange
    end
    object edtAuthorize: TEdit
      Left = 86
      Top = 216
      Width = 303
      Height = 21
      TabOrder = 3
      OnChange = edtNameChange
    end
    object edtToken: TEdit
      Left = 86
      Top = 243
      Width = 303
      Height = 21
      TabOrder = 4
      OnChange = edtNameChange
    end
  end
  object btnFetch: TButton
    Left = 14
    Top = 421
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Fetch Endpoints'
    TabOrder = 5
    OnClick = btnFetchClick
  end
end
