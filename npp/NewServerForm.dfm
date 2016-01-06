object RegisterServerForm: TRegisterServerForm
  Left = 0
  Top = 0
  Caption = 'Register a new Server'
  ClientHeight = 445
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 449
    Height = 404
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Server Identity'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 376
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        DesignSize = (
          441
          376)
        object Label3: TLabel
          Left = 14
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
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
        object Formt: TLabel
          Left = 14
          Top = 122
          Width = 38
          Height = 13
          Caption = 'Format:'
        end
        object edtName: TEdit
          Left = 62
          Top = 13
          Width = 365
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = edtNameChange
        end
        object edtServer: TEdit
          Left = 62
          Top = 43
          Width = 365
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtNameChange
        end
        object cbxFormat: TComboBox
          Left = 62
          Top = 119
          Width = 365
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Items.Strings = (
            'Whatever'
            'XML'
            'JSON')
        end
        object Button3: TButton
          Left = 6
          Top = 347
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Check Format'
          TabOrder = 3
          OnClick = Button3Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Smart on FHIR'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 376
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        DesignSize = (
          441
          376)
        object Label13: TLabel
          Left = 12
          Top = 262
          Width = 369
          Height = 29
          AutoSize = False
          Caption = 
            'Note: Other than the reference server, you must register the plu' +
            'g-in with each SMART-ON-FHIR server yourself'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 12
          Top = 203
          Width = 50
          Height = 13
          Caption = 'Authorize:'
        end
        object Label12: TLabel
          Left = 12
          Top = 230
          Width = 33
          Height = 13
          Caption = 'Token:'
        end
        object Label10: TLabel
          Left = 12
          Top = 181
          Width = 164
          Height = 13
          Caption = 'From the conformance statement:'
        end
        object Bevel1: TBevel
          Left = 12
          Top = 168
          Width = 377
          Height = 11
          Shape = bsTopLine
        end
        object Label9: TLabel
          Left = 86
          Top = 135
          Width = 258
          Height = 13
          Caption = 'The redirect URL must be http://localhost:[port]/done'
        end
        object Label8: TLabel
          Left = 12
          Top = 115
          Width = 67
          Height = 13
          Caption = 'Redirect Port:'
        end
        object Label7: TLabel
          Left = 86
          Top = 79
          Width = 284
          Height = 26
          Caption = 
            'Note: the Notepad++ plug-in is not a confidential app, but this ' +
            'can be provided to help test SMART on FHIR Servers'
          WordWrap = True
        end
        object Label6: TLabel
          Left = 12
          Top = 61
          Width = 65
          Height = 13
          Caption = 'Client Secret:'
        end
        object Label4: TLabel
          Left = 12
          Top = 29
          Width = 44
          Height = 13
          Caption = 'Client Id:'
        end
        object Label5: TLabel
          Left = 12
          Top = 7
          Width = 139
          Height = 13
          Caption = 'From your client registration:'
        end
        object btnFetch: TButton
          Left = 4
          Top = 345
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Fetch Endpoints'
          TabOrder = 0
          OnClick = btnFetchClick
        end
        object edtToken: TEdit
          Left = 86
          Top = 227
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtNameChange
        end
        object edtAuthorize: TEdit
          Left = 86
          Top = 200
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = edtNameChange
        end
        object edtRedirect: TEdit
          Left = 86
          Top = 112
          Width = 55
          Height = 21
          TabOrder = 3
          OnChange = edtNameChange
        end
        object edtClientSecret: TEdit
          Left = 86
          Top = 58
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = edtNameChange
        end
        object edtClientId: TEdit
          Left = 86
          Top = 26
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = edtNameChange
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'CDS-Hooks'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 376
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        DesignSize = (
          441
          376)
        object Label14: TLabel
          Left = 6
          Top = 10
          Width = 212
          Height = 13
          Caption = 'Use this server for the following CDS Hooks:'
        end
        object CheckBox1: TCheckBox
          Left = 14
          Top = 335
          Width = 421
          Height = 17
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Only use this server when connected'
          TabOrder = 0
        end
        object clHooks: TCheckListBox
          Left = 14
          Top = 32
          Width = 413
          Height = 286
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          Items.Strings = (
            'terminology-info (look up information about a code)'
            'identifier-info (look up information about an identifier)')
          TabOrder = 1
        end
        object Button1: TButton
          Left = 326
          Top = 331
          Width = 99
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Query Server'
          TabOrder = 2
          OnClick = Button1Click
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 404
    Width = 449
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      449
      41)
    object btnOk: TButton
      Left = 284
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object Button2: TButton
      Left = 365
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
