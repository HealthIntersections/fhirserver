object EditRegisteredServerForm: TEditRegisteredServerForm
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
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Server Identity'
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
          Left = 62
          Top = 70
          Width = 207
          Height = 13
          Caption = 'The URL must be a FHIR server [base] URL'
          WordWrap = True
        end
        object Formt: TLabel
          Left = 18
          Top = 146
          Width = 38
          Height = 13
          Caption = 'Format:'
        end
        object Label20: TLabel
          Left = 18
          Top = 114
          Width = 39
          Height = 13
          Caption = 'Version:'
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
          Top = 143
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
          Caption = 'Check Server'
          TabOrder = 3
          OnClick = Button3Click
        end
        object cbxVersion: TComboBox
          Left = 63
          Top = 111
          Width = 365
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          Items.Strings = (
            'R2 (1.0.2)'
            'R3 (3.0.1)'
            'R4 (3.3.0)')
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Smart App Launch'
      ImageIndex = 1
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
          Left = 16
          Top = 209
          Width = 407
          Height = 29
          AutoSize = False
          Caption = 
            'Note: You must register the [client] with each Smart App Launch ' +
            'Server yourself'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 10
          Top = 272
          Width = 50
          Height = 13
          Caption = 'Authorize:'
        end
        object Label12: TLabel
          Left = 10
          Top = 299
          Width = 33
          Height = 13
          Caption = 'Token:'
        end
        object Label10: TLabel
          Left = 12
          Top = 250
          Width = 164
          Height = 13
          Caption = 'From the conformance statement:'
        end
        object Bevel1: TBevel
          Left = 10
          Top = 244
          Width = 377
          Height = 11
          Shape = bsTopLine
        end
        object Label15: TLabel
          Left = 12
          Top = 6
          Width = 103
          Height = 13
          Caption = 'Smart on FHIR Mode:'
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
          Left = 84
          Top = 296
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtNameChange
        end
        object edtAuthorize: TEdit
          Left = 84
          Top = 269
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = edtNameChange
        end
        object Notebook1: TNotebook
          Left = 12
          Top = 25
          Width = 423
          Height = 160
          PageIndex = 2
          TabOrder = 3
          object TPage
            Left = 0
            Top = 0
            Caption = 'Default'
            ExplicitWidth = 409
            ExplicitHeight = 201
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'oauth'
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'backend'
            object Notebook2: TNotebook
              Left = 0
              Top = 0
              Width = 423
              Height = 160
              PageIndex = 1
              TabOrder = 0
              object TPage
                Left = 0
                Top = 0
                Caption = 'Default'
                ExplicitWidth = 0
                ExplicitHeight = 0
              end
              object TPage
                Left = 0
                Top = 0
                Caption = 'oauth'
                object Label4: TLabel
                  Left = 13
                  Top = 15
                  Width = 41
                  Height = 13
                  Caption = 'Client ID'
                end
                object Label5: TLabel
                  Left = 13
                  Top = 45
                  Width = 61
                  Height = 13
                  Caption = 'Client Secret'
                end
                object Label6: TLabel
                  Left = 88
                  Top = 72
                  Width = 304
                  Height = 26
                  Caption = 
                    'Note: this app ([client]) is not a confidential app, but the cli' +
                    'ent secret can be provided to help test SMART App Launch Servers'
                  WordWrap = True
                end
                object Label7: TLabel
                  Left = 13
                  Top = 109
                  Width = 67
                  Height = 13
                  Caption = 'Redirect Port:'
                end
                object Label8: TLabel
                  Left = 88
                  Top = 136
                  Width = 258
                  Height = 13
                  Caption = 'The redirect URL must be http://localhost:[port]/done'
                end
                object edtClientId: TEdit
                  Left = 88
                  Top = 15
                  Width = 313
                  Height = 21
                  TabOrder = 0
                end
                object edtClientSecret: TEdit
                  Left = 88
                  Top = 42
                  Width = 313
                  Height = 21
                  TabOrder = 1
                end
                object edtRedirect: TEdit
                  Left = 88
                  Top = 109
                  Width = 313
                  Height = 21
                  TabOrder = 2
                end
              end
              object TPage
                Left = 0
                Top = 0
                Caption = 'backend'
                object Label9: TLabel
                  Left = 13
                  Top = 44
                  Width = 56
                  Height = 13
                  Caption = 'Issuer URL:'
                end
                object Label16: TLabel
                  Left = 13
                  Top = 67
                  Width = 59
                  Height = 13
                  Caption = 'Private Key:'
                end
                object Label17: TLabel
                  Left = 88
                  Top = 96
                  Width = 315
                  Height = 26
                  Caption = 
                    'Choose a generated key (e.g. using OpenSSL that is an RSA-256 pr' +
                    'ivate Key paired with the registered public key'
                  WordWrap = True
                end
                object Label18: TLabel
                  Left = 13
                  Top = 129
                  Width = 59
                  Height = 13
                  Caption = 'Passphrase:'
                end
                object Label19: TLabel
                  Left = 13
                  Top = 15
                  Width = 41
                  Height = 13
                  Caption = 'Client ID'
                end
                object edtIssuerURL: TEdit
                  Left = 88
                  Top = 42
                  Width = 321
                  Height = 21
                  TabOrder = 0
                end
                object edtPrivateKey: TEdit
                  Left = 88
                  Top = 69
                  Width = 321
                  Height = 21
                  TabOrder = 1
                end
                object edtPassphrase: TEdit
                  Left = 88
                  Top = 128
                  Width = 315
                  Height = 21
                  TabOrder = 2
                end
                object edtClientId1: TEdit
                  Left = 88
                  Top = 15
                  Width = 321
                  Height = 21
                  TabOrder = 3
                end
              end
            end
          end
        end
        object cbxSmartMode: TComboBox
          Left = 121
          Top = 2
          Width = 304
          Height = 21
          Style = csDropDownList
          TabOrder = 4
          OnChange = cbxSmartModeChange
          OnClick = cbxSmartModeChange
          Items.Strings = (
            'No Smart App Launch'
            'OAuth Client'
            'Backend Services Client')
        end
        object Button4: TButton
          Left = 120
          Top = 344
          Width = 75
          Height = 25
          Caption = 'Button4'
          TabOrder = 5
          OnClick = Button4Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'CDS-Hooks'
      ImageIndex = 2
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
