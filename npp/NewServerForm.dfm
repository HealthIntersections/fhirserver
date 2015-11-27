object RegisterServerForm: TRegisterServerForm
  Left = 0
  Top = 0
  Caption = 'Register a new Server'
  ClientHeight = 400
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 411
    Height = 359
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 146
    ExplicitTop = 104
    ExplicitWidth = 463
    ExplicitHeight = 439
    object TabSheet1: TTabSheet
      Caption = 'Server Identity'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 403
        Height = 331
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        ExplicitLeft = 8
        ExplicitTop = 238
        ExplicitWidth = 444
        ExplicitHeight = 113
        DesignSize = (
          403
          331)
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
        object edtName: TEdit
          Left = 52
          Top = 13
          Width = 337
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = edtNameChange
          ExplicitWidth = 359
        end
        object edtServer: TEdit
          Left = 52
          Top = 43
          Width = 337
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtNameChange
          ExplicitWidth = 359
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Smart on FHIR'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 403
        Height = 331
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        ExplicitLeft = 254
        ExplicitTop = 340
        ExplicitWidth = 185
        ExplicitHeight = 41
        DesignSize = (
          403
          331)
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
          Top = 300
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Fetch Endpoints'
          TabOrder = 0
          OnClick = btnFetchClick
          ExplicitTop = 348
        end
        object edtToken: TEdit
          Left = 86
          Top = 227
          Width = 301
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtNameChange
        end
        object edtAuthorize: TEdit
          Left = 86
          Top = 200
          Width = 301
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
          Width = 301
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = edtNameChange
        end
        object edtClientId: TEdit
          Left = 86
          Top = 26
          Width = 301
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
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 403
        Height = 331
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        ExplicitLeft = 240
        ExplicitTop = 184
        ExplicitWidth = 185
        ExplicitHeight = 41
        DesignSize = (
          403
          331)
        object Label14: TLabel
          Left = 6
          Top = 10
          Width = 212
          Height = 13
          Caption = 'Use this server for the following CDS Hooks:'
        end
        object CheckBox1: TCheckBox
          Left = 6
          Top = 290
          Width = 383
          Height = 17
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Only use this server when connected'
          TabOrder = 0
          ExplicitTop = 338
          ExplicitWidth = 385
        end
        object CheckListBox1: TCheckListBox
          Left = 14
          Top = 32
          Width = 375
          Height = 241
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          TabOrder = 1
          ExplicitWidth = 377
          ExplicitHeight = 289
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 359
    Width = 411
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitLeft = 256
    ExplicitTop = 562
    ExplicitWidth = 185
    DesignSize = (
      411
      41)
    object btnOk: TButton
      Left = 246
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 494
    end
    object Button2: TButton
      Left = 327
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 575
    end
  end
end
