inherited SettingForm: TSettingForm
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'FHIR notepad++ Plugin Settings'
  ClientHeight = 414
  ClientWidth = 699
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 705
  ExplicitHeight = 443
  PixelsPerInch = 96
  TextHeight = 13
  object TButton
    Left = 8
    Top = 206
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 0
    OnClick = btnEditAsTextClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 699
    Height = 373
    ActivePage = TabSheet2
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 691
        Height = 345
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        object GroupBox1: TGroupBox
          Left = 0
          Top = 52
          Width = 691
          Height = 75
          Align = alTop
          Caption = ' Terminology Server '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          DesignSize = (
            691
            75)
          object Label1: TLabel
            Left = 12
            Top = 24
            Width = 25
            Height = 13
            Caption = 'URL:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label2: TLabel
            Left = 12
            Top = 50
            Width = 662
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'The server must be a FHIR terminology server running the same ve' +
              'rsion of FHIR as this client, and must not require authenticatio' +
              'n'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 659
          end
          object edtServer: TEdit
            Left = 50
            Top = 21
            Width = 624
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            Text = 'edtServer'
          end
        end
        object GroupBox2: TGroupBox
          Left = 0
          Top = 0
          Width = 691
          Height = 52
          Align = alTop
          Caption = '  FHIR Version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          object rbR3: TRadioButton
            Left = 18
            Top = 24
            Width = 79
            Height = 17
            Caption = 'R3'
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object rbR2: TRadioButton
            Left = 120
            Top = 24
            Width = 265
            Height = 17
            Caption = 'DSTU2'
            TabOrder = 1
          end
        end
        object GroupBox5: TGroupBox
          Left = 0
          Top = 127
          Width = 691
          Height = 193
          Align = alClient
          Caption = '  Additional Implementation Guides'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          DesignSize = (
            691
            193)
          object Label3: TLabel
            Left = 584
            Top = 102
            Width = 104
            Height = 79
            AutoSize = False
            Caption = 
              'This should be a list of validation packes downloaded from the r' +
              'elevant implementation guides'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object lbAdditional: TListBox
            Left = 12
            Top = 20
            Width = 566
            Height = 163
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
          end
          object btnAddIG: TButton
            Left = 584
            Top = 16
            Width = 90
            Height = 25
            Caption = 'Add File'
            TabOrder = 1
            OnClick = btnAddIGClick
          end
          object btnDeleteIG: TButton
            Left = 584
            Top = 71
            Width = 90
            Height = 25
            Caption = 'Delete'
            TabOrder = 2
          end
          object Button3: TButton
            Left = 584
            Top = 43
            Width = 90
            Height = 25
            Caption = 'Add URL'
            Enabled = False
            TabOrder = 3
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 320
          Width = 691
          Height = 25
          Align = alBottom
          BevelOuter = bvLowered
          TabOrder = 3
          object Label5: TLabel
            Left = 4
            Top = 4
            Width = 337
            Height = 13
            Caption = 
              'The changes on this page only take effect when notepad++ is rest' +
              'arted'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Misc'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 691
        Height = 345
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          691
          345)
        object GroupBox3: TGroupBox
          Left = 0
          Top = 8
          Width = 688
          Height = 67
          Anchors = [akLeft, akTop, akRight]
          Caption = '  Path Summary Dialog  '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          DesignSize = (
            688
            67)
          object cbPathSummary: TCheckBox
            Left = 12
            Top = 27
            Width = 658
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show Summary Dialog after evaluating a FHIR Path'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
        end
        object GroupBox4: TGroupBox
          Left = 0
          Top = 88
          Width = 688
          Height = 67
          Anchors = [akLeft, akTop, akRight]
          Caption = '  Validation Summary Dialog  '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          DesignSize = (
            688
            67)
          object cbValidationSummary: TCheckBox
            Left = 12
            Top = 27
            Width = 658
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show Summary Dialog after performing validation'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Servers'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 691
        Height = 37
        Align = alTop
        BevelOuter = bvLowered
        ParentBackground = False
        TabOrder = 0
        object btnAdd: TButton
          Left = 3
          Top = 6
          Width = 60
          Height = 25
          Caption = 'Add'
          TabOrder = 0
          OnClick = btnAddClick
        end
        object btnEdit: TButton
          Left = 69
          Top = 6
          Width = 60
          Height = 25
          Caption = 'Edit'
          TabOrder = 1
          OnClick = btnEditClick
        end
        object btnDelete: TButton
          Left = 135
          Top = 6
          Width = 60
          Height = 25
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteClick
        end
        object btnUp: TButton
          Left = 201
          Top = 6
          Width = 60
          Height = 25
          Caption = 'Up'
          TabOrder = 3
          OnClick = btnUpClick
        end
        object btnDown: TButton
          Left = 267
          Top = 6
          Width = 60
          Height = 25
          Caption = 'Down'
          TabOrder = 4
          OnClick = btnDownClick
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 373
    Width = 699
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      699
      41)
    object Button1: TButton
      Left = 535
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 616
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = Button2Click
    end
    object btnEditAsText: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Edit as Text'
      ModalResult = 1
      TabOrder = 2
      OnClick = btnEditAsTextClick
    end
  end
  object od: TOpenDialog
    DefaultExt = '.zip'
    Filter = '*.zip'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select Definitions Source'
    Left = 88
    Top = 378
  end
end
