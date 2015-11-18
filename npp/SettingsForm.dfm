inherited SettingForm: TSettingForm
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'FHIR NPP Plugin'
  ClientHeight = 317
  ClientWidth = 617
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 623
  ExplicitHeight = 346
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
    Width = 617
    Height = 276
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 609
        Height = 248
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          609
          248)
        object Label5: TLabel
          Left = 10
          Top = 212
          Width = 337
          Height = 13
          Caption = 
            'The changes on this page only take effect when notepad++ is rest' +
            'arted'
        end
        object GroupBox1: TGroupBox
          Left = -2
          Top = 6
          Width = 606
          Height = 91
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Terminology Server '
          TabOrder = 0
          DesignSize = (
            606
            91)
          object Label1: TLabel
            Left = 12
            Top = 24
            Width = 25
            Height = 13
            Caption = 'URL:'
          end
          object Label2: TLabel
            Left = 12
            Top = 50
            Width = 577
            Height = 31
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'The server must be a FHIR terminology server running the same ve' +
              'rsion of FHIR as this client, and must not require authenticatio' +
              'n'
            WordWrap = True
            ExplicitWidth = 397
          end
          object edtServer: TEdit
            Left = 50
            Top = 21
            Width = 539
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'edtServer'
          end
        end
        object GroupBox2: TGroupBox
          Left = -2
          Top = 103
          Width = 606
          Height = 91
          Anchors = [akLeft, akTop, akRight]
          Caption = ' Definitions Source '
          TabOrder = 1
          DesignSize = (
            606
            91)
          object Label3: TLabel
            Left = 12
            Top = 26
            Width = 19
            Height = 13
            Caption = 'File:'
          end
          object Label4: TLabel
            Left = 12
            Top = 48
            Width = 577
            Height = 31
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'This is the validation.min.xml.zip file downloaded from the spec' +
              'ification (downloads page). This also must have the same version'
            WordWrap = True
            ExplicitWidth = 397
          end
          object SpeedButton1: TSpeedButton
            Left = 560
            Top = 18
            Width = 29
            Height = 28
            Anchors = [akTop, akRight]
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
              078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
              BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
              CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
              39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
              D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
              DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
              1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
              DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
              EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
              77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
              E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
              8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
              9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
              BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
              FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
              FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
            OnClick = SpeedButton1Click
            ExplicitLeft = 563
          end
          object edtFile: TEdit
            Left = 50
            Top = 21
            Width = 509
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'Edit1'
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Misc'
      ImageIndex = 2
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 609
        Height = 248
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          609
          248)
        object GroupBox3: TGroupBox
          Left = 0
          Top = 8
          Width = 606
          Height = 67
          Anchors = [akLeft, akTop, akRight]
          Caption = '  Path Summary Dialog  '
          TabOrder = 0
          DesignSize = (
            606
            67)
          object cbPathSummary: TCheckBox
            Left = 12
            Top = 27
            Width = 576
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show Summary Dialog after evaluating a FHIR Path'
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
      ExplicitWidth = 612
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 609
        Height = 37
        Align = alTop
        BevelOuter = bvLowered
        ParentBackground = False
        TabOrder = 0
        ExplicitWidth = 612
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
      object vtServers: TVirtualStringTree
        Left = 0
        Top = 37
        Width = 609
        Height = 211
        Align = alClient
        Header.AutoSizeIndex = 1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        TabOrder = 1
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnGetText = vtServersGetText
        ExplicitWidth = 612
        Columns = <
          item
            Position = 0
            Width = 150
            WideText = 'Name'
          end
          item
            Position = 1
            Width = 359
            WideText = 'URL'
          end
          item
            Position = 2
            Width = 100
            WideText = 'Smart on FHIR'
          end>
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 276
    Width = 617
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      617
      41)
    object Button1: TButton
      Left = 453
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
      Left = 534
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
    Left = 96
    Top = 398
  end
end
