object PackageFinderForm: TPackageFinderForm
  Left = 0
  Top = 0
  Caption = 'Package Finder'
  ClientHeight = 472
  ClientWidth = 860
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
    Width = 860
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      860
      41)
    object Label1: TLabel
      Left = 12
      Top = 10
      Width = 24
      Height = 13
      Caption = 'Filter'
    end
    object edtFilter: TEdit
      Left = 46
      Top = 7
      Width = 804
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtFilterChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 432
    Width = 860
    Height = 40
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      860
      40)
    object Label2: TLabel
      Left = 12
      Top = 10
      Width = 36
      Height = 13
      Caption = 'Server:'
    end
    object lblDownload: TLabel
      Left = 215
      Top = 2
      Width = 57
      Height = 13
      Caption = 'lblDownload'
      Visible = False
    end
    object cbxServer: TComboBox
      Left = 64
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Primary Server'
      OnChange = cbxServerChange
      Items.Strings = (
        'Primary Server'
        'Secondary Server'
        'CI Build')
    end
    object btnClose: TButton
      Left = 777
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 1
    end
    object btnInstall: TButton
      Left = 681
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Install'
      TabOrder = 2
      OnClick = btnInstallClick
    end
    object pbDownload: TProgressBar
      Left = 215
      Top = 15
      Width = 370
      Height = 17
      TabOrder = 3
      Visible = False
    end
    object btnCancel: TButton
      Left = 591
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      Visible = False
      OnClick = btnCancelClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 860
    Height = 391
    Align = alClient
    Caption = 'Panel3'
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 2
    object grid: TVirtualStringTree
      Left = 11
      Top = 11
      Width = 838
      Height = 369
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnAddToSelection = gridAddToSelection
      OnDblClick = btnInstallClick
      OnGetText = gridGetText
      OnHeaderClick = gridHeaderClick
      OnRemoveFromSelection = gridRemoveFromSelection
      ExplicitLeft = 12
      ExplicitTop = 16
      Columns = <
        item
          Position = 0
          Text = 'Package Id'
          Width = 120
        end
        item
          Position = 1
          Text = 'Version'
          Width = 60
        end
        item
          Position = 2
          Text = 'Description'
          Width = 150
        end
        item
          Position = 3
          Text = 'Canonical'
          Width = 350
        end
        item
          Position = 4
          Text = 'FHIR Version'
          Width = 60
        end
        item
          Position = 5
          Text = 'Date'
          Width = 80
        end>
    end
  end
end
