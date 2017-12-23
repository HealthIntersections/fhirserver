object MainWindowForm: TMainWindowForm
  Left = 0
  Top = 0
  Caption = 'VCL FHIR Demo'
  ClientHeight = 690
  ClientWidth = 1012
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 111
    Width = 0
    Height = 579
    ExplicitLeft = 289
    ExplicitTop = 65
    ExplicitHeight = 271
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 1012
    Height = 70
    Align = alTop
    TabOrder = 0
    object pnlPatientLeft: TPanel
      Left = 1
      Top = 1
      Width = 304
      Height = 68
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        304
        68)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label2: TLabel
        Left = 8
        Top = 27
        Width = 23
        Height = 13
        Caption = 'DoB:'
      end
      object Label3: TLabel
        Left = 8
        Top = 46
        Width = 39
        Height = 13
        Caption = 'Gender:'
      end
      object lblName: TLabel
        Left = 56
        Top = 8
        Width = 225
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDoB: TLabel
        Left = 56
        Top = 27
        Width = 225
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
      end
      object lblGender: TLabel
        Left = 57
        Top = 46
        Width = 225
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
      end
    end
    object pnlPatientRight: TPanel
      Left = 305
      Top = 1
      Width = 706
      Height = 68
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        706
        68)
      object Label4: TLabel
        Left = 6
        Top = 8
        Width = 26
        Height = 13
        Caption = 'MRN:'
      end
      object lblMRN: TLabel
        Left = 54
        Top = 8
        Width = 572
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 195
      end
      object Label6: TLabel
        Left = 6
        Top = 27
        Width = 43
        Height = 13
        Caption = 'Address:'
      end
      object lblAddress: TLabel
        Left = 54
        Top = 27
        Width = 572
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 195
      end
      object Label8: TLabel
        Left = 6
        Top = 46
        Width = 34
        Height = 13
        Caption = 'Phone:'
      end
      object lblPhone: TLabel
        Left = 55
        Top = 46
        Width = 571
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 194
      end
      object imgPatient: TImage
        Left = 632
        Top = 3
        Width = 60
        Height = 60
        Anchors = [akTop, akRight]
        Proportional = True
        Stretch = True
        ExplicitLeft = 255
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 111
    Width = 1012
    Height = 579
    Align = alClient
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 609
      Top = 1
      Width = 6
      Height = 577
      ExplicitLeft = 409
      ExplicitHeight = 618
    end
    object pnlLists: TPanel
      Left = 1
      Top = 1
      Width = 608
      Height = 577
      Align = alLeft
      TabOrder = 0
      OnResize = pnlListsResize
      object Splitter3: TSplitter
        Left = 1
        Top = 193
        Width = 606
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 234
        ExplicitWidth = 383
      end
      object pnlAllergiesList: TPanel
        Left = 1
        Top = 1
        Width = 606
        Height = 192
        Align = alTop
        BevelOuter = bvNone
        Caption = 'pnlAllergiesList'
        TabOrder = 0
        OnResize = pnlAllergiesListResize
        DesignSize = (
          606
          192)
        object lblAllergiesMessage: TLabel
          Left = 11
          Top = 168
          Width = 596
          Height = 13
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          ParentShowHint = False
          ShowHint = True
          ExplicitTop = 152
        end
        object Label5: TLabel
          Left = 8
          Top = 8
          Width = 49
          Height = 13
          Caption = 'Allergies'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object vtAllergies: TVirtualStringTree
          Left = 4
          Top = 27
          Width = 596
          Height = 134
          Anchors = [akLeft, akTop, akRight, akBottom]
          DefaultNodeHeight = 20
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'DejaVu Sans Mono'
          Font.Style = []
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          Header.SortColumn = 0
          HintMode = hmHint
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toAlwaysSelectNode]
          OnColumnResize = vtAllergiesColumnResize
          OnGetCellText = vtAllergiesGetCellText
          OnGetHint = vtAllergiesGetHint
          OnHeaderClick = vtAllergiesHeaderClick
          OnNodeClick = vtAllergiesNodeClick
          Columns = <
            item
              Position = 0
              Width = 70
              WideText = 'Onset'
            end
            item
              Position = 1
              Width = 80
              WideText = 'Status'
            end
            item
              Position = 2
              Width = 80
              WideText = 'Criticality'
            end
            item
              Position = 3
              Width = 80
              WideText = 'Category'
            end
            item
              Position = 4
              Width = 150
              WideText = 'Substance'
            end
            item
              Position = 5
              Width = 150
              WideText = 'Manifestation'
            end
            item
              Position = 6
              Width = 80
              WideText = 'Recorded'
            end>
        end
      end
      object pnlMedsList: TPanel
        Left = 1
        Top = 196
        Width = 606
        Height = 380
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          606
          380)
        object Label7: TLabel
          Left = 4
          Top = 16
          Width = 68
          Height = 13
          Caption = 'Medications'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblMedicationsMessage: TLabel
          Left = 4
          Top = 359
          Width = 596
          Height = 13
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          ParentShowHint = False
          ShowHint = True
          ExplicitTop = 400
        end
        object vtMedications: TVirtualStringTree
          Left = 5
          Top = 35
          Width = 596
          Height = 318
          Anchors = [akLeft, akTop, akRight, akBottom]
          DefaultNodeHeight = 20
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'DejaVu Sans Mono'
          Font.Style = []
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          Header.SortColumn = 0
          HintMode = hmHint
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect, toAlwaysSelectNode]
          OnColumnResize = vtMedicationsColumnResize
          OnGetCellText = vtMedicationsGetCellText
          OnGetHint = vtMedicationsGetHint
          OnHeaderClick = vtMedicationsHeaderClick
          OnNodeClick = vtMedicationsNodeClick
          Columns = <
            item
              Position = 0
              Width = 70
              WideText = 'Type'
            end
            item
              Position = 1
              Width = 80
              WideText = 'Date'
            end
            item
              Position = 2
              Width = 80
              WideText = 'End Date'
            end
            item
              Position = 3
              Width = 80
              WideText = 'Status'
            end
            item
              Position = 4
              Width = 150
              WideText = 'Medication'
            end
            item
              Position = 5
              Width = 150
              WideText = 'Not Taken'
            end
            item
              Position = 6
              Width = 80
              WideText = 'Source'
            end
            item
              Position = 7
              WideText = 'Reason'
            end>
        end
      end
    end
    object Panel3: TPanel
      Left = 615
      Top = 1
      Width = 396
      Height = 577
      Align = alClient
      TabOrder = 1
      object pgDisplay: TPageControl
        Left = 1
        Top = 1
        Width = 394
        Height = 575
        ActivePage = tabValues
        Align = alClient
        TabOrder = 0
        object tabNarrative: TTabSheet
          Caption = 'Narrative'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object webNarrative: TWebBrowser
            Left = 0
            Top = 0
            Width = 386
            Height = 547
            Align = alClient
            TabOrder = 0
            ExplicitHeight = 588
            ControlData = {
              4C000000E5270000893800000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
        object tabValues: TTabSheet
          Caption = 'Data'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object vtDisplay: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 386
            Height = 547
            Align = alClient
            DefaultNodeHeight = 20
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'DejaVu Sans Mono'
            Font.Style = []
            Header.AutoSizeIndex = 1
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Height = 17
            Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
            NodeDataSize = 8
            ParentFont = False
            TabOrder = 0
            OnGetCellText = vtDisplayGetCellText
            OnInitNode = vtDisplayInitNode
            Columns = <
              item
                Color = clInfoBk
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable]
                Position = 0
                Width = 120
                WideText = 'Name'
              end
              item
                Position = 1
                Width = 266
                WideText = 'Value'
              end>
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1012
    Height = 41
    Align = alTop
    TabOrder = 2
    object Button1: TButton
      Left = 13
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Pt Lookup'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
