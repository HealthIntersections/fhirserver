object ConceptLookupForm: TConceptLookupForm
  Left = 0
  Top = 0
  Caption = 'Concept Lookup'
  ClientHeight = 489
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 452
    Width = 566
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      566
      37)
    object lblServerError: TLabel
      Left = 10
      Top = 0
      Width = 384
      Height = 35
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object btnOk: TButton
      Left = 400
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = btnOkClick
    end
    object Button2: TButton
      Left = 481
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 452
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 1
    ExplicitLeft = 202
    ExplicitTop = 64
    ExplicitWidth = 13
    ExplicitHeight = 3
    object Panel3: TPanel
      Left = 10
      Top = 10
      Width = 546
      Height = 105
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        546
        105)
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 35
        Height = 13
        Caption = 'Filters'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 0
        Top = 24
        Width = 49
        Height = 13
        Caption = 'Value Set:'
      end
      object Label3: TLabel
        Left = 0
        Top = 50
        Width = 53
        Height = 13
        Caption = 'Text Filter:'
      end
      object Label4: TLabel
        Left = 1
        Top = 86
        Width = 48
        Height = 13
        Caption = 'Matches'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbxValueSets: TComboBox
        Left = 58
        Top = 21
        Width = 485
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbxValueSetsChange
      end
      object edtFilter: TEdit
        Left = 59
        Top = 48
        Width = 484
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = edtFilterChange
      end
    end
    object tvExpansion: TVirtualStringTree
      Left = 10
      Top = 115
      Width = 546
      Height = 327
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 2
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      NodeDataSize = 4
      ParentFont = False
      TabOrder = 1
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnAddToSelection = tvExpansionAddToSelection
      OnColumnResize = tvExpansionColumnResize
      OnCompareNodes = tvExpansionCompareNodes
      OnDblClick = tvExpansionDblClick
      OnGetText = tvExpansionGetText
      OnInitChildren = tvExpansionInitChildren
      OnInitNode = tvExpansionInitNode
      ExplicitLeft = 0
      ExplicitTop = 121
      ExplicitWidth = 479
      ExplicitHeight = 320
      Columns = <
        item
          Position = 0
          Width = 250
          WideText = 'System'
        end
        item
          Position = 1
          Width = 115
          WideText = 'Code'
        end
        item
          Position = 2
          Width = 177
          WideText = 'Display'
        end>
    end
  end
end
