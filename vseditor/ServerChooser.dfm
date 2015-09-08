object ServerChooserForm: TServerChooserForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Value Set Choose'
  ClientHeight = 562
  ClientWidth = 784
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
  object tvValuesets: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 784
    Height = 521
    Align = alClient
    Header.AutoSizeIndex = 4
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    NodeDataSize = 4
    TabOrder = 0
    TreeOptions.PaintOptions = [toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnClick = tvValuesetsClick
    OnColumnResize = tvValuesetsColumnResize
    OnCompareNodes = tvValuesetsCompareNodes
    OnDblClick = tvValuesetsDblClick
    OnGetText = tvValuesetsGetText
    OnInitNode = tvValuesetsInitNode
    Columns = <
      item
        Position = 0
        Width = 150
        WideText = 'Name'
      end
      item
        Position = 1
        WideText = 'Status'
      end
      item
        Position = 2
        Width = 200
        WideText = 'Identifier'
      end
      item
        Position = 3
        Width = 200
        WideText = 'Publisher'
      end
      item
        Position = 4
        Width = 180
        WideText = 'Description'
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 521
    Width = 784
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      784
      41)
    object lblId: TLabel
      Left = 12
      Top = 12
      Width = 3
      Height = 13
    end
    object Label1: TLabel
      Left = 10
      Top = 12
      Width = 28
      Height = 13
      Caption = 'Filter:'
    end
    object Button1: TButton
      Left = 697
      Top = 6
      Width = 75
      Height = 31
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOpen: TButton
      Left = 616
      Top = 6
      Width = 75
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'Open'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = tvValuesetsDblClick
    end
    object eFilter: TEdit
      Left = 44
      Top = 10
      Width = 317
      Height = 21
      TabOrder = 2
      OnChange = eFilterChange
    end
  end
end
