object FetchResourceFrm: TFetchResourceFrm
  Left = 0
  Top = 0
  Caption = 'Fetch a resource'
  ClientHeight = 699
  ClientWidth = 724
  Color = clBtnFace
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 357
    Width = 724
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 331
    ExplicitWidth = 327
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 724
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      724
      65)
    object Label1: TLabel
      Left = 4
      Top = 34
      Width = 49
      Height = 13
      Caption = 'Resource:'
    end
    object Label4: TLabel
      Left = 16
      Top = 8
      Width = 36
      Height = 13
      Caption = 'Server:'
    end
    object cbxType: TComboBox
      Left = 59
      Top = 33
      Width = 576
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = cbxTypeChange
    end
    object cbxServers: TComboBox
      Left = 58
      Top = 6
      Width = 577
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = cbxServersChange
    end
    object btnSerevrs: TButton
      Left = 641
      Top = 4
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Manage'
      TabOrder = 2
      OnClick = btnSerevrsClick
    end
    object edtFilter: TEdit
      Left = 641
      Top = 31
      Width = 75
      Height = 21
      Hint = 'Filter'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 654
    Width = 724
    Height = 45
    Align = alBottom
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      724
      45)
    object btnCancel: TButton
      Left = 639
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOpen: TButton
      Left = 555
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Open'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOpenClick
    end
    object btnFirst: TButton
      Left = 10
      Top = -2
      Width = 23
      Height = 25
      Caption = '|<'
      Enabled = False
      TabOrder = 2
    end
    object btnPrev: TButton
      Left = 36
      Top = -2
      Width = 23
      Height = 25
      Caption = '<'
      Enabled = False
      TabOrder = 3
    end
    object btnNext: TButton
      Left = 62
      Top = -2
      Width = 23
      Height = 25
      Caption = '>'
      Enabled = False
      TabOrder = 4
    end
    object btnLast: TButton
      Left = 88
      Top = -2
      Width = 23
      Height = 25
      Caption = '>|'
      Enabled = False
      TabOrder = 5
    end
    object rbXml: TRadioButton
      Left = 349
      Top = 14
      Width = 45
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'XML'
      TabOrder = 6
    end
    object rbJson: TRadioButton
      Left = 400
      Top = 14
      Width = 55
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'JSON'
      Checked = True
      TabOrder = 7
      TabStop = True
    end
    object RadioButton1: TRadioButton
      Left = 453
      Top = 14
      Width = 96
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Server Format'
      TabOrder = 8
    end
  end
  object pnlCriteria: TPanel
    Left = 0
    Top = 65
    Width = 724
    Height = 292
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 724
      Height = 228
      HorzScrollBar.Visible = False
      VertScrollBar.Tracking = True
      Align = alClient
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
      OnResize = FormResize
    end
    object Panel3: TPanel
      Left = 0
      Top = 228
      Width = 724
      Height = 64
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        724
        64)
      object Label2: TLabel
        Left = 10
        Top = 10
        Width = 39
        Height = 13
        Caption = 'Sort By:'
      end
      object Label3: TLabel
        Left = 612
        Top = 10
        Width = 50
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Page Size:'
      end
      object btnSearchAll: TButton
        Left = 639
        Top = 33
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search (All)'
        TabOrder = 0
        OnClick = btnSearchAllClick
      end
      object btnCopy: TButton
        Left = 6
        Top = 33
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Copy URL'
        Enabled = False
        TabOrder = 1
        OnClick = btnCopyClick
      end
      object btnPaste: TButton
        Left = 88
        Top = 33
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Paste URL'
        TabOrder = 2
        OnClick = btnPasteClick
      end
      object btnSearchPaged: TButton
        Left = 531
        Top = 33
        Width = 99
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search (Paged)'
        TabOrder = 3
        OnClick = btnSearchPagedClick
      end
      object cbxSort: TComboBox
        Left = 55
        Top = 6
        Width = 490
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
      object edtPageCount: TSpinEdit
        Left = 664
        Top = 6
        Width = 50
        Height = 22
        Anchors = [akTop, akRight]
        Increment = 10
        MaxValue = 10000
        MinValue = 1
        TabOrder = 5
        Value = 20
      end
      object cbSearchOrder: TCheckBox
        Left = 550
        Top = 9
        Width = 50
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Desc.'
        TabOrder = 6
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 360
    Width = 724
    Height = 294
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 3
    object vtMatches: TVirtualStringTree
      Left = 8
      Top = 8
      Width = 708
      Height = 278
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnAddToSelection = vtMatchesAddToSelection
      OnDblClick = vtMatchesDblClick
      OnGetText = vtMatchesGetText
      OnRemoveFromSelection = vtMatchesRemoveFromSelection
      Columns = <>
    end
  end
end
