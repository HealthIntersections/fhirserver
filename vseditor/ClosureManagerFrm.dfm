object ClosureManagerForm: TClosureManagerForm
  Left = 0
  Top = 0
  Caption = 'Closures Manager'
  ClientHeight = 556
  ClientWidth = 883
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
    Top = 0
    Width = 883
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      883
      41)
    object Label1: TLabel
      Left = 10
      Top = 12
      Width = 45
      Height = 13
      Caption = 'Closures:'
    end
    object Bevel1: TBevel
      Left = 242
      Top = 4
      Width = 7
      Height = 33
      Shape = bsLeftLine
    end
    object cbxClosures: TComboBox
      Left = 58
      Top = 9
      Width = 117
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbxClosuresChange
    end
    object Button1: TButton
      Left = 176
      Top = 7
      Width = 61
      Height = 25
      Caption = 'New...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object btnAddConcept: TButton
      Left = 248
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Add Concept'
      Enabled = False
      TabOrder = 2
      OnClick = btnAddConceptClick
    end
    object btnReset: TButton
      Left = 410
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Reset'
      Enabled = False
      TabOrder = 3
      OnClick = btnResetClick
    end
    object Button4: TButton
      Left = 800
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      TabOrder = 4
      OnClick = Button4Click
    end
    object btnupdate: TButton
      Left = 329
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Update'
      Enabled = False
      TabOrder = 5
      OnClick = btnupdateClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 883
    Height = 486
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Grid'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object grid: TStringGrid
        Left = 0
        Top = 0
        Width = 875
        Height = 458
        Hint = 'Test'
        Align = alClient
        ColCount = 1
        DefaultColWidth = 60
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnDrawCell = gridDrawCell
        OnMouseMove = gridMouseMove
        ExplicitLeft = -456
        ExplicitTop = -211
        ExplicitWidth = 737
        ExplicitHeight = 376
        ColWidths = (
          60)
        RowHeights = (
          20)
      end
    end
    object TabSheet2: TTabSheet
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Caption = 'Lists'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 403
        Top = 0
        Width = 6
        Height = 458
        ExplicitHeight = 487
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 403
        Height = 458
        Align = alLeft
        Caption = 'Panel2'
        ParentBackground = False
        TabOrder = 0
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 401
          Height = 24
          Align = alTop
          Alignment = taLeftJustify
          Caption = '  Concepts'
          ParentBackground = False
          TabOrder = 0
        end
        object lbConcepts: TListBox
          Left = 1
          Top = 25
          Width = 401
          Height = 432
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object Panel3: TPanel
        Left = 409
        Top = 0
        Width = 466
        Height = 458
        Align = alClient
        Caption = 'Panel3'
        ParentBackground = False
        TabOrder = 1
        object Panel5: TPanel
          Left = 1
          Top = 1
          Width = 464
          Height = 24
          Align = alTop
          Alignment = taLeftJustify
          Caption = '  Closures'
          ParentBackground = False
          TabOrder = 0
        end
        object lbClosures: TListBox
          Left = 1
          Top = 25
          Width = 464
          Height = 432
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
        end
      end
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 527
    Width = 883
    Height = 29
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = ' '
    TabOrder = 2
  end
end
