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
    object Button2: TButton
      Left = 248
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Add Concept'
      TabOrder = 2
    end
    object Button3: TButton
      Left = 329
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 3
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
  end
  object grid: TStringGrid
    Left = 0
    Top = 41
    Width = 883
    Height = 515
    Align = alClient
    ColCount = 1
    DefaultColWidth = 60
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 1
    ColWidths = (
      60)
    RowHeights = (
      20)
  end
end
