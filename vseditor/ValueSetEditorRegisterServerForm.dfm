object frmRegisterServer: TfrmRegisterServer
  Left = 0
  Top = 0
  Caption = 'Terminology Server Management'
  ClientHeight = 338
  ClientWidth = 819
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
    Top = 288
    Width = 819
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOpenFile: TButton
      Left = 727
      Top = 8
      Width = 80
      Height = 33
      Cancel = True
      Caption = 'Close'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ModalResult = 2
      ParentFont = False
      TabOrder = 0
      WordWrap = True
    end
    object btnUpdate: TButton
      Left = 6
      Top = 14
      Width = 105
      Height = 25
      Caption = 'Update Cache'
      Enabled = False
      TabOrder = 1
      OnClick = btnUpdateClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 819
    Height = 288
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    Caption = 'Panel2'
    TabOrder = 1
    object tvServers: TVirtualStringTree
      Left = 6
      Top = 6
      Width = 807
      Height = 276
      Align = alClient
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
      NodeDataSize = 0
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnChecked = tvServersChecked
      OnClick = tvServersClick
      OnGetText = tvServersGetText
      OnInitNode = tvServersInitNode
      Columns = <
        item
          CheckType = ctRadioButton
          Position = 0
          Width = 356
          WideText = 'Server URL'
        end
        item
          Position = 1
          Width = 100
          WideText = 'Name'
        end
        item
          Position = 2
          Width = 89
          WideText = 'Value Set #'
        end
        item
          Position = 3
          Width = 103
          WideText = 'Code System #'
        end
        item
          Position = 4
          Width = 121
          WideText = 'Last Checked'
        end>
    end
  end
end
