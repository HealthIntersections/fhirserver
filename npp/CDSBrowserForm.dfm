object CDSBrowser: TCDSBrowser
  Left = 0
  Top = 0
  Caption = 'FHIR Plug-in CDS browser'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      635
      41)
    object btnBack: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Back'
      TabOrder = 0
      OnClick = btnBackClick
    end
    object btnClose: TButton
      Left = 550
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCloseClick
    end
    object btnRefresh: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshClick
    end
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 41
    Width = 635
    Height = 295
    Align = alClient
    TabOrder = 1
    OnBeforeNavigate2 = WebBrowser1BeforeNavigate2
    ExplicitLeft = 264
    ExplicitTop = 156
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C000000A14100007D1E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
