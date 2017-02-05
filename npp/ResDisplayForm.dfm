object ResourceDisplayForm: TResourceDisplayForm
  Left = 0
  Top = 0
  Caption = 'ResourceDisplayForm'
  ClientHeight = 273
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 232
    Width = 560
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      560
      41)
    object Button1: TButton
      Left = 472
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Copy'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 560
    Height = 232
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Presentation'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object WebBrowser1: TWebBrowser
        Left = 0
        Top = 0
        Width = 552
        Height = 204
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 232
        ExplicitTop = 96
        ExplicitWidth = 300
        ExplicitHeight = 150
        ControlData = {
          4C0000000D390000161500000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Source'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 552
        Height = 204
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 17
        ExplicitTop = 37
        ExplicitWidth = 264
        ExplicitHeight = 128
      end
    end
  end
  object sd: TSaveDialog
    Left = 216
    Top = 240
  end
end
