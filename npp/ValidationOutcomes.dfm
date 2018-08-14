object ValidationOutcomeForm: TValidationOutcomeForm
  Left = 0
  Top = 0
  Caption = 'Fetch a resource'
  ClientHeight = 401
  ClientWidth = 777
  Color = clBtnFace
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 356
    Width = 777
    Height = 45
    Align = alBottom
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      777
      45)
    object btnCancel: TButton
      Left = 692
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnOpen: TButton
      Left = 9
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Save'
      TabOrder = 1
      OnClick = btnOpenClick
    end
    object CheckBox1: TCheckBox
      Left = 90
      Top = 16
      Width = 586
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Don'#39't show this summary next time'
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 356
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 6
    TabOrder = 1
    object WebBrowser1: TWebBrowser
      Left = 8
      Top = 8
      Width = 761
      Height = 340
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 128
      ExplicitTop = 122
      ExplicitWidth = 300
      ExplicitHeight = 150
      ControlData = {
        4C000000A74E0000242300000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object sd: TSaveDialog
    DefaultExt = 'html'
    Title = 'Save Validation Outcomes'
    Left = 20
    Top = 312
  end
end
