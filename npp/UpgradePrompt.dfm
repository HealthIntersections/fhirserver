object UpgradePromptForm: TUpgradePromptForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FHIR Plug-in New Release'
  ClientHeight = 218
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 177
    Width = 391
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      391
      41)
    object btnOk: TButton
      Left = 304
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object Button1: TButton
      Left = 10
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Download'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 177
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 1
    object Panel3: TPanel
      Left = 10
      Top = 10
      Width = 371
      Height = 157
      Align = alClient
      BevelOuter = bvLowered
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        371
        157)
      object Label1: TLabel
        Left = 18
        Top = 16
        Width = 330
        Height = 13
        Caption = 
          'A new version of the FHIR Plug-in for Notepad++ has been release' +
          'd'
      end
      object Memo1: TMemo
        Left = 18
        Top = 40
        Width = 334
        Height = 100
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
end
