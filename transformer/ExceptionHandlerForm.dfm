object ExceptionHandlerDialog: TExceptionHandlerDialog
  Left = 673
  Top = 441
  Width = 338
  Height = 226
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Unhandled Exception'
  Color = clBtnFace
  Font.Charset = OEM_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 322
    Height = 36
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 11
      Width = 185
      Height = 14
      Caption = 'An unhandled exception has occurred'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 36
    Width = 322
    Height = 116
    Align = alClient
    TabOrder = 1
    object eMessage: TLabel
      Left = 10
      Top = 11
      Width = 293
      Height = 94
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'eMessage'
      WordWrap = True
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 152
    Width = 322
    Height = 36
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      Left = 232
      Top = 7
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Ok (or not)'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 7
      Top = 5
      Width = 66
      Height = 22
      Caption = '&Copy'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
