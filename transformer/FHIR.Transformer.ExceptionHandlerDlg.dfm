object ExceptionHandlerDialog: TExceptionHandlerDialog
  Left = 673
  Top = 441
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Unhandled Exception'
  ClientHeight = 286
  ClientWidth = 863
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
    Width = 863
    Height = 73
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 11
      Width = 185
      Height = 14
      Caption = 'Transformation Fail'
    end
    object Label2: TLabel
      Left = 10
      Top = 25
      Width = 151
      Height = 14
      Caption = 'Your work may have a problem'
    end
    object Label3: TLabel
      Left = 10
      Top = 39
      Width = 98
      Height = 14
      Caption = 'Unless it lies in code'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 73
    Width = 863
    Height = 177
    Align = alClient
    TabOrder = 1
    ExplicitTop = 36
    ExplicitWidth = 322
    ExplicitHeight = 115
    DesignSize = (
      863
      177)
    object eMessage: TLabel
      Left = 10
      Top = 11
      Width = 834
      Height = 155
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'eMessage'
      WordWrap = True
      ExplicitWidth = 293
      ExplicitHeight = 94
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 250
    Width = 863
    Height = 36
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 151
    ExplicitWidth = 322
    DesignSize = (
      863
      36)
    object Button1: TButton
      Left = 773
      Top = 7
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Ok (or not)'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 232
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
