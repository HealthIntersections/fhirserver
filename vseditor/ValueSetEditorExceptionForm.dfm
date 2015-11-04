object ExceptionDialog: TExceptionDialog
  Left = 310
  Top = 255
  BorderIcons = [biSystemMenu]
  Caption = 'ValueSetEditor Exception Trap'
  ClientHeight = 344
  ClientWidth = 473
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    473
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object BevelDetails: TBevel
    Left = 5
    Top = 154
    Width = 463
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 5
    Top = 130
    Width = 104
    Height = 13
    Caption = 'What you were doing:'
  end
  object SendBtn: TButton
    Left = 393
    Top = 125
    Width = 75
    Height = 25
    Hint = 'Send bug report using default mail client'
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 0
    OnClick = SendBtnClick
  end
  object TextMemo: TMemo
    Left = 4
    Top = 8
    Width = 374
    Height = 105
    Hint = 'Use Ctrl+C to copy the report to the clipboard'
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Ctl3D = True
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
    WantReturns = False
  end
  object OkBtn: TButton
    Left = 393
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK (Not)'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object DetailsBtn: TButton
    Left = 393
    Top = 91
    Width = 75
    Height = 25
    Hint = 'Show or hide additional information|'
    Anchors = [akTop, akRight]
    Caption = '&Details'
    Enabled = False
    TabOrder = 3
    OnClick = DetailsBtnClick
  end
  object DetailsMemo: TMemo
    Left = 4
    Top = 166
    Width = 464
    Height = 171
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
    WordWrap = False
  end
  object Edit1: TEdit
    Left = 116
    Top = 127
    Width = 262
    Height = 21
    TabOrder = 5
  end
end
