object MarkdownPreviewForm: TMarkdownPreviewForm
  Left = 0
  Top = 0
  BorderIcons = [biMaximize]
  Caption = 'Markdown Preview'
  ClientHeight = 526
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 425
    Top = 0
    Width = 8
    Height = 486
    OnMoved = Splitter1Moved
  end
  object Panel1: TPanel
    Left = 0
    Top = 486
    Width = 884
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 492
    ExplicitWidth = 612
    DesignSize = (
      884
      40)
    object btnCancel: TButton
      Left = 801
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      ModalResult = 8
      TabOrder = 0
      ExplicitLeft = 552
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 486
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 496
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 425
      Height = 32
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '     HTML'
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 295
    end
    object Panel7: TPanel
      Left = 0
      Top = 32
      Width = 425
      Height = 454
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 1
      ExplicitLeft = 136
      ExplicitTop = 264
      ExplicitWidth = 185
      ExplicitHeight = 41
    end
  end
  object Panel3: TPanel
    Left = 433
    Top = 0
    Width = 451
    Height = 486
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 472
    ExplicitTop = 168
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 451
      Height = 32
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   Rendered'
      TabOrder = 0
      ExplicitLeft = 2
      ExplicitTop = 1
      ExplicitWidth = 310
    end
    object Panel6: TPanel
      Left = 0
      Top = 32
      Width = 451
      Height = 454
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 1
      ExplicitLeft = 136
      ExplicitTop = 264
      ExplicitWidth = 185
      ExplicitHeight = 41
      object WebBrowser1: TWebBrowser
        Left = 7
        Top = 7
        Width = 437
        Height = 440
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 6
        ExplicitTop = 8
        ExplicitWidth = 442
        ControlData = {
          4C0000002A2D00007A2D00000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
end
