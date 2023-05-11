object CDEOptionsForm: TCDEOptionsForm
  Left = 0
  Top = 0
  Caption = 'Clinical Document Editor Options'
  ClientHeight = 263
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Panel1: TPanel
    Left = 0
    Top = 228
    Width = 358
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      358
      35)
    object Button1: TButton
      Left = 193
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 274
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 358
    Height = 228
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = 'Panel2'
    ParentBackground = False
    TabOrder = 1
    object Panel3: TPanel
      Left = 11
      Top = 11
      Width = 336
      Height = 206
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Label13: TLabel
        Left = 48
        Top = 176
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label12: TLabel
        Left = 48
        Top = 141
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label11: TLabel
        Left = 48
        Top = 109
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 48
        Top = 77
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 48
        Top = 45
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label8: TLabel
        Left = 48
        Top = 13
        Width = 216
        Height = 13
        Caption = '......................................................'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 21
        Top = 13
        Width = 77
        Height = 13
        Caption = 'Low Light Mode '
        Transparent = False
      end
      object Label3: TLabel
        Left = 21
        Top = 45
        Width = 105
        Height = 13
        Caption = 'Printer for Pagination '
        Transparent = False
      end
      object Label4: TLabel
        Left = 21
        Top = 77
        Width = 103
        Height = 13
        Caption = 'Show Spelling Errors  '
        Transparent = False
      end
      object labelx: TLabel
        Left = 48
        Top = 109
        Width = 123
        Height = 13
        Caption = 'Ignore Uppercase Words '
        Transparent = False
      end
      object Label6: TLabel
        Left = 21
        Top = 141
        Width = 61
        Height = 13
        Caption = 'Touch Mode '
        Transparent = False
      end
      object Label7: TLabel
        Left = 22
        Top = 176
        Width = 60
        Height = 13
        Caption = 'Smart Mode '
        Transparent = False
      end
      object Shape1: TShape
        Left = 200
        Top = 12
        Width = 129
        Height = 193
        Pen.Color = clWhite
      end
      object cbLowLight: TCheckBox
        Left = 210
        Top = 12
        Width = 97
        Height = 17
        Caption = 'No'
        TabOrder = 0
        OnClick = cbLowLightClick
      end
      object cbxPrinters: TComboBox
        Left = 210
        Top = 41
        Width = 111
        Height = 21
        Style = csDropDownList
        TabOrder = 1
      end
      object cbSpellingErrors: TCheckBox
        Left = 210
        Top = 76
        Width = 97
        Height = 17
        Caption = 'No'
        TabOrder = 2
        OnClick = cbLowLightClick
      end
      object cbUppercase: TCheckBox
        Left = 210
        Top = 108
        Width = 97
        Height = 17
        Caption = 'No'
        TabOrder = 3
        OnClick = cbLowLightClick
      end
      object cbTouchMode: TCheckBox
        Left = 210
        Top = 140
        Width = 97
        Height = 17
        Caption = 'No'
        TabOrder = 4
        OnClick = cbLowLightClick
      end
      object cbSmartMode: TCheckBox
        Left = 210
        Top = 175
        Width = 97
        Height = 17
        Caption = 'No'
        TabOrder = 5
        OnClick = cbLowLightClick
      end
    end
  end
end
