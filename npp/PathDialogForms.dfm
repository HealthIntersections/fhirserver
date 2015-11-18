inherited PathDialogForm: TPathDialogForm
  BorderStyle = bsSizeable
  Caption = 'Path Evaluation Outcome'
  ClientHeight = 239
  ClientWidth = 400
  Scaled = False
  ExplicitWidth = 416
  ExplicitHeight = 278
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 383
    Height = 175
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      383
      175)
    object Label1: TLabel
      Left = 14
      Top = 16
      Width = 178
      Height = 13
      Caption = 'FHIR Path Evaluation Summary'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 14
      Top = 40
      Width = 351
      Height = 116
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Lines.Strings = (
        'Memo1')
      ParentColor = True
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 198
    Width = 400
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      400
      41)
    object CheckBox1: TCheckBox
      Left = 12
      Top = 10
      Width = 179
      Height = 17
      Caption = 'Don'#39't show this summary again'
      TabOrder = 0
    end
    object Button1: TButton
      Left = 316
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 1
    end
  end
end
