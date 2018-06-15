object UixForm: TUixForm
  Left = 254
  Top = 169
  ClientHeight = 441
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TUixPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 406
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    ParentColor = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ExplicitHeight = 409
  end
  object pnlBottom: TUixPanel
    Left = 0
    Top = 406
    Width = 680
    Height = 35
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BevelWidth = 0
    ParentColor = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    ExplicitTop = 409
    DesignSize = (
      680
      35)
    object btnCancel: TUixButton
      Left = 606
      Top = 0
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnOk: TUixButton
      Left = 526
      Top = 0
      Width = 75
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
end
