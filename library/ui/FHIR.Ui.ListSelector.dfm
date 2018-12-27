object ListSelectorForm: TListSelectorForm
  Left = 0
  Top = 0
  Caption = 'ListSelectorForm'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 635
    Height = 40
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      635
      40)
    object CheckBox1: TCheckBox
      Left = 8
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Select All'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object btnok: TButton
      Left = 471
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 552
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnOkAll: TButton
      Left = 390
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK for All'
      ModalResult = 1
      TabOrder = 3
      OnClick = btnOkAllClick
    end
    object cbDontAsk: TCheckBox
      Left = 128
      Top = 10
      Width = 233
      Height = 17
      Caption = 'Command Automatically without asking'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 259
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    Caption = 'Panel2'
    TabOrder = 1
    object ListBox1: TCheckListBox
      Left = 6
      Top = 6
      Width = 623
      Height = 247
      OnClickCheck = ListBox1ClickCheck
      Align = alClient
      BevelInner = bvLowered
      ItemHeight = 13
      TabOrder = 0
    end
  end
end
