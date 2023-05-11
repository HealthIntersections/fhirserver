object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QR Test'
  ClientHeight = 797
  ClientWidth = 1000
  Color = clMoneyGreen
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 121
    Height = 45
    Caption = 'Current Test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 67
    Width = 121
    Height = 45
    Caption = 'Basic Demo'
    TabOrder = 1
    OnClick = Button2Click
  end
  object btnSegments: TButton
    Left = 16
    Top = 118
    Width = 121
    Height = 51
    Caption = 'btnSegments'
    TabOrder = 2
    OnClick = btnSegmentsClick
  end
  object btnVariety: TButton
    Left = 16
    Top = 175
    Width = 121
    Height = 50
    Caption = 'btnVariety'
    TabOrder = 3
    OnClick = btnVarietyClick
  end
  object PageControl1: TPageControl
    Left = 153
    Top = 8
    Width = 816
    Height = 769
    ActivePage = TabSheet1
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Image1: TImage
        Left = 51
        Top = 19
        Width = 710
        Height = 710
        Stretch = True
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 808
        Height = 741
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnDblClick = Memo1DblClick
      end
    end
  end
  object btnMask: TButton
    Left = 16
    Top = 231
    Width = 121
    Height = 50
    Caption = 'btnMask'
    TabOrder = 5
    OnClick = btnMaskClick
  end
  object btnNewClass: TButton
    Left = 16
    Top = 287
    Width = 121
    Height = 50
    Caption = 'btnNewClass'
    TabOrder = 6
    OnClick = btnNewClassClick
  end
  object btnNewClassSeg: TButton
    Left = 16
    Top = 343
    Width = 121
    Height = 50
    Caption = 'btnNewClassSeg'
    TabOrder = 7
    OnClick = btnNewClassSegClick
  end
end
