object ServerGUI: TServerGUI
  Left = 0
  Top = 0
  Caption = 'FHIR Terminology Server'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 106
    Width = 852
    Height = 305
    Align = alClient
    BorderWidth = 4
    Caption = 'Panel2'
    TabOrder = 0
    object mLog: TMemo
      Left = 5
      Top = 5
      Width = 842
      Height = 295
      Align = alClient
      TabOrder = 0
      ExplicitTop = 6
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 65
    Align = alTop
    TabOrder = 1
    DesignSize = (
      852
      65)
    object Label1: TLabel
      Left = 14
      Top = 8
      Width = 53
      Height = 13
      Caption = 'UTG Folder'
    end
    object Label2: TLabel
      Left = 14
      Top = 35
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object edtFolder: TEdit
      Left = 86
      Top = 5
      Width = 716
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'edtFolder'
      OnChange = edtFolderChange
    end
    object edtPort: TEdit
      Left = 86
      Top = 32
      Width = 756
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'edtPort'
      OnChange = edtPortChange
    end
    object btnFolder: TBitBtn
      Left = 808
      Top = 1
      Width = 35
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 2
      OnClick = btnFolderClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 852
    Height = 41
    Align = alTop
    TabOrder = 2
    DesignSize = (
      852
      41)
    object lblStatus: TLabel
      Left = 86
      Top = 12
      Width = 59
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Not Running'
    end
    object btnStatus: TButton
      Left = 5
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStatusClick
    end
    object btnBrowser: TButton
      Left = 768
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browser'
      TabOrder = 1
      OnClick = btnBrowserClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 138
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 72
    Top = 138
  end
end
