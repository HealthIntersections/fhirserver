object ServerGUI: TServerGUI
  Left = 0
  Height = 411
  Top = 0
  Width = 852
  Caption = 'FHIR Terminology Server'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Menu = MainMenu1
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '2.2.7.0'
  object Panel2: TPanel
    Left = 0
    Height = 305
    Top = 106
    Width = 852
    Align = alClient
    BorderWidth = 4
    Caption = 'Panel2'
    ClientHeight = 305
    ClientWidth = 852
    TabOrder = 0
    object mLog: TMemo
      Left = 5
      Height = 295
      Top = 5
      Width = 842
      Align = alClient
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Height = 65
    Top = 0
    Width = 852
    Align = alTop
    ClientHeight = 65
    ClientWidth = 852
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Height = 13
      Top = 8
      Width = 53
      Caption = 'UTG Folder'
      ParentColor = False
    end
    object Label2: TLabel
      Left = 14
      Height = 13
      Top = 35
      Width = 20
      Caption = 'Port'
      ParentColor = False
    end
    object edtFolder: TEdit
      Left = 86
      Height = 21
      Top = 5
      Width = 716
      Anchors = [akTop, akLeft, akRight]
      OnChange = edtFolderChange
      TabOrder = 0
      Text = 'edtFolder'
    end
    object edtPort: TEdit
      Left = 86
      Height = 21
      Top = 32
      Width = 756
      Anchors = [akTop, akLeft, akRight]
      OnChange = edtPortChange
      TabOrder = 1
      Text = 'edtPort'
    end
    object btnFolder: TBitBtn
      Left = 808
      Height = 25
      Top = 1
      Width = 35
      Anchors = [akTop, akRight]
      Caption = '...'
      OnClick = btnFolderClick
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 0
    Height = 41
    Top = 65
    Width = 852
    Align = alTop
    ClientHeight = 41
    ClientWidth = 852
    TabOrder = 2
    object lblStatus: TLabel
      Left = 86
      Height = 13
      Top = 12
      Width = 59
      Anchors = [akTop, akLeft, akRight]
      Caption = 'Not Running'
      ParentColor = False
    end
    object btnStatus: TButton
      Left = 5
      Height = 25
      Top = 6
      Width = 75
      Caption = 'Start'
      OnClick = btnStatusClick
      TabOrder = 0
    end
    object btnBrowser: TButton
      Left = 768
      Height = 25
      Top = 8
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'Browser'
      OnClick = btnBrowserClick
      TabOrder = 1
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
