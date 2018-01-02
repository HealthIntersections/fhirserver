object ResourceNewForm: TResourceNewForm
  Left = 0
  Top = 0
  Caption = 'Create New Resource'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 35
    Width = 527
    Height = 260
    ActivePage = tbResources
    Align = alClient
    TabOrder = 0
    object tbResources: TTabSheet
      Caption = 'Resources'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbResources: TListBox
        Left = 0
        Top = 0
        Width = 519
        Height = 232
        Align = alClient
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbResourcesClick
        OnDblClick = btnCreateClick
      end
    end
    object tbProfiles: TTabSheet
      Caption = 'Profiles'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbProfiles: TListBox
        Left = 0
        Top = 0
        Width = 519
        Height = 232
        Align = alClient
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbProfilesClick
        OnDblClick = btnCreateClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 527
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 14
      Width = 28
      Height = 13
      Caption = 'Filter:'
    end
    object edtFilter: TEdit
      Left = 44
      Top = 10
      Width = 175
      Height = 21
      TabOrder = 0
      OnChange = edtFilterChange
    end
    object btnCreate: TButton
      Left = 359
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Create'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 1
      OnClick = btnCreateClick
    end
    object Button2: TButton
      Left = 440
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      527
      35)
    object Label2: TLabel
      Left = 10
      Top = 11
      Width = 109
      Height = 13
      Caption = 'Choose a template:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rbJson: TRadioButton
      Left = 464
      Top = 12
      Width = 55
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'JSON'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbXml: TRadioButton
      Left = 413
      Top = 12
      Width = 45
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'XML'
      TabOrder = 1
    end
  end
end
