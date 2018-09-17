object CdaHeaderDialog: TCdaHeaderDialog
  Left = 0
  Top = 0
  Caption = 'CdaHeaderDialog'
  ClientHeight = 601
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
  object Document: TPageControl
    Left = 0
    Top = 0
    Width = 635
    Height = 560
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Document'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 627
        Height = 193
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        object Label1: TLabel
          Left = 24
          Top = 40
          Width = 20
          Height = 13
          Caption = 'Title'
        end
        object Label2: TLabel
          Left = 24
          Top = 66
          Width = 25
          Height = 13
          Caption = 'Code'
        end
        object Label3: TLabel
          Left = 24
          Top = 95
          Width = 44
          Height = 13
          Caption = 'Identifier'
        end
        object Label4: TLabel
          Left = 24
          Top = 125
          Width = 49
          Height = 13
          Caption = 'Date/Time'
        end
        object Label5: TLabel
          Left = 27
          Top = 158
          Width = 47
          Height = 13
          Caption = 'Language'
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 627
          Height = 33
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = '    Document Details'
          TabOrder = 0
        end
        object edtDocTitle: TEdit
          Left = 80
          Top = 37
          Width = 539
          Height = 21
          TabOrder = 1
          Text = 'edtDocTitle'
          OnChange = edtDocTitleChange
        end
        object edtDocCode: TEdit
          Left = 80
          Top = 64
          Width = 458
          Height = 21
          ReadOnly = True
          TabOrder = 2
          Text = 'edtDocCode'
        end
        object Button3: TButton
          Left = 544
          Top = 61
          Width = 75
          Height = 25
          Caption = 'Edit'
          TabOrder = 3
        end
        object edtDocId: TEdit
          Left = 80
          Top = 92
          Width = 377
          Height = 21
          ReadOnly = True
          TabOrder = 4
          Text = 'Edit2'
        end
        object btnDocIdEdit: TButton
          Left = 544
          Top = 91
          Width = 75
          Height = 25
          Caption = 'Edit'
          TabOrder = 5
          OnClick = btnDocIdEditClick
        end
        object btnDocIdGuid: TButton
          Left = 463
          Top = 91
          Width = 75
          Height = 25
          Caption = 'GUID'
          TabOrder = 6
          OnClick = btnDocIdGuidClick
        end
        object edtDocTime: TESBDateTimeEdit
          Left = 79
          Top = 119
          OnChange = edtDocTimeChange
          Year = 1899
          Month = 12
          Day = 30
          Hour = 0
          Minute = 0
          Second = 0
          Millisecond = 0
          TabOrder = 7
        end
        object btnDocTimeNow: TButton
          Left = 544
          Top = 122
          Width = 75
          Height = 25
          Caption = 'Now'
          TabOrder = 8
          OnClick = btnDocTimeNowClick
        end
        object edtDocLang: TEdit
          Left = 80
          Top = 155
          Width = 377
          Height = 21
          TabOrder = 9
          Text = 'Edit2'
          OnChange = edtDocLangChange
        end
        object edtDocTimezone: TEdit
          Left = 232
          Top = 119
          Width = 57
          Height = 21
          TabOrder = 10
          Text = 'edtDocTimezone'
          OnChange = edtDocTimezoneChange
        end
        object btnDocTimeClear: TButton
          Left = 463
          Top = 122
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 11
          OnClick = btnDocTimeClearClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Patient'
      ImageIndex = 1
    end
    object TabSheet3: TTabSheet
      Caption = 'Authors'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet4: TTabSheet
      Caption = 'Authenticators'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet5: TTabSheet
      Caption = 'Encounter'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 560
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      635
      41)
    object Button1: TButton
      Left = 548
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object Button2: TButton
      Left = 467
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object btnDocLangDef: TButton
    Left = 548
    Top = 177
    Width = 75
    Height = 25
    Caption = 'Default'
    TabOrder = 2
    OnClick = btnDocLangDefClick
  end
end
