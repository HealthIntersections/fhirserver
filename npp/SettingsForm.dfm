inherited SettingForm: TSettingForm
  BorderStyle = bsDialog
  Caption = 'FHIR NPP Plugin'
  ClientHeight = 243
  ClientWidth = 446
  OnShow = FormShow
  ExplicitWidth = 452
  ExplicitHeight = 272
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 278
    Top = 206
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
    ExplicitTop = 185
  end
  object Button2: TButton
    Left = 359
    Top = 206
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitTop = 185
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 6
    Width = 426
    Height = 91
    Caption = ' Terminology Server '
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 25
      Height = 13
      Caption = 'URL:'
    end
    object Label2: TLabel
      Left = 12
      Top = 50
      Width = 397
      Height = 31
      AutoSize = False
      Caption = 
        'The server must be a FHIR terminology server running the same ve' +
        'rsion of FHIR as this client, and must not require authenticatio' +
        'n'
      WordWrap = True
    end
    object edtServer: TEdit
      Left = 50
      Top = 21
      Width = 359
      Height = 21
      TabOrder = 0
      Text = 'edtServer'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 103
    Width = 426
    Height = 91
    Caption = ' Definitions Source '
    TabOrder = 3
    object Label3: TLabel
      Left = 12
      Top = 26
      Width = 19
      Height = 13
      Caption = 'File:'
    end
    object Label4: TLabel
      Left = 12
      Top = 48
      Width = 397
      Height = 31
      AutoSize = False
      Caption = 
        'This is the validation.min.xml.zip file downloaded from the spec' +
        'ification (downloads page). This also must have the same version'
      WordWrap = True
    end
    object SpeedButton1: TSpeedButton
      Left = 382
      Top = 20
      Width = 23
      Height = 22
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
        078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
        BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
        CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
        39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
        D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
        DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
        1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
        DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
        EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
        77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
        E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
        8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
        9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
        BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
        FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
        FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = SpeedButton1Click
    end
    object edtFile: TEdit
      Left = 50
      Top = 21
      Width = 329
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
  end
  object od: TOpenDialog
    DefaultExt = '.zip'
    Filter = '*.zip'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select Definitions Source'
    Left = 10
    Top = 204
  end
end
