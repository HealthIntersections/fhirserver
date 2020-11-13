object WorkingForm: TWorkingForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Loading FHIR Packages'
  ClientHeight = 53
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Working...'
  end
  object pbPercent: TProgressBar
    Left = 8
    Top = 27
    Width = 433
    Height = 17
    TabOrder = 0
  end
end
