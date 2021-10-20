unit dlg_scanner;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Clipbrd,
  FPImage, LCLIntf, LCLType,
  ZXing.ScanManager, ZXing.BarCodeFormat, ZXing.ReadResult,
  fui_lcl_utilities;

type

  { TQRCodeScannerForm }

  TQRCodeScannerForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlInfo: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImage : TBitmap;
    FScanner : TScanManager;
    procedure clear;
    procedure readImage;
  end;

var
  QRCodeScannerForm: TQRCodeScannerForm;

implementation

{$R *.lfm}

function codesTBarcodeFormat(bcf : TBarcodeFormat) : String;
begin
  case bcf of
    Auto: result := 'Auto';
    AZTEC: result := 'AZTEC';
    CODABAR: result := 'CODABAR';
    CODE_39: result := 'CODE_39';
    CODE_93: result := 'CODE_93';
    CODE_128: result := 'CODE_128';
    DATA_MATRIX: result := 'DATA_MATRIX';
    EAN_8: result := 'EAN_8';
    EAN_13: result := 'EAN_13';
    ITF: result := 'ITF';
    MAXICODE: result := 'MAXICODE';
    PDF_417: result := 'PDF_417';
    QR_CODE: result := 'QR_CODE';
    RSS_14: result := 'RSS_14';
    RSS_EXPANDED: result := 'RSS_EXPANDED';
    UPC_A: result := 'UPC_A';
    UPC_E: result := 'UPC_E';
    UPC_EAN_EXTENSION: result := 'UPC_EAN_EXTENSION';
    MSI: result := 'MSI';
    PLESSEY: result := 'PLESSEY';
  else
    result := '';
  end;
end;

{ TQRCodeScannerForm }

procedure TQRCodeScannerForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FImage := TBitmap.create;
  FScanner := TScanManager.create(TBarcodeFormat.Auto, nil);
  clear;
end;

procedure TQRCodeScannerForm.clear;
begin
  btnOk.Enabled := false;
  Memo1.text := '';
  pnlInfo.caption := '  QR Code';
  Image1.Picture := nil;
end;

procedure TQRCodeScannerForm.Button1Click(Sender: TObject);
var
  dlg : TOpenDialog;
  bmp : TFPCustomImage;
  s : String;
begin
  dlg := TOpenDialog.create(self);
  try
    dlg.Filter := 'All Known Images|*.bmp; *.jpg; *.gif; *.png; *.tiff|'+
      'All Files|*.*';
    dlg.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];
    if dlg.execute then
    begin
      clear;
      bmp := TFPMemoryImage.create(10, 10);
      try
        bmp.LoadFromFile(dlg.filename);
        FImage.assign(bmp);
        Image1.Picture.Assign(FImage);
        Image1.Refresh;
        Application.ProcessMessages;
        readImage;
      finally
        bmp.free;
      end;
    end;
  finally
    dlg.free;
  end;
end;

procedure TQRCodeScannerForm.Button2Click(Sender: TObject);
var
  bmp : TBitmap;
begin
  if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap))) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
      FImage.assign(bmp);
      Image1.Picture.Assign(FImage);
      Image1.Refresh;
      Application.ProcessMessages;
      readImage;
    finally
      bmp.free;
    end;
  end
  else
    ShowMessage('No image is found on clipboard');
end;

procedure TQRCodeScannerForm.FormDestroy(Sender: TObject);
begin
  FImage.free;
  FScanner.free;
end;

procedure TQRCodeScannerForm.readImage;
var
  bc : TReadResult;
begin
  screen.Cursor := crHourGlass;
  try
    bc := FScanner.Scan(FImage);
    try
      if (bc = nil) then
      begin
        btnOk.Enabled := false;
        pnlInfo.caption := '  QR code: No codes found';
      end
      else
      begin
        btnOk.enabled := true;
        pnlInfo.caption := '  QR code: '+codesTBarcodeFormat(bc.BarcodeFormat)+' found';
        memo1.Text := bc.text;
      end;
    finally
      bc.free;
    end;
  finally
    screen.cursor := crDefault;
  end;
end;

end.

