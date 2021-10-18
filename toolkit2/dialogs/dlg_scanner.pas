unit dlg_scanner;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FPImage,
  ZXing.ScanManager, ZXing.BarCodeFormat, ZXing.ReadResult,
  fui_lcl_utilities;

type

  { TQRCodeScannerForm }

  TQRCodeScannerForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Image1: TImage;
    lblInfo: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FImage : TBitmap;
    FScanner : TScanManager;
    procedure readImage;
  public
    procedure useImage(bmp : TFPCustomImage);
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
end;

procedure TQRCodeScannerForm.FormDestroy(Sender: TObject);
begin
  FImage.free;
  FScanner.free;
end;

procedure TQRCodeScannerForm.FormShow(Sender: TObject);
begin
  Timer1.enabled := true;
end;

procedure TQRCodeScannerForm.Timer1Timer(Sender: TObject);
begin
  readImage;
  Timer1.Enabled := false;
end;

procedure TQRCodeScannerForm.readImage;
var
  bc : TReadResult;
begin
  bc := FScanner.Scan(FImage);
  try
    if (bc = nil) then
    begin
      btnOk.Enabled := false;
      lblInfo.caption := 'No barcode found';
    end
    else
    begin
      btnOk.enabled := true;
      lblInfo.caption := codesTBarcodeFormat(bc.BarcodeFormat)+' found';
      memo1.Text := bc.text;
    end;
  finally
    bc.free;
  end;
end;

procedure TQRCodeScannerForm.useImage(bmp: TFPCustomImage);
begin
  FImage.assign(bmp);
  Image1.Picture.Assign(FImage);
end;

end.

