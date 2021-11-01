unit dlg_scanner;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Clipbrd,
  FPImage, LCLIntf, LCLType, Menus,
  ZXing.ScanManager, ZXing.BarCodeFormat, ZXing.ReadResult,
  PDFiumCore,
  fsl_utilities,
  fui_lcl_utilities;

type

  { TQRCodeScannerForm }

  TQRCodeScannerForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnReset: TButton;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    btnPDF: TButton;
    Image1: TImage;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlInfo: TPanel;
    Splitter1: TSplitter;
    procedure btnPDFClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FImage : TBitmap;
    FOriginalImage : TBitmap;
    FScanner : TScanManager;
    FStart, FEnd : TPoint;
    FHasImage, FDragging : boolean;
    procedure clear;
    procedure DrawRect;
    function ImageScale: Double;
    function IsInImage(p: TPoint): boolean;
    procedure processPDF(pdf: TPdfDocument);
    function processPDFPageObjects(pg: TPdfPage): boolean;
    //procedure processPDFPage(pg: TPdfPage);
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
var
  i : integer;
  m : TMenuItem;
begin
  setForOs(btnOk, btnCancel);
  FImage := TBitmap.create;
  FOriginalImage := TBitmap.create;
  FScanner := TScanManager.create(TBarcodeFormat.QR_CODE, nil);
  //FScreens := TStringList.create;
  //listScreens(FScreens);
  //pmScreens.Items.Clear;
  //for i := 0 to FScreens.count - 1 do
  //begin
  //  m := TMenuItem.create;
  //  pmScreens.Items.Add(m);
  //  m.Caption := FScreens[i];
  //  m.Tag := i;
  //end;
  clear;
end;

procedure TQRCodeScannerForm.clear;
begin
  if FDragging then
  begin
    DrawRect;
    FDragging := false;
  end;
  Image1.Cursor := crDefault;
  FHasImage := false;
  btnReset.enabled := false;
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
        FHasImage := true;
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

procedure TQRCodeScannerForm.btnResetClick(Sender: TObject);
begin
  clear;
  FImage.assign(FOriginalImage);
  FHasImage := true;
  Image1.Picture.Assign(FImage);
  Image1.Refresh;
  Application.ProcessMessages;
  readImage;
end;

function TQRCodeScannerForm.processPDFPageObjects(pg : TPdfPage) : boolean;
var
  obj : TPDFObject;
  bmp : TBitmap;
  rr : TReadResult;
begin
  result := false;
  for obj in pg.Objects do
  begin
    if obj.kind = potImage then
    begin
      bmp := obj.AsBitmap;
      try
        rr := FScanner.Scan(bmp);
        if (rr <> nil) then
        begin
          clear;
          FImage.assign(bmp);
          FHasImage := true;
          Image1.Picture.Assign(FImage);
          Image1.Refresh;
          Application.ProcessMessages;
          readImage;
          exit(true);
        end;
      finally
        bmp.free;
      end;
    end;
  end;
end;

//procedure TQRCodeScannerForm.processPDFPage(pg : TPdfPage);
//var
//  bmp : TBitmap;
//  rr : TReadResult;
//begin
//  bmp := TBitmap.create;
//  try
//    bmp.width := 1500;
//    bmp.height := trunc((bmp.width / pg.Width) * pg.Height);
//    pg.Draw(bmp);
//    clear;
//    FImage.assign(bmp);
//    FHasImage := true;
//    Image1.Picture.Assign(FImage);
//    Image1.Refresh;
//    Application.ProcessMessages;
//    readImage;
//  finally
//    bmp.free;
//  end;
//end;

procedure TQRCodeScannerForm.processPDF(pdf : TPdfDocument);
var
  i : integer;
  pg : TPdfPage;
begin
  for i := 0 to pdf.PageCount - 1 do
  begin
    pg := pdf.Pages[i];
    if processPDFPageObjects(pg) then
      exit;
  end;
  //if pdf.PageCount > 0 then
  //  processPDFPage(pg);
end;

procedure TQRCodeScannerForm.btnPDFClick(Sender: TObject);
var
  dlg : TOpenDialog;
  pdf : TPdfDocument;
begin
  dlg := TOpenDialog.create(self);
  try
    dlg.Filter := 'PDF Files|*.pdf|'+
      'All Files|*.*';
    dlg.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];
    if dlg.execute then
    begin
      clear;
      pdf := TPdfDocument.Create;
      try
        pdf.LoadFromFile(dlg.filename, '', dloMemory);
        processPDF(pdf);
      finally
        pdf.free;
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
      FHasImage := true;
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

procedure TQRCodeScannerForm.Button4Click(Sender: TObject);
var
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  try
    screenshot(bmp);
    FImage.assign(bmp);
    FHasImage := true;
    Image1.Picture.Assign(FImage);
    Image1.Refresh;
    Application.ProcessMessages;
    readImage;
  finally
    bmp.free;
  end;
end;

procedure TQRCodeScannerForm.FormDestroy(Sender: TObject);
begin
  FImage.free;
  FOriginalImage.Free;
  FScanner.free;
end;


procedure TQRCodeScannerForm.DrawRect;
begin
  Image1.Canvas.Pen.Color := clAqua;
  Image1.Canvas.Pen.Width := trunc(2 * ImageScale);
  Image1.Canvas.pen.Mode := pmXor;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(trunc(FStart.X * ImageScale), trunc(FStart.Y * ImageScale), trunc(FEnd.X * ImageScale), trunc(FEnd.Y * ImageScale));
end;

function TQRCodeScannerForm.ImageScale : Double;
var
  hi, wi, hp, wp : integer;
  rw, rh : Double;
begin
  hi := Image1.Height;
  wi := Image1.Width;
  hp := Image1.Picture.Height;
  wp := Image1.Picture.Width;
  rw := wp/wi;
  rh := hp/hi;
  if rh > rw then
    result := rh
  else
    result := rw;
end;

function TQRCodeScannerForm.IsInImage(p : TPoint) : boolean;
begin
  if not FHasImage then
    result := false
  else
    result := (p.y <= Image1.picture.height / ImageScale) and (p.x < Image1.picture.width / ImageScale);
end;

procedure TQRCodeScannerForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FStart.X := X;
  FStart.Y := Y;
  FStart := Image1.ParentToClient(FStart);
  if isInImage(FStart) then
  begin
    FDragging := true;
    FEnd := FStart;
    DrawRect;
  end;
end;

procedure TQRCodeScannerForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
begin
  p.X := X;
  p.Y := Y;
  p := Image1.ParentToClient(p);
  if FDragging then
  begin
    DrawRect;
    if IsInImage(p) then
    begin
      FEnd := p;
      DrawRect;
    end;
  end
  else if IsInImage(p) then
    Image1.Cursor := crCross
  else
    Image1.Cursor := crDefault;
end;

procedure TQRCodeScannerForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  bmp : TBitmap;
  src, dst : TRect;
begin
  if FDragging then
  begin
    DrawRect;
    FDragging := false;
    if (abs(FStart.x - FEnd.x) > 10) and (abs(FStart.y - FEnd.y) > 10) then
    begin
      if not btnReset.enabled then
        FOriginalImage.assign(Image1.picture);
      bmp := TBitmap.create;
      try
        src.Top := Trunc(IntegerMin(FStart.y, FEnd.Y) * ImageScale);
        src.Bottom := Trunc(IntegerMax(FStart.y, FEnd.Y) * ImageScale);
        src.Left := Trunc(IntegerMin(FStart.x, FEnd.x) * ImageScale);
        src.Right := Trunc(IntegerMax(FStart.x, FEnd.x) * ImageScale);
        dst.Top := 0;
        dst.Left := 0;
        dst.Bottom := src.Height;
        dst.right := src.width;
        bmp.height := dst.Height;
        bmp.Width := dst.Width;
        bmp.Canvas.CopyRect(dst, Image1.Canvas, src);
        FImage.assign(bmp);
        FHasImage := true;
        Image1.Picture.Assign(FImage);
        Image1.Refresh;
        Application.ProcessMessages;
        readImage;
      finally
        bmp.free;
      end;
      btnReset.enabled := true;
    end;
  end;
end;

procedure TQRCodeScannerForm.readImage;
var
  bc : TReadResult;
begin
  pnlInfo.caption := '  Looking for images...';
  pnlInfo.update;
  memo1.Color := clBtnFace;
  memo1.Text := '';
  memo1.Update;
  Application.ProcessMessages;

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
        pnlInfo.caption := '  QR code found';
        memo1.Text := bc.text;
        memo1.Color := clWhite;
      end;
    finally
      bc.free;
    end;
  finally
    screen.cursor := crDefault;
  end;
end;

initialization
  {$IFDEF WINDOWS}
  PDFiumDllFileName := 'libpdf.dll';
  {$ENDIF}
end.

