unit uQRCTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, qrcodegen, StdCtrls, ExtCtrls, ComCtrls, uQRCode;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Image1: TImage;
    btnSegments: TButton;
    btnVariety: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btnMask: TButton;
    btnNewClass: TButton;
    btnNewClassSeg: TButton;
    procedure btnMaskClick(Sender: TObject);
    procedure btnNewClassClick(Sender: TObject);
    procedure btnNewClassSegClick(Sender: TObject);
    procedure btnSegmentsClick(Sender: TObject);
    procedure btnVarietyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure LogToMemo(Text : string);
  public
    { Public declarations }
    Bitmap1: TBitmap;
    procedure PrintQr(qrcode: TBytes; filename: string); overload;
    procedure PrintQr(qrcode: TQRCode; filename: string); overload;
  end;

var
  Form1: TForm1;

implementation

uses qrcodegen_demo;

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Bitmap1 := TBitmap.Create;
  Bitmap1.PixelFormat := pf32Bit;
  Bitmap1.SetSize(30, 30);
  Bitmap1.Canvas.Brush.Color := clWhite;
  Bitmap1.Canvas.FillRect(Rect(0, 0, 30, 30));
  Image1.Picture.Assign(Bitmap1);

  PrintQRProc := PrintQR;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Bitmap1.Free;
end;

procedure TForm1.PrintQr(qrcode: TBytes; filename: string);
var
  size: integer;
  x, y: integer;
const
  hdr = 2;
begin
  size := GetQRSize(qrcode);
  Bitmap1.SetSize((hdr * 2 + size) * 4, (hdr * 2 + size) * 4);
  Bitmap1.Canvas.Brush.Color := clWhite;
  Bitmap1.Canvas.FillRect(Rect(0, 0, Bitmap1.Width - 1, Bitmap1.Height - 1));
  for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
    begin
      if GetQRModule(qrcode, x, y) then
        Bitmap1.Canvas.Brush.Color := clBlack
      else
        Bitmap1.Canvas.Brush.Color := clWhite;
      Bitmap1.Canvas.FillRect(Rect((hdr + x) * 4, (hdr + y) * 4, (hdr + x + 1) * 4, (hdr + y + 1) * 4));
    end;
  end;

  WriteToFile(qrcode, size * size, filename + '.bin');

  Image1.Picture.Assign(Bitmap1);
end;

procedure TForm1.PrintQr(qrcode: TQRCode; filename: string);
var
  size: integer;
  x, y: integer;
const
  hdr = 2;
begin
  size := qrcode.Size;
  Bitmap1.SetSize((hdr * 2 + size) * 4, (hdr * 2 + size) * 4);
  Bitmap1.Canvas.Brush.Color := clWhite;
  Bitmap1.Canvas.FillRect(Rect(0, 0, Bitmap1.Width - 1, Bitmap1.Height - 1));
  for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
    begin
      if qrcode.Module[x, y] then
        Bitmap1.Canvas.Brush.Color := clBlack
      else
        Bitmap1.Canvas.Brush.Color := clWhite;
      Bitmap1.Canvas.FillRect(Rect((hdr + x) * 4, (hdr + y) * 4, (hdr + x + 1) * 4, (hdr + y + 1) * 4));
    end;
  end;

  qrcode.SaveToFile(filename + '.bin');

  Image1.Picture.Assign(Bitmap1);
end;

procedure TForm1.LogToMemo(Text : string);
begin
  Memo1.Lines.Add(Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
{$IFDEF QRCODEGEN_TESTING}
  LogProc := LogToMemo;

  testIsAlphanumeric;
  testInitializeFunctionModulesEtc;
  testGetSetModuleRandomly;
  testReedSolomonComputeRemainder;
  testGetAlignmentPatternPositions;
  testGetSetModule;
  testMakeAlphanumeric;
  testMakeNumeric;
  testMakeEci;
  testGetTotalBits;
  testMakeBytes;
  testAddEccAndInterleave;
{$ELSE}
  LogToMemo('Must define QRCODEGEN_TESTING for project to execute tests');
{$ENDIF}
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  doBasicDemo;
end;

procedure TForm1.btnMaskClick(Sender: TObject);
begin
  doMaskDemo;
end;

procedure TForm1.btnSegmentsClick(Sender: TObject);
begin
  doSegmentDemo;
end;

procedure TForm1.btnVarietyClick(Sender: TObject);
begin
  doVarietyDemo;
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.btnNewClassClick(Sender: TObject);
var
  qr : TTextQRCode;
begin
  qr := TTextQRCode.Create('Hello, world!');
  try
    PrintQR(qr, 'NewClass');
  finally
    qr.Free;
  end;
end;

procedure TForm1.btnNewClassSegClick(Sender: TObject);
var
  qr : TSegmentedQRCode;
  golden0, golden1, golden2, concat : ansistring;
  bytes : TBytes;
  i : integer;
begin
  golden0 := 'Golden ratio '#$CF#$86' = 1.';
  golden1 := '6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374';
  golden2 := '......';

  SetLength(bytes, Length(golden0) * sizeof(AnsiChar));
  for i := Low(golden0) to High(golden0) do
    bytes[i - 1] := Byte(AnsiChar(golden0[i]));

  qr := TSegmentedQRCode.Create(eclLow);
  try
    qr.AddBinarySegment(bytes);
    qr.AddNumericSegment('6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374');
    qr.AddTextSegment(golden2);
    PrintQR(qr, 'NewClassSeg');
  finally
    qr.Free;
  end;
end;

end.
