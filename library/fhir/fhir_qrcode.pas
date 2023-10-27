unit fhir_qrcode;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Graphics,
  qrcodegen,
  fsl_base;

type
  TQRCode = class (TFslObject)
  { Metadata }
  strict private
    FECL : TErrorCorrectionLevel;
    FMinVersion : TQRVersion;
    FMaxVersion : TQRVersion;
    FMask : TQRMask;
    FBoostECL : boolean;
  public
    property ECL : TErrorCorrectionLevel read FECL;
    property MinVersion : TQRVersion read FMinVersion;
    property MaxVersion : TQRVersion read FMaxVersion;
    property Mask : TQRMask read FMask;
    property BoostECL : boolean read FBoostECL;

  { QR generation }
  strict private
    FGenerated : boolean;
    function GetSize : integer;
    function GetModule(X, Y : integer) : boolean;
    procedure Generate;
  strict protected
    FBuffer : TBytes;
    function NewTempBuffer : TBytes;
    procedure DoGenerate; virtual; abstract;
  public
    property Size : integer read GetSize;
    property Module[X, Y : integer] : boolean read GetModule;

  public
    procedure SaveToFile(const Filename : string);
    procedure SaveToBitmap(bmp : TBitmap);
  strict protected
    constructor Create(AECL : TErrorCorrectionLevel;
                       AMinVersion : TQRVersion;
                       AMaxVersion : TQRVersion;
                       AMask : TQRMask;
                       ABoostECL : boolean);
  end;


  TTextQRCode = class(TQRCode)
  strict private
    FText : ansistring;
  strict protected
    procedure DoGenerate; override;
  public
    constructor Create(const AText: ansistring;
                       AECL : TErrorCorrectionLevel = eclLow;
                       AMinVersion : TQRVersion = Low(TQRVersion);
                       AMaxVersion : TQRVersion = High(TQRVersion);
                       AMask : TQRMask = qrMaskAuto;
                       ABoostECL : boolean = True);
  end;


  TBinaryQRCode = class(TQRCode)
  strict private
    FBytes : TBytes;
  strict protected
    procedure DoGenerate; override;
  public
    constructor Create(ABytes: TBytes;
                       AECL : TErrorCorrectionLevel = eclLow;
                       AMinVersion : TQRVersion = Low(TQRVersion);
                       AMaxVersion : TQRVersion = High(TQRVersion);
                       AMask : TQRMask = qrMaskAuto;
                       ABoostECL : boolean = True);
  end;


  TSegmentedQRCode = class(TQRCode)
  strict private
    FSegments : TQRSegments;
    procedure AddSegment(const ASegment : TQRSegment);
  strict protected
    procedure DoGenerate; override;
  public
    procedure AddTextSegment(const AText : ansistring);
    procedure AddNumericSegment(ANumber : int64); overload;
    procedure AddNumericSegment(const ANumber : ansistring); overload;
    procedure AddBinarySegment(const ABytes : TBytes);  overload;
    procedure AddBinarySegment(const ABytes : string); overload;
  public
    constructor Create(AECL : TErrorCorrectionLevel = eclLow;
                       AMinVersion : TQRVersion = Low(TQRVersion);
                       AMaxVersion : TQRVersion = High(TQRVersion);
                       AMask : TQRMask = qrMaskAuto;
                       ABoostECL : boolean = True);
  end;

implementation

{ TQRCode }

constructor TQRCode.Create(AECL : TErrorCorrectionLevel; AMinVersion : TQRVersion; AMaxVersion : TQRVersion; AMask : TQRMask; ABoostECL : boolean);
begin
  inherited Create;
  SetLength(FBuffer, QR_BUFFER_LEN_MAX);
  FECL := AECL;
  FMinVersion := AMinVersion;
  FMaxVersion := AMaxVersion;
  FMask := AMask;
  FBoostECL := ABoostECL;
end;

function TQRCode.NewTempBuffer: TBytes;
begin
  Result := nil;
  SetLength(Result, QR_BUFFER_LEN_MAX);
end;

function TQRCode.GetSize: integer;
begin
  if not FGenerated then
    Generate;
  Result := GetQRSize(FBuffer);
end;

function TQRCode.GetModule(X, Y: integer): boolean;
begin
  if not FGenerated then
    Generate;
  Result := qrcodegen.GetQRModule(FBuffer, X, Y);
end;

procedure TQRCode.Generate;
begin
  DoGenerate;
  FGenerated := True;
end;

procedure TQRCode.SaveToBitmap(bmp: TBitmap);
var
  x, y : integer;
const
  hdr = 4;
begin
  if not FGenerated then
    Generate;

  bmp.SetSize ((hdr * 2 + size) * 4, (hdr * 2 + size) * 4);
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect (Rect (0, 0, bmp.Width - 1, bmp.Height - 1));
	for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
    begin
      if Module[x, y] then
        bmp.Canvas.Brush.Color := clBlack
      else
        bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect (Rect ((hdr + x) * 4, (hdr + y) * 4, (hdr + x + 1) * 4, (hdr + y + 1) * 4));
    end;
  end;
end;

procedure TQRCode.SaveToFile(const Filename: string);
var
  bitcount, bytecount: integer;
  stream : TFileStream;
begin
  bitcount := Sqr(GetSize);

  if bitcount mod 8 <> 0 then
    bytecount := (bitcount div 8) + 1
  else
    bytecount := (bitcount div 8);

  stream := TFileStream.Create(filename, fmCreate);
  try
    stream.WriteBuffer(FBuffer[0], bytecount);
  finally
    stream.free;
  end;
end;

{ TTextQRCode }

constructor TTextQRCode.Create(const AText: ansistring; AECL : TErrorCorrectionLevel; AMinVersion : TQRVersion; AMaxVersion : TQRVersion; AMask : TQRMask; ABoostECL : boolean);
begin
  inherited Create(AECL, AMinVersion, AMaxVersion, AMask, ABoostECL);
  FText := AText;
end;

procedure TTextQRCode.DoGenerate;
var
  LTemp : TBytes;
begin
  LTemp := NewTempBuffer;
  EncodeText(FText, LTemp, FBuffer, ECL, MinVersion, MaxVersion, Mask, BoostECL);
end;

{ TBinaryQRCode }

constructor TBinaryQRCode.Create(ABytes: TBytes; AECL: TErrorCorrectionLevel; AMinVersion, AMaxVersion: TQRVersion; AMask: TQRMask; ABoostECL: boolean);
begin
  inherited Create(AECL, AMinVersion, AMaxVersion, AMask, ABoostECL);
  FBytes := ABytes;
end;

procedure TBinaryQRCode.DoGenerate;
begin
  EncodeBinary(FBytes, Length(FBytes), FBuffer, ECL, MinVersion, MaxVersion, Mask, BoostECL);
end;

{ TSegmentedQRCode }

constructor TSegmentedQRCode.Create(AECL: TErrorCorrectionLevel; AMinVersion, AMaxVersion: TQRVersion; AMask: TQRMask; ABoostECL: boolean);
begin
  inherited Create(AECL, AMinVersion, AMaxVersion, AMask, ABoostECL);
end;

procedure TSegmentedQRCode.DoGenerate;
var
  LTemp : TBytes;
begin
  LTemp := NewTempBuffer;
  EncodeSegmentsAdvanced(FSegments, ECL, MinVersion, MaxVersion, Mask, BoostECL, LTemp, FBuffer);
end;

procedure TSegmentedQRCode.AddBinarySegment(const ABytes: string);
begin
  AddBinarySegment(TEncoding.UTF8.GetBytes(ABytes));
end;

procedure TSegmentedQRCode.AddSegment(const ASegment: TQRSegment);
begin
  SetLength(FSegments, Length(FSegments) + 1);
  FSegments[High(FSegments)] := ASegment;
end;

procedure TSegmentedQRCode.AddTextSegment(const AText: ansistring);
var
  segBuffer : TBytes;
  segment : TQRSegment;
begin
  SetLength(segBuffer, CalcQRSegmentBufferSize(qrmALPHANUMERIC, Length(AText)) * sizeof(AnsiChar));
  segment := NewQRSegmentWithAlphanumeric(AText, segBuffer);
  AddSegment(segment);
end;

procedure TSegmentedQRCode.AddNumericSegment(const ANumber: ansistring);
var
  segBuffer : TBytes;
  segment : TQRSegment;
begin
  SetLength(segBuffer, CalcQRSegmentBufferSize(qrmALPHANUMERIC, Length(ANumber)) * sizeof(AnsiChar));
  segment := NewQRSegmentWithNumeric(ANumber, segBuffer);
  AddSegment(segment);
end;

procedure TSegmentedQRCode.AddNumericSegment(ANumber: int64);
begin
  AddNumericSegment(AnsiString(IntToStr(ANumber)));
end;

procedure TSegmentedQRCode.AddBinarySegment(const ABytes: TBytes);
var
  segBuffer : TBytes;
  segment : TQRSegment;
begin
  SetLength(segBuffer, CalcQRSegmentBufferSize(qrmBYTE, Length(ABytes)) * sizeof(byte));
  segment := NewQRSegmentWithBinary(ABytes, segBuffer);
  AddSegment(segment);
end;

end.

