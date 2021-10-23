unit uQRCode;

interface

uses
  SysUtils, Classes, qrcodegen;

type
  TQRCode = class(TObject)
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
    procedure AddBinarySegment(ABytes : TBytes);
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
    stream.Free;
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

procedure TSegmentedQRCode.AddBinarySegment(ABytes: TBytes);
var
  segBuffer : TBytes;
  segment : TQRSegment;
begin
  SetLength(segBuffer, CalcQRSegmentBufferSize(qrmBYTE, Length(ABytes)) * sizeof(byte));
  segment := NewQRSegmentWithBinary(ABytes, segBuffer);
  AddSegment(segment);
end;

end.
