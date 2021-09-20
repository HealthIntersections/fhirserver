unit QlpQrTemplate;

{$I QRCodeGenLib.inc}

interface

uses
  Math,
  SyncObjs,
  QlpIQrTemplate,
  QlpQrCodeCommons,
  QlpBits,
  QlpQRCodeGenLibTypes;

resourcestring
  SVersionOutOfRange = 'Version out of range';
  SVersionNumberOutOfRange = 'Version number out of range';
  SInvalidState = 'Invalid state encountered.';

type
  TQrTemplate = class sealed(TInterfacedObject, IQrTemplate)
  strict private
  const
    MIN_VERSION = TQrCodeCommons.MIN_VERSION;
    MAX_VERSION = TQrCodeCommons.MAX_VERSION;

  class var
    FCache: array [0 .. MAX_VERSION + 1] of IQrTemplate;
    FIsPending: array [0 .. MAX_VERSION + 1] of Boolean;
    FLock: TCriticalSection;

  var
    FVersion: Int32; // In the range [1, 40].
    FSize: Int32; // Derived from version.

    // "FIsFunction" Indicates function modules that are not subjected to masking. Discarded when constructor finishes.
    // Otherwise when the constructor is running, System.length(FIsFunction) == System.length(FTemplate).
    FIsFunction: TQRCodeGenLibInt32Array;
    FTemplate: TQRCodeGenLibInt32Array;
    // Length and values depend on version.
    FDataOutputBitIndexes: TQRCodeGenLibInt32Array;
    // System.length(FMasks) == 8, and System.length(FMasks[i]) == System.length(FTemplate).
    FMasks: TQRCodeGenLibMatrixInt32Array;

    function GetTemplate(): TQRCodeGenLibInt32Array; inline;
    function GetDataOutputBitIndexes(): TQRCodeGenLibInt32Array; inline;
    function GetMasks(): TQRCodeGenLibMatrixInt32Array;

    // Returns the value of the bit at the given coordinates in the given grid.
    function GetModule(const AGrid: TQRCodeGenLibInt32Array; Ax, Ay: Int32)
      : Int32; inline;

    // Returns an ascending list of positions of alignment patterns for this version number.
    // Each position is in the range [0,177], and are used on both the x and y axes.
    // This could be implemented as lookup table of 40 variable-length lists of unsigned bytes.
    function GetAlignmentPatternPositions(): TQRCodeGenLibInt32Array;

    // Computes and returns an array of bit indexes, based on this object's various fields.
    function GenerateZigZagScan(): TQRCodeGenLibInt32Array;

    // Computes and returns a new array of masks, based on this object's various fields.
    function GenerateMasks(): TQRCodeGenLibMatrixInt32Array;

    // Reads this object's version field, and draws and marks all function modules.
    procedure DrawFunctionPatterns();
    // Draws two blank copies of the format bits.
    procedure DrawDummyFormatBits();
    // Draws two copies of the version bits (with its own error correction code),
    // based on this object's version field, iff 7 <= version <= 40.
    procedure DrawVersion();
    // Draws a 9*9 finder pattern including the border separator,
    // with the center module at (Ax, Ay). Modules can be out of bounds.
    procedure DrawFinderPattern(Ax, Ay: Int32);
    // Draws a 5*5 alignment pattern, with the center module
    // at (Ax, Ay). All modules must be in bounds.
    procedure DrawAlignmentPattern(Ax, Ay: Int32);
    // Marks the module at the given coordinates as a function module.
    // Also either sets that module black or keeps its color unchanged.
    procedure DarkenFunctionModule(Ax, Ay, AEnable: Int32); inline;

    // Creates a QR Code template for the given version number.
    constructor Create(AVersion: Int32);

    class constructor CreateQrTemplate();
    class destructor DestroyQrTemplate();

  public

    property Template: TQRCodeGenLibInt32Array read GetTemplate;
    property DataOutputBitIndexes: TQRCodeGenLibInt32Array
      read GetDataOutputBitIndexes;
    property Masks: TQRCodeGenLibMatrixInt32Array read GetMasks;

    class function GetInstance(AVersion: Int32): IQrTemplate; static;
    // Returns the number of data bits that can be stored in a QR Code of the given version number, after
    // all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
    // The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
    class function GetNumRawDataModules(AVersion: Int32): Int32; static;

  end;

implementation

{ TQrTemplate }

constructor TQrTemplate.Create(AVersion: Int32);
begin
  Inherited Create();
  if ((AVersion < MIN_VERSION) or (AVersion > MAX_VERSION)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SVersionOutOfRange);
  end;
  FVersion := AVersion;
  FSize := (AVersion * 4) + 17;
  System.SetLength(FTemplate, ((FSize * FSize) + 31) shr 5);
  System.SetLength(FIsFunction, System.Length(FTemplate));

  DrawFunctionPatterns(); // Reads and writes fields
  FMasks := GenerateMasks(); // Reads fields, returns array

  FDataOutputBitIndexes := GenerateZigZagScan(); // Reads fields, returns array
  FIsFunction := Nil;
end;

class constructor TQrTemplate.CreateQrTemplate;
var
  LIdx: Int32;
begin
  // Initialize static array to their default state to avoid junk values inside.
  for LIdx := System.Low(FCache) to System.High(FCache) do
  begin
    FCache[LIdx] := Nil;
    FIsPending[LIdx] := False;
  end;
  FLock := TCriticalSection.Create;
end;

function TQrTemplate.GetModule(const AGrid: TQRCodeGenLibInt32Array;
  Ax, Ay: Int32): Int32;
var
  LIdx: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((0 <= Ax) and (Ax < FSize));
  System.Assert((0 <= Ay) and (Ay < FSize));
{$ENDIF DEBUG}
  LIdx := (Ay * FSize) + Ax;
  result := TQrCodeCommons.GetBit(AGrid[TBits.Asr32(LIdx, 5)], LIdx);
end;

class function TQrTemplate.GetNumRawDataModules(AVersion: Int32): Int32;
var
  numAlign: Int32;
begin
  if ((AVersion < MIN_VERSION) or (AVersion > MAX_VERSION)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SVersionNumberOutOfRange);
  end;
  result := (((16 * AVersion) + 128) * AVersion) + 64;
  if (AVersion >= 2) then
  begin
    numAlign := (AVersion div 7) + 2;
    result := result - ((((25 * numAlign) - 10) * numAlign) - 55);
    if (AVersion >= 7) then
    begin
      result := result - 36;
    end;
  end;
end;

procedure TQrTemplate.DarkenFunctionModule(Ax, Ay, AEnable: Int32);
var
  LIdx: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((0 <= Ax) and (Ax < FSize));
  System.Assert((0 <= Ay) and (Ay < FSize));
  System.Assert((AEnable = 0) or (AEnable = 1));
{$ENDIF DEBUG}
  LIdx := (Ay * FSize) + Ax;
  FTemplate[TBits.Asr32(LIdx, 5)] := FTemplate[TBits.Asr32(LIdx, 5)] or
    (TBits.LeftShift32(AEnable, LIdx));
  FIsFunction[TBits.Asr32(LIdx, 5)] := FIsFunction[TBits.Asr32(LIdx, 5)] or
    (TBits.LeftShift32(1, LIdx));
end;

class destructor TQrTemplate.DestroyQrTemplate;
var
  LIdx: Int32;
begin
  // Initialize static array to their default state to clear former contents.
  for LIdx := System.Low(FCache) to System.High(FCache) do
  begin
    FCache[LIdx] := Nil;
    FIsPending[LIdx] := False;
  end;
  FLock.Free;
end;

procedure TQrTemplate.DrawAlignmentPattern(Ax, Ay: Int32);
var
  Ldy, Ldx: Int32;
begin
  for Ldy := -2 to 2 do
  begin
    for Ldx := -2 to 2 do
    begin
      DarkenFunctionModule(Ax + Ldx, Ay + Ldy,
        Abs(Max(Abs(Ldx), Abs(Ldy)) - 1));
    end;
  end;
end;

procedure TQrTemplate.DrawDummyFormatBits;
var
  LIdx: Int32;
begin
  // Draw first copy
  for LIdx := 0 to 5 do
  begin
    DarkenFunctionModule(8, LIdx, 0);
  end;
  DarkenFunctionModule(8, 7, 0);
  DarkenFunctionModule(8, 8, 0);
  DarkenFunctionModule(7, 8, 0);

  for LIdx := 9 to System.Pred(15) do
  begin
    DarkenFunctionModule(14 - LIdx, 8, 0);
  end;

  // Draw second copy
  for LIdx := 0 to System.Pred(8) do
  begin
    DarkenFunctionModule(FSize - 1 - LIdx, 8, 0);
  end;

  for LIdx := 8 to System.Pred(15) do
  begin
    DarkenFunctionModule(8, FSize - 15 + LIdx, 0);
  end;
  DarkenFunctionModule(8, FSize - 8, 1); // Always black
end;

procedure TQrTemplate.DrawFinderPattern(Ax, Ay: Int32);
var
  Ldy, Ldx, LDist, Lxx, Lyy, LEnable: Int32;
begin
  for Ldy := -4 to 4 do
  begin
    for Ldx := -4 to 4 do
    begin
      LDist := Max(Abs(Ldy), Abs(Ldx)); // Chebyshev/infinity norm
      Lxx := Ax + Ldx;
      Lyy := Ay + Ldy;
      if ((0 <= Lxx) and (Lxx < FSize) and (0 <= Lyy) and (Lyy < FSize)) then
      begin
        if ((LDist <> 2) and (LDist <> 4)) then
        begin
          LEnable := 1;
        end
        else
        begin
          LEnable := 0;
        end;
        DarkenFunctionModule(Lxx, Lyy, LEnable);
      end;
    end;
  end;
end;

procedure TQrTemplate.DrawFunctionPatterns;
var
  LIIdx, LJIdx, LNumAlign: Int32;
  LAlignPatPos: TQRCodeGenLibInt32Array;
begin
  // Draw horizontal and vertical timing patterns
  for LIIdx := 0 to System.Pred(FSize) do
  begin
    DarkenFunctionModule(6, LIIdx, (not LIIdx) and 1);
    DarkenFunctionModule(LIIdx, 6, (not LIIdx) and 1);
  end;

  // Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
  DrawFinderPattern(3, 3);
  DrawFinderPattern(FSize - 4, 3);
  DrawFinderPattern(3, FSize - 4);

  // Draw numerous alignment patterns
  LAlignPatPos := GetAlignmentPatternPositions();
  LNumAlign := System.Length(LAlignPatPos);
  for LIIdx := 0 to System.Pred(LNumAlign) do
  begin
    for LJIdx := 0 to System.Pred(LNumAlign) do
    begin
      if (not(((LIIdx = 0) and (LJIdx = 0)) or ((LIIdx = 0) and
        (LJIdx = LNumAlign - 1)) or ((LIIdx = LNumAlign - 1) and (LJIdx = 0))))
      then
      begin
        DrawAlignmentPattern(LAlignPatPos[LIIdx], LAlignPatPos[LJIdx]);
      end;
    end;
  end;

  // Draw configuration data
  DrawDummyFormatBits();
  DrawVersion();
end;

procedure TQrTemplate.DrawVersion;
var
  LRem, LIdx, LBit, LBits, La, Lb: Int32;
begin
  if (FVersion < 7) then
  begin
    Exit;
  end;

  // Calculate error correction code and pack bits
  LRem := FVersion; // version is uint6, in the range [7, 40]
  LIdx := 0;
  while LIdx < 12 do
  begin
    LRem := (LRem shl 1) xor ((TBits.Asr32(LRem, 11)) * $1F25);
    System.Inc(LIdx);
  end;
  LBits := (FVersion shl 12) or LRem; // uint18
{$IFDEF DEBUG}
  System.Assert(TBits.Asr32(LBits, 18) = 0);
{$ENDIF DEBUG}
  // Draw two copies
  for LIdx := 0 to System.Pred(18) do
  begin
    LBit := TQrCodeCommons.GetBit(LBits, LIdx);
    La := FSize - 11 + (LIdx mod 3);
    Lb := LIdx div 3;
    DarkenFunctionModule(La, Lb, LBit);
    DarkenFunctionModule(Lb, La, LBit);
  end;
end;

function TQrTemplate.GenerateMasks: TQRCodeGenLibMatrixInt32Array;
var
  LMask, Ly, LIdx, Lx, LBit: Int32;
  LInvert: Boolean;
  LMaskModules: TQRCodeGenLibInt32Array;
begin
  System.SetLength(result, 8);
  for LMask := System.Low(result) to System.High(result) do
  begin
    // resize dimension of inner array
    System.SetLength(result[LMask], System.Length(FTemplate));
    LMaskModules := result[LMask];
    Ly := 0;
    LIdx := 0;
    while Ly < FSize do
    begin
      Lx := 0;
      while Lx < FSize do
      begin
        case LMask of
          0:
            begin
              LInvert := (Lx + Ly) and 1 = 0;
            end;
          1:
            begin
              LInvert := Ly and 1 = 0;
            end;
          2:
            begin
              LInvert := Lx mod 3 = 0;
            end;
          3:
            begin
              LInvert := (Lx + Ly) mod 3 = 0;
            end;
          4:
            begin
              LInvert := ((Lx div 3) + (Ly shr 1)) and 1 = 0;
            end;
          5:
            begin
              LInvert := ((Lx * Ly) and 1) + Lx * Ly mod 3 = 0;
            end;
          6:
            begin
              LInvert := (((Lx * Ly) and 1) + Lx * Ly mod 3) and 1 = 0;
            end;
          7:
            begin
              LInvert := (((Lx + Ly) and 1) + Lx * Ly mod 3) and 1 = 0;
            end
        else
          begin
            raise EInvalidOperationQRCodeGenLibException.CreateRes
              (@SInvalidState);
          end;

        end;

        if LInvert then
        begin
          LBit := 1 and (not GetModule(FIsFunction, Lx, Ly));
        end
        else
        begin
          LBit := 0 and (not GetModule(FIsFunction, Lx, Ly));
        end;

        LMaskModules[TBits.Asr32(LIdx, 5)] := LMaskModules[TBits.Asr32(LIdx, 5)
          ] or (TBits.LeftShift32(LBit, LIdx));

        System.Inc(Lx);
        System.Inc(LIdx);
      end;
      System.Inc(Ly);
    end;
  end;
end;

function TQrTemplate.GenerateZigZagScan: TQRCodeGenLibInt32Array;
var
  LIIdx, LJIdx, LRight, LVert, Lx, Ly: Int32;
  LUpward: Boolean;
begin
  System.SetLength(result, ((GetNumRawDataModules(FVersion) div 8) * 8));
  LIIdx := 0; // Bit index into the data
  LRight := FSize - 1;
  while LRight >= 1 do
  begin // Index of right column in each column pair
    if (LRight = 6) then
    begin
      LRight := 5;
    end;
    LVert := 0;
    while LVert < FSize do
    begin
      // Vertical counter
      LJIdx := 0;
      while LJIdx < 2 do
      begin
        Lx := LRight - LJIdx; // Actual x coordinate
        LUpward := ((LRight + 1) and 2) = 0;
        // Actual y coordinate
        if LUpward then
        begin
          Ly := FSize - 1 - LVert;
        end
        else
        begin
          Ly := LVert;
        end;

        if ((GetModule(FIsFunction, Lx, Ly) = 0) and
          (LIIdx < System.Length(result))) then
        begin
          result[LIIdx] := (Ly * FSize) + Lx;
          System.Inc(LIIdx);
        end;
        System.Inc(LJIdx);
      end;
      System.Inc(LVert);
    end;
    System.Dec(LRight, 2);
  end;
{$IFDEF DEBUG}
  System.Assert(LIIdx = System.Length(result));
{$ENDIF DEBUG}
end;

function TQrTemplate.GetAlignmentPatternPositions: TQRCodeGenLibInt32Array;
var
  LNumAlign, LStep, LIdx, LPos: Int32;
begin
  if (FVersion = 1) then
  begin
    result := Nil;
    Exit;
  end
  else
  begin
    LNumAlign := (FVersion div 7) + 2;
    if (FVersion = 32) then
    begin
      LStep := 26;
    end
    else
    begin
      LStep := (((FVersion * 4) + (LNumAlign * 2) + 1)
        div ((LNumAlign * 2) - 2)) * 2;
    end;
    System.SetLength(result, LNumAlign);
    result[0] := 6;
    LIdx := System.Length(result) - 1;
    LPos := FSize - 7;
    while LIdx >= 1 do
    begin
      result[LIdx] := LPos;
      System.Dec(LIdx);
      System.Dec(LPos, LStep);
    end;
  end;
end;

function TQrTemplate.GetDataOutputBitIndexes: TQRCodeGenLibInt32Array;
begin
  result := System.Copy(FDataOutputBitIndexes);
end;

class function TQrTemplate.GetInstance(AVersion: Int32): IQrTemplate;
begin
  if ((AVersion < MIN_VERSION) or (AVersion > MAX_VERSION)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SVersionOutOfRange);
  end;

  while True do
  begin
    FLock.Acquire;
    try
      result := FCache[AVersion];
      if result <> Nil then
      begin
        Exit;
      end;

      if (not(FIsPending[AVersion])) then
      begin
        FIsPending[AVersion] := True;
        Break;
      end;
    finally
      FLock.Release;
    end;
  end;

  result := TQrTemplate.Create(AVersion);
  FLock.Acquire;
  try
    FCache[AVersion] := result;
    FIsPending[AVersion] := False;
  finally
    FLock.Release;
  end;
end;

function TQrTemplate.GetMasks: TQRCodeGenLibMatrixInt32Array;
var
  LIdx: Int32;
begin
  // since System.Copy() does not support jagged arrays (multidimensional dynamic arrays, we improvise)
  System.SetLength(result, System.Length(FMasks));
  for LIdx := System.Low(result) to System.High(result) do
  begin
    result[LIdx] := System.Copy(FMasks[LIdx]);
  end;
end;

function TQrTemplate.GetTemplate: TQRCodeGenLibInt32Array;
begin
  result := System.Copy(FTemplate);
end;

end.
