unit QlpReedSolomonGenerator;

{$I QRCodeGenLib.inc}

interface

uses
  SyncObjs,
  QlpIReedSolomonGenerator,
  QlpBits,
  QlpArrayUtils,
  QlpQRCodeGenLibTypes;

resourcestring
  SDegreeOutOfRange = 'Degree out of range';

type
  TReedSolomonGenerator = class sealed(TInterfacedObject, IReedSolomonGenerator)

  strict private
  const
    MAX_DEGREE = Int32(30);

  class var
    FCache: array [0 .. MAX_DEGREE + 1] of IReedSolomonGenerator;
    FIsPending: array [0 .. MAX_DEGREE + 1] of Boolean;
    FLock: TCriticalSection;

  var
    // A table of size 256 * degree, where FPolynomialMultiply[i][j] := Multiply(i, coefficients[j]).
    // 'coefficients' is the temporary array computed in the constructor.
    FPolynomialMultiply: TQRCodeGenLibMatrixByteArray;

    // Creates a Reed-Solomon ECC generator polynomial for the given degree.
    constructor Create(ADegree: Int32);
    // Returns the product of the two given field elements modulo GF(2^8/$11D). The arguments and result
    // are unsigned 8-bit integers. This could be implemented as a lookup table of 256*256 entries of uint8.
    class function Multiply(Ax, Ay: Int32): Int32; static;

    class constructor CreateReedSolomonGenerator();
    class destructor DestroyReedSolomonGenerator();

  public
    // Returns the error correction codeword for the given data polynomial and this divisor polynomial.
    procedure GetRemainder(const AData: TQRCodeGenLibByteArray;
      ADataOff, ADataLen: Int32; const AResult: TQRCodeGenLibByteArray);
    class function GetInstance(ADegree: Int32): IReedSolomonGenerator; static;

  end;

implementation

{ TReedSolomonGenerator }

constructor TReedSolomonGenerator.Create(ADegree: Int32);
var
  LCoefficients: TQRCodeGenLibByteArray;
  LRoot, LIIdx, LJIdx: Int32;
begin
  Inherited Create();
  if ((ADegree < 1) or (ADegree > 255)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SDegreeOutOfRange);
  end;

  // The divisor polynomial, whose coefficients are stored from highest to lowest power.
  // For example, x^3 + 255x^2 + 8x + 93 is stored as the uint8 array {255, 8, 93}.
  System.SetLength(LCoefficients, ADegree);
  // Start off with the monomial x^0
  LCoefficients[ADegree - 1] := 1;

  // Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
  // and drop the highest monomial term which is always 1x^degree.
  // Note that r = $02, which is a generator element of this field GF(2^8/$11D).
  LRoot := 1;
  LIIdx := 0;
  while LIIdx < ADegree do
  begin
    // Multiply the current product by (x - r^i)
    for LJIdx := System.Low(LCoefficients) to System.High(LCoefficients) do
    begin
      LCoefficients[LJIdx] :=
        Byte(Multiply(LCoefficients[LJIdx] and $FF, LRoot));
      if ((LJIdx + 1) < System.Length(LCoefficients)) then
      begin
        LCoefficients[LJIdx] := LCoefficients[LJIdx]
          xor (LCoefficients[LJIdx + 1]);
      end;
    end;
    LRoot := Multiply(LRoot, $02);
    System.Inc(LIIdx);
  end;

  System.SetLength(FPolynomialMultiply, 256);

  for LIIdx := System.Low(FPolynomialMultiply)
    to System.High(FPolynomialMultiply) do
  begin
    // resize dimension of inner array
    System.SetLength(FPolynomialMultiply[LIIdx], ADegree);
    for LJIdx := 0 to System.Pred(ADegree) do
    begin
      FPolynomialMultiply[LIIdx][LJIdx] :=
        Byte(Multiply(LIIdx, LCoefficients[LJIdx] and $FF));
    end;
  end;
end;

class constructor TReedSolomonGenerator.CreateReedSolomonGenerator;
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

class destructor TReedSolomonGenerator.DestroyReedSolomonGenerator;
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

class function TReedSolomonGenerator.GetInstance(ADegree: Int32)
  : IReedSolomonGenerator;
begin
  if ((ADegree < 1) or (ADegree > MAX_DEGREE)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SDegreeOutOfRange);
  end;

  while True do
  begin
    FLock.Acquire;
    try
      result := FCache[ADegree];
      if result <> Nil then
      begin
        Exit;
      end;

      if (not(FIsPending[ADegree])) then
      begin
        FIsPending[ADegree] := True;
        break;
      end;
    finally
      FLock.Release;
    end;
  end;

  result := TReedSolomonGenerator.Create(ADegree);
  FLock.Acquire;
  try
    FCache[ADegree] := result;
    FIsPending[ADegree] := False;
  finally
    FLock.Release;
  end;
end;

procedure TReedSolomonGenerator.GetRemainder(const AData
  : TQRCodeGenLibByteArray; ADataOff, ADataLen: Int32;
  const AResult: TQRCodeGenLibByteArray);
var
  LDegree, LIIdx, LJIdx, LDataEnd: Int32;
  LTable: TQRCodeGenLibByteArray;
begin
  LDegree := System.Length(FPolynomialMultiply[0]);
{$IFDEF DEBUG}
  System.Assert(System.Length(AResult) = LDegree);
{$ENDIF DEBUG}
  TArrayUtils.Fill(AResult, Byte(0));
  LIIdx := ADataOff;
  LDataEnd := ADataOff + ADataLen;
  // Polynomial division
  while LIIdx < LDataEnd do
  begin
    LTable := FPolynomialMultiply[(AData[LIIdx] xor AResult[0]) and $FF];
    LJIdx := 0;
    while LJIdx < (System.Pred(LDegree)) do
    begin
      AResult[LJIdx] := Byte(AResult[LJIdx + 1] xor LTable[LJIdx]);
      System.Inc(LJIdx);
    end;
    AResult[LDegree - 1] := LTable[LDegree - 1];
    System.Inc(LIIdx);
  end;

end;

class function TReedSolomonGenerator.Multiply(Ax, Ay: Int32): Int32;
var
  Lz, LIdx: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(((Ax shr 8) = 0) and ((Ay shr 8) = 0));
{$ENDIF DEBUG}
  // Russian peasant multiplication
  Lz := 0;
  LIdx := 7;
  while LIdx >= 0 do
  begin
    Lz := (Lz shl 1) xor ((TBits.Asr32(Lz, 7)) * $11D);
    Lz := Lz xor (((TBits.Asr32(Ay, LIdx)) and 1) * Ax);
    System.Dec(LIdx);
  end;
{$IFDEF DEBUG}
  System.Assert((TBits.Asr32(Lz, 8) = 0));
{$ENDIF DEBUG}
  result := Lz;
end;

end.
