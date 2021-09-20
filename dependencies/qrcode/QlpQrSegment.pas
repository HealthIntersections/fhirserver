unit QlpQrSegment;

{$I QRCodeGenLib.inc}

interface

uses
  Math,
  SysUtils,
  QlpIQrSegment,
  QlpQrSegmentMode,
  QlpBitBuffer,
  QlpGuard,
  QlpBits,
  QlpArrayUtils,
  QlpConverters,
  QlpQRCodeGenLibTypes;

resourcestring
  SInvalidValue = 'Invalid value';
  SECIAssignmentOutOfRange = 'ECI assignment value out of range';
  SUnEncodableCharacters =
    'String contains unencodable characters in alphanumeric mode';
  SNonNumericCharacters = 'String contains non-numeric characters';
  SDataTooLong = 'Data too long';
  SEncodingInstanceNil = 'Encoding instance cannot be nil';
  SSegmentInstanceNil = 'Segment instance cannot be nil';

type

  /// <summary>
  /// <para>
  /// A segment of character/binary/control data in a QR Code symbol.
  /// Instances of this class are immutable.
  /// </para>
  /// <para>
  /// The mid-level way to create a segment is to take the payload data
  /// and call a static factory function such as <see cref="QlpQrSegment|TQrSegment.MakeNumeric(string)">
  /// TQrSegment.MakeNumeric(String)</see>.
  /// </para>
  /// <para>
  /// The low-level way to create a segment is to custom-make the bit
  /// buffer and call the <see cref="QlpQrSegment|TQrSegment.Create(TQrSegmentMode,Int32,TQRCodeGenLibInt32Array,Int32)">
  /// TQrSegment Constructor</see> with appropriate values.
  /// </para>
  /// <para>
  /// <br />This segment class imposes no length restrictions, but QR
  /// Codes have restrictions. <br />Even in the most favorable
  /// conditions, a QR Code can only hold 7089 characters of data. <br />
  /// Any segment longer than this is meaningless for the purpose of
  /// generating QR Codes. <br />This class can represent kanji mode
  /// segments, but provides no help in encoding them.
  /// </para>
  /// </summary>
  TQrSegment = class sealed(TInterfacedObject, IQrSegment)

  strict private

  const
    ALPHANUMERIC_CHARSET
      : String = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

    class var

      FALPHANUMERIC_MAP: TQRCodeGenLibInt32Array;

    class constructor QRSegment();

    function GetMode: TQrSegmentMode; inline;
    function GetNumChars: Int32; inline;
    function GetBitLength: Int32; inline;
    function GetData: TQRCodeGenLibInt32Array; inline;

  public

  var
    FMode: TQrSegmentMode;
    // FBitLength Requires 0 <= FBitLength <= System.Length(FData) * 32.
    FNumChars, FBitLength: Int32;
    // The data bits of this segment. Not null.
    FData: TQRCodeGenLibInt32Array;

    /// <summary>
    /// Constructs a QR Code segment with the specified attributes and data. <br />
    /// The character count (ANumChars) must agree with the mode and the bit
    /// buffer length, <br />but the constraint isn't checked. The specified
    /// bit buffer is cloned and stored.
    /// </summary>
    /// <param name="AMode">
    /// the mode
    /// </param>
    /// <param name="ANumChars">
    /// the data length in characters or bytes, which is non-negative
    /// </param>
    /// <param name="AData">
    /// the data bits
    /// </param>
    /// <param name="ABitLength">
    /// the number of valid prefix bits in the data array
    /// </param>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentInvalidQRCodeGenLibException">
    /// if the character count is negative
    /// </exception>
    constructor Create(AMode: TQrSegmentMode; ANumChars: Int32;
      const AData: TQRCodeGenLibInt32Array; ABitLength: Int32);

    /// <summary>
    /// Mode used by this Segment.
    /// </summary>
    property Mode: TQrSegmentMode read GetMode;

    /// <summary>
    /// The length of this segment's unencoded data. Measured in characters
    /// for <br />numeric/alphanumeric/kanji mode, bytes for byte mode, and 0
    /// for ECI mode. <br />Always zero or positive. Not the same as the
    /// data's bit length.
    /// </summary>
    property NumChars: Int32 read GetNumChars;

    /// <summary>
    /// Requires 0 &lt;= bitLength &lt;= data.length * 32.
    /// </summary>
    property BitLength: Int32 read GetBitLength;

    /// <summary>
    /// The data bits of this segment.
    /// </summary>
    property Data: TQRCodeGenLibInt32Array read GetData;

    /// <summary>
    /// Tests whether the specified string can be encoded as a segment in
    /// numeric mode. <br />A string is encodable if each character is in the
    /// range 0 to 9.
    /// </summary>
    /// <param name="AText">
    /// the string to test for encodability
    /// </param>
    /// <returns>
    /// <c>true</c> if each character is in the range 0 to 9.
    /// </returns>
    class function IsNumeric(const AText: String): Boolean; static;

    /// <summary>
    /// Tests whether the specified string can be encoded as a segment in
    /// alphanumeric mode. <br />A string is encodable iff each character is
    /// in the following set: 0 to 9, A to Z <br />(uppercase only), space,
    /// dollar, percent, asterisk, plus, hyphen, period, slash, colon.
    /// </summary>
    /// <param name="AText">
    /// the string to test for encodability
    /// </param>
    /// <returns>
    /// <c>true</c> if each character is in the alphanumeric mode character
    /// set
    /// </returns>
    class function IsAlphaNumeric(const AText: String): Boolean; static;

    // Calculates the number of bits needed to encode the given segments at the given version.
    // Returns a non-negative number if successful. Otherwise returns -1 if a segment has too
    // many characters to fit its length field, or the total bits exceeds System.High(Int32).
    class function GetTotalBits(const ASegments
      : TQRCodeGenLibGenericArray<IQrSegment>; AVersion: Int32): Int32; static;

    /// <summary>
    /// <para>
    /// Returns a segment representing the specified binary data <br />
    /// encoded in byte mode. All input byte arrays are acceptable.
    /// </para>
    /// <para>
    /// Any text string can be converted to bytes via <c>TEncoding</c>
    /// and encoded as a byte mode segment.
    /// </para>
    /// </summary>
    /// <param name="AData">
    /// the binary data
    /// </param>
    /// <returns>
    /// a segment containing the data
    /// </returns>
    class function MakeBytes(const AData: TQRCodeGenLibByteArray)
      : IQrSegment; static;

    /// <summary>
    /// Returns a segment representing the specified string of decimal digits
    /// encoded in numeric mode.
    /// </summary>
    /// <param name="ADigits">
    /// the text with only digits from 0 to 9 allowed
    /// </param>
    /// <returns>
    /// a segment containing the text
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentInvalidQRCodeGenLibException">
    /// if the string contains non-digit characters
    /// </exception>
    class function MakeNumeric(const ADigits: String): IQrSegment; static;

    /// <summary>
    /// Returns a segment representing the specified text string encoded in
    /// alphanumeric mode. <br />The characters allowed are: 0 to 9, A to Z
    /// (uppercase only), space, <br />dollar, percent, asterisk, plus,
    /// hyphen, period, slash, colon.
    /// </summary>
    /// <param name="AText">
    /// the text with only certain characters allowed
    /// </param>
    /// <returns>
    /// a segment containing the text
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentInvalidQRCodeGenLibException">
    /// if the string contains non-encodable characters
    /// </exception>
    class function MakeAlphaNumeric(const AText: String): IQrSegment; static;

    /// <summary>
    /// Returns a segment representing an Extended Channel Interpretation <br />
    /// (ECI) designator with the specified assignment value.
    /// </summary>
    /// <param name="AAssignValue">
    /// the ECI assignment number (see the AIM ECI specification)
    /// </param>
    /// <returns>
    /// a segment containing the data
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the value is outside the range [0, 10<sup>6</sup>)
    /// </exception>
    class function MakeEci(const AAssignValue: Int32): IQrSegment; static;

    /// <summary>
    /// * Returns an array of zero or more segments to represent the
    /// specified Unicode text string. <br />The result may use various
    /// segment modes and switch modes to optimize the length of the bit
    /// stream.
    /// </summary>
    /// <param name="AText">
    /// the text to be encoded, which can be any Unicode string
    /// </param>
    /// <param name="AEncoding">
    /// the encoding object to be used to convert the text to bytes if the
    /// text does not fall into numeric or alphanumeric.
    /// </param>
    /// <returns>
    /// an array of segments containing the text
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|ENullReferenceQRCodeGenLibException">
    /// if <paramref name="AEncoding" /> is Nil
    /// </exception>
    class function MakeSegments(const AText: String; const AEncoding: TEncoding)
      : TQRCodeGenLibGenericArray<IQrSegment>; static;

  end;

implementation

{ TQrSegment }

constructor TQrSegment.Create(AMode: TQrSegmentMode; ANumChars: Int32;
  const AData: TQRCodeGenLibInt32Array; ABitLength: Int32);
begin
  Inherited Create();
  FMode := AMode;
  FData := AData;
  if ((ANumChars < 0) or (ABitLength < 0) or
    (ABitLength > (System.Length(AData) * Int64(32)))) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.CreateRes(@SInvalidValue);
  end;
  FNumChars := ANumChars;
  FBitLength := ABitLength;
end;

function TQrSegment.GetBitLength: Int32;
begin
  result := FBitLength;
end;

function TQrSegment.GetData: TQRCodeGenLibInt32Array;
begin
  result := System.Copy(FData);
end;

function TQrSegment.GetMode: TQrSegmentMode;
begin
  result := FMode;
end;

function TQrSegment.GetNumChars: Int32;
begin
  result := FNumChars;
end;

class function TQrSegment.GetTotalBits(const ASegments
  : TQRCodeGenLibGenericArray<IQrSegment>; AVersion: Int32): Int32;
var
  LCCBits: Int32;
  LResult: Int64;
  LSegment: IQrSegment;
begin
  LResult := 0;
  for LSegment in ASegments do
  begin
    TGuard.RequireNotNull(LSegment, SSegmentInstanceNil);
    LCCBits := LSegment.Mode.NumCharCountBits(AVersion);
    if (LSegment.NumChars >= (TBits.LeftShift32(1, LCCBits))) then
    begin
      result := -1; // The segment's length doesn't fit the field's bit width
      Exit;
    end;
    LResult := LResult + (Int64(4) + LCCBits + LSegment.BitLength);
    if (LResult > System.High(Int32)) then
    begin
      result := -1; // The sum will overflow an Int32 type
      Exit;
    end;
  end;

  result := Int32(LResult);
end;

class function TQrSegment.IsAlphaNumeric(const AText: String): Boolean;
var
  LIdx, LBottom, LTop: Int32;
  LChar: Char;
begin
{$IFDEF DELPHIXE3_UP}
  LBottom := System.Low(AText);
  LTop := System.High(AText);
{$ELSE}
  LBottom := 1;
  LTop := System.Length(AText);
{$ENDIF DELPHIXE3_UP}
  for LIdx := LBottom to LTop do
  begin
    LChar := AText[LIdx];
    if ((Ord(LChar) >= System.Length(FALPHANUMERIC_MAP)) or
      (FALPHANUMERIC_MAP[Ord(LChar)] = -1)) then
    begin
      result := false;
      Exit;
    end;
  end;
  result := true;
end;

class function TQrSegment.IsNumeric(const AText: String): Boolean;
var
  LIdx, LBottom, LTop: Int32;
  LChar: Char;
begin
{$IFDEF DELPHIXE3_UP}
  LBottom := System.Low(AText);
  LTop := System.High(AText);
{$ELSE}
  LBottom := 1;
  LTop := System.Length(AText);
{$ENDIF DELPHIXE3_UP}
  for LIdx := LBottom to LTop do
  begin
    LChar := AText[LIdx];
    if ((Ord(LChar) < Ord('0')) or (Ord(LChar) > Ord('9'))) then
    begin
      result := false;
      Exit;
    end;
  end;
  result := true;
end;

class function TQrSegment.MakeAlphaNumeric(const AText: String): IQrSegment;
var
  LBitBuffer: TBitBuffer;
  LAccumData, LAccumCount, LIdx, LBottom, LTop: Int32;
  LChar: Char;
begin
  LBitBuffer := TBitBuffer.Create();
  LAccumData := 0;
  LAccumCount := 0;
{$IFDEF DELPHIXE3_UP}
  LBottom := System.Low(AText);
  LTop := System.High(AText);
{$ELSE}
  LBottom := 1;
  LTop := System.Length(AText);
{$ENDIF DELPHIXE3_UP}
  for LIdx := LBottom to LTop do
  begin
    LChar := AText[LIdx];
    if ((Ord(LChar) >= System.Length(FALPHANUMERIC_MAP)) or
      (FALPHANUMERIC_MAP[Ord(LChar)] = -1)) then
    begin
      raise EArgumentInvalidQRCodeGenLibException.CreateRes
        (@SUnEncodableCharacters);
    end;
    LAccumData := (LAccumData * 45) + FALPHANUMERIC_MAP[Ord(LChar)];
    System.Inc(LAccumCount);
    if (LAccumCount = 2) then
    begin
      LBitBuffer.AppendBits(LAccumData, 11);
      LAccumData := 0;
      LAccumCount := 0;
    end;
  end;
  if (LAccumCount > 0) then // 1 character remaining
  begin
    LBitBuffer.AppendBits(LAccumData, 6);
  end;
  result := TQrSegment.Create(TQrSegmentMode.qsmAlphaNumeric,
    System.Length(AText), LBitBuffer.Data, LBitBuffer.BitLength);
end;

class function TQrSegment.MakeBytes(const AData: TQRCodeGenLibByteArray)
  : IQrSegment;
var
  LIdx: Int32;
  LBits: TQRCodeGenLibInt32Array;
begin
  if ((System.Length(AData) * Int64(8)) > System.High(Int32)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes(@SDataTooLong);
  end;
  System.SetLength(LBits, (System.Length(AData) + 3) shr 2);
  for LIdx := System.Low(AData) to System.High(AData) do
  begin
    LBits[TBits.Asr32(LIdx, 2)] := Int32(LBits[TBits.Asr32(LIdx, 2)] or
      TBits.NegativeLeftShift32(AData[LIdx] and $FF, (not LIdx) shl 3));
  end;
  result := TQrSegment.Create(TQrSegmentMode.qsmByte, System.Length(AData),
    LBits, System.Length(AData) * 8);

end;

class function TQrSegment.MakeEci(const AAssignValue: Int32): IQrSegment;
var
  LBitBuffer: TBitBuffer;
begin
  LBitBuffer := TBitBuffer.Create();
  if (AAssignValue < 0) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SECIAssignmentOutOfRange);
  end
  else if (AAssignValue < (1 shl 7)) then
  begin
    LBitBuffer.AppendBits(AAssignValue, 8);
  end
  else if (AAssignValue < (1 shl 14)) then
  begin
    LBitBuffer.AppendBits(2, 2);
    LBitBuffer.AppendBits(AAssignValue, 14);
  end
  else if (AAssignValue < 1000000) then
  begin
    LBitBuffer.AppendBits(6, 3);
    LBitBuffer.AppendBits(AAssignValue, 21);
  end
  else
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SECIAssignmentOutOfRange);
  end;
  result := TQrSegment.Create(TQrSegmentMode.qsmEci, 0, LBitBuffer.Data,
    LBitBuffer.BitLength);
end;

class function TQrSegment.MakeNumeric(const ADigits: String): IQrSegment;
var
  LBitBuffer: TBitBuffer;
  LAccumData, LAccumCount, LIdx, LBottom, LTop: Int32;
  LChar: Char;
begin
  LBitBuffer := TBitBuffer.Create();
  LAccumData := 0;
  LAccumCount := 0;
{$IFDEF DELPHIXE3_UP}
  LBottom := System.Low(ADigits);
  LTop := System.High(ADigits);
{$ELSE}
  LBottom := 1;
  LTop := System.Length(ADigits);
{$ENDIF DELPHIXE3_UP}
  for LIdx := LBottom to LTop do
  begin
    LChar := ADigits[LIdx];
    if ((Ord(LChar) < Ord('0')) or (Ord(LChar) > Ord('9'))) then
    begin
      raise EArgumentInvalidQRCodeGenLibException.CreateRes
        (@SNonNumericCharacters);
    end;
    LAccumData := (LAccumData * 10) + (Ord(LChar) - Ord('0'));
    System.Inc(LAccumCount);
    if (LAccumCount = 3) then
    begin
      LBitBuffer.AppendBits(LAccumData, 10);
      LAccumData := 0;
      LAccumCount := 0;
    end;
  end;
  if (LAccumCount > 0) then // 1 or 2 digits remaining
  begin
    LBitBuffer.AppendBits(LAccumData, (LAccumCount * 3) + 1);
  end;
  result := TQrSegment.Create(TQrSegmentMode.qsmNumeric, System.Length(ADigits),
    LBitBuffer.Data, LBitBuffer.BitLength);
end;

class function TQrSegment.MakeSegments(const AText: String;
  const AEncoding: TEncoding): TQRCodeGenLibGenericArray<IQrSegment>;
begin
  TGuard.RequireNotNull(AEncoding, SEncodingInstanceNil);
  // Select the most efficient segment encoding automatically
  result := Nil;
  if (AText <> '') then
  begin
    if IsNumeric(AText) then
    begin
      result := TQRCodeGenLibGenericArray<IQrSegment>.Create
        (MakeNumeric(AText));
      Exit;
    end
    else if IsAlphaNumeric(AText) then
    begin
      result := TQRCodeGenLibGenericArray<IQrSegment>.Create
        (MakeAlphaNumeric(AText));
      Exit;
    end
    else
    begin
      result := TQRCodeGenLibGenericArray<IQrSegment>.Create
        (MakeBytes(TConverters.ConvertStringToBytes(AText, AEncoding)));
      Exit;
    end;
  end;
end;

class constructor TQrSegment.QRSegment;
var
  LMaxChar, LIdx, LBottom, LTop, LFiller: Int32;
begin
  LFiller := -1;
  LMaxChar := -1;
{$IFDEF DELPHIXE3_UP}
  LBottom := System.Low(ALPHANUMERIC_CHARSET);
  LTop := System.High(ALPHANUMERIC_CHARSET);
{$ELSE}
  LBottom := 1;
  LTop := System.Length(ALPHANUMERIC_CHARSET);
{$ENDIF DELPHIXE3_UP}
  for LIdx := LBottom to LTop do
  begin
    LMaxChar := Max(Ord(ALPHANUMERIC_CHARSET[LIdx]), LMaxChar);
  end;
  System.SetLength(FALPHANUMERIC_MAP, LMaxChar + 1);

  TArrayUtils.Fill(FALPHANUMERIC_MAP, LFiller);

  for LIdx := LBottom to LTop do
  begin
    FALPHANUMERIC_MAP[Ord(ALPHANUMERIC_CHARSET[LIdx])] := LIdx - 1;
  end;
end;

end.
