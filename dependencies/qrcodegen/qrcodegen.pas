unit qrcodegen;

{$ifdef fpc}
{$mode delphi}
{$H+}
{$endif}

(*
  This is a pascal translation of :-

  QR Code generator library (C)

  Copyright (c) Project Nayuki. (MIT License)
  https://www.nayuki.io/page/qr-code-generator-library

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  the Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:
  - The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.
  - The Software is provided "as is", without warranty of any kind, express or
    implied, including but not limited to the warranties of merchantability,
    fitness for a particular purpose and noninfringement. In no event shall the
    authors or copyright holders be liable for any claim, damages or other
    liability, whether in an action of contract, tort or otherwise, arising from,
    out of or in connection with the Software or the use or other dealings in the
    Software.
*)

(*
  This library creates QR Code symbols, which is a type of two-dimension barcode.
  Invented by Denso Wave and described in the ISO/IEC 18004 standard.
  A QR Code structure is an immutable square grid of dark and light cells.
  The library provides functions to create a QR Code from text or binary data.
  The library covers the QR Code Model 2 specification, supporting all versions (sizes)
  from 1 to 40, all 4 error correction levels, and 4 character encoding modes.

  Ways to create a QR Code object:
  - High level: Take the payload data and call EncodeText() or EncodeBinary().
  - Low level: Custom-make the list of segments and call
    EncodeSegments() or EncodeSegmentsAdvanced().
  (Note that all ways require supplying the desired error correction level and various byte buffers.)
*)

interface

uses SysUtils, Classes;


type
  EQRException = class(Exception);

  {$REGION 'The error correction level in a QR Code symbol.'}
  /// <summary>
  /// The error correction level in a QR Code symbol.
  ///</summary>
  {$ENDREGION}
  TErrorCorrectionLevel =
  (
    eclLow,           // The QR Code can tolerate about  7% erroneous codewords
    eclMedium,        // The QR Code can tolerate about 15% erroneous codewords
    eclQuartile,      // The QR Code can tolerate about 25% erroneous codewords
    eclHigh           // The QR Code can tolerate about 30% erroneous codewords
  );

  {$REGION 'The mask pattern used in a QR Code symbol.'}
  /// <summary>
  /// The mask pattern used in a QR Code symbol.
  /// </summary>
  {$ENDREGION}
  TQRMask =
  (
    { A special value to tell the QR Code encoder to automatically select an appropriate mask pattern }
    qrMaskAuto = -1,
    { The eight actual mask patterns }
    qrMask0,
    qrMask1,
    qrMask2,
    qrMask3,
    qrMask4,
    qrMask5,
    qrMask6,
    qrMask7
  );

  {$REGION 'Describes how a segment''s data bits are interpreted.'}
  /// <summary>
  /// Describes how a segment's data bits are interpreted.
  /// </summary>
  {$ENDREGION}
  TQRMode =
  (
    qrmNUMERIC,
    qrmALPHANUMERIC,
    qrmBYTE,
    qrmKANJI,
    qrmECI
  );

  {$REGION 'A segment of character/binary/control data in a QR Code symbol.'}
  /// <summary>
  /// A segment of character/binary/control data in a QR Code symbol.
  /// The mid-level way to create a segment is to take the payload data
  /// and call a factory function such as qrcodegen_makeNumeric().
  /// The low-level way to create a segment is to custom-make the bit buffer
  /// and initialize a qrcodegen_Segment struct with appropriate values.
  /// Even in the most favorable conditions, a QR Code can only hold 7089 characters of data.
  /// Any segment longer than this is meaningless for the purpose of generating QR Codes.
  /// Moreover, the maximum allowed bit length is 32767 because
  /// the largest QR Code (version 40) has 31329 modules.
  /// </summary>
  {$ENDREGION}
  TQRSegment = record
    mode: TQRMode;

    {$REGION 'The length of this segment's unencoded data'}
    /// <summary>
    /// The length of this segment's unencoded data. Measured in characters for
    /// numeric/alphanumeric/kanji mode, bytes for byte mode, and 0 for ECI mode.
    /// Always zero or positive. Not the same as the data's bit length.
    /// </summary>
    {$ENDREGION}
    numChars: integer;

    {$REGION 'The data bits of this segment, packed in bitwise big endian.'}
    /// <summary>
    /// The data bits of this segment, packed in bitwise big endian.
    /// Can be null if the bit length is zero.
    /// </summary>
    {$ENDREGION}
    data: TBytes;

    {$REGION 'The number of valid data bits used in the buffer.'}
    /// <summary>
    /// The number of valid data bits used in the buffer.
    /// Requires 0 <= bitLength <= 32767, and bitLength <= (capacity of data array) * 8.
    /// The character count (numChars) must agree with the mode and the bit buffer length.
    /// </summary>
    {$ENDREGION}
    bitLength: integer;
  end;

  TQRSegments = TArray<TQRSegment>;

  T7Bytes = array [0 .. 6] of byte;
  T7Int = array [0 .. 6] of integer;

  TQRVersion = 1..40; // The minimum and maximum version number supported in the QR Code Model 2 standard

const
  {$REGION 'The worst-case number of bytes needed to store one QR Code, up to and including'}
  /// <summary>
  /// The worst-case number of bytes needed to store one QR Code, up to and including
  /// version 40. This value equals 3918, which is just under 4 kilobytes.
  /// Use this more convenient value to avoid calculating tighter memory bounds for buffers.
  /// </summary>
  {$ENDREGION}
  QR_BUFFER_LEN_MAX = ((((High(TQRVersion)) * 4 + 17) * ((High(TQRVersion)) * 4 + 17) + 7) div 8 + 1);


(* ---- Functions (high level) to generate QR Codes ---- *)

{$REGION 'Encodes the given binary data to a QR Code, returning true if encoding succeeded.'}
/// <summary>
/// Encodes the given binary data to a QR Code, returning true if encoding succeeded.
/// If the data is too long to fit in any version in the given range
/// at the given ECC level, then false is returned.
/// - The input array range dataAndTemp[0 : dataLen] should normally be
///   valid UTF-8 text, but is not required by the QR Code standard.
/// - The variables ecl and mask must correspond to enum constant values.
/// - Requires 1 <= minVersion <= maxVersion <= 40.
/// - The arrays dataAndTemp and qrcode must each have a length of at least
///   qrcodegen_BUFFER_LEN_FOR_VERSION(maxVersion), and cannot overlap.
/// - After the function returns, the contents of dataAndTemp may have changed,
///   and does not represent useful data anymore.
/// - If successful, the resulting QR Code will use byte mode to encode the data.
/// - In the most optimistic case, a QR Code at version 40 with low ECC can hold any byte
///   sequence up to length 2953. This is the hard upper limit of the QR Code standard.
/// - Please consult the QR Code specification for information on
///   data capacities per version, ECC level, and text encoding mode.
/// </summary>
{$ENDREGION}
procedure EncodeText(const text: ansistring; var tempBuffer, qrcode: TBytes; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean);

{$REGION 'Encodes the given binary data to a QR Code, returning true if encoding succeeded.'}
/// <summary>
/// Encodes the given binary data to a QR Code, returning true if encoding succeeded.
/// If the data is too long to fit in any version in the given range
/// at the given ECC level, then false is returned.
/// - The input array range dataAndTemp[0 : dataLen] should normally be
///   valid UTF-8 text, but is not required by the QR Code standard.
/// - The variables ecl and mask must correspond to enum constant values.
/// - Requires 1 <= minVersion <= maxVersion <= 40.
/// - The arrays dataAndTemp and qrcode must each have a length of at least
///   qrcodegen_BUFFER_LEN_FOR_VERSION(maxVersion), and cannot overlap.
/// - After the function returns, the contents of dataAndTemp may have changed,
///   and does not represent useful data anymore.
/// - If successful, the resulting QR Code will use byte mode to encode the data.
/// - In the most optimistic case, a QR Code at version 40 with low ECC can hold any byte
///   sequence up to length 2953. This is the hard upper limit of the QR Code standard.
/// - Please consult the QR Code specification for information on
///   data capacities per version, ECC level, and text encoding mode.
/// </summary>
{$ENDREGION}
procedure EncodeBinary(dataAndTemp: TBytes; dataLen: integer; var qrcode: TBytes; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean);


(* ---- Low-level QR Code encoding functions ---- *)

{$REGION 'Renders a QR Code representing the given segments at the given error correction level.'}
/// <summary>
/// Renders a QR Code representing the given segments at the given error correction level.
/// The smallest possible QR Code version is automatically chosen for the output. Returns true if
/// QR Code creation succeeded, or false if the data is too long to fit in any version. The ECC level
/// of the result may be higher than the ecl argument if it can be done without increasing the version.
/// This function allows the user to create a custom sequence of segments that switches
/// between modes (such as alphanumeric and byte) to encode text in less space.
/// This is a low-level API; the high-level API is EncodeText() and EncodeBinary().
/// To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
/// result in them being clobbered, but the QR Code output will still be correct.
/// But the qrcode array must not overlap tempBuffer or any segment's data buffer.
/// </summary>
{$ENDREGION}
procedure EncodeSegments(const segs: TQRSegments; ecl: TErrorCorrectionLevel; var tempBuffer, qrcode: TBytes);

{$REGION 'Renders a QR Code representing the given segments with the given encoding parameters.'}
/// <summary>
/// Renders a QR Code representing the given segments with the given encoding parameters.
/// Returns true if QR Code creation succeeded, or false if the data is too long to fit in the range of versions.
/// The smallest possible QR Code version within the given range is automatically
/// chosen for the output. Iff boostEcl is true, then the ECC level of the result
/// may be higher than the ecl argument if it can be done without increasing the
/// version. The mask is either between qrMask0 to qrMask7 to force that mask, or
/// qrMaskAuto to automatically choose an appropriate mask (which may be slow).
/// This function allows the user to create a custom sequence of segments that switches
/// between modes (such as alphanumeric and byte) to encode text in less space.
/// This is a low-level API; the high-level API is EncodeText() and EncodeBinary().
/// To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
/// result in them being clobbered, but the QR Code output will still be correct.
/// But the qrcode array must not overlap tempBuffer or any segment's data buffer.
/// </summary>
{$ENDREGION}
procedure EncodeSegmentsAdvanced(const segs: TQRSegments; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean; var tempBuffer, qrcode: TBytes);


(* ---- Basic QR Code information ---- *)

{$REGION 'Returns the side length of the given QR Code, assuming that encoding succeeded.'}
/// <summary>
/// Returns the side length of the given QR Code, assuming that encoding succeeded.
/// The result is in the range [21, 177]. Note that the length of the array buffer
/// is related to the side length - every 'uint8_t qrcode[]' must have length at least
/// qrcodegen_BUFFER_LEN_FOR_VERSION(version), which equals ceil(size^2 / 8 + 1).
/// </summary>
{$ENDREGION}
function GetQRSize(const qrcode: TBytes): integer;

{$REGION 'Returns the color of the module (pixel) at the given coordinates, which is false'}
/// <summary>
/// Returns the color of the module (pixel) at the given coordinates, which is false
/// for light or true for dark. The top left corner has the coordinates (x=0, y=0).
/// If the given coordinates are out of bounds, then false (light) is returned.
/// </summary>
{$ENDREGION}
function GetQRModule(const qrcode: TBytes; x, y: integer): boolean;


(* ---- Segment handling ---- *)

{$REGION 'Tests whether the given string can be encoded as a segment in numeric mode.'}
/// <summary>
/// Tests whether the given string can be encoded as a segment in numeric mode.
/// A string is encodable iff each character is in the range 0 to 9.
/// </summary>
{$ENDREGION}
function CanBeNumericQR(const text: AnsiString): boolean;

{$REGION 'Tests whether the given string can be encoded as a segment in alphanumeric mode.'}
/// <summary>
/// Tests whether the given string can be encoded as a segment in alphanumeric mode.
/// A string is encodable iff each character is in the following set: 0 to 9, A to Z
/// (uppercase only), space, dollar, percent, asterisk, plus, hyphen, period, slash, colon.
/// </summary>
{$ENDREGION}
function CanBeAlphanumericQR(const text: AnsiString): boolean;

{$REGION 'Returns the number of bytes (uint8_t) needed for the data buffer of a segment'}
/// <summary>
/// Returns the number of bytes (uint8_t) needed for the data buffer of a segment
/// containing the given number of characters using the given mode. Notes:
/// - Raises EQRException on failure, i.e. numChars > INT16_MAX or
///   the number of needed bits exceeds INT16_MAX (i.e. 32767).
/// - Otherwise, all valid results are in the range [0, ceil(INT16_MAX / 8)], i.e. at most 4096.
/// - It is okay for the user to allocate more bytes for the buffer than needed.
/// - For byte mode, numChars measures the number of bytes, not Unicode code points.
/// - For ECI mode, numChars must be 0, and the worst-case number of bytes is returned.
///   An actual ECI segment can have shorter data. For non-ECI modes, the result is exact.
/// </summary>
{$ENDREGION}
function CalcQRSegmentBufferSize(mode: TQRMode; numChars: integer): integer;

{$REGION 'Returns a segment representing the given binary data encoded in'}
/// <summary>
/// Returns a segment representing the given binary data encoded in
/// byte mode. All input byte arrays are acceptable. Any text string
/// can be converted to UTF-8 bytes and encoded as a byte mode segment.
/// </summary>
{$ENDREGION}
function NewQRSegmentWithBinary(const data: TBytes; var buf: TBytes): TQRSegment;

{$REGION 'Returns a segment representing the given string of decimal digits encoded in numeric mode.'}
/// <summary>
/// Returns a segment representing the given string of decimal digits encoded in numeric mode.
/// </summary>
{$ENDREGION}
function NewQRSegmentWithNumeric(const digits: AnsiString; var buf: TBytes): TQRSegment;

{$REGION 'Returns a segment representing the given text string encoded in alphanumeric mode.'}
/// <summary>
/// Returns a segment representing the given text string encoded in alphanumeric mode.
/// The characters allowed are: 0 to 9, A to Z (uppercase only), space,
/// dollar, percent, asterisk, plus, hyphen, period, slash, colon.
/// </summary>
{$ENDREGION}
function NewQRSegmentWithAlphanumeric(const text: AnsiString; var buf: TBytes): TQRSegment;

{$REGION 'Returns a segment representing an Extended Channel Interpretation'}
/// <summary>
/// Returns a segment representing an Extended Channel Interpretation
/// (ECI) designator with the given assignment value.
/// </summary>
{$ENDREGION}
function NewQRSegmentWithECI(assignVal: integer; var buf: TBytes): TQRSegment;

{$REGION 'Internal testing declarations'}
{$IFDEF QRCODEGEN_TESTING}
type
  TLogProc = procedure(Text : string) of object;

var
  LogProc : TLogProc;

procedure testReedSolomonComputeDevisor;
procedure testReedSolomonComputeMultiply;
procedure testInitializeFunctionModulesEtc;
procedure testGetSetModule;
procedure testGetSetModuleRandomly;
procedure testIsAlphanumeric;
procedure testGetAlignmentPatternPositions;
procedure testReedSolomonComputeRemainder;
procedure testMakeBytes;
procedure testMakeAlphanumeric;
procedure testMakeNumeric;
procedure testMakeEci;
procedure testGetTotalBits;
procedure testAddEccAndInterleave;
{$ENDIF}
{$ENDREGION}

procedure WriteToFile(const buffer: TBytes; bitcount: integer; const filename: string); overload;

implementation

{$IFNDEF FPC}
uses AnsiStrings;
{$ENDIF}

const
  ALPHANUMERIC_CHARSET : AnsiString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:";';

  {$REGION 'For generating error correction codes.'}
  /// <summary>
  /// For generating error correction codes.
  /// </summary>
  {$ENDREGION}
  ECC_CODEWORDS_PER_BLOCK: array [TErrorCorrectionLevel, TQRVersion] of int8 = (
    // 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    (  7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Low
    ( 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28), // Medium
    ( 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Quartile
    ( 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)); // High

  qrcodegen_REED_SOLOMON_DEGREE_MAX = 30; // Based on the table above

  {$REGION 'For generating error correction codes.'}
  /// <summary>
  /// For generating error correction codes.
  /// </summary>
  {$ENDREGION}
  NUM_ERROR_CORRECTION_BLOCKS: array [TErrorCorrectionLevel, TQRVersion] of int8 = (
    // 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
    (  1,  1,  1,  1,  1,  2,  2,  2,  2,  4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25), // Low
    (  1,  1,  1,  2,  2,  4,  4,  4,  5,  5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49), // Medium
    (  1,  1,  2,  2,  4,  4,  6,  6,  8,  8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68), // Quartile
    (  1,  1,  2,  4,  4,  4,  5,  6,  8,  8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81)); // High

  {$REGION 'For automatic mask pattern selection.'}
  /// <summary>
  /// For automatic mask pattern selection.
  /// </summary>
  {$ENDREGION}
  PENALTY_N1 = 3;
  PENALTY_N2 = 3;
  PENALTY_N3 = 40;
  PENALTY_N4 = 10;

function calcSegmentBitLength(mode: TQRMode; numChars: integer): integer; forward;
function getNumDataCodewords (version : TQRVersion; ecl :TErrorCorrectionLevel) : integer; forward;
function getTotalBits (const segs : TQRSegments; version : TQRVersion) : integer; forward;
function numCharCountBits(mode: TQRMode; version: TQRVersion): integer; forward;
procedure addEccAndInterleave (data : TBytes; version : TQRVersion; ecl : TErrorCorrectionLevel; var res : TBytes); forward;
procedure drawLightFunctionModules(qrcode: TBytes; version: TQRVersion); forward;
procedure initializeFunctionModules (version : TQRVersion; var qrcode : TBytes); forward;
procedure drawCodewords(const data: TBytes; dataLen: integer; var qrcode: TBytes); forward;
function getNumRawDataModules (ver : integer) : integer; forward;
procedure applyMask(const functionModules: TBytes; var qrcode: TBytes; mask: TQRMask); forward;
procedure drawFormatBits(ecl: TErrorCorrectionLevel; mask: TQRMask; var qrcode: TBytes); forward;
function getPenaltyScore(const qrcode: TBytes): integer; forward;
procedure finderPenaltyAddHistory(currentRunLength: integer; var runHistory: T7Int; qrsize: integer); forward;
procedure reedSolomonComputeDivisor (degree : integer; var res : TBytes); forward;
procedure reedSolomonComputeRemainder (const data : TBytes; dataLen : integer; const generator : TBytes; degree : integer; var res : TBytes); overload; forward;
procedure reedSolomonComputeRemainder (const data : PByte; dataLen : integer; const generator : TBytes; degree : integer; var res : PByte); overload; forward;
procedure setModule (var qrcode : TBytes; x, y : integer; isDark : boolean); forward;
function getModule (const qrcode : TBytes; x, y : integer) : boolean; forward;
function reedSolomonMultiply (x, y : byte) : byte; forward;
procedure fillRectangle(left, top, width, height: integer; qrcode: TBytes); forward;
function finderPenaltyCountPatterns(const runHistory: T7Int; qrsize: integer): integer; forward;
function finderPenaltyTerminateAndCount(currentRunColor: boolean; var currentRunLength: integer; var runHistory: T7Int; qrsize: integer): integer; forward;
function getAlignmentPatternPositions(version: TQRVersion; var res: T7Bytes): integer; forward;
procedure setModuleBounded(var qrcode: TBytes; x, y: integer; isDark: boolean); forward;
procedure WriteToFile(const buffer: TBytes; bitcount: integer; prefix: char; filenumber: integer); overload; forward;
procedure WriteToFile(const buffer: TBytes; bitcount: integer; prefix: char; x, y: integer); overload; forward;

{$REGION 'Calculates the number of bytes needed to store any QR Code up to and including the given version number,'}
/// <summary>
/// Calculates the number of bytes needed to store any QR Code up to and including the given version number,
/// as a compile-time constant. For example, 'uint8_t buffer[qrcodegen_BUFFER_LEN_FOR_VERSION(25)];'
/// can store any single QR Code from version 1 to 25 (inclusive). The result fits in an int (or int16).
/// Requires qrcodegen_VERSION_MIN <= n <= High(TQRVersion).
/// </summary>
{$ENDREGION}
function qrcodegen_BUFFER_LEN_FOR_VERSION(n: TQRVersion): integer;
begin
  Result := ((n * 4 + 17) * (n * 4 + 17) + 7) div 8 + 1;
end;

{$REGION 'Returns true iff the i''th bit of x is set to 1. Requires x >= 0 and 0 <= i <= 14.'}
/// <summary>
/// Returns true iff the i'th bit of x is set to 1. Requires x >= 0 and 0 <= i <= 14.
/// </summary>
{$ENDREGION}
function getBit(x, i: integer): boolean; inline;
begin
  Result := ((x shr i) and 1) <> 0;
end;

procedure EncodeText(const text: ansistring; var tempBuffer, qrcode: TBytes; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean);
var
  textLen: integer;
  seg: TQRSegments;
  bufLen: integer;
  i: integer;
begin
  SetLength(seg, 1);
  textLen := length(text);
  if textLen = 0 then
  begin
    EncodeSegmentsAdvanced(nil, ecl, minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode);
    exit;
  end;
  bufLen := QR_BUFFER_LEN_MAX;
  if CanBeNumericQR(text) then
  begin
    if CalcQRSegmentBufferSize(qrmNUMERIC, textLen) > bufLen then
      raise EQRException.Create('Segment buffer size exceeds maximum');
    seg[0] := NewQRSegmentWithNumeric(text, tempBuffer);
  end
  else if CanBeAlphanumericQR(text) then
  begin
    if CalcQRSegmentBufferSize(qrmALPHANUMERIC, textLen) > bufLen then
      raise EQRException.Create('Segment buffer size exceeds maximum');
    seg[0] := NewQRSegmentWithAlphanumeric(text, tempBuffer);
  end
  else
  begin
    if textLen > bufLen then
      raise EQRException.Create('Text length exceeds buffer length');
    for i := 0 to textLen - 1 do
      tempBuffer[i] := byte(text[i + 1]);
    seg[0].mode := qrmBYTE;
    seg[0].bitLength := calcSegmentBitLength(seg[0].mode, textLen);
    seg[0].numChars := textLen;
    SetLength(seg[0].data, length(tempBuffer));
    for i := low(seg[0].data) to high(seg[0].data) do
      seg[0].data[i] := tempBuffer[i];
  end;
  EncodeSegmentsAdvanced(seg, ecl, minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode);
end;

procedure EncodeBinary(dataAndTemp: TBytes; dataLen: integer; var qrcode: TBytes; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean);
var
  seg: TQRSegments;
  i: integer;
begin
  SetLength(seg, 1);
  seg[0].mode := qrmBYTE;
  seg[0].bitLength := calcSegmentBitLength(seg[0].mode, dataLen);
  seg[0].numChars := dataLen;
  SetLength(seg[0].data, length(dataAndTemp));
  for i := low(seg[0].data) to high(seg[0].data) do
    seg[0].data[i] := dataAndTemp[i];
  EncodeSegmentsAdvanced(seg, ecl, minVersion, maxVersion, mask, boostEcl, dataAndTemp, qrcode);
end;

{$REGION 'Appends the given number of low-order bits of the given value to the given byte-based'}
/// <summary>
/// Appends the given number of low-order bits of the given value to the given byte-based
/// bit buffer, increasing the bit length. Requires 0 <= numBits <= 16 and val < 2^numBits.
/// </summary>
{$ENDREGION}
procedure appendBitsToBuffer(val: UInt32; numBits: integer; var buffer: TBytes; var bitLen: integer); overload;
var
  i: integer;
begin
  assert((0 <= numBits) and (numBits <= 16) and (val shr numBits = 0));
  for i := numBits - 1 downto 0 do
  begin
    buffer[bitLen shr 3] := buffer[bitLen shr 3] or ((val shr i) and 1) shl (7 - (bitLen and 7));
    bitLen := bitLen + 1;
  end;
end;

{$REGION 'Appends the given number of low-order bits of the given value to the given byte-based'}
/// <summary>
/// Appends the given number of low-order bits of the given value to the given byte-based
/// bit buffer, increasing the bit length. Requires 0 <= numBits <= 16 and val < 2^numBits.
/// </summary>
{$ENDREGION}
procedure appendBitsToBuffer(mode: TQRMode; numBits: integer; var buffer: TBytes; var bitLen: integer); overload;
var
  val : UInt32;
begin
  case mode of
    qrmNUMERIC: val := $01;
    qrmALPHANUMERIC: val := $02;
    qrmBYTE: val := $04;
    qrmKANJI: val := $08;
    qrmECI: val := $07;
  else
    raise EQRException.Create('Invalid mode');
  end;
  appendBitsToBuffer(val, numBits, buffer, bitLen);
end;


(* ---- Low-level QR Code encoding functions ---- *)

procedure EncodeSegments(const segs: TQRSegments; ecl: TErrorCorrectionLevel; var tempBuffer, qrcode: TBytes);
begin
  EncodeSegmentsAdvanced(segs, ecl, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true, tempBuffer, qrcode);
end;

procedure EncodeSegmentsAdvanced(const segs: TQRSegments; ecl: TErrorCorrectionLevel; minVersion, maxVersion: TQRVersion; mask: TQRMask; boostEcl: boolean; var tempBuffer, qrcode: TBytes);
var
  version: TQRVersion;
  i, j: integer;
  e : TErrorCorrectionLevel;
  dataUsedBits: integer;
  dataCapacityBits, terminatorBits: integer;
  bitLen, bit: integer;
  padByte: byte;
  minPenalty, penalty: integer;
  msk: TQRMask;
begin
  if segs = nil then
    exit;
  if length(segs) = 0 then
    exit;
  assert((Low(TQRVersion) <= minVersion) and (minVersion <= maxVersion) and (maxVersion <= High(TQRVersion)));
  dataUsedBits := -1;

  // Find the minimal version number to use
  for version := minVersion to maxVersion do
  begin
    dataCapacityBits := getNumDataCodewords(version, ecl) * 8; // Number of data bits available
    dataUsedBits := getTotalBits(segs, version);
    if (dataUsedBits <> -1) and (dataUsedBits <= dataCapacityBits) then
      break; // This version number is found to be suitable
    if version = maxVersion then
      raise EQRException.Create('Cannot determine suitable version');
  end;
  assert(dataUsedBits <> -1);

  // Increase the error correction level while the data still fits in the current version number
  for e := eclMedium to eclHigh do
    if (boostEcl) and (dataUsedBits <= getNumDataCodewords(version, e) * 8) then
      ecl := e;

  // Concatenate all segments to create the data bit string
  SetLength(qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(version));
  for i := low(qrcode) to high(qrcode) do
    qrcode[i] := 0;
  bitLen := 0;
  for i := low(segs) to high(segs) do
  begin
    appendBitsToBuffer(segs[i].mode, 4, qrcode, bitLen);
    appendBitsToBuffer(segs[i].numChars, numCharCountBits(segs[i].mode, version), qrcode, bitLen);
    for j := 0 to segs[i].bitLength - 1 do
    begin
      bit := (segs[i].data[j shr 3] shr (7 - (j and 7))) and 1;
      appendBitsToBuffer(UInt32(bit), 1, qrcode, bitLen);
    end;
  end;
  assert(bitLen = dataUsedBits);

  // Add terminator and pad up to a byte if applicable
  dataCapacityBits := getNumDataCodewords(version, ecl) * 8;
  assert(bitLen <= dataCapacityBits);
  terminatorBits := dataCapacityBits - bitLen;
  if terminatorBits > 4 then
    terminatorBits := 4;
  appendBitsToBuffer(0, terminatorBits, qrcode, bitLen);
  appendBitsToBuffer(0, (8 - bitLen mod 8) mod 8, qrcode, bitLen);
  assert(bitLen mod 8 = 0);

  // Pad with alternating bytes until data capacity is reached
  padByte := $ec;
  while bitLen < dataCapacityBits do
  begin
    appendBitsToBuffer(padByte, 8, qrcode, bitLen);
    padByte := padByte xor ($ec xor $11);
  end;

  // Draw function and data codeword modules
  addEccAndInterleave(qrcode, version, ecl, tempBuffer);
  initializeFunctionModules(version, qrcode);
  drawCodewords(tempBuffer, getNumRawDataModules(version) div 8, qrcode);
  drawLightFunctionModules(qrcode, version);
  initializeFunctionModules(version, tempBuffer);

  // Handle masking
  if mask = qrMaskAuto then
  begin // Automatically choose best mask
    minPenalty := High(Int32);
    for msk := qrMask0 to qrMask7 do
    begin
      applyMask(tempBuffer, qrcode, msk);
      drawFormatBits(ecl, msk, qrcode);
      penalty := getPenaltyScore(qrcode);
      if penalty < minPenalty then
      begin
        mask := msk;
        minPenalty := penalty;
      end;
      applyMask(tempBuffer, qrcode, msk); // Undoes the mask due to XOR
    end;
  end;
  assert(mask in [qrMask0..qrMask7]);
  applyMask(tempBuffer, qrcode, mask);
  drawFormatBits(ecl, mask, qrcode);
end;

(* ---- Error correction code generation functions ---- *)

{$REGION 'Appends error correction bytes to each block of the given data array, then interleaves'}
/// <summary>
/// Appends error correction bytes to each block of the given data array, then interleaves
/// bytes from the blocks and stores them in the result array. data[0 : dataLen] contains
/// the input data. data[dataLen : rawCodewords] is used as a temporary work area and will
/// be clobbered by this function. The final answer is stored in result[0 : rawCodewords].
/// </summary>
{$ENDREGION}
procedure addEccAndInterleave(data: TBytes; version: TQRVersion; ecl: TErrorCorrectionLevel; var res: TBytes);
var
  numBlocks, blockEccLen: integer;
  rawCodewords, dataLen: integer;
  numShortBlocks, shortBlockDataLen: integer;
  rsdiv: TBytes;
  i, j, k: integer;
  datlen: integer;
  ecc: PByte;
  dat: PByte;
begin
  // Calculate parameter numbers
  assert(version in [Low(TQRVersion)..High(TQRVersion)]);
  numBlocks := NUM_ERROR_CORRECTION_BLOCKS[ecl, version];
  blockEccLen := ECC_CODEWORDS_PER_BLOCK[ecl, version];
  rawCodewords := getNumRawDataModules(version) div 8;
  dataLen := getNumDataCodewords(version, ecl);
  numShortBlocks := numBlocks - rawCodewords mod numBlocks;
  shortBlockDataLen := rawCodewords div numBlocks - blockEccLen;

  // Split data into blocks, calculate ECC, and interleave
  // (not concatenate) the bytes into a single sequence
  SetLength(rsdiv, qrcodegen_REED_SOLOMON_DEGREE_MAX);
  reedSolomonComputeDivisor(blockEccLen, rsdiv);
  dat := @data[0];
  for i := 0 to numBlocks - 1 do
  begin
    if i < numShortBlocks then
      datLen := shortBlockDataLen
    else
      datLen := shortBlockDataLen + 1;
    ecc := @data[dataLen]; // Temporary storage
    reedSolomonComputeRemainder(dat, datlen, rsdiv, blockEccLen, ecc);
    k := i;
    for j := 0 to datLen - 1 do
    begin
      if j = shortBlockDataLen then
        k := k - numShortBlocks;
      res[k] := dat[j];
      k := k + numBlocks;
    end;
    k := dataLen + i;
    for j := 0 to blockEccLen - 1 do
    begin
      res[k] := ecc[j];
      k := k + numBlocks;
    end;
    dat := dat + datLen;
  end;
end;

{$REGION 'Returns the number of 8-bit codewords that can be used for storing data (not ECC),'}
/// <summary>
/// Returns the number of 8-bit codewords that can be used for storing data (not ECC),
/// for the given version number and error correction level. The result is in the range [9, 2956].
/// </summary>
{$ENDREGION}
function getNumDataCodewords(version: TQRVersion; ecl: TErrorCorrectionLevel): integer;
var
  v: integer;
begin
  v := version;
  Result := getNumRawDataModules(v) div 8
    - ECC_CODEWORDS_PER_BLOCK[ecl][v]
    * NUM_ERROR_CORRECTION_BLOCKS[ecl][v];
end;

{$REGION 'Returns the number of data bits that can be stored in a QR Code of the given version number, after'}
/// <summary>
/// Returns the number of data bits that can be stored in a QR Code of the given version number, after
/// all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
/// The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
/// </summary>
{$ENDREGION}
function getNumRawDataModules(ver: integer): integer;
var
  numAlign: integer;
begin
  Result := (16 * ver + 128) * ver + 64;
  if ver >= 2 then
  begin
    numAlign := ver div 7 + 2;
    Result := Result - ((25 * numAlign - 10) * numAlign - 55);
    if ver >= 7 then
      Result := Result - 36;
  end;
end;

(* ---- Reed-Solomon ECC generator functions ---- *)

{$REGION 'Computes a Reed-Solomon ECC generator polynomial for the given degree, storing in result[0 : degree].'}
/// <summary>
/// Computes a Reed-Solomon ECC generator polynomial for the given degree, storing in result[0 : degree].
/// This could be implemented as a lookup table over all possible parameter values, instead of as an algorithm.
/// </summary>
{$ENDREGION}
procedure reedSolomonComputeDivisor(degree: integer; var res: TBytes);
var
  root: byte;
  i, j: integer;
begin
  assert((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
  // Polynomial coefficients are stored from highest to lowest power, excluding the leading term which is always 1.
  // For example the polynomial x^3 + 255x^2 + 8x + 93 is stored as the uint8 array {255, 8, 93}.
  for i := 0 to degree - 1 do
    res[i] := 0;
  res[degree - 1] := 1; // Start off with the monomial x^0

  // Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
  // drop the highest monomial term which is always 1x^degree.
  // Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
  root := 1;
  for i := 0 to degree - 1 do
  begin
    // Multiply the current product by (x - r^i)
    for j := 0 to degree - 1 do
    begin
      res[j] := reedSolomonMultiply(res[j], root);
      if (j + 1 < degree) then
        res[j] := res[j] xor res[j + 1];
    end;
    root := reedSolomonMultiply(root, $02);
  end;
end;

{$REGION 'Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.'}
/// <summary>
/// Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.
/// The remainder when data[0 : dataLen] is divided by divisor[0 : degree] is stored in result[0 : degree].
/// All polynomials are in big endian, and the generator has an implicit leading 1 term.
/// </summary>
{$ENDREGION}
procedure reedSolomonComputeRemainder(const data: TBytes; dataLen: integer; const generator: TBytes; degree: integer; var res: TBytes); overload;
var
  i, j, k: integer;
  factor: byte;
begin
  assert((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
  for i := 0 to degree - 1 do
    res[i] := 0;
  for i := 0 to dataLen - 1 do // Polynomial division
  begin
    factor := data[i] xor res[0];
    for k := 0 to degree - 2 do // TODO: could use move
      res[k] := res[k + 1];
    res[degree - 1] := 0;
    for j := 0 to degree - 1 do
      res[j] := res[j] xor reedSolomonMultiply(generator[j], factor);
  end;
end;

{$REGION 'Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.'}
/// <summary>
/// Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.
/// The remainder when data[0 : dataLen] is divided by divisor[0 : degree] is stored in result[0 : degree].
/// All polynomials are in big endian, and the generator has an implicit leading 1 term.
/// </summary>
{$ENDREGION}
procedure reedSolomonComputeRemainder(const data: PByte; dataLen: integer; const generator: TBytes; degree: integer; var res: PByte); overload;
var
  i, j, k: integer;
  factor: byte;
begin
  assert((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
  for i := 0 to degree - 1 do
    res[i] := 0;
  for i := 0 to dataLen - 1 do
  begin
    factor := data[i] xor res[0];
    for k := 0 to degree - 2 do // TODO: could use move
      res[k] := res[k + 1];
    res[degree - 1] := 0;
    for j := 0 to degree - 1 do
      res[j] := res[j] xor reedSolomonMultiply(generator[j], factor);
  end;
end;

{$REGION 'Returns the product of the two given field elements modulo GF(2^8/0x11D).'}
/// <summary>
/// Returns the product of the two given field elements modulo GF(2^8/0x11D).
/// All inputs are valid. This could be implemented as a 256*256 lookup table.
/// </summary>
{$ENDREGION}
function reedSolomonMultiply(x, y: byte): byte;
var
  z: byte;
  i: integer;
begin
  // Russian peasant multiplication
  z := 0;
  for i := 7 downto 0 do
  begin
    z := byte((z shl 1) xor ((z shr 7) * $11D));
    z := z xor (((y shr i) and 1) * x);
  end;
  Result := z;
end;

(* ---- Drawing function modules ---- *)

{$REGION 'Clears the given QR Code grid with light modules for the given'}
/// <summary>
/// Clears the given QR Code grid with light modules for the given
/// version's size, then marks every function module as dark.
/// </summary>
{$ENDREGION}
procedure initializeFunctionModules(version: TQRVersion; var qrcode: TBytes);
var
  qrsize: integer;
  alignPatPos: T7Bytes;
  numAlign: integer;
  i, j: integer;
begin
  // Initialize QR Code
  qrsize := version * 4 + 17;
  for i := low(qrcode) to high(qrcode) do
    qrcode[i] := 0;
  qrcode[0] := qrsize;

  // Fill horizontal and vertical timing patterns
  fillRectangle(6, 0, 1, qrsize, qrcode);
  fillRectangle(0, 6, qrsize, 1, qrcode);

  // Fill 3 finder patterns (all corners except bottom right) and format bits
  fillRectangle(0, 0, 9, 9, qrcode);
  fillRectangle(qrsize - 8, 0, 8, 9, qrcode);
  fillRectangle(0, qrsize - 8, 9, 8, qrcode);

  // Fill numerous alignment patterns
  for i := low(alignPatPos) to high(alignPatPos) do
    alignPatPos[i] := $00;
  numAlign := getAlignmentPatternPositions(version, alignPatPos);
  for i := 0 to numAlign - 1 do
  begin
    for j := 0 to numAlign - 1 do
    begin
      // Don't draw on the three finder corners
      if not(((i = 0) and (j = 0)) or ((i = 0) and (j = numAlign - 1)) or ((i = numAlign - 1) and (j = 0))) then
        fillRectangle(alignPatPos[i] - 2, alignPatPos[j] - 2, 5, 5, qrcode);
    end;
  end;

  // Fill version blocks
  if version >= 7 then
  begin
    fillRectangle(qrsize - 11, 0, 3, 6, qrcode);
    fillRectangle(0, qrsize - 11, 6, 3, qrcode);
  end;
end;

{$REGION 'Draws light function modules and possibly some dark modules onto the given QR Code, without changing'}
/// <summary>
/// Draws light function modules and possibly some dark modules onto the given QR Code, without changing
/// non-function modules. This does not draw the format bits. This requires all function modules to be previously
/// marked dark (namely by initializeFunctionModules()), because this may skip redrawing dark function modules.
/// </summary>
{$ENDREGION}
procedure drawLightFunctionModules(qrcode: TBytes; version: TQRVersion);
var
  qrsize: integer;
  i, j, k: integer;
  dist, dx, dy: integer;
  numAlign: integer;
  alignPatPos: T7Bytes;
  rem: integer;
  bits: integer;
begin
  // Draw horizontal and vertical timing patterns
  qrsize := GetQRSize(qrcode);
  i := 7;
  while i < qrsize - 7 do
  begin
    setModule(qrcode, 6, i, false);
    setModule(qrcode, i, 6, false);
    i := i + 2;
  end;

  // Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
  for dy := -4 to 4 do
  begin
    for dx := -4 to 4 do
    begin
      dist := abs(dx);
      if abs(dy) > dist then
        dist := abs(dy);
      if (dist = 2) or (dist = 4) then
      begin
        setModuleBounded(qrcode, 3 + dx, 3 + dy, false);
        setModuleBounded(qrcode, qrsize - 4 + dx, 3 + dy, false);
        setModuleBounded(qrcode, 3 + dx, qrsize - 4 + dy, false);
      end;
    end;
  end;

  // Draw numerous alignment patterns
  numAlign := getAlignmentPatternPositions(version, alignPatPos);
  for i := 0 to numAlign - 1 do
  begin
    for j := 0 to numAlign - 1 do
    begin
      if ((i = 0) and (j = 0)) or ((i = 0) and (j = numAlign - 1)) or ((i = numAlign - 1) and (j = 0)) then
        continue; // Don't draw on the three finder corners
      for dy := -1 to 1 do
        for dx := -1 to 1 do
          setModule(qrcode, alignPatPos[i] + dx, alignPatPos[j] + dy, (dx = 0) and (dy = 0));
    end;
  end;

  // Draw version blocks
  if version >= 7 then
  begin
    // Calculate error correction code and pack bits
    rem := version; // version is uint6, in the range [7, 40]
    for i := 0 to 11 do
      rem := (rem shl 1) xor ((rem shr 11) * $1F25);
    bits := (integer(version) shl 12) or rem; // uint18
    assert((bits shr 18) = 0);

    // Draw two copies
    for i := 0 to 5 do
      for j := 0 to 2 do
      begin
        k := qrsize - 11 + j;
        setModule(qrcode, k, i, (bits and 1) <> 0);
        setModule(qrcode, i, k, (bits and 1) <> 0);
        bits := bits shr 1;
      end;
  end;
end;

{$REGION 'Draws two copies of the format bits (with its own error correction code) based'}
/// <summary>
/// Draws two copies of the format bits (with its own error correction code) based
/// on the given mask and error correction level. This always draws all modules of
/// the format bits, unlike drawLightFunctionModules() which might skip dark modules.
/// </summary>
{$ENDREGION}
procedure drawFormatBits(ecl: TErrorCorrectionLevel; mask: TQRMask; var qrcode: TBytes);
const
  table: array [0 .. 3] of integer = (1, 0, 3, 2);
var
  data, rem: integer;
  i: integer;
  qrsize: integer;
  bits: integer;
begin
  // Calculate error correction code and pack bits
  assert(mask in [qrMask0..qrMask7]);
  data := (table[Ord(ecl)] shl 3) or Ord(mask); // errCorrLvl is uint2, mask is uint3
  rem := data;
  for i := 0 to 9 do
    rem := (rem shl 1) xor ((rem shr 9) * $537);
  bits := (data shl 10 or rem) xor $5412; // uint15
  assert((bits shr 15) = 0, 'Bits ' + IntToHex(bits, 8) + ' shifted ' + IntToHex(bits shr 15, 8));

  // Draw first copy
  for i := 0 to 5 do
    setModule(qrcode, 8, i, getBit(bits, i));
  setModule(qrcode, 8, 7, getBit(bits, 6));
  setModule(qrcode, 8, 8, getBit(bits, 7));
  setModule(qrcode, 7, 8, getBit(bits, 8));
  for i := 9 to 14 do
    setModule(qrcode, 14 - i, 8, getBit(bits, i));

  // Draw second copy
  qrsize := GetQRSize(qrcode);
  for i := 0 to 7 do
    setModule(qrcode, qrsize - 1 - i, 8, getBit(bits, i));
  for i := 8 to 14 do
    setModule(qrcode, 8, qrsize - 15 + i, getBit(bits, i));
  setModule(qrcode, 8, qrsize - 8, true); // Always dark
end;

{$REGION 'Calculates and stores an ascending list of positions of alignment patterns'}
/// <summary>
/// Calculates and stores an ascending list of positions of alignment patterns
/// for this version number, returning the length of the list (in the range [0,7]).
/// Each position is in the range [0,177), and are used on both the x and y axes.
/// This could be implemented as lookup table of 40 variable-length lists of unsigned bytes.
/// </summary>
{$ENDREGION}
function getAlignmentPatternPositions(version: TQRVersion; var res: T7Bytes): integer;
var
  numAlign, step: integer;
  i, pos: integer;
begin
  if version = 1 then
  begin
    Result := 0;
    exit;
  end;
  numAlign := version div 7 + 2;
  if version = 32 then
    step := 26
  else
    step := (version * 4 + numAlign * 2 + 1) div (numAlign * 2 - 2) * 2;
  i := numAlign - 1;
  pos := version * 4 + 10;
  while i >= 1 do
  begin
    res[i] := byte(pos);
    i := i - 1;
    pos := pos - step;
  end;
  res[0] := 6;
  Result := numAlign;
end;

{$REGION 'Sets every pixel in the range [left : left + width] * [top : top + height] to dark.'}
/// <summary>
/// Sets every pixel in the range [left : left + width] * [top : top + height] to dark.
/// </summary>
{$ENDREGION}
procedure fillRectangle(left, top, width, height: integer; qrcode: TBytes);
var
  dx, dy: integer;
begin
  for dy := 0 to height - 1 do
    for dx := 0 to width - 1 do
      setModule(qrcode, left + dx, top + dy, true);
end;

(* ---- Drawing data modules and masking ---- *)

{$REGION 'Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of'}
/// <summary>
/// Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of
/// the QR Code to be dark at function modules and light at codeword modules (including unused remainder bits).
/// </summary>
{$ENDREGION}
procedure drawCodewords(const data: TBytes; dataLen: integer; var qrcode: TBytes);
var
  qrsize: integer;
  i, j, x, y: integer;
  upward: boolean;
  dark: boolean;
  right, vert: integer;
begin
  qrsize := GetQRSize(qrcode);
  i := 0; // Bit index into the data
  // Do the funny zigzag scan
  right := qrsize - 1;
  while right >= 1 do // Index of right column in each column pair
  begin
    if right = 6 then
      right := 5;
    for vert := 0 to qrsize - 1 do // Vertical counter
    begin
      for j := 0 to 1 do
      begin
        x := right - j; // Actual x coordinate
        upward := ((right + 1) and 2) = 0;
        if upward then
          y := qrsize - 1 - vert
        else
          y := vert;
        if (not getModule(qrcode, x, y)) and (i < dataLen * 8) then
        begin
          dark := getBit(data[i shr 3], 7 - (i and 7));
          setModule(qrcode, x, y, dark);
          i := i + 1;
        end;
        // If this QR Code has any remainder bits (0 to 7), they were assigned as
        // 0/false/light by the constructor and are left unchanged by this method
      end;
    end;
    right := right - 2;
  end;
  assert(i = dataLen * 8);
end;

{$REGION 'XORs the codeword modules in this QR Code with the given mask pattern.'}
/// <summary>
/// XORs the codeword modules in this QR Code with the given mask pattern.
/// The function modules must be marked and the codeword bits must be drawn
/// before masking. Due to the arithmetic of XOR, calling applyMask() with
/// the same mask value a second time will undo the mask. A final well-formed
/// QR Code needs exactly one (not zero, two, etc.) mask applied.
/// </summary>
{$ENDREGION}
procedure applyMask(const functionModules: TBytes; var qrcode: TBytes; mask: TQRMask);
var
  qrsize: integer;
  x, y: integer;
  val: boolean;
  invert: boolean;
begin
  assert(mask in [qrMask0..qrMask7]);
  qrsize := GetQRSize(qrcode);
  for y := 0 to qrsize - 1 do
  begin
    for x := 0 to qrsize - 1 do
    begin
      if getModule(functionModules, x, y) then
        continue;
      case mask of
        qrMask0:
          invert := (x + y) mod 2 = 0;
        qrMask1:
          invert := y mod 2 = 0;
        qrMask2:
          invert := x mod 3 = 0;
        qrMask3:
          invert := (x + y) mod 3 = 0;
        qrMask4:
          invert := (x div 3 + y div 2) mod 2 = 0;
        qrMask5:
          invert := x * y mod 2 + x * y mod 3 = 0;
        qrMask6:
          invert := (x * y mod 2 + x * y mod 3) mod 2 = 0;
        qrMask7:
          invert := ((x + y) mod 2 + x * y mod 3) mod 2 = 0;
      else
        raise EQRException.Create('Cannot apply mask');
      end;
      val := getModule(qrcode, x, y);
      setModule(qrcode, x, y, val xor invert);
    end;
  end;
end;

{$REGION 'Calculates and returns the penalty score based on state of the given QR Code's current modules.'}
/// <summary>
/// Calculates and returns the penalty score based on state of the given QR Code's current modules.
/// This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
/// </summary>
{$ENDREGION}
function getPenaltyScore(const qrcode: TBytes): integer;
var
  qrsize: integer;
  x, y, k: integer;
  x1: integer;
  runColor: boolean;
  runX, runY: integer;
  runHistory: T7Int;
  color: boolean;
  total: integer;
  dark: integer;
begin
  qrsize := GetQRSize(qrcode);
  Result := 0;

  // Adjacent modules in row having same color, and finder-like patterns
  for y := 0 to qrsize - 1 do
  begin
    runColor := false;
    runX := 0;
    runHistory[0] := 0;
    for x := 0 to qrsize - 1 do
    begin
      if (getModule(qrcode, x, y) = runColor) then
      begin
        runX := runX + 1;
        if runX = 5 then
          Result := Result + PENALTY_N1
        else if runX > 5 then
          Result := Result + 1;
      end
      else
      begin
        finderPenaltyAddHistory(runX, runHistory, qrsize);
        if not runColor then
        begin
          x1 := finderPenaltyCountPatterns(runHistory, qrsize);
          Result := Result + (x1 * PENALTY_N3);
        end;
        runColor := getModule(qrcode, x, y);
        runX := 1;
      end;
    end;
    Result := Result + (finderPenaltyTerminateAndCount(runColor, runX, runHistory, qrsize) * PENALTY_N3);
  end;

  // Adjacent modules in column having same color, and finder-like patterns
  for x := 0 to qrsize - 1 do
  begin
    runColor := false;
    runY := 0;
    runHistory[0] := 0;
    for y := 0 to qrsize - 1 do
    begin
      if getModule(qrcode, x, y) = runColor then
      begin
        runY := runY + 1;
        if runY = 5 then
          Result := Result + PENALTY_N1
        else if runY > 5 then
          Result := Result + 1;
      end
      else
      begin
        finderPenaltyAddHistory(runY, runHistory, qrsize);
        if not runColor then
          Result := Result + (finderPenaltyCountPatterns(runHistory, qrsize) * PENALTY_N3);
        runColor := getModule(qrcode, x, y);
        runY := 1;
      end;
    end;
    Result := Result + (finderPenaltyTerminateAndCount(runColor, runY, runHistory, qrsize) * PENALTY_N3);
  end;

  // 2*2 blocks of modules having same color
  for y := 0 to qrsize - 2 do
  begin
    for x := 0 to qrsize - 2 do
    begin
      color := getModule(qrcode, x, y);
      if (color = getModule(qrcode, x + 1, y)) and
        (color = getModule(qrcode, x, y + 1)) and
        (color = getModule(qrcode, x + 1, y + 1)) then
        Result := Result + PENALTY_N2;
    end;
  end;

  // Balance of dark and light modules
  dark := 0;
  for y := 0 to qrsize - 1 do
    for x := 0 to qrsize - 1 do
      if getModule(qrcode, x, y) then
        dark := dark + 1;
  total := qrsize * qrsize; // Note that size is odd, so dark/total != 1/2
  // Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
  k := ((abs(dark * 20 - total * 10) + total - 1) div total) - 1;
  Result := Result + (k * PENALTY_N4);
  Result := Result;
end;

{$REGION 'Can only be called immediately after a light run is added, and'}
/// <summary>
/// Can only be called immediately after a light run is added, and
/// returns either 0, 1, or 2. A helper function for getPenaltyScore().
/// </summary>
{$ENDREGION}
function finderPenaltyCountPatterns(const runHistory: T7Int; qrsize: integer): integer;
var
  n: integer;
  core: boolean;
  a, b: boolean;
begin
  n := runHistory[1];
  assert(n <= qrsize * 3);
  core := (n > 0) and (runHistory[2] = n) and (runHistory[3] = n * 3) and
    (runHistory[4] = n) and (runHistory[5] = n);
  // The maximum QR Code size is 177, hence the dark run length n <= 177.
  // Arithmetic is promoted to int, so n*4 will not overflow.
  if runHistory[6] >= n then
    a := true
  else
    a := false;
  if runHistory[0] >= n then
    b := true
  else
    b := false;
  Result := 0;
  if core and (runHistory[0] >= n * 4) and a then
    Result := Result + 1;
  if core and (runHistory[6] >= n * 4) and b then
    Result := Result + 1;
end;

{$REGION 'Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().'}
/// <summary>
/// Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().
/// </summary>
{$ENDREGION}
function finderPenaltyTerminateAndCount(currentRunColor: boolean; var currentRunLength: integer; var runHistory: T7Int; qrsize: integer): integer;
begin
  if currentRunColor then // Terminate dark run
  begin
    finderPenaltyAddHistory(currentRunLength, runHistory, qrsize);
    currentRunLength := 0;
  end;
  currentRunLength := currentRunLength + qrsize; // Add light border to final run
  finderPenaltyAddHistory(currentRunLength, runHistory, qrsize);
  Result := finderPenaltyCountPatterns(runHistory, qrsize);
end;

{$REGION 'Pushes the given value to the front and drops the last value. A helper function for getPenaltyScore().'}
/// <summary>
/// Pushes the given value to the front and drops the last value. A helper function for getPenaltyScore().
/// </summary>
{$ENDREGION}
procedure finderPenaltyAddHistory(currentRunLength: integer; var runHistory: T7Int; qrsize: integer);
var
  i: integer;
begin
  if runHistory[0] = 0 then
    currentRunLength := currentRunLength + qrsize; // Add light border to initial run
  for i := 5 downto 0 do
    runHistory[i + 1] := runHistory[i];
  runHistory[0] := currentRunLength;
end;

(* ---- Basic QR Code information ---- *)

function GetQRSize(const qrcode: TBytes): integer;
begin
  assert(qrcode <> nil);
  Result := 0;
  if length(qrcode) = 0 then
    raise EQRException.Create('QR code buffer is empty');
  Result := qrcode[0];
  assert((Low(TQRVersion) * 4 + 17 <= result)
    and (result <= High(TQRVersion) * 4 + 17));
end;

function GetQRModule(const qrcode: TBytes; x, y: integer): boolean;
var
  qrsize: integer;
begin
  assert(qrcode <> nil);
  Result := false;
  if length(qrcode) = 0 then
    raise EQRException.Create('QR code buffer is empty');
  qrsize := qrcode[0];
  Result := (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize) and getModule(qrcode, x, y);
end;

{$REGION 'Gets the module at the given coordinates, which must be in bounds.'}
/// <summary>
/// Gets the module at the given coordinates, which must be in bounds.
/// </summary>
{$ENDREGION}
function getModule(const qrcode: TBytes; x, y: integer): boolean;
var
  qrsize: integer;
  index: integer;
begin
  if length(qrcode) = 0 then
    raise EQRException.Create('QR code buffer is empty');
  qrsize := qrcode[0];
  assert((21 <= qrsize) and (qrsize <= 177) and (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize), 'qrsize = ' + IntToStr(qrsize) + ' x = ' + IntToStr(x) + ' y = ' + IntToStr(y));
  index := y * qrsize + x;
  Result := getBit(qrcode[(index shr 3) + 1], index and 7);
end;

{$REGION 'Sets the module at the given coordinates, which must be in bounds.'}
/// <summary>
/// Sets the module at the given coordinates, which must be in bounds.
/// </summary>
{$ENDREGION}
procedure setModule(var qrcode: TBytes; x, y: integer; isDark: boolean);
var
  qrsize: integer;
  index, bitIndex, byteIndex: integer;
begin
  if length(qrcode) = 0 then
    exit;
  qrsize := qrcode[0];
  assert((21 <= qrsize) and (qrsize <= 177) and (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize));
  index := (y * qrsize) + x;
  bitIndex := index and 7;
  byteIndex := (index shr 3) + 1;
  if isDark then
    qrcode[byteIndex] := qrcode[byteIndex] or (1 shl bitIndex)
  else
    qrcode[byteIndex] := qrcode[byteIndex] and ((1 shl bitIndex) xor $FF);
end;

{$REGION 'Sets the module at the given coordinates, doing nothing if out of bounds.'}
/// <summary>
/// Sets the module at the given coordinates, doing nothing if out of bounds.
/// </summary>
{$ENDREGION}
procedure setModuleBounded(var qrcode: TBytes; x, y: integer; isDark: boolean);
var
  qrsize: integer;
begin
  if length(qrcode) = 0 then
    exit;
  qrsize := qrcode[0];
  if (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize) then
    setModule(qrcode, x, y, isDark);
end;

(* ---- Segment handling ---- *)

function CanBeNumericQR(const text: AnsiString): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to length(text) do
    if AnsiPos(text[i], AnsiString('0123456789')) = 0 then
      exit;
  Result := true;
end;

function CanBeAlphanumericQR(const text: AnsiString): boolean;
var
  i, x: integer;
begin
  Result := false;
  for i := 1 to length(text) do
  begin
    x := AnsiPos(text[i], ALPHANUMERIC_CHARSET);
    if x = 0 then
      exit;
  end;
  Result := true;
end;

function CalcQRSegmentBufferSize(mode: TQRMode; numChars: integer): integer;
var
  temp: integer;
begin
  temp := calcSegmentBitLength(mode, numChars);
  assert((0 <= temp) and (temp <= High(Int16)));
  Result := (temp + 7) div 8;
end;

{$REGION 'Returns the number of data bits needed to represent a segment'}
/// <summary>
/// Returns the number of data bits needed to represent a segment
/// containing the given number of characters using the given mode. Notes:
/// - Raises EQRException on failure, i.e. numChars > INT16_MAX or
///   the number of needed bits exceeds INT16_MAX (i.e. 32767).
/// - Otherwise, all valid results are in the range [0, INT16_MAX].
/// - For byte mode, numChars measures the number of bytes, not Unicode code points.
/// - For ECI mode, numChars must be 0, and the worst-case number of bits is returned.
///   An actual ECI segment can have shorter data. For non-ECI modes, the result is exact.
//// </summary>
{$ENDREGION}
function calcSegmentBitLength(mode: TQRMode; numChars: integer): integer;
begin
  // All calculations are designed to avoid overflow on all platforms
  if (numChars > High(Int16)) then
    raise EQRException.Create('Too many characters');

  Result := numChars;

  case mode of
    qrmNUMERIC:
      Result := (Result * 10 + 2) div 3; // ceil(10/3 * n);
    qrmALPHANUMERIC:
      Result := (Result * 11 + 1) div 2; // ceil(11/2 * n)
    qrmBYTE:
      Result := Result * 8;
    qrmKANJI:
      Result := Result * 13;
    qrmECI:
      Result := 3 * 8;
  else
    raise EQRException.Create('Invalid mode');
  end;

  assert(Result >= 0);
  if Result > High(Int16) then
    raise EQRException.Create('Too many characters');
end;

function NewQRSegmentWithBinary(const data: TBytes; var buf: TBytes): TQRSegment;
var
  len: integer;
  i: integer;
begin
  len := length(data);
  assert((data <> nil) or (len = 0));
  Result.mode := qrmBYTE;
  Result.bitLength := calcSegmentBitLength(result.mode, len);
  result.numChars := len;
  if result.numChars > 0 then
  begin
    SetLength(buf, length(data));
    for i := low(data) to high(data) do
      buf[i] := data[i]; // TODO: maybe direct assignment or use move
  end
  else
    SetLength(buf, 0);
  SetLength(Result.data, length(buf));
  for i := low(buf) to high(buf) do
    Result.data[i] := buf[i]; // TODO: maybe direct assignment or use move
end;

function NewQRSegmentWithNumeric(const digits: AnsiString; var buf: TBytes): TQRSegment;
var
  accumCount: integer;
  bitLen: integer;
  len: integer;
  accumData: integer;
  i: integer;
begin
  len := length(digits);
  Result.mode := qrmNUMERIC;
  bitLen := calcSegmentBitLength(Result.mode, len);
  Result.numChars := len;
  if bitLen > 0 then
    for i := low(buf) to high(buf) do
      buf[i] := 0;
  Result.bitLength := 0;

  accumData := 0;
  accumCount := 0;
  for i := 1 to length(digits) do
  begin
    assert(('0' <= digits[i]) and (digits[i] <= '9'));
    accumData := (accumData * 10) + ord(digits[i]) - ord('0');
    accumCount := accumCount + 1;
    if accumCount = 3 then
    begin
      appendBitsToBuffer(accumData, 10, buf, Result.bitLength);
      accumData := 0;
      accumCount := 0;
    end;
  end;
  if accumCount > 0 then // 1 or 2 digits remaining
    appendBitsToBuffer(accumData, accumCount * 3 + 1, buf, Result.bitLength);
  assert(Result.bitLength = bitLen);
  SetLength(Result.data, length(buf));
  for i := low(buf) to high(buf) do
    Result.data[i] := buf[i]; // TODO: maybe direct assignment or use move
end;

function NewQRSegmentWithAlphanumeric(const text: AnsiString; var buf: TBytes): TQRSegment;
var
  bitLen: integer;
  len: integer;
  i, x: integer;
  accumCount: integer;
  accumData: word; // TODO: check if this is umsigned int
begin
  len := length(text);
  Result.mode := qrmALPHANUMERIC;
  bitLen := calcSegmentBitLength(Result.mode, len);
  Result.numChars := len;
  if bitLen > 0 then
  begin
    for i := low(buf) to high(buf) do
      buf[i] := 0;
  end;
  Result.bitLength := 0;

  accumData := 0;
  accumCount := 0;
  for i := 1 to len do
  begin
    x := AnsiPos(text[i], ALPHANUMERIC_CHARSET) - 1;
    assert(x >= 0);
    accumData := accumData * 45 + x;
    accumCount := accumCount + 1;
    if accumCount = 2 then
    begin
      appendBitsToBuffer(accumData, 11, buf, result.bitLength);
      accumData := 0;
      accumCount := 0;
    end;
  end;
  if accumCount > 0 then // 1 character remaining
    appendBitsToBuffer(accumData, 6, buf, result.bitLength);
  assert(result.bitLength = bitLen);
  SetLength(Result.data, length(buf));
  for i := low(buf) to high(buf) do
    Result.data[i] := buf[i]; // TODO: maybe direct assignment or use move
end;

function NewQRSegmentWithECI(assignVal: integer; var buf: TBytes): TQRSegment;
var
  i: integer;
begin
  Result.mode := qrmECI;
  Result.numChars := 0;
  Result.bitLength := 0;
  for i := low(buf) to high(buf) do
    buf[i] := 0;
  if assignVal < 0 then
    raise EQRException.Create('Invalid ECI value')
  else if assignVal < 1 shl 7 then
  begin
    appendBitsToBuffer(assignVal, 8, buf, Result.bitLength);
  end
  else if assignVal < 1 shl 14 then
  begin
    appendBitsToBuffer(2, 2, buf, Result.bitLength);
    appendBitsToBuffer(assignVal, 14, buf, Result.bitLength);
  end
  else if (assignVal < 1000000) then
  begin
    appendBitsToBuffer(6, 3, buf, &result.bitLength);
    appendBitsToBuffer(assignVal shr 10, 11, buf, Result.bitLength);
    appendBitsToBuffer(assignVal and $3FF, 10, buf, Result.bitLength);
  end
  else
    raise EQRException.Create('Invalid ECI value');
  SetLength(Result.data, length(buf));
  for i := low(buf) to high(buf) do
    Result.data[i] := buf[i]; // TODO: maybe direct assignment or use move
end;

{$REGION 'Calculates the number of bits needed to encode the given segments at the given version.'}
/// <summary>
/// Calculates the number of bits needed to encode the given segments at the given version.
/// Returns a non-negative number if successful. Otherwise returns -1 if a segment has too
/// many characters to fit its length field, or the total bits exceeds INT16_MAX.
/// </summary>
{$ENDREGION}
function getTotalBits(const segs: TQRSegments; version: TQRVersion): integer;
var
  i: integer;
  numChars, bitLength, ccbits: integer;
begin
  assert((segs <> nil) or (Length(segs) = 0));
  Result := 0;
  if segs = nil then
    exit;
  if length(segs) = 0 then
    exit;
  for i := low(segs) to high(segs) do
  begin
    numChars := segs[i].numChars;
    bitLength := segs[i].bitLength;
    assert((0 <= numChars) and (numChars <= High(Int16)));
    assert((0 <= bitLength) and (bitLength <= High(Int16)));
    ccbits := numCharCountBits(segs[i].mode, version);
    assert((0 <= ccbits) and (ccbits <= 16));
    if numChars >= (1 shl ccbits) then
    begin
      Result := -1;
      exit;
    end;
    Result := Result + 4 + ccbits + bitLength;
    if Result > High(Int16) then
    begin
      Result := -1;
      exit;
    end;
  end;
  Assert((0 <= Result) and (Result <= High(Int16)));
end;

{$REGION 'Returns the bit width of the character count field for a segment in the given mode'}
/// <summary>
/// Returns the bit width of the character count field for a segment in the given mode
/// in a QR Code at the given version number. The result is in the range [0, 16].
/// </summary>
{$ENDREGION}
function numCharCountBits(mode: TQRMode; version: TQRVersion): integer;
begin
  Result := -1;
  case version of
    1 .. 9:
      begin
        case mode of
          qrmNUMERIC:
            Result := 10;
          qrmALPHANUMERIC:
            Result := 9;
          qrmBYTE:
            Result := 8;
          qrmKANJI:
            Result := 8;
          qrmECI:
            Result := 0;
        end;
      end;
    10 .. 26:
      begin
        case mode of
          qrmNUMERIC:
            Result := 12;
          qrmALPHANUMERIC:
            Result := 11;
          qrmBYTE:
            Result := 16;
          qrmKANJI:
            Result := 10;
          qrmECI:
            Result := 0;
        end;
      end;
    27 .. 40:
      begin
        case mode of
          qrmNUMERIC:
            Result := 14;
          qrmALPHANUMERIC:
            Result := 13;
          qrmBYTE:
            Result := 16;
          qrmKANJI:
            Result := 12;
          qrmECI:
            Result := 0;
        end;
      end;
  end;
  if Result < 0 then
    raise EQRException.Create('numCharCountBits failed');
end;

procedure WriteToFile(const buffer: TBytes; bitcount: integer; const filename: string); overload;
var
  bytecount: integer;
  stream : TFileStream;
begin
  if bitcount mod 8 <> 0 then
    bytecount := (bitcount div 8) + 1
  else
    bytecount := (bitcount div 8);

  stream := TFileStream.Create(filename, fmCreate);
  try
    stream.WriteBuffer(buffer[0], bytecount);
  finally
    stream.Free;
  end;
end;

procedure WriteToFile(const buffer: TBytes; bitcount: integer; prefix: char; filenumber: integer); overload;
begin
  WriteToFile(buffer, bitcount, Format('%s%d.bin', [prefix, filenumber]));
end;

procedure WriteToFile(const buffer: TBytes; bitcount: integer; prefix: char; x, y: integer); overload;
begin
  WriteToFile(buffer, bitcount, Format('%s%d-%d.bin', [prefix, x, y]));
end;

{$REGION 'Internal testing routines'}
{$IFDEF QRCODEGEN_TESTING}
procedure Log(const inText : string);
begin
  if Assigned(LogProc) then
    LogProc(inText)
  else
    WriteLn(inText);
end;

procedure itest(a, b: integer);
begin
  if a <> b then
    Log('Test Failed ' + IntToStr(a) + ' v ' + IntToStr(b));
end;

procedure booltest(a, b: boolean);
const
  ft: array [boolean] of string = ('false', 'true');
begin
  if a <> b then
    Log('Test Failed ' + ft[a] + ' v ' + ft[b]);
end;

procedure btest(a, b: byte);
begin
  if a <> b then
    Log('Test Failed ' + IntToHex(a, 2) + ' v ' + IntToHex(b, 2));
end;

(*
static void testAppendBitsToBuffer(void) {
	{
		uint8_t buf[1] = {0};
		int bitLen = 0;
		appendBitsToBuffer(0, 0, buf, &bitLen);
		assert(bitLen == 0);
		assert(buf[0] == 0);
		appendBitsToBuffer(1, 1, buf, &bitLen);
		assert(bitLen == 1);
		assert(buf[0] == 0x80);
		appendBitsToBuffer(0, 1, buf, &bitLen);
		assert(bitLen == 2);
		assert(buf[0] == 0x80);
		appendBitsToBuffer(5, 3, buf, &bitLen);
		assert(bitLen == 5);
		assert(buf[0] == 0xA8);
		appendBitsToBuffer(6, 3, buf, &bitLen);
		assert(bitLen == 8);
		assert(buf[0] == 0xAE);
		numTestCases++;
	}
	{
		uint8_t buf[6] = {0};
		int bitLen = 0;
		appendBitsToBuffer(16942, 16, buf, &bitLen);
		assert(bitLen == 16);
		assert(buf[0] == 0x42 && buf[1] == 0x2E && buf[2] == 0x00 && buf[3] == 0x00 && buf[4] == 0x00 && buf[5] == 0x00);
		appendBitsToBuffer(10, 7, buf, &bitLen);
		assert(bitLen == 23);
		assert(buf[0] == 0x42 && buf[1] == 0x2E && buf[2] == 0x14 && buf[3] == 0x00 && buf[4] == 0x00 && buf[5] == 0x00);
		appendBitsToBuffer(15, 4, buf, &bitLen);
		assert(bitLen == 27);
		assert(buf[0] == 0x42 && buf[1] == 0x2E && buf[2] == 0x15 && buf[3] == 0xE0 && buf[4] == 0x00 && buf[5] == 0x00);
		appendBitsToBuffer(26664, 15, buf, &bitLen);
		assert(bitLen == 42);
		assert(buf[0] == 0x42 && buf[1] == 0x2E && buf[2] == 0x15 && buf[3] == 0xFA && buf[4] == 0x0A && buf[5] == 0x00);
		numTestCases++;
	}
}
*)

// Ported from the Java version of the code.
function addEccAndInterleaveReference(const data: TBytes; version: integer; ecl: TErrorCorrectionLevel): TBytes;
type
  TBytess = array of TBytes;
var
  numBlocks, blockEccLen, rawCodewords, numShortBlocks, shortBlockLen: integer;
  generator: TBytes;
  i, j, k: integer;
  datLen: integer;
  blocks: TBytess;
  // tmp1, tmp2 : TBytes;
begin
  // Calculate parameter numbers
  numBlocks := NUM_ERROR_CORRECTION_BLOCKS[ecl][version];
  blockEccLen := ECC_CODEWORDS_PER_BLOCK[ecl][version];
  rawCodewords := getNumRawDataModules(version) div 8;
  numShortBlocks := numBlocks - rawCodewords mod numBlocks;
  shortBlockLen := rawCodewords div numBlocks;
  SetLength(blocks, numBlocks);
  SetLength(generator, blockecclen);
  reedSolomonComputeDivisor(blockEccLen, generator);
  k := 0;
  for i := 0 to numBlocks - 1 do
  begin
    SetLength(blocks[i], shortBlockLen);
    if i < numShortBlocks then
      datLen := shortBlockLen - blockEccLen
    else
      datLen := shortBlockLen - blockEccLen + 1;
    for j := 0 to datLen do
      blocks[i][j] := data[j + k];
    // needs to change       reedSolomonComputeRemainder (data[k], datLen, generator, blockEccLen, blocks[shortBlockLen + 1 - blockEccLen]);
    k := k + datLen;
  end;

  SetLength(Result, rawCodewords);
  k := 0;
  for i := 0 to shortBlockLen do
  begin
    for j := 0 to numBlocks - 1 do
    begin
      // Skip the padding byte in short blocks
      if (i <> shortBlockLen - blockEccLen) or (j >= numShortBlocks) then
      begin
        result[k] := blocks[j][i];
        k := k + 1;
      end;
    end;
  end;
  for i := low(blocks) to high(blocks) do
    SetLength(Blocks[i], 0);
  SetLength(Blocks, 0);
end;

procedure testAddEccAndInterleave;
var
  version: integer;
  ecl: integer;
  dataLen, dataAndEccLen: integer;
  i: integer;
  pureData, paddedData, actualOutput, expectedOutput: TBytes;
  ok: boolean;
begin
  for version := 1 to 40 do
  begin
    for ecl := 0 to 3 do
    begin
      dataLen := getNumDataCodewords(version, TErrorCorrectionLevel(ecl));
      SetLength(pureData, dataLen);
      for i := low(pureData) to high(pureData) do
        pureData[i] := random(256);
      expectedOutput := addEccAndInterleaveReference(pureData, version, TErrorCorrectionLevel(ecl));
      dataAndEccLen := getNumRawDataModules(version) div 8;
      SetLength(paddedData, dataAndEccLen);
      for i := 0 to dataLen - 1 do
        paddedData[i] := pureData[i];
      SetLength(actualOutput, dataAndEccLen);
      addEccAndInterleave(paddedData, version, TErrorCorrectionLevel(ecl), actualOutput);
      if length(actualOutput) = length(expectedOutput) then
      begin
        ok := true;
        for i := low(actualOutput) to high(expectedOutput) do // dataandecclen
          if actualOutput[i] <> expectedOutput[i] then
            ok := false;
        if not ok then
          Log('Actual Expected DONT Match');
      end
      else
        Log('Length mismatch');
      // assert(memcmp(actualOutput, expectOutput, dataAndEccLen * sizeof(uint8_t)) == 0);
      SetLength(pureData, 0);
      SetLength(expectedOutput, 0);
      SetLength(paddedData, 0);
      SetLength(actualOutput, 0);
    end;
  end;
end;

(*
static void testGetNumDataCodewords(void) {
	const int cases[][3] = {
		{ 3, 1,   44},
		{ 3, 2,   34},
		{ 3, 3,   26},
		{ 6, 0,  136},
		{ 7, 0,  156},
		{ 9, 0,  232},
		{ 9, 1,  182},
		{12, 3,  158},
		{15, 0,  523},
		{16, 2,  325},
		{19, 3,  341},
		{21, 0,  932},
		{22, 0, 1006},
		{22, 1,  782},
		{22, 3,  442},
		{24, 0, 1174},
		{24, 3,  514},
		{28, 0, 1531},
		{30, 3,  745},
		{32, 3,  845},
		{33, 0, 2071},
		{33, 3,  901},
		{35, 0, 2306},
		{35, 1, 1812},
		{35, 2, 1286},
		{36, 3, 1054},
		{37, 3, 1096},
		{39, 1, 2216},
		{40, 1, 2334},
	};
	for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
		const int *tc = cases[i];
		assert(getNumDataCodewords(tc[0], (enum qrcodegen_Ecc)tc[1]) == tc[2]);
		numTestCases++;
	}
}


static void testGetNumRawDataModules(void) {
	const int cases[][2] = {
		{ 1,   208},
		{ 2,   359},
		{ 3,   567},
		{ 6,  1383},
		{ 7,  1568},
		{12,  3728},
		{15,  5243},
		{18,  7211},
		{22, 10068},
		{26, 13652},
		{32, 19723},
		{37, 25568},
		{40, 29648},
	};
	for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
		const int *tc = cases[i];
		assert(getNumRawDataModules(tc[0]) == tc[1]);
		numTestCases++;
	}
}

*)

procedure testReedSolomonComputeDevisor; // seems ok
var
  generator: TBytes;
begin
  SetLength(generator, 30);
  reedSolomonComputeDivisor(1, generator);
  btest(generator[0], $01);
  reedSolomonComputeDivisor(2, generator);
  btest(generator[0], $03);
  btest(generator[1], $02);
  reedSolomonComputeDivisor(5, generator);
  btest(generator[0], $1F);
  btest(generator[1], $C6);
  btest(generator[2], $3F);
  btest(generator[3], $93);
  btest(generator[4], $74);
  reedSolomonComputeDivisor(30, generator);
  btest(generator[0], $D4);
  btest(generator[1], $F6);
  btest(generator[5], $C0);
  btest(generator[12], $16);
  btest(generator[13], $D9);
  btest(generator[20], $12);
  btest(generator[27], $6A);
  btest(generator[29], $96);
end;

procedure testReedSolomonComputeRemainder;
var
  data: TBytes;
  generator: TBytes;
  remainder: TBytes;
begin
  SetLength(data, 0);
  SetLength(generator, 3);
  SetLength(remainder, length(generator));

  reedSolomonComputeDivisor(length(generator), generator);
  reedSolomonComputeRemainder(data, length(data), generator, length(generator), remainder);
  btest(remainder[0], 0);
  btest(remainder[1], 0);
  btest(remainder[2], 0);
  SetLength(data, 2); data[0] := 0; data[1] := 1;
  SetLength(generator, 4);
  SetLength(remainder, length(generator));
  reedSolomonComputeDivisor(length(generator), generator);
  reedSolomonComputeRemainder(data, length(data), generator, length(generator), remainder);
  btest(remainder[0], generator[0]);
  btest(remainder[1], generator[1]);
  btest(remainder[2], generator[2]);
  btest(remainder[3], generator[3]);
  SetLength(data, 5); data[0] := $03; data[1] := $3a; data[2] := $60; data[3] := $12; data[4] := $c7;
  SetLength(generator, 5);
  SetLength(remainder, length(generator));
  reedSolomonComputeDivisor(length(generator), generator);
  reedSolomonComputeRemainder(data, length(data), generator, length(generator), remainder);
  btest(remainder[0], $CB);
  btest(remainder[1], $36);
  btest(remainder[2], $16);
  btest(remainder[3], $FA);
  btest(remainder[4], $9D);
  (*
    {
    uint8_t data[43] = {
    0x38, 0x71, 0xDB, 0xF9, 0xD7, 0x28, 0xF6, 0x8E, 0xFE, 0x5E,
    0xE6, 0x7D, 0x7D, 0xB2, 0xA5, 0x58, 0xBC, 0x28, 0x23, 0x53,
    0x14, 0xD5, 0x61, 0xC0, 0x20, 0x6C, 0xDE, 0xDE, 0xFC, 0x79,
    0xB0, 0x8B, 0x78, 0x6B, 0x49, 0xD0, 0x1A, 0xAD, 0xF3, 0xEF,
    0x52, 0x7D, 0x9A,
    };
    uint8_t generator[30];
    uint8_t remainder[ARRAY_LENGTH(generator)];
    reedSolomonComputeDivisor(ARRAY_LENGTH(generator), generator);
    reedSolomonComputeRemainder(data, ARRAY_LENGTH(data), generator, ARRAY_LENGTH(generator), remainder);
    assert(remainder[ 0] == 0xCE);
    assert(remainder[ 1] == 0xF0);
    assert(remainder[ 2] == 0x31);
    assert(remainder[ 3] == 0xDE);
    assert(remainder[ 8] == 0xE1);
    assert(remainder[12] == 0xCA);
    assert(remainder[17] == 0xE3);
    assert(remainder[19] == 0x85);
    assert(remainder[20] == 0x50);
    assert(remainder[24] == 0xBE);
    assert(remainder[29] == 0xB3);
    numTestCases++;
    }
    }

  *)
end;

procedure testReedSolomonComputeMultiply; // seems ok
const cases: array [0 .. 15, 0 .. 2] of byte = (
    ($00, $00, $00),
    ($01, $01, $01),
    ($02, $02, $04),
    ($00, $6E, $00),
    ($B2, $DD, $E6),
    ($41, $11, $25),
    ($B0, $1F, $11),
    ($05, $75, $BC),
    ($52, $B5, $AE),
    ($A8, $20, $A4),
    ($0E, $44, $9F),
    ($D4, $13, $A0),
    ($31, $10, $37),
    ($6C, $58, $CB),
    ($B6, $75, $3E),
    ($FF, $FF, $E2));
var
  i: integer;
begin
  for i := 0 to 15 do
  begin
    btest(reedSolomonMultiply(cases[i, 0], cases[i, 1]), cases[i, 2]);
  end;
end;

procedure testInitializeFunctionModulesEtc;
var // this seems to test ok
  ver: integer;
  size: integer;
  x, y: integer;
  qrcode: TBytes;
  color: boolean;
  hasLight, hasDark: boolean;
begin
  for ver := 1 to 40 do
  begin
    Log('Testing Version ' + IntToStr(ver));
    SetLength(qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(ver));
    initializeFunctionModules(ver, qrcode);
    // Log('Initialised Function Modules');
    size := GetQRSize(qrcode);
    if ver = 1 then
      itest(size, 21)
    else if ver = 40 then
      itest(size, 177)
    else
      itest(size, ver * 4 + 17);
    hasLight := false;
    hasDark := false;
    for y := 0 to size - 1 do
    begin
      for x := 0 to size - 1 do
      begin
        color := GetQRModule(qrcode, x, y);
        if color then
          hasDark := true
        else
          hasLight := true;
      end;
    end;
    if hasLight and hasDark then
      Log('Light Dark Test ok')
    else
      Log('Light Dark Test Failed');
  end;
end;

procedure testGetAlignmentPatternPositions;
const
  cases: array [0 .. 11, 0 .. 8] of integer = (
    (1, 0, -1, -1, -1, -1, -1, -1, -1),
    (2, 2, 6, 18, -1, -1, -1, -1, -1),
    (3, 2, 6, 22, -1, -1, -1, -1, -1),
    (6, 2, 6, 34, -1, -1, -1, -1, -1),
    (7, 3, 6, 22, 38, -1, -1, -1, -1),
    (8, 3, 6, 24, 42, -1, -1, -1, -1),
    (16, 4, 6, 26, 50, 74, -1, -1, -1),
    (25, 5, 6, 32, 58, 84, 110, -1, -1),
    (32, 6, 6, 34, 60, 86, 112, 138, -1),
    (33, 6, 6, 30, 58, 86, 114, 142, -1),
    (39, 7, 6, 26, 54, 82, 110, 138, 166),
    (40, 7, 6, 30, 58, 86, 114, 142, 170));
var
  i, j: integer;
  p: T7Bytes;
  num: integer;
begin
  for i := 0 to 11 do
  begin
    num := getAlignmentPatternPositions(cases[i, 0], p);
    itest(num, cases[i, 1]);
    for j := 0 to num - 1 do
      itest(p[j], cases[i, 2 + j]);
  end;
end;

procedure testGetSetModule;
var
  qrcode: TBytes;
  size: integer;
  x, y: integer;
  light: boolean;
begin
  SetLength(qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(23));
  initializeFunctionModules(23, qrcode);
  size := GetQRSize(qrcode);
  for y := 0 to size - 1 do
  begin // Clear all to light
    for x := 0 to size - 1 do
      setModule(qrcode, x, y, false);
  end;
  for y := 0 to size - 1 do // Check all light
  begin
    for x := 0 to size - 1 do
      booltest(GetQRModule(qrcode, x, y), false);
  end;
  for y := 0 to size - 1 do // Set all to dark
  begin
    for x := 0 to size - 1 do
      setModule(qrcode, x, y, true);
  end;
  for y := 0 to size - 1 do // Check all dark
  begin
    for x := 0 to size - 1 do
      assert(GetQRModule(qrcode, x, y) = true);
  end;
  // Set some out of bounds modules to light
  setModuleBounded(qrcode, -1, -1, false);
  setModuleBounded(qrcode, -1, 0, false);
  setModuleBounded(qrcode, 0, -1, false);
  setModuleBounded(qrcode, size, 5, false);
  setModuleBounded(qrcode, 72, size, false);
  setModuleBounded(qrcode, size, size, false);
  for y := 0 to size - 1 do // Check all dark
  begin
    for x := 0 to size - 1 do
      booltest(GetQRModule(qrcode, x, y), true);
  end;
  // Set some modules to light
  setModule(qrcode, 3, 8, false);
  setModule(qrcode, 61, 49, false);
  for y := 0 to size - 1 do // Check most dark
  begin
    for x := 0 to size - 1 do
    begin
      light := ((x = 3) and (y = 8)) or ((x = 61) and (y = 49));
      booltest(GetQRModule(qrcode, x, y), not light);
    end;
  end;
end;

procedure testGetSetModuleRandomly;
var
  trials: cardinal;
  qrcode: TBytes;
  size: integer;
  x, y: integer;
  i: cardinal;
  isInBounds, oldColor, newColor: boolean;
  modules: array [0 .. 20, 0 .. 20] of boolean;
begin
  SetLength(qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(1));
  initializeFunctionModules(1, qrcode);
  size := GetQRSize(qrcode);
  for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
      modules[y, x] := GetQRModule(qrcode, x, y);
  end;
  trials := 100000;
  for i := 0 to trials - 1 do
  begin
    x := random(300) mod (size * 2) - size div 2;
    y := random(300) mod (size * 2) - size div 2;
    isInBounds := (0 <= x) and (x < size) and (0 <= y) and (y < size);
    oldColor := isInBounds and modules[y][x];
    if isInBounds then
      booltest(getModule(qrcode, x, y), oldColor);
    booltest(GetQRModule(qrcode, x, y), oldColor);
    newColor := (random(300) mod 2) = 0;
    if isInBounds then
      modules[y][x] := newColor;
    if isInBounds and (random(300) mod 2 = 0) then
      setModule(qrcode, x, y, newColor)
    else
      setModuleBounded(qrcode, x, y, newColor);
  end;
end;

procedure testIsAlphanumeric; // seems ok
type
  TestCase = record
    answer: boolean;
    text: AnsiString;
  end;
const
  cases: array [0 .. 18] of TestCase = (
    (answer: true; text: ''),
    (answer: true; text: '0'),
    (answer: true; text: 'A'),
    (answer: false; text: 'a'),
    (answer: true; text: ' '),
    (answer: true; text: '.'),
    (answer: true; text: '*'),
    (answer: false; text: ','),
    (answer: false; text: '|'),
    (answer: false; text: '@'),
    (answer: true; text: 'XYZ'),
    (answer: false; text: 'XYZ!'),
    (answer: true; text: '79068'),
    (answer: true; text: '+123 ABC$'),
    (answer: false; text: #01),
    (answer: false; text: #127),
    (answer: false; text: #128),
    (answer: false; text: #192),
    (answer: false; text: #255));
var
  i: integer;
begin
  for i := 0 to 18 do
  begin
    booltest(CanBeAlphanumericQR(cases[i].text), cases[i].answer);
  end;
end;

(*
static void testIsNumeric(void) {
	struct TestCase {
		bool answer;
		const char *text;
	};
	const struct TestCase cases[] = {
		{true, ""},
		{true, "0"},
		{false, "A"},
		{false, "a"},
		{false, " "},
		{false, "."},
		{false, "*"},
		{false, ","},
		{false, "|"},
		{false, "@"},
		{false, "XYZ"},
		{false, "XYZ!"},
		{true, "79068"},
		{false, "+123 ABC$"},
		{false, "\x01"},
		{false, "\x7F"},
		{false, "\x80"},
		{false, "\xC0"},
		{false, "\xFF"},
	};
	for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
		assert(CanBeNumericQR(cases[i].text) == cases[i].answer);
		numTestCases++;
	}
}


static void testCalcSegmentBufferSize(void) {
	{
		const size_t cases[][2] = {
			{0, 0},
			{1, 1},
			{2, 1},
			{3, 2},
			{4, 2},
			{5, 3},
			{6, 3},
			{1472, 614},
			{2097, 874},
			{5326, 2220},
			{9828, 4095},
			{9829, 4096},
			{9830, 4096},
			{9831, SIZE_MAX},
			{9832, SIZE_MAX},
			{12000, SIZE_MAX},
			{28453, SIZE_MAX},
			{55555, SIZE_MAX},
			{SIZE_MAX / 6, SIZE_MAX},
			{SIZE_MAX / 4, SIZE_MAX},
			{SIZE_MAX / 2, SIZE_MAX},
			{SIZE_MAX / 1, SIZE_MAX},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
			assert(CalcQRSegmentBufferSize(qrcodegen_Mode_NUMERIC, cases[i][0]) == cases[i][1]);
			numTestCases++;
		}
	}
	{
		const size_t cases[][2] = {
			{0, 0},
			{1, 1},
			{2, 2},
			{3, 3},
			{4, 3},
			{5, 4},
			{6, 5},
			{1472, 1012},
			{2097, 1442},
			{5326, 3662},
			{5955, 4095},
			{5956, 4095},
			{5957, 4096},
			{5958, SIZE_MAX},
			{5959, SIZE_MAX},
			{12000, SIZE_MAX},
			{28453, SIZE_MAX},
			{55555, SIZE_MAX},
			{SIZE_MAX / 10, SIZE_MAX},
			{SIZE_MAX / 8, SIZE_MAX},
			{SIZE_MAX / 5, SIZE_MAX},
			{SIZE_MAX / 2, SIZE_MAX},
			{SIZE_MAX / 1, SIZE_MAX},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
			assert(CalcQRSegmentBufferSize(qrcodegen_Mode_ALPHANUMERIC, cases[i][0]) == cases[i][1]);
			numTestCases++;
		}
	}
	{
		const size_t cases[][2] = {
			{0, 0},
			{1, 1},
			{2, 2},
			{3, 3},
			{1472, 1472},
			{2097, 2097},
			{4094, 4094},
			{4095, 4095},
			{4096, SIZE_MAX},
			{4097, SIZE_MAX},
			{5957, SIZE_MAX},
			{12000, SIZE_MAX},
			{28453, SIZE_MAX},
			{55555, SIZE_MAX},
			{SIZE_MAX / 16 + 1, SIZE_MAX},
			{SIZE_MAX / 14, SIZE_MAX},
			{SIZE_MAX / 9, SIZE_MAX},
			{SIZE_MAX / 7, SIZE_MAX},
			{SIZE_MAX / 4, SIZE_MAX},
			{SIZE_MAX / 3, SIZE_MAX},
			{SIZE_MAX / 2, SIZE_MAX},
			{SIZE_MAX / 1, SIZE_MAX},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
			assert(CalcQRSegmentBufferSize(qrcodegen_Mode_BYTE, cases[i][0]) == cases[i][1]);
			numTestCases++;
		}
	}
	{
		const size_t cases[][2] = {
			{0, 0},
			{1, 2},
			{2, 4},
			{3, 5},
			{1472, 2392},
			{2097, 3408},
			{2519, 4094},
			{2520, 4095},
			{2521, SIZE_MAX},
			{5957, SIZE_MAX},
			{2522, SIZE_MAX},
			{12000, SIZE_MAX},
			{28453, SIZE_MAX},
			{55555, SIZE_MAX},
			{SIZE_MAX / 13 + 1, SIZE_MAX},
			{SIZE_MAX / 12, SIZE_MAX},
			{SIZE_MAX / 9, SIZE_MAX},
			{SIZE_MAX / 4, SIZE_MAX},
			{SIZE_MAX / 3, SIZE_MAX},
			{SIZE_MAX / 2, SIZE_MAX},
			{SIZE_MAX / 1, SIZE_MAX},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(cases); i++) {
			assert(CalcQRSegmentBufferSize(qrcodegen_Mode_KANJI, cases[i][0]) == cases[i][1]);
			numTestCases++;
		}
	}
	{
		assert(CalcQRSegmentBufferSize(qrcodegen_Mode_ECI, 0) == 3);
		numTestCases++;
	}
}


static void testCalcSegmentBitLength(void) {
	struct TestCase {
		size_t numChars;
		int result;
	};
	{
		const struct TestCase CASES[] = {
			{0, 0},
			{1, 4},
			{2, 7},
			{3, 10},
			{4, 14},
			{5, 17},
			{6, 20},
			{1472, 4907},
			{2097, 6990},
			{5326, 17754},
			{9828, 32760},
			{9829, 32764},
			{9830, 32767},
			{9831, -1},
			{9832, -1},
			{12000, -1},
			{28453, -1},
			{SIZE_MAX / 6, -1},
			{SIZE_MAX / 3, -1},
			{SIZE_MAX / 2, -1},
			{SIZE_MAX / 1, -1},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(CASES); i++) {
			assert(calcSegmentBitLength(qrcodegen_Mode_NUMERIC, CASES[i].numChars) == CASES[i].result);
			numTestCases++;
		}
	}
	{
		const struct TestCase CASES[] = {
			{0, 0},
			{1, 6},
			{2, 11},
			{3, 17},
			{4, 22},
			{5, 28},
			{6, 33},
			{1472, 8096},
			{2097, 11534},
			{5326, 29293},
			{5955, 32753},
			{5956, 32758},
			{5957, 32764},
			{5958, -1},
			{5959, -1},
			{12000, -1},
			{28453, -1},
			{SIZE_MAX / 10, -1},
			{SIZE_MAX / 5, -1},
			{SIZE_MAX / 2, -1},
			{SIZE_MAX / 1, -1},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(CASES); i++) {
			assert(calcSegmentBitLength(qrcodegen_Mode_ALPHANUMERIC, CASES[i].numChars) == CASES[i].result);
			numTestCases++;
		}
	}
	{
		const struct TestCase CASES[] = {
			{0, 0},
			{1, 8},
			{2, 16},
			{3, 24},
			{1472, 11776},
			{2097, 16776},
			{4094, 32752},
			{4095, 32760},
			{4096, -1},
			{4097, -1},
			{5957, -1},
			{12000, -1},
			{28453, -1},
			{SIZE_MAX / 15, -1},
			{SIZE_MAX / 12, -1},
			{SIZE_MAX / 7, -1},
			{SIZE_MAX / 3, -1},
			{SIZE_MAX / 1, -1},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(CASES); i++) {
			assert(calcSegmentBitLength(qrcodegen_Mode_BYTE, CASES[i].numChars) == CASES[i].result);
			numTestCases++;
		}
	}
	{
		const struct TestCase CASES[] = {
			{0, 0},
			{1, 13},
			{2, 26},
			{3, 39},
			{1472, 19136},
			{2097, 27261},
			{2519, 32747},
			{2520, 32760},
			{2521, -1},
			{5957, -1},
			{2522, -1},
			{12000, -1},
			{28453, -1},
			{SIZE_MAX / 25, -1},
			{SIZE_MAX / 20, -1},
			{SIZE_MAX / 11, -1},
			{SIZE_MAX / 4, -1},
			{SIZE_MAX / 2, -1},
			{SIZE_MAX / 1, -1},
		};
		for (size_t i = 0; i < ARRAY_LENGTH(CASES); i++) {
			assert(calcSegmentBitLength(qrcodegen_Mode_KANJI, CASES[i].numChars) == CASES[i].result);
			numTestCases++;
		}
	}
	{
		assert(calcSegmentBitLength(qrcodegen_Mode_ECI, 0) == 24);
		numTestCases++;
	}
}


*)
procedure testMakeBytes;
var
  seg: TQRSegment;
  data, buf: TBytes;
begin
  seg := NewQRSegmentWithBinary(nil, buf);
  itest(ord(seg.mode), ord(qrmBYTE));
  itest(seg.numChars, 0);
  itest(seg.bitLength, 0);
  SetLength(data, 1);
  data[0] := $00;
  seg := NewQRSegmentWithBinary(data, buf);
  itest(seg.numChars, 1);
  itest(seg.bitLength, 8);
  Log('data length ' + IntToStr(length(buf)));
  btest(seg.data[0], $00);
  SetLength(data, 3);
  data[0] := $ef;
  data[1] := $bb;
  data[2] := $bf;
  setLength(buf, 0);
  seg := NewQRSegmentWithBinary(data, buf);
  itest(seg.numChars, 3);
  itest(seg.bitLength, 24);
  btest(seg.data[0], $EF);
  btest(seg.data[1], $BB);
  btest(seg.data[2], $BF);
end;

procedure testMakeNumeric;
var
  seg: TQRSegment;
  buf: TBytes;
begin
  seg := NewQRSegmentWithNumeric('', buf);
  itest(integer(seg.mode), integer(qrmNUMERIC));
  itest(seg.numChars, 0);
  itest(seg.bitLength, 0);
  SetLength(buf, 1);
  seg := NewQRSegmentWithNumeric('9', buf);
  itest(seg.numChars, 1);
  itest(seg.bitLength, 4);
  btest(seg.data[0], $90);
  SetLength(buf, 1);
  seg := NewQRSegmentWithNumeric('81', buf);
  itest(seg.numChars, 2);
  itest(seg.bitLength, 7);
  btest(seg.data[0], $A2);
  SetLength(buf, 2);
  seg := NewQRSegmentWithNumeric('673', buf);
  itest(seg.numChars, 3);
  itest(seg.bitLength, 10);
  btest(seg.data[0], $A8);
  btest(seg.data[1], $40);
  SetLength(buf, 5);
  seg := NewQRSegmentWithNumeric('3141592653', buf);
  itest(seg.numChars, 10);
  itest(seg.bitLength, 34);
  btest(seg.data[0], $4E);
  btest(seg.data[1], $89);
  btest(seg.data[2], $F4);
  btest(seg.data[3], $24);
  btest(seg.data[4], $C0);
end;

procedure testMakeAlphanumeric;
var
  buf: TBytes;
  seg: TQRSegment;
begin
  (* seg := NewQRSegmentWithAlphanumeric ('', buf);
    itest (integer (seg.mode), integer (qrmALPHANUMERIC));
    itest (seg.numChars, 0);
    itest (seg.bitLength, 0); *)
  SetLength(buf, 1);
  seg := NewQRSegmentWithAlphanumeric('A', buf);
  itest(seg.numChars, 1);
  itest(seg.bitLength, 6);
  btest(seg.data[0], $28);
  SetLength(buf, 2);
  seg := NewQRSegmentWithAlphanumeric('%:', buf);
  itest(seg.numChars, 2);
  itest(seg.bitLength, 11);
  btest(seg.data[0], $DB);
  btest(seg.data[1], $40);
  SetLength(buf, 3);
  seg := NewQRSegmentWithAlphanumeric('Q R', buf);
  itest(seg.numChars, 3);
  itest(seg.bitLength, 17);
  btest(seg.data[0], $96);
  btest(seg.data[1], $CD);
  btest(seg.data[2], $80);
end;

procedure testMakeEci;
var
  seg: TQRSegment;
  buf: TBytes;
begin
  SetLength(buf, 1);
  seg := NewQRSegmentWithECI(127, buf);
  itest(integer(seg.mode), integer(qrmECI));
  itest(seg.numChars, 0);
  itest(seg.bitLength, 8);
  btest(seg.data[0], $7F);
  SetLength(buf, 2);
  seg := NewQRSegmentWithECI(10345, buf);
  itest(seg.numChars, 0);
  itest(seg.bitLength, 16);
  btest(seg.data[0], $A8);
  btest(seg.data[1], $69);
  SetLength(buf, 3);
  seg := NewQRSegmentWithECI(999999, buf);
  itest(seg.numChars, 0);
  itest(seg.bitLength, 24);
  btest(seg.data[0], $CF);
  btest(seg.data[1], $42);
  btest(seg.data[2], $3F);
end;

procedure testGetTotalBits; // this has issues
var
  segs: TQRSegments;
begin
  itest(getTotalBits(nil, 1), 0);
  itest(getTotalBits(nil, 40), 0);
  Log('First');
  setLength(segs, 1);
  segs[0].mode := qrmBYTE;
  segs[0].numChars := 3;
  segs[0].data := nil;
  segs[0].bitLength := 24;
  itest(getTotalBits(segs, 2), 36);
  itest(getTotalBits(segs, 10), 44);
  itest(getTotalBits(segs, 39), 44);
  Log('Second');
  setLength(segs, 4);
  segs[0].mode := qrmECI;
  segs[0].numChars := 0;
  segs[0].data := nil;
  segs[0].bitLength := 8;
  segs[1].mode := qrmNUMERIC;
  segs[1].numChars := 7;
  segs[1].data := nil;
  segs[1].bitLength := 24;
  segs[2].mode := qrmALPHANUMERIC;
  segs[2].numChars := 1;
  segs[2].data := nil;
  segs[2].bitLength := 6;
  segs[3].mode := qrmKANJI;
  segs[3].numChars := 4;
  segs[3].data := nil;
  segs[3].bitLength := 52;
  itest(getTotalBits(segs, 9), 133);
  itest(getTotalBits(segs, 21), 139);
  itest(getTotalBits(segs, 27), 145);
  Log('Third');
  setLength(segs, 1);
  segs[0].mode := qrmBYTE;
  segs[0].numChars := 4093;
  segs[0].data := nil;
  segs[0].bitLength := 32744;
  itest(getTotalBits(segs, 1), -1);
  itest(getTotalBits(segs, 10), 32764);
  itest(getTotalBits(segs, 27), 32764);
  Log('Forth');
  setLength(segs, 5);
  segs[0].mode := qrmNUMERIC;
  segs[0].numChars := 2047;
  segs[0].data := nil;
  segs[0].bitLength := 6824;
  segs[1].mode := qrmNUMERIC;
  segs[1].numChars := 2047;
  segs[1].data := nil;
  segs[1].bitLength := 6824;
  segs[2].mode := qrmNUMERIC;
  segs[2].numChars := 2047;
  segs[2].data := nil;
  segs[2].bitLength := 6824;
  segs[3].mode := qrmNUMERIC;
  segs[3].numChars := 2047;
  segs[3].data := nil;
  segs[3].bitLength := 6824;
  segs[4].mode := qrmNUMERIC;
  segs[4].numChars := 1617;
  segs[4].data := nil;
  segs[4].bitLength := 5390;
  itest(getTotalBits(segs, 1), -1);
  itest(getTotalBits(segs, 10), 32766);
  itest(getTotalBits(segs, 27), -1);
  Log('Fifth');
  SetLength(segs, 10);
  segs[0].mode := qrmKANJI;
  segs[0].numChars := 255;
  segs[0].data := nil;
  segs[0].bitLength := 3315;
  segs[1].mode := qrmKANJI;
  segs[1].numChars := 255;
  segs[1].data := nil;
  segs[1].bitLength := 3315;
  segs[2].mode := qrmKANJI;
  segs[2].numChars := 255;
  segs[2].data := nil;
  segs[2].bitLength := 3315;
  segs[3].mode := qrmKANJI;
  segs[3].numChars := 255;
  segs[3].data := nil;
  segs[3].bitLength := 3315;
  segs[4].mode := qrmKANJI;
  segs[4].numChars := 255;
  segs[4].data := nil;
  segs[4].bitLength := 3315;
  segs[5].mode := qrmKANJI;
  segs[5].numChars := 255;
  segs[5].data := nil;
  segs[5].bitLength := 3315;
  segs[6].mode := qrmKANJI;
  segs[6].numChars := 255;
  segs[6].data := nil;
  segs[6].bitLength := 3315;
  segs[7].mode := qrmKANJI;
  segs[7].numChars := 255;
  segs[7].data := nil;
  segs[7].bitLength := 3315;
  segs[8].mode := qrmKANJI;
  segs[8].numChars := 255;
  segs[8].data := nil;
  segs[8].bitLength := 3315;
  segs[9].mode := qrmALPHANUMERIC;
  segs[9].numChars := 511;
  segs[9].data := nil;
  segs[9].bitLength := 2811;
  itest(getTotalBits(segs, 9), 32767);
  itest(getTotalBits(segs, 26), -1);
  itest(getTotalBits(segs, 40), -1);
end;

initialization
  LogProc := nil;

{$ENDIF}
{$ENDREGION}

end.
