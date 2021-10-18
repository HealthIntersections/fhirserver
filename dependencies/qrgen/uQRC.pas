unit uQRC;

{$ifdef fpc}
{$mode delphi}
{$H+}
{$endif}
{$pointermath on}

(*
  This is a pascal translation of :-

 * QR Code generator library (C)
 *
 * Copyright (c) Project Nayuki. (MIT License)
 * https://www.nayuki.io/page/qr-code-generator-library
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 * - The above copyright notice and this permission notice shall be included in
 *   all copies or substantial portions of the Software.
 * - The Software is provided "as is", without warranty of any kind, express or
 *   implied, including but not limited to the warranties of merchantability,
 *   fitness for a particular purpose and noninfringement. In no event shall the
 *   authors or copyright holders be liable for any claim, damages or other
 *   liability, whether in an action of contract, tort or otherwise, arising from,
 *   out of or in connection with the Software or the use or other dealings in the
 *   Software.
 *)

interface

// The error correction level in a QR Code symbol.
uses SysUtils;

type
  TMonProc = procedure (s : string);
  qrcodegen_Ecc =
  (
    qrcodegen_Ecc_LOW       = 0,   // The QR Code can tolerate about  7% erroneous codewords
    qrcodegen_Ecc_MEDIUM    = 1,   // The QR Code can tolerate about 15% erroneous codewords
    qrcodegen_Ecc_QUARTILE  = 2,   // The QR Code can tolerate about 25% erroneous codewords
    qrcodegen_Ecc_HIGH      = 3    // The QR Code can tolerate about 30% erroneous codewords
  );
// The mask pattern used in a QR Code symbol.
  qrcodegen_Mask =
  (
    // A special value to tell the QR Code encoder to
    // automatically select an appropriate mask pattern
    qrcodegen_Mask_AUTO = -1,
    // The eight actual mask patterns
    qrcodegen_Mask_0 = 0,
    qrcodegen_Mask_1 = 1,
    qrcodegen_Mask_2 = 2,
    qrcodegen_Mask_3 = 3,
    qrcodegen_Mask_4 = 4,
    qrcodegen_Mask_5 = 5,
    qrcodegen_Mask_6 = 6,
    qrcodegen_Mask_7 = 7
  );
// Describes how a segment's data bits are interpreted.
  qrcodegen_Mode =
  (
	  qrcodegen_Mode_NUMERIC      = $01,
	  qrcodegen_Mode_ALPHANUMERIC = $02,
	  qrcodegen_Mode_BYTE         = $04,
	  qrcodegen_Mode_KANJI        = $08,
	  qrcodegen_Mode_ECI          = $07
  );
(*
 * A segment of character/binary/control data in a QR Code symbol.
 * The mid-level way to create a segment is to take the payload data
 * and call a factory function such as qrcodegen_makeNumeric().
 * The low-level way to create a segment is to custom-make the bit buffer
 * and initialize a qrcodegen_Segment struct with appropriate values.
 * Even in the most favorable conditions, a QR Code can only hold 7089 characters of data.
 * Any segment longer than this is meaningless for the purpose of generating QR Codes.
 * Moreover, the maximum allowed bit length is 32767 because
 * the largest QR Code (version 40) has 31329 modules.
 *)
  qrcodegen_Segment = record
    // The mode indicator of this segment.
	  mode : qrcodegen_Mode;
    // The length of this segment's unencoded data. Measured in characters for
    // numeric/alphanumeric/kanji mode, bytes for byte mode, and 0 for ECI mode.
    // Always zero or positive. Not the same as the data's bit length.
	  numChars : integer;
    // The data bits of this segment, packed in bitwise big endian.
    // Can be null if the bit length is zero.
    data : TBytes;
    // The number of valid data bits used in the buffer. Requires
    // 0 <= bitLength <= 32767, and bitLength <= (capacity of data array) * 8.
    // The character count (numChars) must agree with the mode and the bit buffer length.
	  bitLength : integer;
  end;
  TSegments = TArray<qrcodegen_Segment>;

  T7Bytes = array [0..6] of byte;
  T7Int = array [0..6] of integer;
(*---- Macro constants and functions ----*)
const
  qrcodegen_VERSION_MIN   = 1;    // The minimum version number supported in the QR Code Model 2 standard
  qrcodegen_VERSION_MAX   = 40;   // The maximum version number supported in the QR Code Model 2 standard
  // Calculates the number of bytes needed to store any QR Code up to and including the given version number,
  // as a compile-time constant. For example, 'uint8_t buffer[qrcodegen_BUFFER_LEN_FOR_VERSION(25)];'
  // can store any single QR Code from version 1 to 25 (inclusive). The result fits in an int (or int16).
  // Requires qrcodegen_VERSION_MIN <= n <= qrcodegen_VERSION_MAX.
  // #define qrcodegen_BUFFER_LEN_FOR_VERSION(n)  ((((n) * 4 + 17) * ((n) * 4 + 17) + 7) / 8 + 1)
  // The worst-case number of bytes needed to store one QR Code, up to and including
  // version 40. This value equals 3918, which is just under 4 kilobytes.
  // Use this more convenient value to avoid calculating tighter memory bounds for buffers.
  qrcodegen_BUFFER_LEN_MAX = ((((qrcodegen_VERSION_MAX) * 4 + 17) * ((qrcodegen_VERSION_MAX) * 4 + 17) + 7) div 8 + 1);
  // qrcodegen_BUFFER_LEN_FOR_VERSION(qrcodegen_VERSION_MAX)

  EccNames : array [qrcodegen_Ecc] of string = ('LOW', 'MEDIUM', 'QUARTILE', 'HIGH');

const
  INT8_MAX    = $7f;
  INT16_MAX   = $7fff;         // 32767
  INT32_MAX   = $7fffffff;
  INT64_MAX   = $7fffffffffffffff;

  UINT8_MAX   = $ff;
  UINT16_MAX  = $ffff;
  UINT32_MAX  = $ffffffff;
  UINT64_MAX  = $ffffffffffffffff;

  LONG_MAX 	  = 2147483647; // Maximum value for a variable of type long.
  SIZE_MAX    = UINT32_MAX;
  ALPHANUMERIC_CHARSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:";';

  // For generating error correction codes.
  ECC_CODEWORDS_PER_BLOCK : array [0..3, 0..40] of int8 = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1,  7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Low
	(-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),  // Medium
	(-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Quartile
	(-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30));   // High
  qrcodegen_REED_SOLOMON_DEGREE_MAX = 30;  // Based on the table above
  // For generating error correction codes.
  NUM_ERROR_CORRECTION_BLOCKS : array [0..3, 0..40] of int8 = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),  // Low
	(-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  // Medium
	(-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),  // Quartile
	(-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81));  // High
  // For automatic mask pattern selection.
  PENALTY_N1 =  3;
  PENALTY_N2 =  3;
  PENALTY_N3 = 40;
  PENALTY_N4 = 10;
(*---- Functions (high level) to generate QR Codes ----*)
(*
 * Encodes the given text string to a QR Code, returning true if encoding succeeded.
 * If the data is too long to fit in any version in the given range
 * at the given ECC level, then false is returned.
 * - The input text must be encoded in UTF-8 and contain no NULs.
 * - The variables ecl and mask must correspond to enum constant values.
 * - Requires 1 <= minVersion <= maxVersion <= 40.
 * - The arrays tempBuffer and qrcode must each have a length of at least
 *   qrcodegen_BUFFER_LEN_FOR_VERSION(maxVersion), and cannot overlap.
 * - After the function returns, tempBuffer contains no useful data.
 * - If successful, the resulting QR Code may use numeric,
 *   alphanumeric, or byte mode to encode the text.
 * - In the most optimistic case, a QR Code at version 40 with low ECC
 *   can hold any UTF-8 string up to 2953 bytes, or any alphanumeric string
 *   up to 4296 characters, or any digit string up to 7089 characters.
 *   These numbers represent the hard upper limit of the QR Code standard.
 * - Please consult the QR Code specification for information on
 *   data capacities per version, ECC level, and text encoding mode.
 *)
function qrcodegen_encodeText (const text : ansistring; var tempBuffer, qrcode : TBytes;
ecl : qrcodegen_Ecc; minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean) : boolean;
(*
 * Encodes the given binary data to a QR Code, returning true if encoding succeeded.
 * If the data is too long to fit in any version in the given range
 * at the given ECC level, then false is returned.
 * - The input array range dataAndTemp[0 : dataLen] should normally be
 *   valid UTF-8 text, but is not required by the QR Code standard.
 * - The variables ecl and mask must correspond to enum constant values.
 * - Requires 1 <= minVersion <= maxVersion <= 40.
 * - The arrays dataAndTemp and qrcode must each have a length of at least
 *   qrcodegen_BUFFER_LEN_FOR_VERSION(maxVersion), and cannot overlap.
 * - After the function returns, the contents of dataAndTemp may have changed,
 *   and does not represent useful data anymore.
 * - If successful, the resulting QR Code will use byte mode to encode the data.
 * - In the most optimistic case, a QR Code at version 40 with low ECC can hold any byte
 *   sequence up to length 2953. This is the hard upper limit of the QR Code standard.
 * - Please consult the QR Code specification for information on
 *   data capacities per version, ECC level, and text encoding mode.
 *)
function qrcodegen_encodeBinary (dataAndTemp : TBytes; dataLen : integer; var qrcode : TBytes;
	ecl : qrcodegen_Ecc; minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean) : boolean;
(*---- Functions (low level) to generate QR Codes ----*/
(*
 * Renders a QR Code representing the given segments at the given error correction level.
 * The smallest possible QR Code version is automatically chosen for the output. Returns true if
 * QR Code creation succeeded, or false if the data is too long to fit in any version. The ECC level
 * of the result may be higher than the ecl argument if it can be done without increasing the version.
 * This function allows the user to create a custom sequence of segments that switches
 * between modes (such as alphanumeric and byte) to encode text in less space.
 * This is a low-level API; the high-level API is qrcodegen_encodeText() and qrcodegen_encodeBinary().
 * To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
 * result in them being clobbered, but the QR Code output will still be correct.
 * But the qrcode array must not overlap tempBuffer or any segment's data buffer.
 *)
function qrcodegen_encodeSegments (const segs : TSegments; {len : integer;}
	ecl : qrcodegen_Ecc; var tempBuffer, qrcode : TBytes) : boolean;
(*
 * Renders a QR Code representing the given segments with the given encoding parameters.
 * Returns true if QR Code creation succeeded, or false if the data is too long to fit in the range of versions.
 * The smallest possible QR Code version within the given range is automatically
 * chosen for the output. Iff boostEcl is true, then the ECC level of the result
 * may be higher than the ecl argument if it can be done without increasing the
 * version. The mask is either between qrcodegen_Mask_0 to 7 to force that mask, or
 * qrcodegen_Mask_AUTO to automatically choose an appropriate mask (which may be slow).
 * This function allows the user to create a custom sequence of segments that switches
 * between modes (such as alphanumeric and byte) to encode text in less space.
 * This is a low-level API; the high-level API is qrcodegen_encodeText() and qrcodegen_encodeBinary().
 * To save memory, the segments' data buffers can alias/overlap tempBuffer, and will
 * result in them being clobbered, but the QR Code output will still be correct.
 * But the qrcode array must not overlap tempBuffer or any segment's data buffer.
 *)
function qrcodegen_encodeSegmentsAdvanced (const segs : TSegments; {len : integer;} var ecl : qrcodegen_Ecc;
	minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean; var tempBuffer, qrcode : TBytes) : boolean;
(*
 * Tests whether the given string can be encoded as a segment in numeric mode.
 * A string is encodable iff each character is in the range 0 to 9.
 *)
function qrcodegen_isNumeric (const text : string) : boolean;
(*
 * Tests whether the given string can be encoded as a segment in alphanumeric mode.
 * A string is encodable iff each character is in the following set: 0 to 9, A to Z
 * (uppercase only), space, dollar, percent, asterisk, plus, hyphen, period, slash, colon.
 *)
function qrcodegen_isAlphanumeric (const text : string) : boolean;
(*
 * Returns the number of bytes (uint8_t) needed for the data buffer of a segment
 * containing the given number of characters using the given mode. Notes:
 * - Returns SIZE_MAX on failure, i.e. numChars > INT16_MAX or
 *   the number of needed bits exceeds INT16_MAX (i.e. 32767).
 * - Otherwise, all valid results are in the range [0, ceil(INT16_MAX / 8)], i.e. at most 4096.
 * - It is okay for the user to allocate more bytes for the buffer than needed.
 * - For byte mode, numChars measures the number of bytes, not Unicode code points.
 * - For ECI mode, numChars must be 0, and the worst-case number of bytes is returned.
 *   An actual ECI segment can have shorter data. For non-ECI modes, the result is exact.
 *)
function qrcodegen_calcSegmentBufferSize (mode : qrcodegen_Mode; numChars : integer) : integer;
(*
 * Returns a segment representing the given binary data encoded in
 * byte mode. All input byte arrays are acceptable. Any text string
 * can be converted to UTF-8 bytes and encoded as a byte mode segment.
 *)
function qrcodegen_makeBytes (const data : TBytes; {len : integer;} var buf : TBytes) : qrcodegen_Segment;
(*
 * Returns a segment representing the given string of decimal digits encoded in numeric mode.
 *)
function qrcodegen_makeNumeric (const digits : string; var buf : TBytes) : qrcodegen_Segment;
(*
 * Returns a segment representing the given text string encoded in alphanumeric mode.
 * The characters allowed are: 0 to 9, A to Z (uppercase only), space,
 * dollar, percent, asterisk, plus, hyphen, period, slash, colon.
 *)
function qrcodegen_makeAlphanumeric (const text : string; var buf : TBytes) : qrcodegen_Segment;
(*
 * Returns a segment representing an Extended Channel Interpretation
 * (ECI) designator with the given assignment value.
 *)
function qrcodegen_makeEci (assignVal : integer; var buf : TBytes) : qrcodegen_Segment;
(*---- Functions to extract raw data from QR Codes ----*)
(*
 * Returns the side length of the given QR Code, assuming that encoding succeeded.
 * The result is in the range [21, 177]. Note that the length of the array buffer
 * is related to the side length - every 'uint8_t qrcode[]' must have length at least
 * qrcodegen_BUFFER_LEN_FOR_VERSION(version), which equals ceil(size^2 / 8 + 1).
 *)
function qrcodegen_getSize (const qrcode : TBytes) : integer;
(*
 * Returns the color of the module (pixel) at the given coordinates, which is false
 * for light or true for dark. The top left corner has the coordinates (x=0, y=0).
 * If the given coordinates are out of bounds, then false (light) is returned.
 *)
function qrcodegen_getModule (const qrcode : TBytes; x, y : integer) : boolean;

// temp visible
procedure reedSolomonComputeDivisor (degree : integer; var res : TBytes);
function reedSolomonMultiply (x, y : byte) : byte;
function qrcodegen_BUFFER_LEN_FOR_VERSION (n : integer) : integer;
procedure initializeFunctionModules (version : integer; var qrcode : TBytes);
procedure setModule (var qrcode : TBytes; x, y : integer; isDark : boolean);
procedure setModuleBounded (var qrcode : TBytes; x, y : integer; isDark : boolean);
function getModule (const qrcode : TBytes; x, y : integer) : boolean;
function getAlignmentPatternPositions (version : integer; var res : T7Bytes) : integer;
procedure reedSolomonComputeRemainder (const data : TBytes; dataLen : integer;
		const generator : TBytes; degree : integer; var res : TBytes); overload;
procedure reedSolomonComputeRemainder (const data : PByte; dataLen : integer;
		const generator : TBytes; degree : integer; var res : PByte); overload;
function getTotalBits (const segs : TSegments; version : integer) : integer;
function getNumDataCodewords (version : integer; ecl :qrcodegen_Ecc) : integer;
function getNumRawDataModules (ver : integer) : integer;
procedure addEccAndInterleave (data : TBytes; version : integer; ecl : qrcodegen_Ecc; var res : TBytes);

procedure WriteToFile(const buffer : TBytes; bitcount : integer; const filename : string); overload;
procedure WriteToFile(const buffer : TBytes; bitcount : integer; prefix : char; filenumber : integer); overload;
procedure WriteToFile(const buffer : TBytes; bitcount : integer; prefix : char; x, y : integer); overload;
procedure ShowQR (qrcode: TBytes);
procedure Mon (s : string);

var
  MonProc : TMonProc = nil;

implementation

uses Dialogs{$IFNDEF FPC}, IOUtils{$ENDIF};

procedure Mon (s : string);
begin
  if Assigned (MonProc) then MonProc (s);
end;

procedure WriteToFile(const buffer : TBytes; bitcount : integer; const filename : string); overload;
var
  bytecount : integer;
  temp : TBytes;
begin
  if bitcount mod 8 <> 0 then
    bytecount := (bitcount div 8) + 1
  else
    bytecount := (bitcount div 8);
  temp := Copy(buffer, 0, bytecount);
  {$IFNDEF FPC}
  TFile.WriteAllBytes(filename, temp);
  {$ENDIF}
end;

procedure WriteToFile(const buffer : TBytes; bitcount : integer; prefix : char; filenumber : integer); overload;
begin
  WriteToFile(buffer, bitcount, Format('%s%d.bin', [prefix, filenumber]));
end;

procedure WriteToFile(const buffer : TBytes; bitcount : integer; prefix : char; x, y : integer); overload;
begin
  WriteToFile(buffer, bitcount, Format('%s%d-%d.bin', [prefix, x, y]));
end;

procedure ShowQR (qrcode: TBytes);
var
  size, x, y, sz : integer;
  s : string;
begin
  size := qrcodegen_getSize (qrcode);
  if size > 20 then sz := 20 else sz := size;

  for y := 0 to sz - 1 do
    begin
      s := '';
      for x := 0 to size - 1 do
        if qrcodegen_getModule (qrcode, x, y) then
          s := s + '#'
        else
          s := s + '-';
      Mon(s);
    end;
  Mon ('QR Code size ' + IntToStr (size));
end;

function Modes (mode : qrcodegen_Mode) : string;
begin
  case mode of
	  qrcodegen_Mode_NUMERIC      : Result := 'NUMERIC';
	  qrcodegen_Mode_ALPHANUMERIC : Result := 'ALPHA';
	  qrcodegen_Mode_BYTE         : Result := 'BYTE';
	  qrcodegen_Mode_KANJI        : Result := 'KANJI';
	  qrcodegen_Mode_ECI          : Result := 'ECI';
    else                          Result := '????';
    end;
end;

// macros xx
function qrcodegen_BUFFER_LEN_FOR_VERSION (n : integer) : integer;
begin
  Result := ((n * 4 + 17) * (n * 4 + 17) + 7) div 8 + 1;
end;

// #define qrcodegen_BUFFER_LEN_FOR_VERSION(n)  ((((n) * 4 + 17) * ((n) * 4 + 17) + 7) / 8 + 1)

// Returns true iff the i'th bit of x is set to 1. Requires x >= 0 and 0 <= i <= 14.    xx
function getBit (x, i : integer) : boolean;
begin
	Result := ((x shr i) and 1) <> 0;
end;


(*static bool getBit(int x, int i) {

	return ((x >> i) & 1) != 0;
} *)

// Gets the module at the given coordinates, which must be in bounds. xx
function getModule (const qrcode : TBytes; x, y : integer) : boolean;
var
  qrsize : integer;
  index : integer;
begin
  Result := false;
  if length (qrcode) = 0 then exit;
  qrsize := qrcode[0];
	assert ((21 <= qrsize) and (qrsize <= 177) and (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize), 'qrsize = ' + IntToStr (qrsize) + ' x = ' + IntToStr (x) + ' y = ' + IntToStr (y));
	index := y * qrsize + x;
	Result := getBit (qrcode[(index shr 3) + 1], index and 7);
end;

(*
testable bool getModule(const uint8_t qrcode[], int x, int y) {
	int qrsize = qrcode[0];
	assert(21 <= qrsize && qrsize <= 177 && 0 <= x && x < qrsize && 0 <= y && y < qrsize);
	int index = y * qrsize + x;
	return getBit(qrcode[(index >> 3) + 1], index & 7);
} *)

// Sets the module at the given coordinates, which must be in bounds.    xx
procedure setModule (var qrcode : TBytes; x, y : integer; isDark : boolean);
var
  qrsize : integer;
  index, bitIndex, byteIndex : integer;
begin
  if length (qrcode) = 0 then exit;
  qrsize := qrcode[0];
	assert((21 <= qrsize) and (qrsize <= 177) and (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize));
	index := (y * qrsize) + x;
	bitIndex := index and 7;
	byteIndex := (index shr 3) + 1;
//  if (x < 0) or (y < 0) then Mon ('ERROR X = Y -------------------------');
	if isDark then
		qrcode[byteIndex] := qrcode[byteIndex] or (1 shl bitIndex)
	else
		qrcode[byteIndex] := qrcode[byteIndex] and ((1 shl bitIndex) xor $FF);
end;

(* testable void setModule(uint8_t qrcode[], int x, int y, bool isDark) {
	int qrsize = qrcode[0];
	assert(21 <= qrsize && qrsize <= 177 && 0 <= x && x < qrsize && 0 <= y && y < qrsize);
	int index = y * qrsize + x;
	int bitIndex = index & 7;
	int byteIndex = (index >> 3) + 1;
	if (isDark)
		qrcode[byteIndex] |= 1 << bitIndex;
	else
		qrcode[byteIndex] &= (1 << bitIndex) ^ 0xFF;
} *)

// XORs the codeword modules in this QR Code with the given mask pattern.
// The function modules must be marked and the codeword bits must be drawn
// before masking. Due to the arithmetic of XOR, calling applyMask() with
// the same mask value a second time will undo the mask. A final well-formed
// QR Code needs exactly one (not zero, two, etc.) mask applied.
procedure applyMask (const functionModules : TBytes; var qrcode : TBytes; mask : qrcodegen_Mask);
var
  qrsize : integer;
  x, y : integer;
  val : boolean;
//  temp : integer;
  invert : boolean;
//  flip : boolean;
begin
	assert ((0 <= ord (mask)) and (ord (mask) <= 7));  // Disallows qrcodegen_Mask_AUTO
	qrsize := qrcodegen_getSize (qrcode);
  invert := false;
	for y := 0 to qrsize - 1 do
    begin
      for x := 0 to qrsize - 1 do
        begin
          if getModule (functionModules, x, y) then continue;
          case integer (mask) of
            0 : invert := (x + y) mod 2 = 0;
            1 : invert := y mod 2 = 0;
            2 : invert := x mod 3 = 0;
            3 : invert := (x + y) mod 3 = 0;
            4 : invert := (x div 3 + y div 2) mod 2 = 0;
            5 : invert := x * y mod 2 + x * y mod 3 = 0;
            6 : invert := (x * y mod 2 + x * y mod 3) mod 2 = 0;
            7 : invert := ((x + y) mod 2 + x * y mod 3) mod 2 = 0;
            else begin
              assert (false);
              exit;
            end;
          end;
          val := getModule (qrcode, x, y);
        //  flip := invert = 0;
          setModule (qrcode, x, y, val xor invert);
        end;
    end;
end;

(* static void applyMask(const uint8_t functionModules[], uint8_t qrcode[], enum qrcodegen_Mask mask) {
	assert(0 <= (int)mask && (int)mask <= 7);  // Disallows qrcodegen_Mask_AUTO
	int qrsize = qrcodegen_getSize(qrcode);
	for (int y = 0; y < qrsize; y++) {
		for (int x = 0; x < qrsize; x++) {
			if (getModule(functionModules, x, y))
				continue;
			bool invert;
			switch ((int)mask) {
				case 0:  invert = (x + y) % 2 == 0;                    break;
				case 1:  invert = y % 2 == 0;                          break;
				case 2:  invert = x % 3 == 0;                          break;
				case 3:  invert = (x + y) % 3 == 0;                    break;
				case 4:  invert = (x / 3 + y / 2) % 2 == 0;            break;
				case 5:  invert = x * y % 2 + x * y % 3 == 0;          break;
				case 6:  invert = (x * y % 2 + x * y % 3) % 2 == 0;    break;
				case 7:  invert = ((x + y) % 2 + x * y % 3) % 2 == 0;  break;
				default:  assert(false);  return;
			}
			bool val = getModule(qrcode, x, y);
			setModule(qrcode, x, y, val ^ invert);
		} x
	} y
}  *)

// Returns the number of data bits needed to represent a segment
// containing the given number of characters using the given mode. Notes:
// - Returns -1 on failure, i.e. numChars > INT16_MAX or
//   the number of needed bits exceeds INT16_MAX (i.e. 32767).
// - Otherwise, all valid results are in the range [0, INT16_MAX].
// - For byte mode, numChars measures the number of bytes, not Unicode code points.
// - For ECI mode, numChars must be 0, and the worst-case number of bits is returned.
//   An actual ECI segment can have shorter data. For non-ECI modes, the result is exact. xx
function calcSegmentBitLength (mode : qrcodegen_Mode; numChars : integer) : integer;
var
  res : integer;
begin
	// All calculations are designed to avoid overflow on all platforms
	if (numChars > INT16_MAX) then
    begin
		  Result := -1;
      exit;
    end;
	res := numChars;
	if mode = qrcodegen_Mode_NUMERIC then
		res := (res * 10 + 2) div 3  // ceil(10/3 * n)
	else if mode = qrcodegen_Mode_ALPHANUMERIC then
		res := (res * 11 + 1) div 2  // ceil(11/2 * n)
	else if mode = qrcodegen_Mode_BYTE then
		res := res * 8
	else if mode = qrcodegen_Mode_KANJI then
		res := res * 13
	else if (mode = qrcodegen_Mode_ECI) and (numChars = 0) then
		res := 3 * 8
	else
    begin
      Result := -1;
      exit; // Invalid argument
    end;
  assert (res >= 0);
	if res > INT16_MAX then
		Result := -1
  else
    Result := res;
end;

// Appends the given number of low-order bits of the given value to the given byte-based
// bit buffer, increasing the bit length. Requires 0 <= numBits <= 16 and val < 2^numBits.   xx
procedure appendBitsToBuffer (val : cardinal; numBits : integer; var buffer : TBytes; var bitLen : integer);
var
  i : integer;
begin
	assert ((0 <= numBits) and (numBits <= 16) and (val shr numBits = 0));
//  Mon ('Append Value ' + IntToStr (val) + ' using ' + IntToStr (numBits) + ' starting from ' + IntToStr (bitLen));
	for i := numBits - 1 downto 0 do
    begin
 		  buffer[bitLen shr 3] := buffer[bitLen shr 3] or ((val shr i) and 1) shl (7 - (bitLen and 7));
      bitLen := bitLen + 1;
    end;
end;

(* testable void appendBitsToBuffer(unsigned int val, int numBits, uint8_t buffer[], int *bitLen) {
	assert(0 <= numBits && numBits <= 16 && (unsigned long)val >> numBits == 0);
	for (int i = numBits - 1; i >= 0; i--, (*bitLen)++)
		buffer[*bitLen >> 3] |= ((val >> i) & 1) << (7 - (*bitLen & 7));
} *)

// Public function - see documentation comment in header file.
function qrcodegen_encodeText (const text : ansistring; var tempBuffer, qrcode : TBytes;
ecl : qrcodegen_Ecc; minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean) : boolean;
var
  textLen : integer;
  seg : TSegments;
  bufLen : integer;
  i : integer;
begin
  SetLength (seg, 1);
  Result := false;
  textLen := length (text);
  if textLen = 0 then
    begin
      Result := qrcodegen_encodeSegmentsAdvanced (nil, ecl, minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode);
      exit;
    end;
  bufLen := qrcodegen_BUFFER_LEN_MAX;
  if qrcodegen_isNumeric (text) then
    begin
      Mon ('This is numeric');
		  if qrcodegen_calcSegmentBufferSize (qrcodegen_Mode_NUMERIC, textLen) > bufLen then
        begin
          SetLength (qrcode, 1);
          qrcode[0] := 0;  // Set size to invalid value for safety
	        exit;
        end;
      seg[0] := qrcodegen_makeNumeric (text, tempBuffer);

      Mon ('Length data in segs[0] is ' + IntToStr (Length (seg[0].data)));

    end

  else if qrcodegen_isAlphanumeric (text) then

    begin

      Mon ('This is alpha numeric');

		  if qrcodegen_calcSegmentBufferSize (qrcodegen_Mode_ALPHANUMERIC, textLen) > bufLen then
        begin
          SetLength (qrcode, 1);
          qrcode[0] := 0;  // Set size to invalid value for safety
	        exit;
        end;
		  seg[0] := qrcodegen_makeAlphanumeric (text, tempBuffer);
    end
  else
    begin
      mON ('This is binary');
		  if textLen > bufLen then
        begin
          SetLength (qrcode, 1);
          qrcode[0] := 0;  // Set size to invalid value for safety
          exit;
        end;
		  for i := 0 to textLen - 1 do
    	  tempBuffer[i] := byte (text[i + 1]);
		  seg[0].mode := qrcodegen_Mode_BYTE;
  		seg[0].bitLength := calcSegmentBitLength (seg[0].mode, textLen);
		  if seg[0].bitLength = -1 then
        begin
          SetLength (qrcode, 1);
          qrcode[0] := 0;  // Set size to invalid value for safety
          exit;
        end;
		  seg[0].numChars := textLen;
      seg[0].data := tempBuffer;
	  end;
 	Result := qrcodegen_encodeSegmentsAdvanced (seg, ecl, minVersion, maxVersion, mask, boostEcl, tempBuffer, qrcode);
end;

// Public function - see documentation comment in header file.
function qrcodegen_encodeBinary (dataAndTemp : TBytes; dataLen : integer; var qrcode : TBytes;
	ecl : qrcodegen_Ecc; minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean) : boolean;
var
  seg : TSegments;
  i : integer;
begin
  Result := false;
  SetLength (seg, 1);
	seg[0].mode := qrcodegen_Mode_BYTE;
	seg[0].bitLength := calcSegmentBitLength (seg[0].mode, dataLen);
	if seg[0].bitLength = -1 then
    begin
		  SetLength (qrcode, 1);
      qrcode[0] := 0;  // Set size to invalid value for safety
		  exit;
    end;
	seg[0].numChars := dataLen;
  SetLength (seg[0].data, length (dataAndTemp));
      for i := low (seg[0].data) to high (seg[0].data) do
        seg[0].data[i] := dataAndTemp[i];
	Result := qrcodegen_encodeSegmentsAdvanced (seg, ecl, minVersion, maxVersion, mask, boostEcl, dataAndTemp, qrcode);
end;

function qrcodegen_encodeSegments (const segs : TSegments; {len : integer;}
	ecl : qrcodegen_Ecc; var tempBuffer, qrcode : TBytes) : boolean;
begin
	Result := qrcodegen_encodeSegmentsAdvanced (segs, {len,} ecl,
		qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true, tempBuffer, qrcode);
end;

// Returns the number of data bits that can be stored in a QR Code of the given version number, after
// all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
// The result is in the range [208, 29648]. This could be implemented as a 40-entry lookup table.
function getNumRawDataModules (ver : integer) : integer;
var
  numAlign : integer;
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

// Returns the number of 8-bit codewords that can be used for storing data (not ECC),
// for the given version number and error correction level. The result is in the range [9, 2956].
function getNumDataCodewords (version : integer; ecl :qrcodegen_Ecc) : integer;
var
  v, e : integer;
begin
  v := version;
  e := integer (ecl);
  Result := getNumRawDataModules (v) div 8
		- ECC_CODEWORDS_PER_BLOCK[e][v]
		* NUM_ERROR_CORRECTION_BLOCKS[e][v];
end;
// Returns the bit width of the character count field for a segment in the given mode
// in a QR Code at the given version number. The result is in the range [0, 16].
function numCharCountBits (mode : qrcodegen_Mode; version : integer) : integer;
//var
 // i : integer;
begin
  Result := -1;
  case version of
    1..9 :
      begin
        case mode of
          qrcodegen_Mode_NUMERIC : Result := 10;
          qrcodegen_Mode_ALPHANUMERIC : Result := 9;
          qrcodegen_Mode_BYTE : Result := 8;
          qrcodegen_Mode_KANJI : Result := 8;
          qrcodegen_Mode_ECI : Result := 0;
        end;
      end;
    10..26 :
      begin
        case mode of
          qrcodegen_Mode_NUMERIC : Result := 12;
          qrcodegen_Mode_ALPHANUMERIC : Result := 11;
          qrcodegen_Mode_BYTE : Result := 16;
          qrcodegen_Mode_KANJI : Result := 10;
          qrcodegen_Mode_ECI : Result := 0;
        end;
      end;
    27..40 :
      begin
        case mode of
          qrcodegen_Mode_NUMERIC : Result := 14;
          qrcodegen_Mode_ALPHANUMERIC : Result := 13;
          qrcodegen_Mode_BYTE : Result := 16;
          qrcodegen_Mode_KANJI : Result := 12;
          qrcodegen_Mode_ECI : Result := 0;
        end;
      end;
    end;
//  Mon ('Char Count Bits for ' + Modes (mode) + ' version ' + IntToStr (version) + ' = ' + IntToStr (result));
end;
(*
	i := (version + 7) div 17;
	case mode of
  	qrcodegen_Mode_NUMERIC      :
      begin
        case i of
          0 : Result := 10;
          1 : Result := 12;
          2 : Result := 14;
          end;
      end;
		qrcodegen_Mode_ALPHANUMERIC :
      begin
        case i of
          0 : Result := 9;
          1 : Result := 11;
          2 : Result := 13;
          end;
      end;
		qrcodegen_Mode_BYTE         :
      begin
        case i of
          0 : Result := 8;
          1 : Result := 16;
          2 : Result := 16;
          end;
      end;
		qrcodegen_Mode_KANJI        :
      begin
        case i of
          0 : Result := 8;
          1 : Result := 10;
          2 : Result := 12;
          end;
      end;
		qrcodegen_Mode_ECI          : Result := 0;
		else                          Result := -1; // Dummy value
	  end;
    *)
//  Mon ('Char Count Bits for version ' + IntToStr (version) + ' = ' + IntToStr (result));
(*
After every indicator that selects an encoding mode is a length field that tells
how many characters are encoded in that mode.
The number of bits in the length field depends on the encoding and the symbol version.
Number of bits in a length field (Character Count Indicator) Encoding
Ver.          1–9 	10–26 	27–40
Numeric 	    10 	  12 	    14
Alphanumeric 	9 	  11 	    13
Byte 	        8 	  16 	    16
Kanji 	      8 	  10 	    12
*)
// Calculates the number of bits needed to encode the given segments at the given version.
// Returns a non-negative number if successful. Otherwise returns -1 if a segment has too
// many characters to fit its length field, or the total bits exceeds INT16_MAX.      xx
function getTotalBits (const segs : TSegments; version : integer) : integer;
var
  res : integer;
  i : integer;
  numChars, bitLength, ccbits : integer;
begin
//	assert(segs != NULL || len == 0);
  Result := 0;
  if segs = nil then exit;
  if length (segs) = 0 then exit;
	res := 0;
	for i := low (segs) to high (segs) do
    begin
  //   Mon ('Get Bits for segment ' + IntToStr (i) + ' Chars ' + IntToStr (segs[i].numChars) + ' Bits ' + IntToStr (segs[i].bitLength));
      numChars := segs[i].numChars;
		  bitLength := segs[i].bitLength;
      assert ((0 <= numChars) and (numChars  <= INT16_MAX));
      assert ((0 <= bitLength) and (bitLength <= INT16_MAX));
		  ccbits := numCharCountBits (segs[i].mode, version);
 		  assert ((0 <= ccbits) and (ccbits <= 16));
		  if numChars >= (1 shl ccbits) then
        begin
          Result := -1;
          exit;
        end;
      res := res + 4 + ccbits + bitLength;
 //     Mon ('Adding 4 + ' + IntToStr (ccbits) + ' + ' + IntToStr (bitLength) + ' to Res = ' + IntToStr (res));
      if res > INT16_MAX then
        begin
          Result := -1;
          exit;
        end;
    end;
 // Mon ('Res ' + IntToStr (res));
	assert ((0 <= res) and (res <= INT16_MAX));
	Result := res;
end;
(* testable int getTotalBits(const struct qrcodegen_Segment segs[], size_t len, int version) {
	assert(segs != NULL || len == 0);
	long result = 0;
	for (size_t i = 0; i < len; i++) {
		int numChars  = segs[i].numChars;
		int bitLength = segs[i].bitLength;
		assert(0 <= numChars  && numChars  <= INT16_MAX);
		assert(0 <= bitLength && bitLength <= INT16_MAX);
		int ccbits = numCharCountBits(segs[i].mode, version);
		assert(0 <= ccbits && ccbits <= 16);
		if (numChars >= (1L << ccbits))
			return -1;  // The segment's length doesn't fit the field's bit width
		result += 4L + ccbits + bitLength;
		if (result > INT16_MAX)
			return -1;  // The sum might overflow an int type
	}
	assert(0 <= result && result <= INT16_MAX);
	return (int)result;
} *)
// Appends error correction bytes to each block of the given data array, then interleaves
// bytes from the blocks and stores them in the result array. data[0 : dataLen] contains
// the input data. data[dataLen : rawCodewords] is used as a temporary work area and will
// be clobbered by this function. The final answer is stored in result[0 : rawCodewords].   xx
procedure addEccAndInterleave (data : TBytes; version : integer; ecl : qrcodegen_Ecc; var res : TBytes);
var
  numBlocks, blockEccLen : integer;
  rawCodewords, dataLen : integer;
  numShortBlocks, shortBlockDataLen : integer;
  rsdiv : TBytes;
  i, j, k : integer;
  datlen : integer;
  ecc : PByte;
  dat : PByte;
begin
	// Calculate parameter numbers
  assert ((0 <= ord (ecl)) and (ord (ecl) < 4) and (qrcodegen_VERSION_MIN <= version) and (version <= qrcodegen_VERSION_MAX));
  numBlocks := NUM_ERROR_CORRECTION_BLOCKS[integer (ecl), version];
	blockEccLen := ECC_CODEWORDS_PER_BLOCK[integer (ecl), version];
	rawCodewords := getNumRawDataModules(version) div 8;
	dataLen := getNumDataCodewords(version, ecl);
	numShortBlocks := numBlocks - rawCodewords mod numBlocks;
	shortBlockDataLen := rawCodewords div numBlocks - blockEccLen;
	// Split data into blocks, calculate ECC, and interleave
	// (not concatenate) the bytes into a single sequence
  SetLength (rsdiv, qrcodegen_REED_SOLOMON_DEGREE_MAX);
  reedSolomonComputeDivisor (blockEccLen, rsdiv);
  dat := @data[0];
  Mon ('Data ' + IntToStr (length (data)) + ' res ' + IntToStr (length (res)) +
      ' numBlocks ' + IntToStr (numBlocks) +
      ' datalen ' + IntToStr (dataLen) + ' numShortBlocks ' + IntToStr (numShortBlocks) +
      ' shortBlockDataLen ' + IntToStr (shortBlockDataLen));
  for i := 0 to numBlocks - 1 do
    begin
      if i < numShortBlocks then
        datLen := shortBlockDataLen
      else
        datLen := shortBlockDataLen + 1;
      ecc := @data[dataLen];  // Temporary storage
      reedSolomonComputeRemainder (dat, datlen, rsdiv, blockEccLen, ecc);
      k := i;
		  for j := 0 to datLen - 1 do
        begin
          if j = shortBlockDataLen then k := k - numShortBlocks;
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
(* testable void addEccAndInterleave(uint8_t data[], int version, enum qrcodegen_Ecc ecl, uint8_t result[]) {
	// Calculate parameter numbers
	assert(0 <= (int)ecl && (int)ecl < 4 && qrcodegen_VERSION_MIN <= version && version <= qrcodegen_VERSION_MAX);
	int numBlocks = NUM_ERROR_CORRECTION_BLOCKS[(int)ecl][version];
	int blockEccLen = ECC_CODEWORDS_PER_BLOCK  [(int)ecl][version];
	int rawCodewords = getNumRawDataModules(version) / 8;
	int dataLen = getNumDataCodewords(version, ecl);
	int numShortBlocks = numBlocks - rawCodewords % numBlocks;
	int shortBlockDataLen = rawCodewords / numBlocks - blockEccLen;
	// Split data into blocks, calculate ECC, and interleave
	// (not concatenate) the bytes into a single sequence
	uint8_t rsdiv[qrcodegen_REED_SOLOMON_DEGREE_MAX];
	reedSolomonComputeDivisor(blockEccLen, rsdiv);
	const uint8_t *dat = data;
	for (int i = 0; i < numBlocks; i++) {
		int datLen = shortBlockDataLen + (i < numShortBlocks ? 0 : 1);
		uint8_t *ecc = &data[dataLen];  // Temporary storage
		reedSolomonComputeRemainder(dat, datLen, rsdiv, blockEccLen, ecc);
		for (int j = 0, k = i; j < datLen; j++, k += numBlocks) {  // Copy data
			if (j == shortBlockDataLen)
				k -= numShortBlocks;
			result[k] = dat[j];
		}
		for (int j = 0, k = dataLen + i; j < blockEccLen; j++, k += numBlocks)  // Copy ECC
			result[k] = ecc[j];
		dat += datLen;
	}
} *)
// Draws two copies of the format bits (with its own error correction code) based
// on the given mask and error correction level. This always draws all modules of
// the format bits, unlike drawLightFunctionModules() which might skip dark modules.
procedure drawFormatBits (ecl : qrcodegen_Ecc; mask : qrcodegen_Mask; var qrcode : TBytes);
const
  table : array [0..3] of integer = (1, 0, 3, 2);
var
  data, rem : integer;
  i : integer;
  qrsize : integer;
  bits : integer;
begin
	// Calculate error correction code and pack bits
	assert ((0 <= integer (mask)) and (integer (mask) <= 7));
	data := (table[integer (ecl)] shl 3) or integer (mask);  // errCorrLvl is uint2, mask is uint3
//  Mon ('data is ' + IntToStr (data));
	rem := data;
	for i := 0 to 9 do
  	rem := (rem shl 1) xor ((rem shr 9) * $537);
	bits := (data shl 10 or rem) xor $5412;  // uint15
  assert ((bits shr 15) = 0, 'Bits ' + IntToHex (bits, 8) + ' shifted ' + IntToHex (bits shr 15, 8));
	// Draw first copy
	for i := 0 to 5 do
  	setModule (qrcode, 8, i, getBit (bits, i));
	setModule (qrcode, 8, 7, getBit (bits, 6));
	setModule (qrcode, 8, 8, getBit (bits, 7));
	setModule (qrcode, 7, 8, getBit (bits, 8));
	for i := 9 to 14 do
		setModule (qrcode, 14 - i, 8, getBit (bits, i));
	// Draw second copy
	qrsize := qrcodegen_getSize (qrcode);
	for i := 0 to 7 do
		setModule(qrcode, qrsize - 1 - i, 8, getBit (bits, i));
	for i := 8 to 14 do
		setModule (qrcode, 8, qrsize - 15 + i, getBit (bits, i));
	setModule (qrcode, 8, qrsize - 8, true);  // Always dark
end;
(* static void drawFormatBits(enum qrcodegen_Ecc ecl, enum qrcodegen_Mask mask, uint8_t qrcode[]) {
	// Calculate error correction code and pack bits
	assert(0 <= (int)mask && (int)mask <= 7);
	static const int table[] = {1, 0, 3, 2};
	int data = table[(int)ecl] << 3 | (int)mask;  // errCorrLvl is uint2, mask is uint3
	int rem = data;
	for (int i = 0; i < 10; i++)
		rem = (rem << 1) ^ ((rem >> 9) * 0x537);
	int bits = (data << 10 | rem) ^ 0x5412;  // uint15
	assert(bits >> 15 == 0);
	// Draw first copy
	for (int i = 0; i <= 5; i++)
		setModule(qrcode, 8, i, getBit(bits, i));
	setModule(qrcode, 8, 7, getBit(bits, 6));
	setModule(qrcode, 8, 8, getBit(bits, 7));
	setModule(qrcode, 7, 8, getBit(bits, 8));
	for (int i = 9; i < 15; i++)
		setModule(qrcode, 14 - i, 8, getBit(bits, i));
	// Draw second copy
	int qrsize = qrcodegen_getSize(qrcode);
	for (int i = 0; i < 8; i++)
		setModule(qrcode, qrsize - 1 - i, 8, getBit(bits, i));
	for (int i = 8; i < 15; i++)
		setModule(qrcode, 8, qrsize - 15 + i, getBit(bits, i));
	setModule(qrcode, 8, qrsize - 8, true);  // Always dark
}  *)
// Draws the raw codewords (including data and ECC) onto the given QR Code. This requires the initial state of
// the QR Code to be dark at function modules and light at codeword modules (including unused remainder bits).
procedure drawCodewords (const data : TBytes; dataLen : integer; var qrcode : TBytes);
var
  qrsize : integer;
  i, j, x, y : integer;
  upward : boolean;
  dark : boolean;
  right, vert : integer;
begin
	qrsize := qrcodegen_getSize (qrcode);
  Mon ('Draw Codewords - qrsize = ' + IntToStr (qrsize));
	i := 0;  // Bit index into the data
	// Do the funny zigzag scan
  right := qrsize - 1;
  while right >= 1 do      // Index of right column in each column pair
    begin
      if right = 6 then right := 5;
      for vert := 0 to qrsize - 1 do  // Vertical counter
        begin
          for j := 0 to 1 do
            begin
              x := right - j;  // Actual x coordinate
              upward := ((right + 1) and 2) = 0;
              if upward then
                y := qrsize - 1 - vert
              else
                y := vert;
              if (not getModule (qrcode, x, y)) and (i < dataLen * 8) then
                begin
                  dark := getBit (data[i shr 3], 7 - (i and 7));
                  setModule (qrcode, x, y, dark);
                  i := i + 1;
                end;
              // If this QR Code has any remainder bits (0 to 7), they were assigned as
              // 0/false/light by the constructor and are left unchanged by this method
            end;
        end;
      right := right - 2;
    end;
	assert (i = dataLen * 8);
end;
(* static void drawCodewords(const uint8_t data[], int dataLen, uint8_t qrcode[]) {
	int qrsize = qrcodegen_getSize(qrcode);
	int i = 0;  // Bit index into the data
	// Do the funny zigzag scan
	for (int right = qrsize - 1; right >= 1; right -= 2) {  // Index of right column in each column pair
		if (right == 6)
			right = 5;
		for (int vert = 0; vert < qrsize; vert++) {  // Vertical counter
			for (int j = 0; j < 2; j++) {
				int x = right - j;  // Actual x coordinate
				bool upward = ((right + 1) & 2) == 0;
				int y = upward ? qrsize - 1 - vert : vert;  // Actual y coordinate
				if (!getModule(qrcode, x, y) && i < dataLen * 8) {
					bool dark = getBit(data[i >> 3], 7 - (i & 7));
					setModule(qrcode, x, y, dark);
					i++;
				}
				// If this QR Code has any remainder bits (0 to 7), they were assigned as
				// 0/false/light by the constructor and are left unchanged by this method
			}
		}
	}
	assert(i == dataLen * 8);
} *)
// Sets the module at the given coordinates, doing nothing if out of bounds.   xx
procedure setModuleBounded (var qrcode : TBytes; x, y : integer; isDark : boolean);
var
  qrsize : integer;
begin
  if length (qrcode) = 0 then exit;
  qrsize := qrcode[0];
	if (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize) then
		setModule (qrcode, x, y, isDark);
end;
// Calculates and stores an ascending list of positions of alignment patterns
// for this version number, returning the length of the list (in the range [0,7]).
// Each position is in the range [0,177), and are used on both the x and y axes.
// This could be implemented as lookup table of 40 variable-length lists of unsigned bytes.
function getAlignmentPatternPositions (version : integer; var res : T7Bytes) : integer;
var
  numAlign, step : integer;
  i, pos : integer;
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
      res[i] := byte (pos);
      i := i - 1;
      pos := pos - step;
    end;
	res[0] := 6;
	Result := numAlign;
end;
(*  testable int getAlignmentPatternPositions(int version, uint8_t result[7]) {
	if (version == 1)
		return 0;
	int numAlign = version / 7 + 2;
	int step = (version == 32) ? 26 :
		(version * 4 + numAlign * 2 + 1) / (numAlign * 2 - 2) * 2;
	for (int i = numAlign - 1, pos = version * 4 + 10; i >= 1; i--, pos -= step)
		result[i] = (uint8_t)pos;
	result[0] = 6;
	return numAlign;
} *)
// Draws light function modules and possibly some dark modules onto the given QR Code, without changing
// non-function modules. This does not draw the format bits. This requires all function modules to be previously
// marked dark (namely by initializeFunctionModules()), because this may skip redrawing dark function modules.
procedure drawLightFunctionModules (qrcode : TBytes; version : integer);
var
  qrsize : integer;
  i, j, k : integer;
  dist, dx, dy : integer;
  numAlign : integer;
  alignPatPos : T7Bytes;
  rem : integer;
  bits : cardinal; // long
begin
	// Draw horizontal and vertical timing patterns
	qrsize := qrcodegen_getSize (qrcode);
  i := 7;
	while i < qrsize - 7 do
    begin
      setModule (qrcode, 6, i, false);
      setModule (qrcode, i, 6, false);
      i := i + 2;
	  end;
	// Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
	for dy := -4 to 4 do
    begin
      for dx := -4 to 4 do
        begin
          dist := abs (dx);
          if abs (dy) > dist then
            dist := abs (dy);
          if (dist = 2) or (dist = 4) then
            begin
              setModuleBounded (qrcode, 3 + dx, 3 + dy, false);
              setModuleBounded (qrcode, qrsize - 4 + dx, 3 + dy, false);
              setModuleBounded (qrcode, 3 + dx, qrsize - 4 + dy, false);
            end;
        end;
    end;
	// Draw numerous alignment patterns
 	numAlign := getAlignmentPatternPositions (version, alignPatPos);
  Mon ('Num alignemnt pattern positions ' + IntToStr (numAlign));
	for i := 0 to numAlign - 1 do
    begin
      for j := 0 to numAlign - 1 do
        begin
          if ((i = 0) and (j = 0)) or ((i = 0) and (j = numAlign - 1)) or
            ((i = numAlign - 1) and (j = 0)) then continue;  // Don't draw on the three finder corners
          for dy := -1 to 1 do
            begin
              for dx := -1 to 1 do
                setModule (qrcode, alignPatPos[i] + dx, alignPatPos[j] + dy, (dx = 0) and (dy = 0));
            end;
        end;
    end;
	// Draw version blocks
	if version >= 7 then
    begin
      // Calculate error correction code and pack bits
      rem := version;  // version is uint6, in the range [7, 40]
      for i := 0 to 11 do
        rem := (rem shl 1) xor ((rem shr 11) * $1F25);
      bits := (cardinal (version) shl 12) or cardinal (rem);  // uint18
      assert ((bits shr 18) = 0);
      // Draw two copies
      for i := 0 to 5 do
        begin
          for j := 0 to 2 do
            begin
              k := qrsize - 11 + j;
              setModule (qrcode, k, i, (bits and 1) <> 0);
              setModule (qrcode, i, k, (bits and 1) <> 0);
              bits := bits shr 1;
            end;
        end;
    end;
end;
(* static void drawLightFunctionModules(uint8_t qrcode[], int version) {
	// Draw horizontal and vertical timing patterns
	int qrsize = qrcodegen_getSize(qrcode);
	for (int i = 7; i < qrsize - 7; i += 2) {
		setModule(qrcode, 6, i, false);
		setModule(qrcode, i, 6, false);
	}
	// Draw 3 finder patterns (all corners except bottom right; overwrites some timing modules)
	for (int dy = -4; dy <= 4; dy++) {
		for (int dx = -4; dx <= 4; dx++) {
			int dist = abs(dx);
			if (abs(dy) > dist)
				dist = abs(dy);
			if (dist == 2 || dist == 4) {
				setModuleBounded(qrcode, 3 + dx, 3 + dy, false);
				setModuleBounded(qrcode, qrsize - 4 + dx, 3 + dy, false);
				setModuleBounded(qrcode, 3 + dx, qrsize - 4 + dy, false);
			}
		}
	}
	// Draw numerous alignment patterns
	uint8_t alignPatPos[7];
	int numAlign = getAlignmentPatternPositions(version, alignPatPos);
	for (int i = 0; i < numAlign; i++) {
		for (int j = 0; j < numAlign; j++) {
			if ((i == 0 && j == 0) || (i == 0 && j == numAlign - 1) || (i == numAlign - 1 && j == 0))
				continue;  // Don't draw on the three finder corners
			for (int dy = -1; dy <= 1; dy++) {
				for (int dx = -1; dx <= 1; dx++)
					setModule(qrcode, alignPatPos[i] + dx, alignPatPos[j] + dy, dx == 0 && dy == 0);
			}
		}
	}
	// Draw version blocks
	if (version >= 7) {
		// Calculate error correction code and pack bits
		int rem = version;  // version is uint6, in the range [7, 40]
		for (int i = 0; i < 12; i++)
			rem = (rem << 1) ^ ((rem >> 11) * 0x1F25);
		long bits = (long)version << 12 | rem;  // uint18
		assert(bits >> 18 == 0);
		// Draw two copies
		for (int i = 0; i < 6; i++) {
			for (int j = 0; j < 3; j++) {
				int k = qrsize - 11 + j;
				setModule(qrcode, k, i, (bits & 1) != 0);
				setModule(qrcode, i, k, (bits & 1) != 0);
				bits >>= 1;
			}
		}
	}
}  *)
function h (s : T7Int) : string;
var
  i : integer;
begin
  Result := '';
  for i := low (s) to high (s) do
    Result := Result + ' ' + IntToHex (s[i], 2);
end;
// Pushes the given value to the front and drops the last value. A helper function for getPenaltyScore(). xx
procedure finderPenaltyAddHistory (currentRunLength : integer; var runHistory : T7Int; qrsize : integer);
var
  i : integer;
begin
	if runHistory[0] = 0 then
		currentRunLength := currentRunLength + qrsize;  // Add light border to initial run
 // Mon ('Pre History ' + h (runHistory));
  for i := 5 downto 0 do
    runHistory[i + 1] := runHistory[i];
 // Mon ('Post History ' + h (runHistory));
 	runHistory[0] := currentRunLength;
end;
// Can only be called immediately after a light run is added, and
// returns either 0, 1, or 2. A helper function for getPenaltyScore().
function finderPenaltyCountPatterns (const runHistory : T7Int; qrsize : integer) : integer;
var
  n : integer;
  core : boolean;
  a, b : boolean;
begin
	n := runHistory[1];
 //	assert(n <= qrsize * 3);
	core := (n > 0) and (runHistory[2] = n) and (runHistory[3] = n * 3) and
        (runHistory[4] = n) and (runHistory[5] = n);
	// The maximum QR Code size is 177, hence the dark run length n <= 177.
	// Arithmetic is promoted to int, so n*4 will not overflow.
  if runHistory[6] >= n then a := true else a := false;
  if runHistory[0] >= n then b := true else b := false;
	Result := 0;
  if core and (runHistory[0] >= n * 4) and a then Result := Result + 1;
  if core and (runHistory[6] >= n * 4) and b then Result := Result + 1;
 //Mon ('finderpenalty count patterns ' + IntToStr (Result));
end;
(*
static int finderPenaltyCountPatterns(const int runHistory[7], int qrsize) {
	int n = runHistory[1];
	assert(n <= qrsize * 3);
	bool core = n > 0 && runHistory[2] == n && runHistory[3] == n * 3 && runHistory[4] == n && runHistory[5] == n;
	// The maximum QR Code size is 177, hence the dark run length n <= 177.
	// Arithmetic is promoted to int, so n*4 will not overflow.
	return (core && runHistory[0] >= n * 4 && runHistory[6] >= n ? 1 : 0)
	     + (core && runHistory[6] >= n * 4 && runHistory[0] >= n ? 1 : 0);
}*)
// Must be called at the end of a line (row or column) of modules. A helper function for getPenaltyScore().
function finderPenaltyTerminateAndCount (currentRunColor : boolean; var currentRunLength : integer; var runHistory : T7Int; qrsize : integer) : integer;
begin
	if currentRunColor then  // Terminate dark run
    begin
      finderPenaltyAddHistory (currentRunLength, runHistory, qrsize);
      currentRunLength := 0;
    end;
	currentRunLength := currentRunLength + qrsize;  // Add light border to final run
	finderPenaltyAddHistory (currentRunLength, runHistory, qrsize);
	Result := finderPenaltyCountPatterns (runHistory, qrsize);
end;
(* static int finderPenaltyTerminateAndCount(bool currentRunColor, int currentRunLength, int runHistory[7], int qrsize) {
	if (currentRunColor) {  // Terminate dark run
		finderPenaltyAddHistory(currentRunLength, runHistory, qrsize);
		currentRunLength = 0;
	}
	currentRunLength += qrsize;  // Add light border to final run
	finderPenaltyAddHistory(currentRunLength, runHistory, qrsize);
	return finderPenaltyCountPatterns(runHistory, qrsize);
}*)
// Calculates and returns the penalty score based on state of the given QR Code's current modules.
// This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
function getPenaltyScore (const qrcode : TBytes) : cardinal;
var
  qrsize : integer;
  res : cardinal;
  x, y, k : integer;
  x1 : integer;
  runColor : boolean;
  runX, runY : integer;
  runHistory : T7Int;
  color : boolean;
  total : integer;
  dark : integer;
  s : string;
begin
	qrsize := qrcodegen_getSize (qrcode);
 // Mon('qrcode size is ' + Inttostr (qrsize));
 // showqr (qrcode);
	res := 0;
	// Adjacent modules in row having same color, and finder-like patterns
	for y := 0 to qrsize - 1 do
    begin
      runColor := false;
      runX := 0;
      runHistory[0] := 0;
      s := '';
      for x := 0 to qrsize - 1 do
        begin
			    if (getModule (qrcode, x, y) = runColor) then
            begin
              runX := runX + 1;
              if runX = 5 then
                res := res + PENALTY_N1
              else if runX > 5 then
                res := res + 1;
			      end
          else
            begin
              s := s + ' [' + IntToStr (runX) + '] ';
              finderPenaltyAddHistory (runX, runHistory, qrsize);
              if not runColor then
                begin
                  x1 := finderPenaltyCountPatterns (runHistory, qrsize);
                  s := s + ' ' + IntToStr (x1);
                  if (x1 <> 0) then s := s + ' ' + IntTostr(x) + ',' + IntTostr (y) + ' ';
                  res := res + cardinal (x1 * PENALTY_N3);
                end;
              runColor := getModule (qrcode, x, y);
              runX := 1;
			      end;
		    end;
      Mon (s);
      s := '';
		  res := res + cardinal (finderPenaltyTerminateAndCount (runColor, runX, runHistory, qrsize) * PENALTY_N3);
	  end;
  Mon ('Mid point res ' + Inttostr (res));

	// Adjacent modules in column having same color, and finder-like patterns
	for x := 0 to qrsize - 1 do
    begin
      runColor := false;
      runY := 0;
      runHistory[0] := 0;
      for y := 0 to qrsize - 1 do
        begin
          if getModule (qrcode, x, y) = runColor then
            begin
              runY := runY + 1;
              if runY = 5 then
                res := res + PENALTY_N1
              else if runY > 5 then
                res := res + 1;
            end
          else
            begin
              finderPenaltyAddHistory (runY, runHistory, qrsize);
              if not runColor then
                res := res + cardinal (finderPenaltyCountPatterns (runHistory, qrsize) * PENALTY_N3);
              runColor := getModule (qrcode, x, y);
              runY := 1;
			      end;
		    end;
      res := res + cardinal (finderPenaltyTerminateAndCount (runColor, runY, runHistory, qrsize) * PENALTY_N3);
	  end;
    Mon ('Mid point 2 res ' + Inttostr (res));
	// 2*2 blocks of modules having same color
	for y := 0 to qrsize - 2 do
    begin
		  for x := 0 to qrsize - 2 do
        begin
          color := getModule (qrcode, x, y);
          if (color = getModule(qrcode, x + 1, y)) and
             (color = getModule(qrcode, x, y + 1)) and
             (color = getModule(qrcode, x + 1, y + 1)) then
                res := res + PENALTY_N2;
        end;
	  end;
	// Balance of dark and light modules
	dark := 0;
	for y := 0 to qrsize - 1 do
    begin
      for x := 0 to qrsize - 1 do
        begin
          if getModule (qrcode, x, y) then dark := dark + 1;
        end;
	  end;
	total := qrsize * qrsize;  // Note that size is odd, so dark/total != 1/2
	// Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
  k := ((abs (dark * 20 - total * 10) + total - 1) div total) - 1;
	res := res + cardinal (k * PENALTY_N4);
  Mon ('End point res ' + Inttostr (res));
	Result := res;
end;
(*static long getPenaltyScore(const uint8_t qrcode[]) {
	int qrsize = qrcodegen_getSize(qrcode);
	long result = 0;
	// Adjacent modules in row having same color, and finder-like patterns
	for (int y = 0; y < qrsize; y++) {
		bool runColor = false;
		int runX = 0;
		int runHistory[7] = {0};
		for (int x = 0; x < qrsize; x++) {
			if (getModule(qrcode, x, y) == runColor) {
				runX++;
				if (runX == 5)
					result += PENALTY_N1;
				else if (runX > 5)
					result++;
			} else {
				finderPenaltyAddHistory(runX, runHistory, qrsize);
				if (!runColor)
					result += finderPenaltyCountPatterns(runHistory, qrsize) * PENALTY_N3;
				runColor = getModule(qrcode, x, y);
				runX = 1;
			}
		}
		result += finderPenaltyTerminateAndCount(runColor, runX, runHistory, qrsize) * PENALTY_N3;
	}
	// Adjacent modules in column having same color, and finder-like patterns
	for (int x = 0; x < qrsize; x++) {
		bool runColor = false;
		int runY = 0;
		int runHistory[7] = {0};
		for (int y = 0; y < qrsize; y++) {
			if (getModule(qrcode, x, y) == runColor) {
				runY++;
				if (runY == 5)
					result += PENALTY_N1;
				else if (runY > 5)
					result++;
			} else {
				finderPenaltyAddHistory(runY, runHistory, qrsize);
				if (!runColor)
					result += finderPenaltyCountPatterns(runHistory, qrsize) * PENALTY_N3;
				runColor = getModule(qrcode, x, y);
				runY = 1;
			}
		}
		result += finderPenaltyTerminateAndCount(runColor, runY, runHistory, qrsize) * PENALTY_N3;
	}
	// 2*2 blocks of modules having same color
	for (int y = 0; y < qrsize - 1; y++) {
		for (int x = 0; x < qrsize - 1; x++) {
			bool  color = getModule(qrcode, x, y);
			if (  color == getModule(qrcode, x + 1, y) &&
			      color == getModule(qrcode, x, y + 1) &&
			      color == getModule(qrcode, x + 1, y + 1))
				result += PENALTY_N2;
		}
	}
	// Balance of dark and light modules
	int dark = 0;
	for (int y = 0; y < qrsize; y++) {
		for (int x = 0; x < qrsize; x++) {
			if (getModule(qrcode, x, y))
				dark++;
		}
	}
	int total = qrsize * qrsize;  // Note that size is odd, so dark/total != 1/2
	// Compute the smallest integer k >= 0 such that (45-5k)% <= dark/total <= (55+5k)%
	int k = (int)((labs(dark * 20L - total * 10L) + total - 1) / total) - 1;
	result += k * PENALTY_N4;
	return result;
}
^)
(* long int labs(long int x) returns the absolute value of x. *)
// Sets every pixel in the range [left : left + width] * [top : top + height] to dark. xx
procedure fillRectangle (left, top, width, height : integer; qrcode : TBytes);
var
  dx, dy : integer;
begin
	for dy := 0 to height - 1 do
    begin
		  for dx := 0 to width - 1 do
        setModule (qrcode, left + dx, top + dy, true);
	  end;
end;
// Clears the given QR Code grid with light modules for the given
// version's size, then marks every function module as dark.
procedure initializeFunctionModules (version : integer; var qrcode : TBytes);
var
  qrsize : integer;
  alignPatPos : T7Bytes;
  numAlign : integer;
  i, j : integer;
  s : string;
begin
	// Initialize QR Code
	qrsize := version * 4 + 17;
  for i := low (qrcode) to high (qrcode) do qrcode[i] := 0;     // here ----
	qrcode[0] := qrsize;
	// Fill horizontal and vertical timing patterns
	fillRectangle (6, 0, 1, qrsize, qrcode);
	fillRectangle (0, 6, qrsize, 1, qrcode);
	// Fill 3 finder patterns (all corners except bottom right) and format bits
	fillRectangle (0, 0, 9, 9, qrcode);
	fillRectangle (qrsize - 8, 0, 8, 9, qrcode);
	fillRectangle (0, qrsize - 8, 9, 8, qrcode);
	// Fill numerous alignment patterns
  for i := low (alignPatPos) to high (alignPatPos) do alignPatPos[i] := $00;
	numAlign := getAlignmentPatternPositions (version, alignPatPos);
  s := '';
  for i := low (alignPatPos) to high (alignPatPos) do
    s := s + ' ' + IntToHex (alignPatPos[i], 2);
  Mon ('numAlign ' + Inttostr (numAlign) + ' Vals ' + s);
	for i := 0 to numAlign - 1 do
    begin
      for j := 0 to numAlign - 1 do
        begin
          // Don't draw on the three finder corners
   		//	if (!(     (i == 0 && j == 0)     || (i == 0 && j == numAlign - 1)     || (i == numAlign - 1 && j == 0)))
          if not (((i = 0) and (j = 0)) or ((i = 0) and (j = numAlign - 1)) or ((i = numAlign - 1) and (j = 0))) then
            fillRectangle (alignPatPos[i] - 2, alignPatPos[j] - 2, 5, 5, qrcode);
        end;
    end;
	// Fill version blocks
	if version >= 7 then
    begin
      fillRectangle (qrsize - 11, 0, 3, 6, qrcode);
      fillRectangle (0, qrsize - 11, 6, 3, qrcode);
	  end;
end;
// xx
function qrcodegen_encodeSegmentsAdvanced (const segs : TSegments; var ecl : qrcodegen_Ecc;
	minVersion, maxVersion : integer; mask : qrcodegen_Mask; boostEcl : boolean; var tempBuffer, qrcode : TBytes) : boolean;
var
  version : integer;
  i, j : integer;
  dataUsedBits : integer;
  dataCapacityBits, terminatorBits : integer;
  bitLen, bit : integer;
  padByte : byte;
  minPenalty, penalty : cardinal;
  msk : qrcodegen_Mask;
  c : integer;
begin
  Result := false;
  if segs = nil then exit;
  if length (segs) = 0 then exit;
  Mon ('Encode ' + IntToStr (length (segs)) + ' Segments Advanced.');
 	assert ((qrcodegen_VERSION_MIN <= minVersion) and (minVersion <= maxVersion) and (maxVersion <= qrcodegen_VERSION_MAX));
	assert ((0 <= ord (ecl)) and (ord (ecl) <= 3) and (-1 <= integer (mask)) and (integer (mask) <= 7));
  dataUsedBits := -1;  // stop compile warning
	// Find the minimal version number to use
//  minVersion := 8;
  for version := minVersion to maxVersion do
    begin
		  dataCapacityBits := getNumDataCodewords (version, ecl) * 8;  // Number of data bits available
		  dataUsedBits := getTotalBits (segs, {length (segs),} version);
      if (dataUsedBits <> -1) and (dataUsedBits <= dataCapacityBits) then begin
        Beep;
			  break;  // This version number is found to be suitable
      end;
     	if version = maxVersion then
        begin
          SetLength (qrcode, 1);
          qrcode[0] := 0;
          Result := false;
          exit;
        end;
    end;
  Mon ('Version Chosen ' + IntToStr (version));
	assert (dataUsedBits <> -1);
	// Increase the error correction level while the data still fits in the current version number
	for i := integer (qrcodegen_Ecc_MEDIUM) to integer (qrcodegen_Ecc_HIGH) do  // From low to high
		if (boostEcl) and (dataUsedBits <= getNumDataCodewords (version, qrcodegen_Ecc (i)) * 8) then
			ecl := qrcodegen_Ecc (i);
  Mon ('Ecl Chosen ' + EccNames[ecl]);
	// Concatenate all segments to create the data bit string
  SetLength (qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION (version));
//  WriteToFile(qrcode, Length(qrcode), 'A', 1);
  Mon ('QR Code length now ' + IntToStr (length (qrcode)));
  for i := low (qrcode) to high (qrcode) do qrcode[i] := 0;
  bitLen := 0;
	for i := low (segs) to high (segs) do
    begin
      appendBitsToBuffer (cardinal (segs[i].mode), 4, qrcode, bitLen);
//      WriteToFile(qrcode, bitLen, 'B', i, 1);
      appendBitsToBuffer (segs[i].numChars, numCharCountBits (segs[i].mode, version), qrcode, bitLen);
//      WriteToFile(qrcode, bitLen, 'B', i, 2);
      Mon ('Segs[' + IntToStr (i) + '].bitLength = ' + IntToStr (segs[i].bitLength));
      for j := 0 to segs[i].bitLength - 1 do
        begin
          if j = 112 then
            Beep;
          bit := (segs[i].data[j shr 3] shr (7 - (j and 7))) and 1;
          appendBitsToBuffer (cardinal(bit), 1, qrcode, bitLen);
//          WriteToFile(qrcode, bitLen, 'C', i, j);
         end;
//      WriteToFile(qrcode, bitLen, 'B', i, 3);
	  end;
  Mon ('Nos Bits ' + IntToStr (Bitlen));
  assert (bitLen = dataUsedBits);

	// Add terminator and pad up to a byte if applicable
	dataCapacityBits := getNumDataCodewords (version, ecl) * 8;
	assert (bitLen <= dataCapacityBits);
	terminatorBits := dataCapacityBits - bitLen;
	if terminatorBits > 4 then terminatorBits := 4;
	appendBitsToBuffer (0, terminatorBits, qrcode, bitLen);
	appendBitsToBuffer (0, (8 - bitLen mod 8) mod 8, qrcode, bitLen);
  assert (bitLen mod 8 = 0);
	// Pad with alternating bytes until data capacity is reached
  padByte := $ec;
  c := 0;
  while bitLen < dataCapacityBits do
    begin
      appendBitsToBuffer (padByte, 8, qrcode, bitLen);
      padByte := padByte xor ($ec xor $11);
      Inc(c);
    end;
  Mon ('Nos Bits ' + IntToStr (Bitlen));

	// Draw function and data codeword modules
	addEccAndInterleave (qrcode, version, ecl, tempBuffer);
	initializeFunctionModules (version, qrcode);       // <-- this clears qrcode
	drawCodewords (tempBuffer, getNumRawDataModules(version) div 8, qrcode);
	drawLightFunctionModules (qrcode, version);
	initializeFunctionModules (version, tempBuffer);

	// Handle masking
	if mask = qrcodegen_Mask_AUTO then
    begin // Automatically choose best mask
      minPenalty := LONG_MAX;
      for i := 0 to 7 do
        begin
          msk := qrcodegen_Mask (i);
          applyMask (tempBuffer, qrcode, msk);
          drawFormatBits (ecl, msk, qrcode);
          penalty := getPenaltyScore (qrcode);
          Mon (format('Penalty %d = %d', [i, penalty]));
          if penalty < minPenalty then
            begin
              mask := msk;
              minPenalty := penalty;
            end;
          applyMask (tempBuffer, qrcode, msk);  // Undoes the mask due to XOR
        end;
	end;
	assert ((0 <= ord (mask)) and (ord (mask) <= 7));
	applyMask (tempBuffer, qrcode, mask);
	drawFormatBits (ecl, mask, qrcode);
	Result := true;
end;

// Public function - see documentation comment in header file. xx
function qrcodegen_isNumeric (const text : string) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 1 to length (text) do
     if Pos (text[i], '0123456789') = 0 then
      exit;
  Result := true;
end;

// Public function - see documentation comment in header file.  xx
function qrcodegen_isAlphanumeric (const text : string) : boolean;
var
  i, x : integer;
begin
  Result := false;
  for i := 1 to length (text) do
    begin
      x :=  Pos (text[i], ALPHANUMERIC_CHARSET);
   //   Mon ('Testing ' + text[i] + ' = ' + IntToStr (x));
      if x = 0 then exit;
    end;
  Result := true;
end;

// Public function - see documentation comment in header file.      xx
function qrcodegen_getSize (const qrcode : TBytes) : integer;
begin
//	assert(qrcode != NULL);
  Result := 0;
  if length (qrcode) = 0 then exit;
	Result := qrcode[0];
  assert ((qrcodegen_VERSION_MIN * 4 + 17 <= result)
	  and (result <= qrcodegen_VERSION_MAX * 4 + 17));
end;
// Public function - see documentation comment in header file.     xx
function qrcodegen_getModule (const qrcode : TBytes; x, y : integer) : boolean;
var
  qrsize : integer;
begin
	assert (qrcode <> nil);
  Result := false;
  if length (qrcode) = 0 then exit;
	qrsize := qrcode[0];
	Result := (0 <= x) and (x < qrsize) and (0 <= y) and (y < qrsize) and getModule (qrcode, x, y);
end;
// Public function - see documentation comment in header file.      xx
function qrcodegen_calcSegmentBufferSize (mode : qrcodegen_Mode; numChars : integer) : integer;
var
  temp : integer;
begin
	temp := calcSegmentBitLength (mode, numChars);
	if temp = -1 then
    begin
      Result := INT32_MAX;
      exit;
    end;
  assert ((0 <= temp) and (temp <= INT16_MAX));
	Result := (temp + 7) div 8;
end;
(* size_t qrcodegen_calcSegmentBufferSize(enum qrcodegen_Mode mode, size_t numChars) {
	int temp = calcSegmentBitLength(mode, numChars);
	if (temp == -1)
		return SIZE_MAX;
	assert(0 <= temp && temp <= INT16_MAX);
	return ((size_t)temp + 7) / 8;
} *)
// Public function - see documentation comment in header file.
function qrcodegen_makeBytes (const data : TBytes; var buf : TBytes) : qrcodegen_Segment;
var
  len : integer;
  i : integer;
begin
  len := length (data);
  assert((data <> nil) or (len = 0));
  mon ('length = ' + inttostr (len));
	Result.mode := qrcodegen_Mode_BYTE;
	Result.bitLength := calcSegmentBitLength (result.mode, len);
	assert (result.bitLength <> -1);
	result.numChars := len;
  if result.numChars > 0 then
    begin
      SetLength (buf, length (data));
      for i := low (data) to high (data) do
        buf[i] := data[i]; // maybe direct assignment or use move
    end
  else
    SetLength (buf, 0);
  SetLength (Result.data, length (buf));
  for i := low (buf) to high (buf) do
    Result.data[i] := buf[i]; // maybe direct assignment or use move
end;

// Public function - see documentation comment in header file.
function qrcodegen_makeNumeric (const digits : string; var buf : TBytes) : qrcodegen_Segment;
var
  accumCount : integer;
  bitLen : integer;
  len : integer;
  accumData : integer;
  i : integer;
begin
	len := length (digits);
	Result.mode := qrcodegen_Mode_NUMERIC;
	bitLen := calcSegmentBitLength (Result.mode, len);
 	assert (bitLen <> -1);
	Result.numChars := integer (len);
  if bitLen > 0 then
    for i := low (buf) to high (buf) do buf[i] := 0;
	Result.bitLength := 0;
 	accumData := 0;
	accumCount := 0;
  for i := 1 to length (digits) do
    begin
      assert (('0' <= digits[i]) and (digits[i] <= '9'));
      accumData := (accumData * 10) + ord (digits[i]) - ord ('0');
      accumCount := accumCount + 1;
      if accumCount = 3 then
        begin
          appendBitsToBuffer (accumData, 10, buf, Result.bitLength);
          accumData := 0;
          accumCount := 0;
        end;
	  end;
	if accumCount > 0 then  // 1 or 2 digits remaining
		appendBitsToBuffer (accumData, accumCount * 3 + 1, buf, Result.bitLength);
	assert (Result.bitLength = bitLen);
  SetLength (Result.data, length (buf));
  for i := low (buf) to high (buf) do
    Result.data[i] := buf[i]; // maybe direct assignment or use move
end;

// Public function - see documentation comment in header file.
function qrcodegen_makeAlphanumeric (const text : string; var buf : TBytes) : qrcodegen_Segment;
var
  bitLen : integer;
  len : integer;
  i, x : integer;
  accumCount : integer;
  accumData : word; // check if this is umsigned int
begin
  len := length (text);
	Result.mode := qrcodegen_Mode_ALPHANUMERIC;
	bitLen := calcSegmentBitLength (Result.mode, len);
  assert (bitLen <> -1);
	Result.numChars := len;
	if bitLen > 0 then
    begin
      for i := low (buf) to high (buf) do buf[i] := 0;
    end;
	Result.bitLength := 0;
	accumData := 0;
	accumCount := 0;
  for i := 1 to len do
    begin
      x := Pos (text[i], ALPHANUMERIC_CHARSET) - 1;
      Mon ('Value for ' + text[i] + ' is ' + IntToStr (x));
      assert (x >= 0);
      accumData := accumData * 45 + x;
      accumCount := accumCount + 1;
      if accumCount = 2 then
        begin
          appendBitsToBuffer (accumData, 11, buf, result.bitLength);
          accumData := 0;
          accumCount := 0;
        end;
	  end;
	if accumCount > 0 then  // 1 character remaining
		appendBitsToBuffer (accumData, 6, buf, result.bitLength);
	assert (result.bitLength = bitLen);
  SetLength (Result.data, length (buf));
  for i := low (buf) to high (buf) do
    Result.data[i] := buf[i]; // maybe direct assignment or use move
end;

(*
Alphanumeric encoding mode stores a message more compactly than the byte mode can, but cannot store lower-case letters and has only a limited selection of punctuation marks, which are sufficient for rudimentary web addresses.
Two characters are coded in an 11-bit value by this formula:

    V = 45 × C1 + C2

This has the exception that the last character in an alphanumeric string with an odd length is read as a 6-bit value instead.

00 	0 	09 	9 	18 	I 	27 	R 	36 	Space
01 	1 	10 	A 	19 	J 	28 	S 	37 	$
02 	2 	11 	B 	20 	K 	29 	T 	38 	%
03 	3 	12 	C 	21 	L 	30 	U 	39 	*
04 	4 	13 	D 	22 	M 	31 	V 	40 	+
05 	5 	14 	E 	23 	N 	32 	W 	41 	–
06 	6 	15 	F 	24 	O 	33 	X 	42 	.
07 	7 	16 	G 	25 	P 	34 	Y 	43 	/
08 	8 	17 	H 	26 	Q 	35 	Z 	44 	:   *)

function qrcodegen_makeEci (assignVal : integer; var buf : TBytes) : qrcodegen_Segment;
var
  i : integer;
begin
  Result.mode := qrcodegen_Mode_ECI;
  Result.numChars := 0;
  Result.bitLength := 0;
  for i := low (buf) to high (buf) do buf[i] := 0;
  if assignVal < 0 then
    begin
      assert (false);   // error
      exit;
    end
  else if assignVal < 1 shl 7 then
    begin
		  appendBitsToBuffer (assignVal, 8, buf, Result.bitLength);
    end
  else if assignVal < 1 shl 14 then
    begin
      appendBitsToBuffer (2, 2, buf, Result.bitLength);
      appendBitsToBuffer (assignVal, 14, buf, Result.bitLength);
    end
	else if (assignVal < 1000000) then
    begin
      appendBitsToBuffer (6, 3, buf, &result.bitLength);
      appendBitsToBuffer (assignVal shr 10, 11, buf, Result.bitLength);
      appendBitsToBuffer (assignVal and $3FF, 10, buf, Result.bitLength);
    end
  else
    begin
      assert (false);
    end;
  SetLength (Result.data, length (buf));
  for i := low (buf) to high (buf) do
    Result.data[i] := buf[i]; // maybe direct assignment or use move
end;

// Returns the product of the two given field elements modulo GF(2^8/0x11D).
// All inputs are valid. This could be implemented as a 256*256 lookup table. xx
function reedSolomonMultiply (x, y : byte) : byte;
var
  z : byte;
  i : integer;
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


(* testable uint8_t reedSolomonMultiply(uint8_t x, uint8_t y) {

	// Russian peasant multiplication
	uint8_t z = 0;
	for (int i = 7; i >= 0; i--) {
		z = (uint8_t)((z << 1) ^ ((z >> 7) * 0x11D));
		z ^= ((y >> i) & 1) * x;
	}
	return z;
} *)
// Computes a Reed-Solomon ECC generator polynomial for the given degree, storing in result[0 : degree].
// This could be implemented as a lookup table over all possible parameter values, instead of as an algorithm.     xx
procedure reedSolomonComputeDivisor (degree : integer; var res : TBytes);
var
  root : byte;
  i, j : integer;
begin
	assert ((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
	// Polynomial coefficients are stored from highest to lowest power, excluding the leading term which is always 1.
	// For example the polynomial x^3 + 255x^2 + 8x + 93 is stored as the uint8 array {255, 8, 93}.
  for i := 0 to degree - 1 do res[i] := 0;
	res[degree - 1] := 1;  // Start off with the monomial x^0
	// Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
	// drop the highest monomial term which is always 1x^degree.
	// Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
	root := 1;
	for i := 0 to degree - 1 do
    begin
      // Multiply the current product by (x - r^i)
      for j := 0 to degree - 1 do
        begin
          res[j] := reedSolomonMultiply (res[j], root);
          if (j + 1 < degree) then res[j] := res[j] xor res[j + 1];
        end;
		  root := reedSolomonMultiply (root, $02);
	  end;
end;
(*testable void reedSolomonComputeDivisor(int degree, uint8_t result[]) {
	assert(1 <= degree && degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX);
	// Polynomial coefficients are stored from highest to lowest power, excluding the leading term which is always 1.
	// For example the polynomial x^3 + 255x^2 + 8x + 93 is stored as the uint8 array {255, 8, 93}.
	memset(result, 0, (size_t)degree * sizeof(result[0]));
	result[degree - 1] = 1;  // Start off with the monomial x^0
	// Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
	// drop the highest monomial term which is always 1x^degree.
	// Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
	uint8_t root = 1;
	for (int i = 0; i < degree; i++) {
		// Multiply the current product by (x - r^i)
		for (int j = 0; j < degree; j++) {
			result[j] = reedSolomonMultiply(result[j], root);
			if (j + 1 < degree)
				result[j] ^= result[j + 1];
		}
		root = reedSolomonMultiply(root, 0x02);
	}
} *)
// Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.
// The remainder when data[0 : dataLen] is divided by divisor[0 : degree] is stored in result[0 : degree].
// All polynomials are in big endian, and the generator has an implicit leading 1 term.    xx
procedure reedSolomonComputeRemainder (const data : TBytes; dataLen : integer;
		const generator : TBytes; degree : integer; var res : TBytes);
var
  i, j, k : integer;
  factor : byte;
begin
  assert ((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
  for i := 0 to degree - 1 do res[i] := 0;
	for i := 0 to dataLen - 1 do  // Polynomial division
    begin
		  factor := data[i] xor res[0];
      for k := 0 to degree - 2 do // could use move
        res[k] := res[k + 1];
	  // reinstate 	memmove(&result[0], &result[1], (size_t)(degree - 1) * sizeof(result[0]));
		  res[degree - 1] := 0;
		  for j := 0 to degree - 1 do
			  res[j] := res[j] xor reedSolomonMultiply (generator[j], factor);
    end;
end;
(*testable void reedSolomonComputeRemainder(const uint8_t data[], int dataLen,
		const uint8_t generator[], int degree, uint8_t result[]) {
	assert(1 <= degree && degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX);
	memset(result, 0, (size_t)degree * sizeof(result[0]));
	for (int i = 0; i < dataLen; i++) {  // Polynomial division
		uint8_t factor = data[i] ^ result[0];
		memmove(&result[0], &result[1], (size_t)(degree - 1) * sizeof(result[0]));
		result[degree - 1] = 0;
		for (int j = 0; j < degree; j++)
			result[j] ^= reedSolomonMultiply(generator[j], factor);
	}
} *)
// Computes the Reed-Solomon error correction codeword for the given data and divisor polynomials.
// The remainder when data[0 : dataLen] is divided by divisor[0 : degree] is stored in result[0 : degree].
// All polynomials are in big endian, and the generator has an implicit leading 1 term. xx
procedure reedSolomonComputeRemainder (const data : PByte; dataLen : integer;
		const generator : TBytes; degree : integer; var res : PByte);
var
  i, j, k : integer;
  factor : byte;
begin
  assert ((1 <= degree) and (degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX));
  for i := 0 to degree - 1 do res[i] := 0;
  for i := 0 to dataLen - 1 do
    begin
      factor := data[i] xor res[0];
      for k := 0 to degree - 2 do // could use move
        res[k] := res[k + 1];
		  res[degree - 1] := 0;
		  for j := 0 to degree - 1 do
			  res[j] := res[j] xor reedSolomonMultiply (generator[j], factor);
    end;
end;
(*testable void reedSolomonComputeRemainder(const uint8_t data[], int dataLen,
		const uint8_t generator[], int degree, uint8_t result[])
	assert(1 <= degree && degree <= qrcodegen_REED_SOLOMON_DEGREE_MAX);
	memset(result, 0, (size_t)degree * sizeof(result[0]));
	for (int i = 0; i < dataLen; i++)   // Polynomial division
		uint8_t factor = data[i] ^ result[0];
		memmove(&result[0], &result[1], (size_t)(degree - 1) * sizeof(result[0]));
		result[degree - 1] = 0;
		for (int j = 0; j < degree; j++)
			result[j] ^= reedSolomonMultiply(generator[j], factor);
 *)
end.
