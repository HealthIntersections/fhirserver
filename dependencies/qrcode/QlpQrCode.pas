unit QlpQrCode;

{$I QRCodeGenLib.inc}

interface

uses
  Math,
  Classes,
  SysUtils,
  QlpIQrCode,
  QlpIQrTemplate,
  QlpQrTemplate,
  QlpIQrSegment,
  QlpQrSegment,
  QlpQrSegmentMode,
  QlpBitBuffer,
  QlpIReedSolomonGenerator,
  QlpReedSolomonGenerator,
  QlpQrCodeCommons,
  QlpGuard,
  QlpBits,
  QlpConverters,
  QlpArrayUtils,
  QlpQRCodeGenLibTypes;

resourcestring
  SValueOutOfRange = 'Value out of range';
  SScaleOrBorderTooLarge = 'Scale or border too large';
  SMaskOutOfRange = 'Mask out of range';
  SInvalidState = 'Invalid state encountered.';
  SInvalidValue = 'Invalid value';
  SSegmentTooLong = 'Segment too long';
  SSegmentSizeError = 'Data length = %d bits, Max capacity = %d bits';
  SFileNameEmpty = 'FileName cannot be empty';
  SBorderNegative = 'Border must be non-negative';
  SEncodingInstanceNil = 'Encoding instance cannot be nil';

type

  /// <summary>
  /// A QR Code symbol, which is a type of two-dimension barcode. Invented by
  /// Denso Wave and described in the ISO/IEC 18004 standard. Instances of
  /// this class represent an immutable square grid of black (default) and white (default) cells.
  /// The class provides static factory functions to create a QR Code from
  /// text or binary data. The class covers the QR Code Model 2
  /// specification, supporting all versions (sizes) from 1 to 40, all 4
  /// error correction levels, and 4 character encoding modes.
  /// </summary>
  TQrCode = class sealed(TInterfacedObject, IQrCode)

  public

    type
{$SCOPEDENUMS ON}
    /// <summary>
    /// The error correction level in a QR Code symbol.
    /// </summary>
    TEcc = (
      /// <summary>
      /// The QR Code can tolerate about 7% erroneous codewords.
      /// </summary>
      eccLow = 1,
      /// <summary>
      /// The QR Code can tolerate about 15% erroneous codewords.
      /// </summary>
      eccMedium = 0,
      /// <summary>
      /// The QR Code can tolerate about 25% erroneous codewords.
      /// </summary>
      eccQuartile = 3,
      /// <summary>
      /// The QR Code can tolerate about 30% erroneous codewords.
      /// </summary>
      eccHigh = 2

      );
{$SCOPEDENUMS OFF}
  strict private
  const
    UNIX_NEW_LINE = Char(#10);
    TAB = Char(#9);

    // For use in GetPenaltyScore(), when evaluating which mask is best.
    PENALTY_N1 = Int32(3);
    PENALTY_N2 = Int32(3);
    PENALTY_N3 = Int32(40);
    PENALTY_N4 = Int32(10);

    /// <summary>
    /// helper static constant array for implementing <c>for .. in</c> loops
    /// for <c>TEcc</c> Enums. This was implemented because I needed to retain
    /// the order the enum was defined while looping through it.
    /// </summary>
    EccOrder: array [0 .. 3] of TEcc = (TEcc.eccLow, TEcc.eccMedium,
      TEcc.eccQuartile, TEcc.eccHigh);

 ECC_CODEWORDS_PER_BLOCK : array[0 .. 3, 0 .. 40] of Int8 = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1,  7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Low
	(-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),  // Medium
	(-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  // Quartile
	(-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)  // High
);

 NUM_ERROR_CORRECTION_BLOCKS : array[0 .. 3, 0 .. 40] of Int8 = (
	// Version: (note that index 0 is for padding, and is set to an illegal value)
	//0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
	(-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),  // Low
	(-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  // Medium
	(-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),  // Quartile
	(-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81)  // High
);

  type
    TEccHelper = record helper for TEcc
    strict private
      function GetFormatBits(): Int32; inline;
      function GetToInt32(): Int32;

    public
      property FormatBits: Int32 read GetFormatBits;
      property ToInt32: Int32 read GetToInt32;
    end;

  var
    FVersion, FSize, FMask: Int32;

    /// <summary>
    /// The error correction level used in this QR Code
    /// </summary>
    FErrorCorrectionLevel: TEcc;
    // Private grid of modules/pixels:
    FModules: TQRCodeGenLibInt32Array;
    FBackgroundColor, FForegroundColor: TQRCodeGenLibColor;

    function GetVersion: Int32; inline;
    function GetSize: Int32; inline;
    function GetMask: Int32; inline;
    function GetModules: TQRCodeGenLibInt32Array; inline;
    function GetBackgroundColor: TQRCodeGenLibColor; inline;
    function GetForegroundColor: TQRCodeGenLibColor; inline;
    procedure SetBackgroundColor(const AColor: TQRCodeGenLibColor); inline;
    procedure SetForegroundColor(const AColor: TQRCodeGenLibColor); inline;

    // Draws two copies of the format bits (with its own error correction code)
    // based on the given mask and this object's error correction level field.
    procedure DrawFormatBits(AMask: Int32);

    /// <summary>
    /// Sets the module at the given coordinates to the given color. <br />
    /// Only used by the constructor. Coordinates must be in bounds.
    /// </summary>
    procedure SetModule(Ax, Ay, ABlack: Int32);

    // Draws the given sequence of 8-bit codewords (data and error correction)
    // onto the entire data area of this QR Code, based on the given bit indexes.
    function AddEccAndInterleave(const AData: TQRCodeGenLibByteArray)
      : TQRCodeGenLibByteArray;

    // Draws the given sequence of 8-bit codewords (data and error correction) onto the entire
    // data area of this QR Code symbol. Function modules need to be marked off before this is called.
    procedure DrawCodeWords(const ADataOutputBitIndexes
      : TQRCodeGenLibInt32Array; const AAllCodewords: TQRCodeGenLibByteArray);

    // XORs the codeword modules in this QR Code with the given mask pattern.
    // The function modules must be marked and the codeword bits must be drawn
    // before masking. Due to the arithmetic of XOR, calling ApplyMask() with
    // the same mask value a second time will undo the mask. A final well-formed
    // QR Code needs exactly one (not zero, two, etc.) mask applied.
    procedure ApplyMask(const AMask: TQRCodeGenLibInt32Array);

    // A messy helper function for the constructor. This QR Code must be in an unmasked state when this
    // method is called. The 'mask' argument is the requested mask, which is -1 for auto or 0 to 7 for fixed.
    // This method applies and returns the actual mask chosen, from 0 to 7.
    function HandleConstructorMasking(const AMasks
      : TQRCodeGenLibMatrixInt32Array; AMask: Int32): Int32;

    // Calculates and returns the penalty score based on state of this QR Code's current modules.
    // This is used by the automatic mask choice algorithm to find the mask pattern that yields the lowest score.
    function GetPenaltyScore(): Int32;

    procedure ValidateImageDimensions(AScale, ABorder: Int32);

{$IFDEF LCL}
    function ToBitmapImageInternalLCL(AScale, ABorder: Int32): TQRCodeGenLibBitmap;
{$ENDIF LCL}
{$IFDEF FCL}
    function ToBitmapImageInternalFCL(AScale, ABorder: Int32): TQRCodeGenLibBitmap;
{$ENDIF FCL}
{$IFDEF VCL}
    function ToBitmapImageInternalVCL(AScale, ABorder: Int32): TQRCodeGenLibBitmap;
{$ENDIF VCL}
{$IFDEF FMX}
    function ToBitmapImageInternalFMX(AScale, ABorder: Int32): TQRCodeGenLibBitmap;
{$ENDIF FMX}
    // Returns the number of 8-bit data (i.e. not error correction) codewords contained in any
    // QR Code of the given version number and error correction level, with remainder bits discarded.
    // This stateless pure function could be implemented as a (40*4)-cell lookup table.
    class function GetNumDataCodeWords(AVersion: Int32; AEcl: TEcc)
      : Int32; inline;

    // Pushes the given value to the front and drops the last value. A helper function for GetPenaltyScore().
    class procedure FinderPenaltyAddHistory(ACurrentRunLength: Int32;
      const ARunHistory: TQRCodeGenLibInt32Array); static; inline;

    // Can only be called immediately after a white run is added, and
    // returns either 0, 1, or 2. A helper function for getPenaltyScore().
    function FinderPenaltyCountPatterns(const ARunHistory
      : TQRCodeGenLibInt32Array): Int32; inline;

    // Must be called at the end of a line (row or column) of modules. A helper function for GetPenaltyScore()
    function FinderPenaltyTerminateAndCount(ACurrentRunColor, ACurrentRunLength
      : Int32; const ARunHistory: TQRCodeGenLibInt32Array): Int32; inline;

  public

    const
    MIN_VERSION = TQrCodeCommons.MIN_VERSION;
    MAX_VERSION = TQrCodeCommons.MAX_VERSION;

    /// <summary>
    /// Returns the color of the module (pixel) at the specified coordinates,
    /// which is either <c>false</c> for white or <c>true</c> for black. The
    /// top left corner has the coordinates (x=0, y=0).If the specified
    /// coordinates are out of bounds, then <c>false</c> (white) is returned.
    /// </summary>
    /// <param name="Ax">
    /// the x coordinate, where 0 is the left edge and FSize - 1 is the right
    /// edge
    /// </param>
    /// <param name="Ay">
    /// the y coordinate, where 0 is the top edge and FSize - 1 is the bottom
    /// edge
    /// </param>
    /// <returns>
    /// the module's color, which is either <c>false</c> (white) or <c>true</c>
    /// (black)
    /// </returns>
    function GetModule(Ax, Ay: Int32): Boolean; inline;

    /// <summary>
    /// Constructs a QR Code with the specified version number, error
    /// correction level, data codeword bytes, and mask number.
    /// </summary>
    /// <param name="AVersion">
    /// the version number to use, which must be in the range 1 to 40
    /// (inclusive)
    /// </param>
    /// <param name="AEcl">
    /// the error correction level to use
    /// </param>
    /// <param name="ADataCodewords">
    /// the bytes representing segments to encode (without ECC)
    /// </param>
    /// <param name="AMask">
    /// the mask pattern to use, which is either −1 for automatic choice or
    /// from 0 to 7 for fixed choice
    /// </param>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the version or mask value is out of range, <br />or if the data is
    /// the wrong length for the specified version and error correction level
    /// </exception>
    constructor Create(AVersion: Int32; AEcl: TEcc;
      const ADataCodewords: TQRCodeGenLibByteArray; AMask: Int32);

    /// <summary>
    /// The version number of this QR Code, which is between 1 and 40
    /// (inclusive). <br />This determines the size of this barcode.
    /// </summary>
    property Version: Int32 read GetVersion;

    /// <summary>
    /// The width and height of this QR Code, measured in modules, between <br />
    /// 21 and 177 (inclusive). This is equal to version &#xD7; 4 + 17.
    /// </summary>
    property Size: Int32 read GetSize;

    /// <summary>
    /// The index of the mask pattern used in this QR Code, which is between 0 and 7 (inclusive).
    /// Even if a QR Code is created with automatic masking requested (mask =
    /// &#x2212;1), the resulting object still has a mask value between 0 and 7.
    /// </summary>
    property Mask: Int32 read GetMask;

    /// <summary>
    /// The Modules of this QR Code. Immutable after constructor finishes.
    /// </summary>
    property Modules: TQRCodeGenLibInt32Array read GetModules;

    /// <summary>
    /// property for getting/setting the background color of the QRCode <br />
    /// Object <br />
    /// </summary>
    property BackgroundColor: TQRCodeGenLibColor read GetBackgroundColor
      write SetBackgroundColor;

    /// <summary>
    /// property for getting/setting the foreground color of the QRCode
    /// Object
    /// </summary>
    property ForegroundColor: TQRCodeGenLibColor read GetForegroundColor
      write SetForegroundColor;

    /// <summary>
    /// Returns a bitmap image depicting this QR Code, with the specified
    /// module scale and border modules. For example, ToBitmapImage(scale=10,
    /// border=4) means to pad the QR Code with 4 white border modules on all
    /// four sides, and use 10×10 pixels to represent each module. The
    /// resulting image contains the colors specified by the backgroundcolor
    /// (by default = FFFFFF) and foregroundcolor (by default = 000000)
    /// properties.
    /// </summary>
    /// <param name="AScale">
    /// the side length (measured in pixels, must be positive) of each module
    /// </param>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <returns>
    /// a new bmp image representing this QR Code, with padding and scaling
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the scale or border is out of range, or if <br />{AScale, ABorder,
    /// FSize} cause the image dimensions to exceed System.High(Int32)
    /// </exception>
    /// <remarks>
    /// <b>The caller is responsible for the lifetime of the returned image
    /// object.</b>
    /// </remarks>
    function ToBitmapImage(AScale, ABorder: Int32): TQRCodeGenLibBitmap;

    /// <summary>
    /// Returns a string of SVG code for an image depicting this QR Code,
    /// with the specified number of border modules. The string always uses
    /// Unix newlines Char(#10), regardless of the platform.
    /// </summary>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <returns>
    /// a string representing this QR Code as an SVG XML document
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the border is negative
    /// </exception>
    function ToSvgString(ABorder: Int32): String;

    /// <summary>
    /// saves a string of SVG code for an image depicting this QR Code, with
    /// the specified number of border modules as an svg file. The string
    /// always uses Unix newlines Char(#10), regardless of the platform.
    /// </summary>
    /// <param name="ABorder">
    /// the number of border modules to add, which must be non-negative
    /// </param>
    /// <param name="AFileName">
    /// the filename to save the output to <b>with file extension</b>
    /// </param>
    /// <returns>
    /// true on success and false on failure.
    /// </returns>
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the border is negative
    /// </exception>
    function ToSvgFile(ABorder: Int32; const AFileName: String): Boolean;
    /// <summary>
    /// Returns a QR Code representing the specified Unicode text string at
    /// the specified error correction level. As a conservative upper bound,
    /// this function is guaranteed to succeed for strings that have 738 or
    /// fewer Unicode code points (not UTF-16 code units) if the low error
    /// correction level is used. The smallest possible QR Code version is
    /// automatically chosen for the output. The ECC level of the result may
    /// be higher than the ecl argument if it can be done without increasing
    /// the version.
    /// </summary>
    /// <param name="AText">
    /// the text to be encoded which can be any Unicode string
    /// </param>
    /// <param name="AEcl">
    /// the error correction level to use (boostable)
    /// </param>
    /// <param name="AEncoding">
    /// the encoding object to be used to convert the text to bytes if the
    /// text does not fall into numeric or alphanumeric.
    /// </param>
    /// <returns>
    /// a QR Code representing the text
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EDataTooLongQRCodeGenLibException">
    /// if the text fails to fit in the largest version QR Code at the ECL,
    /// which means it is too long
    /// </exception>
    /// /// <exception cref="QlpQRCodeGenLibTypes|ENullReferenceQRCodeGenLibException">
    /// if <paramref name="AEncoding" /> is Nil
    /// </exception>
    class function EncodeText(const AText: String; AEcl: TEcc;
      const AEncoding: TEncoding): IQrCode; static;

    /// <summary>
    /// Returns a QR Code representing the specified binary data at the
    /// specified error correction level. This function always encodes using
    /// the binary segment mode, not any text mode. The maximum number of
    /// bytes allowed is 2953. The smallest possible QR Code version is
    /// automatically chosen for the output. The ECC level of the result may
    /// be higher than the ecl argument if it can be done without increasing
    /// the version.
    /// </summary>
    /// <param name="AData">
    /// the binary data to encode
    /// </param>
    /// <param name="AEcl">
    /// the error correction level to use
    /// </param>
    /// <returns>
    /// a QR Code representing the data
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EDataTooLongQRCodeGenLibException">
    /// if the data fails to fit in the largest version QR Code at the ECL,
    /// which means it is too long
    /// </exception>
    class function EncodeBinary(const AData: TQRCodeGenLibByteArray; AEcl: TEcc)
      : IQrCode; static;

    /// <summary>
    /// Returns a QR Code representing the specified segments at the
    /// specified error correction level. The smallest possible QR Code
    /// version is automatically chosen for the output. The ECC level of the
    /// result may be higher than the ecl argument if it can be done without
    /// increasing the version. This function allows the user to create a
    /// custom sequence of segments that switches between modes (such as
    /// alphanumeric and byte) to encode text in less space.
    /// </summary>
    /// <param name="ASegs">
    /// the segments to encode
    /// </param>
    /// <param name="AEcl">
    /// the error correction level to use (boostable)
    /// </param>
    /// <returns>
    /// a QR Code representing the segments
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EArgumentOutOfRangeQRCodeGenLibException">
    /// if the data fails to fit in the largest version QR Code at the ECL,
    /// which means it is too long
    /// </exception>
    class function EncodeSegments(const ASegs
      : TQRCodeGenLibGenericArray<IQrSegment>; AEcl: TEcc): IQrCode;
      overload; static;

    /// <summary>
    /// Returns a QR Code representing the specified segments with the
    /// specified encoding parameters. The smallest possible QR Code version
    /// within the specified range is automatically chosen for the output.
    /// Iff boostEcl is {@code true}, then the ECC level of the result may be
    /// higher than the ecl argument if it can be done without increasing the
    /// version. The mask number is either between 0 to 7 (inclusive) to
    /// force that mask, or −1 to automatically choose an appropriate mask
    /// (which may be slow). This function allows the user to create a custom
    /// sequence of segments that switches between modes (such as
    /// alphanumeric and byte) to encode text in less space.
    /// </summary>
    /// <param name="ASegs">
    /// the segments to encode
    /// </param>
    /// <param name="AEcl">
    /// the error correction level to use (boostable)
    /// </param>
    /// <param name="AMinVersion">
    /// the minimum allowed version of the QR Code (at least 1)
    /// </param>
    /// <param name="AMaxVersion">
    /// the maximum allowed version of the QR Code (at most 40)
    /// </param>
    /// <param name="AMask">
    /// the mask number to use (between 0 and 7 (inclusive)), or −1 for
    /// automatic mask
    /// </param>
    /// <param name="ABoostEcl">
    /// increases the ECC level as long as it doesn't increase the version
    /// number
    /// </param>
    /// <returns>
    /// a QR Code representing the segments
    /// </returns>
    /// <exception cref="QlpQRCodeGenLibTypes|EDataTooLongQRCodeGenLibException">
    /// if 1 &#x2264; minVersion &#x2264; maxVersion &#x2264; 40
    /// or &#x2212;1 &#x2264; mask &#x2264; 7 is violated; or if the segments fail to
    /// fit in the maxVersion QR Code at the ECL, which means they are too long
    /// </exception>
    class function EncodeSegments(const ASegs
      : TQRCodeGenLibGenericArray<IQrSegment>; AEcl: TEcc;
      AMinVersion, AMaxVersion, AMask: Int32; ABoostEcl: Boolean): IQrCode;
      overload; static;

  end;

implementation

{ TQrCode.TEccHelper }

function TQrCode.TEccHelper.GetFormatBits: Int32;
begin
  Result := Ord(Self);
end;

function TQrCode.TEccHelper.GetToInt32: Int32;
begin
  case Self of
    TQrCode.TEcc.eccLow:
      begin
        Result := 0;
        Exit;
      end;
    TQrCode.TEcc.eccMedium:
      begin
        Result := 1;
        Exit;
      end;
    TQrCode.TEcc.eccQuartile:
      begin
        Result := 2;
        Exit;
      end;
    TQrCode.TEcc.eccHigh:
      begin
        Result := 3;
        Exit;
      end
  else
    begin
      raise EInvalidOperationQRCodeGenLibException.CreateRes(@SInvalidState);
    end;
  end;
end;

{ TQrCode }

function TQrCode.GetVersion: Int32;
begin
  Result := FVersion;
end;

function TQrCode.HandleConstructorMasking(const AMasks
  : TQRCodeGenLibMatrixInt32Array; AMask: Int32): Int32;
var
  LMinPenalty, LIdx, LPenalty: Int32;
begin
  if (AMask = -1) then
  begin
    // Automatically choose best mask
    LMinPenalty := System.High(Int32);
    for LIdx := 0 to System.Pred(8) do
    begin
      ApplyMask(AMasks[LIdx]);
      DrawFormatBits(LIdx);
      LPenalty := GetPenaltyScore();
      if (LPenalty < LMinPenalty) then
      begin
        AMask := LIdx;
        LMinPenalty := LPenalty;
      end;
      ApplyMask(AMasks[LIdx]); // Undoes the mask due to XOR
    end;
  end;
{$IFDEF DEBUG}
  System.Assert((0 <= AMask) and (AMask <= 7));
{$ENDIF DEBUG}
  ApplyMask(AMasks[AMask]); // Apply the final choice of mask
  DrawFormatBits(AMask); // Overwrite old format bits
  // The caller shall assign this value to the final-declared field
  Result := AMask;
end;

function TQrCode.GetSize: Int32;
begin
  Result := FSize;
end;

function TQrCode.GetMask: Int32;
begin
  Result := FMask;
end;

function TQrCode.GetModules: TQRCodeGenLibInt32Array;
begin
  Result := System.Copy(FModules);
end;

class function TQrCode.GetNumDataCodeWords(AVersion: Int32; AEcl: TEcc): Int32;
var
  LEclInt: Int32;
begin
  LEclInt := AEcl.ToInt32;
  Result := (TBits.Asr32(TQrTemplate.GetNumRawDataModules(AVersion), 3)) -
    (ECC_CODEWORDS_PER_BLOCK[LEclInt][AVersion] * NUM_ERROR_CORRECTION_BLOCKS
    [LEclInt][AVersion]);
end;

class procedure TQrCode.FinderPenaltyAddHistory(ACurrentRunLength: Int32;
  const ARunHistory: TQRCodeGenLibInt32Array);
begin
  System.Move(ARunHistory[0], ARunHistory[1], (System.Length(ARunHistory) - 1) *
    System.SizeOf(Int32));
  ARunHistory[0] := ACurrentRunLength;
end;

function TQrCode.FinderPenaltyCountPatterns(const ARunHistory
  : TQRCodeGenLibInt32Array): Int32;
var
  Ln, LTempA, LTempB: Int32;
  LCore: Boolean;
begin
  Ln := ARunHistory[1];
{$IFDEF DEBUG}
  System.Assert(Ln <= (Size * 3));
{$ENDIF DEBUG}
  LCore := (Ln > 0) and (ARunHistory[2] = Ln) and (ARunHistory[3] = Ln * 3) and
    (ARunHistory[4] = Ln) and (ARunHistory[5] = Ln);

  if ((LCore) and ((ARunHistory[0] >= (Ln * 4)) and (ARunHistory[6] >= Ln)))
  then
  begin
    LTempA := 1;
  end
  else
  begin
    LTempA := 0;
  end;

  if ((LCore) and ((ARunHistory[6] >= (Ln * 4)) and (ARunHistory[0] >= Ln)))
  then
  begin
    LTempB := 1;
  end
  else
  begin
    LTempB := 0;
  end;

  Result := LTempA + LTempB;
end;

function TQrCode.FinderPenaltyTerminateAndCount(ACurrentRunColor,
  ACurrentRunLength: Int32; const ARunHistory: TQRCodeGenLibInt32Array): Int32;
begin
  // Terminate black run
  if (ACurrentRunColor = 1) then
  begin
    FinderPenaltyAddHistory(ACurrentRunLength, ARunHistory);
    ACurrentRunLength := 0;
  end;
  ACurrentRunLength := ACurrentRunLength + Size;
  // Add white border to final run
  FinderPenaltyAddHistory(ACurrentRunLength, ARunHistory);
  Result := FinderPenaltyCountPatterns(ARunHistory);
end;

function TQrCode.GetPenaltyScore: Int32;
var
  LEnd, LBlack, LIndex, LDownIndex, LCurRow, LNextRow, LRunColor, LRunX, LRunY,
    LPadRun, Lx, Lc, LTotal, Lk, Ly: Int32;
  LRunHistory: TQRCodeGenLibInt32Array;
begin
  Result := 0;
  LBlack := 0;
  LIndex := 0;
  System.SetLength(LRunHistory, 7);
  LDownIndex := FSize;
  LEnd := FSize * FSize;

  // Iterate over adjacent pairs of rows
  while LIndex < LEnd do
  begin
    LRunColor := 0;
    LRunX := 0;
    TArrayUtils.Fill(LRunHistory, 0);
    LPadRun := Size; // Add white border to initial run
    LCurRow := 0;
    LNextRow := 0;

    Lx := 0;
    while Lx < FSize do
    begin

      // Adjacent modules having same color
      Lc := TQrCodeCommons.GetBit(FModules[TBits.Asr32(LIndex, 5)], LIndex);

      if (Lc = LRunColor) then
      begin
        System.Inc(LRunX);
        if (LRunX = 5) then
        begin
          Result := Result + PENALTY_N1;
        end
        else if (LRunX > 5) then
        begin
          System.Inc(Result);
        end;
      end
      else
      begin
        FinderPenaltyAddHistory(LRunX + LPadRun, LRunHistory);
        LPadRun := 0;
        if (LRunColor = 0) then
        begin
          Result := Result + FinderPenaltyCountPatterns(LRunHistory) *
            PENALTY_N3;
        end;
        LRunColor := Lc;
        LRunX := 1;
      end;

      LBlack := LBlack + Lc;
      if (LDownIndex < LEnd) then
      begin

        LCurRow := ((LCurRow shl 1) or Lc) and 3;
        LNextRow := ((LNextRow shl 1) or
          TQrCodeCommons.GetBit(FModules[TBits.Asr32(LDownIndex, 5)],
          LDownIndex)) and 3;
        // 2*2 blocks of modules having same color
        if ((Lx >= 1) and ((LCurRow = 0) or (LCurRow = 3)) and
          (LCurRow = LNextRow)) then
        begin
          Result := Result + PENALTY_N2;
        end;
      end;

      System.Inc(Lx);
      System.Inc(LIndex);
      System.Inc(LDownIndex);
    end;

    Result := Result + FinderPenaltyTerminateAndCount(LRunColor,
      LRunX + LPadRun, LRunHistory) * PENALTY_N3;
  end;

  // Iterate over single columns
  Lx := 0;
  while Lx < FSize do
  begin
    LRunColor := 0;
    LRunY := 0;
    TArrayUtils.Fill(LRunHistory, 0);
    LPadRun := Size; // Add white border to initial run
    Ly := 0;
    LIndex := Lx;
    while Ly < FSize do
    begin
      // Adjacent modules having same color
      Lc := TQrCodeCommons.GetBit(FModules[TBits.Asr32(LIndex, 5)], LIndex);

      if (Lc = LRunColor) then
      begin
        System.Inc(LRunY);
        if (LRunY = 5) then
        begin
          Result := Result + PENALTY_N1;
        end
        else if (LRunY > 5) then
        begin
          System.Inc(Result);
        end;
      end
      else
      begin
        FinderPenaltyAddHistory(LRunY + LPadRun, LRunHistory);
        LPadRun := 0;
        if (LRunColor = 0) then
        begin
          Result := Result + FinderPenaltyCountPatterns(LRunHistory) *
            PENALTY_N3;
        end;
        LRunColor := Lc;
        LRunY := 1;
      end;

      System.Inc(Ly);
      System.Inc(LIndex, FSize);
    end;
    Result := Result + FinderPenaltyTerminateAndCount(LRunColor,
      LRunY + LPadRun, LRunHistory) * PENALTY_N3;
    System.Inc(Lx);
  end;

  // Balance of black and white modules
  LTotal := FSize * FSize; // Note that size is odd, so black/total != 1/2
  // Compute the smallest integer k >= 0 such that (45-5k)% <= black/total <= (55+5k)%
  Lk := ((Abs((LBlack * 20) - (LTotal * 10)) + LTotal - 1) div LTotal) - 1;
  Result := Result + (Lk * PENALTY_N4);

end;

function TQrCode.GetModule(Ax, Ay: Int32): Boolean;
var
  LIdx: Int32;
begin
  if ((0 <= Ax) and (Ax < FSize) and (0 <= Ay) and (Ay < FSize)) then
  begin
    LIdx := (Ay * FSize) + Ax;
    Result := TQrCodeCommons.GetBit(FModules[TBits.Asr32(LIdx, 5)], LIdx) <> 0;
  end
  else
  begin
    Result := false;
  end;
end;

function TQrCode.AddEccAndInterleave(const AData: TQRCodeGenLibByteArray)
  : TQRCodeGenLibByteArray;
var
  LNumBlocks, LBlockEccLen, LRawCodewords, LNumShortBlocks, LShortBlockDataLen,
    LIIdx, LKIdx, LJIdx, LLIdx, LDatLen, LCompareResult: Int32;
  LRs: IReedSolomonGenerator;
  LEcc: TQRCodeGenLibByteArray;
begin
  if (System.Length(AData) <> GetNumDataCodeWords(FVersion,
    FErrorCorrectionLevel)) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.Create('');
  end;

  // Calculate parameter numbers
  LNumBlocks := NUM_ERROR_CORRECTION_BLOCKS[FErrorCorrectionLevel.ToInt32]
    [FVersion];
  LBlockEccLen := ECC_CODEWORDS_PER_BLOCK[FErrorCorrectionLevel.ToInt32]
    [FVersion];
  LRawCodewords := TBits.Asr32(TQrTemplate.GetNumRawDataModules(FVersion), 3);
  LNumShortBlocks := LNumBlocks - (LRawCodewords mod LNumBlocks);
  LShortBlockDataLen := (LRawCodewords div LNumBlocks) - LBlockEccLen;

  // Split data into blocks, calculate ECC, and interleave
  // (not concatenate) the bytes into a single sequence
  System.SetLength(Result, LRawCodewords);
  LRs := TReedSolomonGenerator.GetInstance(LBlockEccLen);
  System.SetLength(LEcc, LBlockEccLen); // Temporary storage per iteration

  LIIdx := 0;
  LKIdx := 0;
  while LIIdx < LNumBlocks do
  begin
    if LIIdx < LNumShortBlocks then
    begin
      LCompareResult := 0;
    end
    else
    begin
      LCompareResult := 1;
    end;
    LDatLen := LShortBlockDataLen + LCompareResult;
    LRs.GetRemainder(AData, LKIdx, LDatLen, LEcc);
    LJIdx := 0;
    LLIdx := LIIdx;
    while LJIdx < LDatLen do
    begin
      // Copy data
      if (LJIdx = LShortBlockDataLen) then
      begin
        LLIdx := LLIdx - LNumShortBlocks;
      end;
      Result[LLIdx] := AData[LKIdx];
      System.Inc(LJIdx);
      System.Inc(LKIdx);
      System.Inc(LLIdx, LNumBlocks);
    end;

    LJIdx := 0;
    LLIdx := System.Length(AData) + LIIdx;
    while LJIdx < LBlockEccLen do
    begin
      // Copy ECC
      Result[LLIdx] := LEcc[LJIdx];
      System.Inc(LJIdx);
      System.Inc(LLIdx, LNumBlocks);
    end;
    System.Inc(LIIdx);
  end;
end;

procedure TQrCode.ApplyMask(const AMask: TQRCodeGenLibInt32Array);
var
  LIdx: Int32;
begin
  if (System.Length(AMask) <> System.Length(FModules)) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.Create('');
  end;
  for LIdx := System.Low(AMask) to System.High(AMask) do
  begin
    FModules[LIdx] := FModules[LIdx] xor AMask[LIdx];
  end;
end;

constructor TQrCode.Create(AVersion: Int32; AEcl: TEcc;
  const ADataCodewords: TQRCodeGenLibByteArray; AMask: Int32);
var
  LTpl: IQrTemplate;
  LAllCodewords: TQRCodeGenLibByteArray;
begin
  Inherited Create();
  // Check arguments and initialize fields
  FErrorCorrectionLevel := AEcl;

  if ((AVersion < MIN_VERSION) or (AVersion > MAX_VERSION)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes(@SValueOutOfRange);
  end;

  if ((FMask < -1) or (FMask > 7)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes(@SMaskOutOfRange);
  end;

  // Initialize fields
  FVersion := AVersion;
  FSize := (AVersion * 4) + 17;

  LTpl := TQrTemplate.GetInstance(AVersion);
  // no need to make a copy here because the property already does that for us.
  FModules := LTpl.Template;

  // Compute ECC, draw modules, do masking
  LAllCodewords := AddEccAndInterleave(ADataCodewords);
  DrawCodeWords(LTpl.DataOutputBitIndexes, LAllCodewords);
  FMask := HandleConstructorMasking(LTpl.Masks, AMask);
  FBackgroundColor := QRCodeGenLibWhiteColor;
  FForegroundColor := QRCodeGenLibBlackColor;
end;

procedure TQrCode.SetModule(Ax, Ay, ABlack: Int32);
var
  LIdx: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((0 <= Ax) and (Ax < FSize));
  System.Assert((0 <= Ay) and (Ay < FSize));
  System.Assert((ABlack = 0) or (ABlack = 1));
{$ENDIF DEBUG}
  LIdx := (Ay * FSize) + Ax;

  FModules[TBits.Asr32(LIdx, 5)] := FModules[TBits.Asr32(LIdx, 5)] and
    (not(TBits.LeftShift32(1, LIdx)));

  FModules[TBits.Asr32(LIdx, 5)] := FModules[TBits.Asr32(LIdx, 5)] or
    (TBits.LeftShift32(ABlack, LIdx));
end;

{$IFDEF LCL}

function TQrCode.ToBitmapImageInternalLCL(AScale, ABorder: Int32)
  : TQRCodeGenLibBitmap;
var
  LColumn, LRow: Int32;
  LDoColor: Boolean;
  LBrushColor, LForegroundColor, LBackgroundColor: TQRCodeGenLibColor;
  LScanLine: PByte;
  LBytesPerPixel, LRedOffset, LGreenOffset, LBlueOffset: Byte;
begin
  Result := TQRCodeGenLibBitmap.Create;

  Result.SetSize((FSize + (ABorder * 2)) * AScale, (FSize + (ABorder * 2))
    * AScale);

  LForegroundColor := FForegroundColor;
  LBackgroundColor := FBackgroundColor;

  LBytesPerPixel := Result.RawImage.Description.BitsPerPixel shr 3;
  LRedOffset := Result.RawImage.Description.RedShift shr 3;
  LGreenOffset := Result.RawImage.Description.GreenShift shr 3;
  LBlueOffset := Result.RawImage.Description.BlueShift shr 3;
{$IFNDEF ENDIAN_LITTLE}
  LRedOffset := LBytesPerPixel - 1 - LRedOffset;
  LGreenOffset := LBytesPerPixel - 1 - LGreenOffset;
  LBlueOffset := LBytesPerPixel - 1 - LBlueOffset;
{$ENDIF ENDIAN_LITTLE}
  try
    Result.BeginUpdate(True);
    for LColumn := 0 to System.Pred(Result.Height) do
    begin
      LScanLine := Result.RawImage.GetLineStart(LColumn);
      for LRow := 0 to System.Pred(Result.Width) do
      begin
        LDoColor := GetModule((LRow div AScale) - ABorder,
          (LColumn div AScale) - ABorder);
        if LDoColor then
        begin
          LBrushColor := LForegroundColor;
        end
        else
        begin
          LBrushColor := LBackgroundColor;
        end;
        // Slow !!!
        // Result.Canvas.Pixels[LRow, LColumn] := LBrushColor;
        (LScanLine + LBlueOffset)^ := TConverters.GetBValue(LBrushColor);
        (LScanLine + LGreenOffset)^ := TConverters.GetGValue(LBrushColor);
        (LScanLine + LRedOffset)^ := TConverters.GetRValue(LBrushColor);
        System.Inc(LScanLine, LBytesPerPixel);
      end;
    end;
  finally
    Result.EndUpdate(false);
  end;
end;
{$ENDIF LCL}
{$IFDEF FCL}

function TQrCode.ToBitmapImageInternalFCL(AScale, ABorder: Int32)
  : TQRCodeGenLibBitmap;
var
  LColumn, LRow: Int32;
  LDoColor: Boolean;
  LBrushColor, LForegroundColor, LBackgroundColor: TQRCodeGenLibColor;
begin
  Result := TQRCodeGenLibBitmap.Create((FSize + (ABorder * 2)) * AScale,
    (FSize + (ABorder * 2)) * AScale);

  Result.UsePalette := True;

  LForegroundColor := FForegroundColor;
  LBackgroundColor := FBackgroundColor;

  for LColumn := 0 to System.Pred(Result.Height) do
  begin
    for LRow := 0 to System.Pred(Result.Width) do
    begin
      LDoColor := GetModule((LRow div AScale) - ABorder,
        (LColumn div AScale) - ABorder);
      if LDoColor then
      begin
        LBrushColor := LForegroundColor;
      end
      else
      begin
        LBrushColor := LBackgroundColor;
      end;
      Result.Colors[LRow, LColumn] := LBrushColor;
    end;

  end;
end;
{$ENDIF FCL}
{$IFDEF VCL}

function TQrCode.ToBitmapImageInternalVCL(AScale, ABorder: Int32)
  : TQRCodeGenLibBitmap;
type
  TRGBTriple = record
    B, G, R: Byte;
  end;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0 .. MaxInt div SizeOf(TRGBTriple) - 1]
    of TRGBTriple;
var
  LColumn, LRow: Int32;
  LDoColor: Boolean;
  LBrushColor, LForegroundColor, LBackgroundColor: TQRCodeGenLibColor;
  LScanLine: PRGBTripleArray;
begin
  Result := TQRCodeGenLibBitmap.Create;
  Result.PixelFormat := TwentyFourBitPixelFormat;

  Result.SetSize((FSize + (ABorder * 2)) * AScale, (FSize + (ABorder * 2))
    * AScale);

  LForegroundColor := FForegroundColor;
  LBackgroundColor := FBackgroundColor;

  for LColumn := 0 to System.Pred(Result.Height) do
  begin
    LScanLine := Result.ScanLine[LColumn];
    for LRow := 0 to System.Pred(Result.Width) do
    begin
      LDoColor := GetModule((LRow div AScale) - ABorder,
        (LColumn div AScale) - ABorder);
      if LDoColor then
      begin
        LBrushColor := LForegroundColor;
      end
      else
      begin
        LBrushColor := LBackgroundColor;
      end;
      // Slow !!!
      // Result.Canvas.Pixels[LRow, LColumn] := LBrushColor;
      LScanLine^[LRow].B := TConverters.GetBValue(LBrushColor);
      LScanLine^[LRow].G := TConverters.GetGValue(LBrushColor);
      LScanLine^[LRow].R := TConverters.GetRValue(LBrushColor);
    end;

  end;
end;
{$ENDIF VCL}
{$IFDEF FMX}

function TQrCode.ToBitmapImageInternalFMX(AScale, ABorder: Int32)
  : TQRCodeGenLibBitmap;
var
  LColumn, LRow: Int32;
  LDoColor: Boolean;
  LBrushColor, LForegroundColor, LBackgroundColor: TQRCodeGenLibColor;
  LBitData: TQRCodeGenLibBitmapData;
begin
  Result := TQRCodeGenLibBitmap.Create;
  Result.SetSize((FSize + (ABorder * 2)) * AScale, (FSize + (ABorder * 2))
    * AScale);
  LForegroundColor := FForegroundColor;
  LBackgroundColor := FBackgroundColor;

  if Result.Map(TQRCodeGenLibMapAccess.Write, LBitData) then
  begin
    try
      for LColumn := 0 to System.Pred(Result.Height) do
      begin
        for LRow := 0 to System.Pred(Result.Width) do
        begin
          LDoColor := GetModule((LRow div AScale) - ABorder,
            (LColumn div AScale) - ABorder);
          if LDoColor then
          begin
            LBrushColor := LForegroundColor;
          end
          else
          begin
            LBrushColor := LBackgroundColor;
          end;
          LBitData.SetPixel(LRow, LColumn, LBrushColor);
        end;

      end;

    finally
      Result.Unmap(LBitData);
    end;
  end;

end;
{$ENDIF FMX}

function TQrCode.ToBitmapImage(AScale, ABorder: Int32): TQRCodeGenLibBitmap;
begin
  ValidateImageDimensions(AScale, ABorder);
{$IF DEFINED(LCL)}
  Result := ToBitmapImageInternalLCL(AScale, ABorder);
{$ELSEIF DEFINED(FCL)}
  Result := ToBitmapImageInternalFCL(AScale, ABorder);
{$ELSEIF DEFINED(VCL)}
  Result := ToBitmapImageInternalVCL(AScale, ABorder);
{$ELSEIF DEFINED(FMX)}
  Result := ToBitmapImageInternalFMX(AScale, ABorder);
{$ELSE}
{$MESSAGE ERROR 'This Framework is not supported at the moment.'}
{$IFEND}
end;

function TQrCode.ToSvgString(ABorder: Int32): String;
var
  LColumn, LRow: Int32;
  LBorder: Int64;
  LStringList: TStringList;
  LForegroundColor, LBackgroundColor: String;
begin
  if (ABorder < 0) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.CreateRes(@SBorderNegative);
  end;
  LBorder := ABorder;

  LForegroundColor := TConverters.QRCodeGenLibColorToHTMLHexColor(FForegroundColor);
  LBackgroundColor := TConverters.QRCodeGenLibColorToHTMLHexColor(FBackgroundColor);

  LStringList := TStringList.Create;
  LStringList.LineBreak := '';
  try
    LStringList.Add(Format('<?xml version="1.0" encoding="UTF-8"?>%s',
      [UNIX_NEW_LINE]));
    LStringList.Add
      (Format('<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">%s',
      [UNIX_NEW_LINE]));
    LStringList.Add
      (Format('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 %d %d" stroke="none">%s',
      [FSize + (LBorder * 2), (FSize + LBorder * 2), UNIX_NEW_LINE]));
    LStringList.Add
      (Format('%s<rect width="100%%" height="100%%" fill="#%s"/>%s',
      [TAB, LBackgroundColor, UNIX_NEW_LINE]));
    LStringList.Add(Format('%s<path d="', [TAB]));

    for LColumn := 0 to System.Pred(FSize) do
    begin
      for LRow := 0 to System.Pred(FSize) do
      begin
        if (GetModule(LRow, LColumn)) then
        begin
          if ((LRow <> 0) or (LColumn <> 0)) then
          begin
            LStringList.Add(' ');
          end;
          LStringList.Add(Format('M%d,%dh1v1h-1z', [LRow + LBorder,
            LColumn + LBorder]));
        end;
      end;

    end;

    LStringList.Add(Format('" fill="#%s"/>%s', [LForegroundColor,
      UNIX_NEW_LINE]));
    LStringList.Add(Format('</svg>%s', [UNIX_NEW_LINE]));

    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;

function TQrCode.ToSvgFile(ABorder: Int32; const AFileName: String): Boolean;
var
  LFileStream: TFileStream;
  LBytes: TQRCodeGenLibByteArray;
begin
  if Trim(AFileName) = '' then
  begin
    raise EArgumentInvalidQRCodeGenLibException.CreateRes(@SFileNameEmpty);
  end;
  LFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    try
      LBytes := TConverters.ConvertStringToBytes(ToSvgString(ABorder),
        TEncoding.UTF8);
      LFileStream.WriteBuffer(LBytes[0], System.Length(LBytes) *
        System.SizeOf(Byte));
      Result := True;
    except
      Result := false;
    end;
  finally
    LFileStream.Free;
  end;
end;

procedure TQrCode.ValidateImageDimensions(AScale, ABorder: Int32);
begin
  if ((AScale <= 0) or (ABorder < 0)) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes(@SValueOutOfRange);
  end;

  if ((ABorder > (System.High(Int32) shr 1)) or
    ((FSize + (ABorder * Int64(2))) > (System.High(Int32) div AScale))) then
  begin
    raise EArgumentOutOfRangeQRCodeGenLibException.CreateRes
      (@SScaleOrBorderTooLarge);
  end;
end;

procedure TQrCode.DrawCodeWords(const ADataOutputBitIndexes
  : TQRCodeGenLibInt32Array; const AAllCodewords: TQRCodeGenLibByteArray);
var
  LIIdx, LJIdx, LBit: Int32;
begin
  if ((System.Length(AAllCodewords) * 8) <>
    (System.Length(ADataOutputBitIndexes))) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.Create('');
  end;
  for LIIdx := System.Low(ADataOutputBitIndexes)
    to System.High(ADataOutputBitIndexes) do

  begin
    LJIdx := ADataOutputBitIndexes[LIIdx];
    LBit := TQrCodeCommons.GetBit(AAllCodewords[TBits.Asr32(LIIdx, 3)],
      (not LIIdx) and 7);
    FModules[TBits.Asr32(LJIdx, 5)] := FModules[TBits.Asr32(LJIdx, 5)] or
      (TBits.LeftShift32(LBit, LJIdx));

  end;
end;

procedure TQrCode.DrawFormatBits(AMask: Int32);
var
  LData, LRem, LIdx, LBits: Int32;
begin
  // Calculate error correction code and pack bits
  LData := (FErrorCorrectionLevel.FormatBits shl 3) or AMask;
  // errCorrLvl is uint2, mask is uint3
  LRem := LData;
  LIdx := 0;
  while LIdx < 10 do
  begin
    LRem := (LRem shl 1) xor ((TBits.Asr32(LRem, 9)) * $537);
    System.Inc(LIdx);
  end;
  LBits := ((LData shl 10) or LRem) xor $5412; // uint15
{$IFDEF DEBUG}
  System.Assert(TBits.Asr32(LBits, 15) = 0);
{$ENDIF DEBUG}
  // Draw first copy
  for LIdx := 0 to 5 do
  begin
    SetModule(8, LIdx, TQrCodeCommons.GetBit(LBits, LIdx));
  end;

  SetModule(8, 7, TQrCodeCommons.GetBit(LBits, 6));
  SetModule(8, 8, TQrCodeCommons.GetBit(LBits, 7));
  SetModule(7, 8, TQrCodeCommons.GetBit(LBits, 8));

  for LIdx := 9 to System.Pred(15) do
  begin
    SetModule(14 - LIdx, 8, TQrCodeCommons.GetBit(LBits, LIdx));
  end;

  // Draw second copy
  for LIdx := 0 to System.Pred(8) do
  begin
    SetModule(FSize - 1 - LIdx, 8, TQrCodeCommons.GetBit(LBits, LIdx));
  end;
  for LIdx := 8 to System.Pred(15) do
  begin
    SetModule(8, FSize - 15 + LIdx, TQrCodeCommons.GetBit(LBits, LIdx));
  end;
  SetModule(8, FSize - 8, 1); // Always black
end;

class function TQrCode.EncodeBinary(const AData: TQRCodeGenLibByteArray;
  AEcl: TEcc): IQrCode;
begin
  Result := EncodeSegments(TQRCodeGenLibGenericArray<IQrSegment>.Create
    (TQrSegment.MakeBytes(AData)), AEcl);
end;

class function TQrCode.EncodeSegments(const ASegs
  : TQRCodeGenLibGenericArray<IQrSegment>; AEcl: TEcc): IQrCode;
begin
  Result := EncodeSegments(ASegs, AEcl, MIN_VERSION, MAX_VERSION, -1, True);
end;

class function TQrCode.EncodeSegments(const ASegs
  : TQRCodeGenLibGenericArray<IQrSegment>; AEcl: TEcc;
  AMinVersion, AMaxVersion, AMask: Int32; ABoostEcl: Boolean): IQrCode;
var
  LVersion, LDataUsedBits, LDataCapacityBits, LPadByte: Int32;
  LNewEcl: TEcc;
  LBitBuffer: TBitBuffer;
  LSeg: IQrSegment;
begin
  if (not((MIN_VERSION <= AMinVersion) and (AMinVersion <= AMaxVersion) and
    (AMaxVersion <= MAX_VERSION)) or (AMask < -1) or (AMask > 7)) then
  begin
    raise EArgumentInvalidQRCodeGenLibException.CreateRes(@SInvalidValue);
  end;
  LVersion := AMinVersion;
  // Find the minimal version number to use
  while True do
  begin
    LDataCapacityBits := GetNumDataCodeWords(LVersion, AEcl) * 8;
    // Number of data bits available
    LDataUsedBits := TQrSegment.GetTotalBits(ASegs, LVersion);
    if ((LDataUsedBits <> -1) and (LDataUsedBits <= LDataCapacityBits)) then
    begin
      Break; // This version number is found to be suitable
    end;
    if (LVersion >= AMaxVersion) then
    // All versions in the range could not fit the given data
    begin
      if (LDataUsedBits <> -1) then
      begin
        raise EDataTooLongQRCodeGenLibException.CreateResFmt(@SSegmentSizeError,
          [LDataUsedBits, LDataCapacityBits]);
      end;
      raise EDataTooLongQRCodeGenLibException.CreateRes(@SSegmentTooLong);
    end;
    System.Inc(LVersion);
  end;

  // Increase the error correction level while the data still fits in the current version number

  for LNewEcl in EccOrder do
  begin
    // From low to high
    if ((ABoostEcl) and (LDataUsedBits <= (GetNumDataCodeWords(LVersion,
      LNewEcl) * 8))) then
    begin
      AEcl := LNewEcl;
    end;
  end;

  // Concatenate all segments to create the data bit string
  LBitBuffer := TBitBuffer.Create();
  for LSeg in ASegs do
  begin
    LBitBuffer.AppendBits(LSeg.Mode.ModeBits, 4);
    LBitBuffer.AppendBits(LSeg.NumChars, LSeg.Mode.NumCharCountBits(LVersion));
    LBitBuffer.AppendBits(LSeg.Data, LSeg.BitLength);
  end;

{$IFDEF DEBUG}
  System.Assert(LBitBuffer.BitLength = LDataUsedBits);
{$ENDIF DEBUG}
  // Add terminator and pad up to a byte if applicable
  LDataCapacityBits := GetNumDataCodeWords(LVersion, AEcl) * 8;
{$IFDEF DEBUG}
  System.Assert(LBitBuffer.BitLength <= LDataCapacityBits);
{$ENDIF DEBUG}
  LBitBuffer.AppendBits(0, Min(4, LDataCapacityBits - LBitBuffer.BitLength));
  LBitBuffer.AppendBits(0, (8 - (LBitBuffer.BitLength and 7)) and 7);
{$IFDEF DEBUG}
  System.Assert((LBitBuffer.BitLength and 7) = 0);
{$ENDIF DEBUG}
  // Pad with alternating bytes until data capacity is reached
  LPadByte := $EC;
  while LBitBuffer.BitLength < LDataCapacityBits do
  begin
    LBitBuffer.AppendBits(LPadByte, 8);
    LPadByte := LPadByte xor ($EC xor $11);
  end;

  // Create the QR Code symbol
  Result := TQrCode.Create(LVersion, AEcl, LBitBuffer.GetBytes(), AMask);
end;

class function TQrCode.EncodeText(const AText: String; AEcl: TEcc;
  const AEncoding: TEncoding): IQrCode;
begin
  TGuard.RequireNotNull(AEncoding, SEncodingInstanceNil);
  Result := EncodeSegments(TQrSegment.MakeSegments(AText, AEncoding), AEcl);
end;

function TQrCode.GetBackgroundColor: TQRCodeGenLibColor;
begin
  Result := FBackgroundColor;
end;

function TQrCode.GetForegroundColor: TQRCodeGenLibColor;
begin
  Result := FForegroundColor;
end;

procedure TQrCode.SetBackgroundColor(const AColor: TQRCodeGenLibColor);
begin
  FBackgroundColor := AColor;
end;

procedure TQrCode.SetForegroundColor(const AColor: TQRCodeGenLibColor);
begin
  FForegroundColor := AColor;
end;

end.
