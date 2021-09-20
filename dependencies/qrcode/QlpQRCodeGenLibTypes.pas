unit QlpQRCodeGenLibTypes;

{$I QRCodeGenLib.inc}

interface

uses
{$IF DEFINED(VCL)}
  Vcl.Graphics,
{$ELSEIF DEFINED(FMX)}
  FMX.Graphics,
  UIConsts,
  UITypes,
{$ELSEIF DEFINED(LCL)}
  Graphics,
  Interfaces, // Added so that the LCL will Initialize the WidgetSet
{$ELSEIF DEFINED(FCL)}
  FPWriteBMP, // without this, writing to bitmap will fail
  FPImage, // For FCL Image Support
{$IFEND}
  SysUtils;

type
  EQRCodeGenLibException = class(Exception);
  EDataTooLongQRCodeGenLibException = class(EQRCodeGenLibException);
  EInvalidOperationQRCodeGenLibException = class(EQRCodeGenLibException);
  EIndexOutOfRangeQRCodeGenLibException = class(EQRCodeGenLibException);
  EArgumentQRCodeGenLibException = class(EQRCodeGenLibException);
  EArgumentInvalidQRCodeGenLibException = class(EQRCodeGenLibException);
  EArgumentNilQRCodeGenLibException = class(EQRCodeGenLibException);
  EArgumentOutOfRangeQRCodeGenLibException = class(EQRCodeGenLibException);
  EUnsupportedTypeQRCodeGenLibException = class(EQRCodeGenLibException);
  ENullReferenceQRCodeGenLibException = class(EQRCodeGenLibException);

  /// <summary>
  /// Represents a dynamic array of Byte.
  /// </summary>
  TQRCodeGenLibByteArray = TBytes;

  /// <summary>
  /// Represents a dynamic generic array of Type T.
  /// </summary>
  TQRCodeGenLibGenericArray<T> = array of T;

{$IFDEF DELPHIXE_UP}
  /// <summary>
  /// Represents a dynamic array of Int32.
  /// </summary>
  TQRCodeGenLibInt32Array = TArray<Int32>;

  /// <summary>
  /// Represents a dynamic array of Boolean.
  /// </summary>
  TQRCodeGenLibBooleanArray = TArray<Boolean>;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TQRCodeGenLibStringArray = TArray<String>;

  /// <summary>
  /// Represents a dynamic array of array of Byte.
  /// </summary>
  TQRCodeGenLibMatrixByteArray = TArray<TQRCodeGenLibByteArray>;

  /// <summary>
  /// Represents a dynamic array of array of Int32.
  /// </summary>
  TQRCodeGenLibMatrixInt32Array = TArray<TQRCodeGenLibInt32Array>;

{$ELSE}
  /// <summary>
  /// Represents a dynamic array of Int32.
  /// </summary>
  TQRCodeGenLibInt32Array = array of Int32;

  /// <summary>
  /// Represents a dynamic array of Boolean.
  /// </summary>
  TQRCodeGenLibBooleanArray = array of Boolean;

  /// <summary>
  /// Represents a dynamic array of String.
  /// </summary>
  TQRCodeGenLibStringArray = array of String;

  /// <summary>
  /// Represents a dynamic array of array of Byte.
  /// </summary>
  TQRCodeGenLibMatrixByteArray = array of TQRCodeGenLibByteArray;

  /// <summary>
  /// Represents a dynamic array of array of Int32.
  /// </summary>
  TQRCodeGenLibMatrixInt32Array = array of TQRCodeGenLibInt32Array;

{$ENDIF DELPHIXE_UP}
  TQRCodeGenLibColor =
{$IF DEFINED(VCL_OR_LCL)}TColor{$ELSEIF DEFINED(FCL)}TFPColor{$ELSEIF DEFINED(FMX)}TAlphaColor{$IFEND VCL_OR_LCL};
{$IFDEF FCL}
  TQRCodeGenLibBitmap = TFPCompactImgRGB16Bit;
{$ELSE}
  TQRCodeGenLibBitmap = TBitmap;
{$IFDEF FMX}
  TQRCodeGenLibBitmapData = TBitmapData;
  TQRCodeGenLibMapAccess = TMapAccess;
  TQRCodeGenLibAlphaColorRec = TAlphaColorRec;
{$ENDIF FMX}
{$ENDIF FCL}
  // ===========//
{$IFDEF VCL}

const
  TwentyFourBitPixelFormat = pf24bit;
{$ENDIF VCL}
function QRCodeGenLibWhiteColor: TQRCodeGenLibColor; inline;
function QRCodeGenLibBlackColor: TQRCodeGenLibColor; inline;
{$IF DEFINED(VCL_OR_LCL)}
function QRCodeGenLibColorToRGB(AColor: TQRCodeGenLibColor): LongInt; inline;
{$IFEND VCL_OR_LCL}

implementation

function QRCodeGenLibWhiteColor: TQRCodeGenLibColor;
begin
  Result := {$IF DEFINED(VCL_OR_LCL)}clWhite{$ELSEIF DEFINED(FCL)}colWhite{$ELSEIF DEFINED(FMX)}claWhite{$IFEND VCL_OR_LCL};
end;

function QRCodeGenLibBlackColor: TQRCodeGenLibColor;
begin
  Result := {$IF DEFINED(VCL_OR_LCL)}clBlack{$ELSEIF DEFINED(FCL)}colBlack{$ELSEIF DEFINED(FMX)}claBlack{$IFEND VCL_OR_LCL};
end;

{$IF DEFINED(VCL_OR_LCL)}

function QRCodeGenLibColorToRGB(AColor: TQRCodeGenLibColor): LongInt;
begin
  Result := ColorToRGB(AColor);
end;
{$IFEND VCL_OR_LCL}
{$IFDEF FPC}

initialization

// Set UTF-8 in AnsiStrings, just like Lazarus
SetMultiByteConversionCodePage(CP_UTF8);
// SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$ENDIF FPC}

end.
