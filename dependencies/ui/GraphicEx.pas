Unit GraphicEx;

  // Start of DXE2 section

{$TYPEDADDRESS OFF}

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicColor.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are Copyright
// (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicEx -
//   This unit is an addendum to Graphics.pas, in order to enable your application
//   to import many common graphic files.
//
// See help file for a description of supported image types. Additionally, there is a resample routine
// (Stretch) based on code from Anders Melander (http://www.melander.dk/delphi/resampler/index.html)
// which has been optimized quite a lot to work faster and bug fixed.
//
// version - 9.9
//
// 03-SEP-2000 ml:
//   EPS with alpha channel, workaround for TIFs with wrong alpha channel indication,
//   workaround for bad packbits compressed (TIF) images
// 28-AUG-2000 ml:
//   small bugfixes
// 27-AUG-2000 ml:
//   changed all FreeMemory(P) calls back to ... if Assigned(P) then FreeMem(P); ...
// 24-AUG-2000 ml:
//   small bug in LZ77 decoder removed
// 18-AUG-2000 ml:
//   TIF deflate decoding scheme
// 15-AUG-2000 ml:
//   workaround for TIF images without compression, but prediction scheme set (which is not really used in this case)
// 12-AUG-2000 ml:
//   small changes
// 16-SEP-2008 sz:
//   version for D2009
// 28-SEP-2008 sz:
//   fixed memoryleak in GetGraphicFilter
// 02-NOV-2008 sz:
//   restored PCD format for Delphi 2009
//
//
//
// For older history please look into the help file.
//
// Note: The library provides usually only load support for the listed image formats but will perhaps be enhanced
//       in the future to save those types too. It can be compiled with Delphi 4 or newer versions.
//
//----------------------------------------------------------------------------------------------------------------------

Interface

{$I GraphicConfiguration.inc}

Uses
  Windows, Classes, ExtCtrls, Graphics, SysUtils, JPEG,
  System.Types,
  GraphicCompression, GraphicStrings, GraphicColor;

Type
  TCardinalArray = Array Of Cardinal;
  TByteArray = Array Of Byte;
  TFloatArray = Array Of Single;

  TImageOptions = Set Of (
    ioTiled,       // image consists of tiles not strips (TIF)
    ioBigEndian,   // byte order in values >= words is reversed (TIF, RLA, SGI)
    ioMinIsWhite,  // minimum value in grayscale palette is white not black (TIF)
    ioReversed,    // bit order in bytes is reveresed (TIF)
    ioUseGamma     // gamma correction is used
  );

  // describes the compression used in the image file
  TCompressionType = (
    ctUnknown,     // compression type is unknown
    ctNone,        // no compression at all
    ctRLE,         // run length encoding
    ctPackedBits,  // Macintosh packed bits
    ctLZW,         // Lempel-Zif-Welch
    ctFax3,        // CCITT T.4 (1d), also known as fax group 3
    ctFaxRLE,      // modified Huffman (CCITT T.4 derivative)
    ctFax4,        // CCITT T.6, also known as fax group 4
    ctFaxRLEW,     // CCITT T.4 with word alignment
    ctLZ77,        // Hufman inflate/deflate
    ctJPEG,        // TIF JPEG compression (new version)
    ctOJPEG,       // TIF JPEG compression (old version)
    ctThunderscan, // TIF thunderscan compression
    ctNext,
    ctIT8CTPAD,
    ctIT8LW,
    ctIT8MP,
    ctIT8BL,
    ctPixarFilm,
    ctPixarLog,
    ctDCS,
    ctJBIG,
    ctPCDHuffmann  // PhotoCD Hufman compression
  );

  // properties of a particular image which are set while loading an image or when
  // they are explicitly requested via ReadImageProperties
  PImageProperties = ^TImageProperties;
  TImageProperties = Record
    Version: Cardinal;                 // TIF, PSP, GIF
    Options: TImageOptions;            // all images
    Width,                             // all images
    Height: Cardinal;                  // all images
    ColorScheme: TColorScheme;         // all images
    BitsPerSample,                     // all Images
    SamplesPerPixel,                   // all images
    BitsPerPixel: Byte;                // all images
    Compression: TCompressionType;     // all images
    FileGamma: Single;                 // RLA, PNG
    XResolution,
    YResolution: Single;               // given in dpi (TIF, PCX, PSP)
    Interlaced,                        // GIF, PNG
    HasAlpha: Boolean;                 // TIF, PNG

    // informational data, used internally and/or by decoders
    // TIF
    FirstIFD,
    PlanarConfig,                      // most of this data is needed in the JPG decoder
    CurrentRow,
    TileWidth,
    TileLength,
    BytesPerLine: Cardinal;
    RowsPerStrip: TCardinalArray;
    YCbCrSubSampling,
    JPEGTables: TByteArray;
    JPEGColorMode,
    JPEGTablesMode: Cardinal;
    CurrentStrip,
    StripCount,
    Predictor: Integer;

    // PCD
    Overview: Boolean;                 // true if image is an overview image
    Rotate: Byte;                      // describes how the image is rotated (aka landscape vs. portrait image)
    ImageCount: Word;                  // number of subimages if this is an overview image

    // GIF
    LocalColorTable: Boolean;          // image uses an own color palette instead of the global one

    // RLA
    BottomUp: Boolean;                 // images is bottom to top

    // PSD
    Channels: Byte;                    // up to 24 channels per image

    // PNG
    FilterMode: Byte;
  End;

  // This is the general base class for all image types implemented in GraphicEx.
  // It contains some generally used class/data.
  TGraphicExGraphic = Class(TBitmap)
  Private
    FColorManager: TColorManager;
    FImageProperties: TImageProperties;
    FBasePosition: Cardinal;  // stream start position
    FStream: TStream;         // used for local references of the stream the class is currently loading from
    FProgressRect: TRect;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure Assign(Source: TPersistent); Override;
    Class Function CanLoad(Const FileName: String): Boolean; Overload; Virtual;
    class function CanLoad(Stream: TStream): Boolean; overload; virtual;
    Procedure LoadFromResourceName(Instance: THandle; Const ResName: String);
    Procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Virtual;

    Property ColorManager: TColorManager Read FColorManager;
    Property ImageProperties: TImageProperties Read FImageProperties Write FImageProperties;
  End;

  TGraphicExGraphicClass = Class Of TGraphicExGraphic;

  {$IFDEF SGIGraphic}
  // *.bw, *.rgb, *.rgba, *.sgi images
  TSGIGraphic = Class(TGraphicExGraphic)
  Private
    FRowStart,
    FRowSize: TCardinalArray;    // start and length of a line (if compressed)
    FDecoder: TDecoder;          // ...same applies here
    Procedure ReadAndDecode(Red, Green, Blue, Alpha: Pointer; Row, BPC: Cardinal);
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF AutodeskGraphic}
  // *.cel, *.pic images
  TAutodeskGraphic = Class(TGraphicExGraphic)
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF TIFFGraphic}
  // *.tif, *.tiff images
  // one entry in a an IFD (image file directory)
  TIFDEntry = Packed Record
    Tag: Word;
    DataType: Word;
    DataLength: Cardinal;
    Offset: Cardinal;
  End;

  TTIFFPalette = Array[0..787] Of Word;

  TTIFFGraphic = Class(TGraphicExGraphic)
  Private
    FIFD: Array Of TIFDEntry; // the tags of one image file directory
    FPalette: TTIFFPalette;
    FYCbCrPositioning: Cardinal;
    FYCbCrCoefficients: TFloatArray;
    Function FindTag(Tag: Cardinal; Var Index: Cardinal): Boolean;
    Procedure GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TByteArray); Overload;
    Procedure GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TCardinalArray); Overload;
    Procedure GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TFloatArray); Overload;
    Function GetValue(Stream: TStream; Tag: Cardinal; Default: Single = 0): Single; Overload;
    Function GetValue(Tag: Cardinal; Default: Cardinal = 0): Cardinal; Overload;
    Function GetValue(Tag: Cardinal; Var Size: Cardinal; Default: Cardinal = 0): Cardinal; Overload;
    Procedure SortIFD;
    Procedure SwapIFD;
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Procedure SaveToStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;

    {$IFDEF EPSGraphic}
    TEPSGraphic = Class(TTIFFGraphic)
    Public
      Class Function CanLoad(Stream: TStream): Boolean; Override;
      Procedure LoadFromStream(Stream: TStream); Override;
      Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
    End;
    {$ENDIF} // EPSGraphic
  {$ENDIF} // TIFFGraphic

  {$IFDEF TargaGraphic}
  // *.tga; *.vst; *.icb; *.vda; *.win images
  TTargaGraphic = Class(TGraphicExGraphic)
   Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
    Procedure SaveToStream(Stream: TStream); Overload; Override;
    Procedure SaveToStream(Stream: TStream; Compressed: Boolean); Reintroduce; Overload;
  End;
  {$ENDIF}

  {$IFDEF PCXGraphic}
  // *.pcx; *.pcc; *.scr images
  // Note: Due to the badly designed format a PCX/SCR file cannot be part in a larger stream because the position of the
  //       color palette as well as the decoding size can only be determined by the size of the image.
  //       Hence the image must be the only one in the stream or the last one.
  TPCXGraphic = Class(TGraphicExGraphic)
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF PCDGraphic}
  // *.pcd images
  // Note: By default the BASE resolution of a PCD image is loaded with LoadFromStream.
  TPCDGraphic = Class(TGraphicExGraphic)
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
    class var DefaultResolution: Integer; // 2 = default; 0: 128 × 192; 1: 256 × 384; 2: 512 × 768; 3: 1024 × 1536; 4: 2048 × 3072; 5: 4096 × 6144 optional
  End;
  {$ENDIF}

  {$IFDEF PortableMapGraphic}
  // *.ppm, *.pgm, *.pbm images
  TPPMGraphic = Class(TGraphicExGraphic)
  Private
    FBuffer: array[0..4095] of AnsiChar;
    FIndex: Integer;
    function CurrentChar: AnsiChar;
    function GetChar: AnsiChar;
    Function GetNumber: Cardinal;
    function ReadLine: AnsiString;
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF CUTGraphic}
  // *.cut (+ *.pal) images
  // Note: Also this format should not be used in a stream unless it is the only image or the last one!
  TCUTGraphic = Class(TGraphicExGraphic)
  Private
    FPaletteFile: String;
  Protected
    Procedure LoadPalette;
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromFile(Const FileName: String); Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;

    Property PaletteFile: String Read FPaletteFile Write FPaletteFile;
  End;
  {$ENDIF}

  {$IFDEF GIFGraphic}
  // *.gif images
  TGIFGraphic = Class(TGraphicExGraphic)
  Private
    Function SkipExtensions: Byte;
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF RLAGraphic}
  // *.rla, *.rpf images
  // implementation based on code from Dipl. Ing. Ingo Neumann (ingo@upstart.de, ingo_n@dialup.nacamar.de)
  TRLAGraphic = Class(TGraphicExGraphic)
  Private
    Procedure SwapHeader(Var Header); // start position of the image header in the stream
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF PhotoshopGraphic}
  // *.psd, *.pdd images
  TPSDGraphic = Class(TGraphicExGraphic)
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF PaintshopProGraphic}
  // *.psp images (file version 3 and 4)
  TPSPGraphic = Class(TGraphicExGraphic)
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;
  End;
  {$ENDIF}

  {$IFDEF PortableNetworkGraphic}
  // *.png images
  TChunkType = Array[0..3] Of AnsiChar;

  // This header is followed by a variable number of data bytes, which are followed by the CRC for this data.
  // The actual size of this data is given by field length in the chunk header.
  // CRC is Cardinal (4 byte unsigned integer).
  TPNGChunkHeader = Packed Record
    Length: Cardinal;  // size of data (entire chunk excluding itself, CRC and type)
    case integer of
    0: (ChunkType: TChunkType);
    1: (Mask: DWORD);
  End;

  TPNGGraphic = Class(TGraphicExGraphic)
  Private
    FDecoder: TLZ77Decoder;
    FIDATSize: Integer;        // remaining bytes in the current IDAT chunk
    FRawBuffer,                // buffer to load raw chunk data and to check CRC
    FCurrentSource: Pointer;   // points into FRawBuffer for current position of decoding
    FHeader: TPNGChunkHeader;  // header of the current chunk
    FCurrentCRC: Cardinal;     // running CRC for the current chunk
    FSourceBPP: Integer;       // bits per pixel used in the file
    FPalette: HPALETTE;        // used to hold the palette handle until we can set it finally after the pixel format
                               // has been set too (as this destroys the current palette)
    FTransparency: TByteArray; // If the image is indexed then this array might contain alpha values (depends on file)
                               // each entry corresponding to the same palette index as the index in this array.
                               // For grayscale and RGB images FTransparentColor contains the (only) transparent
                               // color.
    FTransparentColor: TColor; // transparent color for gray and RGB
    FBackgroundColor: TColor;  // index or color ref
    Procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    Function IsChunk(ChunkType: TChunkType): Boolean;
    Function LoadAndSwapHeader: Cardinal;
    Procedure LoadBackgroundColor(Const Description);
    Procedure LoadIDAT(Const Description);
    Procedure LoadTransparency(Const Description);
    Procedure ReadDataAndCheckCRC;
    Procedure ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
    Function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
  Public
    Class Function CanLoad(Stream: TStream): Boolean; Override;
    Procedure LoadFromStream(Stream: TStream); Override;
    Function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; Override;

    Property BackgroundColor: TColor Read FBackgroundColor;
    Property Transparency: TByteArray Read FTransparency;
  End;
  {$ENDIF} // PortableNetworkGraphic

  // ---------- file format management stuff
  TFormatType = (
    ftAnimation,   // format contains an animation (like GIF or AVI)
    ftLayered,     // format supports multiple layers (like PSP, PSD)
    ftMultiImage,  // format can contain more than one image (like TIF or GIF)
    ftRaster,      // format is contains raster data (this is mainly used)
    ftVector       // format contains vector data (like DXF or PSP file version 4)
  );
  TFormatTypes = Set Of TFormatType;
  TFilterSortType = (
    fstNone,        // do not sort entries, list them as they are registered
    fstBoth,        // sort entries first by description then by extension
    fstDescription, // sort entries by description only
    fstExtension    // sort entries by extension only
  );

  TFilterOption = (
    foCompact,          // use the compact form in filter strings instead listing each extension on a separate line
    foIncludeAll,       // include the 'All image files' filter string
    foIncludeExtension  // add the extension to the description
  );
  TFilterOptions = Set Of TFilterOption;

  // The file format list is an alternative to Delphi's own poor implementation which does neither allow to filter
  // graphic formats nor to build common entries in filter strings nor does it care for duplicate entries or
  // alphabetic ordering. Additionally, some properties are maintained for each format to do searches, filter partiuclar
  // formats for a certain case etc.
  TFileFormatList = Class
  Private
    FClassList,
    FExtensionList: TList;
  Protected
    Function FindExtension(Const Extension: String): Integer;
    Function FindGraphicClass(GraphicClass: TGraphicClass): Integer;
  Public
    constructor Create;
    destructor Destroy; Override;

    Procedure Clear;
    Function GetDescription(Graphic: TGraphicClass): String;
    Procedure GetExtensionList(List: TStrings);
    Function GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType; Options: TFilterOptions;
      GraphicClass: TGraphicClass): String;
    Function GraphicFromExtension(S: String): TGraphicClass;
    Function GraphicFromContent(Const FileName: String): TGraphicExGraphicClass; Overload;
    Function GraphicFromContent(Stream: TStream): TGraphicExGraphicClass; Overload;
    Procedure RegisterFileFormat(Const Extension, Common, Individual: String; FormatTypes: TFormatTypes;
      Replace, RegisterDefault: Boolean; GraphicClass: TGraphicClass);
    Procedure UnregisterFileFormat(Const Extension: String; GraphicClass: TGraphicClass);
  End;

  // resampling support types
  TResamplingFilter = (sfBox, sfTriangle, sfHermite, sfBell, sfSpline, sfLanczos3, sfMitchell);

// Resampling support routines
Procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap); Overload;
Procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap); Overload;

Var
  FileFormatList: TFileFormatList;

//----------------------------------------------------------------------------------------------------------------------

Implementation

Uses
  Consts, Math, MZLib;

Type
  // resampling support types
  TRGBInt = Record
   R, G, B: Integer;
  End;

  PRGBWord = ^TRGBWord;
  TRGBWord = Record
   R, G, B: Word;
  End;

  PRGBAWord = ^TRGBAWord;
  TRGBAWord = Record
   R, G, B, A: Word;
  End;

  PBGR = ^TBGR;
  TBGR = Packed Record
   B, G, R: Byte;
  End;

  PBGRA = ^TBGRA;
  TBGRA = Packed Record
   B, G, R, A: Byte;
  End;

  PRGB = ^TRGB;
  TRGB = Packed Record
   R, G, B: Byte;
  End;

  PRGBA = ^TRGBA;
  TRGBA = Packed Record
   R, G, B, A: Byte;
  End;

  PPixelArray = ^TPixelArray;
  TPixelArray = Array[0..0] Of TBGR;

  TFilterFunction = Function(Value: Single): Single;

  // contributor for a Pixel
  PContributor = ^TContributor;
  TContributor = Record
   Weight: Integer; // Pixel Weight
   Pixel: Integer; // Source Pixel
  End;

  TContributors = Array Of TContributor;

  // list of source pixels contributing to a destination pixel
  TContributorEntry = Record
   N: Integer;
   Contributors: TContributors;
  End;

  TContributorList = Array Of TContributorEntry;

Const
  DefaultFilterRadius: Array[TResamplingFilter] Of Single = (0.5, 1, 1, 1.5, 2, 3, 2);

ThreadVar // globally used cache for current image (speeds up resampling about 10%)
  CurrentLineR: Array Of Integer;
  CurrentLineG: Array Of Integer;
  CurrentLineB: Array Of Integer;

//----------------------------------------------------------------------------------------------------------------------

Procedure GraphicExError(ErrorString: String); Overload;

Begin
  Raise EInvalidGraphic.Create(ErrorString);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure GraphicExError(ErrorString: String; Args: Array Of Const); Overload;

Begin
  Raise EInvalidGraphic.CreateFmt(ErrorString, Args);
End;

//----------------------------------------------------------------------------------------------------------------------

// TODO : Pointer arithmetics
procedure Upsample(Width, Height, ScaledWidth: Cardinal; Pixels: PByte);

// Creates a new image that is a integral size greater than an existing one.

Var
  X, Y: Cardinal;
  P, Q, R: PByte;

Begin
  For Y := 0 To Height - 1 Do
  Begin
    P := Pixels + (Height - 1 - Y) * ScaledWidth + (Width - 1);
    Q := Pixels + ((Height - 1 - Y) Shl 1) * ScaledWidth + ((Width - 1) Shl 1);
    Q^ := P^;
    (Q + 1)^ := P^;
    For X := 1 To Width - 1 Do
    Begin
      Dec(P);
      Dec(Q, 2);
      Q^ := P^;
      (Q + 1)^ := Byte((Word(P^) + Word((P + 1)^) + 1) shr 1);
    End;
  end;

  For Y := 0 To Height - 2 Do
  Begin
    P := Pixels + (Y Shl 1) * ScaledWidth;
    Q := P + ScaledWidth;
    R := Q + ScaledWidth;
    For X := 0 To Width - 2 Do
    Begin
      Q^ := Byte((Word(P^) + Word(R^) + 1) shr 1);
      (Q + 1)^ := Byte((Word(P^) + Word((P + 2)^) + Word(R^) + Word((R + 2)^) + 2) shr 2);
      Inc(Q, 2);
      Inc(P, 2);
      Inc(R, 2);
    End;
    Q^ := Byte((Word(P^) + Word(R^) + 1) shr 1);
    Inc(P);
    Inc(Q);
    Q^ := Byte((Word(P^) + Word(R^) + 1) shr 1);
  End;
  P := Pixels + (2 * Height - 2) * ScaledWidth;
  Q := Pixels + (2 * Height - 1) * ScaledWidth;
  Move(P^, Q^, 2 * Width);
End;

//----------------- filter functions for stretching --------------------------------------------------------------------

Function HermiteFilter(Value: Single): Single;

// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1

Begin
  If Value < 0 Then Value := -Value;
  If Value < 1 Then Result := (2 * Value - 3) * Sqr(Value) + 1
               Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function BoxFilter(Value: Single): Single;

// This filter is also known as 'nearest neighbour' Filter.

Begin
  If (Value > -0.5) And (Value <= 0.5) Then Result := 1
                                       Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TriangleFilter(Value: Single): Single;

// aka 'linear' or 'bilinear' filter

Begin
  If Value < 0 Then Value := -Value;
  If Value < 1 Then Result := 1 - Value
               Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function BellFilter(Value: Single): Single;

Begin
  If Value < 0 Then Value := -Value;
  If Value < 0.5 Then Result := 0.75 - Sqr(Value)
                 Else
    If Value < 1.5 Then
    Begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    End
    Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function SplineFilter(Value: Single): Single;

// B-spline filter

Var
  Temp: Single;

Begin
  If Value < 0 Then Value := -Value;
  If Value < 1 Then
  Begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2 / 3;
  End
  Else
    If Value < 2 Then
    Begin
      Value := 2 - Value;
      Result := Sqr(Value) * Value / 6;
    End
    Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function Lanczos3Filter(Value: Single): Single;

  //--------------- local function --------------------------------------------

  Function SinC(Value: Single): Single;

  Begin
    If Value <> 0 Then
    Begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    End
    Else Result := 1;
  End;

  //---------------------------------------------------------------------------

Begin
  If Value < 0 Then Value := -Value;
  If Value < 3 Then Result := SinC(Value) * SinC(Value / 3)
               Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function MitchellFilter(Value: Single): Single;

Const
  B = 1 / 3;
  C = 1 / 3;

Var Temp: Single;

Begin
  If Value < 0 Then Value := -Value;
  Temp := Sqr(Value);
  If Value < 1 Then
  Begin
    Value := (((12 - 9 * B - 6 * C) * (Value * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := Value / 6;
  End
  Else
    If Value < 2 Then
    Begin
      Value := (((-B - 6 * C) * (Value * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * Value)
               + (8 * B + 24 * C));
      Result := Value / 6;
    End
    Else Result := 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Const
  FilterList: Array[TResamplingFilter] Of TFilterFunction = (
    BoxFilter,
    TriangleFilter,
    HermiteFilter,
    BellFilter,
    SplineFilter,
    Lanczos3Filter,
    MitchellFilter
  );

//----------------------------------------------------------------------------------------------------------------------

Procedure FillLineChache(N, Delta: Integer; Line: Pointer);

Var
  I: Integer;
  Run: PBGR;

Begin
  Run := Line;
  For I := 0 To N - 1 Do
  Begin
    CurrentLineR[I] := Run.R;
    CurrentLineG[I] := Run.G;
    CurrentLineB[I] := Run.B;
    Inc(PByte(Run), Delta);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function ApplyContributors(N: Integer; Contributors: TContributors): TBGR;

Var
  J: Integer;
  RGB: TRGBInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: ^TContributor;

Begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  Total := 0;
  Contr := @Contributors[0];
  For J := 0 To N - 1 Do
  Begin
    Weight := Contr.Weight;
    Inc(Total, Weight);
    Pixel := Contr.Pixel;
    Inc(RGB.r, CurrentLineR[Pixel] * Weight);
    Inc(RGB.g, CurrentLineG[Pixel] * Weight);
    Inc(RGB.b, CurrentLineB[Pixel] * Weight);

    Inc(Contr);
  End;

  If Total = 0 Then
  Begin
    Result.R := ClampByte(RGB.R Shr 8);
    Result.G := ClampByte(RGB.G Shr 8);
    Result.B := ClampByte(RGB.B Shr 8);
  End
  Else
  Begin
    Result.R := ClampByte(RGB.R Div Total);
    Result.G := ClampByte(RGB.G Div Total);
    Result.B := ClampByte(RGB.B Div Total);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure DoStretch(Filter: TFilterFunction; Radius: Single; Source, Target: TBitmap);

// This is the actual scaling routine. Target must be allocated already with sufficient size. Source must
// contain valid data, Radius must not be 0 and Filter must not be nil.

Var
  ScaleX,
  ScaleY: Single;  // Zoom scale factors
  I, J,
  K, N: Integer; // Loop variables
  Center: Single; // Filter calculation variables
  Width: Single;
  Weight: Integer;  // Filter calculation variables
  Left,
  Right: Integer; // Filter calculation variables
  Work: TBitmap;
  ContributorList: TContributorList;
  SourceLine,
  DestLine: PPixelArray;
  DestPixel: PBGR;
  Delta,
  DestDelta: Integer;
  SourceHeight,
  SourceWidth,
  TargetHeight,
  TargetWidth: Integer;

Begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;

  If (SourceHeight = 0) Or (SourceWidth = 0) Or
     (TargetHeight = 0) Or (TargetWidth = 0) Then Exit;

  // create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  Try
    Work.Canvas.Lock;
    Try
      Work.PixelFormat := pf24Bit;
      Work.Height := SourceHeight;
      Work.Width := TargetWidth;
      If SourceWidth = 1 Then ScaleX :=  TargetWidth / SourceWidth
                         Else ScaleX :=  (TargetWidth - 1) / (SourceWidth - 1);
      If (SourceHeight = 1) Or (TargetHeight = 1) Then ScaleY :=  TargetHeight / SourceHeight
                                                  Else ScaleY :=  (TargetHeight - 1) / (SourceHeight - 1);

      // pre-calculate filter contributions for a row
      SetLength(ContributorList, TargetWidth);
      // horizontal sub-sampling
      If ScaleX < 1 Then
      Begin
        // scales from bigger to smaller Width
        Width := Radius / ScaleX;
        For I := 0 To TargetWidth - 1 Do
        Begin
          ContributorList[I].N := 0;
          SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
          Center := I / ScaleX;
          Left := Floor(Center - Width);
          Right := Ceil(Center + Width);
          For J := Left To Right Do
          Begin
            Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
            If Weight <> 0 Then
            Begin
              If J < 0 Then N := -J
                       Else
                If J >= SourceWidth Then N := SourceWidth - J + SourceWidth - 1
                                    Else N := J;
              K := ContributorList[I].N;
              Inc(ContributorList[I].N);
              ContributorList[I].Contributors[K].Pixel := N;
              ContributorList[I].Contributors[K].Weight := Weight;
            End;
          End;
        End;
      End
      Else
      Begin
        // horizontal super-sampling
        // scales from smaller to bigger Width
        For I := 0 To TargetWidth - 1 Do
        Begin
          ContributorList[I].N := 0;
          SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
          Center := I / ScaleX;
          Left := Floor(Center - Radius);
          Right := Ceil(Center + Radius);
          For J := Left To Right Do
          Begin
            Weight := Round(Filter(Center - J) * 256);
            If Weight <> 0 Then
            Begin
              If J < 0 Then N := -J
                       Else
               If J >= SourceWidth Then N := SourceWidth - J + SourceWidth - 1
                                   Else N := J;
              K := ContributorList[I].N;
              Inc(ContributorList[I].N);
              ContributorList[I].Contributors[K].Pixel := N;
              ContributorList[I].Contributors[K].Weight := Weight;
            End;
          End;
        End;
      End;

      // now apply filter to sample horizontally from Src to Work
      SetLength(CurrentLineR, SourceWidth);
      SetLength(CurrentLineG, SourceWidth);
      SetLength(CurrentLineB, SourceWidth);
      For K := 0 To SourceHeight - 1 Do
      Begin
        SourceLine := Source.ScanLine[K];
        FillLineChache(SourceWidth, 3, SourceLine);
        DestPixel := Work.ScanLine[K];
        For I := 0 To TargetWidth - 1 Do
          With ContributorList[I] Do
          Begin
            DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
            // move on to next column
            Inc(DestPixel);
          End;
      End;

      // free the memory allocated for horizontal filter weights, since we need the stucture again
      For I := 0 To TargetWidth - 1 Do ContributorList[I].Contributors := Nil;
      ContributorList := Nil;

      // pre-calculate filter contributions for a column
      SetLength(ContributorList, TargetHeight);
      // vertical sub-sampling
      If ScaleY < 1 Then
      Begin
        // scales from bigger to smaller height
        Width := Radius / ScaleY;
        For I := 0 To TargetHeight - 1 Do
        Begin
          ContributorList[I].N := 0;
          SetLength(ContributorList[I].Contributors, Trunc(2 * Width + 1));
          Center := I / ScaleY;
          Left := Floor(Center - Width);
          Right := Ceil(Center + Width);
          For J := Left To Right Do
          Begin
            Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
            If Weight <> 0 Then
            Begin
              If J < 0 Then N := -J
                       Else
                If J >= SourceHeight Then N := SourceHeight - J + SourceHeight - 1
                                     Else N := J;
              K := ContributorList[I].N;
              Inc(ContributorList[I].N);
              ContributorList[I].Contributors[K].Pixel := N;
              ContributorList[I].Contributors[K].Weight := Weight;
            End;
          End;
        End
      End
      Else
      Begin
        // vertical super-sampling
        // scales from smaller to bigger height
        For I := 0 To TargetHeight - 1 Do
        Begin
          ContributorList[I].N := 0;
          SetLength(ContributorList[I].Contributors, Trunc(2 * Radius + 1));
          Center := I / ScaleY;
          Left := Floor(Center - Radius);
          Right := Ceil(Center + Radius);
          For J := Left To Right Do
          Begin
            Weight := Round(Filter(Center - J) * 256);
            If Weight <> 0 Then
            Begin
              If J < 0 Then N := -J
                       Else
                If J >= SourceHeight Then N := SourceHeight - J + SourceHeight - 1
                                     Else N := J;
              K := ContributorList[I].N;
              Inc(ContributorList[I].N);
              ContributorList[I].Contributors[K].Pixel := N;
              ContributorList[I].Contributors[K].Weight := Weight;
            End;
          End;
        End;
      End;

      // apply filter to sample vertically from Work to Target
      SetLength(CurrentLineR, SourceHeight);
      SetLength(CurrentLineG, SourceHeight);
      SetLength(CurrentLineB, SourceHeight);


      SourceLine := Work.ScanLine[0];
      Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
      DestLine := Target.ScanLine[0];
      DestDelta := Integer(Target.ScanLine[1]) - Integer(DestLine);
      For K := 0 To TargetWidth - 1 Do
      Begin
        DestPixel := Pointer(DestLine);
        FillLineChache(SourceHeight, Delta, SourceLine);
        For I := 0 To TargetHeight - 1 Do
          With ContributorList[I] Do
          Begin
            DestPixel^ := ApplyContributors(N, ContributorList[I].Contributors);
            Inc(Integer(DestPixel), DestDelta);
          End;
        Inc(SourceLine);
        Inc(DestLine);
      End;

      // free the memory allocated for vertical filter weights
      For I := 0 To TargetHeight - 1 Do ContributorList[I].Contributors := Nil;
      // this one is done automatically on exit, but is here for completeness
      ContributorList := Nil;
    Finally
      Work.Canvas.Unlock;
    End;
  Finally
    Work.Free;
    CurrentLineR := Nil;
    CurrentLineG := Nil;
    CurrentLineB := Nil;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap);

// Scales the source bitmap to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).

Begin
  If Radius = 0 Then Radius := DefaultFilterRadius[Filter];
  Target.Handle := 0;
  Target.PixelFormat := pf24Bit;
  Target.Width := NewWidth;
  Target.Height := NewHeight;
  Source.PixelFormat := pf24Bit;
  DoStretch(FilterList[Filter], Radius, Source, Target);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap);

Var
  Target: TBitmap;

Begin
  If Radius = 0 Then Radius := DefaultFilterRadius[Filter];
  Target := TBitmap.Create;
  Try
    Target.Canvas.Lock;
    Source.Canvas.Lock;
    Try
      Target.PixelFormat := pf24Bit;
      Target.Width := NewWidth;
      Target.Height := NewHeight;
      Source.PixelFormat := pf24Bit;
      DoStretch(FilterList[Filter], Radius, Source, Target);
      Source.Assign(Target);
    Finally
      Source.Canvas.Unlock;
      Target.Canvas.Unlock;
    End;
  Finally
    Target.Free;
  End;
End;

//----------------- support functions for image loading ----------------------------------------------------------------

procedure SwapShort(P: PWord; Count: Cardinal);

// swaps high and low byte of 16 bit values
// EAX contains P, EDX contains Count

ASM
@@Loop:
              MOV CX, [EAX]
              XCHG CH, CL
              MOV [EAX], CX
              Add EAX, 2
              Dec EDX
              JNZ @@Loop
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure SwapLong(P: PInteger; Count: Cardinal); Overload;

// swaps high and low bytes of 32 bit values
// EAX contains P, EDX contains Count

ASM
@@Loop:
              MOV ECX, [EAX]
              BSWAP ECX
              MOV [EAX], ECX
              Add EAX, 4
              Dec EDX
              JNZ @@Loop
End;

//----------------------------------------------------------------------------------------------------------------------

Function SwapLong(Value: Cardinal): Cardinal; Overload;

// swaps high and low bytes of the given 32 bit value

ASM
              BSWAP EAX
End;

//----------------- various conversion routines ------------------------------------------------------------------------

Procedure Depredict1(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

ASM
@@1:
              MOV CL, [EAX]
              Add [EAX + 1], CL
              Inc EAX
              Dec EDX
              JNZ @@1
End;

//----------------------------------------------------------------------------------------------------------------------

procedure Depredict3(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

ASM
              MOV ECX, EDX
              Shl ECX, 1
              Add ECX, EDX         // 3 * Count
@@1:
              MOV DL, [EAX]
              Add [EAX + 3], DL
              Inc EAX
              Dec ECX
              JNZ @@1
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure Depredict4(P: Pointer; Count: Cardinal);

// EAX contains P and EDX Count

ASM
              Shl EDX, 2          // 4 * Count
@@1:
              MOV CL, [EAX]
              Add [EAX + 4], CL
              Inc EAX
              Dec EDX
              JNZ @@1
End;

//----------------- TGraphicExGraphic ----------------------------------------------------------------------------------

Constructor TGraphicExGraphic.Create;

Begin
  Inherited;
  FColorManager := TColorManager.Create;
End;

//----------------------------------------------------------------------------------------------------------------------

Destructor TGraphicExGraphic.Destroy;

Begin
  FColorManager.Free;
  Inherited;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TGraphicExGraphic.Assign(Source: TPersistent);

Begin
  If Source Is TGraphicExGraphic Then FImageProperties := TGraphicExGraphic(Source).FImageProperties;
  Inherited;
End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TGraphicExGraphic.CanLoad(Const FileName: String): Boolean;

Var
  Stream: TFileStream;

Begin
  Stream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    Result := CanLoad(Stream);
  Finally
    Stream.Free;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TGraphicExGraphic.CanLoad(Stream: TStream): Boolean;

// Descentants have to override this method and return True if they consider the data in Stream
// as loadable by the particular class.
// Note: Make sure the stream position is the same on exit as it was on enter!

Begin
  Result := False;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TGraphicExGraphic.LoadFromResourceID(Instance: THandle; ResID: Integer);

Var
  Stream: TResourceStream;

Begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TGraphicExGraphic.LoadFromResourceName(Instance: THandle; Const ResName: String);

Var
  Stream: TResourceStream;

Begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TGraphicExGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// Descentants must override this method to fill in the actual values.
// Result is always False to show there is no image to load.

Begin
  Finalize(FImageProperties);
  ZeroMemory(@FImageProperties, SizeOf(FImageProperties));
  FImageProperties.FileGamma := 1;
  Result := False;
End;

//----------------- TAutodeskGraphic -----------------------------------------------------------------------------------

{$IFDEF AutodeskGraphic}

Type
  TAutodeskHeader = Packed Record
    Width,
    Height,
    XCoord,
    YCoord: Word;
    Depth,
    Compression: Byte;
    DataSize: Cardinal;
    Reserved: Array[0..15] Of Byte;
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TAutodeskGraphic.CanLoad(Stream: TStream): Boolean;

Var
  FileID: Word;
  Header: TAutodeskHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    Result := (Size - Position) > (SizeOf(FileID) + SizeOf(Header));
    If Result Then
    Begin
      LastPosition := Position;
      Read(FileID, SizeOf(FileID));
      Result := FileID = $9119;
      If Result Then
      Begin
        // read image dimensions
        Read(Header, SizeOf(Header));
        Result := (Header.Depth = 8) And (Header.Compression = 0);
      End;
      Position := LastPosition;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TAutodeskGraphic.LoadFromStream(Stream: TStream);

Var
  FileID: Word;
  FileHeader: TAutodeskHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;

  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream Do
    Begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      Read(FileID, 2);

      // read image dimensions
      Read(FileHeader, SizeOf(FileHeader));

      // read palette entries and create a palette
      ZeroMemory(@LogPalette, SizeOf(LogPalette));
      LogPalette.palVersion := $300;
      LogPalette.palNumEntries := 256;
      For I := 0 To 255 Do
      Begin
        Read(LogPalette.palPalEntry[I], 3);
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[I].peBlue Shl 2;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[I].peGreen Shl 2;
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[I].peRed Shl 2;
      End;

      // setup bitmap properties
      PixelFormat := pf8Bit;
      Palette := CreatePalette(PLogPalette(@LogPalette)^);
      Width := FileHeader.Width;
      Height := FileHeader.Height;
      // finally read image data
      For I := 0 To Height - 1 Do
      Begin
        Read(Scanline[I]^, FileHeader.Width);

        Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      End;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
  End
  Else GraphicExError(gesInvalidImage, ['Autodesk']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TAutodeskGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  FileID: Word;
  Header: TAutodeskHeader;

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    Read(FileID, 2);
    If FileID = $9119 Then
    Begin
      // read image dimensions
      Read(Header, SizeOf(Header));

      ColorScheme := csIndexed;
      Width := Header.Width;
      Height := Header.Height;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := 8;
      Compression := ctNone;

      Result := True;
    End;
  End;
End;

{$ENDIF} // AutodeskGraphic

//----------------- TSGIGraphic ----------------------------------------------------------------------------------------

{$IFDEF SGIGraphic}

Const
  SGIMagic = 474;

  SGI_COMPRESSION_VERBATIM = 0;
  SGI_COMPRESSION_RLE = 1;

Type
  TSGIHeader = Packed Record
    Magic: SmallInt;         // IRIS image file magic number
    Storage,                 // Storage format
    BPC: Byte;               // Number of bytes per pixel channel (1 or 2)
    Dimension: Word;         // Number of dimensions
                             //   1 - one single scanline (and one channel) of length XSize
                             //   2 - two dimensional (one channel) of size XSize x YSize
                             //   3 - three dimensional (ZSize channels) of size XSize x YSize
    XSize,                   // width of image
    YSize,                   // height of image
    ZSize: Word;             // number of channels/planes in image (3 for RGB, 4 for RGBA etc.)
    PixMin,                  // Minimum pixel value
    PixMax: Cardinal;        // Maximum pixel value
    Dummy: Cardinal;         // ignored
    ImageName: array[0..79] of AnsiChar;
    ColorMap: Integer;       // Colormap ID
                             //  0 - default, almost all images are stored with this flag
                             //  1 - dithered, only one channel of data (pixels are packed), obsolete
                             //  2 - screen (palette) image, obsolete
                             //  3 - no image data, palette only, not displayable
    Dummy2: Array[0..403] Of Byte; // ignored
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TSGIGraphic.CanLoad(Stream: TStream): Boolean;

// returns True if the data in Stream represents a graphic which can be loaded by this class

Var
  Header: TSGIHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    Result := (Size - Position) > SizeOf(TSGIHeader);
    If Result Then
    Begin
      LastPosition := Position;
      ReadBuffer(Header, SizeOf(Header));
      // one number as check is too unreliable, hence we take some more fields into the check
      Result := (Swap(Header.Magic) = SGIMagic) And
                (Header.BPC In [1, 2]) And
                (Swap(Header.Dimension) In [1..3]);
      Position := LastPosition;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TSGIGraphic.ReadAndDecode(Red, Green, Blue, Alpha: Pointer; Row, BPC: Cardinal);

Var
  Count: Cardinal;
  RawBuffer: Pointer;

Begin
  With FStream, FImageProperties Do
    // compressed image?
    If Assigned(FDecoder) Then
    Begin
      If Assigned(Red) Then
      Begin
        Position := FBasePosition + FRowStart[Row + 0 * Height];
        Count := BPC * FRowSize[Row + 0 * Height];
        GetMem(RawBuffer, Count);
        Try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Red, Count, Width);
        Finally
          If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        End;
      End;

      If Assigned(Green) Then
      Begin
        Position := FBasePosition + FRowStart[Row + 1 * Height];
        Count := BPC * FRowSize[Row + 1 * Height];
        GetMem(RawBuffer, Count);
        Try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Green, Count, Width);
        Finally
          If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        End;
      End;

      If Assigned(Blue) Then
      Begin
        Position := FBasePosition + FRowStart[Row + 2 * Height];
        Count := BPC * FRowSize[Row + 2 * Height];
        GetMem(RawBuffer, Count);
        Try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Blue, Count, Width);
        Finally
          If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        End;
      End;

      If Assigned(Alpha) Then
      Begin
        Position := FBasePosition + FRowStart[Row + 3 * Height];
        Count := BPC * FRowSize[Row + 3 * Height];
        GetMem(RawBuffer, Count);
        Try
          Read(RawBuffer^, Count);
          FDecoder.Decode(RawBuffer, Alpha, Count, Width);
        Finally
          If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        End;
      End;
    End
    Else
    Begin
      If Assigned(Red) Then
      Begin
        Position := FBasePosition + 512 + (Row * Width);
        Read(Red^, BPC * Width);
      End;

      If Assigned(Green) Then
      Begin
        Position := FBasePosition + 512 + (Row * Width) + (Width * Height);
        Read(Green^, BPC * Width);
      End;

      If Assigned(Blue) Then
      Begin
        Position := FBasePosition + 512 + (Row * Width) + (2 * Width * Height);
        Read(Blue^, BPC * Width);
      End;

      If Assigned(Alpha) Then
      Begin
        Position := FBasePosition + 512 + (Row * Width) + (3 * Width * Height);
        Read(Alpha^, BPC * Width);
      End;
    End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TSGIGraphic.LoadFromStream(Stream: TStream);

Var
  Y: Cardinal;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Header: TSGIHeader;
  Count: Cardinal;

Begin
  // free previous image
  Handle := 0;

  // keep stream reference and start position for seek operations
  FStream := Stream;
  FBasePosition := Stream.Position;

  If ReadImageProperties(Stream, 0) Then
  Begin
    With FImageProperties, Stream Do
    Begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Stream.Position := FBasePosition;
      // read header again, we need some additional information
      ReadBuffer(Header, SizeOf(Header));

      // SGI images are always stored in big endian style
      ColorManager.SourceOptions := [coNeedByteSwap];
      With Header Do ColorMap := SwapLong(ColorMap);

      If Compression = ctRLE Then
      Begin
        Count := Height * SamplesPerPixel;
        SetLength(FRowStart, Count);
        SetLength(FRowSize, Count);
        // read line starts and sizes from stream
        Read(FRowStart[0], Count * SizeOf(Cardinal));
        SwapLong(@FRowStart[0], Count);
        Read(FRowSize[0], Count * SizeOf(Cardinal));
        SwapLong(@FRowSize[0], Count);
        FDecoder := TSGIRLEDecoder.Create(BitsPerSample);
      End
      Else
      Begin
        FDecoder := Nil;
      End;

      // set pixel format before size to avoid possibly large conversion operation
      With ColorManager Do
      Begin
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := 8;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        Case ColorScheme Of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
        Else
          TargetColorScheme := csIndexed;
        End;
        PixelFormat := TargetPixelFormat;
      End;
      Self.Width := Width;
      Self.Height := Height;

      RedBuffer := Nil;
      GreenBuffer := Nil;
      BlueBuffer := Nil;
      AlphaBuffer := Nil;
      Progress(Self, psEnding, 100, True, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      Try
        Count := (BitsPerPixel Div 8) * Width;
        // read lines and put them into the bitmap
        Case ColorScheme Of
          csRGBA:
            Begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              GetMem(AlphaBuffer, Count);
              For  Y := 0 To Height - 1 Do
              Begin
                ReadAndDecode(RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y, Header.BPC);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer],
                                        ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          csRGB:
            Begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              For  Y := 0 To Height - 1 Do
              Begin
                ReadAndDecode(RedBuffer, GreenBuffer, BlueBuffer, Nil, Y, Header.BPC);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer],
                                        ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
        Else
          // any other format is interpreted as being 256 gray scales
          Palette := ColorManager.CreateGrayscalePalette(False);
          For  Y := 0 To Height - 1 Do
          Begin
            ReadAndDecode(ScanLine[Height - Y - 1], Nil, Nil, Nil, Y, Header.BPC);
            Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          End;
        End;

      Finally
        Progress(Self, psEnding, 100, True, FProgressRect, '');

        If Assigned(RedBuffer) Then FreeMem(RedBuffer);
        If Assigned(GreenBuffer) Then FreeMem(GreenBuffer);
        If Assigned(BlueBuffer) Then FreeMem(BlueBuffer);
        If Assigned(AlphaBuffer) Then FreeMem(AlphaBuffer);
        FDecoder.Free;
      End;
    End;
  End
  Else GraphicExError(gesInvalidImage, ['sgi, bw or rgb(a)']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TSGIGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TSGIHeader;
 
Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With FImageProperties Do
  Begin
    Stream.ReadBuffer(Header, SizeOf(Header));
    If Swap(Header.Magic) = SGIMagic Then
    Begin 
      Options := [ioBigEndian];
      BitsPerSample := Header.BPC * 8;
      Width := Swap(Header.XSize);
      Height := Swap(Header.YSize);
      SamplesPerPixel := Swap(Header.ZSize);
      Case SamplesPerPixel Of
        4:
          ColorScheme := csRGBA;
        3:
          ColorScheme := csRGB;
      Else
        // all other is considered as being 8 bit gray scale
        ColorScheme := csIndexed;
      End;

      BitsPerPixel := BitsPerSample * SamplesPerPixel;
      If Header.Storage = SGI_COMPRESSION_RLE Then Compression := ctRLE
                                              Else Compression := ctNone;
      Result := True;
    End;
  End;
End;

{$ENDIF} // SGIGraphic

//----------------- TTIFFGraphic ---------------------------------------------------------------------------------------

{$IFDEF TIFFGraphic}

Const // TIFF tags
  TIFFTAG_SUBFILETYPE = 254;                     // subfile data descriptor
    FILETYPE_REDUCEDIMAGE = $1;                  // reduced resolution version
    FILETYPE_PAGE = $2;                          // one page of many
    FILETYPE_MASK = $4;                          // transparency mask
  TIFFTAG_OSUBFILETYPE = 255;                    // kind of data in subfile (obsolete by revision 5.0)
    OFILETYPE_IMAGE = 1;                         // full resolution image data
    OFILETYPE_REDUCEDIMAGE = 2;                  // reduced size image data
    OFILETYPE_PAGE = 3;                          // one page of many
  TIFFTAG_IMAGEWIDTH = 256;                      // image width in pixels
  TIFFTAG_IMAGELENGTH = 257;                     // image height in pixels
  TIFFTAG_BITSPERSAMPLE = 258;                   // bits per channel (sample)
  TIFFTAG_COMPRESSION = 259;                     // data compression technique
    COMPRESSION_NONE = 1;                        // dump mode
    COMPRESSION_CCITTRLE = 2;                    // CCITT modified Huffman RLE
    COMPRESSION_CCITTFAX3 = 3;                   // CCITT Group 3 fax encoding
    COMPRESSION_CCITTFAX4 = 4;                   // CCITT Group 4 fax encoding
    COMPRESSION_LZW = 5;                         // Lempel-Ziv & Welch
    COMPRESSION_OJPEG = 6;                       // 6.0 JPEG (old version)
    COMPRESSION_JPEG = 7;                        // JPEG DCT compression (new version)
    COMPRESSION_ADOBE_DEFLATE = 8;               // new id but same as COMPRESSION_DEFLATE
    COMPRESSION_NEXT = 32766;                    // next 2-bit RLE
    COMPRESSION_CCITTRLEW = 32771;               // modified Huffman with word alignment
    COMPRESSION_PACKBITS = 32773;                // Macintosh RLE
    COMPRESSION_THUNDERSCAN = 32809;             // ThunderScan RLE
    // codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
    COMPRESSION_IT8CTPAD = 32895;                // IT8 CT w/padding
    COMPRESSION_IT8LW = 32896;                   // IT8 Linework RLE
    COMPRESSION_IT8MP = 32897;                   // IT8 Monochrome picture
    COMPRESSION_IT8BL = 32898;                   // IT8 Binary line art
    // compression codes 32908-32911 are reserved for Pixar
    COMPRESSION_PIXARFILM = 32908;               // Pixar companded 10bit LZW
    COMPRESSION_PIXARLOG = 32909;                // Pixar companded 11bit ZIP
    COMPRESSION_DEFLATE = 32946;                 // Deflate compression (LZ77)
    // compression code 32947 is reserved for Oceana Matrix <dev@oceana.com>
    COMPRESSION_DCS = 32947;                     // Kodak DCS encoding
    COMPRESSION_JBIG = 34661;                    // ISO JBIG
  TIFFTAG_PHOTOMETRIC = 262;                     // photometric interpretation
    PHOTOMETRIC_MINISWHITE = 0;                  // min value is white
    PHOTOMETRIC_MINISBLACK = 1;                  // min value is black
    PHOTOMETRIC_RGB = 2;                         // RGB color model
    PHOTOMETRIC_PALETTE = 3;                     // color map indexed
    PHOTOMETRIC_MASK = 4;                        // holdout mask
    PHOTOMETRIC_SEPARATED = 5;                   // color separations
    PHOTOMETRIC_YCBCR = 6;                       // CCIR 601
    PHOTOMETRIC_CIELAB = 8;                      // 1976 CIE L*a*b*
  TIFFTAG_THRESHHOLDING = 263;                   // thresholding used on data (obsolete by revision 5.0)
    THRESHHOLD_BILEVEL = 1;                      // b&w art scan
    THRESHHOLD_HALFTONE = 2;                     // or dithered scan
    THRESHHOLD_ERRORDIFFUSE = 3;                 // usually floyd-steinberg
  TIFFTAG_CELLWIDTH = 264;                       // dithering matrix width (obsolete by revision 5.0)
  TIFFTAG_CELLLENGTH = 265;                      // dithering matrix height (obsolete by revision 5.0)
  TIFFTAG_FILLORDER = 266;                       // data order within a Byte
    FILLORDER_MSB2LSB = 1;                       // most significant -> least
    FILLORDER_LSB2MSB = 2;                       // least significant -> most
  TIFFTAG_DOCUMENTNAME = 269;                    // name of doc. image is from
  TIFFTAG_IMAGEDESCRIPTION = 270;                // info about image
  TIFFTAG_MAKE = 271;                            // scanner manufacturer name
  TIFFTAG_MODEL = 272;                           // scanner model name/number
  TIFFTAG_STRIPOFFSETS = 273;                    // Offsets to data strips
  TIFFTAG_ORIENTATION = 274;                     // image FOrientation (obsolete by revision 5.0)
    ORIENTATION_TOPLEFT = 1;                     // row 0 top, col 0 lhs
    ORIENTATION_TOPRIGHT = 2;                    // row 0 top, col 0 rhs
    ORIENTATION_BOTRIGHT = 3;                    // row 0 bottom, col 0 rhs
    ORIENTATION_BOTLEFT = 4;                     // row 0 bottom, col 0 lhs
    ORIENTATION_LEFTTOP = 5;                     // row 0 lhs, col 0 top
    ORIENTATION_RIGHTTOP = 6;                    // row 0 rhs, col 0 top
    ORIENTATION_RIGHTBOT = 7;                    // row 0 rhs, col 0 bottom
    ORIENTATION_LEFTBOT = 8;                     // row 0 lhs, col 0 bottom
  TIFFTAG_SAMPLESPERPIXEL = 277;                 // samples per pixel
  TIFFTAG_ROWSPERSTRIP = 278;                    // rows per strip of data
  TIFFTAG_STRIPBYTECOUNTS = 279;                 // bytes counts for strips
  TIFFTAG_MINSAMPLEVALUE = 280;                  // minimum sample value (obsolete by revision 5.0)
  TIFFTAG_MAXSAMPLEVALUE = 281;                  // maximum sample value (obsolete by revision 5.0)
  TIFFTAG_XRESOLUTION = 282;                     // pixels/resolution in x
  TIFFTAG_YRESOLUTION = 283;                     // pixels/resolution in y
  TIFFTAG_PLANARCONFIG = 284;                    // storage organization
    PLANARCONFIG_CONTIG = 1;                     // single image plane
    PLANARCONFIG_SEPARATE = 2;                   // separate planes of data
  TIFFTAG_PAGENAME = 285;                        // page name image is from
  TIFFTAG_XPOSITION = 286;                       // x page offset of image lhs
  TIFFTAG_YPOSITION = 287;                       // y page offset of image lhs
  TIFFTAG_FREEOFFSETS = 288;                     // byte offset to free block (obsolete by revision 5.0)
  TIFFTAG_FREEBYTECOUNTS = 289;                  // sizes of free blocks (obsolete by revision 5.0)
  TIFFTAG_GRAYRESPONSEUNIT = 290;                // gray scale curve accuracy
    GRAYRESPONSEUNIT_10S = 1;                    // tenths of a unit
    GRAYRESPONSEUNIT_100S = 2;                   // hundredths of a unit
    GRAYRESPONSEUNIT_1000S = 3;                  // thousandths of a unit
    GRAYRESPONSEUNIT_10000S = 4;                 // ten-thousandths of a unit
    GRAYRESPONSEUNIT_100000S = 5;                // hundred-thousandths
  TIFFTAG_GRAYRESPONSECURVE = 291;               // gray scale response curve
  TIFFTAG_GROUP3OPTIONS = 292;                   // 32 flag bits
    GROUP3OPT_2DENCODING = $1;                   // 2-dimensional coding
    GROUP3OPT_UNCOMPRESSED = $2;                 // data not compressed
    GROUP3OPT_FILLBITS = $4;                     // fill to byte boundary
  TIFFTAG_GROUP4OPTIONS = 293;                   // 32 flag bits
    GROUP4OPT_UNCOMPRESSED = $2;                 // data not compressed
  TIFFTAG_RESOLUTIONUNIT = 296;                  // units of resolutions
    RESUNIT_NONE = 1;                            // no meaningful units
    RESUNIT_INCH = 2;                            // english
    RESUNIT_CENTIMETER = 3;                      // metric
  TIFFTAG_PAGENUMBER = 297;                      // page numbers of multi-page
  TIFFTAG_COLORRESPONSEUNIT = 300;               // color curve accuracy
    COLORRESPONSEUNIT_10S = 1;                   // tenths of a unit
    COLORRESPONSEUNIT_100S = 2;                  // hundredths of a unit
    COLORRESPONSEUNIT_1000S = 3;                 // thousandths of a unit
    COLORRESPONSEUNIT_10000S = 4;                // ten-thousandths of a unit
    COLORRESPONSEUNIT_100000S = 5;               // hundred-thousandths
  TIFFTAG_TRANSFERFUNCTION = 301;                // colorimetry info
  TIFFTAG_SOFTWARE = 305;                        // name & release
  TIFFTAG_DATETIME = 306;                        // creation date and time
  TIFFTAG_ARTIST = 315;                          // creator of image
  TIFFTAG_HOSTCOMPUTER = 316;                    // machine where created
  TIFFTAG_PREDICTOR = 317;                       // prediction scheme w/ LZW
    PREDICTION_NONE = 1;                         // no prediction scheme used before coding
    PREDICTION_HORZ_DIFFERENCING = 2;            // horizontal differencing prediction scheme used
  TIFFTAG_WHITEPOINT = 318;                      // image white point
  TIFFTAG_PRIMARYCHROMATICITIES = 319;           // primary chromaticities
  TIFFTAG_COLORMAP = 320;                        // RGB map for pallette image
  TIFFTAG_HALFTONEHINTS = 321;                   // highlight+shadow info
  TIFFTAG_TILEWIDTH = 322;                       // rows/data tile
  TIFFTAG_TILELENGTH = 323;                      // cols/data tile
  TIFFTAG_TILEOFFSETS = 324;                     // offsets to data tiles
  TIFFTAG_TILEBYTECOUNTS = 325;                  // Byte counts for tiles
  TIFFTAG_BADFAXLINES = 326;                     // lines w/ wrong pixel count
  TIFFTAG_CLEANFAXDATA = 327;                    // regenerated line info
    CLEANFAXDATA_CLEAN = 0;                      // no errors detected
    CLEANFAXDATA_REGENERATED = 1;                // receiver regenerated lines
    CLEANFAXDATA_UNCLEAN = 2;                    // uncorrected errors exist
  TIFFTAG_CONSECUTIVEBADFAXLINES = 328;          // max consecutive bad lines
  TIFFTAG_SUBIFD = 330;                          // subimage descriptors
  TIFFTAG_INKSET = 332;                          // inks in separated image
    INKSET_CMYK = 1;                             // cyan-magenta-yellow-black
  TIFFTAG_INKNAMES = 333;                        // ascii names of inks
  TIFFTAG_DOTRANGE = 336;                        // 0% and 100% dot codes
  TIFFTAG_TARGETPRINTER = 337;                   // separation target
  TIFFTAG_EXTRASAMPLES = 338;                    // info about extra samples
    EXTRASAMPLE_UNSPECIFIED = 0;                 // unspecified data
    EXTRASAMPLE_ASSOCALPHA = 1;                  // associated alpha data
    EXTRASAMPLE_UNASSALPHA = 2;                  // unassociated alpha data
  TIFFTAG_SAMPLEFORMAT = 339;                    // data sample format
    SAMPLEFORMAT_UINT = 1;                       // unsigned integer data
    SAMPLEFORMAT_INT = 2;                        // signed integer data
    SAMPLEFORMAT_IEEEFP = 3;                     // IEEE floating point data
    SAMPLEFORMAT_VOID = 4;                       // untyped data
  TIFFTAG_SMINSAMPLEVALUE = 340;                 // variable MinSampleValue
  TIFFTAG_SMAXSAMPLEVALUE = 341;                 // variable MaxSampleValue
  TIFFTAG_JPEGTABLES = 347;                      // JPEG table stream

  // Tags 512-521 are obsoleted by Technical Note #2 which specifies a revised JPEG-in-TIFF scheme.

  TIFFTAG_JPEGPROC = 512;                        // JPEG processing algorithm
    JPEGPROC_BASELINE = 1;                       // baseline sequential
    JPEGPROC_LOSSLESS = 14;                      // Huffman coded lossless
  TIFFTAG_JPEGIFOFFSET = 513;                    // Pointer to SOI marker
  TIFFTAG_JPEGIFBYTECOUNT = 514;                 // JFIF stream length
  TIFFTAG_JPEGRESTARTINTERVAL = 515;             // restart interval length
  TIFFTAG_JPEGLOSSLESSPREDICTORS = 517;          // lossless proc predictor
  TIFFTAG_JPEGPOINTTRANSFORM = 518;              // lossless point transform
  TIFFTAG_JPEGQTABLES = 519;                     // Q matrice offsets
  TIFFTAG_JPEGDCTABLES = 520;                    // DCT table offsets
  TIFFTAG_JPEGACTABLES = 521;                    // AC coefficient offsets
  TIFFTAG_YCBCRCOEFFICIENTS = 529;               // RGB -> YCbCr transform
  TIFFTAG_YCBCRSUBSAMPLING = 530;                // YCbCr subsampling factors
  TIFFTAG_YCBCRPOSITIONING = 531;                // subsample positioning
    YCBCRPOSITION_CENTERED = 1;                  // as in PostScript Level 2
    YCBCRPOSITION_COSITED = 2;                   // as in CCIR 601-1
  TIFFTAG_REFERENCEBLACKWHITE = 532;             // colorimetry info
  // tags 32952-32956 are private tags registered to Island Graphics
  TIFFTAG_REFPTS = 32953;                        // image reference points
  TIFFTAG_REGIONTACKPOINT = 32954;               // region-xform tack point
  TIFFTAG_REGIONWARPCORNERS = 32955;             // warp quadrilateral
  TIFFTAG_REGIONAFFINE = 32956;                  // affine transformation mat
  // tags 32995-32999 are private tags registered to SGI
  TIFFTAG_MATTEING = 32995;                      // use ExtraSamples
  TIFFTAG_DATATYPE = 32996;                      // use SampleFormat
  TIFFTAG_IMAGEDEPTH = 32997;                    // z depth of image
  TIFFTAG_TILEDEPTH = 32998;                     // z depth/data tile

  // tags 33300-33309 are private tags registered to Pixar
  //
  // TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH
  // are set when an image has been cropped out of a larger image.
  // They reflect the size of the original uncropped image.
  // The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used
  // to determine the position of the smaller image in the larger one.

  TIFFTAG_PIXAR_IMAGEFULLWIDTH = 33300;          // full image size in x
  TIFFTAG_PIXAR_IMAGEFULLLENGTH = 33301;         // full image size in y
  // tag 33405 is a private tag registered to Eastman Kodak
  TIFFTAG_WRITERSERIALNUMBER = 33405;            // device serial number
  // tag 33432 is listed in the 6.0 spec w/ unknown ownership
  TIFFTAG_COPYRIGHT = 33432;                     // copyright string
  // 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com)
  TIFFTAG_IT8SITE = 34016;                       // site name
  TIFFTAG_IT8COLORSEQUENCE = 34017;              // color seq. [RGB,CMYK,etc]
  TIFFTAG_IT8HEADER = 34018;                     // DDES Header
  TIFFTAG_IT8RASTERPADDING = 34019;              // raster scanline padding
  TIFFTAG_IT8BITSPERRUNLENGTH = 34020;           // # of bits in short run
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH = 34021;   // # of bits in long run
  TIFFTAG_IT8COLORTABLE = 34022;                 // LW colortable
  TIFFTAG_IT8IMAGECOLORINDICATOR = 34023;        // BP/BL image color switch
  TIFFTAG_IT8BKGCOLORINDICATOR = 34024;          // BP/BL bg color switch
  TIFFTAG_IT8IMAGECOLORVALUE = 34025;            // BP/BL image color value
  TIFFTAG_IT8BKGCOLORVALUE = 34026;              // BP/BL bg color value
  TIFFTAG_IT8PIXELINTENSITYRANGE = 34027;        // MP pixel intensity value
  TIFFTAG_IT8TRANSPARENCYINDICATOR = 34028;      // HC transparency switch
  TIFFTAG_IT8COLORCHARACTERIZATION = 34029;      // color character. table
  // tags 34232-34236 are private tags registered to Texas Instruments
  TIFFTAG_FRAMECOUNT = 34232;                    // Sequence Frame Count
  // tag 34750 is a private tag registered to Pixel Magic
  TIFFTAG_JBIGOPTIONS = 34750;                   // JBIG options
  // tags 34908-34914 are private tags registered to SGI
  TIFFTAG_FAXRECVPARAMS = 34908;                 // encoded class 2 ses. parms
  TIFFTAG_FAXSUBADDRESS = 34909;                 // received SubAddr string
  TIFFTAG_FAXRECVTIME = 34910;                   // receive time (secs)
  // tag 65535 is an undefined tag used by Eastman Kodak
  TIFFTAG_DCSHUESHIFTVALUES = 65535;             // hue shift correction data

  // The following are 'pseudo tags' that can be used to control codec-specific functionality.
  // These tags are not written to file.  Note that these values start at $FFFF + 1 so that they'll
  // never collide with Aldus-assigned tags.

  TIFFTAG_FAXMODE = 65536;                       // Group 3/4 format control
    FAXMODE_CLASSIC = $0000;                     // default, include RTC
    FAXMODE_NORTC = $0001;                       // no RTC at end of data
    FAXMODE_NOEOL = $0002;                       // no EOL code at end of row
    FAXMODE_BYTEALIGN = $0004;                   // Byte align row
    FAXMODE_WORDALIGN = $0008;                   // Word align row
    FAXMODE_CLASSF = FAXMODE_NORTC;              // TIFF class F
  TIFFTAG_JPEGQUALITY = 65537;                   // compression quality level
  // Note: quality level is on the IJG 0-100 scale.  Default value is 75
  TIFFTAG_JPEGCOLORMODE = 65538;                 // Auto RGB<=>YCbCr convert?
    JPEGCOLORMODE_RAW = $0000;                   // no conversion (default)
    JPEGCOLORMODE_RGB = $0001;                   // do auto conversion
  TIFFTAG_JPEGTABLESMODE = 65539;                // What to put in JPEGTables
    JPEGTABLESMODE_QUANT = $0001;                // include quantization tbls
    JPEGTABLESMODE_HUFF = $0002;                 // include Huffman tbls
  // Note: default is JPEGTABLESMODE_QUANT or JPEGTABLESMODE_HUFF
  TIFFTAG_FAXFILLFUNC = 65540;                   // G3/G4 fill function
  TIFFTAG_PIXARLOGDATAFMT = 65549;               // PixarLogCodec I/O data sz
    PIXARLOGDATAFMT_8BIT = 0;                    // regular u_char samples
    PIXARLOGDATAFMT_8BITABGR = 1;                // ABGR-order u_chars
    PIXARLOGDATAFMT_11BITLOG = 2;                // 11-bit log-encoded (raw)
    PIXARLOGDATAFMT_12BITPICIO = 3;              // as per PICIO (1.0==2048)
    PIXARLOGDATAFMT_16BIT = 4;                   // signed short samples
    PIXARLOGDATAFMT_FLOAT = 5;                   // IEEE float samples
  // 65550-65556 are allocated to Oceana Matrix <dev@oceana.com>
  TIFFTAG_DCSIMAGERTYPE = 65550;                 // imager model & filter
    DCSIMAGERMODEL_M3 = 0;                       // M3 chip (1280 x 1024)
    DCSIMAGERMODEL_M5 = 1;                       // M5 chip (1536 x 1024)
    DCSIMAGERMODEL_M6 = 2;                       // M6 chip (3072 x 2048)
    DCSIMAGERFILTER_IR = 0;                      // infrared filter
    DCSIMAGERFILTER_MONO = 1;                    // monochrome filter
    DCSIMAGERFILTER_CFA = 2;                     // color filter array
    DCSIMAGERFILTER_OTHER = 3;                   // other filter
  TIFFTAG_DCSINTERPMODE = 65551;                 // interpolation mode
    DCSINTERPMODE_NORMAL = $0;                   // whole image, default
    DCSINTERPMODE_PREVIEW = $1;                  // preview of image (384x256)
  TIFFTAG_DCSBALANCEARRAY = 65552;               // color balance values
  TIFFTAG_DCSCORRECTMATRIX = 65553;              // color correction values
  TIFFTAG_DCSGAMMA = 65554;                      // gamma value
  TIFFTAG_DCSTOESHOULDERPTS = 65555;             // toe & shoulder points
  TIFFTAG_DCSCALIBRATIONFD = 65556;              // calibration file desc
  // Note: quality level is on the ZLIB 1-9 scale. Default value is -1
  TIFFTAG_ZIPQUALITY = 65557;                    // compression quality level
  TIFFTAG_PIXARLOGQUALITY = 65558;               // PixarLog uses same scale

  // TIFF data types
  TIFF_NOTYPE = 0;                               // placeholder
  TIFF_BYTE = 1;                                 // 8-bit unsigned integer
  TIFF_ASCII = 2;                                // 8-bit bytes w/ last byte null
  TIFF_SHORT = 3;                                // 16-bit unsigned integer
  TIFF_LONG = 4;                                 // 32-bit unsigned integer
  TIFF_RATIONAL = 5;                             // 64-bit unsigned fraction
  TIFF_SBYTE = 6;                                // 8-bit signed integer
  TIFF_UNDEFINED = 7;                            // 8-bit untyped data
  TIFF_SSHORT = 8;                               // 16-bit signed integer
  TIFF_SLONG = 9;                                // 32-bit signed integer
  TIFF_SRATIONAL = 10;                           // 64-bit signed fraction
  TIFF_FLOAT = 11;                               // 32-bit IEEE floating point
  TIFF_DOUBLE = 12;                              // 64-bit IEEE floating point

  TIFF_BIGENDIAN = $4D4D;
  TIFF_LITTLEENDIAN = $4949;

  TIFF_VERSION = 42;

Type
  TTIFFHeader = Packed Record
    ByteOrder: Word;
    Version: Word;
    FirstIFD: Cardinal;
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TTIFFGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TTIFFHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      LastPosition := Position;

      Stream.ReadBuffer(Header, SizeOf(Header));
      Result := (Header.ByteOrder = TIFF_BIGENDIAN) Or
                (Header.ByteOrder = TIFF_LITTLEENDIAN);
      If Result Then
      Begin
        If Header.ByteOrder = TIFF_BIGENDIAN Then
        Begin
          Header.Version := Swap(Header.Version);
          Header.FirstIFD := SwapLong(Header.FirstIFD);
        End;

        Result := (Header.Version = TIFF_VERSION) And (Integer(Header.FirstIFD) < (Size - Integer(LastPosition)));
      End;
      Position := LastPosition;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTIFFGraphic.FindTag(Tag: Cardinal; Var Index: Cardinal): Boolean;

// looks through the currently loaded IFD for the entry indicated by Tag;
// returns True and the index of the entry in Index if the entry is there
// otherwise the result is False and Index undefined
// Note: The IFD is sorted so we can use a binary search here.

Var
  L, H, I, C: Integer;

Begin
  Result := False;
  L := 0;
  H := High(FIFD);
  While L <= H Do
  Begin
    I := (L + H) Shr 1;
    C := Integer(FIFD[I].Tag) - Integer(Tag);
    If C < 0 Then L := I + 1
             Else
    Begin
      H := I - 1;
      If C = 0 Then
      Begin
        Result := True;
        L := I;
      End;
    End;
  End;
  Index := L;
End;

//----------------------------------------------------------------------------------------------------------------------

Const
  DataTypeToSize: Array[TIFF_NOTYPE..TIFF_SLONG] Of Byte = (0, 1, 1, 2, 4, 8, 1, 1, 2, 4);

Procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TByteArray);

// returns the values of the IFD entry indicated by Tag

Var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

Begin
  Values := Nil;
  If FindTag(Tag, Index) And
     (FIFD[Index].DataLength > 0) Then
  Begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    If Value <= 4 Then
    Begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      Value := FIFD[Index].Offset;
      For I := 0 To FIFD[Index].DataLength - 1 Do
      Begin
        Case FIFD[Index].DataType Of
          TIFF_BYTE:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        End;
        Value := Value Shr Shift;
      End;
    End
    Else
    Begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // bytes sized data can be read directly instead of looping through the array
      If FIFD[Index].DataType In [TIFF_BYTE, TIFF_ASCII, TIFF_SBYTE, TIFF_UNDEFINED]
        Then Stream.Read(Values[0], Value)
        Else
      Begin
        For I := 0 To High(Values) Do
        Begin
          Stream.Read(Value, DataTypeToSize[FIFD[Index].DataType]);
          Case FIFD[Index].DataType Of
            TIFF_BYTE:
              Value := Byte(Value);
            TIFF_SHORT,
            TIFF_SSHORT:
              Begin
                If ioBigEndian In FImageProperties.Options Then Value := Swap(Word(Value))
                                                           Else Value := Word(Value);
              End;
            TIFF_LONG,
            TIFF_SLONG:
              If ioBigEndian In FImageProperties.Options Then Value := SwapLong(Value);
          End;
          Values[I] := Value;
        End;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TCardinalArray);

// returns the values of the IFD entry indicated by Tag

Var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

Begin
  Values := Nil;
  If FindTag(Tag, Index) And
     (FIFD[Index].DataLength > 0) Then
  Begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    If Value <= 4 Then
    Begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      Value := FIFD[Index].Offset;
      For I := 0 To FIFD[Index].DataLength - 1 Do
      Begin
        Case FIFD[Index].DataType Of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        End;
        Value := Value Shr Shift;
      End;
    End
    Else
    Begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to cardinals
      For I := 0 To High(Values) Do
      Begin
        Stream.Read(Value, DataTypeToSize[FIFD[Index].DataType]);
        Case FIFD[Index].DataType Of
          TIFF_BYTE:
            Value := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            Begin
              If ioBigEndian In FImageProperties.Options Then Value := Swap(Word(Value))
                                                         Else Value := Word(Value);
            End;
          TIFF_LONG,
          TIFF_SLONG:
            If ioBigEndian In FImageProperties.Options Then Value := SwapLong(Value);
        End;
        Values[I] := Value;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.GetValueList(Stream: TStream; Tag: Cardinal; Var Values: TFloatArray);

// returns the values of the IFD entry indicated by Tag

Var
  Index,
  Shift,
  IntValue: Cardinal;
  Value: Single;
  I: Integer;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

Begin
  Values := Nil;
  If FindTag(Tag, Index) And
     (FIFD[Index].DataLength > 0) Then
  Begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[FIFD[Index].DataType] * FIFD[Index].DataLength;

    // data fits into one cardinal -> extract it
    If Value <= 4 Then
    Begin
      Shift := DataTypeToSize[FIFD[Index].DataType] * 8;
      IntValue := FIFD[Index].Offset;
      For I := 0 To FIFD[Index].DataLength - 1 Do
      Begin
        Case FIFD[Index].DataType Of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED:
            Values[I] := Byte(IntValue);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(IntValue);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := IntValue;
        End;
        IntValue := IntValue Shr Shift;
      End;
    End
    Else
    Begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to Single
      For I := 0 To High(Values) Do
      Begin
        Case FIFD[Index].DataType Of
          TIFF_BYTE:
            Begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              Value := Byte(IntValue);
            End;
          TIFF_SHORT,
          TIFF_SSHORT:
            Begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              If ioBigEndian In FImageProperties.Options Then Value := Swap(Word(IntValue))
                                                         Else Value := Word(IntValue);
            End;
          TIFF_LONG,
          TIFF_SLONG:
            Begin
              Stream.Read(IntValue, DataTypeToSize[FIFD[Index].DataType]);
              If ioBigEndian In FImageProperties.Options Then Value := SwapLong(IntValue);
            End;
          TIFF_RATIONAL,
          TIFF_SRATIONAL:
            Begin
              Stream.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
              Stream.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
              If ioBigEndian In FImageProperties.Options Then
              Begin
                FloatNominator := SwapLong(Cardinal(FloatNominator));
                FloatDenominator := SwapLong(Cardinal(FloatDenominator));
              End;
              Value := FloatNominator / FloatDenominator;
            End;
          TIFF_FLOAT:
            Begin
              Stream.ReadBuffer(IntNominator, SizeOf(IntNominator));
              Stream.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
              If ioBigEndian In FImageProperties.Options Then
              Begin
                IntNominator := SwapLong(IntNominator);
                IntDenominator := SwapLong(IntDenominator);
              End;
              Value := IntNominator / IntDenominator;
            End;
          End;
        Values[I] := Value;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTIFFGraphic.GetValue(Stream: TStream; Tag: Cardinal; Default: Single = 0): Single;

// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there

Var
  Index: Cardinal;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

Begin
  Result := Default;
  If FindTag(Tag, Index) Then
  Begin
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    If FIFD[Index].DataLength = 1 Then
    Begin
      Case FIFD[Index].DataType Of
        TIFF_BYTE:
          Result := Byte(FIFD[Index].Offset);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(FIFD[Index].Offset);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          Result := FIFD[Index].Offset;
        TIFF_RATIONAL,
        TIFF_SRATIONAL:
          Begin
            Stream.Position := FBasePosition + FIFD[Index].Offset;
            Stream.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
            Stream.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
            If ioBigEndian In FImageProperties.Options Then
            Begin
              FloatNominator := SwapLong(Cardinal(FloatNominator));
              FloatDenominator := SwapLong(Cardinal(FloatDenominator));
            End;
            Result := FloatNominator / FloatDenominator;
          End;
        TIFF_FLOAT:
          Begin
            Stream.Position := FBasePosition + FIFD[Index].Offset;
            Stream.ReadBuffer(IntNominator, SizeOf(IntNominator));
            Stream.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
            If ioBigEndian In FImageProperties.Options Then
            Begin
              IntNominator := SwapLong(IntNominator);
              IntDenominator := SwapLong(IntDenominator);
            End;
            Result := IntNominator / IntDenominator;
          End;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTIFFGraphic.GetValue(Tag: Cardinal; Default: Cardinal = 0): Cardinal;

// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there

Var
  Index: Cardinal;

Begin
  If Not FindTag(Tag, Index) Then Result := Default
                             Else
  Begin
    Result := FIFD[Index].Offset;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    If FIFD[Index].DataLength = 1 Then
    Begin
      Case FIFD[Index].DataType Of
        TIFF_BYTE:
          Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          ;
      Else
        Result := Default;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTIFFGraphic.GetValue(Tag: Cardinal; Var Size: Cardinal; Default: Cardinal): Cardinal;

// Returns the value of the IFD entry indicated by Tag or the default value if the entry is not there.
// If the tag exists then also the data size is returned.

Var
  Index: Cardinal;

Begin
  If Not FindTag(Tag, Index) Then
  Begin
    Result := Default;
    Size := 0;
  End
  Else
  Begin
    Result := FIFD[Index].Offset;
    Size := FIFD[Index].DataLength;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    If FIFD[Index].DataLength = 1 Then
    Begin
      Case FIFD[Index].DataType Of
        TIFF_BYTE:
          Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT:
          Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: // nothing to do
          ;
      Else
        Result := Default;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.SortIFD;

// Although all entries in the IFD should be sorted there are still files where this is not the case.
// Because the lookup for certain tags in the IFD uses binary search it must be made sure the IFD is
// sorted (what we do here).

  //--------------- local function --------------------------------------------

  Procedure QuickSort(L, R: Integer);

  Var
    I, J, M: Integer;
    T: TIFDEntry;

  Begin
    Repeat
      I := L;
      J := R;
      M := (L + R) Shr 1;
      Repeat
        While FIFD[I].Tag < FIFD[M].Tag Do Inc(I);
        While FIFD[J].Tag > FIFD[M].Tag Do Dec(J);
        If I <= J Then
        Begin
          T := FIFD[I];
          FIFD[I] := FIFD[J];
          FIFD[J] := T;
          Inc(I);
          Dec(J);
        End;
      Until I > J;
      If L < J Then QuickSort(L, J);
      L := I;
    Until I >= R;
  End;

  //--------------- end local functions ---------------------------------------

Begin
  QuickSort(0, High(FIFD));
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.SwapIFD;

// swap the member fields of all entries of the currently loaded IFD from big endian to little endian

Var
  I: Integer;
  Size: Cardinal;

Begin
  For I := 0 To High(FIFD) Do
    With FIFD[I] Do
    Begin
      Tag := Swap(Tag);
      DataType := Swap(DataType);
      DataLength := SwapLong(DataLength);
      
      // determine whether the data fits into 4 bytes
      Size := DataTypeToSize[FIFD[I].DataType] * FIFD[I].DataLength;
      If Size >= 4 Then Offset := SwapLong(Offset)
                   Else
        Case DataType Of
          TIFF_SHORT,
          TIFF_SSHORT:
            If DataLength > 1 Then Offset := SwapLong(Offset)
                              Else Offset := Swap(Word(Offset));
        End;
    End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.LoadFromStream(Stream: TStream);

Var
  IFDCount: Word;
  Buffer: Pointer;
  Run: PByte;
  Pixels,
  EncodedData,
  DataPointerCopy: Pointer;
  Offsets,
  ByteCounts: TCardinalArray;
  ColorMap: Cardinal;

  StripSize: Cardinal;
  Decoder: TDecoder;

  // dynamically assigned handler
  Deprediction: Procedure(P: Pointer; Count: Cardinal);

Begin
  Handle := 0;
  Deprediction := Nil;
  Decoder := Nil;

  // we need to keep the current stream position because all position information
  // are relative to this one
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With FImageProperties Do
    Try
      // tiled images aren't supported
      If ioTiled In Options Then Exit;
      
      FProgressRect := Rect(0, 0, 0, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // read data of the first image file directory (IFD)
      Stream.Position := FBasePosition + FirstIFD;
      Stream.ReadBuffer(IFDCount, SizeOf(IFDCount));
      If ioBigEndian In Options Then IFDCount := Swap(IFDCount);
      SetLength(FIFD, IFDCount);
      Stream.ReadBuffer(FIFD[0], IFDCount * SizeOf(TIFDEntry));
      If ioBigEndian In Options Then SwapIFD;
      SortIFD;

      // --- read the data of the directory which are needed to actually load the image:

      // data organization
      GetValueList(Stream, TIFFTAG_STRIPOFFSETS, Offsets);
      GetValueList(Stream, TIFFTAG_STRIPBYTECOUNTS, ByteCounts);

      // retrive additional tile data if necessary
      If ioTiled In Options Then
      Begin
        GetValueList(Stream, TIFFTAG_TILEOFFSETS, Offsets);
        GetValueList(Stream, TIFFTAG_TILEBYTECOUNTS, ByteCounts);
      End;

      // determine pixelformat and setup color conversion
      With ColorManager Do
      Begin
        If ioBigEndian In Options Then SourceOptions := [coNeedByteSwap]
                                  Else SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        If SourceBitsPerSample = 16 Then TargetBitsPerSample := 8
                                    Else TargetBitsPerSample := SourceBitsPerSample;

        // the JPEG lib does internally a conversion to RGB
        If Compression In [ctOJPEG, ctJPEG] Then SourceColorScheme := csBGR
                                            Else SourceColorScheme := ColorScheme;

        Case SourceColorScheme Of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
          csCMY,
          csCMYK,
          csCIELab,
          csYCbCr:
            TargetColorScheme := csBGR;
          csIndexed:
            Begin
              If HasAlpha Then SourceColorScheme := csGA; // fake indexed images with alpha (used in EPS)
                                                          // as being grayscale with alpha
              TargetColorScheme := csIndexed;
            End;
        Else
          TargetColorScheme := SourceColorScheme;
        End;

        SourceSamplesPerPixel := SamplesPerPixel;
        If SourceColorScheme = csCMYK Then TargetSamplesPerPixel := 3
                                      Else TargetSamplesPerPixel := SamplesPerPixel;
        If SourceColorScheme = csCIELab Then SourceOptions := SourceOptions + [coLabByteRange];

        If SourceColorScheme = csGA Then PixelFormat := pf8Bit
                                    Else PixelFormat := TargetPixelFormat;
      End;
      
      // now that the pixel format is set we can also set the (possibly large) image dimensions
      Self.Width := Width;
      Self.Height := Height;
      If (Width = 0) Or (Height = 0) Then GraphicExError(gesInvalidImage, ['TIF/TIFF']);

      FProgressRect.Right := Width;
      If ColorManager.TargetColorScheme In [csIndexed, csG, csGA] Then
      Begin
        // load palette data and build palette
        If ColorManager.TargetColorScheme = csIndexed Then
        Begin
          ColorMap := GetValue(TIFFTAG_COLORMAP, StripSize, 0);
          If StripSize > 0 Then
          Begin
            Stream.Position := FBasePosition + ColorMap;
            // number of palette entries is also given by the color map tag
            // (3 components each (r,g,b) and two bytes per component)
            Stream.ReadBuffer(FPalette[0] , 2 * StripSize);
            Palette := ColorManager.CreateColorPalette([@FPalette[0], @FPalette[StripSize Div 3],
                                                       @FPalette[2 * StripSize Div 3]], pfPlane16Triple, StripSize, False);
          End;
        End
        Else Palette := ColorManager.CreateGrayScalePalette(ioMinIsWhite In Options);
      End
      Else
        If ColorManager.SourceColorScheme = csYCbCr Then
          ColorManager.SetYCbCrParameters(FYCbCrCoefficients, YCbCrSubSampling[0], YCbCrSubSampling[1]);

      // intermediate buffer for data
      BytesPerLine := (BitsPerPixel * Width + 7) Div 8;

      // determine prediction scheme
      If Compression <> ctNone Then
      Begin
        // Prediction without compression makes no sense at all (as it is to improve
        // compression ratios). Appearently there are image which are uncompressed but still
        // have a prediction scheme set. Hence we must check for it.
        Case Predictor Of
          PREDICTION_HORZ_DIFFERENCING: // currently only one prediction scheme is defined
            Case SamplesPerPixel Of
              4:
                Deprediction := Depredict4;
              3:
                Deprediction := Depredict3;
            Else
              Deprediction := Depredict1;
            End;
        End;
      End;
      
      // create decompressor for the image
      Case Compression Of
        ctNone:
          ;
        {$IFDEF UseLZW}
        ctLZW:
          Decoder := TTIFFLZWDecoder.Create;
        {$ENDIF}
        ctPackedBits:
          Decoder := TPackbitsRLEDecoder.Create;
        ctFaxRLE,
        ctFaxRLEW:
          Decoder := TCCITTMHDecoder.Create(GetValue(TIFFTAG_GROUP3OPTIONS),
                                            ioReversed In Options,
                                            Compression = ctFaxRLEW,
                                            Width);
        ctFax3:
          Decoder := TCCITTFax3Decoder.Create(GetValue(TIFFTAG_GROUP3OPTIONS), ioReversed In Options, False, Width);
        ctJPEG:
          Begin
            // some extra work is needed for JPEG
            GetValueList(Stream, TIFFTAG_JPEGTABLES, JPEGTables);

            Decoder := TTIFFJPEGDecoder.Create(@FImageProperties);
          End;
        ctThunderscan:
          Decoder := TThunderDecoder.Create(Width);
        ctLZ77:
          Decoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, True);
      Else
        {
        COMPRESSION_OJPEG,
        COMPRESSION_CCITTFAX4
        COMPRESSION_NEXT
        COMPRESSION_IT8CTPAD
        COMPRESSION_IT8LW
        COMPRESSION_IT8MP
        COMPRESSION_IT8BL
        COMPRESSION_PIXARFILM
        COMPRESSION_PIXARLOG
        COMPRESSION_DCS
        COMPRESSION_JBIG}
        GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'TIF/TIFF']);
      End;

      If Assigned(Decoder) Then Decoder.DecodeInit;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
 
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      // go for each strip in the image (which might contain more than one line)
      CurrentRow := 0;
      CurrentStrip := 0;
      StripCount := Length(Offsets);
      While CurrentStrip < StripCount Do
      Begin
        Stream.Position := FBasePosition + Offsets[CurrentStrip];
        If CurrentStrip < Length(RowsPerStrip) Then StripSize := BytesPerLine * RowsPerStrip[CurrentStrip]
                                               Else StripSize := BytesPerLine * RowsPerStrip[High(RowsPerStrip)];

        GetMem(Buffer, StripSize);
        Run := Buffer;
        Try
          // decompress strip if necessary
          If Assigned(Decoder) Then
          Begin
            GetMem(EncodedData, ByteCounts[CurrentStrip]);
            Try
              DataPointerCopy := EncodedData;
              Stream.Read(EncodedData^, ByteCounts[CurrentStrip]);
              // need pointer copies here because they could get modified
              // while decoding
              Decoder.Decode(DataPointerCopy, Pointer(Run), ByteCounts[CurrentStrip], StripSize);
            Finally
              If Assigned(EncodedData) Then FreeMem(EncodedData);
            End;
          End
          Else
          Begin
            Stream.Read(Buffer^, StripSize);
          End;

          Run := Buffer;
          // go for each line (row) in the strip
          while (CurrentRow < Height) and ((integer(Run) - integer(Buffer)) < Integer(StripSize)) do
          Begin
            Pixels := ScanLine[CurrentRow];
            // depredict strip if necessary
            If Assigned(Deprediction) Then Deprediction(Run, Width - 1);
            // any color conversion comes last
            ColorManager.ConvertRow([Run], Pixels, Width, $FF);
            Inc(Run, BytesPerLine);
            Inc(CurrentRow);

            Progress(Self, psRunning, MulDiv(CurrentRow, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          End;

        Finally
          If Assigned(Buffer) Then FreeMem(Buffer);
        End;

        Inc(CurrentStrip);
      End;
    Finally
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      If Assigned(Decoder) Then Decoder.DecodeEnd;
      Decoder.Free;
    End;
  End
  Else GraphicExError(gesInvalidImage, ['TIF/TIFF']);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTIFFGraphic.SaveToStream(Stream: TStream);

Begin
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTIFFGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Reads all relevant TIF properties of the image of index ImageIndex (zero based).
// Returns True if the image ImageIndex could be read, otherwise False.

Const
  PhotometricToColorScheme: Array[PHOTOMETRIC_MINISWHITE..PHOTOMETRIC_CIELAB] Of TColorScheme = (
    csG,
    csG,
    csRGBA,
    csIndexed,
    csUnknown,
    csCMYK,
    csYCbCr,
    csUnknown,
    csCIELab
  );

Var
  IFDCount: Word;
  ExtraSamples: TCardinalArray;
  PhotometricInterpretation: Byte;
  TIFCompression: Word;
  Index: Cardinal;
  
  IFDOffset: Cardinal;
  Header: TTIFFHeader;
  LocalBitsPerSample: TCardinalArray;
  
Begin
  // clear image properties
  Result := Inherited ReadImageProperties(Stream, ImageIndex);

  With FImageProperties Do
  Begin
    // rewind stream to header position
    Stream.Position := FBasePosition;

    Stream.ReadBuffer(Header, SizeOf(Header));
    If Header.ByteOrder = TIFF_BIGENDIAN Then
    Begin
      Options := Options + [ioBigEndian];
      Header.Version := Swap(Header.Version);
      Header.FirstIFD := SwapLong(Header.FirstIFD);
    End;

    Version := Header.Version;
    FirstIFD := Header.FirstIFD;
    If Version = TIFF_VERSION Then
    Begin
      IFDOffset := Header.FirstIFD;
      // advance to next IFD until we have the desired one
      Repeat
        Stream.Position := FBasePosition + IFDOffset;
        // number of entries in this directory
        Stream.ReadBuffer(IFDCount, SizeOf(IFDCount));
        If Header.ByteOrder = TIFF_BIGENDIAN Then IFDCount := Swap(IFDCount);

        // if we already have the desired image then get out of here
        If ImageIndex = 0 Then Break;

        Dec(ImageIndex);
        // advance to offset for next IFD
        Stream.Seek(IFDCount * SizeOf(TIFDEntry), soFromCurrent);
        Stream.ReadBuffer(IFDOffset, SizeOf(IFDOffset));
        // no further image available, but the required index is still not found
        If IFDOffset = 0 Then Exit;
      Until False;

      SetLength(FIFD, IFDCount);
      Stream.ReadBuffer(FIFD[0], IFDCount * SizeOf(TIFDEntry));
      If Header.ByteOrder = TIFF_BIGENDIAN Then SwapIFD;
      SortIFD;

      Width := GetValue(TIFFTAG_IMAGEWIDTH);
      Height := GetValue(TIFFTAG_IMAGELENGTH);
      If (Width = 0) Or (Height = 0) Then Exit;

      // data organization
      GetValueList(Stream, TIFFTAG_ROWSPERSTRIP, RowsPerStrip);
      // some images rely on the default size ($FFFFFFFF) if only one stripe is in the image,
      // make sure there's a valid value also in this case
      If (Length(RowsPerStrip) = 0) Or (RowsPerStrip[0] = $FFFFFFFF) Then
      Begin
        SetLength(RowsPerStrip, 1);
        RowsPerStrip[0] := Height;
      End;

      // number of color components per pixel (1 for b&w, 16 and 256 colors, 3 for RGB, 4 for CMYK etc.)
      SamplesPerPixel := GetValue(TIFFTAG_SAMPLESPERPIXEL, 1);

      // number of bits per color component
      GetValueList(Stream, TIFFTAG_BITSPERSAMPLE, LocalBitsPerSample);
      If Length(LocalBitsPerSample) = 0 Then BitsPerSample := 1
                                        Else BitsPerSample := LocalBitsPerSample[0];

      // determine whether image is tiled and retrive tile data if necessary
      TileWidth := GetValue(TIFFTAG_TILEWIDTH, 0);
      TileLength := GetValue(TIFFTAG_TILELENGTH, 0);
      If  (TileWidth > 0) And (TileLength > 0) Then Include(Options, ioTiled);

      // photometric interpretation determines the color space
      PhotometricInterpretation := GetValue(TIFFTAG_PHOTOMETRIC);
      // type of extra information for additional samples per pixel
      GetValueList(Stream, TIFFTAG_EXTRASAMPLES, ExtraSamples);

      // determine whether extra samples must be considered
      HasAlpha := Length(ExtraSamples) > 0;
      // if any of the extra sample contains an invalid value then consider
      // it as being not existant to avoid wrong interpretation for badly
      // written images
      If HasAlpha Then
      Begin
        For Index := 0 To High(ExtraSamples) Do
          If ExtraSamples[Index] > EXTRASAMPLE_UNASSALPHA Then
          Begin
            HasAlpha := False;
            Break;
          End;
      End;

      // currently all bits per sample values are equal
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      // create decompressor for the image
      TIFCompression := GetValue(TIFFTAG_COMPRESSION);
      Case TIFCompression Of
        COMPRESSION_NONE:
          Compression := ctNone;
        COMPRESSION_LZW:
          Compression := ctLZW;
        COMPRESSION_PACKBITS:
          Compression := ctPackedBits;
        COMPRESSION_CCITTRLE:
          Compression := ctFaxRLE;
        COMPRESSION_CCITTRLEW:
          Compression := ctFaxRLEW;
        COMPRESSION_CCITTFAX3:
          Compression := ctFax3;
        COMPRESSION_OJPEG:
          Compression := ctOJPEG;
        COMPRESSION_JPEG:
          Compression := ctJPEG;
        COMPRESSION_CCITTFAX4:
          Compression := ctFax4;
        COMPRESSION_NEXT:
          Compression := ctNext;
        COMPRESSION_THUNDERSCAN:
          Compression := ctThunderscan;
        COMPRESSION_IT8CTPAD:
          Compression := ctIT8CTPAD;
        COMPRESSION_IT8LW:
          Compression := ctIT8LW;
        COMPRESSION_IT8MP:
          Compression := ctIT8MP;
        COMPRESSION_IT8BL:
          Compression := ctIT8BL;
        COMPRESSION_PIXARFILM:
          Compression := ctPixarFilm;
        COMPRESSION_PIXARLOG: // also a LZ77 clone
          Compression := ctPixarLog;
        COMPRESSION_ADOBE_DEFLATE,
        COMPRESSION_DEFLATE: 
          Compression := ctLZ77;
        COMPRESSION_DCS:
          Compression := ctDCS;
        COMPRESSION_JBIG:
          Compression := ctJBIG;
      Else
        Compression := ctUnknown;
      End; 

      If PhotometricInterpretation In [PHOTOMETRIC_MINISWHITE..PHOTOMETRIC_CIELAB] Then
      Begin
        ColorScheme := PhotometricToColorScheme[PhotometricInterpretation];
        If (PhotometricInterpretation = PHOTOMETRIC_RGB) And (SamplesPerPixel < 4) Then ColorScheme := csRGB;
        If PhotometricInterpretation = PHOTOMETRIC_MINISWHITE Then Include(Options, ioMinIsWhite);

        // extra work necessary for YCbCr
        If PhotometricInterpretation = PHOTOMETRIC_YCBCR Then
        Begin
          If FindTag(TIFFTAG_YCBCRSUBSAMPLING, Index)
            Then GetValueList(Stream, TIFFTAG_YCBCRSUBSAMPLING, YCbCrSubSampling)
            Else
            Begin
              // initialize default values if nothing is given in the file
              SetLength(YCbCrSubSampling, 2);
              YCbCrSubSampling[0] := 2;
              YCbCrSubSampling[1] := 2;
            End;
          If FindTag(TIFFTAG_YCBCRPOSITIONING, Index) Then FYCbCrPositioning := GetValue(TIFFTAG_YCBCRPOSITIONING)
                                                      Else FYCbCrPositioning := YCBCRPOSITION_CENTERED;

          If FindTag(TIFFTAG_YCBCRCOEFFICIENTS, Index)
            Then GetValueList(Stream, TIFFTAG_YCBCRCOEFFICIENTS, FYCbCrCoefficients)
            Else
            Begin
              // defaults are from CCIR recommendation 601-1
              SetLength(FYCbCrCoefficients, 3);
              FYCbCrCoefficients[0] := 0.299;
              FYCbCrCoefficients[1] := 0.587;
              FYCbCrCoefficients[2] := 0.114;
            End;
        End;
      End
      Else ColorScheme := csUnknown;

      JPEGColorMode := GetValue(TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RAW);
      JPEGTablesMode := GetValue(TIFFTAG_JPEGTABLESMODE, JPEGTABLESMODE_QUANT Or JPEGTABLESMODE_HUFF);

      PlanarConfig := GetValue(TIFFTAG_PLANARCONFIG);
      // other image properties
      XResolution := GetValue(Stream, TIFFTAG_XRESOLUTION);
      YResolution := GetValue(Stream, TIFFTAG_YRESOLUTION);
      If GetValue(TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH) = RESUNIT_CENTIMETER Then
      Begin
        // Resolution is given in centimeters.
        // Although I personally prefer the metric system over the old english one :-)
        // I still convert to inches because it is an unwritten rule to give image resolutions in dpi.
        XResolution := XResolution * 2.54;
        YResolution := YResolution * 2.54;
      End;

      // determine prediction scheme
      Predictor := GetValue(TIFFTAG_PREDICTOR);

      // determine fill order in bytes
      If GetValue(TIFFTAG_FILLORDER, FILLORDER_MSB2LSB) = FILLORDER_LSB2MSB Then Include(Options, ioReversed);

      // finally show that we found and read an image
      Result := True;
    End;
  End;
End;

//----------------- TEPSGraphic ----------------------------------------------------------------------------------------

{$IFDEF EPSGraphic}

// Note: This EPS implementation does only read embedded pixel graphics in TIF format (preview).
// Credits to:
//   Olaf Stieleke
//   Torsten Pohlmeyer
//   CPS Krohn GmbH
// for providing the base information about how to read the preview image.

Type
  TEPSHeader = Packed Record
    Code: Cardinal;   // alway $C6D3D0C5, if not there then this is not an EPS or it is not a binary EPS
    PSStart,          // Offset PostScript-Code
    PSLen,            // length of PostScript-Code
    MetaPos,          // position of a WMF
    MetaLen,          // length of a WMF 
    TiffPos,          // position of TIFF (preview images should be either WMF or TIF but not both)
    TiffLen: Integer; // length of the TIFF
    Checksum: SmallInt;
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TEPSGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TEPSHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      LastPosition := Position;

      Stream.ReadBuffer(Header, SizeOf(Header));
      Result := (Header.Code = $C6D3D0C5) And
                (Header.TiffPos > Integer(LastPosition) + SizeOf(Header)) And
                (Header.TiffLen > 0);
      Position := LastPosition;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TEPSGraphic.LoadFromStream(Stream: TStream);

Var
  Header: TEPSHeader;

Begin
  Stream.ReadBuffer(Header, SizeOf(Header));
  If Header.Code <> $C6D3D0C5 Then GraphicExError(gesInvalidImage, ['EPS']);
  Stream.Seek(Header.TiffPos - SizeOf(Header), soFromCurrent);
  Inherited;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TEPSGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
End;

{$ENDIF} // EPSGraphic

{$ENDIF} // TIFFGraphic

//----------------- TTargaGraphic --------------------------------------------------------------------------------------

{$IFDEF TargaGraphic}

//  FILE STRUCTURE FOR THE ORIGINAL TRUEVISION TGA FILE
//    FIELD 1: NUMBER OF CHARACTERS IN ID FIELD (1 BYTES)
//    FIELD 2: COLOR MAP TYPE (1 BYTES)
//    FIELD 3: IMAGE TYPE CODE (1 BYTES)
//      = 0  NO IMAGE DATA INCLUDED
//      = 1  UNCOMPRESSED, COLOR-MAPPED IMAGE
//      = 2  UNCOMPRESSED, TRUE-COLOR IMAGE
//      = 3  UNCOMPRESSED, BLACK AND WHITE IMAGE (black and white is actually grayscale)
//      = 9  RUN-LENGTH ENCODED COLOR-MAPPED IMAGE
//      = 10 RUN-LENGTH ENCODED TRUE-COLOR IMAGE
//      = 11 RUN-LENGTH ENCODED BLACK AND WHITE IMAGE
//    FIELD 4: COLOR MAP SPECIFICATION (5 BYTES)
//      4.1: COLOR MAP ORIGIN (2 BYTES)
//      4.2: COLOR MAP LENGTH (2 BYTES)
//      4.3: COLOR MAP ENTRY SIZE (1 BYTES)
//    FIELD 5:IMAGE SPECIFICATION (10 BYTES)
//      5.1: X-ORIGIN OF IMAGE (2 BYTES)
//      5.2: Y-ORIGIN OF IMAGE (2 BYTES)
//      5.3: WIDTH OF IMAGE (2 BYTES)
//      5.4: HEIGHT OF IMAGE (2 BYTES)
//      5.5: IMAGE PIXEL SIZE (1 BYTE)
//      5.6: IMAGE DESCRIPTOR BYTE (1 BYTE)
//        bit 0..3: attribute bits per pixel
//        bit 4..5: image orientation:
//          0: bottom left
//          1: bottom right
//          2: top left
//          3: top right
//        bit 6..7: interleaved flag
//          0: two way (even-odd) interleave (e.g. IBM Graphics Card Adapter), obsolete
//          1: four way interleave (e.g. AT&T 6300 High Resolution), obsolete
//    FIELD 6: IMAGE ID FIELD (LENGTH SPECIFIED BY FIELD 1)
//    FIELD 7: COLOR MAP DATA (BIT WIDTH SPECIFIED BY FIELD 4.3 AND
//             NUMBER OF COLOR MAP ENTRIES SPECIFIED IN FIELD 4.2)
//    FIELD 8: IMAGE DATA FIELD (WIDTH AND HEIGHT SPECIFIED IN FIELD 5.3 AND 5.4)

Const
  TARGA_NO_COLORMAP = 0;
  TARGA_COLORMAP = 1;

  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_BW_IMAGE = 3;
  TARGA_INDEXED_RLE_IMAGE = 9;
  TARGA_TRUECOLOR_RLE_IMAGE = 10;
  TARGA_BW_RLE_IMAGE = 11;

Type
  TTargaHeader = Packed Record
    IDLength,
    ColorMapType,
    ImageType: Byte;
    ColorMapOrigin,
    ColorMapSize: Word;
    ColorMapEntrySize: Byte;
    XOrigin,
    YOrigin,
    Width,
    Height: Word;
    PixelSize: Byte;
    ImageDescriptor: Byte;
  End;


//----------------------------------------------------------------------------------------------------------------------

Class Function TTargaGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TTargaHeader;
  LastPosition: Cardinal;
  
Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      ReadBuffer(Header, SizeOf(Header));
      // Targa images are hard to determine because there is no magic id or something like that.
      // Hence all we can do is to check if all values from the header are within correct limits.
      Result := (Header.ImageType In [TARGA_EMPTY_IMAGE, TARGA_INDEXED_IMAGE, TARGA_TRUECOLOR_IMAGE, TARGA_BW_IMAGE,
                 TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE, TARGA_BW_RLE_IMAGE]) And
                 (Header.ColorMapType In [TARGA_NO_COLORMAP, TARGA_COLORMAP]) And
                 (Header.ColorMapEntrySize In [15, 16, 24, 32]) And
                 (Header.PixelSize In [8, 15, 16, 24, 32]);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTargaGraphic.LoadFromStream(Stream: TStream);

Var
  Run,
  RLEBuffer: PByte;
  I: Integer;
  LineSize: Integer;
  LineBuffer: Pointer;
  ReadLength: Integer;
  LogPalette: TMaxLogPalette;
  Color16: Word;
  Header: TTargaHeader;
  FlipV: Boolean;
  Decoder: TTargaRLEDecoder;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
    With Stream, FImageProperties Do
    Begin
      Stream.Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      Stream.Read(Header, SizeOf(Header));
      FlipV := (Header.ImageDescriptor And $20) <> 0;
      Header.ImageDescriptor := Header.ImageDescriptor And $F;

      // skip image ID
      Seek(Header.IDLength, soFromCurrent);

      With ColorManager Do
      Begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        SourceOptions := [];
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        PixelFormat := TargetPixelFormat;
      End;
      
      If (Header.ColorMapType = TARGA_COLORMAP) Or
         (Header.ImageType In [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE]) Then
      Begin
        If Header.ImageType In [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] Then
          Palette := ColorManager.CreateGrayscalePalette(False)
        Else
        Begin
          LineSize := (Header.ColorMapEntrySize Div 8) * Header.ColorMapSize;
          GetMem(LineBuffer, LineSize);
          Try
            ReadBuffer(LineBuffer^, LineSize);
            Case Header.ColorMapEntrySize Of
              32:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Quad, Header.ColorMapSize, True);
              24:
                Palette := ColorManager.CreateColorPalette([LineBuffer], pfInterlaced8Triple, Header.ColorMapSize, True);
            Else
              With LogPalette Do
              Begin
                // read palette entries and create a palette
                ZeroMemory(@LogPalette, SizeOf(LogPalette));
                palVersion := $300;
                palNumEntries := Header.ColorMapSize;

                // 15 and 16 bits per color map entry (handle both like 555 color format
                // but make 8 bit from 5 bit per color component)
                For I := 0 To Header.ColorMapSize - 1 Do
                Begin
                  Stream.Read(Color16, 2);
                  palPalEntry[I].peBlue := (Color16 And $1F) Shl 3;
                  palPalEntry[I].peGreen := (Color16 And $3E0) Shr 2;
                  palPalEntry[I].peRed := (Color16 And $7C00) Shr 7;
                End;
                Palette := CreatePalette(PLogPalette(@LogPalette)^);
              End;
            End;
          Finally
            If Assigned(LineBuffer) Then FreeMem(LineBuffer);
          End;
        End;
      End;

      Self.Width := Header.Width;
      Self.Height := Header.Height;

      LineSize := Width * (Header.PixelSize Div 8);
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      Case Header.ImageType Of
        TARGA_EMPTY_IMAGE: // nothing to do here
          ;
        TARGA_BW_IMAGE,
        TARGA_INDEXED_IMAGE,
        TARGA_TRUECOLOR_IMAGE:
          Begin
            For I := 0 To Height - 1 Do
            Begin
              If FlipV Then LineBuffer := ScanLine[I]
                       Else LineBuffer := ScanLine[Header.Height - (I + 1)];
              ReadBuffer(LineBuffer^, LineSize);
              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            End;
          End;
        TARGA_BW_RLE_IMAGE,
        TARGA_INDEXED_RLE_IMAGE,
        TARGA_TRUECOLOR_RLE_IMAGE:
          Begin
            RLEBuffer := Nil;
            Decoder := TTargaRLEDecoder.Create(Header.PixelSize);
            Try
              GetMem(RLEBuffer, 2 * LineSize);
              For I := 0 To Height - 1 Do
              Begin
                If FlipV Then LineBuffer := ScanLine[I]
                         Else LineBuffer := ScanLine[Header.Height - (I + 1)];
                ReadLength := Stream.Read(RLEBuffer^, 2 * LineSize);
                Run := RLEBuffer;
                Decoder.Decode(Pointer(Run), LineBuffer, 2 * LineSize, Width);
                Stream.Position := Stream.Position - ReadLength + (Run - RLEBuffer);
                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            Finally
              If Assigned(RLEBuffer) Then FreeMem(RLEBuffer);
              Decoder.Free;
            End;
          End;
      End;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TTargaGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TTargaHeader;

Begin
  Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    Header.ImageDescriptor := Header.ImageDescriptor And $F;

    Width := Header.Width;
    Height := Header.Height;
    BitsPerSample := 8;

    Case Header.PixelSize Of
      8:
        Begin
          If Header.ImageType In [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] Then ColorScheme := csG
                                                                      Else ColorScheme := csIndexed;
          SamplesPerPixel := 1;
        End;
      15,
      16: // actually, 16 bit are meant being 15 bit
        Begin
          ColorScheme := csRGB;
          BitsPerSample := 5;
          SamplesPerPixel := 3;
        End;
      24:
        Begin
          ColorScheme := csRGB;
          SamplesPerPixel := 3;
        End;
      32:
        Begin
          ColorScheme := csRGBA;
          SamplesPerPixel := 4;
        End;
    End;

    BitsPerPixel := SamplesPerPixel * BitsPerSample;
    If Header.ImageType In [TARGA_BW_RLE_IMAGE, TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE]
      Then Compression := ctRLE
      Else Compression := ctNone;

    Width := Header.Width;
    Height := Header.Height;
    Result := True;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTargaGraphic.SaveToStream(Stream: TStream);

Begin
  SaveToStream(Stream, True);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TTargaGraphic.SaveToStream(Stream: TStream; Compressed: Boolean);

// The format of the image to be saved depends on the current properties of the bitmap not
// on the values which may be set in the header during a former load.

Var
  RLEBuffer: Pointer;
  I: Integer;
  LineSize: Integer;
  WriteLength: Cardinal;
  LogPalette: TMaxLogPalette;
  BPP: Byte;
  Header: TTargaHeader;
  Encoder: TTargaRLEDecoder;
  
Begin
  FProgressRect := Rect(0, 0, Width, 1);
  Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
  // prepare color depth
  Case PixelFormat Of
    pf1Bit,
    pf4Bit: // Note: 1 bit and 4 bits per pixel are not supported in the Targa format, an image
            //       with one of these pixel formats is implicitly converted to 256 colors.
      Begin
        PixelFormat := pf8Bit;
        BPP := 1;
      End;
    pf8Bit:
      BPP := 1;
    pf15Bit,
    pf16Bit:
      BPP := 2;
    pf24Bit:
      BPP := 3;
    pf32Bit:
      BPP := 4;
  Else
    BPP := GetDeviceCaps(Canvas.Handle, BITSPIXEL) Div 8;
  End;

  If Not Empty Then
  Begin
    With Header Do
    Begin
      IDLength := 0;
      If BPP = 1 Then ColorMapType := 1
                 Else ColorMapType := 0;
      If Not Compressed Then
        // can't distinct between a B&W and an color indexed image here, so I use always the latter
        If BPP = 1 Then ImageType := TARGA_INDEXED_IMAGE
                   Else ImageType := TARGA_TRUECOLOR_IMAGE
                        Else
        If BPP = 1 Then ImageType := TARGA_INDEXED_RLE_IMAGE
                   Else ImageType := TARGA_TRUECOLOR_RLE_IMAGE;

      ColorMapOrigin := 0;
      // always save entire palette
      ColorMapSize := 256;
      // always save complete color information
      ColorMapEntrySize := 24;
      XOrigin := 0;
      YOrigin := 0;
      Width := Self.Width;
      Height := Self.Height;
      PixelSize := 8 * BPP;
      // if the image is a bottom-up DIB then indicate this in the image descriptor
      If Cardinal(Scanline[0]) > Cardinal(Scanline[1]) Then ImageDescriptor := $20
                                                       Else ImageDescriptor := 0;
    End;
  
    Stream.Write(Header, SizeOf(Header));

    // store color palette if necessary
    If Header.ColorMapType = 1 Then
      With LogPalette Do
      Begin
        // read palette entries
        GetPaletteEntries(Palette, 0, 256, palPalEntry);
        For I := 0 To 255 Do
        Begin
          Stream.Write(palPalEntry[I].peBlue, 1);
          Stream.Write(palPalEntry[I].peGreen, 1);
          Stream.Write(palPalEntry[I].peRed, 1);
        End;
      End;

    LineSize := Width * (Header.PixelSize Div 8);
    Progress(Self, psEnding, 0, False, FProgressRect, '');

    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
    // finally write image data
    If Compressed Then
    Begin
      RLEBuffer := Nil;
      Encoder := TTargaRLEDecoder.Create(Header.PixelSize);
      Try
        GetMem(RLEBuffer, 2 * LineSize);
        For I := 0 To Height - 1 Do
        Begin
          Encoder.Encode(ScanLine[I], RLEBuffer, Width, WriteLength);
          Stream.WriteBuffer(RLEBuffer^, WriteLength);

          Progress(Self, psRunning, 0, False, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        End;
      Finally
        If Assigned(RLEBuffer) Then FreeMem(RLEBuffer);
        Encoder.Free;
      End;
    End
    Else
    Begin
      For I := 0 To Height - 1 Do
      Begin
        Stream.WriteBuffer(ScanLine[I]^, LineSize);

        Progress(Self, psRunning, 0, False, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      End;
    End;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  End;
End;

{$ENDIF} // TargaGraphic

//----------------- TPCXGraphic ----------------------------------------------------------------------------------------

{$IFDEF PCXGraphic}

Type
  TPCXHeader = Record
    FileID: Byte;                      // $0A for PCX files, $CD for SCR files
    Version: Byte;                     // 0: version 2.5; 2: 2.8 with palette; 3: 2.8 w/o palette; 5: version 3
    Encoding: Byte;                    // 0: uncompressed; 1: RLE encoded
    BitsPerPixel: Byte;
    XMin,
    YMin,
    XMax,
    YMax,                              // coordinates of the corners of the image
    HRes,                              // horizontal resolution in dpi
    VRes: Word;                        // vertical resolution in dpi
    ColorMap: Array[0..15] Of TRGB;    // color table
    Reserved,
    ColorPlanes: Byte;                 // color planes (at most 4)
    BytesPerLine,                      // number of bytes of one line of one plane
    PaletteType: Word;                 // 1: color or b&w; 2: gray scale
    Fill: Array[0..57] Of Byte;
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TPCXGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TPCXHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      Result := (Header.FileID In [$0A, $0C]) And
                (Header.Version In [0, 2, 3, 5]) And
                (Header.Encoding In [0, 1]);
      ReadBuffer(Header, SizeOf(Header));
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPCXGraphic.LoadFromStream(Stream: TStream);

Var
  Header: TPCXHeader;

  //--------------- local functions -------------------------------------------

  Procedure MakePalette;

  Var
    PCXPalette: Array[0..255] Of TRGB;
    OldPos: Integer;
    Marker: Byte;

  Begin
    If (Header.Version <> 3) Or (PixelFormat = pf1Bit) Then
    Begin
      Case PixelFormat Of
        pf1Bit:
          Palette := ColorManager.CreateGrayScalePalette(False);
        pf4Bit:
          With Header Do
          Begin
            If paletteType = 2 Then Palette := ColorManager.CreateGrayScalePalette(False)
                               Else Palette := ColorManager.CreateColorPalette([@ColorMap], pfInterlaced8Triple, 16, False);
          End;
        pf8Bit:
          Begin
            OldPos := Stream.Position;
            // 256 colors with 3 components plus one marker byte
            Stream.Position := Stream.Size - 769;
            Stream.Read(Marker, 1);
            If Marker <> $0C Then
            Begin
              // palette ID is wrong, perhaps gray scale?
              If Header.PaletteType = 2 Then Palette := ColorManager.CreateGrayScalePalette(False)
                                        Else ; // ignore palette
            End
            Else
            Begin
              Stream.Read(PCXPalette[0], 768);
              Palette := ColorManager.CreateColorPalette([@PCXPalette], pfInterlaced8Triple, 256, False);
            End;
            Stream.Position := OldPos;
          End;
      End;
    End
    Else
    Begin
      // version 2.8 without palette information, just use the system palette
      // 256 colors will not be correct with this assignment...
      Palette := SystemPalette16;
    End;
  End;

  //--------------- end local functions ---------------------------------------

Var
  PCXSize,
  Size: Cardinal;
  RawBuffer,
  DecodeBuffer: Pointer;
  Run: PByte;
  Plane1,
  Plane2,
  Plane3,
  Plane4: PByte;
  Value,
  Mask: Byte;
  I, J: Integer;
  Line: PByte;
  Increment: Cardinal;
  NewPixelFormat: TPixelFormat;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    Stream.Position := FBasePosition;

    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
    Stream.Read(Header, SizeOf(Header));
    PCXSize := Stream.Size - Stream.Position;
    With Header, FImageProperties Do
    Begin
      If Not (FileID In [$0A, $CD]) Then GraphicExError(gesInvalidImage, ['PCX or SCR']);

      With ColorManager Do
      Begin
        SourceColorScheme := ColorScheme;
        SourceBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        If ColorScheme = csIndexed Then TargetColorScheme := csIndexed
                                   Else TargetColorScheme := csBGR;
        If BitsPerPixel = 2 Then TargetBitsPerSample := 4
                            Else TargetBitsPerSample := BitsPerSample;
        // Note: pixel depths of 2 and 4 bits may not be used with more than one plane
        //       otherwise the image will not show up correctly
        TargetSamplesPerPixel := SamplesPerPixel;
      End;
    
      NewPixelFormat := ColorManager.TargetPixelFormat;
      If NewPixelFormat = pfCustom Then
      Begin
        // there can be a special case comprising 4 planes each with 1 bit
        If (SamplesPerPixel = 4) And (BitsPerPixel = 4) Then NewPixelFormat := pf4Bit
                                                        Else GraphicExError(gesInvalidColorFormat, ['PCX']);
      End;

      PixelFormat := NewPixelFormat;
      // 256 colors palette is appended to the actual PCX data
      If PixelFormat = pf8Bit Then Dec(PCXSize, 769);
      If PixelFormat <> pf24Bit Then MakePalette;

      Self.Width := Width;
      Self.Height := Height;
                                                  
      // adjust alignment of line
      Increment := SamplesPerPixel * Header.BytesPerLine;

      // allocate pixel data buffer and decode data if necessary
      If Compression = ctRLE Then
      Begin
        Size := Increment * Height;
        GetMem(DecodeBuffer, Size);

        GetMem(RawBuffer, PCXSize);
        Try
          Stream.ReadBuffer(RawBuffer^, PCXSize);
          With TPCXRLEDecoder.Create Do
          Try
            Decode(RawBuffer, DecodeBuffer, PCXSize, Size);
          Finally
            Free;
          End;
        Finally
          If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        End;
      End
      Else
      Begin
        GetMem(DecodeBuffer, PCXSize);
        Stream.ReadBuffer(DecodeBuffer^, PCXSize);
      End;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      Try
        Run := DecodeBuffer;

        If (SamplesPerPixel = 4) And (BitsPerPixel = 4) Then
        Begin
          // 4 planes with one bit

          For I := 0 To Height - 1 Do
          Begin
            Plane1 := Run;
            Plane2 := @Run[Increment div 4];
            Plane3 := @Run[2 * (Increment div 4)];
            Plane4 := @Run[3 * (Increment div 4)];

            Line := ScanLine[I];
            // number of bytes to write
            Size := (Width * BitsPerPixel + 7) Div 8;
            Mask := 0;
            While Size > 0 Do
            Begin
              Value := 0;
              For J := 0 To 1 Do
              ASM
                MOV AL, [Value]

                MOV EDX, [Plane4]             // take the 4 MSBs from the 4 runs and build a nibble
                Shl Byte PTR [EDX], 1         // read MSB and prepare next run at the same time
                RCL AL, 1                     // MSB from previous shift is in CF -> move it to AL

                MOV EDX, [Plane3]             // now do the same with the other three runs
                Shl Byte PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane2]
                Shl Byte PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane1]
                Shl Byte PTR [EDX], 1
                RCL AL, 1

                MOV [Value], AL
              End;
              Line^ := Value;
              Inc(Line);
              Dec(Size);

              // two runs above (to construct two nibbles -> one byte), now update marker
              // to know when to switch to next byte in the planes
              Mask := (Mask + 2) Mod 8;
              If Mask = 0 Then
              Begin
                Inc(Plane1);
                Inc(Plane2);
                Inc(Plane3);
                Inc(Plane4);
              End;
            End;
            Inc(Run, Increment);

            Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          End;
        End
        Else
          If PixelFormat = pf24Bit Then
          Begin
            // true color
            For I := 0 To Height - 1 Do
            Begin
              Line := ScanLine[I];
              Plane1 := Run;
              Plane2 := @Run[Increment div 3];
              Plane3 := @Run[2 * (Increment div 3)];
              ColorManager.ConvertRow([Plane1, Plane2, Plane3], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            End
          End
          Else
          Begin
            // other indexed formats
            For I := 0 To Height - 1 Do
            Begin
              Line := ScanLine[I];
              ColorManager.ConvertRow([Run], Line, Width, $FF);
              Inc(Run, Increment);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            End;
          End;
      Finally
        If Assigned(DecodeBuffer) Then FreeMem(DecodeBuffer);
      End;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPCXGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TPCXHeader;

Begin
  Result := Inherited ReadImageProperties(Stream, 0);
  With Stream Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    With FImageProperties Do
    Begin
      If Header.FileID In [$0A, $CD] Then
      Begin
        Width := Header.XMax - Header.XMin + 1;
        Height := Header.YMax - Header.YMin + 1;

        SamplesPerPixel := Header.ColorPlanes;
        BitsPerSample := Header.BitsPerPixel;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        If BitsPerPixel <= 8 Then ColorScheme := csIndexed
                             Else ColorScheme := csRGB;
        If Header.Encoding = 1 Then Compression := ctRLE
                               Else Compression := ctNone;
        XResolution := Header.HRes;
        YResolution := Header.VRes;

        Result := True;
      End;
    End;
  End;
End;

{$ENDIF} // PCXGraphic

//----------------- TPCDGraphic ----------------------------------------------------------------------------------------

{$IFDEF PCDGraphic}

Const
  PCD_BEGIN_BASE16 = 8192;
  PCD_BEGIN_BASE4 = 47104;
  PCD_BEGIN_BASE = 196608;
  PCD_BEGIN_ORIENTATION = 194635;
  PCD_BEGIN = 2048;

  PCD_MAGIC = 'PCD_IPI';

//----------------------------------------------------------------------------------------------------------------------

Class Function TPCDGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: Array Of Byte;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > 3 * $800;
    If Result Then
    Begin
      SetLength(Header, $803);
      ReadBuffer(Header[0], Length(Header));
      Result := (StrLComp(PAnsiChar(@Header[0]), 'PCD_OPA', 7) = 0) or
                (StrLComp(PAnsiChar(@Header[$800]), 'PCD', 3) = 0);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPCDGraphic.LoadFromStream(Stream: TStream);

Var
  C1, C2, YY: PByte;
  YCbCrData: array[0..2] of PByte;
  SourceDummy,
  DestDummy: Pointer;

  Offset, I,
  X, Y,
  ImageIndex,
  Rows,
  Columns: Cardinal;
  ScanLines: Array Of Pointer;

  LineBuffer: Pointer;
  Line,
  Run: PBGR;
  Decoder: TPCDDecoder;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;
  ImageIndex := TPCDGraphic.DefaultResolution; // third image is Base resolution //@@@ SZ

  If ReadImageProperties(Stream, ImageIndex) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
      Columns := 192 Shl Min(ImageIndex, 2);
      Rows := 128 Shl Min(ImageIndex, 2);

      // since row and columns might be swapped because of rotated images
      // we determine the final dimensions once more
      Width := 192 Shl ImageIndex;
      Height := 128 Shl ImageIndex;

      ZeroMemory(@YCbCrData, SizeOf(YCbCrData));
      Try
        GetMem(YCbCrData[0], Width * Height);
        GetMem(YCbCrData[1], Width * Height);
        GetMem(YCbCrData[2], Width * Height);

        // advance to image data
        Offset := 96;
        If Overview Then Offset := 5
                    Else
          If ImageIndex = 1 Then Offset := 23
                            Else
            If ImageIndex = 0 Then Offset := 4;
        Seek(Offset * $800 , soFromCurrent);

        // color conversion setup
        With ColorManager Do
        Begin
          SourceColorScheme := csPhotoYCC;
          SourceBitsPerSample := 8;
          SourceSamplesPerPixel := 3;
          TargetColorScheme := csBGR;
          TargetBitsPerSample := 8;
          TargetSamplesPerPixel := 3;
        End;
        PixelFormat := pf24Bit;
        // PhotoYCC format uses CCIR Recommendation 709 coefficients and is subsampled
        // by factor 2 vertically and horizontally
        ColorManager.SetYCbCrParameters([0.2125, 0.7154, 0.0721], 2, 2);

        Progress(Self, psEnding, 0, False, FProgressRect, '');

        If False Then
        Begin
          // if Overview then ... no info yet about overview image structure
        End
        Else
        Begin
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];
          I := 0;
          Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
          While I < Rows Do
          Begin
            Progress(Self, psRunning, MulDiv(I, 100, Rows), False, FProgressRect, '');
            ReadBuffer(YY^, Columns);
            Inc(YY, Width);
            ReadBuffer(YY^, Columns);
            Inc(YY, Width);
            ReadBuffer(C1^, Columns Shr 1);
            Inc(C1, Width);
            ReadBuffer(C2^, Columns Shr 1);
            Inc(C2, Width);
            Inc(I, 2);
          End;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesUpsampling);
          // Y stands here for maximum number of upsample calls
          Y := 5;
          If ImageIndex >= 3 Then
          Begin
            Inc(Y, 3 * (ImageIndex - 3));

            Decoder := TPCDDecoder.Create(Stream);
            SourceDummy := @YCbCrData;
            DestDummy := Nil;
            Try
              // recover luminance deltas for 1536 x 1024 image
              Progress(Self, psRunning, MulDiv(0, 100, Y), False, FProgressRect, '');
              Upsample(768, 512, Width, YCbCrData[0]);
              Progress(Self, psRunning, MulDiv(1, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[1]);
              Progress(Self, psRunning, MulDiv(2, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[2]);
              Seek(4 * $800, soFromCurrent);

              Decoder.Decode(SourceDummy, DestDummy, Width, 1024);
              If ImageIndex >= 4 Then
              Begin
                // recover luminance deltas for 3072 x 2048 image
                Progress(Self, psRunning, MulDiv(3, 100, Y), False, FProgressRect, '');
                Upsample(1536, 1024, Width, YCbCrData[0]);
                Progress(Self, psRunning, MulDiv(4, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[1]);
                Progress(Self, psRunning, MulDiv(5, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[2]);
                Offset := (Position - Integer(FBasePosition)) Div $800 + 12;
                Seek(FBasePosition + Offset * $800, soFromBeginning);

                Decoder.Decode(SourceDummy, DestDummy, Width, 2048);
                If ImageIndex = 5 Then
                Begin
                  // recover luminance deltas for 6144 x 4096 image (vaporware)
                  Progress(Self, psRunning, MulDiv(6, 100, Y), False, FProgressRect, '');
                  Upsample(3072, 2048, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(7, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(8, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[2]);
                End;
              End;
            Finally
              Decoder.Free;
            End;
          End;

          Progress(Self, psRunning, MulDiv(Y - 1, 100, Y), False, FProgressRect, '');
          Upsample(Width Shr 1, Height Shr 1, Width, YCbCrData[1]);
          Progress(Self, psRunning, MulDiv(Y, 100, Y), False, FProgressRect, '');
          Upsample(Width Shr 1, Height Shr 1, Width, YCbCrData[2]);

          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          // transfer luminance and chrominance channels
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];

          // For the rotated mode where we need to turn the image by 90°. We can speed up loading
          // the image by factor 2 by using a local copy of the Scanline pointers.
          If Rotate In [1, 3] Then
          Begin
            Self.Width := Height;
            Self.Height := Width;
            FProgressRect.Right := Height;

            SetLength(ScanLines, Width);
            For Y := 0 To Width - 1 Do ScanLines[Y] := ScanLine[Y];
            GetMem(LineBuffer, 3 * Width);
          End
          Else
          Begin
            ScanLines := Nil;
            Self.Width := Width;
            Self.Height := Height;
            LineBuffer := Nil;
          End;

          Try
            Case Rotate Of
              1: // rotate -90°
                Begin
                  For Y := 0 To Height - 1 Do
                  Begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    For X := 0 To Width - 1 Do
                    Begin
                      PByte(Line) := @(PByte(ScanLines[Width - X - 1])[Y * 3]);
                      Line^ := Run^;
                      Inc(Run);
                    End;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  End;
                End;
              3: // rotate 90°
                Begin
                  For Y := 0 To Height - 1 Do
                  Begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    For X := 0 To Width - 1 Do
                    Begin
                      PByte(Line) := @(PByte(ScanLines[X])[(Height - Y - 1) * 3]);
                      Line^ := Run^;
                      Inc(Run);
                    End;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  End;
                End;
            Else
              For Y := 0 To Height - 1 Do
              Begin
                ColorManager.ConvertRow([YY, C1, C2], ScanLine[Y], Width, $FF);
                Inc(YY, Width);
                Inc(C1, Width);
                Inc(C2, Width);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
            Progress(Self, psEnding, 0, False, FProgressRect, '');
          Finally
            ScanLines := Nil;
            If Assigned(LineBuffer) Then FreeMem(LineBuffer);
          End;
        End;

      Finally
        If Assigned(YCbCrData[2]) Then FreeMem(YCbCrData[2]);
        If Assigned(YCbCrData[1]) Then FreeMem(YCbCrData[1]);
        If Assigned(YCbCrData[0]) Then FreeMem(YCbCrData[0]);
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPCDGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: Array Of Byte;
  Temp: Cardinal;

Begin
  If ImageIndex > 5 Then ImageIndex := 5;
  Result := Inherited ReadImageProperties(Stream, ImageIndex) And
            ((Stream.Size - Integer(FBasePosition)) > 3 * $800);
  With Stream, FImageProperties Do
  Begin
    SetLength(Header, 3 * $800);
    ReadBuffer(Header[0], Length(Header));
    Try
      Overview := StrLComp(PAnsiChar(@Header[0]), 'PCD_OPA', 7) = 0;
      // determine if image is a PhotoCD image
      if Overview or (StrLComp(PAnsiChar(@Header[$800]), 'PCD', 3) = 0) then
      Begin
        Rotate := Header[$0E02] And 3;

        // image sizes are fixed, depending on the given image index
        If Overview Then ImageIndex := 0;
        Width := 192 Shl ImageIndex;
        Height := 128 Shl ImageIndex;
        If (Rotate = 1) Or (Rotate = 3) Then
        Begin
          Temp := Width;
          Width := Height;
          Height := Temp;
        End;
        ColorScheme := csPhotoYCC;
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        If ImageIndex > 2 Then Compression := ctPCDHuffmann
                          Else Compression := ctNone;
        ImageCount := (Header[10] Shl 8) Or Header[11];

        Result := True;
      End;
    Finally
      Header := Nil;
    End;
  End;
End;

{$ENDIF} // PCDGraphic

//----------------- TPPMGraphic ----------------------------------------------------------------------------------------

{$IFDEF PortableMapGraphic}

Class Function TPPMGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Buffer: array[0..9] of Byte;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > 10;
    If Result Then
    Begin
      ReadBuffer(Buffer, SizeOf(Buffer));
      Result := (Buffer[0] = ord('P')) and (Buffer[1] in [ord('1')..ord('6')]);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.CurrentChar: AnsiChar;

Begin
  If FIndex = SizeOf(FBuffer) Then Result := #0
                              Else Result := FBuffer[FIndex];
End;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetChar: AnsiChar;

// buffered I/O

Begin
  If FIndex = SizeOf(FBuffer) Then
  Begin
    If FStream.Position = FStream.Size Then GraphicExError(gesStreamReadError, ['PPM']);
    FIndex := 0;
    FStream.Read(FBuffer, SizeOf(FBuffer));
  End;
  Result := FBuffer[FIndex];
  Inc(FIndex);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPPMGraphic.GetNumber: Cardinal;

// reads the next number from the stream (and skips all characters which are not in 0..9)

Var
  Ch: AnsiChar;

Begin
  // skip all non-numbers
  Repeat
    Ch := GetChar;
    // skip comments
    If Ch = '#' Then
    Begin
      ReadLine;
      Ch := GetChar;
    End;
  Until Ch In ['0'..'9'];

  // read the number characters and convert meanwhile
  Result := 0;
  Repeat
    Result := 10 * Result + Ord(Ch) - $30;
    Ch := GetChar;
  Until Not (Ch In ['0'..'9']);
End;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadLine: AnsiString;

// reads one text line from stream and skips comments

Var
  Ch: AnsiChar;
  I: Integer;

Begin
  Result := '';
  Repeat
    Ch := GetChar;
    If Ch In [#13, #10] Then Break
                        Else Result := Result + Ch;
  Until False;
  // eat #13#10 combination
  If (Ch = #13) And (CurrentChar = #10) Then GetChar;

  // delete comments
  I := Pos('#', String(Result));
  If I > 0 Then Delete(Result, I, MaxInt);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPPMGraphic.LoadFromStream(Stream: TStream);

Var
  Buffer: AnsiString;
  Line24: PBGR;
  Line8: PByte;
  X, Y: Integer;
  Pixel: Byte;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;
  // copy reference for buffered access
  FStream := Stream;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With FImageProperties Do
    Begin
      Stream.Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      // set index pointer to end of buffer to cause reload
      FIndex := SizeOf(FBuffer);
      With Stream Do
      Begin
        Buffer := ReadLine;
        Case StrToInt(String(Buffer)[2]) Of
          1: // PBM ASCII format (black & white)
            Begin
              PixelFormat := pf1Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 1;
              Palette := ColorManager.CreateGrayScalePalette(True);

              // read image data
              For Y := 0 To Height - 1 Do
              Begin
                Line8 := ScanLine[Y];
                Pixel := 0;
                For X := 1 To Width Do
                Begin
                  Pixel := (Pixel Shl 1) Or (GetNumber And 1);
                  If (X Mod 8) = 0 Then
                  Begin
                    Line8^ := Pixel;
                    Inc(Line8);
                    Pixel := 0;
                  End;
                End;
                If (Width Mod 8) <> 0 Then Line8^ := Pixel Shl (8 - (Width Mod 8));

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          2: // PGM ASCII form (gray scale)
            Begin
              PixelFormat := pf8Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 8;
              Palette := ColorManager.CreateGrayScalePalette(False);

              // read image data
              For Y := 0 To Height - 1 Do
              Begin
                Line8 := ScanLine[Y];
                For X := 0 To Width - 1 Do
                Begin
                  Line8^ := GetNumber;
                  Inc(Line8);
                End;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          3: // PPM ASCII form (true color)
            Begin
              PixelFormat := pf24Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;

              For Y := 0 To Height - 1 Do
              Begin
                Line24 := ScanLine[Y];
                For X := 0 To Width - 1 Do
                Begin
                  Line24.R := GetNumber;
                  Line24.G := GetNumber;
                  Line24.B := GetNumber;
                  Inc(Line24);
                End;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          4: // PBM binary format (black & white)
            Begin
              PixelFormat := pf1Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 1;
              Palette := ColorManager.CreateGrayScalePalette(True);

              // read image data
              For Y := 0 To Height - 1 Do
              Begin
                Line8 := ScanLine[Y];
                For X := 0 To (Width Div 8) - 1 Do
                Begin
                  Line8^ := Byte(GetChar);
                  Inc(Line8);
                End;
                If (Width Mod 8) <> 0 Then Line8^ := Byte(GetChar);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          5: // PGM binary form (gray scale)
            Begin
              PixelFormat := pf8Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;
              ColorManager.TargetSamplesPerPixel := 1;
              ColorManager.TargetBitsPerSample := 8;
              Palette := ColorManager.CreateGrayScalePalette(False);

              // read image data
              For Y := 0 To Height - 1 Do
              Begin
                Line8 := ScanLine[Y];
                For X := 0 To Width - 1 Do
                Begin
                  Line8^ := Byte(GetChar);
                  Inc(Line8);
                End;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
          6: // PPM binary form (true color)
            Begin
              PixelFormat := pf24Bit;
              Self.Width := GetNumber;
              Self.Height := GetNumber;
              // skip maximum color value
              GetNumber;

              // Pixel values are store linearly (but RGB instead BGR).
              // There's one allowed white space which will automatically be skipped by the first
              // GetChar call below
              // now read the pixels
              For Y := 0 To Height - 1 Do
              Begin
                Line24 := ScanLine[Y];
                For X := 0 To Width - 1 Do
                Begin
                  Line24.R := Byte(GetChar);
                  Line24.G := Byte(GetChar);
                  Line24.B := Byte(GetChar);
                  Inc(Line24);
                End;

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              End;
            End;
        End;
      End;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
  End
  Else GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPPMGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Buffer: AnsiString;

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    // set index pointer to end of buffer to cause reload
    FIndex := SizeOf(FBuffer);
    Buffer := ReadLine;

    Compression := ctNone;

    If Buffer[1] = 'P' Then
    Begin
      Case StrToInt(String(Buffer)[2]) Of
        1: // PBM ASCII format (black & white)
          Begin
            Width := GetNumber;
            Height := GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 1;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
        2: // PGM ASCII form (gray scale)
          Begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 8;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
        3: // PPM ASCII form (true color)
          Begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 3;
            BitsPerSample := 8;
            ColorScheme := csRGB;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
        4: // PBM binary format (black & white)
          Begin
            Width := GetNumber;
            Height := GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 1;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
        5: // PGM binary form (gray scale)
          Begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 1;
            BitsPerSample := 8;
            ColorScheme := csIndexed;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
        6: // PPM binary form (true color)
          Begin
            Width := GetNumber;
            Height := GetNumber;
            // skip maximum color value
            GetNumber;

            SamplesPerPixel := 3;
            BitsPerSample := 8;
            ColorScheme := csRGB;
            BitsPerPixel := SamplesPerPixel * BitsPerSample;
          End;
      End;
      Result := True;
    End;
  End;
End;

{$ENDIF} // PortableMapGraphic

//----------------- TCUTGraphic ----------------------------------------------------------------------------------------

{$IFDEF CUTGraphic}

Class Function TCUTGraphic.CanLoad(Stream: TStream): Boolean;

// Note: cut files cannot be determined from stream because the only information
//       is width and height of the image at stream/image start which is by no means
//       enough to identify a cut (or any other) image.

Begin
  Result := False;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TCUTGraphic.LoadFromFile(Const FileName: String);

// overridden to extract an implicit palette file name

Begin
  FPaletteFile := ChangeFileExt(FileName, '.pal');
  Inherited;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TCUTGraphic.LoadFromStream(Stream: TStream);

Var
  Buffer: PByte;
  Run,
  Line: Pointer;
  Decoder: TCUTRLEDecoder;
  CUTSize: Cardinal;
  Y: Integer;

Begin
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      Position := FBasePosition + 6;

      FProgressRect := Rect(0, 0, Width, 0);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      PixelFormat := pf8Bit;
      Self.Width := Width;
      Self.Height := Height;
      LoadPalette;

      CutSize := Stream.Size - Stream.Position;
      Decoder := TCUTRLEDecoder.Create;
      Buffer := Nil;
      Try
        GetMem(Buffer, CutSize);
        Stream.ReadBuffer(Buffer^, CUTSize);

        Run := Buffer;
        For Y := 0 To Height - 1 Do
        Begin
          Line := ScanLine[Y];
          Decoder.Decode(Run, Line, 0, Width);

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        End;

      Finally
        Decoder.Free;
        If Assigned(Buffer) Then FreeMem(Buffer);
      End;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TCUTGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Dummy: Word;
  
Begin
  Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    PixelFormat := pf8Bit;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Width := Dummy;
    ReadBuffer(Dummy, SizeOf(Dummy));
    Height := Dummy;

    ColorScheme := csIndexed;
    BitsPerSample := 8;
    SamplesPerPixel := 1;
    BitsPerPixel := BitsPerSample * SamplesPerPixel;

    Compression := ctRLE;
    
    Result := True;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Type
  // the palette file header is actually more complex than the
  // image file's header, funny...
  PHaloPaletteHeader = ^THaloPaletteHeader;
  THaloPaletteHeader = Packed Record
    ID: array[0..1] of AnsiChar;  // should be 'AH'
    Version,
    Size: Word;
    FileType,
    SubType: Byte;
    BrdID,
    GrMode: Word;
    MaxIndex,
    MaxRed,
    MaxGreen,
    MaxBlue: Word; // colors = MaxIndex + 1
    Signature: array[0..7] of AnsiChar; // 'Dr. Halo'
    Filler: Array[0..11] Of Byte;
  End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TCUTGraphic.LoadPalette;

Var
  Header: PHaloPaletteHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;
  Buffer: Array[0..511] Of Byte;
  Run: PWord;

Begin
  LogPalette.palVersion := $300;
  If FileExists(FPaletteFile) Then
  Begin
    With TFileStream.Create(FPaletteFile, fmOpenRead Or fmShareDenyNone) Do
    Try
      // quite strange file organization here, we need always to load 512 bytes blocks
      // and skip occasionally some bytes
      ReadBuffer(Buffer, SizeOf(Buffer));
      Header := @Buffer;
      LogPalette.palNumEntries := Header.MaxIndex + 1;
      Run := @Buffer;
      Inc(PByte(Run), SizeOf(Header^));
      For I := 0 To LogPalette.palNumEntries - 1 Do
      Begin
        // load next 512 bytes buffer if necessary
        If (Integer(Run) - Integer(@Buffer)) > 506 Then
        Begin
          ReadBuffer(Buffer, SizeOf(Buffer));
          Run := @Buffer;
        End;
        LogPalette.palPalEntry[I].peRed := Run^;
        Inc(Run);
        LogPalette.palPalEntry[I].peGreen := Run^;
        Inc(Run);
        LogPalette.palPalEntry[I].peBlue := Run^;
        Inc(Run);
      End;
    Finally
      Free;
    End;
  End
  Else
  Begin
    LogPalette.palNumEntries := 256;
    // no external palette so use gray scale
    For I := 0 To 255 Do
    Begin
      LogPalette.palPalEntry[I].peBlue := I;
      LogPalette.palPalEntry[I].peGreen := I;
      LogPalette.palPalEntry[I].peRed := I;
    End;
  End;

  // finally create palette
  Palette := CreatePalette(PLogPalette(@LogPalette)^);
End;

{$ENDIF} // CUTGraphic

//----------------- TGIFGraphic ----------------------------------------------------------------------------------------

{$IFDEF GIFGraphic}

Const
  // logical screen descriptor packed field masks
  GIF_GLOBALCOLORTABLE = $80;
  GIF_COLORRESOLUTION = $70;
  GIF_GLOBALCOLORTABLESORTED = $08; 
  GIF_COLORTABLESIZE = $07;

  // image flags
  GIF_LOCALCOLORTABLE = $80;
  GIF_INTERLACED = $40;
  GIF_LOCALCOLORTABLESORTED= $20;

  // block identifiers
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = Ord(',');
  GIF_EXTENSIONINTRODUCER = Ord('!');
  GIF_TRAILER = Ord(';');
  
Type
  TGIFHeader = Packed Record
    Signature: array[0..2] of AnsiChar; // magic ID 'GIF'
    Version: array[0..2] of AnsiChar;   // '87a' or '89a' 
  End;

  TLogicalScreenDescriptor = Packed Record
    ScreenWidth: Word;
    ScreenHeight: Word;
    PackedFields,
    BackgroundColorIndex, // index into global color table
    AspectRatio: Byte;    // actual ratio = (AspectRatio + 15) / 64
  End;

  TImageDescriptor = Packed Record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;     // X position of image with respect to logical screen
    Top: Word;     // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TGIFGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TGIFHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > (SizeOf(TGIFHeader) + SizeOf(TLogicalScreenDescriptor) + SizeOf(TImageDescriptor));
    If Result Then
    Begin
      ReadBuffer(Header, SizeOf(Header));
      Result := UpperCase(String(Header.Signature)) = 'GIF';
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TGIFGraphic.SkipExtensions: Byte;

// Skips all blocks until an image block has been found in the data stream.
// Result is the image block ID if an image block could be found.

Var
  Increment: Byte;

Begin
  With FStream Do
  Begin
    // iterate through the blocks until first image is found
    Repeat
      ReadBuffer(Result, 1);
      If Result = GIF_EXTENSIONINTRODUCER Then
      Begin
        // skip any extension
        ReadBuffer(Result, 1);
        Case Result Of
          GIF_PLAINTEXT:
            Begin
              // block size of text grid data
              ReadBuffer(Increment, 1);
              Seek(Increment, soFromCurrent);
              // skip variable lengthed text block
              Repeat
                // block size
                ReadBuffer(Increment, 1);
                If Increment = 0 Then Break;
                Seek(Increment, soFromCurrent);
              Until False;
            End;
          GIF_GRAPHICCONTROLEXTENSION:
            Begin
              // block size
              ReadBuffer(Increment, 1);
              // skip block and its terminator
              Seek(Increment + 1, soFromCurrent);
            End;
          GIF_COMMENTEXTENSION:
            Repeat
              // block size
              ReadBuffer(Increment, 1);
              If Increment = 0 Then Break;
              Seek(Increment, soFromCurrent);
            Until False;
          GIF_APPLICATIONEXTENSION:
            Begin
              // application id and authentication code plus potential application data
              Repeat
                ReadBuffer(Increment, 1);
                If Increment = 0 Then Break;
                Seek(Increment, soFromCurrent);
              Until False;
            End;
        End;
      End;
    Until (Result = GIF_IMAGEDESCRIPTOR) Or (Result = GIF_TRAILER);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TGIFGraphic.LoadFromStream(Stream: TStream);

Var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  LogPalette: TMaxLogPalette;
  I: Cardinal;
  BlockID: Byte;
  InitCodeSize: Byte;
  RawData,
  Run: PByte;
  TargetBuffer,
  TargetRun,
  Line: Pointer;
  Pass,
  Increment,
  Marker: Integer;
  Decoder: TDecoder;

Begin
  // release old image
  Handle := 0;
  FBasePosition := Stream.Position;
  FStream := Stream;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      ReadBuffer(Header, SizeOf(Header));

      PixelFormat := pf8Bit;
      
      // general information
      ReadBuffer(ScreenDescriptor, SizeOf(ScreenDescriptor));

      ZeroMemory(@LogPalette, SizeOf(LogPalette));
      LogPalette.palVersion := $300;
      // read global color table if given
      If (ScreenDescriptor.PackedFields And GIF_GLOBALCOLORTABLE) <> 0 Then
      Begin
        // the global color table immediately follows the screen descriptor
        LogPalette.palNumEntries := 2 Shl (ScreenDescriptor.PackedFields And GIF_COLORTABLESIZE);
        For I := 0 To LogPalette.palNumEntries - 1 Do
        Begin
          ReadBuffer(LogPalette.palPalEntry[I].peRed, 1);
          ReadBuffer(LogPalette.palPalEntry[I].peGreen, 1);
          ReadBuffer(LogPalette.palPalEntry[I].peBlue, 1);
        End;
        // finally create palette
        Palette := CreatePalette(PLogPalette(@LogPalette)^);
      End;

      BlockID := SkipExtensions;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // image found?
      If BlockID = GIF_IMAGEDESCRIPTOR Then
      Begin
        Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
        ReadBuffer(ImageDescriptor, SizeOf(TImageDescriptor));
        Self.Width := Width;
        Self.Height := Height;

        // if there is a local color table then override the already set one
        If (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) <> 0 Then
        Begin
          // the global color table immediately follows the image descriptor
          LogPalette.palNumEntries := 2 Shl (ImageDescriptor.PackedFields And GIF_COLORTABLESIZE);
          For I := 0 To LogPalette.palNumEntries - 1 Do
          Begin
            ReadBuffer(LogPalette.palPalEntry[I].peRed, 1);
            ReadBuffer(LogPalette.palPalEntry[I].peGreen, 1);
            ReadBuffer(LogPalette.palPalEntry[I].peBlue, 1);
          End;
          Palette := CreatePalette(PLogPalette(@LogPalette)^);
        End;

        ReadBuffer(InitCodeSize, 1);
        // decompress data in one step
        // 1) count data
        Marker := Position;
        Pass := 0;
        Increment := 0;
        Repeat
          If Read(Increment, 1) = 0 Then Break;
          Inc(Pass, Increment);
          Seek(Increment, soFromCurrent);
        Until Increment = 0;

        // 2) allocate enough memory
        GetMem(RawData, Pass);
        // add one extra line of extra memory for badly coded images
        GetMem(TargetBuffer, Width * (Height + 1));

        Try
          // 3) read and decode data
          Position := Marker;
          Increment := 0;
          Run := RawData;
          Repeat
            If Read(Increment, 1) = 0 Then Break;
            Read(Run^, Increment);
            Inc(Run, Increment);
          Until Increment = 0;

          Decoder := TGIFLZWDecoder.Create(InitCodeSize);
          Try
            Run := RawData;
            Decoder.Decode(Pointer(Run), TargetBuffer, Pass, Width * Height);
          Finally
            Decoder.Free;
          End;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          // finally transfer image data
          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          If (ImageDescriptor.PackedFields And GIF_INTERLACED) = 0 Then
          Begin
            TargetRun := TargetBuffer;
            For I := 0 To Height - 1 Do
            Begin
              Line := Scanline[I];
              Move(TargetRun^, Line^, Width);
              Inc(PByte(TargetRun), Width);

              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            End;
          End
          Else
          Begin
            TargetRun := TargetBuffer;
            // interlaced image, need to move in four passes
            For Pass := 0 To 3 Do
            Begin
              // determine start line and increment of the pass
              Case Pass Of
                0:
                  Begin
                    I := 0;
                    Increment := 8;
                  End;
                1:
                  Begin
                    I := 4;
                    Increment := 8;
                  End;
                2:
                  Begin
                    I := 2;
                    Increment := 4;
                  End;
              Else
                I := 1;
                Increment := 2;
              End;

              While I < Height Do
              Begin
                Line := Scanline[I];
                Move(TargetRun^, Line^, Width);
                Inc(PByte(TargetRun), Width);
                Inc(I, Increment);

                If Pass = 3 Then
                Begin
                  // progress events only for last (and most expensive) run
                  Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                End;
              End;
            End;
          End;
          Progress(Self, psEnding, 0, False, FProgressRect, '');
        Finally
          If Assigned(TargetBuffer) Then FreeMem(TargetBuffer);
          If Assigned(RawData) Then FreeMem(RawData);
        End;
      End;
    End;
  End
  Else GraphicExError(gesInvalidImage, ['GIF']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TGIFGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  BlockID: Integer;
  
Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    If UpperCase(String(Header.Signature)) = 'GIF' Then
    Begin
      Version := StrToInt(Copy(String(Header.Version), 1, 2));
      ColorScheme := csIndexed;
      SamplesPerPixel := 1;
      // might be overwritten
      BitsPerSample := 8;
      Compression := ctLZW;

      // general information
      ReadBuffer(ScreenDescriptor, SizeOf(ScreenDescriptor));

      // skip global color table if given
      If (ScreenDescriptor.PackedFields And GIF_GLOBALCOLORTABLE) <> 0 Then
      Begin
        BitsPerSample := (ScreenDescriptor.PackedFields And GIF_COLORTABLESIZE) + 1;
        // the global color table immediately follows the screen descriptor
        Seek(3 * (1 Shl BitsPerSample), soFromCurrent);
      End;

      BlockID := SkipExtensions;

      // image found?
      If BlockID = GIF_IMAGEDESCRIPTOR Then
      Begin
        ReadBuffer(ImageDescriptor, SizeOf(TImageDescriptor));
        Width := ImageDescriptor.Width;
        If Width = 0 Then Width := ScreenDescriptor.ScreenWidth;
        Height := ImageDescriptor.Height;
        If Height = 0 Then Height := ScreenDescriptor.ScreenHeight;

        // if there is a local color table then override the already set one
        LocalColorTable := (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) <> 0;
        If LocalColorTable Then
          BitsPerSample := (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) + 1;
        Interlaced := (ImageDescriptor.PackedFields And GIF_INTERLACED) <> 0;
      End;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      Result := True;
    End;
  End;
End;

{$ENDIF} // GIFGraphic

//----------------- TRLAGraphic ----------------------------------------------------------------------------------------

{$IFDEF RLAGraphic}

// This implementation is based on code from Dipl. Ing. Ingo Neumann (ingo@upstart.de, ingo_n@dialup.nacamar.de).

Type
  TRLAWindow = Packed Record
    Left,
    Right,
    Bottom,
    Top: SmallInt;
  End;

  TRLAHeader = Packed Record
    Window,                            // overall image size
    Active_window: TRLAWindow;         // size of non-zero portion of image (we use this as actual image size)
    Frame,                             // frame number if part of a sequence
    Storage_type,                      // type of image channels (0 - integer data, 1 - float data)
    Num_chan,                          // samples per pixel (usually 3: r, g, b)
    Num_matte,                         // number of matte channels (usually only 1)
    Num_aux,                           // number of auxiliary channels, usually 0
    Revision: SmallInt;                // always $FFFE
    Gamma: array[0..15] of AnsiChar;   // gamma single value used when writing the image
    Red_pri: array[0..23] of AnsiChar; // used chromaticity for red channel (typical format: "%7.4f %7.4f")
    Green_pri: array[0..23] of AnsiChar; // used chromaticity for green channel
    Blue_pri: array[0..23] of AnsiChar;// used chromaticity for blue channel
    White_pt: array[0..23] of AnsiChar;// used chromaticity for white point
    Job_num: Integer;                  // rendering speciifc
    Name: array[0..127] of AnsiChar;   // original file name
    Desc: array[0..127] of AnsiChar;   // a file description
    ProgramName: array[0..63] of AnsiChar; // name of program which created the image
    Machine: array[0..31] of AnsiChar; // name of computer on which the image was rendered
    User: array[0..31] of AnsiChar;    // user who ran the creation program of the image
    Date: array[0..19] of AnsiChar;    // creation data of image (ex: Sep 30 12:29 1993)
    Aspect: array[0..23] of AnsiChar;  // aspect format of the file (external resource)
    Aspect_ratio: array[0..7] of AnsiChar; // float number Width /Height
    Chan: array[0..31] of AnsiChar;    // color space (can be: rgb, xyz, sampled or raw)
    Field: SmallInt;                   // 0 - non-field rendered data, 1 - field rendered data
    Time: array[0..11] of AnsiChar;    // time needed to create the image (used when rendering)
    Filter: array[0..31] of AnsiChar;  // filter name to post-process image data
    Chan_bits,                         // bits per sample
    Matte_type,                        // type of matte channel (see aux_type)
    Matte_bits,                        // precision of a pixel's matte channel (1..32)
    Aux_type,                          // type of aux channel (0 - integer data; 4 - single (float) data
    Aux_bits: SmallInt;                // bits precision of the pixel's aux channel (1..32 bits)
    Aux: array[0..31] of AnsiChar;     // auxiliary channel as either range or depth
    Space: array[0..35] of Byte;       // unused
    Next: Integer;                     // offset for next header if multi-frame image
  End;
  
//----------------------------------------------------------------------------------------------------------------------

Class Function TRLAGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TRLAHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (Swap(Word(Header.Revision)) = $FFFE) And
                ((LowerCase(String(Header.Chan)) = 'rgb') Or
                 (LowerCase(String(Header.Chan)) = 'xyz'));
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TRLAGraphic.LoadFromStream(Stream: TStream);

Var
  Offsets: TCardinalArray;
  RLELength: Word;
  Line: Pointer;
  Y: Integer;

  // RLE buffers
  RawBuffer,
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Decoder: TRLADecoder;

Begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      With ColorManager Do
      Begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        SourceBitsPerSample := BitsPerSample;
        If BitsPerSample > 8 Then TargetBitsPerSample := 8
                             Else TargetBitsPerSample := BitsPerSample;
        SourceColorScheme := ColorScheme;
        If ColorScheme = csRGBA Then TargetColorScheme := csBGRA
                                Else TargetColorScheme := csBGR;

        PixelFormat := TargetPixelFormat;

        If FileGamma <> 1 Then
        Begin
          SetGamma(FileGamma);
          TargetOptions := TargetOptions + [coApplyGamma];
          Include(Options, ioUseGamma);
        End;
      End;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Self.Width := Width;
      Self.Height := Height;

      // each scanline is organized in RLE compressed strips whose location in the stream
      // is determined by the offsets table
      SetLength(Offsets, Height);
      ReadBuffer(Offsets[0], Height * SizeOf(Cardinal));
      SwapLong(@Offsets[0], Height);

      // setup intermediate storage
      Decoder := TRLADecoder.Create;
      RawBuffer := Nil;
      RedBuffer := Nil;
      GreenBuffer := Nil;
      BlueBuffer := Nil;
      AlphaBuffer := Nil;
      Try
        GetMem(RedBuffer, Width);
        GetMem(GreenBuffer, Width);
        GetMem(BlueBuffer, Width);
        GetMem(AlphaBuffer, Width);

        // no go for each scanline
        For Y := 0 To Height - 1 Do
        Begin
          Stream.Position := FBasePosition + Offsets[Y];
          If BottomUp Then Line := ScanLine[Integer(Height) - Y - 1]
                      Else Line := ScanLine[Y];
          // read channel data to decode
          // red
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, RedBuffer, RLELength, Width);
          // green
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, GreenBuffer, RLELength, Width);
          // blue
          ReadBuffer(RLELength, SizeOf(RLELength));
          RLELength := Swap(RLELength);
          ReallocMem(RawBuffer, RLELength);
          ReadBuffer(RawBuffer^, RLELength);
          Decoder.Decode(RawBuffer, BlueBuffer, RLELength, Width);

          If ColorManager.TargetColorScheme = csBGR Then
          Begin
            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], Line, Width, $FF);
          End
          Else
          Begin
            // alpha
            ReadBuffer(RLELength, SizeOf(RLELength));
            RLELength := Swap(RLELength);
            ReallocMem(RawBuffer, RLELength);
            ReadBuffer(RawBuffer^, RLELength);
            Decoder.Decode(RawBuffer, AlphaBuffer, RLELength, Width);

            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], Line, Width, $FF);
          End;

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        End;
      Finally
        If Assigned(RawBuffer) Then FreeMem(RawBuffer);
        If Assigned(RedBuffer) Then FreeMem(RedBuffer);
        If Assigned(GreenBuffer) Then FreeMem(GreenBuffer);
        If Assigned(BlueBuffer) Then FreeMem(BlueBuffer);
        If Assigned(AlphaBuffer) Then FreeMem(AlphaBuffer);
        Decoder.Free;
      End;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TRLAGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TRLAHeader;
  
Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    // data is always given in big endian order, so swap data which needs this
    SwapHeader(Header);
    Options := [ioBigEndian];

    SamplesPerPixel := Header.num_chan;
    If Header.num_matte = 1 Then Inc(SamplesPerPixel);
    BitsPerSample := Header.Chan_bits;
    BitsPerPixel := SamplesPerPixel * BitsPerSample;

    If LowerCase(String(Header.Chan)) = 'rgb' Then
    Begin
      If Header.num_matte > 0 Then ColorScheme := csRGBA
                              Else ColorScheme := csRGB;
    End
    Else
      If LowerCase(String(Header.Chan)) = 'xyz' Then Exit;

    Try
      FileGamma := StrToFloat(String(Header.Gamma));
    Except
    End;

    Compression := ctRLE;

    // dimension of image, top might be larger than bottom denoting a bottom up image
    Width := Header.Active_window.Right - Header.Active_window.Left + 1;
    Height := Abs(Header.Active_window.Bottom - Header.Active_window.Top) + 1;
    BottomUp := (Header.Active_window.Bottom - Header.Active_window.Top) < 0;

    Result := True;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TRLAGraphic.SwapHeader(Var Header);

// separate swap method to ease reading the main flow of the LoadFromStream method

Begin
  With TRLAHeader(Header) Do
  Begin
    SwapShort(@Window, 4);
    SwapShort(@Active_window, 4);
    Frame := Swap(Frame);
    Storage_type := Swap(Storage_type);
    Num_chan := Swap(Num_chan);
    Num_matte := Swap(Num_matte);
    Num_aux := Swap(Num_aux);
    Revision := Swap(Revision);
    Job_num  := SwapLong(Job_num);
    Field := Swap(Field);
    Chan_bits := Swap(Chan_bits);
    Matte_type := Swap(Matte_type);
    Matte_bits := Swap(Matte_bits);
    Aux_type := Swap(Aux_type);
    Aux_bits := Swap(Aux_bits);
    Next := SwapLong(Next);
  End;
End;

{$ENDIF} // RLAGraphic

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

{$IFDEF PhotoshopGraphic}

Const
  // color modes
  PSD_BITMAP = 0;
  PSD_GRAYSCALE = 1;
  PSD_INDEXED = 2;
  PSD_RGB = 3;
  PSD_CMYK = 4;
  PSD_MULTICHANNEL = 7;
  PSD_DUOTONE = 8;
  PSD_LAB = 9;

  PSD_COMPRESSION_NONE = 0;
  PSD_COMPRESSION_RLE = 1; // RLE compression (same as TIFF packed bits)

Type
  TPSDHeader = Packed Record
    Signature: array[0..3] of AnsiChar; // always '8BPS'
    Version: Word;                  // always 1
    Reserved: Array[0..5] Of Byte;  // reserved, always 0
    Channels: Word;                 // 1..24, number of channels in the image (including alpha)
    Rows,
    Columns: Cardinal;              // 1..30000, size of image
    Depth: Word;                    // 1, 8, 16 bits per channel
    Mode: Word;                     // color mode (see constants above)
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TPSDGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TPSDHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (UpperCase(String(Header.Signature)) = '8BPS') And
                (Swap(Header.Version) = 1);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPSDGraphic.LoadFromStream(Stream: TStream);

Var
  Header: TPSDHeader;
  Count: Cardinal;
  Decoder: TDecoder;
  RLELength: Array Of Word;

  Y: Integer;
  BPS: Cardinal;        // bytes per sample either 1 or 2 for 8 bits per channel and 16 bits per channel respectively 
  ChannelSize: Integer; // size of one channel (taking BPS into account)
  Increment: Integer;   // pointer increment from one line to next

  // RLE buffers
  Line,
  RawBuffer,           // all image data compressed
  Buffer: Pointer;     // all iamge data uncompressed
  Run1,                // running pointer in Buffer 1
  Run2,                // etc.
  Run3,
  Run4: PByte;
  RawPalette: Array[0..767] Of Byte;

Begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      Position := FBasePosition;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      ReadBuffer(Header, SizeOf(Header));

      // initialize color manager
      With ColorManager Do
      Begin
        SourceOptions := [coNeedByteSwap];
        SourceBitsPerSample := BitsPerSample;
        If BitsPerSample = 16 Then TargetBitsPerSample := 8
                              Else TargetBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        // color space
        SourceColorScheme := ColorScheme;
        Case ColorScheme Of
          csG,
          csIndexed:
            TargetColorScheme := ColorScheme;
          csRGB:
            TargetColorScheme := csBGR;
          csRGBA:
            TargetColorScheme := csBGRA;
          csCMYK:
            Begin
              TargetColorScheme := csBGR;
              TargetSamplesPerPixel := 3;
            End;
          csCIELab:
            Begin
              // PSD uses 0..255 for a and b so we need to convert them to -128..127
              SourceOptions := SourceOptions + [coLabByteRange, coLabChromaOffset];
              TargetColorScheme := csBGR;
            End;
        End;
      End;
    
      PixelFormat := ColorManager.TargetPixelFormat;
      Self.Width := Width;
      Self.Height := Height;

      // size of palette
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // setup the palette if necessary, color data immediately follows header
      Case ColorScheme Of
        csG:
          Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite In Options);
        csIndexed:
          Begin
            ReadBuffer(RawPalette, Count);
            Count := Count Div 3;
            Palette := ColorManager.CreateColorPalette([@RawPalette, @RawPalette[Count], @RawPalette[2 * Count]],
                                                       pfPlane8Triple, Count, False);
          End;
      End;

      // skip resource and layers section
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // +2 in order to skip the following compression value
      Seek(Count + 2, soFromCurrent);

      // now read out image data
      RawBuffer := Nil;

      If Compression = ctPackedBits Then
      Begin
        Decoder := TPackbitsRLEDecoder.Create;
        SetLength(RLELength, Height * Channels);
        ReadBuffer(RLELength[0], 2 * Length(RLELength));
        SwapShort(@RLELength[0], Height * Channels);
      End
      Else Decoder := Nil;

      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Try
        Case ColorScheme Of
          csG,
          csIndexed:
            Begin
              Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
              // very simple format here, we don't need the color conversion manager
              If Assigned(Decoder) Then
              Begin
                // determine whole compressed size
                Count := 0;
                For Y := 0 To Height - 1 Do Inc(Count, RLELength[Y]);
                GetMem(RawBuffer, Count);
                Try
                  ReadBuffer(RawBuffer^, Count);
                  Run1 := RawBuffer;
                  For Y := 0 To Height - 1 Do
                  Begin
                    Count := RLELength[Y];
                    Line := ScanLine[Y];
                    Decoder.Decode(Pointer(Run1), Line, Count, Width);
                    Inc(Run1, Count);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  End;
                Finally
                  If Assigned(RawBuffer) Then FreeMem(RawBuffer);
                End;
              End
              Else // uncompressed data 
                For Y := 0 To Height - 1 Do
                Begin
                  ReadBuffer(ScanLine[Y]^, Width);

                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                End;
            End;
          csRGB,
          csRGBA,
          csCMYK,
          csCIELab:
            Begin
              Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
              // Data is organized in planes. This means first all red rows, then
              // all green and finally all blue rows. 
              BPS := BitsPerSample Div 8;
              ChannelSize := BPS * Width * Height;

              GetMem(Buffer, Channels * ChannelSize);
              Try
                // first run: load image data and decompress it if necessary
                If Assigned(Decoder) Then
                Begin
                  // determine whole compressed size
                  Count := 0;
                  For Y := 0 To High(RLELength) Do Inc(Count, RLELength[Y]);
                  Count := Count * Cardinal(BPS);
                  GetMem(RawBuffer, Count);
                  Try
                    ReadBuffer(RawBuffer^, Count);
                    Decoder.Decode(RawBuffer, Buffer, Count, Channels * ChannelSize);
                  Finally
                    If Assigned(RawBuffer) Then FreeMem(RawBuffer);
                  End;
                End
                Else
                  ReadBuffer(Buffer^, Channels * ChannelSize);

                Progress(Self, psEnding, 0, False, FProgressRect, '');

                Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
                Increment := BPS * Width;
                // second run: put data into image (convert color space if necessary)
                Case ColorScheme Of
                  csRGB:
                    Begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      For Y := 0 To Height - 1 Do
                      Begin
                        ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      End;
                    End;
                  csRGBA:
                    Begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      Run4 := Run3; Inc(Run4, ChannelSize);
                      For Y := 0 To Height - 1 Do
                      Begin
                        ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);
                        Inc(Run4, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      End;
                    End;
                  csCMYK:
                    Begin
                      // Photoshop CMYK values are given with 0 for maximum values, but the
                      // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
                      // all entries in the buffer.
                      Run1 := Buffer;
                      For Y := 1 To 4 * ChannelSize Do
                      Begin
                        Run1^ := 255 - Run1^;
                        Inc(Run1);
                      End;

                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      Run4 := Run3; Inc(Run4, ChannelSize);
                      For Y := 0 To Height - 1 Do
                      Begin
                        ColorManager.ConvertRow([Run1, Run2, Run3, Run4], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);
                        Inc(Run4, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      End;
                    End;
                  csCIELab:
                    Begin
                      Run1 := Buffer;
                      Run2 := Run1; Inc(Run2, ChannelSize);
                      Run3 := Run2; Inc(Run3, ChannelSize);
                      For Y := 0 To Height - 1 Do
                      Begin
                        ColorManager.ConvertRow([Run1, Run2, Run3], ScanLine[Y], Width, $FF);
                        Inc(Run1, Increment);
                        Inc(Run2, Increment);
                        Inc(Run3, Increment);

                        Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                        OffsetRect(FProgressRect, 0, 1);
                      End;
                    End;
                End;
              Finally
                If Assigned(Buffer) Then FreeMem(Buffer);
              End;
            End;
        End;

      Finally
        Decoder.Free;
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      End;
    End;
  End                                   
  Else GraphicExError(gesInvalidImage, ['PSD or PDD']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPSDGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TPSDHeader;
  Dummy: Word;
  Count: Cardinal;

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    If Header.Signature = '8BPS' Then
    Begin
      With Header Do
      Begin
        // PSD files are big endian only
        Channels := Swap(Channels);
        Rows := SwapLong(Rows);
        Columns := SwapLong(Columns);
        Depth := Swap(Depth);
        Mode := Swap(Mode);
      End;

      Options := [ioBigEndian];
      // initialize color manager
      BitsPerSample := Header.Depth;
      Channels := Header.Channels;
      // 1..24 channels are supported in PSD files, we can only use 4.
      // The documentation states that main image data (rgb(a), cmyk etc.) is always
      // written as first channels in their component order.
      If Channels > 4 Then SamplesPerPixel := 4
                      Else SamplesPerPixel := Channels;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      // color space
      Case Header.Mode Of
        PSD_DUOTONE, // duo tone should be handled as grayscale
        PSD_GRAYSCALE:
          ColorScheme := csG;
        PSD_BITMAP:  // B&W
          Begin
            ColorScheme := csG;
            Include(Options, ioMinIsWhite);
          End;
        PSD_INDEXED: // 8 bits only are assumed because 16 bit wouldn't make sense here
          ColorScheme := csIndexed;
        PSD_MULTICHANNEL,
        PSD_RGB:
          If Header.Channels = 3 Then ColorScheme := csRGB
                                 Else ColorScheme := csRGBA;
        PSD_CMYK:
          ColorScheme := csCMYK;
        PSD_LAB:
          ColorScheme := csCIELab;
      End;

      Width := Header.Columns;
      Height := Header.Rows;

      // size of palette
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      // skip palette (count is always given, might be 0 however, e.g. for RGB)
      Seek(Count, soFromCurrent);

      // skip resource and layers section
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);
      ReadBuffer(Count, SizeOf(Count));
      Count := SwapLong(Count);
      Seek(Count, soFromCurrent);

      ReadBuffer(Dummy, SizeOf(Dummy));
      If Swap(Dummy) = 1 Then Compression := ctPackedBits
                         Else Compression := ctNone;
      Result := True;
    End;
  End;
End;

{$ENDIF} // PhotoshopGraphic

//----------------- TPSPGraphic ----------------------------------------------------------------------------------------

{$IFDEF PaintshopProGraphic}

Const
  // block identifiers
  PSP_IMAGE_BLOCK = 0;          // General Image Attributes Block (main)
  PSP_CREATOR_BLOCK = 1;        // Creator Data Block (main)
  PSP_COLOR_BLOCK = 2;          // Color Palette Block (main and sub)
  PSP_LAYER_START_BLOCK = 3;    // Layer Bank Block (main)
    PSP_LAYER_BLOCK = 4;          // Layer Block (sub)
    PSP_CHANNEL_BLOCK = 5;        // Channel Block (sub)
  PSP_SELECTION_BLOCK = 6;      // Selection Block (main)
  PSP_ALPHA_BANK_BLOCK = 7;     // Alpha Bank Block (main)
    PSP_ALPHA_CHANNEL_BLOCK = 8;  // Alpha Channel Block (sub)
  PSP_THUMBNAIL_BLOCK = 9;      // Thumbnail Block (main)
  PSP_EXTENDED_DATA_BLOCK = 10; // Extended Data Block (main)
  PSP_TUBE_BLOCK = 11;          // Picture Tube Data Block (main)
    PSP_ADJUSTMENT_EXTENSION_BLOCK = 12; // Adjustment Layer Extension Block (sub)
    PSP_VECTOR_EXTENSION_BLOCK = 13;     // Vector Layer Extension Block (sub)
    PSP_SHAPE_BLOCK = 14;                // Vector Shape Block (sub)
    PSP_PAINTSTYLE_BLOCK = 15;           // Paint Style Block (sub)
  PSP_COMPOSITE_IMAGE_BANK_BLOCK = 16; // Composite Image Bank (main)
    PSP_COMPOSITE_ATTRIBUTES_BLOCK = 17; // Composite Image Attributes (sub)
    PSP_JPEG_BLOCK = 18;                 // JPEG Image Block (sub)

  // bitmap types
  PSP_DIB_IMAGE = 0;            // Layer color bitmap
  PSP_DIB_TRANS_MASK = 1;       // Layer transparency mask bitmap
  PSP_DIB_USER_MASK = 2;        // Layer user mask bitmap
  PSP_DIB_SELECTION= 3;         // Selection mask bitmap
  PSP_DIB_ALPHA_MASK = 4;       // Alpha channel mask bitmap
  PSP_DIB_THUMBNAIL = 5;        // Thumbnail bitmap
  PSP_DIB_THUMBNAIL_TRANS_MASK = 6; // Thumbnail transparency mask
  PSP_DIB_ADJUSTMENT_LAYER = 7; // Adjustment layer bitmap
  PSP_DIB_COMPOSITE = 8;        // Composite image bitmap
  PSP_DIB_COMPOSITE_TRANS_MASK = 9; // Composite image transparency

  // composite image type
  PSP_IMAGE_COMPOSITE = 0;      // Composite Image
  PSP_IMAGE_THUMBNAIL = 1;      // Thumbnail Image

  // graphic contents flags
  PSP_GC_RASTERLAYERS = 1;      // At least one raster layer
  PSP_GC_VectorLayers = 2;      // At least one vector layer
  PSP_GC_ADJUSTMENTLAYERS = 4;  // At least one adjustment layer
  // Additional attributes
  PSP_GC_THUMBNAIL = $01000000;              // Has a thumbnail
  PSP_GC_THUMBNAILTRANSPARENCY = $02000000;  // Thumbnail transp.
  PSP_GC_COMPOSITE = $04000000;              // Has a composite image
  PSP_GC_COMPOSITETRANSPARENCY = $08000000;  // Composite transp.
  PSP_GC_FLATIMAGE = $10000000;              // Just a background
  PSP_GC_SELECTION = $20000000;              // Has a selection
  PSP_GC_FLOATINGSELECTIONLAYER = $40000000; // Has float. selection
  PSP_GC_ALPHACHANNELS = $80000000;          // Has alpha channel(s)

  // character style flags
  PSP_STYLE_ITALIC = 1;         // Italic property bit
  PSP_STYLE_STRUCK = 2;         // Strike-out property bit
  PSP_STYLE_UNDERLINED = 4;     // Underlined property bit

  // layer flags
  PSP_LAYER_VISIBLEFLAG = 1;    // Layer is visible
  PSP_LAYER_MASKPRESENCEFLAG = 2; // Layer has a mask

  // Shape property flags
  PSP_SHAPE_ANTIALIASED = 1;    // Shape is anti-aliased
  PSP_SHAPE_Selected = 2;       // Shape is selected
  PSP_SHAPE_Visible = 4;        // Shape is visible

  // Polyline node type flags
  PSP_NODE_UNCONSTRAINED = 0;   // Default node type
  PSP_NODE_SMOOTH = 1;          // Node is smooth
  PSP_NODE_SYMMETRIC = 2;       // Node is symmetric
  PSP_NODE_ALIGNED = 4;         // Node is aligned
  PSP_NODE_ACTIVE = 8;          // Node is active
  PSP_NODE_LOCKED = 16;         // Node is locked (PSP doc says 0x16 here, but this seems to be a typo)
  PSP_NODE_SELECTED = 32;       // Node is selected (PSP doc says 0x32 here)
  PSP_NODE_VISIBLE = 64;        // Node is visible (PSP doc says 0x64 here)
  PSP_NODE_CLOSED = 128;        // Node is closed (PSP doc says 0x128 here)

  // Blend modes
  LAYER_BLEND_NORMAL = 0;
  LAYER_BLEND_DARKEN = 1;
  LAYER_BLEND_LIGHTEN = 2;
  LAYER_BLEND_HUE = 3;
  LAYER_BLEND_SATURATION = 4;
  LAYER_BLEND_COLOR = 5;
  LAYER_BLEND_LUMINOSITY = 6;
  LAYER_BLEND_MULTIPLY = 7;
  LAYER_BLEND_SCREEN = 8;
  LAYER_BLEND_DISSOLVE = 9;
  LAYER_BLEND_OVERLAY = 10;
  LAYER_BLEND_HARD_LIGHT = 11;
  LAYER_BLEND_SOFT_LIGHT = 12;
  LAYER_BLEND_DIFFERENCE = 130;
  LAYER_BLEND_DODGE = 14;
  LAYER_BLEND_BURN = 15;
  LAYER_BLEND_EXCLUSION = 16;
  LAYER_BLEND_ADJUST = 255;

  // Adjustment layer types
  PSP_ADJUSTMENT_NONE = 0;      // Undefined adjustment layer type
  PSP_ADJUSTMENT_LEVEL = 1;     // Level adjustment
  PSP_ADJUSTMENT_CURVE = 2;     // Curve adjustment
  PSP_ADJUSTMENT_BRIGHTCONTRAST = 3; // Brightness-contrast adjustment
  PSP_ADJUSTMENT_COLORBAL = 4;  // Color balance adjustment
  PSP_ADJUSTMENT_HSL = 5;       // HSL adjustment
  PSP_ADJUSTMENT_CHANNELMIXER = 6; // Channel mixer adjustment
  PSP_ADJUSTMENT_INVERT = 7;    // Invert adjustment
  PSP_ADJUSTMENT_THRESHOLD = 8; // Threshold adjustment
  PSP_ADJUSTMENT_POSTER = 9;    // Posterize adjustment

  // Vector shape types
  PSP_VST_Unknown = 0;          // Undefined vector type
  PSP_VST_TEXT = 1;             // Shape represents lines of text
  PSP_VST_POLYLINE = 2;         // Shape represents a multiple segment line
  PSP_VST_ELLIPSE = 3;          // Shape represents an ellipse (or circle)
  PSP_VST_POLYGON = 4;          // Shape represents a closed polygon

  // Text element types
  PSP_TET_UNKNOWN = 0;          // Undefined text element type
  PSP_TET_CHAR = 1;             // A single character code
  PSP_TET_CHARSTYLE = 2;        // A character style change
  PSP_TET_LINESTYLE = 3;        // A line style change

  // Text alignment types
  PSP_TAT_LEFT = 0;             // Left text alignment
  PSP_TAT_CENTER = 1;           // Center text alignment
  PSP_TAT_RIGHT = 2;            // Right text alignment

  // Paint style types
  PSP_STYLE_NONE = 0;           // Undefined paint style
  PSP_STYLE_COLOR = 1;          // Paint using color (RGB or palette index)
  PSP_STYLE_GRADIENT = 2;       // Paint using gradient

  // Channel types
  PSP_CHANNEL_COMPOSITE = 0;    // Channel of single channel bitmap
  PSP_CHANNEL_RED = 1;          // Red channel of 24 bit bitmap
  PSP_CHANNEL_GREEN = 2;        // Green channel of 24 bit bitmap
  PSP_CHANNEL_BLUE = 3;         // Blue channel of 24 bit bitmap

  // Resolution metrics
  PSP_METRIC_UNDEFINED = 0;      // Metric unknown
  PSP_METRIC_INCH = 1;          // Resolution is in inches
  PSP_METRIC_CM = 2;            // Resolution is in centimeters

  // Compression types
  PSP_COMP_NONE = 0;            // No compression
  PSP_COMP_RLE = 1;             // RLE compression
  PSP_COMP_LZ77 = 2;            // LZ77 compression
  PSP_COMP_JPEG = 3;            // JPEG compression (only used by thumbnail and composite image)

  // Picture tube placement mode
  PSP_TPM_Random = 0;           // Place tube images in random intervals
  PSPS_TPM_Constant = 1;        // Place tube images in constant intervals

  // Tube selection mode
  PSP_TSM_RANDOM =0;            // Randomly select the next image in tube to display
  PSP_TSM_INCREMENTAL = 1;     // Select each tube image in turn
  PSP_TSM_ANGULAR = 2;          // Select image based on cursor direction
  PSP_TSM_PRESSURE = 3;         // Select image based on pressure (from pressure-sensitive pad)
  PSP_TSM_VELOCITY = 4;         // Select image based on cursor speed

  // Extended data field types
  PSP_XDATA_TRNS_INDEX = 0;     // Transparency index field

  // Creator field types
  PSP_CRTR_FLD_TITLE = 0;       // Image document title field
  PSP_CRTR_FLD_CRT_DATE = 1;    // Creation date field
  PSP_CRTR_FLD_MOD_DATE = 2;    // Modification date field
  PSP_CRTR_FLD_ARTIST = 3;      // Artist name field
  PSP_CRTR_FLD_CPYRGHT = 4;     // Copyright holder name field
  PSP_CRTR_FLD_DESC = 5;        // Image document description field
  PSP_CRTR_FLD_APP_ID = 6;      // Creating app id field
  PSP_CRTR_FLD_APP_VER = 7;     // Creating app version field

  // Creator application identifier
  PSP_CREATOR_APP_UNKNOWN = 0;  // Creator application unknown
  PSP_CREATOR_APP_PAINT_SHOP_PRO = 1; // Creator is Paint Shop Pro

  // Layer types (file version 3)
  PSP_LAYER_NORMAL = 0;         // Normal layer
  PSP_LAYER_FLOATING_SELECTION = 1; // Floating selection layer

  // Layer types (file version 4)
  PSP_LAYER_UNDEFINED = 0;      // Undefined layer type
  PSP_LAYER_RASTER = 1;         // Standard raster layer
  PSP_LAYER_FLOATINGRASTERSELECTION = 2; // Floating selection (raster layer)
  PSP_LAYER_Vector = 3;         // Vector layer
  PSP_LAYER_ADJUSTMENT = 4;     // Adjustment layer

  MagicID = 'Paint Shop Pro Image File';

Type
  // These block header structures are here for informational purposes only because the data of those
  // headers is read member by member to generalize code for the different file versions
  TPSPBlockHeader3 = Packed Record          // block header file version 3
    HeaderIdentifier: array[0..3] of AnsiChar;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    InitialChunkLength,                     // length of the first sub chunk header or similar
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  End;

  TPSPBlockHeader4 = Packed Record          // block header file version 4
    HeaderIdentifier: array[0..3] of AnsiChar;  // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  End;

  TPSPColorPaletteInfoChunk = Packed Record
    EntryCount: Cardinal;                   // number of entries in the palette
  End;

  TPSPColorPaletteChunk = Array[0..255] Of TRGBQuad; // might actually be shorter 

  TPSPChannelInfoChunk = Packed Record
    CompressedSize,
    UncompressedSize: Cardinal;
    BitmapType,                             // one of the bitmap types
    ChannelType: Word;                      // one of the channel types
  End;

  // PSP defines a channel content chunk which is just a bunch of bytes (size is CompressedSize).
  // There is no sense to define this record type here.

  TPSPFileHeader = Packed Record
    Signature: array[0..31] of AnsiChar;        // the string "Paint Shop Pro Image File\n\x1a", padded with zeroes
    MajorVersion,
    MinorVersion: Word;                
  End;

  TPSPImageAttributes = Packed Record
    Width,
    Height: Integer;
    Resolution: Double;                     // Number of pixels per metric
    ResolutionMetric: Byte;                 // Metric used for resolution (one of the metric constants)
    Compression,                            // compression type of image (not thumbnail, it has its own compression)
    BitDepth,                               // The bit depth of the color bitmap in each Layer of the image document
                                            // (must be 1, 4, 8 or 24).
    PlaneCount: Word;                       // Number of planes in each layer of the image document (usually 1)
    ColorCount: Cardinal;                   // number of colors in each layer (2^bit depth)
    GreyscaleFlag: Boolean;                 // Indicates whether the color bitmap in each layer of image document is a
                                            // greyscale (False = not greyscale, True = greyscale).
    TotalImageSize: Cardinal;               // Sum of the sizes of all layer color bitmaps.
    ActiveLayer: Integer;                   // Identifies the layer that was active when the image document was saved.
    LayerCount: Word;                       // Number of layers in the document.
    GraphicContents: Cardinal;              // A series of flags that helps define the image's graphic contents.
  End;

  TPSPLayerInfoChunk = Packed Record
    //LayerName: array[0..255] of AnsiChar;     // Name of layer (in ASCII text). Has been replaced in version 4
                                            // by a Delphi like short string (length word and variable length string)
    LayerType: Byte;                        // Type of layer.
    ImageRectangle,                         // Rectangle defining image border.
    SavedImageRectangle: TRect;             // Rectangle within image rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    LayerOpacity: Byte;                     // Overall layer opacity.
    BlendingMode: Byte;                     // Mode to use when blending layer.
    Visible: Boolean;                       // TRUE if layer was visible at time of save, FALSE otherwise.
    TransparencyProtected: Boolean;         // TRUE if transparency is protected.
    LinkGroupIdentifier: Byte;              // Identifies group to which this layer belongs.
    MaskRectangle,                          // Rectangle defining user mask border.
    SavedMaskRectangle: TRect;              // Rectangle within mask rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    MaskLinked: Boolean;                    // TRUE if mask linked to layer (i.e., mask moves relative to layer)
    MaskDisabled: Boolean;                  // TRUE if mask is disabled, FALSE otherwise.
    InvertMask: Boolean;                    // TRUE if mask should be inverted when the layer is merged, FALSE otherwise.
    BlendRangeCount: Word;                  // Number of valid source-destination field pairs to follow (note, there are
                                            // currently always 5 such pairs, but they are not necessarily all valid).
    SourceBlendRange1,                      // First source blend range value.
    DestinationBlendRange1,                 // First destination blend range value.
    SourceBlendRange2,
    DestinationBlendRange2,
    SourceBlendRange3,
    DestinationBlendRange3,
    SourceBlendRange4,
    DestinationBlendRange4,
    SourceBlendRange5,
    DestinationBlendRange5: Array[0..3] Of Byte;
    // these fields are obsolete since file version 4 because there's an own chunk for them
    // BitmapCount: Word;                      // Number of bitmaps to follow.
    // ChannelCount: Word;                     // Number of channels to follow.
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TPSPGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Header: TPSPFileHeader;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Header);
    If Result Then
    Begin
      ReadBuffer(Header, SizeOf(Header));
      Result := (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) And
                (Header.MajorVersion >= 3);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPSPGraphic.LoadFromStream(Stream: TStream);

Var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of AnsiChar;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  LastPosition,
  ChunkSize: Cardinal;
  LayerInfo: TPSPLayerInfoChunk;
  ChannelInfo: TPSPChannelInfoChunk;
  LayerName: AnsiString;
  NameLength: Word;
  NextLayerPosition,
  NextMainBlock: Integer;

  // file version 4 specific data
  BitmapCount,
  ChannelCount: Word;

  // load and decoding of image data
  R, G, B, C: PByte;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  CompBuffer: Pointer;
  X, Y,
  Index,
  RowSize: Integer; // size in bytes of one scanline 

  // other data
  RawPalette: Array[0..4 * 256 - 1] Of Byte;

  //--------------- local functions -------------------------------------------

  Function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end). 

  Begin
    With Stream Do
    Begin
      Result := Position < Size;
      If Result Then
      Begin
        ReadBuffer(HeaderIdentifier, SizeOf(HeaderIdentifier));
        ReadBuffer(BlockIdentifier, SizeOf(BlockIdentifier));
        If Header.MajorVersion = 3 Then ReadBuffer(InitialChunkLength, SizeOf(InitialChunkLength));
        ReadBuffer(TotalBlockLength, SizeOf(TotalBlockLength));
      End;
    End;
  End;

  //---------------------------------------------------------------------------

  Procedure ReadAndDecompress(Target: Pointer);

  // reads a stream of data from file stream and decompresses it into Target

  Var
    RawBuffer: Pointer;
    Decoder: TDecoder;
    Source: Pointer;

  Begin
    Decoder := Nil;
    GetMem(RawBuffer, ChannelInfo.CompressedSize);
    Try
      Stream.ReadBuffer(RawBuffer^, ChannelInfo.CompressedSize);
      // pointer might be advanced while decoding, so use a copy
      Source := RawBuffer;
      Case Image.Compression Of
        PSP_COMP_RLE:
          Begin
            Decoder := TPSPRLEDecoder.Create;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          End;
        PSP_COMP_LZ77:
          Begin
            Decoder := TLZ77Decoder.Create(Z_FINISH, False);
            Decoder.DecodeInit;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          End;
        PSP_COMP_JPEG: // here just for completeness, used only in thumbnails and composite images
          ;
      End;
      Decoder.DecodeEnd;
    Finally
      If Assigned(RawBuffer) Then FreeMem(RawBuffer);
      Decoder.Free;
    End;
  End;

  //---------------------------------------------------------------------------

  Procedure ReadChannelData;

  // Reads the actual data of one channel from the current stream position.
  // Decompression is done by the way.

  Begin
    With Stream Do
    Begin
      ReadBlockHeader;
      If Header.MajorVersion > 3 Then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(ChannelInfo, SizeOf(ChannelInfo));
      Case ChannelInfo.ChannelType Of
        PSP_CHANNEL_COMPOSITE: // single channel bitmap (indexed or transparency mask)
          Begin
            GetMem(CompBuffer, ChannelInfo.UncompressedSize);
            If Image.Compression <> PSP_COMP_NONE Then ReadAndDecompress(CompBuffer)
                                                  Else ReadBuffer(CompBuffer^, ChannelInfo.CompressedSize);
          End;
        PSP_CHANNEL_RED:  // red channel of 24 bit bitmap
          Begin
            GetMem(RedBuffer, ChannelInfo.UncompressedSize);
            If Image.Compression <> PSP_COMP_NONE Then ReadAndDecompress(RedBuffer)
                                                  Else ReadBuffer(RedBuffer^, ChannelInfo.CompressedSize);
          End;
        PSP_CHANNEL_GREEN:
          Begin
            GetMem(GreenBuffer, ChannelInfo.UncompressedSize);
            If Image.Compression <> PSP_COMP_NONE Then ReadAndDecompress(GreenBuffer)
                                                  Else ReadBuffer(GreenBuffer^, ChannelInfo.CompressedSize);
          End;
        PSP_CHANNEL_BLUE:
          Begin
            GetMem(BlueBuffer, ChannelInfo.UncompressedSize);
            If Image.Compression <> PSP_COMP_NONE Then ReadAndDecompress(BlueBuffer)
                                                  Else ReadBuffer(BlueBuffer^, ChannelInfo.CompressedSize);
          End;
      End;
    End;
  End;

  //--------------- end local functions ---------------------------------------

Begin
  // free previous image data
  Handle := 0;
  FBasePosition := Stream.Position;
  If ReadImageProperties(Stream, 0) Then
  Begin
    Stream.Position := FBasePosition;
    RedBuffer := Nil;
    GreenBuffer := Nil;
    BlueBuffer := Nil;
    With Stream, FImageProperties Do
    Try
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // Note: To be robust with future PSP images any reader must be able to skip data
      //       which it doesn't know instead of relying on the size of known structures.
      //       Hence there's some extra work needed with the stream (mainly to keep the
      //       current position before a chunk is read and advancing the stream using the
      //       chunk size field).
      ReadBuffer(Header, SizeOf(Header));

      // read general image attribute block
      ReadBlockHeader;
      LastPosition := Position;
      If Version > 3 Then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(Image, SizeOf(Image));
      Position := LastPosition + TotalBlockLength;

      With ColorManager, Image Do
      Begin
        SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := BitsPerSample;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        If ColorScheme = csRGB Then TargetColorScheme := csBGR
                               Else TargetColorScheme := ColorScheme;

        PixelFormat := TargetPixelFormat;
      End;

      // set bitmap properties
      RowSize := 0; // make compiler quiet
      Case BitsPerSample Of
        1:
          RowSize := (Image.Width + 7) Div 8;
        4:
          RowSize := Image.Width Div 2 + 1;
        8:
          RowSize := Image.Width;
      Else
        GraphicExError(gesInvalidColorFormat, ['PSP']);
      End;

      Self.Width := Width;
      Self.Height := Height;
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      // go through main blocks and read what is needed
      Repeat
        If Not ReadBlockHeader Then Break;
        NextMainBlock := Position + Integer(TotalBlockLength);
        // no more blocks?
        If HeaderIdentifier[0] <> '~' Then Break;

        Case BlockIdentifier Of
          PSP_COMPOSITE_IMAGE_BANK_BLOCK:
            Begin
              // composite image block, if present then it must appear before the layer start block
              // and represents a composition of several layers

              // do not need to read anything further
              //Break;
            End;
          PSP_LAYER_START_BLOCK:
            Repeat
              If Not ReadBlockHeader Then Break;

              Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);

              // calculate start of next (layer) block in case we need to skip this one
              NextLayerPosition := Position + Integer(TotalBlockLength);
              // if all layers have been considered the break loop to continue with other blocks if necessary
              If BlockIdentifier <> PSP_LAYER_BLOCK Then Break;

              // layer information chunk
              If Version > 3 Then
              Begin
                LastPosition := Position;
                ReadBuffer(ChunkSize, SizeOf(ChunkSize));
                ReadBuffer(NameLength, SizeOf(NameLength));
                SetLength(LayerName, NameLength);
                If NameLength > 0 Then ReadBuffer(LayerName[1], NameLength);
                ReadBuffer(LayerInfo, SizeOf(LayerInfo));
                Position := LastPosition + ChunkSize;

                // continue only with undefined or raster chunks
                If Not (LayerInfo.LayerType In [PSP_LAYER_UNDEFINED, PSP_LAYER_RASTER]) Then
                Begin
                  Position := NextLayerPosition;
                  Continue;
                End;

                // in file version 4 there's also an additional bitmap chunk which replaces
                // two fields formerly located in the LayerInfo chunk
                LastPosition := Position;
                ReadBuffer(ChunkSize, SizeOf(ChunkSize));
              End
              Else
              Begin
                SetLength(LayerName, 256);
                ReadBuffer(LayerName[1], 256);
                ReadBuffer(LayerInfo, SizeOf(LayerInfo));

                // continue only with normal (raster) chunks
                If LayerInfo.LayerType <> PSP_LAYER_NORMAL Then
                Begin
                  Position := NextLayerPosition;
                  Continue;
                End;
              End;

              ReadBuffer(BitmapCount, SizeOf(BitmapCount));
              ReadBuffer(ChannelCount, SizeOf(ChannelCount));

              // But now we can reliably say whether we have an alpha channel or not.
              // This kind of information can only be read very late and causes us to
              // possibly reallocate the entire image (because it is copied by the VCL
              // when changing the pixel format).
              // I don't know another way (preferably before the size of the image is set).
              If ChannelCount > 3 Then
              Begin
                ColorManager.TargetColorScheme := csBGRA;
                PixelFormat := pf32Bit;
              End;

              If Version > 3 Then Position := LastPosition + ChunkSize;

              // allocate memory for all channels and read raw data
              For X := 0 To ChannelCount - 1 Do ReadChannelData;
              Progress(Self, psEnding, 0, False, FProgressRect, '');

              Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
              R := RedBuffer;
              G := GreenBuffer;
              B := BlueBuffer;
              C := CompBuffer;
              With ColorManager Do
              Begin
                If TargetColorScheme In [csIndexed, csG] Then
                Begin
                  For Y := 0 To Height - 1 Do
                  Begin
                    ColorManager.ConvertRow([C], ScanLine[Y], Width, $FF);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  End;
                End
                Else
                Begin
                  For Y := 0 To Height - 1 Do
                  Begin
                    ColorManager.ConvertRow([R, G, B, C], ScanLine[Y], Width, $FF);
                    Inc(R, RowSize);
                    Inc(G, RowSize);
                    Inc(B, RowSize);
                    Inc(C, RowSize);

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  End;
                End;
              End;
              Progress(Self, psEnding, 0, False, FProgressRect, '');
              // after the raster layer has been read there's no need to loop further
              Break;
            Until False; // layer loop
          PSP_COLOR_BLOCK:  // color palette block (this is also present for gray scale and b&w images)
            Begin
              If Version > 3 Then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
              ReadBuffer(Index, SizeOf(Index));
              ReadBuffer(RawPalette, Index * SizeOf(TRGBQuad));
              Palette := ColorManager.CreateColorPalette([@RawPalette], pfInterlaced8Quad, Index, True);
            End;
        End;

        // explicitly set stream position to next main block as we might have read a block only partially
        Position := NextMainBlock;
      Until False; // main block loop
    Finally
      If Assigned(RedBuffer) Then FreeMem(RedBuffer);
      If Assigned(GreenBuffer) Then FreeMem(GreenBuffer);
      If Assigned(BlueBuffer) Then FreeMem(BlueBuffer);
    End;
  End
  Else GraphicExError(gesInvalidImage, ['PSP']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPSPGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of AnsiChar;  // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  LastPosition,
  ChunkSize: Cardinal;

  //--------------- local functions -------------------------------------------

  Function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end). 

  Begin
    With Stream Do
    Begin
      Result := Position < Size;
      If Result Then
      Begin
        ReadBuffer(HeaderIdentifier, SizeOf(HeaderIdentifier));
        ReadBuffer(BlockIdentifier, SizeOf(BlockIdentifier));
        If Header.MajorVersion = 3 Then ReadBuffer(InitialChunkLength, SizeOf(InitialChunkLength));
        ReadBuffer(TotalBlockLength, SizeOf(TotalBlockLength));
      End;
    End;
  End;

  //--------------- end local functions ---------------------------------------

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Header, SizeOf(Header));
    If (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) And
       (Header.MajorVersion >= 3) Then
    Begin
      Version := Header.MajorVersion;

      // read general image attribute block
      ReadBlockHeader;
      LastPosition := Position;
      If Header.MajorVersion > 3 Then ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      ReadBuffer(Image, SizeOf(Image));
      Position := LastPosition + TotalBlockLength;

      If Image.BitDepth = 24 Then
      Begin
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        ColorScheme := csRGB; // an alpha channel might exist, this is determined by the layer's channel count 
      End
      Else
      Begin
        BitsPerSample := Image.BitDepth;
        SamplesPerPixel := 1;
        If Image.GreyscaleFlag Then ColorScheme := csG
                               Else ColorScheme := csIndexed;
      End;
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      Width := Image.Width;
      Height := Image.Height;

      Case Image.Compression Of
        PSP_COMP_NONE:
          Compression := ctNone;
        PSP_COMP_RLE:
          Compression := ctRLE;
        PSP_COMP_LZ77:
          Compression := ctLZ77;
        PSP_COMP_JPEG:
          Compression := ctJPEG;
      Else
        Compression := ctUnknown;
      End;
      XResolution := Image.Resolution;
      If Image.ResolutionMetric = PSP_METRIC_CM Then XResolution := XResolution * 2.54;
      YResolution := XResolution;
      Result := True;
    End;
  End;
End;

{$ENDIF} // PaintshopProGraphic

//----------------- TPNGGraphic ----------------------------------------------------------------------------------------

{$IFDEF PortableNetworkGraphic}

Const
  PNGMagic: Array[0..7] Of Byte = (137, 80, 78, 71, 13, 10, 26, 10);

  // recognized and handled chunk types
  IHDR: TChunkType = 'IHDR';
  IDAT: TChunkType = 'IDAT';
  IEND: TChunkType = 'IEND';
  PLTE: TChunkType = 'PLTE';
  gAMA: TChunkType = 'gAMA';
  tRNS: TChunkType = 'tRNS';
  bKGD: TChunkType = 'bKGD';

  CHUNKMASK = $20; // used to check bit 5 in chunk types

Type
  // The following chunks structures are those which appear in the data field of the general chunk structure
  // given above.

  // chunk type: 'IHDR'
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = Packed Record
    Width,
    Height: Cardinal;
    BitDepth,          // bits per sample (allowed are 1, 2, 4, 8 and 16)
    ColorType,         // combination of:
                       //   1 - palette used
                       //   2 - colors used
                       //   4 - alpha channel used
                       // allowed values are:
                       //   0 - gray scale (allowed bit depths are: 1, 2, 4, 8, 16)
                       //   2 - RGB (8, 16)
                       //   3 - palette (1, 2, 4, 8)
                       //   4 - gray scale with alpha (8, 16)
                       //   6 - RGB with alpha (8, 16)
    Compression,       // 0 - LZ77, others are not yet defined
    Filter,            // filter mode 0 is the only one currently defined
    Interlaced: Byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  End;

//----------------------------------------------------------------------------------------------------------------------

Class Function TPNGGraphic.CanLoad(Stream: TStream): Boolean;

Var
  Magic: Array[0..7] Of Byte;
  LastPosition: Cardinal;

Begin
  With Stream Do
  Begin
    LastPosition := Position;
    Result := (Size - Position) > SizeOf(Magic);
    If Result Then
    Begin
      ReadBuffer(Magic, SizeOf(Magic));
      Result := CompareMem(@Magic, @PNGMagic, 8);
    End;
    Position := LastPosition;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPNGGraphic.IsChunk(ChunkType: TChunkType): Boolean;

// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type

Const
  Mask = Not $20202020;
Begin
  Result := (FHeader.Mask and Mask) = (PDWORD(@ChunkType)^ and Mask);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPNGGraphic.LoadAndSwapHeader: Cardinal;

// read next chunk header and swap fields to little endian,
// returns the intial CRC value for following checks

Begin
  FStream.ReadBuffer(FHeader, SizeOf(FHeader));
  Result := CRC32(0, @FHeader.ChunkType, 4);
  FHeader.Length := SwapLong(FHeader.Length);
End;

//----------------------------------------------------------------------------------------------------------------------

Function PaethPredictor(a, b, c: Byte): Byte;

Var
  p, pa, pb, pc: Integer;

Begin
  // a = left, b = above, c = upper left
  p := a + b - c;        // initial estimate
  pa := Abs(p - a);      // distances to a, b, c
  pb := Abs(p - b);
  pc := Abs(p - c);
  // return nearest of a, b, c, breaking ties in order a, b, c
  If (pa <= pb) And (pa <= pc) Then Result := a
                               Else
    If pb <= pc Then Result := b
                Else Result := c;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);

// Applies the filter given in Filter to all bytes in Line (eventually using PrevLine).
// Note: The filter type is assumed to be of filter mode 0, as this is the only one currently
//       defined in PNG.
//       In opposition to the PNG documentation different identifiers are used here.
//       Raw refers to the current, not yet decoded value. Decoded refers to the current, already
//       decoded value (this one is called "raw" in the docs) and Prior is the current value in the
//       previous line. For the Paeth prediction scheme a fourth pointer is used (PriorDecoded) to describe
//       the value in the previous line but less the BPP value (Prior[x - BPP]).      

Var
  I: Integer;
  Raw,
  Decoded,
  Prior,
  PriorDecoded,
  TargetRun: PByte;

Begin
  Case Filter Of
    0: // no filter, just copy data
      Move(Line^, Target^, BytesPerRow);
    1: // subtraction filter
      Begin
        Raw := Line;
        TargetRun := Target;
        // Transfer BPP bytes without filtering. This mimics the effect of bytes left to the
        // scanline being zero.
        Move(Raw^, TargetRun^, BPP);

        // now do rest of the line
        Decoded := TargetRun;
        Inc(Raw, BPP);
        Inc(TargetRun, BPP);
        Dec(BytesPerRow, BPP);
        While BytesPerRow > 0 Do
        Begin
          TargetRun^ := Byte(Raw^ + Decoded^);
          Inc(Raw);
          Inc(Decoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
        End;
      End;
    2: // Up filter
      Begin
        Raw := Line;
        Prior := PrevLine;
        TargetRun := Target;
        While BytesPerRow > 0 Do
        Begin
          TargetRun^ := Byte(Raw^ + Prior^);
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        End;
      End;
    3: // average filter
      Begin
        // first handle BPP virtual pixels to the left
        Raw := Line;
        Decoded := Line;
        Prior := PrevLine;
        TargetRun := Target;
        For I := 0 To BPP - 1 Do
        Begin
          TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
        End;
        Dec(BytesPerRow, BPP);

        // now do rest of line
        While BytesPerRow > 0 Do
        Begin
          TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        End;
      End;
   4: // paeth prediction
     Begin
       // again, start with first BPP pixel which would refer to non-existing pixels to the left
       Raw := Line;
       Decoded := Target;
       Prior := PrevLine;
       PriorDecoded := PrevLine;
       TargetRun := Target;
       For I := 0 To BPP - 1 Do
       Begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
         Inc(Raw);
         Inc(Prior);
         Inc(TargetRun);
       End;
       Dec(BytesPerRow, BPP);

       // finally do rest of line
       While BytesPerRow > 0 Do
       Begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(PriorDecoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
       End;
     End;
   End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.LoadFromStream(Stream: TStream);

Var
  Description: TIHDRChunk;

Begin
  // free previous image data
  Handle := 0;

  FBasePosition := Stream.Position;
  FDecoder := Nil;
  FStream := Stream;
  If ReadImageProperties(Stream, 0) Then
  Begin
    With Stream, FImageProperties Do
    Begin
      Position := FBasePosition + 8; // skip magic

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FPalette := 0;
      FTransparency := Nil;
      FBackgroundColor := clWhite;
      FTransparentColor := clNone;

      // first chunk must be an IHDR chunk
      FCurrentCRC := LoadAndSwapHeader;

      FRawBuffer := Nil;
      ColorManager.SourceOptions := [coNeedByteSwap];
      Try
        // read IHDR chunk
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(@Description, 2);

        // currently only one compression type is supported by PNG (LZ77)
        If Compression = ctLZ77 Then
        Begin
          FDecoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
          FDecoder.DecodeInit;
        End
        Else
          GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PNG']);

        // setup is done, now go for the chunks
        Repeat
          FCurrentCRC := LoadAndSwapHeader;
          If IsChunk(IDAT) Then
          Begin
            Progress(Self, psEnding, 0, False, FProgressRect, '');
            LoadIDAT(Description);
            // After reading the image data the next chunk header has already been loaded
            // so continue with code below instead trying to load a new chunk header.
          End
          Else
            If IsChunk(PLTE) Then
            Begin
              // palette chunk
              If (FHeader.Length Mod 3) <> 0 Then GraphicExError(gesInvalidPalette, ['PNG']);
              ReadDataAndCheckCRC;
              // load palette only if the image is indexed colors
              If Description.ColorType = 3 Then
              Begin
                // first setup pixel format before actually creating a palette
                FSourceBPP := SetupColorDepth(Description.ColorType, Description.BitDepth);
                FPalette := ColorManager.CreateColorPalette([FRawBuffer], pfInterlaced8Triple, FHeader.Length Div 3, False);
              End;
              Continue;
            End
            Else                             
              If IsChunk(gAMA) Then
              Begin
                ReadDataAndCheckCRC;
                // the file gamme given here is a scaled cardinal (e.g. 0.45 is expressed as 45000)
                ColorManager.SetGamma(SwapLong(PCardinal(FRawBuffer)^) / 100000);
                ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
                Include(Options, ioUseGamma);
                Continue;
              End
              Else
                If IsChunk(bKGD) Then
                Begin
                  LoadBackgroundColor(Description);
                  Continue;
                End
                Else
                  If IsChunk(tRNS) Then
                  Begin
                    LoadTransparency(Description);
                    Continue;
                  End;

          // Skip unknown or unsupported chunks (+4 because of always present CRC).
          // IEND will be skipped as well, but this chunk is empty, so the stream will correctly
          // end on the first byte after the IEND chunk.
          Seek(FHeader.Length + 4, soFromCurrent);
          If IsChunk(IEND) Then Break;

          // Note: According to the specs an unknown, but as critical marked chunk is a fatal error.
          If (Byte(FHeader.ChunkType[0]) And CHUNKMASK) = 0 Then GraphicExError(gesUnknownCriticalChunk);
        Until False;
      Finally
        If Assigned(FDecoder) Then
        Begin
          FDecoder.DecodeEnd;
          FDecoder.Free;
        End;
        If Assigned(FRawBuffer) Then FreeMem(FRawBuffer);
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      End;
    End;
  End
  Else GraphicExError(gesInvalidImage, ['PNG']);
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPNGGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

Var
  Magic: Array[0..7] Of Byte;
  Description: TIHDRChunk;

Begin
  Result := Inherited ReadImageProperties(Stream, ImageIndex);
  FStream := Stream;
  With Stream, FImageProperties Do
  Begin
    ReadBuffer(Magic, 8);
    If CompareMem(@Magic, @PNGMagic, 8) Then
    Begin
      // first chunk must be an IHDR chunk
      FCurrentCRC := LoadAndSwapHeader;
      If IsChunk(IHDR) Then
      Begin
        Include(Options, ioBigEndian);
        // read IHDR chunk
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(@Description, 2);

        If (Description.Width = 0) Or (Description.Height = 0) Then Exit;

        Width := Description.Width;
        Height := Description.Height;

        If Description.Compression = 0 Then Compression := ctLZ77
                                       Else Compression := ctUnknown;

        BitsPerSample := Description.BitDepth;
        SamplesPerPixel := 1;
        Case Description.ColorType Of
          0:
            ColorScheme := csG;
          2:
            Begin
              ColorScheme := csRGB;
              SamplesPerPixel := 3;
            End;
          3:
            ColorScheme := csIndexed;
          4:
            ColorScheme := csGA;
          6:
            Begin
              ColorScheme := csRGBA;
              SamplesPerPixel := 4;
            End;
        Else
          ColorScheme := csUnknown;
        End;

        BitsPerPixel := SamplesPerPixel * BitsPerSample;
        FilterMode := Description.Filter;
        Interlaced := Description.Interlaced <> 0;
        HasAlpha := ColorScheme In [csGA, csRGBA, csBGRA];

        // find gamma 
        Repeat
          FCurrentCRC := LoadAndSwapHeader;
          If IsChunk(gAMA) Then
          Begin
            ReadDataAndCheckCRC;
            // the file gamme given here is a scaled cardinal (e.g. 0.45 is expressed as 45000)
            FileGamma := SwapLong(PCardinal(FRawBuffer)^) / 100000;
            Break;
          End;

          Seek(FHeader.Length + 4, soFromCurrent);
          If IsChunk(IEND) Then Break;
        Until False;

        Result := True;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.LoadBackgroundColor(Const Description);

// loads the data from the current chunk (must be a bKGD chunk) and fills the bitmpap with that color

Var
  Run: PWord;
  R, G, B: Byte;

Begin
  ReadDataAndCheckCRC;
  With TIHDRChunk(Description) Do
  Begin
    Case ColorType Of
      0, 4: // G(A)
        Begin
          Case BitDepth Of
            2:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          Else // 1, 4, 8 bits gray scale
            FBackgroundColor := Byte(Swap(PWord(FRawBuffer)^));
          End;
        End;
      2, 6:  // RGB(A)
        Begin
          Run := FRawBuffer;
          If BitDepth = 16 Then
          Begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          End
          Else
          Begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          End;
          FBackgroundColor := RGB(R, G, B);
        End;
    Else // indexed color scheme (3)
      FBackgroundColor := PByte(FRawBuffer)^;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.LoadIDAT(Const Description);

// loads image data from the current position of the stream

Const
  // interlace start and offsets
  RowStart: Array[0..6] Of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: Array[0..6] Of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: Array[0..6] Of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: Array[0..6] Of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask: Array[0..6] Of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

Var
  Row: Integer;
  TargetBPP: Integer;
  RowBuffer: array[Boolean] of PAnsiChar; // I use PChar here instead of simple pointer to ease pointer math below
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  BytesPerRow,
  InterlaceRowBytes,
  InterlaceWidth: Integer;

Begin
  Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
  RowBuffer[False] := Nil;
  RowBuffer[True] := Nil;
  Try
    // adjust pixel format etc. if not yet done
    If PixelFormat = pfDevice Then
      FSourceBPP := SetupColorDepth(TIHDRChunk(Description).ColorType, TIHDRChunk(Description).BitDepth);

    If TIHDRChunk(Description).BitDepth = 16 Then TargetBPP := FSourceBPP Div 2
                                             Else TargetBPP := FSourceBPP;

    If FPalette <> 0 Then Palette := FPalette;
    // after setting the pixel format we can set the dimensions too without
    // initiating color conversions
    Width := TIHDRChunk(Description).Width;
    Height := TIHDRChunk(Description).Height;

    // set background and transparency color, these values must be set after the
    // bitmap is actually valid (although, not filled)
    Canvas.Lock;
    Try
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    Finally
      Canvas.Unlock;
    End;
    If FTransparentColor <> clNone Then
    Begin
      TransparentColor := FTransparentColor;
      Transparent := True;
    End;

    // determine maximum number of bytes per row and consider there's one filter byte at the start of each row
    BytesPerRow := TargetBPP * ((Width * TIHDRChunk(Description).BitDepth + 7) Div 8) + 1;

    RowBuffer[True] := AllocMem(BytesPerRow);
    RowBuffer[False] := AllocMem(BytesPerRow);

    // there can be more than one IDAT chunk in the file but then they must directly
    // follow each other (handled in ReadRow)
    EvenRow := True;

    // prepare interlaced images
    If TIHDRChunk(Description).Interlaced = 1 Then
    Begin
      For Pass := 0 To 6 Do
      Begin
        // prepare next interlace run
        If Width <= ColumnStart[Pass] Then Continue;
        InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 - ColumnStart[Pass]) Div ColumnIncrement[Pass];
        InterlaceRowBytes := TargetBPP * ((InterlaceWidth * TIHDRChunk(Description).BitDepth + 7) Div 8) + 1;

        Row := RowStart[Pass];
        While Row < Height Do
        Begin
          ReadRow(RowBuffer[EvenRow], InterlaceRowBytes);
          ApplyFilter(Byte(RowBuffer[EvenRow]^),
                      Pointer(RowBuffer[EvenRow] + 1),
                      Pointer(RowBuffer[Not EvenRow] + 1),
                      Pointer(RowBuffer[EvenRow] + 1),
                      FSourceBPP,
                      InterlaceRowBytes - 1);

          ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, PassMask[Pass]);
          EvenRow := Not EvenRow;
          // continue with next row in interlaced order
          Inc(Row, RowIncrement[Pass]);

          If Pass = 6 Then
          Begin
            // progress event only for last (and most expensive) pass
            Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          End;
        End;
      End;
    End
    Else
    Begin
      For Row := 0 To Height - 1 Do
      Begin
        ReadRow(RowBuffer[EvenRow], BytesPerRow);
        ApplyFilter(Byte(RowBuffer[EvenRow]^),
                    Pointer(RowBuffer[EvenRow] + 1),
                    Pointer(RowBuffer[Not EvenRow] + 1),
                    Pointer(RowBuffer[EvenRow] + 1),
                    FSourceBPP,
                    BytesPerRow - 1);

        ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, $FF);
        EvenRow := Not EvenRow;

        Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      End;
    End;

    // in order to improve safe failness we read all remaining but not read IDAT chunks here
    While IsChunk(IDAT) Do
    Begin
      ReadDataAndCheckCRC;   
      FCurrentCRC := LoadAndSwapHeader;
    End;
  Finally
    If Assigned(RowBuffer[True]) Then FreeMem(RowBuffer[True]);
    If Assigned(RowBuffer[False]) Then FreeMem(RowBuffer[False]);
  End;
  // ending progress event is issued in main method
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.LoadTransparency(Const Description);

// reads the data of the current transparency chunk

Var
  Run: PWord;
  R, G, B: Byte;
  
Begin
  ReadDataAndCheckCRC;
  With TIHDRChunk(Description) Do
  Begin
    Case ColorType Of
      0: // gray
        Begin
          Case BitDepth Of
            2:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
            16:
              R := MulDiv16(Swap(PWord(FRawBuffer)^), 255, 65535);
          Else // 1, 4, 8 bits gray scale
            R := Byte(Swap(PWord(FRawBuffer)^));
          End;
          FTransparentColor := RGB(R, R, R);
        End;
      2:  // RGB
        Begin
          Run := FRawBuffer;
          If BitDepth = 16 Then
          Begin
            R := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(Swap(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(Swap(Run^), 255, 65535); 
          End
          Else
          Begin
            R := Byte(Swap(Run^)); Inc(Run);
            G := Byte(Swap(Run^)); Inc(Run);
            B := Byte(Swap(Run^));
          End;
          FTransparentColor := RGB(R, G, B);
        End;
      4, 6:
        // formats with full alpha channel, they shouldn't have a transparent color 
    Else
      // Indexed color scheme (3), with at most 256 alpha values (for each palette entry).
      SetLength(FTransparency, 255);
      // read the values (at most 256)...
      Move(FRawBuffer^,  FTransparency[0], Max(FHeader.Length, 256));
      // ...and set default values (255, fully opaque) for non-supplied values
      If FHeader.Length < 256 Then FillChar(FTransparency[FHeader.Length], 256 - FHeader.Length, $FF);
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.ReadDataAndCheckCRC;

// Allocates memory in FRawBuffer and reads the next Header.Length bytes from Stream.
// Furthermore, the CRC value following the data is read as well and compared with
// the CRC value which is calculated here.

Var
  FileCRC: Cardinal;

Begin
  ReallocMem(FRawBuffer, FHeader.Length);
  FStream.ReadBuffer(FRawBuffer^, FHeader.Length);
  FStream.ReadBuffer(FileCRC, SizeOf(FileCRC));
  FileCRC := SwapLong(FileCRC);
  // The type field of a chunk is included in the CRC, this serves as initial value
  // for the calculation here and is determined in LoadAndSwapHeader.
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  If FCurrentCRC <> FileCRC Then GraphicExError(gesInvalidCRC, ['PNG']);
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TPNGGraphic.ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);

// reads and decodes one scanline

Var
  LocalBuffer: Pointer;
  PendingOutput: Integer;

Begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  Repeat
    // read pending chunk data if available input has dropped to zero
    If FDecoder.AvailableInput = 0 Then
    Begin
      FIDATSize := 0;
      // read all following chunks until enough data is available or there is no further IDAT chunk
      While FIDATSize = 0 Do
      Begin
        // finish if the current chunk is not an IDAT chunk
        If Not IsChunk(IDAT) Then Exit;

        ReadDataAndCheckCRC;
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;

        // prepare next chunk (plus CRC)
        FCurrentCRC := LoadAndSwapHeader;
      End;
    End;

    // this decode call will advance Source and Target accordingly
    FDecoder.Decode(FCurrentSource,
                    LocalBuffer,
                    FIDATSize - (Integer(FCurrentSource) - Integer(FRawBuffer)),
                    PendingOutput);

    If FDecoder.ZLibResult = Z_STREAM_END Then
    Begin
       If (FDecoder.AvailableOutput <> 0) Or
          (FDecoder.AvailableInput <> 0) Then GraphicExError(gesExtraCompressedData, ['PNG']);
      Break;
    End;

    If FDecoder.ZLibResult <> Z_OK Then GraphicExError(gesCompression, ['PNG']);

    PendingOutput := BytesPerRow - (Integer(LocalBuffer) - Integer(RowBuffer));
  Until PendingOutput = 0;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TPNGGraphic.SetupColorDepth(ColorType, BitDepth: Integer): Integer;

Begin
  Result := 0;
  // determine color scheme and setup related stuff,
  // Note: The calculated BPP value is always at least 1 even for 1 bits per pixel etc. formats
  //       and used in filter calculation.
  Case ColorType Of
    0: // gray scale (allowed bit depths are: 1, 2, 4, 8, 16 bits)
      If BitDepth In [1, 2, 4, 8, 16] Then
      With ColorManager Do
      Begin
        SourceColorScheme := csG;
        TargetColorScheme := csG;

        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        Case BitDepth Of
          2:
            TargetBitsPerSample := 4;
          16:
            TargetBitsPerSample := 8;
        Else
          TargetBitsPerSample := BitDepth;
        End;

        PixelFormat := TargetPixelFormat;
        FPalette := CreateGrayscalePalette(False);
        Result := (BitDepth + 7) Div 8;
      End
      Else GraphicExError(gesInvalidColorFormat, ['PNG']);
    2: // RGB
      If BitDepth In [8, 16] Then
      With ColorManager Do
      Begin
        SourceSamplesPerPixel := 3;
        TargetSamplesPerPixel := 3;
        SourceColorScheme := csRGB;
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf24Bit;
        Result := BitDepth * 3 Div 8;
      End
      Else GraphicExError(gesInvalidColorFormat, ['PNG']);
    3: // palette
      If BitDepth In [1, 2, 4, 8] Then
      With ColorManager Do
      Begin
        SourceColorScheme := csIndexed;
        TargetColorScheme := csIndexed;
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        If BitDepth = 2 Then TargetBitsPerSample := 4
                        Else TargetBitsPerSample := BitDepth;

        PixelFormat := TargetPixelFormat;
        Result := 1;
      End
      Else GraphicExError(gesInvalidColorFormat, ['PNG']);
    4: // gray scale with alpha,
       // For the moment this format is handled without alpha, but might later be converted
       // to RGBA with gray pixels or use a totally different approach.
      If BitDepth In [8, 16] Then
      With ColorManager Do
      Begin
        SourceSamplesPerPixel := 1;
        TargetSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        SourceColorScheme := csGA; 
        TargetColorScheme := csIndexed;
        PixelFormat := pf8Bit;
        FPalette := CreateGrayScalePalette(False);
        Result := 2 * BitDepth Div 8;
      End
      Else GraphicExError(gesInvalidColorFormat, ['PNG']);
    6: // RGB with alpha (8, 16)
      If BitDepth In [8, 16] Then
      With ColorManager Do
      Begin
        SourceSamplesPerPixel := 4;
        TargetSamplesPerPixel := 4;
        SourceColorScheme := csRGBA;
        TargetColorScheme := csBGRA;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf32Bit;

        Result := BitDepth * 4 Div 8;
      End
      Else GraphicExError(gesInvalidColorFormat, ['PNG']);
  Else
    GraphicExError(gesInvalidColorFormat, ['PNG']);
  End;
End;

{$ENDIF} // PortableNetworkGraphic

//----------------- TFileFormatList ------------------------------------------------------------------------------------

Type
  PClassEntry = ^TClassEntry;
  TClassEntry = Record
    GraphicClass: TGraphicClass;
    Description: String;
    Count: Cardinal;
  End;

  PExtensionEntry = ^TExtensionEntry;
  TExtensionEntry = Record
    Extension,
    Description: String;
    FormatTypes: TFormatTypes;
    ClassReference: PClassEntry;
  End;

Constructor TFileFormatList.Create;

Begin
  FClassList := TList.Create;
  FExtensionList := TList.Create;
End;

//----------------------------------------------------------------------------------------------------------------------

Destructor TFileFormatList.Destroy;

Begin
  Clear;
  FClassList.Free;
  FExtensionList.Free;
  Inherited;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TFileFormatList.Clear;

Var
  I: Integer;                         

Begin
  For I := 0 To FClassList.Count - 1 Do
  Begin
    TPicture.UnregisterGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
    Dispose(PClassEntry(FClassList[I])); // need Dispose with type casting to free strings too
  End;
  FClassList.Clear;

  For I := 0 To FExtensionList.Count - 1 Do
    Dispose(PExtensionEntry(FExtensionList[I])); 
  FExtensionList.Clear;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.FindExtension(Const Extension: String): Integer;

// Returns the entry which belongs to the given extension string or -1 if there's nothing in the list for this ext.

Var
  I: Integer;

Begin
  Result := -1;
  If Extension <> '' Then
    For I := 0 To FExtensionList.Count - 1 Do
      If CompareText(PExtensionEntry(FExtensionList[I]).Extension, Extension) = 0 Then
      Begin
        Result := I;
        Break;
      End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.FindGraphicClass(GraphicClass: TGraphicClass): Integer;

// returns the entry index which belongs to the given graphic class or -1

Var
  I: Integer;

Begin
  Result := -1;
  For I := 0 To FClassList.Count - 1 Do
    If PClassEntry(FClassList[I]).GraphicClass = GraphicClass Then
    Begin
      Result := I;
      Break;
    End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.GetDescription(Graphic: TGraphicClass): String;

// returns the registered description string for the given class

Var
  I: Integer;

Begin
  Result := '';
  I := FindGraphicClass(Graphic);
  If I > -1 Then Result := PClassEntry(FClassList[I]).Description;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TFileFormatList.GetExtensionList(List: TStrings);

// returns a list of registered extensions (letters only, no *. part)

Var
  I: Integer;
  ExtEntry: PExtensionEntry;

Begin
  List.Clear;
  For I := 0 To FExtensionList.Count - 1 Do
  Begin
    ExtEntry := FExtensionList[I];
    List.Add(ExtEntry.Extension);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType;
  Options: TFilterOptions; GraphicClass: TGraphicClass): String;

// Creates a string which can directly be used in an open or save dialog's filter property.
// Formats may be used to limit the number of formats to return.
// SortType determines how to sort the entries.
// Compact determines whether to group extensions (= True) or to put every extension on a separate line.
// AllImages finally determines whether to include the 'All image file' entry which includes all allowed extensions
// which qualify by the other properties.
// Usually all these options determine quite nicely which formats are well suited for a particular task
// but sometimes you may find it better to specify a graphic class to limit returned formats further.
// In this case set GraphicClass to the particular class otherwise set it nil.

Var
  I, J: Integer;
  DL, EL, All: TStringList;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;
  S,
  DescriptionFormat: String;

Begin
  Result := '';
  If Formats = [] Then Formats := [ftAnimation..ftVector];
  DL := TStringList.Create;
  DL.Sorted := SortType In [fstDescription, fstBoth];
//  dl.Duplicates := dupAccept;
  EL := TStringList.Create;
  EL.Sorted := SortType In [fstExtension, fstBoth];

  // this string list is used to hold the (possibly sorted) list of all allowed extensions
  All := TStringList.Create;
  All.Sorted := SortType In [fstExtension, fstBoth];
  try

    // using an adjusted format string makes the code below easier for different options
    DescriptionFormat := '%s';
    if foIncludeExtension in Options then DescriptionFormat := DescriptionFormat + '%s';

    if foCompact in Options then
    begin
      // all extension for a particular image class on one line
      for I := 0 to FClassList.Count - 1 do
      begin
        ClassEntry := FClassList[I];
        if (GraphicClass = nil) or (GraphicClass = ClassEntry.GraphicClass) then
        begin
          EL.Clear;
          // collect allowed extensions for the current graphic class,
          // this will automatically sort the entries if wanted
          for J := 0 to FExtensionList.Count - 1 do
          begin
            ExtEntry := FExtensionList[J];
            if (ExtEntry.ClassReference = ClassEntry) and ((ExtEntry.FormatTypes * Formats) <> []) then
              EL.Add(ExtEntry.Extension);
          end;

          // build the extension list and an description entry
          if foIncludeAll in Options then All.AddStrings(EL);
          S := '';
          for J := 0 to EL.Count - 1 do S := S + '*.' + EL[J] + '; ';
          // remove last semicolon and space
          SetLength(S, Length(S) - 2);
          if S <> '' then DL.AddObject(ClassEntry.Description, Pointer(StrNew(PChar(S))));
        end;
      End;
    end
    else
    Begin
      // list each extension separately
      for I := 0 to FExtensionList.Count - 1 do
      Begin
        ExtEntry := FExtensionList[I];
        if ((GraphicClass = nil) or (ExtEntry.ClassReference.GraphicClass = GraphicClass)) and
           ((ExtEntry.FormatTypes * Formats) <> []) then
        begin
          S := ExtEntry.Description;
          if S = '' then S := ExtEntry.ClassReference.Description;
          if DL.IndexOf(S) = -1 then  //@@@ SZ Patched to avoid Memory Leak
            DL.AddObject(S, Pointer(StrNew(PChar('*.' + ExtEntry.Extension))));
          if foIncludeAll in Options then All.Add(ExtEntry.Extension);
        end;
      End;
    End;

    // build final filter string out of the collected sub strings
    if (foIncludeAll in Options) and (All.Count > 0) then
    begin
      // first include the general entry if wanted (this entry is never taken into sort order
      S := '';
      for J := 0 to All.Count - 1 do S := S + '*.' + All[J] + '; ';
      SetLength(S, Length(S) - 2);
      Result := gesAllImages + '|' + S + '|';
    end;

    for I := 0 to DL.Count - 1 do
    begin
      S := PChar(DL.Objects[I]);
      StrDispose(PChar(DL.Objects[I]));
      Result := Result + Format(DescriptionFormat, [DL[I], ' (' + S + ')']) + '|' + S + '|';
    end;
    // remove last separator in string
    if Length(Result) > 0 then SetLength(Result, Length(Result) - 1);
  finally
    All.Free;
    EL.Free;
    DL.Free;
  end;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.GraphicFromExtension(S: String): TGraphicClass;

// Returns the class which belongs to the extension given in S or nil if there's non registered.
// S may contain a regular file name (also UNC is allowed), a string returned from ExtractFileExt (with period) or just
// an extension string.

Var
  Index: Integer;

Begin
  Result := Nil;
  Index := Pos('.', S);
  If Index > 0 Then Delete(S, 1, Index);
  Index := FindExtension(S);
  If Index > -1 Then Result := PExtensionEntry(FExtensionList[Index]).ClassReference.GraphicClass;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.GraphicFromContent(Const FileName: String): TGraphicExGraphicClass;

// description see other overloaded version

Var
  Stream: TFileStream;

Begin
  Stream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    Result := GraphicFromContent(Stream);
  Finally
    Stream.Free;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TFileFormatList.GraphicFromContent(Stream: TStream): TGraphicExGraphicClass;

// Determines the type of image in the stream. This test is only available for TGraphicExGraphic
// classes (this excludes TBitmap, TIcon, TMetaFile etc.).
// Note: Not all image types can be found using this code because they are not
//       uniquely identifyable (e.g. Dr. Halo *.cut images).

Var
  I: Integer;
  T: TGraphicExGraphicClass;

Begin
  Result := Nil;
  For I := 0 To FClassList.Count - 1 Do
  Begin
    If PClassEntry(FClassList[I]).GraphicClass.InheritsFrom(TGraphicExGraphic) Then
    Begin
      T := TGraphicExGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
      If T.CanLoad(Stream) Then
      Begin
        Result := T;
        Break;
      End;
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TFileFormatList.RegisterFileFormat(Const Extension, Common, Individual: String; FormatTypes: TFormatTypes;
  Replace, RegisterDefault: Boolean; GraphicClass: TGraphicClass);

// Registers the given graphic class with the passed extension string. If there's already a class registered with this
// extension then either the registration of the older entry is replaced by the new one (Replace = True) or an exception
// is raised.
// This method takes also care to register the new extension with TPicture to make the default handling work too
// if RegisterDefault is True.
// Further parameters are:
// - Extension: the new extension to be registered (not necessarily with only 3 characters, but without a period).
// - Common: a description string for all extensions registered with the same class used when several extensions are
//   listed on one filter line. Pass '' to avoid changing a previously set value if there's one.
// - Individual: a description string used when each extension is listed separately.
// - FormatTypes: classifies the given file type as being a raster or vector file, with single or multiple images etc.
// - GraphicClass: the TGraphic descentant to be used to load and save the particular file.

Var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry,
  OldReference: PClassEntry;

  //--------------- local functions -------------------------------------------

  Procedure UpdateClassEntry;

  // updates a class entry (creates one if necessary)

  Begin
    If ClassIndex = -1 Then
    Begin
      New(ClassEntry);
      ClassEntry.GraphicClass := GraphicClass;
      ClassEntry.Count := 0;
      FClassList.Add(ClassEntry);
    End
    Else
      ClassEntry := FClassList[ClassIndex];

    If Common <> '' Then ClassEntry.Description := Common;
    Inc(ClassEntry.Count);
    ExtEntry.ClassReference := ClassEntry;
  End;

  //--------------- end local functions ---------------------------------------

Var
  S: String;

Begin
  If Extension <> '' Then
  Begin
    ExtIndex := FindExtension(Extension);
    ClassIndex := FindGraphicClass(GraphicClass);
    If ExtIndex = -1 Then
    Begin
      // extension not yet registered
      New(ExtEntry);
      ExtEntry.Extension := Extension;
      ExtEntry.Description := Individual;
      ExtEntry.FormatTypes := FormatTypes;
      FExtensionList.Add(ExtEntry);
      UpdateClassEntry;
    End
    Else
      If Replace Then
      Begin
        // replace current extension entry with new one
        ExtEntry := FExtensionList[ExtIndex];
        If ExtEntry.ClassReference.GraphicClass <> GraphicClass Then
        Begin
          // assign existing extension to new graphic class
          OldReference := ExtEntry.ClassReference;
          UpdateClassEntry;
          Dec(OldReference.Count);
          // remove the graphic class entry if no longer used
          If OldReference.Count = 0 Then FClassList.Remove(OldReference);
        End;
          // otherwise do nothing
      End
      Else
        GraphicExError(gesRegistration, [Extension]);

    // finally make TPicture work
    S := Individual;
    If S = '' Then S := ClassEntry.Description;
    TPicture.RegisterFileFormat(Extension, S, GraphicClass);
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Procedure TFileFormatList.UnregisterFileFormat(Const Extension: String; GraphicClass: TGraphicClass);

// Removes the entry for the given extension from the internal list.
// If Extension is '' then all associations for the given GraphicClass are removed otherwise the class is ignored and
// only the one particular extension is removed.
// Unregistration from TPicture is done here too, if necessary.

Var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;

Begin
  ExtIndex := FindExtension(Extension);
  // make sure we don't try to remove a non-registered extension
  If (Extension = '') Or (ExtIndex > -1) Then
  Begin
    If ExtIndex > -1 Then
    Begin
      // there's an entry for the extension
      ExtEntry := FExtensionList[ExtIndex];
      Dec(ExtEntry.ClassReference.Count);
      // unregister graphic class too if necessary
      If ExtEntry.ClassReference.Count = 0 Then
      Begin
        TPicture.UnregisterGraphicClass(ExtEntry.ClassReference.GraphicClass);
        Dispose(ExtEntry.ClassReference);
        FClassList.Remove(ExtEntry.ClassReference);
      End;

      // finally delete extension entry
      Dispose(ExtEntry);
      FExtensionList.Delete(ExtIndex);
    End
    Else
    Begin
      // all entries for the given graphic class must be removed
      ClassIndex := FindGraphicClass(GraphicClass);
      ClassEntry := FClassList[ClassIndex];
      For ExtIndex := FExtensionList.Count - 1 DownTo 0 Do
      Begin
        If PExtensionEntry(FExtensionList[ExtIndex]).ClassReference.GraphicClass = GraphicClass Then
        Begin
          Dec(ClassEntry.Count);
          Dispose(PExtensionEntry(FExtensionList[ExtIndex]));
          FExtensionList.Delete(ExtIndex);
          // no need to run through further entries if all references are done
          If ClassEntry.Count = 0 Then Break;
        End;
      End;
      Dispose(ClassEntry);
      FClassList.Delete(ClassIndex);
      TPicture.UnregisterGraphicClass(GraphicClass);
    End;
  End;
End;

//----------------------------------------------------------------------------------------------------------------------

Initialization
  FileFormatList := TFileFormatList.Create;
  With FileFormatList Do
  Begin
    // internally register Delphi's "built in" formats, these will not be unregistered on exit and
    // also not registered with TPicture (because they are already or will soon be)
    RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, False, TBitmap);
    RegisterFileFormat('ico', gesIcons, '', [ftRaster], False, False, TIcon);
    RegisterFileFormat('wmf', gesMetaFiles, '', [ftVector], False, False, TMetafile);
    RegisterFileFormat('emf', gesMetaFiles, gesEnhancedMetaFiles, [ftVector], False, False, TMetafile);
    RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, False, TJPEGImage);
    RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, False, TJPEGImage);

    // register our own formats
    RegisterFileFormat('rle', gesBitmaps, gesRLEBitmaps, [ftRaster], False, True, TBitmap);
    RegisterFileFormat('dib', '', gesDIBs, [ftRaster], False, True, TBitmap);

    {$IFDEF TargaGraphic}
    RegisterFileFormat('win', gesTruevision, '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('vst', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('vda', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('tga', '', '', [ftRaster], False, True, TTargaGraphic);
    RegisterFileFormat('icb', '', '', [ftRaster], False, True, TTargaGraphic);
    {$ENDIF}

    {$IFDEF TIFFGraphic}
    RegisterFileFormat('tiff', gesTIFF, gesMacTIFF, [ftRaster, ftMultiImage], False,
      True, TTIFFGraphic);
    RegisterFileFormat('tif', '', gesPCTIF, [ftRaster, ftMultiImage], False, True, TTIFFGraphic);
    RegisterFileFormat('fax', '', gesGFIFax, [ftRaster, ftMultiImage], False, True, TTIFFGraphic);
      {$IFDEF EPSGraphic}
      RegisterFileFormat('eps', gesEPS, '', [ftRaster], False, True, TEPSGraphic);
      {$ENDIF}
    {$ENDIF}

    {$IFDEF PCXGraphic}
    RegisterFileFormat('pcx', gesZSoft, '', [ftRaster], False, True, TPCXGraphic);
    RegisterFileFormat('pcc', '', '', [ftRaster], False, True, TPCXGraphic);
    RegisterFileFormat('scr', '', gesZSoftWord, [ftRaster], False, True, TPCXGraphic);
    {$ENDIF}

    {$IFDEF RLAGraphic}
    RegisterFileFormat('rpf', gesAliasWaveFront, '', [ftRaster], False, True, TRLAGraphic);
    RegisterFileFormat('rla', '', '', [ftRaster], False, True, TRLAGraphic);
    {$ENDIF}

    {$IFDEF SGIGraphic}
    RegisterFileFormat('sgi', gesSGI, gesSGITrueColor, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('rgba', '', gesSGITrueColorAlpha, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('rgb', '', gesSGITrueColor, [ftRaster], False, True, TSGIGraphic);
    RegisterFileFormat('bw', '', gesSGIMono, [ftRaster], False, True, TSGIGraphic);
    {$ENDIF}

    {$IFDEF PhotoshopGraphic}
    RegisterFileFormat('psd', gesPhotoshop, '', [ftRaster, ftLayered], False, True, TPSDGraphic);
    RegisterFileFormat('pdd', '', '', [ftRaster, ftLayered], False, True, TPSDGraphic);
    {$ENDIF}

    {$IFDEF PortableMapGraphic}
    RegisterFileFormat('ppm', gesPortable, gesPortablePixel, [ftRaster], False, True, TPPMGraphic);
    RegisterFileFormat('pgm', '', gesPortableGray, [ftRaster], False, True, TPPMGraphic);
    RegisterFileFormat('pbm', '', gesPortableMono, [ftRaster], False, True, TPPMGraphic);
    {$ENDIF}

    {$IFDEF AutodeskGraphic}
    RegisterFileFormat('cel', gesAutodesk, '', [ftRaster], False, True, TAutodeskGraphic);
    RegisterFileFormat('pic', gesAutodesk, '', [ftRaster], False, True, TAutodeskGraphic);
    {$ENDIF}

    {$IFDEF PCDGraphic}
    TPCDGraphic.DefaultResolution := 2;
    RegisterFileFormat('pcd', gesKodakPhotoCD, '', [ftRaster], False, True, TPCDGraphic);
    {$ENDIF}

    {$IFDEF GIFGraphic}
    RegisterFileFormat('gif', gesCompuserve, '', [ftRaster, ftMultiImage, ftAnimation], False, True, TGIFGraphic);
    {$ENDIF}

    {$IFDEF CUTGraphic}
    RegisterFileFormat('cut', gesHalo, '', [ftRaster], False, True, TCUTGraphic);
    {$ENDIF}

    {$IFDEF PaintshopProGraphic}
    RegisterFileFormat('psp', gesPaintshopPro, '', [ftRaster, ftVector], False, True, TPSPGraphic);
    {$ENDIF}

    {$IFDEF PortableNetworkGraphic}
    RegisterFileFormat('png', gesPortableNetworkGraphic, '', [ftRaster], False, True, TPNGGraphic);
    {$ENDIF}
  End;
Finalization
  With FileFormatList Do
  Begin
    {$IFDEF PaintshopProGraphic} UnregisterFileFormat('', TPSPGraphic); {$ENDIF}
    {$IFDEF PhotoshopGraphic} UnregisterFileFormat('', TPSDGraphic); {$ENDIF}
    {$IFDEF TargaGraphic} UnregisterFileFormat('', TTargaGraphic); {$ENDIF}
    {$IFDEF TIFFGraphic} UnregisterFileFormat('', TTIFFGraphic); {$ENDIF}
    {$IFDEF SGIGraphic} UnregisterFileFormat('', TSGIGraphic); {$ENDIF}
    {$IFDEF PCXGraphic} UnregisterFileFormat('', TPCXGraphic); {$ENDIF}
    {$IFDEF AutodeskGraphic} UnregisterFileFormat('', TAutodeskGraphic); {$ENDIF}
    {$IFDEF PCDGraphic} UnregisterFileFormat('', TPCDGraphic); {$ENDIF}
    {$IFDEF PortableMapGraphic} UnregisterFileFormat('', TPPMGraphic); {$ENDIF}
    {$IFDEF CUTGraphic} UnregisterFileFormat('', TCUTGraphic); {$ENDIF}
    {$IFDEF GIFGraphic} UnregisterFileFormat('', TGIFGraphic); {$ENDIF}
    {$IFDEF RLAGraphic} UnregisterFileFormat('', TRLAGraphic); {$ENDIF}
    UnregisterFileFormat('rle', TBitmap);
    UnregisterFileFormat('dib', TBitmap);
    {$IFDEF PortableNetworkGraphic} UnregisterFileFormat('', TPNGGraphic); {$ENDIF}

    Free;
  End;

End.

