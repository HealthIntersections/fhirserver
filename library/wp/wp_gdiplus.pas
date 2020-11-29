Unit wp_gdiplus;

{
Copyright (c) 2010+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

Interface


Uses
  Windows, Math, Graphics, Classes, Controls, SysUtils, UITypes, Types, ActiveX, Messages, Forms,
  Winapi.GdipObj, Winapi.GdipApi,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections,
  wp_graphics;

Type
  TArgbColour = ARGB;
  TGdiPlusStatus = TStatus;

{$IFDEF VER130}
  TGPRect = GdipApi.TGPRect;
  TGPRectF = GdipApi.TGPRectF;
{$ELSE}
  TGPRect = Winapi.GdipApi.TGPRect;
  TGPRectF = Winapi.GdipApi.TGPRectF;
{$ENDIF}

  EGdiPlusException = Class(Exception);

Const
  argbAliceBlue = $FFF0F8FF;
  argbAntiqueWhite = $FFFAEBD7;
  argbAqua = $FF00FFFF;
  argbAquamarine = $FF7FFFD4;
  argbAzure = $FFF0FFFF;
  argbBeige = $FFF5F5DC;
  argbBisque = $FFFFE4C4;
  argbBlack = $FF000000;
  argbBlanchedAlmond = $FFFFEBCD;
  argbBlue = $FF0000FF;
  argbBlueViolet = $FF8A2BE2;
  argbBrown = $FFA52A2A;
  argbBurlyWood = $FFDEB887;
  argbCadetBlue = $FF5F9EA0;
  argbChartreuse = $FF7FFF00;
  argbChocolate = $FFD2691E;
  argbCoral = $FFFF7F50;
  argbCornflowerBlue = $FF6495ED;
  argbCornsilk = $FFFFF8DC;
  argbCrimson = $FFDC143C;
  argbCyan = $FF00FFFF;
  argbDarkBlue = $FF00008B;
  argbDarkCyan = $FF008B8B;
  argbDarkGoldenrod = $FFB8860B;
  argbDarkGray = $FFA9A9A9;
  argbDarkGreen = $FF006400;
  argbDarkKhaki = $FFBDB76B;
  argbDarkMagenta = $FF8B008B;
  argbDarkOliveGreen = $FF556B2F;
  argbDarkOrange = $FFFF8C00;
  argbDarkOrchid = $FF9932CC;
  argbDarkRed = $FF8B0000;
  argbDarkSalmon = $FFE9967A;
  argbDarkSeaGreen = $FF8FBC8B;
  argbDarkSlateBlue = $FF483D8B;
  argbDarkSlateGray = $FF2F4F4F;
  argbDarkTurquoise = $FF00CED1;
  argbDarkViolet = $FF9400D3;
  argbDeepPink = $FFFF1493;
  argbDeepSkyBlue = $FF00BFFF;
  argbDimGray = $FF696969;
  argbDodgerBlue = $FF1E90FF;
  argbFirebrick = $FFB22222;
  argbFloralWhite = $FFFFFAF0;
  argbForestGreen = $FF228B22;
  argbFuchsia = $FFFF00FF;
  argbGainsboro = $FFDCDCDC;
  argbGhostWhite = $FFF8F8FF;
  argbGold = $FFFFD700;
  argbGoldenrod = $FFDAA520;
  argbGray = $FF808080;
  argbGreen = $FF008000;
  argbGreenYellow = $FFADFF2F;
  argbHoneydew = $FFF0FFF0;
  argbHotPink = $FFFF69B4;
  argbIndianRed = $FFCD5C5C;
  argbIndigo = $FF4B0082;
  argbIvory = $FFFFFFF0;
  argbKhaki = $FFF0E68C;
  argbLavender = $FFE6E6FA;
  argbLavenderBlush = $FFFFF0F5;
  argbLawnGreen = $FF7CFC00;
  argbLemonChiffon = $FFFFFACD;
  argbLightBlue = $FFADD8E6;
  argbLightCoral = $FFF08080;
  argbLightCyan = $FFE0FFFF;
  argbLightGoldenrodYellow= $FFFAFAD2;
  argbLightGray = $FFD3D3D3;
  argbLightGreen = $FF90EE90;
  argbLightPink = $FFFFB6C1;
  argbLightSalmon = $FFFFA07A;
  argbLightSeaGreen = $FF20B2AA;
  argbLightSkyBlue = $FF87CEFA;
  argbLightSlateGray = $FF778899;
  argbLightSteelBlue = $FFB0C4DE;
  argbLightYellow = $FFFFFFE0;
  argbLime = $FF00FF00;
  argbLimeGreen = $FF32CD32;
  argbLinen = $FFFAF0E6;
  argbMagenta = $FFFF00FF;
  argbMaroon = $FF800000;
  argbMediumAquamarine = $FF66CDAA;
  argbMediumBlue = $FF0000CD;
  argbMediumOrchid = $FFBA55D3;
  argbMediumPurple = $FF9370DB;
  argbMediumSeaGreen = $FF3CB371;
  argbMediumSlateBlue = $FF7B68EE;
  argbMediumSpringGreen = $FF00FA9A;
  argbMediumTurquoise = $FF48D1CC;
  argbMediumVioletRed = $FFC71585;
  argbMidnightBlue = $FF191970;
  argbMintCream = $FFF5FFFA;
  argbMistyRose = $FFFFE4E1;
  argbMoccasin = $FFFFE4B5;
  argbNavajoWhite = $FFFFDEAD;
  argbNavy = $FF000080;
  argbOldLace = $FFFDF5E6;
  argbOlive = $FF808000;
  argbOliveDrab = $FF6B8E23;
  argbOrange = $FFFFA500;
  argbOrangeRed = $FFFF4500;
  argbOrchid = $FFDA70D6;
  argbPaleGoldenrod = $FFEEE8AA;
  argbPaleGreen = $FF98FB98;
  argbPaleTurquoise = $FFAFEEEE;
  argbPaleVioletRed = $FFDB7093;
  argbPapayaWhip = $FFFFEFD5;
  argbPeachPuff = $FFFFDAB9;
  argbPeru = $FFCD853F;
  argbPink = $FFFFC0CB;
  argbPlum = $FFDDA0DD;
  argbPowderBlue = $FFB0E0E6;
  argbPurple = $FF800080;
  argbRed = $FFFF0000;
  argbRosyBrown = $FFBC8F8F;
  argbRoyalBlue = $FF4169E1;
  argbSaddleBrown = $FF8B4513;
  argbSalmon = $FFFA8072;
  argbSandyBrown = $FFF4A460;
  argbSeaGreen = $FF2E8B57;
  argbSeaShell = $FFFFF5EE;
  argbSienna = $FFA0522D;
  argbSilver = $FFC0C0C0;
  argbSkyBlue = $FF87CEEB;
  argbSlateBlue = $FF6A5ACD;
  argbSlateGray = $FF708090;
  argbSnow = $FFFFFAFA;
  argbSpringGreen = $FF00FF7F;
  argbSteelBlue = $FF4682B4;
  argbTan = $FFD2B48C;
  argbTeal = $FF008080;
  argbThistle = $FFD8BFD8;
  argbTomato = $FFFF6347;
  argbTurquoise = $FF40E0D0;
  argbViolet = $FFEE82EE;
  argbWheat = $FFF5DEB3;
  argbWhite = $FFFFFFFF;
  argbWhiteSmoke = $FFF5F5F5;
  argbYellow = $FFFFFF00;
  argbYellowGreen = $FF9ACD32;
  argbLtGray = $FFD3D3D3;
  argbDkGray = $FFA9A9A9;

  argbTransparent = $00000000;
  argbHalfTransparentWhite = $80FFFFFF;
  argbQuarterTransparentWhite = $3FFFFFFF;
  argbThreeQuartersTransparentWhite = $40FFFFFF;
  argbHalfTransparentBlack = $80000000;
  argbQuarterTransparentBlack = $3F000000;
  argbThreeQuartersTransparentBlack = $40000000;
  argbHalfTransparentDkGray = $80A9A9A9;
  argbHalfTransparentLtGray = $80D3D3D3;

  argbOutlook2007Blue = $FFBFDBFF;
  argbOutlook2007HighlightYellow = $FFFFBD69;

  GrayScaleColorMatrixArray : TColorMatrix = ((0.3, 0.3, 0.3, 0, 0), (0.59, 0.59, 0.59, 0, 0), (0.11, 0.11, 0.11, 0, 0), (0, 0, 0, 1, 0), (0, 0, 0, 0, 1));

  SYSRGN = 4;

Procedure RaiseGdiPlusStatusException(Const sLocation : String; Const oGraphicsObject : TGPGraphics; Const sGraphicsObject : String); Overload;
Procedure RaiseGdiPlusStatusException(Const sLocation : String; Const oImageObject : TGPImage; Const sImageObject : String); Overload;
Function StatusToString(aStatus : TStatus) : String;

Function GetClippingRegion(Const aHDC : HDC; aWindowHandle : HWND) : TGPRegion;

Procedure InvalidateRegion(Const aHandle : HWND; Const oRegion : TGPRegion; Const bDraw : Boolean);

Function TransparencyColourMatrix(Const rAlphaValue : Double) : TColorMatrix;

  // From Rectangle and RectangleF objects
Function InflateRectangle(aRect : TGPRect; iWidth, iHeight : Integer) : TGPRect; Overload;
Function InflateRectangle(aRect : TGPRectF; rWidth, rHeight : Single) : TGPRectF; Overload;
Function ContractRectangle(aRect : TGPRect; iWidth, iHeight : Integer) : TGPRect; Overload;
Function ContractRectangle(aRect : TGPRectF; rWidth, rHeight : Single) : TGPRectF; Overload;
Function ContractRectangle(aRect : TGPRect; iSize : Integer) : TGPRect; Overload;
Function ContractRectangle(aRect : TGPRectF; rSize : Single) : TGPRectF; Overload;
Function RectangleIntersects(aRectA, aRectB : TGPRect) : Boolean; Overload;
Function RectangleIntersects(aRectA, aRectB : TGPRectF) : Boolean; Overload;
Function RectangleContainsPoint(aRectA : TGPRect; iX, iY : Integer) : Boolean; Overload;
Function RectangleContainsPoint(aRectA : TGPRectF; rX, rY : Single) : Boolean; Overload;
Function RectangleContainsRectangle(aRectA, aRectB : TGPRect) : Boolean; Overload;
Function RectangleContainsRectangle(aRectA, aRectB : TGPRectF) : Boolean; Overload;
Function OffsetRectangle(aRect : TGPRect; iX, iY : Integer) : TGPRect; Overload;
Function OffsetRectangle(aRect : TGPRectF; rX, rY : Single) : TGPRectF; Overload;
Function RectangleEquals(aRectA, aRectB : TGPRect) : Boolean; Overload;
Function RectangleEquals(aRectA, aRectB : TGPRectF) : Boolean; Overload;
Function RectangleUnion(aRectA, aRectB : TGPRect) : TGPRect; Overload;
Function RectangleUnion(aRectA, aRectB : TGPRectF) : TGPRectF; Overload;
Function TruncateRectangle(aRect : TGPRectF) : TGPRect;
Function RoundRectangle(aRect : TGPRectF) : TGPRect;
Function RoundCeilingRectangle(aRect : TGPRectF) : TGPRect;
Function RoundCeilingRectangleF(aRect : TGPRectF) : TGPRectF;
Function RectangleIntersection(Const aRectA, aRectB : TGPRectF) : TGPRectF; Overload;
Function RectangleIntersection(Const aRectA, aRectB : TGPRect) : TGPRect; Overload;
Function RectangleToString(Const aRectangle : TGPRectF) : String; Overload;
Function RectangleToString(Const aRectangle : TGPRect) : String; Overload;

Function CreateEmptyRectangle : TGPRect;
Function CreateEmptyRectangleF : TGPRectF;

Function GDIPlusRectToVCLRect(Const aRectangle : TGPRect) : TRect;
Function VCLRectToGDIPlusRect(Const aRectangle : TRect) : TGPRect;

Function CreateSize(Const iWidth, iHeight : Integer) : TGPSize;
Function CreateSizeF(Const rWidth, rHeight : Single) : TGPSizeF;
Function RoundCeilingSize(Const aValue : TGPSizeF) : TGPSize;

Function CreatePointF(Const iX, iY : Single) : TGPPointF;
Function CreatePoint(Const iX, iY : Integer) : TGPPoint;

Function MakeRectF(aRect : TRect) : TGPRectF; Overload;
Function MakeRectF(aRect : TGPRect) : TGPRectF; Overload;
Function MakeRectF(Const rX, rY, rWidth, rHeight : Single) : TGPRectF; Overload;

Function VCLColourToGDIPlusColour(Const aColour : TColour) : TArgbColour;
Function GDIPlusColourToVCLColour(Const aARGBColour : TArgbColour) : TColour;

Function ColourAdjustSaturation(iColour : TColour; rValue : Real) : TColour;
Function ColourAdjustLumination(iColour : TColour; rValue : Real) : TColour;

Function ColourAdjustAlpha(iColour : TArgbColour; iNewAlpha : Byte) : TArgbColour; Overload;
Function ColourAdjustAlpha(iColour : TArgbColour; iRatio : Real) : TArgbColour; Overload;

Function ColourLuminance(Const iColour : TArgbColour) : Single;
Function ColourRedValue(Const iColour : TArgbColour) : Byte;
Function ColourGreenValue(Const iColour : TArgbColour) : Byte;
Function ColourBlueValue(Const iColour : TArgbColour) : Byte;
Function ColourContrastRatio(Const iColourA, iColourB : TArgbColour) : Single;
Function ColourPercievedBrightness(Const iColour : TArgbColour) : Single;
Function ColourDifference(Const iColourA, iColourB : TArgbColour) : Integer;
Function ColourValidContrast(Const iForegroundColour, iBackgroundColour : TArgbColour) : Boolean;

Function RandomARGBColour : TArgbColour;

Function ColorAdjustLuma(clrRGB: COLORREF; n: Integer; fScale: Boolean) : COLORREF; Stdcall; External 'shlwapi.dll';
Function ColorHLSToRGB(wHue, wLuminance, wSaturation : Word) : COLORREF; Stdcall; External 'shlwapi.dll';
Procedure ColorRGBToHLS(clrRGB : COLORREF; Out wHue, wLuminance, wSaturation : Word); Stdcall; External 'shlwapi.dll';

Procedure DrawRoundedRectangle(oGraphics : TGPGraphics; oPen : TGPPen; aRect : TGPRect; iDepth : Integer); Overload;
Procedure DrawRoundedRectangle(oGraphics : TGPGraphics; oPen : TGPPen; aRect : TGPRectF; iDepth : Integer); Overload;
Procedure FillRoundedRectangle(oGraphics : TGPGraphics; oBrush : TGPBrush; aRect : TGPRectF; iDepth : Integer); Overload;
Procedure FillRoundedRectangle(oGraphics : TGPGraphics; oBrush : TGPBrush; aRect : TGPRect; iDepth : Integer); Overload;
Procedure RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRectF : TGPRectF; Const iDepth : Integer); Overload;
Procedure RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRect : TGPRect; Const iDepth : Integer); Overload;

Procedure DrawDropDownArrow(Const oGraphics : TGPGraphics; Const oSolidBrush : TGPSolidBrush; Const aRect : TGPRectF);

Function GetRandomRgn(DC : HDC; RGN: HRGN; Num : Integer): Integer; Stdcall; External 'gdi32.dll';

Function GdiPlusIsDebugging : Boolean;

// Codec Management

// GDI+ supported encoders:
// - image/bmp
// - image/jpeg
// - image/gif
// - image/tiff
// - image/png

Function MIMETypeToEncoderCLSID(Const sMIMEType : String) : TGUID;

Function JPEGQualityEncoderParameter(Const iQuality : Integer) : TEncoderParameter;


Type
  TFixedStreamAdapter = Class(TStreamAdapter)
    Public
{$IFDEF VER130}
      Function Stat(Out statstg: TStatStg; grfStatFlag: LongInt): HResult; Override; Stdcall;
{$ELSE}
      Function Stat(Out statstg: TStatStg; grfStatFlag: DWORD): HResult; Override; Stdcall;
{$ENDIF}
  End;

  TGdiPlusBitmapImage = Class(TFslObject)
    Private
      FBitmap : TGPBitmap;
      FTransparentColour : TArgbColour;
      FBytes : TBytes;

      Procedure CheckBitmapStatus;

      Function GetBitmap: TGPBitmap;
      Procedure SetBitmap(Const Value: TGPBitmap);

      Property Bitmap : TGPBitmap Read GetBitmap Write SetBitmap;

    Public
      constructor Create; Override;
      constructor Create(Const iWidth : Integer; Const iHeight : Integer); Overload;

      destructor Destroy; Override;

      Function Link : TGdiPlusBitmapImage;

      Function RawFormat : TGUID;

      Function IsImageFormatBMP : Boolean;
      Function IsImageFormatJPEG : Boolean;
      Function IsImageFormatPNG : Boolean;

      Function HorizontalResolution : Single;
      Function VerticalResolution : Single;

      Procedure LoadFromPixels(width, height, stride: Integer; format: TPixelFormat; aBytes : TBytes);
      Procedure LoadFromData(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
      Procedure LoadFromBuffer(oBuffer : TFslBuffer);
      Procedure LoadFromMemoryStream(Const oMemoryStream : TFslMemoryStream);
      Procedure LoadFromVCLBitmap(Const oBitmap : TBitmap);
      Procedure LoadFromBitmapGraphic(Const oBitmap : TFslBitmapGraphic);
      Procedure LoadFromFile(Const sFileName : String);
      Procedure LoadPNGFromResource(Const sResourceName : String);
      Procedure LoadBMPFromResource(Const sResourceName : String);

      Procedure SaveToMemoryStream(Const oMemoryStream : TFslMemoryStream; Const sImageFormat : String);
      Procedure SaveJPEGToMemoryStream(Const oMemoryStream : TFslMemoryStream; Const iQualityValue : Integer);
      Procedure SaveJPEGToFile(Const sFileName : String; Const iQualityValue : Integer);

      Procedure PopulateBitmapGraphic(Const oBitmap : TFslBitmapGraphic);
      Procedure PopulateJpegGraphic(Const oJpeg : TFslJpegGraphic);

      Procedure RotateFlip(aRotateFlipType : TRotateFlipType);

      Function Width : Integer;
      Function Height : Integer;

      Function NativeBitmap : TGPBitmap;

      Property TransparentColour : TArgbColour Read FTransparentColour Write FTransparentColour;
  End;

  TGdiPlusBitmapImageList = Class(TFslObjectList)
    Private
      Function GetBitmapImageByIndex(Const iIndex : Integer) : TGdiPlusBitmapImage;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TGdiPlusBitmapImageList;

      Property BitmapImageByIndex[Const iIndex : Integer] : TGdiPlusBitmapImage Read GetBitmapImageByIndex; Default;
  End;

Type
  TGdiPlusExtendedGraphicDirectionType = (GdiPlusExtendedGraphicDirectionTypeTop, GdiPlusExtendedGraphicDirectionTypeBottom, GdiPlusExtendedGraphicDirectionTypeLeft, GdiPlusExtendedGraphicDirectionTypeRight);

  TGdiPlusExtendedGraphics = Class(TGPGraphics)
    Private
      Procedure Error(Const sMethod : String; Const sMessage : String);
      Function Condition(bCorrect: Boolean; Const sMethod, sMessage: String): Boolean;

    Public
      Function StringSize(Const sValue : String; Const oFont : TGPFont; Const oStringFormat : TGPStringFormat) : TGPSizeF;

      Function DrawValidatedString(Const sValue : String; Const oFont : TGPFont; Const oStringFormat : TGPStringFormat; Const aLayoutRectF : TGPRectF; Const oBrush : TGPBrush; Const aContraintSize : TGPSize) : TGdiPlusStatus; Overload;
      Function DrawValidatedString(Const sValue : String; Const oFont : TGPFont; Const oStringFormat : TGPStringFormat; Const aLayoutRectF : TGPRectF; Const oBrush : TGPBrush) : TGdiPlusStatus; Overload;

      Function RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRect : TGPRect; Const iDepth : Integer) : TGdiPlusStatus; Overload;
      Function RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRectF : TGPRectF; Const iDepth : Single) : TGdiPlusStatus; Overload;

      Function DrawValidRectangle(Const oPen : TGPPen; Const aRect : TGPRect) : TGdiPlusStatus; Overload;
      Function DrawValidRectangle(Const oPen : TGPPen; Const aRectF : TGPRectF) : TGdiPlusStatus; Overload;

      Function CapsulePath(Const oPath : TGPGraphicsPath; Const aRect : TGPRect) : TGdiPlusStatus; Overload;
      Function CapsulePath(Const oPath : TGPGraphicsPath; Const aRectF : TGPRectF) : TGdiPlusStatus; Overload;

      Function DrawRoundedRectangle(Const oPen : TGPPen; Const aRect : TGPRect; Const iDepth : Integer) : TGdiPlusStatus; Overload;
      Function DrawRoundedRectangle(Const oPen : TGPPen; Const aRectF : TGPRectF; Const iDepth : Single) : TGdiPlusStatus; Overload;

      Function FillRoundedRectangle(Const oBrush : TGPBrush; Const aRect : TGPRect; Const iDepth : Integer) : TGdiPlusStatus; Overload;
      Function FillRoundedRectangle(Const oBrush : TGPBrush; Const aRectF : TGPRectF; Const iDepth : Single) : TGdiPlusStatus; Overload;

      Function DrawTriangle(Const oPen : TGPPen; Const aRect : TGPRect) : TGdiPlusStatus; Overload;
      Function DrawTriangle(Const oPen : TGPPen; Const aRectF : TGPRectF) : TGdiPlusStatus; Overload;

      Function FillTriangle(Const oBrush : TGPBrush; Const aRect : TGPRect) : TGdiPlusStatus; Overload;
      Function FillTriangle(Const oBrush : TGPBrush; Const aRectF : TGPRectF) : TGdiPlusStatus; Overload;

      Function DrawChevron(Const oPen : TGPPen; Const aOriginPoint : TGPPointF) : TGdiPlusStatus;
      Function DrawImage(Const oImage : TGPImage; Const aDestinationRectangle : TGPRectF; Const oImageAttributes : TGPImageAttributes) : TGdiPlusStatus; Overload;
  End;

  EGdiPlusExtendedGraphicsException = Class(Exception);

Type
  TGdiPlusHotSpot = Class(TFslObject)
    Private
      FRegion : TGPRegion;
      FClickable : Boolean;
      FHintStringList : TFslStringList;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TGdiPlusHotSpot;

      Procedure SetRegion(oPath : TGPGraphicsPath); Overload;
      Procedure SetRegion(oPath : TGPRect); Overload;

      Function PointInRegion(X : Integer; Y : Integer) : Boolean;

      Property Clickable : Boolean Read FClickable Write FClickable;
      Property HintStringList : TFslStringList Read FHintStringList;
      Property Region : TGPRegion Read FRegion;
  End;

  TGdiPlusHotSpotList = Class(TFslObjectList)
    Private
      Function GetRegionByIndex(Const iIndex: Integer): TGdiPlusHotSpot;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TGdiPlusHotSpotList;

      Property RegionByIndex[Const iIndex : Integer] : TGdiPlusHotSpot Read GetRegionByIndex; Default;
  End;

  TGdiPlusCustomControl = Class(TWinControl)
    Private
      FTrackMouseLeave : Boolean;
      FIsTrackingMouseLeave : Boolean;
      FHasCompletedInitialPaint : Boolean;
      FTransparent : Boolean;
      FInputEnabled : Boolean;
      FPaintChildControls : Boolean;

      Procedure InvalidateParent;

      Procedure PaintToDC(Const pInputDC : HDC; Const aClippingRectangle : TRect);

      Procedure WMPaint(Var aMessage : TWMPaint); Message WM_PAINT;
      Procedure WMEraseBkgnd(Var aMessage: TWmEraseBkgnd); Message WM_ERASEBKGND;
      Procedure WMMouseLeave(Var aMessage : TMessage); Message WM_MOUSELEAVE;
      Procedure CMEnabledChanged(Var aMessage: TMessage); Message CM_ENABLEDCHANGED;

      Function GetInputEnabled: Boolean;
      Procedure SetInputEnabled(Const Value: Boolean);

    Protected
      Procedure CreateParams(Var aParams: TCreateParams); Override;

      Procedure Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect); Virtual;

      Procedure MouseLeave; Virtual;
      Procedure MouseMove(aShiftState : TShiftState; iX, iY : Integer); Override;

      Procedure SynchronousInvalidate(Const aInvalidRect : TGPRect); Overload;
      Procedure SynchronousInvalidate(Const aInvalidRectF : TGPRectF); Overload;

      Procedure InvalidateRectangle(Const aInvalidRect : TGPRect); Overload;
      Procedure InvalidateRectangle(Const aInvalidRectF : TGPRectF); Overload;

      Procedure ValidateRectangle(Const aInvalidRect : TGPRect); Overload;
      Procedure ValidateRectangle(Const aInvalidRectF : TGPRectF); Overload;

      Procedure Error(Const sMethod : String; Const sMessage : String);

      Function Condition(Const bTruth : Boolean; Const sMethod : String; Const sMessage : String) : Boolean;

      Function Invariant(Const sMethod, sMessage: String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TFslObject; aClass: TFslObjectClass; Const sObject : String): Boolean; Overload;

      Property HasCompletedInitialPaint : Boolean Read FHasCompletedInitialPaint;
      Property PaintChildControls : Boolean Read FPaintChildControls Write FPaintChildControls;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure ShuffleTop;
      Procedure ShuffleBottom;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Function GdiPlusClientToScreen(Const aRectangle : TGPRect) : TGPRect;

      Procedure Invalidate; Override;

      Function ClientRectangle : TGPRect;

      Property Transparent : Boolean Read FTransparent Write FTransparent;
      Property Color;
      Property ParentColor;
      Property TrackMouseLeave : Boolean Read FTrackMouseLeave Write FTrackMouseLeave;
      Property InputEnabled : Boolean Read GetInputEnabled Write SetInputEnabled;
  End;

  EGdiPlusCustomControlException = Class(EFslException);

  TComponent = Classes.TComponent;
  HDC = Windows.HDC;

Type
  TGdiPlusStringFormat = Class(TFslObject)
    Private
      FStringFormat : TGPStringFormat;

      FNoWrap : Boolean;
      FLineLimit : Boolean;
      FRightToLeft : Boolean;
      FDirectionVertical : Boolean;
      FOverrideFlags : Boolean;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TGdiPlusStringFormat;

      Function ProduceStringFormat : TGPStringFormat;
      Function ProduceDrawTextFlags : Integer;

      Procedure TrimModeNone;
      Procedure TrimModeCharacter;
      Procedure TrimModeWord;
      Procedure TrimModeEllipsisCharacter;
      Procedure TrimModeEllipsisWord;
      Procedure TrimModeEllipsisPath;

      Procedure VerticalAlignmentTop;
      Procedure VerticalAlignmentMiddle;
      Procedure VerticalAlignmentBottom;

      Procedure HorizontalAlignmentLeft;
      Procedure HorizontalAlignmentCenter;
      Procedure HorizontalAlignmentRight;

      Procedure ApplyFormatFlags(Const iFlags : Integer);

      Property NoWrap : Boolean Read FNoWrap Write FNoWrap;
      Property LineLimit : Boolean Read FLineLimit Write FLineLimit;
      Property RightToLeft : Boolean Read FRightToLeft Write FRightToLeft;
      Property DirectionVertical : Boolean Read FDirectionVertical Write FDirectionVertical;
      Property OverrideFlags : Boolean Read FOverrideFlags Write FOverrideFlags;
  End;


Type
  TGdiPlusFont = Class(TFslObject)
    Private
      FSize : Real;
      FFontFamily : String;
      FBold : Boolean;
      FUnderline : Boolean;
      FItalic : Boolean;
      FColour : TColour;

      FFont : TGPFont;

      Function FontStyle : Integer;

      Function GetNativeFont : TGPFont;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TGdiPlusFont;

      Function ProduceFont : TGPFont;
      Function CreateGDIFont : TFont;

      Procedure Prepare;

      Property Bold : Boolean Read FBold Write FBold;
      Property Underline : Boolean Read FUnderline Write FUnderline;
      Property Italic : Boolean Read FItalic Write FItalic;
      Property Size : Real Read FSize Write FSize;
      Property FontFamily : String Read FFontFamily Write FFontFamily;
      Property Colour : TColour Read FColour Write FColour;
      Property NativeFont : TGPFont Read GetNativeFont;
  End;

Const
  MARGIN = 0.02;

Type
  TGdiPlusImageAnnotator = class (TFslObject)
  Private
    FGDI : TGPGraphics;
    FFont: TGPFont;
    FBrush : TGPBrush;
    FFontSize: Double;
    FFontName: String;
    FImage: TGdiPlusBitmapImage;
    FFontColour: TGPColor;

    FStride : Double;
    FTop : Double;
    Procedure Init;
    Procedure Closeup;
    procedure SetFontColour(const Value: TGPColor);
    procedure SetFontName(const Value: String);
    procedure SetFontSize(const Value: Double);
    procedure SetImage(const Value: TGdiPlusBitmapImage);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Property Image : TGdiPlusBitmapImage read FImage write SetImage;

    Property FontColour : TGPColor Read FFontColour Write SetFontColour;
    Property FontName : String read FFontName write SetFontName;
    Property FontSize : Double read FFontSize write SetFontSize;

    Procedure Write(Const sText : String; rX, rY : Double);

    Procedure Start(iGridCount : integer);
    Procedure Annotate(sText : String);
  End;

Implementation


Procedure RaiseGdiPlusStatusException(Const sLocation : String; Const oGraphicsObject : TGPGraphics; Const sGraphicsObject : String);
Var
  aStatusType : TGdiPlusStatus;
Begin
  If Not Assigned(oGraphicsObject) Then
    Raise EGdiPlusException.CreateFmt('(%s): %s was not assigned.', [sLocation, sGraphicsObject]);

  aStatusType := oGraphicsObject.GetLastStatus;

  If (aStatusType <> Ok) Then
  Begin
    If (aStatusType = GenericError) Or (aStatusType = Win32Error) Then
{$IFDEF VER130}
      RaiseLastWin32Error
{$ELSE}
      RaiseLastOSError
{$ENDIF}
    Else
      Raise EGdiPlusException.CreateFmt('(%s): %s failed with status %s.', [sLocation, sGraphicsObject, StatusToString(aStatusType)]);
  End;
End;


Procedure RaiseGdiPlusStatusException(Const sLocation : String; Const oImageObject : TGPImage; Const sImageObject : String);
Var
  aStatusType : TGdiPlusStatus;
Begin
  If Not Assigned(oImageObject) Then
    Raise EGdiPlusException.CreateFmt('(%s): %s was not assigned.', [sLocation, sImageObject]);

  aStatusType := oImageObject.GetLastStatus;

  If (aStatusType <> Ok) Then
  Begin
    If (aStatusType = GenericError) Or (aStatusType = Win32Error) Then
{$IFDEF VER130}
      RaiseLastWin32Error
{$ELSE}
      RaiseLastOSError
{$ENDIF}
    Else
      Raise EGdiPlusException.CreateFmt('(%s): %s failed with status %s.', [sLocation, sImageObject, StatusToString(aStatusType)]);
  End;
End;


Function StatusToString(aStatus: TStatus) : String;
Begin
  Case aStatus Of
    Ok : Result := 'Ok';
    GenericError : Result := 'GenericError';
    InvalidParameter : Result := 'InvalidParameter';
    OutOfMemory : Result := 'OutOfMemory';
    ObjectBusy : Result := 'ObjectBusy';
    InsufficientBuffer : Result := 'InsufficientBuffer';
    NotImplemented : Result := 'NotImplemented';
    Win32Error : Result := 'Win32Error';
    WrongState : Result := 'WrongState';
    Aborted : Result := 'Aborted';
    FileNotFound : Result := 'FileNotFound';
    ValueOverflow : Result := 'ValueOverflow';
    AccessDenied : Result := 'AccessDenied';
    UnknownImageFormat : Result := 'UnknownImageFormat';
    FontFamilyNotFound : Result := 'FontFamilyNotFound';
    FontStyleNotFound : Result := 'FontStyleNotFound';
    NotTrueTypeFont : Result := 'NotTrueTypeFont';
    UnsupportedGdiplusVersion : Result := 'UnsupportedGdiplusVersion';
    GdiplusNotInitialized : Result := 'GdiplusNotInitialized';
    PropertyNotFound : Result := 'PropertyNotFound';
    PropertyNotSupported : Result := 'PropertyNotSupported';
  Else
    Result := '<Unknown>';
  End;
End;


Function GetClippingRegion(Const aHDC : HDC; aWindowHandle : HWND) : TGPRegion;
Var
  hRegion : HRGN;
  aOffSetPoint : TPoint;
Begin
  Result := Nil;

  If (aHDC <> 0) Then
  Begin
    hRegion := CreateRectRgn(0, 0, 0, 0);
    Try
      If (GetRandomRgn(aHDC, hRegion, SYSRGN) = 1) Then
      Begin
        aOffSetPoint := Point(0, 0);
        MapWindowPoints(aWindowHandle, 0, aOffSetPoint, 1);

        OffsetRgn(hRegion, -aOffSetPoint.X, -aOffSetPoint.Y);

        Result := TGPRegion.Create(hRegion);
      End;
    Finally
      DeleteObject(hRegion);
    End;
  End;
End;


Procedure InvalidateRegion(Const aHandle : HWND; Const oRegion : TGPRegion; Const bDraw : Boolean);
Var
  hRegion : HRGN;
  oGraphics : TGPGraphics;
Begin
  If Assigned(oRegion) Then
  Begin
    oGraphics := TGPGraphics.Create(aHandle, False);
    Try
      hRegion := oRegion.GetHRGN(oGraphics);
      Try
        InvalidateRgn(aHandle, hRegion, bDraw);
      Finally
        DeleteObject(hRegion);
      End;
    Finally
      oGraphics.Free;
    End;
  End;
End;


Function TransparencyColourMatrix(Const rAlphaValue: Double): TColorMatrix;
Begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0, 0] := 1.0;
  Result[1, 1] := 1.0;
  Result[2, 2] := 1.0;
  Result[3, 3] := rAlphaValue;
  Result[4, 4] := 1.0;
End;


Function MakeRectF(aRect : TRect) : TGPRectF;
Begin
  Result.X := aRect.Left;
  Result.Y := aRect.Top;
  Result.Width := aRect.Right - aRect.Left;
  Result.Height := aRect.Bottom - aRect.Top;
End;


Function ColourAdjustSaturation(iColour : TColour; rValue : Real) : TColour;
Var
  wHue, wLuminance, wSaturation : Word;
Begin
  ColorRGBToHLS(iColour, wHue, wLuminance, wSaturation);

  Result := ColorHLSToRGB(wHue, wLuminance, Trunc(wSaturation * rValue));
End;


Function ColourAdjustLumination(iColour : TColour; rValue : Real) : TColour;
Var
  wHue, wLuminance, wSaturation : Word;
Begin
  ColorRGBToHLS(iColour, wHue, wLuminance, wSaturation);

  Result := ColorHLSToRGB(wHue, Trunc(wLuminance * rValue), wSaturation);
End;


Function ColourAdjustAlpha(iColour : TArgbColour; iNewAlpha : Byte) : TArgbColour;
Begin
  Result := MakeColor(iNewAlpha, GetRed(iColour), GetGreen(iColour), GetBlue(iColour));
End;


Function ColourAdjustAlpha(iColour : TArgbColour; iRatio : Real) : TArgbColour;
Begin
  Result := MakeColor(RealCeiling(iRatio * 256), GetRed(iColour), GetGreen(iColour), GetBlue(iColour));
End;


Function RandomARGBColour : TArgbColour;
Begin
  Result := MakeColor(255, RandomInteger(0, 255), RandomInteger(0, 255), RandomInteger(0, 255));
End;


Procedure DrawRoundedRectangle(oGraphics : TGPGraphics; oPen : TGPPen; aRect : TGPRect; iDepth : Integer);
Begin
  DrawRoundedRectangle(oGraphics, oPen, MakeRectF(aRect), iDepth);
End;


Procedure FillRoundedRectangle(oGraphics : TGPGraphics; oBrush : TGPBrush; aRect : TGPRect; iDepth : Integer);
Begin
  FillRoundedRectangle(oGraphics, oBrush, MakeRectF(aRect), iDepth)
End;


Procedure DrawRoundedRectangle(oGraphics : TGPGraphics; oPen : TGPPen; aRect : TGPRectF; iDepth : Integer); Overload;
Var
  oPath : TGPGraphicsPath;
Begin
  oPath := TGPGraphicsPath.Create;
  Try
    RoundedRectanglePath(oPath, aRect, iDepth);

    oGraphics.DrawPath(oPen, oPath);
  Finally
    oPath.Free;
  End;
End;


Procedure FillRoundedRectangle(oGraphics : TGPGraphics; oBrush : TGPBrush; aRect : TGPRectF; iDepth : Integer); Overload;
Var
  oPath : TGPGraphicsPath;
Begin
  oPath := TGPGraphicsPath.Create;
  Try
    RoundedRectanglePath(oPath, aRect, iDepth);

    oGraphics.FillPath(oBrush, oPath);
  Finally
    oPath.Free;
  End;
End;


Procedure RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRectF : TGPRectF; Const iDepth : Integer);
Begin
  oPath.Reset;

  oPath.AddArc(aRectF.X, aRectF.Y, iDepth, iDepth, 180, 90);
  oPath.AddArc(aRectF.X + aRectF.Width - iDepth, aRectF.Y, iDepth, iDepth, -90, 90);
  oPath.AddArc(aRectF.X + aRectF.Width - iDepth, aRectF.Y + aRectF.Height - iDepth, iDepth, iDepth, 0, 90);
  oPath.AddArc(aRectF.X, aRectF.Y + aRectF.Height - iDepth, iDepth, iDepth, 90, 90);
  oPath.CloseFigure;
End;

Procedure RoundedRectanglePath(Const oPath : TGPGraphicsPath; Const aRect : TGPRect; Const iDepth : Integer); Overload;
Begin
  oPath.Reset;

  oPath.AddArc(aRect.X, aRect.Y, iDepth, iDepth, 180, 90);
  oPath.AddArc(aRect.X + aRect.Width - iDepth, aRect.Y, iDepth, iDepth, -90, 90);
  oPath.AddArc(aRect.X + aRect.Width - iDepth, aRect.Y + aRect.Height - iDepth, iDepth, iDepth, 0, 90);
  oPath.AddArc(aRect.X, aRect.Y + aRect.Height - iDepth, iDepth, iDepth, 90, 90);
  oPath.CloseFigure;
End;


Function MakeRectF(aRect : TGPRect) : TGPRectF;
Begin
  Result.X := aRect.X;
  Result.Y := aRect.Y;
  Result.Width := aRect.Width;
  Result.Height := aRect.Height;
End;


Function VCLColourToGDIPlusColour(Const aColour : TColour) : TArgbColour;
Begin
{$IFDEF VER130}
  Result := GdipAPI.ColorRefToARGB(ColorToRGB(aColour));
{$ELSE}
  Result := WinApi.GdipAPI.ColorRefToARGB(ColorToRGB(aColour));
{$ENDIF}
End;


Function GDIPlusColourToVCLColour(Const aARGBColour : TArgbColour) : TColour;
Begin
{$IFDEF VER130}
  Result := GdipAPI.ARGBToColorRef(aARGBColour);
{$ELSE}
  Result := Winapi.GdipAPI.ARGBToColorRef(aARGBColour);
{$ENDIF}
End;


Function GDIPlusRectToVCLRect(Const aRectangle : TGPRect) : TRect;
Begin
  Result.Left := aRectangle.X;
  Result.Right := aRectangle.X + aRectangle.Width + 1;
  Result.Top := aRectangle.Y;
  Result.Bottom := aRectangle.Y + aRectangle.Height + 1;
End;


Function VCLRectToGDIPlusRect(Const aRectangle : TRect) : TGPRect;
Begin
  Result := MakeRect(aRectangle);
End;


Function RoundFloatingPointRectToRect(Const aRectangle : TGPRectF) : TGPRect;
Begin
  Result.X := RealRoundToInteger(aRectangle.X);
  Result.Y := RealRoundToInteger(aRectangle.Y);
  Result.Width := RealRoundToInteger(aRectangle.Width);
  Result.Height := RealRoundToInteger(aRectangle.Height);
End;


Function InflateRectangle(aRect : TGPRect; iWidth, iHeight : Integer) : TGPRect;
Begin
  Result.X := aRect.X - iWidth;
  Result.Y := aRect.Y - iHeight;
  Result.Width := aRect.Width + (2 * iWidth);
  Result.Height := aRect.Height + (2 * iHeight);
End;


Function InflateRectangle(aRect : TGPRectF; rWidth, rHeight : Single) : TGPRectF;
Begin
  Result.X := aRect.X - rWidth;
  Result.Y := aRect.Y - rHeight;
  Result.Width := aRect.Width + (2 * rWidth);
  Result.Height := aRect.Height + (2 * rHeight);
End;


Function ContractRectangle(aRect : TGPRect; iWidth, iHeight : Integer) : TGPRect;
Begin
  Result := InflateRectangle(aRect, -iWidth, -iHeight);
End;


Function ContractRectangle(aRect : TGPRectF; rWidth, rHeight : Single) : TGPRectF;
Begin
  Result := InflateRectangle(aRect, -rWidth, -rHeight);
End;


Function ContractRectangle(aRect : TGPRect; iSize : Integer) : TGPRect;
Begin
  Result := InflateRectangle(aRect, -iSize, -iSize);
End;


Function ContractRectangle(aRect : TGPRectF; rSize : Single) : TGPRectF;
Begin
  Result := InflateRectangle(aRect, -rSize, -rSize);
End;


Function RectangleIntersects(aRectA, aRectB : TGPRect) : Boolean;
Begin
  Result := ((((aRectB.X < (aRectA.X + aRectA.Width)) And (aRectA.X < (aRectB.X + aRectB.Width))) And (aRectB.Y < (aRectA.Y + aRectA.Height))) And (aRectA.Y < (aRectB.Y + aRectB.Height)));
End;


Function RectangleIntersects(aRectA, aRectB : TGPRectF) : Boolean;
Begin
  Result := ((((aRectB.X < (aRectA.X + aRectA.Width)) And (aRectA.X < (aRectB.X + aRectB.Width))) And (aRectB.Y < (aRectA.Y + aRectA.Height))) And (aRectA.Y < (aRectB.Y + aRectB.Height)));
End;


Function TruncateRectangle(aRect : TGPRectF) : TGPRect;
Begin
  Result.X := Trunc(aRect.X);
  Result.Y := Trunc(aRect.Y);
  Result.Width := Trunc(aRect.Width);
  Result.Height := Trunc(aRect.Height);
End;


Function RoundRectangle(aRect : TGPRectF) : TGPRect;
Begin
  Result.X := Round(aRect.X);
  Result.Y := Round(aRect.Y);
  Result.Width := Round(aRect.Width);
  Result.Height := Round(aRect.Height);
End;


Function RectangleContainsPoint(aRectA : TGPRect; iX, iY : Integer) : Boolean;
Begin
  Result := ((((aRectA.X <= iX) And (iX < (aRectA.X + aRectA.Width))) And (aRectA.Y <= iY)) And (iY < (aRectA.Y + aRectA.Height)));
End;


Function RectangleContainsPoint(aRectA : TGPRectF; rX, rY : Single) : Boolean;
Begin
  Result := ((((aRectA.X <= rX) And (rX < (aRectA.X + aRectA.Width))) And (aRectA.Y <= rY)) And (rY < (aRectA.Y + aRectA.Height)));
End;


Function RectangleContainsRectangle(aRectA, aRectB : TGPRect) : Boolean;
Begin
  Result := ((((aRectA.X <= aRectB.X) And ((aRectB.X + aRectB.Width) <= (aRectA.X + aRectA.Width))) And (aRectA.Y <= aRectB.Y)) And ((aRectB.Y + aRectB.Height) <= (aRectA.Y + aRectA.Height)));
End;


Function RectangleContainsRectangle(aRectA, aRectB : TGPRectF) : Boolean;
Begin
  Result := ((((aRectA.X <= aRectB.X) And ((aRectB.X + aRectB.Width) <= (aRectA.X + aRectA.Width))) And (aRectA.Y <= aRectB.Y)) And ((aRectB.Y + aRectB.Height) <= (aRectA.Y + aRectA.Height)));
End;


Function OffsetRectangle(aRect : TGPRect; iX, iY : Integer) : TGPRect;
Begin
  Result.X := aRect.X + iX;
  Result.Y := aRect.Y + iY;
  Result.Width := aRect.Width;
  Result.Height := aRect.Height;
End;


Function OffsetRectangle(aRect : TGPRectF; rX, rY : Single) : TGPRectF;
Begin
  Result.X := aRect.X + rX;
  Result.Y := aRect.Y + rY;
  Result.Width := aRect.Width;
  Result.Height := aRect.Height;
End;


Function RectangleEquals(aRectA, aRectB : TGPRect) : Boolean;
Begin
  Result := ((((aRectB.X = aRectA.X) And (aRectB.Y = aRectA.Y)) And (aRectB.Width = aRectA.Width)) And (aRectB.Height = aRectA.Height));
End;


Function RectangleEquals(aRectA, aRectB : TGPRectF) : Boolean;
Begin
  Result := ((((aRectB.X = aRectA.X) And (aRectB.Y = aRectA.Y)) And (aRectB.Width = aRectA.Width)) And (aRectB.Height = aRectA.Height));
End;


Function RectangleUnion(aRectA, aRectB : TGPRectF) : TGPRectF;
Var
  rX : Single;
  rY : Single;
  rNum2 : Single;
  rNum4 : Single;
Begin
  rX := Min(aRectA.X, aRectB.X);
  rNum2 := Max((aRectA.X + aRectA.Width), (aRectB.X + aRectB.Width));

  rY := Min(aRectA.Y, aRectB.Y);
  rNum4 := Max((aRectA.Y + aRectA.Height), (aRectB.Y + aRectB.Height));

  Result := MakeRect(rX, rY, rNum2 - rX, rNum4 - rY);
End;


Function RectangleUnion(aRectA, aRectB : TGPRect) : TGPRect;
Var
  rX : Integer;
  rY : Integer;
  rNum2 : Integer;
  rNum4 : Integer;
Begin
  rX := Min(aRectA.X, aRectB.X);
  rNum2 := Max((aRectA.X + aRectA.Width), (aRectB.X + aRectB.Width));

  rY := Min(aRectA.Y, aRectB.Y);
  rNum4 := Max((aRectA.Y + aRectA.Height), (aRectB.Y + aRectB.Height));

  Result := MakeRect(rX, rY, rNum2 - rX, rNum4 - rY);
End;


Function RoundCeilingRectangle(aRect : TGPRectF) : TGPRect;
Begin
  Result.X := Ceil(aRect.X);
  Result.Y := Ceil(aRect.Y);
  Result.Width := Ceil(aRect.Width);
  Result.Height := Ceil(aRect.Height);
End;


Function RoundCeilingRectangleF(aRect : TGPRectF) : TGPRectF;
Begin
  Result.X := Ceil(aRect.X);
  Result.Y := Ceil(aRect.Y);
  Result.Width := Ceil(aRect.Width);
  Result.Height := Ceil(aRect.Height);
End;


Function RectangleIntersection(Const aRectA, aRectB : TGPRectF) : TGPRectF;
Var
  rX : Double;
  rY : Double;
  rNum2 : Double;
  rNum4 : Double;
Begin
  rX := RealMax(aRectA.X, aRectB.X);

  rNum2 := RealMin(aRectA.X + aRectA.Width, aRectB.X + aRectB.Width);

  rY := RealMax(aRectA.Y, aRectB.Y);

  rNum4 := RealMin(aRectA.Y + aRectA.Height, aRectB.Y + aRectB.Height);

  If (rNum2 >= rX) And (rNum4 >= rY) Then
  Begin
    Result.X := rX;
    Result.Y := rY;
    Result.Width := rNum2 - rX;
    Result.Height := rNum4 - rY;
  End
  Else
  Begin
    Result.X := 0;
    Result.Y := 0;
    Result.Width := 0;
    Result.Height := 0;
  End;
End;


Function RectangleIntersection(Const aRectA, aRectB : TGPRect) : TGPRect;
Var
  rX : Integer;
  rY : Integer;
  rNum2 : Integer;
  rNum4 : Integer;
Begin
  rX := IntegerMax(aRectA.X, aRectB.X);

  rNum2 := IntegerMin(aRectA.X + aRectA.Width, aRectB.X + aRectB.Width);

  rY := IntegerMax(aRectA.Y, aRectB.Y);

  rNum4 := IntegerMin(aRectA.Y + aRectA.Height, aRectB.Y + aRectB.Height);

  If (rNum2 >= rX) And (rNum4 >= rY) Then
  Begin
    Result.X := rX;
    Result.Y := rY;
    Result.Width := rNum2 - rX;
    Result.Height := rNum4 - rY;
  End
  Else
  Begin
    Result.X := 0;
    Result.Y := 0;
    Result.Width := 0;
    Result.Height := 0;
  End;
End;


Function RectangleToString(Const aRectangle : TGPRectF) : String;
Begin
  Result := StringFormat('x:%f, y:%f, w:%f, h:%f', [aRectangle.X, aRectangle.Y, aRectangle.Width, aRectangle.Height]);
End;


Function RectangleToString(Const aRectangle : TGPRect) : String;
Begin
  Result := StringFormat('x:%d, y:%d, w:%d, h:%d', [aRectangle.X, aRectangle.Y, aRectangle.Width, aRectangle.Height]);
End;


Function GdiPlusIsDebugging : Boolean;
Begin
  Result := False;
End;


Procedure DrawDropDownArrow(Const oGraphics : TGPGraphics; Const oSolidBrush : TGPSolidBrush; Const aRect : TGPRectF);
Var
  aPointArray : TPointFDynArray;
Begin
  SetLength(aPointArray, 3);

  aPointArray[0] := MakePoint(aRect.X, aRect.Y);
  aPointArray[1] := MakePoint(aRect.X + aRect.Width, aRect.Y);
  aPointArray[2] := MakePoint(aRect.X + (aRect.Width / 2), aRect.Y + aRect.Height);

  oGraphics.FillPolygon(oSolidBrush, PGPPointF(aPointArray), 3);
End;


Function CreatePointF(Const iX, iY : Single) : TGPPointF;
Begin
  Result.X := iX;
  Result.Y := iY;
End;


Function CreatePoint(Const iX, iY : Integer) : TGPPoint;
Begin
  Result.X := iX;
  Result.Y := iY;
End;


Function CreateSize(Const iWidth, iHeight : Integer) : TGPSize;
Begin
  Result.Width := iWidth;
  Result.Height := iHeight;
End;


Function CreateSizeF(Const rWidth, rHeight : Single) : TGPSizeF;
Begin
  Result.Width := rWidth;
  Result.Height := rHeight;
End;


Function RoundCeilingSize(Const aValue : TGPSizeF) : TGPSize;
Begin
  Result.Width := RealCeiling(aValue.Width);
  Result.Height := RealCeiling(aValue.Height);
End;


Function CreateEmptyRectangle : TGPRect;
Begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
End;


Function CreateEmptyRectangleF : TGPRectF;
Begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
End;


Function MIMETypeToEncoderCLSID(Const sMIMEType : String) : TGUID;
Var
  aStatus : TStatus;
  pCodecInfoArray : PImageCodecInfo;
  pCodecInfoArrayItem : PImageCodecInfo;
  bFoundEncoder : Boolean;
  iEncoderCount : Cardinal;
  iEncoderArraySize : Cardinal;
  iEncoderIndex : Cardinal;
Begin
  aStatus := GetImageEncodersSize(iEncoderCount, iEncoderArraySize);

  If aStatus <> Ok Then
    Raise EGdiPlusException.CreateFmt('GetImageEncodersSize failed with error: %s.', [StatusToString(aStatus)]);

  If iEncoderCount = 0 Then
    Raise EGdiPlusException.Create('No encoders found.');

  GetMem(pCodecInfoArray, iEncoderArraySize);
  Try
    aStatus := GetImageEncoders(iEncoderCount, iEncoderArraySize, pCodecInfoArray);

    If aStatus <> Ok Then
      Raise EGdiPlusException.CreateFmt('GetImageEncoders failed with error: %s.', [StatusToString(aStatus)]);

    iEncoderIndex := 0;
    bFoundEncoder := False;
    pCodecInfoArrayItem := pCodecInfoArray;
    While (iEncoderIndex <= iEncoderCount - 1) And Not bFoundEncoder Do
    Begin
      If StringEquals(pCodecInfoArrayItem^.MimeType, sMIMEType) Then
      Begin
        Result := pCodecInfoArrayItem^.Clsid;
        bFoundEncoder := True;
      End;

      Inc(iEncoderIndex);
      Inc(pCodecInfoArrayItem);
    End;

    If Not bFoundEncoder Then
      Raise EGdiPlusException.CreateFmt('Unable to find encoder for type "%s".', [sMIMEType]);
  Finally
    FreeMem(pCodecInfoArray, iEncoderArraySize);
  End;
End;


Function JPEGQualityEncoderParameter(Const iQuality : Integer) : TEncoderParameter;
Begin
  Result.Guid := EncoderQuality;
  Result.Type_ := EncoderParameterValueTypeLong;
  Result.NumberOfValues := 1;
  Result.Value := @iQuality;
End;


Function MakeRectF(Const rX, rY, rWidth, rHeight : Single) : TGPRectF;
Begin
  Result.X := rX;
  Result.Y := rY;
  Result.Height := rHeight;
  Result.Width := rWidth;
End;


Function ColourLuminance(Const iColour : TArgbColour) : Single;
Var
  R : Single;
  G : Single;
  B : Single;
Begin
  // http://www.w3.org/TR/WCAG20/#relativeluminancedef

  R := ColourRedValue(iColour) / 255;
  G := ColourGreenValue(iColour) / 255;
  B := ColourBlueValue(iColour) / 255;

  If R <= 0.03928 Then
    R := R / 12.92
  Else
    R := fsl_utilities.Power((R + 0.055) / 1.055, 2.4);

  If G <= 0.03928 Then
    G := G / 12.92
  Else
    G := fsl_utilities.Power((G + 0.055) / 1.055, 2.4);

  If B <= 0.03928 Then
    B := B / 12.92
  Else
    B := fsl_utilities.Power((B + 0.055) / 1.055, 2.4);

  Result := 0.2126 * R + 0.7152 * G + 0.0722 * B;
End;


Function ColourRedValue(Const iColour : TArgbColour) : Byte;
Begin
  Result := (iColour Shr $10) And $FF;
End;


Function ColourGreenValue(Const iColour : TArgbColour) : Byte;
Begin
  Result := (iColour Shr $8) And $FF;
End;


Function ColourBlueValue(Const iColour : TArgbColour) : Byte;
Begin
  Result := iColour And $FF;
End;


Function ColourContrastRatio(Const iColourA, iColourB : TArgbColour) : Single;
Var
  rLuminanceA : Single;
  rLuminanceB : Single;
  rLighterLuminance : Single;
  rDarkerLuminance : Single;
Begin
  // http://www.w3.org/TR/WCAG20/#contrast-ratiodef
  
  rLuminanceA := ColourLuminance(iColourA);
  rLuminanceB := ColourLuminance(iColourB);

  If rLuminanceA >= rLuminanceB Then
  Begin
    rLighterLuminance := rLuminanceA;
    rDarkerLuminance := rLuminanceB;
  End
  Else
  Begin
    rLighterLuminance := rLuminanceB;
    rDarkerLuminance := rLuminanceA;
  End;

  Result := (rLighterLuminance + 0.05) / (rDarkerLuminance + 0.05);
End;


Function ColourPercievedBrightness(Const iColour : TArgbColour) : Single;
Begin
  Result := ((ColourRedValue(iColour) * 299) + (ColourGreenValue(iColour) * 587) + (ColourBlueValue(iColour) * 114)) / 1000;
End;


Function ColourDifference(Const iColourA, iColourB : TArgbColour) : Integer;
Begin
  Result :=
    (IntegerMax(ColourRedValue(iColourA), ColourRedValue(iColourB)) - IntegerMin(ColourRedValue(iColourA), ColourRedValue(iColourB))) +
    (IntegerMax(ColourGreenValue(iColourA), ColourGreenValue(iColourB)) - IntegerMin(ColourGreenValue(iColourA), ColourGreenValue(iColourB))) +
    (IntegerMax(ColourBlueValue(iColourA), ColourBlueValue(iColourB)) - IntegerMin(ColourBlueValue(iColourA), ColourBlueValue(iColourB)))
End;


Function ColourValidContrast(Const iForegroundColour, iBackgroundColour : TArgbColour) : Boolean;
Begin
  // Based on Web Content Accessibility Guidelines (WCAG) 2.0 (1.4.3 Level AA)
  // http://www.w3.org/TR/2008/REC-WCAG20-20081211/#visual-audio-contrast-contrast

  Result := ColourContrastRatio(iForegroundColour, iBackgroundColour) >= 4.5;

  // Brightness & Difference algorithm
  // Result := (ColourDifference(iForegroundColour, iBackgroundColour) > 500) And (ColourPercievedBrightness(iBackgroundColour) - ColourPercievedBrightness(iForegroundColour) > 125);
End;


Constructor TGdiPlusBitmapImage.Create;
Begin
  Inherited;

  FBitmap := Nil;
  FTransparentColour := argbFuchsia;
End;


Constructor TGdiPlusBitmapImage.Create(Const iWidth: Integer; Const iHeight: Integer);
Begin
  Inherited Create;

  FBitmap := TGPBitmap.Create(iWidth, iHeight, PixelFormat32bppARGB);
End;


Destructor TGdiPlusBitmapImage.Destroy;
Begin
  FBitmap.Free;

  Inherited;
End;


Function TGdiPlusBitmapImage.Link: TGdiPlusBitmapImage;
Begin
  Result := TGdiPlusBitmapImage(Inherited Link);
End;



Function TGdiPlusBitmapImage.NativeBitmap: TGPBitmap;
Begin
  Assert(CheckCondition(Assigned(FBitmap), 'NativeBitmap', 'FBitmap is not assigned.'));

  Result := FBitmap;
End;


Function TGdiPlusBitmapImage.GetBitmap: TGPBitmap;
Begin
  Assert(CheckCondition(Assigned(FBitmap), 'SetBitmap', 'Value is not assigned.'));

  Result := FBitmap;
End;


Procedure TGdiPlusBitmapImage.SetBitmap(Const Value: TGPBitmap);
Begin
  FBitmap.Free;
  FBitmap := Value;
End;


Procedure TGdiPlusBitmapImage.LoadFromFile(Const sFileName: String);
Begin
  Assert(CheckCondition(Not (sFileName = ''), 'LoadFromFile', 'sFileName is blank.'));

  Bitmap := TGPBitmap.Create(sFileName);

  CheckBitmapStatus;
End;


Procedure TGdiPlusBitmapImage.LoadPNGFromResource(Const sResourceName: String);
Var
  oStream : TResourceStream;
  oStreamAdapter : TFixedStreamAdapter;
  oGraphics : TGPGraphics;
  oResourceBitmap : TGPBitmap;
  iResourceBitmapWidth : Integer;
  iResourceBitmapHeight: Integer;
Begin
  Assert(CheckCondition(Not (sResourceName = ''), 'LoadPNGFromResource', 'sResourceName is blank.'));

  // Bitmap from TGPBitmap.FromStream results in access violations when using with ImageAttributes.
  // Create a new bitmap with same height and width to work around this.
  // http://blogs.msdn.com/ericgu/archive/2005/10/12/480270.aspx

  oStream := TResourceStream.Create(hInstance, sResourceName, RT_RCDATA);
  Try
    oStreamAdapter := TFixedStreamAdapter.Create(oStream);

    oResourceBitmap := TGPBitmap.Create(oStreamAdapter);
    Try
      iResourceBitmapWidth := oResourceBitmap.GetWidth;
      iResourceBitmapHeight := oResourceBitmap.GetHeight;

      Bitmap := TGPBitmap.Create(iResourceBitmapWidth, iResourceBitmapHeight, PixelFormat32bppARGB);

      oGraphics := TGPGraphics.Create(Bitmap);
      Try
        oGraphics.DrawImage(oResourceBitmap, 0, 0, iResourceBitmapWidth, iResourceBitmapHeight);
      Finally
        oGraphics.Free;
      End;
    Finally
      oResourceBitmap.Free;
    End;
  Finally
    oStream.Free;
  End;

  CheckBitmapStatus;
End;



{$IFDEF VER130}
function TGdiPlusBitmapImage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBitmap.sizeInBytes);
  inc(result, FTransparentColour.sizeInBytes);
  inc(result, FBytes.sizeInBytes);
end;

Function TFixedStreamAdapter.Stat(Out statstg: TStatStg; grfStatFlag: Integer): HResult;
{$ELSE}
Function TFixedStreamAdapter.Stat(Out statstg: TStatStg; grfStatFlag: DWord): HResult;
{$ENDIF}
Begin
  Result := Inherited Stat(statstg, grfStatFlag);
  statstg.pwcsName := Nil;
End;


Procedure TGdiPlusBitmapImage.LoadBMPFromResource(Const sResourceName: String);
Begin
  Bitmap := TGPBitmap.Create(hInstance, sResourceName);

  CheckBitmapStatus;
End;


Procedure TGdiPlusBitmapImage.CheckBitmapStatus;
Var
  aStatus : TStatus;
Begin
  If Not Assigned(FBitmap) Then
    RaiseError('CheckBitmapStatus', 'Bitmap is not assigned.');

  aStatus := FBitmap.GetLastStatus;

  If (aStatus <> Ok) Then
    RaiseError('CheckBitmapStatus', 'Bitmap failed with status: ' + StatusToString(aStatus));
End;


Procedure TGdiPlusBitmapImage.LoadFromBitmapGraphic(Const oBitmap : TFslBitmapGraphic);
Begin
  Bitmap := TGPBitmap.Create(oBitmap.Handle.Handle, oBitmap.Handle.Palette);

  CheckBitmapStatus;
End;


Procedure TGdiPlusBitmapImage.LoadFromBuffer(oBuffer : TFslBuffer);
var
  oMem : TFslMemoryStream;
Begin
  oMem := TFslMemoryStream.Create;
  Try
    oMem.Buffer := oBuffer.Link;
    LoadFromMemoryStream(oMem);
  Finally
    oMem.Free;
  End;
End;

Procedure TGdiPlusBitmapImage.LoadFromMemoryStream(Const oMemoryStream : TFslMemoryStream);
//Var
//  oIStreamAdapter : TFslIStreamAdapter;
Begin
  Assert(Invariants('SaveToMemoryStream', oMemoryStream, TFslMemoryStream, 'oMemoryStream'));

//  oIStreamAdapter := TFslIStreamAdapter.Create;
//  Try
//    oIStreamAdapter.Stream := oMemoryStream.Link;
//
//    Bitmap := TGPBitmap.Create(oIStreamAdapter);
//
//    CheckBitmapStatus;
//  Finally
//    oIStreamAdapter.Free;
//  End;
End;


Procedure TGdiPlusBitmapImage.LoadFromVCLBitmap(Const oBitmap: TBitmap);
Begin
  Bitmap := TGPBitmap.Create(oBitmap.Handle, oBitmap.Palette);

  CheckBitmapStatus;
End;


Procedure TGdiPlusBitmapImage.SaveToMemoryStream(Const oMemoryStream : TFslMemoryStream; Const sImageFormat : String);
//Var
//  oIStreamAdapter : TFslIStreamAdapter;
Begin
  Assert(Invariants('SaveToMemoryStream', oMemoryStream, TFslMemoryStream, 'oMemoryStream'));

//  oIStreamAdapter := TFslIStreamAdapter.Create;
//  Try
//    oIStreamAdapter.Stream := oMemoryStream.Link;
//
//    Bitmap.Save(oIStreamAdapter, MIMETypeToEncoderCLSID(sImageFormat));
//
//    RaiseGdiPlusStatusException('SaveToMemoryStream', Bitmap, 'Bitmap');
//  Finally
//    oIStreamAdapter.Free;
//  End;
End;


Procedure TGdiPlusBitmapImage.SaveJPEGToMemoryStream(Const oMemoryStream: TFslMemoryStream; Const iQualityValue: Integer);
Var
  aParameters : EncoderParameters;
//  oIStreamAdapter : TFslIStreamAdapter;
Begin
  Assert(Invariants('SaveJPEGToMemoryStream', oMemoryStream, TFslMemoryStream, 'oMemoryStream'));

//  oIStreamAdapter := TFslIStreamAdapter.Create;
//  Try
//    oIStreamAdapter.Stream := oMemoryStream.Link;
//
//    aParameters.Count := 1;
//    aParameters.Parameter[0].Guid := EncoderQuality;
//    aParameters.Parameter[0].NumberOfValues := 1;
//    aParameters.Parameter[0].Type_ := EncoderParameterValueTypeLong;
//    aParameters.Parameter[0].Value := @iQualityValue;
//
//    Bitmap.Save(oIStreamAdapter, MIMETypeToEncoderCLSID('image/jpeg'), @aParameters);
//
//    RaiseGdiPlusStatusException('SaveJPEGToMemoryStream', Bitmap, 'Bitmap');
//  Finally
//    oIStreamAdapter.Free;
//  End;
End;


Procedure TGdiPlusBitmapImage.SaveJPEGToFile(Const sFileName: String; Const iQualityValue: Integer);
Var
  aParameters : EncoderParameters;
Begin
  aParameters.Count := 1;
  aParameters.Parameter[0].Guid := EncoderQuality;
  aParameters.Parameter[0].NumberOfValues := 1;
  aParameters.Parameter[0].Type_ := EncoderParameterValueTypeLong;
  aParameters.Parameter[0].Value := @iQualityValue;

  Bitmap.Save(sFileName, MIMETypeToEncoderCLSID('image/jpeg'), @aParameters);

  RaiseGdiPlusStatusException('SaveJPEGToFile', Bitmap, 'Bitmap');
End;


Function TGdiPlusBitmapImage.Height : Integer;
Begin
  Result := Bitmap.GetHeight;
End;


Function TGdiPlusBitmapImage.Width : Integer;
Begin
  Result := Bitmap.GetWidth;
End;


Procedure TGdiPlusBitmapImage.RotateFlip(aRotateFlipType : TRotateFlipType);
Begin
  CheckBitmapStatus;

  FBitmap.RotateFlip(aRotateFlipType);
End;


Function TGdiPlusBitmapImage.RawFormat : TGUID;
Begin
  CheckBitmapStatus;

  FBitmap.GetRawFormat(Result);
End;


Function TGdiPlusBitmapImage.IsImageFormatBMP: Boolean;
Var
  aImageFormatGUID : TGUID;
Begin
  aImageFormatGUID := RawFormat;

  Result := IsEqualGUID(aImageFormatGUID, ImageFormatMemoryBMP) Or IsEqualGUID(aImageFormatGUID, ImageFormatBMP);
End;


Function TGdiPlusBitmapImage.IsImageFormatJPEG: Boolean;
Begin
  Result := IsEqualGUID(RawFormat, ImageFormatJPEG);
End;


Function TGdiPlusBitmapImage.IsImageFormatPNG: Boolean;
Begin
  Result := IsEqualGUID(RawFormat, ImageFormatPNG);
End;


Procedure TGdiPlusBitmapImage.PopulateBitmapGraphic(Const oBitmap: TFslBitmapGraphic);
Var
  aBitmapHandle : HBITMAP;
Begin
  Assert(CheckCondition(IsImageFormatBMP, 'PopulateBitmapGraphic', 'Unable to populate BitmapGraphic. Format not supported.'));

  FBitmap.GetHBITMAP(argbBlack, aBitmapHandle);

  oBitmap.Handle.Handle := aBitmapHandle;
End;


Procedure TGdiPlusBitmapImage.PopulateJpegGraphic(Const oJpeg : TFslJpegGraphic);
Var
  oBitmapGraphic : TFslBitmapGraphic;
  aBitmapHandle : HBITMAP;
Begin
  FBitmap.GetHBITMAP(argbBlack, aBitmapHandle);

  oBitmapGraphic := TFslBitmapGraphic.Create;
  Try
    oBitmapGraphic.Handle.Handle := aBitmapHandle;

    oJpeg.Handle.Assign(oBitmapGraphic.Handle);
  Finally
    oBitmapGraphic.Free;
  End;
End;


Function TGdiPlusBitmapImage.HorizontalResolution : Single;
Begin
  Result := Bitmap.GetHorizontalResolution;
End;


Function TGdiPlusBitmapImage.VerticalResolution : Single;
Begin
  Result := Bitmap.GetVerticalResolution;
End;


{ TGdiPlusBitmapImageList }


Function TGdiPlusBitmapImageList.GetBitmapImageByIndex(Const iIndex: Integer): TGdiPlusBitmapImage;
Begin
  Result := TGdiPlusBitmapImage(ObjectByIndex[iIndex]);
End;


Function TGdiPlusBitmapImageList.ItemClass: TFslObjectClass;
Begin
  Result := TGdiPlusBitmapImage;
End;


Function TGdiPlusBitmapImageList.Link: TGdiPlusBitmapImageList;
Begin
  Result := TGdiPlusBitmapImageList(Inherited Link);
End;

Procedure TGdiPlusBitmapImage.LoadFromData(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
Begin
  Bitmap := TGPBitmap.Create(gdiBitmapInfo, gdiBitmapData);
  CheckBitmapStatus;
End;


procedure TGdiPlusBitmapImage.LoadFromPixels(width, height, stride: Integer; format: TPixelFormat; aBytes : TBytes);
begin
  FBytes := aBytes;
  Bitmap := TGPBitmap.Create(width, height, stride, format, @FBytes[0]);
  CheckBitmapStatus;
end;




Function TGdiPlusExtendedGraphics.RoundedRectanglePath(Const oPath: TGPGraphicsPath; Const aRect: TGPRect; Const iDepth: Integer): TGdiPlusStatus;
Begin
  Result := RoundedRectanglePath(oPath, MakeRectF(aRect), iDepth);
End;


Function TGdiPlusExtendedGraphics.RoundedRectanglePath(Const oPath: TGPGraphicsPath; Const aRectF: TGPRectF; Const iDepth: Single): TGdiPlusStatus;
Var
  aArcRectF : TGPRectF;
  rDiameter : Single;
Begin
  oPath.StartFigure;

  If (iDepth <= 0) Then
  Begin
    Result := oPath.AddRectangle(aRectF);
  End
  Else
  Begin
    If iDepth >= (RealMin(aRectF.Width, aRectF.Height) / 2) Then
    Begin
      Result := CapsulePath(oPath, aRectF);
    End
    Else
    Begin
      rDiameter := iDepth * 2;

      aArcRectF := MakeRect(aRectF.X, aRectF.Y, rDiameter, rDiameter);
      oPath.AddArc(aArcRectF, 180, 90 );

      aArcRectF.X := (aRectF.X + aRectF.Width) - rDiameter;
      oPath.AddArc(aArcRectF, 270, 90);

      aArcRectF.Y := (aRectF.Y + aRectF.Height) - rDiameter;
      oPath.AddArc(aArcRectF, 0, 90);

      aArcRectF.X := aRectF.X;
      oPath.AddArc(aArcRectF, 90, 90);

      Result := oPath.CloseFigure;
    End;
  End;
End;

Function TGdiPlusExtendedGraphics.DrawValidRectangle(Const oPen: TGPPen; Const aRect: TGPRect): TGdiPlusStatus;
Var
  aModifiedRect : TGPRect;
Begin
  If (oPen.GetWidth = 1) And (oPen.GetAlignment = PenAlignmentInset) Then
  Begin
    aModifiedRect.X := aRect.X;
    aModifiedRect.Y := aRect.Y;
    aModifiedRect.Width := aRect.Width - 1;
    aModifiedRect.Height := aRect.Height - 1;

    Result := DrawRectangle(oPen, aModifiedRect);
  End
  Else
  Begin
    Result := DrawRectangle(oPen, aRect);
  End;
End;


Function TGdiPlusExtendedGraphics.DrawValidRectangle(Const oPen: TGPPen; Const aRectF: TGPRectF): TGdiPlusStatus;
Var
  aModifiedRect : TGPRectF;
Begin
  If (oPen.GetWidth = 1) And (oPen.GetAlignment = PenAlignmentInset) Then
  Begin
    aModifiedRect.X := aRectF.X;
    aModifiedRect.Y := aRectF.Y;
    aModifiedRect.Width := aRectF.Width - 1;
    aModifiedRect.Height := aRectF.Height - 1;

    Result := DrawRectangle(oPen, aModifiedRect);
  End
  Else
  Begin
    Result := DrawRectangle(oPen, aRectF);
  End;
End;


Function TGdiPlusExtendedGraphics.CapsulePath(Const oPath: TGPGraphicsPath; Const aRect: TGPRect): TGdiPlusStatus;
Begin
  Result := CapsulePath(oPath, MakeRectF(aRect));
End;


Function TGdiPlusExtendedGraphics.CapsulePath(Const oPath: TGPGraphicsPath; Const aRectF: TGPRectF): TGdiPlusStatus;
Var
  aArcF : TGPRectF;
Begin
  oPath.StartFigure;

  If aRectF.Width > aRectF.Height Then
  Begin
    aArcF := MakeRect(aRectF.X, aRectF.Y, aRectF.Height, aRectF.Height);
    oPath.AddArc(aArcF, 90, 180);

    aArcF.X := (aRectF.X + aRectF.Width) - aRectF.Height;
    oPath.AddArc(aArcF, 270, 180 );

    Result := oPath.CloseFigure;
  End
  Else If aRectF.Height > aRectF.Width Then
  Begin
    aArcF := MakeRect(aRectF.X, aRectF.Y, aRectF.Width, aRectF.Width);
    oPath.AddArc(aArcF, 180, 180 );

    aArcF.Y := (aRectF.Y + aRectF.Height) - aRectF.Width;
    oPath.AddArc(aArcF, 0, 180 );

    Result := oPath.CloseFigure;
  End
  Else
  Begin
    Result := oPath.AddRectangle(aRectF);
  End;
End;


Function TGdiPlusExtendedGraphics.DrawRoundedRectangle(Const oPen : TGPPen; Const aRect: TGPRect; Const iDepth: Integer): TGdiPlusStatus;
Begin
  Result := DrawRoundedRectangle(oPen, MakeRectF(aRect), iDepth);
End;


Function TGdiPlusExtendedGraphics.DrawRoundedRectangle(Const oPen : TGPPen; Const aRectF: TGPRectF; Const iDepth: Single): TGdiPlusStatus;
Var
  oPath : TGPGraphicsPath;
Begin
  oPath := TGPGraphicsPath.Create;
  Try
    RoundedRectanglePath(oPath, aRectF, iDepth);

    Result := DrawPath(oPen, oPath);
  Finally
    oPath.Free;
  End;
End;


Function TGdiPlusExtendedGraphics.FillRoundedRectangle(Const oBrush: TGPBrush; Const aRect: TGPRect; Const iDepth: Integer): TGdiPlusStatus;
Begin
  Result := FillRoundedRectangle(oBrush, MakeRectF(aRect), iDepth);
End;


Function TGdiPlusExtendedGraphics.FillRoundedRectangle(Const oBrush: TGPBrush; Const aRectF: TGPRectF; Const iDepth: Single): TGdiPlusStatus;
Var
  oPath : TGPGraphicsPath;
Begin
  oPath := TGPGraphicsPath.Create;
  Try
    RoundedRectanglePath(oPath, aRectF, iDepth);

    Result := FillPath(oBrush, oPath);
  Finally
    oPath.Free;
  End;
End;


Function TGdiPlusExtendedGraphics.DrawTriangle(Const oPen: TGPPen; Const aRect: TGPRect): TGdiPlusStatus;
Begin
  Result := DrawTriangle(oPen, MakeRectF(aRect));
End;


Function TGdiPlusExtendedGraphics.DrawTriangle(Const oPen: TGPPen; Const aRectF: TGPRectF): TGdiPlusStatus;
Var
  aPointArray : TPointFDynArray;
Begin
  SetLength(aPointArray, 3);

  aPointArray[0] := MakePoint(aRectF.X, aRectF.Y);
  aPointArray[1] := MakePoint(aRectF.X + aRectF.Width, aRectF.Y);
  aPointArray[2] := MakePoint(aRectF.X + (aRectF.Width / 2), aRectF.Y + aRectF.Height);

  Result := DrawPolygon(oPen, PGPPointF(aPointArray), 3);
End;


Function TGdiPlusExtendedGraphics.FillTriangle(Const oBrush: TGPBrush; Const aRect: TGPRect): TGdiPlusStatus;
Begin
  Result := FillTriangle(oBrush, MakeRectF(aRect));
End;


Function TGdiPlusExtendedGraphics.FillTriangle(Const oBrush: TGPBrush; Const aRectF: TGPRectF): TGdiPlusStatus;
Var
  aPointArray : TPointFDynArray;
Begin
  SetLength(aPointArray, 3);

  aPointArray[0] := MakePoint(aRectF.X, aRectF.Y);
  aPointArray[1] := MakePoint(aRectF.X + aRectF.Width, aRectF.Y);
  aPointArray[2] := MakePoint(aRectF.X + (aRectF.Width / 2), aRectF.Y + aRectF.Height);

  Result := FillPolygon(oBrush, PGPPointF(aPointArray), 3);
End;


Function TGdiPlusExtendedGraphics.DrawValidatedString(Const sValue: String; Const oFont: TGPFont; Const oStringFormat: TGPStringFormat; Const aLayoutRectF: TGPRectF; Const oBrush: TGPBrush) : TGdiPlusStatus;
Begin
  Assert(Condition(Assigned(oFont), 'DrawValidatedString', 'oFont is not assigned.'));
  Assert(Condition(Assigned(oStringFormat), 'DrawValidatedString', 'oStringFormat is not assigned.'));
  Assert(Condition(Assigned(oBrush), 'DrawValidatedString', 'oBrush is not assigned.'));

  Result := Ok;

  If (sValue <> '') Then
    Result := DrawString(sValue, Length(sValue), oFont, aLayoutRectF, oStringFormat, oBrush);
End;


Function TGdiPlusExtendedGraphics.Condition(bCorrect: Boolean; Const sMethod, sMessage: String): Boolean;
Begin
  If Not bCorrect Then
    Error(sMethod, sMessage);

  Result := True;
End;


Procedure TGdiPlusExtendedGraphics.Error(Const sMethod, sMessage: String);
Begin
  If Assigned(Self) Then
    Raise EGdiPlusExtendedGraphicsException.Create('(' + Self.ClassName + '.' + sMethod + '): ' + sMessage)
  Else
    Raise EGdiPlusExtendedGraphicsException.Create('(Nil.' + sMethod + '): ' + sMessage);
End;


Function TGdiPlusExtendedGraphics.DrawValidatedString(Const sValue : String; Const oFont : TGPFont; Const oStringFormat : TGPStringFormat; Const aLayoutRectF: TGPRectF; Const oBrush : TGPBrush; Const aContraintSize : TGPSize) : TGdiPlusStatus;
Begin
  Assert(Condition(Assigned(oFont), 'DrawValidatedString', 'oFont is not assigned.'));
  Assert(Condition(Assigned(oStringFormat), 'DrawValidatedString', 'oStringFormat is not assigned.'));
  Assert(Condition(Assigned(oBrush), 'DrawValidatedString', 'oBrush is not assigned.'));

  Result := Ok;

  If (sValue <> '') And (aLayoutRectF.Width > aContraintSize.Width) And (aLayoutRectF.Height > aContraintSize.Height) Then
    Result := DrawString(sValue, Length(sValue), oFont, aLayoutRectF, oStringFormat, oBrush);
End;


Function TGdiPlusExtendedGraphics.DrawChevron(Const oPen: TGPPen; Const aOriginPoint: TGPPointF): TGdiPlusStatus;
Var
  aPointArray : TPointFDynArray;
Begin
  SetLength(aPointArray, 3);

  aPointArray[0] := CreatePointF(aOriginPoint.X, aOriginPoint.Y);
  aPointArray[1] := CreatePointF(aOriginPoint.X + 2, aOriginPoint.Y + 2);
  aPointArray[2] := CreatePointF(aOriginPoint.X + 4, aOriginPoint.Y);

  DrawLines(oPen, PGPPointF(aPointArray), 3);

  aPointArray[0] := CreatePointF(aOriginPoint.X, aOriginPoint.Y + 1);
  aPointArray[1] := CreatePointF(aOriginPoint.X + 2, aOriginPoint.Y + 2 + 1);
  aPointArray[2] := CreatePointF(aOriginPoint.X + 4, aOriginPoint.Y + 1);

  DrawLines(oPen, PGPPointF(aPointArray), 3);

  aPointArray[0] := CreatePointF(aOriginPoint.X, aOriginPoint.Y + 3);
  aPointArray[1] := CreatePointF(aOriginPoint.X + 2, aOriginPoint.Y + 2 + 3);
  aPointArray[2] := CreatePointF(aOriginPoint.X + 4, aOriginPoint.Y + 3);

  DrawLines(oPen, PGPPointF(aPointArray), 3);

  aPointArray[0] := CreatePointF(aOriginPoint.X, aOriginPoint.Y + 4);
  aPointArray[1] := CreatePointF(aOriginPoint.X + 2, aOriginPoint.Y + 2 + 4);
  aPointArray[2] := CreatePointF(aOriginPoint.X + 4, aOriginPoint.Y + 4);

  Result := DrawLines(oPen, PGPPointF(aPointArray), 3);
End;

Function TGdiPlusExtendedGraphics.DrawImage(Const oImage: TGPImage; Const aDestinationRectangle: TGPRectF; Const oImageAttributes: TGPImageAttributes): TGdiPlusStatus;
Begin
  Result := DrawImage(oImage, aDestinationRectangle, 0, 0, oImage.GetWidth, oImage.GetHeight, UnitPixel, oImageAttributes, Nil, Nil);
End;


Function TGdiPlusExtendedGraphics.StringSize(Const sValue: String; Const oFont: TGPFont; Const oStringFormat: TGPStringFormat): TGPSizeF;
Var
  aLayoutPointF : TGPPointF;
  aOutBoundingRectF : TGPRectF;
Begin
  aLayoutPointF.X := 0;
  aLayoutPointF.Y := 0;

  MeasureString(sValue, Length(sValue), oFont, aLayoutPointF, oStringFormat, aOutBoundingRectF);

  Result.Width := aOutBoundingRectF.Width;
  Result.Height := aOutBoundingRectF.Height;
End;

Constructor TGdiPlusHotSpot.Create;
Begin
  Inherited;

  FClickable := True;
  FRegion := TGPRegion.Create;
  FHintStringList := TFslStringList.Create;
End;


Destructor TGdiPlusHotSpot.Destroy;
Begin
  FHintStringList.Free;
  FRegion.Free;
  FRegion := Nil;

  Inherited;
End;


Procedure TGdiPlusHotSpot.SetRegion(oPath: TGPGraphicsPath);
Begin
  FRegion.MakeEmpty;
  FRegion.Union(oPath);
End;


Function TGdiPlusHotSpot.PointInRegion(X, Y: Integer): Boolean;
Begin
  Result := FRegion.IsVisible(X, Y);
End;


function TGdiPlusHotSpot.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHintStringList.sizeInBytes);
end;

Function TGdiPlusHotSpotList.GetRegionByIndex(Const iIndex: Integer): TGdiPlusHotSpot;
Begin
  Result := TGdiPlusHotSpot(ObjectByIndex[iIndex]);
End;


Function TGdiPlusHotSpotList.ItemClass: TFslObjectClass;
Begin
  Result := TGdiPlusHotSpot;
End;


Function TGdiPlusHotSpotList.Link: TGdiPlusHotSpotList;
Begin
  Result := TGdiPlusHotSpotList(Inherited Link);
End;


Function TGdiPlusHotSpot.Link: TGdiPlusHotSpot;
Begin
  Result := TGdiPlusHotSpot(Inherited Link);
End;


Procedure TGdiPlusHotSpot.SetRegion(oPath: TGPRect);
Begin
  FRegion.MakeEmpty;
  FRegion.Union(oPath);
End;







Constructor TGdiPlusCustomControl.Create(oOwner: TComponent);
Begin
  Inherited;

  If Assigned(oOwner) And (oOwner Is TWinControl) Then
    Parent := TWinControl(oOwner);

  FTrackMouseLeave := True;
  FInputEnabled := True;
End;


Destructor TGdiPlusCustomControl.Destroy;
Begin

  Inherited;
End;


Procedure TGdiPlusCustomControl.WMPaint(Var aMessage : TWMPaint);
Var
  hPaintDC : HDC;
  aPaintStructure : TPaintStruct;
  aClippingRect : TRect;
Begin
  If (aMessage.DC = 0) Then
  Begin
    hPaintDC := BeginPaint(Handle, aPaintStructure);
    Try
      PaintToDC(hPaintDC, aPaintStructure.rcPaint);
    Finally
      EndPaint(Handle, aPaintStructure)
    End;
  End
  Else
  Begin
    GetClipBox(aMessage.DC, aClippingRect);
    PaintToDC(aMessage.DC, aClippingRect);
  End;

  aMessage.Result := 0;
End;


Procedure TGdiPlusCustomControl.PaintToDC(Const pInputDC : HDC; Const aClippingRectangle : TRect);
Var
  hTemporaryDC : HDC;
  hMemoryDC : HDC;
  hMemoryBitmap : HBITMAP;
  hPreviousBitmap : HBITMAP;
  oMemoryGraphics : TGdiPlusExtendedGraphics;
Begin
  If DoubleBuffered Then
  Begin
    hTemporaryDC := GetDC(0);
    Try
      hMemoryBitmap := CreateCompatibleBitmap(hTemporaryDC, ClientRect.Right, ClientRect.Bottom);
    Finally
      ReleaseDC(0, hTemporaryDC);
    End;

    hMemoryDC := CreateCompatibleDC(0);
    hPreviousBitmap := SelectObject(hMemoryDC, hMemoryBitmap);
    Try
      oMemoryGraphics := TGdiPlusExtendedGraphics.Create(hMemoryDC);
      Try
        oMemoryGraphics.SetClip(MakeRect(aClippingRectangle));

        If Not Transparent Then
          oMemoryGraphics.Clear(ColorRefToARGB(ColorToRGB(Color)));

        Paint(oMemoryGraphics, pInputDC, MakeRect(aClippingRectangle));

        If PaintChildControls Then
          PaintControls(hMemoryDC, Nil);
      Finally
        oMemoryGraphics.Free;
      End;

      BitBlt(pInputDC, 0, 0, ClientRect.Right, ClientRect.Bottom, hMemoryDC, 0, 0, SRCCOPY);
    Finally
      SelectObject(hMemoryDC, hPreviousBitmap);
      DeleteDC(hMemoryDC);
      DeleteObject(hMemoryBitmap);
    End;

    FHasCompletedInitialPaint := True;
  End
  Else
  Begin
    oMemoryGraphics := TGdiPlusExtendedGraphics.Create(pInputDC);
    Try
      oMemoryGraphics.SetClip(MakeRect(aClippingRectangle));

      If Not Transparent Then
        oMemoryGraphics.Clear(ColorRefToARGB(ColorToRGB(Color)));

      Paint(oMemoryGraphics, pInputDC, MakeRect(aClippingRectangle));

      If PaintChildControls Then
        PaintControls(pInputDC, Nil);
    Finally
      oMemoryGraphics.Free;
    End;

    FHasCompletedInitialPaint := True;
  End;
End;


Procedure TGdiPlusCustomControl.WMEraseBkgnd(Var aMessage: TWmEraseBkgnd);
Begin
  aMessage.Result := 1;
End;


Procedure TGdiPlusCustomControl.CreateParams(Var aParams: TCreateParams);
Begin
  Inherited CreateParams(aParams);

  aParams.Style := aParams.Style Or WS_CLIPCHILDREN;

  If Transparent Then
    aParams.ExStyle := aParams.ExStyle + WS_EX_TRANSPARENT;
End;


Procedure TGdiPlusCustomControl.Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect);
Begin
End;


Procedure TGdiPlusCustomControl.Error(Const sMethod, sMessage : String);
Begin
  If Assigned(Self) Then
    Raise EGdiPlusCustomControlException.Create('(' + Self.ClassName + '.' + sMethod + '): ' + sMessage)
  Else
    Raise EGdiPlusCustomControlException.Create('(Nil.' + sMethod + '): ' + sMessage);
End;


Procedure TGdiPlusCustomControl.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TGdiPlusCustomControl.AlignClient;
Begin
  Align := alClient;
End;


Procedure TGdiPlusCustomControl.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TGdiPlusCustomControl.AlignRight;
Begin
  Align := alRight;
End;


Procedure TGdiPlusCustomControl.AlignTop;
Begin
  Align := alTop;
End;


Function TGdiPlusCustomControl.Condition(Const bTruth: Boolean; Const sMethod, sMessage: String): Boolean;
Begin
  If Not bTruth Then
    Error(sMethod, sMessage);

  Result := True;
End;


Function TGdiPlusCustomControl.Invariant(Const sMethod, sMessage: String): Boolean;
Begin
  // Call this method as you would the Error method to raise an exception.
  // Use this when you are not sure if self is valid as it is a non-virtual method.

  Raise EFslInvariant.Create(Self, sMethod, sMessage); // Can't use Error method here as it is virtual.

  Result := True;
End;


Function TGdiPlusCustomControl.Invariants(Const sLocation : String; oObject : TObject; aClass: TClass; Const sObject : String) : Boolean;
Begin
  If Not Assigned(aClass) Then
    Invariant('Invariants', 'aClass was not assigned.');

  // Ensure object is assigned.
  If Not Assigned(oObject) Then
    Invariant(sLocation, sObject + ' was not assigned and was expected to have been of class ' + aClass.ClassName);

  // Ensure object is of the expected class.
  If Not oObject.InheritsFrom(aClass) Then
    Invariant(sLocation, sObject + ' was of class ' + oObject.ClassName + ' and should have been of class ' + aClass.ClassName);

  Result := True;
End;


Function TGdiPlusCustomControl.Invariants(Const sLocation : String; oObject : TFslObject; aClass : TFslObjectClass; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, TObject(oObject), aClass, sObject);

  Result := True;
End;


Procedure TGdiPlusCustomControl.ShuffleTop;
Begin
  Top := 0;
End;


Procedure TGdiPlusCustomControl.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TGdiPlusCustomControl.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TGdiPlusCustomControl.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TGdiPlusCustomControl.InvalidateRectangle(Const aInvalidRect : TGPRect);
Var
  aRect : TRect;
Begin
  aRect := GDIPlusRectToVCLRect(aInvalidRect);

  InvalidateRect(Handle, @aRect, False);
  InvalidateParent;
End;


Procedure TGdiPlusCustomControl.InvalidateRectangle(Const aInvalidRectF : TGPRectF);
Var
  aRect : TRect;
Begin
  aRect := GDIPlusRectToVCLRect(RoundRectangle(aInvalidRectF));

  InvalidateRect(Handle, @aRect, False);
  InvalidateParent;
End;


Procedure TGdiPlusCustomControl.ValidateRectangle(Const aInvalidRect: TGPRect);
Var
  aRect : TRect;
Begin
  aRect := GDIPlusRectToVCLRect(aInvalidRect);

  ValidateRect(Handle, @aRect);
End;


Procedure TGdiPlusCustomControl.ValidateRectangle(Const aInvalidRectF: TGPRectF);
Var
  aRect : TRect;
Begin
  aRect := GDIPlusRectToVCLRect(RoundRectangle(aInvalidRectF));

  ValidateRect(Handle, @aRect);
End;


Procedure TGdiPlusCustomControl.WMMouseLeave(Var aMessage: TMessage);
Begin
  MouseLeave;
  FIsTrackingMouseLeave := False;

  aMessage.Result := 0;
End;


Procedure TGdiPlusCustomControl.MouseLeave;
Begin
End;


Procedure TGdiPlusCustomControl.MouseMove(aShiftState: TShiftState; iX, iY: Integer);
Var
  aTrackEvents : TTrackMouseEvent;
Begin
  Inherited;

  If TrackMouseLeave And Not FIsTrackingMouseLeave And InputEnabled Then
  Begin
    FillChar(aTrackEvents, SizeOf(aTrackEvents), 0);
    aTrackEvents.cbSize := SizeOf(aTrackEvents);
    aTrackEvents.dwFlags := TME_LEAVE;
    aTrackEvents.hwndTrack := Handle;
    aTrackEvents.dwHoverTime := 0;

    TrackMouseEvent(aTrackEvents);
    FIsTrackingMouseLeave := True;
  End;
End;


Function TGdiPlusCustomControl.ClientRectangle: TGPRect;
Begin
  Result := MakeRect(ClientRect);
End;


Procedure TGdiPlusCustomControl.InvalidateParent;
Var
  aClientRect : TRect;
Begin
  If Assigned(Parent) And Parent.HandleAllocated Then
  Begin
    aClientRect := BoundsRect;
    InvalidateRect(Parent.Handle, @aClientRect, False);
  End;
End;


Procedure TGdiPlusCustomControl.Invalidate;
Begin
  If Transparent Then
    InvalidateParent;

  If HandleAllocated Then
    InvalidateRect(Handle, Nil, False);
End;


Function TGdiPlusCustomControl.GdiPlusClientToScreen(Const aRectangle : TGPRect): TGPRect;
Var
  aClientOrigin : TPoint;
Begin
  aClientOrigin := ClientOrigin;

  Result.X := aRectangle.X + aClientOrigin.X;
  Result.Y := aRectangle.Y + aClientOrigin.Y;
  Result.Width := aRectangle.Width;
  Result.Height := aRectangle.Height;
End;


Procedure TGdiPlusCustomControl.CMEnabledChanged(Var aMessage: TMessage);
Begin
  Inherited;

  InvalidateRect(Handle, Nil, False);
End;


Function TGdiPlusCustomControl.GetInputEnabled: Boolean;
Begin
  Result := FInputEnabled;
End;


Procedure TGdiPlusCustomControl.SetInputEnabled(Const Value: Boolean);

  Procedure RemoveFocus(Removing: Boolean); // From TWinControl
  Var
    Form: TCustomForm;
  Begin
    Form := GetParentForm(Self);
    If Form <> Nil Then Form.DefocusControl(Self, Removing);
  End;

Begin
  If FInputEnabled <> Value Then
  Begin
    FInputEnabled := Value;

    If Not Value Then
    Begin
      ControlStyle := ControlStyle + [csNoStdEvents];

      If (Parent <> Nil) Then
        RemoveFocus(False);
    End
    Else
      ControlStyle := ControlStyle - [csNoStdEvents];

    InvalidateRect(Handle, Nil, False);
  End;
End;


Procedure TGdiPlusCustomControl.SynchronousInvalidate(Const aInvalidRect : TGPRect);
Var
  aUpdateRect : TRect;
Begin
  aUpdateRect := GDIPlusRectToVCLRect(aInvalidRect);
  RedrawWindow(Handle, @aUpdateRect, 0, RDW_INVALIDATE Or RDW_UPDATENOW);
End;


Procedure TGdiPlusCustomControl.SynchronousInvalidate(Const aInvalidRectF: TGPRectF);
Begin
  SynchronousInvalidate(RoundCeilingRectangle(aInvalidRectF));
End;


Constructor TGdiPlusStringFormat.Create;
Begin
  Inherited;

  FStringFormat := TGPStringFormat.Create;

  If (FStringFormat.GetLastStatus <> Ok) Then
    RaiseError('Create', 'Failed to create FStringFormat (TGPStringFormat). Error Message: ' + StatusToString(FStringFormat.GetLastStatus));

  TrimModeEllipsisCharacter;
  LineLimit := True;
  HorizontalAlignmentCenter;
  VerticalAlignmentMiddle;
End;


Destructor TGdiPlusStringFormat.Destroy;
Begin
  FStringFormat.Free;

  Inherited;
End;


Procedure TGdiPlusStringFormat.ApplyFormatFlags(Const iFlags: Integer);
Begin
  FStringFormat.SetFormatFlags(iFlags);
End;


Procedure TGdiPlusStringFormat.HorizontalAlignmentCenter;
Begin
  FStringFormat.SetAlignment(StringAlignmentCenter);
End;


Procedure TGdiPlusStringFormat.HorizontalAlignmentLeft;
Begin
  FStringFormat.SetAlignment(StringAlignmentNear);
End;


Procedure TGdiPlusStringFormat.HorizontalAlignmentRight;
Begin
  FStringFormat.SetAlignment(StringAlignmentFar);
End;


Function TGdiPlusStringFormat.Link: TGdiPlusStringFormat;
Begin
  Result := TGdiPlusStringFormat(Inherited Link);
End;


Function TGdiPlusStringFormat.ProduceStringFormat: TGPStringFormat;
Var
  iFormatFlags : Integer;
Begin
  If Not OverrideFlags Then
  Begin
     iFormatFlags := 0;

    If NoWrap Then
      iFormatFlags := iFormatFlags Or StringFormatFlagsNoWrap;

    If LineLimit Then
      iFormatFlags := iFormatFlags Or StringFormatFlagsLineLimit;

    If RightToLeft Then
      iFormatFlags := iFormatFlags Or StringFormatFlagsDirectionRightToLeft;

    If DirectionVertical Then
      iFormatFlags := iFormatFlags Or StringFormatFlagsDirectionVertical;

    FStringFormat.SetFormatFlags(iFormatFlags);
  End;

  Result := FStringFormat;
End;


Function TGdiPlusStringFormat.ProduceDrawTextFlags: Integer;
Begin
  Result := 0;

  Case FStringFormat.GetTrimming Of
    StringTrimmingEllipsisCharacter : Result := Result Or DT_END_ELLIPSIS;
    StringTrimmingEllipsisWord : Result := Result Or DT_WORD_ELLIPSIS;
    StringTrimmingEllipsisPath : Result := Result Or DT_PATH_ELLIPSIS;
  End;

  Case FStringFormat.GetLineAlignment Of
    StringAlignmentNear : Result := Result Or DT_BOTTOM;
    StringAlignmentCenter : Result := Result Or DT_VCENTER;
    StringAlignmentFar : Result := Result Or DT_TOP;
  End;

  Case FStringFormat.GetAlignment Of
    StringAlignmentNear : Result := Result Or DT_LEFT;
    StringAlignmentCenter : Result := Result Or DT_CENTER;
    StringAlignmentFar : Result := Result Or DT_RIGHT;
  End;

  If NoWrap Then
    Result := Result Or DT_SINGLELINE;
End;


Procedure TGdiPlusStringFormat.TrimModeCharacter;
Begin
  FStringFormat.SetTrimming(StringTrimmingCharacter);
End;


Procedure TGdiPlusStringFormat.TrimModeEllipsisCharacter;
Begin
  FStringFormat.SetTrimming(StringTrimmingEllipsisCharacter);
End;


Procedure TGdiPlusStringFormat.TrimModeEllipsisPath;
Begin
  FStringFormat.SetTrimming(StringTrimmingEllipsisPath);
End;

Procedure TGdiPlusStringFormat.TrimModeEllipsisWord;
Begin
  FStringFormat.SetTrimming(StringTrimmingEllipsisWord);
End;


Procedure TGdiPlusStringFormat.TrimModeNone;
Begin
  FStringFormat.SetTrimming(StringTrimmingNone);
End;


Procedure TGdiPlusStringFormat.TrimModeWord;
Begin
  FStringFormat.SetTrimming(StringTrimmingWord);
End;


Procedure TGdiPlusStringFormat.VerticalAlignmentBottom;
Begin
  FStringFormat.SetLineAlignment(StringAlignmentFar);
End;


Procedure TGdiPlusStringFormat.VerticalAlignmentMiddle;
Begin
  FStringFormat.SetLineAlignment(StringAlignmentCenter);
End;


Procedure TGdiPlusStringFormat.VerticalAlignmentTop;
Begin
  FStringFormat.SetLineAlignment(StringAlignmentNear);
End;


Constructor TGdiPlusFont.Create;
Begin
  Inherited;

  FFont := Nil;

  FSize := 9;
  FFontFamily := 'Arial';
  FColour := clBlack;
End;


Destructor TGdiPlusFont.Destroy;
Begin
  FFont.Free;

  Inherited;
End;


Function TGdiPlusFont.Link: TGdiPlusFont;
Begin
  Result := TGdiPlusFont(Inherited Link);
End;


Procedure TGdiPlusFont.Prepare;
Begin
  Assert(CheckCondition(Not StringEquals(FFontFamily, ''), 'Font', 'Font family is a blank string.'));
  Assert(CheckCondition(FSize >= 0, 'Font', 'Font size can not be negative.'));

  If Assigned(FFont) Then
    FFont.Free;

  FFont := TGPFont.Create(FFontFamily, FSize, FontStyle);

  If (FFont.GetLastStatus <> Ok) Then
    RaiseError('Prepare', 'TGPFont.Create has failed. Error Message: ' + StatusToString(FFont.GetLastStatus));
End;


Function TGdiPlusFont.ProduceFont : TGPFont;
Begin
  Prepare;

  Result := FFont;
End;


Function TGdiPlusFont.FontStyle : Integer;
Begin
  Result := FontStyleRegular;

  If Bold Then
    Result := Result Or FontStyleBold;

  If Italic Then
    Result := Result Or FontStyleItalic;

  If Underline Then
    Result := Result Or FontStyleUnderline;
End;


Function TGdiPlusFont.GetNativeFont: TGPFont;
Begin
  Assert(CheckCondition(Assigned(FFont), 'GetNativeFont', 'FFont is not assigned.'));
  Assert(CheckCondition(FFont.GetLastStatus = Ok, 'GetNativeFont', 'FFont status is not Ok.'));

  Result := FFont;
End;


Function TGdiPlusFont.CreateGDIFont: TFont;
Begin
  Result := TFont.Create;
  Result.Name := FontFamily;

  If Bold Then
    Result.Style := Result.Style + [fsBold];

  If Italic Then
    Result.Style := Result.Style + [fsItalic];

  If Underline Then
    Result.Style := Result.Style + [fsUnderline];

  Result.Size := RealCeiling(Size);
End;


(*
 oGDI := TGPGraphics.Create(oImage.NativeBitmap);
    Try
      oFont :=  TGPFont.Create('Courier New', 125);
      Try
        oOrigin := MakePoint(120.0, 120.0);
        oBrush := TGPSolidBrush.Create(argbGold);
        Try
        Finally
          oBrush.Free;
        End;
      Finally
        oFont.Free;
      End;
    Finally
      oGDI.Free;
    End;
    *)

function TGdiPlusFont.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFontFamily.length * sizeof(char)) + 12);
end;

{ TGdiPlusImageAnnotator }

destructor TGdiPlusImageAnnotator.Destroy;
begin
  Closeup;
  FImage.Free;
  inherited;
end;

procedure TGdiPlusImageAnnotator.Init;
begin
  assert(Invariants('init', FImage, TGdiPlusBitmapImage, 'Image'));
  assert(CheckCondition(FFontName <> '', 'init', 'a font name must be provided'));
  assert(CheckCondition(FFontSize > 0, 'init', 'a font size must be provided'));

  FGDI := TGPGraphics.Create(FImage.NativeBitmap);
  FFont :=  TGPFont.Create(FFontName, FFontSize);
  FBrush := TGPSolidBrush.Create(FFontColour);
end;

procedure TGdiPlusImageAnnotator.Closeup;
begin
  FBrush.Free;
  FBrush := nil;
  FFont .Free;
  FFont  := nil;
  FGDI.Free;
  FGDI := nil;
end;

procedure TGdiPlusImageAnnotator.SetFontColour(const Value: TGPColor);
begin
  Closeup;
  FFontColour := Value;
end;

procedure TGdiPlusImageAnnotator.SetFontName(const Value: String);
begin
  Closeup;
  FFontName := Value;
end;

procedure TGdiPlusImageAnnotator.SetFontSize(const Value: Double);
begin
  Closeup;
  FFontSize := Value;
end;

procedure TGdiPlusImageAnnotator.SetImage(const Value: TGdiPlusBitmapImage);
begin
  Closeup;
  FImage.Free;
  FImage := Value;
end;

procedure TGdiPlusImageAnnotator.Write(const sText: String; rX, rY: Double);
var
  oOrigin : TGPPointF;
begin
  Init;
  oOrigin := MakePoint(rX, rY);
  FGDI.DrawString(sText, length(sText), FFont, oOrigin, FBrush);
end;

procedure TGdiPlusImageAnnotator.Annotate(sText: String);
begin
  write(sText, MARGIN * FImage.Width, FTop);
  FTop := FTop + FStride;
end;

procedure TGdiPlusImageAnnotator.Start(iGridCount: integer);
begin
  FontSize := (FImage.Height / iGridCount) * 0.6;
  FStride := FImage.Height / iGridCount;
  FTop := MARGIN * FImage.Height;
end;

function TGdiPlusImageAnnotator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFontName.length * sizeof(char)) + 12);
  inc(result, FImage.sizeInBytes);
end;

End.
