unit wp_graphics;

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

{$i fhir.inc}

interface

uses
  Windows,
  SysUtils, Classes, Graphics, Types,
  {$IFDEF DELPHI} Jpeg, PNGImage,  GraphicEx, {$ENDIF}
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_shell;

Type
  TRect = Windows.TRect;
  TPoint = Windows.TPoint;


Function Rect(iLeft, iTop, iRight, iBottom : Integer) : TRect; Overload;
Procedure RectZero(Var aRect : TRect); Overload;
Function RectZero : TRect; Overload;
Function RectEmpty(Const aRect : TRect) : Boolean; Overload;
Function RectEqual(Const A, B : TRect) : Boolean; Overload;

Function RectOffset(Const aRect : TRect; iX, iY : Integer) : TRect; Overload;
Function RectIntersect(Const A, B : TRect) : TRect; Overload;
Function RectSubtract(Const A, B : TRect) : TRect; Overload;
Function RectUnion(Const A, B : TRect) : TRect; Overload;
Function RectHasIntersection(Const A, B : TRect) : Boolean; Overload;
Function RectInflate(Const aRect : TRect; iValue : Integer) : TRect; Overload;
Function RectInflate(Const aRect : TRect; iX, iY : Integer) : TRect; Overload;

Function RectWidth(Const aRect : TRect) : Integer; Overload;
Function RectHeight(Const aRect : TRect) : Integer; Overload;

Function RectHit(Const aRect : TRect; Const aPoint : TPoint) : Boolean; Overload;
Function RectBound(Const aRect, aBoundary : TRect) : TRect; Overload;


Type
  TFslResourceType = (rtAccelerator, rtAniCursor, rtAniIcon, rtBitmap, rtCursor, rtDialog, rtFont, rtFontDir,
    rtGroupCursor, rtGroupIcon, rtIcon, rtMenu, rtMessageTable, rtRCData, rtString, rtVersion);

  TFslResourceStream = Class(TFslMemoryStream)
    Private
      FFilename : String;
      FResourceName : String;
      FResourceType : TFslResourceType;
      FResourceHandle : THandle;
      FResourceData : Pointer;

    Protected
      Function ToWindowsResourceType(oResourceType : TFslResourceType) : PChar;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      Procedure BeforeDestruction; Override;

      Function Link : TFslResourceStream;

      Procedure Open;
      Procedure Close;
      Function Active : Boolean;

      Function ExistsAcceleratorTable : Boolean;
      Function ExistsAnimatedCursor : Boolean;
      Function ExistsAnimatedIcon : Boolean;
      Function ExistsBitmap : Boolean;
      Function ExistsHardwareDependentCursor : Boolean;
      Function ExistsDialogBox : Boolean;
      Function ExistsFont : Boolean;
      Function ExistsFontDirectory : Boolean;
      Function ExistsHardwareIndependentCursor : Boolean;
      Function ExistsHardwareIndependentIcon : Boolean;
      Function ExistsHardwareDependentIcon : Boolean;
      Function ExistsMenu : Boolean;
      Function ExistsMessageTableEntry : Boolean;
      Function ExistsApplicationDefined : Boolean;
      Function ExistsStringTableEntry : Boolean;
      Function ExistsVersion : Boolean;
      Function ExistsByResourceType(Const aResourceType : TFslResourceType) : Boolean;

      Procedure ResourceTypeAcceleratorTable;
      Procedure ResourceTypeAnimatedCursor;
      Procedure ResourceTypeAnimatedIcon;
      Procedure ResourceTypeBitmap;
      Procedure ResourceTypeHardwareDependentCursor;
      Procedure ResourceTypeDialogBox;
      Procedure ResourceTypeFont;
      Procedure ResourceTypeFontDirectory;
      Procedure ResourceTypeHardwareIndependentCursor;
      Procedure ResourceTypeHardwareIndependentIcon;
      Procedure ResourceTypeHardwareDependentIcon;
      Procedure ResourceTypeMenu;
      Procedure ResourceTypeMessageTableEntry;
      Procedure ResourceTypeApplicationDefined;
      Procedure ResourceTypeStringTableEntry;
      Procedure ResourceTypeVersion;

      Function IsResourceTypeAcceleratorTable : Boolean;
      Function IsResourceTypeAnimatedCursor : Boolean;
      Function IsResourceTypeAnimatedIcon : Boolean;
      Function IsResourceTypeBitmap : Boolean;
      Function IsResourceTypeHardwareDependentCursor : Boolean;
      Function IsResourceTypeDialogBox : Boolean;
      Function IsResourceTypeFont : Boolean;
      Function IsResourceTypeFontDirectory : Boolean;
      Function IsResourceTypeHardwareIndependentCursor : Boolean;
      Function IsResourceTypeHardwareIndependentIcon : Boolean;
      Function IsResourceTypeHardwareDependentIcon : Boolean;
      Function IsResourceTypeMenu : Boolean;
      Function IsResourceTypeMessageTableEntry : Boolean;
      Function IsResourceTypeApplicationDefined : Boolean;
      Function IsResourceTypeStringTableEntry : Boolean;
      Function IsResourceTypeVersion : Boolean;

      Property Filename : String Read FFilename Write FFilename;
      Property ResourceName : String Read FResourceName Write FResourceName;
      Property ResourceType : TFslResourceType Read FResourceType Write FResourceType;
  End;


  TFslVCLGraphic = class;

  TFslGraphic = Class(TFslObject)
    Protected
      Function GetWidth: Integer; Virtual;
      Procedure SetWidth(Const Value: Integer); Virtual;

      Function GetHeight: Integer; Virtual;
      Procedure SetHeight(Const Value: Integer); Virtual;

      function GetFrameIndex: Integer; Virtual;
      procedure SetFrameIndex(const Value: Integer); Virtual;
    Public
      Function Link : TFslGraphic;
      Function Clone : TFslGraphic;

      Procedure LoadFromStream(oStream : TFslStream); Virtual;
      Procedure SaveToStream(oStream : TFslStream); Virtual;

      Procedure LoadFromFile(Const sFilename : String); Virtual;
      Procedure SaveToFile(Const sFilename : String); Virtual;

      procedure DrawToStream(oStream : TFslStream; width, height : Integer); Virtual;

      // information about the graphic
      Function TypeName : String; Virtual;
      Function Extension : String; Virtual;
      Function FrameCount : Integer; Virtual;

      // drawing routines
      procedure StretchDraw(oCanvas : TCanvas; aRect : TRect); virtual; // draw to a canvas
      function AsBitmap : TFslVCLGraphic; virtual; // get a bitmap representation

      Property Width : Integer Read GetWidth Write SetWidth;
      Property Height : Integer Read GetHeight Write SetHeight;
      Property FrameIndex : Integer Read GetFrameIndex write SetFrameIndex;
  End;

  TFslGraphicList = Class(TFslObjectList)
    Private
      Function GetGraphic(iIndex: Integer): TFslGraphic;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Graphics[iIndex : Integer] : TFslGraphic Read GetGraphic; Default;
  End;

  TFslVCLGraphic = Class(TFslGraphic)
    Private
      FHandle : TGraphic;
      FBuffer : TFslBuffer;

      Function GetHandle: TGraphic;
      Procedure SetHandle(Const Value: TGraphic);

      Function GetTransparent: Boolean;
      Procedure SetTransparent(Const Value: Boolean);

    Protected
      Function GetWidth: Integer; Override;
      Procedure SetWidth(Const Value: Integer); Override;

      Function GetHeight: Integer; Override;
      Procedure SetHeight(Const Value: Integer); Override;

      Function HandleNew : TGraphic; Virtual;
      Function HandleClass : TGraphicClass; Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslVCLGraphic;
      Function Clone : TFslVCLGraphic;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure LoadFromStream(oStream : TFslStream); Override;
      Procedure SaveToStream(oStream : TFslStream); Override;

      procedure DrawToStream(oStream : TFslStream; width, height : Integer); Override;

      Procedure LoadFromFile(Const sFilename : String); Override;
      Procedure SaveToFile(Const sFilename : String); Override;

      Procedure LoadFromGraphic(Const oGraphic : TGraphic);

      Function HasHandle : Boolean; Virtual;

      Function Empty : Boolean; Virtual;

      procedure StretchDraw(oCanvas : TCanvas; aRect : TRect); Override;

      Property Handle : TGraphic Read GetHandle Write SetHandle;
      Property Transparent : Boolean Read GetTransparent Write SetTransparent;
  End;

  TFslVCLGraphicList = Class(TFslGraphicList)
    Private
      Function GetGraphic(iIndex: Integer): TFslVCLGraphic;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property VCLGraphics[iIndex : Integer] : TFslVCLGraphic Read GetGraphic; Default;
  End;

  TGraphic = Graphics.TGraphic;
  TGraphicClass = Graphics.TGraphicClass;



 TFslBitmapGraphic = Class(TFslVCLGraphic)
    Private
      Function GetHandle: TBitmap;
      Procedure SetHandle(Const Value: TBitmap);

    Protected
      Function HandleClass : TGraphicClass; Override;
      Function HandleNew : TGraphic; Override;

      Function BytesPerLine : Integer;

    Public
      Function Link : TFslBitmapGraphic;
      Function Clone : TFslBitmapGraphic;

      Procedure LoadFromResource(Const sResource : String);

      Function ConstructRotate(Const AngleOfRotation : Double): TBitmap;
      Function ConstructFitToPage(Const iWidth, iHeight : Integer): TFslBitmapGraphic;

      Class Function CanLoad(oStream : TStream) : Boolean;

      Property Handle : TBitmap Read GetHandle Write SetHandle;
  End;

  TFslBitmapGraphicList = Class(TFslVCLGraphicList)
    Private
      Function GetBitmapGraphicByIndex(iIndex: Integer): TFslBitmapGraphic;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property BitmapGraphicByIndex[iIndex : Integer] : TFslBitmapGraphic Read GetBitmapGraphicByIndex; Default;
  End;

  TBitmap = Graphics.TBitmap;

  TFslGraphicMetre = Integer; // 10ths of a millimetre
  TFslGraphicMetreRect = TRect;

  TFslGraphicHandle = THandle;

  TFslGraphicCapability = Class(TFslObject)
    Private
      FHandle : TFslGraphicHandle;
      FPixelsPerInchY : Integer;
      FPixelsPerInchX : Integer;
      FPixelsPerGraphicMetre : Real;

    Protected
      Property Handle : TFslGraphicHandle Read FHandle Write FHandle;

    Public
      Function ToPixel(iValue : TFslGraphicMetre) : Integer; Overload; Virtual;
      Function FromPixel(iValue : Integer) : TFslGraphicMetre; Overload; Virtual;

      Property PixelsPerGraphicMetre : Real Read FPixelsPerGraphicMetre Write FPixelsPerGraphicMetre;
      Property PixelsPerInchX : Integer Read FPixelsPerInchX Write FPixelsPerInchX;
      Property PixelsPerInchY : Integer Read FPixelsPerInchY Write FPixelsPerInchY;
  End;

  TFslGraphicAngle = Word; // 0..360;
  TFslGraphicColour = TColour;


  TFslGraphicObject = Class (TFslObject)
    Private
      FHandle : HGDIOBJ;
      FOnChange : TNotifyEvent;

      FCapability : TFslGraphicCapability;

      Function GetHandle : TFslGraphicHandle;

    Protected
      Function CreateHandle : TFslGraphicHandle; Overload; Virtual;

      Procedure Change; Overload; Virtual;
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;

      Procedure Clear; Overload; Virtual;
      Procedure ClearHandle; Overload; Virtual;

      Property Handle : TFslGraphicHandle Read GetHandle;
      Property Capability : TFslGraphicCapability Read FCapability Write FCapability;
      Property OnChange : TNotifyEvent read FOnChange write FOnChange;
  End;

  TFslPenStyle = (apsSolid, apsDash, apsDashDot, apsDashDotDot, apsDot, apsInsideFrame, apsNone);
  TFslPenEndStyle = (apesSquare, apesFlat, apesRound);
  TFslPenJoinStyle = (apjsMitre, apjsBevel, apjsRound);
  TFslPenWidth = Integer;

  TFslPen = Class (TFslGraphicObject)
    Private
      FWidth : TFslPenWidth;
      FColour : TFslGraphicColour;
      FEndStyle : TFslPenEndStyle;
      FJoinStyle : TFslPenJoinStyle;
      FStyle : TFslPenStyle;

      Procedure SetColour(Const Value: TFslGraphicColour);
      Procedure SetStyle(Const Value: TFslPenStyle);
      Procedure SetEndStyle(Const Value: TFslPenEndStyle);
      Procedure SetJoinStyle(Const Value: TFslPenJoinStyle);
      Procedure SetWidth(Const Value: Integer);

      Function GetStyleAsString: String;
      Procedure SetStyleAsString(Const Value: String);

    Protected
      Function CreateHandle : TFslGraphicHandle; Override;

    Public
      Procedure AfterConstruction; Override;

      Function Link : TFslPen;
      Function Clone : TFslPen;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Clear; Override;

      Procedure SetStyleSolid;
      Procedure SetStyleDash;
      Procedure SetStyleDot;
      Procedure SetStyleNone;

      Property Width : TFslPenWidth Read FWidth Write SetWidth;
      Property Colour : TFslGraphicColour Read FColour Write SetColour;
      Property Style : TFslPenStyle Read FStyle Write SetStyle;
      Property EndStyle : TFslPenEndStyle Read FEndStyle Write SetEndStyle;
      Property JoinStyle : TFslPenJoinStyle Read FJoinStyle Write SetJoinStyle;

      Property StyleAsString : String Read GetStyleAsString Write SetStyleAsString;
  End;


Const
  ADVPENSTYLE_CODES  : Array [TFslPenStyle] Of String = ('Solid',  'Dash',  'DashDot',  'DashDotDot',  'Dot',  'InsideFrame',  'None');
  ADVPENSTYLE_NAMES : Array [TFslPenStyle] Of String = ('Solid',  'Dash',  'Dash Dot',  'Dash Dot Dot',  'Dot',  'Inside Frame',  'None');
  ADVPENSTYLE_VALUES : Array [TFslPenStyle] Of Cardinal =  (PS_SOLID, PS_DASH, PS_DASHDOT, PS_DASHDOTDOT, PS_DOT, PS_INSIDEFRAME, PS_NULL);

  ADVPENENDSTYLE_CODES  : Array [TFslPenEndStyle] Of String = ('Square', 'Flat', 'Round');
  ADVPENENDSTYLE_VALUES : Array [TFslPenEndStyle] Of Cardinal = (PS_ENDCAP_SQUARE, PS_ENDCAP_FLAT, PS_ENDCAP_ROUND);

  ADVPENJOINSTYLE_CODES : Array [TFslPenJoinStyle] Of String = ('Mitre', 'Bevel', 'Round');
  ADVPENJOINSTYLE_VALUES : Array [TFslPenJoinStyle] Of DWord = (PS_JOIN_MITER, PS_JOIN_BEVEL, PS_JOIN_ROUND);

  ADVPENSTYLE_VCLVALUES : Array [TFslPenStyle] Of TPenStyle = (psSolid, psDash, psDashDot, psDashDotDot, psDot, psInsideFrame, psClear);

Type
  TFslBrushStyle = (absSolid, absNull, absHorizontal, absVertical, absFDiagonal, absBDiagonal, absCross, absDiagCross);

  TFslBrush = Class(TFslGraphicObject)
    Private
      FBitmap : TFslBitmapGraphic;
      FColour : TColour;
      FStyle : TFslBrushStyle;

      Function GetBitmap: TFslBitmapGraphic;
      Procedure SetBitmap(Const Value: TFslBitmapGraphic);

      Procedure SetColour(Const Value: TColour);
      Procedure SetStyle(Const Value: TFslBrushStyle);

      Function GetStyleAsString: String;
      Procedure SetStyleAsString(Const Value: String);

    Protected
      Function CreateHandle : TFslGraphicHandle; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslBrush;
      Function Clone : TFslBrush;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Clear; Override;

      Procedure SetStyleSolid;
      Procedure SetStyleClear;

      Function HasBitmap : Boolean;

      // either a bitmap, or a colour and a style
      Property Bitmap : TFslBitmapGraphic Read GetBitmap Write SetBitmap;
      Property Colour : TColour Read FColour Write SetColour;
      Property Style : TFslBrushStyle Read FStyle Write SetStyle;

      Property StyleAsString : String Read GetStyleAsString Write SetStyleAsString;
  End;


Const
  ADVBRUSHSTYLE_CODES : Array [TFslBrushStyle] Of String =
    ('Solid', 'Clear', 'Horizontal', 'Vertical', 'FDiagonal', 'BDiagonal', 'Cross', 'DiagCross');

  ADVBRUSHSTYLE_NAMES : Array [TFslBrushStyle] Of String =
    ('Solid', 'Clear', 'Horizontal', 'Vertical', 'Forward Diagonal', 'Backward Diagonal', 'Cross', 'Diagonal Cross');

  ADVBRUSHSTYLE_VALUES : Array [TFslBrushStyle] Of Cardinal =
    (BS_SOLID, BS_NULL, BS_HATCHED, BS_HATCHED, BS_HATCHED, BS_HATCHED, BS_HATCHED, BS_HATCHED);

  ADVBRUSHSTYLE_HASHVALUES : Array [TFslBrushStyle] Of Cardinal =
    (0, 0, HS_HORIZONTAL,HS_VERTICAL, HS_FDIAGONAL, HS_BDIAGONAL, HS_CROSS, HS_DIAGCROSS);

Type
  TFslFontWeight = (afwUnknown, afwThin, afwExtraLight, afwLight, afwNormal, afwMedium, afwSemiBold, afwBold, afwExtraBold, afwHeavy);
  TFslFontFamily = (affDontCare, affDecorative, affModern, affRoman, affScript, affSwiss);
  TFslFontPitch = (afpDontCare, afpFixed, afpVariable);

  TFslFontSize = Word;

  TFslFont = Class (TFslGraphicObject)
    Private
      FItalic : Boolean;
      FUnderline : Boolean;
      FStrikeOut : Boolean;
      FName : String;
      FCharRotation : TFslGraphicAngle;
      FColour : TFslGraphicColour;
      FFamily : TFslFontFamily;
      FPitch : TFslFontPitch;
      FWeight : TFslFontWeight;
      FSize : TFslFontSize;
      FTextRotation : TFslGraphicAngle;

      Procedure SetColour(Const Value: TFslGraphicColour);
      Procedure SetFamily(Const Value: TFslFontFamily);
      Procedure SetItalic(Const Value: Boolean);
      Procedure SetName(Const Value: String);
      Procedure SetPitch(Const Value: TFslFontPitch);
      Procedure SetCharRotation(Const Value: TFslGraphicAngle);
      Procedure SetSize(Const Value: Word);
      Procedure SetStrikeOut(Const Value: Boolean);
      Procedure SetUnderline(Const Value: Boolean);
      Procedure SetWeight(Const Value: TFslFontWeight);
      Procedure SetTextRotation(Const Value: TFslGraphicAngle);

      Function GetBold: Boolean;
      Procedure SetBold(Const Value: Boolean);

      Function GetWeightAsString: String;
      Procedure SetWeightAsString(Const Value: String);

    Protected
      Function CreateHandle : TFslGraphicHandle; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;

      Function Link : TFslFont;
      Function Clone : TFslFont;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Clear; Override;
      Procedure ClearStyles;

      Procedure SetVCLFont(oFont : TFont);

      Function MakeHandle(iPixelsPerInchY: Integer): TFslGraphicHandle;

      Property Colour : TFslGraphicColour Read FColour Write SetColour;
      Property Size : TFslFontSize Read FSize Write SetSize; // will be mapped to height internally

      // specify either Name or FontFamily and Pitch
      Property Name: String Read FName Write SetName;
      Property Family : TFslFontFamily Read FFamily Write SetFamily;
      Property Pitch : TFslFontPitch Read FPitch Write SetPitch;

      Property CharRotation : TFslGraphicAngle Read FCharRotation Write SetCharRotation;
      Property TextRotation : TFslGraphicAngle Read FTextRotation Write SetTextRotation;
      Property Weight : TFslFontWeight Read FWeight Write SetWeight; // bold := true => weight := fwBold
      Property WeightAsString : String Read GetWeightAsString Write SetWeightAsString;
      Property Bold : Boolean Read GetBold Write SetBold;
      Property Italic : Boolean Read FItalic Write SetItalic;
      Property Underline : Boolean Read FUnderline Write SetUnderline;
      Property StrikeOut : Boolean Read FStrikeOut Write SetStrikeOut;
  End;


Const
  ADVFONTWEIGHT_CODES : Array [TFslFontWeight] Of String =
    ('Unknown', 'Thin', 'ExtraLight', 'Light', 'Normal', 'Medium', 'SemiBold', 'Bold', 'ExtraBold', 'Heavy');

  ADVFONTWEIGHT_NAMES : Array [TFslFontWeight] Of String =
    ('Unknown', 'Thin', 'Extra-Light', 'Light', 'Normal', 'Medium', 'Semi-Bold', 'Bold', 'Extra-Bold', 'Heavy');

  ADVFONTWEIGHT_VALUES : Array [TFslFontWeight] Of Cardinal =
    (FW_DONTCARE, FW_THIN, FW_EXTRALIGHT, FW_LIGHT, FW_NORMAL, FW_MEDIUM, FW_SEMIBOLD, FW_BOLD, FW_EXTRABOLD, FW_HEAVY);

  ADVFONTFAMILY_CODES : Array [TFslFontFamily] Of String =
    ('Unknown', 'Decorative', 'Modern', 'Roman', 'Script', 'Swiss');

  ADVFONTFAMILY_VALUES : Array [TFslFontFamily] Of Cardinal =
    (FF_DECORATIVE, FF_DONTCARE, FF_MODERN, FF_ROMAN, FF_SCRIPT, FF_SWISS);

  ADVFONTPITCH_CODES : Array [TFslFontPitch] Of String =
    ('Unknown', 'Fixed', 'Variable');

  ADVFONTPITCH_VALUES : Array [TFslFontPitch] Of Cardinal =
    (DEFAULT_PITCH, FIXED_PITCH, VARIABLE_PITCH);

  ADVFONTPITCH_VCLMAP : Array [TFslFontPitch] Of TFontPitch =
    (fpDefault, fpVariable, fpFixed);


Type
  TFslIconGraphic = Class(TFslVCLGraphic)
    Private
      Function GetIcon : TIcon;
      Procedure SetIcon(Const Value : TIcon);

    Protected
      Function HandleClass : TGraphicClass; Override;

    Public
      Function Link : TFslIconGraphic;

      Procedure SetMaximumDimensions(Const iWidth, iHeight : Integer);
      Procedure LoadFromResource(Const sResourceName : String);

      Property Icon : TIcon Read GetIcon Write SetIcon;
  End;



type
  TFslJpegGraphic = Class(TFslVCLGraphic)
  Private
    Function GetHandle: TJpegImage;
    Procedure SetHandle(Const Value: TJpegImage);

    Function GetQuality: Integer;
    Procedure SetQuality(Const Value: Integer);

    Function GetGrayScale: Boolean;
    Procedure SetGrayScale(Const Value: Boolean);

  Protected
    Function HandleClass : TGraphicClass; Override;
    Function HandleNew : TGraphic; Override;

  Public
    Function Link : TFslJpegGraphic;
    Function Clone : TFslJpegGraphic;

    Function AsBitmap : TFslBitmapGraphic; Reintroduce;

    Procedure Compress;

    Property Handle : TJpegImage Read GetHandle Write SetHandle;
    Property Quality : Integer Read GetQuality Write SetQuality;
    Property GrayScale : Boolean Read GetGrayScale Write SetGrayScale;
  End;

  TJpegImage = {$IFDEF DELPHI}Jpeg.{$ELSE}Graphics.{$ENDIF} TJpegImage;


  type
    {$IFDEF FPC}
    TPNGGraphic = TPortableNetworkGraphic;
    TPngObject = TPortableNetworkGraphic;
    {$ENDIF}

    TFslPortableNetworkGraphic = Class(TFslVCLGraphic)
    Private
      Function GetHandle: TPNGGraphic;
      Procedure SetHandle(Const Value: TPNGGraphic);

    Protected
      Function HandleClass : TGraphicClass; Override;
      Function HandleNew : TGraphic; Override;

    Public
      Function Link : TFslPortableNetworkGraphic;
      Function Clone : TFslPortableNetworkGraphic;

      Procedure LoadFromResource(Const sResource : String);

      Property Handle : TPNGGraphic Read GetHandle Write SetHandle;

      Class Procedure SavePNGToStream(oImage : TFslVCLGraphic; oStream : TFslStream); Overload; Virtual;
    End;

    {$IFDEF DELPHI}
    TPNGGraphic = GraphicEx.TPNGGraphic;
    {$ENDIF}

implementation



Const
  WINDOWS_RESOURCE_TYPES : Array[TFslResourceType] Of PChar =
    (RT_ACCELERATOR, RT_ANICURSOR, RT_ANIICON, RT_BITMAP, RT_CURSOR, RT_DIALOG, RT_FONT,
     RT_FONTDIR, RT_GROUP_CURSOR, RT_GROUP_ICON, RT_ICON, RT_MENU, RT_MESSAGETABLE, RT_RCDATA, RT_STRING, RT_VERSION);


Constructor TFslResourceStream.Create;
Begin
  Inherited;

  FResourceType := rtRCData;
  FResourceHandle := 0;
End;


Procedure TFslResourceStream.BeforeDestruction;
Begin
  Close;

  Inherited;
End;


Function TFslResourceStream.Link: TFslResourceStream;
Begin
  Result := TFslResourceStream(Inherited Link);
End;


Function TFslResourceStream.ToWindowsResourceType(oResourceType : TFslResourceType) : PChar;
Begin
  Result := WINDOWS_RESOURCE_TYPES[oResourceType];
End;


Procedure TFslResourceStream.Open;
Var
  hModule : Cardinal;
  iResourceSize : Cardinal;
Begin
  iResourceSize := 0;
  FResourceData := Nil;

  hModule := LoadLibrary(PChar(FFilename));
  Try
    FResourceHandle := FindResource(hModule, PChar(FResourceName), ToWindowsResourceType(FResourceType));

    If FResourceHandle <> 0 Then
    Begin
      iResourceSize := SizeofResource(hModule, FResourceHandle);

      If iResourceSize <> 0 Then
      Begin
        FResourceHandle := LoadResource(hModule, FResourceHandle);

        If FResourceHandle <> 0 Then
          FResourceData := LockResource(FResourceHandle);
      End;
    End
    Else
    Begin
      RaiseError('Open', StringFormat('Resource not found "%s"', [FResourceName]));
    End;

    Size := iResourceSize;
    Capacity := iResourceSize;

    Move(FResourceData^, DataPointer^, iResourceSize);
  Finally
    FreeLibrary(hModule);
  End;
End;


Function TFslResourceStream.Active: Boolean;
Begin
  Result := FResourceHandle <> 0;
End;


Function TFslResourceStream.ExistsByResourceType(Const aResourceType : TFslResourceType) : Boolean;
Var
  hModule : Cardinal;
Begin
  hModule := LoadLibrary(PChar(FFileName));
  Try
    Result := FindResource(hModule, PChar(FResourceName), ToWindowsResourceType(aResourceType)) <> 0;
  Finally
    FreeLibrary(hModule);
  End;
End;


Function TFslResourceStream.ExistsAcceleratorTable : Boolean;
Begin
  Result := ExistsByResourceType(rtAccelerator);
End;


Function TFslResourceStream.ExistsAnimatedCursor : Boolean;
Begin
  Result := ExistsByResourceType(rtAniCursor);
End;


Function TFslResourceStream.ExistsAnimatedIcon : Boolean;
Begin
  Result := ExistsByResourceType(rtAniIcon);
End;


Function TFslResourceStream.ExistsBitmap : Boolean;
Begin
  Result := ExistsByResourceType(rtBitmap);
End;


Function TFslResourceStream.ExistsHardwareDependentCursor : Boolean;
Begin
  Result := ExistsByResourceType(rtCursor);
End;


Function TFslResourceStream.ExistsDialogBox : Boolean;
Begin
  Result := ExistsByResourceType(rtDialog);
End;


Function TFslResourceStream.ExistsFont : Boolean;
Begin
  Result := ExistsByResourceType(rtFont);
End;


Function TFslResourceStream.ExistsFontDirectory : Boolean;
Begin
  Result := ExistsByResourceType(rtFontDir);
End;


Function TFslResourceStream.ExistsHardwareIndependentCursor : Boolean;
Begin
  Result := ExistsByResourceType(rtGroupCursor);
End;


Function TFslResourceStream.ExistsHardwareIndependentIcon : Boolean;
Begin
  Result := ExistsByResourceType(rtIcon);
End;


Function TFslResourceStream.ExistsHardwareDependentIcon : Boolean;
Begin
  Result := ExistsByResourceType(rtGroupIcon);
End;


Function TFslResourceStream.ExistsMenu : Boolean;
Begin
  Result := ExistsByResourceType(rtMenu);
End;


Function TFslResourceStream.ExistsMessageTableEntry : Boolean;
Begin
  Result := ExistsByResourceType(rtMessageTable);
End;


Function TFslResourceStream.ExistsApplicationDefined : Boolean;
Begin
  Result := ExistsByResourceType(rtRCData);
End;


Function TFslResourceStream.ExistsStringTableEntry : Boolean;
Begin
  Result := ExistsByResourceType(rtString);
End;


Function TFslResourceStream.ExistsVersion : Boolean;
Begin
  Result := ExistsByResourceType(rtVersion);
End;


Procedure TFslResourceStream.ResourceTypeAcceleratorTable;
Begin
  FResourceType := rtAccelerator;
End;


Procedure TFslResourceStream.ResourceTypeAnimatedCursor;
Begin
  FResourceType := rtAniCursor;
End;


Procedure TFslResourceStream.ResourceTypeAnimatedIcon;
Begin
  FResourceType := rtAniIcon;
End;


Procedure TFslResourceStream.ResourceTypeBitmap;
Begin
  FResourceType := rtBitmap;
End;


Procedure TFslResourceStream.ResourceTypeHardwareDependentCursor;
Begin
  FResourceType := rtCursor;
End;


Procedure TFslResourceStream.ResourceTypeDialogBox;
Begin
  FResourceType := rtDialog;
End;


Procedure TFslResourceStream.ResourceTypeFont;
Begin
  FResourceType := rtFont;
End;


Procedure TFslResourceStream.ResourceTypeFontDirectory;
Begin
  FResourceType := rtFontDir;
End;


Procedure TFslResourceStream.ResourceTypeHardwareIndependentCursor;
Begin
  FResourceType := rtGroupCursor;
End;


Procedure TFslResourceStream.ResourceTypeHardwareIndependentIcon;
Begin
  FResourceType := rtGroupIcon;
End;


Procedure TFslResourceStream.ResourceTypeHardwareDependentIcon;
Begin
  FResourceType := rtIcon;
End;


Procedure TFslResourceStream.ResourceTypeMenu;
Begin
  FResourceType := rtMenu;
End;


Procedure TFslResourceStream.ResourceTypeMessageTableEntry;
Begin
  FResourceType := rtMessageTable;
End;


Procedure TFslResourceStream.ResourceTypeApplicationDefined;
Begin
  FResourceType := rtRCData;
End;


Procedure TFslResourceStream.ResourceTypeStringTableEntry;
Begin
  FResourceType := rtString;
End;


Procedure TFslResourceStream.ResourceTypeVersion;
Begin
  FResourceType := rtVersion;
End;


Procedure TFslResourceStream.Close;
Begin
  // API says no need to call 'FreeResource' in Windows 32 bit.

  FResourceHandle := 0;
End;


Function TFslResourceStream.IsResourceTypeAcceleratorTable : Boolean;
Begin
  Result := FResourceType = rtAccelerator;
End;


Function TFslResourceStream.IsResourceTypeAnimatedCursor : Boolean;
Begin
  Result := FResourceType = rtAniCursor;
End;


Function TFslResourceStream.IsResourceTypeAnimatedIcon : Boolean;
Begin
  Result := FResourceType = rtAniIcon;
End;


Function TFslResourceStream.IsResourceTypeBitmap : Boolean;
Begin
  Result := FResourceType = rtBitmap;
End;


Function TFslResourceStream.IsResourceTypeHardwareDependentCursor : Boolean;
Begin
  Result := FResourceType = rtCursor;
End;


Function TFslResourceStream.IsResourceTypeDialogBox : Boolean;
Begin
  Result := FResourceType = rtDialog;
End;


Function TFslResourceStream.IsResourceTypeFont : Boolean;
Begin
  Result := FResourceType = rtFont;
End;


Function TFslResourceStream.IsResourceTypeFontDirectory : Boolean;
Begin
  Result := FResourceType = rtFontDir;
End;


Function TFslResourceStream.IsResourceTypeHardwareIndependentCursor : Boolean;
Begin
  Result := FResourceType = rtGroupCursor;
End;


Function TFslResourceStream.IsResourceTypeHardwareIndependentIcon : Boolean;
Begin
  Result := FResourceType = rtIcon;
End;


Function TFslResourceStream.IsResourceTypeHardwareDependentIcon : Boolean;
Begin
  Result := FResourceType = rtGroupIcon;
End;


Function TFslResourceStream.IsResourceTypeMenu : Boolean;
Begin
  Result := FResourceType = rtMenu;
End;


Function TFslResourceStream.IsResourceTypeMessageTableEntry : Boolean;
Begin
  Result := FResourceType = rtMessageTable;
End;


Function TFslResourceStream.IsResourceTypeApplicationDefined : Boolean;
Begin
  Result := FResourceType = rtRCData;
End;


Function TFslResourceStream.IsResourceTypeStringTableEntry : Boolean;
Begin
  Result := FResourceType = rtString;
End;


Function TFslResourceStream.IsResourceTypeVersion : Boolean;
Begin
  Result := FResourceType = rtVersion;
End;



function TFslResourceStream.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, (FResourceName.length * sizeof(char)) + 12);
end;

function TFslGraphic.AsBitmap: TFslVCLGraphic;
begin
  result := nil;
  Invariant('AsBitmap', 'Need to override ' + ClassName + '.AsBitmap');
end;

Function TFslGraphic.Clone: TFslGraphic;
Begin
  Result := TFslGraphic(Inherited Clone);
End;


procedure TFslGraphic.DrawToStream(oStream: TFslStream; width, height : Integer);
begin
  Invariant('LoadFromFile', 'Need to override ' + ClassName + '.DrawToStream');
end;

Function TFslGraphic.Link: TFslGraphic;
Begin
  Result := TFslGraphic(Inherited Link);
End;


Procedure TFslGraphic.LoadFromStream(oStream : TFslStream);
Begin
  Invariant('LoadFromStream', 'Need to override ' + ClassName + '.LoadFromStream');
End;


Procedure TFslGraphic.SaveToStream(oStream : TFslStream);
Begin
  Invariant('SaveToStream', 'Need to override ' + ClassName + '.SaveToStream');
End;


Procedure TFslGraphic.LoadFromFile(Const sFilename : String);
Begin
  Invariant('LoadFromFile', 'Need to override ' + ClassName + '.LoadFromFile');
End;


Procedure TFslGraphic.SaveToFile(Const sFilename : String);
Begin
  Invariant('SaveToFile', 'Need to override ' + ClassName + '.SaveToFile');
End;


function TFslGraphic.Extension: String;
begin
  result := '???';
end;

function TFslGraphic.FrameCount: Integer;
begin
  result := 1;
end;

function TFslGraphic.GetFrameIndex: Integer;
begin
  result := 0;
end;

Function TFslGraphic.GetHeight: Integer;
Begin
  Invariant('GetHeight', 'Need to override ' + ClassName + '.GetHeight');

  Result := 0;
End;


Function TFslGraphic.GetWidth: Integer;
Begin
  Invariant('GetWidth', 'Need to override ' + ClassName + '.GetWidth');

  Result := 0;
End;


procedure TFslGraphic.SetFrameIndex(const Value: Integer);
begin
  if Value >= FrameCount then
    RaiseError('SetFrameIndex', 'The maximum Frame Index value for this image is '+inttostr(FrameCount-1));
end;

Procedure TFslGraphic.SetHeight(Const Value: Integer);
Begin
  Invariant('SetHeight', 'Need to override ' + ClassName + '.SetHeight');
End;


Procedure TFslGraphic.SetWidth(Const Value: Integer);
Begin
  Invariant('SetWidth', 'Need to override ' + ClassName + '.SetWidth');
End;


procedure TFslGraphic.StretchDraw(oCanvas: TCanvas; aRect: TRect);
begin
  Invariant('TFslGraphic', 'Need to override ' + ClassName + '.StretchDraw');
end;

Function TFslGraphicList.GetGraphic(iIndex: Integer): TFslGraphic;
Begin
  Result := TFslGraphic(ObjectByIndex[iIndex]);
End;


Function TFslGraphicList.ItemClass: TFslObjectClass;
Begin
  Result := TFslGraphic;
End;


Constructor TFslVCLGraphic.Create;
Begin
  Inherited;

  FHandle := HandleNew;
  FBuffer := TFslBuffer.Create;
End;


Destructor TFslVCLGraphic.Destroy;
Begin
  FHandle.Free;
  FBuffer.Free;

  Inherited;
End;


procedure TFslVCLGraphic.DrawToStream(oStream: TFslStream; width, height : Integer);
begin
  SaveToStream(oStream);
end;

Procedure TFslVCLGraphic.LoadFromStream(oStream: TFslStream);
Var
  oAdapter : TVCLStream;
Begin
  oAdapter := TVCLStream.Create;
  Try
    oAdapter.Stream := oStream.Link;

    Handle.LoadFromStream(oAdapter);
  Finally
    oAdapter.Free;
  End;
End;


Procedure TFslVCLGraphic.SaveToStream(oStream: TFslStream);
Var
  oAdapter : TVCLStream;
Begin
  oAdapter := TVCLStream.Create;
  Try
    oAdapter.Stream := oStream.Link;

    Handle.SaveToStream(oAdapter);
  Finally
    oAdapter.Free;
  End;
End;


Function TFslVCLGraphic.HandleClass: TGraphicClass;
Begin
  Result := Nil;
End;


Function TFslVCLGraphic.HandleNew: TGraphic;
Var
  aClass : TGraphicClass;
Begin
  aClass := HandleClass;

  If Assigned(aClass) Then
  Begin
    Assert(Invariants('HandleNew', aClass, TGraphic, 'aClass'));

    Result := aClass.Create;
  End
  Else
  Begin
    Result := Nil;
  End;
End;


Function TFslVCLGraphic.GetHandle: TGraphic;
Begin
  Assert(CheckCondition(HasHandle, 'GetHandle', 'Handle must be assigned.'));

  Result := FHandle;
End;


Procedure TFslVCLGraphic.SetHandle(Const Value: TGraphic);
Begin
  FHandle.Free;
  FHandle := Value;
End;


Function TFslVCLGraphic.Clone: TFslVCLGraphic;
Begin
  Result := TFslVCLGraphic(Inherited Clone);
End;


Function TFslVCLGraphic.Link: TFslVCLGraphic;
Begin
  Result := TFslVCLGraphic(Inherited Link);
End;


Function TFslVCLGraphic.GetWidth: Integer;
Begin
  Result := Handle.Width;
End;


Procedure TFslVCLGraphic.SetWidth(Const Value: Integer);
Begin
  Handle.Width := Value;
End;


procedure TFslVCLGraphic.StretchDraw(oCanvas: TCanvas; aRect: TRect);
begin
  oCanvas.StretchDraw(aRect, Handle)
end;

Function TFslVCLGraphic.GetHeight: Integer;
Begin
  Result := Handle.Height;
End;


Procedure TFslVCLGraphic.SetHeight(Const Value: Integer);
Begin
  Handle.Height := Value;
End;


Function TFslVCLGraphic.Empty: Boolean;
Begin
  Result := Handle.Empty;
End;


Procedure TFslVCLGraphic.LoadFromFile(Const sFilename: String);
Begin
  Handle.LoadFromFile(sFilename);
End;


Procedure TFslVCLGraphic.LoadFromGraphic(const oGraphic: TGraphic);
Var
  oStream : TMemoryStream;
Begin
  oStream := TMemoryStream.Create;
  Try
    oGraphic.SaveToStream(oStream);
    oStream.Position := 0;
    Handle.LoadFromStream(oStream);
  Finally
    oStream.Free;
  End;
End;


Procedure TFslVCLGraphic.SaveToFile(Const sFilename: String);
Begin
  Handle.SaveToFile(sFilename);
End;


Procedure TFslVCLGraphic.Assign(oObject: TFslObject);
Begin
  Inherited;

  Handle.Assign(TFslVCLGraphic(oObject).Handle);
End;


Function TFslVCLGraphic.HasHandle: Boolean;
Begin
  Result := Assigned(FHandle);
End;


function TFslVCLGraphic.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBuffer.sizeInBytes);
end;

Function TFslVCLGraphicList.GetGraphic(iIndex: Integer): TFslVCLGraphic;
Begin
  Result := TFslVCLGraphic(ObjectByIndex[iIndex]);
End;


Function TFslVCLGraphicList.ItemClass: TFslObjectClass;
Begin
  Result := TFslVCLGraphic;
End;


Function TFslVCLGraphic.GetTransparent: Boolean;
Begin
  Result := Handle.Transparent;
End;


Procedure TFslVCLGraphic.SetTransparent(Const Value: Boolean);
Begin
  Handle.Transparent := True;
End;


Function TFslGraphic.TypeName: String;
Begin
  Result := StringExcludeBefore(ClassName, 'TFsl');
End;





Const
  PIXELMAX = 32768;


Type
  TRGBTripleArray = Array[0..PIXELMAX-1] Of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;


Function TFslBitmapGraphic.Link: TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic(Inherited Link);
End;


Function TFslBitmapGraphic.Clone: TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic(Inherited Clone);
End;


Function TFslBitmapGraphic.HandleClass : TGraphicClass;
Begin
  Result := Graphics.TBitmap;
End;


Function TFslBitmapGraphic.HandleNew: TGraphic;
Begin
  Result := Graphics.TBitmap.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslBitmapGraphic.GetHandle: TBitmap;
Begin
  Result := TBitmap(Inherited Handle);
End;


Procedure TFslBitmapGraphic.SetHandle(Const Value: TBitmap);
Begin
  Inherited Handle := Value;
End;


Function TFslBitmapGraphic.BytesPerLine : Integer;
Begin
  Case Handle.PixelFormat Of
    pf1bit   : Result := RealCeiling(Width / 8);
    pf4bit   : Result := RealCeiling(Width / 2);
    pf8bit   : Result := Width;
    pf15bit  : Result := Width * 2; // TODO: what's the calculation for 15 bit?
    pf16bit  : Result := Width * 2;
    pf24bit  : Result := Width * 3;
    pf32bit  : Result := Width * 4;
  Else
    RaiseError('BytesPerLine', 'PixelFormat not supported.');

    Result := 0;
  End;
End;


Procedure TFslBitmapGraphic.LoadFromResource(Const sResource: String);
Begin
  Handle.LoadFromResourceName(HInstance, sResource);
End;


Function TFslBitmapGraphic.ConstructRotate(Const AngleOfRotation : Double) : TBitmap;
Var
  cosTheta   : Extended;
  i          : Integer;
  iOriginal  : Integer;
  iPrime     : Integer;
  j          : Integer;
  jOriginal  : Integer;
  jPrime     : Integer;
  RowOriginal: pRGBTripleArray;
  RowRotated : pRGBTRipleArray;
  sinTheta   : Extended;
  iRotationAxis : Integer;
  jRotationAxis : Integer;
  Radians : Double;
Begin
  Result := TBitmap.Create;
  Try
    Result.Canvas.Lock;
    Handle.Canvas.Lock;
    Try
      iRotationAxis := Width Div 2;
      jRotationAxis := Height Div 2;

      If (AngleOfRotation = 90) Or (AngleOfRotation = 270) Then
      Begin
        Result.Width  := Handle.Height;
        Result.Height := Handle.Width;
        iRotationAxis := Height Div 2;
      End
      Else
      Begin
        Result.Width  := Handle.Width;
        Result.Height := Handle.Height;
      End;

      Handle.PixelFormat := pf24bit;
      Result.PixelFormat := pf24bit; // Force this

      // If no math library, then use this:
      Radians := -(AngleOfRotation) * PI / 180;

      sinTheta := SIN(Radians);//AngleOfRotation);
      cosTheta := COS(Radians);//AngleOfRotation);

      // Step through each row of rotated image.
      For j := Result.Height - 1 DownTo 0 Do
      Begin
        RowRotated := Result.Scanline[j];
        jPrime := j - jRotationAxis;

        For i := Result.Width - 1 DownTo 0 Do
        Begin
          iPrime := i - iRotationAxis;
          iOriginal := iRotationAxis + ROUND(iPrime * CosTheta - jPrime * sinTheta);
          jOriginal := jRotationAxis + ROUND(iPrime * sinTheta + jPrime * cosTheta);

          // Make sure (iOriginal, jOriginal) is in BitmapOriginal. If not,
          // assign blue color to corner points.
          If (iOriginal >= 0) And (iOriginal <= Handle.Width-1) And (jOriginal >= 0) And (jOriginal <= Handle.Height-1) Then
          Begin
            // Assign pixel from rotated space to current pixel in BitmapRotated
            RowOriginal := Handle.Scanline[jOriginal];
            RowRotated[i] := RowOriginal[iOriginal]
          End
          Else
          Begin
            RowRotated[i].rgbtBlue := 255; // assign "corner" color
            RowRotated[i].rgbtGreen := 0;
            RowRotated[i].rgbtRed := 0
          End;
        End;
      End;
    Finally
      Handle.Canvas.Unlock;
      Result.Canvas.Unlock;
    End;
  Except
    Result.Free;

    Raise;
  End;
End;


Function TFslBitmapGraphic.ConstructFitToPage(Const iWidth, iHeight : Integer) : TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic.Create;
  Try
    If Self.Width > Self.Height Then
    Begin
      Result.Width := iWidth;

      If Self.Height > 0 Then
        Result.Height := Round(iHeight * (Self.Height / Self.Width))
      Else
        Result.Height := iHeight;
    End
    Else If Self.Width < Self.Height Then
    Begin
      Result.Height := iHeight;

      If Self.Width > 0 Then
        Result.Width := Round(iWidth * (Self.Width / Self.Height))
      Else
        Result.Width := iWidth;
    End
    Else
    Begin
      Result.Width := iWidth;
      Result.Height := iHeight;
    End;

    Result.Handle.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), Self.Handle);

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TFslBitmapGraphicList.GetBitmapGraphicByIndex(iIndex: Integer): TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic(ObjectByIndex[iIndex]);
End;


Function TFslBitmapGraphicList.ItemClass: TFslObjectClass;
Begin
  Result := TFslBitmapGraphic;
End;


Class Function TFslBitmapGraphic.CanLoad(oStream: TStream): Boolean;
Var
  aBitmapFileHeader : TBitmapFileHeader;
  iLastPosition : Integer;
Begin
  Result := (oStream.Size - oStream.Position) > SizeOf(aBitmapFileHeader);

  If Result Then
  Begin
    iLastPosition := oStream.Position;

    oStream.ReadBuffer(aBitmapFileHeader, SizeOf(aBitmapFileHeader));

    Result := aBitmapFileHeader.bfType = $4D42;

    oStream.Position := iLastPosition;
  End;
End;






Function TFslPen.Link: TFslPen;
Begin
  Result := TFslPen(Inherited Link);
End;


Function TFslPen.Clone: TFslPen;
Begin
  Result := TFslPen(Inherited Clone);
End;


Procedure TFslPen.Assign(oObject: TFslObject);
Begin
  Inherited;

  // using properties is deliberate
  Width := TFslPen(oObject).FWidth;
  Colour := TFslPen(oObject).FColour;
  EndStyle := TFslPen(oObject).FEndStyle;
  JoinStyle := TFslPen(oObject).FJoinStyle;
  Style := TFslPen(oObject).FStyle;
End;



Procedure TFslPen.Clear;
Begin
  Inherited;

  FWidth := 0;
  FColour := clBlack;
  FEndStyle := apesSquare;
  FJoinStyle := apjsMitre;
  FStyle := apsNone;
End;


Function TFslPen.CreateHandle: TFslGraphicHandle;
Var
  aBrush : TLogBrush;
Begin
  FillChar(aBrush, SizeOf(TLogBrush), 0);
  aBrush.lbStyle := BS_SOLID;
  aBrush.lbColor := ColorToRGB(FColour);
  aBrush.lbHatch := 0;

  Result := ExtCreatePen(
     {PenStyle} PS_GEOMETRIC + ADVPENSTYLE_VALUES[FStyle] +ADVPENENDSTYLE_VALUES[FEndStyle] +ADVPENJOINSTYLE_VALUES[FJoinStyle],
     {Width }   IntegerMax(1, Capability.ToPixel(FWidth)), // pen width can never be less than 1
     {Brush} aBrush, 0, Nil);

  If Result = 0 Then
    RaiseError('CreateHandle', ErrorAsString);
End;


Procedure TFslPen.SetColour(Const Value: TFslGraphicColour);
Begin
  If FColour <> Value Then
  Begin
    ClearHandle;
    Change;
    FColour := Value;
  End;
End;


Procedure TFslPen.SetEndStyle(Const Value: TFslPenEndStyle);
Begin
  If FEndStyle <> Value Then
  Begin
    ClearHandle;
    Change;
    FEndStyle := Value;
  End;
End;


Procedure TFslPen.SetJoinStyle(Const Value: TFslPenJoinStyle);
Begin
  If FJoinStyle <> Value Then
  Begin
    ClearHandle;
    Change;
    FJoinStyle := Value;
  End;
End;


Procedure TFslPen.SetStyle(Const Value: TFslPenStyle);
Begin
  If FStyle <> Value Then
  Begin
    ClearHandle;
    Change;
    FStyle := Value;
  End;
End;


Procedure TFslPen.SetWidth(Const Value: Integer);
Begin
  If FWidth <> Value Then
  Begin
    ClearHandle;
    Change;
    FWidth := Value;
  End;
End;


Procedure TFslPen.AfterConstruction;
Begin
  Inherited;

  Clear;
End;


Procedure TFslPen.SetStyleNone;
Begin
  Style := apsNone;
End;


Procedure TFslPen.SetStyleSolid;
Begin
  Style := apsSolid;
End;


Procedure TFslPen.SetStyleDot;
Begin
  Style := apsDot;
End;


Procedure TFslPen.SetStyleDash;
Begin
  Style := apsDash;
End;


Function TFslPen.GetStyleAsString: String;
Begin
  Result := ADVPENSTYLE_NAMES[Style];
End;


Procedure TFslPen.SetStyleAsString(Const Value: String);
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(ADVPENSTYLE_NAMES, Value);

  If iIndex < 0 Then
    RaiseError('SetStyleAsString', StringFormat('Pen Style ''%s'' not recognised.', [Value]));

  Style := TFslPenStyle(iIndex);
End;


Destructor TFslGraphicObject.Destroy;
Begin
  ClearHandle;

  Inherited;
End;


Procedure TFslGraphicObject.ClearHandle;
Begin
  If FHandle <> 0 Then
    DeleteObject(FHandle);

  FHandle := 0;
End;


Procedure TFslGraphicObject.Change;
Begin
  if assigned(FOnChange) then
    FOnChange(self);
End;


Function TFslGraphicObject.CreateHandle : TFslGraphicHandle;
Begin
  RaiseError('CreateHandle', 'Need to override in '+ClassName);

  Result := 0;
End;


Function TFslGraphicObject.GetHandle: TFslGraphicHandle;
Begin
  If FHandle = 0 Then
    FHandle := CreateHandle;

  Result := FHandle;
End;



Procedure TFslGraphicObject.Clear;
Begin
  ClearHandle;
  Change;
End;


Procedure TFslGraphicObject.AfterConstruction;
Begin
  Inherited;

  Clear;
End;



function TFslGraphicObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCapability.sizeInBytes);
end;

Function TFslGraphicCapability.FromPixel(iValue: Integer): TFslGraphicMetre;
Begin
  Result := Round(iValue / FPixelsPerGraphicMetre);
End;


Function TFslGraphicCapability.ToPixel(iValue: TFslGraphicMetre): Integer;
Begin
  Result := Round(iValue * FPixelsPerGraphicMetre);
End;


Constructor TFslBrush.Create;
Begin
  Inherited;

  FBitmap := Nil;
End;


Destructor TFslBrush.Destroy;
Begin
  FBitmap.Free;

  Inherited;
End;


Procedure TFslBrush.Clear;
Begin
  Inherited;

  Bitmap := Nil;
  FColour := clWhite;
  FStyle := absNull;
End;


Type
  TLogBrush = tagLOGBRUSH;

Function TFslBrush.CreateHandle: TFslGraphicHandle;
Var
  aBrush : TLogBrush;
Begin
  FillChar(aBrush, SizeOf(TLogBrush), 0);

  If Assigned(FBitmap) Then
  Begin
    aBrush.lbStyle := BS_PATTERN;
    aBrush.lbHatch := FBitmap.Handle.Handle;
  End
  Else
  Begin
    aBrush.lbStyle := ADVBRUSHSTYLE_VALUES[FStyle];
    aBrush.lbColor := ColorToRGB(FColour);
    aBrush.lbHatch := ADVBRUSHSTYLE_HASHVALUES[FStyle];
  End;

  Result := CreateBrushIndirect(aBrush);
  If Result = 0 Then
    RaiseError('CreateHandle', ErrorAsString);
End;


Function TFslBrush.Clone: TFslBrush;
Begin
  Result := TFslBrush(Inherited Clone);
End;


Function TFslBrush.Link: TFslBrush;
Begin
  Result := TFslBrush(Inherited Link);
End;


Procedure TFslBrush.Assign(oObject: TFslObject);
Begin
  Inherited;

  // using properties is deliberate
  Bitmap := TFslBrush(oObject).FBitmap.Link;
  FColour := TFslBrush(oObject).FColour;
  FStyle := TFslBrush(oObject).FStyle;
End;



Function TFslBrush.GetBitmap: TFslBitmapGraphic;
Begin
  Assert(Invariants('GetBitmap', FBitmap, TFslBitmapGraphic, 'FBitmap'));

  Result := FBitmap;
End;


Procedure TFslBrush.SetBitmap(Const Value: TFslBitmapGraphic);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetBitmap', Value, TFslBitmapGraphic, 'Value'));

  ClearHandle;
  Change;

  FBitmap.Free;
  FBitmap := Value;
End;


Procedure TFslBrush.SetColour(Const Value: TColour);
Begin
  If FColour <> Value Then
  Begin
    ClearHandle;
    Change;
    FColour := Value;
  End;
End;


Procedure TFslBrush.SetStyle(Const Value: TFslBrushStyle);
Begin
  If FStyle <> Value Then
  Begin
    ClearHandle;
    Change;
    FStyle := Value;
  End;
End;


Procedure TFslBrush.SetStyleClear;
Begin
  Style := absNull;
End;


Procedure TFslBrush.SetStyleSolid;
Begin
  Style := absSolid;
End;


Function TFslBrush.GetStyleAsString: String;
Begin
  Result := ADVBRUSHSTYLE_NAMES[Style];
End;


Procedure TFslBrush.SetStyleAsString(Const Value: String);
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(ADVBRUSHSTYLE_NAMES, Value);

  If iIndex < 0 Then
    RaiseError('SetStyleAsString', StringFormat('Brush Style ''%s'' not recognised.', [Value]));

  Style := TFslBrushStyle(iIndex);
End;


Function TFslBrush.HasBitmap: Boolean;
Begin
  Result := Assigned(FBitmap);
End;



function TFslBrush.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBitmap.sizeInBytes);
end;

Constructor TFslFont.Create;
Begin
  Inherited;

  Clear;
End;


Function TFslFont.Clone: TFslFont;
Begin
  Result := TFslFont(Inherited Clone);
End;


Function TFslFont.Link: TFslFont;
Begin
  Result := TFslFont(Inherited Link);
End;


Procedure TFslFont.Assign(oObject: TFslObject);
Begin
  Inherited;

  // using properties is deliberate
  Italic := TFslFont(oObject).FItalic;
  Underline := TFslFont(oObject).FUnderline;
  StrikeOut := TFslFont(oObject).FStrikeOut;
  Name := TFslFont(oObject).FName;
  CharRotation := TFslFont(oObject).FCharRotation;
  TextRotation := TFslFont(oObject).FTextRotation;
  Colour := TFslFont(oObject).FColour;
  Family := TFslFont(oObject).FFamily;
  Pitch := TFslFont(oObject).FPitch;
  Weight := TFslFont(oObject).FWeight;
  Size := TFslFont(oObject).FSize;
End;



Procedure TFslFont.SetColour(Const Value: TFslGraphicColour);
Begin
  If FColour <> Value Then
  Begin
    ClearHandle;
    Change;
    FColour := Value;
  End;
End;


Procedure TFslFont.SetFamily(Const Value: TFslFontFamily);
Begin
  If FFamily <> Value Then
  Begin
    ClearHandle;
    Change;
    FFamily := Value;
  End;
End;


Procedure TFslFont.SetItalic(Const Value: Boolean);
Begin
  If FItalic <> Value Then
  Begin
    ClearHandle;
    Change;
    FItalic := Value;
  End;
End;


Procedure TFslFont.SetName(Const Value: String);
Begin
  If FName <> Value Then
  Begin
    ClearHandle;
    Change;
    FName := Value;
  End;
End;


Procedure TFslFont.SetPitch(Const Value: TFslFontPitch);
Begin
  If FPitch <> Value Then
  Begin
    ClearHandle;
    Change;
    FPitch := Value;
  End;
End;


Procedure TFslFont.SetCharRotation(Const Value: TFslGraphicAngle);
Begin
  If FCharRotation <> Value Then
  Begin
    ClearHandle;
    Change;
    FCharRotation := Value;
  End;
End;


Procedure TFslFont.SetSize(Const Value: Word);
Begin
  If FSize <> Value Then
  Begin
    ClearHandle;
    Change;
    FSize := Value;
  End;
End;


Procedure TFslFont.SetStrikeOut(Const Value: Boolean);
Begin
  If FStrikeOut <> Value Then
  Begin
    ClearHandle;
    Change;
    FStrikeOut := Value;
  End;
End;


Procedure TFslFont.SetUnderline(Const Value: Boolean);
Begin
  If FUnderline <> Value Then
  Begin
    ClearHandle;
    Change;
    FUnderline := Value;
  End;
End;


Procedure TFslFont.SetWeight(Const Value: TFslFontWeight);
Begin
  If FWeight <> Value Then
  Begin
    ClearHandle;
    Change;
    FWeight := Value;
  End;
End;


Procedure TFslFont.SetTextRotation(Const Value: TFslGraphicAngle);
Begin
  If FTextRotation <> Value Then
  Begin
    ClearHandle;
    Change;
    FTextRotation := Value;
  End;
End;


Procedure TFslFont.ClearStyles;
Begin
  Weight := afwNormal;
  Italic := False;
  Underline := False;
  StrikeOut := False;
End;


Procedure TFslFont.Clear;
Begin
  Inherited;

  FName := '';

  FFamily := affDontCare;
  FPitch := afpDontCare;

  FCharRotation := 0;
  FTextRotation := 0;
  FColour := clBlack;
  FWeight := afwNormal;
  FSize := 10;

  FItalic := False;
  FUnderline := False;
  FStrikeOut := False;
End;


Function TFslFont.CreateHandle: TFslGraphicHandle;
Begin
  Result := MakeHandle(Capability.PixelsPerInchY);
End;


Function TFslFont.MakeHandle(iPixelsPerInchY : Integer): TFslGraphicHandle;
Var
  aFont : TLogFont;
Begin
  FillChar(aFont, SizeOf(TLogFont), 0);

  aFont.lfHeight := -MulDiv(FSize, iPixelsPerInchY, 72);

  aFont.lfWidth := 0; // leave to font mapper
  aFont.lfEscapement := FTextRotation * 10;
  aFont.lfOrientation := (FTextRotation + FCharRotation) * 10;
  aFont.lfWeight := ADVFONTWEIGHT_VALUES[FWeight];
  aFont.lfItalic := Byte(FItalic);
  aFont.lfUnderline := Byte(FUnderline);
  aFont.lfStrikeOut := Byte(FStrikeOut);
  aFont.lfCharSet := DEFAULT_CHARSET;
  aFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  aFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  aFont.lfQuality := DEFAULT_QUALITY;
  aFont.lfPitchAndFamily := ADVFONTPITCH_VALUES[FPitch] + ADVFONTFAMILY_VALUES[FFamily];

  If FName <> '' Then
    Move(FName[1], aFont.lfFaceName, IntegerMin(LF_FACESIZE, Length(FName)));

  Result := CreateFontIndirect(aFont);

  If Result = 0 Then
    RaiseError('CreateHandle', ErrorAsString);
End;


Procedure TFslFont.SetVCLFont(oFont: TFont);
Var
  aStyle : TFontStyles;
Begin
  oFont.Color := FColour;
  oFont.Name := FName;
  oFont.Pitch := ADVFONTPITCH_VCLMAP[FPitch];
  oFont.Size := FSize;

  aStyle := [];

  If Weight >= afwSemiBold Then
    Include(aStyle, fsBold);

  If Italic Then
    Include(aStyle, fsItalic);

  If Underline Then
    Include(aStyle, fsUnderline);

  If StrikeOut Then
    Include(aStyle, fsStrikeOut);

  oFont.Style := aStyle;
End;


Function TFslFont.GetBold: Boolean;
Begin
  Result := Weight >= afwSemiBold;
End;


Procedure TFslFont.SetBold(Const Value: Boolean);
Begin
  If Value Then
    Weight := afwBold
  Else
    Weight := afwNormal;
End;


Function TFslFont.GetWeightAsString: String;
Begin
  Result := ADVFONTWEIGHT_NAMES[Weight];
End;


Procedure TFslFont.SetWeightAsString(Const Value: String);
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(ADVFONTWEIGHT_NAMES, Value);

  If iIndex < 0 Then
    RaiseError('SetWeightAsString', StringFormat('Font Weight ''%s'' not recognised.', [Value]));

  Weight := TFslFontWeight(iIndex);
End;





Function Rect(iLeft, iTop, iRight, iBottom : Integer) : TRect;
Begin
  Result.Left := iLeft;
  Result.Top := iTop;
  Result.Right := iRight;
  Result.Bottom := iBottom;
End;


Procedure RectZero(Var aRect : TRect);
Begin
  SetRectEmpty(Windows.TRect(aRect));
End;


Function RectZero : TRect;
Begin
  RectZero(Result);
End;


Function RectEmpty(Const aRect : TRect) : Boolean;
Begin
  Result := Windows.IsRectEmpty(Windows.TRect(aRect));
End;


Function RectEqual(Const A, B : TRect) : Boolean;
Begin
  Result := Windows.EqualRect(Windows.TRect(A), Windows.TRect(B));
End;


Function RectOffset(Const aRect : TRect; iX, iY : Integer) : TRect;
Begin
  Result := aRect;
  OffsetRect(Windows.TRect(Result), iX, iY);
End;


Function RectIntersect(Const A, B : TRect) : TRect;
Begin
  Windows.IntersectRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;


Function RectSubtract(Const A, B : TRect) : TRect;
Begin
  Windows.SubtractRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;

Function RectUnion(Const A, B : TRect) : TRect;
Begin
  Windows.UnionRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;



Function RectHasIntersection(Const A, B : TRect) : Boolean;
Var
  aTemp : Windows.TRect;
Begin
  Result := Windows.IntersectRect(aTemp, Windows.TRect(A), Windows.TRect(B));
End;


Function RectInflate(Const aRect : TRect; iValue : Integer) : TRect;
Begin
  Result := RectInflate(aRect, iValue, iValue);
End;


Function RectInflate(Const aRect : TRect; iX, iY : Integer) : TRect;
Begin
  Result := aRect;
  Windows.InflateRect(Windows.TRect(Result), iX, iY);
End;


Function RectWidth(Const aRect : TRect) : Integer;
Begin
  Result := aRect.Right - aRect.Left;
End;


Function RectHeight(Const aRect : TRect) : Integer;
Begin
  Result := aRect.Bottom - aRect.Top;
End;


Function RectHit(Const aRect : TRect; Const aPoint : TPoint) : Boolean;
Begin
  Result := Windows.PtInRect(Windows.TRect(aRect), Windows.TPoint(aPoint));
End;


Function RectBound(Const aRect, aBoundary : TRect) : TRect;
Begin
  Result.Left := IntegerMax(aRect.Left, aBoundary.Left);
  Result.Top := IntegerMax(aRect.Top, aBoundary.Top);
  Result.Right := IntegerMin(aRect.Right, aBoundary.Right);
  Result.Bottom := IntegerMin(aRect.Bottom, aBoundary.Bottom);
End;



Type
  TFslIcon = Class(TIcon)
    Private
      FMaximumHeight : Integer;
      FMaximumWidth : Integer;

    Protected
      Function GetHeight : Integer; Override;
      Procedure SetHeight(Value : Integer); Override;

      Function GetWidth : Integer; Override;
      Procedure SetWidth(Value : Integer); Override;
  End;


function TFslFont.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Function TFslIcon.GetHeight : Integer;
Begin
  Result := FMaximumHeight;

  If Result = 0 Then
    Result := Inherited GetHeight;
End;


Procedure TFslIcon.SetHeight(Value : Integer);
Begin
  FMaximumHeight := Value;

  Inherited;
End;


Function TFslIcon.GetWidth : Integer;
Begin
  Result := FMaximumWidth;

  If Result = 0 Then
    Result := Inherited GetWidth;
End;


Procedure TFslIcon.SetWidth(Value : Integer);
Begin
  FMaximumWidth := Value;

  Inherited;
End;


Function TFslIconGraphic.HandleClass : TGraphicClass;
Begin
  Result := TFslIcon;
End;


Function TFslIconGraphic.Link : TFslIconGraphic;
Begin
  Result := TFslIconGraphic(Inherited Link);
End;


Procedure TFslIconGraphic.SetMaximumDimensions(Const iWidth, iHeight : Integer);
Begin
  // Cannot set width and height with an active icon loaded.
  Icon.Handle := 0;

  Icon.Width := iWidth;
  Icon.Height := iHeight;
End;


Procedure TFslIconGraphic.LoadFromResource(Const sResourceName : String);
Begin
  Icon.Handle := LoadImage(HInstance, PChar(sResourceName), IMAGE_ICON, Icon.Width, Icon.Height, LR_MONOCHROME);
End;


Function TFslIconGraphic.GetIcon : TIcon;
Begin
  Result := TIcon(Inherited Handle);
End;


Procedure TFslIconGraphic.SetIcon(Const Value : TIcon);
Begin
  Inherited Handle := Value;
End;


Function TFslJpegGraphic.HandleClass : TGraphicClass;
Begin
  Result := TJpegImage;
End;


Function TFslJpegGraphic.HandleNew: TGraphic;
Begin
  Result := TJpegImage.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslJpegGraphic.GetHandle: TJpegImage;
Begin
  Result := TJpegImage(Inherited Handle);
End;


Procedure TFslJpegGraphic.SetHandle(Const Value: TJpegImage);
Begin
  Inherited Handle := Value;
End;


Function TFslJpegGraphic.AsBitmap: TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic.Create;
  Result.Handle.Assign(Handle);
End;


Function TFslJpegGraphic.GetQuality: Integer;
Begin
  Result := Handle.CompressionQuality;
End;


Procedure TFslJpegGraphic.SetQuality(Const Value: Integer);
Begin
  Handle.CompressionQuality := Value;
End;


Function TFslJpegGraphic.GetGrayScale: Boolean;
Begin
  Result := Handle.GrayScale;
End;


Procedure TFslJpegGraphic.SetGrayScale(Const Value: Boolean);
Begin
  Handle.GrayScale := Value;
End;


Procedure TFslJpegGraphic.Compress;
Begin
  Handle.Compress;
End;


Function TFslJpegGraphic.Clone: TFslJpegGraphic;
Begin
  Result := TFslJpegGraphic(Inherited Clone);
End;


Function TFslJpegGraphic.Link: TFslJpegGraphic;
Begin
  Result := TFslJpegGraphic(Inherited Link);
End;



Function TFslPortableNetworkGraphic.Link: TFslPortableNetworkGraphic;
Begin
  Result := TFslPortableNetworkGraphic(Inherited Link);
End;


Function TFslPortableNetworkGraphic.Clone: TFslPortableNetworkGraphic;
Begin
  Result := TFslPortableNetworkGraphic(Inherited Clone);
End;


Function TFslPortableNetworkGraphic.HandleClass : TGraphicClass;
Begin
  Result := TPngObject;
End;


Function TFslPortableNetworkGraphic.HandleNew: TGraphic;
Begin
  Result := TPngObject.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslPortableNetworkGraphic.GetHandle: TPNGGraphic;
Begin
  Result := TPNGGraphic(Inherited Handle);
End;


Procedure TFslPortableNetworkGraphic.SetHandle(Const Value: TPNGGraphic);
Begin
  Inherited Handle := Value;
End;


Procedure TFslPortableNetworkGraphic.LoadFromResource(Const sResource: String);
Var
  oResourceStream : TFslResourceStream;
  oVCLStream : TFslVCLStream;
Begin
  oResourceStream := TFslResourceStream.Create;
  oVCLStream := TFslVCLStream.Create;
  Try
    oResourceStream.ResourceTypeApplicationDefined;
    oResourceStream.Filename := ProcessName;
    oResourceStream.ResourceName := sResource;
    oResourceStream.Open;
    Try
      oVCLStream.Stream.ReadBuffer(oResourceStream.Buffer.Data^, oResourceStream.Buffer.Capacity);

      Handle.LoadFromStream(oVCLStream.Stream);
    Finally
      oResourceStream.Close;
    End;
  Finally
    oVCLStream.Free;
    oResourceStream.Free;
  End;
End;



Class Procedure TFslPortableNetworkGraphic.SavePNGToStream(oImage: TFslVCLGraphic; oStream: TFslStream);
Var
  oPNG : TPngObject;
  oAdaptor : TVCLStream;
Begin
  oPNG := TPngObject.Create;
  Try
    oPNG.Assign(oImage.Handle);

    oAdaptor := TVCLStream.Create;
    Try
      oAdaptor.Stream := oStream.Link;

      oPNG.SaveToStream(oAdaptor);
    Finally
      oAdaptor.Free;
    End;
  Finally
    oPng.Free;
  End;
End;

end.
