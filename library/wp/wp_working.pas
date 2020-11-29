Unit wp_working;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface


Uses
  Windows, SysUtils, Classes, Graphics, UITypes,
  {$IFDEF DELPHI}
  GraphicEx,
  {$ENDIF}
  fsl_base, fsl_utilities, fsl_stream,
  fsl_collections, fsl_xml,

  wp_printing_base, wp_graphics, wp_definers, wp_types, wp_document;

Const
  MARK__IN = 5;
  MARK_OUT = 10;


Type
  TWPIntegers = TFslPrinterIntegers;
  PWPIntegers = PFslPrinterIntegers;

Type
  TLineBreaks = Array of Integer;

  TWPWorkingAnnotation = Class (TFslObject)
  Private
    FDrawn: Boolean;
    FAligned: Boolean;
    FMeasured: Boolean;
    FDefinitionProvider : TWPAnnotationDefinitionProvider;
    FOwner : String;
    FText: String;
    FAnchor: Integer;
    FTop: Integer;
    FBottom: Integer;
    FInUse : Boolean;

    FLineBreaks : TLineBreaks;
    FColour : TColour;
    FOffsetStart : Integer;
    FOffsetEnd : Integer;
    FSelected: Boolean;
    FWorkingText : String;

    procedure SetText(const Value: String);
    procedure SetAnchor(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetLineBreaks(const Value: TLineBreaks);
    procedure SetColour(const Value: TColour);
    procedure SetSelected(const Value: Boolean);
    Procedure SetDefinitionProvider(Const Value : TWPAnnotationDefinitionProvider);
    function GetWorkingText: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPWorkingAnnotation; Overload;
    Function Clone : TWPWorkingAnnotation; Overload;
    Procedure Assign(oSource : TFslObject); Override;
    Function Describe : String; overload;

    Function Matches(oOther : TWPWorkingAnnotation) : Boolean; Overload; Virtual;

    Property OffsetStart : Integer read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Integer read FOffsetEnd write FOffsetEnd;
    Property Owner : String read FOwner write FOwner;
    Property Text : String Read FText Write SetText;
    Property Colour : TColour read FColour write SetColour;

    Property LineBreaks : TLineBreaks Read FLineBreaks Write SetLineBreaks;

    Property Anchor : Integer read FAnchor write SetAnchor;
    Property Top : Integer Read FTop Write SetTop;
    Property Bottom : Integer Read FBottom Write SetBottom;
    Function Height : Integer;
    Property Selected : Boolean Read FSelected Write SetSelected;

    Property Measured : Boolean Read FMeasured write FMeasured;
    Property Aligned : Boolean Read FAligned Write FAligned;
    Property Drawn : Boolean Read FDrawn Write FDrawn;
    Property InUse : Boolean Read FInUse write FInUse;
    Property DefinitionProvider : TWPAnnotationDefinitionProvider read FDefinitionProvider write SetDefinitionProvider;
    Property WorkingText : String read GetWorkingText;
  End;

  TWPWorkingAnnotationList = Class (TFslObjectList)
  Private
    Function GetAnnotation(iIndex: Integer): TWPWorkingAnnotation;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPWorkingAnnotationList; Overload;

    Function Select(iStart, iStop: Integer) : Boolean; Overload; Virtual;
    Function GetBySelection(iStart, iStop: Integer) : TWPWorkingAnnotation; Overload; Virtual;

    Property Annotation[iIndex : Integer] : TWPWorkingAnnotation Read GetAnnotation; Default;

    Function Add(sOwner, sValue : String) : TWPWorkingAnnotation; Overload;
  End;

  TWPMetrics = Class (TFslObject)
  Private
    FCharCount: Integer;
    FVoiceCharCount: Integer;
    FOffsetCount: Integer;
    FPosition: Integer;
    FVoicePosition: Integer;
    FOffsets: PWPIntegers;
    FHeight: Integer;
    FDescent: Integer;
    FValid: Boolean;
    FMeasuredWidth: Integer;
    Function GetOffset(iIndex: Integer): Integer;
    Procedure SetOffset(iIndex: Integer; Const Value: Integer);
    Procedure SetOffsetCount(Const Value: Integer);
    Function GetWidth : Integer;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPMetrics; Overload;
    Function Clone : TWPMetrics; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property CharCount : Integer Read FCharCount Write FCharCount;
    Property VoiceCharCount : Integer Read FVoiceCharCount Write FVoiceCharCount;
    Property Position : Integer Read FPosition Write FPosition;
    Property VoicePosition : Integer Read FVoicePosition Write FVoicePosition;

    Property Valid : Boolean Read FValid Write FValid;
    Property Height : Integer Read FHeight Write FHeight;
    Property Descent : Integer Read FDescent Write FDescent;
    Property OffsetCount : Integer Read FOffsetCount Write SetOffsetCount;
    Property Offset[iIndex : Integer] : Integer Read GetOffset Write SetOffset;
    Property Offsets : PWPIntegers Read FOffsets;
    Property Width : Integer Read GetWidth;
    Property MeasuredWidth : Integer read FMeasuredWidth write FMeasuredWidth;
  End;

  TWPMapObject = Class (TFslObject)
  Private
    FParent : TWPMapObject;
    FPainted: Boolean;

    FWidth: Integer;
    FHeight: Integer;
    FTop: Integer;
    FLeft: Integer;
    FPiece : TFslObject;
    FBackground : TColour;
    FBackHotspot : TWPHotspot;

    FClipLeft : Integer;
    FClipTop : Integer;
    FClipRight : Integer;
    FClipBottom : Integer;

    Procedure SetHeight(Const Value: Integer);
    Procedure SetLeft(Const Value: Integer);
    Procedure SetTop(Const Value: Integer);
    Procedure SetWidth(Const Value: Integer);
    Function GetRight: Integer;
    Function GetCentre: Integer;
    Function GetBottom: Integer;
    Procedure SetBackground(Const Value: TColour);
    Function GetParent : TWPMapObject;
    Procedure SetParent(Const Value : TWPMapObject);
    Function GetPiece : TFslObject;
    Procedure SetPiece(Const Value : TFslObject);
    Procedure SetBackHotspot(Const Value: TWPHotspot);

    Function GetAnnotation(iId : Integer): TWPWorkingAnnotation;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oPiece : TFslObject); Overload; Virtual;
    destructor Destroy; Override;
    Function Link : TWPMapObject; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean); Overload; Virtual;

    Procedure Copy(oSource : TWPMapObject); Overload; Virtual;
    Procedure WantPainting; Overload; Virtual;
    Function HasClip : Boolean; Overload; Virtual;
    Procedure Clip(iLeft, iTop, iRight, iBottom : Integer);
    Procedure UnClip;

    Function HasPiece : Boolean; Overload; Virtual;
    Function HasParent : Boolean; Overload; Virtual;

    Function WorkingBackground(oCurrent : TWPHotspot) : TColour; Overload; Virtual;

    Function ContainsPoint(iX, iY : Integer; iRightRestriction : Integer = 0) : Boolean;
    Property Painted : Boolean Read FPainted Write FPainted;

    Function asRect : TRect; Overload; Virtual;

    Function debugType : String; Overload; Virtual;
    Function BoundsAsString : String; Overload; Virtual;

    Function VerticalMiddle : Integer;
    Function HorizontalMiddle : Integer;

    Property Left : Integer Read FLeft Write SetLeft;
    Property Width : Integer Read FWidth Write SetWidth;
    Property Right : Integer Read GetRight;
    Property Top : Integer Read FTop Write SetTop;
    Property Height : Integer Read FHeight Write SetHeight;
    Property Parent : TWPMapObject Read GetParent Write SetParent;
    Property Piece : TFslObject Read GetPiece Write SetPiece;
    Property Centre : Integer Read GetCentre;
    Property Bottom : Integer Read GetBottom;
    Property Background : TColour Read FBackground Write SetBackground;
    Property BackHotspot : TWPHotspot Read FBackHotspot Write SetBackHotspot;

    Property ClipLeft : Integer Read FClipLeft Write FClipLeft;
    Property ClipTop : Integer Read FClipTop Write FClipTop;
    Property ClipRight : Integer Read FClipRight Write FClipRight;
    Property ClipBottom : Integer Read FClipBottom Write FClipBottom;
  End;

  TWPMapObjectClass = Class Of TWPMapObject;

  TWPMapObjects = Class (TFslObjectList)
  Private
    Function GetObject(iIndex: Integer): TWPMapObject;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property MapObject[iIndex : Integer] : TWPMapObject Read GetObject; Default;
  End;

  TWPMapObjectsClass = Class Of TWPMapObjects;

  TWPMapSelection = (wsNone, wsAll, wsFromLeft, wsFromRight, wsPart);

  TWPMapItemBreakState = (bsText, bsWhitespace, bsBreak, bsPara);

  TWPMapItem = Class (TWPMapObject)
  Private
    FOffsetLength: Integer;
    FOffsetStart: Integer;
    FDescent: Integer;
    FBreakState: TWPMapItemBreakState;
    FSelection: TWPMapSelection;
    FForeHotspot : TWPHotspot;
    FNose: Integer;
    FAnnotationColour : TColour;

    Procedure SetOffsetLength(Const Value: Integer);
    Procedure SetOffsetStart(Const Value: Integer);
    Procedure SetDescent(Const Value: Integer);
    Procedure SetSelection(Const Value: TWPMapSelection);
    Procedure SetForeHotspot(Const Value: TWPHotspot);
    procedure SetAnnotationColour(const Value: TColour);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;
    Function Link : TWPMapItem; Overload;
    Function Clone : TWPMapItem; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Function OffsetForSelection(iOffset : Integer) : Integer;  Overload; Virtual;
    Function PointForSelection(iOffset : Integer) : Integer; Overload; Virtual;
    Function WorkingOffsetEnd : Integer; Overload; Virtual;

    Function GetText : String;
    Function IsLastInLine : Boolean;

    Function WorkingHotspot : TWPHotspot;
    Function InForeHotspot : Boolean;
    Function InHotspot : Boolean;
    Function InBackHotspot : Boolean;

    Property Nose : Integer Read FNose Write FNose;
    Property Descent : Integer Read FDescent Write SetDescent;
    Property OffsetStart : Integer Read FOffsetStart Write SetOffsetStart;
    Property OffsetLength : Integer Read FOffsetLength Write SetOffsetLength;
    Property BreakState : TWPMapItemBreakState Read FBreakState Write FBreakState;
    Property Selection : TWPMapSelection Read FSelection Write SetSelection;
    Property ForeHotspot : TWPHotspot Read FForeHotspot Write SetForeHotspot;
    Property AnnotationColour : TColour Read FAnnotationColour Write SetAnnotationColour;
  End;

  TWPMapItems = Class (TWPMapObjects)
  Private
    Function GetMapItem(iIndex: Integer): TWPMapItem;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPMapItems; Overload;
    Function Clone : TWPMapItems; Overload;

    Procedure SetNotPainted;
    Function GetByOffset(iOffset : Integer) : TWPMapItem;

    Function First : TWPMapItem;
    Function Last : TWPMapItem;

    Property MapItem[iIndex : Integer] : TWPMapItem Read GetMapItem; Default;
  End;


Const
  NAMES_WPMAPSELECTION : Array [TWPMapSelection] Of String = ('None', 'All', 'FromLeft', 'FromRight', 'Part');
  NAMES_WPMAPITEMBREAKSTATE : Array [TWPMapItemBreakState] Of String = ('Text', 'Whitespace', 'Break', 'Para');

Type
  TWPMapRow = Class (TWPMapObject)
  Private
    FItems: TWPMapItems;
    FBaseLine: Integer;
    FPrintLeftOffset : Integer;
    FPrintTopOffset : Integer;
    FPaintedFirstLeft : Integer;
    FPaintedLastRight : Integer;
    FLine : Integer;
    FColumnOffset : Integer;
    FAllow : Integer;

    Procedure SetItems(Const Value: TWPMapItems);
    Procedure ReParent;
    Function GetItems : TWPMapItems;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPMapRow; Overload;
    Function Clone : TWPMapRow; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean); Overload; Override;
    Function GetLastOffset : Integer; Overload; Virtual;

    Function FirstLeft : Integer; Overload; Virtual;
    Function LastRight : Integer; Overload; Virtual;

    Function SumWidths(iLimit : Integer) : Integer;
    Function ColCount : Integer;

    Function GetText : String;
    Procedure Allow(iAllow : Integer);

    Property BaseLine : Integer Read FBaseLine Write FBaseLine;
    Property Items : TWPMapItems Read GetItems Write SetItems;

    Property PrintLeftOffset : Integer Read FPrintLeftOffset Write FPrintLeftOffset;
    Property PrintTopOffset : Integer Read FPrintTopOffset Write FPrintTopOffset;

    Property Line : Integer Read FLine Write FLine;
    Property ColumnOffset : Integer Read FColumnOffset Write FColumnOffset;
    Property PaintedFirstLeft : Integer Read FPaintedFirstLeft Write FPaintedFirstLeft;
    Property PaintedLastRight : Integer Read FPaintedLastRight Write FPaintedLastRight;
  End;

  TWPMapRows = Class (TWPMapObjects)
  Private
    Function GetRow(iIndex: Integer): TWPMapRow;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPMapRows; Overload;
    Function Clone : TWPMapRows; Overload;

    Function Last : TWPMapRow;
    Function First : TWPMapRow;
    Property Row[iIndex : Integer] : TWPMapRow Read GetRow; Default;
  End;

Type
  TWPMapContainers = Class;

  TWPMapContainer = Class (TWPMapObject)
  Private
    FChildren: TWPMapContainers;
    FRows: TWPMapRows;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FMarginBottom: Integer;
    FChildrenHorizontal : Boolean;
    FPrintLeftOffset : Integer;
    FPrintTopOffset : Integer;
    FPrimary : Boolean;
    FCursor : Integer;

    Procedure SetChildren(Const Value: TWPMapContainers);
    Procedure SetRows(Const Value: TWPMapRows);
    Procedure SetMarginLeft(Const Value: Integer);
    Procedure SetMarginBottom(Const Value: Integer);
    Procedure SetMarginTop(Const Value: Integer);
    Procedure SetMarginRight(Const Value: Integer);
    Procedure ReParent;
    Function GetChildren : TWPMapContainers;
    Function GetRows : TWPMapRows;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPMapContainer; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean); Overload; Override;

    Procedure Clear; Overload; Virtual;

    Function GetLastOffset : Integer; Overload; Virtual;

    Function InnerLeft: Integer; Overload; Virtual;
    Function InnerBottom: Integer; Overload; Virtual;
    Function InnerRight: Integer; Overload; Virtual;
    Function InnerTop: Integer; Overload; Virtual;
    Function InnerWidth : Integer; Overload; Virtual;
    Function InnerHeight : Integer; Overload; Virtual;

    Function Fits(iHeight : Integer) : Boolean; Overload; Virtual;

    Function FirstRow : TWPMapRow;
    Function FirstPopulatedRow : TWPMapRow;
    Function LastRow : TWPMapRow;

    Property ChildrenHorizontal : Boolean Read FChildrenHorizontal Write FChildrenHorizontal;
    Property Children : TWPMapContainers Read GetChildren Write SetChildren;
    Property Rows : TWPMapRows Read GetRows Write SetRows;
    Property MarginLeft : Integer Read FMarginLeft Write SetMarginLeft;
    Property MarginTop : Integer Read FMarginTop Write SetMarginTop;
    Property MarginRight : Integer Read FMarginRight Write SetMarginRight;
    Property MarginBottom : Integer Read FMarginBottom Write SetMarginBottom;

    Property PrintLeftOffset : Integer Read FPrintLeftOffset Write FPrintLeftOffset;
    Property PrintTopOffset : Integer Read FPrintTopOffset Write FPrintTopOffset;
    Property Primary : Boolean Read FPrimary Write FPrimary;
    Property Cursor : Integer Read FCursor Write FCursor;
  End;

  TWPMapContainers = Class (TWPMapObjects)
  Private
    Function GetContainer(iIndex: Integer): TWPMapContainer;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPMapContainers; Overload;
    Function Clone : TWPMapContainers; Overload;
    Function GetLastOffset : Integer; Overload; Virtual;

    Property Container[iIndex : Integer] : TWPMapContainer Read GetContainer; Default;
  End;


Type
  TChangeType = wp_types.TChangeType;

  TWPAnnotationStatus = (AnnotationStatusNone, AnnotationStatusAll, AnnotationStatusStart, AnnotationStatusContinue, AnnotationStatusEnd);

  TWPWorkingDocumentPieceType = ({inline pieces} ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak,
                  { block pieces} ptBreak, ptPara, ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop);

  TWPWorkingDocumentPieceTypes = Set Of TWPWorkingDocumentPieceType;

Const
  CODES_WPPIECETYPE : Array [TWPWorkingDocumentPieceType] Of String = ('Text', 'Image', 'FieldStart', 'FieldEnd', 'LineBreak', 'Break', 'Para', 'TableStart', 'RowStart', 'CellStart', 'TableEnd', 'RowEnd', 'CellEnd', 'SectionStart', 'SectionStop');
  NAMES_WPPIECETYPE : Array [TWPWorkingDocumentPieceType] Of String = ('Text', 'Image', 'Field Start', 'Field End', 'Line Break', 'Break', 'Paragraph', 'Table Start', 'Row Start', 'Cell Start', 'Table End', 'Row End', 'Cell End', 'Section Start', 'Section Stop');
  ALL_PIECE_TYPES = [ptText..ptSectionStop];
  INLINE_PIECE_TYPES = [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak];
  BLOCK_PIECE_TYPES = [ptBreak, ptPara, ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop];

  ctListContents = wp_types.ctListContents;
  ctTextContents = wp_types.ctTextContents;
  ctLayout = wp_types.ctLayout;
  ctPresentation = wp_types.ctPresentation;


Type
  TWPWorkingDocumentPiece = Class (TWPTrackable)
  Private
    FMaps: TWPMapItems;
    FMetrics: TWPMetrics;
    FFont: TWPSFontDetails;
    FStyle: String;
    FIsReadOnly : Boolean;
    FPrev : TWPWorkingDocumentPiece;
    FNext : TWPWorkingDocumentPiece;
    FHotspot : TWPHotspot;
    FFieldHint: String;
    FAnnotationId : Integer;
    FAnnotationStatus: TWPAnnotationStatus;
    FFieldError: String;
    FSourceCol: Integer;
    FSourceLine: Integer;
    FSourceObject : TFslObject;

    Function GetNext : TWPWorkingDocumentPiece;
    Function GetPrev : TWPWorkingDocumentPiece;
    Procedure SetFont(Const Value: TWPSFontDetails);
    Procedure SetMetrics(Const Value: TWPMetrics);
    Function GetMap : TWPMapItem;
    Procedure SetMap(Const Value: TWPMapItem);
    Function GetMetrics : TWPMetrics;
    Function GetFont : TWPSFontDetails;
    Function GetMaps : TWPMapItems;
    Function GetHasHotSpot: Boolean;
    Procedure SetHasHotspot(Const bValue: Boolean);
    Procedure SetHotspot(Const Value: TWPHotspot);
    procedure SetAnnotationId(const Value: Integer);
    procedure SetAnnotationStatus(const Value: TWPAnnotationStatus);
    procedure SetSourceObject(const Value: TFslObject);
  Protected
    Function GetLogicalText: String; Virtual;
    Function GetVisualText: String; Virtual;
    Function GetVoiceText : String; Virtual;

    Function XmlName : String; Virtual;
    Procedure CollectAttributes(oXml : TFslXmlFormatter); Virtual;
    Procedure ProduceChildren(oXml : TFslXmlFormatter); Virtual;

    Function GetPieceType: TWPWorkingDocumentPieceType; Virtual;
    Function DefaultCharCount : Integer; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPWorkingDocumentPiece; Overload;
    Function Clone : TWPWorkingDocumentPiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure ResetRendering; Overload; Virtual;

    Procedure ApplyProperties(oSource: TWPWorkingDocumentPiece); Virtual;
    Procedure MergeProperties(oSource: TWPWorkingDocumentPiece); Virtual;

    Procedure Change(aType : TChangeType; oSource : TWPTrackable); Overload; Override;

    Function Split(iOffset : Integer) : TWPWorkingDocumentPiece; Overload; Virtual;
    Function StyleMatches(oOther : TWPWorkingDocumentPiece) : Boolean;  Overload; Virtual;
    Function AnnotationMatches(oOther : TWPWorkingDocumentPiece) : Boolean;  Overload; Virtual;
    Procedure AssignStyle(oOther : TWPWorkingDocumentPiece);  Overload; Virtual;
    Function IsLineBreak : Boolean; Overload; Virtual;

    Function HasPrev : Boolean; Overload; Virtual;
    Function HasNext : Boolean; Overload; Virtual;
    Function CanCopyAsText : Boolean; Overload; Virtual;
    procedure ClearSource;

    Function ForceHotspot : TWPHotspot;
    Procedure DumpToXml(oXml : TFslXmlFormatter);

    Property PieceType : TWPWorkingDocumentPieceType Read GetPieceType;
    Property Metrics : TWPMetrics Read GetMetrics Write SetMetrics;
    Property Style : String Read FStyle Write FStyle;
    Property Font : TWPSFontDetails Read GetFont Write SetFont;
    Property Hotspot : TWPHotspot Read FHotSpot Write SetHotspot;
    Property HasHotspot : Boolean Read GetHasHotSpot Write SetHasHotspot;

    Function Describe : String;
    Function DescribeV : String; Overload; Virtual;

    // Logical Text and Visual Text must have the same length.
    // Logical is the actual contents.
    // visual is the windows characters drawn on the screen (though for some types they will only be drawn if EditShowHints is true)
    // Voice is a stylised representation for the voice interface. Length may differ from the others
    Property LogicalText : String Read GetLogicalText;
    Property VisualText : String Read GetVisualText;
    Property VoiceText : String Read GetVoiceText;

    Property Prev : TWPWorkingDocumentPiece Read GetPrev Write FPrev;
    Property Next : TWPWorkingDocumentPiece Read GetNext Write FNext;
    Property Maps : TWPMapItems Read GetMaps;
    Property Map : TWPMapItem Read GetMap Write SetMap;
    Property IsReadOnly : Boolean Read FIsReadOnly Write FIsReadOnly;
    Property FieldHint : String Read FFieldHint Write FFieldHint;
    Property FieldError : String read FFieldError write FFieldError;
    Property AnnotationId : Integer read FAnnotationId write SetAnnotationId;
    Property AnnotationStatus : TWPAnnotationStatus Read FAnnotationStatus Write SetAnnotationStatus;
    Property SourceLine : Integer read FSourceLine write FSourceLine;
    Property SourceCol : Integer read FSourceCol write FSourceCol;
    property SourceObject : TFslObject read FSourceObject write SetSourceObject;
  End;

  TWPWorkingDocumentPieceClass = Class Of TWPWorkingDocumentPiece;

  TWPWorkingDocumentPieces = Class (TWPTrackableList)
  Private
    Function GetPiece(iIndex: Integer): TWPWorkingDocumentPiece;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPWorkingDocumentPieces; Overload;
    Function Clone : TWPWorkingDocumentPieces; Overload;

    Function First : TWPWorkingDocumentPiece; Overload; Virtual;
    Function Last : TWPWorkingDocumentPiece; Overload; Virtual;

    Function AsText : String;

    Property Piece[iIndex : Integer] : TWPWorkingDocumentPiece Read GetPiece; Default;
  End;

  TWPWorkingDocumentPieceStack = Class (TWPWorkingDocumentPieces)
    Public
      constructor Create; Override;

      Procedure Push(oPiece : TWPWorkingDocumentPiece);
      Function Pop : TWPWorkingDocumentPiece;
  End;

  TWPWorkingDocumentSpellCheckingState = (scsNotChecked, scsExempt, scsOK, scsWrong, scsFieldWrong);

  TWPWorkingDocumentTextPiece = Class (TWPWorkingDocumentPiece)
  Private
    FContent: String;
    FSpellState: TWPWorkingDocumentSpellCheckingState;
    FDrawnFont: String;
    Procedure SetContent(Const sValue: String);
    Procedure SetSpellState(Const Value: TWPWorkingDocumentSpellCheckingState);
  Protected
    Function GetLogicalText : String; Override;
    Function GetVoiceText : String; Override;
    Function XmlName : String; Override;
    Procedure CollectAttributes(oXml : TFslXmlFormatter); Override;
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TWPWorkingDocumentTextPiece; Overload;
    Function Clone : TWPWorkingDocumentTextPiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Function DescribeV : String; override;

    Function Split(iOffset : Integer) : TWPWorkingDocumentPiece; Override;
    Function CloneStart(iOffset : Integer) : TWPWorkingDocumentTextPiece; Overload; Virtual;
    Function CloneEnd(iOffset : Integer) : TWPWorkingDocumentTextPiece; Overload; Virtual;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;
    Function CanCopyAsText : Boolean; Override;

    Property Content : String Read FContent Write SetContent;
    Property SpellState : TWPWorkingDocumentSpellCheckingState Read FSpellState Write SetSpellState;
    Property DrawnFont : String Read FDrawnFont Write FDrawnFont;
  End;

Const
  WPSPELLCHECKINGSTATE_NAMES : Array [TWPWorkingDocumentSpellCheckingState] Of String = ('Not Checked', 'Exempt', 'OK', 'Wrong', 'WrongField');

Type
  TWPWorkingDocumentLineBreakPiece = Class (TWPWorkingDocumentPiece)
  Private
    {
      A speech magic work around:

      If a user says "new paragraph" while they are in a field, we insert a new line instead.
      in this case, the new line has to have a field width of 2 not 1. But where a user says
      "new line", then it must have a field width of 1. We specifically mark line breaks that
      have a width of 2 with this flag
    }
    FIsSpeechMagicParagraph : Boolean;
  Protected
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;
    Function GetVoiceText : String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    Procedure Assign(oSource : TFslObject); Override;

    Function Link : TWPWorkingDocumentLineBreakPiece; Overload;
    Function Clone : TWPWorkingDocumentLineBreakPiece; Overload;
    Function GetPieceType : TWPWorkingDocumentPieceType; Override;
    Function IsLineBreak : Boolean; Overload; Override;
    Function DescribeV : String; override;

    Property IsSpeechMagicParagraph : Boolean Read FIsSpeechMagicParagraph Write FIsSpeechMagicParagraph;
  End;

Const
  ivaBaseLine = ImageVerticalAlignmentBaseLine;
  ivaBottom = ImageVerticalAlignmentBottom;
  ivaCentered = ImageVerticalAlignmentCentered;
  ivaTop = ImageVerticalAlignmentTop;


Type

  TWPWorkingDocumentImagePiece = Class (TWPWorkingDocumentPiece)
  Private
    FImage : TFslGraphic;
    FSelectionImage : TFslGraphic;
    FDefaultAdornmentColour : TColour;
    FWorkingImage : TFslVCLGraphic;
    FWorkingHeight, FWorkingWidth : integer;

    FBorder : Integer;
    FHeight : Integer;
    FWidth : Integer;
    FBorderColour: TColour;
    FName: String;
    FTransparentColour : TColour;
    FVerticalAlignment : TWordProcessorImageVerticalAlignment;
    FImageMap : TWPImageMap;
    FAdornments : TWPDocumentImageAdornments;
    FSizePolicy: TWordProcessorImageSizePolicy;
    FFrameIndex: integer;

    Procedure SetImage(Const Value: TFslGraphic);
    Procedure SetSelectionImage(Const Value: TFslGraphic);
    Procedure SetBorder(Const Value: Integer);
    Procedure SetBorderColour(Const Value: TColour);
    Procedure SetTransparentColour(Const Value: TColour);
    Procedure SetHeight(Const Value: Integer);
    Procedure SetWidth(Const Value: Integer);

    Procedure ApplyTransparency;
    Procedure ImageMapChange(aType : TChangeType; oSource : TWPTrackable);

    Function MakePenHandle(oPen : TPen; aEndStyle : TFslPenEndStyle; aJoinStyle : TFslPenJoinStyle): HPEN;
    Function MakeImage : TFslBitmapGraphic;
    Procedure Transfer(oTemp, oMask : TFslBitmapGraphic);
    Procedure FillWithWhite(oMask : TFslBitmapGraphic);
    Procedure DrawSection(oMask : TFslBitmapGraphic);

    Procedure BuildWorkingImage(aWidth, aHeight : Integer; bHotspots : Boolean);

    Procedure DrawAdornmentLine(oImage : TFslBitmapGraphic; oAdornment : TWPDocumentImageAdornment);
    Procedure DrawAdornmentShape(oImage : TFslBitmapGraphic; oAdornment : TWPDocumentImageAdornment);
    Procedure DrawAdornmentZoom(oImage : TFslBitmapGraphic; oAdornment : TWPDocumentImageAdornment);
    Procedure DrawAdornmentMark(oImage : TFslBitmapGraphic; oAdornment : TWPDocumentImageAdornment);
    Procedure DrawAdornment(oImage : TFslBitmapGraphic; oAdornment : TWPDocumentImageAdornment);

    Function CaptionMatches(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint) : Boolean;
    Function AdornmentMatches(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
    Function AdornmentMatchesLine(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
    Function AdornmentMatchesRectangle(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
    Function AdornmentMatchesCircle(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
    Function AdornmentMatchesZoom(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
    Function AdornmentMatchesMark(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;

    Function GetImage : TFslGraphic;
    Function GetSelectionImage : TFslGraphic;
    Function GetImageMap : TWPImageMap;
    Function GetHasImageMap : Boolean;
    Procedure SetHasImageMap(bValue : Boolean);
    procedure SetFrameIndex(const Value: integer);
    procedure SetSizePolicy(const Value: TWordProcessorImageSizePolicy);
  Protected
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPWorkingDocumentImagePiece; Overload;
    Function Clone : TWPWorkingDocumentImagePiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;

    Function GetWorkingImage(aWidth, aHeight : Integer; bHotspots, bFast : Boolean): TFslGraphic; Overload; Virtual;
    Function FactorX : Real; Overload; Virtual;
    Function FactorY : Real; Overload; Virtual;

    Function ImageFileExtension : String; Overload; Virtual;
    Function ImageTypename : String; Overload; Virtual;

    Property Name : String Read FName Write FName;
    Function DescribeV : String; override;

    Function HasImage : Boolean; Overload; Virtual;
    Property Image : TFslGraphic Read GetImage Write SetImage;

    Function HasSelectionImage : Boolean; Overload; Virtual;
    Property SelectionImage : TFslGraphic Read GetSelectionImage Write SetSelectionImage;
    Property ImageMap : TWPImageMap Read GetImageMap;
    Property HasImageMap : Boolean Read GetHasImageMap Write SetHasImageMap;


    Function MaxFrameIndex : Integer;
    Property FrameIndex : integer read FFrameIndex write SetFrameIndex;
    Property Border : Integer Read FBorder Write SetBorder;
    Property BorderColour : TColour Read FBorderColour Write SetBorderColour;
    Property Width : Integer Read FWidth Write SetWidth;
    Property Height : Integer Read FHeight Write SetHeight;
    Property VerticalAlignment : TWordProcessorImageVerticalAlignment Read FVerticalAlignment Write FVerticalAlignment;
    Property TransparentColour : TColour Read FTransparentColour Write SetTransparentColour;
    Property SizePolicy : TWordProcessorImageSizePolicy Read FSizePolicy Write SetSizePolicy;

    Property Adornments : TWPDocumentImageAdornments Read FAdornments;
    Procedure ChangeAdornments;
    Function GetExtant(oAdornment : TWPDocumentImageAdornment; aPart : TAdornmentPart; aAction : TAdornmentAction) : TRect;
    Function IsAdornment(oInfo : TWPMouseInfo; iX, iY : Integer) : Boolean;
    Function DefaultAdornmentColour : TColour;
  End;

Const
  NAMES_IMAGEVERTICALALIGNMENT : Array [TWordProcessorImageVerticalAlignment] Of String = ('Base Line', 'Bottom', 'Centered', 'Top');
  NAMES_IMAGESIZEPOLICY : Array [TWordProcessorImageSizePolicy] Of String = ('Manual', 'Page-Width', 'Whole-Page');


Type
  TWPWorkingDocumentFieldStartPiece = Class (TWPWorkingDocumentPiece)
  Private
    FFixedFormat: TWPDocumentFieldFixedFormat;
    FDeletable: Boolean;
    FNamespace : String;
    FName : String;
    FReadOnly: TWPSTriState;
    FDocField: TWPDocumentField;
    FDefinitionProvider: TWPFieldDefinitionProvider;
    FData : TWPDataItemMap;
    FInError: Boolean;
    FWidth : Integer;
    FMinWidthPixels: Integer;
    FCheckables : TFslStringList;
    FCheckedIndex: Integer;
    FLastUpdateValue : String;

    Procedure SetDeletable(Const Value: Boolean);
    Procedure SetFixedFormat(Const Value: TWPDocumentFieldFixedFormat);
    Procedure SetWidth(Const Value: Integer);
    Procedure SetNamespace(Const Value: String);
    Procedure SetName(Const Value: String);
    Procedure SetReadOnly(Const Value: TWPSTriState);
    Function GetNamePair: String;
    Procedure SetNamePair(Const Value: String);
    Procedure SetDefinitionProvider(Const Value: TWPFieldDefinitionProvider);
    Procedure SetDocField(Const Value: TWPDocumentField);
    Procedure SetInError(Const Value: Boolean);
    Function GetRawDataAsText: String;
    Procedure SetRawDataAsText(Const Value: String);
    Function GetDataValue(Const sKey: String): String;
    Procedure SetDataValue(Const sKey, sValue: String);
  Protected
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;
    Function GetLogicalText: String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPWorkingDocumentFieldStartPiece; Overload;
    Function Clone : TWPWorkingDocumentFieldStartPiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure BuildDocField;
    Function HasDefinitionProvider : Boolean;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;

    Property DocField : TWPDocumentField Read FDocField Write SetDocField;
    Property DefinitionProvider : TWPFieldDefinitionProvider Read FDefinitionProvider Write SetDefinitionProvider;

    Property Namespace : String Read FNamespace Write SetNamespace;
    Property Name : String Read FName Write SetName;
    Property NamePair : String Read GetNamePair Write SetNamePair;
    Property InError : Boolean Read FInError Write SetInError;
    Property ReadOnly : TWPSTriState Read FReadOnly Write SetReadOnly;
    Property Deletable : Boolean Read FDeletable Write SetDeletable;
    Property FixedFormat : TWPDocumentFieldFixedFormat Read FFixedFormat Write SetFixedFormat;
    Property Width : Integer Read FWidth Write SetWidth;
    Property MinWidthPixels : Integer Read FMinWidthPixels Write FMinWidthPixels;

    Property DataValue[Const sKey : String] : String Read GetDataValue Write SetDataValue;
    Property Checkables : TFslStringList Read FCheckables;
    Procedure BindToCheckables;
    Property CheckedIndex : Integer Read FCheckedIndex Write FCheckedIndex;
    Function DescribeV : String; override;

    // not for use outside word processor
    Function HasData : Boolean;
    Property RawDataAsText : String Read GetRawDataAsText Write SetRawDataAsText;
    Property RawData : TWPDataItemMap Read FData;
    Property LastUpdateValue : String read FLastUpdateValue write FLastUpdateValue;
    Function TitleOrName : String;
  End;


  TWPWorkingDocumentFieldStopPiece = Class (TWPWorkingDocumentPiece)
  Private
    Procedure SetMatchingStart(Const Value: TWPWorkingDocumentFieldStartPiece);
  Protected
    FMatchingStart : TWPWorkingDocumentFieldStartPiece;
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;
    Function GetLogicalText: String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Link : TWPWorkingDocumentFieldStopPiece; Overload;
    Function Clone : TWPWorkingDocumentFieldStopPiece; Overload;
    Function GetPieceType : TWPWorkingDocumentPieceType; Override;

    // this is a denormalisation for convenience. It is never set in it's
    // own right. The infrastructure keeps it in sync with it's matching
    // field Start - for convenience when navigating
    Property MatchingStart : TWPWorkingDocumentFieldStartPiece Read FMatchingStart Write SetMatchingStart;
  End;

Type
  TWPWorkingDocumentStopType = (stNone, stTableCell, stTableRow, stTable, stSection);

  TWPWorkingDocumentStopPiece = Class (TWPWorkingDocumentPiece)
    Private
      FStopType: TWPWorkingDocumentStopType;
      FMatchingStart : TWPWorkingDocumentPiece;
      Procedure SetMatchingStart(Const Value: TWPWorkingDocumentPiece);

      Procedure SetStopType(Const newType : TWPWorkingDocumentStopType);
    Protected
      Function DefaultCharCount : Integer; Override;
      Function XmlName : String; Override;
      Procedure CollectAttributes(oXml : TFslXmlFormatter); Override;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      constructor Create(stopType : TWPWorkingDocumentStopType); Overload;
      Function Link : TWPWorkingDocumentStopPiece; Overload;
      Function Clone : TWPWorkingDocumentStopPiece; Overload;
      Procedure Assign(oSource : TFslObject); Override;

      Function GetPieceType : TWPWorkingDocumentPieceType; Override;

      Property StopType : TWPWorkingDocumentStopType Read FStopType Write SetStopType;

      // this is a denormalisation for convenience. It is never set in it's
      // own right. The infrastructure keeps it in sync with it's matching
      // field Start - for convenience when navigating
      Property MatchingStart : TWPWorkingDocumentPiece Read FMatchingStart Write SetMatchingStart;
  End;

Const
  NAMES_WorkingDocumentStopType : Array [TWPWorkingDocumentStopType] Of String = ('None', 'TableCell', 'TableRow', 'Table', 'Section');

Type
  TWPWorkingDocumentContainerPiece = Class (TWPWorkingDocumentPiece)
  Private
    FReadOnly: TWPSTriState;
    FContainer: TWPMapContainer;
    Procedure SetContainer(Const Value: TWPMapContainer);
    Procedure SetReadOnly(Const Value: TWPSTriState);
    Function GetContainer : TWPMapContainer;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Procedure Assign(oSource : TFslObject); Override;
    Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;
    Function IsLineBreak : Boolean; Overload; Override;

    Function HasContainer : Boolean;
    Function DescribeV : String; override;

    Property Container : TWPMapContainer Read GetContainer Write SetContainer;
    Property ReadOnly : TWPSTriState Read FReadOnly Write SetReadOnly;
  End;

  TWPWorkingDocumentContainerPieces = Class (TWPWorkingDocumentPieces)
  Private
    Function GetElements(iIndex : Integer):TWPWorkingDocumentContainerPiece;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Elements[iIndex : Integer] : TWPWorkingDocumentContainerPiece Read GetElements; Default;
  End;

Type
  TWPWorkingDocumentParaPiece = Class (TWPWorkingDocumentContainerPiece)
  Private
    FFormat: TWPSParagraphDetails;
    FListNumber: Integer;
    FWorkingRightIndent: Integer;
    FWorkingLeftIndent: Integer;
    FSpeechMagicDouble : Boolean;   // RichEdit is inconsistent about whether para is 1 or 2 characters
    FAdornmentWidth: Integer;
    Procedure SetFormat(Const Value: TWPSParagraphDetails);
    Procedure SetListNumber(Const Value: Integer);
    Function GetFormat : TWPSParagraphDetails;
  Protected
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;
    Function GetVoiceText : String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPWorkingDocumentParaPiece; Overload;
    Function Clone : TWPWorkingDocumentParaPiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;
    Function CanCopyAsText : Boolean; Override;
    Function DescribeV : String; override;

    Property Format : TWPSParagraphDetails Read GetFormat Write SetFormat;
    Property ListNumber : Integer Read FListNumber Write SetListNumber;
    Property WorkingLeftIndent : Integer Read FWorkingLeftIndent Write FWorkingLeftIndent;
    Property WorkingRightIndent : Integer Read FWorkingRightIndent Write FWorkingRightIndent;
    Property SpeechMagicDouble : Boolean Read FSpeechMagicDouble Write FSpeechMagicDouble;
    Property AdornmentWidth : Integer Read FAdornmentWidth Write FAdornmentWidth;
  End;

Type
  TWPWorkingDocumentBreakPieceType = (btLine, btPageBreak);
  TWPWorkingDocumentBreakPieceTypes = Set Of TWPWorkingDocumentBreakPieceType;

  TWPWorkingDocumentBreakPiece = Class (TWPWorkingDocumentContainerPiece)
  Private
    FBreakType : TWPWorkingDocumentBreakPieceType;
    FAlignment : TWordProcessorAlignment;
    FWidth : Real;
    FPenColour : TColour;
    FPenWidth : Integer;
    FPenStyle : TFslPenStyle;
    FEndStyle  : TFslPenEndStyle;

    Procedure SetBreakType(Const Value : TWPWorkingDocumentBreakPieceType);
    Procedure SetWidth(Const Value : Real);
    Procedure SetPenColour(Const Value : TColour);
    Procedure SetPenWidth(Const Value : Integer);
    Procedure SetPenStyle(Const Value : TFslPenStyle);
    Procedure SetEndStyle(Const Value : TFslPenEndStyle);

  Protected
    Function DefaultCharCount : Integer; Override;
    Function GetVisualText: String; Override;

  Public
    Function Link : TWPWorkingDocumentBreakPiece; Overload;
    Function Clone : TWPWorkingDocumentBreakPiece; Overload;

    Procedure Assign(oSource : TFslObject); Override;
    Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;
    Function IsLineBreak : Boolean; Overload; Override;
    Function DescribeV : String; override;

    Property BreakType : TWPWorkingDocumentBreakPieceType Read FBreakType Write SetBreakType;
    Property Width : Real Read FWidth Write SetWidth;
    Property PenColour : TColour Read FPenColour Write SetPenColour;
    Property PenWidth : Integer Read FPenWidth Write SetPenWidth;
    Property PenStyle : TFslPenStyle Read FPenStyle Write SetPenStyle;
    Property EndStyle : TFslPenEndStyle Read FEndStyle Write SetEndStyle;
    Property Alignment : TWordProcessorAlignment Read FAlignment Write FAlignment;
  End;

Const
  WPWorkingDocumentBreakPIECETYPE_NAMES : Array [TWPWorkingDocumentBreakPieceType] Of String = ('Line', 'PageBreak');
  WPWorkingDocumentBreakPIECETYPE_DISPLAYS : Array [TWPWorkingDocumentBreakPieceType] Of String  = ('Horizontal Line', 'Page Break');
  WPWorkingDocumentBreakPIECETYPE_UNIVERSAL = [Low(TWPWorkingDocumentBreakPieceType)..High(TWPWorkingDocumentBreakPieceType)];



Type
  TWPWorkingDocumentSectionDisplayType = (sdtNone, sdtLine, sdtName);

  TWPWorkingDocumentSectionStartPiece = Class (TWPWorkingDocumentContainerPiece)
  Private
    FDisplayName: String;
    FNamespace: String;
    FName: String;
    FDisplayType : TWPWorkingDocumentSectionDisplayType;
    FDeletable : Boolean;
    FIsField : Boolean;
    FKey : String;
    FDocSection : TWPDocumentSection;
    FDefinitionProvider : TWPFieldDefinitionProvider;
    FData : TWPDataItemMap;

    Procedure SetKey(Const Value: String);
    Procedure SetIsField(Const Value: Boolean);
    Procedure SetDeletable(Const Value: Boolean);
    Procedure SetDisplayName(Const Value: String);
    Procedure SetName(Const Value: String);
    Procedure SetDisplayType(Const Value: TWPWorkingDocumentSectionDisplayType);
    Function GetNamePair: String;
    Procedure SetNamePair(Const Value: String);
    Procedure SetDefinitionProvider(Const Value: TWPFieldDefinitionProvider);
    Procedure SetDocSection(Const Value: TWPDocumentSection);
    Function GetRawDataAsText: String;
    Procedure SetRawDataAsText(Const Value: String);
    Function GetDataValue(Const sKey: String): String;
    Procedure SetDataValue(Const sKey, sValue: String);
  Protected
    Function DefaultCharCount : Integer; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPWorkingDocumentSectionStartPiece; Overload;
    Function Clone : TWPWorkingDocumentSectionStartPiece; Overload;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;
    Procedure BuildDocSection;
    Function HasDefinitionProvider : Boolean;

    Function GetPieceType : TWPWorkingDocumentPieceType; Override;
    Function DescribeV : String; override;

    Property DocSection : TWPDocumentSection Read FDocSection Write SetDocSection;
    Property DefinitionProvider : TWPFieldDefinitionProvider Read FDefinitionProvider Write SetDefinitionProvider;

    Property DisplayType : TWPWorkingDocumentSectionDisplayType Read FDisplayType Write SetDisplayType;
    Property Namespace : String Read FNamespace Write FNamespace;
    Property Name : String Read FName Write SetName;
    Property NamePair : String Read GetNamePair Write SetNamePair;
    Property DisplayName : String Read FDisplayName Write SetDisplayName;
    Property Deletable : Boolean Read FDeletable Write SetDeletable;
    Property IsField : Boolean Read FIsField Write SetIsField;
    Property Key : String Read FKey Write SetKey;

    Property DataValue[Const sKey : String] : String Read GetDataValue Write SetDataValue;

    // not for use outside word processor
    Function HasData : Boolean;
    Property RawDataAsText : String Read GetRawDataAsText Write SetRawDataAsText;
    Property RawData : TWPDataItemMap Read FData;
  End;

  // Instead of TWPWorkingDocumentSectionStopPiece, use WPWorkingDocumentStopPiece

Const
  NAMES_WPWorkingDocumentSectionDISPLAYTYPE : Array [TWPWorkingDocumentSectionDisplayType] Of String = ('None', 'Line', 'Name');

Type
  // if the item is a cell, then Left and Top are
  // overruled by cells to the left or top of this cell
  TWPWorkingDocumentTableItemPiece = Class (TWPWorkingDocumentContainerPiece)
    Private
      {
        NOTE: for inner cell, the border width represent the desired border width
        of this cell and its neighbour. As such, the real border width contribute
        by the settings in this cell is only half of the settings value
        (i.e. A border width of 6 mean a 3 width border created in the inside of this cell)
      }
      FLeftBorder : TWPBorder;
      FRightBorder : TWPBorder;
      FTopBorder : TWPBorder;
      FBottomBorder : TWPBorder;

      FWorkingLeftBorder : TWPBorder;
      FWorkingRightBorder : TWPBorder;
      FWorkingTopBorder : TWPBorder;
      FWorkingBottomBorder : TWPBorder;

      Procedure SetLeftBorder(Const Value : TWPBorder);
      Procedure SetRightBorder(Const Value : TWPBorder);
      Procedure SetTopBorder(Const Value : TWPBorder);
      Procedure SetBottomBorder(Const Value : TWPBorder);

      Procedure SetWorkingLeftBorder(Const Value : TWPBorder);
      Procedure SetWorkingRightBorder(Const Value : TWPBorder);
      Procedure SetWorkingTopBorder(Const Value : TWPBorder);
      Procedure SetWorkingBottomBorder(Const Value : TWPBorder);
      Function GetLeftBorder : TWPBorder;
      Function GetRightBorder : TWPBorder;
      Function GetTopBorder : TWPBorder;
      Function GetBottomBorder : TWPBorder;
      Function GetWorkingLeftBorder : TWPBorder;
      Function GetWorkingTopBorder : TWPBorder;
      Function GetWorkingBottomBorder : TWPBorder;
      Function GetWorkingRightBorder : TWPBorder;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      Function Link : TWPWorkingDocumentTableItemPiece; Overload;
      Function Clone : TWPWorkingDocumentTableItemPiece; Overload;
      Procedure Assign(oSource : TFslObject); Override;

      Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;

      Function HasWorkingLeftBorder : Boolean;
      Function HasWorkingRightBorder : Boolean;
      Function HasWorkingTopBorder : Boolean;
      Function HasWorkingBottomBorder : Boolean;

      Procedure ChooseWorkingLeftBorder(oTable : TWPBorder);
      Procedure ChooseWorkingRightBorder(oTable : TWPBorder);
      Procedure ChooseWorkingTopBorder(oTable : TWPBorder);
      Procedure ChooseWorkingBottomBorder(oTable : TWPBorder);

      Property LeftBorder : TWPBorder Read GetLeftBorder Write SetLeftBorder;
      Property RightBorder : TWPBorder Read GetRightBorder Write SetRightBorder;
      Property TopBorder : TWPBorder Read GetTopBorder Write SetTopBorder;
      Property BottomBorder : TWPBorder Read GetBottomBorder Write SetBottomBorder;
      Property WorkingLeftBorder : TWPBorder Read GetWorkingLeftBorder Write SetWorkingLeftBorder;
      Property WorkingRightBorder : TWPBorder Read GetWorkingRightBorder Write SetWorkingRightBorder;
      Property WorkingTopBorder : TWPBorder Read GetWorkingTopBorder Write SetWorkingTopBorder;
      Property WorkingBottomBorder : TWPBorder Read GetWorkingBottomBorder Write SetWorkingBottomBorder;
  End;


Type
  TWPWorkingDocumentTableItemState = (tisFirst, tisMiddle, tisLast, tisOnly);


  TWPWorkingDocumentTableCellStartPiece = Class (TWPWorkingDocumentTableItemPiece)
    Private
      // actual configured
      FSpan : Integer;
      FWidth : Real;
      FBackground : TColour;
      FMarginLeft : Word;
      FMarginTop : Word;
      FMarginRight : Word;
      FMarginBottom : Word;
      FVerticalAlignment : TWordProcessorVerticalAlignment;

      // observational
      FState : TWPWorkingDocumentTableItemState;
      FRow : TObject;
      FWidthMinimum : Integer;
      FWidthMaximum : Integer;
      FWidthCurrent : Integer;
      FMaxLeftParaSpace : Integer;
      FMaxRightParaSpace : Integer;

      Procedure SetSpan(Const Value : Integer);
      Procedure SetWidth(Const Value : Real);
      Procedure SetState(Const Value : TWPWorkingDocumentTableItemState);
      Procedure SetBackground(Const Value : TColour);
      Procedure SetMarginLeft(Const Value : Word);
      Procedure SetMarginTop(Const Value : Word);
      Procedure SetMarginRight(Const Value : Word);
      Procedure SetMarginBottom(Const Value : Word);
      Procedure SetVerticalAlignment(Const Value: TWordProcessorVerticalAlignment);
      Function GetRow : TObject;
      Procedure SetRow(Const Value : TObject);
    Function GetHasRow: Boolean;
    Protected
      Function DefaultCharCount : Integer; Override;
    Public
      constructor Create; Override;

      Function Link : TWPWorkingDocumentTableCellStartPiece; Overload;
      Function Clone : TWPWorkingDocumentTableCellStartPiece; Overload;
      Procedure Assign(oSource : TFslObject); Override;
      Function GetPieceType : TWPWorkingDocumentPieceType; Override;

      Procedure StartContentMeasure;
      Procedure MeasurePieceContent(oPiece : TWPWorkingDocumentPiece; iExtra : Integer = 0);
      Procedure SeeParagraph(oPara : TWPWorkingDocumentParaPiece);
      Procedure FinishContentMeasure;
      Function WidthSpecified(iTotalWidth : Integer) : Integer;

      Function WorkingMarginLeft : Word;
      Function WorkingMarginTop : Word;
      Function WorkingMarginRight : Word;
      Function WorkingMarginBottom : Word;

      Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;
      Procedure MergeProperties(oSource: TWPWorkingDocumentPiece); Override;

      Property MaxLeftParaSpace : Integer Read FMaxLeftParaSpace;
      Property MaxRightParaSpace : Integer Read FMaxRightParaSpace;

      // user / configurable properties
      Property Span : Integer Read FSpan Write SetSpan;
      Property Width : Real Read FWidth Write SetWidth; // percentage with (0-1) of total available size
      Property Background : TColour Read FBackground Write SetBackground;
      Property MarginLeft : Word Read FMarginLeft Write SetMarginLeft;
      Property MarginTop : Word Read FMarginTop Write SetMarginTop;
      Property MarginRight : Word Read FMarginRight Write SetMarginRight;
      Property MarginBottom : Word Read FMarginBottom Write SetMarginBottom;
      Property VerticalAlignment : TWordProcessorVerticalAlignment Read FVerticalAlignment Write SetVerticalAlignment;
      Function DescribeV : String; override;

      // run time properties
      Property WidthMinimum : Integer Read FWidthMinimum;
      Property WidthMaximum : Integer Read FWidthMaximum;
      Property WidthCurrent : Integer Read FWidthCurrent;
      Property State : TWPWorkingDocumentTableItemState Read FState Write SetState;
      Property HasRow : Boolean Read GetHasRow;
      Property Row : TObject Read GetRow Write SetRow; // back link to container Row
  End;

  // Instead of TWPWorkingDocumentTableCellStopPiece, use WPWorkingDocumentStopPiece

  TWPWorkingDocumentTableCellStartPieces = Class (TWPWorkingDocumentPieces)
    Private
      Function GetCell(iIndex: Integer): TWPWorkingDocumentTableCellStartPiece;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Cell[iIndex : Integer] : TWPWorkingDocumentTableCellStartPiece Read GetCell; Default;
  End;

Const
  NAMES_WPWorkingDocumentTableItemState : Array [TWPWorkingDocumentTableItemState] Of String = ('First', 'Middle', 'Last', 'Only');

Type
  TWPWorkingDocumentTableRowStartPiece = Class (TWPWorkingDocumentContainerPiece)
  Private
    Function GetHasTable: Boolean;
    Protected
      FHeader : Boolean;
      FBreakBefore : Boolean;
      FBackground : TColour;
      FLowerPaddingSize : Integer;
      FLowerPaddingColour : TColour;

      FCells : TWPWorkingDocumentTableCellStartPieces;
      FTable : TObject;

      FState : TWPWorkingDocumentTableItemState;
      FOwner : TWPWorkingDocumentTableRowStartPiece; //not owned
      FDepth : Integer;
      FTablePrev : TWPWorkingDocumentTableRowStartPiece; // not owned;
      FTablePrevLevel : TWPWorkingDocumentTableRowStartPiece; // not owned;
      FTableNext : TWPWorkingDocumentTableRowStartPiece; // not owned;
      FTableNextLevel : TWPWorkingDocumentTableRowStartPiece; // not owned;

      Function DefaultCharCount : Integer; Override;
      Procedure SetState(Const Value : TWPWorkingDocumentTableItemState);
      Procedure SetBackground(Const Value : TColour);
      Procedure SetLowerPaddingSize(Const Value: Integer);
      Procedure SetLowerPaddingColour(Const Value: TColour);
      Function GetCells : TWPWorkingDocumentTableCellStartPieces;
      Function GetTable : TObject;
      Procedure SetTable(Const Value : TObject);
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPWorkingDocumentTableRowStartPiece; Overload;
      Function Clone : TWPWorkingDocumentTableRowStartPiece; Overload;
      Procedure Assign(oSource : TFslObject); Override;
      Function GetPieceType : TWPWorkingDocumentPieceType; Override;

      Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;
      Procedure MergeProperties(oSource: TWPWorkingDocumentPiece); Override;

      Function AnyContentReadOnly : Boolean;
      Function CellIsReadOnly(iCol : Integer) : Boolean;

      Function ColumnCount : Integer;
      Function HasOwner : Boolean;

      Procedure StateFirst;
      Procedure StateLast;

      Property Header : Boolean Read FHeader Write FHeader; // don't need set - this is only operative when printing....
      Property BreakBefore : Boolean Read FBreakBefore Write FBreakBefore;
      Property Background : TColour Read FBackground Write SetBackground;
      Property LowerPaddingSize : Integer Read FLowerPaddingSize Write SetLowerPaddingSize;
      Property LowerPaddingColour : TColour Read FLowerPaddingColour Write SetLowerPaddingColour;
      Function DescribeV : String; override;

      Property HasTable : Boolean Read GetHasTable;
      Property State : TWPWorkingDocumentTableItemState Read FState Write SetState;
      Property Cells : TWPWorkingDocumentTableCellStartPieces Read GetCells;
      Property Table : TObject Read GetTable Write SetTable; // back link to container table
      Property Owner : TWPWorkingDocumentTableRowStartPiece Read FOwner Write FOwner;
      Property Depth : Integer Read FDepth Write FDepth;
      Property TablePrev : TWPWorkingDocumentTableRowStartPiece Read FTablePrev Write FTablePrev;
      Property TablePrevLevel : TWPWorkingDocumentTableRowStartPiece Read FTablePrevLevel Write FTablePrevLevel;
      Property TableNext : TWPWorkingDocumentTableRowStartPiece Read FTableNext Write FTableNext;
      Property TableNextLevel : TWPWorkingDocumentTableRowStartPiece Read FTableNextLevel Write FTableNextLevel;
  End;


  TWPWorkingDocumentTableRowStartPieces = Class (TWPWorkingDocumentContainerPieces)
    Private
      Function GetElements(iIndex : Integer):TWPWorkingDocumentTableRowStartPiece;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Elements[iIndex : Integer] : TWPWorkingDocumentTableRowStartPiece Read GetElements; Default;
  End;


Type
  TWPWorkingDocumentTableBorderPolicy = (tbpNone, tbpGrid, tbpLines, tbpInnerLines, tbpDots, tbpInnerDots, tbpBox, tbpBoxLines, tbpCustom, tbpFancy);

  TWPWorkingDocumentTableStartPiece = Class (TWPWorkingDocumentTableItemPiece)
    Private
      FBorderPolicy : TWPWorkingDocumentTableBorderPolicy;
      FHorizontalMargin : Word;
      FVerticalMargin : Word;

      FCenterHorizontalBorder : TWPBorder;
      FCenterVerticalBorder : TWPBorder;
      FBackground : TColour;
      FExpandLastColumn : Boolean;

      FColumnCount : Integer;
      FRows : TWPWorkingDocumentTableRowStartPieces;
      FJagged : Boolean;
      FContentDirty : Boolean;
      FStructureDirty : Boolean;

      Procedure ClearCellBorders;
      Procedure ApplyBorderPolicyNone;
      Procedure ApplyBorderPolicyGrid;
      Procedure ApplyBorderPolicyLines;
      Procedure ApplyBorderPolicyInnerLines;
      Procedure ApplyBorderPolicyDots;
      Procedure ApplyBorderPolicyInnerDots;
      Procedure ApplyBorderPolicyBox;
      Procedure ApplyBorderPolicyBoxLines;
      Procedure ApplyBorderPolicyFancy;
      Procedure ApplyHorizontalMarginWidth;
      Procedure ApplyVerticalMarginWidth;

      Procedure SetCenterHorizontalBorder(Const Value : TWPBorder);
      Procedure SetCenterVerticalBorder(Const Value : TWPBorder);
      Procedure SetBorderPolicy(Const Value : TWPWorkingDocumentTableBorderPolicy);
      Procedure SetHorizontalMargin(Const Value : Word);
      Procedure SetVerticalMargin(Const Value : Word);
      Function GetCenterHorizontalBorder : TWPBorder;
      Function GetCenterVerticalBorder : TWPBorder;
      Procedure SetBackground(Const Value : TColour);
      procedure SetExpandLastColumn(const Value: Boolean);
      Function GetRows : TWPWorkingDocumentTableRowStartPieces;
    Protected
      Function DefaultCharCount : Integer; Override;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPWorkingDocumentTableStartPiece; Overload;
      Function Clone : TWPWorkingDocumentTableStartPiece; Overload;
      Function GetPieceType : TWPWorkingDocumentPieceType; Override;

      Procedure Assign(oSource : TFslObject); Override;
      Procedure ResetRendering; Overload; Override;

      Procedure ApplyProperties(oSource : TWPWorkingDocumentPiece); Override;
      Procedure MergeProperties(oSource: TWPWorkingDocumentPiece); Override;
      Procedure ApplyPolicy; Overload; Virtual;

      Function ColumnIndexForCell(oCell : TWPWorkingDocumentTableCellStartPiece) : Integer; Overload; Virtual;

      Function AnyContentReadOnly : Boolean;
      Function AnyRowsReadOnly : Boolean;
      Function AnyCellsReadOnly(iCol : Integer) : Boolean;

      Function HasCenterHorizontalBorder: Boolean;
      Function HasCenterVerticalBorder: Boolean;
      Function DescribeV : String; override;

      Property CenterHorizontalBorder : TWPBorder Read GetCenterHorizontalBorder Write SetCenterHorizontalBorder;
      Property CenterVerticalBorder : TWPBorder Read GetCenterVerticalBorder Write SetCenterVerticalBorder;
      Property BorderPolicy : TWPWorkingDocumentTableBorderPolicy Read FBorderPolicy Write SetBorderPolicy;
      Property HorizontalMargin : Word Read FHorizontalMargin Write SetHorizontalMargin;
      Property VerticalMargin : Word Read FVerticalMargin Write SetVerticalMargin;
      Property ExpandLastColumn : Boolean read FExpandLastColumn write SetExpandLastColumn;

      Property Background : TColour Read FBackground Write SetBackground;

      Property Rows : TWPWorkingDocumentTableRowStartPieces Read GetRows;
      Property ColumnCount : Integer Read FColumnCount Write FColumnCount;
      Property Jagged : Boolean Read FJagged Write FJagged;
      Property ContentDirty : Boolean Read FContentDirty Write FContentDirty;
      Property StructureDirty : Boolean Read FStructureDirty Write FStructureDirty;
  End;

  TWPWorkingDocumentTableStartPieces = Class (TWPWorkingDocumentContainerPieces)
    Private
      Function GetElements(iIndex : Integer):TWPWorkingDocumentTableStartPiece;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Elements[iIndex : Integer] : TWPWorkingDocumentTableStartPiece Read GetElements; Default;
  End;
  
  // Instead of TWPWorkingDocumentTableCellStopPiece, use WPWorkingDocumentStopPiece

Const
  NAMES_TWPWorkingDocumentTableBORDERPOLICY : Array [TWPWorkingDocumentTableBorderPolicy] Of String = ('None', 'Grid', 'Horizontal Lines', 'Inner Horizontal Lines', 'Horizontal Dotted Lines', 'Inner Horizontal Dotted Lines', 'Outer Box', 'Outer Box With Horizontal Lines', 'Custom', 'Fancy');
  CODES_TWPWorkingDocumentTableBORDERPOLICY : Array [TWPWorkingDocumentTableBorderPolicy] Of String = ('None', 'Grid', 'Lines', 'InnerLines', 'Dots', 'InnerDots', 'Box', 'BoxLines', 'Custom', 'Fancy');


Type
  TWPWorkingAttachment = class (TFslObject)
  private
    FId : String;
    FContent : TFslBuffer;
    FInUse: Boolean;
    FMimeType: String;
    FExtension: String;
//    FPDF : TgtExProPDFDocument;
  protected
    Procedure Loaded; virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TWPWorkingAttachment; overload;
    function Clone : TWPWorkingAttachment; overload;
    procedure Assign(oSource : TFslObject); Override;

    Procedure LoadFromBuffer(oBuffer : TFslBuffer);
    procedure LoadFromString(sSource : AnsiString);
    Procedure LoadFromStream(oStream : TFslStream);
    Procedure SaveToStream(oStream : TFslStream);
    Procedure LoadFromFile(Const sFilename : String);
    Procedure SaveToFile(Const sFilename : String);
    procedure UseAsPdf;

    Property Id : String read FId write FId;
    Property InUse : Boolean read FInUse write FInUse;
    Property Content : TFslBuffer read FContent;

    property MimeType : String read FMimeType write FMimeType;
    property Extension : String read FExtension write FExtension;

//    property PDF : TgtExProPDFDocument read FPDF;
  end;

  TWPWorkingAttachmentList = class (TFslObjectList)
  private
    function GetItemA(index: integer): TWPWorkingAttachment;
  protected
    Function ItemClass : TFslObjectClass; override;
  public
    Property Item[index : integer] : TWPWorkingAttachment read GetItemA; default;

    function GetById(id : String): TWPWorkingAttachment;
    function CompareById(p1, p2 : pointer ) : Integer;
  end;

Type
  TWPCursorDirection = (cdJump, cdLeft, cdRight);

  TWPJumpType = (jtUnknown, jtChar, jtWord, jtSentence, jtParagraph, jtLine, jtSection, jtCell, jtRow, jtTable);

  TWPWorkingDocument = Class (TWPTrackable)
  Private
    FPieces : TWPWorkingDocumentPieces;
    FFieldDefinitionProviders : TWPFieldDefinitionProviderList;
    FAnnotationDefinitionProviders : TWPAnnotationDefinitionProviderList;
    FSourceObjectModel : TFslObject;
    FSourceBytes : TFslBuffer;

    FFieldCount : Integer;
    FMinLength : Integer;
    FCharCount : Integer;
    FVoiceCharCount : Integer;

    FRegenerationNeeded : Boolean;
    FWorking: Boolean;
    FAllowedWords : TFslStringList;
    FAllAnnotations : TWPWorkingAnnotationList;
    FDrawnAnnotations : TWPWorkingAnnotationList;
    FAttachments : TWPWorkingAttachmentList;

    Procedure DoRegenerateMetrics(bSimpleOnly: Boolean);

    Procedure MapRow(oTable : TWPWorkingDocumentTableStartPiece; oRow : TWPWorkingDocumentTableRowStartPiece; oTop, oBottom : TWPBorder);
    Procedure MapTable(oTable : TWPWorkingDocumentTableStartPiece);
    Procedure MapChildren(oRows : TWPWorkingDocumentTableRowStartPieces; oRow : TWPWorkingDocumentTableRowStartPiece; iStart : Integer);
    Function GetCharCount: Integer;
    Function GetFieldCount: Integer;
    Function GetMinLength: Integer;
    Function GetVoiceCharCount: Integer;

    // navigation utilities
    Function PieceForCursor(iCursor : Integer) : TWPWorkingDocumentPiece;
    Function CheckCursorPointScoped(Var iCursor : Integer; aTypes : TWPWorkingDocumentPieceTypes; bLeft, bNoSelectReadOnly : Boolean) : Boolean;

    Procedure SetFieldDefinitionProviders(Const Value : TWPFieldDefinitionProviderList);
    Procedure SetAnnotationDefinitionProviders(Const Value : TWPAnnotationDefinitionProviderList);
    procedure SetSourceObjectModel(const Value: TFslObject);
    procedure SetSourceBytes(const Value: TFslBuffer);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPWorkingDocument; Overload;
    Function Clone : TWPWorkingDocument; Overload;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure Clear; Overload; Virtual;
    Function IsEmpty : Boolean; Overload; Virtual;

    Procedure Change(aType : TChangeType; oSource : TWPTrackable); Overload; Override;
    Procedure RegenerateMetrics(bForce, bSimpleOnly : Boolean); Overload; Virtual;
    procedure RelinkAttachments; Overload; Virtual;

    // Document Utilities
    Procedure BuildEmptyDocument; Overload; Virtual;
    Function NewSection(Const sNameSpace, sName, sDisplay : String) : TWPWorkingDocumentSectionStartPiece; Overload; Virtual;
    Function NewParagraph : TWPWorkingDocumentParaPiece;  Overload; Virtual;
    Function EndSection : TWPWorkingDocumentStopPiece; Overload; Virtual;
    Procedure InsertAtOffset(oPiece : TWPWorkingDocumentPiece; iOffset : Integer); Overload; Virtual;
    Function GetByOffset(iPosition : Integer) : TWPWorkingDocumentPiece; Overload; Virtual;
    Procedure CopyPortion(oDest : TWPWorkingDocumentPieces; iStart, iEnd : Integer); Overload; Virtual;

    // Navigation Utilies
    Function PieceHoldsCursor(oPiece : TWPWorkingDocumentPiece; bNoSelectReadOnly : Boolean) : Boolean;
    Function FindSection(Const sNamespace, sName : String):TWPWorkingDocumentSectionStartPiece;
    Function GetHotspot(ch : Char; Var oItem : TWPMapObject; Var oOwner : TWPWorkingDocumentPiece) : TWPHotspot;
    Function NormalPositionToVoicePosition(iPosition : Integer) : Integer;
    Function VoicePositionToNormalPosition(iPosition : Integer) : Integer;
    Function GetPieceByPosition(iPosition : Integer; Out oPiece : TWPWorkingDocumentPiece; Out iOffset : Integer; Out iIndex : Integer; bQuick : Boolean = True):Boolean; Overload; Virtual;
    Function GetPieceByVoicePosition(iVoicePosition : Integer; Out oPiece : TWPWorkingDocumentPiece; Out iOffset : Integer; Out iIndex : Integer):Boolean; Overload; Virtual;

    Function FirstCursorPosition : Integer;

    Function DocHome(bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function SectionHome(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; bIgnoreFieldSections : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function ParaHome(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function WordLeft(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function NextLeft(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function NextRight(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function WordRight(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function ParaEnd(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function SectionEnd(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; bIgnoreFieldSections : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function DocEnd(bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;

    Function GetJumpPoint(iCursor: Integer; aJumpType : TWPJumpType; iCount : Integer; bNoSelectReadOnly : Boolean; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function CheckCursorPoint(iCursor : Integer; aDir : TWPCursorDirection; bNoSelectReadOnly : Boolean) : Integer; Overload; Virtual;

    Function NearestValidCursor(iCursor : Integer; aDirection : TWPCursorDirection; bNoSelectReadOnly : Boolean) : Integer;

    // Debugging Utilities
    Function AsText : String;
    Function AsClassSequence : String;
    Procedure DumpToXml(oXml : TFslXmlFormatter);
    Function DocSequenceAsText : String;

    // contents
    Property Pieces : TWPWorkingDocumentPieces Read FPieces;
    Property AllAnnotations : TWPWorkingAnnotationList read FAllAnnotations;
    Property DrawnAnnotations : TWPWorkingAnnotationList read FDrawnAnnotations;
    Property Attachments : TWPWorkingAttachmentList read FAttachments; // attachments are immutable

    // status observations
    Property FieldCount : Integer Read GetFieldCount;
    Property MinLength : Integer Read GetMinLength;
    Property CharCount : Integer Read GetCharCount;
    Property VoiceCharCount : Integer Read GetVoiceCharCount;
    Property RegenerationNeeded : Boolean Read FRegenerationNeeded;
    Property FieldDefinitionProviders : TWPFieldDefinitionProviderList Read FFieldDefinitionProviders Write SetFieldDefinitionProviders;
    Property AnnotationDefinitionProviders : TWPAnnotationDefinitionProviderList Read FAnnotationDefinitionProviders Write SetAnnotationDefinitionProviders;
    Property AllowedWords : TFslStringList Read FAllowedWords;
    Property Working : Boolean Read FWorking Write FWorking;

    // other stuff
    Property SourceObjectModel : TFslObject read FSourceObjectModel write SetSourceObjectModel;
    Property SourceBytes : TFslBuffer read FSourceBytes write SetSourceBytes;
  End;

Type
  TWPIterator = Class (TFslObject)
  Private
    FDocument : TWPWorkingDocument;
    FCharCount : Integer;
    FMinimum : Integer;
    FMaximum : Integer;

    Function GetDocument : TWPWorkingDocument;
    Procedure SetDocument(Const Value: TWPWorkingDocument);
  Protected
    Function WorkingMinimum : Integer;
    Function WorkingMaximum : Integer;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(oDocument : TWPWorkingDocument); Overload; Virtual;
    destructor Destroy; Override;

    Procedure SetBounds(Const iMinimum, iMaximum : Integer); Overload; Virtual;

    Procedure First; Overload; Virtual;
    Procedure Next; Overload; Virtual;
    Procedure Prev; Overload; Virtual;
    Procedure Last; Overload; Virtual;

    Function More : Boolean; Overload; Virtual;

    Function HasDocument : Boolean; Overload; Virtual;

    Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;

    Property Maximum : Integer Read FMaximum Write FMaximum;
    Property Minimum : Integer Read FMinimum Write FMinimum;
  End;


  TWPCharIterator = Class (TWPIterator)
  Private
    FIndex : Integer;
    FCurrent : TWPWorkingDocumentPiece;
    FOffset : Integer;
    
    Function GetCurrent: Char;
    Function GetCurrentPosition: Integer;

  protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure First; Override;
    Procedure Next; Override;
    Procedure Prev; Override;
    Procedure Last; Override;
    Procedure MoveTo(iPosition : Integer); Overload; Virtual;

    Function More : Boolean; Override;
    Property Current : Char Read GetCurrent;
    Property CurrentPosition : Integer Read GetCurrentPosition;
  End;

Type
  TWPPieceIterator = Class (TWPIterator)
  Private
    FPieceTypes: TWPWorkingDocumentPieceTypes;

    FCurrentIndex : Integer;
    FCurrent : TWPWorkingDocumentPiece;
    FStart : Integer;
    FStop : Integer;

    Function GetCurrent : TWPWorkingDocumentPiece;
    Function GetPeek : TWPWorkingDocumentPiece;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure First; Override;
    Procedure Next; Override;
    Procedure Prev; Override;
    Procedure Last; Override;

    Function CurrentParagraph(bAllowNil : Boolean = False): TWPWorkingDocumentParaPiece;
    Function More : Boolean; Override;

    Property PieceTypes : TWPWorkingDocumentPieceTypes Read FPieceTypes Write FPieceTypes;

    Property Current : TWPWorkingDocumentPiece Read GetCurrent;
    Property Peek : TWPWorkingDocumentPiece Read GetPeek;
    Property CurrentIndex : Integer Read FCurrentIndex;
    Property Start : Integer Read FStart;
    Property Stop : Integer Read FStop;
  End;

  TWPDocumentTranslator = Class (TFslObject)
    Private
      FDocument : TWPDocument;
      FWorkingDocument : TWPWorkingDocument;
      FWorkingStyles : TWPStyles;
      FRowMap : TFslObjectMatch;
      FSplitter : TWPWordIterator;

      Function GetDocument : TWPDocument;
      Procedure SetDocument(Const Value : TWPDocument);
      Function GetWorkingDocument : TWPWorkingDocument;
      Procedure SetWorkingDocument(Const Value : TWPWorkingDocument);
      Function GetWorkingStyles : TWPStyles;
      Procedure SetWorkingStyles(Const Value : TWPStyles);


      Procedure AddParagraph;

      Procedure TranslateBlock(oBlock : TWPDocumentBlock); Overload;
      Procedure TranslateSection(oSection : TWPDocumentSection); Overload;
      Procedure TranslateTable(oTable : TWPDocumentTable); Overload;
      Procedure TranslateTableRow(oTableRow : TWPDocumentTableRow; oOwner : TWPWorkingDocumentTableRowStartPiece); Overload;
      Procedure TranslateTableCell(oTableCell : TWPDocumentTableCell); Overload;
      Procedure TranslateBreak(oBreak : TWPDocumentBreak); Overload;
      Procedure TranslateParagraph(oParagraph : TWPDocumentParagraph); Overload;
      Procedure TranslateContents(oContent : TWPDocumentContent); Overload;
      Procedure TranslateText(oText : TWPDocumentText); Overload;
      Procedure AddWord(Const sWord : String; oText : TWPDocumentText); Overload;
      Procedure TranslateImage(oImage : TWPDocumentImage); Overload;
      Procedure TranslateField(oField : TWPDocumentField); Overload;
      Procedure TranslateLineBreak(oLinebreak : TWPDocumentLinebreak); Overload;

      Procedure TranslateBlock(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks); Overload;
      Procedure TranslateParagraph(oIter : TWPPieceIterator; oBlocks : TWPDocumentObjects); Overload;
      Procedure TranslateBreak(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks); Overload;
      Procedure TranslateTable(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks); Overload;
      Procedure TranslateTableRow(oIter : TWPPieceIterator; oRows : TWPDocumentTableRows); Overload;
      Procedure TranslateTableCell(oIter : TWPPieceIterator; oCells : TWPDocumentTableCells); Overload;
      Procedure TranslateSection(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks); Overload;
      Procedure TranslateContent(oIter : TWPPieceIterator; oContents : TWPDocumentObjects); Overload;
      Procedure TranslateText(oIter : TWPPieceIterator; oContents : TWPDocumentObjects); Overload;
      Procedure TranslateImage(oIter : TWPPieceIterator; oContents : TWPDocumentObjects); Overload;
      Procedure TranslateField(oIter : TWPPieceIterator; oContents : TWPDocumentObjects); Overload;
      Procedure TranslateLineBreak(oIter : TWPPieceIterator; oContents : TWPDocumentObjects); Overload;

      Function TranslateAttachment(oSource : TWPWorkingAttachment): TWPDocumentAttachment; Overload;
      Function TranslateAttachment(oSource : TWPDocumentAttachment): TWPWorkingAttachment; Overload;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function HasDocument : Boolean; Overload; Virtual;
      Function HasWorkingDocument : Boolean; Overload; Virtual;
      Function HasWorkingStyles : Boolean; Overload; Virtual;

      Procedure TranslateToWorking; Overload; Virtual;
      Procedure TranslateToDocument; Overload; Virtual;

      Function  CreateImage(oImage : TWPDocumentImage) : TWPWorkingDocumentImagePiece; Overload; Virtual;
      Function  CreateField(oField : TWPWorkingDocumentFieldStartPiece) : TWPDocumentField; Overload; Virtual;
      Function  CreateField(oField : TWPDocumentField) : TWPWorkingDocumentFieldStartPiece; Overload; Virtual;
      Function  CreateSection(oPiece : TWPWorkingDocumentSectionStartPiece) : TWPDocumentSection; Overload; Virtual;
      Function  CreateSection(oSection : TWPDocumentSection) : TWPWorkingDocumentSectionStartPiece; Overload; Virtual;

      Property Document : TWPDocument Read GetDocument Write SetDocument;
      Property WorkingDocument : TWPWorkingDocument Read GetWorkingDocument Write SetWorkingDocument;
      Property WorkingStyles : TWPStyles Read GetWorkingStyles Write SetWorkingStyles;
  End;

  TWPWorkingDocumentPieceTracker = Class (TFslObject)
    Private
      FPiece : TWPWorkingDocumentPiece;
      FOffset : Integer;
//      FSMOffset : Integer;
//      FRemoved : Boolean;
      Procedure SetPiece(Const Value: TWPWorkingDocumentPiece);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Property Piece : TWPWorkingDocumentPiece Read FPiece Write SetPiece;
      Property Offset : Integer Read FOffset Write FOffset;
//      Property SMOffset : Integer Read FSMOffset Write FSMOffset;
//      Property Removed : Boolean Read FRemoved Write FRemoved;
  End;

  TWPWorkingDocumentPieceTrackers = Class (TFslObjectList)
  Private
    Function GetPiece(iIndex: Integer): TWPWorkingDocumentPieceTracker;
    Function CompareByOffset(pA, pB : Pointer) : Integer; Overload;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Procedure SortedByOffset;
    Property Piece[iIndex : Integer] : TWPWorkingDocumentPieceTracker Read GetPiece; Default;
  End;


Const
  WorkingDocumentSectionDisplayTypeForDocumentSectionDisplay : Array [TWPDocumentSectionDisplay] Of TWPWorkingDocumentSectionDisplayType = (sdtNone, sdtLine, sdtName);
  TriStateForDocumentObjectReadOnly : Array [TWPDocumentObjectReadOnly] Of TWPSTriState = (tsUnknown, tsTrue, tsFalse);
  WorkingDocumentBreakPieceTypeForDocumentBreakBreakType: Array [TWPDocumentBreakBreakType] Of TWPWorkingDocumentBreakPieceType = (btLine, btPageBreak);
  WorkingDocumentTableBorderPolicyForDocumentTableBorderPolicy : Array [TWPDocumentTableBorderPolicy] Of TWPWorkingDocumentTableBorderPolicy = (tbpNone, tbpGrid, tbpLines, tbpInnerLines, tbpBox, tbpBoxLines, tbpCustom, tbpFancy, tbpDots, tbpInnerDots);
  ImageVerticalAlignmentForDocumentImageVerticalAlignment : Array [TWordProcessorImageVerticalAlignment] Of TWPImageVerticalAlignment = (ivaBaseLine, ivaBottom, ivaCentered, ivaTop);

  DocumentBreakBreakTypeForWorkingDocumentBreakPieceType : Array [TWPWorkingDocumentBreakPieceType] Of TWPDocumentBreakBreakType = (BreakTypeLine, BreakTypePageBreak);
  DocumentSectionDisplayForWorkingDocumentSectionDisplayType : Array [TWPWorkingDocumentSectionDisplayType] Of TWPDocumentSectionDisplay = (DisplayNone, DisplayLine, DisplayTitle);
  DocumentTableBorderPolicyForWorkingDocumentTableBorderPolicy : Array [TWPWorkingDocumentTableBorderPolicy] Of TWPDocumentTableBorderPolicy = (BorderPolicyNone, BorderPolicyGrid, BorderPolicyLines, BorderPolicyInnerLines, BorderPolicyDots, BorderPolicyInnerDots, BorderPolicyBox, BorderPolicyBoxLines, BorderPolicyCustom, BorderPolicyFancy);
  DocumentObjectReadOnlyForTriState : Array [TWPSTriState] Of TWPDocumentObjectReadOnly = (ReadOnlyDefault, ReadOnlyTrue, ReadOnlyFalse);
  DocumentImageVerticalAlignmentForImageVerticalAlignment : Array [TWPImageVerticalAlignment] Of TWordProcessorImageVerticalAlignment = (ImageVerticalAlignmentBaseLine, ImageVerticalAlignmentBottom, ImageVerticalAlignmentCentered, ImageVerticalAlignmentTop);

procedure prop(var result : String; name, value : string);overload;
procedure prop(var result : String; name : string; value : integer); overload;
procedure prop(var result : String; name : string; value : integer; names : array of String); overload;
procedure prop(var result : String; name : string; value : boolean); overload;

type
  TWPWorkingDocumentValidator = Class (TFslObject)
    Private
      FDocument : TWPWorkingDocument;

      Function GetDocument : TWPWorkingDocument;
      Procedure SetDocument(Const Value : TWPWorkingDocument);

      Procedure Validate(oIterator : TWPPieceIterator; aAllowed : TWPWorkingDocumentPieceTypes; aTerminate : TWPWorkingDocumentPieceType); Overload;
      Procedure Validate(oIterator : TWPPieceIterator; aAllowed : TWPWorkingDocumentPieceTypes); Overload;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(oDocument : TWPWorkingDocument); Overload; Virtual;
      destructor Destroy; Override;

      Class Procedure Validate(oDocument : TWPWorkingDocument); Overload; Virtual;

      Procedure Validate; Overload; Virtual;

      Function HasDocument : Boolean; Overload; Virtual;

      Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
  End;

Implementation

procedure prop(var result : String; name, value : string);overload;
begin
  if value <> '' then
  begin
    if result <> '' then
      result := result + '; ';
    result := result + name+': '+value;
  end;
end;

procedure prop(var result : String; name : string; value : integer); overload;
begin
  if value <> 0 then
  begin
    if result <> '' then
      result := result + '; ';
    result := result + name+': '+inttostr(value);
  end;
end;

procedure prop(var result : String; name : string; value : integer; names : array of String); overload;
begin
  if value <> 0 then
  begin
    if result <> '' then
      result := result + '; ';
    result := result + name+': '+names[value];
  end;
end;

procedure prop(var result : String; name : string; value : boolean); overload;
begin
  if value then
  begin
    if result <> '' then
      result := result + '; ';
    result := result + name+': true';
  end;
end;

{ TWPWorkingDocumentPiece }

Constructor TWPWorkingDocumentPiece.Create;
Begin
  Inherited;

  FFont := TWPSFontDetails.Create;
  FMetrics := TWPMetrics.Create;
  FMetrics.CharCount := DefaultCharCount;
  FMetrics.VoiceCharCount := FMetrics.CharCount;
  FMaps := TWPMapItems.Create;

  FStyle := '';
End;

function TWPWorkingDocumentPiece.Describe: String;
begin
  if (self = nil) then
    result := '(nil)'
  else
    result := DescribeV;
end;

function TWPWorkingDocumentPiece.DescribeV: String;
begin
  result := '';
  prop(result, 'style', FStyle);
  prop(result, 'read-only', FIsReadOnly);
  if FHotspot <> nil then
    prop(result, 'hotspot', FHotspot.describe);
  if FFont <> nil then
    prop(result, 'font', FFont.describe);
end;

Destructor TWPWorkingDocumentPiece.Destroy;
Begin
  FSourceObject.Free;
  FMaps.Free;
  FMetrics.Free;
  FFont.Free;
  FHotspot.UnHook(Change);
  FHotspot.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentPiece.Change(aType : TChangeType; oSource : TWPTrackable);
Begin
  Inherited;
  If aType In [ctListContents, ctTextContents, ctLayout] Then
    FMetrics.Valid := False;
  If Assigned(FMaps) Then
    FMaps.SetNotPainted;
End;

Function TWPWorkingDocumentPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptText;
  RaiseError('GetPieceType', 'Need to override '+ClassName+'.GetPieceType');
End;


Function TWPWorkingDocumentPiece.GetLogicalText: String;
Begin
  Result := StringMultiply('|', Metrics.CharCount);
End;


Function TWPWorkingDocumentPiece.GetVisualText: String;
Begin
  Result := GetLogicalText;
End;


Function TWPWorkingDocumentPiece.GetVoiceText : String;
Begin
  Result := StringMultiply(#1, Metrics.CharCount);
End;


Procedure TWPWorkingDocumentPiece.SetFont(Const Value: TWPSFontDetails);
Begin
  FFont.Free;
  FFont := Value;
End;

Procedure TWPWorkingDocumentPiece.SetMetrics(Const Value: TWPMetrics);
Begin
  FMetrics.Free;
  FMetrics := Value;
End;

procedure TWPWorkingDocumentPiece.SetSourceObject(const Value: TFslObject);
begin
  FSourceObject.Free;
  FSourceObject := Value;
end;

Function TWPWorkingDocumentPiece.StyleMatches(oOther: TWPWorkingDocumentPiece): Boolean;
Begin
  Result := (FStyle = oOther.FStyle) And FFont.Matches(oOther.FFont);
End;

Function TWPWorkingDocumentPiece.AnnotationMatches(oOther: TWPWorkingDocumentPiece): Boolean;
Begin
  Result := (FAnnotationId = oOther.FAnnotationId);
End;


Procedure TWPWorkingDocumentPiece.AssignStyle(oOther : TWPWorkingDocumentPiece);
Begin
  FStyle := oOther.FStyle;
  FFont.Assign(oOther.Font);
End;


Procedure TWPWorkingDocumentPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  Maps.Clear; // deliberate - any reason we are assigning invalidates the map
  Metrics.Assign(TWPWorkingDocumentPiece(oSource).FMetrics);
  FMetrics.Valid := false;
  Font := TWPWorkingDocumentPiece(oSource).FFont.Clone;
  FStyle := TWPWorkingDocumentPiece(oSource).FStyle;
  Hotspot := TWPWorkingDocumentPiece(oSource).FHotspot.Clone;
  FieldHint := TWPWorkingDocumentPiece(oSource).FieldHint;
  FieldError := TWPWorkingDocumentPiece(oSource).FieldError;
  AnnotationId := TWPWorkingDocumentPiece(oSource).AnnotationId;
  FSourceCol := TWPWorkingDocumentPiece(oSource).FSourceCol;
  FSourceLine := TWPWorkingDocumentPiece(oSource).FSourceLine;
  SourceObject := TWPWorkingDocumentPiece(oSource).SourceObject.Link;
End;

Procedure TWPWorkingDocumentPiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Var
  iLoop : Integer;
Begin
  Change(ctLayout, Self);
  For iLoop := 0 To FMaps.Count - 1 Do
    FMaps[iLoop].WantPainting;
  Font := TWPWorkingDocumentPiece(oSource).FFont.Clone;
  FStyle := TWPWorkingDocumentPiece(oSource).FStyle;
  Hotspot := TWPWorkingDocumentPiece(oSource).FHotspot.Clone;
End;

Procedure TWPWorkingDocumentPiece.MergeProperties(oSource: TWPWorkingDocumentPiece);
Begin
  If Assigned(oSource) Then
  Begin
    Change(ctLayout, Self);
    Font.Merge(oSource.Font);

    If (Style <> oSOurce.Style) Then
      Style := '';
    // How do we merge hotspot ?
  End;
End;

procedure TWPWorkingDocumentPiece.ClearSource;
begin
  SourceObject := nil;
  SourceLine := 0;
  SourceCol := 0;
end;

Function TWPWorkingDocumentPiece.Clone: TWPWorkingDocumentPiece;
Begin
  Result := TWPWorkingDocumentPiece(Inherited Clone);
End;

Function TWPWorkingDocumentPiece.Link: TWPWorkingDocumentPiece;
Begin
  Result := TWPWorkingDocumentPiece(Inherited Link);
End;

Function TWPWorkingDocumentPiece.Split(iOffset: Integer): TWPWorkingDocumentPiece;
Begin
  Result := Nil;
  RaiseError('split', 'Cannot call split for a '+NAMES_WPPIECETYPE[PieceType]+' piece');
End;


Function TWPWorkingDocumentPiece.GetMap : TWPMapItem;
Begin
  If FMaps.Count = 1 Then
    Result := FMaps[0]
  Else
    Result := Nil;
End;

Procedure TWPWorkingDocumentPiece.SetMap(Const Value: TWPMapItem);
Begin
  FMaps.Clear;
  If Assigned(Value) Then
    FMaps.Add(Value);
End;


Function TWPWorkingDocumentPiece.DefaultCharCount: Integer;
Begin
  Result := 0;
End;


Function TWPWorkingDocumentPiece.IsLineBreak : Boolean;
Begin
  Result := False;
End;


Function TWPWorkingDocumentPiece.GetNext : TWPWorkingDocumentPiece;
Begin
  Result := FNext;
End;

Function TWPWorkingDocumentPiece.GetPrev : TWPWorkingDocumentPiece;
Begin
  Result := FPrev;
End;


Function TWPWorkingDocumentPiece.HasNext : Boolean;
Begin
  Result := Assigned(FNext);
End;


Function TWPWorkingDocumentPiece.HasPrev : Boolean;
Begin
  Result := Assigned(FPrev);
End;


Function TWPWorkingDocumentPiece.CanCopyAsText : Boolean;
Begin
  Result := False;      // Most working piece can't be copied as text
End;


Function TWPWorkingDocumentPiece.GetMetrics : TWPMetrics;
Begin
  Assert(Invariants('GetMetrics', FMetrics, TWPMetrics, 'Metrics'));
  Result := FMetrics;
End;

Function TWPWorkingDocumentPiece.GetFont : TWPSFontDetails;
Begin
  Assert(Invariants('GetFont', FFont, TWPSFontDetails, 'Font'));
  Result := FFont;
End;


Function TWPWorkingDocumentPiece.GetMaps : TWPMapItems;
Begin
  Assert(Invariants('GetMaps', FMaps, TWPMapItems, 'Maps'));
  Result := FMaps;
End;

Function TWPWorkingDocumentPiece.GetHasHotSpot: Boolean;
Begin
  Result := Assigned(FHotspot);
End;

Procedure TWPWorkingDocumentPiece.SetHasHotspot(Const bValue: Boolean);
Begin
  If Not bValue Then
    Hotspot := Nil
  Else If Not Assigned(FHotspot) Then
    Hotspot := TWPHotspot.Create;
End;

Procedure TWPWorkingDocumentPiece.SetHotspot(Const Value: TWPHotspot);
Begin
  FHotspot.UnHook(Change);
  FHotspot.Free;
  FHotSpot := Value;
  FHotSpot.Hook(Change);
  Change(ctLayout, Value);
End;

Function TWPWorkingDocumentPiece.ForceHotspot: TWPHotspot;
Begin
  HasHotspot := True;
  Result := Hotspot;
End;

Procedure TWPWorkingDocumentPiece.DumpToXml(oXml: TFslXmlFormatter);
Begin
  CollectAttributes(oXml);
  oXml.ProduceOpen(XmlName);
  ProduceChildren(oXml);
  oXml.ProduceClose(XmlName);
End;

Procedure TWPWorkingDocumentPiece.ResetRendering;
Begin
  FMaps.Clear;
  FMetrics.Valid := False;
End;

procedure TWPWorkingDocumentPiece.SetAnnotationId(const Value: Integer);
begin
  if Value <> FAnnotationId Then
  Begin
    FAnnotationId := Value;
    Change(ctPresentation, Self);
  End;
end;

procedure TWPWorkingDocumentPiece.SetAnnotationStatus(const Value: TWPAnnotationStatus);
begin
  if Value <> FAnnotationStatus Then
  Begin
    FAnnotationStatus := Value;
    Change(ctPresentation, Self);
  End;
end;

function TWPWorkingDocumentPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMaps.sizeInBytes);
  inc(result, FMetrics.sizeInBytes);
  inc(result, FFont.sizeInBytes);
  inc(result, (FStyle.length * sizeof(char)) + 12);
  inc(result, FPrev.sizeInBytes);
  inc(result, FNext.sizeInBytes);
  inc(result, FHotspot.sizeInBytes);
  inc(result, (FFieldHint.length * sizeof(char)) + 12);
  inc(result, (FFieldError.length * sizeof(char)) + 12);
  inc(result, FSourceObject.sizeInBytes);
end;

{ TWPWorkingDocumentPieces }

Function TWPWorkingDocumentPieces.GetPiece(iIndex: Integer): TWPWorkingDocumentPiece;
Begin
  Result := TWPWorkingDocumentPiece(ObjectByIndex[iIndex]);
End;

Function TWPWorkingDocumentPieces.ItemClass: TFslObjectClass;
Begin
  Result := TWPWorkingDocumentPiece;
End;

Function TWPWorkingDocumentPieces.Link: TWPWorkingDocumentPieces;
Begin
  Result := TWPWorkingDocumentPieces(Inherited Link);
End;

Function TWPWorkingDocumentPieces.Clone: TWPWorkingDocumentPieces;
Begin
  Result := TWPWorkingDocumentPieces(Inherited Clone);
End;


Function TWPWorkingDocumentPieces.First : TWPWorkingDocumentPiece;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result :=  Piece[0];
End;


Function TWPWorkingDocumentPieces.Last : TWPWorkingDocumentPiece;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result :=  Piece[Count - 1];
End;

Function TWPWorkingDocumentPieces.AsText: String;
Var
  iLoop : Integer;
Begin
  Result := '';

  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Piece[iLoop].visualText);
End;


Function TWPWorkingDocumentPiece.XmlName : String; 
Begin
  Result := 'piece';
End;

Procedure TWPWorkingDocumentPiece.CollectAttributes(oXml : TFslXmlFormatter);
Begin
End;

Procedure TWPWorkingDocumentPiece.ProduceChildren(oXml : TFslXmlFormatter);
Begin
End;


{ TWPWorkingDocumentPieceStack }

Constructor TWPWorkingDocumentPieceStack.Create;
Begin
  Inherited;
  Hooking := False;
End;

Function TWPWorkingDocumentPieceStack.Pop: TWPWorkingDocumentPiece;
Begin
  Assert(CheckCondition(Count > 0, 'Pop', 'No pieces to pop'));
  Result := Piece[Count - 1];
  DeleteByIndex(count - 1);
End;

Procedure TWPWorkingDocumentPieceStack.Push(oPiece: TWPWorkingDocumentPiece);
Begin
  Add(oPiece);
End;

{ TWPMapObject }

Constructor TWPMapObject.Create(oPiece : TFslObject);
Begin
  Create;
  FPiece := oPiece;
End;

Destructor TWPMapObject.Destroy;
Begin
  FBackHotspot.Free;
  Inherited;
End;

Procedure TWPMapObject.Assign(oSource: TFslObject);
Begin
  Inherited;
  Piece  := TWPMapObject(oSource).FPiece;
  Copy(TWPMapObject(oSource));
  // the map object doesn't really own this object.
  // it's not to be changed from here. This are just
  // references so they are linked not cloned 
  FBackHotSpot := TWPMapObject(oSource).FBackHotspot.Link;
End;


Procedure TWPMapObject.Copy(oSource : TWPMapObject);
Begin
  FPainted := False; // always
  Width := oSource.FWidth;
  Height := oSource.FHeight;
  Top := oSource.FTop;
  Left := oSource.FLeft;
  Background := oSource.Background;
  FClipLeft := oSource.FClipLeft;
  FClipTop := oSource.FClipTop;
  FClipRight := oSource.FClipRight;
  FClipBottom := oSource.FClipBottom;
End;

Function TWPMapObject.GetBottom: Integer;
Begin
  Result := Top + Height;
End;

Function TWPMapObject.GetCentre: Integer;
Begin
  Result := Left + Width Div 2;
End;

Function TWPMapObject.GetRight: Integer;
Begin
  Result := Left + Width;
End;

Function TWPMapObject.Link: TWPMapObject;
Begin
  Result := TWPMapObject(Inherited Link);
End;

Procedure TWPMapObject.SetBackground(Const Value: TColour);
Begin
  If FBackground <> Value Then
    Begin
    WantPainting;
    FBackground := Value;
    End;
End;

Procedure TWPMapObject.SetHeight(Const Value: Integer);
Begin
  If FHeight <> Value Then
    Begin
    WantPainting;
    FHeight := Value;
    End;
End;

Procedure TWPMapObject.SetLeft(Const Value: Integer);
Begin
  If FLeft <> Value Then
    Begin
    WantPainting;
    FLeft := Value;
    End;
End;


Procedure TWPMapObject.AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean);
Begin
  Inc(FTop, iOffset);
  if (HasPiece) And (TWPWorkingDocumentPiece(Piece).AnnotationId <> 0) And (TWPWorkingDocumentPiece(Piece).AnnotationStatus in [AnnotationStatusAll, AnnotationStatusEnd]) Then
    GetAnnotation(TWPWorkingDocumentPiece(Piece).AnnotationId).Anchor := Bottom - 1;
  If bPaint Then
    WantPainting;
End;


Procedure TWPMapObject.SetTop(Const Value: Integer);
Begin
  If FTop <> Value Then
    Begin
    WantPainting;
    FTop := Value;
    End;
End;

Procedure TWPMapObject.SetWidth(Const Value: Integer);
Begin
  If FWidth <> Value Then
    Begin
    WantPainting;
    FWidth := Value;
    End;
End;

Procedure TWPMapObject.WantPainting;
Begin
  FPainted := False;
End;


Function TWPMapObject.HasClip : Boolean;
Begin
  Result := (FClipLeft <> 0) Or (FClipTop <> 0) Or (FClipRight <> 0) Or (FClipBottom <> 0);
End;


Procedure TWPMapObject.Clip(iLeft, iTop, iRight, iBottom : Integer);
Begin
  FClipLeft := iLeft;
  FClipTop := iTop;
  FClipRight := iRight;
  FClipBottom := iBottom;
End;


Procedure TWPMapObject.UnClip;
Begin
  FClipLeft := 0;
  FClipTop := 0;
  FClipRight := 0;
  FClipBottom := 0;
End;


Function TWPMapObject.ContainsPoint(iX, iY : Integer; iRightRestriction : Integer = 0) : Boolean;
Begin
  Result := (iY >= Top) And (iY <= Bottom) And (iX <= Right) And ( ((iRightRestriction = 0) And (iX >= Left)) Or (iX >= Right-iRightRestriction));
End;


Function TWPMapObject.GetParent : TWPMapObject;
Begin
  Assert(Invariants('GetParent', FParent, TWPMapObject, 'Parent'));
  Result := FParent;
End;


Procedure TWPMapObject.SetParent(Const Value : TWPMapObject);
Begin
  Assert(Invariants('SetParent', Value, TWPMapObject, 'Value'));
  FParent := Value;
End;


Function TWPMapObject.GetPiece : TFslObject;
Begin
  Assert(FPiece <> Nil);
  Result := FPiece;
End;

Procedure TWPMapObject.SetPiece(Const Value : TFslObject);
Begin
  Assert((Value = Nil) Or (Value Is TWPWorkingDocumentPiece) Or (Value Is TWPWorkingDocument));
  FPiece := Value;
End;


Function TWPMapObject.HasPiece : Boolean;
Begin
  Result := Assigned(FPiece);
End;


Function TWPMapObject.HasParent : Boolean;
Begin
  Result := Assigned(FParent);
End;

Procedure TWPMapObject.SetBackHotspot(Const Value: TWPHotspot);
Begin
  If (FBackHotspot <> Value) Then
    Begin
    WantPainting;
    FBackHotspot.Free;
    FBackHotspot := Value;
    End
  Else
    FBackHotspot.Free;
End;


Function TWPMapObject.asRect : TRect;
Begin
  Result.Left := FLeft;
  Result.Right := Right;
  Result.Top := FTop;
  Result.Bottom := Bottom;
End;

Function TWPMapObject.WorkingBackground(oCurrent : TWPHotspot) : TColour;
Begin
  Result := FBackground;
  If BackHotspot <> Nil Then
  Begin
    If (BackHotspot = oCurrent) And (BackHotspot.HoverColour <> DEF_COLOUR) Then
      Result := BackHotspot.HoverColour
    Else If FBackHotspot.LinkColour <> DEF_COLOUR Then
      Result := BackHotspot.LinkColour;
  End;
End;


Function TWPMapObject.HorizontalMiddle: Integer;
Begin
  Result := Left + (Width Div 2);
End;

Function TWPMapObject.VerticalMiddle: Integer;
Begin
  Result := Top + (Height Div 2);
End;

Function TWPMapObject.debugType: String;
Begin
  Result := system.Copy(Piece.className, 4, $FF);
End;

Function TWPMapObject.BoundsAsString: String;
Begin
  Result :=  '('+IntegerToString(FLeft)+', '+IntegerToString(FTop)+') - ('+IntegerToString(FLeft+FWidth)+', '+IntegerToString(FTop+FHeight)+') ['+IntegerToString(FWidth)+', '+IntegerToString(FHeight)+']';
End;

function TWPMapObject.GetAnnotation(iId: Integer): TWPWorkingAnnotation;
var
  oFocus : TWPMapObject;
begin
  oFocus := Self;
  while (oFocus.FParent <> nil) Do
    oFocus := oFocus.Parent;
  if oFocus.FPiece is TWPWorkingDocument Then
    result := TWPWorkingDocument(oFocus.FPiece).FAllAnnotations[iId-1]
  Else
    raise EWPException.create('root element doesn''t point to document');
end;

function TWPMapObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPiece.sizeInBytes);
  inc(result, FBackHotspot.sizeInBytes);
end;

{ TWPMapObjects }

Function TWPMapObjects.GetObject(iIndex: Integer): TWPMapObject;
Begin
  Result := TWPMapObject(ObjectByIndex[iIndex]);
End;

Function TWPMapObjects.ItemClass: TFslObjectClass;
Begin
  Result := TWPMapObject;
End;

{ TWPMapItem }

Destructor TWPMapItem.Destroy;
Begin
  Inherited;
  FForeHotspot.Free;
End;

Procedure TWPMapItem.Assign(oSource: TFslObject);
Begin
  Inherited;
  FOffsetLength := TWPMapItem(oSource).FOffsetLength;
  FOffsetStart := TWPMapItem(oSource).FOffsetStart;
  FDescent := TWPMapItem(oSource).FDescent;
  FBreakState := TWPMapItem(oSource).FBreakState;
  FSelection := TWPMapItem(oSource).FSelection;
  FForeHotSpot := TWPMapItem(oSource).FForeHotspot.Link;
  FNose := TWPMapItem(oSource).FNose;
End;

Function TWPMapItem.Clone: TWPMapItem;
Begin
  Result := TWPMapItem(Inherited Clone);
End;

Function TWPMapItem.Link: TWPMapItem;
Begin
  Result := TWPMapItem(Inherited Link);
End;

Function TWPMapItem.OffsetForSelection(iOffset: Integer): Integer;
Begin
  Result := iOffset - (TWPWorkingDocumentPiece(Piece).Metrics.Position + FOffsetStart);
End;

Function TWPMapItem.PointForSelection(iOffset: Integer): Integer;
Var
  oMetrics : TWPMetrics;
Begin
  oMetrics := TWPWorkingDocumentPiece(Piece).Metrics;
  Result := Left + (oMetrics.Offset[iOffset-oMetrics.Position] - oMetrics.Offset[FOffsetStart{-oMetrics.Position}]);
End;

Procedure TWPMapItem.SetDescent(Const Value: Integer);
Begin
  If FDescent <> Value Then
    Begin
    WantPainting;
    FDescent := Value;
    End;
End;

Procedure TWPMapItem.SetOffsetLength(Const Value: Integer);
Begin
  If FOffsetLength <> Value Then
    Begin
    WantPainting;
    FOffsetLength := Value;
    End;
End;

Procedure TWPMapItem.SetOffsetStart(Const Value: Integer);
Begin
  If FOffsetStart <> Value Then
    Begin
    WantPainting;
    FOffsetStart := Value;
    End;
End;


Procedure TWPMapItem.SetSelection(Const Value: TWPMapSelection);
Begin
  If (FSelection <> Value) Or (Value In [wsFromLeft, wsFromRight, wsPart]) Then
    Begin
    WantPainting;
    FSelection := Value;
    End;
End;


Function TWPMapItem.WorkingOffsetEnd: Integer;
Begin
  Result := TWPWorkingDocumentPiece(Piece).Metrics.Position+ OffsetStart + OffsetLength;
End;


Function TWPMapItem.InHotspot : Boolean;
Begin
  Result := Assigned(FForeHotspot) Or Assigned(BackHotspot) ;
End;


Function TWPMapItem.InForeHotspot : Boolean;
Begin
  Result := Assigned(FForeHotspot);
End;


Function TWPMapItem.InBackHotspot : Boolean;
Begin
  Result := Assigned(BackHotspot) ;
End;

Function TWPMapItem.WorkingHotspot : TWPHotspot;
Begin
  If Assigned(FForeHotspot) Then
    Result := FForeHotspot
  Else
    Result := BackHotspot;
End;

Procedure TWPMapItem.SetForeHotspot(Const Value: TWPHotspot);
Begin
  If (FForeHotspot <> Value) Then
    Begin
    WantPainting;
    FForeHotspot.Free;
    FForeHotspot := Value;
    End
  Else
    FForeHotspot.Free;
End;

procedure TWPMapItem.SetAnnotationColour(const Value: TColour);
begin
  If FAnnotationColour <> Value Then
  Begin
    WantPainting;
    FAnnotationColour := Value;
  End;
end;

function TWPMapItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FForeHotspot.sizeInBytes);
end;

{ TWPMapItems }

Function TWPMapItems.GetMapItem(iIndex: Integer): TWPMapItem;
Begin
  Result := TWPMapItem(ObjectByIndex[iIndex]);
End;

Function TWPMapItems.ItemClass: TFslObjectClass;
Begin
  Result := TWPMapItem;
End;

Function TWPMapItems.Clone: TWPMapItems;
Begin
  Result := TWPMapItems(Inherited Clone);
End;

Function TWPMapItems.Link: TWPMapItems;
Begin
  Result := TWPMapItems(Inherited Link);
End;

Procedure TWPMapItems.SetNotPainted;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    MapItem[iLoop].Painted := False;
End;


Function TWPMapItems.GetByOffset(iOffset : Integer) : TWPMapItem;
Var
  iLoop : Integer;
  oMap : TWPMapItem;
Begin
  Result := Nil;
  iLoop := 0;
  While (iLoop < Count) And (Result = Nil) Do
  Begin
    oMap := MapItem[iLoop];
    If oMap.OffsetStart + oMap.OffsetLength > iOffset Then
      Result := oMap;
    Inc(iLoop);
  End;
End;


Function TWPMapItem.GetText : String;
Begin
  Result := System.Copy(TWPWorkingDocumentPiece(Piece).LogicalText, OffsetStart +1, OffsetLength);
End;


Function TWPMapItem.IsLastInLine : Boolean;
Begin
  Result := False;
  If Assigned(Parent) And (Parent Is TWPMapRow) And (TWPMapRow(Parent).Items[TWPMapRow(Parent).Items.count - 1] = Self) Then
    Result := True;
End;


Function TWPMapItems.First: TWPMapItem;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result := MapItem[0];
End;

Function TWPMapItems.Last: TWPMapItem;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result := MapItem[Count - 1];
End;


Procedure TWPMetrics.Assign(oSource: TFslObject);
Var
  iLoop : Integer;
Begin
  Inherited;
  FCharCount := TWPMetrics(oSource).FCharCount;
  FVoiceCharCount := TWPMetrics(oSource).FVoiceCharCount;
  FPosition := TWPMetrics(oSource).FPosition;
  FVoicePosition := TWPMetrics(oSource).FVoicePosition;
  FHeight := TWPMetrics(oSource).FHeight;
  FDescent := TWPMetrics(oSource).FDescent;

  FValid := TWPMetrics(oSource).FValid;

  SetOffsetCount(TWPMetrics(oSource).FOffsetCount);
  For iLoop := 0 To FOffsetCount - 1 Do
    FOffsets[iLoop] := TWPMetrics(oSource).FOffsets[iLoop];
End;

Function TWPMetrics.Clone: TWPMetrics;
Begin
  Result := TWPMetrics(Inherited Clone);
End;

Constructor TWPMetrics.Create;
Begin
  Inherited;
  FValid := False;
End;

Destructor TWPMetrics.Destroy;
Begin
  SetOffsetCount(0);
  Inherited;
End;

Function TWPMetrics.GetOffset(iIndex: Integer): Integer;
Begin
  Assert(iIndex <= FOffsetCount, StringFormat('Attempt to use invalid offset index (%d/%d)', [iIndex, FOffsetCount]));
  If iIndex < 1 Then
    Result := 0
  Else
    Result := FOffsets[iIndex - 1];
End;

Function TWPMetrics.Link: TWPMetrics;
Begin
  Result := TWPMetrics(Inherited Link);
End;


Procedure TWPMetrics.SetOffset(iIndex: Integer; Const Value: Integer);
Begin
  Assert(iIndex <= FOffsetCount, 'Attempt to use invalid offset index');
  If iIndex >= 1 Then
    FOffsets[iIndex - 1] := Value;
End;


Procedure TWPMetrics.SetOffsetCount(Const Value: Integer);
Begin
  MemoryResize(FOffsets, SizeOf(Integer) * FOffsetCount, SizeOf(Integer) * Value);

  FOffsetCount := Value;
End;


Function TWPMetrics.GetWidth : Integer;
Begin
  Result := Offset[OffsetCount];
End;

{ TWPWorkingDocument }

Constructor TWPWorkingDocument.Create;
Begin
  Inherited;
  FPieces := TWPWorkingDocumentPieces.Create;
  FPieces.Hook(Change);
  FAllAnnotations := TWPWorkingAnnotationList.Create;
  FDrawnAnnotations := TWPWorkingAnnotationList.Create;
  FAllowedWords := TFslStringList.Create;
  FAttachments := TWPWorkingAttachmentList.create;
  FAttachments.SortedBy(FAttachments.CompareById);
End;


Destructor TWPWorkingDocument.Destroy;
Begin
  FAttachments.Free;
  FSourceBytes.Free;
  FSourceObjectModel.Free;
  FPieces.UnHook(Change);
  FPieces.Free;
  FAllowedWords.Free;
  FFieldDefinitionProviders.Free;
  FAnnotationDefinitionProviders.Free;
  FAllAnnotations.Free;
  FDrawnAnnotations.Free;
  Inherited;
End;


Function TWPWorkingDocument.Link: TWPWorkingDocument;
Begin
  Result := TWPWorkingDocument(Inherited Link);
End;


Function TWPWorkingDocument.Clone: TWPWorkingDocument;
Begin
  Result := TWPWorkingDocument(Inherited Clone);
  result.RelinkAttachments;
End;


Procedure TWPWorkingDocument.Assign(oSource: TFslObject);
var
  i : integer;
Begin
  Inherited;
  Pieces.Assign(TWPWorkingDocument(oSource).Pieces);
  AllAnnotations.Assign(TWPWorkingDocument(oSource).AllAnnotations);
  DrawnAnnotations.Assign(TWPWorkingDocument(oSource).DrawnAnnotations);
  for i := 0 to TWPWorkingDocument(oSource).Attachments.count - 1 do
    Attachments.add(TWPWorkingDocument(oSource).Attachments[i].link);
  Change(ctListContents, Self);
  FRegenerationNeeded := True;
End;


Procedure TWPWorkingDocument.BuildEmptyDocument;
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  Clear;

  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    oPara.Format.Defaults;
    oPara.Font.Defaults;

    Pieces.Add(oPara.Link);
  Finally
    oPara.Free;
  End;
End;


Function TWPWorkingDocument.NewParagraph: TWPWorkingDocumentParaPiece;
Begin
  Result := TWPWorkingDocumentParaPiece.Create;
  Try
    If Pieces.count > 0 Then
      Begin
      Result.Style := Pieces[Pieces.Count - 1].Style;
      Result.Font.Assign(Pieces[Pieces.Count - 1].Font);
      End;
    Pieces.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPWorkingDocument.NewSection(Const sNameSpace, sName, sDisplay: String): TWPWorkingDocumentSectionStartPiece;
Begin
  Result := TWPWorkingDocumentSectionStartPiece.Create;
  Try
    Result.NameSpace := sNameSpace;
    Result.Name := sName;
    Result.DisplayName := sDisplay;
    If Pieces.count > 0 Then
      Begin
      Result.Style := Pieces[Pieces.Count - 1].Style;
      Result.Font.Assign(Pieces[Pieces.Count - 1].Font);
      End;
    Pieces.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPWorkingDocument.EndSection : TWPWorkingDocumentStopPiece;
Begin
  Result := TWPWorkingDocumentStopPiece.Create;
  Result.StopType := stSection;
  Try
    If Pieces.count > 0 Then
      Begin
      Result.Style := Pieces[Pieces.Count - 1].Style;
      Result.Font.Assign(Pieces[Pieces.Count - 1].Font);
      End;
    Pieces.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPWorkingDocument.FindSection(Const sNamespace, sName: String): TWPWorkingDocumentSectionStartPiece;
Var
  iLoop : Integer;
Begin
  iLoop := 0;
  Result := Nil;
  While (iLoop < Pieces.Count) And Not Assigned(Result) Do
    Begin
    If (Pieces[iLoop].PieceType = ptSectionStart) And StringEquals(TWPWorkingDocumentSectionStartPiece(Pieces[iLoop]).Namespace, sNamespace) And StringEquals(TWPWorkingDocumentSectionStartPiece(Pieces[iLoop]).Name, sName) Then
      Result := TWPWorkingDocumentSectionStartPiece(Pieces[iLoop])
    Else
      Inc(iLoop);
    End;
End;


Procedure TWPWorkingDocument.Clear;
Begin
  FPieces.Clear;
  FAllowedWords.Clear;
  FRegenerationNeeded := True;
End;


Function TWPWorkingDocument.IsEmpty : Boolean;
Begin
  Result := (Pieces.Count = 0) Or ((Pieces.Count = 1) And (Pieces[0].PieceType = ptPara));
End;


Procedure TWPWorkingDocument.Change(aType : TChangeType; oSource : TWPTrackable);
Begin
  If aType In [ctListContents, ctTextContents] Then
  Begin
    FRegenerationNeeded := True;
    If (oSource = Nil) Then
      RaiseError('change', 'nil source');
  End;
End;



Procedure TWPWorkingDocument.MapRow(oTable : TWPWorkingDocumentTableStartPiece; oRow : TWPWorkingDocumentTableRowStartPiece; oTop, oBottom : TWPBorder);
Var
  iCell : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  If (oRow.Cells.Count <> 0) Then
  Begin

    For iCell := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iCell];
      oCell.ChooseWorkingTopBorder(oTop);
      oCell.ChooseWorkingBottomBorder(oBottom);
      oCell.Row := oRow;
    End;

    If (oRow.Cells.Count = 1) Then
    Begin
      oCell := oRow.Cells[0];
      oCell.State := tisOnly;
      oCell.ChooseWorkingLeftBorder(oTable.LeftBorder);
      oCell.ChooseWorkingRightBorder(oTable.RightBorder);
    End
    Else
    Begin
      oCell := oRow.Cells[0];
      oCell.State := tisFirst;
      oCell.ChooseWorkingLeftBorder(oTable.LeftBorder);
      oCell.ChooseWorkingRightBorder(oTable.CenterVerticalBorder);

      For iCell := 1 To oRow.Cells.Count - 2 Do
      Begin
        oCell := oRow.Cells[iCell];
        oCell.State := tisMiddle;
        oCell.ChooseWorkingLeftBorder(oTable.CenterVerticalBorder);
        oCell.ChooseWorkingRightBorder(oTable.CenterVerticalBorder);
      End;

      oCell := oRow.Cells[oRow.Cells.Count - 1];
      oCell.State := tisLast;
      oCell.ChooseWorkingLeftBorder(oTable.CenterVerticalBorder);
      oCell.ChooseWorkingRightBorder(oTable.RightBorder);
    End;
  End;
End;

Procedure TWPWorkingDocument.MapTable(oTable : TWPWorkingDocumentTableStartPiece);
Var
  iRow : Integer;
  iCols : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  bNesting : Boolean;
Begin
  oTable.Jagged := False;
  bNesting := False;

  Assert(CheckCondition(Not oTable.Rows.IsEmpty, 'MapTable', 'Table has no rows.'));

  iCols := oTable.Rows[0].ColumnCount;
  For iRow := 1 To oTable.Rows.Count - 1 Do
    iCols := IntegerMax(iCols, oTable.Rows[iRow].ColumnCount);
  oTable.ColumnCount := iCols;

  If oTable.Rows.Count = 1 Then
  Begin
    oRow := oTable.Rows[0];
    oRow.Table := oTable;
    oRow.State := tisOnly;
    MapRow(oTable, oRow, oTable.TopBorder, oTable.BottomBorder);
  End
  Else
  Begin
    oRow := oTable.Rows[0];
    oRow.Table := oTable;
    oRow.State := tisFirst;
    If oRow.Cells.Count <> iCols Then
      oTable.Jagged := True;
    MapRow(oTable, oRow, oTable.TopBorder, oTable.CenterHorizontalBorder);
    bNesting := bNesting Or (oRow.Owner <> Nil);

    For iRow := 1 To oTable.Rows.Count - 2 Do
    Begin
      oRow := oTable.Rows[iRow];
      oRow.Table := oTable;
      If oRow.Cells.Count <> iCols Then
        oTable.Jagged := True;
      oRow.State := tisMiddle;
      MapRow(oTable, oRow, oTable.CenterHorizontalBorder, oTable.CenterHorizontalBorder);
      bNesting := bNesting Or (oRow.Owner <> Nil);
    End;

    oRow := oTable.Rows[oTable.Rows.Count - 1];
    oRow.Table := oTable;
    If oRow.Cells.Count <> iCols Then
        oTable.Jagged := True;
    oRow.State := tisLast;
    MapRow(oTable, oRow, oTable.CenterHorizontalBorder, oTable.BottomBorder);
    bNesting := bNesting Or (oRow.Owner <> Nil);
  End;
  oTable.ChooseWorkingTopBorder(Nil);
  oTable.ChooseWorkingRightBorder(Nil);
  oTable.ChooseWorkingLeftBorder(Nil);
  oTable.ChooseWorkingRightBorder(Nil);
  oTable.ApplyPolicy;

  If bNesting Then
    For iRow := 0 To oTable.Rows.Count - 1 Do
      MapChildren(oTable.Rows, oTable.Rows[iRow], iRow + 1);
End;

Procedure TWPWorkingDocument.MapChildren(oRows : TWPWorkingDocumentTableRowStartPieces; oRow : TWPWorkingDocumentTableRowStartPiece; iStart : Integer);
Var
  oLast : TWPWorkingDocumentTableRowStartPiece;
  oThis : TWPWorkingDocumentTableRowStartPiece;
  iLoop : Integer;
Begin
  oLast := Nil;
  For iLoop := iStart To oRows.Count - 1 Do
  Begin
    oThis := oRows[iLoop];
    If (oThis.Owner = oRow) Then
    Begin
      If (oLast <> Nil) Then
        oLast.TableNextLevel := oThis;
      othis.TablePrevLevel := oLast;
      oLast := oThis;
    End;
  End;
  If (Assigned(oLast)) Then
    oLast.TableNextLevel := Nil;
End;

Procedure TWPWorkingDocument.RegenerateMetrics(bForce, bSimpleOnly: Boolean);
Begin
  If bForce Or FRegenerationNeeded Then
    DoRegenerateMetrics(bSimpleOnly);
End;

Procedure TWPWorkingDocument.DoRegenerateMetrics(bSimpleOnly: Boolean);
Var
  iLoop, i : Integer;
  iCurrent : Integer;
  iVoiceCurrent : Integer;
  oWork : TWPWorkingDocumentPiece;
  oTable : TWPWorkingDocumentTableStartPiece;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  oParagraph : TWPWorkingDocumentParaPiece;
  oImage : TWPWorkingDocumentImagePiece;
  bContent : Boolean;
  iValue : Integer;
  oLast : TWPWorkingDocumentPiece;
  oLastRow : TWPWorkingDocumentTableRowStartPiece;
  bTableRegenerationNeeded : Boolean;
  oStartList : TWPWorkingDocumentPieceStack;
Begin
  Try
    oTable := Nil;
    oRow := Nil;
    FFieldCount := 0;
    iCurrent := 0;
    iVoiceCurrent := 0;
    bContent := False;
    iValue := 0;
    oLast := Nil;
    oLastRow := Nil;
    FMinLength := 0;
    bTableRegenerationNeeded := False;
    For iLoop := 0 to FAttachments.Count - 1 Do
      FAttachments[iLoop].InUse := False;

    For iLoop := 0 to FAllAnnotations.Count - 1 Do
    Begin
      FAllAnnotations[iLoop].InUse := False;
      if (FAllAnnotations[iLoop].DefinitionProvider = nil) And Assigned(AnnotationDefinitionProviders) then
      Begin
        FAllAnnotations[iLoop].DefinitionProvider := AnnotationDefinitionProviders.GetByName(FAllAnnotations[iLoop].FOwner, nil).Link;
        if FAllAnnotations[iLoop].DefinitionProvider <> nil then
        FAllAnnotations[iLoop].Colour := FAllAnnotations[iLoop].DefinitionProvider.GetColour(FAllAnnotations[iLoop].Text);
      End;
    End;
    FDrawnAnnotations.Clear;

    oStartList := TWPWorkingDocumentPieceStack.Create;
    Try

      For iLoop := 0 To Pieces.Count - 1 Do
      Begin
        oWork := Pieces[iLoop];
        oWork.Prev := oLast;
        If Assigned(oLast) Then
          oLast.Next := oWork;

        oWork.Metrics.Position := iCurrent;
        oWork.Metrics.VoicePosition := iVoiceCurrent;
        Inc(iCurrent, oWork.Metrics.CharCount);
        Inc(iVoiceCurrent, oWork.Metrics.VoiceCharCount);

        // annotation status
        If oWork.AnnotationId = 0 Then
        Begin
          if Assigned(oLast) and (oLast.AnnotationId <> 0) Then
          Begin
            If oLast.AnnotationStatus = AnnotationStatusStart Then
              oLast.AnnotationStatus := AnnotationStatusAll
            Else
              oLast.AnnotationStatus := AnnotationStatusEnd;
            FAllAnnotations[oLast.AnnotationId-1].OffsetEnd := oWork.Metrics.Position;
          End;
          oWork.AnnotationStatus := AnnotationStatusNone;
        End
        Else
        Begin
          if Assigned(oLast) Then
          Begin
            If (oLast.AnnotationId = oWork.AnnotationId) Then
              oWork.AnnotationStatus := AnnotationStatusContinue
            else
            Begin
              if not FAllAnnotations[oWork.AnnotationId-1].InUse Then
                FDrawnAnnotations.Add(FAllAnnotations[oWork.AnnotationId-1].Link);
              FAllAnnotations[oWork.AnnotationId-1].InUse := true;
              If oLast.AnnotationStatus <> AnnotationStatusNone Then
              Begin
                If oLast.AnnotationStatus = AnnotationStatusStart Then
                  oLast.AnnotationStatus := AnnotationStatusAll
                Else
                  oLast.AnnotationStatus := AnnotationStatusEnd;
                FAllAnnotations[oLast.AnnotationId-1].OffsetEnd := oWork.Metrics.Position;
              End;
              FAllAnnotations[oWork.AnnotationId-1].OffsetStart := oWork.Metrics.Position;
              oWork.AnnotationStatus := AnnotationStatusStart;
            End;
          End
          Else
          Begin
            If not FAllAnnotations[oWork.AnnotationId-1].InUse Then
              FDrawnAnnotations.Add(FAllAnnotations[oWork.AnnotationId-1].Link);
            FAllAnnotations[oWork.AnnotationId-1].InUse := true;
            FAllAnnotations[oWork.AnnotationId-1].OffsetStart := oWork.Metrics.Position;
            oWork.AnnotationStatus := AnnotationStatusStart;
          End;
        End;
        oLast := oWork;


        if (oWork.PieceType in [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak]) And (oWork.AnnotationId <> 0) Then
        Begin
          If not FAllAnnotations[oWork.AnnotationId - 1].InUse Then
            FDrawnAnnotations.Add(FAllAnnotations[oWork.AnnotationId-1].Link);
          FAllAnnotations[oWork.AnnotationId - 1].InUse := true;
        End;

        if (oWork.PieceType = ptImage) Then
        begin
          oImage := TWPWorkingDocumentImagePiece(oWork);
(*          if (oImage.Image is TWPPDFGraphic) then
          begin
            i := FAttachments.IndexByReference(TWPPDFGraphic(oImage.Image).Attachment);
            if i = -1 then
              raise EWPException.create('Attachment mis-allocation')
            else
              FAttachments[i].InUse := true;
          end;*)
        end;

        If Not bSimpleOnly Then
        Begin
        If oWork.PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop] Then
          FMinLength := IntegerMax(FMinLength, oWork.Metrics.Width);

        Case oWork.PieceType Of
          ptText, ptImage, ptFieldStop, ptLineBreak : bContent := True;

          ptFieldStart :
          Begin
            Inc(FFieldCount);
            If (FFieldDefinitionProviders <> Nil) And (TWPWorkingDocumentFieldStartPiece(oWork).Namespace <> '') And
               (Not TWPWorkingDocumentFieldStartPiece(oWork).HasDefinitionProvider) Then
              TWPWorkingDocumentFieldStartPiece(oWork).DefinitionProvider := FFieldDefinitionProviders.GetByName(TWPWorkingDocumentFieldStartPiece(oWork).Namespace, Nil).Link;
            If TWPWorkingDocumentFieldStartPiece(oWork).DocField = Nil Then
              TWPWorkingDocumentFieldStartPiece(oWork).BuildDocField;
          End;

          ptSectionStart  :
          Begin
              If TWPWorkingDocumentSectionStartPiece(oWork).IsField Then
              Inc(FFieldCount);
            oStartList.Push(oWork.Link);
          End;

          ptSectionStop  :
              TWPWorkingDocumentStopPiece(oWork).MatchingStart := oStartList.Pop.Link;

          ptCellStop  :
              TWPWorkingDocumentStopPiece(oWork).MatchingStart := oStartList.Pop.Link;

          ptCellStart  :
            Begin
            If Assigned(oRow) Then
              oRow.Cells.Add(oWork.Link);
            oStartList.Push(oWork.Link);
            End;
          ptRowStop :
            Begin
              oRow := Nil;
              TWPWorkingDocumentStopPiece(oWork).MatchingStart := oStartList.Pop.Link;
            End;

          ptTableStart :
            Begin
              oTable := TWPWorkingDocumentTableStartPiece(oWork);
              bTableRegenerationNeeded := oTable.StructureDirty;
              If bTableRegenerationNeeded Then
              Begin
                oTable.Rows.Clear;
                oLastRow := Nil;
               End
              Else
                oTable := Nil;
            oStartList.Push(oWork.Link);
            End;
          ptTableStop :
            Begin
            If bTableRegenerationNeeded Then
              Begin
                If oLastRow <> Nil Then
                  oLastRow.TableNext := Nil;
                MapTable(oTable);
                oTable := Nil;
                bTableRegenerationNeeded := False;
              End;
            TWPWorkingDocumentStopPiece(oWork).MatchingStart := oStartList.Pop.Link;
            End;
          ptRowStart :
            Begin
            oStartList.Push(oWork.Link);
            If bTableRegenerationNeeded Then
              Begin
                oRow := TWPWorkingDocumentTableRowStartPiece(oWork);
                If (oLastRow <> Nil) Then
                  oLastRow.TableNext := oRow;
                oRow.TablePrev := oLastRow;
                oLastRow := oRow;
                oRow.Cells.Clear;
                If (oRow.hasOwner) Then
                  oRow.Depth := oRow.Owner.Depth + 1
                Else
                  oRow.Depth := 0;
                If oTable <> Nil Then
                  oTable.Rows.Add(oRow.Link);
              End;
            End;
          ptPara :
            Begin
              oParagraph := TWPWorkingDocumentParaPiece(oWork);

              If oParagraph.Format.ListType = WPSParagraphListTypeNumbers Then
              Begin
                If oParagraph.Format.FixedNumber <> DEF_WORD Then
                Begin
                  oParagraph.ListNumber := oParagraph.Format.FixedNumber;
                  iValue := oParagraph.Format.FixedNumber;
                End
                Else
                Begin
                  Inc(iValue);
                  oParagraph.ListNumber := iValue;
                End
              End
              Else If bContent Then
                iValue := 0;
              bContent := False;
            End;
        End;
      End;
      End;
      Assert(CheckCondition(FWorking Or (oStartList.Count = 0), 'Regenerate Metrics', 'imbalance in start list '+IntegerToString(oStartList.Count)));
    Finally
      oStartList.Free;
    End;

    If Assigned(oLast) Then
    Begin
      oLast.Next := Nil;
      If oLast.AnnotationStatus <> AnnotationStatusNone Then
      Begin
        If oLast.AnnotationStatus = AnnotationStatusStart Then
          oLast.AnnotationStatus := AnnotationStatusAll
        Else
          oLast.AnnotationStatus := AnnotationStatusEnd;
        FAllAnnotations[oLast.AnnotationId-1].OffsetEnd := iCurrent;
      End;
    End;

    FCharCount := iCurrent;
    FVoiceCharCount := iVoiceCurrent;

    If Not bSimpleOnly Then
    FRegenerationNeeded := False;
  Except
    On e:Exception Do
    Begin
      {$IFOPT C+}
      e.Message := e.Message + ' (regenerating '+DocSequenceAsText+')';
      {$ENDIF}
      Raise;
    End;
  End;
End;

Function TWPWorkingDocument.DocSequenceAsText: String;
Var
  i : Integer;
Begin
  Result := '';
  For i := 0 To Pieces.Count - 1 Do
    Result := Result + CODES_WPPIECETYPE[Pieces[i].PieceType]+',';
  Delete(Result, Length(Result), 1);
End;

Procedure TWPWorkingDocument.InsertAtOffset(oPiece: TWPWorkingDocumentPiece; iOffset: Integer);
Var
  oExist : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  If Not GetPieceByPosition(iOffset, oExist, iInternal, iIndex) Then
    RaiseError('InsertatOffset', 'Offset not found')
  Else If iInternal = 0 Then
    FPieces.Insert(iIndex, oPiece)
  Else If iInternal < Length(oExist.LogicalText) Then
    RaiseError('InsertatOffset', 'Cannot insert in the middle of a content piece ('+IntegerToString(iInternal)+'/'+IntegerToString(Length(oExist.LogicalText))+' at '+IntegerToString(iIndex)+')')
  Else
    FPieces.Insert(iIndex + 1, oPiece);
End;

function TWPWorkingDocument.GetByOffset(iPosition: Integer): TWPWorkingDocumentPiece;
Var
  iOffset : Integer;
  iIndex : Integer;
begin
  If Not GetPieceByPosition(iPosition, Result, iOffset, iIndex) Then
    Result := Nil;
end;


Procedure TWPWorkingDocument.CopyPortion(oDest: TWPWorkingDocumentPieces; iStart, iEnd: Integer);
Var
  oExist : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iStartIndex : Integer;
  iEndIndex : Integer;
  iLoop : Integer;
Begin
  oDest.Clear;

  If Not GetPieceByPosition(iStart, oExist, iInternal, iStartIndex) Then
    RaiseError('CopyPortion', 'Start Offset not found')
  Else If iInternal <> 0 Then
    RaiseError('CopyPortion', 'Start Offset not clean');

  If Not GetPieceByPosition(iEnd, oExist, iInternal, iEndIndex) Then
    RaiseError('CopyPortion', 'End Offset not found')
  Else If (iInternal = 0) Then
    If (iEndIndex > 0) Then
      Dec(iEndIndex)
    Else
      RaiseError('CopyPortion', 'End at beginning')
  Else If iInternal <> oExist.Metrics.CharCount Then
    RaiseError('InsertatOffset', 'End Offset not clean');

  For iLoop := iStartIndex To iEndIndex Do
    oDest.Add(Pieces[iLoop].Clone);
End;


Function TWPWorkingDocument.GetHotspot(ch: Char; Var oItem: TWPMapObject; Var oOwner : TWPWorkingDocumentPiece): TWPHotspot;
Var
  iLoop : Integer;
  iImage : Integer;
  oWork : TWPWorkingDocumentPiece;
  oImage : TWPWorkingDocumentImagePiece;
  oArea : TWPImageMapArea;
Begin
  Result := Nil;
  oItem := Nil;
  iLoop := 0;
  ch := charLower(ch);
  While (Result = Nil) And (iLoop < FPieces.Count) Do
  Begin
    oWork := FPieces[iLoop];
    If oWork.HasHotspot And (charLower(oWork.Hotspot.Key) = ch) Then
    Begin
      Result := oWork.Hotspot;
      oItem := oWork.Map;
      oOwner := oWork;
    End
    Else If (oWork.PieceType = ptImage) And TWPWorkingDocumentImagePiece(oWork).HasImageMap Then
    Begin
      oImage := TWPWorkingDocumentImagePiece(oWork);
      iImage := 0;
      While (Result = Nil) And (iImage < oImage.ImageMap.Areas.Count) Do
      Begin
        oArea := oImage.ImageMap.Areas[iImage];
        If charLower(oArea.Key) = ch Then
        Begin
          Result := oArea;
          oItem := oWork.Map;
          oOwner := oWork;
        End;
        Inc(iImage);
      End;
    End;
    Inc(iLoop);
  End;
End;


Function TWPWorkingDocument.NormalPositionToVoicePosition(iPosition : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iOffset : Integer;
  iIndex : Integer;
Begin
  If GetPieceByPosition(iPosition, oPiece, iOffset, iIndex) Then
    Result := oPiece.Metrics.VoicePosition + iOffset
  Else
    Result := 0;
End;


Function TWPWorkingDocument.VoicePositionToNormalPosition(iPosition : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iOffset : Integer;
  iIndex : Integer;
Begin
  If GetPieceByVoicePosition(iPosition, oPiece, iOffset, iIndex) Then
    Result := oPiece.Metrics.Position + IntegerMin(iOffset, oPiece.Metrics.CharCount)
  Else
    Result := 0;
End;


Function TWPWorkingDocument.GetPieceByPosition(iPosition: Integer; Out oPiece: TWPWorkingDocumentPiece; Out iOffset: Integer; Out iIndex : Integer; bQuick : Boolean = True): Boolean;
Var
  iLoop : Integer;
  oWork : TWPWorkingDocumentPiece;
  L, H, I, C: Integer;
Begin
  RegenerateMetrics(False, True);
  If iPosition >= FCharCount Then
  Begin
    If FPieces.Count = 0 Then
      Result := False
    Else
    Begin
      Result := True;
      iIndex := FPieces.Count - 1;
      oPiece := FPieces[iIndex];
      iOffset := oPiece.Metrics.CharCount;
    End;
  End
  Else If iPosition < 0 Then
    Result := False
  Else If Not bQuick Or (FPieces.Count < 10) Or (iPosition < FPieces.Count Div 20) Then
  Begin
    Result := False;
    iLoop := 0;
    While (Not Result) And (iLoop < FPieces.Count) Do
      Begin
      oWork := FPieces[iLoop];
      If oWork.Metrics.Position + oWork.Metrics.CharCount > iPosition Then
        Begin
        Result := True;
        oPiece := oWork;
        iIndex := iLoop;
        iOffset := iPosition - oWork.Metrics.Position;
        End;
      Inc(iLoop);
      End;
  End
  Else
  Begin
    Result := False;
    L := 0;
    H := FPieces.Count - 1;
    While L <= H Do
    Begin
      I := (L + H) Shr 1;
      oWork := FPieces[i];
      If (iPosition >= oWork.Metrics.Position) And (iPosition < oWork.Metrics.Position + oWork.Metrics.CharCount) Then
        C := 0
      Else If oWork.Metrics.Position > iPosition Then
        C := 1
      Else
        C := -1;
      If C < 0 Then
        L := I + 1
      Else
      Begin
        H := I - 1;
        If c = 0 Then
        Begin
          Result := True;
          oPiece := oWork;
          iIndex := I;
          iOffset := iPosition - oWork.Metrics.Position;
        End;
      End;
    End;
  End;
End;


Function TWPWorkingDocument.GetPieceByVoicePosition(iVoicePosition: Integer; Out oPiece: TWPWorkingDocumentPiece; Out iOffset: Integer; Out iIndex : Integer): Boolean;
Var
  iLoop : Integer;
  oWork : TWPWorkingDocumentPiece;
Begin
  RegenerateMetrics(False, True);
  If iVoicePosition = FVoiceCharCount Then
    Begin
    If FPieces.Count = 0 Then
      Result := False
    Else
    Begin
      Result := True;
      iIndex := FPieces.Count - 1;
      oPiece := FPieces[iIndex];
      iOffset := oPiece.Metrics.VoiceCharCount;
    End;
    End
  Else If iVoicePosition < 0 Then
    Result := False
  Else
    Begin
    Result := False;
    iLoop := 0;
    While (Not Result) And (iLoop < FPieces.Count) Do
      Begin
      oWork := FPieces[iLoop];
      If oWork.Metrics.VoicePosition + oWork.Metrics.VoiceCharCount > iVoicePosition Then
        Begin
        Result := True;
        oPiece := oWork;
        iIndex := iLoop;
        iOffset := iVoicePosition - oWork.Metrics.VoicePosition;
        End;
      Inc(iLoop);
      End;
    End;
End;


Function TWPWorkingDocument.AsText : String;
Begin
  Result := FPieces.AsText;
End;


Function TWPWorkingDocument.AsClassSequence : String;
Var
  iLoop : Integer;
Begin
  Result := '';

  For iLoop := 0 To Pieces.Count - 1 Do
    StringAppend(Result, Pieces[iLoop].ClassName+cReturn);
End;


Function TWPWorkingDocument.GetCharCount: Integer;
Begin
  RegenerateMetrics(False, True);
  Result := FCharCount;
End;

Function TWPWorkingDocument.GetFieldCount: Integer;
Begin
  RegenerateMetrics(False, False);
  Result := FFieldCount;
End;

Function TWPWorkingDocument.GetMinLength: Integer;
Begin
  RegenerateMetrics(False, False);
  Result := FMinLength;
End;

Function TWPWorkingDocument.GetVoiceCharCount: Integer;
Begin
  RegenerateMetrics(False, True);
  Result := FVoiceCharCount;
End;



Function TWPWorkingDocument.DocEnd(bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Begin
  iNew := CheckCursorPoint(CharCount - 1, cdJump, bNoSelectReadOnly);
  Result := iNew <> -1;
End;


Function TWPWorkingDocument.SectionEnd(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; bIgnoreFieldSections : Boolean; Out iNew : Integer) : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Begin
    While (iIndex < Pieces.Count) And
        ((Pieces[iIndex].PieceType <> ptSectionStop) Or Not bIgnoreFieldSections Or TWPWorkingDocumentSectionStartPiece(TWPWorkingDocumentStopPiece(Pieces[iIndex]).MatchingStart).IsField) Do
      Inc(iIndex);
    Result := iIndex < Pieces.Count;
    If Result Then
      Begin
      iNew := CheckCursorPoint(Pieces[iIndex].Metrics.Position, cdLeft, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End;
    End;
End;


Function TWPWorkingDocument.DocHome(bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Begin
  iNew := CheckCursorPoint(0, cdJump, bNoSelectReadOnly);
  Result := iNew <> -1;
End;

Function TWPWorkingDocument.SectionHome(Const iCurrent : Integer; bNoSelectReadOnly : Boolean; bIgnoreFieldSections : Boolean; Out iNew : Integer) : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Begin
    While (iIndex >= 0) And
       ((Pieces[iIndex].PieceType <> ptSectionStart) Or Not bIgnoreFieldSections Or TWPWorkingDocumentSectionStartPiece(Pieces[iIndex]).IsField) Do
      Dec(iIndex);
    If iIndex = 0 Then
      iNew := 0
    Else
      iNew := CheckCursorPoint(Pieces[iIndex+1].Metrics.Position, cdLeft, bNoSelectReadOnly);
    Result := (iNew <> iCurrent) And (iNew <> -1);
    End;
End;

Function TWPWorkingDocument.NextLeft(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Begin
  Result := iCurrent > 0;
  If Result Then
    Begin
    iNew := CheckCursorPoint(iCurrent - 1, cdLeft, bNoSelectReadOnly);
    Result := (iNew <> iCurrent) And (iNew <> -1);
    End;
End;

Function TWPWorkingDocument.NextRight(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Begin
  Result := iCurrent < CharCount-1;
  If Result Then
    Begin
    iNew := CheckCursorPoint(iCurrent + 1, cdRight, bNoSelectReadOnly);
    Result := (iNew <> iCurrent) And (iNew <> -1);
    End;
End;

Function TWPWorkingDocument.WordLeft(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Var
  oIterator : TWPCharIterator;
  bFound : Boolean;
Begin
  Result := False;
  bFound := False;
  oIterator := TWPCharIterator.Create(Link);
  Try
    oIterator.MoveTo(iCurrent);
    Repeat
      oIterator.Prev;
      If oIterator.More Then
        bFound := bFound Or CharInSet(CharLower(oIterator.Current), ['a'..'z', '0'..'9']);
    Until Not oIterator.More Or (bFound And Not CharInSet(charLower(oIterator.Current), ['a'..'z', '0'..'9']));
    If oIterator.More Then
      Begin
      iNew := CheckCursorPoint(oIterator.CurrentPosition + 1, cdLeft, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End
    Else If bFound Then
      Begin
      iNew := CheckCursorPoint(0, cdLeft, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPWorkingDocument.WordRight(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Var
  oIterator : TWPCharIterator;
  bFound : Boolean;
Begin
  Result := False;
  bFound := False;
  oIterator := TWPCharIterator.Create(Link);
  Try
    oIterator.MoveTo(iCurrent);
    Repeat
      oIterator.Next;
      If oIterator.More Then
        bFound := bFound Or Not CharInSet(charLower(oIterator.Current), ['a'..'z', '0'..'9']);
    Until Not oIterator.More Or (bFound And CharInSet(charLower(oIterator.Current), ['a'..'z', '0'..'9']));
    If oIterator.More Then
      Begin
      iNew := CheckCursorPoint(oIterator.CurrentPosition, cdRight, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End
     Else If bFound Then
      Begin
      iNew := CheckCursorPoint(CharCount - 1, cdRight, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPWorkingDocument.ParaEnd(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Begin
    While (iIndex < Pieces.Count) And (Pieces[iIndex].PieceType <> ptPara) Do
      Inc(iIndex);
    Result := iIndex < Pieces.Count;
    If Result Then
      Begin
      iNew := CheckCursorPoint(Pieces[iIndex].Metrics.Position, cdRight, bNoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End;
    End;
End;

Function TWPWorkingDocument.ParaHome(Const iCurrent: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Begin
    While (iIndex >= 0) And Not (Pieces[iIndex].PieceType In [ptPara, ptSectionStart, ptSectionStop]) Do
      Dec(iIndex);
    If iIndex = 0 Then
      iNew := 0
    Else
      iNew := CheckCursorPoint(Pieces[iIndex+1].Metrics.Position, cdLeft, bNoSelectReadOnly);
    Result := (iNew <> iCurrent) And (iNew <> -1);
    End;
End;

Function TWPWorkingDocument.CheckCursorPointScoped(Var iCursor : Integer; aTypes : TWPWorkingDocumentPieceTypes; bLeft, bNoSelectReadOnly : Boolean) : Boolean;
Var
  ic: Integer;
  bDone : Boolean;
  oPiece : TWPWorkingDocumentPiece;
Begin
  ic := iCursor;
  bDone := False;
  Result := False;
  If bLeft Then
  Begin
    // special case - cannot select the last thing
    If ic = CharCount Then
      Dec(ic);
    While (ic >= 0) And Not bDone And Not Result Do
    Begin
      oPiece := PieceForCursor(ic);
      bDone := (oPiece = Nil) Or Not (oPiece.PieceType In aTypes);
      If Not bDone And PieceHoldsCursor(oPiece, bNoSelectReadOnly) Then
      Begin
        Result := True;
        iCursor := ic;
      End;
    Dec(ic);
    End;
  End
  Else
  Begin
    While (ic < CharCount) And Not Result And Not bDone Do
    Begin
      oPiece := PieceForCursor(ic);
      bDone := (oPiece = Nil) Or Not (oPiece.PieceType In aTypes);
      If Not bDone And PieceHoldsCursor(oPiece, bNoSelectReadOnly) Then
      Begin
        Result := True;
        iCursor := ic;
      End;
      Inc(ic);
    End;
  End;
End;


Function TWPWorkingDocument.CheckCursorPoint(iCursor : Integer; aDir : TWPCursorDirection; bNoSelectReadOnly : Boolean) : Integer;
Const
  left = True;
  right = False;
Var
  bOk : Boolean;
Begin
  If iCursor > CharCount Then
    iCursor := CharCount;
  If (aDir In [cdLeft, cdRight]) Then
  Begin
    bOk := CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES, aDir = cdLeft, bNoSelectReadOnly);
    bOk := bOk And CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES, Not (aDir = cdLeft), bNoSelectReadOnly);
  End
  Else
  Begin
    // first we check in either direction scoped by paragraph
    bOk := CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage], right, bNoSelectReadOnly);
    bOk := bOk Or CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage], left, bNoSelectReadOnly);
    // second we check in either direction scoped by cell
    bOk := bOk Or CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage, ptPara], right, bNoSelectReadOnly);
    bOk := bOk Or CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage, ptPara], left, bNoSelectReadOnly);
    // third we check in either direction scoped by row
    bOk := bOk Or CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage, ptPara, ptCellStart, ptCellStop], right, bNoSelectReadOnly);
    bOk := bOk Or CheckCursorPointScoped(iCursor, [ptText, ptFieldStart, ptFieldStop, ptImage, ptPara, ptCellStart, ptCellStop], left, bNoSelectReadOnly);
    // fourth we check in either direction scoped by section
    bOk := bOk Or CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES - [ptSectionStart, ptSectionStop], right, bNoSelectReadOnly);
    bOk := bOk Or CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES - [ptSectionStart, ptSectionStop], left, bNoSelectReadOnly);
    // finally, we openly search the oDocument in either direction
    bOk := bOk Or CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES, right, bNoSelectReadOnly);
    bOk := bOk Or CheckCursorPointScoped(iCursor, ALL_PIECE_TYPES, left, bNoSelectReadOnly);
  End;
  If bOk Then
    Result := iCursor
  Else
    Result := -1;
End;


Function TWPWorkingDocument.PieceForCursor(iCursor: Integer): TWPWorkingDocumentPiece;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  If GetPieceByPosition(iCursor, oPiece, iInternal, iIndex) Then
    Result := oPiece
  Else
  Begin
    Assert(False, 'Unable to find position '+IntegerToString(iCursor)+' in document with '+IntegerToString(FCharCount)+' size');
    Result := Nil;
  End;
End;


Function TWPWorkingDocument.PieceHoldsCursor(oPiece : TWPWorkingDocumentPiece; bNoSelectReadOnly : Boolean): Boolean;
Begin
  Result := (oPiece.PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptBreak, ptPara])
      And (Not bNoSelectReadOnly Or Not oPiece.IsReadOnly Or
     ((oPiece.PieceType = ptFieldStop) And (TWPWorkingDocumentFieldStopPiece(oPiece).MatchingStart.ReadOnly = tsFalse)));
End;



Function TWPWorkingDocument.NearestValidCursor(iCursor: Integer; aDirection : TWPCursorDirection; bNoSelectReadOnly : Boolean): Integer;
Begin
  Result := CheckCursorPoint(iCursor, aDirection, bNoSelectReadOnly);
End;

Function TWPWorkingDocument.GetJumpPoint(iCursor: Integer; aJumpType: TWPJumpType; iCount: Integer; bNoSelectReadOnly : Boolean; Out iNew: Integer): Boolean;
Var
  iLoop : Integer;
Begin
   Result := True;
  If iCount = 0 Then
  Begin
    iNew := iCursor;
  End
  Else
  Case aJumpType Of
    jtChar : iNew := iCursor + iCount;
    jtWord :
      Begin
        iNew := iCursor;
        For iLoop := 1 To Abs(iCount) Do
          If iCount < 0 Then
            WordLeft(iNew, bNoSelectReadOnly, iNew)
          Else
            WordRight(iNew, bNoSelectReadOnly, iNew);
      End;
    jtSentence :
      RaiseError('GetJumpPoint', 'Sentence Jump Not supported yet');
    jtParagraph :
      Begin
        iNew := iCursor;
        For iLoop := 1 To Abs(iCount) Do
          If iCount < 0 Then
          Begin
            ParaHome(iNew, bNoSelectReadOnly, iNew);
            Dec(iNew);
          End
          Else
          Begin
            ParaEnd(iNew, bNoSelectReadOnly, iNew);
            Inc(iNew);
          End;
      End;
    jtLine :
      RaiseError('GetJumpPoint', 'Line Jump Not supported yet');
    jtSection :
      Begin
        iNew := iCursor;
        For iLoop := 1 To Abs(iCount) Do
          If iCount < 0 Then
          Begin
            SectionHome(iNew, bNoSelectReadOnly, True, iNew);
            Dec(iNew);
          End
          Else
          Begin
            SectionEnd(iNew, bNoSelectReadOnly, True, iNew);
            Inc(iNew);
          End;
      End;
    jtCell :
      RaiseError('GetJumpPoint', 'Line Jump Not supported yet');
    jtRow :
      RaiseError('GetJumpPoint', 'Line Jump Not supported yet');
    jtTable :
      RaiseError('GetJumpPoint', 'Line Jump Not supported yet');
  Else //  jtUnknown
      RaiseError('GetJumpPoint', 'Unknown Jump Type');
  End;
  If iCount > 0 Then
    iNew := NearestValidCursor(iNew, cdRight, bNoSelectReadOnly)
  Else
    iNew := NearestValidCursor(iNew, cdLeft, bNoSelectReadOnly);
End;


Procedure TWPWorkingDocument.DumpToXml(oXml: TFslXmlFormatter);
Var
  iLoop : Integer;
Begin
  oXml.Attributes.add('fieldCount', IntegerToString(FFieldCount));
  oXml.Attributes.add('minLength', IntegerToString(FMinLength));
  oXml.Attributes.add('charCount', IntegerToString(FCharCount));
  oXml.Attributes.add('voiceCharCount', IntegerToString(FVoiceCharCount));
  oXml.Attributes.add('regenerationNeeded', BooleanToString(FRegenerationNeeded));
  oXml.ProduceOpen('document');
  For iLoop := 0 To Pieces.Count - 1 Do
    Pieces[iLoop].DumpToXml(oXml);
  oXml.ProduceClose('document');
End;


Function TWPWorkingDocument.FirstCursorPosition: Integer;
Begin
 Result := NearestValidCursor(0, cdJump, True);
End;

Procedure TWPWorkingDocument.SetFieldDefinitionProviders(Const Value: TWPFieldDefinitionProviderList);
Begin
  FFieldDefinitionProviders.Free;
  FFieldDefinitionProviders := Value;
End;

procedure TWPWorkingDocument.SetSourceBytes(const Value: TFslBuffer);
begin
  FSourceBytes.Free;
  FSourceBytes := Value;
end;

procedure TWPWorkingDocument.SetSourceObjectModel(const Value: TFslObject);
begin
  FSourceObjectModel.Free;
  FSourceObjectModel := Value;
end;

Procedure TWPWorkingDocument.SetAnnotationDefinitionProviders(Const Value: TWPAnnotationDefinitionProviderList);
Begin
  FAnnotationDefinitionProviders.Free;
  FAnnotationDefinitionProviders := Value;
End;

procedure TWPWorkingDocument.RelinkAttachments;
var
  i : integer;
  oWork : TWPWorkingDocumentPiece;
  oImage : TWPWorkingDocumentImagePiece;
  att : TWPWorkingAttachment;
begin
  for i := 0 to Pieces.Count - 1 do
  Begin
    oWork := Pieces[i];
    if (oWork.PieceType = ptImage) Then
    begin
      oImage := TWPWorkingDocumentImagePiece(oWork);
(*      if (oImage.Image is TWPPDFGraphic) then
      begin
        att := FAttachments.GetById(TWPPDFGraphic(oImage.Image).Attachment.Id);
        if att = nil then
          raise EWPException.create('Attachment mis-allocation for id = '+TWPPDFGraphic(oImage.Image).Attachment.Id);
        TWPPDFGraphic(oImage.Image).Attachment := att.Link;
      end;*)
    end;
  End;
end;

function TWPWorkingDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPieces.sizeInBytes);
  inc(result, FFieldDefinitionProviders.sizeInBytes);
  inc(result, FAnnotationDefinitionProviders.sizeInBytes);
  inc(result, FSourceObjectModel.sizeInBytes);
  inc(result, FSourceBytes.sizeInBytes);
  inc(result, FAllowedWords.sizeInBytes);
  inc(result, FAllAnnotations.sizeInBytes);
  inc(result, FDrawnAnnotations.sizeInBytes);
  inc(result, FAttachments.sizeInBytes);
end;

{ TWPWorkingDocumentTextPiece }

Procedure TWPWorkingDocumentTextPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  Content := TWPWorkingDocumentTextPiece(oSource).Content;
  SpellState := TWPWorkingDocumentTextPiece(oSource).SpellState;
  DrawnFont := TWPWorkingDocumentTextPiece(oSource).DrawnFont;
End;

Function TWPWorkingDocumentTextPiece.Clone: TWPWorkingDocumentTextPiece;
Begin
  Result := TWPWorkingDocumentTextPiece(Inherited Clone);
End;

Function TWPWorkingDocumentTextPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptText;
End;

Function TWPWorkingDocumentTextPiece.GetLogicalText: String;
Begin
  Result := FContent;
End;

Function TWPWorkingDocumentTextPiece.Link: TWPWorkingDocumentTextPiece;
Begin
  Result := TWPWorkingDocumentTextPiece(Inherited Link);
End;

Procedure TWPWorkingDocumentTextPiece.SetContent(Const sValue: String);
Begin
  If Length(sValue) > MAX_WORD_LENGTH Then
    RaiseError('SetContent', 'Word Length is too long');

  FContent := sValue;
  Metrics.CharCount := Length(sValue);
  Metrics.VoiceCharCount := Length(sValue);
  If IsWordBreak(sValue) Then
    FSpellState := scsExempt
  Else
    FSpellState := scsNotChecked;
  Change(ctTextContents, Self);
End;

Function TWPWorkingDocumentTextPiece.Split(iOffset: Integer): TWPWorkingDocumentPiece;
Begin
  Result := TWPWorkingDocumentTextPiece.Create;
  Try
    TWPWorkingDocumentTextPiece(Result).Content := Copy(FContent, iOffset + 1, MAXINT);
    TWPWorkingDocumentTextPiece(Result).DrawnFont := DrawnFont;
    Content := Copy(FContent, 1, iOffset);
    Result.Style := Style;
    Result.Font.Assign(Font);
    result.AnnotationId := AnnotationId;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TWPWorkingDocumentTextPiece.CloneStart(iOffset : Integer) : TWPWorkingDocumentTextPiece;
Begin
  Result := TWPWorkingDocumentTextPiece.Create;
  Try
    Result.Content := Copy(FContent, 1, iOffset);
    Result.Style := Style;
    Result.Font.Assign(Font);
    result.AnnotationId := AnnotationId;
    Result.DrawnFont := DrawnFont;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TWPWorkingDocumentTextPiece.CloneEnd(iOffset : Integer) : TWPWorkingDocumentTextPiece;
Begin
  Result := TWPWorkingDocumentTextPiece.Create;
  Try
    Result.Content := Copy(FContent, iOffset + 1, MAXINT);
    Result.Style := Style;
    Result.Font.Assign(Font);
    Result.DrawnFont := DrawnFont;
    result.AnnotationId := AnnotationId;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPWorkingDocumentTextPiece.SetSpellState(Const Value: TWPWorkingDocumentSpellCheckingState);
Begin
  FSpellState := Value;
  Change(ctPresentation, Self);
End;


Function TWPWorkingDocumentTextPiece.GetVoiceText : String;
Begin
  Result := FContent;
End;


Function TWPWorkingDocumentTextPiece.CanCopyAsText : Boolean;
Begin
  Result := True;
End;


Procedure TWPWorkingDocumentTextPiece.CollectAttributes(oXml: TFslXmlFormatter);
Begin
  Inherited;
  oXml.Attributes.add('Content', FContent);
  oXml.Attributes.add('SpellState', WPSPELLCHECKINGSTATE_NAMES[FSpellState]);
End;

function TWPWorkingDocumentTextPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'content', FContent);
  prop(result, 'drawn-font', FDrawnFont);
end;

Function TWPWorkingDocumentTextPiece.XmlName: String;
Begin
  Result := 'text';
End;

function TWPWorkingDocumentTextPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FContent.length * sizeof(char)) + 12);
  inc(result, (FDrawnFont.length * sizeof(char)) + 12);
end;

{ TWPWorkingDocumentLineBreakPiece }

Constructor TWPWorkingDocumentLineBreakPiece.Create;
Begin
  Inherited;
  Metrics.VoiceCharCount := Length(GetVoiceText);
End;


Function TWPWorkingDocumentLineBreakPiece.Clone: TWPWorkingDocumentLineBreakPiece;
Begin
  Result := TWPWorkingDocumentLineBreakPiece(Inherited Clone);
End;

Function TWPWorkingDocumentLineBreakPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

function TWPWorkingDocumentLineBreakPiece.DescribeV: String;
begin
  result:= inherited describeV;
  prop(result, 'speechmagic', FIsSpeechMagicParagraph);
end;

Function TWPWorkingDocumentLineBreakPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptLineBreak;
End;

Function TWPWorkingDocumentLineBreakPiece.GetVisualText: String;
Begin
  Result := CHAR_BREAK_HINT;
End;

Function TWPWorkingDocumentLineBreakPiece.Link: TWPWorkingDocumentLineBreakPiece;
Begin
  Result := TWPWorkingDocumentLineBreakPiece(Inherited Link);
End;


Function TWPWorkingDocumentLineBreakPiece.IsLineBreak : Boolean;
Begin
  Result := True;
End;


Function TWPWorkingDocumentLineBreakPiece.GetVoiceText : String;
Begin
  Result := #13#10;
End;

Procedure TWPWorkingDocumentLineBreakPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  FIsSpeechMagicParagraph := TWPWorkingDocumentLineBreakPiece(oSource).FIsSpeechMagicParagraph;
End;

function TWPWorkingDocumentLineBreakPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TWPWorkingDocumentImagePiece }

Constructor TWPWorkingDocumentImagePiece.Create;
Begin
  Inherited;
  FTransparentColour := DEF_COLOUR;
  FAdornments := TWPDocumentImageAdornments.Create;
  FDefaultAdornmentColour := clBlack;
End;

function TWPWorkingDocumentImagePiece.DescribeV: String;
begin
  result := inherited DescribeV;

  prop(result, 'def-colour', FDefaultAdornmentColour);
  prop(result, 'border', FBorder);
  prop(result, 'height', FHeight);
  prop(result, 'width', FWidth);
  prop(result, 'border-colour', FBorderColour);
  prop(result, 'name', FName);
  prop(result, 'transparent-colour', FTransparentColour);
  prop(result, 'vert-align', Ord(FVerticalAlignment), NAMES_WORDPROCESSORIMAGEVERTICALALIGNMENT);
  prop(result, 'size-policy', Ord(FSizePolicy), NAMES_WORDPROCESSORIMAGESIZEPOLICY);
  prop(result, 'FrameIndex', FFrameIndex);
end;

Destructor TWPWorkingDocumentImagePiece.Destroy;
Begin
  FAdornments.Free;
  FImage.Free;
  FSelectionImage.Free;
  FImageMap.UnHook(ImageMapChange);
  FImageMap.Free;
  FWorkingImage.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentImagePiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  FDefaultAdornmentColour := clWhite;
  Image := TWPWorkingDocumentImagePiece(oSource).FImage.Clone;
  FDefaultAdornmentColour := TWPWorkingDocumentImagePiece(oSource).FDefaultAdornmentColour;
  SelectionImage := TWPWorkingDocumentImagePiece(oSource).FSelectionImage.Clone;
  FBorder := TWPWorkingDocumentImagePiece(oSource).FBorder;
  FSizePolicy := TWPWorkingDocumentImagePiece(oSource).SizePolicy;
  FHeight := TWPWorkingDocumentImagePiece(oSource).FHeight;
  FWidth  := TWPWorkingDocumentImagePiece(oSource).FWidth;
  FBorderColour := TWPWorkingDocumentImagePiece(oSource).FBorderColour;
  FTransparentColour := TWPWorkingDocumentImagePiece(oSource).FTransparentColour;
  FName   := TWPWorkingDocumentImagePiece(oSource).FName;
  FFrameIndex := TWPWorkingDocumentImagePiece(oSource).FFrameIndex;

  // There is no set on the imagemap property
  FImageMap.UnHook(ImageMapChange);
  FImageMap.Free;
  FImageMap := TWPWorkingDocumentImagePiece(oSource).FImageMap.Clone;
  FImageMap.Hook(ImageMapChange);
  FWorkingImage.Free;
  FWorkingImage := Nil;

  FAdornments.Assign(TWPWorkingDocumentImagePiece(oSource).FAdornments);

  VerticalAlignment := TWPWorkingDocumentImagePiece(oSource).VerticalAlignment;
  ApplyTransparency;
End;

Procedure TWPWorkingDocumentImagePiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Begin
  Inherited;
  Image := TWPWorkingDocumentImagePiece(oSource).FImage.Clone;
  SelectionImage := TWPWorkingDocumentImagePiece(oSource).FSelectionImage.Clone;
  FBorder := TWPWorkingDocumentImagePiece(oSource).FBorder;
  FHeight := TWPWorkingDocumentImagePiece(oSource).FHeight;
  FWidth  := TWPWorkingDocumentImagePiece(oSource).FWidth;
  FBorderColour := TWPWorkingDocumentImagePiece(oSource).FBorderColour;
  FTransparentColour := TWPWorkingDocumentImagePiece(oSource).FTransparentColour;
  FSizePolicy := TWPWorkingDocumentImagePiece(oSource).FSizePolicy;
  FName   := TWPWorkingDocumentImagePiece(oSource).FName;
  FrameIndex := TWPWorkingDocumentImagePiece(oSource).FrameIndex;

  // There is no set on the imagemap property
  FImageMap.UnHook(ImageMapChange);
  FImageMap.Free;
  FImageMap := TWPWorkingDocumentImagePiece(oSource).FImageMap.Clone;
  FImageMap.Hook(ImageMapChange);
  FWorkingImage.Free;
  FWorkingImage := Nil;

  VerticalAlignment := TWPWorkingDocumentImagePiece(oSource).VerticalAlignment;
  ApplyTransparency;
End;


Function TWPWorkingDocumentImagePiece.Clone: TWPWorkingDocumentImagePiece;
Begin
  Result := TWPWorkingDocumentImagePiece(Inherited Clone);
End;


Function TWPWorkingDocumentImagePiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;


Function TWPWorkingDocumentImagePiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptImage;
End;

Function TWPWorkingDocumentImagePiece.GetVisualText: String;
Begin
  Result := CHAR_IMAGE_HINT;
End;

Function TWPWorkingDocumentImagePiece.ImageFileExtension: String;
Begin
  If FName <> '' Then
    Result := PathExtension(FName)
  {$IFDEF DELPHI}
  Else If (FImage is TFslVCLGraphic) and (TFslVCLGraphic(FImage).Handle Is TGraphicExGraphic) Then
    Result := Copy(TFslVCLGraphic(FImage).Handle.ClassName, 2, Length(TFslVCLGraphic(FImage).Handle.ClassName) - 8)
  {$ENDIF}
  Else If (FImage is TFslVCLGraphic) and (TFslVCLGraphic(FImage).Handle Is TJPEGImage) Then
    Result := 'jpg'
(*
  Else if FImage is TWPPDFGraphic then
    result := 'pdf'
*)
  Else
    Result := 'bmp';
End;

Function TWPWorkingDocumentImagePiece.ImageTypename: String;
Begin
  {$IFDEF DELPHI}
  If (FImage is TFslVCLGraphic) and (TFslVCLGraphic(FImage).Handle Is TGraphicExGraphic) Then
    Result := Copy(TFslVCLGraphic(FImage).Handle.ClassName, 2, Length(TFslVCLGraphic(FImage).Handle.ClassName) - 8)
  Else
  {$ENDIF}
    If (FImage is TFslVCLGraphic) and (TFslVCLGraphic(FImage).Handle Is TJPEGImage) Then
    Result := 'JPEG'
(*
  Else If (FImage is TWPPDFGraphic) Then
    Result := 'PDF'
*)
  Else
    Result := 'Bitmap';
End;

Function TWPWorkingDocumentImagePiece.Link: TWPWorkingDocumentImagePiece;
Begin
  Result := TWPWorkingDocumentImagePiece(Inherited Link);
End;

Procedure TWPWorkingDocumentImagePiece.SetBorder(Const Value: Integer);
Begin
  FBorder := Value;
  Change(ctLayout, Self);
End;

Procedure TWPWorkingDocumentImagePiece.SetBorderColour(Const Value: TColour);
Begin
  FBorderColour := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentImagePiece.SetTransparentColour(Const Value: TColour);
Begin
  FTransparentColour := Value;
  ApplyTransparency;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentImagePiece.SetHeight(Const Value: Integer);
Begin
  FHeight := Value;
  FWorkingImage.Free;
  FWorkingImage := Nil;
  Change(ctLayout, Self);
End;

Procedure TWPWorkingDocumentImagePiece.SetImage(Const Value: TFslGraphic);
Begin
  FImage.Free;
  FImage := Value;
  FWorkingImage.Free;
  FWorkingImage := Nil;
  FImage.FrameIndex := FFrameIndex;
  ApplyTransparency;
  Change(ctTextContents, Self);
  If FDefaultAdornmentColour <> clWhite Then
  Begin
    FDefaultAdornmentColour := clBlack;
    DefaultAdornmentColour;
  End;
End;

procedure TWPWorkingDocumentImagePiece.SetFrameIndex(const Value: integer);
begin
  FFrameIndex := Value;
  if assigned(FImage) then
    FImage.FrameIndex := FFrameIndex;
  Change(ctTextContents, Self);
end;

Procedure TWPWorkingDocumentImagePiece.SetSelectionImage(Const Value: TFslGraphic);
Begin
  FSelectionImage.Free;
  FSelectionImage := Value;
  FWorkingImage.Free;
  FWorkingImage := Nil;
  ApplyTransparency;
  Change(ctPresentation, Self);
End;

procedure TWPWorkingDocumentImagePiece.SetSizePolicy(const Value: TWordProcessorImageSizePolicy);
begin
  FSizePolicy := Value;
  FWorkingImage.Free;
  FWorkingImage := Nil;
  Change(ctLayout, Self);
end;

Procedure TWPWorkingDocumentImagePiece.SetWidth(Const Value: Integer);
Begin
  FWidth := Value;
  FWorkingImage.Free;
  FWorkingImage := Nil;
  Change(ctLayout, Self);
End;


Function TWPWorkingDocumentImagePiece.GetImage : TFslGraphic;
Begin
  Assert(Invariants('GetImage', FImage, TFslGraphic, 'Image'));
  Result := FImage;
End;


Function TWPWorkingDocumentImagePiece.HasImage : Boolean;
Begin
  Result := Assigned(FImage);
End;


Function TWPWorkingDocumentImagePiece.GetSelectionImage : TFslGraphic;
Begin
  Assert(Invariants('GetSelectionImage', FSelectionImage, TFslGraphic, 'SelectionImage'));
  Result := FSelectionImage;
End;


Function TWPWorkingDocumentImagePiece.HasSelectionImage : Boolean;
Begin
  Result := Assigned(FSelectionImage);
End;


Function TWPWorkingDocumentImagePiece.GetImageMap : TWPImageMap;
Begin
  Assert(Invariants('GetImageMap', FImageMap, TWPImageMap, 'ImageMap'));
  Result := FImageMap;
End;


Function TWPWorkingDocumentImagePiece.GetHasImageMap : Boolean;
Begin
  Result := Assigned(FImageMap);
End;


Procedure TWPWorkingDocumentImagePiece.SetHasImageMap(bValue : Boolean);
Begin
  If Not bValue Then
  Begin
    FImageMap.Free;
    FImageMap := Nil;
  End
  Else If Not Assigned(FImageMap) Then
  Begin
    FImageMap := TWPImageMap.Create;
    FImageMap.Hook(ImageMapChange);
  End;
End;

Procedure TWPWorkingDocumentImagePiece.ApplyTransparency;
Begin
  If Assigned(FImage) And (FImage is TFslVCLGraphic) and (TFslVCLGraphic(FImage).Handle Is TBitmap) Then
  Begin
    If TransparentColour = DEF_COLOUR Then
      TBitmap(TFslVCLGraphic(FImage).Handle).Transparent := False
    Else
    Begin
      TBitmap(TFslVCLGraphic(FImage).Handle).Transparent := True;
      TBitmap(TFslVCLGraphic(FImage).Handle).TransparentMode := tmFixed;
      TBitmap(TFslVCLGraphic(FImage).Handle).TransparentColor := TransparentColour;
    End;
  End;
  If Assigned(FSelectionImage) And (FSelectionImage is TFslVCLGraphic) and (TFslVCLGraphic(FSelectionImage).Handle Is TBitmap) Then
  Begin
    If TransparentColour = DEF_COLOUR Then
      TBitmap(TFslVCLGraphic(FSelectionImage).Handle).Transparent := False
    Else
    Begin
      TBitmap(TFslVCLGraphic(FSelectionImage).Handle).Transparent := True;
      TBitmap(TFslVCLGraphic(FSelectionImage).Handle).TransparentMode := tmFixed;
      TBitmap(TFslVCLGraphic(FSelectionImage).Handle).TransparentColor := TransparentColour;
    End;
  End;
End;


Function TWPWorkingDocumentImagePiece.GetWorkingImage(aWidth, aHeight : Integer; bHotspots, bFast : Boolean): TFslGraphic;
var
  bGen : boolean;
Begin
  bGen :=
    (FAdornments.Count > 0) or // if we have adornments, we cannot use the underlying image
    (not (FImage Is TFslVCLGraphic) and (aWidth <> 0) and bFast) or // if it's not a VCL bitmap, we'll generate one and use it as a cache (unless width = 0, or not bfast, in which case, we want the source)
    (bHotspots And HasImageMap And ImageMap.HasSelection And HasSelectionImage And (FImage Is TFslBitmapGraphic)); // or we have image map and hotspots (phasing it)
  If not bGen Then
    Result := FImage
  Else
  Begin
    If Not Assigned(FWorkingImage) or (FWorkingWidth <> aWidth) or (FWorkingHeight <> aHeight) Then
      BuildWorkingImage(aWidth, aHeight, bHotspots);
    Result := FWorkingImage;
  End;
End;


// the problem that leads to the double handling is that MaskBlt doesn't
// work at anything other than 100%. So we prepare an intermediate merged
// image using MaskBlt at native image size, and store this in the working
// image
// An alternative reason to build a working image is that we want to cache
// up drawn adornments

Procedure TWPWorkingDocumentImagePiece.BuildWorkingImage;
Var
  oMask : TFslBitmapGraphic;
  oTemp : TFslBitmapGraphic;
  iLoop : Integer;
  rect : TRect;
Begin
  oTemp := MakeImage;
  Try
    if FImage is TFslVCLGraphic then
      oTemp.Handle.Canvas.Draw(0,0, TFslVCLGraphic(FImage).Handle)
    else
    begin
      rect.Left := 0;
      rect.Top := 0;
      rect.Right := rect.Left + oTemp.Width;
      rect.Bottom := rect.Top + oTemp.Height;
      FImage.StretchDraw(oTemp.Handle.Canvas, rect);
    end;
    FWorkingWidth := aWidth;
    FWorkingHeight := aHeight;
    // 1st, do we have a selection:
    If bHotspots And HasImageMap And ImageMap.HasSelection And HasSelectionImage And (FImage Is TFslBitmapGraphic) Then
    Begin
      oMask := MakeImage;
      Try
        oMask.Handle.Monochrome := True;
        FillWithWhite(oMask);
        DrawSection(oMask);
        Transfer(oTemp, oMask);
      Finally
        oMask.Free;
      End;
    End;
    // draw any annotations
    For iLoop := 0 To FAdornments.Count - 1 Do
      DrawAdornment(oTemp, FAdornments[iLoop]);
    FWorkingImage := oTemp.Link;
  Finally
    oTemp.Free;
  End;
End;


Function TWPWorkingDocumentImagePiece.MakeImage: TFslBitmapGraphic;
Begin
  Result := TFslBitmapGraphic.Create;
  Try
    Result.Height := FImage.Height;
    Result.Width := FImage.Width;
    Result.Handle.HandleType := bmDIB;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPWorkingDocumentImagePiece.FillWithWhite(oMask : TFslBitmapGraphic);
Begin
  oMask.Handle.Canvas.Pen.Width := 1;
  oMask.Handle.Canvas.Pen.Color := clWhite;
  oMask.Handle.Canvas.Pen.Style := psSolid;
  oMask.Handle.Canvas.Brush.Color := clWhite;
  oMask.Handle.Canvas.Rectangle(0,0, FImage.Width, FImage.Height);
End;


Procedure TWPWorkingDocumentImagePiece.DrawSection(oMask : TFslBitmapGraphic);
Var
  aPoints : Array Of TPoint;
  iLoop : Integer;
  oCoords : TWPCoordinateList;
Begin
  oCoords := ImageMap.Selection.Coordinates;
  oMask.Handle.Canvas.Pen.Width := 1;
  oMask.Handle.Canvas.Pen.Color := clBlack;
  oMask.Handle.Canvas.Pen.Style := psSolid;
  oMask.Handle.Canvas.Pen.Handle := MakePenHandle(oMask.Handle.Canvas.Pen, apesRound, apjsRound);
  oMask.Handle.Canvas.Brush.Color := clBlack;
  SetLength(aPoints, oCoords.Count+1);
  For iLoop := 0 To oCoords.Count - 1 Do
  Begin
    aPoints[iLoop].X := oCoords[iLoop].X;
    aPoints[iLoop].Y := oCoords[iLoop].Y;
  End;
  aPoints[oCoords.Count].X := oCoords[0].X;
  aPoints[oCoords.Count].Y := oCoords[0].Y;
  oMask.Handle.Canvas.Polygon(aPoints);
End;


Procedure TWPWorkingDocumentImagePiece.Transfer(oTemp, oMask : TFslBitmapGraphic);
Const
  ROP_DstCopy = $00AA0029;
Begin
  Win32Check(
     MaskBlt(oTemp.Handle.Canvas.Handle, 0, 0, FImage.Width, FImage.Height,
             TFslBitmapGraphic(FSelectionImage).Handle.Canvas.Handle, 0, 0,
             oMask.Handle.Handle, 0, 0, MakeRop4(ROP_DstCopy, SRCCOPY)));
End;

Procedure TWPWorkingDocumentImagePiece.ImageMapChange;
Begin
  FWorkingImage.Free;
  FWorkingImage := Nil;
  Change(aType, oSource);
End;

Function TWPWorkingDocumentImagePiece.MakePenHandle(oPen : TPen; aEndStyle : TFslPenEndStyle; aJoinStyle : TFslPenJoinStyle): HPEN;
Var
  aBrush : TLogBrush;
Const
  PenStyles: Array[TPenStyle] Of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME, PS_SOLID{$IFDEF DELPHI}, PS_SOLID{$ENDIF});
  PenModes: Array[TPenMode] Of Word =
    (R2_BLACK, R2_WHITE, R2_NOP, R2_NOT, R2_COPYPEN, R2_NOTCOPYPEN, R2_MERGEPENNOT,
     R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN, R2_MERGEPEN, R2_NOTMERGEPEN,
     R2_MASKPEN, R2_NOTMASKPEN, R2_XORPEN, R2_NOTXORPEN);
Begin
  FillChar(aBrush, SizeOf(TLogBrush), 0);
  aBrush.lbStyle := BS_SOLID;
  aBrush.lbColor := ColorToRGB(oPen.Color);
  aBrush.lbHatch := 0;

  Result := ExtCreatePen(
     {PenStyle} PS_GEOMETRIC + PenStyles[oPen.Style] +PS_ENDCAP_FLAT +PS_JOIN_MITER,
     {Width }   IntegerMax(1, oPen.Width), // pen width can never be less than 1
     {Brush} aBrush, 0, Nil);
  SetROP2(Result, PenModes[oPen.Mode]);

End;


function TWPWorkingDocumentImagePiece.MaxFrameIndex: Integer;
begin
  result := Image.FrameCount;
end;

Function TWPWorkingDocumentImagePiece.FactorX: Real;
Begin
  Result := Width/Image.Width;
End;


Function TWPWorkingDocumentImagePiece.FactorY: Real;
Begin
  Result := Height/Image.Height;
End;



Procedure TWPWorkingDocumentImagePiece.DrawAdornment(oImage: TFslBitmapGraphic; oAdornment: TWPDocumentImageAdornment);
Begin
  Case oAdornment.AdornmentType Of
    iatLine: DrawAdornmentLine(oImage, oAdornment);
    iatRectangle: DrawAdornmentShape(oImage, oAdornment);
    iatCircle: DrawAdornmentShape(oImage, oAdornment);
    iatZoom: DrawAdornmentZoom(oImage, oAdornment);
    iatMark: DrawAdornmentMark(oImage, oAdornment);
  End;
End;

Procedure TWPWorkingDocumentImagePiece.DrawAdornmentLine(oImage: TFslBitmapGraphic; oAdornment: TWPDocumentImageAdornment);
Var
  points : Array Of TPoint;
  i : Integer;
  oCanvas : TCanvas;
Begin
  oCanvas := oImage.Handle.Canvas;
  oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[oAdornment.PenStyle];
  oCanvas.Pen.Color := oAdornment.PenColour;
  oCanvas.Pen.Width := oAdornment.PenWidth;
  SetLength(points, oAdornment.Coordinates.Count);
  For i := 0 To oAdornment.Coordinates.Count - 1 Do
    points[i] := oAdornment.Coordinates[i].AsPoint;
  oImage.Handle.Canvas.Polyline(points);

  If (oAdornment.Caption <> '') Then
  Begin
    oCanvas.Brush.Style := bsClear;
    oCanvas.Font.Color := oAdornment.PenColour;
    oAdornment.Font.Apply(oCanvas.Font, False);
    oCanvas.TextOut(oAdornment.CaptionPoint.X, oAdornment.CaptionPoint.Y, oAdornment.Caption);
  End;
End;

Procedure TWPWorkingDocumentImagePiece.DrawAdornmentMark(oImage: TFslBitmapGraphic; oAdornment: TWPDocumentImageAdornment);
Var
  oCanvas : TCanvas;
  w, hp : Integer;
Begin
  oCanvas := oImage.Handle.Canvas;
  oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[oAdornment.PenStyle];
  oCanvas.Pen.Color := oAdornment.PenColour;
  oCanvas.Pen.Width := oAdornment.PenWidth;
  oCanvas.MoveTo(oAdornment.Coordinates[0].X - MARK_OUT, oAdornment.Coordinates[0].Y);
  oCanvas.LineTo(oAdornment.Coordinates[0].X - MARK__IN, oAdornment.Coordinates[0].Y);
  oCanvas.MoveTo(oAdornment.Coordinates[0].X + MARK_OUT, oAdornment.Coordinates[0].Y);
  oCanvas.LineTo(oAdornment.Coordinates[0].X + MARK__IN, oAdornment.Coordinates[0].Y);
  oCanvas.MoveTo(oAdornment.Coordinates[0].X, oAdornment.Coordinates[0].Y+MARK_OUT);
  oCanvas.LineTo(oAdornment.Coordinates[0].X, oAdornment.Coordinates[0].Y+MARK__IN);
  oCanvas.MoveTo(oAdornment.Coordinates[0].X, oAdornment.Coordinates[0].Y-MARK_OUT);
  oCanvas.LineTo(oAdornment.Coordinates[0].X, oAdornment.Coordinates[0].Y-MARK__IN);

  oAdornment.Font.Apply(oCanvas.Font, False);
  w := oCanvas.TextWidth(oAdornment.Caption);

  // how this is drawn: we draw text to the preferred side if we can, or on the other side if it fits there
  // the preferred side is to the left if the captionpoint is to the left of the mark, or vice versa
  If oAdornment.Coordinates[0].X < oAdornment.CaptionPoint.X Then
  Begin
    // to the right
    If (oAdornment.CaptionPoint.X + 5 + w > oImage.Width) And (oAdornment.CaptionPoint.X - (w + 5) >= 0) Then
      hp := oAdornment.CaptionPoint.X - (w + 5)
    Else
      hp := oAdornment.CaptionPoint.X + 5;
  End
  Else
  Begin
    // to the left
    If (oAdornment.CaptionPoint.X - (5 + w) < 0) And (oAdornment.CaptionPoint.X + (w + 5) <= oImage.Width) Then
      hp := oAdornment.CaptionPoint.X + 5
    Else
      hp := oAdornment.CaptionPoint.X - (w + 5);
  End;
  oCanvas.MoveTo(oAdornment.Coordinates[0].X, oAdornment.Coordinates[0].Y);
  oCanvas.LineTo(oAdornment.CaptionPoint.X, oAdornment.CaptionPoint.Y);
  oCanvas.Brush.Style := bsClear;
  oCanvas.Font.Color := oAdornment.PenColour;
  oAdornment.Font.Apply(oCanvas.Font, False);

  oCanvas.TextOut(hp, oAdornment.CaptionPoint.Y - oCanvas.TextHeight(oAdornment.Caption) Div 2, oAdornment.Caption);
End;

Procedure TWPWorkingDocumentImagePiece.DrawAdornmentShape(oImage: TFslBitmapGraphic; oAdornment: TWPDocumentImageAdornment);
Var
  l, t, b, r : Integer;
  oCanvas : TCanvas;
Begin
  oCanvas := oImage.Handle.Canvas;
  oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[oAdornment.PenStyle];
  oCanvas.Pen.Color := oAdornment.PenColour;
  oCanvas.Pen.Width := oAdornment.PenWidth;
  oCanvas.Brush.Style := bsClear;

  l := oAdornment.Coordinates[0].X;
  t := oAdornment.Coordinates[0].y;
  r := oAdornment.Coordinates[1].X;
  b := oAdornment.Coordinates[1].Y;

  If oAdornment.AdornmentType = iatCircle Then
    oCanvas.Ellipse(l, t, r, b)
  Else
    oCanvas.Rectangle(l, t, r, b);

  If (oAdornment.Caption <> '') Then
  Begin
    oCanvas.Brush.Style := bsClear;
    oCanvas.Font.Color := oAdornment.PenColour;
    oAdornment.Font.Apply(oCanvas.Font, False);
    oCanvas.TextOut(oAdornment.CaptionPoint.X, oAdornment.CaptionPoint.Y, oAdornment.Caption);
  End;
End;

Procedure TWPWorkingDocumentImagePiece.DrawAdornmentZoom(oImage: TFslBitmapGraphic; oAdornment: TWPDocumentImageAdornment);
Var
  l1, t1, r1, b1 : Integer;
  l2, t2, r2, b2 : Integer;
  oCanvas : TCanvas;
  oClip : THandle;
Begin
  oCanvas := oImage.Handle.Canvas;
  oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[oAdornment.PenStyle];
  oCanvas.Pen.Color := oAdornment.PenColour;
  oCanvas.Pen.Width := oAdornment.PenWidth;
  oCanvas.Brush.Style := bsClear;

  L1 := oAdornment.Coordinates[0].X;
  T1 := oAdornment.Coordinates[0].y;
  R1 := oAdornment.Coordinates[1].X;
  B1 := oAdornment.Coordinates[1].Y;
  L2 := oAdornment.Coordinates[2].X;
  T2 := oAdornment.Coordinates[2].y;
  R2 := oAdornment.Coordinates[3].X;
  B2 := oAdornment.Coordinates[3].Y;

  // first we draw the connectors
  oClip := CreateRectRgn(L1, T1, R1, B1);
  Try
    ExtSelectClipRgn(oCanvas.Handle, oClip, RGN_DIFF);
    oCanvas.MoveTo(L1, T1);
    oCanvas.LineTo(L2, T2);
    oCanvas.MoveTo(R1, T1);
    oCanvas.LineTo(R2, T2);
    oCanvas.MoveTo(L1, B1);
    oCanvas.LineTo(L2, B2);
    oCanvas.MoveTo(R1, B1);
    oCanvas.LineTo(R2, B2);
    SelectClipRgn(oCanvas.Handle, 0);
  Finally
    DeleteObject(oClip);
  End;

  // second, we draw the zoomed image
  StretchBlt(oCanvas.Handle, l2, t2, r2-l2, b2-t2,
             oCanvas.Handle, l1, t1, r1-l1, b1-t1,
             SRCCOPY);

  // now we draw the connectors etc
  oCanvas.Rectangle(l1, t1, r1, b1);
  oCanvas.Rectangle(l2, t2, r2, b2);

  If (oAdornment.Caption <> '') Then
  Begin
    oCanvas.Brush.Style := bsClear;
    oCanvas.Font.Color := oAdornment.PenColour;
    oAdornment.Font.Apply(oCanvas.Font, False);
    oCanvas.TextOut(oAdornment.CaptionPoint.X, oAdornment.CaptionPoint.Y, oAdornment.Caption);
  End;
End;

Procedure TWPWorkingDocumentImagePiece.ChangeAdornments;
Begin
  FWorkingImage.Free;
  FWorkingImage := Nil;
  Change(ctTextContents, Self);
End;

Function TWPWorkingDocumentImagePiece.IsAdornment(oInfo : TWPMouseInfo; iX, iY : Integer) : Boolean;
Var
  i : Integer;
  aPart : TAdornmentPart;
  aPoint : TPoint;
  aAction : TAdornmentAction;
Begin
  Result := False;
  aPoint.X := iX;
  aPoint.Y := iY;
  For i := Adornments.Count - 1 DownTo 0 Do
  Begin
    If AdornmentMatches(Adornments[i], aPoint, aPart, aAction) Then
    Begin
      oInfo.Adornment := Adornments[i].Link;
      oInfo.AdornmentPart := aPart;
      oInfo.AdornmentAction := aAction;
      Result := True;
    End;
  End
End;

Function TWPWorkingDocumentImagePiece.AdornmentMatches(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Begin
  Case oAdornment.AdornmentType Of
    iatLine: Result := AdornmentMatchesLine(oAdornment, aPoint, aPart, aAction);
    iatRectangle: Result := AdornmentMatchesRectangle(oAdornment, aPoint, aPart, aAction);
    iatCircle: Result := AdornmentMatchesCircle(oAdornment, aPoint, aPart, aAction);
    iatZoom: Result := AdornmentMatchesZoom(oAdornment, aPoint, aPart, aAction);
    iatMark: Result := AdornmentMatchesMark(oAdornment, aPoint, aPart, aAction);
  Else
    RaiseError('AdornmentMatches', 'Unhandled TWPDocumentImageAdornmentType.');

    Result := False;
  End;
End;

Function TWPWorkingDocumentImagePiece.AdornmentMatchesLine(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Var
  i : Integer;
Begin
  aAction := aaMove; 
  Result := False;
  i := 0;
  While (Not Result) And (i < oAdornment.Coordinates.count) Do
  Begin
    Result := (Abs(oAdornment.Coordinates[i].X - aPoint.x) < 5) And (Abs(oAdornment.Coordinates[i].Y - aPoint.y) < 5);
    Inc(i);
  End;
  If Result Then
    aPart := apAll
  Else
  Begin
    Result := CaptionMatches(oAdornment, aPoint);
    If Result Then
      aPart := apCaption;
  End;
End;

Function TWPWorkingDocumentImagePiece.AdornmentMatchesRectangle(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Var
  l, t, b, r : Integer;
Begin
  l := oAdornment.Coordinates[0].X;
  t := oAdornment.Coordinates[0].y;
  r := oAdornment.Coordinates[1].X;
  b := oAdornment.Coordinates[1].Y;

  Result := False;
  If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
  Begin
    Result := True;
    aAction := aaDragTopLeft;
  End
  Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
  Begin
    Result := True;
    aAction := aaDragTopRight;
  End
  Else If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
  Begin
    Result := True;
    aAction := aaDragBottomLeft;
  End
  Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
  Begin
    Result := True;
    aAction := aaDragBottomRight;
  End
  Else If
    { along top}    ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Or
    { along bottom} ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Or
    { along left}   ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < l + 5) And (aPoint.x > l - 5)) Or
    { along right}  ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < r + 5) And (aPoint.x > r - 5)) Then
  Begin
    Result := True;
    aAction := aaMove;
  End;

  If Result Then
    aPart := apAll
  Else
  Begin
    Result := CaptionMatches(oAdornment, aPoint);
    If Result Then
      aPart := apCaption;
  End;
End;


Function TWPWorkingDocumentImagePiece.AdornmentMatchesCircle(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Var
  l, t, b, r : Integer;
  hVal, vval, d, q : Double;
Begin
  l := oAdornment.Coordinates[0].X;
  t := oAdornment.Coordinates[0].y;
  r := oAdornment.Coordinates[1].X;
  b := oAdornment.Coordinates[1].Y;

  hVal := (aPoint.X - (l + r) Div 2) / (r - l) * 2;
  vVal := (aPoint.Y - (t + b) Div 2) / (b - t) * 2;
  d := Sqrt(hVal * hVal + vVal * vVal);
  q := Sqrt(Sqr(5 / (r - l)) + sqr(5 / (t - b)));

 Result := False;
  If ((aPoint.x > ((l+r)Div 2)-5) And (aPoint.x < ((l+r)Div 2) + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
  Begin
    Result := True;
    aAction := aaDragTop;
  End
  Else If ((aPoint.x > ((l+r)Div 2)-5) And (aPoint.x < ((l+r)Div 2) + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
  Begin
    Result := True;
    aAction := aaDragBottom;
  End
  Else If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < ((t+b)Div 2) + 5) And (aPoint.y > ((t+b)Div 2) - 5)) Then
  Begin
    Result := True;
    aAction := aaDragLeft;
  End
  Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < ((t+b)Div 2) + 5) And (aPoint.y > ((t+b)Div 2) - 5)) Then
  Begin
    Result := True;
    aAction := aaDragRight;
  End
  Else If abs(1- d) < q Then
  Begin
    Result := True;
    aAction := aaMove;
  End;

  If Result Then
    aPart := apAll
  Else
  Begin
    Result := CaptionMatches(oAdornment, aPoint);
    If Result Then
      aPart := apCaption;
  End;
End;

Function TWPWorkingDocumentImagePiece.AdornmentMatchesZoom(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Var
  l, t, b, r : Integer;
Begin
  l := oAdornment.Coordinates[0].X;
  t := oAdornment.Coordinates[0].y;
  r := oAdornment.Coordinates[1].X;
  b := oAdornment.Coordinates[1].Y;

  Result := False;
  If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
  Begin
    Result := True;
    aAction := aaDragTopLeft;
  End
  Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
  Begin
    Result := True;
    aAction := aaDragTopRight;
  End
  Else If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
  Begin
    Result := True;
    aAction := aaDragBottomLeft;
  End
  Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
  Begin
    Result := True;
    aAction := aaDragBottomRight;
  End
  Else If
    { along top}    ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Or
    { along bottom} ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Or
    { along left}   ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < l + 5) And (aPoint.x > l - 5)) Or
    { along right}  ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < r + 5) And (aPoint.x > r - 5)) Then
  Begin
    Result := True;
    aAction := aaMove;
  End;

  If Result Then
    aPart := apPrimary
  Else
  Begin
    l := oAdornment.Coordinates[2].X;
    t := oAdornment.Coordinates[2].y;
    r := oAdornment.Coordinates[3].X;
    b := oAdornment.Coordinates[3].Y;

    Result := False;
    If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
    Begin
      Result := True;
      aAction := aaDragTopLeft;
    End
    Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Then
    Begin
      Result := True;
      aAction := aaDragTopRight;
    End
    Else If ((aPoint.x > l-5) And (aPoint.x < l + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
    Begin
      Result := True;
      aAction := aaDragBottomLeft;
    End
    Else If ((aPoint.x > r-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Then
    Begin
      Result := True;
      aAction := aaDragBottomRight;
    End
    Else If
      { along top}    ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < t + 5) And (aPoint.y > t - 5)) Or
      { along bottom} ((aPoint.x > l-5) And (aPoint.x < r + 5) And (aPoint.y < b + 5) And (aPoint.y > b - 5)) Or
      { along left}   ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < l + 5) And (aPoint.x > l - 5)) Or
      { along right}  ((aPoint.y > t-5) And (aPoint.y < b + 5) And (aPoint.x < r + 5) And (aPoint.x > r - 5)) Then
    Begin
      Result := True;
      aAction := aaMove;
    End;

    If Result Then
      aPart := apZoom
    Else
    Begin
      Result := CaptionMatches(oAdornment, aPoint);
      If Result Then
        aPart := apCaption;
    End;
  End;
End;


Function TWPWorkingDocumentImagePiece.AdornmentMatchesMark(oAdornment : TWPDocumentImageAdornment; aPoint : TPoint; Var aPart : TAdornmentPart; Var aAction : TAdornmentAction) : Boolean;
Begin
  aAction := aaMove;
  Result := (Abs(oAdornment.Coordinates[0].X - aPoint.x) < 5) And (Abs(oAdornment.Coordinates[0].Y - aPoint.y) < 5);
  If Result Then
    aPart := apPrimary
  Else
  Begin
    Result := CaptionMatches(oAdornment, aPoint);
    If Result Then
      aPart := apCaption;
  End;

End;


Function TWPWorkingDocumentImagePiece.CaptionMatches(oAdornment: TWPDocumentImageAdornment; aPoint : TPoint): Boolean;
Begin
  If oAdornment.Caption = '' Then
    Result := False
  Else
    Result := RectHit(GetExtant(oAdornment, apCaption, aaMove), aPoint);
End;

Function TWPWorkingDocumentImagePiece.GetExtant(oAdornment: TWPDocumentImageAdornment; aPart: TAdornmentPart; aAction : TAdornmentAction): TRect;
Var
  i, w, hp : Integer;
  oCanvas : TCanvas;
Begin
  If aPart = apCaption Then
  Begin
    oCanvas := TFslBitmapGraphic(FWorkingImage).Handle.Canvas;
    oAdornment.Font.Apply(oCanvas.Font, False);
    w := oCanvas.Textwidth(oAdornment.Caption);
    If (oAdornment.AdornmentType = iatMark) Then
    Begin
      If oAdornment.Coordinates[0].X < oAdornment.CaptionPoint.X Then
      Begin
        // to the right
        If (oAdornment.CaptionPoint.X + 5 + w > FImage.Width) And (oAdornment.CaptionPoint.X - (w + 5) >= 0) Then
          hp := oAdornment.CaptionPoint.X - (w + 5)
        Else
          hp := oAdornment.CaptionPoint.X + 5;
      End
      Else
      Begin
        // to the left
        If (oAdornment.CaptionPoint.X - (5 + w) < 0) And (oAdornment.CaptionPoint.X + (w + 5) <= FImage.Width) Then
          hp := oAdornment.CaptionPoint.X + 5
        Else
          hp := oAdornment.CaptionPoint.X - (w + 5);
      End;
      Result.Top := oAdornment.CaptionPoint.Y - oCanvas.TextHeight(oAdornment.Caption) Div 2;
      Result.Left := hp;
      Result.Bottom := oAdornment.CaptionPoint.Y + oCanvas.TextHeight(oAdornment.Caption) Div 2;
      Result.Right := hp + w;
    End
    Else
    Begin
      Result.Top := oAdornment.CaptionPoint.Y;
      Result.Left := oAdornment.CaptionPoint.X;
      Result.Right := Result.Left + w;
      Result.Bottom := Result.Top + oCanvas.TextHeight(oAdornment.Caption);
    End;
  End
  Else If aPart = apZoom Then
  Begin
    Assert(oAdornment.AdornmentType = iatZoom);
    Result.Left := oAdornment.Coordinates[2].X;
    Result.Top := oAdornment.Coordinates[2].Y;
    Result.Right := oAdornment.Coordinates[3].X;
    Result.Bottom := oAdornment.Coordinates[3].Y;
  End
  Else If aPart In [apAll, apPrimary] Then
  Begin
    Case oAdornment.AdornmentType Of
      iatLine:
        Begin
        Result.Top := MAXINT;
        Result.Left := MaxInt;
        Result.Bottom := 0;
        Result.Right := 0;
        For i := 0 To oAdornment.Coordinates.Count - 1 Do
        Begin
          Result.Left := IntegerMin(Result.Left, oAdornment.Coordinates[i].X);
          Result.Right := IntegerMax(Result.Right, oAdornment.Coordinates[i].X);
          Result.Top := IntegerMin(Result.Top, oAdornment.Coordinates[i].Y);
          Result.Bottom := IntegerMax(Result.Bottom, oAdornment.Coordinates[i].Y);
        End;
        End;
      iatMark :
        Begin
          Result.Left := oAdornment.Coordinates[0].X - MARK_OUT;
          Result.Right := oAdornment.Coordinates[0].X + MARK_OUT;
          Result.Top := oAdornment.Coordinates[0].Y - MARK_OUT;
          Result.Bottom := oAdornment.Coordinates[0].Y + MARK_OUT;
        End;
      iatRectangle, iatCircle, iatZoom :
        Begin
        Result.Left := oAdornment.Coordinates[0].X;
        Result.Top := oAdornment.Coordinates[0].Y;
        Result.Right := oAdornment.Coordinates[1].X;
        Result.Bottom := oAdornment.Coordinates[1].Y;
        End;
    End;
    If aPart = apAll Then
    Begin
      If (aAction = aaMove) And (oAdornment.AdornmentType = iatZoom) Then
        Result := RectUnion(Result, GetExtant(oAdornment, apZoom, aaMove));
      If (aAction = aaMove) And (oAdornment.Caption <> '') Then
        Result := RectUnion(Result, GetExtant(oAdornment, apCaption, aaMove));
    End;
  End;
End;

Function ColourToGrayScale(aColour : TColor) : Word;
Begin
  If aColour = -1 Then
   Result := 0
  Else
    Result := (GetRValue(aColour) + GetGValue(aColour) + GetBValue(aColour));
End;

Function TWPWorkingDocumentImagePiece.DefaultAdornmentColour: TColour;
Var
  oBmp : TBitmap;
  iR, iC : Integer;
  aCol : Int64;
Begin
  If FDefaultAdornmentColour = clBlack Then
  Begin
    If FImage Is TFslBitmapGraphic Then
    Begin
      oBmp := TFslBitmapGraphic(FImage).Handle;
    // scan The pixels, getting the average
      aCol := 0;
      if (oBmp.Height < 100) or (oBmp.Width < 100) then
        For iR := 0 To oBmp.Height Do
          For iC := 0 To oBmp.Width Do
            Inc(aCol, ColourToGrayScale(oBmp.Canvas.Pixels[iR, iC]))
      else
        For iR := 0 To oBmp.Height div 101 Do
          For iC := 0 To oBmp.Width div 101 Do
            Inc(aCol, ColourToGrayScale(oBmp.Canvas.Pixels[iR*100, iC*100]));

      If (aCol / (oBmp.Height * oBmp.Width * 3)) > 127 Then
        FDefaultAdornmentColour := HTML_COLOUR_VALUES[hcMediumblue]
      Else
        FDefaultAdornmentColour := HTML_COLOUR_VALUES[hcGold];
    End
    Else
      FDefaultAdornmentColour := HTML_COLOUR_VALUES[hcGreenyellow];
  End;
  Result := FDefaultAdornmentColour;
End;



function TWPWorkingDocumentImagePiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FImage.sizeInBytes);
  inc(result, FSelectionImage.sizeInBytes);
  inc(result, FWorkingImage.sizeInBytes);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FImageMap.sizeInBytes);
  inc(result, FAdornments.sizeInBytes);
end;

{ TWPWorkingDocumentFieldStartPiece }


Procedure TWPWorkingDocumentFieldStartPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  FNamespace := TWPWorkingDocumentFieldStartPiece(oSource).FNamespace;
  DefinitionProvider := TWPWorkingDocumentFieldStartPiece(oSource).FDefinitionProvider.Link;
  FName := TWPWorkingDocumentFieldStartPiece(oSource).FName;
  FDeletable := TWPWorkingDocumentFieldStartPiece(oSource).FDeletable;
  FData.Assign(TWPWorkingDocumentFieldStartPiece(oSource).FData);
  FFixedFormat := TWPWorkingDocumentFieldStartPiece(oSource).FFixedFormat;
  FReadOnly := TWPWorkingDocumentFieldStartPiece(oSource).FReadOnly;
  FInError := TWPWorkingDocumentFieldStartPiece(oSource).FInError;
  FWidth := TWPWorkingDocumentFieldStartPiece(oSource).FWidth;
  FCheckables.Assign(TWPWorkingDocumentFieldStartPiece(oSource).FCheckables);
  FCheckedIndex := TWPWorkingDocumentFieldStartPiece(oSource).FCheckedIndex;
  FDocField := TWPWorkingDocumentFieldStartPiece(oSource).FDocField.Clone;
End;


Function TWPWorkingDocumentFieldStartPiece.Clone: TWPWorkingDocumentFieldStartPiece;
Begin
  Result := TWPWorkingDocumentFieldStartPiece(Inherited Clone);
End;


Function TWPWorkingDocumentFieldStartPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;


Function TWPWorkingDocumentFieldStartPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptFieldStart;
End;


Function TWPWorkingDocumentFieldStartPiece.GetVisualText: String;
Begin
  If FCheckables.Count > 0 Then
    Result := StringMultiply('x', FCheckables.Count)
  Else
    Result := FIELD_START_CHAR;
End;


Function TWPWorkingDocumentFieldStartPiece.GetLogicalText: String;
Begin
  If FCheckables.Count > 0 Then
    Result := StringMultiply('x', FCheckables.Count)
  Else
    Result := FIELD_START_CHAR;
End;


Function TWPWorkingDocumentFieldStartPiece.Link: TWPWorkingDocumentFieldStartPiece;
Begin
  Result := TWPWorkingDocumentFieldStartPiece(Inherited Link);
End;


Procedure TWPWorkingDocumentFieldStartPiece.SetDeletable(Const Value: Boolean);
Begin
  FDeletable := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentFieldStartPiece.SetFixedFormat(Const Value: TWPDocumentFieldFixedFormat);
Begin
  FFixedFormat := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentFieldStartPiece.SetNamespace(Const Value: String);
Begin
  FNamespace := Value;
End;


Procedure TWPWorkingDocumentFieldStartPiece.SetName(Const Value: String);
Begin
  FName := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentFieldStartPiece.SetReadOnly(Const Value: TWPSTriState);
Begin
  FReadOnly := Value;
  Change(ctLayout, Self);
End;

Function TWPWorkingDocumentFieldStartPiece.GetNamePair: String;
Begin
  If FNamespace <> '' Then
    Result := FNamespace +'::'+FName
  Else
    Result := FName;
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetNamePair(Const Value: String);
Var
  i : Integer;
Begin
  i := StringFind(value, '::');
  If (i = 0) Then
    FName := Value
  Else
  Begin
    FNamespace := Copy(Value, 1, i-1);
    FName := Copy(Value, i+2, MAXINT);
  End;
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetDefinitionProvider(Const Value: TWPFieldDefinitionProvider);
Begin
  FDefinitionProvider.Free;
  FDefinitionProvider := Value;
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetDocField(Const Value: TWPDocumentField);
Begin
  FDocField.Free;
  FDocField := Value;
End;

Constructor TWPWorkingDocumentFieldStartPiece.Create;
Begin
  Inherited;
  FData := TWPDataItemMap.Create;
  FCheckables := TFslStringList.Create;
End;

function TWPWorkingDocumentFieldStartPiece.DescribeV: String;
begin
  result := inherited DescribeV;

  prop(result, 'deletable', FDeletable);
  prop(result, 'dixed-format', Ord(FFixedFormat), TWPDOCUMENTOBJECT_FIXED_FORMAT);
  prop(result, 'namespace', FNamespace);
  prop(result, 'name', FName);
  prop(result, 'readonly', Ord(FReadOnly), NAMES_WPSTRISTATE);
  prop(result, 'data', FData.AsText);
  prop(result, 'width', FWidth);
  prop(result, 'min-width', FMinWidthPixels);
  prop(result, 'checked-index', FCheckedIndex);
end;

Destructor TWPWorkingDocumentFieldStartPiece.Destroy;
Begin
  FCheckables.Free;
  FDocField.Free;
  FDefinitionProvider.Free;
  FData.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentFieldStartPiece.BuildDocField;
Var
  oTran : TWPDocumentTranslator;
Begin
  oTran := TWPDocumentTranslator.Create;
  Try
    DocField := oTran.CreateField(Self);
  Finally
    oTran.Free;
  End;
End;

Function TWPWorkingDocumentFieldStartPiece.HasDefinitionProvider: Boolean;
Begin
  Result := Assigned(FDefinitionProvider);
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetInError(Const Value: Boolean);
Begin
  FInError := Value;
  Change(ctPresentation, Self);
End;

Function TWPWorkingDocumentFieldStartPiece.GetRawDataAsText: String;
Begin
  Result := FData.AsText;
End;

Function TWPWorkingDocumentFieldStartPiece.HasData: Boolean;
Begin
  Result := FData.Count > 0;
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetRawDataAsText(Const Value: String);
Begin
  FData.AsText := Value;
End;

Function TWPWorkingDocumentFieldStartPiece.GetDataValue(Const sKey: String): String;
Begin
  Result := FData.GetValueByKey(sKey);
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetDataValue(Const sKey, sValue: String);
Begin
  FData.SetValueByKey(sKey, sValue);
End;

Procedure TWPWorkingDocumentFieldStartPiece.SetWidth(Const Value: Integer);
Begin
  FWidth := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentFieldStartPiece.BindToCheckables;
Begin
  Metrics.CharCount := IntegerMax(1, FCheckables.Count);
  Metrics.VoiceCharCount := Metrics.CharCount;
  Change(ctTextContents, Self);
End;

function TWPWorkingDocumentFieldStartPiece.TitleOrName: String;
begin
  if DocField.DataValue['Title'] <> '' then
    result := DocField.DataValue['Title']
  else
    result := Name;
end;

function TWPWorkingDocumentFieldStartPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FNamespace.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FDocField.sizeInBytes);
  inc(result, FDefinitionProvider.sizeInBytes);
  inc(result, FData.sizeInBytes);
  inc(result, FCheckables.sizeInBytes);
  inc(result, (FLastUpdateValue.length * sizeof(char)) + 12);
end;

{ TWPWorkingDocumentFieldStopPiece }

Function TWPWorkingDocumentFieldStopPiece.Clone: TWPWorkingDocumentFieldStopPiece;
Begin
  Result := TWPWorkingDocumentFieldStopPiece(Inherited Clone);
End;

Function TWPWorkingDocumentFieldStopPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

Destructor TWPWorkingDocumentFieldStopPiece.Destroy;
Begin
  FMatchingStart.Free;
  Inherited;
End;

Function TWPWorkingDocumentFieldStopPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptFieldStop;
End;

Function TWPWorkingDocumentFieldStopPiece.GetVisualText: String;
Begin
  Result := FIELD_END_CHAR;
End;

Function TWPWorkingDocumentFieldStopPiece.GetLogicalText: String;
Begin
  Result := FIELD_END_CHAR;
End;

Function TWPWorkingDocumentFieldStopPiece.Link: TWPWorkingDocumentFieldStopPiece;
Begin
  Result := TWPWorkingDocumentFieldStopPiece(Inherited Link);
End;

Procedure TWPWorkingDocumentFieldStopPiece.SetMatchingStart(Const Value: TWPWorkingDocumentFieldStartPiece);
Begin
  FMatchingStart.Free;
  FMatchingStart := Value;
End;

function TWPWorkingDocumentFieldStopPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMatchingStart.sizeInBytes);
end;

Constructor TWPWorkingDocumentStopPiece.Create;
Begin
  Inherited;

  FStopType := stNone;
End;

Constructor TWPWorkingDocumentStopPiece.Create(stopType : TWPWorkingDocumentStopType);
Begin
  Create;

  FStopType := stopType;
End;

Function TWPWorkingDocumentStopPiece.Link: TWPWorkingDocumentStopPiece;
Begin
  Result := TWPWorkingDocumentStopPiece(Inherited Link);
End;

Function TWPWorkingDocumentStopPiece.Clone: TWPWorkingDocumentStopPiece;
Begin
  Result := TWPWorkingDocumentStopPiece(Inherited Clone);
End;

Procedure TWPWorkingDocumentStopPiece.Assign(oSource : TFslObject);
Begin
  Inherited;

  FStopType := TWPWorkingDocumentStopPiece(oSource).FStopType;
End;

Procedure TWPWorkingDocumentStopPiece.SetStopType(Const newType : TWPWorkingDocumentStopType);
Begin
  If FStopType = stNone Then
    FStopType := newType
  Else
    RaiseError('SetStopType', 'Cannot reassign StopType once assigned.');
End;

Function TWPWorkingDocumentStopPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

Function TWPWorkingDocumentStopPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Case FStopType Of
    stTableCell:Result := ptCellStop;
    stTableRow: Result := ptRowStop;
    stTable:    Result := ptTableStop;
    stSection:  Result := ptSectionStop;
    // stField:    Result := ptFieldStop; // won't replace field stop piece for now
  Else
    Begin
      Result := ptText; // to suppress the hint, as we will raise an error
      RaiseError('GetPieceType', 'PieceType is not yet defined.');
    End;
  End;
End;


Procedure TWPWorkingDocumentStopPiece.CollectAttributes(oXml: TFslXmlFormatter);
Begin
  Inherited;
  oXml.Attributes.add('type', NAMES_WorkingDocumentStopType[FStopType]);
End;

Function TWPWorkingDocumentStopPiece.XmlName: String;
Begin
  Result := 'stop';
End;

Destructor TWPWorkingDocumentStopPiece.Destroy;
Begin
  FMatchingStart.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentStopPiece.SetMatchingStart(Const Value: TWPWorkingDocumentPiece);
Begin
  FMatchingStart.Free;
  FMatchingStart := Value;
End;

function TWPWorkingDocumentStopPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMatchingStart.sizeInBytes);
end;

{ TWPWorkingDocumentContainerPiece }

Procedure TWPWorkingDocumentContainerPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  Container := Nil;
  FReadOnly := TWPWorkingDocumentContainerPiece(oSource).FReadOnly;
End;

function TWPWorkingDocumentContainerPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'readonly', Ord(FReadOnly), NAMES_WPSTRISTATE);
end;

Destructor TWPWorkingDocumentContainerPiece.Destroy;
Begin
  FContainer.Free;
  Inherited;
End;

Function TWPWorkingDocumentContainerPiece.HasContainer : Boolean;
Begin
  Result := Assigned(FContainer); 
End;


Procedure TWPWorkingDocumentContainerPiece.SetContainer(Const Value: TWPMapContainer);
Begin
  FContainer.Free;
  FContainer := Value;
End;


Procedure TWPWorkingDocumentContainerPiece.SetReadOnly(Const Value: TWPSTriState);
Begin
  FReadOnly := Value;
  Change(ctLayout, Self);
End;


Function TWPWorkingDocumentContainerPiece.GetContainer : TWPMapContainer;
Begin
  Assert(Invariants('GetContainer', FContainer, TWPMapContainer, 'Container'));
  Result := FContainer;
End;


Function TWPWorkingDocumentContainerPiece.IsLineBreak : Boolean;
Begin
  Result := True;
End;


function TWPWorkingDocumentContainerPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContainer.sizeInBytes);
end;

Function TWPWorkingDocumentContainerPieces.GetElements(iIndex : Integer):TWPWorkingDocumentContainerPiece;
Begin
  Result := TWPWorkingDocumentContainerPiece(ObjectByIndex[iIndex]);
End;


Function TWPWorkingDocumentContainerPieces.ItemClass : TFslObjectClass;
Begin
  Result := TWPWorkingDocumentContainerPiece;
End;


Procedure TWPWorkingDocumentContainerPiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Begin
  Inherited;
  If HasContainer Then
    Container.WantPainting;
End;

{ TWPWorkingDocumentParaPiece }

Procedure TWPWorkingDocumentParaPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  Format := TWPWorkingDocumentParaPiece(oSource).FFormat.Clone;
  FListNumber := TWPWorkingDocumentParaPiece(oSource).FListNumber;
  FWorkingRightIndent := 0;
  FWorkingLeftIndent := 0;
  FAdornmentWidth := 0;
End;

Function TWPWorkingDocumentParaPiece.Clone: TWPWorkingDocumentParaPiece;
Begin
  Result := TWPWorkingDocumentParaPiece(Inherited Clone);
End;

Constructor TWPWorkingDocumentParaPiece.Create;
Begin
  Inherited;
  FFormat := TWPSParagraphDetails.Create;
  Metrics.VoiceCharCount := Length(GetVoiceText);
End;

Function TWPWorkingDocumentParaPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;


function TWPWorkingDocumentParaPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'readonly', Ord(FReadOnly), NAMES_WPSTRISTATE);

  if (FFormat <> nil) then
    prop(result, 'format', FFormat.Describe);
  prop(result, 'number', FListNumber);
  prop(result, 'speechmagic', FSpeechMagicDouble);
  prop(result, 'adornment-width', FAdornmentWidth);
end;

Destructor TWPWorkingDocumentParaPiece.Destroy;
Begin
  FFormat.Free;
  Inherited;
End;

Function TWPWorkingDocumentParaPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptPara;
End;

Function TWPWorkingDocumentParaPiece.GetVisualText: String;
Begin
  Result := CHAR_PILCROW;
End;

Function TWPWorkingDocumentParaPiece.Link: TWPWorkingDocumentParaPiece;
Begin
  Result := TWPWorkingDocumentParaPiece(Inherited Link);
End;

Procedure TWPWorkingDocumentParaPiece.SetFormat(Const Value: TWPSParagraphDetails);
Begin
  FFormat.Free;
  FFormat := Value;
End;

Procedure TWPWorkingDocumentParaPiece.SetListNumber(Const Value: Integer);
Begin
  If FListNumber <> Value Then
    Begin
    FListNumber := Value;
    Change(ctPresentation, Self);
    End;
End;


Function TWPWorkingDocumentParaPiece.GetVoiceText : String;
Begin
  Result := #13#10;
End;


Function TWPWorkingDocumentParaPiece.GetFormat : TWPSParagraphDetails;
Begin
  Assert(Invariants('GetFormat', FFormat, TWPSParagraphDetails, 'Format'));
  Result := FFormat;
End;


Function TWPWorkingDocumentParaPiece.CanCopyAsText : Boolean;
Begin
  Result := True;
End;



function TWPWorkingDocumentParaPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFormat.sizeInBytes);
end;

{ TWPWorkingDocumentBreakPiece }

Function TWPWorkingDocumentBreakPiece.Clone: TWPWorkingDocumentBreakPiece;
Begin
  Result := TWPWorkingDocumentBreakPiece(Inherited Clone);
End;

Function TWPWorkingDocumentBreakPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

function TWPWorkingDocumentBreakPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'break-type', ord(FBreakType), WPWorkingDocumentBreakPIECETYPE_NAMES);
  prop(result, 'alignment', Ord(FAlignment), NAMES_WPSALIGNMENT);
  prop(result, 'width', trunc(FWidth * 100));
  prop(result, 'pen-colour', FPenColour);
  prop(result, 'pen-width', FPenWidth);
  prop(result, 'pen-style', Ord(FPenStyle), ADVPENSTYLE_CODES);
  prop(result, 'pen-end-style', Ord(FEndStyle), ADVPENENDSTYLE_CODES);
end;



Function TWPWorkingDocumentBreakPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptBreak;
End;

Function TWPWorkingDocumentBreakPiece.GetVisualText: String;
Begin
  Result := '_';
End;

Function TWPWorkingDocumentBreakPiece.Link: TWPWorkingDocumentBreakPiece;
Begin
  Result := TWPWorkingDocumentBreakPiece(Inherited Link);
End;


Procedure TWPWorkingDocumentBreakPiece.Assign(oSource : TFslObject);
Begin
  Inherited;
  FBreakType := TWPWorkingDocumentBreakPiece(oSource).FBreakType;
  FWidth := TWPWorkingDocumentBreakPiece(oSource).FWidth;
  FPenColour := TWPWorkingDocumentBreakPiece(oSource).FPenColour;
  FPenWidth := TWPWorkingDocumentBreakPiece(oSource).FPenWidth;
  FPenStyle := TWPWorkingDocumentBreakPiece(oSource).FPenStyle;
  FEndStyle := TWPWorkingDocumentBreakPiece(oSource).FEndStyle;
  FAlignment := TWPWorkingDocumentBreakPiece(oSource).FAlignment;
End;

Procedure TWPWorkingDocumentBreakPiece.ApplyProperties(oSource : TWPWorkingDocumentPiece);
Begin
  Inherited;
  FBreakType := TWPWorkingDocumentBreakPiece(oSource).FBreakType;
  FWidth := TWPWorkingDocumentBreakPiece(oSource).FWidth;
  FPenColour := TWPWorkingDocumentBreakPiece(oSource).FPenColour;
  FPenWidth := TWPWorkingDocumentBreakPiece(oSource).FPenWidth;
  FPenStyle := TWPWorkingDocumentBreakPiece(oSource).FPenStyle;
  FEndStyle := TWPWorkingDocumentBreakPiece(oSource).FEndStyle;
  FAlignment := TWPWorkingDocumentBreakPiece(oSource).FAlignment;
End;


Function TWPWorkingDocumentBreakPiece.IsLineBreak : Boolean;
Begin
  Result := True;
End;


Procedure TWPWorkingDocumentBreakPiece.SetBreakType(Const Value : TWPWorkingDocumentBreakPieceType);
Begin
  FBreakType := Value;
  Change(ctLayout, Self);
End;


Procedure TWPWorkingDocumentBreakPiece.SetWidth(Const Value : Real);
Begin
  FWidth := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentBreakPiece.SetPenColour(Const Value : TColour);
Begin
  FPenColour := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentBreakPiece.SetPenWidth(Const Value : Integer);
Begin
  FPenWidth := Value;
  Change(ctLayout, Self);
End;


Procedure TWPWorkingDocumentBreakPiece.SetPenStyle(Const Value : TFslPenStyle);
Begin
  FPenStyle := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPWorkingDocumentBreakPiece.SetEndStyle(Const Value : TFslPenEndStyle);
Begin
  FEndStyle := Value;
  Change(ctPresentation, Self);
End;

{ TWPWorkingDocumentSectionStartPiece }

Constructor TWPWorkingDocumentSectionStartPiece.Create;
Begin
  Inherited;
  FData := TWPDataItemMap.Create;
End;


function TWPWorkingDocumentSectionStartPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  Prop(result, 'display-name', FDisplayName);
  Prop(result, 'namespace', FNamespace);
  Prop(result, 'name', FName);
  Prop(result, 'display-type', Ord(FDisplayType), NAMES_WPWorkingDocumentSectionDISPLAYTYPE);
  Prop(result, 'deleteable', FDeletable);
  Prop(result, 'isfield', FIsField);
  Prop(result, 'key', FKey);
  Prop(result, 'data', FData.AsText);

end;

Destructor TWPWorkingDocumentSectionStartPiece.Destroy;
Begin
  FDocSection.Free;
  FDefinitionProvider.Free;
  FData.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentSectionStartPiece.Assign(oSource: TFslObject);
Begin
  Inherited;
  FDisplayName := TWPWorkingDocumentSectionStartPiece(oSource).FDisplayName;
  FNamespace := TWPWorkingDocumentSectionStartPiece(oSource).FNamespace;
  FName := TWPWorkingDocumentSectionStartPiece(oSource).FName;
  FData.Assign(TWPWorkingDocumentSectionStartPiece(oSource).FData);
  FDisplayType := TWPWorkingDocumentSectionStartPiece(oSource).FDisplayType;
  FDeletable := TWPWorkingDocumentSectionStartPiece(oSource).FDeletable;
  FIsField := TWPWorkingDocumentSectionStartPiece(oSource).FIsField;
  FKey := TWPWorkingDocumentSectionStartPiece(oSource).FKey;
  DefinitionProvider := TWPWorkingDocumentSectionStartPiece(oSource).FDefinitionProvider.Link;
End;

Function TWPWorkingDocumentSectionStartPiece.Clone: TWPWorkingDocumentSectionStartPiece;
Begin
  Result := TWPWorkingDocumentSectionStartPiece(Inherited Clone);
End;

Function TWPWorkingDocumentSectionStartPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

Function TWPWorkingDocumentSectionStartPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptSectionStart;
End;

Function TWPWorkingDocumentSectionStartPiece.Link: TWPWorkingDocumentSectionStartPiece;
Begin
  Result := TWPWorkingDocumentSectionStartPiece(Inherited Link);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetIsField(Const Value: Boolean);
Begin
  FIsField := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDeletable(Const Value: Boolean);
Begin
  FDeletable := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetKey(Const Value: String);
Begin
  FKey := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDisplayName(Const Value: String);
Begin
  FDisplayName := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetName(Const Value: String);
Begin
  FName := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDisplayType(Const Value: TWPWorkingDocumentSectionDisplayType);
Begin
  FDisplayType := Value;
  Change(ctPresentation, Self);
End;

Procedure TWPWorkingDocumentSectionStartPiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Begin
  Inherited;
  DisplayName := TWPWorkingDocumentSectionStartPiece(oSource).DisplayName;
  Namespace := TWPWorkingDocumentSectionStartPiece(oSource).Namespace;
  Name := TWPWorkingDocumentSectionStartPiece(oSource).Name;
  FData.Assign(TWPWorkingDocumentSectionStartPiece(oSource).FData);
  DisplayType := TWPWorkingDocumentSectionStartPiece(oSource).DisplayType;
  Deletable := TWPWorkingDocumentSectionStartPiece(oSource).Deletable;
  IsField := TWPWorkingDocumentSectionStartPiece(oSource).IsField;
  Key := TWPWorkingDocumentSectionStartPiece(oSource).Key;
End;

Function TWPWorkingDocumentSectionStartPiece.GetNamePair: String;
Begin
  If FNamespace <> '' Then
    Result := FNamespace +'::'+FName
  Else
    Result := FName;
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetNamePair(Const Value: String);
Var
  i : Integer;
Begin
  i := StringFind(value, '::');
  If (i = 0) Then
    FName := Value
  Else
  Begin
    FNamespace := Copy(Value, 1, i-1);
    FName := Copy(Value, i+2, MAXINT);
  End;
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDefinitionProvider(Const Value: TWPFieldDefinitionProvider);
Begin
  FDefinitionProvider.Free;
  FDefinitionProvider := Value;
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDocSection(Const Value: TWPDocumentSection);
Begin
  FDocSection.Free;
  FDocSection := Value;
End;

Procedure TWPWorkingDocumentSectionStartPiece.BuildDocSection;
Var
  oTran : TWPDocumentTranslator;
Begin
  oTran := TWPDocumentTranslator.Create;
  Try
    DocSection := oTran.CreateSection(Self);
  Finally
    oTran.Free;
  End;
End;

Function TWPWorkingDocumentSectionStartPiece.HasDefinitionProvider: Boolean;
Begin
  Result := Assigned(FDefinitionProvider);
End;

Function TWPWorkingDocumentSectionStartPiece.HasData: Boolean;
Begin
  Result := FData.Count > 0;
End;

Function TWPWorkingDocumentSectionStartPiece.GetRawDataAsText: String;
Begin
  Result := FData.AsText;
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetRawDataAsText(Const Value: String);
Begin
  FData.AsText := Value;
End;

Function TWPWorkingDocumentSectionStartPiece.GetDataValue(Const sKey: String): String;
Begin
  Result := FData.GetValueByKey(sKey);
End;

Procedure TWPWorkingDocumentSectionStartPiece.SetDataValue(Const sKey, sValue: String);
Begin
  FData.SetValueByKey(sKey, sValue);
End;

function TWPWorkingDocumentSectionStartPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDisplayName.length * sizeof(char)) + 12);
  inc(result, (FNamespace.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, FDocSection.sizeInBytes);
  inc(result, FDefinitionProvider.sizeInBytes);
  inc(result, FData.sizeInBytes);
end;

{ TWPWorkingDocumentTableItemPiece }

Constructor TWPWorkingDocumentTableItemPiece.Create;
Begin
  Inherited;
  LeftBorder := TWPBorder.Create;
  RightBorder := TWPBorder.Create;
  TopBorder := TWPBorder.Create;
  BottomBorder := TWPBorder.Create;
End;

Destructor TWPWorkingDocumentTableItemPiece.Destroy;
Begin
  FLeftBorder.Unhook(Change);
  FLeftBorder.Free;
  FRightBorder.UnHook(Change);
  FRightBorder.Free;
  FTopBorder.UnHook(Change);
  FTopBorder.Free;
  FBottomBorder.UnHook(Change);
  FBottomBorder.Free;
  FWorkingLeftBorder.Free;
  FWorkingRightBorder.Free;
  FWorkingTopBorder.Free;
  FWorkingBottomBorder.Free;
  Inherited;
End;


Procedure TWPWorkingDocumentTableItemPiece.Assign(oSource : TFslObject);
Begin
  Inherited;

  FLeftBorder.Assign(TWPWorkingDocumentTableItemPiece(oSource).FLeftBorder);
  FRightBorder.Assign(TWPWorkingDocumentTableItemPiece(oSource).FRightBorder);
  FTopBorder.Assign(TWPWorkingDocumentTableItemPiece(oSource).FTopBorder);
  FBottomBorder.Assign(TWPWorkingDocumentTableItemPiece(oSource).FBottomBorder);
End;


Function TWPWorkingDocumentTableItemPiece.Clone: TWPWorkingDocumentTableItemPiece;
Begin
  Result := TWPWorkingDocumentTableItemPiece(Inherited Clone);
End;


Function TWPWorkingDocumentTableItemPiece.Link: TWPWorkingDocumentTableItemPiece;
Begin
  Result := TWPWorkingDocumentTableItemPiece(Inherited Link);
End;


Procedure TWPWorkingDocumentTableItemPiece.SetLeftBorder(Const Value : TWPBorder);
Begin
  FLeftBorder.UnHook(Change);
  FLeftBorder.Free;
  FLeftBorder := Value;
  FLeftBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableItemPiece.SetRightBorder(Const Value : TWPBorder);
Begin
  FRightBorder.UnHook(Change);
  FRightBorder.Free;
  FRightBorder := Value;
  FRightBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableItemPiece.SetTopBorder(Const Value : TWPBorder);
Begin
  FTopBorder.UnHook(Change);
  FTopBorder.Free;
  FTopBorder := Value;
  FTopBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableItemPiece.SetBottomBorder(Const Value : TWPBorder);
Begin
  FBottomBorder.UnHook(Change);
  FBottomBorder.Free;
  FBottomBorder := Value;
  FBottomBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableItemPiece.SetWorkingLeftBorder(Const Value : TWPBorder);
Begin
  FWorkingLeftBorder.Free;
  FWorkingLeftBorder := Value;
End;


Procedure TWPWorkingDocumentTableItemPiece.SetWorkingRightBorder(Const Value : TWPBorder);
Begin
  FWorkingRightBorder.Free;
  FWorkingRightBorder := Value;
End;


Procedure TWPWorkingDocumentTableItemPiece.SetWorkingTopBorder(Const Value : TWPBorder);
Begin
  FWorkingTopBorder.Free;
  FWorkingTopBorder := Value;
End;


Procedure TWPWorkingDocumentTableItemPiece.SetWorkingBottomBorder(Const Value : TWPBorder);
Begin
  FWorkingBottomBorder.Free;
  FWorkingBottomBorder := Value;
End;

Function TWPWorkingDocumentTableItemPiece.HasWorkingLeftBorder : Boolean;
Begin
  Result := Assigned(FWorkingLeftBorder);
End;


Function TWPWorkingDocumentTableItemPiece.HasWorkingRightBorder : Boolean;
Begin
  Result := Assigned(FWorkingRightBorder);
End;


Function TWPWorkingDocumentTableItemPiece.HasWorkingTopBorder : Boolean;
Begin
  Result := Assigned(FWorkingTopBorder);
End;


Function TWPWorkingDocumentTableItemPiece.HasWorkingBottomBorder : Boolean;
Begin
  Result := Assigned(FWorkingBottomBorder);
End;


Procedure TWPWorkingDocumentTableItemPiece.ChooseWorkingLeftBorder(oTable : TWPBorder);
Begin
  If LeftBorder.Defined Then
    oTable := LeftBorder;
  If Not HasWorkingLeftBorder Or (WorkingLeftBorder <> oTable) Then
    WorkingLeftBorder := oTable.Link;
End;


Procedure TWPWorkingDocumentTableItemPiece.ChooseWorkingRightBorder(oTable : TWPBorder);
Begin
  If RightBorder.Defined Then
    oTable := RightBorder;
  If Not HasWorkingRightBorder Or (WorkingRightBorder <> oTable) Then
    WorkingRightBorder := oTable.Link;
End;


Procedure TWPWorkingDocumentTableItemPiece.ChooseWorkingTopBorder(oTable : TWPBorder);
Begin
  If TopBorder.Defined Then
    oTable := TopBorder;
  If Not HasWorkingTopBorder Or (WorkingTopBorder <> oTable) Then
    WorkingTopBorder := oTable.Link;
End;


Procedure TWPWorkingDocumentTableItemPiece.ChooseWorkingBottomBorder(oTable : TWPBorder);
Begin
  If BottomBorder.Defined Then
    oTable := BottomBorder;
  If Not HasWorkingBottomBorder Or (WorkingBottomBorder <> oTable) Then
    WorkingBottomBorder := oTable.Link;
End;

Function TWPWorkingDocumentTableItemPiece.GetLeftBorder : TWPBorder;
Begin
  Assert(Invariants('GetLeftBorder', FLeftBorder, TWPBorder, 'LeftBorder'));
  Result := FLeftBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetRightBorder : TWPBorder;
Begin
  Assert(Invariants('GetRightBorder', FRightBorder, TWPBorder, 'RightBorder'));
  Result := FRightBorder;
End;

Function TWPWorkingDocumentTableItemPiece.GetTopBorder : TWPBorder;
Begin
  Assert(Invariants('GetTopBorder', FTopBorder, TWPBorder, 'TopBorder'));
  Result := FTopBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetBottomBorder : TWPBorder;
Begin
  Assert(Invariants('GetBottomBorder', FBottomBorder, TWPBorder, 'BottomBorder'));
  Result := FBottomBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetWorkingLeftBorder : TWPBorder;
Begin
  Assert(Invariants('GetWorkingLeftBorder', FWorkingLeftBorder, TWPBorder, 'WorkingLeftBorder'));
  Result := FWorkingLeftBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetWorkingRightBorder : TWPBorder;
Begin
  Assert(Invariants('GetWorkingRightBorder', FWorkingRightBorder, TWPBorder, 'WorkingRightBorder'));
  Result := FWorkingRightBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetWorkingTopBorder : TWPBorder;
Begin
  Assert(Invariants('GetWorkingTopBorder', FWorkingTopBorder, TWPBorder, 'WorkingTopBorder'));
  Result := FWorkingTopBorder;
End;


Function TWPWorkingDocumentTableItemPiece.GetWorkingBottomBorder : TWPBorder;
Begin
  Assert(Invariants('GetWorkingBottomBorder', FWorkingBottomBorder, TWPBorder, 'WorkingBottomBorder'));
  Result := FWorkingBottomBorder;
End;


Procedure TWPWorkingDocumentTableItemPiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Var
  oItem: TWPWorkingDocumentTableItemPiece;
Begin
  Inherited;
  oItem := TWPWorkingDocumentTableItemPiece(oSource);
  
  If (oItem <> Nil) Then
  Begin
    LeftBorder.Assign(oItem.LeftBorder);
    RightBorder.Assign(oItem.RightBorder);
    TopBorder.Assign(oItem.TopBorder);
    BottomBorder.Assign(oItem.BottomBorder);
  End;
End;

function TWPWorkingDocumentTableItemPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLeftBorder.sizeInBytes);
  inc(result, FRightBorder.sizeInBytes);
  inc(result, FTopBorder.sizeInBytes);
  inc(result, FBottomBorder.sizeInBytes);
  inc(result, FWorkingLeftBorder.sizeInBytes);
  inc(result, FWorkingRightBorder.sizeInBytes);
  inc(result, FWorkingTopBorder.sizeInBytes);
  inc(result, FWorkingBottomBorder.sizeInBytes);
end;

{ TWPWorkingDocumentTableCellStartPiece }


Constructor TWPWorkingDocumentTableCellStartPiece.Create;
Begin
  Inherited;
  Span := 1;
  FBackground := DEF_COLOUR;
  FMarginLeft := DEF_WORD;
  FMarginTop := DEF_WORD;
  FMarginRight := DEF_WORD;
  FMarginBottom := DEF_WORD;
  FVerticalAlignment := VerticalAlignmentTop;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.Assign(oSource : TFslObject);
Begin
  Inherited;
  FSpan := TWPWorkingDocumentTableCellStartPiece(oSource).FSpan;
  FWidth := TWPWorkingDocumentTableCellStartPiece(oSource).FWidth;
  FBackground := TWPWorkingDocumentTableCellStartPiece(oSource).FBackground;
  FMarginLeft := TWPWorkingDocumentTableCellStartPiece(oSource).FMarginLeft;
  FMarginTop := TWPWorkingDocumentTableCellStartPiece(oSource).FMarginTop;
  FMarginRight := TWPWorkingDocumentTableCellStartPiece(oSource).FMarginRight;
  FMarginBottom := TWPWorkingDocumentTableCellStartPiece(oSource).FMarginBottom;
  FVerticalAlignment := TWPWorkingDocumentTableCellStartPiece(oSource).FVerticalAlignment;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetSpan(Const Value : Integer);
Begin
  If FSpan <> Value Then
    Begin
    FSpan := Value;
    Change(ctTextContents, Self);
    End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetWidth(Const Value : Real);
Begin
  If FWidth <> Value Then
    Begin
    FWidth := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetState(Const Value : TWPWorkingDocumentTableItemState);
Begin
  If FState <> Value Then
    Begin
    FState := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetBackground(Const Value : TColour);
Begin
  If FBackground <> Value Then
    Begin
    FBackground := Value;
    Change(ctLayout, Self);
    End;
End;

Procedure TWPWorkingDocumentTableCellStartPiece.SetMarginLeft(Const Value : Word);
Begin
  If FMarginLeft <> Value Then
  Begin
    FMarginLeft := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetMarginTop(Const Value : Word);
Begin
  If FMarginTop <> Value Then
  Begin
    FMarginTop := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetMarginRight(Const Value : Word);
Begin
  If FMarginRight <> Value Then
  Begin
    FMarginRight := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetMarginBottom(Const Value : Word);
Begin
  If FMarginBottom <> Value Then
  Begin
    FMarginBottom := Value;
    Change(ctLayout, Self);
  End;
End;


Function TWPWorkingDocumentTableCellStartPiece.WorkingMarginLeft : Word;
Begin
  If FMarginLeft = DEF_WORD Then
    Result := DEFAULT_CELL_MARGIN_LEFT
  Else
    Result := FMarginLeft;
End;


Function TWPWorkingDocumentTableCellStartPiece.WorkingMarginTop : Word;
Begin
  If FMarginTop = DEF_WORD Then
    Result := DEFAULT_CELL_MARGIN_TOP
  Else
    Result := FMarginTop;
End;


Function TWPWorkingDocumentTableCellStartPiece.WorkingMarginRight : Word;
Begin
  If FMarginRight = DEF_WORD Then
    Result := DEFAULT_CELL_MARGIN_RIGHT
  Else
    Result := FMarginRight;
End;


Function TWPWorkingDocumentTableCellStartPiece.WorkingMarginBottom : Word;
Begin
  If FMarginBottom = DEF_WORD Then
    Result := DEFAULT_CELL_MARGIN_BOTTOM
  Else
    Result := FMarginBottom;
End;


Function TWPWorkingDocumentTableCellStartPiece.Clone: TWPWorkingDocumentTableCellStartPiece;
Begin
  Result := TWPWorkingDocumentTableCellStartPiece(Inherited Clone);
End;

Function TWPWorkingDocumentTableCellStartPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;

function TWPWorkingDocumentTableCellStartPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'span', FSpan);
  prop(result, 'width', trunc(FWidth * 100));
  prop(result, 'background', FBackground);
  prop(result, 'left-margin', FMarginLeft);
  prop(result, 'top-margin', FMarginTop);
  prop(result, 'right-margin', FMarginRight);
  prop(result, 'bottom-margin', FMarginBottom);
  prop(result, 'vert-align', Ord(FVerticalAlignment), NAMES_WORDPROCESSORVERTICALALIGNMENT);

end;

Function TWPWorkingDocumentTableCellStartPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptCellStart;
End;

Function TWPWorkingDocumentTableCellStartPiece.Link: TWPWorkingDocumentTableCellStartPiece;
Begin
  Result := TWPWorkingDocumentTableCellStartPiece(Inherited Link);
End;


Procedure TWPWorkingDocumentTableCellStartPiece.MeasurePieceContent(oPiece : TWPWorkingDocumentPiece; iExtra : Integer = 0);
Begin
  Inc(FWidthCurrent, oPiece.Metrics.Width+ iExtra);
  FWidthMinimum := IntegerMax(FWidthMinimum, oPiece.Metrics.Width+ iExtra);
  If oPiece.IsLineBreak Then
    FinishContentMeasure;
End;

Procedure TWPWorkingDocumentTableCellStartPiece.SeeParagraph(oPara : TWPWorkingDocumentParaPiece);
Begin
  FMaxLeftParaSpace := IntegerMax(oPara.WorkingLeftIndent, FMaxLeftParaSpace);
  FMaxRightParaSpace := IntegerMax(oPara.WorkingRightIndent, FMaxRightParaSpace);
End;


Procedure TWPWorkingDocumentTableCellStartPiece.StartContentMeasure;
Begin
  FWidthMaximum := 0;
  FWidthMinimum := 0;
  FWidthCurrent := 0;
  FMaxLeftParaSpace := 0;
  FMaxRightParaSpace := 0;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.FinishContentMeasure;
Begin
  FWidthMaximum := IntegerMax(FWidthMaximum, FWidthCurrent);
  FWidthCurrent := 0;
End;



Function TWPWorkingDocumentTableCellStartPiece.WidthSpecified(iTotalWidth : Integer) : Integer;
Begin
  Result := Trunc(iTotalWidth * FWidth);
End;


Function TWPWorkingDocumentTableCellStartPiece.GetRow : TObject;
Begin
  Assert(Invariants('GetRow', FRow, TObject, 'Row'));
  Result := FRow;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.SetRow(Const Value : TObject);
Begin
  Assert(Invariants('SetRow', Value, TWPWorkingDocumentTableRowStartPiece, 'Value'));
  FRow := Value;
End;





Procedure TWPWorkingDocumentTableCellStartPiece.SetVerticalAlignment(Const Value: TWordProcessorVerticalAlignment);
Begin
  If FVerticalAlignment <> Value Then
  Begin
    FVerticalAlignment := Value;
    Change(ctLayout, Self);
  End;
End;

Procedure TWPWorkingDocumentTableCellStartPiece.ApplyProperties(oSource: TWPWorkingDocumentPiece);
Begin
  Inherited;
  Width := TWPWorkingDocumentTableCellStartPiece(oSource).Width;
  Background := TWPWorkingDocumentTableCellStartPiece(oSource).Background;
  MarginLeft := TWPWorkingDocumentTableCellStartPiece(oSource).MarginLeft;
  MarginTop := TWPWorkingDocumentTableCellStartPiece(oSource).MarginTop;
  MarginRight := TWPWorkingDocumentTableCellStartPiece(oSource).MarginRight;
  MarginBottom := TWPWorkingDocumentTableCellStartPiece(oSource).MarginBottom;
  VerticalAlignment := TWPWorkingDocumentTableCellStartPiece(oSource).VerticalAlignment;
End;


Procedure TWPWorkingDocumentTableCellStartPiece.MergeProperties(oSource: TWPWorkingDocumentPiece);
Var
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  Inherited;
  If Assigned(oSource) And (oSource.PieceType = ptCellStart) Then
  Begin
    oCell := TWPWorkingDocumentTableCellStartPiece(oSource);
    If Width <> oCell.Width Then
      Width := 0;
    If Background <> oCell.Background Then
      Background := DEF_COLOUR;
    If MarginLeft <> oCell.MarginLeft Then
      MarginLeft := 0;
    If MarginTop <> oCell.MarginTop Then
      MarginTop := 0;
    If MarginRight <> oCell.MarginRight Then
      MarginRight := 0;
    If MarginBottom <> oCell.MarginBottom Then
      MarginBottom := 0;
    If VerticalAlignment <> oCell.VerticalAlignment Then
      VerticalAlignment := VerticalAlignmentTop;
  End;
End;

Function TWPWorkingDocumentTableCellStartPieces.GetCell(iIndex: Integer): TWPWorkingDocumentTableCellStartPiece;
Begin
  Result := TWPWorkingDocumentTableCellStartPiece(ObjectByIndex[iIndex]);
End;


Function TWPWorkingDocumentTableCellStartPieces.ItemClass : TFslObjectClass;
Begin
  Result := TWPWorkingDocumentTableCellStartPiece;
End;


Function TWPWorkingDocumentTableCellStartPiece.GetHasRow: Boolean;
Begin
  Result := FRow <> Nil;
End;

{ TWPWorkingDocumentTableRowStartPiece }

Constructor TWPWorkingDocumentTableRowStartPiece.Create;
Begin
  Inherited;
  FCells := TWPWorkingDocumentTableCellStartPieces.Create;
  FCells.Hooking := False;
  FBackground := DEF_COLOUR;
  FDepth := 0;

  FLowerPaddingSize := 0;
  FLowerPaddingColour := DEF_COLOUR;
End;


function TWPWorkingDocumentTableRowStartPiece.DescribeV: String;
begin
  result := inherited DescribeV;
  prop(result, 'header', FHeader);
  prop(result, 'break-before', FBreakBefore);
  prop(result, 'background', FBackground);
  prop(result, 'padding-size', FLowerPaddingSize);
  prop(result, 'padding-colour', FLowerPaddingColour);
end;

Destructor TWPWorkingDocumentTableRowStartPiece.Destroy;
Begin
  FCells.Free;
  Inherited;
End;


Procedure TWPWorkingDocumentTableRowStartPiece.Assign(oSource : TFslObject);
Begin
  Inherited;

  FHeader := TWPWorkingDocumentTableRowStartPiece(oSource).FHeader;
  FBreakBefore := TWPWorkingDocumentTableRowStartPiece(oSource).FBreakBefore;
  FLowerPaddingSize := TWPWorkingDocumentTableRowStartPiece(oSource).FLowerPaddingSize;
  FLowerPaddingColour := TWPWorkingDocumentTableRowStartPiece(oSource).FLowerPaddingColour;
  FBackground := TWPWorkingDocumentTableRowStartPiece(oSource).FBackground;
  FState := TWPWorkingDocumentTableRowStartPiece(oSource).State;
  FCells.Clear;
End;



Procedure TWPWorkingDocumentTableRowStartPiece.SetState(Const Value : TWPWorkingDocumentTableItemState);
Begin
  If FState <> Value Then
    Begin
    FState := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPWorkingDocumentTableRowStartPiece.SetBackground(Const Value : TColour);
Begin
  If FBackground <> Value Then
    Begin
    FBackground := Value;
    Change(ctLayout, Self);
    End;
End;

Function TWPWorkingDocumentTableRowStartPiece.Clone: TWPWorkingDocumentTableRowStartPiece;
Begin
  Result := TWPWorkingDocumentTableRowStartPiece(Inherited Clone);
End;


Function TWPWorkingDocumentTableRowStartPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;


Function TWPWorkingDocumentTableRowStartPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptRowStart;
End;


Function TWPWorkingDocumentTableRowStartPiece.Link: TWPWorkingDocumentTableRowStartPiece;
Begin
  Result := TWPWorkingDocumentTableRowStartPiece(Inherited Link);
End;


Procedure TWPWorkingDocumentTableRowStartPiece.ApplyProperties(oSource : TWPWorkingDocumentPiece);
Begin
  Inherited;
  Header := TWPWorkingDocumentTableRowStartPiece(oSource).Header;
  BreakBefore := TWPWorkingDocumentTableRowStartPiece(oSource).BreakBefore;
  Background := TWPWorkingDocumentTableRowStartPiece(oSource).Background;
  LowerPaddingSize := TWPWorkingDocumentTableRowStartPiece(oSource).LowerPaddingSize;
  LowerPaddingColour := TWPWorkingDocumentTableRowStartPiece(oSource).LowerPaddingColour;
End;


Procedure TWPWorkingDocumentTableRowStartPiece.MergeProperties(oSource: TWPWorkingDocumentPiece);
Var
  oRow: TWPWorkingDocumentTableRowStartPiece;
Begin
  Inherited;

  If Assigned(oSource) And (oSource.PieceType = ptRowStart) Then
  Begin
    oRow := TWPWorkingDocumentTableRowStartPiece(oSource);

    If Header <> oRow.Header Then Header := False;
    If BreakBefore <> oRow.BreakBefore Then BreakBefore := False;
    If Background <> oRow.Background Then Background := DEF_COLOUR;
    If LowerPaddingSize <> oRow.LowerPaddingSize Then LowerPaddingSize := DEF_WORD;
    If LowerPaddingColour <> oRow.LowerPaddingColour Then LowerPaddingColour := DEF_COLOUR;
  End;
End;


Function TWPWorkingDocumentTableRowStartPiece.ColumnCount : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Cells.Count - 1 Do
    Inc(Result, Cells[iLoop].Span);
End;


Function TWPWorkingDocumentTableRowStartPiece.AnyContentReadOnly : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  For iLoop := 0 To Cells.Count - 1 Do
    Result := Result Or Cells[iLoop].IsReadOnly;
End;


Function TWPWorkingDocumentTableRowStartPiece.CellIsReadOnly(iCol : Integer) : Boolean;
Begin
  Result := (Cells.Count > iCol) And Cells[iCol].IsReadOnly;
End;


Function TWPWorkingDocumentTableRowStartPiece.GetCells : TWPWorkingDocumentTableCellStartPieces;
Begin
  Assert(Invariants('GetCells', FCells, TWPWorkingDocumentTableCellStartPieces, 'Cells'));
  Result := FCells;
End;


Function TWPWorkingDocumentTableRowStartPiece.GetTable : TObject;
Begin
  Assert(Invariants('GetTable', FTable, TObject, 'Table'));
  Result := FTable;
End;


Procedure TWPWorkingDocumentTableRowStartPiece.SetTable(Const Value : TObject);
Begin
  Assert(Invariants('GetTable', Value, TWPWorkingDocumentTableStartPiece, 'Value'));
  FTable := Value;
End;




Procedure TWPWorkingDocumentTableRowStartPiece.StateFirst;
Begin
  If FState In [tisLast, tisOnly] Then
    FState := tisOnly
  Else
    FState := tisFirst;
End;

Procedure TWPWorkingDocumentTableRowStartPiece.StateLast;
Begin
  If FState In [tisFirst, tisOnly] Then
    FState := tisOnly
  Else
    FState := tisLast;
End;

Function TWPWorkingDocumentTableRowStartPiece.HasOwner: Boolean;
Begin
  Result := Assigned(FOwner);
End;


function TWPWorkingDocumentTableRowStartPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCells.sizeInBytes);
  inc(result, FOwner.sizeInBytes);
  inc(result, FTablePrev.sizeInBytes);
  inc(result, FTablePrevLevel.sizeInBytes);
  inc(result, FTableNext.sizeInBytes);
  inc(result, FTableNextLevel.sizeInBytes);
end;

Function TWPWorkingDocumentTableRowStartPieces.GetElements(iIndex : Integer):TWPWorkingDocumentTableRowStartPiece;
Begin
  Result := TWPWorkingDocumentTableRowStartPiece(ObjectByIndex[iIndex]);
End;


Function TWPWorkingDocumentTableRowStartPieces.ItemClass : TFslObjectClass;
Begin
  Result := TWPWorkingDocumentTableRowStartPiece;
End;

Procedure TWPWorkingDocumentTableRowStartPiece.SetLowerPaddingSize(Const Value: Integer);
Begin
  If FLowerPaddingSize <> Value Then
    Begin
    FLowerPaddingSize := Value;
    Change(ctLayout, Self);
    End;
End;

Procedure TWPWorkingDocumentTableRowStartPiece.SetLowerPaddingColour(Const Value: TColour);
Begin
  If FLowerPaddingColour <> Value Then
    Begin
    FLowerPaddingColour := Value;
    Change(ctLayout, Self);
    End;
End;

Function TWPWorkingDocumentTableRowStartPiece.GetHasTable: Boolean;
Begin
  Result := FTable <> Nil;
End;

{ TWPWorkingDocumentTableStartPiece }

Constructor TWPWorkingDocumentTableStartPiece.Create;
Begin
  Inherited;
  FRows := TWPWorkingDocumentTableRowStartPieces.Create;
  FRows.Hooking := False;

  CenterHorizontalBorder := TWPBorder.Create;
  CenterVerticalBorder := TWPBorder.Create;
  FBackground := DEF_COLOUR;
  FHorizontalMargin := DEF_WORD;
  FVerticalMargin := DEF_WORD;
  FStructureDirty := True;
  FContentDirty := True;
End;


function TWPWorkingDocumentTableStartPiece.DescribeV: String;
begin
  result := Inherited describeV;
  prop(result, 'border-policy', Ord(FBorderPolicy), CODES_TWPWorkingDocumentTableBORDERPOLICY);
  prop(result, 'horiz-margin', FHorizontalMargin);
  prop(result, 'vert-margin', FVerticalMargin);
end;


Destructor TWPWorkingDocumentTableStartPiece.Destroy;
Begin
  FCenterVerticalBorder.UnHook(Change);
  FCenterVerticalBorder.Free;
  FCenterHorizontalBorder.UnHook(Change);
  FCenterHorizontalBorder.Free;
  FRows.Free;
  Inherited;
End;


Procedure TWPWorkingDocumentTableStartPiece.Assign(oSource : TFslObject);
Begin
  Inherited;

  FBorderPolicy := TWPWorkingDocumentTableStartPiece(oSource).FBorderPolicy;
  FHorizontalMargin := TWPWorkingDocumentTableStartPiece(oSource).FHorizontalMargin;
  FVerticalMargin := TWPWorkingDocumentTableStartPiece(oSource).FVerticalMargin;

  FCenterHorizontalBorder.Assign(TWPWorkingDocumentTableStartPiece(oSource).FCenterHorizontalBorder);
  FCenterVerticalBorder.Assign(TWPWorkingDocumentTableStartPiece(oSource).FCenterVerticalBorder);
  FBackground := TWPWorkingDocumentTableStartPiece(oSource).FBackground;
  FExpandLastColumn := TWPWorkingDocumentTableStartPiece(oSource).FExpandLastColumn;

  FRows.Clear;
  FColumnCount := 0;
  FJagged := False;
End;


Function TWPWorkingDocumentTableStartPiece.Clone: TWPWorkingDocumentTableStartPiece;
Begin
  Result := TWPWorkingDocumentTableStartPiece(Inherited Clone);
End;


Function TWPWorkingDocumentTableStartPiece.DefaultCharCount: Integer;
Begin
  Result := 1;
End;


Function TWPWorkingDocumentTableStartPiece.GetPieceType: TWPWorkingDocumentPieceType;
Begin
  Result := ptTableStart;
End;


Function TWPWorkingDocumentTableStartPiece.Link: TWPWorkingDocumentTableStartPiece;
Begin
  Result := TWPWorkingDocumentTableStartPiece(Inherited Link);
End;


Function TWPWorkingDocumentTableStartPiece.ColumnIndexForCell(oCell : TWPWorkingDocumentTableCellStartPiece) : Integer;
Var
  iIndex : Integer;
Begin
  Result := -1;
  iIndex := 0;
  While (Result = -1) And (iIndex < Rows.Count) Do
  Begin
    Result := Rows[iIndex].Cells.IndexByReference(oCell);
    Inc(iIndex);
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.SetCenterHorizontalBorder(Const Value : TWPBorder);
Begin
  FCenterHorizontalBorder.UnHook(Change);
  FCenterHorizontalBorder.Free;
  FCenterHorizontalBorder := Value;
  FCenterHorizontalBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableStartPiece.SetCenterVerticalBorder(Const Value : TWPBorder);
Begin
  FCenterVerticalBorder.UnHook(Change);
  FCenterVerticalBorder.Free;
  FCenterVerticalBorder := Value;
  FCenterVerticalBorder.Hook(Change);
End;


Procedure TWPWorkingDocumentTableStartPiece.SetBorderPolicy(Const Value : TWPWorkingDocumentTableBorderPolicy);
Begin
  If Value <> FBorderPolicy Then
  Begin
    FBorderPolicy := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.SetHorizontalMargin(Const Value : Word);
Begin
  If Value <> FHorizontalMargin Then
  Begin
    FHorizontalMargin := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.SetVerticalMargin(Const Value : Word);
Begin
  If Value <> FVerticalMargin Then
  Begin
    FVerticalMargin := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.SetBackground(Const Value : TColour);
Begin
  If FBackground <> Value Then
    Begin
    FBackground := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyPolicy;
Begin
  Case BorderPolicy Of
    tbpNone : ApplyBorderPolicyNone;
    tbpGrid : ApplyBorderPolicyGrid;
    tbpLines : ApplyBorderPolicyLines;
    tbpInnerLines : ApplyBorderPolicyInnerLines;
    tbpDots : ApplyBorderPolicyDots;
    tbpInnerDots : ApplyBorderPolicyInnerDots;
    tbpBox : ApplyBorderPolicyBox;
    tbpBoxLines : ApplyBorderPolicyBoxLines;
    tbpFancy : ApplyBorderPolicyFancy;
  End;
  If FHorizontalMargin <> DEF_WORD Then
    ApplyHorizontalMarginWidth;
  If FVerticalMargin <> DEF_WORD Then
    ApplyVerticalMarginWidth;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyProperties(oSource : TWPWorkingDocumentPiece);
Var
  bPolicyChanged: Boolean;
  oTable: TWPWorkingDocumentTableStartPiece;
Begin
  Inherited;
  oTable := TWPWorkingDocumentTableStartPiece(oSource);

  If oTable <> Nil Then
    Begin
      bPolicyChanged := (oTable.BorderPolicy <> BorderPolicy)
            Or (oTable.HorizontalMargin <> DEF_WORD)
            Or (oTable.VerticalMargin <> DEF_WORD);

      If oTable.BorderPolicy <> BorderPolicy Then
      Begin
        BorderPolicy := oTable.BorderPolicy;
        ClearCellBorders;
      End;

      If oTable.HorizontalMargin <> DEF_WORD Then
        HorizontalMargin := oTable.HorizontalMargin;

      If oTable.VerticalMargin <> DEF_WORD Then
        VerticalMargin := oTable.VerticalMargin;

      If oTable.Background <> DEF_COLOUR Then
        Background := oTable.Background;

      ExpandLastColumn := oTable.ExpandLastColumn;

      If bPolicyChanged Then
        ApplyPolicy;

      // Update border
      CenterHorizontalBorder.Assign(oTable.CenterHorizontalBorder);
      CenterVerticalBorder.Assign(oTable.CenterVerticalBorder);
    End;
End;

Procedure TWPWorkingDocumentTableStartPiece.MergeProperties(oSource: TWPWorkingDocumentPiece);
Var
  oTable: TWPWorkingDocumentTableStartPiece;
  bPolicyChanged: Boolean;
Begin
  Inherited;

  If Assigned(oSource) And (oSource.PieceType = ptTableStart) Then
  Begin
    oTable := TWPWorkingDocumentTableStartPiece(oSource);
    bPolicyChanged := (oTable.BorderPolicy <> BorderPolicy)
        Or (oTable.HorizontalMargin <> HorizontalMargin)
        Or (oTable.VerticalMargin <> VerticalMargin);

    If oTable.BorderPolicy <> BorderPolicy Then
    Begin
      BorderPolicy := tbpNone;
      ClearCellBorders;
    End;

    If oTable.HorizontalMargin <> HorizontalMargin Then
      HorizontalMargin := DEF_WORD;

    If oTable.VerticalMargin <> VerticalMargin Then
      VerticalMargin := DEF_WORD;

    If oTable.Background <> Background Then
      Background := DEF_COLOUR;

    ExpandLastColumn := oTable.ExpandLastColumn;

    If bPolicyChanged Then
      ApplyPolicy;
  End;
End;

Procedure TWPWorkingDocumentTableStartPiece.ClearCellBorders;
Var
  iRow : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  iCell : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  For iRow := 0 To Rows.Count - 1 Do
  Begin
    oRow := Rows[iRow];
    For iCell := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iCell];
      oCell.LeftBorder.Defined := False;
      oCell.RightBorder.Defined := False;
      oCell.TopBorder.Defined := False;
      oCell.BottomBorder.Defined := False;
      oCell.Change(ctLayout, oCell);
    End;
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyNone;
Begin
  LeftBorder.Clear;
  RightBorder.Clear;
  BottomBorder.Clear;
  TopBorder.Clear;
  CenterHorizontalBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyGrid;
Begin
  LeftBorder.SimpleLine;
  RightBorder.SimpleLine;
  BottomBorder.SimpleLine;
  TopBorder.SimpleLine;
  CenterHorizontalBorder.SimpleLine;
  CenterVerticalBorder.SimpleLine;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyLines;
Begin
  BottomBorder.SimpleLine;
  TopBorder.SimpleLine;
  CenterHorizontalBorder.SimpleLine;

  LeftBorder.Clear;
  RightBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyInnerLines;
Begin
  CenterHorizontalBorder.SimpleLine;

  BottomBorder.Clear;
  TopBorder.Clear;
  LeftBorder.Clear;
  RightBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyDots;
Begin
  BottomBorder.SimpleDot;
  TopBorder.SimpleDot;
  CenterHorizontalBorder.SimpleDot;

  LeftBorder.Clear;
  RightBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyInnerDots;
Begin
  CenterHorizontalBorder.SimpleDot;

  BottomBorder.Clear;
  TopBorder.Clear;
  LeftBorder.Clear;
  RightBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyBox;
Begin
  LeftBorder.SimpleLine;
  RightBorder.SimpleLine;
  BottomBorder.SimpleLine;
  TopBorder.SimpleLine;

  CenterHorizontalBorder.Clear;
  CenterVerticalBorder.Clear;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyFancy;
Begin
  LeftBorder.SimpleFancy;
  RightBorder.SimpleFancy;
  BottomBorder.SimpleFancy;
  TopBorder.SimpleFancy;

  CenterHorizontalBorder.Clear;
  CenterVerticalBorder.Clear;
End;



Procedure TWPWorkingDocumentTableStartPiece.ApplyBorderPolicyBoxLines;
Begin
  LeftBorder.SimpleLine;
  RightBorder.SimpleLine;
  BottomBorder.SimpleLine;
  TopBorder.SimpleLine;
  CenterHorizontalBorder.SimpleLine;
  CenterVerticalBorder.Clear;
End;


Function TWPWorkingDocumentTableStartPiece.AnyContentReadOnly : Boolean;
Var
  iLoop : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  Result := False;
  For iLoop := 0 To Rows.Count - 1 Do
  Begin
    oRow := Rows[iLoop];
    Result := Result Or (oRow.IsReadOnly Or oRow.AnyContentReadOnly);
  End;
End;


Function TWPWorkingDocumentTableStartPiece.AnyCellsReadOnly(iCol : Integer) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  For iLoop := 0 To Rows.Count - 1 Do
    Result := Result Or Rows[iLoop].CellIsReadOnly(iCol);
End;


Function TWPWorkingDocumentTableStartPiece.AnyRowsReadOnly : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  For iLoop := 0 To Rows.Count - 1 Do
    Result := Result Or Rows[iLoop].IsReadOnly;
End;


Function TWPWorkingDocumentTableStartPiece.GetCenterHorizontalBorder : TWPBorder;
Begin
  Assert(Invariants('GetCenterHorizontalBorder', FCenterHorizontalBorder, TWPBorder, 'CenterHorizontalBorder'));
  Result := FCenterHorizontalBorder;
End;


Function TWPWorkingDocumentTableStartPiece.GetCenterVerticalBorder : TWPBorder;
Begin
  Assert(Invariants('GetCenterVerticalBorder', FCenterVerticalBorder, TWPBorder, 'CenterVerticalBorder'));
  Result := FCenterVerticalBorder;
End;


Function TWPWorkingDocumentTableStartPiece.GetRows : TWPWorkingDocumentTableRowStartPieces;
Begin
  Assert(Invariants('GetRows', FRows, TWPWorkingDocumentTableRowStartPieces, 'Rows'));
  Result := FRows;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyHorizontalMarginWidth;
Var
  iRow : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  iCell : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  For iRow := 0 To Rows.Count - 1 Do
  Begin
    oRow := Rows[iRow];
    For iCell := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iCell];
      oCell.MarginLeft := FHorizontalMargin;
      oCell.MarginRight := FHorizontalMargin;
    End;
  End;
End;


Procedure TWPWorkingDocumentTableStartPiece.ApplyVerticalMarginWidth;
Var
  iRow : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  iCell : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  For iRow := 0 To Rows.Count - 1 Do
  Begin
    oRow := Rows[iRow];
    For iCell := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iCell];
      oCell.MarginTop := FVerticalMargin;
      oCell.MarginBottom := FVerticalMargin;
    End;
  End;
End;

Function TWPWorkingDocumentTableStartPiece.HasCenterHorizontalBorder: Boolean;
Begin
  Result := Assigned(FCenterHorizontalBorder);
End;

Function TWPWorkingDocumentTableStartPiece.HasCenterVerticalBorder: Boolean;
Begin
  Result := Assigned(FCenterVerticalBorder);
End;

Procedure TWPWorkingDocumentTableStartPiece.ResetRendering;
Begin
  Inherited;
  FContentDirty := True;
  FStructureDirty := True;
End;

procedure TWPWorkingDocumentTableStartPiece.SetExpandLastColumn(const Value: Boolean);
begin
  If FExpandLastColumn <> Value Then
  Begin
    FExpandLastColumn := Value;
    Change(ctLayout, Self);
  End;
end;

function TWPWorkingDocumentTableStartPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCenterHorizontalBorder.sizeInBytes);
  inc(result, FCenterVerticalBorder.sizeInBytes);
  inc(result, FRows.sizeInBytes);
end;

{ TWPWorkingDocumentTableStartPieces }

Function TWPWorkingDocumentTableStartPieces.GetElements(iIndex: Integer): TWPWorkingDocumentTableStartPiece;
Begin
  Result := TWPWorkingDocumentTableStartPiece(ObjectByIndex[iIndex]);
End;

Function TWPWorkingDocumentTableStartPieces.ItemClass: TFslObjectClass;
Begin
  Result := TWPWorkingDocumentTableStartPiece;
End;

Constructor TWPMapContainer.Create;
Begin
  Inherited Create;
  FChildren := TWPMapContainers.Create;
  FRows := TWPMapRows.Create;
  FPrimary := True;
End;

Destructor TWPMapContainer.Destroy;
Begin
  FChildren.Free;
  FRows.Free;
  Inherited;
End;

Procedure TWPMapContainer.Clear;
Begin
  FChildren.Clear;
  FRows.Clear;
  WantPainting;
End;

Function TWPMapContainer.Link: TWPMapContainer;
Begin
  Result := TWPMapContainer(Inherited Link);
End;

Procedure TWPMapContainer.SetChildren(Const Value: TWPMapContainers);
Begin
  FChildren.Free;
  FChildren := Value;
End;

Procedure TWPMapContainer.SetRows(Const Value: TWPMapRows);
Begin
  FRows.Free;
  FRows := Value;
End;

Procedure TWPMapContainer.SetMarginLeft(Const Value: Integer);
Begin
  If FMarginLeft <> Value Then
    Begin
    WantPainting;
    FMarginLeft := Value;
    End;
End;

Function TWPMapContainer.InnerLeft: Integer;
Begin
  Result := Left + MarginLeft;
End;


Procedure TWPMapContainer.SetMarginBottom(Const Value: Integer);
Begin
  If FMarginBottom <> Value Then
    Begin
    WantPainting;
    FMarginBottom := Value;
    End;
End;

Function TWPMapContainer.InnerBottom: Integer;
Begin
  Result := Bottom - MarginBottom;
End;


Procedure TWPMapContainer.SetMarginTop(Const Value: Integer);
Begin
  If FMarginTop <> Value Then
    Begin
    WantPainting;
    FMarginTop := Value;
    End;
End;

Function TWPMapContainer.InnerTop: Integer;
Begin
  Result := Top + MarginTop;
End;


Procedure TWPMapContainer.SetMarginRight(Const Value: Integer);
Begin
  If FMarginRight <> Value Then
    Begin
    WantPainting;
    FMarginRight := Value;
    End;
End;

Function TWPMapContainer.InnerRight: Integer;
Begin
  Result := Right - MarginRight;
End;


Function TWPMapContainer.InnerWidth: Integer;
Begin
  Result := InnerRight - InnerLeft;
End;

Procedure TWPMapContainer.Assign(oSource: TFslObject);
Begin
  Inherited;
  Children := TWPMapContainer(oSource).FChildren.Clone;
  Rows := TWPMapContainer(oSource).FRows.Clone;
  FMarginLeft := TWPMapContainer(oSource).FMarginLeft;
  FMarginRight := TWPMapContainer(oSource).FMarginRight;
  FMarginTop := TWPMapContainer(oSource).FMarginTop;
  FMarginBottom := TWPMapContainer(oSource).FMarginBottom;
  ReParent;
End;


Procedure TWPMapContainer.ReParent;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To FChildren.Count - 1 Do
    FChildren[iLoop].Parent := Self;
  For iLoop := 0 To FRows.Count - 1 Do
    FRows[iLoop].Parent := Self;
End;


Function TWPMapContainer.Fits(iHeight : Integer) : Boolean;
Begin
  Result := FCursor + iHeight <= Bottom;
End;


Procedure TWPMapContainer.AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean);
Var
  iLoop : Integer;
Begin
  Inherited AdjustTop(iOffset, iLineOffset, bPaint);
  For iLoop := 0 To FRows.Count - 1 Do
    FRows[iLoop].AdjustTop(iOffset, iLineOffset, bPaint);
  For iLoop := 0 To FChildren.Count - 1 Do
    FChildren[iLoop].AdjustTop(iOffset, iLineOffset, bPaint);
End;

Function TWPMapContainer.GetChildren : TWPMapContainers;
Begin
  Assert(Invariants('GetChildren', FChildren, TWPMapContainers, 'Children'));
  Result := FChildren;
End;

Function TWPMapContainer.GetRows : TWPMapRows;
Begin
  Assert(Invariants('GetRows', FRows, TWPMapRows, 'Rows'));
  Result := FRows;
End;

function TWPMapContainer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FChildren.sizeInBytes);
  inc(result, FRows.sizeInBytes);
end;

{ TWPMapContainers }

Function TWPMapContainers.GetContainer(iIndex: Integer): TWPMapContainer;
Begin
  Result := TWPMapContainer(ObjectByIndex[iIndex]);
End;

Function TWPMapContainers.ItemClass: TFslObjectClass;
Begin
  Result := TWPMapContainer;
End;

Function TWPMapContainers.Clone: TWPMapContainers;
Begin
  Result := TWPMapContainers(Inherited Clone);
End;

Function TWPMapContainers.Link: TWPMapContainers;
Begin
  Result := TWPMapContainers(Inherited Link);
End;

Function TWPMapContainers.GetLastOffset: Integer;
Begin
  If Count = 0 Then
    Result := -1
  Else
    Result := Container[Count - 1].GetLastOffset;
End;

Function TWPMapContainer.GetLastOffset: Integer;
Begin
  If (FRows <> Nil) And (FRows.Count > 0) Then
    Result := FRows[FRows.Count - 1].GetLastOffset
  Else If (FChildren <> Nil) And (FChildren.Count > 0) Then
    Result := FChildren[FChildren.Count - 1].GetLastOffset
  Else
    Result := -1;
End;

Function TWPMapContainer.FirstRow: TWPMapRow;
Begin
  If Children.Count > 0 Then
    Result := Children[0].FirstRow
  Else If Rows.Count > 0 Then
    Result := Rows[0]
  Else
    Result := Nil;
End;

Function TWPMapContainer.LastRow: TWPMapRow;
Begin
  If Children.Count > 0 Then
    Result := Children[Children.Count - 1].FirstRow
  Else If Rows.Count > 0 Then
    Result := Rows[Rows.Count - 1]
  Else
    Result := Nil;
End;

Constructor TWPMapRow.Create;
Begin
  Inherited Create;

  FItems := TWPMapItems.Create;
End;


Destructor TWPMapRow.Destroy;
Begin
  FItems.Free;

  Inherited;
End;


Function TWPMapRow.Link: TWPMapRow;
Begin
  Result := TWPMapRow(Inherited Link);
End;

Function TWPMapRow.Clone: TWPMapRow;
Begin
  Result := TWPMapRow(Inherited Clone);
End;

Procedure TWPMapRow.SetItems(Const Value: TWPMapItems);
Begin
  FItems.Free;
  FItems := Value;
End;

Function TWPMapRow.FirstLeft: Integer;
Begin
  If Items.count = 0 Then
    Result := Left
  Else
    Result := Items[0].Left;
End;

Function TWPMapRow.LastRight: Integer;
Begin
  If Items.count = 0 Then
    Result := Left + FAllow
  Else
    Result := Items[Items.Count - 1].Right + FAllow;
End;


Function TWPMapRow.ColCount : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Items.Count - 1 Do
    Inc(Result, Items[iLoop].OffsetLength);
End;


Function TWPMapRow.SumWidths(iLimit: Integer): Integer;
Var
  iLoop : Integer;
Begin
  If iLimit < 0 Then
    iLimit := Items.Count - 1;

  Result := 0;
  For iLoop := 0 To iLimit Do
    Inc(Result, Items[iLoop].Width);
End;

Procedure TWPMapRow.Assign(oSource: TFslObject);
Begin
  Inherited;
  Items := TWPMapRow(oSource).FItems.Clone;
  FBaseLine := TWPMapRow(oSource).FBaseLine;
  ReParent;
End;

Procedure TWPMapRow.ReParent;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To FItems.Count - 1 Do
    FItems[iLoop].Parent := Self;
End;


Procedure TWPMapRow.AdjustTop(iOffset, iLineOffset : Integer; bPaint : Boolean);
Var
  iLoop : Integer;
Begin
  Inherited AdjustTop(iOffset, iLineOffset, bPaint);
  FLine := FLine + iLineOffset;
  For iLoop := 0 To FItems.Count - 1 Do
    FItems[iLoop].AdjustTop(iOffset, iLineOffset, bPaint);
End;

function TWPMapRow.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FItems.sizeInBytes);
end;

{ TWPMapRows }

Function TWPMapRows.GetRow(iIndex: Integer): TWPMapRow;
Begin
  Result := TWPMapRow(ObjectByIndex[iIndex]);
End;

Function TWPMapRows.ItemClass: TFslObjectClass;
Begin
  Result := TWPMapRow;
End;

Function TWPMapRows.Clone: TWPMapRows;
Begin
  Result := TWPMapRows(Inherited Clone);
End;

Function TWPMapRows.Link: TWPMapRows;
Begin
  Result := TWPMapRows(Inherited Link);
End;


Function TWPMapRows.Last : TWPMapRow;
Begin
  Result := Row[Count - 1];
End;


Function TWPMapRows.First : TWPMapRow;
Begin
  Result := Row[0];
End;


Function TWPMapRow.GetText : String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Items.Count - 1 Do
    Result := Result + Items[iLoop].GetText;
End;


Function TWPMapRow.GetItems : TWPMapItems;
Begin
  Assert(Invariants('GetItems', FItems, TWPMapItems, 'Items'));
  Result := FItems;
End;


Function TWPMapRow.GetLastOffset: Integer;
Begin
  If (FItems <> Nil) And (FItems.Count > 0) Then
    Result := FItems[FItems.Count - 1].WorkingOffsetEnd
  Else
    Result := -1;
End;

Procedure TWPMapRow.Allow(iAllow: Integer);
Begin
  FAllow := iAllow;
End;

{ TWPCharIterator }

Procedure TWPCharIterator.First;
Begin
  moveTo(WorkingMinimum);
End;

Procedure TWPCharIterator.Last;
Begin
  moveTo(WorkingMaximum);
End;

Function TWPCharIterator.GetCurrent: Char;
Begin
  Assert(CheckCondition(Assigned(FCurrent), 'GetCurrent', 'no content'));
  Result := FCurrent.LogicalText[FOffset + 1];
End;

Function TWPCharIterator.GetCurrentPosition: Integer;
Begin
  Result := FCurrent.Metrics.Position + FOffset;
End;

Function TWPCharIterator.More: Boolean;
Begin
  Result := Assigned(FCurrent);
End;

Procedure TWPCharIterator.MoveTo(iPosition: Integer);
Begin
  If Not Document.GetPieceByPosition(iPosition, FCurrent, FOffset, FIndex) Then
    FCurrent := Nil;
End;

Procedure TWPCharIterator.Prev;
Begin
  If Assigned(FCurrent) Then
    Begin
    If FOffset > 1 Then
      Dec(FOffset)
    Else If FIndex > 0 Then
      Begin
      Dec(FIndex);
      FCurrent := Document.Pieces[FIndex];
      FOffset := FCurrent.Metrics.CharCount-1;
      End
    Else If FOffset = 0 Then
      FCurrent := Nil
    Else
      FOffset := 0;
    End;
End;

Procedure TWPCharIterator.Next;
Begin
  If Assigned(FCurrent) Then
    Begin
    If FCurrent.Metrics.Position + FOffset >= WorkingMaximum - 1 Then
      FCurrent := Nil
    Else If FOffset < FCurrent.Metrics.CharCount - 1 Then
      Inc(FOffset)
    Else If FIndex < Document.Pieces.Count - 1 Then
      Begin
      Inc(FIndex);
      FCurrent := Document.Pieces[FIndex];
      FOffset := 0;
      End
    Else
      FCurrent := Nil;
    End;
End;

function TWPCharIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCurrent.sizeInBytes);
end;

Constructor TWPIterator.Create;
Begin
  Inherited;
  FMinimum := -1;
  FMaximum := -1;
End;


Constructor TWPIterator.Create(oDocument: TWPWorkingDocument);
Begin
  Create;
  Document := oDocument;
End;


Destructor TWPIterator.Destroy;
Begin
  FDocument.Free;
  Inherited;
End;


Function TWPIterator.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWPIterator.GetDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Procedure TWPIterator.SetDocument(Const Value: TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;

  If HasDocument Then
    FCharCount := Document.CharCount
  Else
    FCharCount := 0;
End;


Procedure TWPIterator.SetBounds(Const iMinimum, iMaximum: Integer);
Begin
  FMinimum := iMinimum;
  FMaximum := iMaximum;
End;


Procedure TWPIterator.First;
Begin
  RaiseError('First', 'Must override '+Classname+'.First');
End;


Procedure TWPIterator.Last;
Begin
  RaiseError('First', 'Must override '+Classname+'.Last');
End;


Procedure TWPIterator.Next;
Begin
  RaiseError('Next', 'Must override '+Classname+'.Next');
End;


Procedure TWPIterator.Prev;
Begin
  RaiseError('Prev', 'Must override '+Classname+'.Prev');
End;


Function TWPIterator.More: Boolean;
Begin
  Result := False;
  RaiseError('More', 'Must override '+Classname+'.More');
End;


Function TWPIterator.WorkingMaximum: Integer;
Begin
  If FMaximum = -1 Then
    Result := FCharCount
  Else
    Result := IntegerMin(FCharCount, FMaximum);
End;


Function TWPIterator.WorkingMinimum: Integer;
Begin
  Result := IntegerMax(0, FMinimum);
End;


function TWPIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
end;

Constructor TWPDocumentTranslator.Create;
Begin
  Inherited;
  FRowMap := TFslObjectMatch.Create;
  FSplitter := TWPWordIterator.Create;
End;


Destructor TWPDocumentTranslator.Destroy;
Begin
  FSplitter.Free;
  FRowMap.Free;
  FWorkingDocument.Free;
  FWorkingStyles.Free;
  FDocument.Free;
  Inherited;
End;


Function TWPDocumentTranslator.GetDocument : TWPDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPDocument, 'Document'));
  Result := FDocument;
End;


Procedure TWPDocumentTranslator.SetDocument(Const Value : TWPDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;


Function TWPDocumentTranslator.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWPDocumentTranslator.GetWorkingDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetWorkingDocument', FWorkingDocument, TWPWorkingDocument, 'WorkingDocument'));
  Result := FWorkingDocument;
End;


Procedure TWPDocumentTranslator.SetWorkingDocument(Const Value : TWPWorkingDocument);
Begin
  FWorkingDocument.Free;
  FWorkingDocument := Value;
End;


Function TWPDocumentTranslator.HasWorkingDocument : Boolean;
Begin
  Result := Assigned(FWorkingDocument);
End;


Function TWPDocumentTranslator.GetWorkingStyles : TWPStyles;
Begin
  Assert(Invariants('GetWorkingStyles', FWorkingStyles, TWPStyles, 'WorkingStyles'));
  Result := FWorkingStyles;
End;


Procedure TWPDocumentTranslator.SetWorkingStyles(Const Value : TWPStyles);
Begin
  FWorkingStyles.Free;
  FWorkingStyles := Value;
End;


Function TWPDocumentTranslator.HasWorkingStyles : Boolean;
Begin
  Result := Assigned(FWorkingStyles);
End;


Procedure TWPDocumentTranslator.TranslateToWorking;
Var
  iLoop : Integer;
  oSource : TWPStyle;
  oDest : TWPStyle;
Begin
  WorkingDocument.Clear;
  WorkingDocument.AllowedWords.Assign(Document.AllowedWords);

  For iLoop := 0 to Document.Annotations.Count - 1 Do
    WorkingDocument.AllAnnotations.Add(Document.Annotations[iLoop].Link);
  For iLoop := 0 to Document.Attachments.Count - 1 Do
    WorkingDocument.Attachments.Add(TranslateAttachment(Document.Attachments[iLoop]));

  For iLoop := 0 To Document.Styles.Count - 1 Do
  Begin
    oSource := Document.Styles[iLoop];
    oDest := WorkingStyles.GetByName(oSource.Name);
    If Not Assigned(oDest) Then
      WorkingStyles.Add(oSource.Clone);
  End;

  If Document.Blocks.Count = 0 Then
    AddParagraph
  Else
    For iLoop := 0 To Document.Blocks.Count - 1 Do
      TranslateBlock(Document.Blocks[iLoop]);
  WorkingDocument.relinkAttachments;
  WorkingDocument.RegenerateMetrics(False, False);
End;


Procedure TWPDocumentTranslator.TranslateBlock(oBlock : TWPDocumentBlock);
Begin
  If oBlock Is TWPDocumentSection Then
    TranslateSection(TWPDocumentSection(oBlock))
  Else If oBlock Is TWPDocumentTable Then
    TranslateTable(TWPDocumentTable(oBlock))
  Else If oBlock Is TWPDocumentBreak Then
    TranslateBreak(TWPDocumentBreak(oBlock))
  Else If oBlock Is TWPDocumentParagraph Then
    TranslateParagraph(TWPDocumentParagraph(oBlock))
  Else
    RaiseError('TranslateBlock', 'Unexpected Block Type '+oBlock.className);
End;


Function  TWPDocumentTranslator.CreateSection(oSection : TWPDocumentSection) : TWPWorkingDocumentSectionStartPiece;
Begin
  Result := TWPWorkingDocumentSectionStartPiece.Create;
  Try
    Result.Namespace := oSection.Namespace;
    Result.Name := oSection.Name;
    Result.RawData.Assign(oSection.RawData);
    Result.DisplayName := oSection.Title;
    Result.Deletable := oSection.Deletable;
    Result.IsField := oSection.IsField;
    Result.Key := oSection.Key;
    Result.DisplayType := WorkingDocumentSectionDisplayTypeForDocumentSectionDisplay[oSection.Display];
    Result.ReadOnly := TriStateForDocumentObjectReadOnly[oSection.ReadOnly];
    Result.Hotspot := oSection.Hotspot.Clone;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateSection(oSection : TWPDocumentSection);
Var
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  WorkingDocument.Pieces.Add(CreateSection(oSection));

  If oSection.Blocks.Count = 0 Then
    AddParagraph
  Else
    For iLoop := 0 To oSection.Blocks.Count - 1 Do
      TranslateBlock(oSection.Blocks[iLoop]);

  oStop := TWPWorkingDocumentStopPiece.Create;
  oStop.StopType := stSection;
  Try
    WorkingDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateBreak(oBreak : TWPDocumentBreak);
Var
  oPiece : TWPWorkingDocumentBreakPiece;
Begin
  oPiece := TWPWorkingDocumentBreakPiece.Create;
  Try
    oPiece.BreakType := WorkingDocumentBreakPieceTypeForDocumentBreakBreakType[oBreak.BreakType];
    oPiece.Alignment := oBreak.Alignment;
    oPiece.Width := oBreak.Width;
    oPiece.PenColour := oBreak.PenColour;
    oPiece.PenWidth := oBreak.PenWidth;
    oPiece.PenStyle := oBreak.PenStyle;
    oPiece.EndStyle := oBreak.EndStyle;
    WorkingDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;



Procedure TWPDocumentTranslator.TranslateTable(oTable : TWPDocumentTable);
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  If Not oTable.HasNonEmptyRows Then
    Exit;

  oStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    oStart.CenterHorizontalBorder.Assign(oTable.CenterHorizontalBorder);
    oStart.CenterVerticalBorder.Assign(oTable.CenterVerticalBorder);
    oStart.Background := oTable.Background;
    oStart.BorderPolicy := WorkingDocumentTableBorderPolicyForDocumentTableBorderPolicy[oTable.BorderPolicy];
    oStart.LeftBorder.Assign(oTable.LeftBorder);
    oStart.TopBorder.Assign(oTable.TopBorder);
    oStart.RightBorder.Assign(oTable.RightBorder);
    oStart.BottomBorder.Assign(oTable.BottomBorder);
    oStart.ReadOnly := TriStateForDocumentObjectReadOnly[oTable.ReadOnly];
    oStart.HorizontalMargin := oTable.HorizontalMargin;
    oStart.VerticalMargin := oTable.VerticalMargin;
    oStart.ExpandLastColumn := oTable.ExpandLastColumn;
    WorkingDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  For iLoop := 0 To oTable.Rows.Count - 1 Do
    TranslateTableRow(oTable.Rows[iLoop], Nil);

  oStop := TWPWorkingDocumentStopPiece.Create;
  oStop.StopType := stTable;
  Try
    WorkingDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateTableRow(oTableRow : TWPDocumentTableRow; oOwner : TWPWorkingDocumentTableRowStartPiece);
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  If oTableRow.Cells.Count = 0 Then
    Exit;
    
  oStart := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    oStart.Hotspot := oTableRow.Hotspot.Clone;
    oStart.Header := oTableRow.Header;
    oStart.BreakBefore := oTableRow.BreakBefore;
//    oStart.LeftBorder.Assign(oTableRow.LeftBorder);
//    oStart.TopBorder.Assign(oTableRow.TopBorder);
//    oStart.RightBorder.Assign(oTableRow.RightBorder);
//    oStart.BottomBorder.Assign(oTableRow.BottomBorder);
    oStart.Background := oTableRow.Background;
    oStart.LowerPaddingSize := oTableRow.LowerPaddingSize;
    oStart.LowerPaddingColour := oTableRow.LowerPaddingColour;
    oStart.ReadOnly := TriStateForDocumentObjectReadOnly[oTableRow.ReadOnly];
    oStart.Owner := oOwner;
    WorkingDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  For iLoop := 0 To oTableRow.Cells.Count - 1 Do
    TranslateTableCell(oTableRow.Cells[iLoop]);

  oStop := TWPWorkingDocumentStopPiece.Create;
  oStop.StopType := stTableRow;
  Try
    WorkingDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  For iLoop := 0 To oTableRow.Rows.Count - 1 Do
    TranslateTableRow(oTableRow.Rows[iLoop], oStart);
End;


Procedure TWPDocumentTranslator.TranslateTableCell(oTableCell : TWPDocumentTableCell);
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    oStart.Hotspot := oTableCell.Hotspot.Clone;
    oStart.Span := oTableCell.Span;
    oStart.Width := oTableCell.Width;
    oStart.Background := oTableCell.Background;
    oStart.MarginLeft := oTableCell.MarginLeft;
    oStart.MarginTop := oTableCell.MarginTop;
    oStart.MarginRight := oTableCell.MarginRight;
    oStart.MarginBottom := oTableCell.MarginBottom;
    oStart.LeftBorder.Assign(oTableCell.LeftBorder);
    oStart.TopBorder.Assign(oTableCell.TopBorder);
    oStart.RightBorder.Assign(oTableCell.RightBorder);
    oStart.BottomBorder.Assign(oTableCell.BottomBorder);
    oStart.Background := oTableCell.Background;
    oStart.VerticalAlignment := oTableCell.VerticalAlignment;
    oStart.ReadOnly := TriStateForDocumentObjectReadOnly[oTableCell.ReadOnly];
    WorkingDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  If oTableCell.Paragraphs.Count = 0 Then
    AddParagraph
  Else
    For iLoop := 0 To oTableCell.Paragraphs.Count - 1 Do
      TranslateParagraph(oTableCell.Paragraphs[iLoop]);

  oStop := TWPWorkingDocumentStopPiece.Create;
  oStop.StopType := stTableCell;
  Try
    WorkingDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateParagraph(oParagraph : TWPDocumentParagraph);
Var
  iLoop : Integer;
  oPara : TWPWorkingDocumentParaPiece;
Begin
  For iLoop := 0 To oParagraph.Contents.Count - 1 Do
    TranslateContents(oParagraph.Contents[iLoop]);

  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    oPara.Style := oParagraph.Style;
    oPara.Format.Assign(oParagraph.Format);
    oPara.Font.Assign(oParagraph.Font);

    WorkingDocument.Pieces.Add(oPara.Link);
  Finally
    oPara.Free;
  End;
End;


Procedure TWPDocumentTranslator.AddParagraph;
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    WorkingDocument.Pieces.Add(oPara.Link);
  Finally
    oPara.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateContents(oContent : TWPDocumentContent);
Begin
  If oContent Is TWPDocumentText Then
    TranslateText(TWPDocumentText(oContent))
  Else If oContent Is TWPDocumentImage Then
    TranslateImage(TWPDocumentImage(oContent))
  Else If oContent Is TWPDocumentField Then
    TranslateField(TWPDocumentField(oContent))
  Else If oContent Is TWPDocumentLinebreak Then
    TranslateLineBreak(TWPDocumentLinebreak(oContent))
  Else
    RaiseError('TranslateContents', 'Unexpected Content Type '+oContent.className);
End;


Procedure TWPDocumentTranslator.TranslateText(oText : TWPDocumentText);
Begin
  FSplitter.Init(oText.Value);
  While FSplitter.More Do
    AddWord(FSplitter.Next, oText);
End;


Procedure TWPDocumentTranslator.AddWord(Const sWord : String; oText : TWPDocumentText);
Var
  oPiece : TWPWorkingDocumentTextPiece;
Begin
  oPiece := TWPWorkingDocumentTextPiece.Create;
  Try
    oPiece.Content := sWord;
    oPiece.Style := oText.Style;
    oPiece.Font.Assign(oText.Font);
    oPiece.DrawnFont := oText.DrawnFont;
    oPiece.AnnotationId := oText.Annotation;

    WorkingDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPDocumentTranslator.TranslateLineBreak(oLinebreak : TWPDocumentLinebreak);
Var
  oPiece : TWPWorkingDocumentLinebreakPiece;
Begin
  oPiece := TWPWorkingDocumentLinebreakPiece.Create;
  Try
    oPiece.Style := oLinebreak.Style;
    oPiece.Font.Assign(oLinebreak.Font);
    oPiece.AnnotationId := oLinebreak.Annotation;

    WorkingDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateImage(oImage : TWPDocumentImage);
Var
  oPiece : TWPWorkingDocumentImagePiece;
Begin
  oPiece := TWPWorkingDocumentImagePiece.Create;
  Try
    oPiece.Hotspot := oImage.Hotspot.Clone;
    oPiece.AnnotationId := oImage.Annotation;
    oPiece.Name := oImage.Name;
    If oImage.HasImage Then
      oPiece.Image := oImage.Image.Clone
    Else
      oPiece.Image := Nil;
    If oImage.HasSelectionImage Then
      oPiece.SelectionImage := oImage.SelectionImage.Clone
    Else
      oPiece.SelectionImage := Nil;

    oPiece.HasImageMap := oImage.HasMap;
    If (oPiece.hasImageMap) Then
      oPiece.ImageMap.Assign(oImage.Map);
    oPiece.Adornments.Assign(oImage.Adornments);

    oPiece.Border := oImage.BorderWidth;
    oPiece.BorderColour := oImage.BorderColour;
    oPiece.TransparentColour := oImage.TransparentColour;
    oPiece.Width := oImage.ImageWidth;
    oPiece.Height := oImage.ImageHeight;
    oPiece.SizePolicy := oImage.SizePolicy;
    oPiece.FrameIndex := oImage.FrameIndex;
    oPiece.VerticalAlignment := oImage.VerticalAlignment;

    oPiece.Style := oImage.Style;
    oPiece.Font.Assign(oImage.Font);

    WorkingDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Function  TWPDocumentTranslator.CreateField(oField : TWPDocumentField) : TWPWorkingDocumentFieldStartPiece;
Begin
  Result := TWPWorkingDocumentFieldStartPiece.Create;
  Try
    Result.Namespace := oField.Namespace;
    result.AnnotationId := oField.Annotation;
    Result.Name := oField.Name;
    Result.ReadOnly := TriStateForDocumentObjectReadOnly[oField.ReadOnly];
    Result.Deletable := oField.Deletable;
    Result.RawData.Assign(oField.RawData);
    Result.FixedFormat := oField.FixedFormat;
    Result.CheckedIndex := oField.CheckedIndex;
    Result.Width := oField.Width;
    Result.Hotspot := oField.Hotspot.Clone;

    Result.Style := oField.Style;
    Result.Font.Assign(oField.Font);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentTranslator.CreateImage(oImage: TWPDocumentImage): TWPWorkingDocumentImagePiece;
Begin
  result := TWPWorkingDocumentImagePiece.Create;
  Try
    result.Hotspot := oImage.Hotspot.Clone;
    result.Name := oImage.Name;
    If oImage.HasImage Then
      result.Image := oImage.Image.Clone
    Else
      result.Image := Nil;
    If oImage.HasSelectionImage Then
      result.SelectionImage := oImage.SelectionImage.Clone
    Else
      result.SelectionImage := Nil;

    result.HasImageMap := oImage.HasMap;
    If (result.hasImageMap) Then
      result.ImageMap.Assign(oImage.Map);
    result.Adornments.Assign(oImage.Adornments);

    result.Border := oImage.BorderWidth;
    result.BorderColour := oImage.BorderColour;
    result.TransparentColour := oImage.TransparentColour;
    result.Width := oImage.ImageWidth;
    result.Height := oImage.ImageHeight;
    result.VerticalAlignment := ImageVerticalAlignmentForDocumentImageVerticalAlignment[oImage.VerticalAlignment];
    result.SizePolicy := oImage.SizePolicy;

    result.Style := oImage.Style;
    result.Font.Assign(oImage.Font);
    result.Link;
  Finally
    result.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateField(oField : TWPDocumentField);
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  iLoop : Integer;
Begin
  oStart := CreateField(oField);
  Try
    WorkingDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  For iLoop := 0 To oField.Contents.Count - 1 Do
    TranslateContents(oField.Contents[iLoop]);

  oStop := TWPWorkingDocumentFieldStopPiece.Create;
  Try
    oStop.Style := oField.Style;
    oStop.Font.Assign(oField.Font);

    WorkingDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateToDocument;
Var
  oIter : TWPPieceIterator;
  i : Integer;
  oAnnot : TWPDocumentAnnotation;
Begin
  FRowMap.Clear;
  Document.Clear;
  Document.AllowedWords.Assign(WorkingDocument.AllowedWords);

  If Document.Styles <> WorkingStyles Then
    Document.Styles.Assign(WorkingStyles);

  For i := 0 to WorkingDocument.AllAnnotations.Count - 1 Do
  Begin
    oAnnot := Document.Annotations.New;
    Try
      oAnnot.Owner := WorkingDocument.AllAnnotations[i].Owner;
      oAnnot.Text := WorkingDocument.AllAnnotations[i].Text;
      Document.Annotations.add(oAnnot.Link);
    Finally
      oAnnot.Free;
    End;
  End;

  For i := 0 to WorkingDocument.Attachments.Count - 1 Do
    Document.Attachments.Add(TranslateAttachment(WorkingDocument.Attachments[i]));

  oIter := TWPPieceIterator.Create(WorkingDocument.Link);
  Try
    oIter.PieceTypes := ALL_PIECE_TYPES;
    oIter.First;
    While oIter.More Do
      TranslateBlock(oIter, Document.Blocks);
  Finally
    oIter.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateBlock(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks);
Begin
  Case oIter.Current.PieceType Of
    ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptPara: TranslateParagraph(oIter, oBlocks);
    ptBreak : TranslateBreak(oIter, oBlocks);
    ptTableStart : TranslateTable(oIter, oBlocks);
    ptSectionStart : TranslateSection(oIter, oBlocks);
  Else
    // ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStop
    RaiseError('TranslateBlock', 'Illegal Piece Type '+NAMES_WPPIECETYPE[oIter.Current.PieceType]);
  End;
End;

Procedure TWPDocumentTranslator.TranslateBreak(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks);
Var
  oBreak : TWPDocumentBreak;
  oPiece : TWPWorkingDocumentBreakPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptBreak, 'TranslateBreak', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptBreak]])));
  oPiece := TWPWorkingDocumentBreakPiece(oIter.Current);

  oBreak := TWPDocumentBreak.Create;
  Try
    oBreak.BreakType := DocumentBreakBreakTypeForWorkingDocumentBreakPieceType[oPiece.BreakType];
    oBreak.Alignment := oPiece.Alignment;
    oBreak.Width := oPiece.Width;
    oBreak.PenColour := oPiece.PenColour;
    oBreak.PenWidth := oPiece.PenWidth;
    oBreak.PenStyle := oPiece.PenStyle;
    oBreak.EndStyle := oPiece.EndStyle;

    oBlocks.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;

  oIter.Next;
End;


Function TWPDocumentTranslator.CreateSection(oPiece : TWPWorkingDocumentSectionStartPiece) : TWPDocumentSection;
Begin
  Result := TWPDocumentSection.Create;
  Try
    Result.Namespace := oPiece.Namespace;
    Result.Name := oPiece.Name;
    Result.RawData.Assign(oPiece.RawData);
    Result.Title := oPiece.DisplayName;
    Result.Display := DocumentSectionDisplayForWorkingDocumentSectionDisplayType[oPiece.DisplayType];
    Result.Deletable := oPiece.Deletable;
    Result.IsField := oPiece.IsField;
    Result.Key := oPiece.Key;
    Result.ReadOnly := DocumentObjectReadOnlyForTriState[oPiece.ReadOnly];
    Result.Hotspot := oPiece.Hotspot.Clone;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentTranslator.TranslateSection(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks);
Var
  oSection : TWPDocumentSection;
  oPiece : TWPWorkingDocumentSectionStartPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptSectionStart, 'TranslateSectionStart', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptSectionStart]])));
  oPiece := TWPWorkingDocumentSectionStartPiece(oIter.Current);

  oSection := CreateSection(oPiece);
  Try
    oIter.Next;

    While oIter.More And (oIter.Current.PieceType <> ptSectionStop) Do
      TranslateBlock(oIter, oSection.Blocks);

    oBlocks.Add(oSection.Link);
  Finally
    oSection.Free;
  End;

  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateParagraph(oIter : TWPPieceIterator; oBlocks : TWPDocumentObjects);
Var
  oParagraph : TWPDocumentParagraph;
  oPiece : TWPWorkingDocumentParaPiece;
Begin
  oParagraph := TWPDocumentParagraph.Create;
  Try
    While oIter.More And (oIter.Current.PieceType <> ptPara) Do
      TranslateContent(oIter, oParagraph.Contents);

    If Not oIter.More Then
      RaiseError('TranslateParagraph', 'Premature end of paragraph');

    Assert(CheckCondition(oIter.Current.PieceType = ptPara, 'TranslatePara', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptPara]])));
    oPiece := TWPWorkingDocumentParaPiece(oIter.Current);

    oParagraph.Style := oPiece.Style;
    oParagraph.Font.Assign(oPiece.Font);
    oParagraph.Format.Assign(oPiece.Format);

    oBlocks.Add(oParagraph.Link);
  Finally
    oParagraph.Free;
  End;

  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateTable(oIter : TWPPieceIterator; oBlocks : TWPDocumentBlocks);
Var
  oTable : TWPDocumentTable;
  oPiece : TWPWorkingDocumentTableStartPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptTableStart, 'TranslateTableStart', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptTableStart]])));

  oPiece := TWPWorkingDocumentTableStartPiece(oIter.Current);

  oTable := TWPDocumentTable.Create;
  Try
    oTable.CenterHorizontalBorder.Assign(oPiece.CenterHorizontalBorder);
    oTable.CenterVerticalBorder.Assign(oPiece.CenterVerticalBorder);
    oTable.BorderPolicy := DocumentTableBorderPolicyForWorkingDocumentTableBorderPolicy[oPiece.BorderPolicy];
    oTable.LeftBorder.Assign(oPiece.LeftBorder);
    oTable.TopBorder.Assign(oPiece.TopBorder);
    oTable.RightBorder.Assign(oPiece.RightBorder);
    oTable.BottomBorder.Assign(oPiece.BottomBorder);
    oTable.Background := oPiece.Background;
    oTable.ReadOnly := DocumentObjectReadOnlyForTriState[oPiece.ReadOnly];
    oTable.HorizontalMargin := oPiece.HorizontalMargin;
    oTable.VerticalMargin := oPiece.VerticalMargin;
    oTable.ExpandLastColumn := oPiece.ExpandLastColumn;
    oIter.Next;

    While oIter.More And (oIter.Current.PieceType <> ptTableStop) Do
      TranslateTableRow(oIter, oTable.Rows);

    oBlocks.Add(oTable.Link);
  Finally
    oTable.Free;
  End;

  oIter.Next;
End;



Procedure TWPDocumentTranslator.TranslateTableRow(oIter : TWPPieceIterator; oRows : TWPDocumentTableRows);
Var
  oTableRow : TWPDocumentTableRow;
  oPiece : TWPWorkingDocumentTableRowStartPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptRowStart, 'TranslateTableRowStart', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptRowStart]])));
  oPiece := TWPWorkingDocumentTableRowStartPiece(oIter.Current);

  oTableRow := TWPDocumentTableRow.Create;
  Try
    oTableRow.Hotspot := oPiece.Hotspot.Clone;

    oTableRow.Header := oPiece.Header;
    oTableRow.BreakBefore := oPiece.BreakBefore;
    oTableRow.Background := oPiece.Background;
    oTableRow.LowerPaddingSize := oPiece.LowerPaddingSize;
    oTableRow.LowerPaddingColour := oPiece.LowerPaddingColour;
    oTableRow.ReadOnly := DocumentObjectReadOnlyForTriState[oPiece.ReadOnly];
    // todo: borders...

    oIter.Next;

    While oIter.More And (oIter.Current.PieceType <> ptRowStop) Do
      TranslateTableCell(oIter, oTableRow.Cells);

    If oPiece.Owner <> Nil Then
       TWPDocumentTableRow(FRowMap.GetValueByKey(oPiece.Owner)).Rows.Add(oTableRow.Link)
    Else
      oRows.Add(oTableRow.Link);

    FRowMap.Add(oPiece.Link, oTableRow.Link);
  Finally
    oTableRow.Free;
  End;

  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateTableCell(oIter : TWPPieceIterator; oCells : TWPDocumentTableCells);
Var
  oTableCell : TWPDocumentTableCell;
  oPiece : TWPWorkingDocumentTableCellStartPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptCellStart, 'TranslateTableCellStart', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptCellStart]])));
  oPiece := TWPWorkingDocumentTableCellStartPiece(oIter.Current);

  oTableCell := TWPDocumentTableCell.Create;
  Try
    oTableCell.Hotspot := oPiece.Hotspot.Clone;

    oTableCell.Span := oPiece.Span;
    oTableCell.Width := oPiece.Width;
    oTableCell.Background := oPiece.Background;
    oTableCell.MarginLeft := oPiece.MarginLeft;
    oTableCell.MarginTop := oPiece.MarginTop;
    oTableCell.MarginRight := oPiece.MarginRight;
    oTableCell.MarginBottom := oPiece.MarginBottom;
    oTableCell.Background := oPiece.Background;
    oTableCell.ReadOnly := DocumentObjectReadOnlyForTriState[oPiece.ReadOnly];
    oTableCell.LeftBorder.Assign(oPiece.LeftBorder);
    oTableCell.TopBorder.Assign(oPiece.TopBorder);
    oTableCell.RightBorder.Assign(oPiece.RightBorder);
    oTableCell.BottomBorder.Assign(oPiece.BottomBorder);
    oTableCell.VerticalAlignment := oPiece.VerticalAlignment;

    oIter.Next;

    While oIter.More And (oIter.Current.PieceType <> ptCellStop) Do
      TranslateParagraph(oIter, oTableCell.Paragraphs);

    oCells.Add(oTableCell.Link);
  Finally
    oTableCell.Free;
  End;

  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateContent(oIter : TWPPieceIterator; oContents : TWPDocumentObjects);
Begin
  Case oIter.Current.PieceType Of
    ptText : TranslateText(oIter, oContents);
    ptImage : TranslateImage(oIter, oContents);
    ptFieldStart : TranslateField(oIter, oContents);
    ptLineBreak : TranslateLineBreak(oIter, oContents);
  Else
    // ptBreak, ptPara, ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop, ptFieldStop,
    RaiseError('TranslateContent', 'Illegal Piece Type '+NAMES_WPPIECETYPE[oIter.Current.PieceType]);
  End;
End;


Procedure TWPDocumentTranslator.TranslateLineBreak(oIter : TWPPieceIterator; oContents : TWPDocumentObjects);
Var
  oLineBreak : TWPDocumentLineBreak;
  oPiece : TWPWorkingDocumentLineBreakPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptLineBreak, 'TranslateLineBreak', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptLineBreak]])));
  oPiece := TWPWorkingDocumentLineBreakPiece(oIter.Current);

  oLineBreak := TWPDocumentLineBreak.Create;
  Try
    oLineBreak.Style := oPiece.Style;
    oLineBreak.Font.Assign(oPiece.Font);
    oLineBreak.Annotation := oPiece.AnnotationId;

    oContents.Add(oLineBreak.Link);
  Finally
    oLinebreak.Free;
  End;
  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateImage(oIter : TWPPieceIterator; oContents : TWPDocumentObjects);
Var
  oImage : TWPDocumentImage;
  oPiece : TWPWorkingDocumentImagePiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptImage, 'TranslateImage', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptImage]])));
  oPiece := TWPWorkingDocumentImagePiece(oIter.Current);

  oImage := TWPDocumentImage.Create;
  Try
    oImage.Hotspot := oPiece.Hotspot.Clone;
    oImage.Name := oPiece.Name;
    If oPiece.HasImage Then
      oImage.Image := oPiece.Image.Clone
    Else
      oImage.Image := Nil;
    If oPiece.HasSelectionImage Then
      oImage.SelectionImage := oPiece.SelectionImage.Clone
    Else
      oImage.SelectionImage := Nil;

    oImage.HasMap := oPiece.HasImageMap;
    If (oPiece.hasImageMap) Then
      oImage.Map.Assign(oPiece.ImageMap);
    oImage.Annotation := oPiece.AnnotationId;

    oImage.Adornments.Assign(oPiece.Adornments);
    oImage.BorderWidth := oPiece.Border;
    oImage.BorderColour := oPiece.BorderColour;
    oImage.TransparentColour := oPiece.TransparentColour;
    oImage.SizePolicy := oPiece.SizePolicy;
    oImage.ImageWidth := oPiece.Width;
    oImage.ImageHeight := oPiece.Height;
    oImage.VerticalAlignment := oPiece.VerticalAlignment;
    oImage.FrameIndex := oPiece.FrameIndex;

    oImage.Style := oPiece.Style;
    oImage.Font.Assign(oPiece.Font);

    oContents.Add(oImage.Link);
  Finally
    oImage.Free;
  End;
  oIter.Next;
End;


Function  TWPDocumentTranslator.CreateField(oField : TWPWorkingDocumentFieldStartPiece) : TWPDocumentField;
Begin
  Result := TWPDocumentField.Create;
  Try
    Result.Namespace := oField.Namespace;
    Result.Name := oField.Name;
    Result.ReadOnly := DocumentObjectReadOnlyForTriState[oField.ReadOnly];
    Result.Deletable := oField.Deletable;
    Result.RawData.Assign(oField.RawData);
    Result.FixedFormat := oField.FixedFormat;
    Result.CheckedIndex := oField.CheckedIndex;
    Result.Width := oField.Width;
    Result.Hotspot := oField.Hotspot.Clone;
    result.Annotation := oField.AnnotationId;

    Result.Style := oField.Style;
    Result.Font.Assign(oField.Font);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPDocumentTranslator.TranslateField(oIter : TWPPieceIterator; oContents : TWPDocumentObjects);
Var
  oField : TWPDocumentField;
  oPiece : TWPWorkingDocumentFieldStartPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptFieldStart, 'TranslateField', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptFieldStart]])));
  oPiece := TWPWorkingDocumentFieldStartPiece(oIter.Current);

  oField := CreateField(oPiece);
  Try
    oIter.Next;
    While oIter.More And (oIter.Current.PieceType <> ptFieldStop) Do
      TranslateContent(oIter, oField.Contents);

    If Not oIter.More Then
      RaiseError('TranslateField', 'Premature end of Field');

    oContents.Add(oField.Link);
  Finally
    oField.Free;
  End;
  oIter.Next;
End;


Procedure TWPDocumentTranslator.TranslateText(oIter : TWPPieceIterator; oContents : TWPDocumentObjects);
Var
  oText : TWPDocumentText;
  oPiece : TWPWorkingDocumentTextPiece;
  oFirstPiece : TWPWorkingDocumentTextPiece;
Begin
  Assert(CheckCondition(oIter.Current.PieceType = ptText, 'TranslateText', StringFormat('Unexpected Type %s looking for %s', [NAMES_WPPIECETYPE[oIter.Current.PieceType], NAMES_WPPIECETYPE[ptText]])));
  oPiece := TWPWorkingDocumentTextPiece(oIter.Current);
  oFirstPiece := oPiece;

  oText := TWPDocumentText.Create;
  Try
    oText.Value := oPiece.Content;
    oText.Style := oPiece.Style;
    oText.Font.Assign(oPiece.Font);
    oText.DrawnFont := oPiece.DrawnFont;
    oPiece.AnnotationId := oText.Annotation;

    oIter.Next;
    While oIter.More And (oIter.Current.PieceType = ptText) And TWPWorkingDocumentTextPiece(oIter.Current).StyleMatches(oFirstPiece) And (TWPWorkingDocumentTextPiece(oIter.Current).DrawnFont = oFirstPiece.DrawnFont) Do
    Begin
      oText.Value := oText.Value + TWPWorkingDocumentTextPiece(oIter.Current).Content;
      oIter.Next;
    End;
    oContents.Add(oText.Link);
  Finally
    oText.Free;
  End;
End;


function TWPDocumentTranslator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FWorkingDocument.sizeInBytes);
  inc(result, FWorkingStyles.sizeInBytes);
  inc(result, FRowMap.sizeInBytes);
  inc(result, FSplitter.sizeInBytes);
end;

{ TWPPieceIterator }


Function TWPPieceIterator.GetCurrent : TWPWorkingDocumentPiece;
Begin
  Assert(Invariants('GetCurrent', FCurrent, TWPWorkingDocumentPiece, 'Current'));
  Result := FCurrent;
End;

Function TWPPieceIterator.GetPeek : TWPWorkingDocumentPiece;
Begin
  If FCurrent = Nil Then
    Result := Nil
  Else
    Result := FCurrent.Next;
End;

Procedure TWPPieceIterator.First;
Begin
  If Document.GetPieceByPosition(WorkingMinimum, FCurrent, FStart, FCurrentIndex) Then
    Begin
    If Not (FCurrent.PieceType In FPieceTypes) Then
      Next
    Else If (FCurrent.Metrics.Position + FCurrent.Metrics.CharCount < WorkingMaximum) Then
      FStop := (FCurrent.Metrics.Position + FCurrent.Metrics.CharCount) - WorkingMaximum;
    End
  Else
    FCurrent := Nil;
End;

Procedure TWPPieceIterator.Next;
Begin
  If Assigned(FCurrent) Then
    Begin
    FStart := 0;
    Repeat
      Inc(FCurrentIndex);
      If FCurrentIndex < Document.Pieces.Count Then
        FCurrent := Document.Pieces[FCurrentIndex]
      Else
        FCurrent := Nil;
    Until Not Assigned(FCurrent) Or (FCurrent.Metrics.position > WorkingMaximum) Or (FCurrent.PieceType In FPieceTypes);
    If Assigned(FCurrent) Then
      Begin
      FStop := FCurrent.Metrics.CharCount;
      If FCurrent.Metrics.position >= WorkingMaximum Then
        FCurrent := Nil
      Else If (FCurrent.Metrics.Position + FCurrent.Metrics.CharCount < WorkingMaximum) Then
        FStop := (FCurrent.Metrics.Position + FCurrent.Metrics.CharCount) - WorkingMaximum;
      End;
    End;
End;

Procedure TWPPieceIterator.Prev;
Begin
  If Assigned(FCurrent) Then
    Begin
    FStop := 0;
    FStart := 0;
    Repeat
      Dec(FCurrentIndex);
      If FCurrentIndex >= 0 Then
        FCurrent := Document.Pieces[FCurrentIndex]
      Else
        FCurrent := Nil;
    Until Not Assigned(FCurrent) Or (FCurrent.Metrics.position + FCurrent.Metrics.CharCount < WorkingMinimum) Or (FCurrent.PieceType In FPieceTypes);
    If Assigned(FCurrent) Then
      If FCurrent.Metrics.position + FCurrent.Metrics.CharCount < WorkingMinimum Then
        FCurrent := Nil
      Else If (FCurrent.Metrics.Position < WorkingMinimum) Then
        FStart := WorkingMinimum - FCurrent.Metrics.Position;
    End;
End;

Procedure TWPPieceIterator.Last;
Begin
  If Document.GetPieceByPosition(WorkingMaximum, FCurrent, FStop, FCurrentIndex) Then
    Begin
    If Not (FCurrent.PieceType In FPieceTypes) Then
      Prev
    Else If (FCurrent.Metrics.Position < WorkingMinimum) Then
      FStart := WorkingMinimum - FCurrent.Metrics.Position;
    End
  Else
    FCurrent := Nil;
End;


Function TWPPieceIterator.More: Boolean;
Begin
  Result := Assigned(FCurrent);
End;

Function TWPPieceIterator.CurrentParagraph(bAllowNil : Boolean = False): TWPWorkingDocumentParaPiece;
Var
  iIndex : Integer;
Begin
  Result := Nil;
  iIndex := currentIndex;
  While (iIndex < Document.Pieces.Count) And (Document.Pieces[iIndex].PieceType <> ptPara) Do
    Inc(iIndex);
  If iIndex < Document.Pieces.Count Then
    Result := TWPWorkingDocumentParaPiece(Document.Pieces[iIndex])
  Else If Not bAllowNil Then
    RaiseError('GetCurrentParagraph', 'End of paragraph not found');
End;

function TWPPieceIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCurrent.sizeInBytes);
end;

{ TWPWorkingDocumentPieceTracker }

Destructor TWPWorkingDocumentPieceTracker.Destroy;
Begin
  FPiece.Free;
  Inherited;
End;

Procedure TWPWorkingDocumentPieceTracker.SetPiece(Const Value: TWPWorkingDocumentPiece);
Begin
  FPiece.Free;
  FPiece := Value;
End;

function TWPWorkingDocumentPieceTracker.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPiece.sizeInBytes);
end;

{ TWPWorkingDocumentPieceTrackers }

Function TWPWorkingDocumentPieceTrackers.CompareByOffset(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(TWPWorkingDocumentPieceTracker(pA).FOffset, TWPWorkingDocumentPieceTracker(pB).FOffset);
End;

Function TWPWorkingDocumentPieceTrackers.GetPiece(iIndex: Integer): TWPWorkingDocumentPieceTracker;
Begin
  Result := TWPWorkingDocumentPieceTracker(ItemByIndex[iIndex]);
End;

Function TWPWorkingDocumentPieceTrackers.ItemClass: TFslObjectClass;
Begin
  Result := TWPWorkingDocumentPieceTracker;
End;

Procedure TWPWorkingDocumentPieceTrackers.SortedByOffset;
Begin
  SortedBy(CompareByOffset);
End;

{ TWPWorkingAnnotation }

constructor TWPWorkingAnnotation.Create;
begin
  inherited;
  FDrawn := False;
  FAligned := False;
  FMeasured := False;
end;

function TWPWorkingAnnotation.Describe: String;
begin
  if self = nil then
    result := '(nil)'
  else
  begin
    if FDefinitionProvider <> nil then
      prop(result, 'owner', FDefinitionProvider.GetNamespace);
    prop(result, 'text', FText);
    prop(result, 'anchor', FAnchor);
  end;
end;

destructor TWPWorkingAnnotation.Destroy;
begin
  FDefinitionProvider.Free;
  inherited;
end;

function TWPWorkingAnnotation.Link: TWPWorkingAnnotation;
begin
  Result := TWPWorkingAnnotation(Inherited Link);
end;

function TWPWorkingAnnotation.Clone: TWPWorkingAnnotation;
begin
  Result := TWPWorkingAnnotation(Inherited Clone);
end;


procedure TWPWorkingAnnotation.SetText(const Value: String);
begin
  if Length(Value) > 1024 Then
    RaiseError('SetText', 'Annotation is too long (max 1024 characters)');
  FText := Value;
  FWorkingText := '';
  FDrawn := False;
  FAligned := False;
  FMeasured := False;
end;

procedure TWPWorkingAnnotation.SetAnchor(const Value: Integer);
begin
  If FAnchor <> Value Then
  Begin
    FAnchor := Value;
    FAligned := False;
    FDrawn := False;
  End;
end;

procedure TWPWorkingAnnotation.SetBottom(const Value: Integer);
begin
  If FBottom <> Value Then
  Begin
    FBottom := Value;
    FDrawn := False;
  End;
end;

procedure TWPWorkingAnnotation.SetTop(const Value: Integer);
begin
  If FTop <> Value Then
  Begin
    FTop := Value;
    FDrawn := False;
  End;
end;

procedure TWPWorkingAnnotation.SetLineBreaks(const Value: TLineBreaks);
begin
  FLineBreaks := Value;
  FAligned := False;
  FDrawn := False;
end;

procedure TWPWorkingAnnotation.SetColour(const Value: TColour);
begin
  FColour := Value;
  FDrawn := False;
end;

function TWPWorkingAnnotation.Height: Integer;
begin
  result := Bottom - Top;
end;

procedure TWPWorkingAnnotation.SetSelected(const Value: Boolean);
begin
  If (FSelected <> Value) Then
  Begin
    FSelected := Value;
    FDrawn := False;
  End;
end;

procedure TWPWorkingAnnotation.Assign(oSource: TFslObject);
begin
  inherited;
  FOwner := TWPWorkingAnnotation(oSource).FOwner;
  FText := TWPWorkingAnnotation(oSource).FText;
  FColour := TWPWorkingAnnotation(oSource).FColour;

  FDrawn := False;
  FAligned := False;
  FMeasured := False;
end;

function TWPWorkingAnnotation.Matches(oOther: TWPWorkingAnnotation): Boolean;
begin
  result := (FText = oOther.FText) And (FColour = oOther.FColour);
end;


procedure TWPWorkingAnnotation.SetDefinitionProvider(const Value: TWPAnnotationDefinitionProvider);
begin
  FDefinitionProvider.Free;
  FDefinitionProvider := Value;
end;

function TWPWorkingAnnotation.GetWorkingText: String;
begin
  if (FWorkingText = '') And (FDefinitionProvider <> nil) Then
    FWorkingText := FDefinitionProvider.GetDisplay(FText);
  result := FWorkingText;
end;

function TWPWorkingAnnotation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinitionProvider.sizeInBytes);
  inc(result, (FOwner.length * sizeof(char)) + 12);
  inc(result, (FText.length * sizeof(char)) + 12);
  inc(result, (FWorkingText.length * sizeof(char)) + 12);
end;

{ TWPWorkingAnnotationList }


function TWPWorkingAnnotationList.GetAnnotation(iIndex: Integer): TWPWorkingAnnotation;
begin
  Result := TWPWorkingAnnotation(Inherited ObjectByIndex[iIndex]);
end;

function TWPWorkingAnnotationList.ItemClass: TFslObjectClass;
begin
  Result := TWPWorkingAnnotation;
end;

function TWPWorkingAnnotationList.Link: TWPWorkingAnnotationList;
begin
  Result := TWPWorkingAnnotationList(Inherited Link);
end;

Function TWPWorkingAnnotationList.Select(iStart, iStop: Integer) : Boolean;
Var
  iLoop : Integer;
  oAnnotation : TWPWorkingAnnotation;
  bSelect : Boolean;
begin
  Result := False;
  For iLoop := 0 To Count - 1 Do
  Begin
    oAnnotation := Annotation[iLoop];
    bSelect := oAnnotation.InUse and (iStart >= oAnnotation.OffsetStart) And (iStop <= oAnnotation.OffsetEnd);
    Result := Result Or (oAnnotation.Selected <> bSelect);
    oAnnotation.Selected := bSelect;
  End;
end;

Function TWPWorkingAnnotationList.GetBySelection(iStart, iStop: Integer) : TWPWorkingAnnotation;
Var
  iLoop : Integer;
  oAnnotation : TWPWorkingAnnotation;
begin
  Result := Nil;
  For iLoop := 0 To Count - 1 Do
  Begin
    oAnnotation := Annotation[iLoop];
    if oAnnotation.InUse and (iStart >= oAnnotation.OffsetStart) And (iStop <= oAnnotation.OffsetEnd) Then
      result := oAnnotation;
  End;
end;



Function TWPWorkingAnnotationList.Add(sOwner, sValue: String) : TWPWorkingAnnotation;
begin
  result := TWPWorkingAnnotation.Create;
  Try
    result.Owner := sOwner;
    result.Text := sValue;
    Add(result.Link);
  Finally
    result.Free;
  End;
end;

function TWPMapContainer.InnerHeight: Integer;
begin
  Result := InnerBottom - InnerTop;
end;

function TWPMapContainer.FirstPopulatedRow: TWPMapRow;
var
  i : integer;
begin
  result := nil;
  i := 0;
  while (result = nil) and (i < Rows.Count) do
  begin
    if (Rows[i].Items.Count > 0) then
      result := Rows[i];
    inc(i);
  end;
end;

{ TWPWorkingAttachmentList }

function TWPWorkingAttachmentList.CompareById(p1, p2: pointer): Integer;
begin
  result := StringCompare(TWPWorkingAttachment(p1).FId, TWPWorkingAttachment(p2).FId);
end;

function TWPWorkingAttachmentList.GetById(id: String): TWPWorkingAttachment;
var
  oAttachment : TWPWorkingAttachment;
  iIndex : integer;
begin
  oAttachment := TWPWorkingAttachment.Create;
  try
    oAttachment.FId := id;
    if Find(oAttachment, iIndex, CompareById) then
      result := Item[iIndex]
    else
      result := nil;
  finally
    oAttachment.Free;
  end;
end;

function TWPWorkingAttachmentList.GetItemA(index: integer): TWPWorkingAttachment;
begin
  result := TWPWorkingAttachment(ObjectByIndex[index]);
end;

function TWPWorkingAttachmentList.ItemClass: TFslObjectClass;
begin
  result := TWPWorkingAttachment;
end;


{ TWPWorkingAttachment }

procedure TWPWorkingAttachment.Assign(oSource: TFslObject);
begin
  inherited;
  FId := TWPWorkingAttachment(oSource).FId;
  FContent.assign(TWPWorkingAttachment(oSource).FContent);
  Loaded;
end;

function TWPWorkingAttachment.Clone: TWPWorkingAttachment;
begin
  result := TWPWorkingAttachment(Inherited Clone);
end;

constructor TWPWorkingAttachment.Create;
begin
  inherited;
  FContent := TFslBuffer.create;
  FExtension := '.bin';
  FMimeType := 'application/binary';
end;

destructor TWPWorkingAttachment.Destroy;
begin
  FContent.Free;
//  FPDF.Free;
  inherited;
end;


function TWPWorkingAttachment.Link: TWPWorkingAttachment;
begin
  result := TWPWorkingAttachment(Inherited Link);
end;

procedure TWPWorkingAttachment.Loaded;
begin
end;

procedure TWPWorkingAttachment.LoadFromBuffer(oBuffer: TFslBuffer);
begin
  FContent.Assign(oBuffer);
  Loaded;
end;

procedure TWPWorkingAttachment.LoadFromFile(Const sFilename : String);
begin
  FContent.LoadFromFileName(sFilename);
  Loaded;
end;

procedure TWPWorkingAttachment.LoadFromStream(oStream: TFslStream);
begin
  FContent.LoadFromStream(oStream);
  Loaded;
end;

procedure TWPWorkingAttachment.LoadFromString(sSource: AnsiString);
begin
  FContent.AsAscii := sSource;
  Loaded;
end;


procedure TWPWorkingAttachment.SaveToFile(const sFilename: String);
begin
  FContent.SaveToFileName(sFilename);
end;

procedure TWPWorkingAttachment.SaveToStream(oStream: TFslStream);
begin
  FContent.SaveToStream(oStream);
end;


function TWPWorkingAttachment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, FContent.sizeInBytes);
  inc(result, (FMimeType.length * sizeof(char)) + 12);
  inc(result, (FExtension.length * sizeof(char)) + 12);
end;

function TWPDocumentTranslator.TranslateAttachment(oSource: TWPWorkingAttachment): TWPDocumentAttachment;
begin
  result := TWPDocumentAttachment.create;
  try
    result.id := oSource.id;
    result.MimeType := oSource.MimeType;
    result.Extension := oSource.Extension;
    result.Content.assign(oSource.Content);
    result.link;
  finally
    result.free;
  end;
end;

function TWPDocumentTranslator.TranslateAttachment(oSource: TWPDocumentAttachment): TWPWorkingAttachment;
begin
  result := TWPWorkingAttachment.create;
  try
    result.id := oSource.id;
    result.MimeType := oSource.MimeType;
    result.Extension := oSource.Extension;
    result.Content.assign(oSource.Content);
    result.link;
  finally
    result.free;
  end;
end;

procedure TWPWorkingAttachment.UseAsPdf;
var
  mem : TMemoryStream;
begin
  Extension := '.pdf';
  MimeType := 'application/pdf';
//  FPDF.Free;
//  FPDF := TgtExProPDFDocument.create(nil);
//  mem := TMemoryStream.Create;
//  try
//    mem.Write(Content.Data^, Content.Capacity);
//    mem.Position := 0;
//    FPDF.LoadFromStream(mem);
//  finally
//    mem.Free;
//  end;
end;


Constructor TWPWorkingDocumentValidator.Create(oDocument : TWPWorkingDocument);
Begin
  Create;
  FDocument := oDocument;
End;


Destructor TWPWorkingDocumentValidator.Destroy;
Begin
  FDocument.Free;
  Inherited;
End;


function TWPWorkingDocumentValidator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
end;

Class Procedure TWPWorkingDocumentValidator.Validate(oDocument : TWPWorkingDocument);
Var
  oThis : TWPWorkingDocumentValidator;
Begin
  oThis := TWPWorkingDocumentValidator.Create(oDocument.Link);
  Try
    oThis.Validate;
  Finally
    oThis.Free;
  End;
End;

Function TWPWorkingDocumentValidator.GetDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Procedure TWPWorkingDocumentValidator.SetDocument(Const Value : TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;


Function TWPWorkingDocumentValidator.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Procedure TWPWorkingDocumentValidator.Validate;
Var
  oIterator : TWPPieceIterator;
Begin
  If HasDocument Then
  Begin
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.First;
      Validate(oIterator,  [ptText, ptImage, ptFieldStart, ptLineBreak, ptBreak, ptPara, ptTableStart, ptSectionStart]);
    Finally
      oIterator.Free;
    End;
  End;
End;


Procedure TWPWorkingDocumentValidator.Validate(oIterator : TWPPieceIterator; aAllowed : TWPWorkingDocumentPieceTypes);
Var
  aType : TWPWorkingDocumentPieceType;
  iPos : Integer;
Begin
  While oIterator.More Do
  Begin
    aType := oIterator.Current.PieceType;
    iPos := oIterator.Current.Metrics.Position;

    oIterator.Next;
    If aType In aAllowed Then
    Begin
      Case aType Of
        ptText, ptImage, ptLineBreak : Validate(oIterator, [ptText, ptImage, ptFieldStart, ptLineBreak], ptPara);
        ptFieldStart : Validate(oIterator, [ptText, ptImage, ptLineBreak], ptFieldStop);
        ptBreak : ; // no implications
        ptPara : ; // no implications
        ptTableStart : Validate(oIterator, [ptRowStart], ptTableStop);
        ptRowStart : Validate(oIterator, [ptCellStart], ptRowStop);
        ptCellStart :  Validate(oIterator,  [ptText, ptImage, ptFieldStart, ptLineBreak, ptBreak, ptPara], ptCellStop);
        ptSectionStart : Validate(oIterator,  [ptText, ptImage, ptFieldStart, ptLineBreak, ptBreak, ptPara, ptTableStart, ptSectionStart], ptSectionStop);
      Else
        RaiseError('Validate', 'Internal Error validating the document ('+NAMES_WPPIECETYPE[oIterator.Current.PieceType]+') '+IntegerToString(iPos));
      End;
    End
    Else
      RaiseError('Validate', 'The Document contains an unexpected and illegal piece type '+NAMES_WPPIECETYPE[aType]+' at position '+IntegerToString(iPos));
  End;
End;


Procedure TWPWorkingDocumentValidator.Validate(oIterator : TWPPieceIterator; aAllowed : TWPWorkingDocumentPieceTypes; aTerminate : TWPWorkingDocumentPieceType);
Var
  aType : TWPWorkingDocumentPieceType;
  iPos : Integer;
  bFound : Boolean;
Begin
  bFound := False;
  While oIterator.More And Not bFound Do
  Begin
    aType := oIterator.Current.PieceType;
    iPos := oIterator.Current.Metrics.Position;

    oIterator.Next;
    If aType = aTerminate Then
      bFound := True
    Else If aType In aAllowed Then
    Begin
      Case aType Of
        ptText, ptImage, ptLineBreak : If Not (aTerminate In [ptPara, ptFieldStop]) Then
          Validate(oIterator, [ptText, ptImage, ptFieldStart, ptLineBreak], ptPara);
        ptFieldStart : Validate(oIterator, [ptText, ptImage, ptLineBreak], ptFieldStop);
        ptBreak : ; // no implications
        ptPara : ; // no implications
        ptTableStart : Validate(oIterator, [ptRowStart], ptTableStop);
        ptRowStart : Validate(oIterator, [ptCellStart], ptRowStop);
        ptCellStart :  Validate(oIterator,  [ptText, ptImage, ptFieldStart, ptLineBreak, ptBreak, ptPara], ptCellStop);
        ptSectionStart : Validate(oIterator,  [ptText, ptImage, ptFieldStart, ptLineBreak, ptBreak, ptPara, ptTableStart, ptSectionStart], ptSectionStop);
      Else
        RaiseError('Validate', 'Internal Error validating the document ('+NAMES_WPPIECETYPE[aType]+') '+IntegerToString(iPos));
      End;
    End
    Else
      RaiseError('Validate', 'The Document contains an unexpected and illegal piece type '+NAMES_WPPIECETYPE[aType]+' at position '+IntegerToString(iPos)+' (looking for '+NAMES_WPPIECETYPE[aTerminate]+')');
  End;

  If Not bFound Then
      RaiseError('Validate', 'The Document ended unexpectedly looking for a '+NAMES_WPPIECETYPE[aTerminate]);
End;


End.
