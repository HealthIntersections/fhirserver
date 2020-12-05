Unit FHIR.WP.Renderer;

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


Interface


Uses
  Windows, SysUtils, Classes, Vcl.Graphics, Vcl.Imaging.PNGImage, System.UITypes,

  fsl_base, fsl_utilities, fsl_threads, fsl_collections,

  wp_graphics, wp_printing_win,
  wp_types, wp_working, FHIR.WP.Icons, FHIR.WP.Engine, FHIR.WP.Settings, wp_definers;

Const
  CHAR_BULLETS : Array [TWPSParagraphBulletType] Of Char = ('l', 'l', #161, 'n');
  FIELD_DROPPER_WIDTH = 10;
  FIELD_DROPPER_UP = 2;
  FIELD_DROPPER_DOWN = 1;
  FIELD_DROPPER_COLOUR = clGray;

  CHECK_IMAGE = 12;
  CHECK_PAD_LEFT = 4;
  CHECK_PAD_MID = 4;
  CHECK_PAD_RIGHT_INNER = 4;
  CHECK_PAD_RIGHT_OUTER = 4;
  CHECK_ROUND_EDGE = 4;
  CHECK_TOTAL_LEFT = CHECK_IMAGE + CHECK_PAD_LEFT + CHECK_PAD_MID;
  CHECK_TOTAL = CHECK_TOTAL_LEFT + CHECK_PAD_RIGHT_INNER + CHECK_PAD_RIGHT_OUTER;


Type
  TWPRendererState = Class(TFslObject)
    Private
      FBackground : TColour;
      FReadOnly : TWPSTriState;
      FForeHotspot : TWPHotspot;
      FBackHotspot : TWPHotspot;
      Procedure SetForeHotspot(Const Value: TWPHotspot);
      Procedure SetBackHotspot(Const Value: TWPHotspot);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRendererState; Overload;
      Function Clone : TWPRendererState; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Background : TColour Read FBackground Write FBackground;
      Property ReadOnly : TWPSTriState Read FReadOnly Write FReadOnly;
      Property ForeHotspot : TWPHotspot Read FForeHotspot Write SetForeHotspot;
      Property BackHotspot : TWPHotspot Read FBackHotspot Write SetBackHotspot;
  End;

  TWPRendererStates = Class(TFslObjectList)
    Private
      FSettings : TWPSettings;
      FReadOnlyColour : TColour;

      Function GetState(Const iIndex : TColour) : TWPRendererState;
      Procedure SetState(Const iIndex : TColour; Const oValue : TWPRendererState);

      Procedure SetSettings(oSettings : TWPSettings);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByBackground(pA, pB : Pointer) : TColour; Overload; Virtual;
      Function CompareByReadOnly(pA, pB : Pointer) : TColour; Overload; Virtual;

      Function Get(Const aValue : TColour) : TWPRendererState; Reintroduce; Overload; Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Function Link : TWPRendererStates; Overload;
      Function Clone : TWPRendererStates; Overload;

      Function New : TWPRendererState; Reintroduce; Overload; Virtual;

      Function IndexByBackground(Const aValue : TColour) : TColour; Overload; Virtual;
      Function IndexByReadOnly(Const aValue : TWPSTriState) : TColour; Overload; Virtual;

      Function GetByBackground(Const aValue : TColour) : TWPRendererState; Overload; Virtual;
      Function GetByReadOnly(Const aValue : TWPSTriState) : TWPRendererState; Overload; Virtual;

      Function ExistsByBackground(Const aValue : TColour) : Boolean; Overload; Virtual;
      Function ExistsByReadOnly(Const aValue : TWPSTriState) : Boolean; Overload; Virtual;

      Procedure SortedByBackground; Overload; Virtual;
      Procedure SortedByReadOnly; Overload; Virtual;

      Function IsSortedByBackground : Boolean; Overload; Virtual;
      Function IsSortedByReadOnly : Boolean; Overload; Virtual;

      Procedure Initialise(oSettings : TWPSettings; aReadOnly : TWPSTriState; aReadOnlyColour : TColour); Overload; Virtual;
      Procedure Add(aColour : TColour; aReadOnly : TWPSTriState; oForeHotspot, oBackHotspot : TWPHotspot); Overload; Virtual;

      Property State[Const iIndex : TColour] : TWPRendererState Read GetState Write SetState; Default;

      Function WorkingBackground : TColour;
      Function WorkingForeHotspot : TWPHotspot;
      Function WorkingBackHotspot : TWPHotspot;
      Function WorkingState : TWPSTriState;

      Function PreviousWorkingState : TWPSTriState;
      Procedure PushState(oField : TWPWorkingDocumentFieldStartPiece; aSpecifiedBackground : TColour = DEF_COLOUR); Overload;
      Procedure PushState(oContainer : TWPWorkingDocumentContainerPiece; aBackground : TColour); Overload;
      Procedure PopState;

      Procedure SetCurrentState(oPiece : TWPWorkingDocumentContainerPiece); Overload;
      Procedure SetCurrentState(oRow : TWPMapRow); Overload;
      Procedure SetCurrentState(oPiece : TWPWorkingDocumentPiece; oItem : TWPMapItem); Overload;

      Property Settings : TWPSettings Read FSettings Write SetSettings;
      Property ReadOnlyColour : TColour Read FReadOnlyColour Write FReadOnlyColour;
  End;


Const
  TWPRENDERERSTATE_Background_FIELD = 'Background';
  TWPRENDERERSTATE_FOREHOTSPOT_FIELD = 'forehotspot';
  TWPRENDERERSTATE_BACKHOTSPOT_FIELD = 'backhotspot';
  TWPRENDERERSTATE_ReadOnly_FIELD = 'ReadOnly';

Const
  SCREEN_DPI = 96;

Type
  TSize = windows.TSize;
  TTextMetric = Windows.TTextMetric;
  TCopyMode = Vcl.Graphics.TCopyMode;

  TWPCanvas = Class (TFslObject)
    Private
      FFont : TFslFont;
      FPointSizeX : Integer;
      FPointSizeY : Integer;

      Function GetFont : TFslFont;
      Procedure SetFont(oFont : TFslFont);
    Protected
      Procedure FontChange(oSender : TObject);  Overload; Virtual;
      Function PointSize : Integer; Overload; Virtual;

      Function MakePenHandle(oPen : TPen; aEndStyle : TFslPenEndStyle; aJoinStyle : TFslPenJoinStyle): HPEN;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure MoveVertical(iTop, iOffset : Integer); Overload; Virtual;
      Procedure Clip(Const ARect : TRect); Overload; Virtual;
      Procedure Clip(oObject : TWPMapObject); Overload; Virtual;
      Procedure UnClip; Overload; Virtual;

      Procedure DrawRect(aColour : TColour; iLeft, iTop, iRight, iBottom : Integer); Overload; Virtual;
      Procedure DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer); Overload; Virtual;
      Procedure DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; iX1, iY1, iX2, iY2 : Integer); Overload; // Virtual;
      Procedure DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer); Overload; Virtual;
      Procedure DrawPolyLine(aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iXOffset, iYOffset : Integer; oCoords : TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = True); Overload; Virtual;
      Procedure DrawText(aBackground : TColour; iLeft, iTop : Integer; Const sText : String); Overload; Virtual;
      Procedure DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer); Overload; Virtual;
      Procedure DrawBorder(aColour : TColour; iLeft, iTop, iRight, iBottom, iLeftMargin, iTopMargin, iRightMargin, iBottomMargin : Integer);  Overload; Virtual;
      Procedure DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer); Overload; Virtual;
      Procedure DrawRoundOutline(aColour : TColour; iLeft, iTop, iRight, iBottom, iRadius : Integer); Overload; Virtual;
      Procedure DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic;
                                iX, iY : Integer; // this is the point the curve rotates about
                                iInnerRadius, iOuterRadius : Integer; // the inner and outer diameters
                                iRotation : Integer); Overload; Virtual; // the rotation in degrees
      Procedure DrawTriangle(aColour : TColour; iX1, iY1, iX2, iY2, iX3, iY3: Integer); Overload; Virtual;

      Function DPIX : Integer; Overload; Virtual;
      Function DPIY : Integer; Overload; Virtual;

      Function TextExtent(Const sText : String) : TSize; Overload; Virtual;
      Function GetTextMetrics : TTextMetric; Overload; Virtual;
      Function TextHeight(Const sText : String) : Integer; Overload; Virtual;
      Function TextWidth(Const sText : String) : Integer; Overload; Virtual;
      Procedure GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers); Overload; Virtual;

      Property Font : TFslFont Read GetFont Write SetFont;
      Property PointSizeX : Integer Read FPointSizeX Write FPointSizeX;
      Property PointSizeY : Integer Read FPointSizeY Write FPointSizeY;

      Function ColourLighten(iColour : TColour; Const iRatio : Real) : TColour;
      Function ColourDarken(iColour : TColour; Const iRatio : Real) : TColour;
//      Function ColourReduceContrast(iColour : TColour; Const iRatio : Real) : TColour;
//      Function ColourIncreaseContrast(iColour : TColour; Const iRatio : Real) : TColour;
  End;

Type
  TWPRendererParagraphContextItemType = (itText, itWhitespace, itBreak);

  TWPRendererParagraphContext = Class (TFslObject)
  Private
    FContainer: TWPMapContainer;
    FStateStack : TWPRendererStates;
    FBuffer : TWPMapItems;
    FRow : TWPMapRow;
    FRowWidth: Integer;
    FBufferWidth: Integer;
    Procedure SetContainer(Const Value: TWPMapContainer);
    Procedure AddToBuffer(oItem : TWPMapItem);
    Procedure AddBufferToRow;
    Procedure StartNewLine;
    Procedure AddPiecePartToRow(oItem : TWPMapItem; oPiece : TWPWorkingDocumentPiece; Var iCursor : Integer);
    Function GetContainer : TWPMapContainer;
    procedure SetStateStack(const Value: TWPRendererStates);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oContainer : TWPMapContainer; oStateStack : TWPRendererStates); Overload; Virtual;
    destructor Destroy; Override;

    Procedure AddItem(oItem : TWPMapItem); Overload; Virtual;

    Property Container : TWPMapContainer Read GetContainer Write SetContainer;
    Property StateStack : TWPRendererStates Read FStateStack Write SetStateStack;
    Procedure Finish;
  End;

  TWPPaginations = Array Of Integer;

  TWPRenderState = Record
    Completed : Boolean;
    VerticalOffset : Integer;
    LineOffset : Integer;
  End;

  TWPBounds = Record
    Left : Integer;
    Top : Integer;
    Right : Integer;
  End;

  TWPErrorEvent = Procedure (Const sContext, sMessage : String) Of Object;

  TWPRenderer = Class (TWPSettable)
  Private
    FDocument: TWPWorkingDocument;
    FOnError : TWPErrorEvent;
    FSelection: TWPSelection;

    FSomeSelected : Boolean;
    FWidth : Integer;
    FOnSetDocumentHeight: TNotifyEvent;

    FMap : TWPMapContainer;
    FSectionHeaderHeight : Integer;
    FSectionFieldCharWidth : Integer;
    FSectionFieldCharHeight : Integer;
    FIndentWidth : Integer;
    FStyles: TWPStyles;
    FCanvas : TWPCanvas;

    FFieldFontSize : TSize;
    FFieldDescent : Integer;
    FStateStack : TWPRendererStates;

    FMeasureAll : Boolean;
    FUpdating : Boolean;
    FPaintAll : Boolean;
    FValid : Boolean;
    FWorking : Boolean;

    FAdjustTop : Integer;
    FAdjustOffset : Integer;
    FDefaultTableBorder : TWPBorder;
    FCurrentHotspot : TWPHotspot;
    FCurrentButton : TFslObject;
    FAltKeyDown: Boolean;
    FNominalPageHeight : Integer;

    // administration
    Procedure SetDocument(Const Value: TWPWorkingDocument);
    Procedure SetSelection(Const Value: TWPSelection);
    Procedure SetStyles(Const Value: TWPStyles);
    Function GetHeight: Integer;
    Function GetCanvas : TWPCanvas;
    Procedure SetCanvas(Const Value : TWPCanvas);
    Procedure SetWorking(Const Value : Boolean);
    Function GetDocument : TWPWorkingDocument;
    Function GetSelection : TWPSelection;
    Function GetStyles : TWPStyles;
    Procedure SetMeasureAll(Const Value : Boolean);
    Procedure SetCurrentHotspot(Const Value: TWPHotspot);
    Procedure SetCurrentButton(Const Value: TFslObject);
    Procedure NeedPaintByHotspot(Const Value: TWPHotspot);

    // FMap management
    Procedure SetWidth(Const Value: Integer);

    Function ProduceStartingSectionMap(oSection: TWPWorkingDocumentSectionStartPiece; aBackground : TColour) : TWPMapItem;
    Function ProduceClosingSectionMap(oSection: TWPWorkingDocumentSectionStartPiece; aBackground : TColour) : TWPMapItem;


    // base measurement
    Procedure MeasureImage(oImage : TWPWorkingDocumentImagePiece);
    Procedure MeasurePiece(oPiece : TWPWorkingDocumentPiece; Const sText : String); Overload;
    Procedure MeasurePiece(oPiece : TWPWorkingDocumentPiece; bAll : Boolean; Var oCell : TWPWorkingDocumentTableCellStartPiece; Var oField : TWPWorkingDocumentFieldStartPiece; Var iFieldWidth : Integer); Overload;
    Procedure MeasureSection(oSection : TWPWorkingDocumentSectionStartPiece);

    // layout utilities
    Procedure ApplySectionHeaderFont;
    Procedure ApplyFieldFont;
    Function GetSectionHeaderHeight : Integer;
    Function GetFieldCharWidth: Integer;
    Function GetFieldCharHeight: Integer;
    Function ApplyFont(oPiece : TWPWorkingDocumentPiece; oStyle : TWPStyle; bAllowDecorations : Boolean = True) : TWPSFontState; Overload;
    Function ApplyFont(oPiece : TWPWorkingDocumentPiece; oStyle : TWPStyle; aColour : TColour; bAllowDecorations : Boolean = True) : TWPSFontState; Overload;
    Function GetBulletWidth(oParagraph : TWPWorkingDocumentParaPiece; oStyle : TWPStyle) : Integer;
    Function GetNumberWidth(oParagraph : TWPWorkingDocumentParaPiece; oStyle : TWPStyle) : Integer;
    Function GetNumberFormat(oParagraph : TWPWorkingDocumentParaPiece; oStyle : TWPStyle; iValue : Integer) : String;
    Function GetBulletChar(oFormat : TWPSParagraphDetails; oStyle : TWPStyle) : Char;
    Function CurrentParagraph(oCurrent : TWPWorkingDocumentPiece) : TWPWorkingDocumentParaPiece;
    Procedure CheckParagraphBounds(oParagraph : TWPWorkingDocumentParaPiece);

    Procedure AddToContainer(oContainer, oAdded : TWPMapContainer; Var iContainerCursor : Integer);
    Procedure TrimContainer(oContainer : TWPMapContainer; iContainerCursor : Integer);

    Procedure AdjustHeightOfChildren(oContainer : TWPMapContainer; iStart, iOffset, iLineOffset : Integer);
    Procedure MarkVerticalAdjustment(iTop, iOffset : Integer);

    // paragraph layout
    Procedure CalculateParagraphMargins(oParagraph : TWPWorkingDocumentParaPiece; Var aBounds : TWPBounds);
    Procedure AddPieceToPara(oPiece : TWPWorkingDocumentPiece; oContext : TWPRendererParagraphContext; iNose, iTail : Integer);
    Procedure LayoutRow(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; bLast : Boolean; oStyle : TWPStyle);
    Function AlignRowLeft(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
    Function AlignRowMiddle(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
    Function AlignRowRight(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
    Function AlignRowJustified(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
    Procedure AdjustHeights(oRow : TWPMapRow);
    Function LayoutPara(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; oOperationRange : TOperationRange; iColumnOffset : Integer; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;

    Function LayoutSection(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; oOperationRange : TOperationRange; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer) : Integer;
    Function LayoutTableCell(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oMetrics : TWPTableColumnMetrics; oContainer : TWPMapContainer; oCells : TWPWorkingDocumentTableCellStartPieces; iColumnOffset : Integer; Var iMetricIndex : Integer; Var iBottom : Integer; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;
    Function LayoutTableRow(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oMetrics : TWPTableColumnMetrics; oContainer : TWPMapContainer; Var iLine : Integer) : Integer;
    Function LayoutTable(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; Var iContainerCursor : Integer; Var iLine : Integer) : Integer;
    Function LayoutBreak(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; iColumnOffset : Integer; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;

    Function RowForItem(oMap : TWPMapItem) : TWPMapRow;
    // paint utilities
    Procedure PaintBullet(oContainer : TWPMapContainer; oParagraph : TWPWorkingDocumentParaPiece; oStyle : TWPStyle);
    Procedure PaintNumber(oContainer : TWPMapContainer; oParagraph : TWPWorkingDocumentParaPiece; oStyle : TWPStyle);
    Procedure PaintWrongSpelling(bSelected : Boolean; aColour : TColour; iY, iLeft, iRight : Integer);
    Procedure PaintPieceBackground(oMap : TWPMapItem; oRow : TWPMapRow; oPiece : TWPWorkingDocumentPiece; oStyle : TWPStyle; bParentBackground : Boolean);
    Procedure PaintTextItem(oPiece: TWPWorkingDocumentPiece; oItem: TWPMapItem; sText: String; aFontState : TWPSFontState; aCapsState : TWPSCapsState; oStyle : TWPStyle; bParentBackground, bUnderlineHotspot : Boolean);
    Procedure PaintFieldHint(oPiece: TWPWorkingDocumentPiece; oItem: TWPMapItem; Const bStart : Boolean; aFontState : TWPSFontState; oStyle : TWPStyle; bParentBackground, bUnderlineHotspot : Boolean);
    Procedure PaintFieldCheckables(oItem : TWPMapItem; oStyle : TWPStyle; oPiece: TWPWorkingDocumentFieldStartPiece);
    Procedure PaintButton(iLeft, iRight : Integer; oItem : TWPMapItem; Const sText : String; bChecked : Boolean);

    // background and readonly state management
    Function ReadOnlyColour : TColour;

    Procedure DrawVerticalBorder(oPiece : TWPWorkingDocumentTableCellStartPiece; oBorder : TWPBorder; iX, iY1, iY2 : Integer; bLeft : Boolean; bShared : Boolean);
    Procedure DrawHorizontalBorder(oPiece : TWPWorkingDocumentTableCellStartPiece; oBorder : TWPBorder; iX1, iY, iX2 : Integer; bTop : Boolean; bShared : Boolean; aState : TWPWorkingDocumentTableItemState);
    Procedure DrawDefaultBorders(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);
    Procedure DrawBorders(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);

//    Function PointSize : Integer;
    Function GetMap : TWPMapContainer;
    Procedure NeedPaintByButton(Const oButton: TFslObject);
    procedure SetAltKeyDown(const Value: Boolean);

  Protected
    Procedure DoUpdate; Overload; Virtual; // called when image needs drawing
    Function WantFastDrawing : Boolean; Virtual;

    // metrics
    Procedure BuildMetrics; Virtual;

    Procedure MeasurePieces(bAll : Boolean = False); Overload; Virtual;
    Procedure InitialiseMap;
    Procedure LayoutDocument(oDocument : TWPWorkingDocument; oOperationRange : TOperationRange);
    Procedure ApplySelectionToDocument;

    Procedure ClipContents(oRow : TWPMapRow; oClip : TWPMapContainer); Overload;
    Procedure ClipContents(oContainer : TWPMapContainer; oClip : TWPMapContainer = Nil); Overload;

    Procedure ApplyVerticalAdjustment;

    // painting routines
    Procedure PaintText(oMap : TWPMapItem; oPiece : TWPWorkingDocumentTextPiece; bUnderlineHotspot : Boolean);
    Procedure PaintImage(oMap : TWPMapItem; oImage : TWPWorkingDocumentImagePiece);
    Procedure PaintFieldStart(oMap : TWPMapItem; oPiece : TWPWorkingDocumentFieldStartPiece);
    Procedure PaintFieldEnd(oMap : TWPMapItem; oPiece : TWPWorkingDocumentFieldStopPiece);
    Procedure PaintLineBreak(oMap : TWPMapItem; oPiece : TWPWorkingDocumentLineBreakPiece);
    Procedure PaintRowEdge(oRow : TWPMapRow);
    Procedure PaintRowEdges(oContainer : TWPMapContainer);
    Procedure PaintParaMap(oMap : TWPMapItem; oPiece : TWPWorkingDocumentParaPiece);
    Procedure PaintParaContainer(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentParaPiece);
    Procedure PaintTableCell(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);
    Procedure PaintTableRow(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableRowStartPiece);
    Procedure PaintTable(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableStartPiece);
    Procedure PaintSection(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentSectionStartPiece);
    Procedure PaintBreak(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentBreakPiece);
    Procedure PaintAnnotationDecoration(oMap : TWPMapItem; oRow : TWPMapRow; oPiece : TWPWorkingDocumentPiece);

    Function Printing : Boolean; Overload; Virtual;
    Function ApplyOutputColourRules(bBackground : Boolean; aColour : TColour) : TColour; Overload; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPRenderer; Overload;
    Function Clone : TWPRenderer; Overload;

    Procedure Clear; Overload; Virtual;
    Procedure Update; Overload; Virtual;
    Property Height : Integer Read GetHeight;

    Function HasDocument : Boolean;

    Procedure ChangeSettings; Overload; Virtual;
    Function SpaceForAnnotations : Integer;
    Function ShowHotspots(draw : boolean) : Boolean;

    // there is a storm of updates while things are being set up. Working prevents needless rendering
    Property Working : Boolean Read FWorking Write SetWorking;
    Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
    Property Selection : TWPSelection Read GetSelection Write SetSelection;
    Property Styles : TWPStyles Read GetStyles Write SetStyles;
    Property Width : Integer Read FWidth Write SetWidth;
    Property CurrentHotspot : TWPHotspot Read FCurrentHotspot Write SetCurrentHotspot;
    Property CurrentButton : TFslObject Read FCurrentButton Write SetCurrentButton;
    Property SomeSelected : Boolean Read FSomeSelected;
    Property OnSetDocumentHeight : TNotifyEvent Read FOnSetDocumentHeight Write FOnSetDocumentHeight;
    Property SectionHeaderHeight : Integer Read FSectionHeaderHeight;
    Property SectionFieldCharWidth : Integer Read FSectionFieldCharWidth;
    Property SectionFieldCharHeight : Integer Read FSectionFieldCharHeight;
    Property Canvas : TWPCanvas Read GetCanvas Write SetCanvas;
    Property Valid : Boolean Read FValid Write FValid;
    Property PaintAll : Boolean Read FPaintAll Write FPaintAll;
    Property MeasureAll : Boolean Read FMeasureAll Write SetMeasureAll;
    Property FieldFontSize : TSize Read FFieldFontSize;
    Property FieldDescent : Integer Read FFieldDescent;
    Property AdjustTop : Integer Read FAdjustTop;
    Property AdjustOffset : Integer Read FAdjustOffset;
    Property DefaultTableBorder : TWPBorder Read FDefaultTableBorder;
    Property Map : TWPMapContainer Read GetMap;
    Property OnError : TWPErrorEvent Read FOnError Write FOnError;
    Property AltKeyDown : Boolean read FAltKeyDown write SetAltKeyDown;
    Property NominalPageHeight : Integer read FNominalPageHeight write FNominalPageHeight;
  End;

Type

  TWPScreenCanvas = Class (TWPCanvas)
    Private
      FStatedWidth : Integer;
      FStatedHeight : Integer;
      FInternalTop : Integer;
      FInternalBottom : Integer;

      FOnUpdateImage : TNotifyEvent;
      FImage : TBitmap;

      FHorizontalMargin : Integer;
      FVerticalMargin : Integer;
      FFontChanged : Boolean;
      FClip : THandle;
      FBackground : TColour;
      FContrast : Real;

      Procedure NeedImage;
      Procedure FreeImage;
      Procedure ApplyFont;
      Procedure SetBackground(Const Value : TColour);
      Procedure SetContrast(Const Value : Real);
      Procedure Initialise(iWidth, iHeight : Integer);
      Procedure ClearTail;
    Protected
      Procedure FontChange(oSender : TObject); Override;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function PaintControl(aHandle : THandle; iHeight, iWidth, iOffset : Integer) : Integer;

      Function SetImageSize(iWidth, iHeight : Integer) : Boolean; Overload; Virtual;
      Procedure SaveImage(Const sFilename : String); Overload; Virtual;
      Procedure SavePNG(Const sFilename : String); Overload; Virtual;
      Function SaveToImage(height, width : Cardinal) : TFslBitmapGraphic; Overload; Virtual;

      Function Visible(iTop, iBottom : Integer) : Boolean;

      Procedure MoveVertical(iTop, iOffset : Integer); Overload; Override;
      Procedure Clip(Const aRect : TRect); Overload; Override;
      Procedure Clip(oObject : TWPMapObject); Overload; Override;
      Procedure UnClip; Overload; Override;

      Procedure DrawRect(aColour : TColour; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer); Overload; Override;
      Procedure DrawText(aBackground : TColour; iLeft, iTop : Integer; Const sText : String); Overload; Override;
      Procedure DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer); Overload; Override;
      Procedure DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic; iX, iY : Integer; iInnerRadius, iOuterRadius : Integer; iRotation : Integer); Overload; Override;
      Procedure DrawPolyLine(aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iXOffset, iYOffset : Integer; oCoords : TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = True); Overload; Override;
      Procedure DrawTriangle(aColour : TColour; iX1, iY1, iX2, iY2, iX3, iY3: Integer); Overload; Override;
      Procedure DrawRoundOutline(aColour : TColour; iLeft, iTop, iRight, iBottom, iRadius : Integer); Overload; Override;

      Function DPIX : Integer; Overload; Override;
      Function DPIY : Integer; Overload; Override;

      Function TextExtent(Const sText : String) : TSize; Overload; Override;
      Function GetTextMetrics : TTextMetric; Overload; Override;
      Function TextHeight(Const sText : String) : Integer; Overload; Override;
      Function TextWidth(Const sText : String) : Integer; Overload; Override;
      Procedure GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers); Overload; Override;

      Procedure Yield; Overload; Virtual;
      Procedure Scrub; Overload; Virtual;
      Function ApplyContrast(aColour : TColour) : TColour;

      Property Height : Integer Read FStatedHeight;
      Property Width : Integer Read FStatedWidth;
      Property HorizontalMargin : Integer Read FHorizontalMargin Write FHorizontalMargin;
      Property VerticalMargin : Integer Read FVerticalMargin Write FVerticalMargin;
      Property InternalTop : Integer Read FInternalTop;
      Property InternalBottom : Integer Read FInternalBottom;
      Property OnUpdateImage : TNotifyEvent Read FOnUpdateImage Write FOnUpdateImage;
      Property Background : TColour Read FBackground Write SetBackground;
      Property Contrast : Real Read FContrast Write SetContrast;
      Property StatedWidth : Integer Read FStatedWidth;
      Property StatedHeight : Integer Read FStatedHeight;

  End;


Var
  INTERNAL_HEIGHT : Integer;   // initialised to the height of the largest screen


Function RandomColour(aColour : TColour; bForce : Boolean = False) : TColour; Overload;


Const
  EDGE_GRACE = 3;

Type
  TWPScreenRenderer = Class (TWPRenderer)
  Private
    FOperator : TWPOperator;
    FLastVersion : Integer;
    FPageOffsets : TWPPaginations;
    FPagePositions : TWPPaginations;

    Function GetCanvas : TWPScreenCanvas;
    Procedure SetCanvas(Const Value : TWPScreenCanvas);
    Procedure SetOperator(Const Value : TWPOperator);

    Function IsEdge(oItem : TWPMapItem; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function IsInHotSpot(oItem : TWPMapItem; oPiece : TWPWorkingDocumentImagePiece; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetMouseInfoInItem(oItem : TWPMapItem; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetMouseInfoInRow(oRow : TWPMapRow; iX, iY, iLimit1, iLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetMouseInfoInRows(oRows : TWPMapRows; iX, iY, iLimit1, iLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetMouseInfoInContainers(oContainers : TWPMapContainers; bHorizontal : Boolean; iX, iY, iYLimit, iXLimit1, iXLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetMouseInfoInContainer(oContainer : TWPMapContainer; iX, iY, iYLimit, iXLimit1, iXLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
    Function GetPointForOffset(iOffset : Integer; Var iX, iY, iHeight : Integer; Var aColour : TColour; Out oRegion : TWPMapItem) : Boolean; Overload;

    Function GetEffectiveTopOfRow(oRow : TWPMapRow) : Integer;
    Function GetEffectiveBottomOfRow(oRow : TWPMapRow) : Integer;
    Function GetRowForRegion(oRegion : TWPMapItem; Out oRow : TWPMapRow) : Boolean;

    Function GetByLineCol(oRow : TWPMapRow; iLine, iCol : Integer; Out iPosition : Integer) : Boolean; Overload;
    Function GetByLineCol(oRows : TWPMapRows; iLine, iCol : Integer; Out iPosition : Integer) : Boolean; Overload;
    Function GetByLineCol(oContainer : TWPMapContainer; iLine, iCol : Integer; Out iPosition : Integer) : Boolean; Overload;
    Function GetByLineCol(oContainers : TWPMapContainers; iLine, iCol : Integer; Out iPosition : Integer) : Boolean; Overload;

    Procedure PaintPageMarker(iPageNo, iStart, iEnd : Integer);
    Procedure PaintPageMarkers;
    Procedure PaintMargins;
    Procedure Paint(bAll : Boolean);
    Procedure PaintPiece(oPiece : TWPWorkingDocumentPiece; bPaintAnyway : Boolean);
    Procedure PaintCanvas(oSender : TObject);
    Function GetOperator : TWPOperator;

    Function ApplyAnnotationFont(oDefinitionProvider : TWPAnnotationDefinitionProvider) : Integer;
    Procedure MeasureAnnotation(oAnnotation : TWPWorkingAnnotation);
    Procedure AlignAnnotation(oAnnotation : TWPWorkingAnnotation; iTop, iHeight : Integer);
    Procedure DrawAnnotation(oAnnotation : TWPWorkingAnnotation; iTop : Integer);

    Procedure CheckPageOffsets;
  Protected
    Procedure DoUpdate; Overload; Override;
    Procedure BuildMetrics; Override;
    Function WantFastDrawing : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPScreenRenderer; Overload;
    Function Clone : TWPScreenRenderer; Overload;

    Procedure ChangeSettings; Override;

    Function GetPointForOffset(iOffset : Integer; Var iX, iY, iHeight : Integer; Var aColour : TColour) : Boolean;  Overload; Virtual;
    Function getMouseInfoForPoint(iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean; Overload; Virtual;
    Function GetLineCol(iPosition : Integer; Out iLine, iCol : Integer): Boolean; Overload; Virtual;
    Function GetByLineCol(iLine, iCol : Integer; Out iPosition : Integer) : Boolean; Overload; Virtual;

    Function GetScreenRectForOffset(Const iOffset : Integer) : TRect; Overload; Virtual;

    Function PageUp(Const iCurrent, iPageHeight : Integer; Var iNew : Integer) : Boolean; Overload; Virtual;
    Function LineUp(Const iCurrent : Integer; Var iNew : Integer) : Boolean; Overload; Virtual;
    Function LineDown(Const iCurrent : Integer; Var iNew : Integer) : Boolean; Overload; Virtual;
    Function PageDown(Const iCurrent, iPageHeight : Integer; Var iNew : Integer) : Boolean; Overload; Virtual;
    Function LineHome(Const iCurrent : Integer; Out iNew : Integer) : Boolean; Overload; Virtual;
    Function LineEnd(Const iCurrent : Integer; Out iNew : Integer) : Boolean; Overload; Virtual;

    Function asRect : TRect; Overload; Virtual;

    Procedure PaintAnnotations(bAll : Boolean);
    Function PaintControl(aHandle : THandle; iControlHeight, iControlWidth, iOffset : Integer) : Integer;
    Procedure SaveImage(Const sFilename : String); Overload; Virtual;
    Procedure SavePNG(Const sFilename : String); Overload; Virtual;
    Procedure Scrub; Overload; Virtual;
    Procedure Yield; Overload; Virtual;

    Function HasPaginations : Boolean;
    Procedure ClearPaginations;
    Procedure SetPaginations(Const aPages : TWPPaginations);
    Function GetPageForOffset(iOffset : Integer):Integer;
    Function PageCount : Integer;
    Property LastVersion : Integer Read FLastVersion;

    Property Operator : TWPOperator Read GetOperator Write SetOperator;
    Property Canvas : TWPScreenCanvas Read GetCanvas Write SetCanvas;
    Property PageOffsets : TWPPaginations Read FPageOffsets;
    Property PagePositions : TWPPaginations Read FPagePositions;
  End;

  TWPSpanPolicy = (
    spSpan,      // content is allowed to span pages
    spError,     // an exception will be raised if the content spans more than one page
    spTruncate); // if the content spans more than one page, a truncation marker will be placed at the end of the first page


  TWPPageLayoutController = Class (TFslObject)
    Private
      FSpanPolicy : TWPSpanPolicy;
    Public
      constructor Create(aSpanPolicy : TWPSpanPolicy); Overload; Virtual;

      Function Link : TWPPageLayoutController; Overload;

      Function SpanPolicy : TWPSpanPolicy; Overload; Virtual;
      Function Width(oCanvas : TFslPrinterCanvas) : Integer; Overload; Virtual;
      Function Left(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer; Overload; Virtual;
      Function Top(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer; Overload; Virtual;
      Function Height(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer; Overload; Virtual;

      Function IsSpanPolicySpan : Boolean; Overload; Virtual;
      Function IsSpanPolicyError : Boolean; Overload; Virtual;
      Function IsSpanPolicyTruncate : Boolean; Overload; Virtual;


//      Procedure SetSpanPolicy(aSpanPolicy : TWPSpanPolicy); Overload; Virtual;
      Procedure SpanPolicySpan; Overload; Virtual;
      Procedure SpanPolicyError; Overload; Virtual;
      Procedure SpanPolicyTruncate; Overload; Virtual;

  End;

Const
  NAMES_SPAN_POLICY : Array [TWPSpanPolicy] of String = ('Span', 'Error', 'Truncate');

Type
  TWPPage = Class(TFslObject)
    Private
      FMap : TWPMapContainer;
      FCursor : Integer;
      FTruncated : Boolean;

      Function GetMap : TWPMapContainer;
      Procedure SetMap(Const Value : TWPMapContainer);
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPPage; Overload;
      Function Clone : TWPPage; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Fits(iHeight : Integer) : Boolean;
      function isEmpty : boolean;

      Function GetLastOffset : Integer; Overload; Virtual;

      Property Map : TWPMapContainer Read GetMap Write SetMap;
      Property Cursor : Integer Read FCursor Write FCursor;
      Property Truncated : Boolean Read FTruncated Write FTruncated;
  End;

  EWPPage = Class(EFslException)
  End;

  TWPPages = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPPage;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPPage);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPPage; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPPages; Overload;
      Function Clone : TWPPages; Overload;

      Function New : TWPPage; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPPage Read GetElement Write SetElement; Default;
  End;

Type
  TWPVisualRange = Class (TWPRange)
    Private
      FRenderer : TWPScreenRenderer;
      FPageHeight : Integer;
      Function GetRenderer: TWPScreenRenderer;
      Procedure SetRenderer(Const Value: TWPScreenRenderer);
    Protected
      Function GetWorkingWidth : Integer; Override;
    Public
      destructor Destroy; Override;

      Procedure CutOff;  Overload; Override;

      Procedure ChangeState(bDraw : Boolean = True); Override;

      Function PageUp(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function LineUp(bSelect : Boolean) : Boolean; Overload; Virtual;

      Function GoLineStart(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function GoLineEnd(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function LineDown(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function PageDown(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function LineMove(iDelta : Integer; bSelect : Boolean) : Boolean; Overload; Virtual;

      Function HasRenderer : Boolean;
      Property Renderer : TWPScreenRenderer Read GetRenderer Write SetRenderer;
      Property PageHeight : Integer Read FPageHeight Write FPageHeight; // used for pageup / page down
  End;


Implementation

Function RandomColour() : TColour; overload;
Begin
  Case RandomInteger(16) Of
    0:Result := clAqua;
    1:Result := clBlack;
    2:Result := clBlue;
    3:Result := clDkGray;
    4:Result := clFuchsia;
    5:Result := clGray;
    6:Result := clGreen;
    7:Result := clLime;
    8:Result := clLtGray;
    9:Result := clMaroon;
    11:Result := clNavy;
    12:Result := clOlive;
    13:Result := clPurple;
    14:Result := clRed;
    15:Result := clSilver;
  Else
    Result := clYellow;
  End
End;

{

Column widths are determined as follows:

1. Cells
--------
  For each Cell, calculate
    WMin: the formatted content may span any number of lines but may not overflow the cell box.
    WMax: formatting the content without breaking lines other than where explicit line breaks occur.
    WSpec: The specified width of the cell in pixels


2. Rows
-----------------
  For each column,

  #1. determine a WMin, WMax, and WSpec column width from the cells that span only that column.
  #2. For each cell that spans more than one column, increase the WMin of the columns
      it spans so that together, they are at least as wide as the cell WMin. Do the same for the
      WMax and WSpec. Widen all spanned columns by approximately the same amount.
  #3. if WSpec is <> 0 then WMin := WSpec

3. Columns
--------
  if the sum(WMin) > Total Width Then
    each column gets (WMin  / SUM(WMin) * Total)
  else
    each column gets WMin
    sum WMax-  WMin as you go
    if Sum(WMax-WMin) < total - allocated then
      give each column as much as they want
    else for each column, if WMax-WMin < avg(avail) then
      give space
      sum WMax, WMin
      repeat until no space available

This algorithm was derived from the CSS2 Specification
(http://www.w3.org/TR/CSS21/tables.html#auto-table-layout)

Additional Notes:

 * the table has a list of rows and a columncount - the number of virtual columns
 * each rows has a list of cells
 * rows must have at least one cell
 * each cell has 3 metrics, WMin, WMax, and WSpec - WMin and WMax are calculated by the
   renderer prior to calling this class
 * each cell has overhead which is:
    for the left most column: (left border width + DEFAULT_CELL_MARGIN_LEFT + DEFAULT_CELL_MARGIN_RIGHT + right border width) * PointSize
    For all othe columns: (DEFAULT_CELL_MARGIN_LEFT + DEFAULT_CELL_MARGIN_RIGHT + right border width) * PointSize

 * the calculation should fill out FColumns, giving the widths of the column sizes including the overhead
}

Type
  TWPRendererTableColumnSizeCalculator = Class (TFslObject)
    Private
      FTable : TWPWorkingDocumentTableStartPiece;
      FColumns : TWPTableColumnMetrics;
      FPointSize : Integer;
      FWidth : Integer;
      FNestingIndent: Integer;

      Procedure SetTable(Const Value : TWPWorkingDocumentTableStartPiece);
      Procedure SetColumns(Const Value : TWPTableColumnMetrics);

      Procedure InitialiseColumns;
      Procedure SumColumnWidths;
      Procedure CountMargins(oCell : TWPWorkingDocumentTableCellStartPiece; iFirstCol : Integer);
      Procedure CountCellWidth(oCell : TWPWorkingDocumentTableCellStartPiece; iFirstCol : Integer);
      Procedure AllColumns(oColumns : TWPTableColumnMetrics);
      Procedure AllocatedSpecifiedColumns(oColumns : TWPTableColumnMetrics);
      Procedure AllocateMinimums(oColumns : TWPTableColumnMetrics);
      Procedure AllocateExtra(oColumns : TWPTableColumnMetrics);
      Function GetColumns : TWPTableColumnMetrics;
      Function GetTable : TWPWorkingDocumentTableStartPiece;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(oTable : TWPWorkingDocumentTableStartPiece; oColumns : TWPTableColumnMetrics; iFPointSize, iWidth : Integer); Overload; Virtual;
      destructor Destroy; Override;

      Procedure Calculate; Overload; Virtual;

      Property Table : TWPWorkingDocumentTableStartPiece Read GetTable Write SetTable;
      Property Columns : TWPTableColumnMetrics Read GetColumns Write SetColumns;
      Property PointSize : Integer Read FPointSize Write FPointSize;
      Property Width : Integer Read FWidth Write FWidth;
      Property NestingIndent : Integer Read FNestingIndent write FNestingIndent;
  End;

Constructor TWPRendererTableColumnSizeCalculator.Create(oTable : TWPWorkingDocumentTableStartPiece; oColumns : TWPTableColumnMetrics; iFPointSize, iWidth : Integer);
Begin
  Create;

  FTable := oTable;
  FColumns := oColumns;
  FPointSize := iFPointSize;
  FWidth := iWidth;
End;


Destructor TWPRendererTableColumnSizeCalculator.Destroy;
Begin
  FTable.Free;
  FColumns.Free;

  Inherited;
End;


Procedure TWPRendererTableColumnSizeCalculator.SetTable(Const Value : TWPWorkingDocumentTableStartPiece);
Begin
  FTable.Free;
  FTable := Value;
End;


Procedure TWPRendererTableColumnSizeCalculator.SetColumns(Const Value : TWPTableColumnMetrics);
Begin
  FColumns.Free;
  FColumns := Value;
End;


Procedure TWPRendererTableColumnSizeCalculator.Calculate;
Var
  oToAllocate : TWPTableColumnMetrics;
Begin
  InitialiseColumns;
  SumColumnWidths;

  oToAllocate := TWPTableColumnMetrics.Create;
  Try
    AllColumns(oToAllocate);
    AllocatedSpecifiedColumns(oToAllocate);
    AllocateMinimums(oToAllocate);
    AllocateExtra(oToAllocate);
  Finally
    oToAllocate.Free;
  End;
End;


Procedure TWPRendererTableColumnSizeCalculator.InitialiseColumns;
Var
  iLoop : Integer;
Begin
  FColumns.Clear;
  For iLoop := 1 To Table.ColumnCount Do
    FColumns.Add(FColumns.New);
End;


Procedure TWPRendererTableColumnSizeCalculator.SumColumnWidths;
Var
  iSpan : Integer;
  iRow : Integer;
  iCell : Integer;
  iCol : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  For iSpan := 1 To Table.ColumnCount Do
  Begin
    For iRow := 0 To Table.Rows.Count - 1 Do
    Begin
      oRow := Table.Rows[iRow];
      iCol := 0;
      For iCell := 0 To oRow.Cells.Count - 1 Do
      Begin
        oCell := oRow.Cells[iCell];
        If oCell.Span = iSpan Then
          CountMargins(oCell, iCol);
        Inc(iCol, oCell.Span)
      End;
    End;
  End;

  For iSpan := 1 To Table.ColumnCount Do
  Begin
    For iRow := 0 To Table.Rows.Count - 1 Do
    Begin
      oRow := Table.Rows[iRow];
      iCol := 0;
      For iCell := 0 To oRow.Cells.Count - 1 Do
      Begin
        oCell := oRow.Cells[iCell];
        If oCell.Span = iSpan Then
          CountCellWidth(oCell, iCol);
        Inc(iCol, oCell.Span)
      End;
    End;
  End;
End;


Procedure TWPRendererTableColumnSizeCalculator.CountMargins(oCell : TWPWorkingDocumentTableCellStartPiece; iFirstCol : Integer);
Var
  oCol : TWPRendererTableColumnMetric;
  iBorder : Integer;
Begin
  oCol := FColumns[iFirstCol];

  If oCell.Span = 1 Then
  Begin
    iBorder := oCell.WorkingMarginLeft;
    If (iFirstCol = 0) Then
      iBorder := iBorder + NestingIndent * TWPWorkingDocumentTableRowStartPiece(oCell.Row).Depth;

    If oCell.WorkingLeftBorder.Defined Then
      If (oCell.State In [tisFirst, tisOnly]) Then
        Inc(iBorder, oCell.WorkingLeftBorder.ActualWidth)
      Else
        Inc(iBorder, (oCell.WorkingLeftBorder.ActualWidth Div 2));
    oCol.DeadLefts.Add((iBorder+oCell.MaxLeftParaSpace) * FPointSize);

    iBorder := oCell.WorkingMarginRight;
    If oCell.WorkingRightBorder.Defined Then
      If (oCell.State In [tisLast, tisOnly]) Then
        Inc(iBorder, oCell.WorkingRightBorder.ActualWidth)
      Else
        Inc(iBorder, (oCell.WorkingRightBorder.ActualWidth Div 2));
    oCol.DeadRights.Add((iBorder+oCell.MaxRightParaSpace) * FPointSize);
    // now walk the paragraphs finding any indents or lists
    // now walk the paragraphs finding any indents or lists
  End
  Else
  Begin
    oCol := FColumns[iFirstCol];
    iBorder := oCell.WorkingMarginLeft;
    If (iFirstCol = 0) Then
      iBorder := iBorder + NestingIndent * TWPWorkingDocumentTableRowStartPiece(oCell.Row).Depth;

    If oCell.WorkingLeftBorder.Defined Then
      If (oCell.State In [tisFirst, tisOnly]) Then
        Inc(iBorder, oCell.WorkingLeftBorder.ActualWidth)
      Else
        Inc(iBorder, (oCell.WorkingLeftBorder.ActualWidth Div 2));
    oCol.DeadLefts.Add((iBorder+oCell.MaxLeftParaSpace) * FPointSize);

    oCol := FColumns[iFirstCol + oCell.Span - 1];
    iBorder := oCell.WorkingMarginRight;
    If oCell.WorkingRightBorder.Defined Then
      If (oCell.State In [tisLast, tisOnly]) Then
        Inc(iBorder, oCell.WorkingRightBorder.ActualWidth)
      Else
        Inc(iBorder, (oCell.WorkingRightBorder.ActualWidth Div 2));
    oCol.DeadRights.Add((iBorder+oCell.MaxRightParaSpace) * FPointSize);
  End;
End;

Procedure TWPRendererTableColumnSizeCalculator.CountCellWidth(oCell : TWPWorkingDocumentTableCellStartPiece; iFirstCol : Integer);
Var
  oCol : TWPRendererTableColumnMetric;
  iLoop : Integer;
  iMin : Integer;
  iMax : Integer;
  iSpec : Integer;
Begin
  oCol := FColumns[iFirstCol];
  If oCell.Span = 1 Then
  Begin
    oCol.Minimum := IntegerMax(oCell.WidthMinimum, oCol.Minimum);
    oCol.Maximum := IntegerMax(oCell.WidthMaximum, oCol.Maximum);
    oCol.Specified := IntegerMax(oCell.WidthSpecified(FWidth), oCol.Specified);
  End
  Else
  Begin
    iMin := oCol.Minimum;
    iMax := oCol.Maximum;
    iSpec := oCol.Specified;
    For iLoop := iFirstCol + 1 To iFirstCol + oCell.Span - 1 Do
    Begin
      oCol := FColumns[iLoop];
      Inc(iMin, oCol.Minimum + oCol.DeadLeft + oCol.DeadRight);
      Inc(iMax, oCol.Maximum + oCol.DeadLeft + oCol.DeadRight);
      Inc(iSpec, oCol.Specified);
    End;

    iMin := IntegerMax(0, oCell.WidthMinimum - iMin);
    iMax := IntegerMax(0, oCell.WidthMaximum - iMax);
    iSpec := IntegerMax(0, oCell.WidthSpecified(FWidth) - iSpec);

    For iLoop := iFirstCol To iFirstCol + oCell.Span - 1 Do
    Begin
      oCol := FColumns[iLoop];
      oCol.Minimum := oCol.Minimum + Trunc(iMin / oCell.Span);
      oCol.Maximum := oCol.Maximum + Trunc(iMax / oCell.Span);
      oCol.Specified := oCol.Specified + Trunc(iSpec / oCell.Span);
    End;
  End;
End;

(*
don't do this at this time

Procedure TWPRendererTableColumnSizeCalculator.CheckRequiredSpace;
Var
  iLoop : Integer;
  iRequired : Integer;
  oCol : TWPRendererTableColumnMetric;
Begin
  iRequired := 0;
  For iLoop := 0 To FColumns.Count - 1 Do
  Begin
    oCol := FColumns[iLoop];
    If oCol.Specified > 0 Then
    Begin
      If oCol.Specified < oCol.Border + oCol.Margin + MIN_CELL_SPACE * FPointSize Then
        Error('CheckRequiredSpace', StringFormat('Insufficient space for column %d', [iLoop + 1]));
      Inc(iRequired, oCol.Specified)
    End
    Else
      Inc(iRequired, oCol.Border + oCol.Margin + MIN_CELL_SPACE * FPointSize);
  End;
  If iRequired > FWidth Then
    Error('CheckRequiredSpace', StringFormat('Insufficient space for the table (required = %d, available = %d)', [iRequired, FWidth]));
End;
*)

Procedure TWPRendererTableColumnSizeCalculator.AllColumns(oColumns : TWPTableColumnMetrics);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To FColumns.Count - 1 Do
    oColumns.Add(FColumns[iLoop].Link);
End;


Procedure TWPRendererTableColumnSizeCalculator.AllocatedSpecifiedColumns(oColumns : TWPTableColumnMetrics);
Var
  iLoop : Integer;
  oCol : TWPRendererTableColumnMetric;
Begin
  For iLoop := oColumns.Count - 1 DownTo 0 Do
  Begin
    oCol := oColumns[iLoop];
    If oCol.Specified > 0 Then
    Begin
      oCol.Actual := oCol.Specified;
      oColumns.DeleteByIndex(iLoop);
    End;
  End;
End;


Procedure TWPRendererTableColumnSizeCalculator.AllocateMinimums(oColumns : TWPTableColumnMetrics);
Var
  iLoop : Integer;
  iTotal : Integer;
  iMinTotal : Integer;
  rAdjust : Double;
  oCol : TWPRendererTableColumnMetric;
Begin
  iTotal := 0;
  iMinTotal := 0;
  For iLoop := oColumns.Count - 1 DownTo 0 Do
  Begin
    oCol := oColumns[iLoop];
    Inc(iMinTotal, oCol.Minimum);
    Inc(iTotal, oCol.DeadLeft + oCol.DeadRight + oCol.Minimum);
  End;

  If iMinTotal > 0 Then
    rAdjust := ((FColumns.SumActual + iTotal) - FWidth) / iMinTotal
  Else
    rAdjust := 0;

  If rAdjust > 0 Then
  Begin
    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      oCol.Actual := oCol.DeadLeft + oCol.DeadRight + IntegerMax(0, Trunc(oCol.Minimum - (rAdjust * oCol.Minimum)));
      If oCol.Maximum = oCol.Minimum Then
        oColumns.DeleteByIndex(iLoop);
    End;
  End;
End;

Procedure TWPRendererTableColumnSizeCalculator.AllocateExtra(oColumns : TWPTableColumnMetrics);
Var
  iLoop, iDiff : Integer;
  rAverage : Real;
  oCol : TWPRendererTableColumnMetric;
  bNone : Boolean;
Begin
  bNone := False;
  rAverage := 0;

  While (oColumns.Count > 1) And Not bNone Do
  Begin
    bNone := True;
    rAverage := (FWidth - FColumns.SumActual) / oColumns.Count;

    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      If oCol.Maximum + oCol.DeadLeft + oCol.DeadRight <= Trunc(oCol.Actual + rAverage) Then
      Begin
        bNone := False;
        oCol.Actual := oCol.Maximum + oCol.DeadLeft + oCol.DeadRight;
        oColumns.DeleteByIndex(iLoop);
      End;
    End;
  End;

  If oColumns.Count = 1 Then
  Begin
    oCol := oColumns[0];
    oCol.Actual := 0;
    oCol.Actual := IntegerMin(FWidth - FColumns.SumActual, oCol.Maximum + oCol.DeadLeft + oCol.DeadRight);
  End
  Else
  Begin
    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      oCol.Actual := Trunc(oCol.Actual + rAverage);
    End;
  End;

  iDiff := FWidth - FColumns.SumActual;
  If (iDiff > 0) And (iDiff < FColumns.Count) Then
  Begin
    For iLoop := 0 To iDiff - 1 Do
      FColumns[iLoop].Actual := FColumns[iLoop].Actual + 1;
  End;

  If (FWidth - FColumns.SumActual > 0) and FTable.ExpandLastColumn Then
    FColumns[FColumns.Count - 1].Actual := FColumns[FColumns.Count - 1].Actual + (FWidth - FColumns.SumActual);
End;


Function TWPRendererTableColumnSizeCalculator.GetTable : TWPWorkingDocumentTableStartPiece;
Begin
  Assert(Invariants('GetTable', FTable, TWPWorkingDocumentTableStartPiece, 'Table'));
  Result := FTable;
End;


Function TWPRendererTableColumnSizeCalculator.GetColumns : TWPTableColumnMetrics;
Begin
  Assert(Invariants('GetColumns', FColumns, TWPTableColumnMetrics, 'Columns'));
  Result := FColumns;
End;




function TWPRendererTableColumnSizeCalculator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTable.sizeInBytes);
  inc(result, FColumns.sizeInBytes);
end;

Constructor TWPCanvas.Create;
Begin
  Inherited;
  Font := TFslFont.Create;
End;


Destructor TWPCanvas.Destroy;
Begin
  FFont.Free;
  Inherited;
End;


Function TWPCanvas.TextExtent(Const sText : String) : TSize;
Begin
  RaiseError('TextExtent', 'Must Override '+ClassName+'.TextExtent');
End;


Function TWPCanvas.GetTextMetrics : TTextMetric;
Begin
  RaiseError('GetTextMetrics :', 'Must Override '+ClassName+'.GetTextMetrics :');
End;


Function TWPCanvas.TextHeight(Const sText : String) : Integer;
Begin
  RaiseError('TextHeight', 'Must Override '+ClassName+'.TextHeight');
  Result := 0;
End;


Function TWPCanvas.TextWidth(Const sText : String) : Integer;
Begin
  RaiseError('TextWidth', 'Must Override '+ClassName+'.TextWidth');
  Result := 0;
End;


Procedure TWPCanvas.GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers);
Begin
  RaiseError('GetTextExtents', 'Must Override '+ClassName+'.GetTextExtents');
End;

Procedure TWPCanvas.DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer);
Begin
  RaiseError('DrawRect', 'Must Override '+ClassName+'.DrawRect');
End;


Procedure TWPCanvas.DrawRect(aColour: TColour; iLeft, iTop, iRight, iBottom: Integer);
Begin
  RaiseError('DrawRect', 'Must Override '+ClassName+'.DrawRect');
End;


Procedure TWPCanvas.DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; iX1, iY1, iX2, iY2 : Integer);
Begin
  DrawLine(iWidth, aColour, aPenStyle, apesRound, iX1, iY1, iX2, iY2);
End;


Procedure TWPCanvas.DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer);
Begin
  RaiseError('DrawLine', 'Must Override '+ClassName+'.DrawLine');
End;


Procedure TWPCanvas.DrawText(aBackground : TColour; iLeft, iTop: Integer; Const sText: String);
Begin
  RaiseError('DrawText', 'Must Override '+ClassName+'.DrawText');
End;


Procedure TWPCanvas.DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer);
Begin
  RaiseError('DrawSquiggle', 'Must Override '+ClassName+'.DrawSquiggle');
End;


Procedure TWPCanvas.DrawBorder(aColour : TColour; iLeft, iTop, iRight, iBottom, iLeftMargin, iTopMargin, iRightMargin, iBottomMargin : Integer);
Begin
  DrawRect(aColour, iLeft,                   iTop,                        iLeft + iLeftMargin,   iBottom);
  DrawRect(aColour, iLeft,                   iTop,                        iRight,                iTop + iTopMargin);
  DrawRect(aColour, iRight - iRightMargin,   iTop,                        iRight,                iBottom);
  DrawRect(aColour, iLeft,                   iBottom - iBottomMargin - 1, iRight,                iBottom);
End;


Procedure TWPCanvas.DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer);
Begin
  RaiseError('DrawImage', 'Must Override '+ClassName+'.DrawImage');
End;

Procedure TWPCanvas.SetFont(oFont : TFslFont);
Begin
  FFont.Free;
  FFont := oFont;
  FFont.OnChange := FontChange;
  FontChange(Nil);
End;


Procedure TWPCanvas.FontChange(oSender : TObject);
Begin
End;


Function TWPCanvas.PointSize : Integer;
Begin
  Result := (FPointSizeX + FPointSizeY) Div 2;
End;


Procedure TWPCanvas.MoveVertical(iTop, iOffset : Integer);
Begin
  RaiseError('MoveVertical', 'Must override '+ClassName+'.MoveVertical');
End;


Procedure TWPCanvas.Clip(Const aRect : TRect);
Begin
  RaiseError('Clip', 'Must override '+ClassName+'.Clip');
End;


Procedure TWPCanvas.Clip(oObject : TWPMapObject);
Begin
  RaiseError('Clip', 'Must override '+ClassName+'.Clip');
End;


Procedure TWPCanvas.UnClip;
Begin
  RaiseError('UnClip', 'Must override '+ClassName+'.UnClip');
End;


Function TWPCanvas.DPIX : Integer;
Begin
  Result := 0;
  RaiseError('DPIX', 'Must override '+ClassName+'.DPIX');
End;


Function TWPCanvas.DPIY : Integer;
Begin
  Result := 0;
  RaiseError('DPIY', 'Must override '+ClassName+'.DPIY');
End;


Function TWPCanvas.GetFont : TFslFont;
Begin
  Assert(Invariants('GetFont', FFont, TFslFont, 'Font'));
  Result := FFont;
End;


Procedure TWPCanvas.DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic;  iX, iY, iInnerRadius, iOuterRadius, iRotation: Integer);
Begin
  RaiseError('DrawCurve', 'Must override '+ClassName+'.DrawCurve');
End;

Procedure TWPCanvas.DrawPolyLine(aColour: TColour; aPenStyle: TFslPenStyle; aEndStyle: TFslPenEndStyle; iXOffset, iYOffset : Integer; oCoords: TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = True);
Begin
  RaiseError('DrawPolyLine', 'Must Override '+ClassName+'.DrawPolyLine');
End;

Function TWPCanvas.MakePenHandle(oPen : TPen; aEndStyle : TFslPenEndStyle; aJoinStyle : TFslPenJoinStyle): HPEN;
Var
  aBrush : TLogBrush;
Const
  PenStyles: Array[TPenStyle] Of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME {$IFNDEF VER130}, PS_SOLID, PS_SOLID{$ENDIF});
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


Procedure TWPCanvas.DrawTriangle(aColour: TColour; iX1, iY1, iX2, iY2, iX3, iY3: Integer);
Begin
  RaiseError('DrawTriangle', 'Must Override '+ClassName+'.DrawTriangle');
End;

Procedure TWPCanvas.DrawRoundOutline(aColour : TColour; iLeft, iTop, iRight, iBottom, iRadius : Integer);
Begin
  RaiseError('DrawRoundOutline', 'Must Override '+ClassName+'.DrawRoundOutline');
End;


Function TWPCanvas.ColourDarken(iColour: TColour; Const iRatio: Real): TColour;
Begin
  TColourParts(Result).Red := Trunc(RealMin(TColourParts(iColour).Red * iRatio, 255));
  TColourParts(Result).Green := Trunc(RealMin(TColourParts(iColour).Green * iRatio, 255));
  TColourParts(Result).Blue := Trunc(RealMin(TColourParts(iColour).Blue * iRatio, 255));
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;

//function TWPCanvas.ColourIncreaseContrast(iColour: TColour; const iRatio: Real): TColour;
//begin
//
//end;
//
//function TWPCanvas.ColourReduceContrast(iColour: TColour; const iRatio: Real): TColour;
//begin
//
//end;


Function TWPCanvas.ColourLighten(iColour: TColour; Const iRatio: Real): TColour;
Begin
  TColourParts(Result).Red := 255-Trunc(RealMin((255 -TColourParts(iColour).Red) * (1-iRatio), 255));
  TColourParts(Result).Green := 255-Trunc(RealMin((255 -TColourParts(iColour).Green) * (1-iRatio), 255));
  TColourParts(Result).Blue := 255-Trunc(RealMin((255 -TColourParts(iColour).Blue) * (1-iRatio), 255));
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;


function TWPCanvas.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFont.sizeInBytes);
end;

Constructor TWPRendererState.Create;
Begin
  Inherited;
End;


Destructor TWPRendererState.Destroy;
Begin
  FForeHotspot.Free;
  FBackHotspot.Free;
  Inherited;
End;


Function TWPRendererState.Link : TWPRendererState;
Begin
  Result := TWPRendererState(Inherited Link);
End;


Function TWPRendererState.Clone : TWPRendererState;
Begin
  Result := TWPRendererState(Inherited Clone);
End;


Procedure TWPRendererState.Assign(oObject : TFslObject);
Begin
  Inherited;

  Background := TWPRendererState(oObject).Background;
  ReadOnly := TWPRendererState(oObject).ReadOnly;
  ForeHotspot := TWPRendererState(oObject).ForeHotspot.Link;
  BackHotspot := TWPRendererState(oObject).BackHotspot.Link;
End;



function TWPRendererState.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FForeHotspot.sizeInBytes);
  inc(result, FBackHotspot.sizeInBytes);
end;

Destructor TWPRendererStates.Destroy;
Begin
  FSettings.Free;
  Inherited;
End;


Procedure TWPRendererStates.SetSettings(oSettings : TWPSettings);
Begin
  FSettings.Free;
  FSettings := oSettings;
End;

Function TWPRendererStates.Link : TWPRendererStates;
Begin
  Result := TWPRendererStates(Inherited Link);
End;


Function TWPRendererStates.Clone : TWPRendererStates;
Begin
  Result := TWPRendererStates(Inherited Clone);
End;


Function TWPRendererStates.New : TWPRendererState;
Begin
  Result := TWPRendererState(Inherited New);
End;


Function TWPRendererStates.ItemClass : TFslObjectClass;
Begin
  Result := TWPRendererState;
End;


Function TWPRendererStates.GetState(Const iIndex : TColour) : TWPRendererState;
Begin
  Result := TWPRendererState(ObjectByIndex[iIndex]);
End;


Procedure TWPRendererStates.SetState(Const iIndex : TColour; Const oValue : TWPRendererState);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRendererStates.CompareByBackground(pA, pB : Pointer) : TColour;
Begin
  Result := IntegerCompare(TWPRendererState(pA).Background, TWPRendererState(pB).Background);
End;


Function TWPRendererStates.CompareByReadOnly(pA, pB : Pointer) : TColour;
Begin
  Result := IntegerCompare(TColour(TWPRendererState(pA).ReadOnly), TColour(TWPRendererState(pB).ReadOnly));
End;


Function TWPRendererStates.IndexByBackground(Const aValue : TColour) : TColour;
Var
  oState : TWPRendererState;
Begin
  oState := New;
  Try
    oState.Background := aValue;

    If Not Find(oState, Result, CompareByBackground) Then
    Begin
      Result := -1;
    End;
  Finally
    oState.Free;
  End;
End;


Function TWPRendererStates.IndexByReadOnly(Const aValue : TWPSTriState) : TColour;
Var
  oState : TWPRendererState;
Begin
  oState := New;
  Try
    oState.ReadOnly := aValue;

    If Not Find(oState, Result, CompareByReadOnly) Then
    Begin
      Result := -1;
    End;
  Finally
    oState.Free;
  End;
End;


Function TWPRendererStates.Get(Const aValue : TColour) : TWPRendererState;
Begin
  Result := TWPRendererState(Inherited Get(aValue));
End;


Function TWPRendererStates.GetByBackground(Const aValue : TColour) : TWPRendererState;
Begin
  Result := Get(IndexByBackground(aValue));
End;


Function TWPRendererStates.GetByReadOnly(Const aValue : TWPSTriState) : TWPRendererState;
Begin
  Result := Get(IndexByReadOnly(aValue));
End;


Function TWPRendererStates.ExistsByBackground(Const aValue : TColour) : Boolean;
Begin
  Result := ExistsByIndex(IndexByBackground(aValue));
End;


Function TWPRendererStates.ExistsByReadOnly(Const aValue : TWPSTriState) : Boolean;
Begin
  Result := ExistsByIndex(IndexByReadOnly(aValue));
End;


Procedure TWPRendererStates.SortedByBackground;
Begin
  SortedBy(CompareByBackground);
End;


Procedure TWPRendererStates.SortedByReadOnly;
Begin
  SortedBy(CompareByReadOnly);
End;


Function TWPRendererStates.IsSortedByBackground : Boolean;
Begin
  Result := IsSortedBy(CompareByBackground);
End;


Function TWPRendererStates.IsSortedByReadOnly : Boolean;
Begin
  Result := IsSortedBy(CompareByReadOnly);
End;


Procedure TWPRendererStates.Initialise(oSettings : TWPSettings; aReadOnly : TWPSTriState; aReadOnlyColour : TColour);
Begin
  Clear;
  Settings := oSettings.Link;
  ReadOnlyColour := aReadOnlyColour;
  if (oSettings <> Nil) Then
    Add(oSettings.Background, aReadOnly, Nil, Nil)
  Else
    Add(clWhite, aReadOnly, Nil, Nil);
End;


Procedure TWPRendererStates.Add(aColour : TColour; aReadOnly : TWPSTriState; oForeHotspot, oBackHotspot : TWPHotspot);
Var
  oNew : TWPRendererState;
Begin
  oNew := New;
  Try
    oNew.Background := aColour;
    oNew.ReadOnly := aReadOnly;
    oNew.ForeHotspot := oForeHotspot.Link;
    oNew.BackHotspot := oBackHotspot.Link;
    Add(oNew.Link);
  Finally
    oNew.Free;
  End;
End;


function TWPRendererStates.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSettings.sizeInBytes);
end;

Procedure TWPRendererState.SetForeHotspot(Const Value: TWPHotspot);
Begin
  FForeHotspot.Free;
  FForeHotspot := Value;
End;


Procedure TWPRendererState.SetBackHotspot(Const Value: TWPHotspot);
Begin
  FBackHotspot.Free;
  FBackHotspot := Value;
End;

Function TWPRendererStates.WorkingBackground : TColour;
Begin
  If Count > 0 Then
    Result := State[Count-1].background
  Else If Settings <> Nil Then
    Result := Settings.Background
  Else
    Result := clWhite;
End;


Function TWPRendererStates.WorkingForeHotspot : TWPHotspot;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result := State[Count-1].ForeHotspot;
End;


Function TWPRendererStates.WorkingBackHotspot : TWPHotspot;
Begin
  If Count = 0 Then
    Result := Nil
  Else
    Result := State[Count-1].BackHotspot;
End;


Function TWPRendererStates.WorkingState : TWPSTriState;
Begin
  If Count = 0 Then
    Result := tsFalse
  Else
    Result := State[Count-1].readOnly;
End;


Function TWPRendererStates.PreviousWorkingState : TWPSTriState;
Begin
  If Count < 2 Then
    Result := tsFalse
  Else
    Result := State[Count-2].readOnly;
End;

Procedure TWPRendererStates.PushState(oField : TWPWorkingDocumentFieldStartPiece; aSpecifiedBackground : TColour = DEF_COLOUR);
Var
  aState : TWPSTriState;
  aBackground : TColour;
Begin
  If (Settings <> nil) And Settings.ReadOnly Then
  Begin
    // if it's read-only, the state of the editing of the field is
    // neither here nor there
    Add(WorkingBackground, tsFalse, oField.Hotspot, WorkingBackHotspot);
  End
  Else
  Begin
    aState := oField.ReadOnly;
    If (aState = tsUnknown) Then
      aState := WorkingState;
    If oField.IsReadOnly Then
      Add(ReadOnlyColour, aState, oField.Hotspot, WorkingBackHotspot)
    Else If ((Settings = nil) or not Settings.FormsMode) and oField.HasDefinitionProvider And oField.DefinitionProvider.GetBackground(oField.DocField, not Settings.Readonly and (Settings.FieldWrappers in [wpfpSquareBrackets, wpfpHints]), aBackground) Then
      Add(aBackground, aState, oField.Hotspot, WorkingBackHotspot)
    Else if aSpecifiedBackground <> DEF_COLOUR Then
      Add(aSpecifiedBackground, aState, oField.Hotspot, WorkingBackHotspot)
    Else
      Add(WorkingBackground, aState, oField.Hotspot, WorkingBackHotspot);
{
    aState := oField.ReadOnly;

    If aState = tsUnknown Then
      Add(WorkingBackground, WorkingState, oField.Hotspot, WorkingBackHotspot)
    Else If aState = tsTrue Then
    Begin
      If Settings.FieldMarkersInColour Then
      Else
        Add(WorkingBackground, tsTrue, oField.Hotspot, WorkingBackHotspot);
    End
    Else If Settings.FieldMarkersInColour And oField.HasDefinitionProvider And oField.DefinitionProvider.GetBackground(oField.DocField, aBackground) Then
      Add(aBackground, tsFalse, oField.Hotspot, WorkingBackHotspot)
    Else
      Add(Settings.Background, tsFalse, oField.Hotspot, WorkingBackHotspot);}
  End;
End;

Procedure TWPRendererStates.PushState(oContainer : TWPWorkingDocumentContainerPiece; aBackground : TColour);
Var
  aState : TWPSTriState;
  aColour : TColour;
  oHotspot : TWPHotspot;
Begin
  If oContainer.HasHotspot Then
    oHotspot := oContainer.Hotspot
  Else
    oHotspot := WorkingBackHotspot;

  If (Settings <> nil) And Settings.ReadOnly Then
  Begin
    If aBackground <> DEF_COLOUR Then
      Add(aBackground, tsTrue, Nil, oHotspot)
    Else
      Add(WorkingBackground, tsFalse, Nil, oHotspot);
  End
  Else
  Begin
    aState := oContainer.ReadOnly;

    If aBackground <> DEF_COLOUR Then
      aColour := aBackground
    Else If aState = tsTrue Then
      aColour := ReadOnlyColour
    Else If aState = tsFalse Then
      aColour := DEFAULT_BACKGROUND
    Else // If aState = tsUnknown Then
    Begin
      aState := WorkingState;
      aColour := WorkingBackground;
    End;

    Add(aColour, aState, Nil, oHotspot);
  End;
End;


Procedure TWPRendererStates.PopState;
Begin
  If Count > 0 Then
    DeleteByIndex(Count - 1);
End;

Procedure TWPRendererStates.SetCurrentState(oPiece : TWPWorkingDocumentContainerPiece);
Begin
  oPiece.Container.Background := WorkingBackground;
  oPiece.IsReadOnly := WorkingState = tsTrue;
  oPiece.Container.BackHotspot := WorkingBackHotspot.Link;

End;


Procedure TWPRendererStates.SetCurrentState(oPiece: TWPWorkingDocumentPiece; oItem: TWPMapItem);
Begin
  if (oItem <> nil) Then
  Begin
    oItem.Background := WorkingBackground;
    oItem.ForeHotspot := WorkingForeHotspot.Link;
    oItem.BackHotspot := WorkingBackHotspot.Link;
  End;

  // fields are special - the field itself is readonly if it's context is read-only or it's contents are read only
  oPiece.IsReadOnly := (WorkingState = tsTrue) Or ((oPIece.PieceType In [ptFieldStart, ptFieldStop]) And (PreviousWorkingState = tsTrue));
End;

Procedure TWPRendererStates.SetCurrentState(oRow: TWPMapRow);
Begin
  oRow.Background := WorkingBackground;
  oRow.BackHotspot := WorkingBackHotspot.Link;
End;

{ TWPRendererParagraphContext }

Constructor TWPRendererParagraphContext.Create(oContainer: TWPMapContainer; oStateStack : TWPRendererStates);
Begin
  Create;
  FBuffer := TWPMapItems.Create;
  StateStack := oStateStack;
  Container := oContainer;
End;

Destructor TWPRendererParagraphContext.Destroy;
Begin
  FBuffer.Free;
  FContainer.Free;
  FStateStack.Free;
  Inherited;
End;


Function TWPRendererParagraphContext.GetContainer : TWPMapContainer;
Begin
  Assert(Invariants('GetContainer', FContainer, TWPMapContainer, 'Container'));
  Result := FContainer;
End;


Procedure TWPRendererParagraphContext.SetContainer(Const Value: TWPMapContainer);
Begin
  FContainer.Free;
  FContainer := Value;
  FContainer.Rows.Clear;

  FRowWidth := 0;
  FBufferWidth := 0;
  StartNewLine;
End;


Procedure TWPRendererParagraphContext.AddItem(oItem : TWPMapItem);
Begin
  Case oItem.BreakState Of
    bsText :
      AddToBuffer(oItem);
    bsBreak :
      Begin
      If (FBuffer.Count > 0) And (FRowWidth + FBufferWidth > FContainer.InnerWidth) Then
        StartNewLine;
      AddBufferToRow;
      AddToBuffer(oItem);
      AddBufferToRow;
      StartNewLine;
      End;
    bsWhitespace :
      Begin
      If (FBuffer.Count > 0) And (FRowWidth + FBufferWidth > FContainer.InnerWidth) Then
        StartNewLine;
      AddBufferToRow;
      AddToBuffer(oItem);
      AddBufferToRow;
      End;
    bsPara :
      Begin
      If (FBuffer.Count > 0) And (FRowWidth + FBufferWidth > FContainer.InnerWidth) Then
        StartNewLine;
      AddBufferToRow;
      AddToBuffer(oItem);
      AddBufferToRow;
      End;
  End;
End;

Procedure TWPRendererParagraphContext.AddToBuffer(oItem: TWPMapItem);
Begin
  FBufferWidth := FBUfferWidth + oItem.Width;
  FBUffer.Add(oItem.Link);
End;

Procedure TWPRendererParagraphContext.AddPiecePartToRow(oItem : TWPMapItem; oPiece : TWPWorkingDocumentPiece; Var iCursor : Integer);
Var
  oNew : TWPMapItem;
  iStart : Integer;
Begin
  oNew := oItem.Clone;
  oPiece.Maps.Add(oNew);
  oNew.OffsetStart := iCursor;
  iStart := oPiece.Metrics.Offset[iCursor];
  Inc(iCursor); // must fit at least one
  While (iCursor < oPiece.Metrics.OffsetCount) And (FRowWidth + (oPiece.Metrics.Offset[iCursor+1]- iStart) <= FContainer.InnerWidth) Do
    Inc(iCursor);
  oNew.Width := oPiece.Metrics.Offset[iCursor]- iStart;
  oNew.OffsetLength := iCursor - oNew.OffsetStart;
  oNew.Parent := FRow;
  FRow.Items.Add(oNew.Link);
  FRowWidth := FRowWidth + oNew.Width;
End;

Procedure TWPRendererParagraphContext.AddBufferToRow;
Var
  iLoop : Integer;
  iCursor : Integer;
  oitem : TWPMapItem;
  oPiece : TWPWorkingDocumentPiece;
Begin
  If FBufferWidth > FContainer.InnerWidth Then
  Begin
    // this buffer is too big to fit on a single row. We are simply going to break it across the rows
    iLoop := 0;
    While iLoop < FBuffer.Count Do
    Begin
      oItem := FBuffer[iLoop];
      If (FRowWidth + oItem.Width < FContainer.InnerWidth) Or ((FRow.Items.Count = 0) And (oItem.OffsetLength = 1)) Then
      Begin
        FBuffer[iLoop].Parent := FRow;
        FRow.Items.Add(oItem.Link);
        FRowWidth := FRowWidth + oItem.Width;
      End
      Else
      Begin
        oPiece := TWPWorkingDocumentPiece(oItem.Piece);
        oPiece.Maps.Clear;
        iCursor := 0;
        While (iCursor < oPiece.Metrics.OffsetCount) Do
        Begin
          If (FRowWidth + (oPiece.Metrics.Offset[iCursor+1] - oPiece.Metrics.Offset[iCursor]) > FContainer.InnerWidth) And (FRow.Items.Count > 0) Then
            StartNewLine;
          AddPiecePartToRow(oItem, oPiece, iCursor);
        End;
      End;
      Inc(iLoop);
    End;
  End
  Else
  Begin
    FRowWidth := FRowWidth + FBufferWidth;
    For iLoop := 0 To FBuffer.Count - 1 Do
    Begin
      FBuffer[iLoop].Parent := FRow;
      FRow.Items.Add(FBuffer[iLoop].Link);
    End;
  End;
  FBuffer.Clear;
  FBufferWidth := 0;
End;

Procedure TWPRendererParagraphContext.StartNewLine;
Begin
  FRowWidth := 0;
  FRow := TWPMapRow.Create;
  FRow.Parent := FContainer;
  FContainer.Rows.Add(FRow);
  FRow.Left := Container.InnerLeft;
  FRow.Width := Container.InnerWidth;
  FStateStack.SetCurrentState(FRow);
End;

procedure TWPRendererParagraphContext.SetStateStack(const Value: TWPRendererStates);
begin
  FStateStack.Free;
  FStateStack := Value;
end;

procedure TWPRendererParagraphContext.Finish;
begin
  If FBuffer.Count > 0 Then
    AddBufferToRow;
end;

function TWPRendererParagraphContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContainer.sizeInBytes);
  inc(result, FStateStack.sizeInBytes);
  inc(result, FBuffer.sizeInBytes);
  inc(result, FRow.sizeInBytes);
end;

{ TWPRenderer }

Constructor TWPRenderer.Create;
Begin
  Inherited;
  FMap := TWPMapContainer.Create;
  FStateStack := TWPRendererStates.Create;
  FUpdating := False;
  FPaintAll := False;

  FDefaultTableBorder := TWPBorder.Create;
  FDefaultTableBorder.Defined := True;
  FDefaultTableBorder.Colour := clLtGray;
  FDefaultTableBorder.Width := 1;
  FDefaultTableBorder.Style := apsSolid;
End;


Destructor TWPRenderer.Destroy;
Begin
  FCurrentHotspot.Free;
  FCurrentButton.Free;
  FDefaultTableBorder.Free;
  FCanvas.Free;
  FMap.Free;
  FStateStack.Free;
  FStyles.Free;
  FSelection.Free;
  FDocument.Free;
  Inherited;
End;


Procedure TWPRenderer.Clear;
Begin
  FMap.Clear;
  FMeasureAll := True;
  FPaintAll := True;
  FValid := False;
End;


Function TWPRenderer.Clone: TWPRenderer;
Begin
  Result := TWPRenderer(Inherited Clone);
End;


Function TWPRenderer.Link: TWPRenderer;
Begin
  Result := TWPRenderer(Inherited Link);
End;


Procedure TWPRenderer.SetCanvas(Const Value : TWPCanvas);
Begin
  FCanvas.Free;
  FCanvas := Value;
  BuildMetrics;
End;


Procedure TWPRenderer.SetWorking(Const Value : Boolean);
Begin
  If (FWorking <> Value) Then
  Begin
  FWorking := Value;
  If FWorking And Not Valid Then
    Update;
  End;
End;


Procedure TWPRenderer.SetDocument(Const Value: TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
  FValid := False;
End;


Procedure TWPRenderer.SetSelection(Const Value: TWPSelection);
Begin
  FSelection.Free;
  FSelection := Value;
  FValid := False;
End;


Procedure TWPRenderer.SetStyles(Const Value: TWPStyles);
Begin
  FStyles.Free;
  FStyles := Value;
  FValid := False;
End;


Function TWPRenderer.GetHeight: Integer;
Begin
  If Not Valid Then
    Update;
  Result := FMap.Height;
End;



Procedure TWPRenderer.BuildMetrics;
Var
  aSize : TSize;
Begin
  ApplyFieldFont;
  FFieldFontSize := FCanvas.TextExtent(FIELD_START_CHAR);
  FFieldDescent := FCanvas.GetTextMetrics.tmDescent;
  FIndentWidth := FCanvas.TextExtent('WW').cx;

  aSize := FCanvas.TextExtent(FIELD_END_CHAR);
  FFieldFontSize.cx := IntegerMax(FFieldFontSize.cx, aSize.cx);

  FValid := False;
End;


Function TWPRenderer.GetSectionHeaderHeight: Integer;
Begin
  If FSectionHeaderHeight = 0 Then
    Begin
    ApplySectionHeaderFont;
    FSectionHeaderHeight := FCanvas.TextHeight('pP');
    FSectionFieldCharWidth := FCanvas.TextWidth('[]') Div 2;
  End;
  Result := FSectionHeaderHeight;
End;


Function TWPRenderer.GetFieldCharWidth: Integer;
Begin
  If FSectionFieldCharWidth = 0 Then
    Begin
    ApplyFieldFont;
    FSectionFieldCharWidth := FCanvas.TextWidth('[]') Div 2;
    FSectionFieldCharHeight := FCanvas.TextHeight('[]');
    End;
  Result := FSectionFieldCharWidth;
End;


Function TWPRenderer.GetFieldCharHeight: Integer;
Begin
  GetFieldCharWidth;
  Result := FSectionFieldCharHeight;
End;


Procedure TWPRenderer.ApplySectionHeaderFont;
Begin
  FCanvas.Font.Clear;
  FCanvas.Font.Colour := clGray;
  FCanvas.Font.Name := 'Microsoft Sans Serif';
  FCanvas.Font.Size := Trunc(10 * Settings.Scale);
  FCanvas.Font.Italic := True;
End;


Procedure TWPRenderer.ApplyFieldFont;
Begin
  FCanvas.Font.Clear;
  FCanvas.Font.Colour := clGray;
  FCanvas.Font.Name := 'Microsoft Sans Serif';
  FCanvas.Font.Size := Trunc(12 * Settings.Scale);
  FCanvas.Font.Bold := True;
End;


Function TWPRenderer.ApplyFont(oPiece : TWPWorkingDocumentPiece; oStyle : TWPStyle; bAllowDecorations : Boolean = True) : TWPSFontState;
Var
  oFont : TWPSFontDetails;
Begin

  If Assigned(oStyle) Then
  Begin
    oFont := oPiece.Font;

    FCanvas.Font.Clear;

    FCanvas.Font.Name := oStyle.WorkingFontName(oFont);
    // Small-caps is implementing as 3/4 size of normal font size, and caping all characters
    If oFont.Capitalization = fcsSmallCaps Then
      FCanvas.Font.Size := Trunc(oStyle.WorkingFontSize(oFont) * Settings.Scale * 0.75)
    Else
      FCanvas.Font.Size := Trunc(oStyle.WorkingFontSize(oFont) * Settings.Scale);
    FCanvas.Font.Colour := oStyle.WorkingFontForeground(oFont);

    If oStyle.WorkingFontBold(oFont) = tsTrue Then
      FCanvas.Font.Bold := True;

    If oStyle.WorkingFontItalic(oFont) = tsTrue Then
      FCanvas.Font.Italic := True;

    If bAllowDecorations And (oStyle.WorkingFontUnderline(oFont) = tsTrue) Then
      FCanvas.Font.Underline := True;

    If bAllowDecorations And (oStyle.WorkingFontStrikeThrough(oFont) = tsTrue) Then
      FCanvas.Font.Strikeout := True;

    If (oPiece.PieceType = ptText) And (TWPWorkingDocumentTextPiece(oPiece).DrawnFont <> '') Then
      FCanvas.Font.Name := TWPWorkingDocumentTextPiece(oPiece).DrawnFont;

    Result := oStyle.WorkingFontState(oFont);
  End
  Else
  Begin
    Result := fsNormal;
  End;
End;


Function TWPRenderer.ApplyFont(oPiece: TWPWorkingDocumentPiece; oStyle : TWPStyle; aColour: TColour; bAllowDecorations : Boolean = True): TWPSFontState;
Begin
  Result := ApplyFont(oPiece, oStyle, bAllowDecorations);
  FCanvas.Font.Colour := aColour;
End;


Procedure TWPRenderer.Doupdate;
Begin
End;


Procedure TWPRenderer.Update;
Begin
  If FWorking And Not FUpdating And (FWidth <> 0) Then
  Begin
    FUpdating := True;
    Try
      DoUpdate;
    Finally
      FUpdating := False;
    End;
    If Assigned(OnSetDocumentHeight) Then
      OnSetDocumentHeight(Self);
  End;
End;


function TWPRenderer.WantFastDrawing: Boolean;
begin
  result := false;
end;


Procedure TWPRenderer.MeasurePieces(bAll : Boolean);
Var
  iLoop : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
  oField : TWPWorkingDocumentFieldStartPiece;
  iFieldWidth : Integer;
Begin
  Document.RegenerateMetrics(False, False);
  oCell := Nil;
  oField := Nil;
  iFieldWidth := 0;
  For iLoop := 0 To FDocument.Pieces.Count - 1 Do
    MeasurePiece(FDocument.Pieces[iLoop], bAll, oCell, oField, iFieldWidth);
  If Assigned(oCell) Then
    oCell.FinishContentMeasure;
End;


Procedure TWPRenderer.MeasurePiece(oPiece : TWPWorkingDocumentPiece; bAll : Boolean; Var oCell : TWPWorkingDocumentTableCellStartPiece; Var oField : TWPWorkingDocumentFieldStartPiece; Var iFieldWidth : Integer);
Begin
  Assert(Invariants('MeasurePiece', oPiece, TWPWorkingDocumentPiece, 'oPiece'));

  Case oPiece.PieceType Of
    ptText, ptLineBreak, ptPara:
      Begin
        If bAll Or Not oPiece.Metrics.Valid Then
        Begin
          MeasurePiece(oPiece, oPiece.VisualText);
          If (oPiece.PieceType = ptPara) Then
            CheckParagraphBounds(TWPWorkingDocumentParaPiece(oPiece));
        End;
        If Assigned(oCell) Then
        Begin
          oCell.MeasurePieceContent(oPiece);
          If (oPiece.PieceType = ptPara) Then
            oCell.SeeParagraph(TWPWorkingDocumentParaPiece(oPiece));
        End;
        If Assigned(oField) Then
          iFieldWidth := iFieldWidth + oPiece.Metrics.Width;
      End;
    ptFieldStart :
      Begin
        If bAll Or Not oPiece.Metrics.Valid Then
        Begin
          MeasurePiece(oPiece, oPiece.VisualText);
          If (oPiece.PieceType = ptPara) Then
            CheckParagraphBounds(TWPWorkingDocumentParaPiece(oPiece));
        End;
        If Assigned(oCell) Then
        Begin
          oCell.MeasurePieceContent(oPiece);
          If (oPiece.PieceType = ptPara) Then
            oCell.SeeParagraph(TWPWorkingDocumentParaPiece(oPiece));
        End;
        oField := TWPWorkingDocumentFieldStartPiece(oPiece);
        iFieldWidth := 0;
      End;
    ptFieldStop :
      Begin
        If bAll Or Not oPiece.Metrics.Valid Then
        Begin
          MeasurePiece(oPiece, oPiece.VisualText);
          If (oPiece.PieceType = ptPara) Then
            CheckParagraphBounds(TWPWorkingDocumentParaPiece(oPiece));
        End;
        If Assigned(oCell) Then
        Begin
          oCell.MeasurePieceContent(oPiece, IntegerMax(0, oField.MinWidthPixels - iFieldWidth));
          If (oPiece.PieceType = ptPara) Then
            oCell.SeeParagraph(TWPWorkingDocumentParaPiece(oPiece));
        End;
        oField := Nil;
        iFieldWidth := 0;
      End;
    ptImage :
      Begin
        If bAll Or Not oPiece.Metrics.Valid or (Width - SpaceForAnnotations <> oPiece.Metrics.MeasuredWidth) Then
          MeasureImage(TWPWorkingDocumentImagePiece(oPiece));
        If Assigned(oCell) Then
          oCell.MeasurePieceContent(oPiece);
      End;
    ptSectionStart : MeasureSection(TWPWorkingDocumentSectionStartPiece(oPiece));
    ptCellStart :
      Begin
        If Assigned(oCell) Then
          oCell.FinishContentMeasure;
        oCell := TWPWorkingDocumentTableCellStartPiece(oPiece);
        oCell.StartContentMeasure;
      End;
    ptCellStop : oCell := Nil;
    ptBreak :
      Begin
      oPiece.Metrics.OffsetCount := 1;
      oPiece.Metrics.Valid := True;
      End;
    ptSectionStop, ptTableStart, ptRowStart, ptTableStop, ptRowStop : oPiece.Metrics.Valid := True;
  Else
    RaiseError('MeasurePiece', 'Piece type '+NAMES_WPPIECETYPE[oPiece.PieceType]+' not measured yet');
  End;
End;


Procedure TWPRenderer.CheckParagraphBounds(oParagraph : TWPWorkingDocumentParaPiece);
Var
  oStyle : TWPStyle;
Begin
  oStyle := Styles.GetByNameOrDefault(oParagraph.Style);
  ApplyFont(oParagraph, oStyle);
  oParagraph.WorkingLeftIndent := 0;
  If oStyle.WorkingParagraphLeftIndent(oParagraph.Format) <> DEF_WORD Then
    oParagraph.WorkingLeftIndent := oStyle.WorkingParagraphLeftIndent(oParagraph.Format) * FIndentWidth;
  oParagraph.WorkingRightIndent := 0;
  If oStyle.WorkingParagraphRightIndent(oParagraph.Format) <> DEF_WORD Then
    oParagraph.WorkingRightIndent := oStyle.WorkingParagraphRightIndent(oParagraph.Format) * FIndentWidth;
  oParagraph.AdornmentWidth := 0;
  oParagraph.WorkingLeftIndent := oParagraph.WorkingLeftIndent + GetBulletWidth(oParagraph, oStyle) + GetNumberWidth(oParagraph, oStyle);
End;


Procedure TWPRenderer.MeasurePiece(oPiece : TWPWorkingDocumentPiece; Const sText : String);
Var
  aSize : TSize;
  aOffset : TWPSFontState;
  aMetrics : TTextMetric;
  oStyle : TWPStyle;
  aCapsState : TWPSCapsState;
  oField : TWPWorkingDocumentFieldStartPiece;
  iOffset : Integer;
  sValue : String;
Begin
  Assert(Invariants('MeasurePiece', oPiece, TWPWorkingDocumentPiece, 'oPiece'));

  sValue := sText;

  oStyle := Styles.GetByNameOrDefault(oPiece.Style);
  aOffset := ApplyFont(oPiece, oStyle);
  aSize := FCanvas.TextExtent(sValue);
  aMetrics := FCanvas.GetTextMetrics;
  oPiece.Metrics.Height := aSize.cy;
  oPiece.Metrics.Descent := aMetrics.tmDescent;
  If aOffset In [fsSuperscript, fsSubscript] Then
    FCanvas.Font.Size := FCanvas.Font.Size - FCanvas.Font.Size Div 2;

  aCapsState := oStyle.WorkingFontCapitalization(oPiece.Font);
  If (aCapsState = fcsAllCaps) Or (aCapsState = fcsSmallCaps) Then
    sValue := StringUpper(sValue)
  Else If aCapsState = fcsNoCaps Then
    sValue := Lowercase(sValue);

  oPiece.Metrics.OffsetCount := Length(sValue);
  If (Settings.FieldWrappers = wpfpHints) And (oPiece.PieceType In [ptFieldStart, ptFieldStop]) Then
    oPiece.Metrics.Offsets^[0] := 3
  Else
    FCanvas.GetTextExtents(sValue, aSize, oPiece.Metrics.Offsets);
  If Not Printing And (Canvas.Font.Italic Or (aMetrics.tmItalic <> 0)) And (sValue <> ' ') Then
    oPiece.Metrics.Offset[oPiece.Metrics.OffsetCount] := oPiece.Metrics.Offset[oPiece.Metrics.OffsetCount] + aMetrics.tmInternalLeading;  // it's supposed to be overhang, but that only applies when gdi does it. This is the nearest value and it seems to be good enough
  If oPiece.PieceType = ptFieldStart Then
  Begin
    oField := TWPWorkingDocumentFieldStartPiece(oPiece);
    If (oField.Width > 0) Then
    Begin
      aSize := FCanvas.TextExtent('WiapP');
      oField.MinWidthPixels := Trunc(aSize.cx * oField.Width / 5);
    End;
    If oField.Checkables.Count > 0 Then
    Begin
      oPiece.Metrics.Height := oPiece.Metrics.Height + 2;
      oPiece.Metrics.Descent := oPiece.Metrics.Descent + 1;
      oPiece.Metrics.OffsetCount := oField.Checkables.Count;
      For iOffset := 0 To oField.Checkables.Count - 1 Do
      Begin
        aSize := FCanvas.TextExtent(oField.Checkables[iOffset]);
        oPiece.Metrics.Offset[iOffset+1] := oPiece.Metrics.Offset[iOffset] + aSize.cx + (Canvas.PointSizeX * CHECK_TOTAL);
      End;
    End;
  End;
  oPiece.Metrics.Valid := True;
End;


Procedure TWPRenderer.MeasureImage(oImage : TWPWorkingDocumentImagePiece);
var
  scaleX, scaleY : Double;
Begin
  oImage.Metrics.OffsetCount := 1;
  case oImage.SizePolicy of
    ImageSizeManual:
      begin
      oImage.Metrics.Offset[1] :=  Trunc((oImage.Width + oImage.Border * 2) * Settings.Scale * Canvas.PointSizeX);
      oImage.Metrics.Height := Trunc((oImage.Height+oImage.Border * 2) * Settings.Scale * Canvas.PointSizeY);
      end;
    ImageSizePageWidth:
      begin
      // the image is width wide.
      // the height is scaled accordingly
      oImage.Metrics.Offset[1] :=  Width - SpaceForAnnotations;
      scaleY := (Width - SpaceForAnnotations) / oImage.Image.Width;
      oImage.Metrics.Height := Trunc(oImage.Image.Height * scaleY);

      oImage.Width := oImage.Metrics.Offset[1];
      oImage.Height := oImage.Metrics.Height;
      end;
    ImageSizeWholePage:
      begin
      // the image is either limited by width or height
      scaleY := (Width - SpaceForAnnotations) / oImage.Image.Width;
      scaleX := NominalPageHeight / oImage.Image.Height;
      if (scaleX > scaleY) then
        scaleX := scaleY
      else
        scaleY := scaleX;
      oImage.Metrics.Offset[1] :=  Trunc(oImage.Image.Width * scaleY);
      oImage.Metrics.Height := Trunc(oImage.Image.Height * scaleX);

      oImage.Width := oImage.Metrics.Offset[1];
      oImage.Height := oImage.Metrics.Height;
      end;
  end;
  oImage.Metrics.Descent := 0;
  oImage.Metrics.Valid := True;
  oImage.Metrics.MeasuredWidth := Width - SpaceForAnnotations;
End;


Procedure TWPRenderer.MeasureSection(oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  // nothing?
  oSection.Metrics.Valid := True;
End;


Procedure TWPRenderer.ClipContents(oRow : TWPMapRow; oClip : TWPMapContainer);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oRow.Items.Count - 1 Do
    oRow.Items[iLoop].Clip(oClip.InnerLeft, oClip.InnerTop, oClip.InnerRight, oClip.InnerBottom);
End;

Procedure TWPRenderer.ClipContents(oContainer : TWPMapContainer; oClip : TWPMapContainer = Nil);
Var
  iLoop : Integer;
Begin
  If Not Assigned(oClip) Then
    oClip := oContainer;
  For iLoop := 0 To oContainer.Rows.Count - 1 Do
    ClipContents(oContainer.Rows[iLoop], oClip);
  For iLoop := 0 To oContainer.Children.Count - 1 Do
    ClipContents(oContainer.Children[iLoop], oClip);
End;

Procedure TWPRenderer.LayoutDocument(oDocument : TWPWorkingDocument; oOperationRange : TOperationRange);
Var
  aBounds : TWPBounds;
  aState : TWPRenderState;
  iCursor : Integer;
  iLine : Integer;
  iColWidth : Integer;
  oCurrent : TWPWorkingDocumentPiece;
Begin
  FStateStack.Initialise(Settings, tsFalse, ReadOnlyColour);

  aBounds.Left := 0 + Settings.HorizontalMargin;
  aBounds.Top := 0 + Settings.VerticalMargin;
  aBounds.Right := Width - Settings.HorizontalMargin - SpaceForAnnotations;
  aState.Completed := False;
  aState.VerticalOffset := 0;

  FMap.MarginLeft := Settings.HorizontalMargin;
  FMap.MarginRight := Settings.HorizontalMargin;
  FMap.MarginTop := Settings.VerticalMargin;
  FMap.MarginBottom := Settings.VerticalMargin;
  FMap.Left := 0;
  FMap.Top := 0;
  FMap.Width := Width - SpaceForAnnotations;

  iCursor := 0;
  iLine := 0;

  If Document.Pieces.Count > 0 Then
  Begin
    oCurrent := Document.Pieces[0];

    While Not aState.Completed And Assigned(oCurrent) Do
    Begin
      Case oCurrent.PieceType Of
        ptSectionStart : aBounds.Top := LayoutSection(aBounds, oCurrent, FMap, oOperationRange, iCursor, aState, iLine);
        ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptPara : aBounds.Top := LayoutPara(aBounds, oCurrent, FMap, oOperationRange, 0, iCursor, aState, iLine, iColWidth);
        ptTableStart : aBounds.Top := LayoutTable(aBounds, oCurrent, FMap, iCursor, iLine);
        ptBreak : aBounds.Top := LayoutBreak(aBounds, oCurrent, FMap, 0, iCursor, aState, iLine, iColWidth);
      Else
        // ptRow, ptCell, ptTableEnd
        RaiseError('LayoutDocument', 'Unexpected Piece Type '+NAMES_WPPIECETYPE[oCurrent.PieceType]);
      End;

      If aState.Completed And (aState.VerticalOffset <> 0) Then
        AdjustHeightOfChildren(FMap, iCursor, aState.VerticalOffset, aState.LineOffset);
    End;
  End;

  If aState.Completed Then
    aBounds.Top := FMap.Children[FMap.Children.Count - 1].Bottom
  Else
    TrimContainer(FMap, iCursor);

  FMap.Height := aBounds.Top + Settings.VerticalMargin;
End;


Function TWPRenderer.LayoutBreak(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; iColumnOffset : Integer; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;
Var
  oBreak : TWPWorkingDocumentBreakPiece;
  oRow : TWPMapRow;
  oItem : TWPMapItem;
Begin
  oBreak := TWPWorkingDocumentBreakPiece(oCurrent);

  If Not oBreak.HasContainer Then
  Begin
    oBreak.Container := TWPMapContainer.Create;
    oBreak.Container.Piece := oBreak;
  End;
  AddToContainer(oContainer, oBreak.Container, iContainerCursor);

  oBreak.Container.Left := aBounds.Left;
  oBreak.Container.Top := aBounds.Top;
  oBreak.Container.Width := aBounds.Right - aBounds.Left;
  FStateStack.SetCurrentState(oBreak);
  oBreak.Container.Height := GetSectionHeaderHeight + 4; // margin of 2 on either side
  Result := oBreak.Container.Bottom;
  oCurrent := oCurrent.Next;

  // for convenience, we add a row and a map - helps with the interactive editing
  oRow := TWPMapRow.Create;
  Try
    oRow.Line := iLine;
    Inc(iLine);
    oRow.ColumnOffset := iColumnOffset;
    oRow.Copy(oBreak.Container);
    oRow.Parent := oBreak.Container;
    oItem := TWPMapItem.Create;
    Try
      oItem.Copy(oRow);
      oItem.OffsetStart := 0;
      oItem.OffsetLength := 1;
      oItem.Piece := oBreak;
      oItem.Parent := oRow;
      oBreak.Map := oItem.Link;
      oRow.Items.Add(oItem.Link);
    Finally
      oItem.Free;
    End;
    oBreak.Container.Rows.Add(oRow.Link);
  Finally
    oRow.Free;
  End;
  iMaxRowCols := 1;
End;

Function TWPRenderer.LayoutSection(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; oOperationRange : TOperationRange; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer) : Integer;
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
  iCursor : Integer;
  iColWidth : Integer;
  iMin : Integer;
Begin
  oSection := TWPWorkingDocumentSectionStartPiece(oCurrent);

  FStateStack.PushState(oSection, DEF_COLOUR);
  If Not oSection.HasContainer Then
    oSection.Container := TWPMapContainer.Create;
  oSection.Container.Piece := oSection;
  AddToContainer(oContainer, oSection.Container, iContainerCursor);
  oSection.Container.Parent := oContainer;
  oSection.Container.Left := aBounds.Left;
  oSection.Container.Top := aBounds.Top;
  oSection.Container.Width := aBounds.Right - aBounds.Left;
  FStateStack.SetCurrentState(oSection);
  iMin := 0;

  If (Not Printing And oSection.IsField) Then
  Begin
    oSection.Container.MarginLeft := GetFieldCharWidth;
    oSection.Container.MarginRight := GetFieldCharWidth;
    aBounds.Left := aBounds.Left + GetFieldCharWidth;
    aBounds.Right := aBounds.Right - GetFieldCharWidth;
    iMin := GetFieldCharHeight;
  End
  Else
    Case oSection.DisplayType Of
      sdtLine : oSection.Container.MarginTop := 2;
      sdtName : oSection.Container.MarginTop := 2 + GetSectionHeaderHeight;
    Else //  sdtNone
    End;
  Inc(aBounds.Top, oSection.Container.MarginTop);

  oCurrent := oCurrent.Next; // skip over current section
  iCursor := 0;

  While Not aState.Completed And Assigned(oCurrent) And (oCurrent.PieceType <> ptSectionStop) Do
    Begin
    Case oCurrent.PieceType Of
      ptSectionStart : aBounds.Top := LayoutSection(aBounds, oCurrent, oSection.Container, oOperationRange, iCursor, aState, iLine);
      ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptPara : aBounds.Top := LayoutPara(aBounds, oCurrent, oSection.Container, oOperationRange, 0, iCursor, aState, iLine, iColWidth);
      ptTableStart : aBounds.Top := LayoutTable(aBounds, oCurrent, oSection.Container, iCursor, iLine);
      ptBreak : aBounds.Top := LayoutBreak(aBounds, oCurrent, oSection.Container, 0, iCursor, aState, iLine, iColWidth);
    Else
      // ptRow, ptCell, ptTableEnd
      RaiseError('LayoutDocument', 'Unexpected Piece Type '+NAMES_WPPIECETYPE[oCurrent.PieceType]);
    End;
    If aState.Completed And (aState.VerticalOffset <> 0) Then
      AdjustHeightOfChildren(oSection.Container, iCursor, aState.VerticalOffset, aState.LineOffset);
    End;
  Assert(Invariants('LayoutSection', oCurrent, TWPWorkingDocumentPiece, 'Found End of Document expecting end of section'));
  oCurrent := oCurrent.Next; // skip Section End

  If aState.Completed Then
    aBounds.Top := oSection.Container.Children[oSection.Container.Children.Count - 1].Bottom
  Else
    TrimContainer(oSection.Container, iCursor);

  // add a margin at the bottom of the section
  oSection.Container.MarginBottom := DEFAULT_SECTION_BOTTOM_MARGIN_POINTS * Canvas.PointSizeY;
  Inc(aBounds.Top, oSection.Container.MarginBottom);

  If (aBounds.Top - oSection.Container.Top) < iMin Then
    aBounds.Top := oSection.Container.Top + iMin;

  oSection.Container.Height := aBounds.Top - oSection.Container.Top;
  Result := aBounds.Top;
  FStateStack.PopState;
End;


Function TWPRenderer.LayoutTableCell(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oMetrics : TWPTableColumnMetrics; oContainer : TWPMapContainer; oCells : TWPWorkingDocumentTableCellStartPieces; iColumnOffset : Integer; Var iMetricIndex : Integer; Var iBottom : Integer; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;
Var
  oTableCell : TWPWorkingDocumentTableCellStartPiece;
  iWidth : Integer;
  iLoop : Integer;
  iCursor : Integer;
  aState : TWPRenderState;
  iColWidth : Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oTableCell := TWPWorkingDocumentTableCellStartPiece(oCurrent);
  oRow := TWPWorkingDocumentTableRowStartPiece(oTableCell.Row);

  FStateStack.PushState(oTableCell, oTableCell.Background);
  If Not oTableCell.HasContainer Then
  Begin
    oTableCell.Container := TWPMapContainer.Create;
    oTableCell.Container.Piece := oTableCell;
    oContainer.Children.Insert(oRow.Cells.IndexByReference(oTableCell), oTableCell.Container.Link);
    oTableCell.Container.Parent := oContainer;
  End;
  oTableCell.Container.Left := aBounds.Left;
  oTableCell.Container.Top := aBounds.Top;
  FStateStack.SetCurrentState(oTableCell);

  iWidth := 0;
  For iLoop := 1 To oTableCell.Span Do
  Begin
    Inc(iWidth, oMetrics[iMetricIndex].Actual);
    Inc(iMetricIndex);
  End;
  oTableCell.Container.Width := iWidth;

  If (oTableCell.State In [tisFirst, tisOnly]) Then
    oTableCell.Container.Width := oTableCell.Container.Width - oRow.Depth * Settings.NestingIndent;

  oTableCell.Container.MarginLeft := (oTableCell.WorkingMarginLeft + oTableCell.WorkingLeftBorder.ActualWidth) * Canvas.PointSizeX;
  oTableCell.Container.MarginTop := (oTableCell.WorkingMarginTop + oTableCell.WorkingTopBorder.ActualWidth) * Canvas.PointSizeY;
  oTableCell.Container.MarginRight := (oTableCell.WorkingMarginRight + oTableCell.WorkingRightBorder.ActualWidth) * Canvas.PointSizeX;
  oTableCell.Container.MarginBottom := (oTableCell.WorkingMarginBottom + oTableCell.WorkingBottomBorder.ActualWidth) * Canvas.PointSizeY;
  oCells.Add(oTableCell.Link);

  aBounds.Left := oTableCell.Container.InnerLeft;
  aBounds.Right := oTableCell.Container.InnerRight;
  aBounds.Top := oTableCell.Container.InnerTop;
  iCursor := 0;
  iMaxRowCols := 0;

  oCurrent := oCurrent.Next;
  While Assigned(oCurrent) And (oCurrent.PieceType <> ptCellStop) Do
    Begin
    Case oCurrent.PieceType Of
      ptBreak : aBounds.Top := LayoutBreak(aBounds, oCurrent, oTableCell.Container, iColumnOffset, iCursor, aState, iLine, iColWidth);
      ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptPara : aBounds.Top := LayoutPara(aBounds, oCurrent, oTableCell.Container, Nil, iColumnOffset, iCursor, aState, iLine, iColWidth);
    Else
      // ptRow, ptCell, ptTableEnd
      RaiseError('LayoutTableCell', 'Unexpected Piece Type '+NAMES_WPPIECETYPE[oCurrent.PieceType]);
    End;
    iMaxRowCols := IntegerMax(iMaxRowCols, iColWidth);
    End;
  oCurrent := oCurrent.Next;
  TrimContainer(oTableCell.Container, iCursor);

  aBounds.Top := aBounds.Top + oTableCell.Container.MarginBottom;

  If aBounds.Top > iBottom Then
    iBottom := aBounds.Top;
  oTableCell.Container.Height := aBounds.Top - oTableCell.Container.Top;
  Result := oTableCell.Container.Right;
  FStateStack.PopState;
End;


Function TWPRenderer.LayoutTableRow(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oMetrics : TWPTableColumnMetrics; oContainer : TWPMapContainer; Var iLine : Integer) : Integer;
Var
  oTableRow : TWPWorkingDocumentTableRowStartPiece;
  oCells : TWPWorkingDocumentTableCellStartPieces;
  iBottom : Integer;
  iMetrics : Integer;
  iLoop : Integer;
  iFirstLine : Integer;
  iLastLine : Integer;
  oCell : TWPWorkingDocumentTableCellStartPiece;
  iColumnOffset : Integer;
  iColWidth : Integer;
  iDiff : Integer;
  iInner : Integer;
Begin
  oTableRow := TWPWorkingDocumentTableRowStartPiece(oCurrent);

  FStateStack.PushState(oTableRow, oTableRow.Background);
  If (Not oTableRow.HasContainer) Then
  Begin
    oTableRow.Container := TWPMapContainer.Create;
    oTableRow.Container.Piece := oTableRow;
  End;
  If TWPWorkingDocumentTableStartPiece(oTableRow.Table).StructureDirty Then
  Begin
    oContainer.Children.Add(oTableRow.Container.Link);
    oTableRow.Container.Parent := oContainer;
    oTableRow.Container.Children.Clear;
    for iLoop := 0 to oTableRow.Cells.Count - 1 Do
      oTableRow.Cells[iLoop].Container := nil;
  End;

  oTableRow.Container.ChildrenHorizontal := True;
  aBounds.Left := aBounds.Left + oTableRow.Depth * Settings.NestingIndent;
  oTableRow.Container.Left := aBounds.Left;
  oTableRow.Container.Top := aBounds.Top;
  oTableRow.Container.Width := aBounds.Right - aBounds.Left;
  FStateStack.SetCurrentState(oTableRow);

  iBottom := aBounds.Top;
  iMetrics := 0;

  iFirstLine := iLine;
  iLastLine := iLine;
  iColumnOffset := 0;

  oCells := TWPWorkingDocumentTableCellStartPieces.Create(False);
  Try
    oCurrent := oCurrent.Next;
    While Assigned(oCurrent) And (oCurrent.PieceType <> ptRowStop) Do
    Begin
      iLine := iFirstLine;
      If oCurrent.PieceType = ptCellStart Then
        aBounds.Left := LayoutTableCell(aBounds, oCurrent, oMetrics, oTableRow.Container, oCells, iColumnOffset, iMetrics, iBottom, iLine, iColWidth)
      Else
        RaiseError('LayoutTableRow', 'Unexpected content '+NAMES_WPPIECETYPE[oCurrent.PieceType]+' in table');
      iLastLine := IntegerMax(iLastLine, iLine);
      Inc(iColumnOffset, iColWidth);
    End;

    oCurrent := oCurrent.Next;
    For iLoop := 0 To oCells.Count - 1 Do
    Begin
      oCell := oCells[iLoop];
      oCell.Container.Height := iBottom - oCell.Container.Top;
      ClipContents(oCell.Container);
      If oCell.Container.Children.Count > 0 Then
      Begin
        iDiff := oCell.Container.InnerBottom - oCell.Container.Children[oCell.Container.Children.Count-1].Bottom;
        Case oCell.VerticalAlignment Of
          VerticalAlignmentTop : iDiff := 0;
          VerticalAlignmentCentered : iDiff := iDiff Div 2;
        End;
        If iDiff <> 0 Then
        Begin
          For iInner := 0 To oCell.Container.Children.Count - 1 Do
            oCell.Container.Children[iInner].AdjustTop(iDiff, 0, True);
        End;
      End;
    End;
  Finally
    oCells.Free;
  End;

  If oTableRow.Container.Children.Count = 0 Then
    RaiseError('LayoutTableRow', 'Row has no children');
  iBottom := iBottom + oTableRow.LowerPaddingSize;
  oTableRow.Container.Height := (iBottom - oTableRow.Container.Top);
  Result := iBottom;
  iLine := iLastLine;
  FStateStack.PopState;
End;


Function TWPRenderer.LayoutTable(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; Var iContainerCursor : Integer; Var iLine : Integer) : Integer;
Var
  oTable : TWPWorkingDocumentTableStartPiece;
  oMetrics : TWPTableColumnMetrics;
  oCalc : TWPRendererTableColumnSizeCalculator;
Begin
  oTable := TWPWorkingDocumentTableStartPiece(oCurrent);

  FStateStack.PushState(oTable, oTable.Background);
  oMetrics := TWPTableColumnMetrics.Create;
  Try
    oCalc := TWPRendererTableColumnSizeCalculator.Create(oTable.Link, oMetrics.Link, Canvas.PointSizeX, aBounds.Right - aBounds.Left);
    Try
      oCalc.NestingIndent := Settings.NestingIndent;
      oCalc.Calculate;
    Finally
      oCalc.Free;
    End;

    If Not oTable.HasContainer Then
    Begin
      oTable.Container := TWPMapContainer.Create;
      oTable.Container.Piece := oTable;
    End;
    If oTable.StructureDirty Then
      oTable.Container.Children.Clear;

    AddToContainer(oContainer, oTable.Container, iContainerCursor);
    oTable.Container.Parent := oContainer;
    oTable.Container.Left := aBounds.Left;
    oTable.Container.Top := aBounds.Top;
    oTable.Container.Width := aBounds.Right - aBounds.Left;
    oTable.Container.MarginRight :=  oTable.Container.Width - oMetrics.SumActual;
    FStateStack.SetCurrentState(oTable);

    aBounds.Left := oTable.Container.Left;
    aBounds.Right := oTable.Container.InnerRight;

    oCurrent := oCurrent.Next;
    While Assigned(oCurrent) And (oCurrent.PieceType <> ptTableStop) Do
    Begin
      If oCurrent.PieceType = ptRowStart Then
        aBounds.Top := LayoutTableRow(aBounds, oCurrent, oMetrics, oTable.Container, iLine)
      Else
        RaiseError('LayoutTable', 'Unexpected content '+NAMES_WPPIECETYPE[oCurrent.PieceType]+' in table');
    End;
  Finally
    oMetrics.Free;
  End;
  oCurrent := oCurrent.Next; // jump table end
  oTable.StructureDirty := False;

  oTable.Container.Height := aBounds.Top - oTable.Container.Top;
  Result := aBounds.Top;
  FStateStack.PopState;
End;


Function TWPRenderer.CurrentParagraph(oCurrent : TWPWorkingDocumentPiece) : TWPWorkingDocumentParaPiece;
Var
  oTemp : TWPWorkingDocumentPiece;
Begin
  oTemp := oCurrent;
  While Assigned(oTemp) And (oTemp.pieceType <> ptPara) Do
    oTemp := oTemp.Next;

  Result := oTemp as TWPWorkingDocumentParaPiece;
End;


Function TWPRenderer.LayoutPara(aBounds : TWPBounds; Var oCurrent : TWPWorkingDocumentPiece; oContainer : TWPMapContainer; oOperationRange : TOperationRange; iColumnOffset : Integer; Var iContainerCursor : Integer; Var aState : TWPRenderState; Var iLine : Integer; Out iMaxRowCols : Integer) : Integer;
Var
  oParagraph : TWPWorkingDocumentParaPiece;
  oContext : TWPRendererParagraphContext;
  iLoop : Integer;
  oRow : TWPMapRow;
  iMargin : Integer;
  bPaint : Boolean;
  oStyle : TWPStyle;
  oField : TWPWorkingDocumentFieldStartPiece;
  iTail, iNose : Integer;
  iFieldWidth : Integer;
  bShowField : Boolean;
Begin
  Assert(Invariants('LayoutPara', oCurrent, TWPWorkingDocumentPiece, 'oCurrent'));

  oField := Nil;
  iFieldWidth := 0;

  oParagraph := CurrentParagraph(oCurrent);
  FStateStack.PushState(oParagraph, DEF_COLOUR);
  oStyle := Styles.GetByNameOrDefault(oParagraph.Style);

  Assert(Invariants('LayoutPara', oParagraph, TWPWorkingDocumentParaPiece, 'oParagraph'));

  iMaxRowCols := 0;

  If Not oParagraph.HasContainer Then
  Begin
    oParagraph.Container := TWPMapContainer.Create;
    oParagraph.Container.Piece := oParagraph;
  End;

  AddToContainer(oContainer, oParagraph.Container, iContainerCursor);

  If Not (oOperationRange.Valid And
     ((oParagraph.Metrics.Position < oOperationRange.Start) Or (oCurrent.Metrics.Position > oOperationRange.Stop)) And
     (aBounds.Left = oParagraph.Container.Left) And (aBounds.Right = oParagraph.Container.Right) And (oParagraph.Container.Background = FStateStack.WorkingBackground)) Then
  Begin
    oParagraph.Container.Left := aBounds.Left;
    oParagraph.Container.Top := aBounds.Top;
    oParagraph.Container.Width := aBounds.Right - aBounds.Left;
    FStateStack.SetCurrentState(oParagraph);
    CalculateParagraphMargins(oParagraph, aBounds);

    oParagraph.Container.Rows.Clear;

    oContext := TWPRendererParagraphContext.Create(oParagraph.Container.Link, FStateStack.Link);
    Try
      While oCurrent.PieceType <> ptPara Do
        Begin
        iTail := 0;
        iNose := 0;
        bShowField := True;
        If oCurrent.PieceType  = ptFieldStart Then
        Begin
          oField := TWPWorkingDocumentFieldStartPiece(oCurrent);
          iFieldWidth := 0;
          If Settings.FormsMode And (oField.Checkables.Count = 0) Then
            FStateStack.PushState(oField, clWhite)
          Else
            FStateStack.PushState(oField);
          bShowField := (Settings.FieldWrappers <> wpfpNone) Or (oField.Checkables.Count > 0);
        End
        Else If oCurrent.PieceType = ptFieldStop Then
        Begin
          Assert(Invariants('layoutPara', oField, TWPWorkingDocumentFieldStartPiece, 'Previously observed field start'));
          TWPWorkingDocumentFieldStopPiece(oCurrent).MatchingStart := oField.Link;
          iNose := IntegerMax(0, oField.MinWidthPixels - iFieldWidth);
          If Not Printing And oField.HasDefinitionProvider And oField.DefinitionProvider.hasUIAssistance(oField.DocField) Then
            iTail := FIELD_DROPPER_WIDTH;
          bShowField := (Settings.FieldWrappers <> wpfpNone) And (oField.Checkables.Count = 0);
          oField := Nil;
        End
        Else If oField <> Nil Then
          Inc(iFieldWidth, oCurrent.Metrics.Width);

        If bShowField Then
          AddPieceToPara(oCurrent, oContext, iNose, iTail)
        Else
          oCurrent.Maps.Clear;
        If oCurrent.PieceType = ptFieldStop Then
          FStateStack.PopState;
        oCurrent := oCurrent.Next;
        End;
      AddPieceToPara(oCurrent, oContext, 0, 0);
      oCurrent := oCurrent.Next;
      oContext.Finish;
    Finally
      oContext.Free;
    End;

    Result := oParagraph.Container.InnerTop;
    For iLoop := 0 To oParagraph.Container.Rows.count - 1 Do
      Begin
      oRow := oParagraph.Container.Rows[iLoop];
      oRow.Line := iLine;
      oRow.ColumnOffset := iColumnOffset;
      Inc(iLine);
      iMaxRowCols := IntegerMax(iMaxRowCols, oRow.ColCount);
      oRow.Top := Result;
      LayoutRow(oParagraph, oParagraph.Container.Rows[iLoop], iLoop = oParagraph.Container.Rows.count - 1, oStyle);
      Inc(Result, oParagraph.Container.Rows[iLoop].Height);
      End;
    If oStyle.WorkingParagraphMarginBottom(oParagraph.Format) <> DEF_WORD Then
      iMargin := oStyle.WorkingParagraphMarginBottom(oParagraph.Format) * Canvas.PointSizeY
    Else If oParagraph.HasNext And (oParagraph.Next.PieceType = ptCellStop) Then
      iMargin := 0
    Else
      iMargin := DEFAULT_PARAGRAPH_MARGIN_BOTTOM * Canvas.PointSizeY;
    oParagraph.Container.MarginBottom := iMargin;
    Inc(Result, iMargin);
    oParagraph.Container.Rows[oParagraph.Container.Rows.count - 1].Height := oParagraph.Container.Rows[oParagraph.Container.Rows.count - 1].Height + iMargin;
    oParagraph.Container.Height := Result - oParagraph.Container.Top;
  End
  Else
  Begin
    If (oCurrent.Metrics.Position > oOperationRange.Stop) Then
    Begin
      aState.Completed := True;
      aState.VerticalOffset := aBounds.Top - oParagraph.Container.Top;
      aState.LineOffset := iLine - oParagraph.Container.Rows.First.Line;
      If aState.VerticalOffset <> 0 Then
        MarkVerticalAdjustment(oParagraph.Container.Top, aState.VerticalOffset);
    End
    Else
    Begin
      oCurrent:= oParagraph.Next;
      iLine := oParagraph.Container.Rows.Last.Line + 1;
    End;
    If aState.VerticalOffset <> 0 Then
      oParagraph.Container.AdjustTop(aState.VerticalOffset, aState.LineOffset, False);
    Result := oParagraph.Container.Bottom;
  End;
  bPaint := False;
  For iLoop := 0 To oParagraph.Container.Rows.Count - 1 Do
    bPaint := bPaint Or Not oParagraph.Container.Rows[iLoop].Painted;
  If bPaint Then
    oParagraph.Container.WantPainting;
  FStateStack.PopState;
End;


Procedure TWPRenderer.CalculateParagraphMargins(oParagraph : TWPWorkingDocumentParaPiece; Var aBounds : TWPBounds);
Begin
  Inc(aBounds.Left, oParagraph.WorkingLeftIndent);
  Dec(aBounds.Right, oParagraph.WorkingRightIndent);
  oParagraph.Container.MarginLeft := oParagraph.WorkingLeftIndent;
  oParagraph.Container.MarginRight := oParagraph.WorkingRightIndent;
End;

Function NextCeiling(value, increment : Integer) : Integer;
Begin
  Result := 0;
  While Result < value Do
    Inc(Result, increment);
End;

Function TWPRenderer.GetBulletWidth(oParagraph: TWPWorkingDocumentParaPiece; oStyle : TWPStyle): Integer;
Begin
  If oStyle.WorkingParagraphListType(oParagraph.Format) = WPSParagraphListTypeBullets Then
    Begin
    ApplyFont(oParagraph, oStyle);
    FCanvas.Font.ClearStyles;
    FCanvas.Font.Name := 'Wingdings';
    FCanvas.Font.Size := Trunc(FCanvas.Font.Size * BULLET_SCALE_FACTOR);
    Result := FCanvas.TextWidth(GetBulletChar(oParagraph.Format, oStyle)+' ');
    Result := NextCeiling(Result, FIndentWidth);
    oParagraph.AdornmentWidth := Result;
    End
  Else
    Result := 0;
End;


Function TWPRenderer.GetNumberWidth(oParagraph: TWPWorkingDocumentParaPiece; oStyle : TWPStyle): Integer;
Begin
  If oStyle.WorkingParagraphListType(oParagraph.Format) = WPSParagraphListTypeNumbers Then
    Begin
    ApplyFont(oParagraph, oStyle);
    Result := FCanvas.TextWidth(GetNumberFormat(oParagraph, oStyle, oParagraph.ListNumber)+'  ');
    Result := NextCeiling(Result, FIndentWidth);
    oParagraph.AdornmentWidth := Result;
    End
  Else
    Result := 0;
End;

Function ToAlpha(iValue : Integer):String;
Var
  iChunk : Integer;
Begin
  Result := '';
  Repeat
    Dec(iValue);
    iChunk := iValue Mod 26;
    iValue := (iValue - iChunk) Div 26;
    Result := chr(ord('A') + iChunk) + Result;
  Until iValue = 0;
End;

Function ToRoman(iValue : Integer):String;
Const
  Arabics: Array[1..13] Of Integer = (1,4,5,9,10,40,50,90,100,400,500,900,1000);
  Romans:  Array[1..13] Of String = ('I','IV','V','IX','X','XL', 'L','XC','C','CD','D','CM','M');
Var
  i: Integer;
Begin
 For i := 13 DownTo 1 Do
   While (iValue >= Arabics[i]) Do
   Begin
     iValue := iValue - Arabics[i];
     Result := Result + Romans[i];
   End;
End;


Function TWPRenderer.GetBulletChar(oFormat: TWPSParagraphDetails; oStyle : TWPStyle): Char;
Begin
  Result := CHAR_BULLETS[oStyle.WorkingParagraphBulletType(oFormat)];
End;


Function TWPRenderer.GetNumberFormat(oParagraph: TWPWorkingDocumentParaPiece; oStyle : TWPStyle; iValue: Integer): String;
Begin
  If iValue < 0 Then
    iValue := 1;

  Case oStyle.WorkingParagraphNumberType(oParagraph.Format) Of
    tnLowerAlpha : Result := Lowercase(ToAlpha(iValue));
    tnUpperAlpha : Result := ToAlpha(iValue);
    tnLowerRoman : Result := Lowercase(ToRoman(iValue));
    tnUpperRoman : Result := ToRoman(iValue);
  Else // tnUnknown, tnArabic
    Result := IntegerToString(iValue);
  End;

  Case oStyle.WorkingParagraphNumberFormat(oParagraph.Format) Of
    nwDot : Result := Result + '.';
    nwSlash : Result := Result + '/';
    nwParenthesis : Result := '('+Result+')';
    nwColon : Result := Result + ':';
    nwSemiColon : Result := Result + ';';
  Else
    // nothing
  End;
End;


Procedure TWPRenderer.AddPieceToPara(oPiece : TWPWorkingDocumentPiece; oContext : TWPRendererParagraphContext; iNose, iTail : Integer);
Var
  oItem : TWPMapItem;
Begin
  If Not ((oPiece.PieceType In INLINE_PIECE_TYPES) Or (oPiece.PieceType = ptPara)) Then
    RaiseError('AddPieceToPara', 'Encountered a piece that is not in a paragraph (usual cause, improperly closed paragraph) ['+NAMES_WPPIECETYPE[oPiece.PieceType]+' at '+IntegerToString(oPiece.Metrics.Position)+']');

  If oPiece.Maps.Count <> 1 Then
    oPiece.Map := TWPMapItem.Create(oPiece);
  oItem := TWPMapItem(oPiece.Maps[0]);

  oItem.Descent := oPiece.Metrics.Descent;
  oItem.Height := oPiece.Metrics.Height;
  FStateStack.SetCurrentState(oPiece, oItem);

  If oPiece.Metrics.Offsetcount = 0 Then
    RaiseError('AddPieceToPara', 'Metrics OffsetCount = 0 on '+oPiece.ClassName);

  oItem.OffsetStart := 0;
  oItem.OffsetLength := Length(oPiece.VisualText);
  oItem.Width := iNose + oPiece.Metrics.Offset[oItem.OffsetLength] + iTail;
  oItem.Nose := iNose;

  If oPiece.PieceType = ptLineBreak Then
    oItem.BreakState := bsBreak
  Else If oPiece.PieceType = ptPara Then
    oItem.BreakState := bsPara
  Else If oPiece.VisualText = ' ' Then
    oItem.BreakState := bsWhitespace
  Else
    oItem.BreakState := bsText;

  oContext.AddItem(oItem);
End;


Procedure TWPRenderer.LayoutRow(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; bLast : Boolean; oStyle : TWPStyle);
Var
  iLimit : Integer;
  iLoop : Integer;
  iWork : Integer;
  bPaint : Boolean;
  oItem : TWPMapItem;
  iAnnotationColour : TColour;
Begin
  iLimit := oRow.Items.Count - 1;
  While (iLimit >= 0) And ((TWPWorkingDocumentPiece(oRow.Items[iLimit].Piece).PieceType = ptPara) Or (TWPWorkingDocumentPiece(oRow.Items[iLimit].Piece).PieceType = ptLineBreak) Or (TWPWorkingDocumentPiece(oRow.Items[iLimit].Piece).VisualText = ' ')) Do
    Dec(iLimit);

  oRow.Left := oParagraph.Container.InnerLeft;
  oRow.Width := oParagraph.Container.InnerWidth;

  Case oStyle.WorkingParagraphAlign(oParagraph.Format) Of
    WordProcessorParagraphAlignmentCentre : iWork := AlignRowMiddle(oParagraph, oRow, iLimit);
    WordProcessorParagraphAlignmentRight : iWork := AlignRowRight(oParagraph, oRow, iLimit);
    WordProcessorParagraphAlignmentJustify :
      If bLast Then
        iWork := AlignRowLeft(oParagraph, oRow, iLimit)
      Else
        iWork := AlignRowJustified(oParagraph, oRow, iLimit);
  Else //   taUnknown, taLeft
    iWork := AlignRowLeft(oParagraph, oRow, iLimit);
  End;
  For iLoop := iLimit + 1 To oRow.Items.Count - 1 Do
    Begin
    oItem := oRow.Items[iLoop];
    oItem.Left := iWork;
    Inc(iWork, oItem.Width);
    End;
  AdjustHeights(oRow);
  bPaint := False;
  For iLoop := 0 To oRow.Items.Count - 1 Do
  Begin
    bPaint := bPaint Or Not oRow.Items[iLoop].Painted;
    If TWPWorkingDocumentPiece(oRow.Items[iLoop].Piece).AnnotationStatus in [AnnotationStatusEnd, AnnotationStatusAll] Then
      Document.AllAnnotations[TWPWorkingDocumentPiece(oRow.Items[iLoop].Piece).AnnotationId-1].Anchor := oRow.Bottom-1;
  End;
  If bPaint Or (oRow.FirstLeft > oRow.PaintedFirstLeft) Or (oRow.PaintedLastRight = 0) Or (oRow.LastRight < oRow.PaintedLastRight) Then
    oParagraph.Map.WantPainting;
  iAnnotationColour := clTransparent;
  For iLoop := 0 to oRow.Items.Count - 1 Do
  Begin
    oRow.Items[iLoop].AnnotationColour := iAnnotationColour;
    If TWPWorkingDocumentPiece(oRow.Items[iLoop].Piece).AnnotationStatus in [AnnotationStatusEnd, AnnotationStatusAll] then
      iAnnotationColour := Document.AllAnnotations[TWPWorkingDocumentPiece(oRow.Items[iLoop].Piece).AnnotationId-1].Colour;
  End;
End;


Function TWPRenderer.AlignRowLeft(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
Var
  iLoop : Integer;
  oItem : TWPMapItem;
Begin
  Result := oRow.Left;
  For iLoop := 0 To iLimit Do
    Begin
    oItem := oRow.Items[iLoop];
    oItem.Left := Result;
    Inc(Result, oItem.Width);
    End;
End;


Function TWPRenderer.AlignRowMiddle(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
Var
  iGap : Integer;
  iLoop : Integer;
  oItem : TWPMapItem;
Begin
  iGap := oRow.Width - oRow.SumWidths(iLimit);
  Result := oRow.Left + iGap Div 2;
  For iLoop := 0 To iLimit Do
    Begin
    oItem := oRow.Items[iLoop];
    oItem.Left := Result;
    Inc(Result, oItem.Width);
    End;
End;


Function TWPRenderer.AlignRowRight(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
Var
  iWork : Integer;
  iLoop : Integer;
  oItem : TWPMapItem;
Begin
  iWork := oRow.Right;
  For iLoop := iLimit DownTo 0 Do
    Begin
    oItem := oRow.Items[iLoop];
    Dec(iWork, oItem.Width);
    oItem.Left := iWork;
    End;
  Result := oRow.Right;
End;


Function TWPRenderer.AlignRowJustified(oParagraph : TWPWorkingDocumentParaPiece; oRow : TWPMapRow; iLimit : Integer) : Integer;
Var
  iLoop : Integer;
  iCount : Integer;
  iGap : Integer;
  rSpace : Real;
  rWork : Real;
  oItem : TWPMapItem;
Begin
  iGap := oRow.Width - oRow.SumWidths(iLimit);
  iCount := 0;
  For iLoop := 0 To iLimit Do
   If oRow.Items[iLoop].BreakState = bsWhitespace Then
     Inc(iCount);
  If iCount = 0 Then
    Result := AlignRowLeft(oParagraph, oRow, iLimit)
  Else
    Begin
    rSpace := iGap / iCount;
    rWork := oRow.Left;
    For iLoop := 0 To iLimit Do
      Begin
      oItem := oRow.Items[iLoop];
      oItem.Left := Trunc(rWork);
      If iLoop > 0 Then
        oRow.Items[iLoop-1].Width := oItem.Left - oRow.Items[iLoop-1].Left;
      rWork := rWork + oItem.Width;
      If oItem.BreakState = bsWhitespace Then
        rWork := rWork + rSpace;
      End;
    Result := oRow.Right;
    End;
End;


Procedure TWPRenderer.AdjustHeights(oRow : TWPMapRow);
Var
  iHeight : Integer;
  iDescent : Integer;
  iLoop : Integer;
  oItem : TWPMapItem;
Begin
  iHeight := 0;
  iDescent := 0;
  For iLoop := 0 To oRow.Items.Count - 1 Do
    Begin
    oItem := oRow.Items[iLoop];
    iHeight := IntegerMax(iHeight, oItem.Height - oItem.Descent);
    iDescent := IntegerMax(iDescent, IntegerMax(oItem.Descent, 4)); // allow 4 for spelling squiggle
    End;

  oRow.Height := iHeight + iDescent;
  oRow.BaseLine := iHeight;

  For iLoop := 0 To oRow.Items.Count - 1 Do
  Begin
    oItem := oRow.Items[iLoop];
    If (TWPWorkingDocumentPiece(oItem.Piece).PieceType <> ptImage) Or (TWPWorkingDocumentImagePiece(oItem.Piece).VerticalAlignment = ivaBaseLine) Then
      oItem.Top := oRow.Top + IntegerMax(0, iHeight - (oItem.Height - oItem.Descent))
    Else If TWPWorkingDocumentImagePiece(oItem.Piece).VerticalAlignment = ivaBottom Then
      oItem.Top := oRow.Top + (oRow.Height - oItem.Height)
    Else If TWPWorkingDocumentImagePiece(oItem.Piece).VerticalAlignment = ivaCentered Then
      oItem.Top := oRow.Top + ((oRow.Height - oItem.Height) Div 2)
    Else // TWPWorkingDocumentImagePiece(oItem.Piece).VerticalAlignment = ivaTop
      oItem.Top := oRow.Top;
  End;
End;


Procedure TWPRenderer.PaintBullet(oContainer: TWPMapContainer; oParagraph: TWPWorkingDocumentParaPiece; oStyle : TWPStyle);
Var
  iLeft, iHeight, iTop : Integer;
  cBullet : Char;
Begin
  If oStyle.WorkingParagraphListType(oParagraph.Format) = WPSParagraphListTypeBullets Then
    Begin
    ApplyFont(oParagraph, oStyle);
    FCanvas.Font.ClearStyles;
    FCanvas.Font.Name := 'Wingdings';
    FCanvas.Font.Size := Trunc(FCanvas.Font.Size * BULLET_SCALE_FACTOR);
    FCanvas.Font.Colour := ApplyOutputColourRules(False, FCanvas.Font.Colour);

    cBullet := GetBulletChar(oParagraph.Format, oStyle);
    If oContainer.Rows.Count > 0  Then
      iLeft := oContainer.Rows[0].FirstLeft - oParagraph.AdornmentWidth
    Else
      Case oStyle.WorkingParagraphAlign(oParagraph.Format) Of
        WordProcessorParagraphAlignmentCentre : iLeft := oContainer.Centre  - oParagraph.AdornmentWidth Div 2;
        WordProcessorParagraphAlignmentRight : iLeft := oContainer.innerRight -  oParagraph.AdornmentWidth;
      Else
        iLeft := oContainer.InnerLeft - oParagraph.AdornmentWidth;
      End;
    iHeight := FCanvas.TextHeight(cBullet);
    iTop := oContainer.InnerTop + oContainer.FirstPopulatedRow.BaseLine - iHeight + FCanvas.GetTextMetrics.tmDescent;
    FCanvas.DrawText(clTransparent, iLeft, iTop, cBullet);
    End;
End;


Procedure TWPRenderer.PaintNumber(oContainer: TWPMapContainer; oParagraph: TWPWorkingDocumentParaPiece; oStyle : TWPStyle);
Var
  iLeft, iTop, iHeight : Integer;
  sNumber : String;
Begin
  If oStyle.WorkingParagraphListType(oParagraph.Format) = WPSParagraphListTypeNumbers Then
    Begin
    ApplyFont(oParagraph, oStyle);
    sNumber := GetNumberFormat(oParagraph, oStyle, oParagraph.ListNumber);
    FCanvas.Font.Colour := ApplyOutputColourRules(False, FCanvas.Font.Colour);

    If oContainer.Rows.Count > 0  Then
      iLeft := oContainer.Rows[0].FirstLeft - oParagraph.AdornmentWidth
    Else
      Case oStyle.WorkingParagraphAlign(oParagraph.Format) Of
        WordProcessorParagraphAlignmentCentre : iLeft := oContainer.Centre  - oParagraph.AdornmentWidth Div 2;
        WordProcessorParagraphAlignmentRight : iLeft := oContainer.innerRight - oParagraph.AdornmentWidth;
      Else
        iLeft := oContainer.InnerLeft - oParagraph.AdornmentWidth;
      End;
    iHeight := FCanvas.TextHeight(sNumber);
    iTop := oContainer.InnerTop + oContainer.FirstPopulatedRow.BaseLine - iHeight + FCanvas.GetTextMetrics.tmDescent;
    FCanvas.DrawText(clTransparent, iLeft, iTop, sNumber);
    End;
End;


Procedure TWPRenderer.PaintWrongSpelling(bSelected : Boolean; aColour : TColour; iY, iLeft, iRight: Integer);
Begin
  If bSelected Then
    FCanvas.DrawSquiggle((Not aColour) And $00FFFFFF, iLeft, iY+2, iRight-1)
  Else
    FCanvas.DrawSquiggle(aColour, iLeft, iY+2, iRight-1);
End;


Procedure TWPRenderer.ApplySelectionToDocument;
Var
  iLoop : Integer;
  iMap : Integer;
  oPiece : TWPWorkingDocumentPiece;
  oMap : TWPMapItem;
Begin
  If FSelection.HasSelection Then
  Begin
    For iLoop := 0 To FDocument.Pieces.Count - 1 Do
    Begin
      oPiece := FDocument.Pieces[iLoop];
      For iMap := 0 To oPiece.Maps.Count - 1 Do
      Begin
        oMap := oPiece.Maps[iMap];
        If (oPiece.Metrics.Position+oMap.OffsetStart > FSelection.SelEnd) Or (oPiece.Metrics.Position+oMap.OffsetStart + oMap.OffsetLength < FSelection.SelStart) Then
          oMap.Selection := wsNone
        Else If (oPiece.Metrics.Position+oMap.OffsetStart >= FSelection.SelStart) And (oPiece.Metrics.Position+oMap.OffsetStart + oMap.OffsetLength <= FSelection.SelEnd) Then
          oMap.Selection := wsAll
        Else If (oPiece.Metrics.Position+oMap.OffsetStart < FSelection.SelStart) And (oPiece.Metrics.Position+oMap.OffsetStart + oMap.OffsetLength > FSelection.SelEnd) Then
          oMap.Selection := wsPart
        Else If (oPiece.Metrics.Position+oMap.OffsetStart < FSelection.SelStart) Then
          oMap.Selection := wsFromRight
        Else
          oMap.Selection := wsFromLeft;
(*
        oPiece.Metrics.Position > FSelection.SelEnd) Or (oPiece.Metrics.Position + oPiece.Metrics.OffsetCount < FSelection.SelStart) Then
          oPiece.Map.Selection := wsNone
        Else If (oPiece.Metrics.Position >= FSelection.SelStart) And (oPiece.Metrics.Position + oPiece.Metrics.OffsetCount <= FSelection.SelEnd) Then
          oPiece.Map.Selection := wsAll
        Else If (oPiece.Metrics.Position < FSelection.SelStart) And (oPiece.Metrics.Position + oPiece.Metrics.OffsetCount > FSelection.SelEnd) Then
          oPiece.Map.Selection := wsPart
        Else If (oPiece.Metrics.Position < FSelection.SelStart) Then
          oPiece.Map.Selection := wsFromRight
        Else
          oPiece.Map.Selection := wsFromLeft;
*)
      End;
    End;
    FSomeSelected := True;
  End
  Else If FSomeSelected Then
  Begin
    FSomeSelected := False;
    For iLoop := 0 To FDocument.Pieces.Count - 1 Do
    Begin
      oPiece := FDocument.Pieces[iLoop];
      For iMap := 0 To oPiece.Maps.Count - 1 Do
        oPiece.Maps[iMap].Selection := wsNone;
    End
  End;
End;


Procedure TWPRenderer.InitialiseMap;
Begin
  FMap.Children.Clear;
  FMap.Piece := Document;
  FMap.Height := 0;
  FMap.MarginLeft := Settings.HorizontalMargin;
  FMap.MarginRight := Settings.HorizontalMargin;
  FMap.MarginTop := Settings.VerticalMargin;
  FMap.MarginBottom := Settings.VerticalMargin;
  FMap.Left := 0;
  FMap.Top := 0;
  FMap.Width := FWidth - SpaceForAnnotations;
End;


{
Function TWPRenderer.WorkingStateSource : TWPWorkingDocumentPiece;
Begin
  If FStateStack.Count = 0 Then
    Result := Nil
  Else
    Result := FStateStack[FStateStack.Count-1].Source;
End;

Function TWPRenderer.HotSpotNotOverruled(oHotspot : TWPHotspot):Boolean;
Var
  iLoop : Integer;
  iLast : Integer;
Begin
  // the hotspot has been overruled if it is
  // listed in the stack as a source and is not last
  iLast := -1;
  For iLoop := 0 To FStateStack.Count - 1 Do
    If Assigned(FStateStack[iLoop].Source) And (FStateStack[iLoop].Source.Hotspot = oHotspot) Then
      iLast := iLoop;
  Result := (iLast = -1) Or (iLast = FStateStack.Count - 1);
End;



Function TWPRenderer.GetHotspotOwner(oPiece : TWPWorkingDocumentPiece; oHotspot : TWPHotspot) : TWPWorkingDocumentPiece;
Var
  iStateStackIndex : Integer;
  oStateStackSource : TWPWorkingDocumentPiece;
Begin
  If (oPiece.Hotspot = oHotspot) Then
  Begin
    Result := oPiece;
  End
  Else
  Begin
    Result := Nil;

    For iStateStackIndex := 0 To FStateStack.Count - 1 Do
    Begin
      oStateStackSource := FStateStack[iStateStackIndex].Source;

      If Assigned(oStateStackSource) And (oStateStackSource.Hotspot = oHotspot) Then
        Result := oStateStackSource;
    End;
  End;
End;
}

Function TWPRenderer.ReadOnlyColour : TColour;
Begin
  If Printing Then
    Result := Settings.BackGround
  Else
    Result := DEFAULT_BACKGROUND_READONLY;
End;


Function TWPRenderer.RowForItem(oMap : TWPMapItem) : TWPMapRow;
Begin
  If oMap.HasParent And (oMap.Parent Is TWPMapRow) Then
    Result := TWPMapRow(oMap.Parent)
  Else
    Result := Nil;
End;

Procedure TWPRenderer.PaintPieceBackground(oMap : TWPMapItem; oRow : TWPMapRow; oPiece : TWPWorkingDocumentPiece; oStyle : TWPStyle; bParentBackground : Boolean);
Var
  iTop : Integer;
  iBottom : Integer;
  aColour : TColour;
Begin
  If Assigned(oRow) Then
    Begin
    iTop := oRow.Top;
    iBottom := oRow.Bottom;
    End
  Else
    Begin
    iTop := oMap.Top;
    iBottom := oMap.Bottom;
    End;

  If bParentBackground Then
    aColour := oMap.WorkingBackground(FCurrentHotspot)
  Else
  Begin
    If Assigned(oStyle) Then
      aColour := oStyle.WorkingFontBackground(oPiece.Font, oMap.WorkingBackground(FCurrentHotspot))
    Else
      aColour := oPiece.Font.Background;
  End;

  If (aColour = DEF_COLOUR) Then
    aColour := Settings.Background;

  aColour := ApplyOutputColourRules(True, aColour);

  If Not Printing Or (aColour <> Settings.Background) Then
  Begin
    FCanvas.DrawRect(aColour, oMap.Left, iTop, oMap.Right, iBottom);
    Case oMap.Selection Of
      wsAll :      FCanvas.DrawRect(clBlack, oMap.Left, iTop, oMap.Right, iBottom);
      wsFromLeft : FCanvas.DrawRect(clBlack, oMap.Left, iTop, oMap.PointForSelection(FSelection.SelEnd), iBottom);
      wsFromRight: FCanvas.DrawRect(clBlack, oMap.PointForSelection(FSelection.SelStart), iTop, oMap.Right, iBottom);
      wsPart :     FCanvas.DrawRect(clBlack, oMap.PointForSelection(FSelection.SelStart), iTop, oMap.PointForSelection(FSelection.SelEnd), iBottom);
    // else wsNone
    End;
  End;
End;


Procedure TWPRenderer.PaintFieldHint(oPiece: TWPWorkingDocumentPiece; oItem: TWPMapItem; Const bStart : Boolean; aFontState : TWPSFontState; oStyle : TWPStyle; bParentBackground, bUnderlineHotspot : Boolean);
Begin
  PaintTextItem(oPiece, oItem, ' ', aFontState, fcsNormal, oStyle, bParentBackground, bUnderlineHotspot);
  If (Settings.FieldWrappers = wpfpHints) Then
    If bStart Then
    Begin
  //    Canvas.DrawLine(1, clGray, apsSolid, oItem.Left, oItem.Bottom - oItem.Height div 2, oItem.Left, oItem.Bottom-1);
  //    Canvas.DrawLine(1, clGray, apsSolid, oItem.Left, oItem.Bottom - 1, oItem.Right-1, oItem.Bottom - 1);
      Canvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsSolid, oItem.Left, oItem.Bottom - 3, oItem.Left, oItem.Bottom-1);
      Canvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsSolid, oItem.Left, oItem.Bottom - 1, oItem.Left + 2, oItem.Bottom - 1);
    End
    Else
    Begin
  //    Canvas.DrawLine(1, ApplyOutputColourRules(false, clGray), apsSolid, oItem.Right-1, oItem.Top + oItem.Height div 2, oItem.Right-1, oItem.Top);
  //    Canvas.DrawLine(1, ApplyOutputColourRules(false, clGray), apsSolid, oItem.Right-oItem.Width, oItem.Top, oItem.Right-1, oItem.Top);
  //    Canvas.DrawLine(1, ApplyOutputColourRules(false, clGray), apsSolid, oItem.Right-1, oItem.Bottom - oItem.Height div 2, oItem.Right-1, oItem.Bottom-1);
  //    Canvas.DrawLine(1, ApplyOutputColourRules(false, clGray), apsSolid, oItem.Left, oItem.Bottom - 1, oItem.Right-1, oItem.Bottom - 1);
      Canvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsSolid, oItem.Right-1, oItem.Top + 2, oItem.Right-1, oItem.Top);
      Canvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsSolid, oItem.Right-3, oItem.Top, oItem.Right-1, oItem.Top);
    End;
End;

Procedure TWPRenderer.PaintTextItem(oPiece: TWPWorkingDocumentPiece; oItem: TWPMapItem; sText: String; aFontState : TWPSFontState;
                  aCapsState : TWPSCapsState; oStyle : TWPStyle; bParentBackground, bUnderlineHotspot : Boolean);
Var
  iOffset : Integer;
  bSpelling : Boolean;
  aSpellColor : TColour;
Begin
  Canvas.Clip(oItem);
  Try
    PaintPieceBackground(oItem, RowForItem(oItem), oPiece, oStyle, bParentBackground);

    If (aCapsState = fcsAllCaps) Or (aCapsState = fcsSmallCaps) Then
      sText := StringUpper(sText)
    Else If aCapsState = fcsNoCaps Then
      sText := Lowercase(sText);

    If oItem.InForeHotspot And ShowHotspots(true) Then
    Begin
      If (oPiece.Font.Underline = tsUnknown) And bUnderlineHotspot Then
        FCanvas.Font.Underline := oItem.ForeHotspot.LinkUnderline;
      If (oItem.ForeHotspot = CurrentHotspot) Then
      Begin
        If oItem.ForeHotspot.HoverColour = DEF_COLOUR Then
          FCanvas.Font.Colour := Settings.HoverColour
        Else
          FCanvas.Font.Colour := oItem.ForeHotspot.HoverColour;
      End
      Else If oPiece.Font.Foreground = DEF_COLOUR Then
      Begin
        If (oItem.ForeHotspot.LinkColour = DEF_COLOUR) Then
          FCanvas.Font.Colour := Settings.LinkColour
        Else
          FCanvas.Font.Colour := oItem.ForeHotspot.LinkColour;
      End;
    End;

    FCanvas.Font.Colour := ApplyOutputColourRules(False, FCanvas.Font.Colour);

    Case aFontState Of
      fsSuperscript :
        Begin
        iOffset := oItem.Descent-(FCanvas.Font.Size Div 5);
        FCanvas.Font.Size := FCanvas.Font.Size - FCanvas.Font.Size Div 2;
        End;
      fsSubscript :
        Begin
        iOffset := oItem.Descent + (FCanvas.Font.Size Div 2);
        FCanvas.Font.Size := FCanvas.Font.Size - FCanvas.Font.Size Div 2;
        End;
    Else // None or unknown
      iOffset := 0;
    End;

    bSpelling := Settings.SpellingErrors And Not Printing And Not oPiece.IsReadOnly And
     (((oPiece.PieceType = ptText) And (TWPWorkingDocumentTextPiece(oPiece).SpellState In [scsWrong, scsFieldWrong])) Or
      ((oPiece.PieceType = ptFieldStart) And TWPWorkingDocumentFieldStartPiece(oPiece).InError) Or
      ((oPiece.PieceType = ptFieldStop) And TWPWorkingDocumentFieldStopPiece(oPiece).MatchingStart.InError));

  If (oPiece.PieceType In [ptFieldStart, ptFieldStop]) Or (TWPWorkingDocumentTextPiece(oPiece).SpellState  = scsFieldWrong) Then
      aSpellColor := clBlue
    Else
      aSpellColor := clRed;

    Case oItem.Selection Of
      wsAll :
        Begin
        If bSpelling Then
          PaintWrongSpelling(True, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.Right-1);

        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.Left + oItem.Nose, oItem.Top + iOffset, sText);
        End;
      wsFromLeft :
        Begin
        If bSpelling Then
          Begin
          PaintWrongSpelling(True, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelEnd));
          PaintWrongSpelling(False, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelEnd), oItem.Right);
          End;
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.Left + oItem.Nose, oItem.Top + iOffset, Copy(sText, 1, oItem.OffsetForSelection(FSelection.SelEnd)));
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.PointForSelection(FSelection.SelEnd) + oItem.Nose, oItem.Top + iOffset, Copy(sText,  oItem.OffsetForSelection(FSelection.SelEnd) + 1, MAXINT));
        End;
      wsFromRight :
        Begin
        If bSpelling Then
          Begin
          PaintWrongSpelling(False, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelStart));
          PaintWrongSpelling(True, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelStart), oItem.Right);
          End;
        FCanvas.DrawText(clTransparent, oItem.Left + oItem.Nose, oItem.Top + iOffset, Copy(sText, 1, oItem.OffsetForSelection(FSelection.SelStart)));
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.PointForSelection(FSelection.SelStart) + oItem.Nose, oItem.Top + iOffset, Copy(sText,  oItem.OffsetForSelection(FSelection.SelStart) + 1, MAXINT));
        End;
      wsPart :
        Begin
        If bSpelling Then
          Begin
          PaintWrongSpelling(False, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelStart));
          PaintWrongSpelling(True, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelStart), oItem.PointForSelection(FSelection.SelEnd));
          PaintWrongSpelling(False, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelEnd), oItem.Right);
          End;
        FCanvas.DrawText(clTransparent, oItem.Left + oItem.Nose, oItem.Top + iOffset, Copy(sText, 1, oItem.OffsetForSelection(FSelection.SelStart)));
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.PointForSelection(FSelection.SelStart) + oItem.Nose, oItem.Top + iOffset, Copy(sText,  oItem.OffsetForSelection(FSelection.SelStart) + 1, MAXINT));
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        FCanvas.DrawText(clTransparent, oItem.PointForSelection(FSelection.SelEnd) + oItem.Nose, oItem.Top + iOffset, Copy(sText,  oItem.OffsetForSelection(FSelection.SelEnd) + 1, MAXINT));
        End;
    Else // wsNone
      If bSpelling Then
        PaintWrongSpelling(False, aSpellColor, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.Right);
      FCanvas.DrawText(clTransparent, oItem.Left + oItem.Nose, oItem.Top + iOffset, sText);
    End;
  Finally
    Canvas.UnClip;
  End;
End;


Procedure TWPRenderer.PaintRowEdges(oContainer: TWPMapContainer);
Var
  iLoop : Integer;
Begin
  Assert(Invariants('PaintRowEdges', oContainer, TWPMapContainer, 'oContainer'));

  For iLoop := 0 To oContainer.Rows.Count - 1 Do
    PaintRowEdge(oContainer.Rows[iLoop]);
End;


Procedure TWPRenderer.PaintRowEdge(oRow: TWPMapRow);
Var
  oContainer : TWPMapContainer;
  iLeft : Integer;
  iRight : Integer;
  oBackground : TColour;
Begin
  oBackground := oRow.WorkingBackground(FCurrentHotspot);
  If oRow.HasParent And (oRow.Parent Is TWPMapContainer) Then
    Begin
    oContainer := TWPMapContainer(oRow.Parent);
    iLeft := oContainer.Left;
    iRight := oContainer.Right;
    End
  Else
    Begin
    iLeft := oRow.Left;
    iRight := oRow.Right;
    End;

  If (Not Printing Or (oBackground <> Settings.Background)) Then
  Begin
    FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), iLeft, oRow.Top, oRow.FirstLeft, oRow.Bottom);
    If oRow.LastRight < iRight Then
    Begin
      FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oRow.LastRight, oRow.Top, iRight, oRow.Bottom);
      if (oRow.Items.Count > 0) And (oRow.Items[oRow.Items.Count - 1].AnnotationColour <> clTransparent) Then
        FCanvas.DrawLine(1, oRow.Items[oRow.Items.Count - 1].AnnotationColour, apsDot, oRow.LastRight, oRow.Bottom-1, iRight, oRow.Bottom-1);
    End;
  End;
  oRow.PaintedFirstLeft := oRow.FirstLeft;
  oRow.PaintedLastRight := oRow.LastRight;
End;


Procedure TWPRenderer.PaintLineBreak(oMap : TWPMapItem; oPiece: TWPWorkingDocumentLineBreakPiece);
Var
  oStyle : TWPStyle;
Begin
  oStyle := Styles.GetByNameOrDefault(oPiece.Style);
  If Settings.EditHints Then
    PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, clGray), fcsNormal, oStyle, False, False)
  Else
    PaintTextItem(oPiece, oMap, ' ', ApplyFont(oPiece, oStyle, clGray), fcsNormal, oStyle, True, False);
  PaintAnnotationDecoration(oMap, RowForItem(oMap), oPiece);
End;


Procedure TWPRenderer.PaintButton(iLeft, iRight : Integer; oItem : TWPMapItem; Const sText : String; bChecked : Boolean);
Var
  iwl, iwr, iwt, iwb : Integer;
Begin
  iwl := oItem.Left + iLeft;
  iwr := oItem.Left + iRight - (Canvas.PointSizeX * CHECK_PAD_RIGHT_INNER) - 1;
  iwt := oItem.Top;
  iwb := oItem.Bottom - 1;

  FCanvas.DrawText(clTransparent, iwl + (Canvas.PointSizeX * CHECK_TOTAL_LEFT), iwt + 1, sText);
  If Printing Then
    If bChecked Then
      FCanvas.DrawImage(WPIconModule.Check_Checked_High, cmSrcCopy, iwl + (Canvas.PointSizeX * CHECK_PAD_LEFT), ( iwt + iwb) Div 2 - (Canvas.PointSizeX * CHECK_IMAGE) Div 2, iwl + (Canvas.PointSizeX * (CHECK_PAD_LEFT + CHECK_IMAGE)), (iwt + iwb) Div 2 + (Canvas.PointSizeX * CHECK_IMAGE) Div 2)
    Else
      FCanvas.DrawImage(WPIconModule.Check_Unchecked_High, cmSrcCopy, iwl + (Canvas.PointSizeX * CHECK_PAD_LEFT), (iwt + iwb) Div 2 - (Canvas.PointSizeX * CHECK_IMAGE) Div 2, iwl + (Canvas.PointSizeX * (CHECK_PAD_LEFT +  CHECK_IMAGE)), (iwt + iwb) Div 2 + (Canvas.PointSizeX * CHECK_IMAGE) Div 2)
  Else
    If bChecked Then
      FCanvas.DrawImage(WPIconModule.Check_Checked, cmSrcCopy, iwl + (Canvas.PointSizeX * CHECK_PAD_LEFT), ( iwt + iwb) Div 2 - (Canvas.PointSizeX * CHECK_IMAGE) Div 2, iwl + (Canvas.PointSizeX * (CHECK_PAD_LEFT + CHECK_IMAGE)), (iwt + iwb) Div 2 + (Canvas.PointSizeX * CHECK_IMAGE) Div 2)
    Else
      FCanvas.DrawImage(WPIconModule.Check_Unchecked, cmSrcCopy, iwl + (Canvas.PointSizeX * CHECK_PAD_LEFT), (iwt + iwb) Div 2 - (Canvas.PointSizeX * CHECK_IMAGE) Div 2, iwl + (Canvas.PointSizeX * (CHECK_PAD_LEFT +  CHECK_IMAGE)), (iwt + iwb) Div 2 + (Canvas.PointSizeX * CHECK_IMAGE) Div 2);
  If Printing Then
    FCanvas.DrawRoundOutline(clSilver, iwl, iwt, iwr, iwb, 12 * Canvas.PointSizeX)
  Else
    FCanvas.DrawRoundOutline(clSilver, iwl, iwt, iwr, iwb, 8 * Canvas.PointSizeX);
End;


Procedure TWPRenderer.PaintFieldCheckables(oItem : TWPMapItem; oStyle : TWPStyle; oPiece: TWPWorkingDocumentFieldStartPiece);
Var
  iLoop : Integer;
Begin
  Canvas.Clip(oItem);
  Try
    PaintPieceBackground(oItem, RowForItem(oItem), oPiece, oStyle, False);

    Case oItem.Selection Of
      wsAll :
        Begin
        If oPiece.InError Then
          PaintWrongSpelling(True, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.Right-1);

        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;

        For iLoop := oItem.OffsetStart To oItem.OffsetStart + oItem.OffsetLength - 1 Do
          PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart],
             oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart],
             oItem, oPiece.Checkables[iLoop], iLoop + 1 = oPiece.CheckedIndex);
        End;
      wsFromLeft :
        Begin
        If oPiece.InError Then
          Begin
          PaintWrongSpelling(True, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelEnd));
          PaintWrongSpelling(False, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelEnd), oItem.Right);
          End;
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        For iLoop := 0 To oItem.OffsetForSelection(FSelection.SelEnd) - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        For iLoop := oItem.OffsetForSelection(FSelection.SelEnd) To oPiece.Checkables.Count - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        End;
      wsFromRight :
        Begin
        If oPiece.InError Then
          Begin
          PaintWrongSpelling(False, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelStart));
          PaintWrongSpelling(True, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelStart), oItem.Right);
          End;
        For iLoop := 0 To oItem.OffsetForSelection(FSelection.SelStart) - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        For iLoop := oItem.OffsetForSelection(FSelection.SelStart) To oPiece.Checkables.Count - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        End;
      wsPart :
        Begin
        If oPiece.InError Then
          Begin
          PaintWrongSpelling(False, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.PointForSelection(FSelection.SelStart));
          PaintWrongSpelling(True, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelStart), oItem.PointForSelection(FSelection.SelEnd));
          PaintWrongSpelling(False, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.PointForSelection(FSelection.SelEnd), oItem.Right);
          End;
        For iLoop := 0 To oItem.OffsetForSelection(FSelection.SelStart) - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        For iLoop := oItem.OffsetForSelection(FSelection.SelStart) To oItem.OffsetForSelection(FSelection.SelEnd) - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        FCanvas.Font.Colour := (Not FCanvas.Font.Colour) And $00FFFFFF;
        For iLoop := oItem.OffsetForSelection(FSelection.SelEnd) To oPiece.Checkables.Count - 1 Do
          if (iLoop >= oItem.OffsetStart) And (iLoop < oItem.OffsetStart + oItem.OffsetLength) Then
            PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart], oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart], oItem, oPiece.Checkables[iLoop], iLoop+1 = oPiece.CheckedIndex);
        End;
    Else // wsNone
      If oPiece.InError Then
        PaintWrongSpelling(False, clBlue, oItem.Top + oItem.Height - oItem.Descent, oItem.Left, oItem.Right);
      For iLoop := oItem.OffsetStart To oItem.OffsetStart + oItem.OffsetLength - 1 Do
        PaintButton(oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart],
           oPiece.Metrics.Offset[iLoop+1] - oPiece.Metrics.Offset[oItem.OffsetStart],
           oItem, oPiece.Checkables[iLoop], iLoop + 1 = oPiece.CheckedIndex);
    End;
  Finally
    Canvas.UnClip;
  End;
End;


Procedure TWPRenderer.PaintFieldStart(oMap : TWPMapItem; oPiece: TWPWorkingDocumentFieldStartPiece);
Var
  oStyle : TWPStyle;
Begin
  oStyle := Styles.GetByNameOrDefault(oPiece.Style);

  If oPiece.Checkables.Count > 0 Then
  Begin
    ApplyFont(oPiece, oStyle);
    PaintFieldCheckables(oMap, oStyle, oPiece)
  End
  Else If Settings.FieldWrappers In [wpfpHints, wpfpInvisible] Then
  Begin
    If oPiece.IsReadOnly And (oPiece.ReadOnly = tsFalse) Then
      PaintFieldHint(oPiece, oMap, True, ApplyFont(oPiece, oStyle, DEF_COLOUR_FIELD_ACTIVE), oStyle, False, True)
    Else
      PaintFieldHint(oPiece, oMap, True, ApplyFont(oPiece, oStyle, clGray), oStyle, False, True)
  End
  Else
  Begin
    If oPiece.IsReadOnly And (oPiece.ReadOnly = tsFalse) Then
      PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, DEF_COLOUR_FIELD_ACTIVE), fcsNormal, oStyle, False, True)
    Else
      PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, clGray), fcsNormal, oStyle, False, True)
  End;
  PaintAnnotationDecoration(oMap, RowForItem(oMap), oPiece);
End;


Procedure TWPRenderer.PaintFieldEnd(oMap : TWPMapItem; oPiece: TWPWorkingDocumentFieldStopPiece);
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStyle : TWPStyle;
Begin
  oStart := oPiece.MatchingStart;
  Assert(Invariants('PaintFieldEnd', oStart, TWPWorkingDocumentFieldStartPiece, 'Field Start'));

  oStyle := Styles.GetByNameOrDefault(oPiece.Style);
  If Settings.FieldWrappers In [wpfpHints, wpfpInvisible] Then
  Begin
    If oStart.IsReadOnly And (TWPWorkingDocumentFieldStartPiece(oStart).ReadOnly = tsFalse) Then
      PaintFieldHint(oPiece, oMap, False, ApplyFont(oPiece, oStyle, DEF_COLOUR_FIELD_ACTIVE), oStyle, False, True)
    Else
      PaintFieldHint(oPiece, oMap, False, ApplyFont(oPiece, oStyle, clGray), oStyle, False, True);
  End
  Else
  Begin
    If oStart.IsReadOnly And (TWPWorkingDocumentFieldStartPiece(oStart).ReadOnly = tsFalse) Then
      PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, DEF_COLOUR_FIELD_ACTIVE), fcsNormal, oStyle, False, True)
    Else
      PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, clGray), fcsNormal, oStyle, False, True);
  End;
  If Not Printing And oPiece.MatchingStart.HasDefinitionProvider And oPiece.MatchingStart.DefinitionProvider.HasUIAssistance(oPiece.MatchingStart.DocField) Then
    Canvas.DrawTriangle(FIELD_DROPPER_COLOUR, oMap.Right - FIELD_DROPPER_WIDTH + 2, oMap.VerticalMiddle-FIELD_DROPPER_UP, oMap.Right-2, oMap.VerticalMiddle-FIELD_DROPPER_UP, oMap.Right - (FIELD_DROPPER_WIDTH Div 2), oMap.VerticalMiddle+FIELD_DROPPER_DOWN);
 If oMap.Nose > 0 Then
  Begin
    If Settings.FormsMode And (oMap.Nose > 0) Then
    Begin
      If oMap.Selection <> wsNone Then
      Begin
        Canvas.DrawRect(clBlack, oMap.Left, oMap.Top, oMap.Left+oMap.Nose, oMap.Bottom);
        Canvas.DrawLine(1, clSilver, apsDot, oMap.Left, oMap.Bottom - 4, oMap.Left+oMap.Nose, oMap.Bottom - 4);
        If oStart.InError Then
          PaintWrongSpelling(True, clBlue, oMap.Top + oMap.Height - oMap.Descent, oMap.Left, oMap.Right-1);
      End
      Else
      Begin
        Canvas.DrawRect(clWhite, oMap.Left, oMap.Top, oMap.Left+oMap.Nose, oMap.Bottom);
        Canvas.DrawLine(1, clSilver, apsDot, oMap.Left, oMap.Bottom - 4, oMap.Left+oMap.Nose, oMap.Bottom - 4);
        If oStart.InError Then
          PaintWrongSpelling(True, clBlue, oMap.Top + oMap.Height - oMap.Descent, oMap.Left, oMap.Right-1);
      End;
    End;
  End;
  PaintAnnotationDecoration(oMap, RowForItem(oMap), oPiece);
End;


Procedure TWPRenderer.PaintImage(oMap : TWPMapItem; oImage: TWPWorkingDocumentImagePiece);
Var
  oRow : TWPMapRow;
  aCopyMode : TCopyMode;
  aColour : TColour;
  aAltColour : TColour;
  aRect : TRect;
  oStyle : TWPStyle;
  iLoop : Integer;
  bHover : Boolean;
  oArea : TWPImageMapArea;
Begin
  // TODO: output colour rules
  oRow := RowForItem(oMap);
  Assert(Invariants('PaintImage', oRow, TWPMapRow, 'Row'));
  aRect.Left := oRow.Left;
  aRect.Top := oRow.Top;
  aRect.Right := oRow.Right;
  aRect.Bottom := oRow.Bottom;

  FCanvas.Clip(aRect);
  Try
    oStyle := Styles.GetByNameOrDefault(oImage.Style);
    PaintPieceBackground(oMap, oRow, oImage, oStyle, True);

    If oMap.Selection = wsAll Then
    Begin
      aCopyMode := cmNotSrcCopy;
      aColour := (Not oImage.BorderColour) And $00FFFFFF;
      aAltColour := (Not clFuchsia) And $00FFFFFF;;
    End
    Else
    Begin
      aCopyMode := cmSrcCopy;
      aColour := oImage.BorderColour;
      aAltColour := clFuchsia;
    End;

    If oImage.Border > 0 Then
      FCanvas.DrawBorder(aColour, oMap.Left, oMap.Top, oMap.Right, oMap.Bottom, oImage.Border * Canvas.PointSizeX, oImage.Border * Canvas.PointSizeY, oImage.Border * Canvas.PointSizeX, oImage.Border * Canvas.PointSizeY);

    If oImage.HasImage Then
    Begin
      FCanvas.DrawImage(oImage.GetWorkingImage(oMap.Width, oMap.Height, ShowHotspots(false), (oMap.Selection = wsAll) or WantFastDrawing), aCopyMode, oMap.Left + oImage.Border * Canvas.PointSizeX, oMap.Top + oImage.Border * Canvas.PointSizeY,
                                                 oMap.Right - oImage.Border * Canvas.PointSizeX, oMap.Bottom - oImage.Border * Canvas.PointSizeY)
    End
    Else
      FCanvas.DrawRect(aAltColour, oMap.Left + oImage.Border * Canvas.PointSizeX, oMap.Top + oImage.Border * Canvas.PointSizeY,
                                   oMap.Right - oImage.Border * Canvas.PointSizeX, oMap.Bottom - oImage.Border * Canvas.PointSizeY);

    If oImage.HasImageMap And ShowHotspots(false) Then
    Begin
      // first, draw any explicit links
      bHover := False;
      For iLoop := 0 To oImage.ImageMap.Areas.Count - 1 Do
      Begin
        oArea := oImage.ImageMap.Areas[iLoop];
        If (oArea = FCurrentHotspot) Then
          bHover := True
        Else If (oArea.LinkColour <> DEF_COLOUR) Then
          FCanvas.DrawPolyLine(oArea.LinkColour, apsDot, apesRound, oMap.Left, oMap.Top, oArea.Coordinates, oImage.FactorX, oImage.FactorY);
      End;

      // second, draw the hover link is appropriate
      If bHover And (FCurrentHotspot.HoverColour <> DEF_COLOUR) Then
        FCanvas.DrawPolyLine(FCurrentHotspot.HoverColour, apsDot, apesRound, oMap.Left + oImage.Border * Canvas.PointSizeX,
                             oMap.Top + oImage.Border * Canvas.PointSizeY, TWPImageMapArea(FCurrentHotspot).Coordinates, oImage.FactorX, oImage.FactorY);
    End;
  Finally
    FCanvas.UnClip;
  End;
  If Not Printing And Not Settings.ReadOnly And Not oImage.IsReadOnly and (oImage.SizePolicy = ImageSizeManual) Then
  Begin
    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Left, oMap.Top, oMap.Left+IMAGE_CORNER_HINT, oMap.Top);
    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Left, oMap.Top, oMap.Left, oMap.Top+IMAGE_CORNER_HINT);

    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Right-1, oMap.Top, oMap.Right-IMAGE_CORNER_HINT-1, oMap.Top);
    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Right-1, oMap.Top, oMap.Right-1, oMap.Top+IMAGE_CORNER_HINT);

    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Right-1, oMap.Bottom, oMap.Right-IMAGE_CORNER_HINT-1, oMap.Bottom);
    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Right-1, oMap.Bottom, oMap.Right-1, oMap.Bottom-IMAGE_CORNER_HINT);

    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Left, oMap.Bottom, oMap.Left+IMAGE_CORNER_HINT, oMap.Bottom);
    FCanvas.DrawLine(1, clBlack, apsSolid, oMap.Left, oMap.Bottom, oMap.Left, oMap.Bottom-IMAGE_CORNER_HINT);
  End;
  PaintAnnotationDecoration(oMap, oRow, oImage);
End;



Procedure TWPRenderer.PaintParaMap(oMap : TWPMapItem; oPiece: TWPWorkingDocumentParaPiece);
Var
  oStyle : TWPStyle;
Begin
  oStyle := Styles.GetByNameOrDefault(oPiece.Style);
  If Settings.EditHints Then
    PaintTextItem(oPiece, oMap, oPiece.VisualText, ApplyFont(oPiece, oStyle, clGray, False), fcsNormal, oStyle, False, False)
  Else
    PaintTextItem(oPiece, oMap, ' ', ApplyFont(oPiece, oStyle, clGray, False), fcsNormal, oStyle, True, False);
  PaintAnnotationDecoration(oMap, RowForItem(oMap), oPiece);
End;

Procedure TWPRenderer.PaintParaContainer(oContainer : TWPMapContainer; oPiece: TWPWorkingDocumentParaPiece);
Var
  oStyle : TWPStyle;
  oBackground : TColour;
Begin
  Assert(Invariants('PaintParaContainer', oContainer, TWPMapContainer, 'oContainer'));
  oBackground := oContainer.WorkingBackground(FCurrentHotspot);

  oStyle := Styles.GetByNameOrDefault(oPiece.Style);

  If Not Printing Then
    PaintRowEdges(oContainer);


  PaintBullet(oContainer, oPiece, oStyle);
  PaintNumber(oContainer, oPiece, oStyle);

  If (Not Printing Or (oBackground <> Settings.Background)) And (oContainer.MarginBottom > 0) Then
    FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Left, oContainer.InnerBottom, oContainer.Right, oContainer.Bottom);
End;


Procedure TWPRenderer.PaintTable(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableStartPiece);
Var
  oBackground : TColour;
Begin
  oBackground := oContainer.WorkingBackground(FCurrentHotspot);
  If (Not Printing Or (oBackground <> Settings.Background)) Then
    FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.InnerRight, oContainer.Top, oContainer.Right, oContainer.Bottom);
End;


Procedure TWPRenderer.PaintTableRow(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableRowStartPiece);
Var
  iRight : Integer;
  iLoop : Integer;
  iMid : Integer;
  oFocus : TWPWorkingDocumentTableRowStartPiece;
  oBackground : TColour;
Begin
  oBackground := oContainer.WorkingBackground(Nil);
  If (Not Printing Or (oBackground <> Settings.Background)) Then
  Begin
    If oContainer.Children.Count = 0 Then
      RaiseError('PaintTableRow', 'Table Row has no contents');
    FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), TWPMapContainer(oContainer.Parent).InnerLeft, oContainer.Top+1, oContainer.Children[0].Left, oContainer.Bottom);
    If (oPiece.TablePrev <> Nil) And (oPiece.TablePrev.Cells.count > 0) Then
      iRight := IntegerMin(oContainer.Children[0].Left, oPiece.TablePrev.Cells[0].Container.Left)
    Else
      iRight := oContainer.Children[0].Left;
    FCanvas.DrawLine(1, ApplyOutputColourRules(True, oBackground), apsSolid, apesSquare, TWPMapContainer(oContainer.Parent).InnerLeft, oContainer.Top, iRight, oContainer.Top);
    iMid := (oContainer.Top + oContainer.Bottom) Div 2;
    iRight := TWPMapContainer(oContainer.Parent).InnerLeft + Settings.NestingIndent Div 2 + Settings.NestingIndent * oPiece.Depth;
    oFocus := oPiece;
    For iLoop := 1 To oPiece.Depth Do
    Begin

      Dec(iRight, Settings.NestingIndent);
      If oFocus.TableNextLevel <> Nil Then
        FCanvas.DrawLine(1, clBlack, apsDot, apesSquare, iRight, oContainer.Top + oContainer.Top Mod 2, iRight, oContainer.Bottom)
      Else If iLoop = 1 Then
        FCanvas.DrawLine(1, clBlack, apsDot, apesSquare, iRight, oContainer.Top + oContainer.Top Mod 2, iRight, iMid);

      If iLoop = 1 Then
        FCanvas.DrawLine(1, clBlack, apsDot, apesSquare, iRight+1, iMid, iRight + Settings.NestingIndent Div 2, iMid);

      oFocus := oFocus.Owner;
    End;
    FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Children[oContainer.Children.Count - 1].Right, oContainer.Top+1, TWPMapContainer(oContainer.Parent).InnerRight, oContainer.Bottom);

    // paint the padding below row
    If oPiece.LowerPaddingSize > 0 Then
    Begin
      If oPiece.LowerPaddingColour <> DEF_COLOUR Then
        oBackground := oPiece.LowerPaddingColour;
      FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Children[0].Left, oContainer.Bottom - oPiece.LowerPaddingSize,
                        oContainer.Children[oContainer.Children.Count - 1].Right, oContainer.Bottom);
    End;
  End;
End;


Procedure TWPRenderer.PaintTableCell(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);
Var
  oBackground : TColour;
Begin
  oBackground := oContainer.WorkingBackground(FCurrentHotspot);
  If (Not Printing Or (oBackground <> Settings.Background)) Then
  Begin
    If oContainer.Children.Count > 0 Then
    Begin
      FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Left, oContainer.Top, oContainer.Right, oContainer.Children[0].Top);
      FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Left, oContainer.Children[oContainer.Children.Count - 1].Bottom, oContainer.Right, oContainer.Bottom);
    End;

    If oContainer.Children.Count = 0 Then
      FCanvas.DrawRect(ApplyOutputColourRules(True, oBackground), oContainer.Left, oContainer.Top, oContainer.Right, oContainer.Bottom)
    Else
      FCanvas.DrawBorder(ApplyOutputColourRules(True{?}, oBackground), oContainer.Left, oContainer.Top, oContainer.Right, oContainer.Bottom,
                oContainer.MarginLeft, oContainer.MarginTop, oContainer.MarginRight, oContainer.MarginBottom);
  End;

  If Not Printing And Settings.TableBorders Then
    DrawDefaultBorders(oContainer, oPiece);

  DrawBorders(oContainer, oPiece);
End;


Procedure TWPRenderer.DrawBorders(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);
Var
  i1, i2: Integer;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  iLeft : Integer;
  bSharedBorder : Boolean;
Begin
  // top & bottom draws top|bottom/left and top|bottom/right
  // left is the less
  oRow := TWPWorkingDocumentTableRowStartPiece(oPiece.Row);
  If (oRow.Cells.First = oPiece) And (oRow.TablePrev <> Nil) And (oRow.TablePrev.Cells.Count > 0) And oRow.TablePrev.Cells[0].HasContainer Then
    iLeft := IntegerMin(oContainer.Left, oRow.TablePrev.Cells[0].Container.Left)
  Else
    iLeft := oContainer.Left;

  bSharedBorder := (Not (oRow.State In [tisFirst, tisOnly])) And (TWPWorkingDocumentTableRowStartPiece(oRow.Prev).LowerPaddingSize = 0);
  DrawHorizontalBorder(oPiece, oPiece.WorkingTopBorder, iLeft, oContainer.Top, oContainer.Right, True, bSharedBorder, oPiece.State);

  If (oRow.Cells.First = oPiece) And (oRow.TableNext <> Nil) And (oRow.TableNext.Cells.Count > 0) And oRow.TableNext.Cells[0].HasContainer Then
    iLeft := IntegerMin(oContainer.Left, oRow.TableNext.Cells[0].Container.Left)
  Else
    iLeft := oContainer.Left;

  bSharedBorder := (Not (oRow.State In [tisLast, tisOnly])) And (oRow.LowerPaddingSize = 0);
  DrawHorizontalBorder(oPiece, oPiece.WorkingBottomBorder, iLeft, oContainer.Bottom-1, oContainer.Right, False, bSharedBorder, oPiece.State);

  i1 := 0;
  i2 := 0;
  If TWPWorkingDocumentTableRowStartPiece(oPiece.Row).State In [tisFirst, tisOnly] Then
    i1 := oPiece.WorkingTopBorder.ActualWidth * FCanvas.PointSizeY;
  If TWPWorkingDocumentTableRowStartPiece(oPiece.Row).State In [tisLast, tisOnly] Then
    i2 := oPiece.WorkingBottomBorder.ActualWidth * FCanvas.PointSizeY;

  DrawVerticalBorder(oPiece, oPiece.WorkingLeftBorder,  oContainer.Left,  oContainer.Top + i1, oContainer.Bottom - i2, True, Not (oPiece.State In [tisFirst, tisOnly]));
  DrawVerticalBorder(oPiece, oPiece.WorkingRightBorder, oContainer.Right-1, oContainer.Top + i1, oContainer.Bottom - i2, False, Not (oPiece.State In [tisLast, tisOnly]));
End;

Procedure TWPRenderer.DrawDefaultBorders(oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentTableCellStartPiece);
Var
  oRow : TWPWorkingDocumentTableRowStartPiece;
  bNotSharedBorder : Boolean;
Begin
  oRow := TWPWorkingDocumentTableRowStartPiece(oPiece.Row);
  bNotSharedBorder := (oRow.State In [tisFirst, tisOnly]);
  bNotSharedBorder := bNotSharedBorder Or (TWPWorkingDocumentTableRowStartPiece(oRow.Prev).LowerPaddingSize > 0);
  If Not oPiece.WorkingTopBorder.Defined And bNotSharedBorder Then
    DrawHorizontalBorder(oPiece, FDefaultTableBorder, oContainer.Left, oContainer.Top, oContainer.Right, True, False, tisOnly);

  If Not oPiece.WorkingLeftBorder.Defined And (oPiece.State In [tisFirst, tisOnly]) Then
    DrawVerticalBorder(oPiece, FDefaultTableBorder, oContainer.Left, oContainer.Top, oContainer.Bottom, True, False);

  // always draw bottom/right table line when the border is not defined
  If Not oPiece.WorkingRightBorder.Defined Then
    DrawVerticalBorder(oPiece, FDefaultTableBorder, oContainer.Right-1, oContainer.Top, oContainer.Bottom, False, False);
  If Not oPiece.WorkingBottomBorder.Defined Then
    DrawHorizontalBorder(oPiece, FDefaultTableBorder, oContainer.Left, oContainer.Bottom-1, oContainer.Right, False, False, tisOnly);
End;


Procedure TWPRenderer.DrawHorizontalBorder(oPiece : TWPWorkingDocumentTableCellStartPiece; oBorder : TWPBorder; iX1, iY, iX2 : Integer; bTop : Boolean; bShared : Boolean; aState : TWPWorkingDocumentTableItemState);
Var
  iLeft : Integer;
  iTop : Integer;
  iRight : Integer;
  iBottom : Integer;
  i1, i2: Integer;
  iwy : Integer;
  iwx : Integer;
Begin
  If oBorder.Defined And (oBorder.Style <> apsNone) Then
  Begin
    iwy := oBorder.Width * Canvas.PointSizeY;
    iwx := oBorder.Width * Canvas.PointSizeX;
    If oBorder.Width = 1 Then
    Begin
      If Not (TWPWorkingDocumentTableStartPiece(TWPWorkingDocumentTableRowStartPiece(oPiece.Row).Table).BorderPolicy In [tbpGrid, tbpLines, tbpInnerLines, tbpBox, tbpBoxLines, tbpFancy])
         Or (Not bShared Or bTop) Then
        Canvas.DrawLine(1, ApplyOutputColourRules(False, oBorder.Colour), oBorder.Style, iX1, iY, iX2, iY);
    End
    Else If oBorder.Fancy Then
    Begin
      i1 := 0;
      i2 := 0;
      If (oBorder.LowOuterlimit <> DEF_WORD) And (aState In [tisFirst, tisOnly]) Then
        i1 := oBorder.LowOuterlimit * Canvas.PointSizeX;
      If (oBorder.HighOuterlimit <> DEF_WORD) And (aState In [tisLast, tisOnly]) Then
        i2 := oBorder.HighOuterlimit * Canvas.PointSizeX;

      If oBorder.OuterColour2 <> DEF_COLOUR Then
      Begin
        If i1 <> 0 Then
        Begin
          If bTop Then
            Canvas.DrawRect(oBorder.OuterColour2, iX1, iY, iX1+i1, iY + iwy)
          Else
            Canvas.DrawRect(oBorder.OuterColour2, iX1, iY - iwy, iX1+i1, iY+1);
        End;
        If i2 <> 0 Then
        Begin
          If bTop Then
            Canvas.DrawRect(oBorder.OuterColour2, iX2-i2, iY, iX2, iY + iwy)
          Else
            Canvas.DrawRect(oBorder.OuterColour2, iX2-i2, iY - iwy, iX2, iY+1);
        End;
      End;

      If oBorder.OuterColour <> DEF_COLOUR Then
        If bTop Then
          Canvas.DrawRect(oBorder.OuterColour, iX1+i1, iY, iX2-i2, iY + iwy)
        Else
          Canvas.DrawRect(oBorder.OuterColour, iX1+i1, iY - iwy, iX2-i2, iY+1);

      If bTop Then
      Begin
        If bShared Then
        Begin
          iTop := iY;
          iBottom := iTop + (iwy Div 4)
        End
        Else
        Begin
          iTop := iY +  (iwy Div 4);
          iBottom := iTop + (iwy Div 2);
        End;
      End
      Else
      Begin
        If bShared Then
        Begin
          iTop := iY - (iwy Div 4) + 1;
          iBottom := iY  + 1;
        End
        Else
        Begin
          iTop := iY - (iwy Div 4) * 3 + 1;
          iBottom := iY - (iwy Div 4) + 1;
        End;
      End;
      If (aState In [tisFirst, tisOnly]) Then
        iLeft := iX1 + iwx
      Else
        iLeft := iX1;
      If (aState In [tisLast, tisOnly]) Then
        iRight := iX2 - iwx
      Else
        iRight := iX2;
      If oBorder.BrushImage <> Nil Then
        Canvas.DrawRect(oBorder.BrushImage, iLeft, iTop, iRight, iBottom)
      Else
        Canvas.DrawRect(oBorder.Colour, iLeft, iTop, iRight, iBottom);
      If (aState In [tisFirst, tisOnly]) Then
      Begin
        If bTop Then
          Canvas.DrawCurve(oBorder.Colour, oPiece.Background, oBorder.BrushImage, iLeft, iY + iwy, iwx Div 4, (iwx Div 4) * 3, 90)
        Else
          Canvas.DrawCurve(oBorder.Colour, oPiece.Background, oBorder.BrushImage, iLeft, iY - iwy, iwx Div 4, (iwx Div 4) * 3, 180);
      End;
      If (aState In [tisLast, tisOnly]) Then
      Begin
        If bTop Then
          Canvas.DrawCurve(oBorder.Colour, oPiece.Background, oBorder.BrushImage, iRight, iY + iwy, iwx Div 4, (iwx Div 4) * 3-1, 0)
        Else
          Canvas.DrawCurve(oBorder.Colour, oPiece.Background, oBorder.BrushImage, iRight, iY - iwy, iwx Div 4, (iwx Div 4) * 3-1, 270);
      End;
      If oPiece.Background <> DEF_COLOUR Then
        If bTop Then
          Canvas.DrawRect(oPiece.Background, iLeft, iBottom, iRight, iY + iwy)
        Else
          Canvas.DrawRect(oPiece.Background, iLeft, iY - iwy, iRight, iTop);
    End
    Else
    Begin
      If bTop Then
      Begin
        iTop := iY;
        If bShared Then
          iBottom := iY + (iwy Div 2)
        Else
          iBottom := iY + iwy;
      End
      Else
      Begin
        If bShared Then
          iTop := iY  - (iwy Div 2) + 1
        Else
          iTop := iY  - iwy + 1;
        iBottom := iY + 1;
      End;
      iLeft := iX1;
      iRight := iX2;
      // counter a window problem where rectangle of 1 width/height is shorten by 1 pixel
      If (iBottom - iTop = 1) Then
        Canvas.DrawLine(1, oBorder.Colour, oBorder.Style, iLeft, iTop, iRight, iTop)
      Else
        Canvas.DrawRect(oBorder.Colour, iLeft, iTop, iRight, iBottom);
    End;
  End;
End;


Procedure TWPRenderer.DrawVerticalBorder(oPiece : TWPWorkingDocumentTableCellStartPiece; oBorder : TWPBorder; iX, iY1, iY2 : Integer; bLeft : Boolean; bShared : Boolean);
Var
  iLeft : Integer;
  iTop : Integer;
  iRight : Integer;
  iBottom : Integer;
  i1, i2: Integer;
  iwx : Integer;
Begin
  If oBorder.Defined And (oBorder.Style <> apsNone) Then
  Begin
    iwx := oBorder.Width * Canvas.PointSizeY;
    If oBorder.Width = 1 Then
    Begin
      If Not (TWPWorkingDocumentTableStartPiece(TWPWorkingDocumentTableRowStartPiece(oPiece.Row).Table).BorderPolicy In [tbpGrid, tbpLines, tbpInnerLines, tbpBox, tbpBoxLines, tbpFancy])
         Or (Not bShared Or bLeft) Then
        Canvas.DrawLine(1, ApplyOutputColourRules(False, oBorder.Colour), oBorder.Style, iX, iY1, iX, iY2);
    End
    Else If oBorder.Fancy Then
    Begin
      i1 := 0;
      i2 := 0;
      If oBorder.LowOuterlimit <> DEF_WORD Then
        i1 := oBorder.LowOuterlimit * Canvas.PointSizeY;
      If oBorder.HighOuterlimit <> DEF_WORD Then
        i2 := oBorder.HighOuterlimit * Canvas.PointSizeY;

      If oBorder.OuterColour2 <> DEF_COLOUR Then
      Begin
        If i1 <> 0 Then
        Begin
          If bLeft Then
            Canvas.DrawRect(oBorder.OuterColour2, iX, iY1, iX + iwx, iY1+i1)
          Else
            Canvas.DrawRect(oBorder.OuterColour2, iX - iwx, iY1, iX+1, iY1+i1);
        End;
        If i2 <> 0 Then
        Begin
          If bLeft Then
            Canvas.DrawRect(oBorder.OuterColour2, iX, iY2-i2, iX + iwx, iY2)
          Else
            Canvas.DrawRect(oBorder.OuterColour2, iX - iwx, iY2-i2, iX+1, iY2);
        End;
      End;


      If oBorder.OuterColour <> DEF_COLOUR Then
        If bLeft Then
          Canvas.DrawRect(oBorder.OuterColour, iX, iY1+i1, iX + iwx, iY2-i2)
        Else
          Canvas.DrawRect(oBorder.OuterColour, iX - iwx, iY1+i1, iX+1, iY2-i2);

      If bLeft Then
      Begin
        If bShared Then
        Begin
          iLeft := iX;
          iRight := iLeft + (iwx Div 4)
        End
        Else
        Begin
          iLeft := iX + (iwx Div 4);
          iRight := iLeft + (iwx Div 2);
        End;
      End
      Else
      Begin
        If bShared Then
        Begin
          iLeft := iX - (iwx Div 2) + 1  + (iwx Div 4);
          iRight := iX + 1;
        End
        Else
        Begin
          iLeft := iX - iwx + 1  + (iwx Div 4);
          iRight := iX + 1 - (iwx Div 4);
        End;
      End;
      iTop := iY1;
      iBottom := iY2;
      If oBorder.BrushImage <> Nil Then
        Canvas.DrawRect(oBorder.BrushImage, iLeft, iTop, iRight, iBottom)
      Else
        Canvas.DrawRect(oBorder.Colour, iLeft, iTop, iRight, iBottom);
      If oPiece.Background <> DEF_COLOUR Then
        If bLeft Then
          Canvas.DrawRect(oPiece.Background, iRight, iTop, iX + iwx, iBottom)
        Else
          Canvas.DrawRect(oPiece.Background, iX - iwx, iTop, iLeft, iBottom);
    End
    Else
    Begin
      If bLeft Then
      Begin
        iLeft := iX;
        If bShared Then
          iRight := iX + iwx Div 2
        Else
          iRight := iX + iwx;
      End
      Else
      Begin
        If bShared Then
          iLeft := iX - iwx Div 2 + 1
        Else
          iLeft := iX - iwx + 1;
        iRight := iX + 1;
      End;
      iTop := iY1;
      iBottom := iY2;
      // counter a window problem where rectangle of 1 width/height is shorten by 1 pixel
      If (iRight - iLeft = 1) Then
        Canvas.DrawLine(1, oBorder.Colour, oBorder.Style, iLeft, iTop, iLeft, iBottom)
      Else
        Canvas.DrawRect(oBorder.Colour, iLeft, iTop, iRight, iBottom);
    End;
  End;
End;


Function TWPRenderer.ProduceStartingSectionMap(oSection: TWPWorkingDocumentSectionStartPiece; aBackground : TColour) : TWPMapItem;
Begin
  Result := TWPMapItem.Create(oSection);
  Try
    Result.Parent := oSection.Container;
    Result.Top := oSection.Container.Top;
    Result.Height := GetFieldCharHeight;
    Result.Left := oSection.Container.Left;
    Result.Width := GetFieldCharWidth;
    Result.Background := aBackground;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TWPRenderer.ProduceClosingSectionMap(oSection: TWPWorkingDocumentSectionStartPiece; aBackground : TColour) : TWPMapItem;
Begin
  Result := TWPMapItem.Create(oSection);
  Try
    Result.Parent := oSection.Container;
    Result.Top := oSection.Container.Bottom - GetFieldCharHeight;
    Result.Height := GetFieldCharHeight;
    Result.Left := oSection.Container.Right - GetFieldCharWidth;
    Result.Width := GetFieldCharWidth;
    Result.Background := aBackground;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPRenderer.PaintSection(oContainer : TWPMapContainer; oPiece: TWPWorkingDocumentSectionStartPiece);
Var
  sText : String;
  iLeft : Integer;
  aBackground : TColour;
  oMap : TWPMapItem;
  oLastPiece : TWPWorkingDocumentPiece;
  oLastContainer : TWPMapContainer;
Begin
  aBackground := oContainer.WorkingBackground(FCurrentHotspot);
  If Assigned(oContainer) Then
  Begin
    If Not Printing Or (aBackground <> Settings.Background) Then
      FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), oContainer.Left, oContainer.Bottom-oContainer.MarginBottom, oContainer.Right, oContainer.Bottom);
    If oContainer.Primary Then
    Begin
      If oPiece.IsField And oPiece.HasContainer Then
      Begin
        FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), oContainer.Left, oContainer.Top, oContainer.InnerLeft, oContainer.Bottom);
        FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), oContainer.InnerRight, oContainer.Top, oContainer.Right, oContainer.Bottom);

        oLastPiece := oPiece;
        While oLastPiece.HasNext And (oLastPiece.PieceType <> ptSectionStop) Do
          oLastPiece := oLastPiece.Next;

        If Assigned(oLastPiece) And oLastPiece.HasPrev And (oLastPiece.Prev Is TWPWorkingDocumentParaPiece) Then
        Begin
          oLastContainer := TWPWorkingDocumentParaPiece(oLastPiece.Prev).Container;
          FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), oContainer.Left, oLastContainer.Bottom, oContainer.Right, oContainer.Bottom);
        End;

        ApplyFieldFont;
        oMap := ProduceStartingSectionMap(oPiece, aBackground);
        Try
          PaintTextItem(oPiece, oMap, '[', fsNormal, fcsNormal, Nil, True, False);
        Finally
          oMap.Free;
        End;
        oMap := ProduceClosingSectionMap(oPiece, aBackground);
        Try
          PaintTextItem(oPiece, oMap, ']', fsNormal, fcsNormal, Nil, True, False);
        Finally
          oMap.Free;
        End;
      End
      Else If (oPiece.DisplayType <> sdtNone) Then
      Begin
        If Not Printing Or (aBackground <> Settings.Background) Then
          FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), oContainer.Left, oContainer.Top, oContainer.Right, oContainer.InnerTop);
        FCanvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsDot, oContainer.Left,  oContainer.Top + 1 + oContainer.MarginTop Div 2, oContainer.Right, oContainer.Top + 1 + oContainer.MarginTop Div 2);

        If oPiece.DisplayType = sdtName Then
        Begin
          sText := oPiece.DisplayName;
          ApplySectionHeaderFont;
          sText := ' '+sText+' ';
          iLeft := IntegerMax(oContainer.InnerLeft, oContainer.Left + 10 * Canvas.PointSizeX);
          If Printing Then
            FCanvas.DrawRect(ApplyOutputColourRules(True, aBackground), iLeft, oContainer.Top, iLeft + FCanvas.TextWidth(sText), oContainer.InnerTop);
          FCanvas.DrawText(ApplyOutputColourRules(True, aBackground), iLeft, oContainer.Top+2, sText);
        End;
      End;
    End;
  End;
End;


Procedure TWPRenderer.PaintBreak(oContainer : TWPMapContainer; oPiece: TWPWorkingDocumentBreakPiece);
Var
  aColour : TColour;
  sText : String;
  iLeft : Integer;
  iRight : Integer;
Begin
  If Assigned(oContainer) Then
  Begin
    If oPiece.Map.Selection = wsNone Then
      aColour := oContainer.WorkingBackground(CurrentHotspot)
    Else
      aColour := clBlack;
    FCanvas.DrawRect(ApplyOutputColourRules(True, aColour), oContainer.Left, oContainer.Top, oContainer.Right, oContainer.Bottom);
    Case oPiece.BreakType Of
      btLine:
      Begin
      Case oPiece.Alignment Of
        WordProcessorAlignmentLeft :
          Begin
          iLeft := oContainer.Left;
          iRight := oContainer.Left + Trunc(oContainer.Width * oPiece.Width);
          End;
        WordProcessorAlignmentRight :
          Begin
          iLeft := oContainer.Right - Trunc(oContainer.Width * oPiece.Width);
          iRight := oContainer.Right;
          End;
      Else // Centre is the default for legacy reasons
        // Calc. from left/right instead of centre to avoid drawing outside container boundary
        iLeft := oContainer.Left + Trunc((oContainer.Width * (1 - oPiece.Width)) / 2);
        iRight := oContainer.Right - Trunc((oContainer.Width * (1 - oPiece.Width)) / 2);
      End;
      FCanvas.DrawLine(oPiece.PenWidth, ApplyOutputColourRules(False, oPiece.PenColour), oPiece.PenStyle, oPiece.EndStyle, iLeft, (oContainer.Top + oContainer.Bottom) Div 2, iRight, (oContainer.Top + oContainer.Bottom) Div 2);
      End;

      btPageBreak :
      Begin
        If Not Printing Then
        Begin
          FCanvas.DrawLine(1, ApplyOutputColourRules(False, clGray), apsDot, oContainer.Left,  (oContainer.Top + oContainer.Bottom) Div 2, oContainer.Right, (oContainer.Top + oContainer.Bottom) Div 2);
          ApplySectionHeaderFont;
          sText := ' Page Break ';
          iLeft := IntegerMax(oContainer.InnerLeft, oContainer.Left + 10 * Canvas.PointSizeX);
          FCanvas.DrawText(ApplyOutputColourRules(False, aColour), iLeft, oContainer.Top+2, sText);
        End;
      End;

    End;
  End;
End;


Procedure TWPRenderer.PaintText(oMap : TWPMapItem; oPiece: TWPWorkingDocumentTextPiece; bUnderlineHotspot : Boolean);
Var
  oStyle : TWPStyle;
Begin
  oStyle := Styles.GetByNameOrDefault(oPiece.Style);
  If Settings.EditHints And (oPiece.VisualText = ' ') And (oMap.OffsetStart = 0) Then
    PaintTextItem(oPiece, oMap, CHAR_SPACE_HINT, ApplyFont(oPiece, oStyle, clGray), oStyle.WorkingFontCapitalization(oPiece.Font), oStyle, False, bUnderlineHotspot)
  Else
    PaintTextItem(oPiece, oMap, Copy(oPiece.VisualText, oMap.OffsetStart + 1, oMap.OffsetLength), ApplyFont(oPiece, oStyle), oStyle.WorkingFontCapitalization(oPiece.Font), oStyle, False, bUnderlineHotspot);
  PaintAnnotationDecoration(oMap, RowForItem(oMap), oPiece);
End;


Function TWPRenderer.Printing : Boolean;
Begin
  Result := False;
End;


Procedure TWPRenderer.AddToContainer(oContainer, oAdded : TWPMapContainer; Var iContainerCursor : Integer);
Var
  iIndex : Integer;
Begin
  oAdded.Parent := oContainer;

  If (iContainerCursor >= oContainer.Children.Count) Then
    oContainer.Children.Add(oAdded.Link)
  Else If (oContainer.Children[iContainerCursor] <> oAdded) Then
  Begin
    iIndex := oContainer.Children.IndexByReference(oAdded);
    If iIndex = -1 Then
      oContainer.Children.Insert(iContainerCursor, oAdded.Link)
    Else
    Begin
      Assert(CheckCondition(iIndex > iContainerCursor, 'AddToContainer', StringFormat('Already exists in the list before addition point (%d/%d)', [iIndex, iContainerCursor])));
      oContainer.Children.DeleteRange(iContainerCursor, iIndex - 1);
      Assert(CheckCondition(oContainer.Children[iContainerCursor] = oAdded, 'AddToContainer', 'Delete Range Failed'));
    End;
  End;

  Inc(iContainerCursor)
End;


Procedure TWPRenderer.TrimContainer(oContainer : TWPMapContainer; iContainerCursor : Integer);
Begin
  If iContainerCursor < oContainer.Children.Count Then
    oContainer.Children.DeleteRange(iContainerCursor, oContainer.Children.Count - 1);
End;


Procedure TWPRenderer.AdjustHeightOfChildren(oContainer : TWPMapContainer; iStart, iOffset, iLineOffset : Integer);
Var
  iLoop : Integer;
Begin
  For iLoop := iStart To oContainer.Children.Count - 1 Do
    oContainer.Children[iLoop].AdjustTop(iOffset, iLineOffset, False);
End;


Procedure TWPRenderer.MarkVerticalAdjustment(iTop, iOffset : Integer);
Begin
  Assert(CheckCondition(FAdjustTop = 0, 'MarkVerticalAdjustment', 'Attempt to specify more than one vertical adjustment'));
  FAdjustTop := iTop;
  FAdjustOffset := iOffset;
End;


Procedure TWPRenderer.ApplyVerticalAdjustment;
Begin
  If FAdjustTop <> 0 Then
    FCanvas.MoveVertical(FAdjustTop, FAdjustOffset);
  FAdjustTop := 0;
  FAdjustOffset := 0;
End;


Function TWPRenderer.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWPRenderer.GetMap : TWPMapContainer;
Begin
  Assert(Invariants('GetMap', FMap, TWPMapContainer, 'Map'));
  Result := FMap;
End;


Function TWPRenderer.GetCanvas : TWPCanvas;
Begin
  Assert(Invariants('GetCanvas', FCanvas, TWPCanvas, 'Canvas'));
  Result := FCanvas;
End;


Function TWPRenderer.GetDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Function TWPRenderer.GetSelection : TWPSelection;
Begin
  Assert(Invariants('GetSelection', FSelection, TWPSelection, 'Selection'));
  Result := FSelection;
End;


Function TWPRenderer.GetStyles : TWPStyles;
Begin
  Assert(Invariants('GetStyles', FStyles, TWPStyles, 'Styles'));
  Result := FStyles;
End;


Procedure TWPRenderer.SetMeasureAll(Const Value : Boolean);
Begin
  FMeasureAll := Value;
  If Value Then
  Begin
    Valid := False;
    PaintAll := True;
  End;
End;


Procedure TWPRenderer.ChangeSettings;
Begin
  FValid := False;
  MeasureAll := True;
  PaintAll := True;
  BuildMetrics;
  Update;
End;


Procedure TWPRenderer.SetWidth(Const Value: Integer);
var
  iLoop : integer;
Begin
  If FWidth <> Value Then
    Begin
    FWidth := Value;
    FValid := False;
    For iLoop := 0 To FDocument.Pieces.Count - 1 Do
      if (FDocument.Pieces[iLoop].PieceType = ptImage) and (TWPWorkingDocumentImagePiece(FDocument.Pieces[iLoop]).SizePolicy <> ImageSizeManual) then
        FDocument.Pieces[iLoop].Metrics.Valid := false;
    Update;
    End;
End;


Procedure TWPRenderer.NeedPaintByHotspot(Const Value: TWPHotspot);
Var
  iLoop : Integer;
  iInner : Integer;
  oPiece : TWPWorkingDocumentPiece;
  oCont : TWPWorkingDocumentContainerPiece;
  oMap : TWPMapItem;
Begin
  For iLoop := 0 To FDocument.Pieces.Count - 1 Do
  Begin
    oPiece := FDocument.Pieces[iLoop];
    For iInner := 0 To oPiece.Maps.Count - 1 Do
    Begin
      oMap := oPiece.Maps[iInner];
      If (oMap.ForeHotspot = Value) Or (oMap.BackHotspot = Value) Then
        oMap.Painted := False;
    End;
    If (oPiece Is TWPWorkingDocumentContainerPiece) Then
    Begin
      oCont := TWPWorkingDocumentContainerPiece(oPiece);
      If oCont.HasContainer And (oCont.Container.BackHotspot = Value) Then
        oCont.Container.Painted := False;
    End;
  End;
End;


Procedure TWPRenderer.SetCurrentHotspot(Const Value: TWPHotspot);
Begin
  If (FCurrentHotspot <> Value) Then
    Begin
    If (Assigned(FCurrentHotspot)) Then
      NeedPaintByHotspot(FCurrentHotspot);
    FCurrentHotspot.Free;
    FCurrentHotspot := Value;
    If (Assigned(FCurrentHotspot)) Then
      NeedPaintByHotspot(FCurrentHotspot);
    DoUpdate;
    End
  Else
   Value.Free;
End;


Procedure TWPRenderer.NeedPaintByButton(Const oButton : TFslObject);
Var
  iInner : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  If (oButton Is TWPWorkingDocumentPiece) Then
  Begin
    oPiece := TWPWorkingDocumentPiece(oButton);
    For iInner := 0 To oPiece.Maps.Count - 1 Do
      oPiece.Maps[iInner].Painted := False;
    If (oPiece Is TWPWorkingDocumentContainerPiece) Then
      If TWPWorkingDocumentContainerPiece(oPiece).HasContainer Then
        TWPWorkingDocumentContainerPiece(oPiece).Container.Painted := False;
  End;
End;


Procedure TWPRenderer.SetCurrentButton(Const Value: TFslObject);
Begin
  If (FCurrentButton <> Value) Then
    Begin
    If (Assigned(FCurrentButton)) Then
      NeedPaintByButton(FCurrentButton);
    FCurrentButton.Free;
    FCurrentButton := Value;
    If (Assigned(FCurrentButton)) Then
      NeedPaintByButton(FCurrentButton);
    DoUpdate;
    End;
End;




Function TWPRenderer.ApplyOutputColourRules(bBackground: Boolean; aColour: TColour): TColour;
Begin
  Result := aColour;
End;


function TWPRenderer.SpaceForAnnotations: Integer;
begin
  If Printing Or (FDocument.DrawnAnnotations.Count = 0) Then
    Result := 0
  Else
    Result := Settings.AnnotationWidth;
end;

procedure TWPRenderer.PaintAnnotationDecoration(oMap : TWPMapItem; oRow : TWPMapRow; oPiece: TWPWorkingDocumentPiece);
Var
  oCoords : TWPCoordinateList;
begin
  if Printing Then
    Exit;

  // first, do we paint our own annotation?
  Case oPiece.AnnotationStatus Of
   AnnotationStatusAll:
      Begin
        oCoords := TWPCoordinateList.Create;
        Try
          oCoords.Add(oMap.Right-1, oRow.Bottom-1);
          oCoords.Add(oMap.Right-1, oRow.Top);
          oCoords.Add(oMap.Left, oRow.Top);
          oCoords.Add(oMap.Left, oRow.Bottom-1);
          FCanvas.DrawPolyLine(Document.AllAnnotations[oPiece.AnnotationId-1].Colour, apsDot, apesSquare, 0,0, oCoords, 1, 1, true);
        Finally
          oCoords.Free;
        End;
      End;
    AnnotationStatusStart:
      Begin
        oCoords := TWPCoordinateList.Create;
        Try
          oCoords.Add(oMap.Right-1, oRow.Top);
          oCoords.Add(oMap.Left, oRow.Top);
          oCoords.Add(oMap.Left, oRow.Bottom-1);
          oCoords.Add(oMap.Right-1, oRow.Bottom-1);
          FCanvas.DrawPolyLine(Document.AllAnnotations[oPiece.AnnotationId-1].Colour, apsDot, apesSquare, 0,0, oCoords, 1, 1, false);
        Finally
          oCoords.Free;
        End;
      End;
    AnnotationStatusEnd:
      Begin
        oCoords := TWPCoordinateList.Create;
        Try
          oCoords.Add(oMap.Left, oRow.Top);
          oCoords.Add(oMap.Right-1, oRow.Top);
          oCoords.Add(oMap.Right-1, oRow.Bottom-1);
          oCoords.Add(oMap.Left, oRow.Bottom-1);
          FCanvas.DrawPolyLine(Document.AllAnnotations[oPiece.AnnotationId-1].Colour, apsDot, apesSquare, 0,0, oCoords, 1, 1, false);
        Finally
          oCoords.Free;
        End;
      End;
    AnnotationStatusContinue:
      Begin
        FCanvas.DrawLine(1, Document.AllAnnotations[oPiece.AnnotationId-1].Colour, apsDot, oMap.Left, oRow.Bottom-1, oMap.Right, oRow.Bottom - 1);
        FCanvas.DrawLine(1, Document.AllAnnotations[oPiece.AnnotationId-1].Colour, apsDot, oMap.Left, oRow.Top, oMap.Right, oRow.Top);
      End;
    // oPiece.AnnotationStatus = AnnotationStatusNone. ok, do we paint someone elses?
   Else if (oMap.AnnotationColour <> clTransparent) Then
     Canvas.DrawLine(1, oMap.AnnotationColour, apsDot, oMap.Left, oRow.Bottom-1, oMap.Right, oRow.Bottom - 1);
   End;
End;

function TWPRenderer.ShowHotspots(draw : boolean): Boolean;
begin
  result := (Settings.Hotspots = wphmAlways) or ((Settings.Hotspots = wphmAltKey) and (draw or AltKeyDown));
end;

procedure TWPRenderer.SetAltKeyDown(const Value: Boolean);
begin
  if FAltKeyDown <> Value then
  begin
    FAltKeyDown := Value;
    PaintAll := true;
    Update;
  end;
end;

function TWPRenderer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FSelection.sizeInBytes);
  inc(result, FMap.sizeInBytes);
  inc(result, FStyles.sizeInBytes);
  inc(result, FCanvas.sizeInBytes);
  inc(result, FStateStack.sizeInBytes);
  inc(result, FDefaultTableBorder.sizeInBytes);
  inc(result, FCurrentHotspot.sizeInBytes);
  inc(result, FCurrentButton.sizeInBytes);
end;

Constructor TWPScreenRenderer.Create;
Begin
  Inherited;
  Canvas := TWPScreenCanvas.Create;
  Canvas.PointSizeX := 1;
  Canvas.PointSizeY := 1;
  Canvas.OnUpdateImage := PaintCanvas;
  SetLength(FPageOffsets, 0);
End;


Destructor TWPScreenRenderer.Destroy;
Begin
  FOperator.Free;
  Inherited;
End;


Function TWPScreenRenderer.Link : TWPScreenRenderer;
Begin
  Result := TWPScreenRenderer(Inherited Link);
End;


Function TWPScreenRenderer.Clone : TWPScreenRenderer;
Begin
  Result := TWPScreenRenderer(Inherited Clone);
End;


Function TWPScreenRenderer.GetCanvas : TWPScreenCanvas;
Begin
  Assert(Invariants('GetCanvas', Inherited Canvas, TWPScreenCanvas, 'Canvas'));
  Result := TWPScreenCanvas(Inherited Canvas);
End;


Procedure TWPScreenRenderer.SetCanvas(Const Value : TWPScreenCanvas);
Begin
  Inherited Canvas := Value;
End;


Procedure TWPScreenRenderer.SetOperator(Const Value : TWPOperator);
Begin
  FOperator.Free;
  FOperator := Value;
End;

Function TWPScreenRenderer.PaintControl(aHandle : THandle; iControlHeight, iControlWidth, iOffset : Integer) : Integer;
Begin
  Result := Canvas.PaintControl(aHandle, iControlHeight, iControlWidth, iOffset);
End;

Procedure TWPScreenRenderer.DoUpdate;
Var
  bAll : Boolean;
Begin
  // do we need to redo the metrics?
  If (FLastVersion <> Operator.Version) Or Not Valid Then
  Begin
//    DebugStep('init -);
    If (Not Map.HasPiece Or (Map.Piece <> Document)) Then
      InitialiseMap;

//    DebugStep('measure);
    MeasurePieces(MeasureAll);
//    DebugStep('layout);
    LayoutDocument(Document, FOperator.RendererRange);
//    DebugStep('pa);
    CheckPageOffsets;

    FLastVersion := Operator.Version;
    Valid := True;
    MeasureAll := False;
    FOperator.RendererRange.Clear;
  End;

//  DebugStep('app);
  ApplySelectionToDocument;
  bAll := Canvas.SetImageSize(Map.Width + SpaceForAnnotations, Map.Height);
//  DebugStep('app);
  ApplyVerticalAdjustment;
//  DebugStep('paint);
  Paint(bAll Or PaintAll);
  PaintAll := False;
//  DebugStep('done);
End;


Procedure TWPScreenRenderer.Paint(bAll : Boolean);
Var
  oPiece : TWPWorkingDocumentPiece;
Begin
  PaintMargins;
  oPiece := Document.Pieces[0];
  While Assigned(oPiece) Do
  Begin
    PaintPiece(oPiece, bAll);
    oPiece := oPiece.Next;
  End;
  if SpaceForAnnotations > 0 Then
    PaintAnnotations(bAll);
End;


Procedure TWPScreenRenderer.PaintPiece(oPiece : TWPWorkingDocumentPiece; bPaintAnyway : Boolean);
Var
  oMap : TWPMapItem;
  iLoop : Integer;
  oContainer : TWPMapContainer;
Begin
  Assert(Invariants('PaintPiece', oPiece, TWPWorkingDocumentPiece, 'oPiece'));

  If oPiece.PieceType In [ptTableStart, ptCellStart, ptRowStart, ptSectionStart, ptBreak] Then
  Begin
    oContainer := TWPMapContainer(TWPWorkingDocumentContainerPiece(oPiece).Container);
    If Assigned(oContainer) And Canvas.Visible(oContainer.Top, oContainer.Bottom) And (bPaintAnyway Or Not oContainer.Painted) Then
      Case oPiece.PieceType Of
        ptTableStart : PaintTable(oContainer, TWPWorkingDocumentTableStartPiece(oPiece));
        ptRowStart : PaintTableRow(oContainer, TWPWorkingDocumentTableRowStartPiece(oPiece));
        ptCellStart : PaintTableCell(oContainer, TWPWorkingDocumentTableCellStartPiece(oPiece));
        ptSectionStart : PaintSection(oContainer, TWPWorkingDocumentSectionStartPiece(oPiece));
        ptTableStop : ;
        ptRowStop : ;
        ptCellStop : ;
        ptSectionStop : ; // nothing to do
        ptBreak : PaintBreak(oContainer, TWPWorkingDocumentBreakPiece(oPiece));
      Else
        RaiseError('PaintPiece', 'unknown Piece Type 1 ['+NAMES_WPPIECETYPE[oPiece.PieceType]+']');
      End;
    oContainer.Painted := True;
  End
  Else
  For iLoop := 0 To oPiece.Maps.Count - 1 Do
    Begin
      oMap := oPiece.Maps[iLoop];

      If Canvas.Visible(oMap.Top, oMap.Bottom) And (bPaintAnyway Or Not oMap.Painted) Then
      Begin
        Case oPiece.PieceType Of
          ptText : If oMap.InHotspot Then
              PaintText(oMap, TWPWorkingDocumentTextPiece(oPiece), Not oMap.isLastInLine Or (TWPWorkingDocumentTextPiece(oPiece).VisualText <> ' '))
            Else
              PaintText(oMap, TWPWorkingDocumentTextPiece(oPiece), False);
          ptImage : PaintImage(oMap, TWPWorkingDocumentImagePiece(oPiece));
          ptFieldStart : PaintFieldStart(oMap, TWPWorkingDocumentFieldStartPiece(oPiece));
          ptFieldStop : PaintFieldEnd(oMap, TWPWorkingDocumentFieldStopPiece(oPiece));
          ptLineBreak : PaintLineBreak(oMap, TWPWorkingDocumentLineBreakPiece(oPiece));
          ptPara :
            Begin
              PaintParaContainer(TWPWorkingDocumentParaPiece(oPiece).Container, TWPWorkingDocumentParaPiece(oPiece));
              PaintParaMap(oMap, TWPWorkingDocumentParaPiece(oPiece));
              TWPWorkingDocumentParaPiece(oPiece).Container.Painted := True;
            End;
        Else
          RaiseError('PaintPiece', 'unknown Piece Type 2 ['+NAMES_WPPIECETYPE[oPiece.PieceType]+']');
        End;
        oMap.Painted := True;
      End;
    End;
End;


Function TWPScreenRenderer.IsEdge(oItem : TWPMapItem; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  bTop : Boolean;
  bBottom : Boolean;
  bLeft : Boolean;
  bRight : Boolean;
Begin
  bTop := (iY >= oItem.Top) And (iY <= oItem.Top + EDGE_GRACE);
  bBottom := (iY <= oItem.Bottom) And (iY >= oItem.Bottom - EDGE_GRACE);
  bLeft := (iX >= oItem.Left) And (iX <= oItem.Left + EDGE_GRACE);
  bRight := (iX <= oItem.Right) And (iX >= oItem.Right - EDGE_GRACE);
  Result := True;
  oInfo.Subject := oItem.Piece.Link;
  If (bTop And bBottom) Or (bLeft And bRight) Then
    Result := False
  Else If (bTop And bLeft) Then
    oInfo.EdgeAction := eaLeftTop
  Else If (bBottom And bRight) Then
    oInfo.EdgeAction := eaRightBottom
  Else If (bTop And bRight) Then
    oInfo.EdgeAction := eaRightTop
  Else If (bBottom And bLeft) Then
    oInfo.EdgeAction := eaLeftBottom
  Else If (bLeft) Then
    oInfo.EdgeAction := eaLeft
  Else If (bRight) Then
    oInfo.EdgeAction := eaRight
  Else If (bTop) Then
    oInfo.EdgeAction := eaTop
  Else If (bBottom) Then
    oInfo.EdgeAction := eaBottom
  Else
    Result := False;
End;


Function TWPScreenRenderer.GetMouseInfoInItem(oItem : TWPMapItem; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iLoop : Integer;
  iBase : Integer;
  iClosest : Integer;
  iGap : Integer;
  iLeft : Integer;
  oImage : TWPWorkingDocumentImagePiece;
Begin
  Assert(Invariants('GetOffsetInItem', oItem.Piece, TWPWorkingDocumentPiece, 'oItem.Piece'));

  Result := True;

  iLoop := oItem.OffsetStart;
  iBase := 0;

  oPiece := TWPWorkingDocumentPiece(oItem.Piece);
  if (oPiece.FieldHint <> '') and (oPiece.FieldError <> '') then
  begin
    oInfo.HintText := oPiece.FieldHint+' (Error: '+oPiece.FieldError+')';
    oInfo.HintError := True;
  end
  else
  begin
    oInfo.HintText := oPiece.FieldHint;
    oInfo.HintError := False;
  end;

  if (oInfo.HintText = '') then
  begin
    if (oItem.Piece Is TWPWorkingDocumentFieldStartPiece) And TWPWorkingDocumentFieldStartPiece(oItem.Piece).HasDefinitionProvider then
      oInfo.HintText := TWPWorkingDocumentFieldStartPiece(oItem.Piece).DefinitionProvider.HintForField(TWPWorkingDocumentFieldStartPiece(oItem.Piece).DocField, fhmFull)
    else if (oItem.Piece Is TWPWorkingDocumentFieldStopPiece) And TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.HasDefinitionProvider then
      oInfo.HintText := TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.DefinitionProvider.HintForField(TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.DocField, fhmFull)
  end;

  If (oItem.Piece Is TWPWorkingDocumentFieldStopPiece) And TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.HasDefinitionProvider And
     TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.DefinitionProvider.HasUIAssistance(TWPWorkingDocumentFieldStopPiece(oItem.Piece).MatchingStart.DocField) And
     oItem.ContainsPoint(iX, iY, FIELD_DROPPER_WIDTH) Then
  Begin
    oInfo.ProposedAction := msButton;
    oInfo.Subject := oItem.Piece.Link;
  End
  Else If (oItem.Piece Is TWPWorkingDocumentImagePiece) Then
  Begin
    oImage := TWPWorkingDocumentImagePiece(oItem.Piece);
    oInfo.Subject := oImage.Link;
    If IsInHotSpot(oItem, oImage, iX, iY, oInfo) Then
      oInfo.ProposedAction := msHotspot
    Else If not Settings.ReadOnly and (not TWPWorkingDocumentPiece(oItem.Piece).IsReadOnly) And oImage.IsAdornment(oInfo, Trunc((iX - oItem.Left) * (oImage.Image.Width / oImage.Width)), Trunc((iY-oItem.Top) * (oImage.Image.Height / oImage.Height))) Then
      oInfo.ProposedAction := msImageTool
    Else If not Settings.ReadOnly and (not TWPWorkingDocumentPiece(oItem.Piece).IsReadOnly) And (oImage.SizePolicy = ImageSizeManual) And IsEdge(oItem, iX, iY, oInfo) And Not oImage.IsReadOnly And Not Settings.ReadOnly And (FOperator.CurrentImageTool = itSelect) Then
      oInfo.ProposedAction := msDragEdge
    Else
      oInfo.ProposedAction := msImageTool;
  End
  Else If oItem.InHotspot And ShowHotspots(false) And oItem.ContainsPoint(ix, iY) Then
  Begin
    oInfo.ProposedAction := msHotspot;
    oInfo.Hotspot := oItem.WorkingHotspot.Link;
  End
  Else If oItem.InHotspot And ShowHotspots(false) And Assigned(oItem.BackHotspot) Then
  Begin
    oInfo.ProposedAction := msHotspot;
    oInfo.Hotspot := oItem.BackHotspot.Link;
  End
  Else
    oInfo.ProposedAction := msSelect;


  If oPiece.PieceType In [ptLineBreak, ptPara] Then
    oInfo.Offset := oPiece.Metrics.Position
  Else if (oPiece.PieceType = ptFieldStart) and (TWPWorkingDocumentFieldStartPiece(oPiece).Checkables.Count > 0) Then
  Begin
    oInfo.ProposedAction := msButton;
    oInfo.Subject := oPiece.Link;
    Repeat
      iLeft := (oPiece.Metrics.Offset[iLoop] - oPiece.Metrics.Offset[oItem.OffsetStart]) - iBase + oItem.Left + oItem.Nose;
      Inc(iLoop);
    Until (iLoop > oItem.OffsetStart + oItem.OffsetLength) Or (iLeft > iX);
    oInfo.Offset := oPiece.Metrics.Position + (iLoop - 1);
  End
  Else
    Begin
    iBase := oPiece.Metrics.Offset[iLoop];
    iClosest := iLoop;
    iGap := MAXINT;
    Repeat
      iLeft := oPiece.Metrics.Offset[iLoop] - iBase + oItem.Left + oItem.Nose;
      If abs(iX - iLeft) < iGap Then
        Begin
        iGap := abs(iX - iLeft);
        iClosest := iLoop;
        End;
      Inc(iLoop);
    Until (iLoop > oItem.OffsetStart + oItem.OffsetLength) Or (iLeft > iX);
    oInfo.Offset := oPiece.Metrics.Position + iClosest;
    End;
End;


Function TWPScreenRenderer.GetMouseInfoInRow(oRow : TWPMapRow; iX, iY, iLimit1, iLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  iLoop : Integer;
Begin
  iLoop := 0;
  While (iLoop < oRow.Items.Count) And (iX > oRow.Items[iLoop].Right) Do
    Inc(iLoop);
  If (iLoop < oRow.Items.Count) And ((iLoop > 0) Or (iX >= iLimit1) Or (Not ShowHotspots(false))) Then
    Result := GetMouseInfoInItem(oRow.Items[iLoop], iX, iY, oInfo)
  Else If (iLoop >= oRow.Items.Count) And ((oRow.Items.Count > 0) And (Not ShowHotspots(false) Or (ix <= iLimit2))) Then
    Result := GetMouseInfoInItem(oRow.Items[oRow.Items.Count - 1], iX, iY, oInfo)
  Else
    Result := False;
End;


Function TWPScreenRenderer.GetMouseInfoInRows(oRows : TWPMapRows; iX, iY, iLimit1, iLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  iLoop : Integer;
Begin
  iLoop := 0;
  While (iLoop < oRows.Count) And ( iY > oRows[iLoop].Bottom) Do
    Inc(iLoop);
  If iLoop < oRows.Count Then
    Result := GetMouseInfoInRow(oRows[iLoop], iX, iY, iLimit1, iLimit2, oInfo)
  Else If oRows.Count > 0 Then
    Result := GetMouseInfoInRow(oRows[oRows.Count - 1], iX, iY, iLimit1, iLimit2, oInfo)
  Else
    Result := False;
End;


Function TWPScreenRenderer.GetMouseInfoInContainers(oContainers : TWPMapContainers; bHorizontal : Boolean; iX, iY, iYLimit, iXLimit1, iXLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  iLoop : Integer;
  iLower : Integer;
Begin
  iLoop := 0;
  iLower := 0;
  If bHorizontal Then
    While (iLoop < oContainers.Count) And ( iX > oContainers[iLoop].Right) Do
      Inc(iLoop)
  Else
    While (iLoop < oContainers.Count) And ( iY > oContainers[iLoop].Bottom) Do
      Inc(iLoop);

  If (oContainers.Count > 0) Then
    If bHorizontal Then
      iLower := oContainers[0].Left
    Else
      iLower := oContainers[0].Top;

  If (iLoop < oContainers.Count) Then
  Begin
    If (Not ShowHotspots(false) Or (iY >= iLower)) Then
      Result := GetMouseInfoInContainer(oContainers[iLoop], iX, iY, iYLimit, iXLimit1, iXLimit2, oInfo)
    Else
      Result := False;
  End
  Else If (oContainers.Count > 0) And (Not ShowHotspots(false) Or (iY < iYLimit))  Then
    Result := GetMouseInfoInContainer(oContainers[oContainers.Count - 1], iX, iY, iYLimit, iXLimit1, iXLimit2, oInfo)
  Else
    Result := False;
End;


Function TWPScreenRenderer.GetMouseInfoInContainer(oContainer : TWPMapContainer; iX, iY, iYLimit, iXLimit1, iXLimit2 : Integer; oInfo : TWPMouseInfo) : Boolean;
Begin
  If oContainer.Piece Is TWPWorkingDocumentTableRowStartPiece Then
  Begin
    iXLimit1 := oContainer.Left;
    iXLimit2 := oContainer.Right;
  End;

  iYLimit := oContainer.Bottom;
  If oContainer.Rows.Count > 0 Then
    Result := GetMouseInfoInRows(oContainer.Rows, iX, iY, iXLimit1, iXLimit2, oInfo)
  Else
    Result := GetMouseInfoInContainers(oContainer.Children, oContainer.ChildrenHorizontal, iX, iY, iYLimit, iXLimit1, iXLimit2, oInfo);
End;


Function TWPScreenRenderer.GetPointForOffset(iOffset : Integer; Var iX, iY, iHeight : Integer; Var aColour : TColour) : Boolean;
Var
  oRegion : TWPMapItem;
Begin
  iX := 0;
  iY := 0;
  iHeight := 0;
  Result := HasDocument And GetPointForOffset(iOffset, iX, iY, iHeight, aColour, oRegion);
End;


Function TWPScreenRenderer.GetPointForOffset(iOffset : Integer; Var iX, iY, iHeight : Integer; Var aColour : TColour; Out oRegion : TWPMapItem) : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  oMap : TWPMapItem;
Begin
  Result := Document.GetPieceByPosition(iOffset, oPiece, iInternal, iIndex);
  If Result Then
  Begin
    oMap := oPiece.Maps.GetByOffset(iInternal);
    Result := Assigned(oMap);
    If Result Then
    Begin
      iX := oMap.Left + (oPiece.Metrics.Offset[iInternal] - oPiece.Metrics.Offset[oMap.OffsetStart]);
      iY := oMap.Top;
      iHeight := oMap.Height;
      oRegion := oMap;
      If oPiece.Font.Background <> DEF_COLOUR Then
        aColour := oPiece.Font.Background
      Else
        aColour := oMap.Background;
    End;
  End;
End;


Function TWPScreenRenderer.GetMouseInfoForPoint(iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
var
  iLoop : integer;
  oAnnot : TWPWorkingAnnotation;
Begin
  if (SpaceForAnnotations = 0) Or (ix < Width - SpaceForAnnotations) Then
  Begin
    Result := GetMouseInfoInContainer(Map, iX, iY, Map.Height, Map.Left, Map.Right, oInfo);
    If Result And (oInfo.Offset = Document.CharCount) Then
      oInfo.Offset := oInfo.Offset - 1;
  End
  Else if (ix < Width - SpaceForAnnotations + 4) Then
  Begin
    result := true;
    oInfo.ProposedAction := msDragEdge;
    oInfo.EdgeAction := eaLeft;
    oInfo.Offset := -1;
    oInfo.Subject := Document.AllAnnotations.Link;
    oInfo.HintText := 'Adjust Annotations Width';
  End
  Else
  Begin
    result := False;
    For iLoop := 0 to Document.AllAnnotations.Count - 1 Do
    Begin
      oAnnot := Document.AllAnnotations[iLoop];
      if oAnnot.InUse and (oAnnot.Top <= iY) and (oAnnot.Bottom >= iY) Then
      Begin
        result := true;
        oInfo.ProposedAction := msHotspot;
        oInfo.Subject := oAnnot.Link;
        break;
      End;
    End;
    if not result then
    Begin
      result := true;
      oInfo.ProposedAction := msNull;
      oInfo.Offset := -1;
    End;
  End
End;


Procedure TWPScreenRenderer.PaintMargins;
Begin
  // we don't need to do this
  //  Canvas.DrawRect(Background, 0, 0, Canvas.Width, Margin);

  Canvas.DrawRect(Settings.Background, 0, 0, Settings.HorizontalMargin, Canvas.Height);
  If HasPaginations Then
    PaintPageMarkers;

  // draw the right and last margin - make sure it isn't full of crap if the document has shrunk
  Canvas.DrawRect(RandomColour(Settings.Background, False), 0, Map.Bottom - Settings.VerticalMargin, Canvas.Width, Map.Bottom);
  Canvas.DrawRect(RandomColour(Settings.Background, False), Canvas.Width - Settings.HorizontalMargin, 0, Canvas.Width, Canvas.Height);
End;



Function TWPScreenRenderer.GetEffectiveTopOfRow(oRow : TWPMapRow) : Integer;
Var
  oContainer : TWPMapContainer;
  oChild : TWPMapContainer;
Begin
  oContainer := TWPMapContainer(oRow.Parent);
  If oContainer.Rows.IndexByReference(oRow) = 0 Then
  Begin
    Result := oContainer.Top;
    oChild := oContainer;
    oContainer := TWPMapContainer(oContainer.Parent);
    // actually, the first container with rows, not the first container
    While Assigned(oContainer) And (oContainer.ChildrenHorizontal Or (oContainer.Children.IndexByReference(oChild) = 0)) Do
    Begin
      Result := oContainer.Top;
      oChild := oContainer;
      If oContainer.HasParent Then
        oContainer := TWPMapContainer(oContainer.Parent)
      Else
        oContainer := Nil;
    End;
  End
  Else
    Result := oRow.Top;
End;


Function TWPScreenRenderer.GetEffectiveBottomOfRow(oRow : TWPMapRow) : Integer;
Var
  oContainer : TWPMapContainer;
  oChild : TWPMapContainer;
Begin
  oContainer := TWPMapContainer(oRow.Parent);
  If oContainer.Rows.IndexByReference(oRow) = oContainer.Rows.Count - 1 Then
  Begin
    Result := oContainer.Bottom;
    oChild := oContainer;
    oContainer := TWPMapContainer(oContainer.Parent);
    // actually, the last container with rows, not the last container
    While Assigned(oContainer) And (oContainer.ChildrenHorizontal Or (oContainer.Children.IndexByReference(oChild) = oContainer.Children.Count - 1)) Do
    Begin
      Result :=  oContainer.Bottom;
      oChild := oContainer;
      If oContainer.HasParent Then
        oContainer := TWPMapContainer(oContainer.Parent)
      Else
        oContainer := Nil;
    End;
  End
  Else
    Result := oRow.Bottom;
End;


Function TWPScreenRenderer.GetRowForRegion(oRegion : TWPMapItem; Out oRow : TWPMapRow) : Boolean;
Begin
  Result := Assigned(oRegion);
  If Assigned(oRegion) Then
    oRow := oRegion.Parent As TWPMapRow;
End;


Function TWPScreenRenderer.PageUp(Const iCurrent, iPageHeight : Integer; Var iNew : Integer) : Boolean;
Var
  iX, iY, iHeight : Integer;
  oRegion : TWPMapItem;
  oRow : TWPMapRow;
  aColour : TColour;
  oInfo : TWPMouseInfo;
Begin
  Result := GetPointForOffset(iCurrent, iX, iY, iHeight, aColour, oRegion) And
            GetRowForRegion(oRegion, oRow);
  If Result Then
  Begin
    iY := GetEffectiveTopOfRow(oRow);
    oInfo := TWPMouseInfo.Create;
    Try
      Result := GetMouseInfoForPoint(iX, iY-(iPageHeight+1), oInfo) And (oInfo.Offset <> iCurrent);
      iNew := oInfo.Offset;
    Finally
      oInfo.Free;
    End;
  End;
End;


Function TWPScreenRenderer.LineUp(Const iCurrent : Integer; Var iNew : Integer) : Boolean;
Var
  iX, iY, iHeight : Integer;
  oRegion : TWPMapItem;
  oRow : TWPMapRow;
  aColour : TColour;
  oInfo : TWPMouseInfo;
Begin
  Result := GetPointForOffset(iCurrent, iX, iY, iHeight, aColour, oRegion) And
            GetRowForRegion(oRegion, oRow);
  If Result Then
  Begin
    iY := GetEffectiveTopOfRow(oRow);
    oInfo := TWPMouseInfo.Create;
    Try
      Result := GetMouseInfoForPoint(iX, iY-1, oInfo) And (oInfo.Offset <> iCurrent);
      iNew := oInfo.Offset;
    Finally
      oInfo.Free;
    End;
  End;
End;


Function TWPScreenRenderer.LineDown(Const iCurrent : Integer; Var iNew : Integer) : Boolean;
Var
  iX, iY, iHeight : Integer;
  oRegion : TWPMapItem;
  oRow : TWPMapRow;
  aColour : TColour;
  oInfo : TWPMouseInfo;
Begin
  Result := GetPointForOffset(iCurrent, iX, iY, iHeight, aColour, oRegion) And
            GetRowForRegion(oRegion, oRow);
  If Result Then
  Begin
    iY := GetEffectiveBottomOfRow(oRow);
    oInfo := TWPMouseInfo.Create;
    Try
      Result := GetMouseInfoForPoint(iX, iY+1, oInfo) And (oInfo.Offset <> iCurrent);
      iNew := oInfo.Offset;
    Finally
      oInfo.Free;
    End;
  End;
End;


Function TWPScreenRenderer.PageDown(Const iCurrent, iPageHeight : Integer; Var iNew : Integer) : Boolean;
Var
  iX, iY, iHeight : Integer;
  oRegion : TWPMapItem;
  oRow : TWPMapRow;
  aColour : TColour;
  oInfo : TWPMouseInfo;
Begin
  Result := GetPointForOffset(iCurrent, iX, iY, iHeight, aColour, oRegion) And
            GetRowForRegion(oRegion, oRow);
  If Result Then
  Begin
    iY := GetEffectiveBottomOfRow(oRow);
    oInfo := TWPMouseInfo.Create;
    Try
      Result := GetMouseInfoForPoint(iX, iY+iPageHeight+1, oInfo) And (oInfo.Offset <> iCurrent);
      iNew := oInfo.Offset;
    Finally
      oInfo.Free;
    End;
  End;
End;

Function TWPScreenRenderer.LineEnd(Const iCurrent: Integer; Out iNew: Integer): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Document.GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Result := (oPiece.Maps.Count = 1) And (oPiece.Maps[0] Is TWPMapItem) And oPiece.Maps[0].HasParent And (oPiece.Maps[0].Parent Is TWPMapRow);
  If Result Then
    Begin
    If TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 1].Piece Is TWPWorkingDocumentParaPiece Then
      If TWPMapRow(oPiece.Maps[0].Parent).Items.Count > 1 Then
        iNew := TWPWorkingDocumentPiece(TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 2].Piece).Metrics.Position+TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 2].OffsetStart + TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 2].OffsetLength
      Else
        Result := False
    Else
      iNew := TWPWorkingDocumentPiece(TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 1].Piece).Metrics.Position+TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 1].OffsetStart + TWPMapRow(oPiece.Maps[0].Parent).Items[TWPMapRow(oPiece.Maps[0].Parent).Items.count - 1].OffsetLength - 1;
    Result := Result And (iNew <> iCurrent);
    If Result Then
      Begin
      iNew := Document.CheckCursorPoint(iNew, cdLeft, Settings.NoSelectReadOnly);
      Result := (iNew <> iCurrent) And (iNew <> -1);
      End;
    End;
End;

Function TWPScreenRenderer.LineHome(Const iCurrent: Integer; Out iNew: Integer): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Document.GetPieceByPosition(iCurrent, oPiece, iInternal, iIndex);
  If Result Then
    Result := (oPiece.Maps.Count > 0) And (oPiece.Maps[0] Is TWPMapItem) And oPiece.Maps[0].HasParent And (oPiece.Maps[0].Parent Is TWPMapRow);
  If Result Then
    Begin
    iNew := Document.CheckCursorPoint(TWPWorkingDocumentPiece(TWPMapRow(oPiece.Maps[0].Parent).Items[0].Piece).Metrics.Position+TWPMapRow(oPiece.Maps[0].Parent).Items[0].OffsetStart, cdRight, Settings.NoSelectReadOnly);
    Result := (iNew <> iCurrent) And (iNew <> -1);
    End;
End;


Procedure TWPScreenRenderer.SaveImage(Const sFilename : String);
Begin
  Canvas.saveImage(sFilename);
End;


Procedure TWPScreenRenderer.SavePNG(Const sFilename : String);
Begin
  Canvas.savePNG(sFilename);
End;


Procedure TWPScreenRenderer.PaintCanvas(oSender : TObject);
Begin
  Paint(True);
End;


Function TWPScreenRenderer.GetByLineCol(oRow : TWPMapRow; iLine, iCol : Integer; Out iPosition : Integer) : Boolean;
Var
  iLoop : Integer;
  iOffset : Integer;
  oItem : TWPMapItem;
Begin
  Result := False;
  If oRow.Line = iLine Then
  Begin
    iLoop := 0;
    iOffset := oRow.ColumnOffset;
    While Not Result And (iLoop < oRow.Items.Count) Do
    Begin
      oItem := oRow.Items[iLoop];
      If iCol < oItem.OffsetLength + iOffset Then
      Begin
        Result := True;
        iPosition := (iCol - iOffset) + (TWPWorkingDocumentPiece(oItem.Piece).Metrics.Position - oItem.OffsetStart);
      End
      Else
        iOffset := iOffset + oItem.OffsetLength;
      Inc(iLoop);
    End;
  End;
End;

Function TWPScreenRenderer.GetByLineCol(oRows : TWPMapRows; iLine, iCol : Integer; Out iPosition : Integer) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  iLoop := 0;
  While Not Result And (iLoop < oRows.Count) Do
  Begin
    Result := GetByLineCol(oRows[iLoop], iLine, iCol, iPosition);
    Inc(iLoop);
  End;
End;


Function TWPScreenRenderer.GetByLineCol(oContainer : TWPMapContainer; iLine, iCol : Integer; Out iPosition : Integer) : Boolean;
Begin
  Result := GetByLineCol(oContainer.Rows, iLine, iCol, iPosition);
  If Not Result Then
    Result := GetByLineCol(oContainer.Children, iLine, iCol, iPosition);
End;


Function TWPScreenRenderer.GetByLineCol(oContainers : TWPMapContainers; iLine, iCol : Integer; Out iPosition : Integer) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  iLoop := 0;
  While Not Result And (iLoop < oContainers.Count) Do
  Begin
    Result := GetByLineCol(oContainers[iLoop], iLine, iCol, iPosition);
    Inc(iLoop);
  End;
End;


Function TWPScreenRenderer.GetByLineCol(iLine, iCol : Integer; Out iPosition : Integer) : Boolean;
Begin
  Result := GetByLineCol(Map.Children, iLine, iCol, iPosition);
End;


Function TWPScreenRenderer.GetLineCol(iPosition : Integer; Out iLine, iCol : Integer): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iOffset : Integer;
  iIndex : Integer;
  oMap : TWPMapItem;
  oOther : TWPMapItem;
  oRow : TWPMapRow;
  iLoop : Integer;
Begin
  Result := Document.GetPieceByPosition(iPosition, oPiece, iOffset, iIndex);
  If Result Then
  Begin
    oMap := oPiece.Maps.GetByOffset(iOffset);
    Result := Assigned(oMap);
    If Result Then
    Begin
      oRow := TWPMapRow(oMap.Parent);
      iLine := oRow.Line;
      iCol := iOffset - oMap.OffsetStart + oRow.ColumnOffset;
      iLoop := 0;
      oOther := oRow.Items[0];
      While (oOther <> oMap) And (iLoop < oRow.Items.Count) Do
      Begin
        Inc(iCol, oOther.OffsetLength);
        Inc(iLoop);
        oOther := oRow.Items[iLoop];
      End;
    End;
  End;
End;


Procedure TWPScreenRenderer.ChangeSettings;
Begin
  Canvas.HorizontalMargin := Settings.HorizontalMargin;
  Canvas.VerticalMargin := Settings.VerticalMargin;
  Canvas.Background := Settings.Background;
  If Settings.LowLight Then
    Canvas.Contrast := DEFAULT_LOW_LIGHT_CONTRAST
  Else
    Canvas.Contrast := DEFAULT_NORMAL_CONTRAST;
  Inherited;
End;


Procedure TWPScreenRenderer.Yield;
Begin
  PaintAll := True;
  Canvas.Yield;
End;



Function TWPScreenRenderer.GetOperator : TWPOperator;
Begin
  Assert(Invariants('GetOperator', FOperator, TWPOperator, 'Operator'));
  Result := FOperator;
End;


Function TWPScreenRenderer.GetScreenRectForOffset(Const iOffset : Integer) : TRect;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  oMap : TWPMapItem;
Begin
  ZeroMemory(@Result, SizeOf(TRect));
  If Document.GetPieceByPosition(iOffset, oPiece, iInternal, iIndex) Then
  Begin
    oMap := oPiece.Maps.GetByOffset(iInternal);
    If Assigned(oMap) Then
    Begin
      Result.Left := oMap.Left;
      Result.Right := oMap.Right;
      Result.Top := oMap.Top;
      Result.Bottom := oMap.Bottom;
    End;
  End;
End;


Function TWPScreenRenderer.IsInHotSpot(oItem : TWPMapItem; oPiece : TWPWorkingDocumentImagePiece; iX, iY : Integer; oInfo : TWPMouseInfo) : Boolean;
Var
  iLoop : Integer;
  oArea : TWPImageMapArea;
  oMapItem : TWPMapItem;
Begin
  iX := Round((iX - oItem.Left) / oPiece.FactorX);
  iY := Round((iY - oItem.Top) / oPiece.FactorY);

  If oPiece.HasImageMap Then
  Begin
    // firstly, we have to correct x and y for the scaling on the image.

    // then we have to find out whether x and y are in a map
    iLoop := 0;
    While (iLoop < oPiece.ImageMap.Areas.Count) And (oInfo.Hotspot = Nil) Do
    Begin
      oArea := oPiece.ImageMap.Areas[iLoop];
      If oArea.Contains(iX, iY) Then
        oInfo.Hotspot := oArea.Link;
      Inc(iLoop);
    End;
    Result := oInfo.Hotspot <> Nil;
  End
  Else If oPiece.Maps.Count > 0 Then
  Begin
    iLoop := 0;
    While (iLoop < oPiece.Maps.Count) And (oInfo.Hotspot = Nil) Do
    Begin
      oMapItem := oPiece.Maps[iLoop];
      If Assigned(oMapItem.ForeHotSpot) Then
        oInfo.Hotspot := oMapItem.ForeHotspot.Link
      Else If Assigned(oMapItem.BackHotspot) Then
        oInfo.Hotspot := oMapItem.BackHotspot.Link;
      Inc(iLoop);
    End;
    Result := oInfo.Hotspot <> Nil;
  End
  Else
  Begin
    Result := False
  End;
End;


Function TWPScreenRenderer.asRect: TRect;
Begin
  Result := Map.asRect;
End;

Procedure TWPScreenRenderer.Scrub;
Begin
  Canvas.Scrub;
End;

Function TWPScreenRenderer.HasPaginations: Boolean;
Begin
  Result := Length(FPageOffsets) > 0;
End;

Procedure TWPScreenRenderer.ClearPaginations;
Begin
  SetLength(FPageOffsets, 0);
  Valid := False;
End;

Procedure TWPScreenRenderer.SetPaginations(Const aPages: TWPPaginations);
Var
  iLoop : Integer;
Begin
  SetLength(FPageOffsets, Length(aPages));
  For iLoop := Low(aPages) To High(aPages) Do
    FPageOffsets[iLoop] := aPages[iLoop];
  Valid := False;
  DoUpdate;
End;


Procedure TWPScreenRenderer.CheckPageOffsets;
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
  oMap : TWPMapObject;
  iOffset : Integer;
  iIndex : Integer;
  iInner : Integer;
  iTarget : Integer;
Begin
  If (Length(FPageOffsets) > 0) Then
  Begin
    SetLength(FPagePositions, Length(FPageOffsets));
    For iLoop := Low(FPageOffsets) To High(FPageOffsets) Do
    Begin
      If FPageOffsets[iLoop] > Document.CharCount Then
        oPiece := Document.Pieces.Last
      Else If Not Document.GetPieceByPosition(FPageOffsets[iLoop], oPiece, iOffset, iIndex) Then
        // RaiseError('CheckPageOffsets', 'Unable to find page offset (content) '+IntegerToString(FPageOffsets[iLoop]));
        oPiece := Nil;
      If (oPiece <> Nil) Then
      Begin
        oMap := Nil;
        iTarget := FPageOffsets[iLoop];
        While (oMap = Nil) And (oPiece <> Nil) Do
        Begin
          For iInner := 0 To oPiece.Maps.Count - 1 Do
            If oPiece.Maps[iInner].OffsetStart+oPiece.Maps[iInner].OffsetLength > iTarget Then
              oMap := oPiece.Maps[iInner];
          oPiece := oPiece.Prev;
          iTarget := 0;
        End;
        If (oMap = Nil) Then
          FPagePositions[iLoop] := Height
        Else If Not (oMap Is TWPMapObject) Or (TWPMapRow(oMap.Parent).Items.Last = oMap) Then
          FPagePositions[iLoop] := oMap.Bottom
        Else
          FPagePositions[iLoop] := oMap.Top + oMap.Height Div 2;
      End;
    End;
  End;
End;

Procedure TWPScreenRenderer.PaintPageMarkers;
Var
  iLast: Integer;
  iLoop : Integer;
Begin
  Canvas.Font.Clear;
  Canvas.Font.Colour := clGray;
  Canvas.Font.Name := 'Microsoft Sans Serif';
  Canvas.Font.Size := 6;

  If Settings.HorizontalMargin > MIN_PAGINATION_WIDTH Then
  Begin
    iLast := Settings.VerticalMargin;
    For iLoop := Low(FPagePositions) To High(FPagePositions) Do
    Begin
      If (iLoop = High(FPagePositions)) Then
        PaintPageMarker(iLoop + 1, iLast, Map.Bottom-Settings.VerticalMargin)
      Else
        PaintPageMarker(iLoop + 1, iLast, FPagePositions[iLoop]);
      iLast := FPagePositions[iLoop];
    End;
  End;
End;


Procedure TWPScreenRenderer.PaintPageMarker(iPageNo, iStart, iEnd : Integer);
Var
 s : String;
Begin
  If iEnd - iStart > 4 Then
  Begin
    Canvas.DrawLine(1, RandomColour(clBlack, False), apsSolid, apesSquare, 3, iStart + 1, 8, iStart + 1);
    Canvas.DrawLine(1, RandomColour(clBlack, False), apsSolid, apesSquare, 3, iEnd - 3, 8, iEnd - 3);
    Canvas.DrawLine(1, RandomColour(clSilver, False), apsSolid, apesSquare, 0, iEnd - 2, 6, iEnd - 2);
    Canvas.DrawLine(1, RandomColour(clBlack, False), apsSolid, apesSquare, 2, iStart + 1, 2, iEnd - 2);
    Canvas.DrawLine(2, RandomColour(clSilver, False), apsSolid, apesSquare, 1, iStart + 2, 1, iEnd - 2);
    s := IntegerToString(iPageNo);
    If (Canvas.TextHeight(s) < iEnd - iStart - 6) And
       (Canvas.TextWidth(s) < Settings.HorizontalMargin - 8) Then
      Canvas.DrawText(clTransparent, 6, iStart + 4, s);
  End;
End;

Function TWPScreenRenderer.GetPageForOffset(iOffset: Integer): Integer;
Var
  iLoop : Integer;
Begin
  Result := -1;
  If (iOffset >= 0) And (iOffset < Document.CharCount) Then
  Begin
    iLoop := Low(FPageOffsets);
    While (Result = -1) And (iLoop <= High(FPageOffsets)) Do
    Begin
      If iOffset < FPageOffsets[iLoop] Then
        Result := iLoop + 1;
      Inc(iLoop);
    End;
  End;
End;

Function TWPScreenRenderer.PageCount: Integer;
Begin
  Result := Length(FPageOffsets);
End;

procedure TWPScreenRenderer.PaintAnnotations(bAll : Boolean);
var
  iLoop : Integer;
  iTop : Integer;
  iHeight : Integer;
  oAnnotation : TWPWorkingAnnotation;
  bDraw : Boolean;
  bMargin : Boolean;
  oAnnotations : TWPWorkingAnnotationList;
begin
  if SpaceForAnnotations = 0 Then
    Exit;

  oAnnotations := Document.DrawnAnnotations;
  iTop := 0;
  bDraw := True;

  if oAnnotations.Count > 0 Then
  Begin
    For iLoop := 0 to oAnnotations.Count - 1 Do
    Begin
      oAnnotation := oAnnotations[iLoop];
      If bAll Or Not oAnnotation.Measured Then
        MeasureAnnotation(oAnnotation);
    End;

    iTop := ANNOTATION_TOP_GAP;
    For iLoop := 0 to oAnnotations.Count - 1 Do
    Begin
      oAnnotation := oAnnotations[iLoop];
      iHeight := ApplyAnnotationFont(oAnnotation.DefinitionProvider);
      If bAll Or Not oAnnotation.Aligned Or (oAnnotation.Top <> iTop) Then
        AlignAnnotation(oAnnotation, iTop, iHeight);
      iTop := oAnnotation.Bottom + ANNOTATION_GAP;
    End;

    bMargin := False;
    For iLoop := 0 to oAnnotations.Count - 1 Do
      bMargin := bMargin or Not oAnnotations[iLoop].Drawn;

    if bMargin Then
      Canvas.DrawRect(Settings.Background, Map.Right - Settings.HorizontalMargin, 0, Map.Right+1, Map.Bottom);

    iTop := 0;
    For iLoop := 0 to oAnnotations.Count - 1 Do
    Begin
      bDraw := False;
      oAnnotation := oAnnotations[iLoop];
      If bAll Or Not oAnnotation.Drawn Then
      Begin
        bDraw := True;
        DrawAnnotation(oAnnotation, iTop);
      End;
      iTop := oAnnotation.Bottom;
    End;
  End;

  if bDraw Then
  Begin
    Canvas.DrawRect(ColourMultiply(Settings.Background, 0.95), Map.Right+1, iTop, Map.Right + SpaceForAnnotations, Map.Bottom);
    Canvas.DrawLine(1, ColourMultiply(Settings.Background, 0.90), apsSolid, apesSquare,  Map.Right+1, iTop, Map.Right+1, Map.Bottom);
  End;

  Canvas.DrawLine(1, ColourMultiply(Settings.Background, 0.90), apsSolid, apesSquare,  Map.Right+1, 0, Map.Right + SpaceForAnnotations, 0);
  Canvas.DrawLine(1, ColourMultiply(Settings.Background, 0.90), apsSolid, apesSquare,  Map.Right+1, Map.Bottom, Map.Right + SpaceForAnnotations, Map.Bottom);
end;

Function FindNextLineBreak(Const sText : String; Const aOffsets : TWPIntegers; Const iCursor, iWidth : Integer) : Integer;
Var
  iLimit : Integer;
  iRight : Integer;
Begin
  iLimit := iCursor;
  if iCursor = 0 Then
    iRight := iWidth
  Else
    iRight := aOffsets[iCursor-1] + iWidth;
  While (iLimit < Length(sText)) And (aOffsets[iLimit] < iRight) Do
    inc(iLimit);

  Result := iLimit;
  if (iLimit < Length(sText)) Then
  Begin
    While (Result > iCursor) And (Result > 1) And Not CharInSet(sText[Result-1], CHAR_WORD_BREAK) Do
      dec(Result);
    if (result = iCursor) Or (Result <= 2) Then
      Result := iLimit;
  End;
  If Result = iCursor Then
    result := iCursor + 1;
End;


Procedure TWPScreenRenderer.MeasureAnnotation(oAnnotation : TWPWorkingAnnotation);
Var
  aSize : TSize;
  aOffsets : TWPIntegers;
  iCursor : Integer;
  aLines : TLineBreaks;
Begin
  ApplyAnnotationFont(oAnnotation.DefinitionProvider);
  aSize := Canvas.TextExtent(oAnnotation.WorkingText);
  ZeroMemory(@aOffsets, Sizeof(aOffsets));
  Canvas.GetTextExtents(oAnnotation.WorkingText, aSize, @aOffsets);

  SetLength(aLines, 0);
  iCursor := 0;
  while iCursor < Length(oAnnotation.WorkingText) Do
  Begin
    iCursor := FindNextLineBreak(oAnnotation.WorkingText, aOffsets, iCursor, SpaceForAnnotations - ANNOTATION_HORIZ_DEADSPACE);
    if (iCursor < Length(oAnnotation.WorkingText)) Then
    Begin
      SetLength(aLines, Length(aLines)+1);
      aLines[Length(aLines)-1] := iCursor;
    End;
  End;

  oAnnotation.LineBreaks := aLines;
  oAnnotation.Measured := True;
End;


procedure TWPScreenRenderer.AlignAnnotation(oAnnotation: TWPWorkingAnnotation; iTop, iHeight: Integer);
begin
  oAnnotation.Top := IntegerMax(oAnnotation.Anchor, iTop);
  oAnnotation.Bottom := oAnnotation.Top + (iHeight * (Length(oAnnotation.LineBreaks)+1)) +
    ANNOTATION_TOP_LINE_WIDTH + ANNOTATION_TOP_PADDING +
    ANNOTATION_BOTTOM_LINE_WIDTH + ANNOTATION_BOTTOM_PADDING;
  oAnnotation.Aligned := True;
end;

Procedure TWPScreenRenderer.DrawAnnotation(oAnnotation: TWPWorkingAnnotation; iTop: Integer);
Var
  iLoop : Integer;
  iLine : Integer;
  iLast : Integer;
  iW : Integer;
  iHeight : Integer;
begin
  Canvas.DrawRect(Canvas.ColourDarken(Settings.Background, 0.95),
     Map.Right+1, iTop, Map.Right + SpaceForAnnotations, oAnnotation.Bottom);
  Canvas.DrawLine(1, Canvas.ColourDarken(Settings.Background, 0.90), apsDot, apesSquare,
     Map.Right+1, iTop, Map.Right+1, oAnnotation.Bottom);
  if oAnnotation.Selected Then
    Canvas.DrawRect(Settings.Background, Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Top,
                  Map.Right + SpaceForAnnotations - ANNOTATION_RIGHT_GAP, oAnnotation.Bottom)
  Else
    Canvas.DrawRect(Canvas.ColourLighten(oAnnotation.Colour, 0.80), Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Top,
                  Map.Right + SpaceForAnnotations - ANNOTATION_RIGHT_GAP, oAnnotation.Bottom);

  iHeight := ApplyAnnotationFont(oAnnotation.DefinitionProvider);

  iLine := oAnnotation.Top + ANNOTATION_TOP_LINE_WIDTH + ANNOTATION_TOP_PADDING;
  if Length(oAnnotation.LineBreaks) = 0 Then
    Canvas.DrawText(clTransparent, Map.Right+ANNOTATION_LEFT_DEADSPACE, iLine, oAnnotation.WorkingText)
  Else
  Begin
    iLast := 1;
    For iLoop := 0 to Length(oAnnotation.LineBreaks) - 1 Do
    Begin
      Canvas.DrawText(clTransparent, Map.Right+ANNOTATION_LEFT_DEADSPACE, iLine, StringSlice(oAnnotation.WorkingText, iLast, oAnnotation.LineBreaks[iLoop]-1));
      iLast := oAnnotation.LineBreaks[iLoop];
      Inc(iLine, iHeight);
    End;
    Canvas.DrawText(clTransparent, Map.Right+ANNOTATION_LEFT_DEADSPACE, iLine, StringSlice(oAnnotation.WorkingText, iLast, Length(oAnnotation.WorkingText)));
  End;

  if oAnnotation.Selected Then
    iW := 2
  Else
    iW := 1;
  Canvas.DrawLine(iW, oAnnotation.Colour, apsDot, apesSquare,
    Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Top,       Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Bottom-1);
  Canvas.DrawLine(iW, oAnnotation.Colour, apsDot, apesSquare,
    Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Bottom-1,  Map.Right + SpaceForAnnotations - (ANNOTATION_RIGHT_GAP), oAnnotation.Bottom-1);
  Canvas.DrawLine(iW, oAnnotation.Colour, apsDot, apesSquare,
    Map.Right + SpaceForAnnotations - (ANNOTATION_RIGHT_GAP), oAnnotation.Bottom-1, Map.Right + SpaceForAnnotations - (ANNOTATION_RIGHT_GAP), oAnnotation.Top);
  Canvas.DrawLine(iW, oAnnotation.Colour, apsDot, apesSquare,
    Map.Right + SpaceForAnnotations - (ANNOTATION_RIGHT_GAP), oAnnotation.Top, Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Top);
  Canvas.DrawLine(1, oAnnotation.Colour, apsDot, apesSquare, Map.Right+ANNOTATION_LEFT_GAP, oAnnotation.Top, Map.Right+ANNOTATION_LEFT_GAP-ANNOTATION_LEFT_STUB, oAnnotation.Top);
  Canvas.DrawLine(1, oAnnotation.Colour, apsDot, apesSquare, Map.Right, oAnnotation.Anchor, Map.Right+ANNOTATION_LEFT_GAP-ANNOTATION_LEFT_STUB, oAnnotation.Top);
  Canvas.DrawLine(1, oAnnotation.Colour, apsDot, apesSquare, Map.Right, oAnnotation.Anchor, Map.InnerRight, oAnnotation.Anchor);
End;


Function TWPScreenRenderer.ApplyAnnotationFont(oDefinitionProvider : TWPAnnotationDefinitionProvider) : Integer;
Begin
  Canvas.Font.Clear;
  Canvas.Font.Colour := clBlack;
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Trunc(oDefinitionProvider.FontSize * Settings.Scale);
  Result := Canvas.TextHeight('pP');
End;

procedure TWPScreenRenderer.BuildMetrics;
begin
  inherited;
end;

Function TWPScreenRenderer.WantFastDrawing : Boolean;
begin
  result := (FOperator.LastChange > 0) and (FOperator.LastChange > universalDateTime - (FASTDRAW_LAG * DATETIME_SECOND_ONE));
end;


//-- Debugging -----------------------------------------------------------------

Const
  GbDebugColour : Boolean = False;

Function RandomColour(aColour : TColour; bForce : Boolean = False) : TColour; Overload;
Begin
  If GbDebugColour Or bForce Then
    Case RandomInteger(16) Of
      0:Result := clAqua;
      1:Result := clBlack;
      2:Result := clBlue;
      3:Result := clDkGray;
      4:Result := clFuchsia;
      5:Result := clGray;
      6:Result := clGreen;
      7:Result := clLime;
      8:Result := clLtGray;
      9:Result := clMaroon;
      11:Result := clNavy;
      12:Result := clOlive;
      13:Result := clPurple;
      14:Result := clRed;
      15:Result := clSilver;
    Else
      Result := clYellow;
    End
  Else
    Result := aColour;
End;


//-- Administration ------------------------------------------------------------

function TWPScreenRenderer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOperator.sizeInBytes);
end;

Constructor TWPScreenCanvas.Create;
Begin
  Inherited;
  FContrast := 1;
  Initialise(0,0);
End;

Destructor TWPScreenCanvas.Destroy;
Begin
  FreeImage;

  Inherited;
End;


//-- Image Management ----------------------------------------------------------

Procedure TWPScreenCanvas.Initialise(iWidth, iHeight : Integer);
Begin
  FStatedWidth := iWidth;
  FStatedHeight := iHeight;

  FreeImage;

  FInternalTop := 0;
  FInternalBottom := INTERNAL_HEIGHT;
End;


Procedure TWPScreenCanvas.FreeImage;
Begin
  If Assigned(FImage) Then
  Begin
    {$IFNDEF NO_VCL_CANVAS_LOCK}
    FImage.Canvas.Unlock;
    {$ENDIF}

    FImage.Free;
    FImage := Nil;
  End;
End;

Procedure TWPScreenCanvas.NeedImage;
Begin
  If Not Assigned(FImage) Then
  Begin
    Try
      FImage := TBitmap.Create;
      {$IFNDEF NO_VCL_CANVAS_LOCK}
      FImage.Canvas.Lock;
      {$ENDIF}
      FImage.HandleType := bmDIB;
      FImage.Width := FStatedWidth;
      FImage.Height := INTERNAL_HEIGHT;

      DrawRect(Background, 0, 0, FStatedWidth, INTERNAL_HEIGHT);
    Except
      On E : EOutOfResources Do
        RaiseError('NeedImage', StringFormat('Unable to allocate image buffer for word processor (%s)', [E.Message]))
      Else
        Raise;
    End;
  End;
End;


Function TWPScreenCanvas.SetImageSize(iWidth, iHeight : Integer) : Boolean;
Begin
  Result := False;
  If iWidth <> FStatedWidth Then
  Begin
    Initialise(iWidth, iHeight);
    Result := True;
  End
  Else If iHeight <> FStatedHeight Then
  Begin
    FStatedHeight := iHeight;
    ClearTail;
  End;
End;


Function TWPScreenCanvas.PaintControl(aHandle : THandle; iHeight, iWidth, iOffset : Integer) : Integer;
Begin
  NeedImage;

  If (iOffset < FInternalTop) Or (iOffset + iHeight > FInternalBottom) Then
  Begin
    Assert(CheckCondition(iHeight < INTERNAL_HEIGHT, 'PaintControl', StringFormat('Height Metrics Error: Visible %d is greater than internal %d', [iHeight, INTERNAL_HEIGHT])));
    Assert(CheckCondition(Assigned(FOnUpdateImage), 'PaintControl', 'Internal Paint event not assigned'));

    FInternalTop := IntegerMax(IntegerMin(IntegerMax(iOffset - INTERNAL_HEIGHT Div 2, iOffset + iHeight - INTERNAL_HEIGHT), FStatedHeight - INTERNAL_HEIGHT), 0);
    FInternalBottom := FInternalTop + INTERNAL_HEIGHT;
    DrawRect(Background, 0, 0, FStatedWidth, INTERNAL_HEIGHT);
    FOnUpdateImage(Self);
  End;

  // now draw the screen
  Result := IntegerMin(Width, iWidth);
  BitBlt(aHandle, 0, 0, Result, IntegerMin(iHeight, Height), FImage.Canvas.Handle, 0, iOffset - FInternalTop, SRCCOPY);
End;


Procedure TWPScreenCanvas.MoveVertical(iTop, iOffset : Integer);
Begin
  NeedImage;
  FInternalBottom := IntegerMin(FInternalBottom + iOffset, FInternalTop + FImage.Height);

  BitBlt(FImage.Canvas.Handle, 0, iTop + iOffset - FInternalTop, FImage.Width, FImage.Height,
         FImage.Canvas.Handle, 0, iTop - FInternalTop, SRCCOPY);
End;


Procedure TWPScreenCanvas.SaveImage(Const sFilename : String);
Begin
  NeedImage;
  FImage.SaveToFile(sFilename);
End;


Procedure TWPScreenCanvas.SavePNG(Const sFilename : String);
Var
  oPNG : TPngObject;
  oFile : TFileStream;
Begin
  NeedImage;

  oPNG := TPngObject.Create;
  Try
    oPNG.Assign(FImage);

    oFile := TFileStream.Create(sFilename, fmCreate);
    Try
      oPNG.SaveToStream(oFile);
    Finally
      oFile.Free;
    End;
  Finally
    oPng.Free;
  End;
End;


Function TWPScreenCanvas.Visible(iTop, iBottom : Integer) : Boolean;
Begin
  Result := Not ((iTop > FInternalBottom) Or (iBottom < FInternalTop));
End;



//-- Metrics -------------------------------------------------------------------

Function TWPScreenCanvas.TextExtent(Const sText : String) : TSize;
Begin
  NeedImage;
  ApplyFont;
  Result := FImage.Canvas.TextExtent(sText);
End;


Function TWPScreenCanvas.GetTextMetrics : TTextMetric;
Begin
  NeedImage;
  ApplyFont;
  Windows.GetTextMetrics(FImage.Canvas.Handle, Result);
End;


Function TWPScreenCanvas.TextHeight(Const sText : String) : Integer;
Begin
  NeedImage;
  ApplyFont;
  Result := FImage.Canvas.TextHeight(sText);
End;


Function TWPScreenCanvas.TextWidth(Const sText : String) : Integer;
Begin
  NeedImage;
  ApplyFont;
  Result := FImage.Canvas.TextWidth(sText);
End;


Procedure TWPScreenCanvas.GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers);
Var
  iMax : Integer;
Begin
  NeedImage;
  ApplyFont;
  Win32Check(GetTextExtentExPoint(FImage.Canvas.Handle, pchar(sText), Length(sText), 30000, @iMax, @Offsets[0], aSize));
End;


Function TWPScreenCanvas.DPIX : Integer;
Begin
  NeedImage;
  Result := GetDeviceCaps(FImage.Canvas.Handle, LOGPIXELSX);
End;


Function TWPScreenCanvas.DPIY : Integer;
Begin
  NeedImage;
  Result := GetDeviceCaps(FImage.Canvas.Handle, LOGPIXELSX);
End;


//-- Drawing -------------------------------------------------------------------

Function TWPScreenCanvas.ApplyContrast(aColour : TColour) : TColour;
Begin
  If FContrast = 1 Then
    Result := RandomColour(aColour)
  Else
    Result := RandomColour(ColourDarken(aColour, FContrast));
End;


Procedure TWPScreenCanvas.DrawRect(aColour: TColour; iLeft, iTop, iRight, iBottom: Integer);
Begin
  NeedImage;
  If (iRight > iLeft) And (iBottom > iTop) Then
  Begin
    aColour := ApplyContrast(aColour);
    FImage.Canvas.Brush.Color := aColour;
    FImage.Canvas.Brush.Style := bsSolid;
    FImage.Canvas.Pen.Width := 1;
    FImage.Canvas.Pen.Color := aColour;
    FImage.Canvas.Pen.Style := psSolid;
    FImage.Canvas.Pen.Mode := pmCopy;
    FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesFlat, apjsMitre);
    FImage.Canvas.Rectangle(iLeft, iTop - FInternalTop, iRight, iBottom - FInternalTop);
    SetBKMode(FImage.Canvas.Handle, clTransparent);
  End;
End;


Procedure TWPScreenCanvas.DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer);
Begin
  NeedImage;
  If (iRight > iLeft) And (iBottom > iTop) Then
  Begin
    FImage.Canvas.Brush.Bitmap := oBitmap.Handle;
    FImage.Canvas.Pen.Width := 0;
    FImage.Canvas.Pen.Style := psClear;
    FImage.Canvas.Pen.Mode := pmCopy;
    FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesFlat, apjsMitre);
    FImage.Canvas.Rectangle(iLeft, iTop - FInternalTop, iRight+1, iBottom - FInternalTop+1);
    SetBKMode(FImage.Canvas.Handle, clTransparent);
  End;
End;


Procedure TWPScreenCanvas.DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer);
Begin
  NeedImage;
  If (iX1 <> iX2) Or (iY1 <> iY2) Then
  Begin
    aColour := ApplyContrast(aColour);
    FImage.Canvas.Pen.Width := iWidth;
    FImage.Canvas.Pen.Color := aColour;
    FImage.Canvas.Pen.Style := ADVPENSTYLE_VCLVALUES[aPenStyle];
    FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesFlat, apjsMitre);
    FImage.Canvas.MoveTo(iX1, iY1 - FInternalTop);
    FImage.Canvas.LineTo(iX2, iY2 - FInternalTop);
  End;
End;


Procedure TWPScreenCanvas.DrawText(aBackground : TColour; iLeft, iTop: Integer; Const sText: String);
Begin
  NeedImage;
  If sText <> '' Then
  Begin
    Font.Colour := ApplyContrast(Font.Colour);
    ApplyFont;
    If aBackground = clTransparent Then
      SetBkMode(FImage.Canvas.Handle, clTransparent)
    Else
    Begin
      SetBkMode(FImage.Canvas.Handle, OPAQUE);
      SetBkColor(FImage.Canvas.Handle, ApplyContrast(aBackground));
    End;

    FImage.Canvas.TextOut(iLeft, iTop - FInternalTop, sText);
  End;
End;


Procedure TWPScreenCanvas.DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer);
Var
  iLoop : Integer;
Begin
  NeedImage;
  aColour := ApplyContrast(aColour);
  For iLoop := iX1 To iX2 Do
  Begin
    Case iLoop Mod 4 Of
      0:
        Begin
        FImage.Canvas.Pixels[iLoop, iY-1 - FInternalTop] := aColour;
        FImage.Canvas.Pixels[iLoop, iY-1 - FInternalTop-1] := aColour;
        End;
      2:
        Begin
        FImage.Canvas.Pixels[iLoop, iY+1 - FInternalTop] := aColour;
        FImage.Canvas.Pixels[iLoop, iY+1 - FInternalTop-1] := aColour;
        End;
    Else
      Begin
      FImage.Canvas.Pixels[iLoop, iY - FInternalTop] := aColour;
      FImage.Canvas.Pixels[iLoop, iY - FInternalTop-1] := aColour;
      End;
    End;
  End;
End;

    (*
{$R-}
{Courtesy of Joe C. Hecht, http://www.efg2.com/Lab/Library/UseNet/1999/0114c.txt}
Procedure TWPScreenCanvas.StretchDrawIcon(aRect : TRect; oImage : TIcon);
var
  IconInfo : TIconInfo;
  bmMaskInfo : Windows.TBitmap;
  bmColorInfo : Windows.TBitmap;
  dc : hDc;
  CanvasHandle : HDC;
  OldBrushColor : TColor;
  OldPenColor : TColor;
  OldBkColor : TColor;
  oldbm : HBitmap;
  bmMaskInfobmHeightdiv2 : integer;
begin
  FillChar(bmMaskInfo, sizeof(bmMaskInfo), #0);
  FillChar(bmColorInfo, sizeof(bmColorInfo), #0);
  GetIconInfo(oImage.Handle, IconInfo);
//  if (IconInfo.FIcon) then
//  Begin
    if (IconInfo.hbmMask <> 0) then
      GetObject(IconInfo.hbmMask, sizeof(bmMaskInfo), @bmMaskInfo);
    if (IconInfo.hbmColor <> 0) then
      GetObject(IconInfo.hbmColor, sizeof(bmColorInfo), @bmColorInfo);
    if (IconInfo.hbmMask <> 0) then
    Begin
      dc := CreateCompatibleDc(FImage.Canvas.Handle);
      OldBrushColor := FImage.Canvas.Brush.Color;
      FImage.Canvas.Brush.Color := clWhite;
      OldPenColor := FImage.Canvas.Pen.Color;
      FImage.Canvas.Pen.Color := clBlack;
      OldBkColor := SetBkColor(CanvasHandle, RGB(255, 255, 255));
      if ((bmMaskInfo.bmBitsPixel * bmMaskInfo.bmPlanes = 1) AND (IconInfo.hbmColor = 0)) then
      begin
        bmMaskInfobmHeightdiv2 := bmMaskInfo.bmHeight div 2;
        oldbm := SelectObject(dc, IconInfo.hbmMask);
        StretchBlt(CanvasHandle, aRect.Left, aRect.Top, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top, dc, 0, 0,
               bmMaskInfo.bmWidth, bmMaskInfobmHeightdiv2, SRCAND);
        StretchBlt(CanvasHandle, aRect.Left, aRect.Top, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top, dc, 0, bmMaskInfobmHeightdiv2,
               bmMaskInfo.bmWidth, bmMaskInfobmHeightdiv2, SRCINVERT);
        SelectObject(dc, oldbm);
        DeleteObject(IconInfo.hbmMask);
      end
      else
      begin
        oldbm := SelectObject(dc, IconInfo.hbmMask);
        MaskBlt(FImage.Canvas.Handle,
        StretchBlt(FImage.Canvas.Handle, aRect.Left, aRect.Top, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top, dc, 0, 0,
               bmMaskInfo.bmWidth, bmMaskInfo.bmHeight, SRCAND);
        SelectObject(dc, oldbm);
        DeleteObject(IconInfo.hbmMask);
        oldbm := SelectObject(dc, IconInfo.hbmColor);
//        StretchBlt(FImage.Canvas.Handle, aRect.Left, aRect.Top, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top, dc, 0, 0,
//               bmColorInfo.bmWidth, bmColorInfo.bmHeight, SRCINVERT);
        SelectObject(dc, oldbm);
        DeleteObject(IconInfo.hbmColor);
      end;
      FImage.Canvas.Brush.Color := OldBrushColor;
      FImage.Canvas.Pen.Color := OldPenColor;
      SetBkColor(CanvasHandle, OldBkColor);
      DeleteDc(Dc);
    End;
//  End;
end;
            *)
Procedure TWPScreenCanvas.DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer);
Const
  PIXELMAX = 32768;
Type
  TRGBTripleArray = Array[0..PIXELMAX-1] Of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

Var
  aRect : TRect;
  oLocal : TFslBitmapGraphic;
  iRow : Integer;
  iColumn : Integer;
  aRow : pRGBTRipleArray;
Begin
  NeedImage;
  aRect.Left := iLeft;
  aRect.Top := iTop - FInternalTop;
  aRect.Right := iRight;
  aRect.Bottom := iBottom - FInternalTop;
  FImage.Canvas.CopyMode := aCopyMode;

  If FContrast = 1 Then
  Begin
    oImage.StretchDraw(FImage.Canvas, aRect);
  End
  Else
  Begin
    oLocal := TFslBitmapGraphic.Create;
    Try
      oLocal.Height := oImage.Height;
      oLocal.Width := oImage.Width;
      oLocal.Handle.PixelFormat := pf24bit;
      oLocal.Handle.Canvas.Draw(0, 0, TFslVCLGraphic(oImage).Handle);

      For iRow := 0 To oLocal.Handle.Height - 1 Do
      Begin
        aRow := oLocal.Handle.ScanLine[iRow];
        For iColumn := 0 To oLocal.Handle.Width - 1 Do
        Begin
          aRow[iColumn].rgbtBlue := Trunc(RealMin(aRow[iColumn].rgbtBlue * FContrast, 255));
          aRow[iColumn].rgbtGreen := Trunc(RealMin(aRow[iColumn].rgbtGreen * FContrast, 255));
          aRow[iColumn].rgbtRed := Trunc(RealMin(aRow[iColumn].rgbtRed * FContrast, 255));
        End;
      End;

      FImage.Canvas.StretchDraw(aRect, oLocal.Handle);
    Finally
      oLocal.Free;
    End;
  End;
End;


Procedure TWPScreenCanvas.ApplyFont;
Begin
  NeedImage;
  If FFontChanged Then
  Begin
    Font.SetVCLFont(FImage.Canvas.Font);
    FFontChanged := False;
  End;
End;


Procedure TWPScreenCanvas.FontChange(oSender : TObject);
Begin
  FFontChanged := True;
End;


Procedure TWPScreenCanvas.Clip(Const aRect : TRect);
Begin
  NeedImage;
  Assert(CheckCondition(FClip = 0, 'Clip', 'Unable to nest calls to clip'));
  FClip := CreateRectRgn(aRect.Left, aRect.Top - FInternalTop, aRect.Right, aRect.Bottom - FInternalTop);
  SelectClipRgn(FImage.Canvas.Handle, FClip);
End;

Procedure TWPScreenCanvas.Clip(oObject : TWPMapObject);
Begin
  NeedImage;
  If oObject.HasClip Then
  Begin
    Assert(CheckCondition(FClip = 0, 'Clip', 'Unable to nest calls to clip'));
    FClip := CreateRectRgn(oObject.ClipLeft, oObject.ClipTop - FInternalTop, oObject.ClipRight, oObject.ClipBottom - FInternalTop);
    SelectClipRgn(FImage.Canvas.Handle, FClip);
  End;
End;


Procedure TWPScreenCanvas.UnClip;
Begin
  NeedImage;
  If FClip <> 0 Then
  Begin
    SelectClipRgn(FImage.Canvas.Handle, 0);
    DeleteObject(FClip);
    FClip := 0;
  End;
End;


Procedure TWPScreenCanvas.ClearTail;
Begin
  DrawRect(ApplyContrast(Background), 0, FStatedHeight, Width, Height);
End;


Procedure TWPScreenCanvas.SetBackground(Const Value : TColour);
Begin
  If Value <> FBackground Then
  Begin
    FBackground := Value;
    Initialise(FStatedWidth, FStatedHeight);
  End;
End;


Procedure TWPScreenCanvas.SetContrast(Const Value : Real);
Begin
  If Value <> FContrast Then
  Begin
    FContrast := Value;
    Initialise(FStatedWidth, FStatedHeight);
  End;
End;


Procedure TWPScreenCanvas.Yield;
Begin
  FImage.Free;
  FImage := Nil;
End;


Procedure TWPScreenCanvas.DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic; iX, iY : Integer; iInnerRadius, iOuterRadius : Integer; iRotation : Integer);
Var
  aPoints : Array Of TPoint;
  idx1, idx2, idy1, idy2 : Integer;
Begin
  NeedImage;
  iY := iY - FInternalTop;
  FImage.Canvas.Pen.Width := 0;
  FImage.Canvas.Pen.Style := psClear;

  idx1 := 0;
  idx2 := 0;
  idy1 := 0;
  idy2 := 0;
  If iRotation = 270 Then
  Begin
    idx2 := 1;
    idy2 := 1;
  End Else If iRotation = 0 Then
  Begin
    idx2 := 1;
    idy2 := -1;
  End;

  If aInnerColour <> DEF_COLOUR Then
  Begin
    FImage.Canvas.Pen.Color := aInnerColour;
    FImage.Canvas.Brush.Color := aInnerColour;
    SetLength(aPoints, 6);
    aPoints[0].x  := iX;
    aPoints[0].y  := iY;
    aPoints[1].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idx1;
    aPoints[1].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idy1;
    aPoints[2].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idx1;
    aPoints[2].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idy1;
    aPoints[3].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idx1;
    aPoints[3].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idy1;
    aPoints[4].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idx1;
    aPoints[4].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idy1;
    aPoints[5].x  := iX;
    aPoints[5].y  := iY;
    FImage.Canvas.Polygon(aPoints);
  End;

  FImage.Canvas.Pen.Color := aColour;
  FImage.Canvas.Brush.Color := aColour;
  If oBitmap <> Nil Then
    FImage.Canvas.Brush.Bitmap := oBitmap.Handle;

  SetLength(aPoints, 12);
  aPoints[0].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[0].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[1].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idx1;
  aPoints[1].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idy1;
  aPoints[2].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idx1;
  aPoints[2].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idy1;
  aPoints[3].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idx1;
  aPoints[3].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idy1;
  aPoints[4].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idx1;
  aPoints[4].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idy1;
  aPoints[5].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iOuterRadius);
  aPoints[5].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iOuterRadius);
  aPoints[6].x  := iX + Trunc(cos(DegreesToRadians(75 + iRotation)) * iOuterRadius)+idx2;
  aPoints[6].y  := iY - Trunc(sin(DegreesToRadians(75 + iRotation)) * iOuterRadius)+idy2;
  aPoints[7].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iOuterRadius)+idx2;
  aPoints[7].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iOuterRadius)+idy2;
  aPoints[8].x  := iX + Trunc(cos(DegreesToRadians(45 + iRotation)) * iOuterRadius)+idx2;
  aPoints[8].y  := iY - Trunc(sin(DegreesToRadians(45 + iRotation)) * iOuterRadius)+idy2;
  aPoints[9].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iOuterRadius)+idx2;
  aPoints[9].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iOuterRadius)+idy2;
  aPoints[10].x := iX + Trunc(cos(DegreesToRadians(15 + iRotation)) * iOuterRadius)+idx2;
  aPoints[10].y := iY - Trunc(sin(DegreesToRadians(15 + iRotation)) * iOuterRadius)+idy2;
  aPoints[11].x := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[11].y := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iOuterRadius)+idy2;
  FImage.Canvas.Polygon(aPoints);
End;


Procedure TWPScreenCanvas.DrawTriangle(aColour : TColour; iX1, iY1, iX2, iY2, iX3, iY3: Integer);
Var
  aPoints : Array Of TPoint;
Begin
  FImage.Canvas.Pen.Width := 1;
  FImage.Canvas.Pen.Color := aColour;
  FImage.Canvas.Pen.Style := psSolid;
  FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesRound, apjsRound);
  FImage.Canvas.Brush.Color := aColour;
  FImage.Canvas.Brush.Style := bsSolid;

  SetLength(aPoints, 4);
  aPoints[0].X := iX1;
  aPoints[0].Y := iY1 - FInternalTop;
  aPoints[1].X := iX2;
  aPoints[1].Y := iY2 - FInternalTop;
  aPoints[2].X := iX3;
  aPoints[2].Y := iY3 - FInternalTop;
  aPoints[3].X := iX1;
  aPoints[3].Y := iY1 - FInternalTop;

  FImage.Canvas.Polygon(aPoints);
End;

Procedure TWPScreenCanvas.DrawPolyLine(aColour: TColour; aPenStyle: TFslPenStyle; aEndStyle: TFslPenEndStyle; iXOffset, iYOffset: Integer; oCoords: TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = True);
Var
  aPoints : Array Of TPoint;
  iLoop : Integer;
Begin
  If oCoords.Count > 0 Then
  Begin
    FImage.Canvas.Pen.Width := 1;
    FImage.Canvas.Pen.Color := aColour;
    FImage.Canvas.Pen.Style := ADVPENSTYLE_VCLVALUES[aPenStyle];
    FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesRound, apjsRound);
    If bClosed Then
    Begin
      SetLength(aPoints, oCoords.Count+1);
      aPoints[oCoords.Count].X := iXOffset + round(oCoords[0].X * rFactorX);
      aPoints[oCoords.Count].Y := iYOffset + round(oCoords[0].Y * rFactorY) - FInternalTop;
    End
    Else
      SetLength(aPoints, oCoords.Count);
    For iLoop := 0 To oCoords.Count - 1 Do
    Begin
      aPoints[iLoop].X := iXOffset + round(oCoords[iLoop].X * rFactorX);
      aPoints[iLoop].Y := iYOffset + round(oCoords[iLoop].Y * rFactorY) - FInternalTop;
    End;
    FImage.Canvas.Polyline(aPoints);
  End;
End;

Procedure TWPScreenCanvas.Scrub;
Begin
  FImage.Canvas.brush.Style := bsDiagCross;
  FImage.Canvas.brush.Color := clLime;
  FImage.Canvas.Rectangle(0,0,FImage.Width, FImage.Height);
End;

Function TWPScreenCanvas.SaveToImage(height, width : Cardinal): TFslBitmapGraphic;
Begin
  NeedImage;

  Result := TFslBitmapGraphic.Create;
  Try
    Result.Handle := TBitmap.Create;
    {$IFNDEF NO_VCL_CANVAS_LOCK}
    Result.Handle.Canvas.Lock;
    Try
    {$ENDIF}
      Result.Handle.HandleType := bmDIB;
      Result.Handle.Width := width;
      Result.Handle.Height := height;
      Result.Handle.Canvas.Draw(0,0, FImage);
    {$IFNDEF NO_VCL_CANVAS_LOCK}
    Finally
      Result.Handle.Canvas.Unlock;
    End;
    {$ENDIF}

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure TWPScreenCanvas.DrawRoundOutline(aColour : TColour; iLeft, iTop, iRight, iBottom, iRadius : Integer);
Begin
  FImage.Canvas.Pen.Width := 1;
  FImage.Canvas.Pen.Color := aColour;
  FImage.Canvas.Pen.Style := psSolid;
  FImage.Canvas.Pen.Handle := MakePenHandle(FImage.Canvas.Pen, apesRound, apjsRound);
  FImage.Canvas.Brush.Color := aColour;
  FImage.Canvas.Brush.Style := bsClear;
  FImage.Canvas.RoundRect(iLeft, iTop - FInternalTop, iRight, iBottom - FInternalTop, iRadius, iRadius);
End;

Constructor TWPPageLayoutController.Create(aSpanPolicy : TWPSpanPolicy);
Begin
  Create;
  FSpanPolicy := aSpanPolicy;
End;


//Procedure TWPPageLayoutController.SetSpanPolicy(aSpanPolicy : TWPSpanPolicy);
//Begin
//  FSpanPolicy := aSpanPolicy;
//End;


Function TWPPageLayoutController.Link : TWPPageLayoutController;
Begin
  Result := TWPPageLayoutController(Inherited Link);
End;


Function TWPPageLayoutController.SpanPolicy : TWPSpanPolicy;
Begin
  Result := FSpanPolicy;
End;


Function TWPPageLayoutController.Width(oCanvas : TFslPrinterCanvas) : Integer;
Begin
  Result := 0;
  RaiseError('GetWidth', 'Need to Override '+ClassName+'.GetWidth');
End;


Function TWPPageLayoutController.Left(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer;
Begin
  Result := 0;
  RaiseError('GetLeft', 'Need to Override '+ClassName+'.GetLeft');
End;


Function TWPPageLayoutController.Top(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer;
Begin
  Result := 0;
  RaiseError('GetTop', 'Need to Override '+ClassName+'.GetTop');
End;


Function TWPPageLayoutController.Height(iPage : Integer; oCanvas : TFslPrinterCanvas):Integer;
Begin
  Result := 0;
  RaiseError('GetHeight', 'Need to Override '+ClassName+'.GetHeight');
End;


Function TWPPageLayoutController.IsSpanPolicySpan : Boolean;
Begin
  Result := SpanPolicy = spSpan;
End;


Function TWPPageLayoutController.IsSpanPolicyError : Boolean;
Begin
  Result := SpanPolicy = spError;
End;


Function TWPPageLayoutController.IsSpanPolicyTruncate : Boolean;
Begin
  Result := SpanPolicy = spTruncate;
End;


Procedure TWPPageLayoutController.SpanPolicySpan;
Begin
  FSpanPolicy := spSpan;
End;


Procedure TWPPageLayoutController.SpanPolicyError;
Begin
  FSpanPolicy := spError;
End;


Procedure TWPPageLayoutController.SpanPolicyTruncate;
Begin
  FSpanPolicy := spTruncate;
End;

Constructor TWPPage.Create;
Begin
  Inherited;
End;


Destructor TWPPage.Destroy;
Begin
  FMap.Free;
  Inherited;
End;


Function TWPPage.GetMap : TWPMapContainer;
Begin
  Assert(Invariants('GetMap', FMap, TWPMapContainer, 'Map'));
  Result := FMap;
End;


function TWPPage.isEmpty: boolean;
begin
  result := FMap.Children.IsEmpty and FMap.Rows.IsEmpty;
end;

Procedure TWPPage.SetMap(Const Value : TWPMapContainer);
Begin
  FMap.Free;
  FMap := Value;
End;


Function TWPPage.Link : TWPPage;
Begin
  Result := TWPPage(Inherited Link);
End;


Function TWPPage.Clone : TWPPage;
Begin
  Result := TWPPage(Inherited Clone);
End;


Procedure TWPPage.Assign(oObject : TFslObject);
Begin
  Inherited;
End;


Function TWPPage.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPPage;
End;


Function TWPPage.Fits(iHeight : Integer) : Boolean;
Begin
  Result := FCursor + iHeight < Map.Bottom;
End;


function TWPPage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMap.sizeInBytes);
end;

Function TWPPages.Link : TWPPages;
Begin
  Result := TWPPages(Inherited Link);
End;


Function TWPPages.Clone : TWPPages;
Begin
  Result := TWPPages(Inherited Clone);
End;


Function TWPPages.New : TWPPage;
Begin
  Result := TWPPage(Inherited New);
End;


Function TWPPages.ItemClass : TFslObjectClass;
Begin
  Result := TWPPage;
End;


Function TWPPages.GetElement(Const iIndex : Integer) : TWPPage;
Begin
  Result := TWPPage(ObjectByIndex[iIndex]);
End;


Procedure TWPPages.SetElement(Const iIndex : Integer; Const oValue : TWPPage);
Begin
  ObjectByIndex[iIndex] := oValue;
End;


Function TWPPages.Get(Const aValue : Integer) : TWPPage;
Begin
  Result := TWPPage(Inherited Get(aValue));
End;


Function TWPPage.GetLastOffset: Integer;
Begin
  Result := FMap.GetLastOffset;
End;


{ TWPVisualRange }

Destructor TWPVisualRange.Destroy;
Begin
  FRenderer.Free;
  Inherited;
End;

Function TWPVisualRange.GetRenderer: TWPScreenRenderer;
Begin
  Assert(CheckCondition(HasRenderer, 'GetRenderer', 'Renderer not provided'));
  Result := FRenderer;
End;

Function TWPVisualRange.HasRenderer: Boolean;
Begin
  Result := FRenderer <> Nil;
End;

Procedure TWPVisualRange.SetRenderer(Const Value: TWPScreenRenderer);
Begin
  FRenderer.Free;
  FRenderer := Value;
End;

Function TWPVisualRange.PageUp(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.PageUp(SelCursorToChange(bSelect), PageHeight, iNew);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft);
End;

Function TWPVisualRange.LineUp(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.LineUp(SelCursorToChange(bSelect), iNew);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft)
  Else If bSelect Then
    Result := GoLineStart(True);
End;

Function TWPVisualRange.GoLineStart(bSelect: Boolean): Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.LineHome(SelCursorToChange(bSelect), iNew);
  If Result Or Selection.HasSelection Then
    MoveCursor(bSelect, iNew, cdJump);
End;

Function TWPVisualRange.GoLineEnd(bSelect: Boolean): Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.LineEnd(SelCursorToChange(bSelect), iNew);
  If Result Or Selection.HasSelection Then
    MoveCursor(bSelect, iNew, cdJump);
End;

Function TWPVisualRange.LineDown(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.LineDown(SelCursorToChange(bSelect), iNew);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight);
End;

Function TWPVisualRange.PageDown(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := FRenderer.PageDown(SelCursorToChange(bSelect), PageHeight, iNew);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight);
End;



Function TWPVisualRange.LineMove(iDelta: Integer; bSelect: Boolean): Boolean;
Begin
  // TODO
  Result := False;
End;


Procedure TWPVisualRange.ChangeState(bDraw: Boolean);
Begin
  If bDraw And (FRenderer <> Nil) Then
    FRenderer.Update;
  Inherited;
End;

Function TWPVisualRange.GetWorkingWidth: Integer;
Begin
  Result := Renderer.Width;
End;

procedure TWPVisualRange.CutOff;
begin
  inherited;
  Renderer := Nil;
end;


Initialization
  INTERNAL_HEIGHT := GetSystemMetrics(79 {SM_CYVIRTUALSCREEN});
End.
