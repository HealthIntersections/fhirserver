Unit FHIR.WP.Control;

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

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Forms, Graphics, Messages, Math, Contnrs, Vcl.Touch.GestureMgr,

  DropBMPTarget, DropSource, DropTarget,

  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_shell,
  fsl_mapi,

  fUi_vclx_Base, fUi_vclx_controls,
  wp_graphics, wp_printing_win,
  wp_types, wp_document, wp_format, wp_working, FHIR.WP.Settings,
  FHIR.WP.Unicode,
  FHIR.WP.Printing, FHIR.WP.Engine, wp_definers, FHIR.WP.Handler, wp_native,
  FHIR.WP.Renderer, FHIR.WP.Menus, FHIR.WP.Spelling, FHIR.WP.Icons, FHIR.WP.Builder,
  FHIR.WP.Dragon;

Type
  TWPCapability = FHIR.WP.Engine.TWPCapability;
  TWPCapabilities = FHIR.WP.Engine.TWPCapabilities;

Const
  canInsert = FHIR.WP.Engine.canInsert;
  canWrite = FHIR.WP.Engine.canWrite;
  canDelete = FHIR.WP.Engine.canDelete;
  canCut = FHIR.WP.Engine.canCut;
  canUndo = FHIR.WP.Engine.canUndo;
  canRedo = FHIR.WP.Engine.canRedo;
  canInsertImage = FHIR.WP.Engine.canInsertImage;
  canImageProps = FHIR.WP.Engine.canImageProps;
  canLineProps = FHIR.WP.Engine.canLineProps;
  canInsertField = FHIR.WP.Engine.canInsertField;
  canFieldProps = FHIR.WP.Engine.canFieldProps;
  canRemoveField = FHIR.WP.Engine.canRemoveField;
  canSpliceField = FHIR.WP.Engine.canSpliceField;
  canGotoField = FHIR.WP.Engine.canGotoField;
  canFormat = FHIR.WP.Engine.canFormat;
  canInsertTable = FHIR.WP.Engine.canInsertTable;
  canConvertToTable = FHIR.WP.Engine.canConvertToTable;
  canSelectTable = FHIR.WP.Engine.canSelectTable;
  canTableProps = FHIR.WP.Engine.canTableProps;
  canRemoveTable = FHIR.WP.Engine.canRemoveTable;
  canInsertRowAbove = FHIR.WP.Engine.canInsertRowAbove;
  canInsertRowBelow = FHIR.WP.Engine.canInsertRowBelow;
  canSelectRow = FHIR.WP.Engine.canSelectRow;
  canRowProps = FHIR.WP.Engine.canRowProps;
  canRemoveRow = FHIR.WP.Engine.canRemoveRow;
  canInsertColumn = FHIR.WP.Engine.canInsertColumn;
  canRemoveColumn = FHIR.WP.Engine.canRemoveColumn;
  canInsertLine = FHIR.WP.Engine.canInsertLine;
  canInsertPageBreak = FHIR.WP.Engine.canInsertPageBreak;
  SCROLLBAR_WIDTH = 16;
  UM_CLOSE_TOUCH = WM_USER + 26;

Type
  TWordProcessor = Class;

  TWPHotspotInformation = Class (TFslObject)
    Private
      FBox : TRect;
      FHotspot : TWPHotspot;
      FMouseButton: TMouseButton;
      FHandled: Boolean;
      Procedure SetHotspot(Const Value: TWPHotspot);
    Public
      destructor Destroy; Override;
      // the screen co-ordinates for the hotspot
      Property Box : TRect Read FBox Write FBox;
      // the hotspot itself
      Property Hotspot : TWPHotspot Read FHotspot Write SetHotspot;
      // the mousebutton
      Property MouseButton : TMouseButton Read FMouseButton Write FMouseButton;
      // whether the WP should act is if it's been handled
      // (usually for right mouse button: should WP bring up it's normal popup?)
      // default true
      Property Handled : Boolean Read FHandled Write FHandled;
  End;


  TFieldUpdateStatus = (fusDeliberate, fusChange, fusLoading);

  TWPInspector = Class (TUixPanel)
    Protected
      FHeader : TUixPanel;
      FFooter : TUixPanel;
      FFooterCaption : TUixLabel;
      Function DesiredWidth : Integer; Virtual;
      Function InspectorCaption : String; Virtual;
      Procedure WantClose(oSender : TObject); Virtual;
      Procedure Initialise; Override;
  End;

  TWPInspectorList = Class(TUixPanelList)
    Private
      Function GetPanelByIndex(Const iIndex : Integer) : TWPInspector;

    Protected
      Function ItemClass : TComponentClass; Overload; Override;

    Public
      Property InspectorByIndex[Const iIndex : Integer] : TWPInspector Read GetPanelByIndex; Default;
  End;

  // get's called constantly while user moves mouse over a hotspot, and then
  // once with active = false when user leaves
  TWPHotSpotHoverEvent = Procedure (oSender : TWordProcessor; bActive : Boolean; oInfo : TWPHotspotInformation) Of Object;
  TWPHotSpotEvent = Procedure (oSender : TWordProcessor; oInfo : TWPHotspotInformation) Of Object;
  TWPCodeCompletionEvent = Function (oSender : TObject; Const sText : String; oItems : TWPCompletionItems) : Boolean Of Object;
  TWPTemplateEvent = Function (oSender : TObject) : TFslBuffer Of Object; // must return a native format buffer if any content is to be inserted
  TWPNamedTemplateEvent = Function (oSender : TObject; Const iId : Integer; Const sName : String; Var aFormat : TWPFormat) : TFslBuffer Of Object; // must return a native format buffer if any content is to be inserted
  TWPFieldUpdateEvent = Procedure (oSender : TObject; aState : TFieldUpdateStatus; oDefinitionProvider : TWPFieldDefinitionProvider; oField : TWPDocumentField; sContent : String; bValid : boolean) of Object;

  TWordProcessCursorDetailsMode = (cdBlink, cdSelect, cdDrag);
  TWordProcessorSelectionHot = (shNeither, shStart, shEnd);

  TWordProcessCursorDetails = Class (TFslObject)
  Private
    FMode : TWordProcessCursorDetailsMode;
    FShowing: Boolean;
    FLeft: Integer;
    FHeight: Integer;
    FTop: Integer;
    FColour: TColour;

    FDragMarkTop : Integer;
    FDragMarkLeft : Integer;
    FDragMarkHeight : Integer;
    FDragMarkColour: TColour;

    FSelStartTop : Integer;
    FSelStartLeft : Integer;
    FSelStartHeight : Integer;
    FSelStartColour: TColour;

    FSelEndTop : Integer;
    FSelEndLeft : Integer;
    FSelEndHeight : Integer;
    FSelEndColour: TColour;

    FSelHot : TWordProcessorSelectionHot;
    Procedure Clear;
  Public
    Property Showing : Boolean Read FShowing Write FShowing;
    Property Left : Integer Read FLeft Write FLeft;
    Property Top : Integer Read FTop Write FTop;
    Property Height : Integer Read FHeight Write FHeight;
    Property Colour : TColour Read FColour Write FColour;
  End;

  TWordProcessorDocumentHandler = Class (TWPDocumentHandler)
    Private
      FOwner : TWordProcessor;
    Protected
      Function GetHost : TObject; Override;
      Function GetHostConfiguredStyles : TWPStyles; Override;
      Function GetHostWorkingStyles : TWPStyles; Override;
      Function GetHostDocument : TWPWorkingDocument; Override;
      Procedure SetHostDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles); Override;
    Public
      constructor Create(oOwner : TWordProcessor); Overload; Virtual;
  End;


  TWordProcessorCheckor = Class (TFslObject)
    Public
      Function Invariants(Const sLocation : String; oObject : TFslObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
  End;

  TWPMouseActor = Class (TFslObject)
    Private
      FWordProcessor : TWordProcessor;
    Protected
      FLastTop : Integer;
      FLastBottom : Integer;
      FLastLeft : Integer;
      FLastRight : Integer;
      FOffsetSquareforScroll : Boolean;
      Procedure Square(iLeft, iTop, iRight, iBottom : Integer);
    Public
      constructor Create(oWordProcessor : TWordProcessor); Overload; Virtual;
      destructor Destroy; Override;

      Function Link : TWPMouseActor; Overload;

      Procedure Disconnect; Overload; Virtual;

      Function RequiresDown : Boolean; Overload; Virtual;
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Virtual;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Virtual;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Virtual;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Virtual;
      Procedure Close(oNewActor : TWPMouseActor); Overload; Virtual;
  End;

  TWPTouchManager = class (TFslObject)
  private
    FOwner : TWordProcessor;
    FLoaded : boolean;
    FGestures : TGestureManager;
    FZoomStart : Integer;
    FZoomBase : real;
    FPanStart : Integer;
    FPanBase : Integer;
    procedure DoGesture(sender :TObject; const EventInfo: TGestureEventInfo; var Handled :boolean);

    Function Zoom(EventInfo : TGestureEventInfo) : boolean;
    Function Pan(EventInfo : TGestureEventInfo) : boolean;
    Function TwoFingers(EventInfo : TGestureEventInfo) : boolean;
  public
    constructor Create(owner : TWordProcessor);
    destructor Destroy; override;

    procedure Load;
    Procedure Unload;
  end;

  TWordProcessor = Class (TUixControl)
  Private
    FDefaultFieldDefinition: TWPFieldDefinitionProvider;
    FInspectors: TWPInspectorList;
    FSettings : TWPSettings;
    FCheckor : TWordProcessorCheckor;
    FDocument: TWPWorkingDocument;
    FConfiguredStyles: TWPStyles;
    FWorkingStyles: TWPStyles;
    FOperator: TWPOperator;
    FRangeManager : TWPRangeManager;
    FPrimaryRange: TWPVisualRange;
    FRenderer : TWPScreenRenderer;
    FTimer : TUixTimer;
    FCursorDetails : TWordProcessCursorDetails;
    FMouseActor : TWPMouseActor;
    FJustDoubleClicked : Boolean;
    FOnContentChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FPopup : TWPPopupMenu;
    FObservers : TWPObservers;
    FFontsAllowed: TFslStringList;
    FOnCodeCompletion : TWPCodeCompletionEvent;
    FSpeller: TWPSpeller;
    FDocumentHandler : TWPDocumentHandler;
    FVoiceActive : Boolean;
    FExistingSearch : TWPSearchDetails;
    FExistingReplace : TWPReplaceDetails;
    FNextControl : TWinControl;
    FPreviousControl : TWinControl;
    FDropBMP : TDropBMPTarget;
    FAltNum : String;
    FWorkingWidthLimit : Integer;
    FOnHotSpot : TWPHotSpotEvent;
    FOnHotSpotHover : TWPHotSpotHoverEvent;
    FResetMouseCursor : Boolean;
    FTranslator : TWPDocumentTranslator;
    FScrollPoint : TPoint;     // Current scroll position.
    FPropertyServer : TWPPropertyServer;
    FPlaybackManager : TDragonPlaybackManager;
    FCursorPersist : Boolean;
    FCursorOutside : Boolean;
    FModified : Boolean;
    FLastSnapshot : Integer;
    FScrollBarShowing : Boolean;
    FNonVisibleMode : Boolean; // support for non-visual unit tests
    FCodeCompletionListBox : TWPCodeCompletionListBox;
    FTouchMenu : TUixPanel;
    FSymbolFilter : String;
    FSymbolBlock : TUnicodeBlock;
    FTouch : TWPTouchManager;

    // pagination support. These fields are only used to support page estimations.
    // printers must be supplied to the print dialog etc for actual printing.
    FPrinter : TFslPrinter;
    FPageLayoutController : TWPPageLayoutController;
    FPaginator : TWPPaginator;
    FOnTemplate: TWPTemplateEvent;
    FOnNamedTemplate: TWPNamedTemplateEvent;
    FOnPageDesign : TNotifyEvent;
    FOnPrint : TNotifyEvent;
    FOnExport : TNotifyEvent;
    FOnImport : TNotifyEvent;
    FOnSave : TNotifyEvent;
    FOnSaveAs : TNotifyEvent;
    FOnOpen : TNotifyEvent;
    FOnNew : TNotifyEvent;
    FOnUpdateField : TWPFieldUpdateEvent;

    // macros
    FMacro : TWPMacro;
    FOnSettingsChanged: TNotifyEvent;
    FOnDocumentLoaded: TNotifyEvent;

    Procedure SetDefaultFieldDefinition(Const Value: TWPFieldDefinitionProvider);
    Procedure VoiceSelect(iTextStart, iTextLength : Integer; bDraw : Boolean);

    Procedure DragDropFiles(Var VMsg: TMessage); Message wm_DropFiles;
    Procedure SetFocusedControl(oControl : TWinControl);
    Function GetDocumentHandler : TWPDocumentHandler;
    Function GetOperator : TWPOperator;
    Function GetSpeller : TWPSpeller;
    Procedure SetSpeller(Const Value: TWPSpeller);
    Function GetDocument : TWPWorkingDocument;
    Function GetRenderer : TWPScreenRenderer;
    Function GetSelection : TWPSelection;

    Function GetConfiguredStyles : TWPStyles;
    Procedure SetConfiguredStyles(Const Value: TWPStyles);
    Function GetWorkingStyles : TWPStyles;
    Procedure SetWorkingStyles(Const Value: TWPStyles);
    Procedure SetWorkingWidthLimit(Const Value: Integer);
    Function GetPageLayoutController : TWPPageLayoutController;
    Procedure SetPageLayoutController(Const Value : TWPPageLayoutController);
    Function GetPrinter : TFslPrinter;
    Procedure SetPrinter(Const Value : TFslPrinter);

    Function GetSettings : TWPSettings;
    Procedure SetSettings(Const Value : TWPSettings);
    Procedure SettingsChange(Const aProperties :  TWPSettingsProperties);

    Function GetNextControl : TWinControl;
    Function GetPreviousControl : TWinControl;

    Procedure Timer(oSender : TObject);
    Procedure ToggleCursor;
    Procedure DrawCursor(bShow : Boolean);
    Procedure ActorContentChange(oSender : TObject);
    Procedure ActorSelectionChange(oSender : TObject);
    Procedure ActorInsertContent(oSender : TObject; iPoint : Integer; oPieces : TWPWorkingDocumentPieces);
    Procedure ActorDeleteContent(oSender : TObject; iPoint : Integer; oPieces : TWPWorkingDocumentPieces);
    Procedure LayoutDocumentHeightChange(oSender : TObject);
    Function GetFontsAllowed : TFslStringList;
    Procedure SetFontsAllowed(Const Value: TFslStringList);
    Procedure InsertCompletionItem(oItem : TWPCompletionItem);
    Function GetCursorEndPos : TPoint;
    Function ActualWidth : Integer;
    Procedure DragMarkCleared;
    Procedure ClearDragMark;
    Procedure DrawDragMark(iOffset : Integer);

    Procedure InvokePopop;
    Procedure WMEraseBkgnd(Var Message: TWmEraseBkgnd); Message WM_ERASEBKGND;
    Procedure WMContextMenu(Var aMessage : TWMContextMenu); Message WM_CONTEXTMENU;

    Function GetExistingSearch : TWPSearchDetails;
    Procedure SetExistingSearch(Const Value : TWPSearchDetails);
    Function GetExistingReplace : TWPReplaceDetails;
    Procedure SetExistingReplace(Const Value : TWPReplaceDetails);

    Procedure CheckDocument(oDocument : TWPWorkingDocument);
    function BindToFields(oDocument : TWPWorkingDocument) : boolean;
    Procedure CheckField(oErrors : TFslStringList; oPiece : TWPWorkingDocumentFieldStartPiece);

    Function GetSelectionSummary : String; Overload; Virtual;
    Function GetSelectionEnd: Integer;
    Function GetSelectionStart: Integer;

    Procedure CommitField;

    Procedure ShowError(Const sTitle, sMessage : String; iDuration : Integer);
    Procedure HandleError(Const sContext, sMessage : String);

    Function IsAnnotationKey(iKey: Word; Const aShift: TWPShiftStates) : Boolean;
    Function ProcessKey(iKey: Word; Const aShift: TWPShiftStates) : Boolean;

    Procedure LoadNewDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles);
    Function GetReadOnly : Boolean;
    Procedure SetReadOnly(Const Value : Boolean);

    Function GetPlaybackManager : TDragonPlaybackManager;
    Procedure SetPlaybackManager(Const Value : TDragonPlaybackManager);

    Function CheckForHotspot(ch : Char) : Boolean;

    Procedure UpdateScrollBar;
    Procedure ScrollbarPositionChanged;

    Procedure Paginate;
    Procedure FinishPagination;
    Function MakeMouseActor(aAction : TWPMouseAction; iOffset : Integer) : TWPMouseActor;
    Procedure CheckLostHotspot;
    Function MouseIsOver : Boolean;
    Function GetTopLine : Integer;
    Procedure SetTopLine(Const iValue : Integer);
    Function GetThumb : Integer;
    Procedure SetThumb(iValue : Integer);

    Procedure GetSelectedTableAsTextArray(Var aRows: Array Of TStringList);
    Procedure SetOnTemplate(Const Value: TWPTemplateEvent);

    Procedure ProduceSnapShot(Const sFilename, sCause: String);
    Function FetchTemplateContent(Const iId : Integer; Const sCode : String; var aFormat : TWPFormat) : TFslStream;

    Procedure ToggleMacro;
    Procedure ExecuteMacro;
    Procedure CheckForAllowedWord(Sender:TObject; Const Word:String; Var CheckType:TWordCheckType; Var Replacement:String);
    Procedure AddAllowedWord(Sender:TObject; Const Word:String);
    Function GetImageTool: TImageTool;
    Procedure SetImageTool(Const Value: TImageTool);
    Function WantDragSelection(x, y : integer; var oActor : TWPMouseActor) : Boolean;
    Function DoNew : Boolean;
    Function DoOpen : Boolean;
    Function DoSave : Boolean;
//    Function DoSaveAs : Boolean;
    Function DoPrint : Boolean;
  Protected
    Procedure ApplyCursorState(Const aCursor : TCursor; bPersist : Boolean);
    Procedure CloseTouchMenu(Var Message: TMessage); Message UM_CLOSE_TOUCH;

    Procedure DoExit; Override;
    Procedure WMVScroll(Var Message: TWMVScroll); Message WM_VSCROLL;
    Procedure WMGetDlgCode(Var Msg: TWMGetDlgCode); Message WM_GETDLGCODE;
    Procedure CreateWnd; Override;
    Procedure Paint; Override;
    Procedure Resize; Override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); Override;
    Procedure DblClick; Override;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); Override;
    Procedure KeyUp(Var Key: Word; Shift: TShiftState); Override;
    Procedure ShowPopup(iX, iY : Integer; oInfo : TWPMouseInfo);
    Function DoMouseWheel(aShift: TShiftState; iWheelDelta: Integer; aMousePos: Windows.TPoint): Boolean; Override;
    Procedure ScrollToTop;

    Procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; Var Accept: Boolean); Override;
    Procedure BMPDrop(oSender: TObject; aShiftState: TShiftState; aPoint: Windows.TPoint; Var iEffect: LongInt);

    Procedure DoHotspotHover(bActive : Boolean; aBox : TRect; oHotspot : TWPHotspot);
    Function DoHotspot(aBox : TRect; oHotspot : TWPHotspot; Const aMouseButton: TMouseButton) : Boolean;
    Procedure DoButton(aBox : TRect; oButton : TFslObject; iOffset : Integer);
    Procedure DoAnnotationClick(oAnnotation : TWPWorkingAnnotation);

    Function NoUserResponse : Boolean; Virtual;
    Function ShowCursorAlways : Boolean; Virtual;
    Procedure NotifyObservers; Virtual;
    Procedure NotifyObserversInsert(iPoint : Integer; oPieces : TWPWorkingDocumentPieces); Virtual;
    Procedure NotifyObserversDelete(iPoint : Integer; oPieces : TWPWorkingDocumentPieces); Virtual;

    Property CursorEndPos : TPoint Read GetCursorEndPos;
    Procedure CMMouseLeave(Var Message: TMessage); Message CM_MOUSELEAVE;
    Procedure CMMouseEnter(Var Message: TMessage); Message CM_MOUSEENTER;
    Property TopLine : Integer Read GetTopLine Write SetTopLine;
    Property Thumb : Integer Read GetThumb Write SetThumb;
    Function GetPrimaryRange : TWPVisualRange;
    Property Popup : TWPPopupMenu Read FPopup;
    Procedure DoPaint; Virtual;
    Procedure DoSnapshot(oWriter : TWPSnapshotWriter); Overload; Virtual;
    Procedure ConfigureRange(oRange : TWPRange); Overload; Virtual;

    Procedure CheckNotInMacro;
    Procedure Ring(aColour : TColour);
  Public
    constructor Create(oOwner : TComponent); Override;
    destructor Destroy; Override;

    Procedure Clear; Overload; Virtual;
    Function ClearFormatting : Boolean; Overload; Virtual; // will fail for tables, fields, images

    Function Empty : Boolean; Overload; Virtual;
    Function SelectedText : String; Overload; Virtual;
    Function TextForLine(iLine : Integer) : String; Overload; Virtual;

    Function HasDocument : Boolean; Overload; Virtual;
    Function HasPlaybackManager : Boolean; Overload; Virtual;
    Function InSpeechMode : Boolean; Virtual;
    procedure CheckOkForSpeech; Virtual;

    Procedure RegisterObserver(oObject : TObject; aEvent : TNotifyEvent); Overload; Virtual;
    Procedure UnregisterObserver(oObject : TObject); Overload; Virtual;
    Procedure DefineFieldProvider(oDefinition : TWPFieldDefinitionProvider; bDefault : Boolean);
    Procedure DefineAnnotationProvider(oDefinition : TWPAnnotationDefinitionProvider);
    Procedure RegisterInspector(oInspector : TWPInspector);
    Procedure RemoveInspector(oInspector : TWPInspector);

    Procedure FontDialog; Overload; Virtual;
    Procedure ParaDialog; Overload; Virtual;
    Procedure StyleDialog; Overload; Virtual;
    Procedure ChangeCaseDialog; Overload; Virtual;
    Procedure PasteSpecialDialog; Overload; Virtual;
    Procedure InsertImageDialog; Overload; Virtual;
    Procedure InsertPDFDialog; Overload; Virtual;
    Procedure ImagePropertiesDialog; Overload; Virtual;
    Procedure ImageMapsDialog; Overload; Virtual;
    Procedure InsertSymbol; Overload; Virtual;
    Procedure EditAnnotation; Overload; Virtual;
    Procedure DeleteAnnotation; Overload; Virtual;
    Procedure AddAnnotation(iStart, iEnd : Integer; Const sNamespace, sContent : String); Overload; Virtual;
    Procedure InsertTemplate; Overload; Virtual;
    Procedure InsertTableDialog; Overload; Virtual;
    Procedure SortTableDialog; Overload; Virtual;
    Procedure TablePropertiesDialog; Overload; Virtual;
    Procedure LinePropertiesDialog; Overload; Virtual;
    Procedure SearchDialog; Overload; Virtual;
    Procedure ReplaceDialog; Overload; Virtual;
    Function  CheckFields(oErrors : TFslStringList): Boolean; Overload; Virtual;
    Procedure CheckSpelling; Overload; Virtual;
    Procedure CodeCompletePrompt; Overload; Virtual;
    Procedure CodeCompletePromptList(oList : TWPCompletionItems); Overload; Virtual;
    Procedure CompletionBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    Procedure ReviewWordDialog; Overload; Virtual;

    Procedure InsertField(oDefn : TWPFieldDefinitionProvider); Overload; Virtual;
    Procedure FieldPropertiesDialog; Overload; Virtual;
    Procedure FieldSectionPropertiesDialog; Overload; Virtual;
    Procedure InsertAnnotation(oDefn : TWPAnnotationDefinitionProvider); Overload; Virtual;
    Function AllFieldsValid(var sError : String) : boolean; Overload; Virtual;
    function ListAllFields(Namespace : String) : TWPDocumentFields;
    Function HasFields : Boolean;
    function GetContentForField(oField : TWPDocumentField) : String;

    Procedure ShowFieldError(Const sNamespace, sName, sTitle, sMessage : String; iDuration : Integer = 3000); Overload; Virtual;
    Procedure ShowSectionError(Const sNamespace, sName, sTitle, sMessage : String; iDuration : Integer = 3000); Overload; Virtual;
    function GetSourceLocation(var line, col : integer) : boolean;

    Procedure VoiceSaveSession(Const sFilename : String); Overload; Virtual;
    Procedure VoiceLoadSession(Const sFilename : String); Overload; Virtual;

    Procedure VoiceActivate; Overload; Virtual;
    Procedure VoiceDeactivate; Overload; Virtual;
    Procedure VoicePlayback; Overload; Virtual;
    Procedure VoiceAccelerate; Overload; Virtual;
    Procedure VoiceDecelerate; Overload; Virtual;
    Procedure VoiceStop; Overload; Virtual;

    Procedure VoiceGetTextChanges(Var iTextStart, iTextLength : Integer; Var sText : String; Var iVisibleStart, iVisibleLength : Integer); Overload; Virtual;
    Procedure VoiceMakeTextChanges(iTextStart, iTextLength : Integer; sText : String); Overload; Virtual;
    Procedure VoiceGetSelectionRange(Var iSelectionStart, iSelectionLength : Integer); Overload; Virtual;
    Procedure VoiceSetSelectionRange(iSelectionStart, iSelectionLength : Integer); Overload; Virtual;

    // support tools
    Procedure SaveImage(Const sFilename : String); Overload; Virtual;
    Procedure SnapShot(Const sCause : String; Const DoMail : Boolean); Overload; Virtual;

    Function DocumentHeight : Integer; Overload; Virtual;
    Function MinimumRecommendedHeight : Integer;
    Function MinimumRecommendedWidth : Integer;

    Function HasNextControl : Boolean; Overload; Virtual;
    Function HasPreviousControl : Boolean; Overload; Virtual;
    Function HasSpeller : Boolean; Overload; Virtual;
    Function HasWorkingStyles : Boolean; Overload; Virtual;
    Function InTouchMode : boolean;

    Function HasVoicePlayback : Boolean; Overload; Virtual;

    Function CheckOffsetVisible(iIndex : Integer) : Boolean; Overload; Virtual;

    Function HasImageArea(Const sURL: String; Out oImage: TWPWorkingDocumentImagePiece; Out oArea: TWPImageMapArea): Boolean; Overload; Virtual;
    Procedure SelectImageArea(oImage: TWPWorkingDocumentImagePiece; oArea: TWPImageMapArea); Overload; Virtual;

    Procedure Yield; Overload; Virtual;// call this when WP is no longer visible

    Function ListAllFonts : TStringList;
    Function ListSpecialFonts : TStringList;

    Function GoToFieldByName(Const sNamespace, sName : String) : Boolean;

    Function CheckOkForConsoleMode(Var sMessage : String) : Boolean;
    Function CheckOkForNoParagraphs : Boolean;

    // external redirectors
    Function Capabilities : TWPCapabilities; Overload; Virtual;
    Procedure Undo; Overload; Virtual;
    Procedure Redo; Overload; Virtual;
    Procedure Cut; Overload; Virtual;
    Procedure Copy; Overload; Virtual;
    Procedure CopyAllToClipboard; Overload; Virtual;
    Function Paste(Var sError : String) : Boolean; Overload; Virtual;
    Procedure ApplyStyle(Const sName : String); Overload; Virtual;
    Procedure ApplyStyleToLine(Const sName : String); Overload; Virtual;
    Procedure Insert(Const sText : String);

    // not for public use
    Function ProduceRange : TWPVisualRange;
    Procedure ConsumeRange(oRange : TWPRange);
    Procedure UpdateTextMetrics; Overload; Virtual;
    Procedure DeactivateBMPDrop;
    Procedure ShowPopupMenu;
    Procedure ScrollTo(y : Integer);

    // you can change the styles in this list, but it is only consulted when the document is cleared or changed
    Property ConfiguredStyles : TWPStyles Read GetConfiguredStyles Write SetConfiguredStyles;
    Property DocumentHandler : TWPDocumentHandler Read GetDocumentHandler;
    Property Actor : TWPOperator Read GetOperator;
    Property Speller : TWPSpeller Read GetSpeller Write SetSpeller;
    Property SelectionSummary: String Read GetSelectionSummary;
    Property SelectionStart: Integer Read GetSelectionStart;
    Property SelectionEnd: Integer Read GetSelectionEnd;
    Property PlaybackManager : TDragonPlaybackManager Read GetPlaybackManager Write SetPlaybackManager;

    // published solely for internal use. DO NOT USE OUTSIDE WORD PROCESSOR
    Procedure RefreshPopup;
    Procedure ForceRender; // for automated testing only
    Property WorkingStyles : TWPStyles Read GetWorkingStyles Write SetWorkingStyles;
    Property Document : TWPWorkingDocument Read GetDocument;
    Property PrimaryRange : TWPVisualRange Read GetPrimaryRange;
    Property Renderer : TWPScreenRenderer Read GetRenderer;
    Property Selection : TWPSelection Read GetSelection;
    function BindToField(oDocument: TWPWorkingDocument; cursor : integer; oPiece : TWPWorkingDocumentFieldStartPiece) : boolean;
    Procedure ReplaceFieldContent(oDocument: TWPWorkingDocument; cursor : integer; oPiece : TWPWorkingDocumentFieldStartPiece; txt : String);
    function BindToSection(oPiece : TWPWorkingDocumentSectionStartPiece) : boolean;

    Property ReadOnly : Boolean Read GetReadOnly Write SetReadOnly;

    // pagination support
    Property PageLayoutController : TWPPageLayoutController Read GetPageLayoutController Write SetPageLayoutController;
    Property Printer : TFslPrinter Read GetPrinter Write SetPrinter;

    // properties
    Function GetProperties : TWPPropertyList; // returns a list of current properties and their values
    Procedure SetProperty(oProperty : TWPProperty; Const sValue : String);

    Property Modified : Boolean Read FModified Write FModified;
    Property Settings : TWPSettings Read GetSettings Write SetSettings;

    Property WorkingWidthLimit : Integer Read FWorkingWidthLimit Write SetWorkingWidthLimit;

    Property FontsAllowed : TFslStringList Read GetFontsAllowed Write SetFontsAllowed;

    Property OnSelectionChanged : TNotifyEvent Read FOnSelectionChanged Write FOnSelectionChanged;  //physical selection point has changed (status bar shuld update)
    Property OnSettingsChanged : TNotifyEvent Read FOnSettingsChanged write FOnSettingsChanged;
    Property OnContentChanged : TNotifyEvent Read FOnContentChanged Write FOnContentChanged;    //content of document has changed
    Property OnCodeCompletion : TWPCodeCompletionEvent Read FOnCodeCompletion Write FOnCodeCompletion;
    Property OnTemplate : TWPTemplateEvent Read FOnTemplate Write SetOnTemplate;
    Property OnNamedTemplate : TWPNamedTemplateEvent Read FOnNamedTemplate Write FOnNamedTemplate;
    Property OnHotSpot : TWPHotSpotEvent Read FOnHotSpot Write FOnHotSpot;
    Property OnHotSpotHover : TWPHotSpotHoverEvent Read FOnHotSpotHover Write FOnHotSpotHover;
    Property OnDocumentLoaded : TNotifyEvent read FOnDocumentLoaded write FOnDocumentLoaded;

    Property NextControl : TWinControl Read GetNextControl Write FNextControl;
    Property PreviousControl : TWinControl Read GetPreviousControl Write FPreviousControl;
    Property Color;
    Property NonVisibleMode : Boolean Read FNonVisibleMode Write FNonVisibleMode;
    Property OnKeyDown;

    // internal use only...
    Property Inspectors : TWPInspectorList Read FInspectors;
    Property VoiceActive : Boolean Read FVoiceActive;
    Property ScrollPoint : TPoint Read FScrollPoint;
    Property CursorPersist : Boolean Read FCursorPersist;
    Property CursorOutside : Boolean Read FCursorOutside;
    Property Observers : TWPObservers Read FObservers;
    Property DefaultFieldDefinition: TWPFieldDefinitionProvider Read FDefaultFieldDefinition Write SetDefaultFieldDefinition;
    Property ExistingSearch : TWPSearchDetails Read GetExistingSearch Write SetExistingSearch;
    Property ExistingReplace : TWPReplaceDetails Read GetExistingReplace Write SetExistingReplace;
    Property JustDoubleClicked : Boolean Read FJustDoubleClicked;
    Property CursorDetails : TWordProcessCursorDetails Read FCursorDetails;
    Property MouseActor : TWPMouseActor Read FMouseActor;
    Property ResetMouseCursor : Boolean Read FResetMouseCursor;
    Property AltNum : String Read FAltNum;
    Function HasExistingSearch : Boolean;
    Function HasExistingReplace : Boolean;
    Property ImageTool : TImageTool Read GetImageTool Write SetImageTool;
    Function CreateTouchMenu : TUixPanel;
    Procedure WantCloseTouchMenu;

    Procedure StartRecordingMacro;
    Procedure StopRecordingMacro;
    Property Macro : TWPMacro Read FMacro;
    Property OnPageDesign : TNotifyEvent read FOnPageDesign write FOnPageDesign;
    Property OnPrint : TNotifyEvent read FOnPrint write FOnPrint;
    Property OnExport : TNotifyEvent read FOnExport write FOnExport;
    Property OnImport : TNotifyEvent read FOnImport write FOnImport;
    Property OnSave : TNotifyEvent read FOnSave write FOnSave;
    Property OnSaveAs : TNotifyEvent read FOnSaveAs write FOnSaveAs;
    Property OnOpen : TNotifyEvent read FOnOpen write FOnOpen;
    Property OnNew : TNotifyEvent read FOnNew write FOnNew;
    Property OnUpdateField : TWPFieldUpdateEvent read FOnUpdateField write FOnUpdateField;

  End;

  TRect = Windows.TRect;
  TMouseButton = Controls.TMouseButton;

  TWPCompletionItem = wp_types.TWPCompletionItem;
  TWPCompletionItems = wp_types.TWPCompletionItems;
  TWPDocumentEvent = FHIR.WP.Handler.TWPDocumentEvent;
  TWPSaveImageEvent = FHIR.WP.Handler.TWPSaveImageEvent;
  TWPLoadImageEvent = FHIR.WP.Handler.TWPLoadImageEvent;

  TNotifyEvent = Classes.TNotifyEvent;
//  TDragonPlaybackManager = DragonPlaybackManagers.TDragonPlaybackManager;
  TWPLinkedInspector = Class(TWPInspector)
    Private
      FWordProcessor : TWordProcessor;

    Protected
      Procedure WantClose(oSender: TObject); Override;
      Procedure WPChange(oSender : TObject); Virtual;
      Procedure SetWordProcessor(Const Value: TWordProcessor); Virtual;

    Public
      Property WordProcessor : TWordProcessor Read FWordProcessor Write SetWordProcessor;
  End;

  TWPPropertyInspector = Class (TWPLinkedInspector)
    Private
      FDisplay : TWordProcessor;
      FAllProperties : TFslStringObjectMatch;

      Procedure SelectionChanged(oSender : TObject);
      Procedure ProcessList(oBuilder : TWPDocumentBuilder; oProps : TWPPropertyList; oOwner : TWPDocumentTableRow);
      Procedure CommitField(oField: TWPDocumentField; Const sContent: String);
      Procedure SetUp;
    Protected
      Procedure WPChange(oSender : TObject); Override;
      Function DesiredWidth : Integer; Override;
      Function InspectorCaption : String; Override;
    Public
      Procedure Initialise; Override;
      Procedure Finalise; Override;
  End;

  TWPSystemSnapshotWriter = class (TWPSnapshotWriter)
  private
    Procedure ProduceSelection(oSelection : TWPSelection);
    Procedure ProduceOperation(oOperation : TWPOperation);
    Procedure ProduceRange(oRange : TWPRange);
  public
    Procedure ProduceOperator(oOperator : TWPOperator);
    Procedure ProduceRenderer(oRenderer : TWPScreenRenderer);
    Procedure ProduceRanges(oRanges : TWPRangeManager; oPrimary : TWPVisualRange);
    Procedure ProduceSettings(oSettings : TWPSettings);
  end;

Const
  mbLeft = Controls.mbLeft;
  mbRight = Controls.mbRight;
  mbMiddle = Controls.mbMiddle;


Implementation


Uses
  ShellApi, Dialogs, FHIR.WP.Dialogs, FHIR.WP.FieldDefiners;

{ TWPMouseActor }

Constructor TWPMouseActor.Create(oWordProcessor: TWordProcessor);
Begin
  Inherited Create;
  FWordProcessor := oWordProcessor;
End;

Procedure TWPMouseActor.Close(oNewActor: TWPMouseActor);
Begin
End;

Function TWPMouseActor.Cursor(oInfo: TWPMouseInfo; bDoing: Boolean): TCursor;
Begin
  Result := crArrow;
End;

Procedure TWPMouseActor.Down(aShift: TShiftState; iX, iY: Integer; oInfo: TWPMouseInfo; bDouble: Boolean);
Begin
End;

Function TWPMouseActor.Link: TWPMouseActor;
Begin
  Result := TWPMouseActor(Inherited Link);
End;

Procedure TWPMouseActor.Move(iX, iY: Integer; bOk: Boolean; oInfo: TWPMouseInfo);
Begin
End;

Procedure TWPMouseActor.Up(aShift: TShiftState; iX, iY: Integer; bOk: Boolean; oInfo: TWPMouseInfo);
Begin
End;

Function TWPMouseActor.RequiresDown: Boolean;
Begin
  Result := True;
End;

Type
  TWPMouseImageActor = Class (TWPMouseActor)
    Private
      FImage : TWPWorkingDocumentImagePiece;
    Protected
      Procedure Restrict(Var iX, iY : Integer);
      Function LocalX(iX: Integer):Integer;
      Function LocalY(iY: Integer):Integer;
  End;

Function TWPMouseImageActor.LocalX(iX: Integer): Integer;
Begin
  Result := Trunc((iX - FImage.Map.Left) * (FImage.Image.Width / FImage.Width));
End;

Function TWPMouseImageActor.LocalY(iY: Integer): Integer;
Begin
  Result := Trunc((iY - FImage.Map.Top) * (FImage.Image.Height / FImage.Height));
End;

Procedure TWPMouseImageActor.Restrict(Var iX, iY: Integer);
Begin
  iX := Max(Min(iX, FImage.Map.Right), FImage.Map.Left);
  iY := Max(Min(iY, FImage.Map.Bottom), FImage.Map.Top);
End;


Type
  TWPActorDrawLine = Class (TWPMouseImageActor)
    Private
      FMark : TWPDocumentImageAdornment;
      FLocals : TWPCoordinateList;
      Procedure Update;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;


Procedure TWPActorDrawLine.Update;
Var
  points : Array Of TPoint;
  i : Integer;
  oCanvas : TCanvas;
Begin
  oCanvas := FWordProcessor.Canvas;
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  oCanvas.Lock;
  Try
  {$ENDIF}
    oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[FMark.PenStyle];
    oCanvas.Pen.Mode := pmCopy;
    oCanvas.Pen.Color := FMark.PenColour;
    oCanvas.Pen.Width := FMark.PenWidth;
    SetLength(points, FLocals.Count);
    For i := 0 To FLocals.Count - 1 Do
      points[i] := FLocals[i].AsPoint;
    oCanvas.Polyline(points);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    oCanvas.Unlock;
  End;
  {$ENDIF}
End;

Function TWPActorDrawLine.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := WPIconModule.CURSOR_PEN;
End;

Procedure TWPActorDrawLine.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  Restrict(iX, iY);
  If (FMark <> Nil) And (Not FLocals.LastIs(iX, iY)) Then
  Begin
    FMark.Coordinates.Add(LocalX(iX), LocalY(iY));
    FLocals.Add(iX, iY-FWordProcessor.FScrollPoint.Y);
    Update;
  End;
End;

Procedure TWPActorDrawLine.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FOffsetSquareforScroll := True;
  FImage := oInfo.Subject As TWPWorkingDocumentImagePiece;
  FMark := TWPDocumentImageAdornment.Create;
  FMark.AdornmentType := iatLine;
  FMark.PenColour := FImage.DefaultAdornmentColour;
  FMark.Font.Foreground := FImage.DefaultAdornmentColour;
  FMark.PenWidth := 1;
  FMark.PenStyle := apsSolid;
  FMark.Coordinates.Add(LocalX(iX), LocalY(iY));
  FLocals := TWPCoordinateList.Create;
  FLocals.Add(iX, iY-FWordProcessor.FScrollPoint.Y);
  Update;
End;

Procedure TWPActorDrawLine.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If FMark.Coordinates.Count > 1 Then
  Begin
    FWordProcessor.PrimaryRange.ApplyImageAdornment(FImage, FMark);
    FWordProcessor.ImageTool := itSelect;
  End;
  FLocals.Free;
  FMark.Free;
  FMark := Nil;
  FImage := Nil;
End;

Type
  TWPActorDrawRectangle = Class (TWPMouseImageActor)
    Private
      FCircle : Boolean;
      FMark : TWPDocumentImageAdornment;
      FStart : TPoint;
      FEnd : TPoint;
      FStartLocal : TPoint;
      FEndLocal : TPoint;
      Procedure Update;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;


Procedure TWPActorDrawRectangle.Update;
Var
  oCanvas : TCanvas;
Begin
  oCanvas := FWordProcessor.Canvas;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  oCanvas.Lock;
  Try
  {$ENDIF}
    oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[FMark.PenStyle];
    oCanvas.Pen.Mode := pmNot;
    oCanvas.Pen.Color := FMark.PenColour;
    oCanvas.Pen.Width := FMark.PenWidth;
    oCanvas.Brush.Style := bsClear;
    If FCircle Then
      oCanvas.Ellipse(Fstart.x, FStart.y, FEnd.X, FEnd.y)
    Else
      oCanvas.Rectangle(Fstart.x, FStart.y, FEnd.X, FEnd.y);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    oCanvas.Unlock;
  End;
  {$ENDIF}
End;

Function TWPActorDrawRectangle.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  If FCircle Then
    Result := WPIconModule.CURSOR_CIRCLE
  Else
    Result := WPIconModule.CURSOR_BOX;
End;

Procedure TWPActorDrawRectangle.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  Restrict(iX, iY);
  If (FMark <> Nil) Then
  Begin
    Update;
    FEnd.X := iX;
    FEnd.Y := iY-FWordProcessor.FScrollPoint.Y;
    FEndLocal.X := LocalX(iX);
    FEndLocal.Y := LocalY(iY);
    Update;
  End;
End;

Procedure TWPActorDrawRectangle.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FImage := oInfo.Subject As TWPWorkingDocumentImagePiece;
  Restrict(iX, iY);
  FMark := TWPDocumentImageAdornment.Create;
  If FCircle Then
    FMark.AdornmentType := iatCircle
  Else
    FMark.AdornmentType := iatRectangle;
  FMark.PenColour := FImage.DefaultAdornmentColour;
  FMark.Font.Foreground := FImage.DefaultAdornmentColour;
  FMark.PenWidth := 1;
  FMark.PenStyle := apsDot;
  FStart.X := iX;
  FStart.Y := iY-FWordProcessor.FScrollPoint.Y;
  FStartLocal.X := LocalX(iX);
  FStartLocal.Y := LocalY(iY);
  FEnd := FStart;
  Update;
End;

Procedure TWPActorDrawRectangle.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If (FStart.x <> FEnd.x) Or (FStart.y <> FEnd.y) Then
  Begin
    FMark.Coordinates.Add(Min(FStartLocal.x, FEndLocal.x), Min(FStartLocal.y, FEndLocal.y));
    FMark.Coordinates.Add(Max(FStartLocal.x, FEndLocal.x), Max(FStartLocal.y, FEndLocal.y));
    FWordProcessor.PrimaryRange.ApplyImageAdornment(FImage, FMark);
    FWordProcessor.ImageTool := itSelect;
  End;
  FMark.Free;
  FMark := Nil;
  FImage := Nil;
End;

Type
  TWPActorDrawMark = Class (TWPMouseImageActor)
    Private
      FMark : TWPDocumentImageAdornment;
      FStart : TPoint;
      FEnd : TPoint;
      FStartLocal : TPoint;
      FEndLocal : TPoint;
      Procedure Update;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;


Procedure TWPActorDrawMark.Update;
Var
  oCanvas : TCanvas;
Begin
  oCanvas := FWordProcessor.Canvas;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  oCanvas.Lock;
  Try
  {$ENDIF}
    oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[FMark.PenStyle];
    oCanvas.Pen.Mode := pmNot;
    oCanvas.Pen.Color := FMark.PenColour;
    oCanvas.Pen.Width := FMark.PenWidth;
    oCanvas.Brush.Style := bsClear;
    oCanvas.MoveTo(FStart.X, FStart.Y);
    oCanvas.LineTo(FEnd.X, FEnd.Y);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    oCanvas.Unlock;
  End;
  {$ENDIF}
End;

Function TWPActorDrawMark.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := WPIconModule.CURSOR_MARK;
End;

Procedure TWPActorDrawMark.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  Restrict(iX, iY);
  If (FMark <> Nil) Then
  Begin
    Update;
    FEnd.X := iX;
    FEnd.Y := iY-FWordProcessor.FScrollPoint.Y;
    FEndLocal.X := LocalX(iX);
    FEndLocal.Y := LocalY(iY);
    Update;
  End;
End;

Procedure TWPActorDrawMark.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FImage := oInfo.Subject As TWPWorkingDocumentImagePiece;
  FMark := TWPDocumentImageAdornment.Create;
  FMark.AdornmentType := iatMark;
  FMark.PenColour := FImage.DefaultAdornmentColour;
  FMark.Font.Foreground := FImage.DefaultAdornmentColour;
  FMark.PenWidth := 1;
  FMark.PenStyle := apsDash;
  FStart.X := iX;
  FStart.Y := iY-FWordProcessor.FScrollPoint.Y;
  FStartLocal.X := LocalX(iX);
  FStartLocal.Y := LocalY(iY);
  FEnd := FStart;
  Update;
End;

Procedure TWPActorDrawMark.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Var
  oDialog : TWPAdornmentDialog;
  oImage : TWPWorkingDocumentImagePiece;
Begin
  If (FStart.x <> FEnd.x) Or (FStart.y <> FEnd.y) Then
  Begin
    Update;
    FMark.Coordinates.Add(FStartLocal.x, FStartLocal.y);
    FMark.CaptionPoint.X := FEndLocal.x;
    FMark.CaptionPoint.Y := FEndLocal.Y;
    oDialog := TWPAdornmentDialog.Create(FWordProcessor);
    Try
      // uh, self will likely disappear underneath us....
      oDialog.Adornment := FMark.Clone;
      FMark.Free;
      FMark := Nil;
      oImage := FImage;
      FImage := Nil;
      oDialog.FontsAllowed := FWordProcessor.FFontsAllowed.Link;
      If oDialog.Execute Then
      Begin
        FWordProcessor.PrimaryRange.ApplyImageAdornment(oImage, oDialog.Adornment);
        FWordProcessor.ImageTool := itSelect;
      End;
    Finally
      oDialog.Free;
    End;
  End;
End;

Type
  TWPActorDrawZoom = Class (TWPMouseImageActor)
    Private
      FCircle : Boolean;
      FMark : TWPDocumentImageAdornment;
      FStart : TPoint;
      FEnd : TPoint;
      FStartLocal : TPoint;
      FEndLocal : TPoint;
      Procedure Update;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;


Procedure TWPActorDrawZoom.Update;
Var
  oCanvas : TCanvas;
Begin
  oCanvas := FWordProcessor.Canvas;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  oCanvas.Lock;
  Try
  {$ENDIF}
    oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[FMark.PenStyle];
    oCanvas.Pen.Mode := pmNot;
    oCanvas.Pen.Color := FMark.PenColour;
    oCanvas.Pen.Width := FMark.PenWidth;
    oCanvas.Brush.Style := bsClear;
    If FCircle Then
      oCanvas.Ellipse(Fstart.x, FStart.y, FEnd.X, FEnd.y)
    Else
      oCanvas.Rectangle(Fstart.x, FStart.y, FEnd.X, FEnd.y);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    oCanvas.Unlock;
  End;
  {$ENDIF}
End;

Function TWPActorDrawZoom.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := WPIconModule.CURSOR_ZOOM;
End;

Procedure TWPActorDrawZoom.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  Restrict(iX, iY);
  If (FMark <> Nil) Then
  Begin
    Update;
    FEnd.X := iX;
    FEnd.Y := iY-FWordProcessor.FScrollPoint.Y;
    FEndLocal.X := LocalX(iX);
    FEndLocal.Y := LocalY(iY);
    Update;
  End;
End;

Procedure TWPActorDrawZoom.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FImage := oInfo.Subject As TWPWorkingDocumentImagePiece;
  Restrict(iX, iY);
  FMark := TWPDocumentImageAdornment.Create;
  FMark.AdornmentType := iatZoom;
  FMark.PenColour := FImage.DefaultAdornmentColour;
  FMark.Font.Foreground := FImage.DefaultAdornmentColour;
  FMark.PenWidth := 1;
  FMark.PenStyle := apsDot;
  FStart.X := iX;
  FStart.Y := iY-FWordProcessor.FScrollPoint.Y;
  FStartLocal.X := LocalX(iX);
  FStartLocal.Y := LocalY(iY);
  FEnd := FStart;
  Update;
End;

Function FitZoom(Const rImage, rFocus : TRect; iScale : Integer; Var rZoom : TRect) : Boolean;
Var
  w,h : Integer;
Begin
  w := (rFocus.Right - rFocus.Left) * iScale;
  h := (rFocus.Bottom - rFocus.Top) * iScale;
  // can we fit it on a diagonal?
  // right, then left, bottom, then top
  Result := True;
  If (rFocus.Right + w + 40 < rImage.Right) And (rFocus.Bottom + h + 40 < rImage.Bottom) Then
  Begin
    rZoom.Left := rFocus.Right + 40;
    rZoom.Top := rFocus.Bottom + 40;
  End
  Else If (rFocus.Left - (w + 40) > 0) And (rFocus.Bottom + h + 40 < rImage.Bottom) Then
  Begin
    rZoom.Left := rFocus.Left - (w + 40);
    rZoom.Top := rFocus.Bottom + 40;
  End
  Else If (rFocus.Right + w + 40 < rImage.Right) And (rFocus.Top - (h + 40) > 0) Then
  Begin
    rZoom.Left := rFocus.Right + 40;
    rZoom.Top := rFocus.Top - (h + 40);
  End
  Else If (rFocus.Left - (w + 40) > 0) And (rFocus.Top - (h + 40) > 0) Then
  Begin
    rZoom.Left := rFocus.Left - (w + 40);
    rZoom.Top := rFocus.Top - (h + 40);
  End
  Else If (rFocus.Bottom + h + 20 < rImage.Bottom) And (w < rImage.Right) Then
  Begin
    rZoom.Left := (rImage.Right Div 2) - (w Div 2);
    rZoom.Top := rFocus.Bottom + 20;
  End
  Else If (rFocus.Top - (h + 20) > 0) And (w < rImage.Right) Then
  Begin
    rZoom.Left := (rImage.Right Div 2) - (w Div 2);
    rZoom.Top := rFocus.Top - (h + 20);
  End
  Else If (rFocus.Right + w + 20 < rImage.Right) And (h < rImage.Bottom) Then
  Begin
    rZoom.Left := rFocus.Right + 20;
    rZoom.Top := (rImage.Bottom Div 2) - (h Div 2);
  End
  Else If (rFocus.Left - (w + 20) > 0) And (h < rImage.Bottom) Then
  Begin
    rZoom.Left := rFocus.Left - (w + 20);
    rZoom.Top := (rImage.Bottom Div 2) - (h Div 2);
  End
  Else
    Result := False;
  rZoom.Right := rZoom.Left + w;
  rZoom.Bottom := rZoom.Top + h;
  If Result Then
  Begin
  Assert(rZoom.Left >= 0);
  Assert(rZoom.Top >= 0);
  Assert(rZoom.Bottom <= rImage.Bottom);
  Assert(rZoom.right <= rImage.Right);
  End;
End;

Procedure TWPActorDrawZoom.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Var
  rImage, rFocus, rZoom : TRect;
Begin
  Update;
  If (Abs(FStart.x - FEnd.x) > 10) And (Abs(FStart.y - FEnd.y) > 10) Then
  Begin
    rImage.Top := 0;
    rImage.Left := 0;
    rImage.Right := FImage.Image.Width;
    rImage.Bottom := FImage.Image.Height;

    rFocus.Top := Min(FStartLocal.y, FEndLocal.y);
    rFocus.Left := Min(FStartLocal.x, FEndLocal.x);
    rFocus.Bottom := Max(FStartLocal.y, FEndLocal.y);
    rFocus.Right := Max(FStartLocal.x, FEndLocal.x);

    // what we have to do is to determine where the expanded area is going to be.
    If Not FitZoom(rImage, rFocus, 8, rZoom) And
       Not FitZoom(rImage, rFocus, 4, rZoom) And
       Not FitZoom(rImage, rFocus, 2, rZoom) Then
      MessageDlg('Unable to zoom such a large area. Choose a smaller area', mterror, [mbok], 0)
    Else
    Begin
      FMark.Coordinates.Add(rFocus.Left, rFocus.Top);
      FMark.Coordinates.Add(rFocus.Right, rFocus.Bottom);
      FMark.Coordinates.Add(rZoom.Left, rZoom.Top);
      FMark.Coordinates.Add(rZoom.Right, rZoom.Bottom);
      FWordProcessor.PrimaryRange.ApplyImageAdornment(FImage, FMark);
      FWordProcessor.ImageTool := itSelect;
    End;
  End;
  FMark.Free;
  FMark := Nil;
  FImage := Nil;
End;


Type
  TWPActorNull = Class (TWPMouseImageActor)
    Private
    Public
  End;

Type
  TWPActorSelect = Class (TWPMouseImageActor)
    Private
      FDouble : Boolean;
      FAdornment : TWPDocumentImageAdornment;
      FAdornmentPart : TAdornmentPart;
      FAdornmentAction : TAdornmentAction;
      FStartX : Integer;
      FStartY : Integer;
      FCurrentX : Integer;
      FCurrentY : Integer;
      FMinX : Integer;
      FMaxX : Integer;
      FMinY : Integer;
      FMaxY : Integer;
      FPart : TRect;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Procedure TWPActorSelect.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  If (Not FWordProcessor.NoUserResponse) Then
    If oInfo.Adornment <> Nil Then
    Begin
      FOffsetSquareforScroll := True;
      FDouble := bDouble;
      FImage := TWPWorkingDocumentImagePiece(oInfo.Subject);
      FStartX := iX;
      FStartY := iY;
      FCurrentX := FStartX;
      FCurrentY := FStartY;
      FAdornment := oInfo.Adornment;
      FAdornmentPart := oInfo.AdornmentPart;
      FAdornmentAction := oInfo.AdornmentAction;
      FPart := FImage.GetExtant(FAdornment, FAdornmentPart, FAdornmentAction);

      // scale extant for image scaling
      FPart.Left := Trunc(FPart.Left * FImage.Width / FImage.Image.Width);
      FPart.Right := Trunc(FPart.Right * FImage.Width / FImage.Image.Width);
      FPart.Top := Trunc(FPart.Top * FImage.Height / FImage.Image.Height);
      FPart.Bottom := Trunc(FPart.Bottom * FImage.Height / FImage.Image.Height);

      FMinX := FCurrentX - FPart.Left;
      FMinY := FCurrentY - FPart.Top;
      FMaxX := FCurrentX + FImage.Width - FPart.Right - 1;
      FMaxY := FCurrentY + FImage.Height - FPart.Bottom - 1;
      // that's the absolute limits. There's also a limit when resizing - the adornment is not allowed to get smaller than 20*20
      // todo: is that scaled pixels? Can an adornment get to small to handle?
      Case FAdornmentAction Of
        aaDragTop :
            FMaxY := FImage.Map.Top + FPart.Bottom - 20;
        aaDragBottom :
            FMinY := FImage.Map.Top + FPart.Top + 20;
        aaDragLeft :
            FMaxX := FImage.Map.Left + FPart.Right - 20;
        aaDragRight :
            FMinX := FImage.Map.Left + FPart.Left + 20;
        aaDragTopLeft :
          Begin
            FMaxX := FImage.Map.Left + FPart.Right - 20;
            FMaxY := FImage.Map.Top + FPart.Bottom - 20;
          End;
        aaDragTopRight :
          Begin
            FMinX := FImage.Map.Left + FPart.Left + 20;
            FMaxY := FImage.Map.Top + FPart.Bottom - 20;
          End;
        aaDragBottomLeft :
          Begin
            FMaxX := FImage.Map.Left + FPart.Right - 20;
            FMinY := FImage.Map.Top + FPart.Top + 20;
          End;
        aaDragBottomRight :
          Begin
            FMinX := FImage.Map.Left + FPart.Left + 20;
            FMinY := FImage.Map.Top + FPart.Top + 20;
          End;
      End;

      FPart.Top := FPart.Top + (iY - Trunc(LocalY(iY) * FImage.Height / FImage.Image.Height));
      FPart.Bottom := FPart.Bottom + (iY - Trunc(LocalY(iY) * FImage.Height / FImage.Image.Height));
      FPart.Left := FPart.Left + (iX - Trunc(LocalX(iX) * FImage.Width / FImage.Image.Width));
      FPart.Right := FPart.Right + (iX - Trunc(LocalX(iX) * FImage.Width / FImage.Image.Width));

      Square(FPart.Left, FPart.Top, FPart.Right, FPart.Bottom);
    End
    Else If bDouble Then
    Begin
      FWordProcessor.PrimaryRange.SelectWord();
      FDouble := True;
    End
    Else
    Begin
      FWordProcessor.PrimaryRange.MoveTo(oInfo.Offset);
    End;
End;

Function TWPActorSelect.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
  Function CursorForAction(aAction : TAdornmentAction) : TCursor;
  Begin
    Case aAction Of
      aaMove : Result := crHandPoint;
      aaDragTop : Result := crSizeNS;
      aaDragBottom : Result := crSizeNS;
      aaDragLeft : Result := crSizeWE;
      aaDragRight : Result := crSizeWE;
      aaDragTopLeft : Result := crSizeNWSE;
      aaDragTopRight : Result := crSizeNESW;
      aaDragBottomLeft : Result := crSizeNESW;
      aaDragBottomRight : Result := crSizeNWSE;
    Else
      Result := crDefault;
    End;
  End;
Begin
  If (FAdornment <> Nil) Then
    Result := CursorForAction(FAdornmentAction)
  Else If (oInfo.Adornment <> Nil) Then
    Result := CursorForAction(oInfo.AdornmentAction)
  Else
    Result := crIBeam;
End;

Procedure TWPActorSelect.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If bOk And Not FDouble And Not FWordProcessor.NoUserResponse and ((FAdornment = Nil) or not FWordProcessor.Settings.ReadOnly) Then
  Begin
    If FAdornment <> Nil Then
    Begin
      Square(FLastLeft, FLastTop, FLastRight, FLastBottom);
      FCurrentX := IntegerMin(IntegerMax(iX, FMinX), FMaxX);
      FCurrentY := IntegerMin(IntegerMax(iY, FMinY), FMaxY);
      Case FAdornmentAction Of
        aaMove : Square(FPart.Left + (FCurrentX - FStartX), FPart.Top + (FCurrentY - FStartY), FPart.Right + (FCurrentX - FStartX), FPart.Bottom + (FCurrentY - FStartY));
        aaDragTop : Square(FPart.Left, FPart.Top + (FCurrentY - FStartY), FPart.Right, FPart.Bottom);
        aaDragBottom : Square(FPart.Left, FPart.Top, FPart.Right, FPart.Bottom + (FCurrentY - FStartY));
        aaDragLeft : Square(FPart.Left + (FCurrentX - FStartX), FPart.Top, FPart.Right, FPart.Bottom);
        aaDragRight : Square(FPart.Left, FPart.Top, FPart.Right + (FCurrentX - FStartX), FPart.Bottom);
        aaDragTopLeft : Square(FPart.Left + (FCurrentX - FStartX), FPart.Top + (FCurrentY - FStartY), FPart.Right, FPart.Bottom);
        aaDragTopRight : Square(FPart.Left, FPart.Top + (FCurrentY - FStartY), FPart.Right + (FCurrentX - FStartX), FPart.Bottom);
        aaDragBottomLeft : Square(FPart.Left + (FCurrentX - FStartX), FPart.Top, FPart.Right, FPart.Bottom + (FCurrentY - FStartY));
        aaDragBottomRight : Square(FPart.Left, FPart.Top, FPart.Right + (FCurrentX - FStartX), FPart.Bottom + (FCurrentY - FStartY));
      End;
    End
    Else
      FWordProcessor.PrimaryRange.SelectTo(oInfo.Offset);
  End;
End;

Procedure TWPActorSelect.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Var
  oDialog : TWPAdornmentDialog;
  bReadOnly : Boolean;
Begin
  If FAdornment <> Nil Then
  Begin
    Square(FLastLeft, FLastTop, FLastRight, FLastBottom);
    bReadOnly := FWordProcessor.Settings.ReadOnly or
      not (oInfo.Subject Is TWPWorkingDocumentPiece) or
      TWPWorkingDocumentPiece(oInfo.Subject).IsReadOnly;
    If FDouble and not bReadOnly Then
    Begin
      oDialog := TWPAdornmentDialog.Create(FWordProcessor);
      Try
        oDialog.Adornment := oInfo.Adornment.Clone;
        oDialog.FontsAllowed := FWordProcessor.FFontsAllowed.Link;
        If oDialog.Execute Then
          FWordProcessor.PrimaryRange.ApplyImageAdornment(TWPWorkingDocumentImagePiece(oInfo.Subject), oDialog.Adornment);
      Finally
        oDialog.Free;
      End;
    End
    Else If (FCurrentX - FStartX <> 0) Or (FCurrentY - FStartY <> 0) Then
      FWordProcessor.PrimaryRange.MoveImageAdornment(FImage, FAdornment, FAdornmentPart, FAdornmentAction, Trunc((FCurrentX - FStartX) * (FImage.Image.Width / FImage.Width)), Trunc((FCurrentY - FStartY) * (FImage.Image.Height / FImage.Height)));
    FAdornment := Nil;
    FImage := Nil;
  End;
End;

Type
  TWPActorDragSelection = Class (TWPMouseActor)
    Private
      Start : boolean;
      XOffset, YOffset : integer;
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Procedure TWPActorDragSelection.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  if Start then
    FWordProcessor.FCursorDetails.FSelHot := shStart
  else
    FWordProcessor.FCursorDetails.FSelHot := shEnd;
End;

Function TWPActorDragSelection.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := crSizeWE;
End;

Procedure TWPActorDragSelection.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
var
  offs : integer;
Begin
  if Start then
  begin
    inc(iX, XOffset);
    inc(iY, YOffset);
    FWordProcessor.Renderer.getMouseInfoForPoint(iX, iY, oInfo);
    offs := Max(oInfo.Offset, 0);
    if offs < FWordProcessor.PrimaryRange.Selection.SelEnd then
    begin
      FWordProcessor.PrimaryRange.MoveTo(FWordProcessor.PrimaryRange.Selection.SelEnd, false);
      FWordProcessor.PrimaryRange.SelectTo(offs, true);
    end;
  end
  else
  begin
    dec(iX, XOffset);
    dec(iY, YOffset);
    FWordProcessor.Renderer.getMouseInfoForPoint(iX, iY, oInfo);
    offs := Min(oInfo.Offset, FWordProcessor.Document.CharCount);
    if offs > FWordProcessor.PrimaryRange.Selection.SelStart then
    begin
      FWordProcessor.PrimaryRange.MoveTo(FWordProcessor.PrimaryRange.Selection.SelStart, false);
      FWordProcessor.PrimaryRange.SelectTo(offs, true);
    end;
  end;
End;

Procedure TWPActorDragSelection.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  FWordProcessor.FCursorDetails.FSelHot := shNeither;
End;


Type
  TWPActorDragBlock = Class (TWPMouseActor)
    Public
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Procedure TWPActorDragBlock.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  // nothing
End;

Function TWPActorDragBlock.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  If bDoing Then
    Result := crDrag
  Else
    Result := crArrow;
End;


Procedure TWPActorDragBlock.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  FWordProcessor.ClearDragMark;
  If Not bOk Or FWordProcessor.PrimaryRange.Selection.InSelection(oInfo.Offset) Or FWordProcessor.NoUserResponse Then
    FWordProcessor.ApplyCursorState(crNoDrop, True)
  Else
  Begin
    FWordProcessor.ApplyCursorState(crDrag, True);
    FWordProcessor.DrawDragMark(oInfo.Offset);
  End;
End;

Procedure TWPActorDragBlock.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  FWordProcessor.ClearDragMark;
  If bOk Then
  Begin
    If FWordProcessor.PrimaryRange.Selection.InSelection(oInfo.Offset) Then
      FWordProcessor.PrimaryRange.MoveTo(oInfo.Offset)
    Else
      FWordProcessor.PrimaryRange.MoveSelection(oInfo.Offset);
  End;
End;


Type
  TWPActorDragEdge = Class (TWPMouseActor)
    Private
      FButton : TFslObject;
      FEdgeAction : TEdgeAction;
      iStartingLeft : Integer;
      iStartingTop : Integer;
      iStartingBottom : Integer;
      iStartingRight : Integer;
      Function WorkingLeft(iX, iY : Integer):Integer;
      Function WorkingRight(iX, iY : Integer):Integer;
      Function WorkingTop(iX, iY : Integer):Integer;
      Function WorkingBottom(iX, iY : Integer):Integer;
    Public
      destructor Destroy; Override;
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Destructor TWPActorDragEdge.Destroy;
Begin
  FButton.Free;
  Inherited;
End;

Procedure TWPMouseActor.Square(iLeft, iTop, iRight, iBottom : Integer);
Var
  iYOffs : Integer;
Begin
  If FOffsetSquareforScroll Then
    iYOffs := FWordProcessor.FScrollPoint.Y
  Else
    iYOffs := 0;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  FWordProcessor.Canvas.Lock;
  Try
  {$ENDIF}
    FWordProcessor.Canvas.Pen.Style := psSolid;
    FWordProcessor.Canvas.Pen.Mode := pmNot;
    FWordProcessor.Canvas.Pen.Color := clNavy;
    FWordProcessor.Canvas.MoveTo(iLeft, iTop - iYOffs);
    FWordProcessor.Canvas.LineTo(iRight, iTop - iYOffs);
    FWordProcessor.Canvas.LineTo(iRight, iBottom - iYOffs);
    FWordProcessor.Canvas.LineTo(iLeft, iBottom - iYOffs);
    FWordProcessor.Canvas.LineTo(iLeft, iTop - iYOffs);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    FWordProcessor.Canvas.Unlock;
  End;
  {$ENDIF}

  FLastTop := iTop;
  FLastBottom := iBottom;
  FLastLeft := iLeft;
  FLastRight := iRight;
End;

Function TWPActorDragEdge.WorkingLeft(iX, iY : Integer):Integer;
var
  ratio : double;
Begin
  If (FEdgeAction = eaLeft) Then
    Result := IntegerMin(iX, iStartingRight - MIN_IMAGE_DRAG)
  Else If (FEdgeAction = eaLeftTop) then
  begin
    // the working left is the average of the x/y changes in terms of the existing ratio of the image
    ratio := (((iX - iStartingLeft) / (iStartingRight - iStartingLeft)) +
              ((iY - iStartingTop) / (iStartingBottom - iStartingTop))) / 2;
    result := IntegerMin(iStartingLeft + trunc((iStartingRight - iStartingLeft) * ratio), iStartingRight - MIN_IMAGE_DRAG);
  end
  else If (FEdgeAction = eaLeftBottom) then
  begin
    ratio := (((iX - iStartingLeft) / (iStartingRight - iStartingLeft)) +
              ((iY - iStartingBottom) / (iStartingTop - iStartingBottom))) / 2;
    result := IntegerMin(iStartingLeft + trunc((iStartingRight - iStartingLeft) * ratio), iStartingRight - MIN_IMAGE_DRAG);
  end
  else
    Result := iStartingLeft;
End;

Function TWPActorDragEdge.WorkingRight(iX, iY : Integer):Integer;
var
  ratio : double;
Begin
  If (FEdgeAction In [eaRight]) Then
    Result := IntegerMax(iX, iStartingLeft + MIN_IMAGE_DRAG)
  else if FEdgeAction = eaRightBottom then
  begin
    ratio := (((iX - iStartingRight) / (iStartingLeft - iStartingRight)) +
              ((iY - iStartingBottom) / (iStartingTop - iStartingBottom))) / 2;
    result := IntegerMax(iStartingRight - trunc((iStartingRight - iStartingLeft) * ratio), iStartingLeft + MIN_IMAGE_DRAG);
  end
  Else if FEdgeAction = eaRightTop then
  begin
    ratio := (((iX - iStartingRight) / (iStartingLeft - iStartingRight)) +
              ((iY - iStartingTop) / (iStartingBottom - iStartingTop))) / 2;
    result := IntegerMax(iStartingRight - trunc((iStartingRight - iStartingLeft) * ratio), iStartingLeft + MIN_IMAGE_DRAG);
  end
  Else
    Result := iStartingRight;
End;

Function TWPActorDragEdge.WorkingTop(iX, iY : Integer):Integer;
var
  ratio : double;
Begin
  If (FEdgeAction In [eaTop]) Then
    Result := IntegerMin(iY, iStartingBottom - MIN_IMAGE_DRAG)
  Else if FEdgeAction = eaRightTop then
  begin
    ratio := (((iX - iStartingRight) / (iStartingLeft - iStartingRight)) +
              ((iY - iStartingTop) / (iStartingBottom - iStartingTop))) / 2;
    result := IntegerMin(iStartingTop + trunc((iStartingBottom - iStartingTop) * ratio), iStartingBottom - MIN_IMAGE_DRAG);
  end
  Else if FEdgeAction = eaLeftTop then
  begin
    ratio := (((iX - iStartingLeft) / (iStartingRight - iStartingLeft)) +
              ((iY - iStartingTop) / (iStartingBottom - iStartingTop))) / 2;
    result := IntegerMin(iStartingTop + trunc((iStartingBottom - iStartingTop) * ratio), iStartingBottom - MIN_IMAGE_DRAG);
  end
  Else
    Result := iStartingTop;
End;

Function TWPActorDragEdge.WorkingBottom(iX, iY : Integer):Integer;
var
  ratio : double;
Begin
  If (FEdgeAction In [eaBottom]) Then
    Result := IntegerMax(iY, iStartingTop + MIN_IMAGE_DRAG)
  Else If (FEdgeAction = eaRightBottom) then
  begin
    ratio := (((iX - iStartingRight) / (iStartingLeft - iStartingRight)) +
              ((iY - iStartingBottom) / (iStartingTop - iStartingBottom))) / 2;
    result := IntegerMax(iStartingbottom - trunc((iStartingBottom - iStartingTop) * ratio), iStartingTop + MIN_IMAGE_DRAG);
  end
  Else If (FEdgeAction = eaLeftBottom) then
  begin
    ratio := (((iX - iStartingLeft) / (iStartingRight - iStartingLeft)) +
              ((iY - iStartingBottom) / (iStartingTop - iStartingBottom))) / 2;
    result := IntegerMax(iStartingbottom - trunc((iStartingBottom - iStartingTop) * ratio), iStartingTop + MIN_IMAGE_DRAG);
  end
  Else
    Result := iStartingBottom;
End;

Procedure TWPActorDragEdge.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FButton := oInfo.Subject.Link;
  FEdgeAction := oInfo.EdgeAction;
  If FButton Is TWPWorkingDocumentImagePiece Then
  Begin
    iStartingLeft := TWPWorkingDocumentImagePiece(FButton).Map.Left;
    iStartingTop := TWPWorkingDocumentImagePiece(FButton).Map.Top;
    iStartingRight := TWPWorkingDocumentImagePiece(FButton).Map.Right;
    iStartingBottom := TWPWorkingDocumentImagePiece(FButton).Map.Bottom;
  End
  Else if FButton is TWPWorkingAnnotationList Then
  Begin
    iStartingLeft := FWordProcessor.Left + FWordProcessor.Width - FWordProcessor.Renderer.SpaceForAnnotations;
    iStartingTop := FWordProcessor.Top;
    iStartingRight := FWordProcessor.Left + FWordProcessor.Width;
    iStartingBottom := FWordProcessor.Top+ + FWordProcessor.Height;
  End;
  Square(WorkingLeft(iX, iY), WorkingTop(iX, iY), WorkingRight(iX, iY), WorkingBottom(iX, iY));
End;

Function TWPActorDragEdge.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Case oInfo.EdgeAction Of
    eaLeft: Result := crSizeWE;
    eaRight: Result := crSizeWE;
    eaTop: Result := crSizeNS;
    eaBottom: Result := crSizeNS;
    eaLeftTop: Result := crSizeNWSE;
    eaRightTop: Result := crSizeNESW;
    eaLeftBottom: Result := crSizeNESW;
    eaRightBottom: Result := crSizeNWSE;
  Else
    Result := crNoDrop;
  End;
End;

Procedure TWPActorDragEdge.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  Square(FLastLeft, FLastTop, FLastRight, FLastBottom);
  Square(WorkingLeft(iX, iY), WorkingTop(iX, iY), WorkingRight(iX, iY), WorkingBottom(iX, iY));
End;

Procedure TWPActorDragEdge.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Var
  oImage : TWPWorkingDocumentImagePiece;
Begin
  Square(FLastLeft, FLastTop, FLastRight, FLastBottom);
  If FButton Is TWPWorkingDocumentImagePiece Then
  Begin
    FWordProcessor.PrimaryRange.MoveTo(TWPWorkingDocumentImagePiece(FButton).Metrics.Position, false);
    FWordProcessor.PrimaryRange.SelectTo(TWPWorkingDocumentImagePiece(FButton).Metrics.Position+1, false);
    oImage := TWPWorkingDocumentImagePiece(FButton.Clone);
    Try
      oImage.Width := WorkingRight(iX, iY) - WorkingLeft(iX, iY);
      oImage.Height := WorkingBottom(iX, iY) - WorkingTop(iX, iY);
      FWordProcessor.PrimaryRange.SetImageProperties(oImage);
    Finally
      oImage.Free;
    End;
  End
  Else if FButton is TWPWorkingAnnotationList Then
  Begin
    FWordProcessor.Settings.AnnotationWidth := FWordProcessor.Width - iX;
  End;
End;

Type
  TWPActorButton = Class (TWPMouseActor)
    Private
      FButton : TFslObject;
      FDouble : Boolean;
    Public
      destructor Destroy; Override;
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Destructor TWPActorButton.Destroy;
Begin
  FButton.Free;
  Inherited;
End;

Procedure TWPActorButton.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FButton := oInfo.Subject.Link;
  FDouble := bDouble;
  If (FWordProcessor.Renderer.CurrentButton <> oInfo.Subject) Then
  Begin
    FWordProcessor.Renderer.CurrentButton := oInfo.Subject.Link;
    FWordProcessor.DoPaint;
  End;
End;

Function TWPActorButton.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := crArrow;
End;

Procedure TWPActorButton.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If (oInfo.Subject <> FButton) Or FDouble Or FWordProcessor.NoUserResponse and not FWordProcessor.Settings.ReadOnly Then
    FWordProcessor.ApplyCursorState(crNoDrop, True)
  Else
    FWordProcessor.ApplyCursorState(crArrow, True);
End;

Procedure TWPActorButton.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  // we armour against everything disappearing - like if the document is reloaded under us in response
  Link;
  Try
    If (oInfo.Subject = FButton) And Not FDouble Then
      FWordProcessor.DoButton(FWordProcessor.FRenderer.GetScreenRectForOffset(oInfo.Offset), oInfo.Subject, oInfo.Offset);
    If Assigned(FWordProcessor) Then
    Begin
      FWordProcessor.Renderer.CurrentButton := Nil;
      FWordProcessor.DoPaint;
    End;
  Finally
    Unlink;
  End;
End;


Type
  TWPActorHotspot = Class (TWPMouseActor)
    Private
      FHotspot : TWPHotspot;
      FSubject : TFslObject;
      FDouble : Boolean;
    Public
      destructor Destroy; Override;
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
  End;

Destructor TWPActorHotspot.Destroy;
Begin
  If Assigned(FWordProcessor) And (FWordProcessor.Renderer.CurrentHotspot <> Nil) Then
    FWordProcessor.Renderer.CurrentHotspot := Nil;
  FHotspot.Free;
  FSubject.Free;
  Inherited;
End;

Procedure TWPActorHotspot.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FHotspot := oInfo.Hotspot.Link;
  FSubject := oInfo.Subject.Link;
  FDouble := bDouble;
  If (FWordProcessor.Renderer.CurrentHotspot <> oInfo.Hotspot) Then
  Begin
    FWordProcessor.Renderer.CurrentHotspot := oInfo.Hotspot.Link;
    FWordProcessor.DoPaint;
  End;
End;

Function TWPActorHotspot.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := crHandPoint;
End;

Procedure TWPActorHotspot.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If (oInfo.Hotspot <> FHotspot) Or (oInfo.Subject <> FSubject) Or FDouble Then
    FWordProcessor.ApplyCursorState(crNoDrop, True)
  Else
    FWordProcessor.ApplyCursorState(crHandPoint, True);
End;

Procedure TWPActorHotspot.Up(aShift: TShiftState; iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If (oInfo.Hotspot = FHotspot) And (oInfo.Subject = FSubject) And Not FDouble Then
  Begin
    if oInfo.Hotspot <> nil Then
    Begin
      Link;
      Try
        FWordProcessor.DoHotspot(FWordProcessor.FRenderer.GetScreenRectForOffset(oInfo.Offset), oInfo.Hotspot, mbLeft);
      Finally
        Unlink;
      End;
    End
    Else
    Begin
        FWordProcessor.DoAnnotationClick(oInfo.Subject as TWPWorkingAnnotation);
    End;
  End;
  If Assigned(FWordProcessor) Then
  Begin
    Assert(Invariants('Up', FWordProcessor, TWordProcessor, 'FWordProcessor'));

    FWordProcessor.Renderer.CurrentHotspot := Nil;
    FWordProcessor.DoPaint;
  End;
End;

Type
  TWPActorHover  = Class (TWPMouseActor)
    Private
      FHotspot : TWPHotspot;
      FSubject : TFslObject;
      FBox : TRect;
    Public
      destructor Destroy; Override;
      Function RequiresDown : Boolean; Overload; Override;
      Function Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor; Overload; Override;
      Procedure Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean); Overload; Override;
      Procedure Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo); Overload; Override;
      Procedure Close(oNewActor : TWPMouseActor); Overload; Override;
  End;

Destructor TWPActorHover.Destroy;
Begin
  FHotspot.Free;
  FSubject.Free;
  Inherited;
End;

Function TWPActorHover.RequiresDown : Boolean;
Begin
  Result := False;
End;

Procedure TWPActorHover.Down(aShift: TShiftState; iX, iY: Integer; oInfo : TWPMouseInfo; bDouble : Boolean);
Begin
  FHotspot := oInfo.Hotspot.Link;
  FSubject := oInfo.Subject.Link;
  FWordProcessor.Renderer.CurrentHotspot := oInfo.Hotspot.Link;
  FWordProcessor.DoPaint;
  FBox := FWordProcessor.FRenderer.GetScreenRectForOffset(oInfo.Offset);
  FWordProcessor.DoHotspotHover(True, FBox, oInfo.Hotspot);
End;

Function TWPActorHover.Cursor(oInfo : TWPMouseInfo; bDoing : Boolean) : TCursor;
Begin
  Result := crHandPoint;
End;

Procedure TWPActorHover.Move(iX, iY: Integer; bOk : Boolean; oInfo : TWPMouseInfo);
Begin
  If oInfo.Hotspot <> FHotspot Then
  Begin
    If oInfo.Hotspot <> Nil Then
    Begin
     FWordProcessor.DoHotspotHover(False, FBox, FHotspot);
     FHotspot.Free;
     FHotspot := Nil;
     FWordProcessor.Renderer.CurrentHotspot := oInfo.Hotspot.Link;
     FHotspot := oInfo.Hotspot.Link;
     FWordProcessor.DoPaint;
     FBox := FWordProcessor.FRenderer.GetScreenRectForOffset(oInfo.Offset);
     FWordProcessor.DoHotspotHover(True, FBox, oInfo.Hotspot);
    End
    Else
    Begin
      Close(Nil);
      FWordProcessor.FMouseActor := Nil;
      Free;
    End;
  End
  Else if oInfo.Subject <> FSubject Then
  Begin
    Close(Nil);
    FWordProcessor.FMouseActor := Nil;
    Free;
  End;
End;

Procedure TWPActorHover.Close(oNewActor : TWPMouseActor);
Begin
  FWordProcessor.DoHotspotHover(False, FBox, FHotspot);
  If (oNewActor = Nil) Or Not (oNewActor Is TWPActorHotspot) Then
  Begin
    FWordProcessor.Renderer.CurrentHotspot := Nil;
    FWordProcessor.DoPaint;
  End;
End;


Constructor TWordProcessor.Create(oOwner: TComponent);
Begin
  Inherited Create(oOwner);
  {$IFDEF UNICODE}
  FTouch := TWPTouchManager.create(self);
  {$ENDIF}
  FMacro := TWPMacro.Create;
  FSettings := TWPSettings.Create;
  FCheckor := TWordProcessorCheckor.Create;
  FTranslator := TWPDocumentTranslator.Create;
  FCursorOutside := False;

  FInspectors := TWPInspectorList.Create;
  FInspectors.OwnsObjects := False;

  FRangeManager := TWPRangeManager.Create;
  FOperator := TWPOperator.Create;
  FOperator.OnDo := FRangeManager.RangeDo;
  FOperator.OnUndo := FRangeManager.RangeUndo;
  FOperator.OnRedo := FRangeManager.RangeRedo;
  FOperator.Settings := FSettings.Link;
  FOperator.OnInsertContent := ActorInsertContent;
  FOperator.OnDeleteContent := ActorDeleteContent;

  FRenderer := TWPScreenRenderer.Create;
  FRenderer.Settings := FSettings.Link;
  FRenderer.Working := False;
  FRenderer.Operator := FOperator.Link;
  FRenderer.OnError := HandleError;

  FDocumentHandler := TWordProcessorDocumentHandler.Create(Self);
  FDocumentHandler.Settings := Settings.Link;

  FFontsAllowed := TFslStringList.Create;

  FRenderer.OnSetDocumentHeight := LayoutDocumentHeightChange;
  FCursorDetails := TWordProcessCursorDetails.Create;

  TabStop := True;

  FConfiguredStyles := TWPStyles.Create;

  FPopup := TWPPopupMenu.Create(Self);

  FObservers := TWPObservers.Create;
  Color := clWhite;

  Constraints.MinHeight := 30;
  Constraints.MinWidth := 50;

  FDropBMP := TDropBMPTarget.Create(Self);
  FDropBMP.Register(Self);
  FDropBMP.OnDrop := BMPDrop;
  FDropBMP.Dragtypes := [dtCopy];
  FDropBMP.GetDataOnEnter := False;

  FPlaybackManager := Nil;

  FSettings.OnChange := SettingsChange;

  FPropertyServer := TWPPropertyServer.Create;
  FPropertyServer.Owner := Self;

  FPrimaryRange := ProduceRange;
  FDocumentHandler.OnLoadImage := FPrimaryRange.DefaultLoadImage;

  FOperator.MasterRangeId := PrimaryRange.Id;
  FRenderer.Selection := PrimaryRange.Selection.Link;

  Clear;

  FTimer := TUixTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.Enabled := True;
  FTimer.OnTimer := Timer;
End;


function TWordProcessor.CreateTouchMenu: TUixPanel;
begin
  if FTouchMenu <> nil then
  begin
    FTouchMenu.Free;
    FTouchMenu := nil;
  end;
  FTouchMenu := TUixPanel.Create(self);
  FTouchMenu.Parent := self;
  FTouchMenu.Top := -1;
  FTouchMenu.BringToFront;
  FTouchMenu.Height := 100;
  FTouchMenu.Width := 100;
  FTouchMenu.Color := clWhite;
  FTouchMenu.ParentColor := false;
  FTouchMenu.BevelInnerRaised;
  FtouchMenu.BevelOuterLowered;
  {$IFDEF UNICODE}
  FTouchMenu.ParentBackground := false;
  {$ENDIF}
  FTouchMenu.Transparent := false;
  FTouchMenu.Show;
  FTouchMenu.SetFocus;
  result := FTouchmenu;
end;

Destructor TWordProcessor.Destroy;
Begin
  If (FPaginator <> Nil) Then
  Begin
    FPaginator.Stop;
    FPaginator.Wait;
    FPaginator.Free;
  End;

  FMacro.Free;
  ConsumeRange(PrimaryRange);
  FPrimaryRange := Nil;

  FRangeManager.Free;

  If Assigned(FMouseActor) Then
    FMouseActor.Disconnect;
  FMouseActor.Free;
  FPropertyServer.Free;

  FPrinter.Free;
  FPageLayoutController.Free;

  Try
    // NOTE: TWinControlProxy.DestroyWnd may raise EWPException.create('Failed to Unregister '+ Parent.name);
    FDropBMP.UnregisterAll;
  Except
    // NOTE: suppressing is bad, but allowing an exception out of a destructor is worse [CH2011-11-28]
  End;

  FDropBMP.Free;

  FExistingSearch.Free;
  FExistingReplace.Free;
  FSpeller.Free;
  FObservers.Free;
  FPopup.Free;
  FTimer.Free;
  FCursorDetails.Free;
  FRenderer.Free;
  FDocument.Free;
  FConfiguredStyles.Free;
  FWorkingStyles.Free;
  FOperator.Free;
  FFontsAllowed.Free;
  FPlaybackManager.Free;
  FDocumentHandler.Free;
  FDefaultFieldDefinition.Free;
  FTranslator.Free;
  FCheckor.Free;
  FSettings.Free;
  FInspectors.Free;
  {$IFDEF UNICODE}
  FTouch.Free;
  {$ENDIF}
  Inherited;
End;


Procedure TWordProcessor.CreateWnd;
Begin
  Inherited;

  DragAcceptFiles(handle, True);
  SettingsChange(WPSETTINGS_PROPERTIES_ALL);
End;

Function TWordProcessor.ProduceRange : TWPVisualRange;
Var
  oRange : TWPVisualRange;
Begin
  CheckNotInMacro;
  oRange := TWPVisualRange.Create;
  Try
    oRange.Owner := Self;
    oRange.Document := FDocument.Link;
    oRange.Operator_ := FOperator.Link;
    oRange.Renderer := FRenderer.Link;
    If oRange.HasDocument Then
      oRange.GoHome(False);
    oRange.OnContentChange := ActorContentChange;
    oRange.OnSelectionChange := ActorSelectionChange;
    ConfigureRange(oRange);
    FRangeManager.Add(oRange);
    Result := oRange; // no link: the function result is not owned
  Finally
    oRange.Free;
  End;
End;

Procedure TWordProcessor.ConfigureRange(oRange : TWPRange);
Begin
End;

Procedure TWordProcessor.ConsumeRange(oRange : TWPRange);
Begin
  // cut off from the infrastructure - prevent use when inappropriate
  oRange.CutOff;
  FRangeManager.Remove(oRange);
End;


Function TWordProcessor.MakeMouseActor(aAction : TWPMouseAction; iOffset : Integer) : TWPMouseActor;
Begin
  If (aAction = msSelect) And PrimaryRange.Selection.HasSelection And PrimaryRange.Selection.InSelection(iOffset) And (canCut In PrimaryRange.Capabilities) And Not NoUserResponse Then
     aAction := msDragBlock;

  Case aAction Of
    msNull : Result := TWPActorNull.Create(Self);
    msSelect : Result := TWPActorSelect.Create(Self);
    msDragBlock : Result := TWPActorDragBlock.Create(Self);
    msDragEdge : Result := TWPActorDragEdge.Create(Self);
    msButton : Result := TWPActorButton.Create(Self);
    msHotspot : Result := TWPActorHotspot.Create(Self);
    msImageTool :
      Case ImageTool Of
         itSelect : Result := TWPActorSelect.Create(Self);
         itLine : Result := TWPActorDrawLine.Create(Self);
         itRectangle : Result := TWPActorDrawRectangle.Create(Self);
         itCircle :
           Begin
           Result := TWPActorDrawRectangle.Create(Self);
           TWPActorDrawRectangle(Result).FCircle := True;
           End;
         itMark : Result := TWPActorDrawMark.Create(Self);
         itZoom : Result := TWPActorDrawZoom.Create(Self);
      Else
        Result := Nil;
      End
  Else
    // action is Free or Hover. This is probably an error, but we will let it pass
    Result := Nil;
  End;
End;

function shiftToString(shiftstate: TShiftState):String;
begin
  result := '';

  if ssshift in shiftstate then
    result := result + ':shift';
  if ssalt in shiftstate then
    result := result + ':alt';
  if ssctrl in shiftstate then
    result := result + ':ctrl';
  if ssleft in shiftstate then
    result := result + ':left';
  if ssright in shiftstate then
    result := result + ':right';
  if ssmiddle in shiftstate then
    result := result + ':middle';
  if ssdouble in shiftstate then
    result := result + ':double';
  {$IFDEF UNICODE}
  if sstouch in shiftstate then
    result := result + ':touch';
  if sspen in shiftstate then
    result := result + ':pen';
  if sscommand in shiftstate then
    result := result + ':command';
  {$ENDIF}

  result := copy(result, 2, $ff);
end;

Function TWordProcessor.WantDragSelection(x, y : integer; var oActor : TWPMouseActor) : Boolean;
begin
  result := false;
  if FCursorDetails.FMode <> cdSelect then
    result := false
  else if (x >= FCursorDetails.FSelStartLeft - 35) and (x <= FCursorDetails.FSelStartLeft+5) and
          (y >= FCursorDetails.FSelStartTop - 5) and (y <= FCursorDetails.FSelStartTop+ 35) then
  begin
    result := true;
    oActor := TWPActorDragSelection.create(self);
    TWPActorDragSelection(oActor).Start := true;
    TWPActorDragSelection(oActor).XOffset := 30;
    TWPActorDragSelection(oActor).YOffset := -30;
  end
  else if (x >= FCursorDetails.FSelEndLeft-5) and (x <= FCursorDetails.FSelEndLeft + 35) and
          (y >= FCursorDetails.FSelEndTop-5) and (y <= FCursorDetails.FSelEndTop + 35) then
  begin
    result := true;
    oActor := TWPActorDragSelection.create(self);
    TWPActorDragSelection(oActor).Start := false;
    TWPActorDragSelection(oActor).XOffset := 30;
    TWPActorDragSelection(oActor).YOffset := 30;
  end
end;

Procedure TWordProcessor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  oInfo : TWPMouseInfo;
  oActor : TWPMouseActor;
  bDbl : Boolean;
Begin
  Inherited;
  // writeln('mouse down: '+inttostr(x)+','+inttostr(y)+'  ('+shiftToString(shift)+')');

  If FCodeCompletionListBox <> nil Then
  Begin
    FCodeCompletionListBox.Free;
    FCodeCompletionListBox := nil;
  End;
  if FTouchMenu <> nil then
  Begin
    FTouchMenu.Free;
    FTouchMenu := nil;
  End;

  If Settings.Interactive And Not FMacro.Recording Then
  Begin
    If (Button = mbLeft) Then
    Begin
      bDbl := FJustDoubleClicked;
      FJustDoubleClicked := False;

      oInfo := TWPMouseInfo.Create;
      Try
        if Not WantDragSelection(X, Y + FScrollPoint.Y, oActor) then
          If Not FRenderer.getMouseInfoForPoint(X, Y + FScrollPoint.Y, oInfo) Then
            oActor := Nil
          Else
            oActor := MakeMouseActor(oInfo.ProposedAction, oInfo.Offset);

        Hint := oInfo.HintText;
        ShowHint := Hint <> '';

        Try
          // let the existing hover action decide how to terminate depending on the new value
          If (FMouseActor <> Nil) Then
          Begin
            FMouseActor.Close(oActor);
            FMouseActor.Free;
            FMouseActor := Nil;
          End;

          If (oActor <> Nil) Then
          Begin
            ApplyCursorState(oActor.Cursor(oInfo, True), True);
            oActor.Down(Shift, X, Y + FScrollPoint.Y, oInfo, bDbl);
          End
          Else
          Begin
            ApplyCursorState(crArrow, False);
            SoundBeepExclamation;
          End;

          FMouseActor := oActor.Link;

          SetCapture(Handle);
          SetFocus;
        Finally
          oActor.Free;
        End;
      Finally
        oInfo.Free;
      End;
    End
    Else If (FMouseActor <> Nil) Then
    Begin
      FMouseActor.Close(Nil);
      FMouseActor.Free;
      FMouseActor := Nil;
    End;
  End;
End;

Procedure TWordProcessor.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  oInfo : TWPMouseInfo;
  oActor : TWPMouseActor;
  bOk : Boolean;
Begin
  Inherited;
//   writeln('mouse move: '+inttostr(x)+','+inttostr(y)+'  ('+shiftToString(shift)+')');

  If Settings.Interactive And Not FMacro.Recording Then
  Begin
    If Not (ssLeft In Shift) And Assigned(FMouseActor) And FMouseActor.RequiresDown Then
    Begin
      FMouseActor.Free;
      FMouseActor := Nil;
    End;

    oInfo := TWPMouseInfo.Create;
    Try
      bOk := FRenderer.getMouseInfoForPoint(X, Y + FScrollPoint.Y, oInfo);
      Hint := oInfo.HintText;
      ShowHint := Hint <> '';

      If (FMouseActor <> Nil) Then
        FMouseActor.Move(x,Y + FScrollPoint.Y, bOk, oInfo)
      Else If Not bOk Then
        ApplyCursorState(crArrow, False)
      Else If (oInfo.ProposedAction = msHotspot) Then
      Begin
        FMouseActor := TWPActorHover.Create(Self);
        ApplyCursorState(FMouseActor.Cursor(oInfo, False), False);
        FMouseActor.Down(Shift, X, Y + FScrollPoint.Y, oInfo, False);
      End
      Else
      Begin
        oActor := MakeMouseActor(oInfo.ProposedAction, oInfo.Offset);
        Try
          ApplyCursorState(oActor.Cursor(oInfo, False), False);
        Finally
          oActor.Free;
        End;
      End
    Finally
      oInfo.Free;
    End;
  End;
End;

Procedure TWordProcessor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  oInfo : TWPMouseInfo;
  bOk : Boolean;
  bContinue : Boolean;
Begin
  Inherited;

 // writeln('mouse up: '+inttostr(x)+','+inttostr(y)+'  ('+shiftToString(shift)+')');


  If Settings.Interactive And Not FMacro.Recording Then
  Begin
    oInfo := TWPMouseInfo.Create;
    Try
      bOk := FRenderer.getMouseInfoForPoint(X, Y + FScrollPoint.Y, oInfo);
      bContinue := True;

      If bOk And (Button = mbRight) Then
      Begin
        If Not NoUserResponse Then
        Begin
          If Not PrimaryRange.Selection.HasSelection Or Not PrimaryRange.Selection.InSelection(oInfo.Offset) Then
            PrimaryRange.MoveTo(oInfo.Offset);

          If (oInfo.Hotspot = Nil) Or Not DoHotspot(Renderer.GetScreenRectForOffset(oInfo.Offset), oInfo.hotSpot, mbRight) Then
            ShowPopup(X, Y, oInfo);
        End;
      End
      Else If Assigned(FMouseActor) Then
      Begin
        FMouseActor.Up(Shift, X, Y + FScrollPoint.Y, bOk, oInfo);

        bContinue := Assigned(FMouseActor) And Assigned(FMouseActor.FWordProcessor);

        If bContinue Then
          FMouseActor.Free;

        FMouseActor := Nil;
      End;

      ReleaseCapture;

      If bContinue Then
        MouseMove(Shift, X, Y);

      Cursor := Screen.Cursor;
      Screen.Cursor := crDefault;
    Finally
      oInfo.Free;
    End;
  End;
End;

Procedure TWordProcessor.DblClick;
Begin
  If Settings.Interactive And Not FMacro.Recording Then
    FJustDoubleClicked := True;
End;

Procedure TWordProcessor.Paint;
Var
  aMouse : TPoint;
  oInfo : TWPMouseInfo;
  oActor : TWPMouseActor;
Begin
  Inherited;

  FRenderer.Working := True;

  DragMarkCleared;

  If FCursorDetails.Showing Then
    ToggleCursor;

  DoPaint;

  If FCursorDetails.Showing Then
    ToggleCursor;

  If FResetMouseCursor And GetCursorPos(Windows.TPoint(aMouse)) Then
  Begin
    FResetMouseCursor := False;
    aMouse := TPoint(ScreenToClient(Windows.TPoint(aMouse)));
    If RectHit(TRect(BoundsRect), aMouse) Then
    Begin
      oInfo := TWPMouseInfo.Create;
      Try
        If FRenderer.GetMouseInfoForPoint(aMouse.x, aMouse.y+FScrollPoint.Y, oInfo) Then
        Begin
          oActor := MakeMouseActor(oInfo.ProposedAction, oInfo.Offset);
          Try
            ApplyCursorState(oActor.Cursor(oInfo, False), False);
          Finally
            oActor.Free;
          End;
        End;
      Finally
        oInfo.Free;
      End;
    End;
  End;
End;


Procedure TWordProcessor.ActorSelectionChange(oSender: TObject);
Begin
  If FCursorDetails.Showing Then
    ToggleCursor;

  FCursorDetails.Clear;
  CheckOffsetVisible(PrimaryRange.Selection.SelActive);
  if FDocument.AllAnnotations.Select(Selection.WorkingSelStart, Selection.WorkingSelEnd) Then
    FRenderer.PaintAnnotations(false);
  If FRenderer.Working Then
  Begin
     If Assigned(parent) Then
      Paint;
     ToggleCursor;
  End;

  NotifyObservers;

  If Assigned(FOnSelectionChanged) Then
    FOnSelectionChanged(Self);
End;


Function TWordProcessor.CheckOffsetVisible(iIndex : Integer) : Boolean;
Var
  iTop, iLeft, iHeight : Integer;
  aColour : TColour;
Begin
  Result := False;

  If FRenderer.GetPointForOffset(iIndex, iLeft, iTop, iHeight, aColour) Then
  Begin
    If (iTop - FScrollPoint.Y <= 0) Then
    Begin
      FScrollPoint.Y := FScrollPoint.Y + (iTop - FScrollPoint.Y);

      ScrollbarPositionChanged;
    End
    Else If (iTop + iHeight - FScrollPoint.Y > ClientHeight) Then
    Begin
      FScrollPoint.Y := FScrollPoint.Y + (iTop + iHeight - FScrollPoint.Y) - ClientHeight;

      ScrollbarPositionChanged;
    End;

    Result := True;
  End;
End;


Procedure TWordProcessor.DragMarkCleared;
Begin
  FCursorDetails.Fmode := cdBlink;
End;


Procedure TWordProcessor.ClearDragMark;
Begin
  FCursorDetails.Fmode := cdBlink;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Canvas.Lock;
  Try
  {$ENDIF}
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Renderer.Canvas.ApplyContrast(FCursorDetails.FDragMarkColour);
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(FCursorDetails.FDragMarkLeft, FCursorDetails.FDragMarkTop - FScrollPoint.Y);
    Canvas.LineTo(FCursorDetails.FDragMarkLeft, FCursorDetails.FDragMarkTop + FCursorDetails.FDragMarkHeight - FScrollPoint.Y - 1);
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    Canvas.Unlock;
  End;
  {$ENDIF}
End;


Procedure TWordProcessor.DrawDragMark(iOffset : Integer);
Begin
  If FRenderer.GetPointForOffset(iOffset, FCursorDetails.FDragMarkLeft, FCursorDetails.FDragMarkTop, FCursorDetails.FDragMarkHeight, FCursorDetails.FDragMarkColour) Then
  Begin
    FCursorDetails.FMode := cdDrag;

    {$IFNDEF NO_VCL_CANVAS_LOCK}
    Canvas.Lock;
    Try
    {$ENDIF}
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := Renderer.Canvas.ApplyContrast(clDkGray);
      Canvas.Pen.Width := 2;
      Canvas.MoveTo(FCursorDetails.FDragMarkLeft, FCursorDetails.FDragMarkTop - FScrollPoint.Y);
      Canvas.LineTo(FCursorDetails.FDragMarkLeft, FCursorDetails.FDragMarkTop + FCursorDetails.FDragMarkHeight - FScrollPoint.Y - 1);
    {$IFNDEF NO_VCL_CANVAS_LOCK}
    Finally
      Canvas.Unlock;
    End;
    {$ENDIF}
  End;
End;


Procedure TWordProcessor.DrawCursor(bShow : Boolean);
Begin
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Canvas.Lock;
  Try
  {$ENDIF}
    if InTouchMode and (FCursorDetails.FMode <> cdBlink) then
    begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := Renderer.Canvas.ApplyContrast(clBlack);
      Canvas.Pen.Width := 1;
      Canvas.Brush.Style := bsSolid;
      if FCursorDetails.FMode = cdDrag then
        Canvas.Brush.Color := clGreen
      else if FCursorDetails.FSelHot = shStart then
        Canvas.Brush.Color := clRed
      else
        Canvas.Brush.Color := clBlue;
      Canvas.Ellipse(
          FCursorDetails.FSelStartLeft-20, FCursorDetails.FSelStartTop - FScrollPoint.Y,
          FCursorDetails.FSelStartLeft, FCursorDetails.FSelStartTop - FScrollPoint.Y+20);
      if FCursorDetails.FMode = cdDrag then
        Canvas.Brush.Color := clGreen
      else if FCursorDetails.FSelHot = shEnd then
        Canvas.Brush.Color := clRed
      else
        Canvas.Brush.Color := clBlue;

      Canvas.Ellipse(
          FCursorDetails.FSelEndLeft, FCursorDetails.FSelEndTop - FScrollPoint.Y,
          FCursorDetails.FSelEndLeft+20, FCursorDetails.FSelEndTop - FScrollPoint.Y+20);
    end
    else
    begin
      If Settings.Interactive And Settings.Blinking And bShow Then
      Begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := Renderer.Canvas.ApplyContrast(clBlack);
      End
      Else
      Begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := Renderer.Canvas.ApplyContrast(FCursorDetails.FColour);
      End;

      Canvas.Pen.Width := 2;
      Canvas.MoveTo(FCursorDetails.Left, FCursorDetails.Top - FScrollPoint.Y);
      Canvas.LineTo(FCursorDetails.Left, FCursorDetails.Top + FCursorDetails.Height - FScrollPoint.Y - 1);
    end;
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    Canvas.Unlock;
  End;
  {$ENDIF}
End;


Procedure TWordProcessor.ToggleCursor;
Var
  iTop, iLeft, iHeight : Integer;
  aColour : TColour;
Begin
  If Settings.Blinking And Settings.Interactive And FCursorDetails.Showing Then
  Begin
    FCursorDetails.Showing := False;

    DrawCursor(False);
  End
  Else If Settings.Blinking And Settings.Interactive And (Focused Or ShowCursorAlways) Then
  begin
    if not PrimaryRange.Selection.HasSelection Then
    Begin
      If FCursorDetails.Height = 0 Then
      Begin
        FRenderer.GetPointForOffset(PrimaryRange.Selection.Cursor, iLeft, iTop, iHeight, aColour);

        FCursorDetails.Left := iLeft;
        FCursorDetails.Top := iTop;
        FCursorDetails.Height := iHeight;
        FCursorDetails.Colour := aColour;
      End;

      FCursorDetails.Showing := True;
      DrawCursor(FCursorDetails.Showing);
    end
    else if InTouchMode then
    begin
      If (FCursorDetails.Height = 0) Then
      Begin
        if FCursorDetails.FMode = cdBlink then
          FCursorDetails.FMode := cdSelect;
        FRenderer.GetPointForOffset(PrimaryRange.Selection.SelEnd, iLeft, iTop, iHeight, aColour);
        FCursorDetails.FSelEndLeft := iLeft;
        FCursorDetails.FSelEndTop := iTop + iHeight;
        FCursorDetails.FSelEndHeight := iHeight;
        FCursorDetails.FSelEndColour := aColour;
        FRenderer.GetPointForOffset(PrimaryRange.Selection.SelStart, iLeft, iTop, iHeight, aColour);
        FCursorDetails.FSelStartLeft := iLeft;
        FCursorDetails.FSelStartTop := iTop + iHeight;
        FCursorDetails.FSelStartHeight := iHeight;
        FCursorDetails.FSelStartColour := aColour;
      End;
      FCursorDetails.Showing := True;
      DrawCursor(FCursorDetails.Showing);
    End;
  End;
End;

Procedure TWordProcessor.Resize;
Var
  iNewWidth : Integer;
  iWidth : Integer;
  bWorking : Boolean;
Begin
  Inherited;
  FOperator.MarkChange;

  // actor's page height should always adjust to height change
  If (PrimaryRange.PageHeight <> (Height - 2 * Settings.VerticalMargin)) Then
    PrimaryRange.PageHeight := Height - 2 * Settings.VerticalMargin;

  If Width <> 0 Then
  Begin
    iWidth := IntegerMin(ActualWidth, MAX_INTERNAL_WIDTH);

    // Check if scroll bar will be visible
    If (Settings.ShowVerticalScrollbar Or Settings.Interactive) And ((MinimumRecommendedHeight) > ClientHeight) Then
      iNewWidth := IntegerMax(100, iWidth - VerticalScrollBarWidth)
    Else
      iNewWidth := IntegerMax(100, iWidth);

    If FWorkingWidthLimit > 0 Then
      iNewWidth := IntegerMin(iNewWidth, FWorkingWidthLimit);

    If (iNewWidth > 0) And (FRenderer.Width <> iNewWidth) Then
    Begin
      ToggleCursor;

      bWorking := FRenderer.Working;
      Try
        FRenderer.Working := False;
        FRenderer.Width := iNewWidth;
        FRenderer.NominalPageHeight := Height - 2 * Settings.VerticalMargin;
      Finally
        FRenderer.Working := bWorking;
      End;
      Repaint;

      FCursorDetails.Clear;
    End;
  End;

  If FRenderer.Working Then
  Begin
    UpdateScrollBar;

    If Settings.Interactive And Settings.Blinking Then
      DrawCursor(True);
  End;
End;


Procedure TWordProcessor.SetWorkingWidthLimit(Const Value: Integer);
Begin
  Assert(Condition((Value = 0) Or (Value > 100), 'SetWorkingWidthLimit', 'Value must be > 100'));
  FWorkingWidthLimit := Value;
  Resize;
End;

Function TWordProcessor.GetConfiguredStyles : TWPStyles;
Begin
  Assert(FCheckor.Invariants('GetStyles', FConfiguredStyles, TWPStyles, 'Styles'));
  Result := FConfiguredStyles;
End;


Procedure TWordProcessor.SetConfiguredStyles(Const Value: TWPStyles);
Begin
  Assert(Value <> Nil, 'Cannot set nil styles');

  FConfiguredStyles.Free;
  FConfiguredStyles := Value;
End;


Function TWordProcessor.HasWorkingStyles : Boolean;
Begin
  Result := Assigned(FWorkingStyles);
End;


Function TWordProcessor.GetWorkingStyles : TWPStyles;
Begin
  Assert(FCheckor.Invariants('GetStyles', FWorkingStyles, TWPStyles, 'Styles'));
  Result := FWorkingStyles;
End;


Procedure TWordProcessor.SetWorkingStyles(Const Value: TWPStyles);
Begin
  Assert(Value <> Nil, 'Cannot set nil styles');

  FWorkingStyles.Free;
  FWorkingStyles := Value;
End;



Procedure TWordProcessor.Timer(oSender: TObject);
Var
  iSystem : Cardinal;
  iLast : Cardinal;
  hnd : HWnd;
Begin
  if Renderer.AltKeyDown and (GetAsyncKeyState(vkAlt) = 0) then
  begin
    Renderer.AltKeyDown := false;
    Invalidate;
  end;
  if (FOperator.LastChange > 0) and (FOperator.LastChange < UniversalDateTime - (FASTDRAW_REDRAW * DATETIME_SECOND_ONE)) then
  begin
    FOperator.LastChange := 0;
    FRenderer.PaintAll := true;
    FRenderer.Update;
    DoPaint;
  end;
  if FCodeCompletionListBox <> nil Then
  Begin
    hnd := GetFocus;
    if (hnd <> self.Handle) And (hnd <> FCodeCompletionListBox.Handle) Then
    begin
      FCodeCompletionListBox.Free;
      FCodeCompletionListBox := nil;
    end;
  End;
  if FTouchMenu <> nil then
  Begin
    hnd := GetFocus;
    if (hnd <> self.Handle) And (hnd <> FTouchMenu.Handle) Then
    begin
      FTouchMenu.Free;
      FTouchMenu := nil;
    end;
  End;
  ToggleCursor;
  CheckLostHotspot;
  If (Assigned(FPaginator)) Then
  Begin
    FinishPagination;
  End
  Else
  Begin
    iSystem := GetTickCount;
    iLast := FOperator.LastAction;
    If (iSystem < iLast) Then
      // check for clock wrap
      FOperator.LastAction := iSystem
    Else If (iSystem - iLast > PAGINATION_DELAY) Then
      Paginate;
  End;
  If FMacro.LastError And (FMacro.LastErrorTime < now - 2 * DATETIME_SECOND_ONE) Then
  Begin
    FMacro.LastError := False;
    Invalidate;
  End;
End;


Function ShiftState(aShift : TShiftState) : TWPShiftStates;
Begin
  Result := [];

  If ssShift In aShift Then
    include(Result, wssShift);

  If ssAlt In aShift Then
    include(Result, wssAlt);

  If ssCtrl In aShift Then
    include(Result, wssCtrl);

  If ssLeft In aShift Then
    include(Result, wssLeft);

  If ssRight In aShift Then
    include(Result, wssRight);

  If ssMiddle In aShift Then
    include(Result, wssMiddle);

  If ssDouble In aShift Then
    include(Result, wssDouble);
End;


Procedure TWordProcessor.KeyUp(Var Key: Word; Shift: TShiftState);
var
  value : integer;
Begin
  Inherited;

  if Key = vkAlt then
  begin
    Renderer.AltKeyDown := false;
    Invalidate;
  end;

  If (Key = vkAlt) And (Shift = []) And (FAltNum <> '') Then
  Begin
    If StringIsInteger32(FAltNum) Then
    begin
      value := StringToInteger32(FAltNum);
      if (value >= 32) and (value <= 255) then
        PrimaryRange.Insert(chr(value));
    end;

    FAltNum := '';
  End;
End;


Procedure TWordProcessor.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  Inherited;
  If FCodeCompletionListBox <> nil Then
  Begin
    FCodeCompletionListBox.Free;
    FCodeCompletionListBox := nil;
  End;
  if FTouchMenu <> nil then
  Begin
    FTouchMenu.Free;
    FTouchMenu := nil;
  End;

  if Key = vkAlt then
  begin
    Renderer.AltKeyDown := true;
    Invalidate;
  End;


  If Settings.Interactive Then
  Begin
    If ProcessKey(Key, ShiftState(Shift)) Then
      Key := 0;
  End;
End;


Function TWordProcessor.CheckForHotspot(ch : Char) : Boolean;
Var
  oHotspot : TWPHotspot;
  oItem : TWPMapObject;
  oOwner : TWPWorkingDocumentPiece;
Begin
  oHotspot := Document.GetHotspot(ch, oItem, oOwner);

  Result := Assigned(oHotspot);

  If Result Then
  Begin
    If (oOwner <> Nil) And (oOwner Is TWPWorkingDocumentFieldStartPiece) And (oHotspot.URL = '') Then
    Begin
      PrimaryRange.MoveTo(oOwner.Metrics.Position+1);
      While Not (oOwner.Next Is TWPWorkingDocumentFieldStopPiece) Do
      Begin
        PrimaryRange.SelectTo(oOwner.Next.Next.Metrics.Position);
        oOwner := oOwner.Next;
      End;
    End
    Else If (oOwner <> Nil) And (oOwner Is TWPWorkingDocumentSectionStartPiece) And (oHotspot.URL = '') Then
    Begin
      PrimaryRange.MoveTo(oOwner.Metrics.Position+1);
      While (oOwner.Next Is TWPWorkingDocumentSectionStartPiece) Do
      Begin
        PrimaryRange.MoveTo(oOwner.Next.Metrics.Position+1);
        oOwner := oOwner.Next;
      End;
    End
    Else If Assigned(oItem) Then
      DoHotspot(oItem.asRect, oHotspot, mbLeft)
    Else
      DoHotspot(Renderer.asRect, oHotspot, mbLeft);
  End;
End;


Procedure StringToFile(Const sname, sContent : String);
Var
  oFile : TFslFile;
  aBytes : TBytes;
Begin
  oFile := TFslFile.Create(sName, fmCreate);
  Try
    aBytes := TEncoding.UTF8.GetBytes(sContent);
    oFile.Write(aBytes[0], Length(aBytes));
  Finally
    oFile.Free;
  End;
End;


Function TWordProcessor.IsAnnotationKey(iKey: Word; Const aShift: TWPShiftStates) : Boolean;
var
  i : integer;
Begin
  result := false;
  if canInsertAnnotation in PrimaryRange.Capabilities Then
    For i := 0 to Settings.AnnotationDefinitions.Count - 1 Do
    Begin
      if Settings.AnnotationDefinitions[i].IsInsertKey(iKey, aShift, SelectedText) Then
      Begin
        result := true;
        InsertAnnotation(Settings.AnnotationDefinitions[i]);
      End;
    End;
End;

Function TWordProcessor.ProcessKey(iKey: Word; Const aShift: TWPShiftStates) : Boolean;
Var
  sError : String;
  bRecord : Boolean;
Begin
  If NoUserResponse Then
  Begin
    Result := False;
    Exit;
  End;

  Try
    bRecord := True;
    Result := True;

    if Selection.HasSelection and IsAnnotationKey(iKey, aShift) Then
    Begin
      result := true;
      Exit;
    End;

    If aShift = [wssAlt] Then
    Begin
      Case iKey Of
        vkAlt: FAltNum := '';
        vkA..vkZ: Result := CheckForHotspot(chr(ord('0') + (iKey - vk0)));
        vk0..vk9: Result := CheckForHotspot(chr(ord('A') + (iKey - vkA)));
        vkNum0 : FAltNum := FAltNum + '0';
        vkNum1 : FAltNum := FAltNum + '1';
        vkNum2 : FAltNum := FAltNum + '2';
        vkNum3 : FAltNum := FAltNum + '3';
        vkNum4 : FAltNum := FAltNum + '4';
        vkNum5 : FAltNum := FAltNum + '5';
        vkNum6 : FAltNum := FAltNum + '6';
        vkNum7 : FAltNum := FAltNum + '7';
        vkNum8 : FAltNum := FAltNum + '8';
        vkNum9 : FAltNum := FAltNum + '9';
        vkBack  : PrimaryRange.Undo;
        vkMinus : result := PrimaryRange.ListStartDecrement;
        vkEquals : result := PrimaryRange.ListStartReset;
      Else
        Result := False;
      End;
    End
    Else If (aShift = [wssAlt, wssShift]) Then
    Begin
      Case iKey Of
        vkBack  : PrimaryRange.Redo;
        vkEquals : result := PrimaryRange.ListStartIncrement;
      Else
        Result := False;
      End;
    End
    Else If ((aShift = []) Or (aShift = [wssShift])) Then
      Case iKey Of
        vkHome  : PrimaryRange.GoLineStart(wssShift In aShift);
        vkEnd   : PrimaryRange.GoLineEnd(wssShift In aShift);
        vkLeft  : PrimaryRange.Left(wssShift In aShift);
        vkRight : PrimaryRange.Right(wssShift In aShift);
        vkBack  : PrimaryRange.DeleteLeft;
        vkDel   : If (aShift = []) Then
                    PrimaryRange.DeleteRight
                  Else
                    PrimaryRange.Cut;
        vkSpace : PrimaryRange.Insert(' ');
        vkInsert : If (aShift = []) Then
                  Result := False
                Else If Not PrimaryRange.Paste(sError) Then
                  DialogError(sError, 0);
        vkEnter : If Not PrimaryRange.Selection.HasSelection And PrimaryRange.HasCurrentFieldStart Then
                    CommitField
                  Else If aShift = [wssShift] Then
                    PrimaryRange.LineBreak
                  Else
                    PrimaryRange.NewParagraph;
        vkPageUp : PrimaryRange.PageUp(wssShift In aShift);
        vkPageDown : PrimaryRange.PageDown(wssShift In aShift);
        vkUp    : PrimaryRange.LineUp(wssShift In aShift);
        vkDown  : PrimaryRange.LineDown(wssShift In aShift);
        vkTab :
          Begin
          If wssShift In aShift Then
          Begin
            CheckNotInMacro;
            If Not PrimaryRange.PreviousField Then
              If HasPreviousControl() Then
                SetFocusedControl(PreviousControl)
              Else
                Result := False;
          End
          Else
          Begin
            If Not PrimaryRange.NextField Then
              If HasNextControl() Then
                SetFocusedControl(NextControl)
              Else
                Result := False;
          End;
          End;
        vk0..vk9:
        Begin
          If wssShift In aShift Then
          Begin
            Case iKey Of
              vk0:PrimaryRange.Insert(')');
              vk1:PrimaryRange.Insert('!');
              vk2:PrimaryRange.Insert('@');
              vk3:PrimaryRange.Insert('#');
              vk4:PrimaryRange.Insert('$');
              vk5:PrimaryRange.Insert('%');
              vk6:PrimaryRange.Insert('^');
              vk7:PrimaryRange.Insert('&');
              vk8:PrimaryRange.Insert('*');
              vk9:PrimaryRange.Insert('(');
            End;
          End
          Else
          Begin
            PrimaryRange.Insert(chr(ord('0')+(iKey - vk0)));
          End;
        End;
        vkNum0..vkNum9 : PrimaryRange.Insert(chr(ord('0')+(iKey - vkNum0)));
        vkSEMICOLON :    If wssShift In aShift Then PrimaryRange.Insert(':') Else PrimaryRange.Insert(';');
        vkCOMMA :        If wssShift In aShift Then PrimaryRange.Insert('<') Else PrimaryRange.Insert(',');
        vkFULLSTOP :     If wssShift In aShift Then PrimaryRange.Insert('>') Else PrimaryRange.Insert('.');
        vkSLASH :        If wssShift In aShift Then PrimaryRange.Insert('?') Else PrimaryRange.Insert('/');
        vkSINGLEQUOTE:   If wssShift In aShift Then PrimaryRange.Insert('"') Else PrimaryRange.Insert('''');
        vkSQUARELEFT :   If wssShift In aShift Then PrimaryRange.Insert('{') Else PrimaryRange.Insert('[');
        vkSQUARERIGHT:   If wssShift In aShift Then PrimaryRange.Insert('}') Else PrimaryRange.Insert(']');
        vkBACKSLASH :    If wssShift In aShift Then PrimaryRange.Insert('|') Else PrimaryRange.Insert('\');
        vkLEFTQUOTE :    If wssShift In aShift Then PrimaryRange.Insert('~') Else PrimaryRange.Insert('`');
        vkMINUS :        If wssShift In aShift Then PrimaryRange.Insert('_') Else PrimaryRange.Insert('-');
        vkEQUALS :       If wssShift In aShift Then PrimaryRange.Insert('+') Else PrimaryRange.Insert('=');
        vkNumDivide :    PrimaryRange.Insert('/');
        vkNumDecimal :   PrimaryRange.Insert('.');
        vkNumMultiply :  PrimaryRange.Insert('*');
        vkNumSubtract :  PrimaryRange.Insert('-');
        vkNumAdd :       PrimaryRange.Insert('+');
        vkF3 :  If Settings.Search Then
                  If HasExistingSearch Then PrimaryRange.Search(ExistingSearch) Else SearchDialog
                Else
                  Result := False;
        vkF7 :           CheckSpelling;
        vkContextMenu :  InvokePopop;
        vkA..vkZ:
        Begin
          If (wssShift In aShift) <> IsCapitalKeyToggled Then
            PrimaryRange.Insert(chr(ord('A') + (iKey - vkA)))
          Else
            PrimaryRange.Insert(chr(ord('a') + (iKey - vkA)));
        End;
      Else
        Result := False;
      End
    Else If ((aShift = [wssCtrl]) Or (aShift = [wssCtrl, wssShift])) Then
    Begin
      Case iKey Of
        vkHome : PrimaryRange.GoHome(wssShift In aShift);
        vkEnd : PrimaryRange.GoEnd(wssShift In aShift);
        vkLEFT : PrimaryRange.LeftWord(wssShift In aShift);
        vkRIGHT : PrimaryRange.RightWord(wssShift In aShift);
        vkBack : PrimaryRange.DeleteWordLeft;
        vkDel : PrimaryRange.DeleteWordRight;
        vkF1 : Settings.EditHints := Not Settings.EditHints;
        vkEnter : PrimaryRange.InsertPageBreak;

        vkEquals :
        Begin
          If wssShift In aShift Then
            PrimaryRange.ApplySuperscript(True)
          Else
            PrimaryRange.ApplySubscript(True);
        End;

        vkSpace : CodeCompletePrompt;

        vkTab :
        Begin
          If wssShift In aShift Then
            Result := PrimaryRange.PreviousTableCell
          Else
            Result := PrimaryRange.NextTableCell;
        End;

        vkInsert :
        Begin
          If (aShift = [wssCtrl, wssShift]) Then
            Result := False
          Else
            PrimaryRange.Copy;
        End;

        vkC : PrimaryRange.Copy;

        vkA : PrimaryRange.SelectAll;
        vkB : PrimaryRange.ApplyBold(Not PrimaryRange.IsBold);
        vkE : PrimaryRange.AlignCentre;

        vkF :
        Begin
          If Settings.Search Then
            SearchDialog
          Else
            Result := False;
        End;

        vkM : InsertSymbol;
        vkG : PrimaryRange.InsertColumnRight;
        vkH : PrimaryRange.InsertRowBelow;
        vkI : PrimaryRange.ApplyItalic(Not PrimaryRange.IsItalic);
        vkL : PrimaryRange.AlignLeft;
        vkN : result := DoNew;
        vkO : result := DoOpen;
        vkR : If (aShift = [wssCtrl, wssShift]) Then
              Begin
                bRecord := False;
                ToggleMacro
              End
              Else
                PrimaryRange.AlignRight;
        vkP : If (aShift = [wssCtrl, wssShift]) Then
                ExecuteMacro
              Else
                Result := DoPrint;
        vkS:  If (aShift = [wssCtrl, wssShift]) Then
                SnapShot('ctrl-alt-shift-s', false)
              Else
                Result := DoSave;
        vkT : PrimaryRange.ConvertTextToTable;
        vkU : PrimaryRange.ApplyUnderline(Not PrimaryRange.IsUnderline);

        vkV :
        Begin
          If Not PrimaryRange.Paste(sError) Then
            DialogError(sError, 0);
        End;

        vkX : PrimaryRange.Cut;
        vkY : PrimaryRange.Redo;
        vkD :
          If wssShift In aShift Then
          Begin
            SaveImage(SystemTemp + 'image.bmp');
            ExecuteOpen(SystemTemp + 'image.bmp');
          End
          Else
          Begin
            StringToFile(SystemTemp + 'native.txt', DocumentHandler.AsNative);
            ExecuteOpen(SystemTemp + 'native.txt');
          End;

        vkZ :
        Begin
          If wssShift In aShift Then
            PrimaryRange.Redo
          Else
            PrimaryRange.Undo;
        End;
      Else
        Result := False;
      End
    End
    else
    Begin
      Result := False;
    End;
    If FMacro.Recording And bRecord And Result Then
      FMacro.Actions.Add(TWPMacroKeyAction.Create(iKey, aShift));
  Except
    On e : ELibraryException Do
    Begin
      SoundBeepAsterisk;
      FMacro.LastError := True;
      Invalidate;
      Result := False;
    End;
    On e:Exception Do
      Raise;
  End;
End;


Procedure TWordProcessor.WMGetDlgCode(Var Msg: TWMGetDlgCode);
Begin
  Inherited;

  Msg.Result := DLGC_WANTARROWS Or DLGC_WANTCHARS Or DLGC_WANTTAB;// Or DLGC_WANTALLKEYS;
End;


Procedure TWordProcessor.ActorContentChange(oSender: TObject);
Begin
  FRenderer.ClearPaginations;
  FModified := True;
  If Assigned(FOnContentChanged) Then
    FOnContentChanged(Self);
End;


Procedure TWordProcessor.ShowPopup(iX, iY: Integer; oInfo : TWPMouseInfo);
Var
  aPoint : TPoint;
Begin
  If Settings.AllowPopup And Not FMacro.Recording Then
  Begin
    aPoint.x := iX;
    aPoint.y := iY;
    aPoint := TPoint(ClientToScreen(Windows.TPoint(aPoint)));
    FPopup.MouseInfo := oInfo.Link;
    FPopup.Popup(aPoint.x, aPoint.y);
  End;
End;


procedure TWordProcessor.ShowPopupMenu();
var
  oInfo : TWPMouseInfo;
  x, y, h : integer;
  c : TColour;
  bOk : boolean;
begin
  if PrimaryRange.Selection.HasSelection then
    FRenderer.GetPointForOffset(PrimaryRange.Selection.SelEnd, x, y, h, c)
  else
    FRenderer.GetPointForOffset(PrimaryRange.Selection.Cursor, x, y, h, c);

  oInfo := TWPMouseInfo.Create;
  try
    bOk := FRenderer.getMouseInfoForPoint(X, Y + FScrollPoint.Y, oInfo);

    if bOk then
    begin
      if not NoUserResponse then
      Begin
        if not PrimaryRange.Selection.HasSelection or not PrimaryRange.Selection.InSelection(oInfo.Offset) then
          PrimaryRange.MoveTo(oInfo.Offset);

        if (oInfo.Hotspot = nil) or not DoHotspot(Renderer.GetScreenRectForOffset(oInfo.Offset), oInfo.hotSpot, mbRight) then
          ShowPopup(X, Y, oInfo);
      end;
    end
  finally
    oInfo.Free;
  end;
end;

Procedure TWordProcessor.Clear;
Begin
  CheckNotInMacro;
  FDocumentHandler.NewDocument;

  // Can't assert empty, as the application may override the definition of a cleared document.
End;


// TODO: use a dynamic range to clear everything
Function TWordProcessor.ClearFormatting : Boolean;
Begin
  CheckNotInMacro;
  Result := PrimaryRange.ClearFormatting;
End;


procedure TWordProcessor.CloseTouchMenu(var Message: TMessage);
begin
  FTouchMenu.Free;
  FTouchMenu := nil;
end;

procedure TWordProcessor.WantCloseTouchMenu;
begin
  PostMessage(Handle, UM_CLOSE_TOUCH, 0, 0);
end;

Procedure TWordProcessor.NotifyObservers;
Begin
  FObservers.Notify(Self);
End;

Procedure TWordProcessor.UnregisterObserver(oObject: TObject);
Begin
  FObservers.Unregister(oObject);
End;

Procedure TWordProcessor.RegisterObserver(oObject: TObject; aEvent: TNotifyEvent);
Begin
  FObservers.Register(oObject, aEvent);
End;


Function TWordProcessor.GetFontsAllowed : TFslStringList;
Begin
  Assert(FCheckor.Invariants('GetFontsAllowed', FFontsAllowed, TFslStringList, 'FontsAllowed'));
  Result := FFontsAllowed;
End;


Procedure TWordProcessor.SetFontsAllowed(Const Value: TFslStringList);
Begin
  FFontsAllowed.Free;
  FFontsAllowed := Value;
End;


Procedure TWordProcessor.LayoutDocumentHeightChange(oSender: TObject);
Begin
  UpdateScrollBar;
End;


Procedure TWordProcessor.UpdateScrollBar;
Var
  aScrollInfo : TScrollInfo;
  iRequiredRendererHeight : Integer;
Begin
  iRequiredRendererHeight := MinimumRecommendedHeight;

  If (Settings.ShowVerticalScrollbar Or Settings.Interactive) And (iRequiredRendererHeight > ClientHeight) Then
  Begin
    ScrollbarPositionChanged;

    FillChar(aScrollInfo, SizeOf(aScrollInfo), 0);
    aScrollInfo.cbSize := SizeOf(aScrollInfo);
    aScrollInfo.nPage := IntegerMax(0, ClientHeight - (2 * Settings.VerticalMargin));
    aScrollInfo.nMin := 0;
    aScrollInfo.nMax := iRequiredRendererHeight;
    aScrollInfo.nPos := FScrollPoint.Y;
    aScrollInfo.fMask := SIF_ALL;

    If Settings.ShowVerticalScrollbar Then
      aScrollInfo.fMask := aScrollInfo.fMask Or SIF_DISABLENOSCROLL;

    If Not Settings.Interactive Then
      aScrollInfo.nPage := aScrollInfo.nMax + 1;  // disable the scrollbar if one is shown, or remove it if not

    FScrollBarShowing := True;
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, aScrollInfo, True);
  End
  Else
  Begin
    FScrollPoint.Y := 0;

    ScrollbarPositionChanged;

    SetScrollPos(Handle, SB_VERT, FScrollPoint.Y, False);
    FScrollBarShowing := False;
    ShowScrollbar(Handle, SB_VERT, False);
  End;
End;


Function TWordProcessor.DoMouseWheel(aShift: TShiftState; iWheelDelta: Integer; aMousePos: Windows.TPoint): Boolean;
Var
  iY : Integer;
Begin
  If Settings.Interactive And Not FMacro.Recording And (aShift = []) And (MinimumRecommendedHeight > ClientHeight) Then
  Begin
    iY := FScrollPoint.Y;

    FScrollPoint.Y := FScrollPoint.Y - iWheelDelta;

    ScrollbarPositionChanged;

    MouseMove(aShift, aMousePos.x, aMousePos.y);

    If iY <> FScrollPoint.Y Then
      DoPaint;
  End;

  Result := True;
End;


function TWordProcessor.DoNew: Boolean;
begin
  result := assigned(FOnNew);
  if result then
    FOnNew(self);
end;

function TWordProcessor.DoOpen: Boolean;
begin
  result := assigned(FOnOpen);
  if result then
    FOnOpen(self);
end;

Procedure TWordProcessor.WMVScroll(Var Message : TWMVScroll);
Var
  aScroll : TScrollInfo;
  iY : Integer;
Begin
  iY := FScrollPoint.Y;

  FillChar(aScroll, SizeOf(aScroll), 0);
  aScroll.cbSize := SizeOf(aScroll);
  aScroll.fMask := SIF_TRACKPOS Or SIF_POS;
  GetScrollInfo(Handle, SB_VERT, aScroll);

  Case Message.ScrollCode Of
    SB_LINEDOWN      : Inc(FScrollPoint.Y, 16);
    SB_LINEUP        : Dec(FScrollPoint.Y, 16);
    SB_PAGEDOWN      : Inc(FScrollPoint.Y, ClientHeight - (2 * Settings.VerticalMargin));
    SB_PAGEUP        : Dec(FScrollPoint.Y, ClientHeight - (2 * Settings.VerticalMargin));
    SB_TOP           : FScrollPoint.Y := 0;
    SB_BOTTOM        : FScrollPoint.Y := (MinimumRecommendedHeight + (2 * Settings.VerticalMargin));
    SB_THUMBTRACK    : FScrollPoint.Y := aScroll.nTrackPos;
  End;

  If (iY <> FScrollPoint.Y) Then
  Begin
    ScrollbarPositionChanged;

    DoPaint;
  End;
End;


Procedure TWordProcessor.ScrollbarPositionChanged;
Begin
  FScrollPoint.Y := IntegerMax(0, IntegerMin(MinimumRecommendedHeight - ClientHeight + (2 * Settings.VerticalMargin), FScrollPoint.Y));

  If FCursorDetails.Showing Then
    ToggleCursor;

  SetScrollPos(Handle, SB_VERT, FScrollPoint.Y, True);

  ToggleCursor;
End;


Procedure TWordProcessor.FontDialog;
Var
  oDialog : TWPFontDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPFontDialog.Create(Owner);
  Try
    oDialog.Font.Assign(PrimaryRange.Font);
    oDialog.Readonly := Settings.ReadOnly;
    oDialog.FontsAllowed := FFontsAllowed.Link;
    oDialog.ConsoleMode := Settings.ConsoleMode;

    If oDialog.Execute Then
    Begin
      PrimaryRange.Font.Assign(oDialog.Font);
      PrimaryRange.ApplyFont(oDialog.Font);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.PasteSpecialDialog;
Var
  oDialog : TWPPasteSpecialDialog;
  sError : String;
Begin
  CheckNotInMacro;
  oDialog := TWPPasteSpecialDialog.Create(Self);
  Try
    oDialog.ReadOnly := Settings.ReadOnly;
    If oDialog.Execute Then
      If Not PrimaryRange.PasteSpecial(oDialog.Format, sError) Then
        DialogError(sError);
  Finally
    oDialog.Free;
  End;
End;


Procedure TWordProcessor.ParaDialog;
Var
  oDialog : TWPParagraphDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPParagraphDialog.Create(Owner);
  Try
    oDialog.ReadOnly := Settings.ReadOnly;
    oDialog.Paragraph.Assign(PrimaryRange.Paragraph);

    If oDialog.Execute Then
      Begin
      PrimaryRange.Paragraph.Assign(oDialog.Paragraph);
      PrimaryRange.ApplyParagraph(PrimaryRange.Paragraph);
      End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.StyleDialog;
Var
  oDialog : TWPStyleDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPStyleDialog.Create(Owner);
  Try
    oDialog.ReadOnly := Settings.ReadOnly;
    oDialog.Style.Assign(PrimaryRange.WorkingStyle);

    If oDialog.Execute Then
      PrimaryRange.WorkingStyle.Assign(oDialog.Style);
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.ChangeCaseDialog;
Var
  oDialog : TWPChangeCaseDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPChangeCaseDialog.Create(Owner);
  Try
    If oDialog.Execute Then
      Begin
      PrimaryRange.ApplyChangeCase(oDialog.ChangeCaseType);
      End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.DragOver(Source: TObject; X, Y: Integer; State: TDragState; Var Accept: Boolean);
Begin
  windows.Beep(800,20);
End;

Procedure TWordProcessor.DragDropFiles(Var VMsg: TMessage);
Var
  iTotal : Integer;
  iLoop : Integer;
  a: Array [0..500] Of Char;
  aPoint : TPoint;
  oInfo : TWPMouseInfo;
Begin
  Inherited;
  CheckNotInMacro;
  oInfo := TWPMouseInfo.Create;
  Try
    If Settings.Interactive And DragQueryPoint(VMsg.wParam, Windows.TPoint(aPoint)) And
        FRenderer.GetMouseInfoForPoint(IntegerMin(IntegerMax(aPoint.x, 0), ActualWidth - Settings.HorizontalMargin * 2), aPoint.y + FScrollPoint.Y, oInfo) Then
    Begin
    iTotal := DragQueryFile(VMsg.wParam, $FFFFFFFF, Nil, 0);
    For iLoop := 0 To iTotal - 1 Do
      Begin
      DragQueryFile(VMsg.wParam, iLoop, a, 500);
      PrimaryRange.DropFile(oInfo.Offset, a);
      End;
    End;
  Finally
    oInfo.Free;
  End;
End;

Procedure TWordProcessor.InsertImageDialog;
Var
  oDialog : TOpenDialog;
Begin
  CheckNotInMacro;
  oDialog := TOpenDialog.Create(Self);
  Try
    oDialog.Filter := 'All Images (*.jpeg;*.jpg;*.gif;*.png;*.ico;*.bmp)|*.jpeg;*.jpg;*.gif;*.png;*.ico;*.bmp|JPEG Images (*.jpeg;*.jpg)|*.jpeg;*.jpg|PNG Images (*.png)|*.png|GIF Images (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|All Files|*.*';
    oDialog.Title := 'Open Image';
    If oDialog.Execute Then
    Begin
      PrimaryRange.InsertImage(oDialog.FileName);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.InsertPDFDialog;
Var
  oDialog : TOpenDialog;
Begin
  CheckNotInMacro;
  oDialog := TOpenDialog.Create(Self);
  Try
    oDialog.Filter := 'PDF Documents (*.pdf)|*.pdf|All Files|*.*';
    oDialog.Title := 'Open PDF Document';
    If oDialog.Execute Then
    Begin
      PrimaryRange.InsertPDF(oDialog.FileName);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.ImagePropertiesDialog;
Var
  oImage : TWPWorkingDocumentImagePiece;
  oDialog : TWPImageDialog;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasCurrentImage Then
    Error('ImagePropertiesDialog', 'No Image is in scope');

  oImage := PrimaryRange.CurrentImage;

  oDialog := TWPImageDialog.Create(Owner);
  Try
    oDialog.Image := oImage.Clone;
    oDialog.ReadOnly := Settings.ReadOnly;
    If oDialog.Execute Then
    Begin
      PrimaryRange.SetImageProperties( oDialog.Image);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.ImageMapsDialog;
Var
  oImage : TWPWorkingDocumentImagePiece;
  oDialog : TWPImageMapDialog;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasCurrentImage Then
    Error('ImageMapsDialog', 'No Image is in scope');

  oImage := PrimaryRange.CurrentImage;

  oDialog := TWPImageMapDialog.Create(Owner);
  Try
    oDialog.Image := oImage.Clone;
    oDialog.ReadOnly := Settings.ReadOnly;
    If oDialog.Execute Then
    Begin
      PrimaryRange.SetImageProperties(oDialog.Image);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.InsertCompletionItem(oItem : TWPCompletionItem);
Var
  bOk : Boolean;
  sMessage : String;
  oStream : TFslStream;
  aFormat : TWPFormat;
Begin
  If oItem.Content <> '' Then
  Begin
    If PrimaryRange.HasCurrentFieldStart Then
    Begin
      PrimaryRange.SelectField(False);
      bOk := PrimaryRange.Insert(oItem.Content);
    End
    Else
      bOk := PrimaryRange.ReplaceCurrentWord(oItem.Content, sMessage);
  End
  Else
  Begin
    aFormat := wpfNative;
    oStream := FetchTemplateContent(oItem.Id, oItem.Code, aFormat);
    Try
      bOk := PrimaryRange.ReplaceCurrentWord(oStream, aFormat, sMessage)
    Finally
      oStream.Free;
    End;
  End;
  If Not bOk Then
  Begin
    FRenderer.Update;
    DialogError(sMessage);
  End Else If PrimaryRange.HasCurrentFieldStart Then
    CommitField;
  SetFocusedControl(self);
End;

Procedure TWordProcessor.CodeCompletePromptList(oList : TWPCompletionItems);
Begin
  CheckNotInMacro;
  If oList.Count = 1 Then
    InsertCompletionItem(oList[0])
  Else
  Begin
    If CheckOffsetVisible(PrimaryRange.Selection.Cursor) Then
      Paint;

    FCodeCompletionListBox := TWPCodeCompletionListBox.Create(self);
    FCodeCompletionListBox.Parent := Self;
    FCodeCompletionListBox.Left := CursorEndPos.X;
    FCodeCompletionListBox.Top := CursorEndPos.Y;
    FCodeCompletionListBox.List := oList.Link;
    FCodeCompletionListBox.OnInsert := InsertCompletionItem;
    FCodeCompletionListBox.SetFocus;

    If FCodeCompletionListBox.Top + FCodeCompletionListBox.Height > ClientHeight Then
      FCodeCompletionListBox.Top := Max(ClientHeight - FCodeCompletionListBox.Height, 0);

    If FCodeCompletionListBox.Height > ClientHeight Then
      FCodeCompletionListBox.Height := ClientHeight;
  End;
End;

Procedure TWordProcessor.CodeCompletePrompt;
Var
  sText : String;
  oList : TWPCompletionItems;
  bHandled : Boolean;
Begin
  CheckNotInMacro;
  If Not Settings.ReadOnly Then
  Begin
    bHandled := False;
    If PrimaryRange.HasCurrentFieldStart And PrimaryRange.CurrentFieldStart.HasDefinitionProvider Then
      bHandled := PrimaryRange.CurrentFieldStart.DefinitionProvider.CodeComplete(PrimaryRange.CurrentFieldStart.DocField, PrimaryRange.CurrentFieldContent);
    If Not bHandled Then
    Begin
      oList := TWPCompletionItems.Create;
      Try
        If Assigned(FOnCodeCompletion) And PrimaryRange.CursorInWord(sText) And FOnCodeCompletion(Self, sText, oList) And (oList.Count > 0) Then
          CodeCompletePromptList(oList)
        Else
          SoundBeepExclamation;
      Finally
        oList.Free;
      End;
    End;
  End;
End;


Procedure TWordProcessor.DoPaint;
Var
  iActualWidth : Integer;
  iWidth : Integer;
  aColor : TColour;
  aRect : TRect;
Begin
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Canvas.Lock;
  Try
  {$ENDIF}
    Settings.Background := Color;
    iActualWidth := ActualWidth;
    If FScrollBarShowing Then
      iActualWidth := iActualWidth - SCROLLBAR_WIDTH;

    If Not FNonVisibleMode Then
    Begin
      iWidth := FRenderer.PaintControl(Canvas.Handle, Height, iActualWidth,  FScrollPoint.Y);
      aColor := Renderer.Canvas.ApplyContrast(RandomColour(Color, False));
      Canvas.Brush.Color := aColor;
      Canvas.Brush.Style := bsSolid;
      If (iWidth < iActualWidth) Then
      Begin
        aRect.Left := iWidth;
        aRect.Top := 0;
        aRect.Right := iActualWidth;
        aRect.Bottom := Height;
        Canvas.FillRect(aRect);
      End;
      If FRenderer.Height - FScrollPoint.Y < Height Then
      Begin
        aRect.Left := 0;
        aRect.Top := FRenderer.Height - FScrollPoint.Y;
        aRect.Right := iActualWidth;
        aRect.Bottom := Height;
        Canvas.FillRect(aRect);
      End;

      If FMacro.LastError Then
        Ring(HTML_COLOUR_VALUES[hcCrimson])
      Else If FMacro.State = msRecording Then
        Ring(HTML_COLOUR_VALUES[hcForestgreen])
      Else If FMacro.State = msPlaying Then
        Ring(HTML_COLOUR_VALUES[hcDodgerblue])
      Else If Renderer.AltKeyDown Then
        Ring(HTML_COLOUR_VALUES[hcLinen]);
    End;
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    Canvas.Unlock;
  End;
  {$ENDIF}
End;



function TWordProcessor.DoPrint: Boolean;
begin
  result := assigned(FOnPrint);
  if result then
    FOnPrint(self);
end;

Procedure TWordProcessor.InsertSymbol;
Var
  oDialog : TWPSymbolDialog;
Begin
  CheckNotInMacro;
  If Not (canInsert In PrimaryRange.Capabilities) Then
    Error('InsertSymbol', 'Cannot insert here');

  oDialog := TWPSymbolDialog.Create(Owner);
  Try
//    oDialog.Filter := FSymbolFilter;
//    oDialog.Block := FSymbolBlock;
    oDialog.SpecifiedFont := PrimaryRange.Font.Link;
    ODialog.FontsAllowed.Assign(FFontsAllowed);
    oDIalog.AllowSpecialSymbols := Settings.AllowSpecialSymbols;
    If oDialog.Execute Then
    Begin
//      FSymbolFilter := oDialog.Filter;
//      FSymbolBlock := oDialog.Block;
      PrimaryRange.InsertSymbol(oDialog.InsertChar, oDialog.DrawnFontName);
    End;
  Finally
    oDialog.Free;
  End;
End;

Function TWordProcessor.GetSpeller : TWPSpeller;
Begin
  Assert(FCheckor.Invariants('GetSpeller', FSpeller, TWPSpeller, 'Speller'));
  Result := FSpeller;
End;


Procedure TWordProcessor.SetSpeller(Const Value: TWPSpeller);
Begin
  FSpeller.Free;
  FSpeller := Value;
  FOperator.Speller := FSpeller.Link;
  FSpeller.Settings := FSettings.Link;
  FSpeller.AllowedWords := FDocument.AllowedWords.Link;
End;


Function TWordProcessor.HasVoicePlayback : Boolean;
Begin
  Result := HasPlaybackManager And FVoiceActive;
End;


Procedure TWordProcessor.VoicePlayback;
Begin
  If HasVoicePlayback Then
    PlaybackManager.Playback;
End;


Procedure TWordProcessor.VoiceStop;
Begin
  If HasVoicePlayback Then
    PlaybackManager.Stop;
End;


Procedure TWordProcessor.VoiceAccelerate;
Begin
  If HasVoicePlayback Then
    PlaybackManager.Accelerate;
End;


Procedure TWordProcessor.VoiceDecelerate;
Begin
  If HasVoicePlayback Then
    PlaybackManager.Deccelerate;
End;


Procedure StringAppendStart(Var VStr: String; Var VLen: Integer);
Begin
  VLen := Length(VStr);
  SetLength(VStr, Length(VStr) + 4096);
End;

Procedure StringAppend(Var VStr: String; AStrToAdd: String; Var VLen: Integer; ADivChar: Char = #0);
  //procedure StringAppend(var s: String; ns: String; var len: Integer; divchar: Char = #0);
Var
  LOffset: Integer;
Begin
  If (AStrToAdd = '') And (ADivChar = #0) Then
    Begin
    Exit;
    End;
  If (ADivChar <> #0) And (VLen <> 0) Then
    LOffset := 1
  Else
    LOffset := 0;
  If VLen + LOffset + Length(AStrToAdd) > Length(VStr) Then
    SetLength(VStr, Length(VStr) + Integermax(4096, LOffset + Length(AStrToAdd)));
  If LOffset = 1 Then
    VStr[VLen + 1] := ADivChar;
  move(AStrToAdd[1], VStr[VLen + LOffset + 1], Length(AStrToAdd));
  Inc(VLen, LOffset + Length(AStrToAdd));
End;

Procedure StringAppendClose(Var VStr: String; ALen: Integer);
Begin
  SetLength(VStr, ALen);
End;


Procedure TWordProcessor.VoiceGetTextChanges(Var iTextStart, iTextLength : Integer; Var sText : String; Var iVisibleStart, iVisibleLength : Integer);
Var
  iLen : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  iTextStart := 0;
  iTextLength := FDocument.VoiceCharCount;

  sText := '';

  StringAppendStart(sText, iLen);

  oPiece := FDocument.Pieces.First;
  While Assigned(oPiece) Do
  Begin
    StringAppend(sText, oPiece.VoiceText, iLen);

    If oPiece.HasNext Then
      oPiece := oPiece.Next
    Else
      oPiece := Nil;
  End;

  StringAppendClose(sText, iLen);

  iVisibleStart := iTextStart;
  iVisibleLength := iTextLength;
End;


Procedure TWordProcessor.VoiceMakeTextChanges(iTextStart, iTextLength : Integer; sText : String);
Var
  sWord : String;
  bSplit : Boolean;
Begin
  CheckNotInMacro;
  VoiceSelect(iTextStart, iTextLength, False);

  PrimaryRange.DeleteSelection;

  While sText <> '' Do
  Begin
    bSplit := StringSplit(sText, #13#10, sWord, sText);

    If sWord <> '' Then
      PrimaryRange.Insert(sWord);

    If bSplit Then
      PrimaryRange.NewParagraph;
  End;
End;


Procedure TWordProcessor.VoiceGetSelectionRange(Var iSelectionStart, iSelectionLength : Integer);
Begin
  If PrimaryRange.Selection.HasSelection Then
  Begin
    iSelectionStart := FDocument.NormalPositionToVoicePosition(PrimaryRange.Selection.SelStart);
    iSelectionLength := FDocument.NormalPositionToVoicePosition(PrimaryRange.Selection.SelEnd) - iSelectionStart;
  End
  Else
  Begin
    iSelectionStart := FDocument.NormalPositionToVoicePosition(PrimaryRange.Selection.Cursor);
    iSelectionLength := 0;
  End;
End;


Procedure TWordProcessor.VoiceSelect(iTextStart, iTextLength : Integer; bDraw : Boolean);
Begin
  PrimaryRange.MoveTo(FDocument.VoicePositionToNormalPosition(iTextStart), bDraw);
  If iTextLength <> 0 Then
    PrimaryRange.SelectTo(FDocument.VoicePositionToNormalPosition(iTextStart + iTextLength), bDraw);
End;


Procedure TWordProcessor.VoiceSetSelectionRange(iSelectionStart, iSelectionLength : Integer);
Begin
  VoiceSelect(iSelectionStart, iSelectionLength, True);
End;


Function TWordProcessor.GetCursorEndPos: TPoint;
Var
  iCursor : Integer;
  iLeft, iTop, iHeight : Integer;
  aColour : TColour;
Begin
  If FCursorDetails.FShowing Then
    Begin
    Result.x := FCursorDetails.Left;
    Result.y := FCursorDetails.Top + FCursorDetails.Height;
    End
  Else
    Begin
    If PrimaryRange.Selection.HasSelection Then
      iCursor := PrimaryRange.Selection.SelEnd
    Else
      iCursor := PrimaryRange.Selection.Cursor;
    If FRenderer.GetPointForOffset(iCursor, iLeft, iTop, iHeight, aColour) Then
      Begin
      Result.x := iLeft;
      Result.y := iTop + iHeight - FScrollPoint.Y;
      End
    Else
      Begin
      Result.x := FCursorDetails.Left;
      Result.y := FCursorDetails.Top + FCursorDetails.Height - FScrollPoint.Y;
      End;
    End;
End;



Procedure TWordProcessor.InvokePopop;
Begin
  If Settings.Interactive And (FCursorDetails.Left <> 0) Then
    ShowPopup(FCursorDetails.FLeft, FCursorDetails.FTop + FCursorDetails.FHeight, Nil);
End;


Procedure TWordProcessor.InsertTableDialog;
Var
  oDialog : TWPInsertTableDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPInsertTableDialog.Create(Self);
  Try
    If oDialog.Execute Then
    Begin
      PrimaryRange.InsertTable(oDialog.Rows, oDialog.Columns);
    End;
  Finally
    oDialog.Free;
  End;
End;


Procedure TWordProcessor.TablePropertiesDialog;
Var
  oDialog : TWPTablePropertiesDialog;
Begin
  CheckNotInMacro;
  If Not (PrimaryRange.HasCurrentTableStart Or PrimaryRange.HasSelectedTable) Then
    Error('TablePropertiesDialog', 'No Table is in scope');

  oDialog := TWPTablePropertiesDialog.Create(Owner);
  Try
    oDialog.SetRange(PrimaryRange);
    oDialog.ReadOnly := Settings.ReadOnly;
    If oDialog.Execute Then
    Begin
      {  TODO: Macro
      MacroRecord(TWPMacroTableProperties.Create(oDialog.Table.Clone));
      MacroRecord(TWPMacroTableRowProperties.Create(oDialog.TableRow.Clone));
      }
      PrimaryRange.SetTableProperties(oDialog.Table, oDialog.Row, oDialog.Cell);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.LinePropertiesDialog;
Var
  oBreak : TWPWorkingDocumentBreakPiece;
  oDialog : TWPLineDialog;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasCurrentLine Then
    Error('RowPropertiesDialog', 'No Table Row is in scope');

  oBreak := PrimaryRange.CurrentLine;

  oDialog := TWPLineDialog.Create(Owner);
  Try
    oDialog.Line := oBreak.Clone;
    oDialog.ReadOnly := Settings.ReadOnly;
    If oDialog.Execute Then
    Begin
      PrimaryRange.SetLineProperties(oDialog.Line);
    End;
  Finally
    oDialog.Free;
  End;
End;


Procedure TWordProcessor.VoiceSaveSession(Const sFilename : String);
Begin
  If HasPlaybackManager Then
    PlaybackManager.SaveSession(sFilename);
End;


Procedure TWordProcessor.VoiceLoadSession(Const sFilename : String);
Begin
  If HasPlaybackManager Then
    PlaybackManager.LoadSession(sFilename);
End;


Procedure TWordProcessor.VoiceActivate;
Begin
  If Not FVoiceActive Then
  Begin
    FVoiceActive := True;

    If HasPlaybackManager Then
      PlaybackManager.Open;
  End;
End;


Procedure TWordProcessor.VoiceDeactivate;
Begin
  If FVoiceActive Then
  Begin
    If HasPlaybackManager Then
      PlaybackManager.Close;

    FVoiceActive := False;
  End;
End;


Function TWordProcessor.HasPlaybackManager: Boolean;
Begin
  Result := Assigned(FPlaybackManager);
End;

Procedure TWPMouseActor.Disconnect;
Begin
  FWordProcessor := Nil;
End;

Destructor TWPMouseActor.Destroy;
Begin
  Disconnect;
  Inherited;
End;

{ TWordProcessCursorDetails }

Procedure TWordProcessCursorDetails.Clear;
Begin
  FLeft := 0;
  FHeight := 0;
  FTop := 0;
End;


Procedure TWordProcessor.WMEraseBkgnd(Var Message: TWmEraseBkgnd);
Begin
  //  no erase background
  Message.Result := 1;
End;


Procedure TWordProcessor.SaveImage(Const sFilename : String);
Begin
  FRenderer.SaveImage(sFilename);
End;


Function TWordProcessor.GetExistingSearch : TWPSearchDetails;
Begin
  Assert(Condition(HasExistingSearch, 'GetExistingSearch', 'No ExistingSearch'));
  Result := FExistingSearch;
End;


Procedure TWordProcessor.SetExistingSearch(Const Value : TWPSearchDetails);
Begin
  FExistingSearch.Free;
  FExistingSearch := Value;
End;


Function TWordProcessor.HasExistingSearch : Boolean;
Begin
  Result := Assigned(FExistingSearch);
End;


function TWordProcessor.HasFields: Boolean;
var
  oIter : TWPPieceIterator;

begin
  result := False;
  oIter := TWPPieceIterator.Create;
  try
    oIter.Document := Document.Link;
    oIter.PieceTypes := [ptFieldStart];
    oIter.First;
    while oIter.More do
    begin
      result := true;
      oIter.Next;
    end;
  finally
    oIter.Free;
  end;
end;

Procedure TWordProcessor.SearchDialog;
Var
  oDialog : TWPSearchDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPSearchDialog.Create(Self);
  Try
    If HasExistingSearch Then
      oDialog.SearchDetails.Assign(ExistingSearch);
    oDialog.Range := PrimaryRange.Link;
    If oDialog.Execute Then
      ExistingSearch := oDialog.SearchDetails.Link;
  Finally
    oDialog.Free;
  End;
End;

Function TWordProcessor.GetExistingReplace : TWPReplaceDetails;
Begin
  Assert(Condition(HasExistingReplace, 'GetExistingReplace', 'No ExistingReplace'));
  Result := FExistingReplace;
End;


Procedure TWordProcessor.SetExistingReplace(Const Value : TWPReplaceDetails);
Begin
  FExistingReplace.Free;
  FExistingReplace := Value;
End;


Function TWordProcessor.HasExistingReplace : Boolean;
Begin
  Result := Assigned(FExistingReplace);
End;


Procedure TWordProcessor.ReplaceDialog;
Var
  oDialog : TWPReplaceDialog;
Begin
  CheckNotInMacro;
  oDialog := TWPReplaceDialog.Create(Self);
  Try
    If HasExistingReplace Then
      oDialog.ReplaceDetails.Assign(ExistingReplace);
    oDialog.Range := PrimaryRange.Link;
    If oDialog.Execute Then
      ExistingReplace := oDialog.ReplaceDetails.Link;
  Finally
    oDialog.Free;
  End;
End;


procedure TWordProcessor.ReplaceFieldContent(oDocument: TWPWorkingDocument; cursor: integer; oPiece: TWPWorkingDocumentFieldStartPiece; txt: String);
var
  iend : integer;
  text : TWPWorkingDocumentTextPiece;
begin
  inc(cursor);
  iend := cursor;
  while (iend < oDocument.Pieces.Count) and (oDocument.Pieces[iend].PieceType <> ptFieldStop) do
    inc(iend);
  if (iEnd = oDocument.Pieces.Count) then
    raise EWPException.create('Unable to find end of field');
  if (iEnd > cursor) then
    oDocument.Pieces.DeleteRange(cursor, iEnd-1);
  if (txt <> '') then
  begin
    text := TWPWorkingDocumentTextPiece.Create;
    oDocument.Pieces.insert(cursor, text);
    text.ApplyProperties(oPiece);
    text.Content := txt;
  end;
end;

Procedure TWordProcessor.CheckSpelling;
Begin
  CheckNotInMacro;
  If Not HasSpeller Then
    Error('CheckSpelling', 'There is no spelling support');
  Speller.Check(Self, CheckForAllowedWord, AddAllowedWord);
  PrimaryRange.ResetSpelling;
  Renderer.Valid := False;
  Renderer.PaintAll := True;
  Renderer.Update;
  Invalidate;
End;


Procedure TWordProcessor.SetFocusedControl(oControl : TWinControl);
Var
  oParent : TWinControl;
Begin
  oParent := GetParentForm(Self);
  If Assigned(oParent) Then
    TForm(oParent).ActiveControl := oControl;
End;


Function TWordProcessor.GetNextControl : TWinControl;
Begin
  Assert(Condition(HasNextControl, 'GetNextControl', 'No Next Control'));
  Result := FNextControl;
End;


Function TWordProcessor.GetPreviousControl : TWinControl;
Begin
  Assert(Condition(HasPreviousControl, 'GetPreviousControl', 'No Previous Control'));
  Result := FPreviousControl;
End;


Function TWordProcessor.HasNextControl : Boolean;
Begin
  Result := Assigned(FNextControl);
End;


Function TWordProcessor.HasPreviousControl : Boolean;
Begin
  Result := Assigned(FPreviousControl);
End;

Function TWordProcessor.Empty : Boolean;
Begin
  Result := Not HasDocument Or FDocument.IsEmpty;
End;


Function TWordProcessor.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWordProcessor.HasSpeller : Boolean;
Begin
  Result := Assigned(FSpeller);
End;


Procedure TWordProcessor.CheckDocument(oDocument : TWPWorkingDocument);
Begin
  {$IFOPT C+}
  TWPWorkingDocumentValidator.Validate(oDocument);
  {$ENDIF}

  If oDocument.Pieces.Count = 0 Then
    Error('CheckDocument', 'Document is empty and cannot be loaded into the word processor');
End;

Function TWordProcessor.GetSelectionStart: Integer;
Begin
  If PrimaryRange.Selection.HasSelection Then
    Result := PrimaryRange.Selection.SelStart
  Else
    Result := PrimaryRange.Selection.Cursor;
End;

Function TWordProcessor.GetSelectionEnd: Integer;
Begin
  If PrimaryRange.Selection.HasSelection Then
    Result := PrimaryRange.Selection.SelEnd
  Else
    Result := PrimaryRange.Selection.Cursor;
End;


Procedure TWordProcessor.BMPDrop(oSender: TObject; aShiftState: TShiftState; aPoint: Windows.TPoint; Var iEffect: LongInt);
Var
  oImage : TFslBitmapGraphic;
  oInfo : TWPMouseInfo;
Begin
  Inherited;
  CheckNotInMacro;
  oInfo := TWPMouseInfo.Create;
  Try
    If Settings.Interactive And FRenderer.GetMouseInfoForPoint(IntegerMin(IntegerMax(aPoint.x, 0), ActualWidth - Settings.HorizontalMargin * 2), aPoint.y + FScrollPoint.Y, oInfo) Then
    Begin
      oImage := TFslBitmapGraphic.Create;
      Try
        oImage.Handle.Assign(FDropBMP.Bitmap);
        PrimaryRange.DropImage(oInfo.Offset, oImage);
      Finally
        oImage.Free;
      End;
    End;
  Finally
    oInfo.Free;
  End;
End;



Procedure TWordProcessor.ShowError(Const sTitle, sMessage : String; iDuration : Integer);
Var
  oBalloon : TUixBalloon;
  aPoint : TPoint;
  iCursor : Integer;
  iTop, iLeft, iHeight : Integer;
  aColour : TColour;
Begin
  If PrimaryRange.Selection.HasSelection Then
    iCursor := PrimaryRange.Selection.SelEnd
  Else
    iCursor := PrimaryRange.Selection.Cursor;
  If Not FRenderer.GetPointForOffset(iCursor, iLeft, iTop, iHeight, aColour) Then
  Begin
    iLeft := 0;
    iTop := 0;
  End;

  aPoint.x := IntegerMin(iLeft - Settings.HorizontalMargin, ActualWidth);
  aPoint.y := IntegerMin(iTop + iHeight - FScrollPoint.Y, Height);
  aPoint := TPoint(ClientToScreen(Windows.TPoint(aPoint)));

  oBalloon := TUixBalloon.CreateNew(Self);
  oBalloon.Parent := Self;
  oBalloon.AbsoluteX := aPoint.x;
  oBalloon.AbsoluteY := aPoint.y;
  oBalloon.Title := sTitle;
  oBalloon.Message := sMessage;
  oBalloon.TimeoutDuration := iDuration;
  oBalloon.ShowBalloon;
End;


Procedure TWordProcessor.ShowFieldError(Const sNamespace, sName, sTitle, sMessage : String; iDuration : Integer = 3000);
Begin
  If PrimaryRange.SelectFieldByName(sNamespace, sName) Then
    ShowError(sTitle, sMessage, iDuration)
  Else
    Error('ShowFieldError', 'Field "'+sNamespace+'::'+sName+'" not found');
End;


Procedure TWordProcessor.ShowSectionError(Const sNamespace, sName, sTitle, sMessage : String; iDuration : Integer = 3000);
Begin
  If PrimaryRange.SelectSectionByName(sNamespace, sName) Then
    ShowError(sTitle, sMessage, iDuration)
  Else
    Error('ShowSectionError', 'Section "'+sNamespace+'::'+sName+'" not found');
End;


Function PageNoToString(iPage : Integer) : String;
Begin
  If iPage = -1 Then
    Result := '?'
  Else
    Result := IntegerToString(iPage);
End;

Function TWordProcessor.GetSelectionSummary : String;
Var
  iRow1 : Integer;
  iCol1 : Integer;
  iRow2 : Integer;
  iCol2 : Integer;
Begin
  If PrimaryRange.Selection.HasSelection Then
  Begin
    If FRenderer.GetLineCol(PrimaryRange.Selection.SelStart, iRow1, iCol1) And FRenderer.GetLineCol(PrimaryRange.Selection.SelEnd, iRow2, iCol2) Then
    Begin
      If iRow1 = iRow2 Then
        Result := 'Line '+IntegerToString(iRow1+1)+' Columns '+IntegerToString(iCol1+1)+' to '+IntegerToString(iCol2+1)
      Else
        Result := 'Line '+IntegerToString(iRow1+1)+' Column '+IntegerToString(iCol1+1)+' to Line '+IntegerToString(iRow2+1)+' Column '+IntegerToString(iCol2+1)
    End
    Else
    Begin
      Result := StringFormat('Error: selection = %d/%d', [PrimaryRange.Selection.SelStart, PrimaryRange.Selection.SelEnd]);
    End;
  End
  Else
  Begin
    If Not FRenderer.GetLineCol(PrimaryRange.Selection.Cursor, iRow1, iCol1) Then
    Begin
      If PrimaryRange.Selection.Cursor <= Document.FirstCursorPosition Then
        Result := 'Line 1 Column 1' // this is always true, and sometimes we end up here before things are properly laid out, and don't come back
      Else
        Result := StringFormat('Error: selection = %d', [PrimaryRange.Selection.Cursor]);
    End
    Else
    Begin
      Result := 'Line '+IntegerToString(iRow1+1)+' Column '+IntegerToString(iCol1+1);
      If Renderer.HasPaginations Then
        Result := Result+' (Page '+PageNoToString(Renderer.GetPageForOffset(PrimaryRange.Selection.Cursor))+'/'+IntegerToString(Renderer.PageCount)+')';
    End;
  End;
End;



Function TWordProcessor.GetPrimaryRange : TWPVisualRange;
Begin
  Assert(Condition(Assigned(FPrimaryRange), 'GetPrimaryRange', 'FPrimaryRange must be assigned.'));

  Result := FPrimaryRange;
End;


Procedure TWordProcessor.DoHotspotHover(bActive : Boolean; aBox : TRect; oHotspot : TWPHotspot);
Var
  oInfo : TWPHotspotInformation;
Begin
  If Assigned(FOnHotSpotHover) Then
  Begin
    aBox.Top := aBox.Top - FScrollPoint.Y;
    aBox.Bottom := aBox.Bottom - FScrollPoint.Y;
    oInfo := TWPHotspotInformation.Create;
    Try
      oInfo.Hotspot := oHotspot.Link;
      oInfo.Box := aBox;
      oinfo.Handled := False;
      FOnHotSpotHover(Self, bActive, oInfo);
    Finally
      oInfo.Free;
    End;
  End;
End;


Function TWordProcessor.DoHotspot(aBox : TRect; oHotspot : TWPHotspot; Const aMouseButton: TMouseButton) : Boolean;
Var
  oInfo : TWPHotspotInformation;
Begin
  If Assigned(FOnHotSpot) Then
  Begin
    aBox.Top := aBox.Top - FScrollPoint.Y;
    aBox.Bottom := aBox.Bottom - FScrollPoint.Y;
    oInfo := TWPHotspotInformation.Create;
    Try
      oInfo.Hotspot := oHotspot.Link;
      oInfo.Box := aBox;
      oinfo.MouseButton := aMouseButton;
      oinfo.Handled := True;
      FOnHotSpot(Self, oInfo);
      Result := oInfo.Handled;
    Finally
      oInfo.Free;
    End;
  End
  Else
    Result := False;
End;


Function TWordProcessor.DocumentHeight : Integer;
Begin
  Result := Renderer.Height;
End;


Function TWordProcessor.MinimumRecommendedHeight : Integer;
Begin
  Result := DocumentHeight;
End;


// the minimum recommended width is the width of the largest single
// image or word + the margins
Function TWordProcessor.MinimumRecommendedWidth : Integer;
Begin
  Result := Document.MinLength + Settings.HorizontalMargin * 2;
End;

Procedure TWordProcessor.Yield;
Begin
  Renderer.Yield;
End;



Function TWordProcessor.GetOperator : TWPOperator;
Begin
  Assert(FCheckor.Invariants('GetOperator', FOperator, TWPOperator, 'Actor'));
  Result := FOperator;
End;


Function TWordProcessor.GetDocument : TWPWorkingDocument;
Begin
  Assert(FCheckor.Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Function TWordProcessor.GetRenderer : TWPScreenRenderer;
Begin
  Assert(FCheckor.Invariants('GetRenderer', FRenderer, TWPScreenRenderer, 'Renderer'));
  Result := FRenderer;
End;


Function TWordProcessor.GetSelection : TWPSelection;
Begin
  Assert(FCheckor.Invariants('GetSelection', PrimaryRange.Selection, TWPSelection, 'Selection'));
  Result := PrimaryRange.Selection;
End;


Function TWordProcessor.GetDocumentHandler : TWPDocumentHandler;
Begin
  Assert(FCheckor.Invariants('GetDocumentHandler', FDocumentHandler, TWPDocumentHandler, 'DocumentHandler'));
  Result := FDocumentHandler;
End;


Function TWordProcessorCheckor.Invariants(Const sLocation : String; oObject : TFslObject; aClass : TClass; Const sObject : String) : Boolean;
Begin
  Result := Inherited Invariants(sLocation, oObject, aClass, sObject);
End;

{ TWordProcessorDocumentHandler }

Constructor TWordProcessorDocumentHandler.Create(oOwner : TWordProcessor);
Begin
  Create;
  FOwner := oOwner;
End;


Function TWordProcessorDocumentHandler.GetHost : TObject;
Begin
  Result := FOwner;
End;


Function TWordProcessorDocumentHandler.GetHostConfiguredStyles : TWPStyles;
Begin
  Result := FOwner.ConfiguredStyles;
End;


Function TWordProcessorDocumentHandler.GetHostWorkingStyles : TWPStyles;
Begin
  Result := FOwner.WorkingStyles;
End;


Function TWordProcessorDocumentHandler.GetHostDocument : TWPWorkingDocument;
Begin
  Result := FOwner.FDocument;
End;


Procedure TWordProcessorDocumentHandler.SetHostDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles);
Begin
  FOwner.LoadNewDocument(oDocument, oStyles);
End;

Procedure TWordProcessor.LoadNewDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles);
Begin
  CheckDocument(oDocument);
  oDocument.FieldDefinitionProviders := FSettings.FieldDefinitions.Link;
  oDocument.AnnotationDefinitionProviders := FSettings.AnnotationDefinitions.Link;
  oDocument.RegenerateMetrics(true, false);
  if BindToFields(oDocument) then
    oDocument.RegenerateMetrics(true, false);

  If Assigned(FMouseActor) Then
    FMouseActor.Disconnect;
  FMouseActor.Free;
  FMouseActor := Nil;

  If Not oStyles.HasDefaultStyle Then
    oStyles.UseSystemDefaultStyle;

  Renderer.Working := False;
  FDocument.Free;
  FDocument := Nil;
  FScrollPoint.y := 0;
  FScrollPoint.x := 0;
  FWorkingStyles.Free;
  FWorkingStyles := oStyles.Link;
  FOperator.Styles := FWorkingStyles.Link;
  FRenderer.Styles := FWorkingStyles.Link;
  {$IFDEF UNICODE}
  if InTouchMode then
    FTouch.Load;
  {$ENDIF}
  FDocument := oDocument.Link;
  If FSpeller <> Nil Then
    FSpeller.AllowedWords := FDocument.AllowedWords.Link;

  ConsumeRange(PrimaryRange);
  FPrimaryRange := Nil;

  FRangeManager.Clear;

  FPrimaryRange := ProduceRange;

  FOperator.MasterRangeId := PrimaryRange.Id;
  FRenderer.Selection := PrimaryRange.Selection.Link;
  FRenderer.Document := FDocument.Link;
  FOperator.Document := FDocument.Link;

  FRenderer.Clear;
  ActorContentChange(Self);
  PrimaryRange.GoHome(False);
  PrimaryRange.CheckSpelling;
  FResetMouseCursor := True;
  Renderer.Working := True;
  if (assigned(OnDocumentLoaded)) then
    OnDocumentLoaded(self);
  Invalidate;
End;


Function TWordProcessor.GetSettings : TWPSettings;
Begin
  Assert(Condition(Assigned(FSettings), 'GetSettings', 'FSettings must be assigned'));

  Result := FSettings;
End;


function TWordProcessor.GetSourceLocation(var line, col: integer): boolean;
var
  offset, index : integer;
  piece : TWPWorkingDocumentPiece;
begin
  if Selection.HasSelection then
    result := Document.GetPieceByPosition(Selection.SelStart, piece, offset, index)
  else
    result := Document.GetPieceByPosition(Selection.Cursor, piece, offset, index);

  if result then
  begin
    line := piece.SourceLine;
    col := piece.SourceCol;
    result := (line <> 0) and (col <> 0);
  end;
end;

Procedure TWordProcessor.SetSettings(Const Value : TWPSettings);
Begin
  FSettings.OnChange := Nil;
  FSettings.Free;
  FSettings := Value.Link;
  FSettings.OnChange := SettingsChange;
  If Settings.FieldWrappers = wpfpNone Then
    Settings.FieldWrappers := wpfpSquareBrackets; // wpfpNone has consequences for cursor routines
  SettingsChange(WPSETTINGS_PROPERTIES_ALL);
End;


Procedure TWordProcessor.SettingsChange(Const aProperties :  TWPSettingsProperties);
Begin
  FRenderer.ChangeSettings;
  DoPaint;
  PrimaryRange.ChangeState(False);
  NotifyObservers;
  if Assigned(FOnSettingsChanged) then
    FOnSettingsChanged(self);
  If Assigned(FOnSelectionChanged) Then
    FOnSelectionChanged(Self);
End;


Function TWordProcessor.GetReadOnly : Boolean;
Begin
  Result := Settings.ReadOnly;
End;


Procedure TWordProcessor.SetReadOnly(Const Value : Boolean);
Begin
  If Value Then
    Settings.ModeReadonly
  Else
    Settings.ModeWordProcessor;
End;


Function TWordProcessor.GetPlaybackManager : TDragonPlaybackManager;
Begin
  Assert(FCheckor.Invariants('GetPlaybackManager', FPlaybackManager, TDragonPlaybackManager, 'FPlaybackManager'));

  Result := FPlaybackManager;
End;


Procedure TWordProcessor.SetPlaybackManager(Const Value : TDragonPlaybackManager);
Begin
  Assert(Not Assigned(Value) Or FCheckor.Invariants('SetPlaybackManager', Value, TDragonPlaybackManager, 'Value'));

  FPlaybackManager.Free;
  FPlaybackManager := Value;
End;


Function TWordProcessor.HasImageArea(Const sURL : String; Out oImage : TWPWorkingDocumentImagePiece; Out oArea : TWPImageMapArea) : Boolean;
Var
  iLoop : Integer;
  oWork : TWPWorkingDocumentPiece;
Begin
  iLoop := 0;
  Result := False;
  While Not Result And (iLoop < FDocument.Pieces.Count) Do
  Begin
    oWork := FDocument.Pieces[iLoop];
    If (oWork.PieceType = ptImage) Then
    Begin
      oImage := TWPWorkingDocumentImagePiece(oWork);
      If (oImage.HasImageMap) Then
      Begin
        oArea := oImage.ImageMap.GetAreaByURL(sURL);
        Result := oArea <> Nil;
      End;
    End;
    Inc(iLoop);
  End;
End;


Procedure TWordProcessor.SelectImageArea(oImage: TWPWorkingDocumentImagePiece; oArea: TWPImageMapArea);
Begin
  CheckNotInMacro;
  PrimaryRange.SelectImageArea(oImage, oArea);
End;


Procedure TWordProcessor.ApplyCursorState(Const aCursor : TCursor; bPersist : Boolean);
Begin
  If FCursorPersist Or Not FCursorOutside Then
  Begin
    If (aCursor = crIBeam) And Not Settings.Selecting Then
      Screen.Cursor := crArrow
    Else
      Screen.Cursor := aCursor
  End
  Else
  Begin
    Screen.Cursor := crDefault;
  End;

  FCursorPersist := bPersist;
End;


Function TWordProcessor.GetPageLayoutController : TWPPageLayoutController;
Begin
  Result := FPageLayoutController;
End;


Procedure TWordProcessor.SetPageLayoutController(Const Value : TWPPageLayoutController);
Begin
  FPageLayoutController.Free;
  FPageLayoutController := Value;
End;


Function TWordProcessor.GetPrinter : TFslPrinter;
Begin
  Result := FPrinter;
End;


Procedure TWordProcessor.SetPrinter(Const Value : TFslPrinter);
Begin
  FPrinter.Free;
  FPrinter := Value;
End;

Procedure TWordProcessor.Paginate;
Var
  oPaginator : TWPPaginator;
Begin
  // can we paginate?
  If (Settings.Pagination And (Assigned(FPrinter) And Assigned(FPageLayoutController))) And
    // do we need to paginate?
       (Not FRenderer.HasPaginations And Not Assigned(FPaginator)) Then
  Begin
    oPaginator := TWPPaginator.Create;
    Try
      oPaginator.Operator := FOperator.Link;
      oPaginator.Document := FDocument.Clone;
      oPaginator.Styles := FWorkingStyles.Link;
      oPaginator.Printer := FPrinter.Link;
      oPaginator.PageLayoutController := FPageLayoutController.Link;
      FPaginator := oPaginator.Link;
      oPaginator.Start;
    Finally
      oPaginator.Free;
    End;
  End;
End;

Procedure TWordProcessor.FinishPagination;
Begin
  If FPaginator.Terminated Then
  Begin
    If FPaginator.Completed Then
    Begin
      FRenderer.SetPaginations(FPaginator.Paginations);
      If Assigned(FOnSelectionChanged) Then
        FOnSelectionChanged(Self);
      Invalidate;
    End;
    FPaginator.Free;
    FPaginator := Nil;
  End;
End;

Procedure TWordProcessor.CheckLostHotspot;
Begin
  If Renderer.ShowHotspots(false) And (FRenderer.CurrentHotspot <> Nil) And Not MouseIsOver Then
  Begin
    MouseMove([], -1, -1);
  End;
End;

Function TWordProcessor.MouseIsOver: Boolean;
Var
  aMouse : TPoint;
Begin
  If (GetCursorPos(Windows.TPoint(aMouse))) Then
  Begin
    aMouse := TPoint(ScreenToClient(Windows.TPoint(aMouse)));
    Result := (aMouse.x > 0) And (aMouse.x < ActualWidth) And (aMouse.y > 0) And (aMouse.y < Height);
  End
  Else
    Result := False;
End;

Function TWordProcessor.GetProperties: TWPPropertyList;
Begin
  Result := FPropertyServer.GetProperties;
End;


Procedure TWordProcessor.SetProperty(oProperty: TWPProperty; Const sValue: String);
Begin
  FPropertyServer.SetProperty(oProperty, sValue);
End;

Function EnumFontsProc(Var LogFont: TLogFont; Var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; Stdcall;
Var
  oList : TStringList;
Begin
  Result := 1;
  oList := TStringlist(Data);
  If FontType And TRUETYPE_FONTTYPE > 0 Then
    oList.Add(LogFont.lfFaceName);
End;

Function TWordProcessor.ListAllFonts: TStringList;
Var
  iLoop : Integer;
Begin
  Result := TStringList.Create;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Canvas.Lock;
  Try
  {$ENDIF}
    EnumFonts(Canvas.Handle,  Nil, @EnumFontsProc, pointer(Result));
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    Canvas.Unlock;
  End;
  {$ENDIF}

  For iLoop := Result.Count - 1 DownTo 0 Do
  Begin
    If ((FontsAllowed.Count > 0) And Not FontsAllowed.ExistsByValue(Result[iLoop])) or (result[iLoop][1] = '@') Then
      Result.Delete(iLoop);
  End;

  Result.Sort;
End;

Function EnumSpecialFontsProc(Var LogFont: TLogFont; Var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; Stdcall;
Var
  oList : TStringList;
Begin
  Result := 1;
  oList := TStringlist(Data);
  If (FontType And TRUETYPE_FONTTYPE > 0) And (LogFont.lfCharSet = SYMBOL_CHARSET) Then
    oList.Add(LogFont.lfFaceName);
End;

Function TWordProcessor.ListSpecialFonts: TStringList;
Var
  iLoop : Integer;
Begin
  Result := TStringList.Create;

  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Canvas.Lock;
  Try
  {$ENDIF}
    EnumFonts(Canvas.Handle,  Nil, @EnumSpecialFontsProc, pointer(Result));
  {$IFNDEF NO_VCL_CANVAS_LOCK}
  Finally
    Canvas.Unlock;
  End;
  {$ENDIF}

  For iLoop := Result.Count - 1 DownTo 0 Do
  Begin
    If (FontsAllowed.Count > 0) And Not (FontsAllowed.ExistsByValue(Result[iLoop])) Then
      Result.Delete(iLoop);
  End;

  Result.Sort;
End;

Procedure TWordProcessor.CommitField;
Begin
  If PrimaryRange.HasCurrentFieldStart And PrimaryRange.CurrentFieldStart.HasDefinitionProvider Then
    PrimaryRange.CurrentFieldStart.DefinitionProvider.Commit(PrimaryRange.CurrentFieldStart.DocField, PrimaryRange.CurrentFieldContent);
  if PrimaryRange.HasCurrentFieldStart And Assigned(OnUpdateField) Then
  begin
    PrimaryRange.CurrentFieldStart.LastUpdateValue := PrimaryRange.CurrentFieldContent;
    OnUpdateField(self, fusDeliberate, PrimaryRange.CurrentFieldStart.DefinitionProvider, PrimaryRange.CurrentFieldStart.DocField, PrimaryRange.CurrentFieldContent, not PrimaryRange.CurrentFieldStart.InError);
  end;
End;


Function TWordProcessor.GoToFieldByName(Const sNamespace, sName : String) : Boolean;
Begin
  CheckNotInMacro;
  Result := PrimaryRange.GotoFieldByName(sNamespace, sName);
End;


Procedure TWordProcessor.DoButton(aBox : TRect; oButton : TFslObject; iOffset : Integer);
Begin
  If (oButton Is TWPWorkingDocumentFieldStopPiece) And TWPWorkingDocumentFieldStopPiece(oButton).MatchingStart.HasDefinitionProvider And
    TWPWorkingDocumentFieldStopPiece(oButton).MatchingStart.DefinitionProvider.HasUIAssistance(TWPWorkingDocumentFieldStopPiece(oButton).MatchingStart.DocField) Then
  Begin
    PrimaryRange.MoveTo(TWPWorkingDocumentFieldStopPiece(oButton).Metrics.Position, True);
    CodeCompletePrompt;
  End
  Else If (oButton is TWPWorkingDocumentFieldStartPiece) And (TWPWorkingDocumentFieldStartPiece(oButton).Checkables.Count > 0) Then
  Begin
    PrimaryRange.MoveTo(TWPWorkingDocumentFieldStartPiece(oButton).Metrics.Position+TWPWorkingDocumentFieldStartPiece(oButton).Metrics.CharCount, True);
    PrimaryRange.SetFieldCheckIndex(TWPWorkingDocumentFieldStartPiece(oButton), iOffset - TWPWorkingDocumentFieldStartPiece(oButton).Metrics.Position);
  End
  Else
    MessageDlg('not done yet', mtInformation, [mbok], 0);
End;


Procedure TWordProcessor.CMMouseLeave(Var Message: TMessage);
Begin
  Inherited;
  If Not FCursorPersist Then
    Screen.Cursor := crDefault;
  FCursorOutside := True;
End;

Procedure TWordProcessor.CMMouseEnter(Var Message: TMessage);
Begin
  Inherited;
  FCursorOutside := False;
End;

{ TWPHotspotInformation }

Destructor TWPHotspotInformation.Destroy;
Begin
  FHotspot.Free;
  Inherited;
End;

Procedure TWPHotspotInformation.SetHotspot(Const Value: TWPHotspot);
Begin
  FHotspot.Free;
  FHotspot := Value;
End;

Function TWordProcessor.SelectedText: String;
Begin
 Result := PrimaryRange.SelectedText;
End;

Function TWordProcessor.GetThumb: Integer;
Begin
  Result := FScrollPoint.Y;
End;

Procedure TWordProcessor.SetThumb(iValue : Integer);
Begin
  FScrollPoint.Y := iValue;
  ScrollbarPositionChanged;
End;

Function TWordProcessor.GetTopLine: Integer;
Begin
  Result := 0;
End;

Procedure TWordProcessor.SetTopLine(Const iValue: Integer);
Begin

End;


Function TWordProcessor.TextForLine(iLine: Integer): String;
Begin
//  Result := Actor.TextForLine(iLine);
End;

Function TWordProcessor.Capabilities: TWPCapabilities;
Begin
  Result := PrimaryRange.Capabilities;
End;

Procedure TWordProcessor.Copy;
Begin
  CheckNotInMacro;
  PrimaryRange.Copy;
End;

Procedure TWordProcessor.Cut;
Begin
  CheckNotInMacro;
  PrimaryRange.Cut;
End;

Procedure TWordProcessor.Redo;
Begin
  CheckNotInMacro;
  PrimaryRange.Redo;
End;

Procedure TWordProcessor.Undo;
Begin
  CheckNotInMacro;
  PrimaryRange.Undo;
End;

Function TWordProcessor.Paste(Var sError: String): Boolean;
Begin
  CheckNotInMacro;
  Result := PrimaryRange.Paste(sError);
End;

Procedure TWordProcessor.ApplyStyle(Const sName: String);
Begin
  CheckNotInMacro;
  PrimaryRange.Style := sName;
End;

Procedure TWordProcessor.ApplyStyleToLine(Const sName: String);
Begin
  CheckNotInMacro;
  PrimaryRange.GoLineEnd(False);
  PrimaryRange.GoLineStart(True);
  PrimaryRange.Style := sName;
  PrimaryRange.GoLineEnd(False);
End;

Procedure TWordProcessor.CopyAllToClipboard;
Var
  oRange : TWPVisualRange;
Begin
  CheckNotInMacro;
  oRange := ProduceRange;
  Try
    oRange.SelectAll;
    oRange.Copy;
  Finally
    ConsumeRange(oRange);
  End;
End;

Procedure TWordProcessor.Insert(Const sText: String);
Begin
  CheckNotInMacro;
  PrimaryRange.Insert(sText);
End;


Procedure TWordProcessor.SortTableDialog;
Var
  oDialog: TWPSortTableDialog;
  aRows: Array Of TStringList;
  aSortedPos: Array Of Integer;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasSelectedTable Then
    Error('SortTableDialog', 'No Table is in scope');
  If PrimaryRange.SelectedTable.Jagged Then
    Error('SortTableDialog', 'Sort is not available for jagged Table');

  oDialog := TWPSortTableDialog.Create(Owner);
  Try
    oDialog.Table := PrimaryRange.SelectedTable;
    If oDialog.Execute Then
    Begin
      SetLength(aRows, oDialog.Table.Rows.Count);
      GetSelectedTableAsTextArray(aRows);

      SetLength(aSortedPos, Length(aRows));
      SortTextArrays(oDialog.Sorts, aRows, aSortedPos);
      PrimaryRange.ReorderTableRows(aSortedPos);
    End;
  Finally
    oDialog.Free;
  End;
End;

Procedure TWordProcessor.GetSelectedTableAsTextArray(Var aRows: Array Of TStringList);
Var
  bInCell: Boolean;
  sCellData: String;
  iStart, iStop: Integer;
  iLoop: Integer;
  iRowIndex : Integer;
Begin
  iRowIndex := 0;
  bInCell := False;
  iStart := Document.Pieces.IndexByReference(PrimaryRange.SelectedTable);
  iStop := Document.Pieces.IndexByReference(PrimaryRange.SelectedTableStop);
  For iLoop := iStart To iStop Do
  Begin
    Case Document.Pieces[iLoop].PieceType Of
      ptRowStart:   aRows[iRowIndex] := TStringList.Create;
      ptRowStop:    Inc(iRowIndex);
      ptCellStart:
        Begin
          bInCell := True;
          sCellData := '';
        End;
      ptCellStop:
        Begin
          bInCell := False;
          aRows[iRowIndex].Add(sCellData);
        End;
      Else
        If bInCell Then
          sCellData := sCellData + Document.Pieces[iLoop].LogicalText;
    End;
  End;
End;

Procedure TWordProcessor.UpdateTextMetrics;
Begin
  // nothing here.
End;

Procedure TWordProcessor.DeactivateBMPDrop;
Begin
  FDropBMP.UnregisterAll;
End;

Procedure TWordProcessor.DoExit;
Begin
  Inherited;
End;

Function TWordProcessor.NoUserResponse: Boolean;
Begin
  Result := False;
End;

Function TWordProcessor.ShowCursorAlways: Boolean;
Begin
  Result := False;
End;


Procedure TWordProcessor.NotifyObserversInsert(iPoint : Integer; oPieces : TWPWorkingDocumentPieces);
Begin
End;


Procedure TWordProcessor.NotifyObserversDelete(iPoint : Integer; oPieces : TWPWorkingDocumentPieces);
Begin
End;


Procedure TWordProcessor.ActorDeleteContent(oSender: TObject; iPoint: Integer; oPieces : TWPWorkingDocumentPieces);
Begin
  NotifyObserversDelete(iPoint, oPieces);
End;

Procedure TWordProcessor.ActorInsertContent(oSender: TObject; iPoint: Integer; oPieces : TWPWorkingDocumentPieces);
Begin
  If HasPlaybackManager Then
    PlaybackManager.CloseCorrectionDialog;

  NotifyObserversInsert(iPoint, oPieces);
End;

Procedure TWordProcessor.InsertTemplate;
Var
  oBuffer : TFslBuffer;
Begin
  CheckNotInMacro;
  If Assigned(FOnTemplate) Then
  Begin
    oBuffer := FOnTemplate(Self);
    Try
      If Assigned(oBuffer) Then
        PrimaryRange.InsertTemplate(oBuffer);
    Finally
      oBuffer.Free;
    End;
  End;
End;


Procedure TWordProcessor.SetOnTemplate(Const Value: TWPTemplateEvent);
Begin
  FOnTemplate := Value;
  Settings.InsertTemplates := Assigned(FOnTemplate);
End;


Function TWordProcessor.FetchTemplateContent(Const iId : Integer; Const sCode : String; var aFormat : TWPFormat) : TFslStream;
Var
  oBuffer : TFslBuffer;
  oStream : TFslMemoryStream;
Begin
  If Not Assigned(FOnNamedTemplate) Then
    Error('FetchTemplateContent', 'template content not provided');
  oBuffer := FOnNamedTemplate(Self, iId, sCode, aFormat);
  Try
    oStream := TFslMemoryStream.Create;
    Try
      oStream.Buffer := oBuffer.Link;
      Result := oStream.Link;
    Finally
      oStream.Free;
    End;
  Finally
    oBuffer.Free;
  End;
End;


Procedure TWordProcessor.InsertField(oDefn : TWPFieldDefinitionProvider);
Var
  oField : TWPDocumentField;
  oPiece : TWPWorkingDocumentFieldStartPiece;
  oSection : TWPDocumentSection;
  oPieceS : TWPWorkingDocumentSectionStartPiece;
  bCanBeSection, bAcceptExistingContent : Boolean;
Begin
  CheckNotInMacro;
  If oDefn = Nil Then
    oDefn := FDefaultFieldDefinition;

  If oDefn = Nil Then
    SoundBeepExclamation
  Else
  Begin
    oField := TWPDocumentField.Create;
    Try
      oField.Style := PrimaryRange.Style;
      oField.Font.Assign(PrimaryRange.Font);
      bCanBeSection := canInsertFieldSection In Capabilities;
      oSection := Nil;
      If oDefn.NewField(oField, bCanBeSection, bAcceptExistingContent, oSection) Then
      Begin
        If (oSection <> Nil) Then
        Begin
          Try
            If (Not bCanBeSection) Then
              Error('InsertField', 'Cannot insert a section at this time');
            oPieceS := FTranslator.CreateSection(oSection);
            Try
              oPieceS.Namespace := oDefn.GetNamespace;
              PrimaryRange.InsertFieldSection(oPieceS, oSection.Blocks.AsText);
            Finally
              oPieceS.Free;
            End;
          Finally
            oSection.Free;
          End;
        End
        Else
        Begin
          oPiece := FTranslator.CreateField(oField);
          Try
            oPiece.Namespace := oDefn.GetNamespace;
            PrimaryRange.InsertField(oPiece, True, bAcceptExistingContent, oField.Contents.AsText);
          Finally
            oPiece.Free;
          End;
        End;
      End;
    Finally
      oField.Free;
    End;
  End;
End;



Procedure TWordProcessor.FieldPropertiesDialog;
Var
  oField : TWPWorkingDocumentFieldStartPiece;
  oTran : TWPDocumentTranslator;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasCurrentFieldStart Then
    Error('FieldPropertiesDialog', 'No Field is in scope');
  If Not PrimaryRange.CurrentFieldStart.HasDefinitionProvider Then
    Error('FieldPropertiesDialog', 'Current Field has no definition provider');
  If PrimaryRange.CurrentFieldStart.DefinitionProvider.EditField(PrimaryRange.CurrentFieldStart.DocField) Then
  Begin
    oTran := TWPDocumentTranslator.Create;
    Try
      oField := oTran.CreateField(PrimaryRange.CurrentFieldStart.DocField);
      Try
        PrimaryRange.SetFieldProperties(oField);
      Finally
        oField.Free;
      End;
    Finally
      oTran.Free;
    End;
  End;
End;

Procedure TWordProcessor.FieldSectionPropertiesDialog;
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
  oTran : TWPDocumentTranslator;
Begin
  CheckNotInMacro;
  If Not PrimaryRange.HasCurrentSectionStart Then
    Error('FieldSectionPropertiesDialog', 'No Section is in scope');
  If Not PrimaryRange.CurrentSectionStart.HasDefinitionProvider Then
    Error('FieldSectionPropertiesDialog', 'Current Section has no definition provider');
  If PrimaryRange.CurrentSectionStart.DefinitionProvider.EditField(PrimaryRange.CurrentSectionStart.DocSection) Then
  Begin
    oTran := TWPDocumentTranslator.Create;
    Try
      oSection := oTran.CreateSection(PrimaryRange.CurrentSectionStart.DocSection);
      Try
        PrimaryRange.SetSectionProperties(oSection);
      Finally
        oSection.Free;
      End;
    Finally
      oTran.Free;
    End;
  End;
End;


Procedure TWordProcessor.DefineFieldProvider(oDefinition: TWPFieldDefinitionProvider; bDefault : Boolean);
Begin
  Settings.FieldDefinitions.Add(oDefinition);
  If bDefault Then
    DefaultFieldDefinition := oDefinition.Link;
End;

Procedure TWordProcessor.DefineAnnotationProvider(oDefinition: TWPAnnotationDefinitionProvider);
Begin
  Settings.AnnotationDefinitions.Add(oDefinition);
End;

function TWordProcessor.BindToFields(oDocument: TWPWorkingDocument) : boolean;
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  result := false;
  iLoop := 0;
  while iLoop < oDocument.Pieces.Count do
  Begin
    oPiece := oDocument.Pieces[iLoop];
    If oPiece.PieceType = ptFieldStart Then
      result := BindToField(oDocument, iLoop, TWPWorkingDocumentFieldStartPiece(oPiece)) or result
    Else If oPiece.PieceType = ptSectionStart Then
      result := BindToSection(TWPWorkingDocumentSectionStartPiece(oPiece)) or result;
    inc(iLoop);
  End;
  For iLoop := 0 to oDocument.AllAnnotations.Count - 1 Do
  begin
    oDocument.AllAnnotations[iLoop].DefinitionProvider := Settings.AnnotationDefinitions.GetByName(oDocument.AllAnnotations[iLoop].Owner, nil).Link;
    if oDocument.AllAnnotations[iLoop].DefinitionProvider <> nil Then
      oDocument.AllAnnotations[iLoop].Colour := oDocument.AllAnnotations[iLoop].DefinitionProvider.GetColour(oDocument.AllAnnotations[iLoop].Text);
  End;
End;

function TWordProcessor.BindToField(oDocument: TWPWorkingDocument; cursor : integer; oPiece: TWPWorkingDocumentFieldStartPiece) : boolean;
var
  s, txt : String;
  p : TWPWorkingDocumentPiece;
Begin
  result := false;
  oPiece.BuildDocField;
  oPiece.DefinitionProvider := Settings.FieldDefinitions.GetByName(oPiece.Namespace, DefaultFieldDefinition).Link;
  if Assigned(OnUpdateField) or oPiece.HasDefinitionProvider Then
  begin
    s := '';
    p := oPiece.Next;
    while p.PieceType <> ptFieldStop do
    begin
      s := s + p.LogicalText;
      p := p.Next;
    end;
    oPiece.LastUpdateValue := s;
    if oPiece.HasDefinitionProvider and oPiece.DefinitionProvider.UpdateFieldOnLoad(oPiece.DocField, s, txt) then
    begin
      replaceFieldContent(oDocument, cursor, oPiece, txt);
      result := true;
    end
    else if assigned(OnUpdateField) then
    begin
      if oPiece.HasDefinitionProvider then
        OnUpdateField(self, fusLoading, oPiece.DefinitionProvider, oPiece.DocField, oPiece.DefinitionProvider.PreparePublicValue(oPiece.DocField, s), not oPiece.InError)
      else
        OnUpdateField(self, fusLoading, oPiece.DefinitionProvider, oPiece.DocField, s, not oPiece.InError);
    end;
  end;
  if oPiece.HasDefinitionProvider And oPiece.DefinitionProvider.HasCheckables(oPiece.DocField) Then
  Begin
    oPiece.DefinitionProvider.GetCheckables(oPiece.DocField, oPiece.Checkables);
    oPiece.BindToCheckables;
  End;
End;

function TWordProcessor.BindToSection(oPiece: TWPWorkingDocumentSectionStartPiece) : boolean;
Begin
  result := false;
  If (oPiece.IsField) Then
  Begin
    oPiece.BuildDocSection;
    oPiece.DefinitionProvider := Settings.FieldDefinitions.GetByName(oPiece.Namespace, DefaultFieldDefinition).Link;
  End;
End;

Procedure TWordProcessor.SetDefaultFieldDefinition(Const Value: TWPFieldDefinitionProvider);
Begin
  FDefaultFieldDefinition.Free;
  FDefaultFieldDefinition := Value;
End;

Procedure TWordProcessor.RegisterInspector(oInspector : TWPInspector);
Begin
  oInspector.Parent := Self;
  oInspector.AlignRight;
  FInspectors.Add(oInspector);
  Resize;
  Invalidate;
End;


Procedure TWordProcessor.RemoveInspector(oInspector : TWPInspector);
Begin
  FInspectors.Remove(oInspector);
  Resize;
  Invalidate;
End;


Function TWordProcessor.ActualWidth: Integer;
Var
  iLoop : Integer;
Begin
  Result := Width;
  For iLoop := 0 To FInspectors.Count - 1 Do
    Dec(Result, FInspectors[iLoop].Width);
End;


Procedure TWordProcessor.SnapShot(Const sCause: String; Const DoMail : Boolean);
Var
  sFilename : String;
  oMapi : TFslMapi;
Begin
  sFileName := IncludeTrailingBackslash(SystemTemp)+'wp'+DateTimeFormat(LocalDateTime, 'yyyymmddhhnnsszzz')+'.snapshot';
  ProduceSnapShot(sFilename, sCause);
  If (IsDebuggerPresent) Then
  Begin
    ExecuteOpen(sFileName+'.xml');
    ExecuteOpen(sFileName+'.kdoc');
  End
  Else
  Begin
    oMapi := TFslMapi.Create;
    Try
      oMapi.Tos.Add(Settings.SnapshotEmail);
      oMapi.Attachments.Add(sFileName+'.png');
      oMapi.Attachments.Add(sFileName+'.xml');
      oMapi.Preview := True;
      oMapi.Send;
    Finally
      oMapi.Free;
    End;
  End;
End;


Procedure TWordProcessor.ProduceSnapShot(Const sFilename, sCause: String);
Var
  oWriter : TWPSystemSnapshotWriter;
  oFile : TFslFile;
Begin
  Try
    FRenderer.SavePNG(sFilename+'.png');
  Except
    On E:Exception Do
      StringToFile(sFilename+'.png', e.Message);
  End;
  Try
    oFile := TFslFile.Create(sFilename+'.xml', fmCreate);
    Try
      oWriter := TWPSystemSnapshotWriter.Create;
      Try
        oWriter.Filename := sFilename+'.xml';
        oWriter.Stream := oFile.Link;
        oWriter.Start(sCause);
        oWriter.ProduceDocument(FDocument);
        oWriter.ProduceOperator(FOperator);
        oWriter.ProduceStyles('Configured', ConfiguredStyles);
        oWriter.ProduceStyles('Working', WorkingStyles);
        oWriter.ProduceSettings(FSettings);
        oWriter.ProduceRanges(FRangeManager, PrimaryRange);
        oWriter.ProduceRenderer(Renderer);
        DoSnapShot(oWriter);
        oWriter.Stop;
      Finally
        oWriter.Free;
      End;
    Finally
      oFile.Free;
    End;
  Except
    On E:Exception Do
      StringToFile(sFilename+'.xml', e.Message);
  End;
  Try
    FDocumentHandler.SaveNative(sFilename+'.kdoc');
  Except
    On E:Exception Do
      StringToFile(sFilename+'.kdoc', e.Message);
  End;
End;

Procedure TWordProcessor.RefreshPopup;
Begin
 // nothing
End;

procedure TWordProcessor.ScrollTo(y: Integer);
begin
  FScrollPoint.Y := IntegerMax(0, IntegerMin(MinimumRecommendedHeight - ClientHeight + (2 * Settings.VerticalMargin), Y));
  ScrollbarPositionChanged;
  DoPaint;
end;

Procedure TWordProcessor.ScrollToTop;
Begin
  FScrollPoint.Y := 0;
  ScrollbarPositionChanged;
  DoPaint;
End;

Procedure TWordProcessor.HandleError(Const sContext, sMessage: String);
Begin
  If FLastSnapshot <> FOperator.Version Then
  Begin
    SnapShot(sMessage, False);
    FLastSnapshot := FOperator.Version;
  End;
  Error(sContext, sMessage);
End;

function TWordProcessor.DoSave: Boolean;
begin
  result := assigned(FOnSave);
  if result then
    FOnSave(self);
end;

{
function TWordProcessor.DoSaveAs: Boolean;
begin
  result := assigned(FOnSaveAs);
  if result then
    FOnSaveAs(self);
end;
}

Procedure TWordProcessor.DoSnapshot(oWriter: TWPSnapshotWriter);
Var
  iLoop : Integer;
Begin
  oWriter.ProduceOpen('status');
  oWriter.ProduceText('Inspectors', IntegerToString(Inspectors.Count));
  If FontsAllowed <> Nil Then
    oWriter.ProduceText('FontsAllowed', FontsAllowed.AsCSV);
  oWriter.ProduceText('VoiceActive', BooleanToString(VoiceActive));
  If HasNextControl Then
    oWriter.ProduceText('NextControl', 'True');
  If HasPreviousControl Then
    oWriter.ProduceText('PreviousControl', 'True');
  oWriter.ProduceText('WorkingWidthLimit', IntegerToString(WorkingWidthLimit));
  oWriter.ProduceText('ScrollPoint', IntegerToString(ScrollPoint.x)+':'+IntegerToString(ScrollPoint.y));
  oWriter.ProduceText('CursorPersist', BooleanToString(CursorPersist));
  oWriter.ProduceText('CursorOutside', BooleanToString(CursorOutside));
  oWriter.ProduceText('Observers', IntegerToString(Observers.count));

  If DefaultFieldDefinition <> Nil Then
    oWriter.ProduceText('DefaultFieldDefinition', DefaultFieldDefinition.GetNamespace);

  For iLoop := 0 To Settings.FieldDefinitions.Count - 1 Do
  Begin
    oWriter.Attributes.Match['namespace'] := Settings.FieldDefinitions[iLoop].GetNamespace;
    oWriter.Attributes.Match['title'] := Settings.FieldDefinitions[iLoop].GetTitle;
    oWriter.Attributes.Match['IconIndex'] := IntegerToString(Settings.FieldDefinitions[iLoop].GetStdIconIndex);
    oWriter.Attributes.Match['TouchIconIndex'] := IntegerToString(Settings.FieldDefinitions[iLoop].GetTouchIconIndex);
    oWriter.ProduceTag('FieldDefinition');
  End;

  If (HasExistingSearch) Then
  Begin
    oWriter.Attributes.Match['Text'] := ExistingSearch.Text;
    oWriter.Attributes.Match['Direction'] := WPSEARCHDIRECTION_NAMES[ExistingSearch.Direction];
    oWriter.Attributes.Match['WholeDocument'] := BooleanToString(ExistingSearch.WholeDocument);
    oWriter.Attributes.Match['CaseSensitive'] := BooleanToString(ExistingSearch.CaseSensitive);
    oWriter.Attributes.Match['WholeWords'] := BooleanToString(ExistingSearch.WholeWords);
    oWriter.ProduceTag('ExistingSearch');
  End;

  If HasExistingReplace Then
  Begin
    oWriter.Attributes.Match['Replace'] := ExistingReplace.Replace;
    oWriter.Attributes.Match['Selection'] := BooleanToString(ExistingReplace.Selection);
    oWriter.Attributes.Match['Prompt'] := BooleanToString(ExistingReplace.Prompt);
    oWriter.ProduceTag('ExistingReplace');
  End;

  If Printer <> Nil Then
  Begin
    oWriter.Attributes.Match['Port'] := Printer.Definition.Port;
    oWriter.Attributes.Match['Driver'] := Printer.Definition.Driver;
    oWriter.Attributes.Match['Name'] := Printer.Definition.Name;
    oWriter.Attributes.Match['default'] := BooleanToString(Printer.Definition.IsDefault);
    oWriter.ProduceTag('Printer');
  End;
  If PageLayoutController <> Nil Then
  Begin
    oWriter.Attributes.Match['SpanPolicy'] := NAMES_SPAN_POLICY[PageLayoutController.SpanPolicy];
    oWriter.ProduceTag('PageController');
  End;

  oWriter.Attributes.Match['Modified'] := BooleanToString(Modified);
  oWriter.Attributes.Match['JustDoubleClicked'] := BooleanToString(JustDoubleClicked);

  oWriter.Attributes.Match['Cursor.Showing'] := BooleanToString(CursorDetails.Showing);
  oWriter.Attributes.Match['Cursor.Left'] := IntegerToString(CursorDetails.Left);
  oWriter.Attributes.Match['Cursor.Top'] := IntegerToString(CursorDetails.Top);
  oWriter.Attributes.Match['Cursor.Height'] := IntegerToString(CursorDetails.Height);
  oWriter.Attributes.Match['Cursor.Colour'] := ColourToString(CursorDetails.Colour);
  If MouseActor <> Nil Then
    oWriter.Attributes.Match['MouseActor'] := MouseActor.ClassName;
  oWriter.Attributes.Match['ResetMouseCursor'] := BooleanToString(ResetMouseCursor);
  oWriter.Attributes.Match['AltNum'] := AltNum;
  oWriter.ProduceTag('cursor');
  oWriter.ProduceClose('status');
End;

Procedure TWordProcessor.ForceRender;
Begin
  Paint;
End;


Procedure TWordProcessor.WMContextMenu(Var aMessage : TWMContextMenu);
Begin
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=42752

  aMessage.Result := 1;
End;


Type
  TConsoleUnsuitableReason = (curFont, curParagraph, curBreak, curTable, curImage, curField);
  TConsoleUnsuitableReasonSet = Set Of TConsoleUnsuitableReason;

Const
  MSG_TConsoleUnsuitableReason : Array [TConsoleUnsuitableReason] Of String = ('Unsupported Font Formatting', 'Unsupported Paragraph Formatting', 'A Page or Break', 'A Table', 'An Image', 'A Field or Section');

Function TWordProcessor.CheckOkForConsoleMode(Var sMessage: String): Boolean;
Var
  aFail : TConsoleUnsuitableReasonSet;
  aLoop : TConsoleUnsuitableReason;
  iLoop : Integer;
  oDefault : TWPStyle;
  oPiece : TWPWorkingDocumentPiece;
  oStyle : TWPStyle;
Begin
  aFail := [];

  oDefault := WorkingStyles.DefaultStyle;

  For iLoop := 0 To FDocument.Pieces.Count - 1 Do
  Begin
    oPiece := FDocument.Pieces[iLoop];
    Case oPiece.PieceType Of
     ptText :
       Begin
         oStyle := WorkingStyles.GetByNameOrDefault(oPiece.Style);
         If oStyle.WorkingFontName(oPiece.Font) <> oDefault.Font.Name Then
           Include(aFail, curFont);
         If oStyle.WorkingFontSize(oPiece.Font) <> oDefault.Font.Size Then
           Include(aFail, curFont);
         If oStyle.WorkingFontStrikethrough(oPiece.Font) = tsTrue Then
           Include(aFail, curFont);
         If oStyle.WorkingFontState(oPiece.Font) In [fsSuperscript, fsSubscript] Then
           Include(aFail, curFont);
         If oStyle.WorkingFontForeground(oPiece.Font) <> DEFAULT_FOREGROUND Then
           Include(aFail, curFont);
         If oStyle.WorkingFontBackground(oPiece.Font, MAXINT) <> MAXINT Then
           Include(aFail, curFont);
         If oStyle.WorkingFontCapitalization(oPiece.Font) In [fcsAllCaps, fcsNoCaps, fcsSmallCaps] Then
           Include(aFail, curFont);
       End;
     ptPara :
       Begin
         oStyle := WorkingStyles.GetByNameOrDefault(oPiece.Style);
         If oStyle.WorkingFontName(oPiece.Font) <> oDefault.Font.Name Then
           Include(aFail, curFont);
         If oStyle.WorkingFontSize(oPiece.Font) <> oDefault.Font.Size Then
           Include(aFail, curFont);
         If oStyle.WorkingFontStrikethrough(oPiece.Font) = tsTrue Then
           Include(aFail, curFont);
         If oStyle.WorkingFontState(oPiece.Font) In [fsSuperscript, fsSubscript] Then
           Include(aFail, curFont);
         If oStyle.WorkingFontForeground(oPiece.Font) <> DEFAULT_FOREGROUND Then
           Include(aFail, curFont);
         If oStyle.WorkingFontBackground(oPiece.Font, MAXINT) <> MAXINT Then
           Include(aFail, curFont);
         If oStyle.WorkingFontCapitalization(oPiece.Font) In [fcsAllCaps, fcsNoCaps, fcsSmallCaps] Then
           Include(aFail, curFont);

         If oStyle.WorkingParagraphAlign(TWPWorkingDocumentParaPiece(oPiece).Format) In [WordProcessorParagraphAlignmentCentre, WordProcessorParagraphAlignmentRight, WordProcessorParagraphAlignmentJustify] Then
           Include(aFail, curParagraph);
         If oStyle.WorkingParagraphLeftIndent(TWPWorkingDocumentParaPiece(oPiece).Format) <> 0 Then
           Include(aFail, curParagraph);
         If oStyle.WorkingParagraphRightIndent(TWPWorkingDocumentParaPiece(oPiece).Format) <> 0 Then
           Include(aFail, curParagraph);
         If oStyle.WorkingParagraphListType(TWPWorkingDocumentParaPiece(oPiece).Format) In [WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers] Then
           Include(aFail, curParagraph);
       End;
     ptImage : Include(aFail, curImage);
     ptFieldStart, ptFieldStop : Include(aFail, curField);
     ptLineBreak, ptBreak : Include(aFail, curBreak);
     ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop : Include(aFail, curTable);
     ptSectionStart, ptSectionStop : Include(aFail, curField);
    End;
  End;

  Result := True;
  sMessage := '';
  For aLoop := Low(TConsoleUnsuitableReason) To High(TConsoleUnsuitableReason) Do
    If aLoop In aFail Then
    Begin
      Result := False;
      sMessage := sMessage + '  '+MSG_TConsoleUnsuitableReason[aLoop] + #13#10;
    End;
End;

Function TWordProcessor.CheckOkForNoParagraphs: Boolean;
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := True;
  For iLoop := 0 To FDocument.Pieces.Count - 1 Do
  Begin
    oPiece := FDocument.Pieces[iLoop];
    If oPiece.PieceType = ptPara Then
      Result := Result And (iLoop = FDocument.Pieces.Count - 1) // last one ok
    Else
      Result := Result And (oPiece.PieceType In [ptText, ptImage, ptLineBreak, ptFieldStart, ptFieldStop]);
  End;
End;


Procedure TWordProcessor.CheckNotInMacro;
Begin
  If FMacro.Recording Then
    Raise ELibraryException.Create('Cannot perform this operation while a macro is being recorded');
End;

Procedure TWordProcessor.Ring(aColour: TColour);
Begin
  Canvas.Pen.Style := psSolid;
  Canvas.pen.Color := aColour;
  Canvas.pen.Width := 4;
  Canvas.MoveTo(0,0);
  Canvas.LineTo(Width-1, 0);
  Canvas.LineTo(Width-1, Height-1);
  Canvas.LineTo(0, Height-1);
  Canvas.LineTo(0,0);
End;

Procedure TWordProcessor.ToggleMacro;
Begin
  Case FMacro.State Of
    msIdle : FMacro.State := msRecording;
    msRecording : FMacro.State := msIdle;
    msPlaying : CheckNotInMacro;
  End;
  Invalidate;
    End;

Procedure TWordProcessor.ExecuteMacro;
Var
  i : Integer;
  oAction : TWPMacroAction;
Begin
  CheckNotInMacro;
  NonVisibleMode := False;
  FMacro.State := msPlaying;
  Try
    Try
      For i := 0 To FMacro.Actions.Count - 1 Do
      Begin
        oAction := FMacro.Actions[i];
        Case oAction.ActionType Of
          matKey : If Not ProcessKey(TWPMacroKeyAction(oAction).Key, TWPMacroKeyAction(oAction).Shift) Then
            Begin
            SoundBeepExclamation;
            Abort;
            End;
        Else
          raise EWPException.create('Unknown action type '+FMacro.Actions[i].ClassName);
        End;
      End;
    Except
      FMacro.LastError := True;
      Raise;
    End;
  Finally
    FMacro.State := msIdle;
    NonVisibleMode := False;
    Invalidate;
  End;
  End;

Procedure TWordProcessor.StartRecordingMacro;
Begin
  If FMacro.State <> msIdle Then
    raise EWPException.create('Connect record a macro');
  ToggleMacro;
End;

Procedure TWordProcessor.StopRecordingMacro;
Begin
  If FMacro.State <> msRecording Then
    raise EWPException.create('Not recording a macro');
  ToggleMacro;
End;


Procedure TWordProcessor.CheckForAllowedWord(Sender: TObject; Const Word: String; Var CheckType: TWordCheckType; Var Replacement: String);
Begin
  If FDocument.AllowedWords.ExistsByValue(lowercase(Word)) Then
    CheckType := wcAccepted;
End;

Procedure TWordProcessor.ReviewWordDialog;
Var
  oDlg : TWPAllowedWordsDialog;
  iCount : Integer;
Begin
  oDlg := TWPAllowedWordsDialog.Create(Self);
  Try
    oDlg.Words.Assign(Document.AllowedWords);
    If oDlg.Execute Then
    Begin
      For iCount := 0 To oDlg.ToDictionary.Count - 1 Do
        FSpeller.AddWord(oDlg.ToDictionary[iCount]);
      Document.AllowedWords.Assign(oDlg.Words);
      PrimaryRange.ResetSpelling;
      Renderer.Valid := False;
      Renderer.PaintAll := True;
      Renderer.Update;
      Invalidate;
    End;
  Finally
    oDlg.Free;
  End;
End;

Procedure TWordProcessor.AddAllowedWord(Sender: TObject; Const Word: String);
Begin
  FDocument.AllowedWords.Add(Word);
End;


Function TWordProcessor.GetImageTool: TImageTool;
Begin
  Result := FOperator.CurrentImageTool;
End;

Procedure TWordProcessor.SetImageTool(Const Value: TImageTool);
Begin
  FOperator.CurrentImageTool := Value;
End;

procedure TWordProcessor.CompletionBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = vkEscape Then
    FCodeCompletionListBox.Visible := false;
end;

function TWordProcessor.CheckFields(oErrors: TFslStringList): Boolean;
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  For iLoop := 0 To Document.Pieces.Count - 1 Do
  Begin
    oPiece := Document.Pieces[iLoop];
    If oPiece.PieceType = ptFieldStart Then
      CheckField(oErrors, TWPWorkingDocumentFieldStartPiece(oPiece));
  End;
  result := oErrors.Count = 0;
end;

procedure TWordProcessor.CheckField(oErrors: TFslStringList; oPiece: TWPWorkingDocumentFieldStartPiece);
begin
  if oPiece.InError Then
    if oPiece.FieldHint = '' Then
      oErrors.Add('Field "'+oPiece.Name+'" content not acceptable')
    Else
      oErrors.Add('Field "'+oPiece.Name+'" should be : '+oPiece.FieldHint)
end;

procedure TWordProcessor.DoAnnotationClick(oAnnotation: TWPWorkingAnnotation);
begin
  PrimaryRange.MoveTo(oAnnotation.OffsetStart, false);
  PrimaryRange.SelectTo(oAnnotation.OffsetEnd, true);
  EditAnnotation;
end;

procedure TWordProcessor.InsertAnnotation(oDefn: TWPAnnotationDefinitionProvider);
var
  sContent : String;
  action : TEditAnnotationResult;
Begin
  CheckNotInMacro;
  If Not (canInsertAnnotation In PrimaryRange.Capabilities) Then
    Error('InsertAnnotation', 'Cannot insert annotation here');

  sContent := '';
  action := oDefn.EditAnnotation(PrimaryRange.SelectedText, sContent);
  if action = earChanged then
    PrimaryRange.InsertAnnotation(oDefn, sContent);
end;

procedure TWordProcessor.DeleteAnnotation;
Var
  oAnnotation : TWPWorkingAnnotation;
Begin
  If Not (canManageAnnotation In PrimaryRange.Capabilities) Then
    Error('DeleteAnnotation', 'Cannot delete annotation');
  oAnnotation := FDocument.AllAnnotations.GetBySelection(Selection.WorkingSelStart, Selection.WorkingSelEnd);
  If (oAnnotation = nil) Then
    Error('DeleteAnnotation', 'Cannot delete annotation - annotation not found');

  PrimaryRange.DeleteAnnotation;
end;

procedure TWordProcessor.EditAnnotation;
Var
  oAnnotation : TWPWorkingAnnotation;
  oProvider : TWPAnnotationDefinitionProvider;
  sText : String;
  action : TEditAnnotationResult;
Begin
  If Not (canManageAnnotation In PrimaryRange.Capabilities) Then
    Error('EditAnnotation', 'Cannot edit annotation');
  oAnnotation := FDocument.AllAnnotations.GetBySelection(Selection.WorkingSelStart, Selection.WorkingSelEnd);
  If (oAnnotation = nil) Then
    Error('EditAnnotation', 'Cannot edit annotation - annotation not found');

  PrimaryRange.MoveTo(oAnnotation.OffsetStart, false);
  PrimaryRange.SelectTo(oAnnotation.OffsetEnd, true);
  oProvider := Settings.AnnotationDefinitions.GetByName(oAnnotation.Owner, nil);
  if oProvider = nil then
    raise EWPException.create('Unknown Annotation Type')
  else
  begin
    sText := oAnnotation.Text;
    action := oProvider.EditAnnotation(PrimaryRange.SelectedText, sText);
    if action = earChanged Then
      PrimaryRange.EditAnnotation(sText)
    else if action = earDelete then
      PrimaryRange.DeleteAnnotation;
  end;
end;

procedure TWordProcessor.AddAnnotation(iStart, iEnd: Integer; const sNamespace, sContent: String);
var
  oDefn : TWPAnnotationDefinitionProvider;
Begin
  CheckNotInMacro;

  oDefn := Settings.AnnotationDefinitions.GetByName(sNamespace, nil);
  if oDefn = nil then
    Error('AddAnnotation', 'Unknown Annotation Type');

  PrimaryRange.MoveTo(iStart, false);
  PrimaryRange.SelectTo(iEnd, true);

  If Not (canInsertAnnotation In PrimaryRange.Capabilities) Then
    Error('InsertAnnotation', 'Cannot insert annotation here');

  PrimaryRange.InsertAnnotation(oDefn, sContent);
end;

function TWordProcessor.InSpeechMode: Boolean;
begin
  result := false;
end;

function TWordProcessor.InTouchMode: boolean;
{$IFDEF UNICODE}
var
  flags : uLONG;
{$ENDIF}
begin
  result := {$IFDEF UNICODE}(Settings.TouchMode = wptmAlways)  or
   ((Settings.TouchMode = wptmOnTouchDevice) and (IsTouchWindow(0, @flags))){$ELSE} false {$ENDIF};
end;

procedure TWordProcessor.CheckOkForSpeech;
var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
begin
  for iLoop := 0 to FDocument.Pieces.Count - 1 Do
  begin
    oPiece := FDocument.Pieces[iLoop];
    if (oPiece.PieceType = ptFieldStart) and (TWPWorkingDocumentFieldStartPiece(oPiece).Checkables.Count > 0) then
      raise EWPException.create('Unable to start dicatation as the content includes a field with buttons - this is not supported in voice mode');
  End;
end;

function TWordProcessor.AllFieldsValid(var sError : String): boolean;
var
  oIter : TWPPieceIterator;
  oField : TWPWorkingDocumentFieldStartPiece;
begin
  oIter := TWPPieceIterator.Create;
  try
    oIter.Document := Document.Link;
    oIter.PieceTypes := [ptFieldStart];
    oIter.First;
    result := true;
    while oIter.More do
    begin
      oField := TWPWorkingDocumentFieldStartPiece(oIter.Current);
      result := result and not oField.InError;
      if not result and (sError = '') then
        if oField.DefinitionProvider <> nil then
         sError := oField.DefinitionProvider.HintForField(oField.DocField, fhmError)+' Error: '+TWPWorkingDocumentFieldStartPiece(oIter.Current).FieldError
        else
         sError := oField.FieldError;
      oIter.Next;
    end;
  finally
    oIter.Free;
  end;
end;

function TWordProcessor.ListAllFields(Namespace: String): TWPDocumentFields;
var
  oIter : TWPPieceIterator;
begin
  result := TWPDocumentFields.create;
  try
    oIter := TWPPieceIterator.Create;
    try
      oIter.Document := Document.Link;
      oIter.PieceTypes := [ptFieldStart];
      oIter.First;
      while oIter.More do
      begin
        if (Namespace = '') or (Namespace = TWPWorkingDocumentFieldStartPiece(oIter.Current).Namespace) then
          result.add(TWPWorkingDocumentFieldStartPiece(oIter.Current).DocField.Link);
        oIter.Next;
      end;
    finally
      oIter.Free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TWordProcessor.GetContentForField(oField: TWPDocumentField): String;
var
  oIter : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  found : boolean;
begin
  oIter := TWPPieceIterator.Create;
  try
    oIter.Document := Document.Link;
    oIter.PieceTypes := [ptFieldStart];
    oIter.First;
    found := false;
    while oIter.More and not found do
    begin
      if (TWPWorkingDocumentFieldStartPiece(oIter.Current).DocField = oField) then
      begin
        found := true;
        oPiece := oIter.Current.Next;
        result := '';
        while (oPiece.PieceType <> ptFieldStop) do
        begin
          result := result + oPiece.LogicalText;
          oPiece := oPiece.Next;
        end;
      end;
      oIter.Next;
    end;
  finally
    oIter.Free;
  end;
end;





{ TWPInspectorList }

function TWPInspectorList.GetPanelByIndex(const iIndex: Integer): TWPInspector;
begin
  Result := TWPInspector(Items[iIndex]);
end;

function TWPInspectorList.ItemClass: TComponentClass;
begin
  Result := TWPInspector;
end;

{ TWPInspector }

function TWPInspector.DesiredWidth: Integer;
begin
  Result := 200;
end;

procedure TWPInspector.Initialise;
Var
  oButton : TUixButton;
begin
  inherited;
  BorderWidth := 2;
  AlignRight;
  Width := DesiredWidth;

  FHeader := TUixPanel.Create(Self);
  FHeader.Parent := Self;
  FHeader.Height := 20;
  FHeader.Caption := InspectorCaption;
  FHeader.Color := clActiveCaption;
  FHeader.Font.Color := clCaptionText;
  FHeader.Font.Style := [fsBold];
  FHeader.AlignTop;


  oButton := TUixButton.Create(FHeader);
  oButton.Left := DesiredWidth - 70;
  oButton.Height := 20;
  oButton.Width := 66;
  oButton.Top := 0;
  oButton.Caption := 'Close';
  oButton.OnClick := WantClose;

  FFooter := TUixPanel.Create(Self);
  FFooter.Parent := Self;
  FFooter.Height := 20;
  FFooter.Caption := '';
  FFooter.Color := clBtnFace;
  FFooter.AlignBottom;

  FFooterCaption := TUixLabel.Create(self);
  FFooterCaption.Parent := FFooter;
  FFooterCaption.Top := 2;
  FFooterCaption.Left := 2;
  FFooterCaption.Font.Color := clBtnText;
  FFooterCaption.Font.Style := [];
  FFooterCaption.Caption := '--';
End;

function TWPInspector.InspectorCaption: String;
begin
  Result := 'Caption';
end;

procedure TWPInspector.WantClose(oSender: TObject);
begin
end;


{ TWPPropertyInspector }

Procedure TWPPropertyInspector.Initialise;
Begin
  Inherited;
  FAllProperties := TFslStringObjectMatch.Create;
End;

Procedure TWPPropertyInspector.Setup;
Var
  oDefn : TWPPropertyFieldDefinitionProvider;
Begin
  FDisplay := TWordProcessor.Create(Self);
  FDisplay.Parent := Self;
  FDisplay.AlignClient;
  oDefn := TWPPropertyFieldDefinitionProvider.Create(FDisplay);
  Try
    oDefn.OnCommit := CommitField;
    FDisplay.DefineFieldProvider(oDefn.Link, False);
  Finally
    oDefn.Free;
  End;
  FDisplay.ShowHint := False;
  FDisplay.Settings.Hold;
  Try
    FDisplay.Settings.AllowPopup := True;
    FDisplay.Settings.Images := True;
    FDisplay.Settings.Margin := 2;
    FDisplay.Settings.TableBorders := False;
    FDisplay.Settings.ShowDocumentInspector := False;
    FDisplay.Settings.Background := clWhite; // $FFD5CF;
    FDisplay.Settings.LinkColour := clAqua;
    FDisplay.Settings.HoverColour := clLime;
    FDisplay.Settings.Pagination := False;
    FDisplay.Settings.FieldWrappers := wpfpInvisible;
    FDisplay.Settings.NoSelectReadOnly := True;
  Finally
    FDisplay.Settings.Release;
  End;
  FDisplay.WorkingStyles.DefaultStyle.Font.Name := 'Verdana';
  FDisplay.WorkingStyles.DefaultStyle.Font.Size := 8;
  FDisplay.OnSelectionChanged := SelectionChanged;
End;


Procedure TWPPropertyInspector.ProcessList(oBuilder: TWPDocumentBuilder; oProps : TWPPropertyList; oOwner: TWPDocumentTableRow);
Var
  iLoop : Integer;
  oProp : TWPProperty;
  oRow : TWPDocumentTableRow;
  oCell : TWPDocumentTableCell;
  oField : TWPDocumentField;
Begin
  For iLoop := 0 To oProps.Count - 1 Do
  Begin
    oProp := oProps[iLoop];
    If oProp.Kind = pkGroup Then
    Begin
      oRow := oBuilder.StartTableRow(oOwner, ReadOnlyTrue);
      oRow.TopBorder.SimpleDot;
      oCell := oBuilder.StartTableCell(2, ReadOnlyTrue);
      oCell.Background := clGray;
      oCell.MarginLeft := 4;
      oCell.MarginTop := 1;
      oCell.MarginBottom := 1;
      oBuilder.StartParagraph;
      oBuilder.AddText(oProp.Name, True, False, 8);
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;
      oBuilder.EndTableRow();
      ProcessList(oBuilder, oProp.Children, oRow);
    End
    Else
    Begin
      oBuilder.StartTableRow(oOwner, ReadOnlyTrue);
      oCell := oBuilder.StartTableCell(1, ReadOnlyTrue);
      oCell.Width := 0.5;
      oCell.MarginLeft := 4;
      oCell.MarginTop := 1;
      oCell.MarginBottom := 1;
      If (iLoop > 0) Then
        oCell.TopBorder.SimpleDot;
      oBuilder.StartParagraph;
      oBuilder.AddText(oProp.Name, False, False, 8);
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;
      oCell := oBuilder.StartTableCell(1, ReadOnlyTrue);
      oCell.Width := 0.5;
      If (iLoop > 0) Then
        oCell.TopBorder.SimpleDot;
      oCell.MarginLeft := 1;
      oCell.MarginTop := 1;
      oCell.MarginBottom := 1;
      oBuilder.StartParagraph;
      If oProp.Editable Then
      Begin
        oCell.Background := clWhite;
        oField := oBuilder.StartField;
        oField.ReadOnly := ReadOnlyFalse;
        oField.Namespace := NS_FIELD_PROPERTY;
        oField.Name := IntegerToString(oProp.Id);
        FAllProperties.Add(oField.Name, oProp.Link);
        oField.Font.Size := 8;
      End;
      oBuilder.AddText(oProp.Value, False, False, 8);
      If oProp.Editable Then
        oBuilder.EndField;
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;
      oBuilder.EndTableRow();
    End;
  End;
End;

Procedure TWPPropertyInspector.WPChange(oSender: TObject);
Var
  oBuilder : TWPDocumentBuilder;
  oTable : TWPDocumentTable;
  oProps : TWPPropertyList;
  sName : String;
Begin
  If FDisplay = Nil Then
    Setup;

  If FDisplay.PrimaryRange.HasCurrentFieldStart And (FDisplay.PrimaryRange.CurrentFieldStart.Namespace = NS_FIELD_PROPERTY) Then
    sName := FDisplay.PrimaryRange.CurrentFieldStart.Name;

  oBuilder := TWPDocumentBuilder.Create;
  Try
    oBuilder.Document := TWPDocument.Create;
    oBuilder.Document.Styles := FDisplay.WorkingStyles.Link;

    oBuilder.Start;

    oTable := oBuilder.StartTable(ReadOnlyTrue);
    oTable.BorderPolicy := BorderPolicyCustom;

    FAllProperties.clear;

    oProps := WordProcessor.GetProperties;
    Try
      ProcessList(oBuilder, oProps, Nil);
    Finally
      oProps.Free;
    End;
    oBuilder.EndTable;
    oBuilder.Stop;
    FDisplay.DocumentHandler.LoadDocument(oBuilder.Document);
  Finally
    oBuilder.Free;
  End;
  If sName <> '' Then
    FDisplay.GotoFieldByName(NS_FIELD_PROPERTY, sName);
End;

Procedure TWPPropertyInspector.SelectionChanged(oSender: TObject);
Begin
  FFooterCaption.Caption := FDisplay.SelectionSummary + ' ['+FDisplay.Selection.Summary+']';
End;

Procedure TWPPropertyInspector.CommitField(oField: TWPDocumentField; Const sContent: String);
Var
  oProp : TWPProperty;
Begin
  If (oField.Namespace = NS_FIELD_PROPERTY) Then
  Begin
    oProp := TWPProperty(FAllProperties.GetValueByKey(oField.Name));
    If oProp <> Nil Then
    Begin
      Try
        WordProcessor.SetProperty(oProp, sContent);
      Except
        On E : Exception Do
          MessageDlg('Error Setting '+oProp.Name+' value to "'+sContent+'": '+e.Message, mtError, [mbok], 0);
      End;
    End;
  End;
  WordProcessor.SetFocus;
End;

Procedure TWPPropertyInspector.Finalise;
Begin
  FAllProperties.Free;
  Inherited;
End;

Function TWPPropertyInspector.DesiredWidth: Integer;
Begin
  Result := 220;
End;

Function TWPPropertyInspector.InspectorCaption: String;
Begin
  Result := 'Document Properties';
End;


Procedure TWPLinkedInspector.SetWordProcessor(Const Value: TWordProcessor);
Begin
  If FWordProcessor <> Nil Then
    FWordProcessor.UnregisterObserver(Self);

  FWordProcessor := Value;

  If FWordProcessor <> Nil Then
  Begin
    FWordProcessor.RegisterObserver(Self, WPChange);
    WPChange(FWordProcessor);
  End;
End;


Procedure TWPLinkedInspector.WantClose(oSender: TObject);
Var
  oWordProcessor : TWordProcessor;
Begin
  oWordProcessor := WordProcessor;
  WordProcessor := Nil;
  oWordProcessor.RemoveInspector(Self);
  Free;
End;


Procedure TWPLinkedInspector.WPChange(oSender: TObject);
Begin
End;

{ TWPTouchManager }

constructor TWPTouchManager.create(owner: TWordProcessor);
begin
  inherited Create;
  FLoaded := false;
  FOwner := owner;
end;

destructor TWPTouchManager.destroy;
begin
  Unload;
  FGestures.Free;
  inherited;
end;

Const
  FLAGNAMES : array [TInteractiveGestureFlag] of string = ('begin', 'inertia', 'end');

function flagsToString(flags: TInteractiveGestureFlags):String;
var
  a : TInteractiveGestureFlag;
begin
  result:= '';
  for a := Low(TInteractiveGestureFlag) to High(TInteractiveGestureFlag) do
    if (a in flags)then
      result := result + ',' + FLAGNAMES[a];
  result := copy(result,2,100);
end;

function gesturetostr(i:integer):string;
begin
  if i = igiZoom then
    result := 'Zoom'
  else if i=igiPan then
    result := 'Pan'
  else if i = igiRotate then
    result := 'Rotate'
  else if i = igiTwoFingerTap then
    result := 'TwoFingerTap'
  else if i = igiPressAndTap then
    result := 'PressAndTap'
  else
    result := inttostr(i);
end;


procedure TWPTouchManager.DoGesture(sender: TObject;  const EventInfo: TGestureEventInfo; var Handled: boolean);
begin
  case EventInfo.GestureID of
    igiZoom : handled := Zoom(EventInfo);
    igiPan : handled := Pan(EventInfo);
    igiTwoFingerTap : handled := TwoFingers(EventInfo);
  else
    Handled := false;
  end;
end;

procedure TWPTouchManager.Load;
begin
  if not FLoaded then
  begin
    FLoaded := true;
   // FGestures := TGestureManager.create(FOwner);
    FOwner.OnGesture := DoGesture;
   // FOwner.Touch.GestureManager := FGestures;
    //FGestures.StandardGestures[FOwner] := [sgLeft];
    //FGestures.FindGesture(FOwner, sgiLeft).Action := FAction;
    FOwner.Touch.InteractiveGestureOptions := [{igoPanSingleFingerHorizontal, }igoPanSingleFingerVertical, igoPanInertia{, igoPanGutter, igoParentPassthrough}];
    FOwner.Touch.InteractiveGestures := [igZoom, igPan{, igRotate, }, igTwoFingerTap{, igPressAndTap}];
    FOwner.Touch.TabletOptions := [toPressAndHold];
 //   FOwner.perform(CM_TABLETOPTIONSCHANGED, 0 ,0);
  end;
end;

function TWPTouchManager.Pan(EventInfo: TGestureEventInfo): boolean;
begin
  if gfBegin in EventInfo.Flags then
  begin
    FPanStart := EventInfo.Location.Y;
    FPanBase := FOwner.ScrollPoint.Y;
  end
  else
  begin
    FOwner.ScrollTo(FPanBase-(EventInfo.Location.Y - FPanStart));
    writeln('Pan: '+ inttostr(EventInfo.Location.Y - FPanStart));
  end;
//    FOwner.Settings.Scale := Min(Max(FZoomBase *(EventInfo.Distance / FZoomStart), 0.1), 10);
  {
  writeln('Loc = '+inttostr(EventInfo.Location.X)+', '+inttostr(EventInfo.Location.y)+
   '. flags = '+Flagstostring(EventInfo.Flags)+'. angle = '+floatToStr(EventInfo.Angle)+
   '. InertiaVector = '+inttostr(EventInfo.InertiaVector.X)+', '+inttostr(EventInfo.InertiaVector.y)+
   '. Distance = '+inttostr(EventInfo.Distance)+
   '. taplocation = '+inttostr(EventInfo.TapLocation.X)+', '+inttostr(EventInfo.TapLocation.y));
   }
end;

function TWPTouchManager.TwoFingers(EventInfo: TGestureEventInfo): boolean;
begin
  FOwner.ShowPopupMenu();
end;

procedure TWPTouchManager.Unload;
begin
  if FLoaded then
  begin
    FLoaded := false;
    FOwner.Touch.GestureManager := nil;
  end;
end;

function TWPTouchManager.Zoom(EventInfo: TGestureEventInfo): boolean;
begin
  if gfBegin in EventInfo.Flags then
  begin
    FZoomStart := EventInfo.Distance;
    FZoomBase := FOwner.Settings.Scale;
  end
  else
    FOwner.Settings.Scale := Min(Max(FZoomBase *(EventInfo.Distance / FZoomStart), 0.5), 10);
end;


{ TWPSystemSnapshotWriter }

Procedure TWPSystemSnapshotWriter.ProduceRenderer(oRenderer : TWPScreenRenderer);
Var
  sText : String;
  iLoop : Integer;
Begin
  Attributes.Match['LastVersion'] := IntegerToString(oRenderer.LastVersion);
  Attributes.Match['SomeSelected'] := BooleanToString(oRenderer.SomeSelected);
  Attributes.Match['Width'] := IntegerToString(oRenderer.Width);
  Attributes.Match['SectionHeaderHeight'] := IntegerToString(oRenderer.SectionHeaderHeight);
  Attributes.Match['SectionFieldCharWidth'] := IntegerToString(oRenderer.SectionFieldCharWidth);
  Attributes.Match['SectionFieldCharHeight'] := IntegerToString(oRenderer.SectionFieldCharHeight);
  Attributes.Match['FieldFontSize'] := IntegerToString(oRenderer.FieldFontSize.cx)+':'+IntegerToString(oRenderer.FieldFontSize.cy);
  Attributes.Match['FieldDescent'] := IntegerToString(oRenderer.FieldDescent);
  Attributes.Match['MeasureAll'] := BooleanToString(oRenderer.MeasureAll);
  Attributes.Match['PaintAll'] := BooleanToString(oRenderer.PaintAll);
  Attributes.Match['Valid'] := BooleanToString(oRenderer.Valid);
  Attributes.Match['Working'] := BooleanToString(oRenderer.Working);
  Attributes.Match['AdjustTop'] := IntegerToString(oRenderer.AdjustTop);
  Attributes.Match['AdjustOffset'] := IntegerToString(oRenderer.AdjustOffset);
  Attributes.Match['CurrentHotspot'] := GetId(oRenderer.CurrentHotspot);
  Attributes.Match['CurrentButton'] := GetId(oRenderer.CurrentButton);

  Attributes.Match['canvas-StatedWidth'] := IntegerToString(oRenderer.Canvas.StatedWidth);
  Attributes.Match['canvas-StatedHeight'] := IntegerToString(oRenderer.Canvas.StatedHeight);
  Attributes.Match['canvas-InternalTop'] := IntegerToString(oRenderer.Canvas.InternalTop);
  Attributes.Match['canvas-InternalBottom'] := IntegerToString(oRenderer.Canvas.InternalBottom);
  Attributes.Match['canvas-HorizontalMargin'] := IntegerToString(oRenderer.Canvas.HorizontalMargin);
  Attributes.Match['canvas-VerticalMargin'] := IntegerToString(oRenderer.Canvas.VerticalMargin);
  Attributes.Match['canvas-Background'] := ColourToString(oRenderer.Canvas.Background);
  Attributes.Match['canvas-Contrast'] := RealToString(oRenderer.Canvas.Contrast);

  ProduceOpen('renderer');
  ProduceBorder('defaultBorder', oRenderer.DefaultTableBorder);
  ProduceContainer(oRenderer.Map);
  sText := '';
  For iLoop := Low(oRenderer.PageOffsets) To High(oRenderer.PageOffsets) Do
    sText := sText + IntegerToString(oRenderer.PageOffsets[iLoop])+ ' ';
  ProduceText('PageOffsets', sText);
  sText := '';
  For iLoop := Low(oRenderer.PagePositions) To High(oRenderer.PagePositions) Do
    sText := sText + IntegerToString(oRenderer.PagePositions[iLoop])+ ' ';
  ProduceText('PagePositions', sText);

  ProduceClose('renderer');
End;

Procedure TWPSystemSnapshotWriter.ProduceRanges(oRanges: TWPRangeManager; oPrimary: TWPVisualRange);
Var
  iLoop : Integer;
Begin
  ProduceOpen('Ranges');
  For iLoop := 0 To oRanges.List.Count - 1 Do
  Begin
    If oRanges.List[iLoop] = oPrimary Then
    Begin
      Attributes.Match['PageHeight'] := IntegerToString(oPrimary.PageHeight);
      Attributes.Match['Primary'] := 'True';
    End;
    ProduceRange(oRanges.List[iLoop]);
  End;
  ProduceClose('Ranges');
  ProduceOpen('Log');
  for iLoop := 0 to oPrimary.Log.Count - 1 do
  begin
    Attributes.Match['Time'] := oPrimary.Log[iLoop].Time;
    Attributes.Match['Action'] := oPrimary.Log[iLoop].Action;
    Attributes.Match['Selection'] := oPrimary.Log[iLoop].Selection;
    Attributes.Match['Details'] := oPrimary.Log[iLoop].Details;
    Attributes.Match['Outcome'] := oPrimary.Log[iLoop].outcome;
    ProduceTag('Action');
  end;
  ProduceClose('Log');
End;


Procedure TWPSystemSnapshotWriter.ProduceRange(oRange: TWPRange);
Var
  aLoop : TWPCapability;
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['rangeId'] := IntegerToString(oRange.Id);
  Attributes.Match['Style'] := oRange.Style;

  ProduceOpen('Range');

  sText := '';
  For aLoop := Low(TWPCapability) To High(TWPCapability) Do
    sText := sText + WPCAPABILITY_NAMES[aLoop]+' ';
  ProduceText('Capabilities', sText);

  ProduceFormat(oRange.Font);
  ProduceParaFormat(oRange.Paragraph);
  ProduceSelection(oRange.Selection);

  If oRange.HasCurrentParagraph Then
    Attributes.Match['CurrentParagraph'] := GetId(oRange.CurrentParagraph);
  If oRange.HasCurrentImage Then
    Attributes.Match['CurrentImage'] := GetId(oRange.CurrentImage);
  If oRange.HasCurrentLine Then
    Attributes.Match['CurrentLine'] := GetId(oRange.CurrentLine);
  If oRange.HasCurrentSectionStart Then
    Attributes.Match['CurrentSectionStart'] := GetId(oRange.CurrentSectionStart);
  If oRange.HasCurrentSectionStop Then
    Attributes.Match['CurrentSectionStop'] := GetId(oRange.CurrentSectionStop);
  If oRange.HasCurrentFieldStart Then
    Attributes.Match['CurrentFieldStart'] := GetId(oRange.CurrentFieldStart);
  If oRange.HasCurrentFieldStop Then
    Attributes.Match['CurrentFieldStop'] := GetId(oRange.CurrentFieldStop);
  If oRange.HasCurrentTableStart Then
    Attributes.Match['CurrentTableStart'] := GetId(oRange.CurrentTableStart);
  If oRange.HasCurrentTableStop Then
    Attributes.Match['CurrentTableStop'] := GetId(oRange.CurrentTableStop);
  If oRange.HasCurrentTableRowStart Then
    Attributes.Match['CurrentTableRowStart'] := GetId(oRange.CurrentTableRowStart);
  If oRange.HasCurrentTableRowStop Then
    Attributes.Match['CurrentTableRowStop'] := GetId(oRange.CurrentTableRowStop);
  If oRange.HasCurrentTableCellStart Then
    Attributes.Match['CurrentTableCellStart'] := GetId(oRange.CurrentTableCellStart);
  If oRange.HasCurrentTableCellStop Then
    Attributes.Match['CurrentTableCellStop'] := GetId(oRange.CurrentTableCellStop);
  If oRange.HasSelectedTable Then
    Attributes.Match['SelectedTable'] := GetId(oRange.SelectedTable);

  sText := '';
  For iLoop := 0 To oRange.SelectedRows.Count - 1 Do
    sText := sText + GetId(oRange.SelectedRows[iLoop])+' ';
  Attributes.Match['SelectedRows'] := sText;

  sText := '';
  For iLoop := 0 To oRange.SelectedCells.Count - 1 Do
    sText := sText + GetId(oRange.SelectedCells[iLoop])+' ';
  Attributes.Match['SelectedCells'] := sText;
  ProduceTag('Current');
  ProduceClose('Range');
End;


Procedure TWPSystemSnapshotWriter.ProduceSelection(oSelection: TWPSelection);
Begin
  Attributes.Match['SelStart'] := IntegerToString(oSelection.SelStart);
  Attributes.Match['Cursor'] := IntegerToString(oSelection.Cursor);
  Attributes.Match['SelEnd'] := IntegerToString(oSelection.SelEnd);
  Attributes.Match['Desired'] := IntegerToString(oSelection.Desired);
  ProduceTag('Selection');
End;

Procedure TWPSystemSnapshotWriter.ProduceOperator(oOperator: TWPOperator);
Var
  iLoop : Integer;
Begin
  Attributes.Match['MasterRangeId'] := IntegerToString(oOperator.MasterRangeId);
  Attributes.Match['Version'] := IntegerToString(oOperator.Version);
  Attributes.Match['CurrentOp'] := BooleanToString(oOperator.CurrentOp <> Nil);
  Attributes.Match['Status'] := NAMES_WPOPERATIONSTATUS[oOperator.Status];
  Attributes.Match['RendererRange.Valid'] := BooleanToString(oOperator.RendererRange.Valid);
  Attributes.Match['RendererRange.Start'] := IntegerToString(oOperator.RendererRange.Start);
  Attributes.Match['RendererRange.Stop'] := IntegerToString(oOperator.RendererRange.Stop);
  Attributes.Match['MaxUndoDepth'] := IntegerToString(oOperator.MaxUndoDepth);
  Attributes.Match['DirectCount'] := IntegerToString(oOperator.DirectCount);
  Attributes.Match['DirectText'] := oOperator.DirectText;
  Attributes.Match['LastAction'] := IntegerToString(oOperator.LastAction);
  ProduceOpen('operator');

  ProduceOpen('Undo');
  For iLoop := 0 To oOperator.UndoStack.Count - 1 Do
    ProduceOperation(oOperator.UndoStack[iLoop]);
  ProduceClose('Undo');

  ProduceOpen('Redo');
  For iLoop := 0 To oOperator.RedoStack.Count - 1 Do
    ProduceOperation(oOperator.RedoStack[iLoop]);
  ProduceClose('Redo');

  ProduceClose('operator');
End;

Procedure TWPSystemSnapshotWriter.ProduceOperation(oOperation: TWPOperation);
Var
  iLoop : Integer;
Begin
  Attributes.Match['RangeId'] := IntegerToString(oOperation.RangeId);
  Attributes.Match['Closed'] := BooleanToString(oOperation.Closed);
  Attributes.Match['OpType'] := NAMES_WPOPERATIONTYPE[oOperation.OpType];
  Attributes.Match['Start'] := IntegerToString(oOperation.Start);
  Attributes.Match['CommencementFinish'] := IntegerToString(oOperation.CommencementFinish);
  Attributes.Match['TerminationFinish'] := IntegerToString(oOperation.TerminationFinish);
  Attributes.Match['MoveDestination'] := IntegerToString(oOperation.MoveDestination);
  Attributes.Match['UndoCursor'] := oOperation.UndoCursor;
  Attributes.Match['RedoCursor'] := oOperation.RedoCursor;
  Attributes.Match['AddedAmount'] := IntegerToString(oOperation.AddedAmount);
  Attributes.Match['AddedAmountThisIteration'] := IntegerToString(oOperation.AddedAmountThisIteration);
  Attributes.Match['DeletedAmount'] := IntegerToString(oOperation.DeletedAmount);
  Attributes.Match['Reiterating'] := BooleanToString(oOperation.Reiterating);
  Attributes.Match['AddedText'] := oOperation.AddedText;
  Attributes.Match['AddedTextThisIteration'] := oOperation.AddedTextThisIteration;
  ProduceOpen('Operation');

  ProduceOpen('OriginalPieces');
  For iLoop := 0 To oOperation.OriginalPieces.Count - 1 Do
    ProducePiece(oOperation.OriginalPieces[iLoop]);
  ProduceClose('OriginalPieces');

  ProduceOpen('ModifiedPieces');
  For iLoop := 0 To oOperation.ModifiedPieces.Count - 1 Do
    ProducePiece(oOperation.ModifiedPieces[iLoop]);
  ProduceClose('ModifiedPieces');

  ProduceOpen('UndoCursors');
  For iLoop := 0 To oOperation.UndoCursors.Count - 1 Do
    ProduceText(oOperation.UndoCursors.KeyByIndex[iLoop], IntegerToString(oOperation.UndoCursors.ValueByIndex[iLoop]));
  ProduceClose('UndoCursors');
  ProduceClose('Operation');
End;

Procedure TWPSystemSnapshotWriter.ProduceSettings(oSettings : TWPSettings);
Begin
  ProduceOpen('settings');
  ProduceText('Background', ColourToString(oSettings.Background));
  ProduceText('LowLight', BooleanToString(oSettings.LowLight));
  ProduceText('Margin', IntegerToString(oSettings.Margin));
  ProduceText('HorizontalMargin', IntegerToString(oSettings.HorizontalMargin));
  ProduceText('VerticalMargin', IntegerToString(oSettings.VerticalMargin));
  ProduceText('Scale', RealToString(oSettings.Scale));
  ProduceText('TableBorders', BooleanToString(oSettings.TableBorders));
  ProduceText('NestingIndent', IntegerToString(oSettings.NestingIndent));
  ProduceText('Pagination', BooleanToString(oSettings.Pagination));
  ProduceText('LinkColour', ColourToString(oSettings.LinkColour));
  ProduceText('HoverColour', ColourToString(oSettings.HoverColour));
  ProduceText('Hotspots', NAMES_WPHotspotMode[oSettings.Hotspots]);
  ProduceText('EditHints', BooleanToString(oSettings.EditHints));
  ProduceText('SpellingErrors', BooleanToString(oSettings.SpellingErrors));
  ProduceText('FieldWrappers', NAMES_WPFieldPresentation[oSettings.FieldWrappers]);
  ProduceText('Interactive', BooleanToString(oSettings.Interactive));
  ProduceText('ReadOnly', BooleanToString(oSettings.ReadOnly));
  ProduceText('Images', BooleanToString(oSettings.Images));
  ProduceText('ImageMapEditing', BooleanToString(oSettings.ImageMapEditing));
  ProduceText('Format', BooleanToString(oSettings.Format));
  ProduceText('Selecting', BooleanToString(oSettings.Selecting));
  ProduceText('Blinking', BooleanToString(oSettings.Blinking));
  ProduceText('ShowVerticalScrollbar', BooleanToString(oSettings.ShowVerticalScrollbar));
  ProduceText('Search', BooleanToString(oSettings.Search));
  ProduceText('AllowPopup', BooleanToString(oSettings.AllowPopup));
  ProduceText('SnapshotEmail', oSettings.SnapshotEmail);
  ProduceText('ShowDocumentInspector', BooleanToString(oSettings.ShowDocumentInspector));
  ProduceText('NoSelectReadOnly', BooleanToString(oSettings.NoSelectReadOnly));
  ProduceText('InsertTemplates', BooleanToString(oSettings.InsertTemplates));
  ProduceText('AutosavePath', oSettings.AutosavePath);
  ProduceText('AutosaveFrequency', IntegerToString(oSettings.AutosaveFrequency));
  ProduceText('AutosaveId', oSettings.AutosaveId);
  ProduceClose('settings');
End;


End.



