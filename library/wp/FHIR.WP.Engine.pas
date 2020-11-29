Unit FHIR.WP.Engine;

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
  Windows, SysUtils, Classes, Graphics, StdCtrls,
  IdUri,

  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_shell, fsl_wininet, fsl_xml,

  fui_vclx_base,
  wp_graphics, wp_clipboard,
  wp_definers, wp_types, wp_document, wp_working, FHIR.WP.v2Ft, wp_format, wp_factory,
  FHIR.WP.Settings, FHIR.WP.Spelling;

 Type
  TImageTool = (itSelect, itLine, itRectangle, itCircle, itMark, itZoom);

  TWPOperationType = (otChange, otInsert, otDelete, otCharInsert, otMove, otTable, otTrim);
  {otCharInsert is a special case to allow for reopening an existing operation}
  {otTrim is a selective delete within the range}

  TWPOperation = Class (TFslObject)
  Private
    FRangeId : Integer;
    FClosed: Boolean;
    FOpType: TWPOperationType;

    // range control
    FStart : Integer;
    // range at commencement is the range from which content was changed,
    // the content from this range can be found in FPieces
    FCommencementFinish : Integer;
    // range at termination was the range in which content was changed
    // after undo this range can be found in FRedoPieces
    FTerminationFinish : Integer;
    // if the operation was a move, then FTerminationFinish = FTerminationStart,
    // and the content from commencement is found at MoveDestination.
    // MoveDestination is correct after FStart - FCommencementFinish have been deleted from the document
    FMoveDestination : Integer;

    // cursor before operation was created - will be restored after undo
    FUndoCursor: String;
    // cursor before operation was undone, will be restored after redo
    FRedoCursor: String;

    // undo cursors for other ranges
    FUndoCursors : TFslStringIntegerMatch;

    // content restored when undoing
    FOriginalPieces : TWPWorkingDocumentPieces;
    // content to be restored when redoing, blank until undo
    FModifiedPieces : TWPWorkingDocumentPieces;
    FInsertedPieces : TWPWorkingDocumentPieces;
    // content tracking
    FRemovedPieces : TWPWorkingDocumentPieceTrackers;
    FTablePieces : TWPWorkingDocumentPieceTrackers;

    // tracking content being added for otInsert, otCharInsert, Termination will be calculated based on this
    FAddedAmount: Integer;
    FAddedAmountThisIteration : Integer;
    FDeletedAmount: Integer; // for relating ranges to each other
    FReiterating : Boolean;
    FAddedText : String;
    FAddedTextThisIteration : String;

    Function GetOriginalPieces : TWPWorkingDocumentPieces;
    Function GetModifiedPieces : TWPWorkingDocumentPieces;
    Function GetInsertedPieces : TWPWorkingDocumentPieces;
    Function GetRemovedPieces : TWPWorkingDocumentPieceTrackers;
    Function GetTablePieces : TWPWorkingDocumentPieceTrackers;

    Procedure SetMoveDestination(Const Value : Integer);
    Procedure SetOriginalPieces(Const Value : TWPWorkingDocumentPieces);
    Procedure SetModifiedPieces(Const Value : TWPWorkingDocumentPieces);
    Procedure SetInsertedPieces(Const Value : TWPWorkingDocumentPieces);
    Procedure SetRemovedPieces(Const Value : TWPWorkingDocumentPieceTrackers);
    Procedure SetTablePieces(Const Value : TWPWorkingDocumentPieceTrackers);
    Function GetUndoCursors: TFslStringIntegerMatch;
    Function GetUndoText: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;
    Function Link : TWPOperation;

    Procedure Assign(oSource : TFslObject); Override;

    Function AsText : String;
    Function OriginalLength : Integer;
    Function HasUndoCursors : Boolean;
    Procedure ReIterate;

    Procedure Add(iAmount : Integer; Const sText : String);
    Procedure Remove(iAmount : Integer; Const sText : String);

    Property RangeId : Integer Read FRangeId Write FRangeId;
    Property OpType: TWPOperationType Read FOpType Write FOpType;
    Property Closed : Boolean Read FClosed Write FClosed;
    Property Start : Integer Read FStart Write FStart;
    Property CommencementFinish : Integer Read FCommencementFinish Write FCommencementFinish;
    Property TerminationFinish : Integer Read FTerminationFinish Write FTerminationFinish;
    Property MoveDestination : Integer Read FMoveDestination Write SetMoveDestination;
    Property AddedAmount: Integer Read FAddedAmount;
    Property AddedAmountThisIteration: Integer Read FAddedAmountThisIteration;
    Property UndoText : String Read GetUndoText;
    Property AddedText: String Read FAddedText;
    Property AddedTextThisIteration: String Read FAddedTextThisIteration;
    Property DeletedAmount: Integer Read FDeletedAmount Write FDeletedAmount;
    Property UndoCursor: String Read FUndoCursor Write FUndoCursor;
    Property RedoCursor: String Read FRedoCursor Write FRedoCursor;
    Property UndoCursors: TFslStringIntegerMatch Read GetUndoCursors;
    Property OriginalPieces : TWPWorkingDocumentPieces Read GetOriginalPieces Write SetOriginalPieces;
    Property ModifiedPieces : TWPWorkingDocumentPieces Read GetModifiedPieces Write SetModifiedPieces;
    Property InsertedPieces : TWPWorkingDocumentPieces Read GetInsertedPieces Write SetInsertedPieces;
    Property RemovedPieces : TWPWorkingDocumentPieceTrackers Read GetRemovedPieces Write SetRemovedPieces;
    Property TablePieces : TWPWorkingDocumentPieceTrackers Read GetTablePieces Write SetTablePieces;

    Property Reiterating : Boolean Read FReiterating;
  End;

  TWPOperations = Class (TFslObjectList)
  Private
    Function GetOperation(iIndex: Integer): TWPOperation;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Operation[iIndex : Integer] : TWPOperation Read GetOperation; Default;
  End;

  TWPOperationStack = Class (TWPOperations)
  Public
    Function Current : TWPOperation; Overload; Virtual;
    Procedure Push(oOp : TWPOperation); Overload; Virtual;
    Procedure Pop; Overload; Virtual;
    Procedure CloseLast; Overload; Virtual;
    Function LastUnClosed : Boolean; Overload; Virtual;
    Procedure Trim(iMax : Integer); Overload; Virtual;
  End;

Const
  NAMES_WPOPERATIONTYPE : Array [TWPOperationType] Of String = ('Change', 'Insert', 'Delete', 'CharInsert', 'Move', 'Table', 'Trim');

Type
  TOperationRange = Class (TFslObject)
    Private
      FValid : Boolean;
      FStart : Integer;
      FStop : Integer;
      Procedure Update(iStart, iStop : Integer);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;

      Function Valid : Boolean;
      Procedure Clear;

      Property Start : Integer Read FStart;
      Property Stop : Integer Read FStop;
  End;

  TWPContentEvent = Procedure (oSender : TObject; iPoint : Integer; oPieces : TWPWorkingDocumentPieces) Of Object;

  TWPOperationStatus = (osNone, osRanging, osChanging, osExtending, osMoved, osDeleting);

  TWPSyncEvent = Procedure(oOperation : TWPOperation) Of Object;

Type
  TWPSelection = Class (TFslObject)
  Private
    FSelStart: Integer;
    FCursor: Integer;
    FSelEnd: Integer;
    FDesired : Integer; // -1 means there is no desired cursor position, SelEnd/SelStart is final
    Function GetWorkingSelEnd: Integer;
    Function GetWorkingSelStart: Integer;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TWPSelection; Overload;
    Function Clone : TWPSelection; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Function Summary : String; Overload; Virtual;

    Function Save : String; Overload; Virtual;
    Procedure Restore(sSaved : String); Overload; Virtual;

    Function HasSelection : Boolean; Overload; Virtual;
    Function InSelection(iPosition : Integer) : Boolean; Overload; Virtual;
    Function SelToMove : Integer; Overload; Virtual;
    Function SelActive : Integer; Overload; Virtual; // which piece of the selection should be in view

    Procedure Reset; Overload; Virtual;

    // iPosition must be a valid cursor position
    Function MoveTo(Const iPosition : Integer): Boolean; Overload; Virtual;

    // iPosition must be a valid cursor position
    // iDesired is where the user said they wanted to end. This is different to iPosition when
    // iPosition is not valid cursor position. in this case, iPosition is the effective
    // position, but SelToMove will be based on iDesired not iPosition
    Function SelectTo(Const iPosition, iDesired : Integer) : Boolean; Overload; Virtual;

    Procedure AdjustForDeletion(iStart, iLength: Integer); Overload; Virtual;
    Procedure AdjustForInsertion(iStart, iLength: Integer); Overload; Virtual;

    // iStart and iEnd must both be valid cursor positions
    Function Select(Const iStart, iEnd : Integer) : Boolean; Overload; Virtual;
    Function SetSelStart(Const iIndex : Integer) : Boolean; Overload; Virtual;
    Function SetSelEnd(Const iIndex : Integer) : Boolean; Overload; Virtual;

    // if there's a selection, then Cursor contains the reference point of the selection that doesn't change
    Property Cursor : Integer Read FCursor; // id of Cursor
    Property SelStart : Integer Read FSelStart; // -1 if no selection
    Property WorkingSelStart : Integer Read GetWorkingSelStart; // -1 if no selection
    Property SelEnd : Integer Read FSelEnd; // Cursor if no selection
    Property WorkingSelEnd : Integer Read GetWorkingSelEnd; // Cursor if no selection
    Property Desired : Integer Read FDesired;
  End;

  TWPOperator = Class (TWPSettable)
    Private
      FStyles: TWPStyles;
      FSpeller: TWPSpeller;
      FDocument: TWPWorkingDocument;
      FMasterRangeId : Integer;
      FOnDo : TWPSyncEvent; // Do is different to Redo because Do can be iterative
      FOnUndo : TWPSyncEvent;
      FOnRedo : TWPSyncEvent;
      FOnInsertContent: TWPContentEvent;
      FOnDeleteContent: TWPContentEvent;
      FCurrentImageTool : TImageTool;


      Function GetDocument : TWPWorkingDocument;
      Procedure SetDocument(Const Value: TWPWorkingDocument);
      Function GetSpeller : TWPSpeller;
      Procedure SetSpeller(Const Value: TWPSpeller);
      Function GetStyles : TWPStyles;
      Procedure SetStyles(Const Value: TWPStyles);

    Private
      FVersion : Integer;
      FUndoStack : TWPOperationStack;
      FRedoStack : TWPOperationStack;
      FCurrentOp : TWPOperation;
      FStatus : TWPOperationStatus;
      FRendererRange : TOperationRange;
      FMaxUndoDepth : Integer;
      FDirectCount : Integer;
      FDirectText : String;
      FLastAction : Cardinal;
    FLastChange: TDateTime;

      Procedure CleanEndRange(oOperation : TWPOperation);
      Procedure DeleteEndRange(oOperation : TWPOperation);
      Procedure InsertContent(oOperation : TWPOperation);
      Procedure CopyContent(oOperation : TWPOperation);
      Procedure CopyUndoContent(oOperation : TWPOperation);
      Procedure InsertRedoContent(oOperation : TWPOperation);
      Procedure CutMovedContent(oOperation : TWPOperation);

      Procedure CleanRange(oOperation : TWPOperation); Overload;
      Procedure DeleteRange(oOperation : TWPOperation); Overload;

      Function FindPoint(iOffset : Integer) : Integer;
      Function GetRendererRange : TOperationRange;

      Procedure MarkTableStructureForUpdate(Const iChangeStart, iChangeEnd: Integer); Overload;
      Procedure MarkTableStructureForUpdate(Const iIndex: Integer); Overload;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPOperator; Overload;

      Procedure ClearUndo;  Overload; Virtual;

      Function CanUndo(iRangeId : Integer) : Boolean; Overload; Virtual;
      Function Undo(iRangeId : Integer; oSelection : TWPSelection) : Boolean;  Overload; Virtual;
      Function CanRedo(iRangeId : Integer) : Boolean;  Overload; Virtual;
      Function Redo(iRangeId : Integer; oSelection : TWPSelection): Boolean;  Overload; Virtual;

      Function StartOperation(iRangeId : Integer; aOpType : TWPOperationType; bReopen : Boolean; oSelection : TWPSelection) : Boolean;  Overload; Virtual;

      // if cursor has already been changed prior to creating operation
      Procedure SetPriorCursor(sCursor : String);  Overload; Virtual;

      // before you start making changes, you can expand the range multiple times
      Procedure ExpandRange(iStartDelta, iEndDelta : Integer);  Overload; Virtual;
      Procedure SetRange(iStart, iEnd : Integer);  Overload; Virtual;
      Function MakeCleanCut(iOffset : Integer) : Integer;

      // once you clean the range, you can no longer change the range
      Procedure CleanRange; Overload; Virtual;

      // you can remove the entire range if you want.
      Procedure DeleteRange; Overload;  Virtual;

      // to move selection from one place to another. Only for otMove
      Procedure MoveContent(iDestination : Integer); Overload; Virtual;
      Function EndOfMovedRange : Integer;

      // once you cleaned the range, and possibly removed the range, then you can add content
      Procedure InsertPiece(oPiece : TWPWorkingDocumentPiece);  Overload; Virtual;
      Procedure InsertPiece(oPiece, oBefore : TWPWorkingDocumentPiece);  Overload; Virtual;
      Procedure AppendPiece(oPiece : TWPWorkingDocumentPiece);  Overload; Virtual;
//      Procedure AppendPieceAtEnd(oPiece : TWPWorkingDocumentPiece);  Overload; Virtual; // at the end of the range, assuming you did a clean not a delete
      Procedure AppendDeletedPieces;

      // direct append; appending updates metrics etc, but if you are going to do a series,
      // this is unnecessary, so this sequence allows for fast appending
      Function DirectAppendIndex : Integer;
      Function DirectAppendPiece(oPiece: TWPWorkingDocumentPiece; iIndex : Integer) : Integer;
      Procedure EndDirectAppend;

      // get the piece which is immediately before appended content
      Function PieceBeforeAppend : TWPWorkingDocumentPiece;

      Procedure RemovePiece(oPiece : TWPWorkingDocumentPiece); Overload; Virtual;

      Procedure SetClosingRange(iEnd : Integer); Overload; Virtual;

      // once all is done, finish the operation (and merge if possible)
      Procedure FinishOperation;  Overload; Virtual;

      // conditions have changed and we need to close the last operation if it is still open
      Procedure CloseLastOperation;  Overload; Virtual;

      Procedure AbortOperation;  Overload; Virtual;

      Function StartOfRange : Integer;  Overload; Virtual;
      Function EndOfRange : Integer;  Overload; Virtual;

      // for writing documents- no change to be made, but still need a clean range
      Procedure CleanRange(iStart, iEnd : Integer); Overload;  Virtual;
      Procedure MergeDocument;  Overload; Virtual;

      // status management - turn fast drawing on
      procedure MarkChange;

      Function RedoType : TWPOperationType; Overload; Virtual;
      Function UndoType : TWPOperationType; Overload; Virtual;

      Property Version : Integer Read FVersion;
      Property LastAction : Cardinal Read FLastAction Write FLastAction; // only should be written from external if clock cycles

      Property RendererRange : TOperationRange Read GetRendererRange;
      Property MaxUndoDepth : Integer Read FMaxUndoDepth Write FMaxUndoDepth;
      Property CurrentOp : TWPOperation Read FCurrentOp;

      Property Status : TWPOperationStatus Read FStatus;
      Property DirectCount : Integer Read FDirectCount;
      Property DirectText : String Read FDirectText;
      Property UndoStack : TWPOperationStack Read FUndoStack;
      Property RedoStack : TWPOperationStack Read FRedoStack;

    Public
      Function HasStyles : Boolean;
      Function HasSpeller : Boolean;
      Function HasDocument : Boolean;
      Property Styles : TWPStyles Read GetStyles Write SetStyles;
      Property Speller : TWPSpeller Read GetSpeller Write SetSpeller;
      Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
      Property MasterRangeId : Integer Read FMasterRangeId Write FMasterRangeId;
      Property CurrentImageTool : TImageTool read FCurrentImageTool write FCurrentImageTool;

      Property OnDo : TWPSyncEvent Read FOnDo Write FOnDo;
      Property OnUndo : TWPSyncEvent Read FOnUndo Write FOnUndo;
      Property OnRedo : TWPSyncEvent Read FOnRedo Write FOnRedo;
      Property OnInsertContent: TWPContentEvent Read FOnInsertContent Write FOnInsertContent;
      Property OnDeleteContent: TWPContentEvent Read FOnDeleteContent Write FOnDeleteContent;
      Property LastChange : TDateTime read FLastChange write FLastChange;
  End;

Const
  NAMES_WPOPERATIONSTATUS : Array [TWPOperationStatus] Of String = ('None', 'Ranging', 'Changing', 'Extending', 'Moved', 'Deleting');

  INTERNAL_LOG_LIMIT = 500;

Type
  TWPCapability = (canInsert,         // currently able to insert text
                   canWrite,          // can write the selection
                   canDelete,         // can delete the current selection
                   canCut,            // canWrite and canDelete
                   canUndo,           // there is undos to undo if wanted
                   canRedo,           // there is redos to redo if wanted
                   canInsertImage,    // can insert an image
                   canImageProps,     // can set image properties
                   canLineProps,      // can set line properties
                   canInsertField,    // can insert a field
                   canInsertFieldSection,    // can insert a field-type section
                   canFieldProps,     // can set field properties
                   canFieldSectionProps,     // can set field-type section properties
                   canRemoveField,    // can remove a field (selected/in oRange and have Rights)
                   canRemoveFieldSection,    // can remove a field-type section (selected/in oRange and have Rights)
                   canSpliceField,    // can remove a field and leave contents (selected/in oRange and have Rights)
                   canGotoField,      // fields exist
                   canFormat,         // can apply format
                   canInsertTable,    // can insert a table
                   canConvertToTable, // can convert Text to Table
                   canSelectTable,    // can select a table row
                   canTableProps,     // can set table properties
                   canRemoveTable,    // can remove entire table
                   canInsertRowAbove, // can insert a row above this one
                   canInsertRowBelow, // can insert a row above this one
                   canSelectRow,      // can select a row
                   canRowProps,       // can set table row properties
                   canRemoveRow,      // can remove table row
                   canInsertColumn,   // can insert a column (either to left or Right)
                   canRemoveColumn,   // can remove table column
                   canInsertLine,     // can insert a line
                   canInsertPageBreak,// can insert a Page Break
                   canSortTable,      // can sort table row using values in selected column
                   canMergeCells,     // if two or more adjacent cells can be merged
                   canSplitCell,      // if a cell can be split into two
                   canInsertTemplate, // can insert a template. note that insertion will not always be successful - will depend on contents of template
                   canInsertAnnotation,// can insert a comment (annotation)
                   canManageAnnotation // can edit or delete a comment
                   );
  TWPCapabilities = Set Of TWPCapability;

  TRangeSearchItem = (rsiPara, rsiField, rsiTableCell, rsiTableRow, rsiTable, rsiSection);
  TRangeSearchItems = Set Of TRangeSearchItem;

  TPieceTypeCount = Array [TWPWorkingDocumentPieceType] Of Integer;

  TWPChangeType = (ctContent, ctSelection, ctStatus);
  TWPChangeTypes = Set Of TWPChangeType;

  TWPChangeCaseType = (cctNone, cctSentenceCase, cctLowerCase, cctUpperCase, cctTitleCase, cctToggleCase);

Const
  dleft = false;
  dRight = true;
  CT_ALL = [ctContent, ctSelection, ctStatus];
  WPCHANGECASETYPE_NAMES : Array[TWPChangeCaseType] Of String = ('None', 'Sentence case', 'lower case', 'UPPER CASE', 'Title Case', 'tOGGLE cASE');
  WPCAPABILITY_NAMES : Array [TWPCapability] Of String = (
     'Insert', 'Write', 'Delete', 'Cut', 'Undo', 'Redo', 'InsertImage', 'ImageProps',
     'LineProps', 'InsertField', 'InsertFieldSection', 'FieldProps', 'FieldSectionProps',
     'RemoveField', 'RemoveFieldSection', 'SpliceField', 'GotoField', 'Format', 'InsertTable',
     'ConvertToTable', 'SelectTable', 'TableProps', 'RemoveTable', 'InsertRowAbove', 'InsertRowBelow',
     'SelectRow', 'RowProps', 'RemoveRow', 'InsertColumn', 'RemoveColumn', 'InsertLine', 'InsertPageBreak',
     'SortTable', 'MergeCells', 'SplitCell', 'InsertTemplate', 'InsertComment', 'ManageComment');

Type
  TWPSpeechMagicInsertOptions = Class (TFslObject)
    Private
      FSpeechMagicDouble: Boolean;
      FIgnoreBackground: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Property SpeechMagicDouble : Boolean Read FSpeechMagicDouble Write FSpeechMagicDouble;
      Property IgnoreBackground : Boolean Read FIgnoreBackground Write FIgnoreBackground;
  End;

  TWPAction = Class (TFslObject)
  private
    FSelection: String;
    FDetails: String;
    FAction: string;
    FOutcome: string;
    FTime: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Property Time : String read FTime write FTime;
    Property Action : string read FAction write FAction;
    Property Selection : String read FSelection write FSelection;
    Property Details : String read FDetails write FDetails;
    Property outcome : string read FOutcome write FOutcome;
  End;

  TWPActionsList = Class (TFslObjectList)
  private
    function GetAction(index: integer): TWPAction;
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Property Action[index : integer] : TWPAction read GetAction;  default;
  End;

Type
  TWPSearchDirection = (sdForwards, sdBackwards);

  TWPSearchDetails = class (TFslObject)
    Private
      FText : String;
      FDirection : TWPSearchDirection;
      FWholeDocument : Boolean;
      FCaseSensitive : Boolean;
      FWholeWords : Boolean;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPSearchDetails; Overload;
      Function Clone : TWPSearchDetails; Overload;
      Procedure Assign(oSource : TFslObject); Override;

      Property Text : String read FText write FText;
      Property Direction : TWPSearchDirection read FDirection write FDirection;
      Property WholeDocument : Boolean read FWholeDocument write FWholeDocument;
      Property CaseSensitive : Boolean read FCaseSensitive write FCaseSensitive;
      Property WholeWords : Boolean read FWholeWords write FWholeWords;

      Function Describe : string;
  End;

Const
  WPSEARCHDIRECTION_NAMES : array [TWPSearchDirection] of String = ('Forwards', 'Direction');


Type
  TWPReplaceDetails = class (TWPSearchDetails)
    Private
      FReplace : String;
      FSelection : Boolean;
      FPrompt : Boolean;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPReplaceDetails; Overload;
      Function Clone : TWPReplaceDetails; Overload;
      Procedure Assign(oSource : TFslObject); Override;

      Property Replace : String read FReplace write FReplace;
      Property Selection : Boolean read FSelection write FSelection;
      Property Prompt : Boolean read FPrompt write FPrompt;
  End;

Type
  TWPSearchIterator = Class (TFslObject)
    Private
      FDocument : TWPWorkingDocument;
      FDirection : TWPSearchDirection;
      FLimitStart : Integer;
      FLimitStop : Integer;

      FMore : Boolean;
      FCursor : Integer;
      FCurrent : TWPWorkingDocumentPiece;
      FIndex : Integer;
      FOffset : Integer;

      Function InRange(iOffset : Integer) : Boolean;
      Procedure Backwards;
      Procedure Forwards;
      Function GetDocument : TWPWorkingDocument;
      Procedure SetDocument(Const Value : TWPWorkingDocument);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Procedure Open(iLimit : Integer); Overload; Virtual;
      Procedure Next; Overload; Virtual;

      Function More : Boolean; Overload; Virtual;
      Function GetText(iLength : Integer) : String; Overload; Virtual;
      Function CharAtOffset(iPosition : Integer):Char; Overload; Virtual;

      Function HasDocument : Boolean;

      Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
      Property Direction : TWPSearchDirection Read FDirection Write FDirection;
      Property Cursor : Integer Read FCursor;
      Property LimitStart : Integer Read FLimitStart Write FLimitStart;
      Property LimitStop : Integer Read FLimitStop Write FLimitStop;
  End;

Type
  TWPSearch = Class (TFslObject)
    Private
      FDocument : TWPWorkingDocument;
      FSelection : TWPSelection;
      FSearchDetails : TWPSearchDetails;
      FLimitStart : Integer;
      FLimitStop : Integer;

      Function GetDocument : TWPWorkingDocument;
      Function GetSelection : TWPSelection;
      Function GetSearchDetails : TWPSearchDetails;
      Procedure SetDocument(Const Value : TWPWorkingDocument);
      Procedure SetSelection(Const Value : TWPSelection);
      Procedure SetSearchDetails(Const Value : TWPSearchDetails);

      Function Matches(oIterator : TWPSearchIterator; Out iStart, iEnd : Integer) : Boolean;

      Function Search(iLimit : Integer; Out iStart, iEnd : Integer) : Boolean; Overload;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(oDocument : TWPWorkingDocument; oSelection : TWPSelection; oSearchDetails : TWPSearchDetails); Overload; Virtual;
      destructor Destroy; Override;

      Function Search(Out iStart, iEnd : Integer) : Boolean; Overload; Virtual;

      Function HasDocument : Boolean;
      Function HasSelection : Boolean;
      Function HasSearchDetails : Boolean;

      Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
      Property Selection : TWPSelection Read GetSelection Write SetSelection;
      Property SearchDetails : TWPSearchDetails Read GetSearchDetails Write SetSearchDetails;
      Property LimitStart : Integer Read FLimitStart Write FLimitStart;
      Property LimitStop : Integer Read FLimitStop Write FLimitStop;
  End;

  TWPRange = Class (TFslObject)
    Private
      FOwner : TObject;
      FDocument: TWPWorkingDocument;
      FOnContentChange: TNotifyEvent;
      FOnSelectionChange: TNotifyEvent;
      FOperator : TWPOperator;
      FCanManipulateColumns: Boolean;
      FCanConvertTextToTable : Boolean;

      Function GetDocument : TWPWorkingDocument;
      Function GetOperator : TWPOperator;
      Procedure SetDocument(Const Value: TWPWorkingDocument);
      Procedure SetOperator(Const Value: TWPOperator);
    Private
      FId : Integer;
      FFont: TWPSFontDetails;
      FParagraph: TWPSParagraphDetails;
      FStyle: String;
      FLastStyleVersion : Integer;
      FLastStyleSummary : String;
      FCapabilities : TWPCapabilities;
      FSelection: TWPSelection;
      FLog : TWPActionsList;
      FStart : TDateTime;

      FCurrentParagraph : TWPWorkingDocumentParaPiece;
      FCurrentImage : TWPWorkingDocumentImagePiece;
      FCurrentLine : TWPWorkingDocumentBreakPiece;
      FCurrentSectionStart : TWPWorkingDocumentSectionStartPiece;
      FCurrentSectionStop : TWPWorkingDocumentStopPiece;
      FCurrentFieldStart : TWPWorkingDocumentFieldStartPiece;
      FCurrentFieldStop : TWPWorkingDocumentFieldStopPiece;
      FCurrentTableStart : TWPWorkingDocumentTableStartPiece;
      FCurrentTableStop : TWPWorkingDocumentStopPiece;
      FCurrentTableRowStart : TWPWorkingDocumentTableRowStartPiece;
      FCurrentTableRowStop : TWPWorkingDocumentStopPiece;
      FCurrentTableCellStart : TWPWorkingDocumentTableCellStartPiece;
      FCurrentTableCellStop : TWPWorkingDocumentStopPiece;

      FSelectedTable : TWPWorkingDocumentTableStartPiece;
      FSelectedRows   : TWPWorkingDocumentTableRowStartPieces;
      FSelectedCells  : TWPWorkingDocumentTableCellStartPieces;


      Procedure SetStyle(Const sName : String);

      Function GetCurrentParagraph : TWPWorkingDocumentParaPiece;
      Function GetCurrentImage : TWPWorkingDocumentImagePiece;
      Function GetCurrentLine : TWPWorkingDocumentBreakPiece;
      Function GetCurrentSectionStart : TWPWorkingDocumentSectionStartPiece;
      Function GetCurrentSectionStop : TWPWorkingDocumentStopPiece;
      Function GetCurrentFieldStart : TWPWorkingDocumentFieldStartPiece;
      Function GetCurrentFieldStop : TWPWorkingDocumentFieldStopPiece;
      Function GetCurrentTableStart : TWPWorkingDocumentTableStartPiece;
      Function GetCurrentTableStop : TWPWorkingDocumentStopPiece;
      Function GetCurrentTableRowStart : TWPWorkingDocumentTableRowStartPiece;
      Function GetCurrentTableRowStop : TWPWorkingDocumentStopPiece;
      Function GetCurrentTableCellStart : TWPWorkingDocumentTableCellStartPiece;
      Function GetCurrentTableCellStop : TWPWorkingDocumentStopPiece;
      Function GetSelectedTable : TWPWorkingDocumentTableStartPiece;
      Function GetSelectedTableStop : TWPWorkingDocumentStopPiece;
      Function GetSelectedRows : TWPWorkingDocumentTableRowStartPieces;
      Function GetSelectedCells : TWPWorkingDocumentTableCellStartPieces;

      Function GetApplicableField(cursor : integer) : TWPWorkingDocumentFieldStartPiece;

      Procedure ClearCurrentStatus;
      Procedure LookForImageAndLine;
      Function SearchSelection : TRangeSearchItems;
      Procedure SearchForwards(aSearch : TRangeSearchItems; iStart : Integer);
      Procedure SearchBackwards(aSearch : TRangeSearchItems; iEnd : Integer);
      Function FindNextPiece(oSource : TWPWorkingDocumentPiece; aFind, aStop : TWPWorkingDocumentPieceType; bNested : Boolean ) : TWPWorkingDocumentPiece;
      Function FindPreviousPiece(oSource : TWPWorkingDocumentPiece; aFind, aStop : TWPWorkingDocumentPieceType; bNested : Boolean ) : TWPWorkingDocumentPiece;
      Procedure FindMissingPartners;
      Function LineBreakToRight : Boolean;

      Function FirstInLine : Boolean;
      Function ApplyStyle(Const sStyle : String) : Boolean;
      Function ApplyParagraphStyle(oStyle : TWPStyle) : Boolean;
      Function DoInsertCell(Var iAppendIndex: Integer) : TWPWorkingDocumentPiece;
      Function DoInsertRow(iCols : Integer; Var iAppendIndex: Integer) : TWPWorkingDocumentPiece;
      Function CurrentColIndex  : Integer;
      Procedure DoInsertColumnInRow(oRow : TWPWorkingDocumentTableRowStartPiece; iIndex : Integer);
      Function DoInsertColumn(iIndex : Integer) : Boolean;
      Procedure DoDeleteColumnInRow(oRow : TWPWorkingDocumentTableRowStartPiece; iIndex : Integer);
      Function DoDeleteColumn(iIndex : Integer) : Boolean;
      Procedure ConfigureInsertedTable(oTable : TWPWorkingDocumentTableStartPiece);
      Function TryAndFindTableBefore : Boolean;
      Function TryAndFindTableAfter : Boolean;

      Function CheckNoColumnsToInsert(oDocument: TWPWorkingDocument; iColumnCount: Integer; Var sError: String): Boolean; Overload;
      Function DoInsertCopiedRow(oDocument: TWPWorkingDocument; oStyles: TWPStyles; iPos: Integer) : Boolean; Overload;
      Procedure DoInsertCopiedRow(oDocument: TWPWorkingDocument); Overload;
      Function DoInsertCopiedRows(oDocument: TWPWorkingDocument; oStyles: TWPStyles; iPos: Integer) : Boolean; Overload;
      Procedure DoInsertCopiedRows(oDocument: TWPWorkingDocument; iIndex : Integer); Overload;

      Procedure DoReorderRows(Const oRows: TWPWorkingDocumentPieces; Const aSortedPos: Array Of Integer; Const iStart, iStop: Integer; Var oPieces: TWPWorkingDocumentPieces); Overload;
      Procedure CopyRowByReference(Const oStart: TWPWorkingDocumentTableRowStartPiece; Var oPieces: TWPWorkingDocumentPieces);

      Function DoCopy(bDebug : Boolean) : Boolean;
      Function PasteInsertClipboard(aReaderClass : TWPReaderClass; oClip : TWPClipboard; aContentType : TWPClipboardContentType; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean; Overload;
      Function PasteInsert(aReaderClass : TWPReaderClass; oSource : TFslStream; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean; Overload;
      Function PasteInsertWorkingDocument(oDocument : TWPWorkingDocument; oStyles : TWPStyles; bSkipLastPara : Boolean; Var sError : String) : Boolean; Overload;
      Function PasteJPEG(oClip : TWPClipboard) : Boolean;
      Function PasteBitmap(oClip : TWPClipboard) : Boolean;
      Function ReadClipboardContent(oClip : TWPClipboard; aContentType : TWPClipboardContentType; Var sError : String) : TFslMemoryStream; Overload;
      Function ParseClipboardContent(aReaderClass : TWPReaderClass; oClip : TWPClipboard; aContentType : TWPClipboardContentType; oStyles : TWPStyles; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : TWPWorkingDocument; Overload;
      Function ParseClipboardContent(aReaderClass : TWPReaderClass; oSource: TFslStream; oStyles : TWPStyles; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : TWPWorkingDocument; Overload;

      Function DoDeleteSelection(sCursor : String): Boolean;
      Function DoMove(iIndex : Integer; aDirection : TWPCursorDirection; bDraw : Boolean = True) : Boolean; Overload;
      Function SelectWholeParagraphs : TWPWorkingDocumentPiece;
      Function ConvertParagraphsToTable(oLast : TWPWorkingDocumentPiece) : TWPWorkingDocumentPiece;
      Function InsertInOperation(sText : String; oFont : TWPSFontDetails = Nil; Const sStyle : String = ''): Integer;
      Function GetFieldRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
      Function GetParaRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
      Function DoSelect(iIndex : Integer; aDirection : TWPCursorDirection; bDraw : Boolean = True) : Boolean; Overload;
      Procedure PrepareBalancedFragment(oDocument : TWPWorkingDocument); Overload;
      Procedure PrepareBalancedFragment(oDocument : TWPWorkingDocument; iStart, iStop : Integer); Overload;
      Procedure WriteRange(aWriterClass : TWPWriterClass; oDocument : TWPWorkingDocument; oDest : TFslBuffer; iWidth : Integer = 0);
      Function HasElements(oDocument : TWPWorkingDocument; Const aTypesOne, aTypesTwo : TWPWorkingDocumentPieceTypes; bTypesTwoNotLast : Boolean; Var sTypes : String) : Boolean;
      Function HasFieldCheckables(oDocument : TWPWorkingDocument) : Boolean;
      Function InputOkWithField(aChar: Char): Boolean;
      Function GetRangeCapabilities(iStart, iEnd : Integer) :  TWPCapabilities;
      Function GetSelectionCapabilities :  TWPCapabilities;
      Function GetSectionRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
      Function ContextIsReadOnly : Boolean;
      Function TrimSelectionStart(iCursor : Integer) : Integer;
      Function TrimSelectionEnd(iCursor : Integer) : Integer;
      Procedure AddPriorPieces(oSource, oDest : TWPWorkingDocument; iPosition : Integer; aUntil : TWPWorkingDocumentPieceType; aInclude : TWPWorkingDocumentPieceTypes;
                             Var aAdded : TWPWorkingDocumentPieceTypes; Var aBalanced : TPieceTypeCount);
      Procedure AddPostPieces(oSource, oDest : TWPWorkingDocument; iPosition : Integer; aUntil : TWPWorkingDocumentPieceType; aInclude : TWPWorkingDocumentPieceTypes);
      Procedure InsertMissingParagraphs(oDocument : TWPWorkingDocument);
      Function GetTextInRange(iStart, iStop : Integer) : String;
      Function IsProtectedPara(iPosition : Integer):Boolean;

      Function AtStartOfSentence(checkEmpty : Boolean) : Boolean;
      Function AtStartOfPara(checkEmpty : Boolean) : Boolean;
      Function atEndOfPara : Boolean;
      Function ParaText : String;

//      Function NextParaIsNotList : Boolean;
      Function AdjacentIsPara(toRight : Boolean) : Boolean;
      Function GetCapabilities: TWPCapabilities;
      Function InsideField : Boolean;
      Function CanTableBeSorted(oTable: TWPWorkingDocumentTableStartPiece): Boolean;
      Function ReadStyle : String;
      Procedure ReadFont;
      Procedure ReadParagraph;
      Function ConsoleFontOk(oFont, oCompare : TWPSFontDetails) : Boolean;
      Function ConsoleParaOK(oPara : TWPSParagraphDetails) : Boolean;

      Procedure ApplyBaseStyle(oPiece : TWPWorkingDocumentPiece);
      Function SelectionMatchesFieldOuter : Boolean;
      Procedure UpdateTextMetrics;

    Protected
      Procedure MoveCursor(bSelect : Boolean; iNew : Integer; aDirection : TWPCursorDirection; bDraw : Boolean = True);
      Function SelCursorToChange(bSelect : Boolean) : Integer;
      Function GetWorkingWidth : Integer;  Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPRange; Overload;
      Procedure Consume; Overload; Virtual;
      Procedure CutOff;  Overload; Virtual;
      Procedure DefaultLoadImage(oSender : TObject; Const Context : String; Const sName : String; Out oBuffer : TFslBuffer);

      Procedure ChangeContent;  // used to track dirty status. If a redraw is required, ChangeState will always be called.
      Procedure ChangeState(bDraw : Boolean = True); Virtual;  // called when state/selection/screen changes. bDraw = false if another call is about to come
      Procedure ChangeReadyState;
      Procedure UpdateCurrentStatus;

      Function HasCurrentParagraph : Boolean;
      Function HasCurrentImage : Boolean;
      Function HasCurrentLine : Boolean;
      Function HasCurrentSectionStart : Boolean;
      Function HasCurrentSectionStop : Boolean;
      Function HasCurrentFieldStart : Boolean;
      Function HasCurrentFieldStop : Boolean;
      Function HasCurrentTableStart : Boolean;
      Function HasCurrentTableStop : Boolean;
      Function HasCurrentTableRowStart : Boolean;
      Function HasCurrentTableRowStop : Boolean;
      Function HasCurrentTableCellStart : Boolean;
      Function HasCurrentTableCellStop : Boolean;

      Function HasSelectedTable : Boolean;
      Function HasSelectedRows : Boolean;
      Function HasSelectedCells : Boolean;

      Property CurrentParagraph : TWPWorkingDocumentParaPiece Read GetCurrentParagraph;
      Property CurrentImage : TWPWorkingDocumentImagePiece Read GetCurrentImage;
      Property CurrentLine : TWPWorkingDocumentBreakPiece Read GetCurrentLine;
      Property CurrentSectionStart : TWPWorkingDocumentSectionStartPiece Read GetCurrentSectionStart;
      Property CurrentSectionStop : TWPWorkingDocumentStopPiece Read GetCurrentSectionStop;
      Property CurrentFieldStart : TWPWorkingDocumentFieldStartPiece Read GetCurrentFieldStart;
      Property CurrentFieldStop : TWPWorkingDocumentFieldStopPiece Read GetCurrentFieldStop;
      Property CurrentTableStart : TWPWorkingDocumentTableStartPiece Read GetCurrentTableStart;
      Property CurrentTableStop : TWPWorkingDocumentStopPiece Read GetCurrentTableStop;
      Property CurrentTableRowStart : TWPWorkingDocumentTableRowStartPiece Read GetCurrentTableRowStart;
      Property CurrentTableRowStop : TWPWorkingDocumentStopPiece Read GetCurrentTableRowStop;
      Property CurrentTableCellStart : TWPWorkingDocumentTableCellStartPiece Read GetCurrentTableCellStart;
      Property CurrentTableCellStop : TWPWorkingDocumentStopPiece Read GetCurrentTableCellStop;
      Property SelectedTable : TWPWorkingDocumentTableStartPiece Read GetSelectedTable;
      Property SelectedTableStop: TWPWorkingDocumentStopPiece Read GetSelectedTableStop;
      Property SelectedRows : TWPWorkingDocumentTableRowStartPieces Read GetSelectedRows;
      Property SelectedCells : TWPWorkingDocumentTableCellStartPieces Read GetSelectedCells;

      Function ClearFormatting : Boolean; Overload; Virtual;

      Function ApplyChangeCase(aCaseType: TWPChangeCaseType): Boolean;
      Function IsStartOfSentence(oText: TWPWorkingDocumentTextPiece): Boolean;

      Function ApplyFont(oFont : TWPSFontDetails) : Boolean; Overload; Virtual;
      Function ApplyFontName(sName : String) : Boolean; Overload; Virtual;
      Function ApplyFontSize(iSize : Integer) : Boolean; Overload; Virtual;
      Function ApplyForeground(iColour : TColour) : Boolean; Overload; Virtual;
      Function ApplyBackground(iColour : TColour) : Boolean; Overload; Virtual;
      Function ApplyBold(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyBold(aValue : TWPSTriState) : Boolean; Overload; Virtual;
      Function ApplyUnderline(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyUnderline(aValue : TWPSTriState) : Boolean; Overload; Virtual;
      Function ApplyItalic(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyItalic(aValue : TWPSTriState) : Boolean; Overload; Virtual;
      Function ApplyStrikethrough(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyStrikethrough(aValue : TWPSTriState) : Boolean; Overload; Virtual;
      Function ApplyState(aValue : TWPSFontState) : Boolean; Overload; Virtual;
      Function ApplySubscript(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplySuperscript(bValue : Boolean) : Boolean; Overload; Virtual;
      Function IsBold : Boolean; Overload; Virtual;
      Function IsUnderline : Boolean; Overload; Virtual;
      Function IsItalic : Boolean; Overload; Virtual;
      Function ApplyParagraph(oParagraph : TWPSParagraphDetails): Boolean; Overload; Virtual;
      Function AlignLeft : Boolean; Overload; Virtual;
      Function AlignCentre : Boolean; Overload; Virtual;
      Function AlignRight : Boolean; Overload; Virtual;
      Function AlignJustify : Boolean; Overload; Virtual;
      Function ApplyBullets(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyNumbers(bValue : Boolean) : Boolean; Overload; Virtual;
      Function ApplyLeftIndent(iOffset : Integer) : Boolean; Overload; Virtual;
      Function ListStartDecrement : Boolean; Overload; Virtual;
      Function ListStartIncrement : Boolean; Overload; Virtual;
      Function ListStartReset : Boolean; Overload; Virtual;


      // undo/redo
      Function CanUndo_ : Boolean;
      Function Undo : Boolean; Overload; Virtual;
      Function CanRedo_ : Boolean;
      Function Redo : Boolean; Overload; Virtual;
      Procedure ClearUndoRedo; Overload; Virtual;

      // current status
      Function CursorInWord(Out sText : String):Boolean; Overload; Virtual;
      Function CursorInWord(Out sText : String; Out aSpell : TWPWorkingDocumentSpellCheckingState):Boolean; Overload; Virtual;
      Function CursorInWord(Out sText : String; Out iStart, iStop : Integer):Boolean; Overload; Virtual;
      Function CursorInWord(Out sText : String; Out aSpell : TWPWorkingDocumentSpellCheckingState; Out iStart, iStop : Integer):Boolean; Overload; Virtual;

      // cursor/selection operations
      Function SelectedText : String; Overload; Virtual;
      Function TextByRange(Const iStart, iStop : Integer) : String; Overload; Virtual;
      Function MoveTo(iIndex : Integer; bDraw : Boolean = True) : Boolean; Overload; Virtual;
      Function SelectTo(iIndex : Integer; bDraw : Boolean = True) : Boolean;  Overload; Virtual;
      Function SetSelStart(iIndex : Integer) : Boolean;  Overload; Virtual;
      Function SetSelEnd(iIndex : Integer) : Boolean;  Overload; Virtual;
      Function GoHome(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function LeftWord(bSelect : Boolean; bDraw : Boolean = True) : Boolean; Overload; Virtual;
      Function Left(bSelect : Boolean; bDraw : Boolean = True) : Boolean; Overload; Virtual;
      Function Right(bSelect : Boolean; bDraw : Boolean = True) : Boolean; Overload; Virtual;
      Function RightWord(bSelect : Boolean; bDraw : Boolean = True) : Boolean; Overload; Virtual;
      Function GoEnd(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function GoHomeInSection(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function GoEndInSection(bSelect : Boolean) : Boolean; Overload; Virtual;
      Function SelectAll : Boolean; Overload; Virtual;
      Function SelectSection(bOuter : Boolean) : Boolean; Overload; Virtual;
      Function SelectPara : Boolean; Overload; Virtual;
      Function SelectField(bOuter : Boolean) : Boolean; Overload; Virtual;
      Function SelectWord : Boolean; Overload; Virtual;
      Function SelectRange(iStart, iEnd : Integer; bDraw : Boolean = True) : Boolean;  Overload; Virtual;

      Function CanMoveSelection(iDestination : Integer) : Boolean;
      Function MoveSelection(iDestination : Integer) : Boolean; Overload; Virtual;

      Function SelectFieldByName(Const sNamespace, sName : String) : Boolean; Overload; Virtual;
      Function SelectSectionByName(Const sNamespace, sName : String) : Boolean; Overload; Virtual;

      // Comments
      Function InsertAnnotation(oOwner : TWPAnnotationDefinitionProvider; Const sText : String) : Boolean; Overload; Virtual;
      Function EditAnnotation(Const sText : String) : Boolean; Overload; Virtual;
      Procedure DeleteAnnotation(); Overload; Virtual;

      // Breaks
      Function InsertLine : Boolean;
      Function SetLineProperties(oLine : TWPWorkingDocumentBreakPiece) : Boolean; Overload; Virtual;
      Function InsertPageBreak : Boolean;

      // Sections
      Function SetSectionProperties(oSection : TWPWorkingDocumentSectionStartPiece) : Boolean; Overload; Virtual;
      Function InsertFieldSection(oDetails : TWPWorkingDocumentSectionStartPiece; Const sText : String = '') : Boolean; Overload; Virtual;
      Function RemoveFieldSection(bKeepContent : Boolean) : Boolean; Overload; Virtual;

      // tables
      Function InsertTable(iRows, iCols : Integer) : Boolean;
      Function ConvertTextToTable : Boolean;
      Function InsertCopiedRowAbove(Var sError: String) : Boolean;
      Function InsertCopiedRowBelow(Var sError: String) : Boolean;
      Function InsertRowAbove : Boolean;
      Function InsertRowBelow : Boolean;
      Function InsertColumnLeft : Boolean;
      Function InsertColumnRight : Boolean;
      Function MergeCells : Boolean;
      Function SplitCell : Boolean;
      Function SelectRow : Boolean;
      Function SelectTable : Boolean;
      Function DeleteTable : Boolean;
      Function DeleteRow : Boolean;
      Function DeleteColumn : Boolean;
      Function SetTableProperties(oTable : TWPWorkingDocumentTableStartPiece; oTableRow : TWPWorkingDocumentTableRowStartPiece; oTableCell : TWPWorkingDocumentTableCellStartPiece) : Boolean; Overload;
      Function SetTableProperties(oTable : TWPWorkingDocumentTableStartPiece) : Boolean; Overload;
      Function SetTableRowProperties(oTableRow : TWPWorkingDocumentTableRowStartPiece) : Boolean;
      Function SetTableCellProperties(oTableCell : TWPWorkingDocumentTableCellStartPiece) : Boolean;
      Function PreviousTableCell : Boolean;
      Function NextTableCell : Boolean;
      Function ReorderTableRows(Const aSortedPos: Array Of Integer): Boolean;

      // fields
      Function InsertField(oDetails : TWPWorkingDocumentFieldStartPiece; bNoStyle, bAcceptExistingContent : Boolean; Const sText : String = '') : Boolean; Overload; Virtual;
      Function FirstField : Boolean; Overload; Virtual;
      Function NextField : Boolean; Overload; Virtual;
      Function PreviousField : Boolean; Overload; Virtual;
      Function GotoFieldByName(Const sNamespace, sName : String):Boolean; Overload; Virtual;
      Function GotoFieldByDoc(Field : TWPDocumentField):Boolean; Overload; Virtual;
      Function RemoveField(bKeepContent : Boolean) : Boolean; Overload; Virtual;
      Function SetFieldProperties(oField : TWPWorkingDocumentFieldStartPiece) : Boolean; Overload; Virtual;
      Function SetFieldCheckIndex(oField : TWPWorkingDocumentFieldStartPiece; iIndex : Integer) : Boolean; Overload; Virtual;
      Function CurrentFieldContent: String;
      Function InsertTemplate(oBuffer : TFslBuffer) : Boolean;
      Function SetFieldContents(namespace, name, content : String; annotation : TWPAnnotationDefinitionProvider) : Boolean; overload;
      Function SetFieldContents(oField : TWPDocumentField; content : String; annotation : TWPAnnotationDefinitionProvider) : Boolean; overload;

      // search/replace
      Function ReplaceCurrentWord(Const oSource : TFslStream; aFormat : TWPFormat; Var sMessage : String) : Boolean; Overload; Virtual;
      Function ReplaceCurrentWord(Const sText: String; Var sMessage : String) : Boolean; Overload; Virtual;
      Function Search(oDetails : TWPSearchDetails; iRangeStart : Integer = -1; iRangeStop : Integer = -1) : Boolean; Overload; Virtual;


      // clipboard
      Function Cut : Boolean; Overload; Virtual;
      Function Copy : Boolean; Overload; Virtual;
      Function CopyDebug : Boolean; Overload; Virtual;
      Function Paste(Var sError : String; oSpeechMagicOptions : TWPSpeechMagicInsertOptions = Nil) : Boolean; Overload; Virtual;
      Function PasteSpecial(aContentType : TWPClipboardContentType; Var sError : String; oSpeechMagicOptions : TWPSpeechMagicInsertOptions = Nil) : Boolean; Overload; Virtual;
      Function Insert(oStream: TFslStream; aContentType : TWPClipboardContentType; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean; Overload; Virtual;
      Function Insert(oStream: TFslStream; aFormat : TWPFormat; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean; Overload; Virtual;
      Function DropText(iIndex : Integer; sText : String; Var sError : String) : Boolean; Overload; Virtual;
      Function SaveSelection(oStream: TFslStream; aFormat : TWPFormat) : Boolean; Overload; Virtual;

      // images
      Function DropFile(iIndex : Integer; Const sFilename : String) : Boolean; Overload; Virtual;
      Function DropImage(iOffset : Integer; oImage : TFslVCLGraphic) : Boolean; Overload; Virtual;
      Function InsertImage(sFilename : String) : Boolean; Overload; Virtual;
      Function InsertPDF(sFilename : String) : Boolean; Overload; Virtual;
      Function InsertImage(Const sName: String; stream : TFslAccessStream) : Boolean; Overload; Virtual;
      Function InsertImage(Const sName: String; oImage : TFslGraphic) : Boolean; Overload; Virtual;
      Function InsertImage(Const sName: String; oPDF : TWPWorkingAttachment; iPage : integer) : Boolean; Overload; Virtual;
      Function SetImageProperties(oImage : TWPWorkingDocumentImagePiece) : Boolean; Overload; Virtual;
      Procedure ApplyImageAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment); Overload; Virtual;
      Procedure DeleteAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment); Overload; Virtual;
      Procedure MoveAdornmentToBack(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment); Overload; Virtual;
      Procedure MoveAdornmentToFront(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment); Overload; Virtual;
      Procedure MoveImageAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment; aPart : TAdornmentPart; aAction : TAdornmentAction; iX, iY : Integer); Overload; Virtual;
      Procedure SelectImageArea(oImage : TWPWorkingDocumentImagePiece; oArea : TWPImageMapArea); Overload; Virtual;


      // text editing
      Function NewParagraph : Boolean; Overload; Virtual;
      Function LineBreak(bSpeechMagicIsPara : Boolean = False) : Boolean; Overload; Virtual;
      Function Insert(aChar : Char; sStyle : String = ''; oFont : TWPSFontDetails = Nil) : Boolean; Overload; Virtual;
      Function Insert(sText : String; oFont : TWPSFontDetails = Nil; sStyle : String = ''; bSelect : Boolean = False) : Boolean; Overload; Virtual;
      Function InsertSymbol(aChar : Char; Const sDrawnFontName : String) : Boolean;
      Function DeleteSelection : Boolean; Overload; Virtual;
      Function DeleteLeft : Boolean; Overload; Virtual;
      Function DeleteRight : Boolean; Overload; Virtual;
      Function DeleteWordLeft : Boolean; Overload; Virtual;
      Function DeleteWordRight : Boolean; Overload; Virtual;
      Function MergeParagraph(toRight : Boolean) : Boolean; Overload; Virtual;

      // spelling
      Procedure CheckSpelling(sWord : String = ''); Overload; Virtual;
      Procedure ResetSpelling; Overload; Virtual;

      // internal log
      procedure LogAction(action, details : String; ok : boolean); overload;
      procedure LogAction(action, details : String); overload;

      Property Style : String Read FStyle Write SetStyle;
      Function WorkingStyle : TWPStyle;
      Property Font : TWPSFontDetails Read FFont;
      Property Paragraph : TWPSParagraphDetails Read FParagraph;
      Property Selection : TWPSelection Read FSelection;
      Property Capabilities : TWPCapabilities Read FCapabilities; // operations that can currently be performed on this range
      Property Id : Integer Read FId;

    Public
      Function HasDocument : Boolean;
      Property Document : TWPWorkingDocument Read GetDocument Write SetDocument;
      Property Operator_ : TWPOperator Read GetOperator Write SetOperator;
      Property Owner : TObject Read FOwner Write FOwner;
      Property Log : TWPActionsList read FLog;

      Property CanManipulateColumns : Boolean Read FCanManipulateColumns Write FCanManipulateColumns;
      Property CanConvertTextToTable : Boolean Read FCanConvertTextToTable Write FCanConvertTextToTable;


      // these events are private to WP structures. Use WordProcessor events externally
      Property OnContentChange : TNotifyEvent Read FOnContentChange Write FOnContentChange;
      Property OnSelectionChange : TNotifyEvent Read FOnSelectionChange Write FOnSelectionChange;
  End;

  TWPRangeList = Class (TFslObjectList)
  Private
    Function GetRange(iIndex: Integer): TWPRange;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TWPRangeList; Overload;
    Function Clone : TWPRangeList; Overload;

    Property Range[iIndex : Integer] : TWPRange Read GetRange; Default;
  End;


Const
  ALL_RANGESEARCHITEMS = [rsiPara, rsiField, rsiTableCell, rsiTableRow, rsiTable, rsiSection];

Type
  TWPRangeManager = Class (TFslObject)
    Private
      FList : TWPRangeList;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Clear; Overload; Virtual;
      Procedure Add(oRange : TWPRange); Overload; Virtual;
      Procedure Remove(oRange : TWPRange); Overload; Virtual;

      // coordination between Ranges
      Procedure RangeDo(oOperation : TWPOperation);
      Procedure RangeUndo(oOperation : TWPOperation);
      Procedure RangeRedo(oOperation : TWPOperation);

      Property List : TWPRangeList read FList;
  End;

  TWPPropertyServer = Class (TFslObject)
    Private
      FOwner : TObject;
      Procedure AddDocumentProperties(oList: TWPPropertyList);
      Procedure AddFieldProperties(oList: TWPPropertyList);
      Procedure AddImageProperties(oList: TWPPropertyList);
      Procedure AddLineProperties(oList: TWPPropertyList);
      Procedure AddParagraphProperties(oList: TWPPropertyList);
      Procedure AddSectionProperties(oList: TWPPropertyList);
      Procedure AddSelectionProperties(oList: TWPPropertyList);
      Procedure AddTableCellProperties(oList: TWPPropertyList);
      Procedure AddTableProperties(oList: TWPPropertyList);
      Procedure AddTableRowProperties(oList: TWPPropertyList);
      Procedure CheckAddTextProperties(oList: TWPPropertyList);
      Procedure AddHotspot(oList: TWPPropertyList; oHotSpot : TWPHotspot; iOffset : Integer);
      Procedure AddTableItem(oList : TWPPropertyList; oItem : TWPWorkingDocumentTableItemPiece; iOffset : Integer);

      Function ReadStyleName(Const sValue : String) : String;
      Function ReadFontName(Const sValue : String) : String;
      Function ReadKey(Const sValue : String) : String;
      Function ReadWord(Const sValue : String) : Word;
      Function ReadInteger(Const sValue : String; bNegAllowed : Boolean) : Integer;
      Function ReadFloat(Const sValue : String; bNonPosAllowed : Boolean) : Real;
      Function ReadBoolean(Const sValue : String) : Boolean;
      Function ReadColour(Const sValue : String) : TColour;
      Function ReadEnum(Const sValue : String; aValues : Array Of String) : Integer;
      Function ReadTristate(Const sValue : String) : TWPSTristate;

      Procedure SetFieldProperty(iId : Integer; Const sValue : String);
      Procedure SetImageProperty(iId : Integer; Const sValue : String);
      Procedure SetParagraphProperty(iId : Integer; Const sValue : String);
      Procedure SetTableCellProperty(iId : Integer; Const sValue : String);
      Procedure SetTableRowProperty(iId : Integer; Const sValue : String);
      Procedure SetTableProperty(iId : Integer; Const sValue : String);
      Procedure SetSectionProperty(iId : Integer; Const sValue : String);
      Procedure SetLineProperty(iId : Integer; Const sValue : String);
      Procedure SetDocProperty(iId : Integer; Const sValue : String);
    Public
      Function GetProperties : TWPPropertyList; // returns a list of current properties and their values
      Procedure SetProperty(oProperty : TWPProperty; Const sValue : String);
      Property Owner : TObject Read FOwner Write FOwner;

  End;

  TWPMacroActionType = (matKey);

  TWPMacroAction = class (TFslObject)
  Protected
    function GetActionType: TWPMacroActionType; Virtual;
    Procedure Save(oText : TFslTextFormatter); Virtual;
  public
    Function Link : TWPMacroAction;
    Property ActionType : TWPMacroActionType read GetActionType;
  End;

  TWPMacroKeyAction = class (TWPMacroAction)
  private
    FShift: TWPShiftStates;
    FKey: Word;
    class function read(sLine : String): TWPMacroKeyAction;
  Protected
    function GetActionType: TWPMacroActionType; Override;
    Procedure Save(oText : TFslTextFormatter); Override;
  public
    constructor Create(iKey: Word; aShift: TWPShiftStates); Overload;

    Property Key: Word read FKey write FKey;
    Property Shift: TWPShiftStates read FShift write FShift;
  End;

  TWPMacroActions = class (TFslObjectList)
  private
    function GetActions(iIndex: Integer): TWPMacroAction;
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Property Actions[iIndex : Integer] : TWPMacroAction read GetActions; default;
  End;

  TWPMacroState = (msIdle, msRecording, msPlaying);

  TWPMacro = class (TFslObject)
  Private
    FState : TWPMacroState;
    FLastError : Boolean;
    FLastErrorTime : TDateTime;
    FActions : TWPMacroActions;
    procedure SetState(const Value: TWPMacroState);
    procedure SetLastError(const Value: Boolean);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Recording : Boolean;

    Procedure Save(oStream : TFslStream);
    Procedure Load(oStream : TFslStream);

    Property Actions : TWPMacroActions read FActions;
    Property State : TWPMacroState read FState write SetState;
    Property LastError : Boolean read FLastError write SetLastError;
    Property LastErrorTime : TDateTime read FLastErrorTime write FLastErrorTime;
  End;


Const
  NAMES_ShiftState : array [TWPShiftState] of String = ('Shift', 'Alt', 'Ctrl', 'Left', 'Right', 'Middle', 'Double');

type
  TWPCodeCompletionListBoxInsertCompletionItemEvent = procedure (oItem : TWPCompletionItem) Of Object;

  TWPCodeCompletionListBox = class (TListbox)
  Private
    FList : TWPCompletionItems;
    FOnInsert: TWPCodeCompletionListBoxInsertCompletionItemEvent;
    Procedure SetList(Const Value: TWPCompletionItems);
  Protected
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); Override;
    procedure DblClick; Override;
  Public

    destructor Destroy; Override;
    Property List : TWPCompletionItems Read FList Write SetList;
    Property OnInsert : TWPCodeCompletionListBoxInsertCompletionItemEvent read FOnInsert Write FOnInsert;
  End;

Implementation

Uses
  wp_imaging, wp_rtf, wp_html, wp_native, wp_odt, wp_text,
  FHIR.WP.Control, FHIR.WP.Dialogs;

{ TWPRange }

Var
  GId : Integer = 0;

Constructor TWPRange.Create;
Begin
  Inherited;
  Inc(GId);
  FId := GId;
  FFont := TWPSFontDetails.Create;
  FParagraph := TWPSParagraphDetails.Create;
  FSelection := TWPSelection.Create;
  FLog := TWPActionsList.Create;

  FSelectedRows := TWPWorkingDocumentTableRowStartPieces.Create;
  FSelectedRows.Hooking := False;
  FSelectedCells := TWPWorkingDocumentTableCellStartPieces.Create;
  FSelectedCells.Hooking := False;

  FCanManipulateColumns := True;
  FCanConvertTextToTable := True;
  FStart := now;
End;

Destructor TWPRange.Destroy;
Begin
  FParagraph.Free;
  FFont.Free;
  FDocument.Free;
  FSelection.Free;
  FOperator.Free;
  FLog.free;

  FSelectedRows.Free;
  FSelectedCells.Free;
  Inherited;
End;

Function TWPRange.Link: TWPRange;
Begin
  Result := TWPRange(Inherited Link);
End;

Procedure TWPRange.SetDocument(Const Value: TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;


Procedure TWPRange.SetOperator(Const Value: TWPOperator);
Begin
  FOperator.Free;
  FOperator := Value;
End;


Function TWPRange.HasCurrentParagraph : Boolean;
Begin
  Result := Assigned(FCurrentParagraph);
End;


Function TWPRange.HasCurrentImage : Boolean;
Begin
  Result := Assigned(FCurrentImage);
End;


Function TWPRange.HasCurrentLine : Boolean;
Begin
  Result := Assigned(FCurrentLine);
End;


Function TWPRange.HasCurrentSectionStart : Boolean;
Begin
  Result := Assigned(FCurrentSectionStart);
End;


Function TWPRange.HasCurrentSectionStop : Boolean;
Begin
  Result := Assigned(FCurrentSectionStop);
End;


Function TWPRange.HasCurrentFieldStart : Boolean;
Begin
  Result := Assigned(FCurrentFieldStart);
End;


Function TWPRange.HasCurrentFieldStop : Boolean;
Begin
  Result := Assigned(FCurrentFieldStop);
End;


Function TWPRange.HasCurrentTableStart : Boolean;
Begin
  Result := Assigned(FCurrentTableStart);
End;


Function TWPRange.HasCurrentTableStop : Boolean;
Begin
  Result := Assigned(FCurrentTableStop);
End;


Function TWPRange.HasCurrentTableRowStart : Boolean;
Begin
  Result := Assigned(FCurrentTableRowStart);
End;


Function TWPRange.HasCurrentTableRowStop : Boolean;
Begin
  Result := Assigned(FCurrentTableRowStop);
End;


Function TWPRange.HasCurrentTableCellStart : Boolean;
Begin
  Result := Assigned(FCurrentTableCellStart);
End;


Function TWPRange.HasCurrentTableCellStop : Boolean;
Begin
  Result := Assigned(FCurrentTableCellStop);
End;

Function TWPRange.HasSelectedTable : Boolean;
Begin
  Result := Assigned(FSelectedTable);
End;

Function TWPRange.HasSelectedRows : Boolean;
Begin
  Result := FSelectedRows.Count > 0;
End;

Function TWPRange.HasSelectedCells : Boolean;
Begin
  Result := FSelectedCells.Count > 0;
End;

Function TWPRange.GetCurrentParagraph : TWPWorkingDocumentParaPiece;
Begin
  Assert(CheckCondition(HasCurrentParagraph, 'GetCurrentParagraph', 'CurrentParagraph is not assigned'));
  Result := FCurrentParagraph;
End;


Function TWPRange.GetCurrentImage : TWPWorkingDocumentImagePiece;
Begin
  Assert(CheckCondition(HasCurrentImage, 'GetCurrentImage', 'CurrentImage is not assigned'));
  Result := FCurrentImage;
End;


Function TWPRange.GetCurrentLine : TWPWorkingDocumentBreakPiece;
Begin
  Assert(CheckCondition(HasCurrentLine, 'GetCurrentLine', 'CurrentLine is not assigned'));
  Result := FCurrentLine;
End;


Function TWPRange.GetCurrentSectionStart : TWPWorkingDocumentSectionStartPiece;
Begin
  Assert(CheckCondition(HasCurrentSectionStart, 'GetCurrentSectionStart', 'CurrentSectionStart is not assigned'));
  Result := FCurrentSectionStart;
End;


Function TWPRange.GetCurrentSectionStop : TWPWorkingDocumentStopPiece;
Begin
  Assert(CheckCondition(HasCurrentSectionStop, 'GetCurrentSectionStop', 'CurrentSectionStop is not assigned'));
  Assert(CheckCondition(FCurrentSectionStop.StopType = stSection, 'GetCurrentSectionStop', 'CurrentSectionStop is of invalid type'));
  Result := FCurrentSectionStop;
End;


Function TWPRange.GetCurrentFieldStart : TWPWorkingDocumentFieldStartPiece;
Begin
  Assert(CheckCondition(HasCurrentFieldStart, 'GetCurrentFieldStart', 'CurrentFieldStart is not assigned'));
  Result := FCurrentFieldStart;
End;


Function TWPRange.GetCurrentFieldStop : TWPWorkingDocumentFieldStopPiece;
Begin
  Assert(CheckCondition(HasCurrentFieldStop, 'GetCurrentFieldStop', 'CurrentFieldStop is not assigned'));
  Result := FCurrentFieldStop;
End;


Function TWPRange.GetCurrentTableStart : TWPWorkingDocumentTableStartPiece;
Begin
  Assert(CheckCondition(HasCurrentTableStart, 'GetCurrentTableStart', 'CurrentTableStart is not assigned'));
  Result := FCurrentTableStart;
End;


Function TWPRange.GetCurrentTableStop : TWPWorkingDocumentStopPiece;
Begin
  Assert(CheckCondition(HasCurrentTableStop, 'GetCurrentTableStop', 'CurrentTableStop is not assigned'));
  Assert(CheckCondition(FCurrentTableStop.StopType = stTable, 'GetCurrentSectionStop', 'CurrentSectionStop is of invalid type'));
  Result := FCurrentTableStop;
End;


Function TWPRange.GetCurrentTableRowStart : TWPWorkingDocumentTableRowStartPiece;
Begin
  Assert(CheckCondition(HasCurrentTableRowStart, 'GetCurrentTableRowStart', 'CurrentTableRowStart is not assigned'));
  Result := FCurrentTableRowStart;
End;


Function TWPRange.GetCurrentTableRowStop : TWPWorkingDocumentStopPiece;
Begin
  Assert(CheckCondition(HasCurrentTableRowStop, 'GetCurrentTableRowStop', 'CurrentTableRowStop is not assigned'));
  Assert(CheckCondition(FCurrentTableRowStop.StopType = stTableRow, 'GetCurrentSectionStop', 'CurrentSectionStop is of invalid type'));
  Result := FCurrentTableRowStop;
End;


Function TWPRange.GetCurrentTableCellStart : TWPWorkingDocumentTableCellStartPiece;
Begin
  Assert(CheckCondition(HasCurrentTableCellStart, 'GetCurrentTableCellStart', 'CurrentTableCellStart is not assigned'));
  Result := FCurrentTableCellStart;
End;


Function TWPRange.GetCurrentTableCellStop : TWPWorkingDocumentStopPiece;
Begin
  Assert(CheckCondition(HasCurrentTableCellStop, 'GetCurrentTableCellStop', 'CurrentTableCellStop is not assigned'));
  Assert(CheckCondition(FCurrentTableCellStop.StopType = stTableCell, 'GetCurrentSectionStop', 'CurrentSectionStop is of invalid type'));
  Result := FCurrentTableCellStop;
End;

Function TWPRange.GetSelectedTable : TWPWorkingDocumentTableStartPiece;
Begin
  Result := FSelectedTable;
End;

Function TWPRange.GetSelectedTableStop : TWPWorkingDocumentStopPiece;
Begin
  Assert(CheckCondition(HasSelectedTable, 'GetSelectedTableStop', 'Table is not selected'));
  Result := TWPWOrkingDocumentStopPiece(FindNextPiece(SelectedTable, ptTableStop, ptTableStart, False));
End;

Function TWPRange.GetSelectedRows : TWPWorkingDocumentTableRowStartPieces;
Begin
  Result := FSelectedRows;
End;

Function TWPRange.GetSelectedCells : TWPWorkingDocumentTableCellStartPieces;
Begin
  Result := FSelectedCells;
End;

Procedure TWPRange.ClearCurrentStatus;
Begin
  FCurrentParagraph := Nil;
  FCurrentImage := Nil;
  FCurrentLine := Nil;
  FCurrentSectionStart := Nil;
  FCurrentSectionStop := Nil;
  FCurrentFieldStart := Nil;
  FCurrentFieldStop := Nil;
  FCurrentTableStart := Nil;
  FCurrentTableStop := Nil;
  FCurrentTableRowStart := Nil;
  FCurrentTableRowStop := Nil;
  FCurrentTableCellStart := Nil;
  FCurrentTableCellStop := Nil;

  FSelectedTable := Nil;
  FSelectedRows.Clear;
  FSelectedCells.Clear;
End;


Procedure TWPRange.UpdateCurrentStatus;
Var
  aSearch : TRangeSearchItems;
  iStart : Integer;
  iEnd : Integer;
Begin
  ClearCurrentStatus;
  If Selection.HasSelection Then
  Begin
    aSearch := SearchSelection;
    iStart := Selection.SelStart-1;
    iEnd := Selection.SelEnd+1;
  End
  Else
  Begin
    aSearch := ALL_RangeSearchITEMS;
    iStart := Selection.Cursor - 1;
    iEnd := Selection.Cursor;
    LookForImageAndLine;
  End;

  SearchBackwards(aSearch, iStart);
  SearchForwards(aSearch, iEnd);
  FindMissingPartners;
End;


procedure TWPRange.LogAction(action, details: String; ok: boolean);
var
  oAction : TWPAction;
begin
  if FLog.Count > INTERNAL_LOG_LIMIT then
    FLog.DeleteByIndex(0);
  oAction := TWPAction.Create;
  try
    oAction.time := inttostr(trunc((now - FStart) / DATETIME_MILLISECOND_ONE));
    oAction.Action := action;
    oAction.Selection := FSelection.Summary;
    oAction.Details := details;
    oAction.outcome := BoolToStr(ok);
    FLog.Add(oAction.Link);
  finally
    oAction.Free;
  end;
end;

procedure TWPRange.LogAction(action, details: String);
var
  oAction : TWPAction;
begin
  if FLog.Count > INTERNAL_LOG_LIMIT then
    FLog.DeleteByIndex(0);
  oAction := TWPAction.Create;
  try
    oAction.time := inttostr(trunc((now - FStart) / DATETIME_MILLISECOND_ONE));
    oAction.Action := action;
    oAction.Selection := FSelection.Summary;
    oAction.Details := details;
    FLog.Add(oAction.Link);
  finally
    oAction.Free;
  end;
end;

Procedure TWPRange.LookForImageAndLine;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  If Document.GetPieceByPosition(FSelection.Cursor, oPiece, iInternal, iIndex) Then
    Begin
    If oPiece.pieceType = ptImage Then
      FCurrentImage := TWPWorkingDocumentImagePiece(oPiece)
    Else If (iIndex > 0) And (iInternal = 0) And (Document.Pieces[iIndex-1].pieceType = ptImage) Then
      FCurrentImage := TWPWorkingDocumentImagePiece(Document.Pieces[iIndex-1])
    Else If (oPiece.pieceType = ptBreak) And (TWPWorkingDocumentBreakPiece(oPiece).IsLineBreak) Then
      FCurrentLine := TWPWorkingDocumentBreakPiece(oPiece)
    Else If (iIndex > 0) And (iInternal = 0) And (Document.Pieces[iIndex-1].pieceType = ptBreak) And (TWPWorkingDocumentBreakPiece(Document.Pieces[iIndex-1]).IsLineBreak) Then
      FCurrentLine := TWPWorkingDocumentBreakPiece(Document.Pieces[iIndex-1])
    End;
End;

Function TWPRange.SearchSelection : TRangeSearchItems;
Var
  oIterator : TWPPieceIterator;
  bFirst : Boolean;
  bNoTableItems: Boolean;
Begin
  Result := ALL_RangeSearchITEMS;
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
    oIterator.First;
    bFirst := True;
    bNoTableItems := False;
    While oIterator.More Do
    Begin
      If Not bFirst Then
      Begin
        FCurrentImage := Nil;
        FCurrentLine := Nil;
      End;

      If HasCurrentParagraph Then
        FCurrentParagraph := Nil; // cause we have encountered text after the end of the first paragraph, therefore the selection spans more than one paragraph

      If HasCurrentFieldStop Then
      Begin
        FCurrentFieldStart := Nil;
        FCurrentFieldStop := Nil;
      End;

      Case oIterator.Current.PieceType Of
        ptImage :
          If bFirst Then
            FCurrentImage := TWPWorkingDocumentImagePiece(oIterator.Current);
        ptBreak :
          If bFirst And (TWPWorkingDocumentBreakPiece(oIterator.Current).IsLineBreak) Then
            FCurrentLine := TWPWorkingDocumentBreakPiece(oIterator.Current);
        ptPara :
          If rsiPara In Result Then
          Begin
            FCurrentParagraph := TWPWorkingDocumentParaPiece(oIterator.Current);
            Exclude(Result, rsiPara);
          End;
        ptFieldStart :
          If bFirst And (rsiField In Result) Then
          Begin
            FCurrentFieldStart := TWPWorkingDocumentFieldStartPiece(oIterator.Current);
            Exclude(Result, rsiField);
          End;
        ptFieldStop :
          Begin
          If (rsiField In Result) Or HasCurrentFieldStart Then
            FCurrentFieldStop := TWPWorkingDocumentFieldStopPiece(oIterator.Current);
          Exclude(Result, rsiField);
          End;

        // because of the way that selection in sections and tables works, any
        // selection spanning an item means it cannot be current
        ptCellStop : Exclude(Result, rsiTableCell);
        ptCellStart:
          Begin
            If Not bNoTableItems Then FSelectedCells.Add(TWPWorkingDocumentTableCellStartPiece(oIterator.Current).Link);
            Exclude(Result, rsiTableCell);
          End;
        ptRowStop : Exclude(Result, rsiTableRow);
        ptRowStart:
          Begin
            If Not bNoTableItems Then FSelectedRows.Add(TWPWorkingDocumentTableRowStartPiece(oIterator.Current).Link);
            Exclude(Result, rsiTableRow);
          End;
        ptTableStop : Exclude(Result, rsiTable);
        ptTableStart:
          Begin
            // selection can't span multiple table
            bNoTableItems := True;
            FSelectedCells.Clear;
            FSelectedRows.Clear;
            Exclude(Result, rsiTable);
          End;
        ptSectionStart, ptSectionStop : Exclude(Result, rsiSection);
      End;
      oIterator.Next;
      bFirst := False;
    End;
  Finally
    oIterator.Free;
  End;
End;

Procedure TWPRange.SearchBackwards(aSearch : TRangeSearchItems; iEnd : Integer);
Var
  oIterator : TWPPieceIterator;
  iSectionCount : Integer;
  oTable: TWPWorkingDocumentTableStartPiece;
  oRow : TWPWorkingDocumentTableRowStartPiece;
  oCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  oTable := Nil;
  oRow := Nil;
  oCell := Nil;

  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.SetBounds(0, iEnd);
    oIterator.Last;
    iSectionCount := 1;
    While oIterator.More And (rsiSection In aSearch) Do
    Begin
      Case oIterator.Current.PieceType Of
        ptFieldStart :
          If (rsiField In aSearch) Then
          Begin
            FCurrentFieldStart := TWPWorkingDocumentFieldStartPiece(oIterator.Current);
            Exclude(aSearch, rsiField);
          End;
        ptFieldStop :
          Exclude(aSearch, rsiField);

        ptTableStart :
          Begin
            If (oTable = Nil) Then oTable := TWPWorkingDocumentTableStartPiece(oIterator.Current);
            If (rsiTable In aSearch) Then
            Begin
              FCurrentTableStart := TWPWorkingDocumentTableStartPiece(oIterator.Current);
              Exclude(aSearch, rsiTable);
            End;
          End;
        ptTableStop :
          Exclude(aSearch, rsiTable);

        ptRowStart :
          Begin
            If (oRow = Nil) Then oRow := TWPWorkingDocumentTableRowStartPiece(oIterator.Current);
            If (rsiTableRow In aSearch) Then
            Begin
              FCurrentTableRowStart := TWPWorkingDocumentTableRowStartPiece(oIterator.Current);
              Exclude(aSearch, rsiTableRow);
            End;
          End;
        ptRowStop :
          Exclude(aSearch, rsiTableRow);

        ptCellStart :
          Begin
            If (oCell = Nil) Then oCell := TWPWorkingDocumentTableCellStartPiece(oIterator.Current);
            If (rsiTableCell In aSearch) Then
            Begin
              FCurrentTableCellStart := TWPWorkingDocumentTableCellStartPiece(oIterator.Current);
              Exclude(aSearch, rsiTableCell);
            End;
          End;
        ptCellStop :
          Exclude(aSearch, rsiTableCell);

        ptSectionStop :
          Inc(iSectionCount);
        ptSectionStart :
          Begin
          Dec(iSectionCount);
          If (iSectionCount = 0) And  (rsiSection In aSearch) Then
            Begin
            FCurrentSectionStart := TWPWorkingDocumentSectionStartPiece(oIterator.Current);
            Exclude(aSearch, rsiSection);
            End;
          End;
      End;
      oIterator.Prev;
    End;
  Finally
    oIterator.Free;
  End;
  If Selection.HasSelection Then
  Begin
    If oTable <> Nil Then FSelectedTable := oTable;
    If oRow <> Nil Then FSelectedRows.Insert(0, oRow.Link);
    If oCell <> Nil Then FSelectedCells.Insert(0, oCell.Link);
  End;
End;


Procedure TWPRange.SearchForwards(aSearch : TRangeSearchItems; iStart : Integer);
Var
  oIterator : TWPPieceIterator;
Begin
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.SetBounds(iStart, Document.CharCount);
    oIterator.First;
    While oIterator.More And (rsiPara In aSearch) Do
    Begin
      If oIterator.Current.PieceType = ptPara Then
      Begin
        FCurrentParagraph := TWPWorkingDocumentParaPiece(oIterator.Current);
        Exclude(aSearch, rsiPara);
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Procedure TWPRange.FindMissingPartners;
Begin
  If HasCurrentSectionStart And Not HasCurrentSectionStop Then
    FCurrentSectionStop := TWPWorkingDocumentStopPiece(FindNextPiece(FCurrentSectionStart, ptSectionStop, ptSectionStart, True));
  If HasCurrentSectionStop And Not HasCurrentSectionStart Then
    FCurrentSectionStart := TWPWorkingDocumentSectionStartPiece(FindPreviousPiece(FCurrentSectionStop, ptSectionStart, ptSectionStop, True));

  If HasCurrentFieldStart And Not HasCurrentFieldStop Then
    FCurrentFieldStop := TWPWorkingDocumentFieldStopPiece(FindNextPiece(FCurrentFieldStart, ptFieldStop, ptFieldStart, False));
  If HasCurrentFieldStop And Not HasCurrentFieldStart Then
    FCurrentFieldStart := TWPWorkingDocumentFieldStartPiece(FindPreviousPiece(FCurrentFieldStop, ptFieldStart, ptFieldStop, False));

  If HasCurrentTableStart And Not HasCurrentTableStop Then
    FCurrentTableStop := TWPWorkingDocumentStopPiece(FindNextPiece(FCurrentTableStart, ptTableStop, ptTableStart, False));
  If HasCurrentTableStop And Not HasCurrentTableStart Then
    FCurrentTableStart := TWPWorkingDocumentTableStartPiece(FindPreviousPiece(FCurrentTableStop, ptTableStart, ptTableStop, False));

  If HasCurrentTableRowStart And Not HasCurrentTableRowStop Then
    FCurrentTableRowStop := TWPWorkingDocumentStopPiece(FindNextPiece(FCurrentTableRowStart, ptRowStop, ptRowStart, False));
  If HasCurrentTableRowStop And Not HasCurrentTableRowStart Then
    FCurrentTableRowStart := TWPWorkingDocumentTableRowStartPiece(FindPreviousPiece(FCurrentTableRowStop, ptRowStart, ptRowStop, False));

  If HasCurrentTableCellStart And Not HasCurrentTableCellStop Then
    FCurrentTableCellStop := TWPWorkingDocumentStopPiece(FindNextPiece(FCurrentTableCellStart, ptCellStop, ptCellStart, False));
  If HasCurrentTableCellStop And Not HasCurrentTableCellStart Then
    FCurrentTableCellStart := TWPWorkingDocumentTableCellStartPiece(FindPreviousPiece(FCurrentTableCellStop, ptCellStart, ptCellStop, False));
End;


Function TWPRange.FindNextPiece(oSource : TWPWorkingDocumentPiece; aFind, aStop : TWPWorkingDocumentPieceType; bNested : Boolean ) : TWPWorkingDocumentPiece;
Var
  iIndex : Integer;
  iNest : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  iNest := 0;
  Result := Nil;
  iIndex := Document.Pieces.IndexByReference(oSource) + 1;
  While (iIndex < Document.Pieces.Count) And Not Assigned(Result) Do
  Begin
    oPiece := Document.Pieces[iIndex];
    If oPiece.PieceType = aStop Then
    Begin
      If bNested Then
        Inc(iNest)
      Else
        RaiseError('FindNextPiece', StringFormat('Found %s looking for %s', [NAMES_WPPieceTYPE[aStop],NAMES_WPPieceTYPE[aFind]]));
    End
    Else If (oPiece.PieceType = aFind) Then
    Begin
      If (iNest = 0) Then
        Result := oPiece
      Else
        Dec(iNest);
    End;
    Inc(iIndex);
  End;
  Assert(Invariants('FindNextPiece', Result, TWPWorkingDocumentPiece, 'Result'));
End;


Function TWPRange.FindPreviousPiece(oSource : TWPWorkingDocumentPiece; aFind, aStop : TWPWorkingDocumentPieceType; bNested : Boolean ) : TWPWorkingDocumentPiece;
Var
  iIndex : Integer;
  iNest : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  iNest := 0;
  Result := Nil;
  iIndex := Document.Pieces.IndexByReference(oSource) - 1;
  While (iIndex >= 0) And Not Assigned(Result) Do
  Begin
    oPiece := Document.Pieces[iIndex];
    If oPiece.PieceType = aStop Then
    Begin
      If bNested Then
        Inc(iNest)
      Else
        RaiseError('FindNextPiece', StringFormat('Found %s looking for %s', [NAMES_WPPIECETYPE[aStop],NAMES_WPPIECETYPE[aFind]]));
    End
    Else If (oPiece.PieceType = aFind) Then
    Begin
      If (iNest = 0) Then
        Result := oPiece
      Else
        Dec(iNest);
    End;
    Dec(iIndex);
  End;
  Assert(Invariants('FindNextPiece', Result, TWPWorkingDocumentPiece, 'Result'));
End;


Function TWPRange.GetDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Function TWPRange.GetOperator : TWPOperator;
Begin
  Assert(Invariants('GetOperator', FOperator, TWPOperator, 'Operator_'));
  Result := FOperator;
End;


Procedure TWPRange.ApplyBaseStyle(oPiece: TWPWorkingDocumentPiece);
Begin
  oPiece.Style := Style;
  oPiece.Font.Assign(Font);
End;

Procedure TWPRange.SetStyle(Const sName : String);
Begin
  If sName <> FStyle Then
  Begin
    FStyle := sName;
    ApplyStyle(sName);
  End;
End;

Function TWPRange.ApplyBold(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Bold := ToTriState(bValue);
    Result := ApplyFont(oFont);
    if result then
      Font.Bold := oFont.Bold;
  Finally
    oFont.Free;
  End;
End;


Function TWPRange.ApplyBold(aValue: TWPSTriState) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Bold := aValue;
    Result := ApplyFont(oFont);
    if result then
      Font.Bold := oFont.Bold;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyItalic(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Italic := ToTriState(bValue);
    Result := ApplyFont(oFont);
    if result then
      Font.Italic := oFont.Italic;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyItalic(aValue: TWPSTriState) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Italic := aValue;
    Result := ApplyFont(oFont);
    if result then
      Font.Italic := oFont.Italic;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyStrikethrough(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Strikethrough := ToTriState(bValue);
    Result := ApplyFont(oFont);
    if result then
      Font.Strikethrough := oFont.Strikethrough;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyStrikethrough(aValue: TWPSTriState) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Strikethrough := aValue;
    Result := ApplyFont(oFont);
    if result then
      Font.Strikethrough := oFont.Strikethrough;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyState(aValue: TWPSFontState) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.State := aValue;
    Result := ApplyFont(oFont);
    if result then
      Font.State := oFont.State;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplySubscript(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    If bValue Then
      oFont.State := fsSubscript
    Else
      oFont.State := fsNormal;
    Result := ApplyFont(oFont);
    if result then
      Font.State := oFont.State;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplySuperscript(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    If bValue Then
      oFont.State := fsSuperscript
    Else
      oFont.State := fsNormal;
    Result := ApplyFont(oFont);
    if result then
      Font.State := oFont.State;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyUnderline(bValue: Boolean) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Underline := ToTriState(bValue);
    Result := ApplyFont(oFont);
    if result then
      Font.Underline := oFont.Underline;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyUnderline(aValue: TWPSTristate) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Underline := aValue;
    Result := ApplyFont(oFont);
    if result then
      Font.Underline := oFont.Underline;
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyForeground(iColour: TColour) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Foreground := iColour;
    Font.Foreground := oFont.Foreground;
    Result := ApplyFont(oFont);
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyBackground(iColour: TColour) : Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Background := iColour;
    Font.Background := oFont.Background;
    Result := ApplyFont(oFont);
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.IsBold: Boolean;
Begin
  Result := Font.Bold = tsTrue;
End;

Function TWPRange.IsItalic: Boolean;
Begin
  Result := Font.Italic = tsTrue;
End;

Function TWPRange.IsUnderline: Boolean;
Begin
  Result := Font.Underline = tsTrue;
End;

Function TWPRange.ApplyFontName(sName: String): Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Name := sName;
    Font.Name := oFont.Name;
    Result := ApplyFont(oFont);
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyFontSize(iSize: Integer): Boolean;
Var
  oFont : TWPSFontDetails;
Begin
  oFont := TWPSFontDetails.Create;
  Try
    oFont.Size := iSize;
    Font.Size := oFont.Size;
    Result := ApplyFont(oFont);
  Finally
    oFont.Free;
  End;
End;

Function TWPRange.ApplyFont(oFont: TWPSFontDetails): Boolean;
Var
  oIterator : TWPPieceIterator;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFormat In Capabilities) And ConsoleFontOk(oFont, FFont);
  LogAction('ApplyFont', oFont.Describe, Result);
  If Result Then
  begin
    if Selection.HasSelection Then
    Begin
      Operator_.StartOperation(FId, otChange, False, Selection);
      Operator_.CleanRange;
      oIterator := TWPPieceIterator.Create(FDocument.Link);
      Try
        oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.First;
        While oIterator.More Do
          Begin
          oIterator.Current.Font.Update(oFont);
          oIterator.Current.Change(ctLayout, oIterator.Current);
          oIterator.Next;
          End;
      Finally
        oIterator.Free;
      End;
      Operator_.FinishOperation;
      ChangeContent;
      ChangeState;
    End
    Else If Not Operator_.Settings.ReadOnly Then
    Begin
      If FirstInLine Then
      Begin
        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.ExpandRange(-1, 0);
        Operator_.CleanRange;
        oIterator := TWPPieceIterator.Create(FDocument.Link);
        Try
          oIterator.SetBounds(Selection.Cursor - 1, Selection.Cursor);
          oIterator.PieceTypes := ALL_PIECE_TYPES;
          oIterator.First;
          While oIterator.More Do
          Begin
            oIterator.Current.Font.Update(oFont);
            oIterator.Current.Change(ctLayout, oIterator.Current);
            oIterator.Next;
          End;
        Finally
          oIterator.Free;
        End;
        Operator_.FinishOperation;
        ChangeContent;
        ChangeState;
      End
      Else
      Begin
        Result := True;
        Operator_.CloseLastOperation;
        FFont.Update(oFont);
        ChangeReadyState;
      End;
    End;
  End;
End;


Function TWPRange.AlignCentre: Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    oParagraph.Align := WordProcessorParagraphAlignmentCentre;
    Result := ApplyParagraph(oParagraph);
    if result then
      Paragraph.Align := WordProcessorParagraphAlignmentCentre;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.AlignJustify: Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    oParagraph.Align := WordProcessorParagraphAlignmentJustify;
    Result := ApplyParagraph(oParagraph);
    if result then
      Paragraph.Align := WordProcessorParagraphAlignmentJustify;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.AlignLeft: Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    oParagraph.Align := WordProcessorParagraphAlignmentLeft;
    Result := ApplyParagraph(oParagraph);
    if result then
      Paragraph.Align := WordProcessorParagraphAlignmentLeft;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.AlignRight: Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    oParagraph.Align := WordProcessorParagraphAlignmentRight;
    Result := ApplyParagraph(oParagraph);
    if result then
      Paragraph.Align := WordProcessorParagraphAlignmentRight;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.ApplyBullets(bValue : Boolean): Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    If bValue Then
      oParagraph.ListType := WPSParagraphListTypeBullets
    Else
      oParagraph.ListType := WPSParagraphListTypeNone;
    Result := ApplyParagraph(oParagraph);
    if result then
      Paragraph.ListType := oParagraph.ListType;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.ListStartDecrement : Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  if (CurrentParagraph = nil) or (CurrentParagraph.Format.ListType <> WPSParagraphListTypeNumbers) or
     (CurrentParagraph.Format.FixedNumber = 0) or (CurrentParagraph.Format.FixedNumber = DEF_WORD) then
    result := false
  else
  begin
    oParagraph := TWPSParagraphDetails.Create;
    Try
      if CurrentParagraph.Format.FixedNumber <= 1 then
        oParagraph.FixedNumber := DEF_WORD
      else
        oParagraph.FixedNumber := CurrentParagraph.Format.FixedNumber - 1;
      Result := ApplyParagraph(oParagraph);
    Finally
      oParagraph.Free;
    End;
  end;
end;

Function TWPRange.ListStartIncrement : Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  if (CurrentParagraph = nil) or (CurrentParagraph.Format.ListType <> WPSParagraphListTypeNumbers) or (CurrentParagraph.Format.FixedNumber = 0) then
    result := false
  else
  begin
    oParagraph := TWPSParagraphDetails.Create;
    Try
      if CurrentParagraph.Format.FixedNumber = DEF_WORD then
        oParagraph.FixedNumber := 2
      else
        oParagraph.FixedNumber := CurrentParagraph.Format.FixedNumber + 1;
      Result := ApplyParagraph(oParagraph);
    Finally
      oParagraph.Free;
    End;
  end;
end;

Function TWPRange.ListStartReset : Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  if (CurrentParagraph = nil) or (CurrentParagraph.Format.ListType <> WPSParagraphListTypeNumbers) or (CurrentParagraph.Format.FixedNumber = 0) then
    result := false
  else
  begin
    oParagraph := TWPSParagraphDetails.Create;
    Try
      oParagraph.FixedNumber := DEF_WORD;
      Result := ApplyParagraph(oParagraph);
    Finally
      oParagraph.Free;
    End;
  end;
end;

Function TWPRange.ApplyNumbers(bValue : Boolean): Boolean;
Var
  oParagraph : TWPSParagraphDetails;
Begin
  oParagraph := TWPSParagraphDetails.Create;
  Try
    If bValue Then
      oParagraph.ListType := WPSParagraphListTypeNumbers
    Else
      oParagraph.ListType := WPSParagraphListTypeNone;
    oParagraph.NumberFormat := nwDot;
    Result := ApplyParagraph(oParagraph);
    if result then
    begin
      Paragraph.ListType := oParagraph.ListType;
      Paragraph.NumberFormat := oParagraph.NumberFormat;
    end;
  Finally
    oParagraph.Free;
  End;
End;

Function TWPRange.ApplyParagraph(oParagraph : TWPSParagraphDetails): Boolean;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  oLast : TWPWorkingDocumentParaPiece;
  sSaved : String;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFormat In Capabilities) And ConsoleParaOK(oParagraph);
  LogAction('ApplyParagraph', oParagraph.describe, result);
  If Result Then
    Begin
    // we may actually make changes outside the selected
    // So the first thing we have to do is to set the Selection up properly
    sSaved := Selection.Save;

    If Selection.HasSelection Then
      Begin
      oIterator := TWPPieceIterator.Create(Document.Link);
      Try
        If Not Document.GetPieceByPosition(Selection.SelEnd, oPiece, iInternal, iIndex) Then
          RaiseError('ApplyParagraph', 'Unable to find Selection end');
        While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
          Inc(iIndex);
        If FDocument.Pieces.ExistsByIndex(iIndex) Then
        Begin
          oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
          Selection.Select(Selection.SelStart, oLast.Metrics.Position+oLast.Metrics.CharCount);
        End;

        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);

        Operator_.CleanRange;
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
        oIterator.First;
        If oIterator.More Then
          Begin
          oLast := oIterator.CurrentParagraph;
          oLast.Change(oLast.Format.Update(oParagraph), oLast);
          While oIterator.More Do
            Begin
            If oLast <> oIterator.CurrentParagraph(True) Then
              Begin
              oLast := oIterator.CurrentParagraph(True);
              If Assigned(oLast) Then
                Begin
                oLast.Change(oLast.Format.Update(oParagraph), oLast);
                End;
              End;
            oIterator.Next;
            End;
          End
      Finally
        oIterator.Free;
      End;
      End
    Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
      Begin
      While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
        Inc(iIndex);
      If iIndex < FDocument.Pieces.count Then
        Begin
        oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
        sSaved := Selection.Save;
        Selection.Select(oLast.Metrics.Position, oLast.Metrics.Position+oLast.Metrics.CharCount);
        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);
        Operator_.CleanRange;
        oLast.Change(oLast.Format.Update(oParagraph), oLast);
        End;
      End;
    Operator_.FinishOperation;
    Selection.Restore(sSaved);
    ChangeContent;
    ChangeState;
    End;
End;


Function TWPRange.ApplyLeftIndent(iOffset: Integer): Boolean;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  oLast : TWPWorkingDocumentParaPiece;
  sSaved : String;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFormat In Capabilities) And Not Operator_.Settings.ConsoleMode;
  If Result Then
    Begin
    // we may actually make changes outside the selected
    // So the first thing we have to do is to set the Selection up properly
    sSaved := Selection.Save;

    If Selection.HasSelection Then
      Begin
      oIterator := TWPPieceIterator.Create(Document.Link);
      Try
        If Not Document.GetPieceByPosition(Selection.SelEnd, oPiece, iInternal, iIndex) Then
          RaiseError('ApplyParagraph', 'Unable to find Selection end');
        While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
          Inc(iIndex);
        oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
        Selection.Select(Selection.SelStart, oLast.Metrics.Position+oLast.Metrics.CharCount);

        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);

        Operator_.CleanRange;
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
        oIterator.First;
        If oIterator.More Then
          Begin
          oLast := oIterator.CurrentParagraph;
          If oLast.Format.LeftIndent = DEF_WORD Then
            oLast.Format.LeftIndent := IntegerMax(0, iOffset)
          Else
            oLast.Format.LeftIndent := IntegerMax(0, oLast.Format.LeftIndent + iOffset);
          oLast.Change(ctLayout, oLast);
          While oIterator.More Do
            Begin
            If oLast <> oIterator.CurrentParagraph Then
              Begin
              oLast := oIterator.CurrentParagraph;
              If oLast.Format.LeftIndent = DEF_WORD Then
                oLast.Format.LeftIndent := IntegerMax(0, iOffset)
              Else
                oLast.Format.LeftIndent := IntegerMax(0, oLast.Format.LeftIndent + iOffset);
              oLast.Change(ctLayout, oLast);
              End;
            oIterator.Next;
            End;
          End
      Finally
        oIterator.Free;
      End;
      End
    Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
      Begin
      While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
        Inc(iIndex);
      If iIndex < FDocument.Pieces.count Then
        Begin
        oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
        sSaved := Selection.Save;
        Selection.Select(oLast.Metrics.Position, oLast.Metrics.Position+oLast.Metrics.CharCount);
        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);
        Operator_.CleanRange;
        If oLast.Format.LeftIndent = DEF_WORD Then
          oLast.Format.LeftIndent := IntegerMax(0, iOffset)
        Else
          oLast.Format.LeftIndent := IntegerMax(0, oLast.Format.LeftIndent + iOffset);
        oLast.Change(ctLayout, oLast);
        End;
      End;
    Operator_.FinishOperation;
    Selection.Restore(sSaved);
    ChangeContent;
    ChangeState;
    End;
End;


Function TWPRange.ApplyStyle(Const sStyle: String) : Boolean;
Var
  oStyle : TWPStyle;
  oIterator : TWPPieceIterator;
  iStart : Integer;
  iEnd : Integer;
Begin
  oStyle := Operator_.Styles.GetByName(sStyle);

  If oStyle.HasParagraphAspect Then
    Result := ApplyParagraphStyle(oStyle)
  Else
  Begin
    Result := Not Operator_.Settings.ReadOnly And (Selection.HasSelection Or (Selection.Cursor = Document.CharCount - 1));
    If Result Then
      Begin
      Operator_.StartOperation(FId, otChange, False, Selection);
      If Selection.HasSelection Then
      Begin
        iStart := Selection.SelStart;
        iEnd := Selection.SelEnd;
      End
      Else
      Begin
        iStart := Selection.Cursor;
        iEnd := Selection.Cursor;
      End;
      // special case: if the Selection ends at the very end, we will
      // apply the style to the last paragraph marker
      // even though it is not selected (it can't be)
      If (iEnd = Document.CharCount - 1) Then
        Inc(iEnd);
      Operator_.SetRange(iStart, iEnd);
      Operator_.CleanRange;

      oIterator := TWPPieceIterator.Create(FDocument.Link);
      Try

        oIterator.SetBounds(iStart, iEnd);
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.First;
        While oIterator.More Do
          Begin
          oIterator.Current.Style := sStyle;
          oIterator.Current.Font.Defaults;
          oIterator.Current.Change(ctLayout, oIterator.Current);
          oIterator.Next;
          End;
      Finally
        oIterator.Free;
      End;
      Operator_.FinishOperation;
      ChangeContent;
      ChangeState;
      End
    Else If Not Operator_.Settings.ReadOnly Then
      Begin
      Result := True;
      Operator_.CloseLastOperation;
      ChangeReadyState;
      End;
  End;
End;

Function TWPRange.FirstInLine : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Document.GetPieceByPosition(IntegerMax(0, Selection.Cursor-1), oPiece, iInternal, iIndex);
  If Result Then
    Begin
    Result :=  oPiece.PieceType = ptPara;
    End;
End;


Function TWPRange.ApplyParagraphStyle(oStyle : TWPStyle) : Boolean;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  oLast : TWPWorkingDocumentParaPiece;
  oFirst : TWPWorkingDocumentPiece;
  sSaved : String;
  iStart : Integer;
  iEnd : Integer;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFormat In Capabilities);
  If Result Then
    Begin
    // we may actually make changes outside the selected 
    // So the first thing we have to do is to set the Selection up properly
    sSaved := Selection.Save;

    If Selection.HasSelection Then
    Begin
      iStart := Selection.SelStart;
      iEnd := Selection.SelEnd;
    End
    Else
    Begin
      iStart := Selection.Cursor;
      iEnd := Selection.Cursor;
    End;

    // adjust to include the end of a partially selected paragraph
    If Not Document.GetPieceByPosition(iEnd, oPiece, iInternal, iIndex) Then
      RaiseError('ApplyParagraph', 'Unable to find Selection end');
    While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
      Inc(iIndex);
    If FDocument.Pieces.ExistsByIndex(iIndex) Then
    Begin
      oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
      Selection.Select(iStart, oLast.Metrics.Position+oLast.Metrics.CharCount);
    End;

    // adjust to include the start of a partially selected paragraph
    If Not Document.GetPieceByPosition(iStart, oPiece, iInternal, iIndex) Then
      RaiseError('ApplyParagraph', 'Unable to find Selection Start');
    While (iIndex > 0) And (FDocument.Pieces[iIndex-1].PieceType In INLINE_PIECE_TYPES) Do
      Dec(iIndex);
    If FDocument.Pieces.ExistsByIndex(iIndex) Then
    Begin
      oFirst := FDocument.Pieces[iIndex];
      Selection.Select(oFirst.Metrics.Position, Selection.SelEnd);
    End;

    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.SetPriorCursor(sSaved);

    // ok, ready!
    Operator_.CleanRange;
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.First;
      If oIterator.More Then
        Begin
        oLast := oIterator.CurrentParagraph;
        oLast.Style := oStyle.Name;
        oLast.Format.Defaults;
        oLast.Change(ctLayout, oLast);
        While oIterator.More Do
          Begin
          oIterator.Current.Style := oStyle.Name;
          oIterator.Current.Font.Defaults;
          oIterator.Current.Change(ctLayout, oIterator.Current);
          If oLast <> oIterator.CurrentParagraph(True) Then
            Begin
            oLast := oIterator.CurrentParagraph(True);
            If Assigned(oLast) Then
              Begin
              oLast.Format.Update(oStyle.Paragraph);
              oLast.Change(ctLayout, oLast);
              End;
            End;
          oIterator.Next;
          End;
        End
    Finally
      oIterator.Free;
    End;

    Operator_.FinishOperation;
    Selection.Restore(sSaved);
    ChangeContent;
    ChangeState;
    End;
End;


Function TWPRange.SetLineProperties(oLine: TWPWorkingDocumentBreakPiece) : Boolean;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canLineProps In Capabilities) And HasCurrentLine And Not Operator_.Settings.ConsoleMode;
  LogAction('SetLineProperties', oLine.Describe, result);
  If Result Then
    Begin
    Selection.Select(CurrentLine.Metrics.Position, CurrentLine.Metrics.Position+1);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.CleanRange;
    CurrentLine.ApplyProperties(oLine);
    Operator_.FinishOperation;
    DoMove(CurrentLine.Metrics.Position, cdJump, False);
    ChangeContent;
    ChangeState;
    End;
End;

Function TWPRange.InsertLine : Boolean;
Var
  oBreak : TWPWorkingDocumentBreakPiece;
  oPiece : TWPWorkingDocumentPiece;
  oPara : TWPWorkingDocumentParaPiece;
  iNew : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canInsertLine In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertLine', '', result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;

    oPiece := Operator_.PieceBeforeAppend;
    If Not (oPiece = Nil) And Not (oPiece.PieceType In [ptBreak, ptPara, ptTableStop, ptRowStop, ptCellStop, ptSectionStop]) Then
    Begin
      oPara := TWPWorkingDocumentParaPiece.Create;
      Try
        ApplyBaseStyle(oPara);
        Operator_.AppendPiece(oPara.Link);
      Finally
        oPara.Free;
      End;
    End;

    oBreak := TWPWorkingDocumentBreakPiece.Create;
    Try
      oBreak.BreakType := btLine;
      oBreak.PenColour := clBlack;
      oBreak.Width := 1;
      oBreak.PenStyle := apsSolid;
      oBreak.PenWidth := 1;
      ApplyBaseStyle(oBreak);
      Operator_.AppendPiece(oBreak.Link);
      iNew := Operator_.EndOfRange;
    Finally
      oBreak.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.InsertPageBreak : Boolean;
Var
  oBreak : TWPWorkingDocumentBreakPiece;
  oPara : TWPWorkingDocumentParaPiece;
  oPiece : TWPWorkingDocumentPiece;
  iNew : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canInsertPageBreak In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertPageBreak', '', result);
  If Result Then
  Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;

    oPiece := Operator_.PieceBeforeAppend;
    If Not (oPiece = Nil) And Not (oPiece.PieceType In [ptBreak, ptPara, ptTableStop, ptRowStop, ptCellStop, ptSectionStop]) Then
    Begin
      oPara := TWPWorkingDocumentParaPiece.Create;
      Try
        ApplyBaseStyle(oPara);
        Operator_.AppendPiece(oPara.Link);
      Finally
        oPara.Free;
      End;
    End;

    oBreak := TWPWorkingDocumentBreakPiece.Create;
    Try
      oBreak.BreakType := btPageBreak;
      ApplyBaseStyle(oBreak);
      Operator_.AppendPiece(oBreak.Link);
      iNew := Operator_.EndOfRange;
    Finally
      oBreak.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
  End;
End;

Function TWPRange.SetSectionProperties(oSection: TWPWorkingDocumentSectionStartPiece) : Boolean;
Begin
  Result := Not Operator_.Settings.ReadOnly And HasCurrentSectionStart And Not Operator_.Settings.ConsoleMode;
  LogAction('SetSectionProperties', oSection.describe, result);
  If Result Then
    Begin
    Selection.Select(CurrentSectionStart.Metrics.Position, CurrentSectionStart.Metrics.Position+1);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.CleanRange;
    CurrentSectionStart.ApplyProperties(oSection);
    CurrentSectionStart.BuildDocSection;
    Operator_.FinishOperation;
    DoMove(CurrentSectionStart.Metrics.Position, cdJump, False);
    ChangeContent;
    ChangeState;
    End;
End;

Function TWPRange.ConvertTextToTable : Boolean;
Var
  sSaved : String;
  oPiece : TWPWorkingDocumentPiece;
  oEnd : TWPWorkingDocumentPiece;
  iNew : Integer;
Begin
  Result := (canConvertToTable In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('ConvertTextToTable', '', result);
  If Result Then
  Begin
    sSaved := Selection.Save;
    oPiece := SelectWholeParagraphs;
    Result := Assigned(oPiece);
    If Result Then
    Begin
      Operator_.StartOperation(FId, otTable, False, Selection);
      Operator_.SetPriorCursor(sSaved);
      Operator_.CleanRange; // still need to do it even though it's redundant
      oEnd := ConvertParagraphsToTable(oPiece);
      Document.RegenerateMetrics(False, False);
      iNew := oPiece.Metrics.Position;
      Operator_.SetClosingRange(oEnd.Metrics.Position + 1);   // it's 1 long
      Operator_.FinishOperation;
      ChangeContent;
      DoMove(iNew, cdJump);
    End;
  End;
End;


Function TWPRange.InsertTable(iRows, iCols : Integer) : Boolean;
Var
  iLoop : Integer;
  iNew : Integer;
  iAppendIndex: Integer;
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  oParagraph : TWPWorkingDocumentParaPiece;
Begin
  Result := (canInsertTable In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertTable', inttostr(iRows)+'x'+inttostr(iCols), result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;

    {  Because table is added piece-by-piece, we can't use normal append
       As when start piece is added, the document is no longer ballanced
    }
    iAppendIndex := Operator_.DirectAppendIndex;
    oParagraph := TWPWorkingDocumentParaPiece.Create;
    Try
      ApplyBaseStyle(oParagraph);
      iAppendIndex := Operator_.DirectAppendPiece(oParagraph.Link, iAppendIndex);
    Finally
      oParagraph.Free;
    End;

    oStart := TWPWorkingDocumentTableStartPiece.Create;
    Try
      ApplyBaseStyle(oStart);
      oStart.BorderPolicy := tbpGrid;
      iAppendIndex := Operator_.DirectAppendPiece(oStart.Link, iAppendIndex);
    Finally
      oStart.Free;
    End;

    iNew := Operator_.EndOfRange+2;

    For iLoop := 1 To iRows Do
      DoInsertRow(iCols, iAppendIndex);

    oStop := TWPWorkingDocumentStopPiece.Create(stTable);
    Try
      ApplyBaseStyle(oStop);
      Operator_.DirectAppendPiece(oStop.Link, iAppendIndex);
    Finally
      oStop.Free;
    End;

    Operator_.EndDirectAppend;
    Operator_.FinishOperation;
    Operator_.RendererRange.Clear;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;

Function TWPRange.InsertCopiedRowAbove(Var sError: String) : Boolean;
Var
  oDocument: TWPWorkingDocument;
  oStyles : TWPStyles;
  oClip : TWPClipboard;
Begin
  Result := (canInsertRowAbove In Capabilities) And Clipboard.HasContentType(wcctNative) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertCopiedRowAbove', '', result);
  If Result Then
  Begin
    oClip := TWPClipboard.Create;
    oStyles := Operator_.Styles.Clone;
    oDocument := Nil;
    Try
      oClip.Open;
      oDocument := ParseClipboardContent(TWPNativeReader, oClip, wcctNative, oStyles, Nil, sError);

      Result := CheckNoColumnsToInsert(oDocument, CurrentTableStart.ColumnCount, sError);
      If Result Then
        Result := DoInsertCopiedRows(oDocument, oStyles, CurrentTableRowStart.Metrics.Position)
      Else If CurrentTableStart.ColumnCount = 1 Then
        Result := DoInsertCopiedRow(oDocument, oStyles, CurrentTableRowStart.Metrics.Position);

    Finally
      oClip.Free;
      oDocument.Free;
      oStyles.Free;
    End;
  End;
End;


Function TWPRange.InsertCopiedRowBelow(Var sError: String) : Boolean;
Var
  oDocument: TWPWorkingDocument;
  oStyles : TWPStyles;
  oClip : TWPClipboard;
Begin
  Result := (canInsertRowAbove In Capabilities) And Clipboard.HasContentType(wcctNative) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertCopiedRowBelow', '', result);
  If Result Then
  Begin
    oClip := TWPClipboard.Create;
    oStyles := Operator_.Styles.Clone;
    oDocument := Nil;
    Try
      oClip.Open;
      oDocument := ParseClipboardContent(TWPNativeReader, oClip, wcctNative, oStyles, Nil, sError);

      Result := CheckNoColumnsToInsert(oDocument, CurrentTableStart.ColumnCount, sError);
      If Result Then
        Result := DoInsertCopiedRows(oDocument, oStyles, CurrentTableRowStop.Metrics.Position + CurrentTableRowStop.Metrics.CharCount)
      Else If CurrentTableStart.ColumnCount = 1 Then
        Result := DoInsertCopiedRow(oDocument, oStyles, CurrentTableRowStop.Metrics.Position + CurrentTableRowStop.Metrics.CharCount);

    Finally
      oClip.Free;
      oDocument.Free;
      oStyles.Free;
    End;
  End;
End;

Function TWPRange.InsertRowAbove : Boolean;
Var
  iNew : Integer;
  iCols : Integer;
  iAppendIndex: Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := (canInsertRowAbove In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertRowAbove', '', result);
  If Result Then
    Begin
    iCols := CurrentTableStart.ColumnCount;
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.SetRange(CurrentTableRowStart.Metrics.Position, CurrentTableRowStart.Metrics.Position);
    Operator_.CleanRange;
    iAppendIndex := Operator_.DirectAppendIndex;
    oPiece := DoInsertRow(iCols, iAppendIndex);
    Operator_.EndDirectAppend;
    CurrentTableStart.StructureDirty := True;
    Document.RegenerateMetrics(False, False);
    iNew := oPiece.Metrics.Position;
    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.InsertRowBelow : Boolean;
Var
  iNew : Integer;
  iCols : Integer;
  iAppendIndex: Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := (canInsertRowBelow In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertRowBelow', '', result);
  If Result Then
    Begin
    iCols := CurrentTableStart.ColumnCount;
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.SetRange(CurrentTableRowStop.Metrics.Position + CurrentTableRowStop.Metrics.CharCount,
                       CurrentTableRowStop.Metrics.Position + CurrentTableRowStop.Metrics.CharCount);
    Operator_.CleanRange;
    iAppendIndex := Operator_.DirectAppendIndex;
    oPiece := DoInsertRow(iCols, iAppendIndex);
    Operator_.EndDirectAppend;
    CurrentTableStart.StructureDirty := True;
    Document.RegenerateMetrics(False, False);
    iNew := oPiece.Metrics.Position;
    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump, True);
    End;
End;

Function TWPRange.MergeCells : Boolean;
Var
  s : String;
  iIndex, iNew : Integer;
Begin
  Result := canMergeCells In Capabilities;
  LogAction('MergeCells', '', result);
  If Result Then
  Begin
    s := Selection.Save;
    Operator_.StartOperation(FId, otTable, False, Selection);
    Operator_.SetRange(CurrentTableRowStart.Metrics.Position, CurrentTableRowStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
    Operator_.SetPriorCursor(s);
    Operator_.CleanRange;
    CurrentTableCellStart.Span := CurrentTableCellStart.Span + 1;
    // paragraph at end of cell
    iIndex := Document.Pieces.IndexByReference(CurrentTableCellStop.Prev);
    iNew := CurrentTableCellStop.Prev.Metrics.Position;
    Document.Pieces.DeleteByIndex(iIndex);
    Document.Pieces.DeleteByIndex(iIndex);
    Document.Pieces.DeleteByIndex(iIndex);
    CurrentTableStart.StructureDirty := True;
    Document.RegenerateMetrics(False, False);
    Operator_.SetClosingRange(CurrentTableRowStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
  End;
End;


Function TWPRange.CurrentColIndex : Integer;
Begin
  Assert(Invariants('CurrentColIndex', CurrentTableStart, TWPWorkingDocumentTableStartPiece, 'Current Table Start'));
  Assert(Invariants('CurrentColIndex', CurrentTableCellStart, TWPWorkingDocumentTableCellStartPiece, 'Current Table Cell Start'));

  Result := CurrentTableStart.ColumnIndexForCell(CurrentTableCellStart);
End;


Procedure TWPRange.DoInsertColumnInRow(oRow : TWPWorkingDocumentTableRowStartPiece; iIndex : Integer);
Var
  oCell : TWPWorkingDocumentPiece;
  iLoop : Integer;
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  oPara : TWPWorkingDocumentParaPiece;
Begin
  If iIndex < oRow.Cells.Count Then
    oCell := oRow.Cells[iIndex]
  Else
  Begin
    oCell := oRow.Cells[oRow.Cells.Count - 1];
    iLoop := Document.Pieces.IndexByReference(oCell);
    While oCell.PieceType <> ptRowStop Do
    Begin
      Inc(iLoop);
      oCell := Document.Pieces[iLoop]
    End;
  End;

  // we want to insert a cell before oCell

  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ApplyBaseStyle(oStart);
    Operator_.InsertPiece(oStart.Link, oCell);
  Finally
    oStart.Free;
  End;

  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    ApplyBaseStyle(oPara);
    Operator_.InsertPiece(oPara.Link, oCell);
  Finally
    oPara.Free;
  End;

  oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
  Try
    ApplyBaseStyle(oStop);
    Operator_.InsertPiece(oStop.Link, oCell);
  Finally
    oStop.Free;
  End;
End;


Procedure TWPRange.DoDeleteColumnInRow(oRow : TWPWorkingDocumentTableRowStartPiece; iIndex : Integer);
Var
  oCell : TWPWorkingDocumentPiece;
  iLoop : Integer;
Begin
  Assert(CheckCondition(iIndex < oRow.Cells.Count, 'DoDeleteColumnInRow', 'Column to delete is not valid'));
  oCell := oRow.Cells[iIndex];
  iLoop := Document.Pieces.IndexByReference(oCell);
  Assert(CheckCondition(iLoop > -1, 'DoDeleteColumnInRow', 'Cell not Found in Document'));

  While (iLoop < Document.Pieces.Count) And (Document.Pieces[iLoop].PieceType <> ptCellStop) Do
    Document.Pieces.DeleteByIndex(iLoop);
  If iLoop < Document.Pieces.Count Then
    Document.Pieces.DeleteByIndex(iLoop);
End;


Function TWPRange.DoInsertColumn(iIndex : Integer) : Boolean;
Var
  iNew : Integer;
  iRow : Integer;
  oTable : TWPWorkingDocumentTableStartPiece;
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oTable := CurrentTableStart;
  oRow := CurrentTableRowStart;
  Operator_.StartOperation(FId, otTable, False, Selection);
  Operator_.SetRange(CurrentTableStart.Metrics.Position,
                     CurrentTableStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
  Operator_.CleanRange;

  For iRow := 0 To oTable.Rows.Count - 1 Do
    DoInsertColumnInRow(oTable.Rows[iRow], iIndex);

  oTable.StructureDirty := True;
  Document.RegenerateMetrics(False, False);
  iNew := oRow.Cells[iIndex].Metrics.Position + 1;
  Operator_.SetClosingRange(CurrentTableStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
  Operator_.FinishOperation;
  ChangeContent;
  DoMove(iNew, cdJump);
  Result := True;
End;


Function TWPRange.DoDeleteColumn(iIndex : Integer) : Boolean;
Var
  iNew : Integer;
  iRow : Integer;
  oTable : TWPWorkingDocumentTableStartPiece;
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oTable := CurrentTableStart;
  oRow := CurrentTableRowStart;
  Operator_.StartOperation(FId, otTable, False, Selection);
  Operator_.SetRange(CurrentTableStart.Metrics.Position,
                     CurrentTableStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
  Operator_.CleanRange;

  For iRow := 0 To oTable.Rows.Count - 1 Do
    DoDeleteColumnInRow(oTable.Rows[iRow], iIndex);

  oTable.StructureDirty := True;
  Document.RegenerateMetrics(False, False);
  iNew := oRow.Cells[IntegerMax(0, iIndex-1)].Metrics.Position + 1;
  Operator_.SetClosingRange(CurrentTableStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
  Operator_.FinishOperation;
  ChangeContent;
  DoMove(iNew, cdJump);
  Result := True;
End;


Function TWPRange.InsertColumnLeft : Boolean;
Begin
  Result := (canInsertColumn In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertColumnLeft', '', result);
  If Result Then
    Result := DoInsertColumn(CurrentColIndex);
End;


Function TWPRange.InsertColumnRight : Boolean;
Begin
  Result := (canInsertColumn In Capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertColumnRight', '', result);
  If Result Then
    Result := DoInsertColumn(CurrentColIndex+1);
End;

Function TWPRange.TryAndFindTableAfter : Boolean;
Var
  oIterator : TWPPieceIterator;
Begin
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.SetBounds(Selection.WorkingSelEnd, Document.CharCount);
    oIterator.First;
    Result := False;
    While oIterator.More And (Not Result) Do
    Begin
      If oIterator.Current.PieceType = ptCellStart Then
      Begin
        Result := True;
        DoMove(oIterator.Current.Metrics.Position + 1, cdRight, False);
        DoSelect(CurrentTableCellStop.Metrics.Position, cdLeft)
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.TryAndFindTableBefore : Boolean;
Var
  oIterator : TWPPieceIterator;
Begin
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.SetBounds(0, Selection.WorkingSelStart);
    oIterator.Last;
    Result := False;
    While oIterator.More And (Not Result) Do
    Begin
      If oIterator.Current.PieceType = ptCellStart Then
      Begin
        Result := True;
        DoMove(oIterator.Current.Metrics.Position + 1, cdRight, False);
        DoSelect(CurrentTableCellStop.Metrics.Position, cdLeft)
      End;
      oIterator.Prev;
    End;
  Finally
    oIterator.Free;
  End;
End;


Function TWPRange.PreviousTableCell : Boolean;
Var
  iNew : Integer;
Begin
  LogAction('PreviousTableCell', '');
  If Not HasCurrentTableStart Then
    Result := TryAndFindTableBefore
  Else
  Begin
    Result := HasCurrentTableCellStart;
    If Result Then
    Begin
      iNew := CurrentTableCellStart.Metrics.Position;
      DoMove(iNew-1, cdLeft, False);
      If HasCurrentTableCellStart Then
        DoSelect(CurrentTableCellStart.Metrics.Position, cdRight)
      Else
        PreviousTableCell;
    End;
  End;
End;


Function TWPRange.NextTableCell : Boolean;
Var
  iNew : Integer;
Begin
  LogAction('NextTableCell', '');
  If Not HasCurrentTableStart Then
    Result := TryAndFindTableAfter
  Else
  Begin
    Result := HasCurrentTableCellStart;
    If Result Then
    Begin
      iNew := CurrentTableCellStop.Metrics.Position;
      DoMove(iNew, cdRight, False);
      If HasCurrentTableCellStop Then
        DoSelect(CurrentTableCellStop.Metrics.Position, cdLeft)
      Else
        NextTableCell;
    End;
  End;
End;


Function TWPRange.SelectRow : Boolean;
Var
  oStop : TWPWorkingDocumentStopPiece;
Begin
  Result := HasCurrentTableRowStart;
  LogAction('SelectRow', '', result);
  If Result Then
  Begin
    oStop := CurrentTableRowStop;
    MoveTo(CurrentTableRowStart.Metrics.Position + CurrentTableRowStart.Metrics.CharCount, False);
    SelectTo(oStop.Metrics.Position);
  End;
End;


Function TWPRange.SelectTable : Boolean;
Var
  oStop : TWPWorkingDocumentStopPiece;
Begin
  Result := HasCurrentTableStart;
  LogAction('SelectTable', '', result);
  If Result Then
  Begin
    oStop := CurrentTableStop;
    MoveTo(CurrentTableStart.Metrics.Position + CurrentTableStart.Metrics.CharCount, False);
    SelectTo(oStop.Metrics.Position);
  End;
End;


Function TWPRange.DeleteTable : Boolean;
Var
  iNew : Integer;
Begin
  Result := canRemoveTable In Capabilities;
  LogAction('DeleteTable', '', result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otDelete, False, Selection);
    Operator_.SetRange(CurrentTableStart.Metrics.Position, CurrentTableStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
    Operator_.DeleteRange;
    iNew := Operator_.EndOfRange;
    Operator_.FinishOperation;
    ChangeContent;
    Document.NextLeft(iNew, Operator_.Settings.NoSelectReadOnly, iNew);
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.DeleteRow : Boolean;
Var
  iNew : Integer;
Begin
  Result := canRemoveRow In Capabilities;
  LogAction('DeleteRow', '', result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otDelete, False, Selection);
    Operator_.SetRange(CurrentTableRowStart.Metrics.Position, CurrentTableRowStop.Metrics.Position + CurrentTableRowStop.Metrics.CharCount);
    Operator_.DeleteRange;
    iNew := Operator_.EndOfRange;
    CurrentTableStart.StructureDirty := True;
    Operator_.FinishOperation;
    ChangeContent;
    Document.NextLeft(iNew, Operator_.Settings.NoSelectReadOnly, iNew);
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.DeleteColumn : Boolean;
Begin
  Result := canRemoveColumn In Capabilities;
  LogAction('DeleteColumn', '', result);
  If Result Then
    Result := DoDeleteColumn(CurrentColIndex);
End;

Function TWPRange.SetTableProperties(oTable : TWPWorkingDocumentTableStartPiece;
                oTableRow : TWPWorkingDocumentTableRowStartPiece; oTableCell : TWPWorkingDocumentTableCellStartPiece) : Boolean;
Var
  iLoop : Integer;
  bUpdateTable, bUpdateRow, bUpdateCell: Boolean;
  oApplyTable : TWPWorkingDocumentTableStartPiece;
Begin
  bUpdateTable := Assigned(oTable) And (canTableProps In Capabilities) And (HasCurrentTableStart Or HasSelectedTable);
  bUpdateRow := Assigned(oTableRow) And (canRowProps In Capabilities) And (HasCurrentTableRowStart Or HasSelectedRows);
  bUpdateCell := Assigned(oTableCell) And (canRowProps In Capabilities) And (HasCurrentTableCellStart Or HasSelectedCells);
  Result := (Not Operator_.Settings.ReadOnly) And (bUpdateTable Or bUpdateRow Or bUpdateCell) And Not Operator_.Settings.ConsoleMode;
  LogAction('SetTableProperties', oTable.Describe+' /' +oTableRow.describe + ' / '+oTableCell.describe, result);
  If Result Then
  Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    If HasCurrentTableStart Then
      oApplyTable := CurrentTableStart
    Else
      oApplyTable := SelectedTable;

    // update table
    If bUpdateTable Then
    Begin
      oApplyTable.ApplyProperties(oTable)
    End;

    // update row
    If bUpdateRow Then
    Begin
      If HasCurrentTableRowStart Then
        CurrentTableRowStart.ApplyProperties(oTableRow)
      Else
      Begin
        For iLoop := 0 To SelectedRows.Count - 1 Do
          SelectedRows[iLoop].ApplyProperties(oTableRow);
      End;
    End;

    // update cell
    If bUpdateCell Then
    Begin
      If HasCurrentTableCellStart Then
        CurrentTableCellStart.ApplyProperties(oTableCell)
      Else
      Begin
        For iLoop := 0 To SelectedCells.Count - 1 Do
          SelectedCells[iLoop].ApplyProperties(oTableCell);
      End;
    End;

    oApplyTable.StructureDirty := True;
    Document.RegenerateMetrics(False, False);
    Operator_.FinishOperation;
    ChangeContent;
    ChangeState;
  End;
End;

Function TWPRange.SetTableProperties(oTable: TWPWorkingDocumentTableStartPiece) : Boolean;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canTableProps In Capabilities) And (HasCurrentTableStart Or HasSelectedTable) And Not Operator_.Settings.ConsoleMode;
  LogAction('SetTableProperties', oTable.Describe, result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    If HasCurrentTableStart Then
      CurrentTableStart.ApplyProperties(oTable)
    Else
      SelectedTable.ApplyProperties(oTable);
    Operator_.FinishOperation;
    ChangeContent;
    ChangeState;
    End;
End;

Function TWPRange.SetTableRowProperties(oTableRow: TWPWorkingDocumentTableRowStartPiece) : Boolean;
Var
  iLoop: Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canRowProps In Capabilities) And (HasCurrentTableRowStart Or HasSelectedRows) And Not Operator_.Settings.ConsoleMode;
  LogAction('SetTableRowProperties', oTableRow.describe, result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    If HasCurrentTableRowStart Then
      CurrentTableRowStart.ApplyProperties(oTableRow)
    Else
    Begin
      For iLoop := 0 To SelectedRows.Count - 1 Do
        SelectedRows[iLoop].ApplyProperties(oTableRow);
    End;
    Operator_.FinishOperation;
    ChangeContent;
    ChangeState;
    End;
End;


Function TWPRange.SetTableCellProperties(oTableCell: TWPWorkingDocumentTableCellStartPiece) : Boolean;
Var
  iLoop: Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canRowProps In Capabilities) And (HasCurrentTableCellStart Or HasSelectedCells) And Not Operator_.Settings.ConsoleMode;
  LogAction('SetTableCellProperties', oTableCell.describe, result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    If HasCurrentTableCellStart Then
      CurrentTableCellStart.ApplyProperties(oTableCell)
    Else
    Begin
      For iLoop := 0 To SelectedCells.Count - 1 Do
        SelectedCells[iLoop].ApplyProperties(oTableCell);
    End;
    Operator_.FinishOperation;
    ChangeContent;
    ChangeState;
    End;
End;

Function TWPRange.DoInsertCell(Var iAppendIndex: Integer) : TWPWorkingDocumentPiece;
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  oPara : TWPWorkingDocumentParaPiece;
Begin
  {  Because cell is added piece-by-piece, we can't use normal append
     As when start piece is added, the document is no longer ballanced
  }
  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ApplyBaseStyle(oStart);
    iAppendIndex := Operator_.DirectAppendPiece(oStart.Link, iAppendIndex);
  Finally
    oStart.Free;
  End;

  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    ApplyBaseStyle(oPara);
    iAppendIndex := Operator_.DirectAppendPiece(oPara.Link, iAppendIndex);
    Result := oPara;
  Finally
    oPara.Free;
  End;

  oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
  Try
    ApplyBaseStyle(oStop);
    iAppendIndex := Operator_.DirectAppendPiece(oStop.Link, iAppendIndex);
  Finally
    oStop.Free;
  End;
End;


Function TWPRange.DoInsertRow(iCols : Integer; Var iAppendIndex: Integer) : TWPWorkingDocumentPiece;
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  {  Because row is added piece-by-piece, we can't use normal append
     As when start piece is added, the document is no longer ballanced
  }
  oStart := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ApplyBaseStyle(oStart);
    iAppendIndex := Operator_.DirectAppendPiece(oStart.Link, iAppendIndex);
  Finally
    oStart.Free;
  End;

  Result := DoInsertCell(iAppendIndex);
  For iLoop := 2 To iCols Do
    DoInsertCell(iAppendIndex);

  oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
  Try
    ApplyBaseStyle(oStop);
    iAppendIndex := Operator_.DirectAppendPiece(oStop.Link, iAppendIndex);
  Finally
    oStop.Free;
  End;
End;

Function TWPRange.InsertFieldSection(oDetails : TWPWorkingDocumentSectionStartPiece; Const sText : String = '') : Boolean;
Var
  oStart : TWPWorkingDocumentSectionStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iNew : Integer;
  oPiece :  TWPWorkingDocumentPiece;
  oPara : TWPWorkingDocumentParaPiece;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canInsertFieldSection In capabilities) And Not Operator_.Settings.ConsoleMode And Not Operator_.Settings.NoParagraphs;
  LogAction('InsertFieldSection', oDetails.describe+' '+sText, result);

  If Result Then
  Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.CleanRange;

    oPiece := Operator_.PieceBeforeAppend;
    If Not (oPiece = Nil) And Not (oPiece.PieceType In [ptBreak, ptPara, ptTableStop, ptRowStop, ptCellStop, ptSectionStop]) Then
    Begin
      oPara := TWPWorkingDocumentParaPiece.Create;
      Try
        ApplyBaseStyle(oPara);
        Operator_.AppendPiece(oPara.Link);
      Finally
        oPara.Free;
      End;
    End;

    FDocument.Working := True;
    Try

      oStart := TWPWorkingDocumentSectionStartPiece.Create;
      Try
        oStart.Assign(oDetails);
        oStart.IsField := True;
        oStart.BuildDocSection;
        oStart.DefinitionProvider := Operator_.Settings.FieldDefinitions.GetByName(oStart.Namespace, Nil).Link;

        ApplyBaseStyle(oStart);
        Operator_.AppendPiece(oStart.Link);

        If sText <> '' Then
          InsertInOperation(sText, oStart.Font);

        oPara := TWPWorkingDocumentParaPiece.Create;
        Try
          ApplyBaseStyle(oPara);
          Operator_.AppendPiece(oPara.Link);
        Finally
          oPara.Free;
        End;

        oStop := TWPWorkingDocumentStopPiece.Create(stSection);
        Try
          ApplyBaseStyle(oStop);
          Operator_.AppendPiece(oStop.Link);
        Finally
          oStop.Free;
        End;
      Finally
        oStart.Free;
      End;
    Finally
      FDocument.Working := False;
    End;

    If sText = '' Then
      iNew := Operator_.StartOfRange + 1
    Else
      iNew := Operator_.EndOfRange;

    Operator_.FinishOperation;

    ChangeContent;

    DoMove(iNew, cdJump);
  End;
End;


Function TWPRange.InsertField(oDetails: TWPWorkingDocumentFieldStartPiece; bNoStyle, bAcceptExistingContent : Boolean; Const sText : String = '') : Boolean;
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  iNew : Integer;
  bKeep : Boolean;
  i : integer;
  piece : TWPWorkingDocumentPiece;
Begin

  Result := Not Operator_.Settings.ReadOnly And (canInsertField In capabilities) And Not Operator_.Settings.ConsoleMode;
  LogAction('InsertField', oDetails.describe+' '+sText, result);

  If Result Then
  Begin
    bKeep := Selection.HasSelection and (sText = '') and bAcceptExistingContent;
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;

    oStart := TWPWorkingDocumentFieldStartPiece.Create;
    Try
      oStart.Assign(oDetails);
      oStart.BuildDocField;
      oStart.DefinitionProvider := Operator_.Settings.FieldDefinitions.GetByName(oStart.Namespace, Nil).Link;
      If oStart.HasDefinitionProvider And oStart.DefinitionProvider.HasCheckables(oStart.DocField) Then
      Begin
        oStart.DefinitionProvider.GetCheckables(oStart.DocField, oStart.Checkables);
        oStart.BindToCheckables;
      End;

      If Not bNoStyle Then
      Begin
        ApplyBaseStyle(oStart);
      End;
      Operator_.AppendPiece(oStart.Link);

      If sText <> '' Then
        InsertInOperation(sText, oStart.Font)
      else if bKeep then
        for i := 0 to Operator_.CurrentOp.OriginalPieces.Count - 1 do
        begin
          piece := Operator_.CurrentOp.OriginalPieces[i].Clone;
          // piece.Hotspot := oStart.Hotspot.Link;
          Operator_.AppendPiece(piece);
        end;

      oStop := TWPWorkingDocumentFieldStopPiece.Create;
      Try
        ApplyBaseStyle(oStop);
        Operator_.AppendPiece(oStop.Link);
      Finally
        oStop.Free;
      End;
    Finally
      oStart.Free;
    End;

    If (sText = '') and not bKeep Then
      iNew := Operator_.StartOfRange + 1
    Else
      iNew := Operator_.EndOfRange;

    Operator_.FinishOperation;

    ChangeContent;

    DoMove(iNew, cdJump);
  End;
End;

Function IsMatchingSection(oPiece : TWPWorkingDocumentPiece; Const sNamespace, sName : String) : Boolean; overload;
Begin
  Result := (oPiece.PieceType = ptSectionStart) And (TWPWorkingDocumentSectionStartPiece(oPiece).IsField) And
         ((sNamespace = '') Or StringEquals(TWPWorkingDocumentSectionStartPiece(oPiece).Namespace, sNamespace)) And
         StringEquals(TWPWorkingDocumentSectionStartPiece(oPiece).Name, sName);
End;


Function IsMatchingField(oPiece : TWPWorkingDocumentPiece; Const sNamespace, sName : String) : Boolean; overload;
Begin
  Result := (oPiece.PieceType = ptFieldStart) And
        ((sNamespace = '') Or StringEquals(TWPWorkingDocumentFieldStartPiece(oPiece).Namespace, sNamespace)) And
        StringEquals(TWPWorkingDocumentFieldStartPiece(oPiece).Name, sName);
End;


Function IsMatchingField(oPiece : TWPWorkingDocumentPiece; field : TWPDocumentField) : Boolean; overload;
Begin
  Result := (oPiece.PieceType = ptFieldStart) And (TWPWorkingDocumentFieldStartPiece(oPiece).DocField = field);
End;


Function TWPRange.GotoFieldByName(Const sNamespace, sName : String):Boolean;
Var
  oIterator :  TWPPieceIterator;
Begin
  LogAction('GotoFieldByName', sNamespace+'#'+sName);
  Result := False;
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := [ptFieldStart, ptSectionStart];
    oIterator.First;
    While oIterator.More And Not IsMatchingSection(oIterator.Current, sNamespace, sName) And Not IsMatchingField(oIterator.Current, sNamespace, sName) Do
      oIterator.Next;

    If oIterator.More Then
    Begin
      Result := True;
      If Selection.MoveTo(oIterator.Current.Metrics.Position+1) Then
      Begin
        UpdateCurrentStatus;
        SelectField(False);
        ChangeState;
      End;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.GotoFieldByDoc(field : TWPDocumentField):Boolean;
Var
  oIterator :  TWPPieceIterator;
Begin
  LogAction('GotoFieldByDoc', field.Namespace+'#'+field.Name);
  Result := False;
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := [ptFieldStart, ptSectionStart];
    oIterator.First;
    While oIterator.More And Not IsMatchingField(oIterator.Current, field) Do
      oIterator.Next;

    If oIterator.More Then
    Begin
      Result := True;
      If Selection.MoveTo(oIterator.Current.Metrics.Position+1) Then
      Begin
        UpdateCurrentStatus;
        SelectField(False);
        ChangeState;
      End;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.FirstField: Boolean;
Var
  oIterator :  TWPPieceIterator;
Begin
  LogAction('FirstField', '');
  Result := False;
  DoMove(0, cdJump, True);
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.SetBounds(Selection.Cursor, Document.CharCount);
    oIterator.PieceTypes := [ptFieldStart, ptSectionStart];
    oIterator.First;
    While oIterator.More Do
    Begin
      If (oIterator.Current.PieceType = ptFieldStart) Or (TWPWorkingDocumentSectionStartPiece(oIterator.Current).IsField) Then
      Begin
        Result := True;
        If Selection.MoveTo(oIterator.Current.Metrics.Position+1) Then
        Begin
          UpdateCurrentStatus;
          SelectField(False);
          ChangeState;
        End;
        Break;
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.NextField: Boolean;
Var
  oIterator :  TWPPieceIterator;
  iStart, iStop : Integer;
Begin
  LogAction('NextField', '');
  Result := False;
  If Selection.HasSelection Then
    DoMove(Selection.SelStart, cdJump, True);
  If GetFieldRange(True, iStart, iStop) Then
    DoMove(iStop, cdJump, True);

  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.SetBounds(Selection.Cursor, Document.CharCount);
    oIterator.PieceTypes := [ptFieldStart, ptSectionStart];
    oIterator.First;
    While oIterator.More Do
    Begin
      If (oIterator.Current.PieceType = ptFieldStart) Or (TWPWorkingDocumentSectionStartPiece(oIterator.Current).IsField) Then
      Begin
        Result := True;
        If Selection.MoveTo(oIterator.Current.Metrics.Position+1) Then
        Begin
          UpdateCurrentStatus;
          SelectField(False);
          ChangeState;
        End;
        Break;
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Function TWPRange.PreviousField: Boolean;
Var
  oIterator :  TWPPieceIterator;
  iStart, iStop : Integer;
Begin
  LogAction('PreviousField', '');
  Result := False;
  If Selection.HasSelection Then
    DoMove(Selection.SelStart, cdJump, True);
  If GetFieldRange(True, iStart, iStop) Then
    DoMove(iStart - 1, cdLeft, True);

  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.SetBounds(0, Selection.Cursor);
    oIterator.PieceTypes := [ptFieldStart, ptSectionStart];
    oIterator.Last;
    While oIterator.More Do
    Begin
      If (oIterator.Current.PieceType = ptFieldStart) Or (TWPWorkingDocumentSectionStartPiece(oIterator.Current).IsField) Then
      Begin
        Result := True;
        If Selection.MoveTo(oIterator.Current.Metrics.Position+1) Then
        Begin
          UpdateCurrentStatus;
          SelectField(False);
          ChangeState;
        End;
        Break;
      End;
      oIterator.Prev;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.RemoveField(bKeepContent: Boolean): Boolean;
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  sCursor : String;
  iNew : Integer;
Begin
  LogAction('RemoveField', BooleanToString(bKeepContent));
  If Not Operator_.Settings.ReadOnly And (canRemoveField In capabilities)  And Not Operator_.Settings.ConsoleMode Then
    Begin
    oStart := CurrentFieldStart;
    Result := Assigned(oStart);
    If Result Then
      Begin
      oStop := CurrentFieldStop;
      If Not Assigned(oStop) Then
        RaiseError('RemoveField', 'End of Field not found');
      sCursor := Selection.Save;
      DoMove(oStart.Metrics.Position, cdJump, False);
      DoSelect(oStop.Metrics.Position + oStop.Metrics.CharCount, cdJump, False);

      If bKeepContent Then
      Begin
        Operator_.StartOperation(FId, otTrim, False, Selection);
        Operator_.SetPriorCursor(sCursor);
        Operator_.CleanRange;
        Operator_.RemovePiece(oStart);
        Operator_.RemovePiece(oStop);
        iNew := Operator_.EndOfRange;
        Operator_.FinishOperation;
      End
      Else
      Begin
        Operator_.StartOperation(FId, otDelete, False, Selection);
        Operator_.SetPriorCursor(sCursor);
        Operator_.DeleteRange;
        iNew := Operator_.EndOfRange;
        Operator_.FinishOperation;
      End;
      ChangeContent;
      MoveTo(iNew);
      End;
    End
  Else
    Result := False;
End;

Function TWPRange.RemoveFieldSection(bKeepContent: Boolean): Boolean;
Var
  oStart : TWPWorkingDocumentSectionStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  sCursor : String;
  iNew : Integer;
Begin
  LogAction('RemoveFieldSection', BooleanToString(bKeepContent));
  If Not Operator_.Settings.ReadOnly And (canRemoveFieldSection In capabilities) Then
    Begin
    oStart := CurrentSectionStart;
    Result := Assigned(oStart);
    If Result Then
      Begin
      oStop := CurrentSectionStop;
      If Not Assigned(oStop) Then
        RaiseError('RemoveSection', 'End of Field not found');
      sCursor := Selection.Save;
      Selection.MoveTo(oStart.Metrics.Position);
      Selection.SelectTo(oStop.Metrics.Position + oStop.Metrics.CharCount, oStop.Metrics.Position + oStop.Metrics.CharCount);

      If bKeepContent Then
      Begin
        Operator_.StartOperation(FId, otTrim, False, Selection);
        Operator_.SetPriorCursor(sCursor);
        Operator_.CleanRange;
        Operator_.RemovePiece(oStart);
        Operator_.RemovePiece(oStop);
        iNew := Operator_.EndOfRange;
        Operator_.FinishOperation;
      End
      Else
      Begin
        Operator_.StartOperation(FId, otDelete, False, Selection);
        Operator_.SetPriorCursor(sCursor);
        Operator_.DeleteRange;
        iNew := Operator_.EndOfRange;
        Operator_.FinishOperation;
      End;
      ChangeContent;
      MoveTo(iNew);
      End;
    End
  Else
    Result := False;
End;

Function TWPRange.SetFieldProperties(oField: TWPWorkingDocumentFieldStartPiece) : Boolean;
Var
  oCurrent : TWPWorkingDocumentFieldStartPiece;
  oPiece : TWPWorkingDocumentPiece;
  sSelection : String;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFieldProps In Capabilities) And Not Operator_.Settings.ConsoleMode;
  LogAction('SetFieldProperties', oField.describe, result);
  If Result Then
    Begin
    oCurrent := CurrentFieldStart;
    If Not Assigned(oCurrent) Then
      RaiseError('SetFieldProperties', 'No Field in Focus');

    sSelection := Selection.Save;
    MoveTo(oCurrent.Metrics.Position, False);
    SelectTo(oCurrent.Metrics.Position + oCurrent.Metrics.CharCount, False);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.SetPriorCursor(sSelection);
    Operator_.CleanRange;

    oCurrent.Namespace := oField.Namespace;
    oCurrent.Name := oField.Name;
    oCurrent.Deletable := oField.Deletable;
    oCurrent.RawData.Assign(oField.RawData);
    oCurrent.FixedFormat := oField.FixedFormat;
    oCurrent.Width := oField.Width;
    oCurrent.Hotspot := oField.Hotspot.Clone;
    oCurrent.ReadOnly := oField.ReadOnly;
    oCurrent.CheckedIndex := oField.CheckedIndex;
    oCurrent.BuildDocField;
    oCurrent.InError := False;
    oCurrent.Checkables.Clear;
    If oCurrent.HasDefinitionProvider And oCurrent.DefinitionProvider.HasCheckables(oCurrent.DocField) Then
      oCurrent.DefinitionProvider.GetCheckables(oCurrent.DocField, oCurrent.Checkables);
    oCurrent.BindToCheckables;


    oPiece := oCurrent;
    // spelling state of the contents may be changed, and we wouldn't know...
    While (oPiece <> Nil) And (oPiece.PieceType <> ptFieldStop) Do
    Begin
      If (oPiece.PieceType = ptText) Then
         TWPWorkingDocumentTextPiece(oPiece).SpellState := scsNotChecked;
      oPiece := oPiece.Next;
    End;
    If oPiece.PieceType <> ptFieldStop Then
      oPiece.Change(ctPresentation, oPiece);

    Operator_.FinishOperation;
    Selection.Restore(sSelection);
    CheckSpelling;
    ChangeContent;
    ChangeState;
    End;
End;



Function TWPRange.Undo : Boolean;
Begin
  Result := (canUndo In Capabilities) And Operator_.Undo(FId, Selection);
  LogAction('Undo', '', result);
  If Result Then
  Begin
    ChangeContent;
    ChangeState;
  End;
End;

Function TWPRange.Redo : Boolean;
Begin
  Result := (canRedo In Capabilities) And Operator_.Redo(FId, Selection);
  LogAction('Redo', '', result);
  If Result Then
  Begin
    ChangeContent;
    ChangeState;
  End;
End;


Function TWPRange.ReplaceCurrentWord(Const oSource : TFslStream; aFormat : TWPFormat; Var sMessage : String) : Boolean;
Var
  sCurrent : String;
  iStart : Integer;
  iStop : Integer;
Begin
  // TODO: chjeck field
  Result := Not Operator_.Settings.ReadOnly And CursorInWord(sCurrent, iStart, iStop);
  LogAction('ReplaceCurrentWord', '(stream)', result);
  If Result Then
    Begin
    DoMove(iStart, cdJump, False);
    DoSelect(iStop, cdJump, False);
    Result := Insert(oSource, aFormat, True, Nil, sMessage);
    End;
End;

Function TWPRange.ReplaceCurrentWord(Const sText: String; Var sMessage : String) : Boolean;
  Function AddWord(Const sWord : String) : Integer;
  Var
    oText : TWPWorkingDocumentTextPiece;
  Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      oText.Content := sWord;
      ApplyBaseStyle(oText);
      Operator_.AppendPiece(oText.Link);
      Result := Operator_.EndOfRange;
    Finally
      oText.Free;
    End;
  End;
Var
  iStart : Integer;
  iStop : Integer;
  iNew : Integer;
  sInsert : String;
  oSplit : TWPWordIterator;
Begin
  // TODO: check field
  Result := False;
  If Operator_.Settings.ReadOnly Then
    sMessage := 'Content is read only'
  Else If Not (CanInsert In Capabilities) Then
    sMessage := 'Unable to insert at this point'
  Else
  Begin
    Result := True;
    LogAction('ReplaceCurrentWord', sText, result);
    If Selection.HasSelection Then
    Begin
      iStart := Selection.SelStart;
      iStop := Selection.SelEnd;
    End
    Else
    Begin
      iStart := Selection.Cursor;
      iStop := Selection.Cursor;
    End;

    DoMove(iStart, cdJump, False);
    DoSelect(iStop, cdJump, False);
    sInsert := StringReplace(sText, [#13, #10], '');

    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;

    iNew := Operator_.EndOfRange;

    oSplit := TWPWordIterator.Create;
    Try
      oSplit.Init(sInsert);
      While oSplit.More Do
        iNew := AddWord(oSplit.Next);
    Finally
      oSplit.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.Search(oDetails : TWPSearchDetails; iRangeStart : Integer = -1; iRangeStop : Integer = -1) : Boolean;
Var
  oSearch : TWPSearch;
  iStart : Integer;
  iStop : Integer;
Begin
  result := false;
  LogAction('Search', oDetails.describe, result);
  oSearch := TWPSearch.Create(Document.Link, Selection.Link, oDetails.Link);
  Try
    oSearch.LimitStart := iRangeStart;
    oSearch.LimitStop := iRangeStop;
    Result := oSearch.Search(iStart, iStop);
    If Result Then
    Begin
      MoveTo(iStart, False);
      SelectTo(iStop, True);
    End;
  Finally
    oSearch.Free;
  End;
End;


Function TWPRange.Cut : Boolean;
Begin
  LogAction('Cut', '');
  Result := Not Operator_.Settings.ReadOnly And (canDelete In Capabilities) And Copy And DeleteSelection;
End;


Function TWPRange.CopyDebug : Boolean;
Begin
  LogAction('Copy', '');
  Result := DoCopy(True);
End;

Function TWPRange.Copy : Boolean;
Begin
  LogAction('Copy', '');
  Result := DoCopy(False);
End;


Function TWPRange.Paste(Var sError : String; oSpeechMagicOptions : TWPSpeechMagicInsertOptions = Nil) : Boolean;
Begin
  LogAction('Paste', '');
  Result := PasteSpecial(wcctUnknown, sError, oSpeechMagicOptions);
End;

Function TWPRange.PasteSpecial(aContentType : TWPClipboardContentType; Var sError : String; oSpeechMagicOptions : TWPSpeechMagicInsertOptions = Nil) : Boolean;
Var
  bTextOnly : Boolean;
  oClip : TWPClipboard;
Begin
  LogAction('PasteSpecial', WPCLIPBOARDCONTENTTYPE_NAMES[aContentType]);
  // TODO: are we in a field?
  bTextOnly := Operator_.Settings.ConsoleMode;

  Result := False;
    oClip := TWPClipboard.Create;
    Try
      oClip.Open;
    If bTextOnly And Not ((aContentType In [wcctUnknown, wcctText]) And oClip.HasText) Then
      sError := 'Unable to paste this content in this mode'
    Else If canInsert In capabilities Then
    Begin

      // so, the question is, what should we choose if no particular clipboard
      // format is specified? In principle, our order of preference is this:
      //  native, odt, rtf, html, text, jpeg, or bmp
      // we put jpeg/bmp at the end because apps like powerpoint paste bitmap
      // renditions of the content as well as the text
      // however there is one circumstance where we prefer the graphical rendition over
      // any alternatives, and this is when there is no text. This arises when an IE
      // user copies an image. IE places the image and an html rendition of it.
      // we're not interested in the html rendition - it does nothing for us.
        If Not bTextOnly And (aContentType = wcctUnknown) And Not oClip.HasText And (oClip.HasBitmap Or oClip.HasJPEG) Then
        Begin
          If oClip.HasJPEG Then
            Result := PasteJPEG(oClip)
          Else // oClip.HasBitmap
            Result := PasteBitmap(oClip)
        End
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctNative]) And oClip.HasNative Then
          Result := PasteInsertClipboard(TWPNativeReader, oClip, wcctNative, oSpeechMagicOptions, sError)
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctRTF]) And oClip.HasRTF Then
          Result := PasteInsertClipboard(TWPRTFReader, oClip, wcctRTF, oSpeechMagicOptions, sError)
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctHTML]) And oClip.HasHTML Then
          Result := PasteInsertClipboard(TWPHTMLReader, oClip, wcctHTML, oSpeechMagicOptions, sError)
  {      Else If not bTextOnly And (aContentType In [wcctUnknown, wcctOdt]) And oClip.HasOdt Then
        Result := PasteInsertClipboard(TWPOdtReader, oClip, wcctOdt, oSpeechMagicOptions, sError)}
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctFormattedText]) And oClip.HasFormattedText Then
          Result := PasteInsertClipboard(TWPTextReader, oClip, wcctFormattedText, oSpeechMagicOptions, sError)
        Else If (aContentType In [wcctUnknown, wcctUnicode]) And oClip.HasUnicode Then
          Result := PasteInsertClipboard(TWPTextReader, oClip, wcctUnicode, oSpeechMagicOptions, sError)
        Else If (aContentType In [wcctUnknown, wcctText]) And oClip.HasText Then
          Result := PasteInsertClipboard(TWPTextReader, oClip, wcctText, oSpeechMagicOptions, sError)
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctJPEG]) And oClip.HasJPEG Then
          Result := PasteJPEG(oClip)
        Else If Not bTextOnly And (aContentType In [wcctUnknown, wcctBitmap]) And oClip.HasBitmap Then
          Result := PasteBitmap(oClip)
      Else
      Begin
        Result := False;
        sError := 'Unable to Read Any clipboard formats ['+oClip.contentNames+']';
      End;
  End
  Else If Selection.hasSelection Then
    sError := 'Unable to replace this Selection'
  Else
    sError := 'Unable to insert at this point';
  Finally
    oClip.Free;
  End;
End;

Function TWPRange.DropText(iIndex : Integer; sText : String; Var sError : String) : Boolean;
Var
  oStream : TFslStringStream;
Begin
  LogAction('DropText', sText+' @ '+inttostr(iIndex));
  // TODO: check for field
  oStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oStream.Data := sText;
{$ELSE}
    oStream.Bytes := TEncoding.UTF8.GetBytes(sText);
{$ENDIF}

    MoveTo(iIndex, False);
    Result := Insert(oStream, wpfText, False, Nil, sError);
  Finally
    oStream.Free;
  End;
End;

Function TWPRange.Insert(oStream: TFslStream; aContentType : TWPClipboardContentType; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean;
Begin
  LogAction('Insert(stream)', WPCLIPBOARDCONTENTTYPE_NAMES[aContentType]);
  Result := (canInsert In Capabilities) And (Not Operator_.Settings.ConsoleMode Or (aContentType = wcctText)) And PasteInsert(ReaderForContentType(aContentType), oStream, bSkipLastPara, oSpeechMagicOptions, sError);
  UpdateTextMetrics;
End;


Function TWPRange.Insert(oStream: TFslStream; aFormat : TWPFormat; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean;
Begin
  LogAction('Insert(stream)', WPFORMAT_NAMES[aFormat]);
  Result := (canInsert In Capabilities) And (Not Operator_.Settings.ConsoleMode Or (aFormat = wpfText)) And PasteInsert(ReaderForFormat(aFormat), oStream, bSkipLastPara, oSpeechMagicOptions, sError);
  UpdateTextMetrics;
End;

Function TWPRange.ReadClipboardContent(oClip : TWPClipboard; aContentType : TWPClipboardContentType; Var sError : String) : TFslMemoryStream;
Var
  sValue: String;
  iStart, iEnd : Integer;
  oBuffer : TFslBuffer;
  oMemory : TFslMemoryStream;
Begin
  // TODO: are we in a field?
  oBuffer := TFslBuffer.Create;
  oMemory := TFslMemoryStream.Create;
  Try
    oClip.PasteContent(aContentType, oBuffer, aContentType <> wcctUnicode);

    // Automatically extract the HTML content (without the header info)
    If aContentType = wcctHTML Then
    Begin
      sValue := oBuffer.AsText;
      istart := StringFindInsensitive(sValue, '<html');
      iEnd := StringFindInsensitive(sValue, '</html>');
      oBuffer.AsText := StringCopy(sValue, iStart, iEnd - iStart + 1 + 7);
    End;

    // result
    oMemory.Buffer := oBuffer.Link;
    Result := oMemory.Link;
  Finally
    oMemory.Free;
    oBuffer.Free;
  End;
End;

Function TWPRange.ParseClipboardContent(aReaderClass : TWPReaderClass; oClip : TWPClipboard; aContentType : TWPClipboardContentType; oStyles : TWPStyles; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : TWPWorkingDocument;
Var
  oMemory: TFslMemoryStream;
Begin
  oMemory := Nil;
  Try
    oMemory := ReadClipboardContent(oClip, aContentType, sError);
    Result := ParseClipboardContent(aReaderClass, oMemory, oStyles, oSpeechMagicOptions, sError);
  Finally
    oMemory.Free;
  End;
End;

Function TWPRange.ParseClipboardContent(aReaderClass : TWPReaderClass; oSource: TFslStream; oStyles : TWPStyles; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : TWPWorkingDocument;
Var
  oReader : TWPReader;
  oDocument: TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oDocument.Pieces.Hooking := False; // This allows us to link the pieces to something else, saves a lot of resources
  oReader := aReaderClass.Create;
  Try
    oReader.Stream := oSource.Link;
    oReader.Styles := oStyles.Link;
    oReader.Font := Font.Link;
    oReader.Style := Style;
    oReader.IsFragment := True;
    oReader.MustCloseWithPara := False;
    oReader.Context := '';
    oReader.OnLoadImage := TWordProcessor(FOwner).DocumentHandler.OnLoadImage;
    If Not Assigned(oReader.OnLoadImage) Then
      oReader.OnLoadImage := DefaultLoadImage;
    If Assigned(oSpeechMagicOptions) Then
    Begin
      oReader.SpeechMagicDouble := oSpeechMagicOptions.SpeechMagicDouble;
      If oReader Is TWPRTFReader Then
        TWPRTFReader(oReader).IgnoreBackground := oSpeechMagicOptions.IgnoreBackground;
    End;
    oReader.Read(oDocument);

    Result := oDocument.Link;
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Function TWPRange.PasteInsertClipboard(aReaderClass : TWPReaderClass; oClip : TWPClipboard; aContentType : TWPClipboardContentType; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean;
Var
  oMemory : TFslMemoryStream;
  oDocument: TWPWorkingDocument;
  oStyles : TWPStyles;
Begin
  // TODO: are we in a field?
  oStyles := Operator_.Styles.Clone;
  oMemory := Nil;
  oDocument := Nil;
  Try
    oMemory := ReadClipboardContent(oClip, aContentType, sError);
    oDocument := ParseClipboardContent(aReaderClass, oMemory, oStyles, oSpeechMagicOptions, sError);
    Result := PasteInsertWorkingDocument(oDocument, oStyles, False, sError);
  Finally
    oMemory.Free;
    oDocument.Free;
    oStyles.Free;
  End;
End;

Function TWPRange.PasteInsert(aReaderClass : TWPReaderClass; oSource : TFslStream; bSkipLastPara : Boolean; oSpeechMagicOptions : TWPSpeechMagicInsertOptions; Var sError : String) : Boolean;
Var
  oDocument : TWPWorkingDocument;
  oStyles : TWPStyles;
Begin
  oDocument := Nil;
  oStyles := Operator_.Styles.Clone;
  Try
    oDocument := ParseClipboardContent(aReaderClass, oSource, oStyles, oSpeechMagicOptions, sError);
    Result := PasteInsertWorkingDocument(oDocument, oStyles, bSkipLastPara, sError)
  Finally
    oStyles.Free;
    oDocument.Free;
  End;
End;

Function TWPRange.DropFile(iIndex : Integer; Const sFilename : String) : Boolean;
Begin
  LogAction('DropFile', sFilename+' @ '+inttostr(iIndex));
  If (canInsertImage In Capabilities) And Not Operator_.Settings.ConsoleMode Then
  Begin
    MoveTo(iIndex);
    Result := InsertImage(sFilename);
  End
  Else
    Result := False;
End;


Function TWPRange.DropImage(iOffset : Integer; oImage : TFslVCLGraphic) : Boolean;
Begin
  LogAction('DropImage', '');
  If (canInsertImage In Capabilities) And Not Operator_.Settings.ConsoleMode Then
  Begin
    MoveTo(iOffset);
    Result := InsertImage('dropped.bmp', oImage);
  End
  Else
    Result := False;
End;


Function TWPRange.GetWorkingWidth : Integer;
Begin
  Result := 200;
End;


Function TWPRange.InsertImage(Const sName: String; oImage : TFslGraphic) : Boolean;
Var
  oPiece : TWPWorkingDocumentImagePiece;
  iNew : Integer;
  iWidth : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canInsertImage In Capabilities) And Not Operator_.Settings.ConsoleMode;
  LogAction('InsertImage(object)', sName, result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;
    oPiece := TWPImageLoader.WrapWorking(oImage);
    Try
      iWidth := GetWorkingWidth;
      If oPiece.Width > iWidth - Operator_.Settings.HorizontalMargin * 2 Then
      Begin
        oPiece.Height := Trunc(oPiece.Height * ((iWidth - Operator_.Settings.VerticalMargin * 2) / (oPiece.Width * Operator_.Settings.Scale)));
        oPiece.Width := Trunc((iWidth - Operator_.Settings.HorizontalMargin * 2) / Operator_.Settings.scale);
      End;
      oPiece.Name := PathFilename(sName);
      ApplyBaseStyle(oPiece);
      Operator_.AppendPiece(oPiece.Link);
      iNew := Operator_.EndOfRange;
    Finally
      oPiece.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;

Function TWPRange.InsertImage(Const sName: String; oPDF : TWPWorkingAttachment; iPage : integer) : Boolean;
Var
  oPiece : TWPWorkingDocumentImagePiece;
  oSpace : TWPWorkingDocumentTextPiece;
  iNew : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canInsertImage In Capabilities) And Not Operator_.Settings.ConsoleMode;
  LogAction('InsertImage(object)', sName, result);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;
    oPiece := TWPWorkingDocumentImagePiece.Create;
    Try
      oPiece.Border := 0;
      oPiece.SizePolicy := ImageSizeWholePage;
//      oPiece.Image := TWPPDFGraphic.Create(oPDF.Link);
      oPiece.FrameIndex := iPage;
      oPiece.Name := PathFilename(sName);
      ApplyBaseStyle(oPiece);

      Operator_.AppendPiece(oPiece.Link);

      Operator_.EndOfRange;
    Finally
      oPiece.Free;
    End;

    oSpace := TWPWorkingDocumentTextPiece.Create;
    Try
      oSpace.Content := ' ';
      ApplyBaseStyle(oSpace);
      Operator_.AppendPiece(oSpace.Link);
      iNew := Operator_.EndOfRange;
    Finally
      oSpace.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.InsertImage(sFilename: String) : Boolean;
Var
  oLoader : TWPImageLoader;
  oImage : TFslGraphic;
Begin
  if SameText('.pdf', PathExtension(sFilename)) then
    result := insertPDF(sFilename)
  else
  begin
    LogAction('InsertImage', sFilename);
    oLoader := TWPImageLoader.Create;
    Try
      oLoader.DicomDictionary := Operator_.Settings.DicomDictionary.Link;
      oLoader.Filename := sFilename;
      oImage := oLoader.Load;
      Try
        Result := InsertImage(sFilename, oImage);
      Finally
        oImage.Free;
      End;
    Finally
      oLoader.Free;
    End;
  end;
End;

Function TWPRange.InsertPDF(sFilename: String) : Boolean;
Var
  oPDF : TWPWorkingAttachment;
  i : integer;
Begin
  LogAction('InsertPDF', sFilename);
  oPDF := TWPWorkingAttachment.create;
  Try
    oPDF.Id := GUIDToString(CreateGUID);
    FDocument.Attachments.Add(oPDF.Link);
    oPDF.LoadFromFile(sFileName);
    oPDF.UseAsPDF;
    Result := true;
//    for i := 0 to oPDF.PDF.PageCount - 1 do
//      Result := InsertImage(sFilename, oPDF, i) and result;
  Finally
    oPDF.Free;
  End;
End;


Function TWPRange.SetImageProperties(oImage: TWPWorkingDocumentImagePiece) : Boolean;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canImageProps In Capabilities) And HasCurrentImage And Not Operator_.Settings.ConsoleMode;
  LogAction('SetImageProperties', oImage.describe, result);
  If Result Then
    Begin
    Selection.Select(CurrentImage.Metrics.Position, CurrentImage.Metrics.Position+1);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.CleanRange;
    CurrentImage.ApplyProperties(oImage);
    Operator_.FinishOperation;
    DoMove(CurrentImage.Metrics.Position, cdJump, False);
    ChangeContent;
    ChangeState;
    End;
End;

Procedure TWPRange.DeleteAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment);
Var
  sSelect : String;
Begin
  LogAction('DeleteAdornment', oImage.describe+' / '+oAdornment.describe);
  sSelect := Selection.Save;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.SetPriorCursor(sSelect);
  Operator_.CleanRange;
  oImage.Adornments.DeleteByReference(oAdornment);
  oImage.ChangeAdornments;
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Procedure TWPRange.MoveAdornmentToBack(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment);
Var
  sSelect : String;
  iIndex : Integer;
Begin
  LogAction('MoveAdornmentToBack', oImage.describe+' / '+oAdornment.describe);
  sSelect := Selection.Save;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.SetPriorCursor(sSelect);
  Operator_.CleanRange;
  iIndex := oImage.Adornments.IndexById(oAdornment.Id);
  oImage.Adornments.Move(iIndex, 0);
  oImage.ChangeAdornments;
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Procedure TWPRange.MoveAdornmentToFront(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment);
Var
  sSelect : String;
  iIndex : Integer;
Begin
  LogAction('MoveAdornmentToFront', oImage.describe+' / '+oAdornment.describe);
  sSelect := Selection.Save;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.SetPriorCursor(sSelect);
  Operator_.CleanRange;
  iIndex := oImage.Adornments.IndexById(oAdornment.Id);
  oImage.Adornments.Move(iIndex, oImage.Adornments.Count);
  oImage.ChangeAdornments;
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Procedure TWPRange.MoveImageAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment; aPart : TAdornmentPart; aAction : TAdornmentAction; iX, iY : Integer);
Var
  sSelect : String;
  oLocal : TWPDocumentImageAdornment;
Begin
  LogAction('MoveImageAdornment', oImage.describe+' / '+oAdornment.describe);
  sSelect := Selection.Save;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.SetPriorCursor(sSelect);
  Operator_.CleanRange;
  oLocal := oImage.Adornments.GetById(oAdornment.Id);
  oLocal.Move(aPart, aAction, iX, iY);
  oImage.ChangeAdornments;
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Procedure TWPRange.ApplyImageAdornment(oImage : TWPWorkingDocumentImagePiece; oAdornment : TWPDocumentImageAdornment);
Var
  sSelect : String;
  oLocal : TWPDocumentImageAdornment;
Begin
  LogAction('ApplyImageAdornment', oImage.describe+' / '+oAdornment.describe);
  sSelect := Selection.Save;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.SetPriorCursor(sSelect);
  Operator_.CleanRange;
  oLocal := oImage.Adornments.GetById(oAdornment.Id);
  If oLocal <> Nil Then
    oLocal.Assign(oAdornment)
  Else
    oImage.Adornments.Add(oAdornment.Clone);
  oImage.ChangeAdornments;
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Procedure TWPRange.SelectImageArea(oImage : TWPWorkingDocumentImagePiece; oArea : TWPImageMapArea);
Begin
  LogAction('SelectImageArea', oImage.describe);
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position+1);
  Operator_.StartOperation(FId, otChange, False, Selection);
  Operator_.CleanRange;
  oImage.ImageMap.Select(oArea);
  Operator_.FinishOperation;
  Selection.Select(oImage.Metrics.Position, oImage.Metrics.Position);
  ChangeContent;
  ChangeState;
End;

Function TWPRange.PasteJPEG(oClip : TWPClipboard) : Boolean;
Begin
  Result := False;
  RaiseError('PasteJPEG', 'JPEG pasting not implemented yet');
End;


Function TWPRange.PasteBitmap(oClip : TWPClipboard) : Boolean;
Var
  oImage : TFslBitmapGraphic;
Begin
  oImage := TFslBitmapGraphic.Create;
  Try
    oClip.PasteBitmap(oImage);
    Result := InsertImage('pasted.bmp-'+FormatDateTime('yyyymmddhhnnsss.zzz', now), oImage);
  Finally
    oImage.Free;
  End;
End;


Function TWPRange.SaveSelection(oStream: TFslStream; aFormat: TWPFormat) : Boolean;
Var
  oBuffer : TFslBuffer;
  oDocument : TWPWorkingDocument;
  canCopyAsText : Boolean;
  iLoop : Integer;
Begin
  LogAction('SaveSelection', WPFORMAT_NAMES[aFormat]);
  Result := canWrite In Capabilities;
  If Result Then
  Begin
    oDocument := TWPWorkingDocument.Create;
    Try
      PrepareBalancedFragment(oDocument);

      canCopyAsText := False;
      iLoop := 0;
      While (iLoop <= oDocument.Pieces.Count - 1) And (Not canCopyAsText) Do
      Begin
        canCopyAsText := canCopyAsText Or (oDocument.Pieces[iLoop].CanCopyAsText);
        iLoop := iLoop + 1;
      End;

      oBuffer := TFslBuffer.Create;
      Try
        Case aFormat Of
          wpfNative: WriteRange(TWPNativeWriter, oDocument, oBuffer);
          wpfHTML: WriteRange(TWPHTMLWriter, oDocument, oBuffer);
          wpfRTF: WriteRange(TWPRTFWriter, oDocument, oBuffer);
//          wpfODT: WriteRange(TWPODTWriter, oDocument, oBuffer);
          wpfText:
             If canCopyAsText Then
               WriteRange(TWPTextWriter, oDocument, oBuffer, Operator_.Settings.TextWrapWidth)
             Else
               Result := False;
        End;
        oBuffer.SaveToStream(oStream);
      Finally
        oBuffer.Free;
      End;
    Finally
     oDocument.Free;
    End;
  End;
End;

Function TWPRange.PasteInsertWorkingDocument(oDocument : TWPWorkingDocument; oStyles : TWPStyles; bSkipLastPara : Boolean; Var sError : String) : Boolean;
Var
  iNew : Integer;
  oPiece : TWPWorkingDocumentPiece;
  bPara : Boolean;
  iLoop : Integer;
  sTypes : String;
  iIndex : Integer;
  oField : TWPWorkingDocumentFieldStartPiece;
Begin
  oField := Nil;
  Result := False;
  Assert(CheckCondition(Not oDocument.Pieces.Hooking, 'PasteInsert', 'Document is hooking; cannot paste'));
  If Operator_.Settings.ReadOnly Then
    sError := 'Document cannot be edited'
  Else If (HasCurrentFieldStart And HasElements(oDocument, [ptFieldStart, ptFieldStop, ptBreak, ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop], [ptPara], bSkipLastPara, sTypes)) Then
    sError := 'Cannot insert '+sTypes+' into a field'
  Else If (HasCurrentTableStart And HasElements(oDocument, [ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop, ptBreak], [], False, sTypes)) Then
    sError := 'Cannot insert '+sTypes+' into a table'
  Else If TWordProcessor(Owner).InSpeechMode And HasFieldCheckables(oDocument) Then
    sError := 'Attempt to use a field that has buttons while in speech mode'
  Else
  Begin
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;
    iIndex := Operator_.DirectAppendIndex;
    If ((Operator_.PieceBeforeAppend = Nil) Or (Operator_.PieceBeforeAppend.PieceType In [ptText, ptImage, ptFieldStop, ptLineBreak])) And
      (oDocument.Pieces.First.PieceType In [ptTableStart, ptSectionStart, ptBreak]) Then
    Begin
      iIndex := Operator_.DirectAppendPiece(CurrentParagraph.Clone, iIndex);
    End;
    iLoop := 0;
    while iLoop < oDocument.Pieces.count Do
    Begin
      oPiece := oDocument.Pieces[iLoop];
      bPara := oPiece.PieceType = ptPara;
      If Not bPara Or Not Operator_.Settings.NoParagraphs Then
      Begin
        If oPiece.PieceType = ptFieldStart Then
        Begin
          oField := TWPWorkingDocumentFieldStartPiece(oPiece);
          TWordProcessor(FOwner).BindToField(oDocument, iLoop, oField);
        End
        Else If oPiece.PieceType = ptSectionStart Then
          TWordProcessor(FOwner).BindToSection(TWPWorkingDocumentSectionStartPiece(oPiece))
        Else If oPiece.PieceType = ptFieldStop Then
        Begin
          TWPWorkingDocumentFieldStopPiece(oPiece).MatchingStart := oField.Link;
          oField := Nil;
        End;

        If Not (bPara And bSkipLastPara And (iLoop = oDocument.Pieces.count - 1)) Then
          iIndex := Operator_.DirectAppendPiece(oPiece.Link, iIndex);
      End;
      inc(iLoop);
    End;
    Operator_.endDirectAppend;

    iNew := Operator_.EndOfRange;
    Operator_.FinishOperation;
    Operator_.Styles.MergeStyles(oStyles, False);
    ChangeContent;
    DoMove(iNew, cdJump);
    Result := True;
  End;
End;

Procedure DebugSource(oBuffer : TFslBuffer; Const sTitle : String);
Begin
  oBuffer.SaveToFileName(SystemTemp + sTitle + '.txt');
  ExecuteOpen(SystemTemp + sTitle + '.txt');
End;


Function TWPRange.DoCopy(bDebug : Boolean) : Boolean;
Var
  oClip : TWPClipboard;
  oBuffer : TFslBuffer;
  oDocument : TWPWorkingDocument;
  canCopyAsText : Boolean;
  iLoop : Integer;
  oImg : TFslGraphic;
  oTemp : TFslVCLGraphic;
Begin
  Result := canWrite In Capabilities;
  If Result Then
    Begin
    oDocument := TWPWorkingDocument.Create;
    Try
      PrepareBalancedFragment(oDocument);

      canCopyAsText := False;
      iLoop := 0;
      While (iLoop <= oDocument.Pieces.Count - 1) And (Not canCopyAsText) Do
      Begin
        canCopyAsText := canCopyAsText Or (oDocument.Pieces[iLoop].CanCopyAsText);
        iLoop := iLoop + 1;
      End;

      oClip := TWPClipboard.Create;
      Try
        oClip.Open;
        oClip.Clear;

        oBuffer := TFslBuffer.Create;
        Try
          WriteRange(TWPNativeWriter, oDocument, oBuffer);
          oClip.CopyNative(oBuffer);
          If bDebug Then
            DebugSource(oBuffer, 'Native_Source');

          WriteRange(TWPRTFWriter, oDocument, oBuffer);
          oClip.CopyRTF(oBuffer);
          If bDebug Then
            DebugSource(oBuffer, 'RTF_source');

{          WriteRange(TWPOdtWriter, oDocument, oBuffer);
          oClip.CopyOdt(oBuffer);
          If bDebug Then
            DebugSource(oBuffer, 'ODT_source');
}
          // If cannot copy as text, then don't
          If canCopyAsText Then
          Begin
            WriteRange(TWPTextWriter, oDocument, oBuffer, Operator_.Settings.TextWrapWidth);
            oClip.CopyFormattedText(oBuffer);
            If bDebug Then
              DebugSource(oBuffer, 'Text_Formatted_source');

            WriteRange(TWPTextWriter, oDocument, oBuffer, -1);
            oClip.CopyText(oBuffer);
            If bDebug Then
              DebugSource(oBuffer, 'Text_source');


            WriteRange(TWPHL7FTWriter, oDocument, oBuffer, Operator_.Settings.TextWrapWidth);
{$IFDEF VER130}
            oBuffer.AsText := StringReplace(oBuffer.AsText, #1, '\');
{$ELSE}
            oBuffer.AsBytes := TEncoding.UTF8.GetBytes(StringReplace(String(oBuffer.AsText), #1, '\'));
{$ENDIF}
            oClip.CopyHL7(oBuffer);
            If bDebug Then
              DebugSource(oBuffer, 'HL7');

          End;

          // TODO: should only copy when image is reference or visual text existed
          WriteRange(TWPHTMLWriter, oDocument, oBuffer);
          oClip.CopyHTML(oBuffer);
          If bDebug Then
            DebugSource(oBuffer, 'HTML_source');


          If (oDocument.Pieces.Count = 1) And (oDocument.Pieces[0].PieceType = ptImage) Then
          begin
            oImg := TWPWorkingDocumentImagePiece(oDocument.Pieces[0]).GetWorkingImage(0, 0, False, false);
            if oImg is TFslVCLGraphic then
              oClip.CopyImage(TFslVCLGraphic(oImg))
            else
            begin
              oTemp := oImg.AsBitmap;
              try
                oClip.CopyImage(oTemp);
              finally
                oTemp.Free;
              end;
            end;
          end;
        Finally
          oBuffer.Free;
        End;
      Finally
        oClip.Free;
      End;
    Finally
     oDocument.Free;
    End;
    End;
End;

Function TWPRange.NewParagraph : Boolean;
Var
  oParagraph : TWPWorkingDocumentParaPiece;
  iNew : Integer;
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  oStyle : TWPStyle;
  oSelection : TWPSelection;
Begin
  Result := False;
  If (canInsert In Capabilities) And Not HasCurrentFieldStart And Not Operator_.Settings.NoParagraphs Then
  Begin
    If HasCurrentFieldStart Then
    Begin
      Result := LineBreak(True);
    End
    Else If Operator_.Settings.SmartParagraphs and (atStartOfPara(true) and (CurrentParagraph.Format.ListType in [WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers]) {and NextParaIsNotList}) then
    Begin
      result := ApplyNumbers(false);
    End
    else
    Begin
      LogAction('NewParagraph', '');
      // special case: if the next piece is a paragraph, and it has a style that resets at the end of the paragraph then
      If Not Selection.HasSelection Then
      Begin
        If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
        Begin
          If (iInternal = 0) And (oPiece.PieceType = ptPara) Then
          Begin
            oStyle := Operator_.Styles.GetByName(Style);
            If (oStyle = Nil) Or Not (oStyle.ResetOnNewParagraph) Then
              oStyle := Operator_.Styles.GetByName(oPiece.Style);
            If (oStyle <> Nil) And (oStyle.ResetOnNewParagraph) Then
            Begin
              // we're actually going to insert a paragraph after this one in the normal style
              oSelection := Selection.Clone;
              Try
                Selection.MoveTo(Selection.Cursor + 1);
                Operator_.StartOperation(FId, otInsert, False, Selection);
                Operator_.SetPriorCursor(oSelection.Summary);
                Operator_.CleanRange;
                oParagraph := TWPWorkingDocumentParaPiece.Create;
                Try
                  oParagraph.Style := Operator_.Styles.DefaultStyle.Name;
                  Operator_.InsertPiece(oParagraph.Link);
                Finally
                  oParagraph.Free;
                End;
                iNew := Operator_.StartOfRange;
                Operator_.FinishOperation;
                ChangeContent;
                DoMove(iNew, cdJump);
                Result := True;
              Finally
                oSelection.Free;
              End;
            End;
          End;
        End;
      End;

      If Not Result Then
      Begin
        Result := True;

        Operator_.StartOperation(FId, otInsert, False, Selection);

        Operator_.DeleteRange;

        If Document.GetPieceByPosition(Operator_.StartOfRange, oPiece, iInternal, iIndex) Then
          Begin
          While (iIndex < Document.Pieces.Count) And (Document.Pieces[iIndex].PieceType <> ptPara) Do
            Inc(iIndex);

          If iIndex < Document.Pieces.Count Then
            oPiece := Document.Pieces[iIndex]
          Else
            oPiece := Nil;
          End
        Else
          oPiece := Nil;

        oParagraph := TWPWorkingDocumentParaPiece.Create;
        Try
          If oPiece <> Nil Then
          Begin
            oParagraph.Style := oPiece.Style;
            oParagraph.Font.Assign(oPiece.Font);
          End;
          ApplyBaseStyle(oParagraph);
          If Assigned(oPiece) Then
            Begin
            oParagraph.Format.Assign(TWPWorkingDocumentParaPiece(oPiece).Format);
            TWPWorkingDocumentParaPiece(oPiece).Format.FixedNumber := DEF_WORD;
            End;
          Operator_.InsertPiece(oParagraph.Link);
        Finally
          oParagraph.Free;
        End;
        iNew := Operator_.EndOfRange;
        Operator_.FinishOperation;
        ChangeContent;
        DoMove(iNew, cdJump);
      End;
    End;
    UpdateTextMetrics;
  End;
End;

Function TWPRange.LineBreak(bSpeechMagicIsPara : Boolean = False): Boolean;
Var
  oBreak : TWPWorkingDocumentLineBreakPiece;
  iNew : Integer;
Begin
  If Not (canInsert In Capabilities) And Not Operator_.Settings.ConsoleMode Then
    Result := False
  Else
    Begin
    Result := True;
    LogAction('LineBreak', '', result);
    Operator_.StartOperation(FId, otInsert, False, Selection);
    Operator_.DeleteRange;
    oBreak := TWPWorkingDocumentLineBreakPiece.Create;
    Try
      oBreak.IsSpeechMagicParagraph := bSpeechMagicIsPara;
      ApplyBaseStyle(oBreak);
      Operator_.AppendPiece(oBreak.Link);
      iNew := Operator_.EndOfRange;
    Finally
      oBreak.Free;
    End;

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
    End;
End;


Function TWPRange.InsertSymbol(aChar : Char; Const sDrawnFontName : String) : Boolean;
Var
  oText : TWPWorkingDocumentTextPiece;
  iNew : Integer;
Begin
  result := false;
  LogAction('InsertSymbol', aChar+' : '+sDrawnFontName, result);
  If Not (canInsert In Capabilities) Then
    Result := False
  Else
    Begin
    If Not HasCurrentFieldStart Or Not HasCurrentFieldStop Or InputOkWithField(aChar) Then
    Begin
      If Operator_.StartOperation(FId, otCharInsert, False, Selection) Then
        Operator_.DeleteRange
      Else
        Operator_.CleanRange;

      oText := TWPWorkingDocumentTextPiece.Create;
      Try
        oText.Content := aChar;
        oText.Style := Style;
        oText.Font.Assign(Font);
        oText.DrawnFont := sDrawnFontName;
        Operator_.AppendPiece(oText.Link);
        iNew := Operator_.EndOfRange;
      Finally
        oText.Free;
      End;

      Operator_.FinishOperation;
      ChangeContent;
      DoMove(iNew, cdJump);
      Result := True;
      End
    Else
      Result := False;
    End;
End;


Function TWPRange.LineBreakToRight : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iInternal : Integer;
Begin
  If FDocument.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
    Result := oPiece.PieceType In [ptBreak]
  Else
    Result := False;
End;


Function TWPRange.Insert(aChar: Char; sStyle : String; oFont : TWPSFontDetails): Boolean;
Var
  oText : TWPWorkingDocumentTextPiece;
  iNew : Integer;
  bMakeList : boolean;
Begin
  result := false;
  LogAction('Insert', aChar+' : '+sStyle+' : '+oFont.Describe, result);

  If Not (canInsert In Capabilities) Then
    Result := False
  else
  Begin
    bMakeList := not FSelection.HasSelection and Operator_.Settings.SmartParagraphs and (aChar = ' ') and atEndOfPara and (ParaText = '*');

    If LineBreakToRight Then
    Begin
      NewParagraph;
      Left(False, False);
    End;

    If sStyle = '' Then
      sStyle := Style;

    If Not Assigned(oFont) Then
      oFont := Font;

    If Operator_.Settings.CapitaliseFirstInSentence And AtStartOfSentence(true) Then
      aChar := UpCase(aChar);

    If Not HasCurrentFieldStart Or Not HasCurrentFieldStop Or InputOkWithField(aChar) Then
    Begin
      If Operator_.StartOperation(FId, otCharInsert, StringIsAlphanumeric(aChar), Selection) Then
        Operator_.DeleteRange
      Else
        Operator_.CleanRange;

      oText := TWPWorkingDocumentTextPiece.Create;
      Try
        oText.Content := aChar;
        oText.Style := sStyle;
        oText.Font.Assign(oFont);
        Operator_.AppendPiece(oText.Link);
        iNew := Operator_.EndOfRange;
      Finally
        oText.Free;
      End;

      Operator_.FinishOperation;
      ChangeContent;
      DoMove(iNew, cdJump);
      Result := True;
      End
    Else
      Result := False;
    if result and bMakeList then
    begin
      ApplyBullets(true);
      Left(true, false);
      Left(true, false);
      DeleteSelection;
    end;
  End;
  If (aChar = '.') Then
    UpdateTextMetrics;
End;

Function TWPRange.Insert(sText : String; oFont : TWPSFontDetails = Nil; sStyle : String = ''; bSelect : Boolean = False): Boolean;
Var
  iOld : Integer;
  iNew : Integer;
  sLeft : String;
  bOk : Boolean;
  bFirst : Boolean;
  bField : Boolean;
Begin
  LogAction('Insert', sText+' : '+sStyle+' : '+oFont.Describe);
  If Not (canInsert In Capabilities) And ConsoleFontOk(oFont, FFont) Then
  Begin
    Result := False
  End
  Else
    Begin
    bField := HasCurrentFieldStart;
    bOk := StringSplit(sText, #13#10, sLeft, sText);
    bFirst := True;

    While bFirst Or (sLeft <> '') Or bOk Do
    Begin
      If bFirst Or (sLeft <> '') Then
      Begin
        iOld := Selection.WorkingSelStart;

        Operator_.StartOperation(FId, otInsert, False, Selection);
        Operator_.DeleteRange;

        iNew := IntegerMax(Operator_.EndOfRange, InsertInOperation(sLeft, oFont, sStyle));

        Operator_.FinishOperation;
        ChangeContent;

        If bSelect Then
        Begin
          DoMove(iOld, cdJump);
          DoSelect(iNew, cdRight);
        End
        Else
        Begin
         DoMove(iNew, cdJump);
      End;
      End;

      If bOk Then
      Begin
        If bField Then
          LineBreak(True)
        Else If Not Operator_.Settings.NoParagraphs Then
        NewParagraph;
      End;

      bOk := StringSplit(sText, #13#10, sLeft, sText);
      bFirst := False;
    End;
    Result := True;
    End;

  If StringContainsAny(sText, ['.', ',']) Then
    UpdateTextMetrics;
End;

Function TWPRange.DeleteSelection : Boolean;
Begin
  result := false;
  LogAction('DeleteSelection', '', result);
  Result := DoDeleteSelection(Selection.Save);
End;


Function TWPRange.DeleteWordLeft : Boolean;
Var
  sSaved : String;
Begin
  result := false;
  LogAction('DeleteWordLeft', '', result);
  sSaved := Selection.Save;
  Result := Selection.HasSelection;
  If Not Result Then
    Result := LeftWord(True, False);
  If Result Then
    Result := DoDeleteSelection(sSaved);
  If Not Result Then
    Begin
    Selection.Restore(sSaved);
    ChangeState;
    End;
End;


Function TWPRange.DoDeleteSelection(sCursor : String): Boolean;
Var
  iNew : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And Selection.HasSelection And (canDelete In GetSelectionCapabilities);
  If Result Then
    Begin
    Operator_.StartOperation(FId, otDelete, False, Selection);
    Operator_.SetPriorCursor(sCursor);
    Operator_.DeleteRange;
    iNew := Operator_.StartOfRange;
    Operator_.FinishOperation;
    ChangeContent;
    MoveTo(iNew);
    End;
End;

Function TWPRange.DeleteLeft : Boolean;
Var
  sSaved : String;
  oParagraph : TWPSParagraphDetails;
Begin
  result := false;
  LogAction('DeleteLeft', '', result);

  sSaved := Selection.Save;
  Result := Selection.HasSelection;
  if (CanFormat in Capabilities) and Operator_.Settings.SmartParagraphs and AtStartOfSentence(false) and (CurrentParagraph.Format.ListType in [WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers]) then
  begin
    oParagraph := TWPSParagraphDetails.Create;
    Try
      oParagraph.ListType := WPSParagraphListTypeNone;
      Result := ApplyParagraph(oParagraph);
      if Result then
        Paragraph.ListType := WPSParagraphListTypeNone;
    Finally
      oParagraph.Free;
    End;
  end
  else if Operator_.Settings.SmartParagraphs and AdjacentIsPara(dleft) then
    result := MergeParagraph(dleft)
  else
  begin
    If Not Result Then
      Result := Left(True, False);
    If Result Then
      Result := DoDeleteSelection(sSaved);
  end;
  If Not Result Then
    Begin
    Selection.Restore(sSaved);
    ChangeState;
    End;
End;

Function TWPRange.DeleteRight : Boolean;
Var
  sSaved : String;
Begin
  result := false;
  LogAction('DeleteRight', '', result);
  sSaved := Selection.Save;
  Result := Selection.HasSelection;
  if Operator_.Settings.SmartParagraphs and AdjacentIsPara(dRight) then
    result := MergeParagraph(dRight)
  else
  begin
   If Not Result Then
     Result := Right(True, False);
    If Result Then
      Result := DoDeleteSelection(sSaved);
  end;
  If Not Result Then
    Begin
    Selection.Restore(sSaved);
    ChangeState;
    End;
End;

Function TWPRange.DeleteWordRight : Boolean;
Var
  sSaved : String;
Begin
  result := false;
  LogAction('DeleteWordRight', '', result);
  sSaved := Selection.Save;
  Result := Selection.HasSelection;
  If Not Result Then
    Result := RightWord(True, False);
  If Result Then
    Result := DoDeleteSelection(sSaved);
  If Not Result Then
    Begin
    Selection.Restore(sSaved);
    ChangeState;
    End;
End;

Function TWPRange.SelectionMatchesFieldOuter : Boolean;
Begin
  If HasCurrentFieldStart And HasCurrentFieldStop And Selection.HasSelection  Then
    Result := (CurrentFieldStart.Metrics.Position = Selection.SelStart) And (CurrentFieldStop.Metrics.Position + 1 = Selection.SelEnd)
  Else
    Result := False;
End;

function TWPRange.GetApplicableField(cursor: integer): TWPWorkingDocumentFieldStartPiece;
Var
  iIndex : Integer;
  iOffset : Integer;
  oPiece : TWPWorkingDocumentPiece;
  found : boolean;
Begin
  result := nil;
  found := Document.GetPieceByPosition(cursor, oPiece, iIndex, iOffset);
  If found Then
  begin
    while oPiece <> nil do
    begin
      if oPiece.PieceType = ptFieldStart then
      begin
        result := TWPWorkingDocumentFieldStartPiece(oPiece);
        break;
      end;
      if oPiece.PieceType in [ptFieldStop, ptBreak, ptPara, ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop] then
        break;
      oPiece := oPiece.Prev;
    end;
  end;
end;

Function TWPRange.GetCapabilities: TWPCapabilities;
Var
  aSelection : TWPCapabilities;
  bInsideField : Boolean;
Begin
  Result := [];

  // selection independent
  // canUndo - if undo's in stack
  // canRedo - if redo's in stack
  // canGotoField - if fields exist

  If Not Operator_.Settings.ReadOnly And Operator_.CanUndo(FId) Then
    Include(Result, canUndo);
  If Not Operator_.Settings.ReadOnly And Operator_.CanRedo(FId) Then
    Include(Result, canRedo);
  If Document.FieldCount > 0 Then
    Include(Result, canGotoField);

  // selection dependent:
  aSelection := [];

  If Selection.HasSelection Then
    Begin

    // canCut - if Operator_ thinks so (no read only content, coherent selection, ?)
    // canInsert - if canCut
    // canFormat - if not in read only content or ?
    // canInsertImage - if canCut
    // canInsertField - if CanCut
    // canFieldProps - if selection is within or spanning a single field
    // canRemoveField - if canCut and selection is within or spanning a single field

    aSelection := GetSelectionCapabilities;

    If canWrite In aSelection Then
      Include(Result, canWrite);
    If canInsertAnnotation In aSelection Then
      Include(Result, canInsertAnnotation);

    If Not Operator_.Settings.ReadOnly And (canDelete In aSelection) Then
      Begin
      Include(Result, canCut);
      Include(Result, canDelete);
      if (canWrite in result) then
        Include(Result, canInsert);
      If Operator_.Settings.Images And Operator_.Settings.Format Then
        Include(Result, canInsertImage);
      End;

    If Not Operator_.Settings.ReadOnly Then
      Begin
        If HasSelectedTable Then Include(Result, canTableProps);
        If HasSelectedRows Then Include(Result, canRowProps);
        If HasSelectedTable And CanTableBeSorted(SelectedTable) Then Include(Result, canSortTable);
      End;
    bInsideField := False;
    End
  Else If Not Operator_.Settings.ReadOnly Then
    Begin
    // canInsert - if not in read only content
    // canFormat - if not in read only content or ?
    // canCut - false
    // canInsertImage - if CanInsert and AllowImages
    // canImageProps - false
    // canLineProps - false
    // canInsertField - if CanInsert
    // canFieldProps - if in field
    // canRemoveField - if in field
    // canEditComment - if in annotated area

    bInsideField := InsideField;
    If Not ContextIsReadOnly And Not bInsideField Then
      Include(Result, canInsert);

    If Operator_.Settings.Images And (canInsert In Result) And Operator_.Settings.Format Then
      Include(Result, canInsertImage);
    End
  Else
    Begin
    bInsideField := False;
    End;

  If Not Operator_.Settings.ReadOnly And (Document.AllAnnotations.GetBySelection(Selection.WorkingSelStart, Selection.WorkingSelEnd) <> Nil)
    And Operator_.Settings.ManageAnnotations Then
      Include(Result, canManageAnnotation);

  If Not Operator_.Settings.ReadOnly And HasCurrentImage And Operator_.Settings.Format And Not Operator_.Settings.FormsMode And Not CurrentImage.IsReadOnly Then
    Include(Result, canImageProps);

  If Not Operator_.Settings.ReadOnly And HasCurrentLine And Operator_.Settings.Format And Not Operator_.Settings.FormsMode And Not CurrentLine.IsReadOnly Then
    Include(Result, canLineProps);

  If Not Operator_.Settings.ReadOnly And Not Operator_.Settings.FormsMode Then
    If (canInsert In Result) And Not HasCurrentFieldStart Then
      Include(Result, canInsertField);

  If Not Operator_.Settings.ReadOnly Then
  Begin
    If HasCurrentSectionStart And (CurrentSectionStart.IsField) And Not Operator_.Settings.FormsMode Then
    Begin
      If ((Not CurrentSectionStart.IsReadOnly) Or (CurrentSectionStart.ReadOnly = tsFalse)) Then
      Begin
        If CurrentSectionStart.HasDefinitionProvider And CurrentSectionStart.DefinitionProvider.UserCanEditField(CurrentSectionStart.DocSection) Then
          Include(Result, canFieldSectionProps);
      End;
      Include(Result, canRemoveFieldSection);
    End;

    If HasCurrentFieldStart Then
    Begin
      If Operator_.Settings.Format And (SelectionMatchesFieldOuter Or (CurrentFieldStart.FixedFormat = fffAnyPart)) And Not bInsideField And (Not Selection.HasSelection Or (CanFormat In aSelection)) Then
        Include(Result, CanFormat);

      If (CurrentFieldStart.Deletable) Or (TWPWorkingDocumentFieldStartPiece(CurrentFieldStart).HasDefinitionProvider And TWPWorkingDocumentFieldStartPiece(CurrentFieldStart).DefinitionProvider.UserCanDeleteField(TWPWorkingDocumentFieldStartPiece(CurrentFieldStart).DocField)) And Not Operator_.Settings.FormsMode Then
      Begin
        If (Not CurrentFieldStart.IsReadOnly) Or (CurrentFieldStart.ReadOnly = tsFalse) Then
        Begin
          If CurrentFieldStart.HasDefinitionProvider And CurrentFieldStart.DefinitionProvider.UserCanEditField(CurrentFieldStart.DocField) Then
            Include(Result, canFieldProps);
          If HasCurrentParagraph And Not CurrentParagraph.IsReadOnly Then
            Include(Result, canRemoveField);
        End
        Else If (CurrentFieldStart.ReadOnly = tsTrue) And HasCurrentParagraph And Not CurrentParagraph.IsReadOnly Then
          Include(Result, canRemoveField);
      End;
    End
    Else If Not Operator_.Settings.FormsMode Then
    Begin
      If (canInsert In Result) Then
      Begin
        Include(Result, canInsertLine);
        If Not HasCurrentTableStart Then
          Include(Result, canInsertPageBreak);
      End;

      If Operator_.Settings.Format Then
      Begin
        If Selection.HasSelection Then
        Begin
          If canFormat In aSelection Then
            Include(Result, canFormat);
        End
        Else If Not ContextIsReadOnly And Not bInsideField Then
          Include(Result, canFormat);
      End;
    End;
  End;

  If Not HasCurrentTableStart And (canInsert In Result) And Not Operator_.Settings.FormsMode Then
  Begin
//    If Selection.HasSelection Then
    If CanConvertTextToTable Then
      Include(Result, canConvertToTable);
    Include(Result, canInsertTable);
  End;

  If HasCurrentTableStart Then
  Begin
    Include(Result, canSelectTable);
    If HasCurrentTableRowStart Then
      Include(Result, canSelectRow);
  End;


  If Not Operator_.Settings.ReadOnly And HasCurrentTableStart And Not Operator_.Settings.FormsMode Then
  Begin
    If Not CurrentTableStart.IsReadOnly Then
    Begin
      Include(Result, canTableProps);
      If Not CurrentTableStart.AnyContentReadOnly And
          Not ((CurrentTableStart = Document.Pieces[0]) And (CurrentTableStop = Document.Pieces[Document.Pieces.Count - 1])) Then
        Include(Result, canRemoveTable);

      If CanTableBeSorted(CurrentTableStart) Then Include(Result, canSortTable);
    End;

    If HasCurrentTableRowStart Then
    Begin
      If Not CurrentTableStart.IsReadOnly Then
      Begin
        If Not (CurrentTableRowStart.Header Or ((CurrentTableRowStart.State = tisFirst) And CurrentTableRowStart.IsReadOnly)) Then
          Include(Result, canInsertRowAbove);
        Include(Result, canInsertRowBelow);
      End;

      If Not CurrentTableRowStart.IsReadOnly And (CurrentTableRowStart.State <> tisOnly) Then
        Include(Result, canRemoveRow); // but not if there is only row or it is read only

      If Not CurrentTableRowStart.IsReadOnly {And (CurrentTableRowStart.State In [tisFirst, tisOnly])} Then
        Include(Result, canRowProps);

      If Not FSelection.HasSelection And HasCurrentTableCellStart And Not CurrentTableRowStart.IsReadOnly And CanManipulateColumns And (CurrentTableCellStart.State In [tisFirst, tisMiddle]) Then
        Include(Result, canMergeCells);

      If Not FSelection.HasSelection And HasCurrentTableCellStart And Not CurrentTableRowStart.IsReadOnly And CanManipulateColumns And (CurrentTableCellStart.Span > 1) Then
        Include(Result, canSplitCell);

    End;

    If HasCurrentTableCellStart And Not CurrentTableStart.Jagged And Not CurrentTableStart.AnyRowsReadOnly And CanManipulateColumns Then
    Begin
      Include(Result, canInsertColumn);
      If Not CurrentTableStart.AnyCellsReadOnly(CurrentTableStart.ColumnIndexForCell(CurrentTableCellStart)) And Not (CurrentTableCellStart.State = tisOnly) Then
        Include(Result, canRemoveColumn);
    End;
  End;
  If (canInsertTable In Result) And (canInsertField In Result) And Not (HasCurrentSectionStart And CurrentSectionStart.IsField) Then
    Include(Result, canInsertFieldSection);

   If (canInsertField In Result) And Operator_.Settings.InsertTemplates Then
      Include(Result, canInsertTemplate);

End;

Function TWPRange.DoMove(iIndex : Integer; aDirection : TWPCursorDirection; bDraw : Boolean) : Boolean;
Begin
  If Selection.MoveTo(Document.NearestValidCursor(iIndex, aDirection, Operator_.Settings.NoSelectReadOnly)) Or bDraw Then
  Begin
    CheckSpelling;
    ChangeState(bDraw);
  End;
  Result := True;
End;

Function TWPRange.DoSelect(iIndex : Integer; aDirection : TWPCursorDirection; bDraw : Boolean): Boolean;
Var
  iActual : Integer;
Begin
  Result := Operator_.Settings.Selecting;
  If Result Then
  Begin
    // now we have to find the nearest proper selection for iIndex
    iActual := Document.NearestValidCursor(iIndex, aDirection, Operator_.Settings.NoSelectReadOnly);
    If Selection.SelectTo(iActual, iIndex) Then
    Begin
      CheckSpelling;
      ChangeState(bDraw);
    End;
  End;
End;

Function TWPRange.MoveTo(iIndex : Integer; bDraw : Boolean) : Boolean;
Begin
  Operator_.CloseLastOperation;
  LogAction('MoveTo', Format('%d, %d', [iIndex, ord(bDraw)]));
  Result := DoMove(iIndex, cdJump, bDraw);
End;

Function TWPRange.SelectTo(iIndex : Integer; bDraw : Boolean): Boolean;
Begin
  Operator_.CloseLastOperation;
  LogAction('SelectTo', Format('%d, %d', [iIndex, ord(bDraw)]));
  Result := DoSelect(iIndex, cdJump, bDraw);
End;

Procedure TWPRange.MoveCursor(bSelect : Boolean; iNew : Integer; aDirection : TWPCursorDirection; bDraw : Boolean);
Begin
  If bSelect Then
    DoSelect(iNew, aDirection, bDraw)
  Else
    DoMove(iNew, aDirection, bDraw);
End;

Function TWPRange.SelCursorToChange(bSelect : Boolean) : Integer;
Begin
  If bSelect And Selection.HasSelection Then
    Result := Selection.SelToMove
  Else If Selection.HasSelection Then
    Result := Selection.SelToMove
  Else
    Result := Selection.Cursor;
End;

Function TWPRange.SetSelStart(iIndex : Integer) : Boolean;
Begin
  Result := Operator_.Settings.Selecting;
  LogAction('SetSelStart', Format('%d', [iIndex]), result);
  If Result Then
  Begin
    Operator_.CloseLastOperation;
    Selection.SetSelStart(iIndex);
    CheckSpelling;
    ChangeState(True);
  End;
End;

Function TWPRange.SetSelEnd(iIndex : Integer) : Boolean;
Begin
  Result := Operator_.Settings.Selecting;
  LogAction('SetSelEnd', Format('%d', [iIndex]), result);
  If Result Then
  Begin
    Operator_.CloseLastOperation;
    Selection.SetSelEnd(iIndex);
    CheckSpelling;
    ChangeState(True);
  End;
End;

Function TWPRange.GoHome(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.DocHome(Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('GoHome', Format('%d, %d', [iNew, ord(bSelect)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft);
End;

Function TWPRange.LeftWord(bSelect : Boolean; bDraw : Boolean = True) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.WordLeft(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('LeftWord', Format('%d, %d/%d', [iNew, ord(bSelect), ord(bDraw)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft, bDraw);
End;

Function TWPRange.Left(bSelect, bDraw : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.NextLeft(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('Left', Format('%d, %d/%d', [iNew, ord(bSelect), ord(bDraw)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft, bDraw);
End;

Function TWPRange.Right(bSelect : Boolean; bDraw : Boolean = True) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.NextRight(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('LeftRight', Format('%d, %d/%d', [iNew, ord(bSelect), ord(bDraw)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight, bDraw);
End;

Function TWPRange.RightWord(bSelect : Boolean; bDraw : Boolean = True) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.WordRight(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('RightWord', Format('%d, %d/%d', [iNew, ord(bSelect), ord(bDraw)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight, bDraw);
End;

Function TWPRange.GoEnd(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.DocEnd(Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('GoEnd', Format('%d, %d', [iNew, Ord(bSelect)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight);
End;

Function TWPRange.GoHomeInSection(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.SectionHome(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, True, iNew);
  LogAction('GoHomeInSection', Format('%d, %d', [iNew, Ord(bSelect)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdLeft);
End;

Function TWPRange.GoEndInSection(bSelect : Boolean) : Boolean;
Var
  iNew : Integer;
Begin
  Operator_.CloseLastOperation;
  Result := Document.SectionEnd(SelCursorToChange(bSelect), Operator_.Settings.NoSelectReadOnly, True, iNew);
  If Not Result Then
    Result := Document.DocEnd(Operator_.Settings.NoSelectReadOnly, iNew);
  LogAction('GoEndInSection', Format('%d, %d', [iNew, Ord(bSelect)]), result);
  If Result Then
    MoveCursor(bSelect, iNew, cdRight);
End;


Function TWPRange.SelectAll : Boolean;
Begin
  Result := Operator_.Settings.Selecting;
  LogAction('SelectAll', Format('%d - %d', [0, Document.CharCount - 1]), result);
  If Result Then
  Begin
    Operator_.CloseLastOperation;
    Selection.Select(0, Document.CharCount - 1);
    ChangeState;
  End;
End;

Function TWPRange.SelectRange(iStart, iEnd: Integer; bDraw: Boolean): Boolean;
Begin
  LogAction('SelectRange', Format('%d - %d', [iStart, iEnd]));
  MoveTo(iStart, False);
  Result := SelectTo(iEnd, bDraw);
End;


Function TWPRange.SelectPara : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iInternal : Integer;
  iLoop : Integer;
Begin
  Result := FDocument.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex);
  LogAction('SelectPara', Format('%d', [iIndex]), result);
  If Result Then
    Begin
    iLoop := iIndex;
    While (iLoop >= 0) And Not (FDOcument.Pieces[iLoop].PieceType In [ptSectionStart, ptSectionStop, ptPara]) Do
        Dec(iLoop);
    MoveTo(FDocument.Pieces[iLoop+1].Metrics.Position, False);

    iLoop := iIndex;
    While (iLoop < FDocument.Pieces.Count) And (FDocument.Pieces[iLoop].PieceType <> ptPara) Do
        Inc(iLoop);
    SelectTo(FDocument.Pieces[iLoop].Metrics.Position + oPiece.Metrics.OffsetCount, True);
    End;
End;

Function TWPRange.SelectWord : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iInternal : Integer;
  iLoop : Integer;
Begin
  Result := FDocument.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex);
  LogAction('SelectWord', Format('%d', [iIndex]), result);
  If Result Then
    Begin
    If (oPiece.PieceType = ptText) And (Not IsWordBreak(oPiece.LogicalText)) Then
      Begin
      iLoop := iIndex;
      While (iLoop >= 0) And (FDOcument.Pieces[iLoop].PieceType = ptText) And (Not IsWordBreak(FDOcument.Pieces[iLoop].LogicalText)) Do
        Dec(iLoop);
      MoveTo(FDocument.Pieces[iLoop+1].Metrics.Position, False);
      iLoop := iIndex;
      While (iLoop < FDocument.Pieces.Count) And (FDocument.Pieces[iLoop].PieceType = ptText) And (Not IsWordBreak(FDocument.Pieces[iLoop].LogicalText)) Do
        Inc(iLoop);
      SelectTo(FDocument.Pieces[iLoop-1].Metrics.Position + oPiece.Metrics.OffsetCount, True);
      End
    Else
      Begin
      MoveTo(oPiece.Metrics.Position, False);
      SelectTo(oPiece.Metrics.Position + oPiece.Metrics.OffsetCount, True);
      End;
    End;
End;

Function TWPRange.InputOkWithField(aChar: Char): Boolean;
Var
  sProposed : String;
  iOffset : Integer;
Begin
  iOffset := CurrentFieldStart.Metrics.Position + CurrentFieldStart.Metrics.CharCount;
  sProposed := CurrentFieldContent;
  If Selection.HasSelection Then
  Begin
    Delete(sProposed, Selection.SelStart - iOffset + 1, Selection.SelEnd - Selection.SelStart);
    System.Insert(aChar, sProposed, Selection.SelStart - iOffset);
  End
  Else
    System.Insert(aChar, sProposed, Selection.Cursor - iOffset + 1);
  Result := Not CurrentFieldStart.HasDefinitionProvider Or CurrentFieldStart.DefinitionProvider.CheckProposed(CurrentFieldStart.DocField, sProposed);
End;

Function TWPRange.InsertInOperation(sText : String; oFont : TWPSFontDetails = Nil; Const sStyle : String = ''): Integer;
  Function AddWord(Const sWord : String) : Integer;
  Var
    oText : TWPWorkingDocumentTextPiece;
  Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      oText.Content := sWord;
      ApplyBaseStyle(oText);
      If sStyle <> '' Then
        oText.Style := sStyle;
      If Assigned(oFont) Then
        oText.Font.Assign(oFont);
      Operator_.AppendPiece(oText.Link);
      Result := Operator_.EndOfRange;
    Finally
      oText.Free;
    End;
  End;
  Function AddPara(bSpeechMagicDouble : Boolean) : Integer;
  Var
    oPara : TWPWorkingDocumentParaPiece;
  Begin
    oPara := TWPWorkingDocumentParaPiece.Create;
    Try
      ApplyBaseStyle(oPara);
      If (sStyle <> '') Then
        oPara.Style := sStyle;
      If Assigned(oFont) Then
        oPara.Font.Assign(oFont);
      oPara.SpeechMagicDouble := bSpeechMagicDouble;
      Operator_.AppendPiece(oPara.Link);
      Result := Operator_.EndOfRange;
    Finally
      oPara.Free;
    End;
  End;
  Function AddBreak : Integer;
  Var
    oBreak : TWPWorkingDocumentLineBreakPiece;
  Begin
    oBreak := TWPWorkingDocumentLineBreakPiece.Create;
    Try
      ApplyBaseStyle(oBreak);
      If sStyle <> '' Then
        oBreak.Style := sStyle;
      If Assigned(oFont) Then
        oBreak.Font.Assign(oFont);
      Operator_.AppendPiece(oBreak.Link);
      Result := Operator_.EndOfRange;
    Finally
      oBreak.Free;
    End;
  End;
Var
  oSplit : TWPWordIterator;
  s : String;
Begin
  // TODO: are we in a field?

  Result := -1;
  oSplit := TWPWordIterator.Create;
  Try
    oSplit.Init(sText);
    While oSplit.More Do
    Begin
      s := oSplit.Next;
      If s = #13 Then
      Begin
        If HasCurrentFieldStart Then
        Begin
          AddWord(' '); // for speech magic offset calculations
          Result := AddBreak;
        End
        Else
          Result := addPara(True);
      End
      Else If s = #11 Then
        Result := AddBreak
      Else
        Result := AddWord(s);
    End;
  Finally
    oSplit.Free;
  End;
End;

Procedure TWPRange.PrepareBalancedFragment(oDocument : TWPWorkingDocument);
Begin
  PrepareBalancedFragment(oDocument, Selection.SelStart, Selection.SelEnd);
End;

Procedure TWPRange.PrepareBalancedFragment(oDocument : TWPWorkingDocument; iStart : Integer; iStop : Integer);
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iOffset : Integer;
  aAdded : TWPWorkingDocumentPieceTypes;
  aLoop : TWPWorkingDocumentPieceType;
  aBalanced : TPieceTypeCount;
  bNeedsPara : Boolean;
  oText : TWPWorkingDocumentTextPiece;
Begin
  oDocument.Pieces.Clear;
  aAdded := [];
  bNeedsPara := False;
  For aLoop := Low(TWPWorkingDocumentPieceType) To High(TWPWorkingDocumentPieceType) Do
    aBalanced[aLoop] := 0;
  iStart := TrimSelectionStart(iStart);
  iStop := TrimSelectionEnd(iStop);

  If (iStart <= iStop) And Document.GetPieceByPosition(iStart, oPiece, iOffset, iIndex) Then
  Begin
    // prepare a balanced view of the oRange
    While Assigned(oPiece) And (oPiece.Metrics.Position + oPiece.Metrics.CharCount <= iStop) Do
    Begin
      Case oPiece.PieceType Of
        ptText :
          Begin
          oDocument.Pieces.Add(TWPWorkingDocumentTextPiece(oPiece).CloneEnd(iOffset));
          bNeedsPara := True;
          End;
        ptImage, ptLineBreak :
          bNeedsPara := True;
        ptFieldStart :
          Begin
          Inc(aBalanced[ptFieldStart]);
          bNeedsPara := True;
          End;
        ptFieldStop :
          Begin
          bNeedsPara := True;
          If Not (ptFieldStart In aAdded) Then
            AddPriorPieces(Document, oDocument, iIndex, ptFieldStart, [], aAdded, aBalanced);
          Dec(aBalanced[ptFieldStart]);
          End;
        ptTableStart : Inc(aBalanced[ptTableStart]);
        ptRowStart :
          Begin
          If Not (ptTableStart In aAdded) Then
            AddPriorPieces(Document, oDocument, iIndex, ptTableStart, [], aAdded, aBalanced);
          Inc(aBalanced[ptRowStart]);
          End;
        ptCellStart :
          Begin
          If Not (ptRowStart In aAdded) Then
          Begin
            AddPriorPieces(Document, oDocument, iIndex, ptRowStart, [ptCellStart, ptCellStop], aAdded, aBalanced);
            AddPriorPieces(Document, oDocument, iIndex, ptTableStart, [], aAdded, aBalanced);
          End;
          Inc(aBalanced[ptCellStart]);
          End;
        ptTableStop :
          Begin
          If Not (ptTableStart In aAdded) Then
            AddPriorPieces(Document, oDocument, iIndex, ptTableStart, [], aAdded, aBalanced);
          Dec(aBalanced[ptTableStart]);
          End;
        ptRowStop :
          Begin
          If Not (ptRowStart In aAdded) Then
            AddPriorPieces(Document, oDocument, iIndex, ptTableStart, [ptRowStart, ptCellStart], aAdded, aBalanced);
          Dec(aBalanced[ptRowStart]);
          End;
        ptCellStop :
          Begin
          If Not (ptRowStart In aAdded) Then
          Begin
            AddPriorPieces(Document, oDocument, iIndex, ptRowStart, [ptCellStart, ptCellStop], aAdded, aBalanced);
            AddPriorPieces(Document, oDocument, iIndex, ptTableStart, [], aAdded, aBalanced);
          End;
          Dec(aBalanced[ptCellStart]);
          End;
        ptPara :
          bNeedsPara := False;
        ptSectionStart, ptSectionStop : ; // we do not copy these
      End;
      If Not (oPiece.PieceType In [ptText, ptSectionStart, ptSectionStop]) Then
      Begin
        oDocument.Pieces.Add(oPiece.Clone);
        Include(aAdded, oPiece.PieceType);
      End;
      Inc(iIndex);
      iOffset := 0;
      If iIndex < Document.Pieces.Count Then
        oPiece := Document.Pieces[iIndex]
      Else
        oPiece := Nil;
    End;
    If Assigned(oPiece) And (oPiece.PieceType = ptText) And (oPiece.Metrics.Position < iStop) Then
    Begin
      oText := TWPWorkingDocumentTextPiece(oPiece).CloneStart(iStop - oPiece.Metrics.Position);
      Try
        oText.Content := System.Copy(oText.Content, 1 + iOffset, MAXINT);
        oDocument.Pieces.Add(oText.Link);
        bNeedsPara := True;
        Include(aAdded, ptText);
      Finally
        oText.Free;
      End;
    End;

    Assert(CheckCondition(aBalanced[ptFieldStart] <= 1, 'WriteRange', 'Multiple unmatched field starts'));
    Assert(CheckCondition(aBalanced[ptCellStart] <= 1, 'WriteRange', 'Multiple unmatched cell starts'));
    Assert(CheckCondition(aBalanced[ptRowStart] <= 1, 'WriteRange', 'Multiple unmatched row starts'));
    Assert(CheckCondition(aBalanced[ptTableStart] <= 1, 'WriteRange', 'Multiple unmatched Table starts'));

    If aBalanced[ptFieldStart] = 1 Then
      oDocument.Pieces.Add(TWPWorkingDocumentFieldStopPiece.Create);
    If bNeedsPara And (aAdded * BLOCK_PIECE_TYPES <> []) Then
      AddPostPieces(Document, oDocument, iIndex, ptPara, []);
//    If aBalanced[ptCellStart] = 1 Then
//      oDocument.Pieces.Add(TWPWorkingDocumentStopPiece.Create(stTableCell));
    If aBalanced[ptRowStart] = 1 Then
      AddPostPieces(Document, oDocument, iIndex, ptRowStop, [ptCellStart, ptCellStop]);
    If aBalanced[ptTableStart] = 1 Then
      oDocument.Pieces.Add(TWPWorkingDocumentStopPiece.Create(stTable));
  End;
  InsertMissingParagraphs(oDocument);
  oDocument.RegenerateMetrics(False, False);
End;



Procedure TWPRange.WriteRange(aWriterClass : TWPWriterClass; oDocument : TWPWorkingDocument; oDest : TFslBuffer; iWidth : Integer);
Var
  oStream : TFslStringStream;
  oWriter : TWPWriter;
Begin
  oDest.Clear;
  oStream := TFslStringStream.Create;
  Try
    oWriter := aWriterClass.Create;
    Try
      oWriter.Stream := oStream.Link;
      oWriter.Styles := Operator_.Styles.Link;
      If oWriter Is TWPTextWriter Then
        TWPTextWriter(oWriter).Width := iWidth;
      If oWriter Is TWPHL7FTWriter Then
      Begin
        TWPHL7FTWriter(oWriter).UseHighlighting := True;
        TWPHL7FTWriter(oWriter).AllOneField := True;
      End;
      oWriter.Write(oDocument);
    Finally
      oWriter.Free;
    End;
{$IFDEF VER130}
    oStream.Position := 0;
    oDest.Capacity := oStream.size;
    oDest.LoadFromStream(oStream);
{$ELSE}
    oDest.AsBytes := oStream.Bytes;
{$ENDIF}
  Finally
    oStream.Free;
  End;
End;



Function TWPRange.HasElements(oDocument : TWPWorkingDocument; Const aTypesOne, aTypesTwo : TWPWorkingDocumentPieceTypes; bTypesTwoNotLast : Boolean; Var sTypes : String) : Boolean;
Var
  iLoop : Integer;
  aType : TWPWorkingDocumentPieceType;
  aFound : TWPWorkingDocumentPieceTypes;
Begin
  Result := False;
  aFound := [];

  For iLoop := 0 To oDocument.Pieces.Count - 1 Do
  Begin
    aType := oDocument.Pieces[iLoop].PieceType;
    If (aType In aTypesOne) Or ((Not bTypesTwoNotLast Or (iLoop <> oDocument.Pieces.Count - 1)) And (aType In aTypesTwo)) Then
    Begin
      Result := True;
      include(aFound, aType);
    End;
  End;

  sTypes := '';
  If [ptFieldStart, ptFieldStop] * aFound <> [] Then
    sTypes := 'Fields';

  If [ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop] * aFound <> [] Then
    StringAppend(sTypes, 'Tables', ' and ');
  If [ptPara] * aFound <> [] Then
    StringAppend(sTypes, 'Paragaphs', ' and ');
  If [ptBreak] * aFound <> [] Then
    StringAppend(sTypes, 'Breaks', ' and ');
  If [ptSectionStart, ptSectionStop] * aFound <> [] Then
    StringAppend(sTypes, 'Sections', ' and ');
End;


Function TWPRange.CursorInWord(Out sText : String):Boolean;
Var
  iStart : Integer;
  iStop : Integer;
Begin
  Result := CursorInWord(sText, iStart, iStop);
End;

Function TWPRange.CursorInWord(Out sText : String; Out aSpell : TWPWorkingDocumentSpellCheckingState):Boolean;
Var
  iStart, iStop : Integer;
Begin
  Result := CursorInWord(sText, aSpell, iStart, iStop);
End;

Function TWPRange.CursorInWord(Out sText : String; Out iStart, iStop : Integer):Boolean;
Var
  aSpell : TWPWorkingDocumentSpellCheckingState;
Begin
  Result := CursorInWord(sText, aSpell, iStart, iStop);
End;

Function TWPRange.CursorInWord(Out sText : String; Out aSpell : TWPWorkingDocumentSpellCheckingState; Out iStart, iStop : Integer):Boolean;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  If Selection.HasSelection Then
    Begin
    sText := '';
    Result := True;
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.First;
      iStart := oIterator.Current.Metrics.Position;
      iStop := oIterator.Current.Metrics.Position + oIterator.Current.Metrics.OffsetCount;
      aSpell := TWPWorkingDocumentTextPiece(oIterator.Current).SpellState;
      While oIterator.More Do
        Begin
        If oIterator.Current.PieceType = ptText Then
          Begin
          If IsWordBreak(oIterator.Current.LogicalText) Then
            Result := False
          Else
            sText := sText + oIterator.Current.LogicalText;
          iStop := oIterator.Current.Metrics.Position + oIterator.Current.Metrics.OffsetCount;
          End
        Else
          Result := False;
        oIterator.Next;
        End;
      Result := Result And (sText <> '');
    Finally
      oIterator.Free;
    End;
    End
  Else
    Begin
    Result := Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex)
                 And ((iInternal > 0) And (oPiece.PieceType = ptText)) Or
                     ((iInternal = 0) And (iIndex > 0) And (Document.Pieces[iIndex - 1].PieceType = ptText));
    If Result Then
      Begin
      If iInternal = 0 Then
        Dec(iIndex);
      While (iIndex > 0) And (Document.Pieces[iIndex - 1].PieceType = ptText) And (Not IsWordBreak(Document.Pieces[iIndex - 1].LogicalText)) Do
        Dec(iIndex);
      sText := Document.Pieces[iIndex].LogicalText;
      iStart := Document.Pieces[iIndex].Metrics.Position;
      aSpell := TWPWorkingDocumentTextPiece(Document.Pieces[iIndex]).SpellState;
      iStop := Document.Pieces[iIndex].Metrics.Position + Document.Pieces[iIndex].Metrics.OffsetCount;
      While (iIndex < Document.Pieces.Count - 1) And (Document.Pieces[iIndex + 1].PieceType = ptText) And (Not IsWordBreak(Document.Pieces[iIndex + 1].LogicalText)) Do
        Begin
        Inc(iIndex);
        sText := sText + Document.Pieces[iIndex].LogicalText;
        iStop := Document.Pieces[iIndex].Metrics.Position + Document.Pieces[iIndex].Metrics.OffsetCount;
        End;
      End;
    End;
End;

Function TWPRange.SelectFieldByName(Const sNamespace, sName : String) : Boolean;
Var
  oIterator :  TWPPieceIterator;
  oField : TWPWorkingDocumentFieldStartPiece;
Begin
  LogAction('SelectFieldByName', sNamespace+'#'+sName);
  Result := False;
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := [ptFieldStart];
    oIterator.First;
    While oIterator.More And Not Result Do
    Begin
      oField := TWPWorkingDocumentFieldStartPiece(oIterator.Current);
      If ((sNamespace= '') Or StringEquals(oField.namespace, sName)) And StringEquals(oField.name, sName) Then
      Begin
        Result := True;
        Selection.MoveTo(oIterator.Current.Metrics.Position+1);
        SelectField(False);
        ChangeState;
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Function TWPRange.SelectSectionByName(Const sNamespace, sName : String) : Boolean;
Var
  oIterator :  TWPPieceIterator;
  oField : TWPWorkingDocumentSectionStartPiece;
Begin
  LogAction('SelectSectionByName', sNamespace+'#'+sName);
  Result := False;
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := [ptSectionStart];
    oIterator.First;
    While oIterator.More And Not Result Do
    Begin
      oField := TWPWorkingDocumentSectionStartPiece(oIterator.Current);
      If ((sNamespace= '') Or StringEquals(oField.namespace, sName)) And StringEquals(oField.name, sName) Then
      Begin
        Result := True;
        Selection.MoveTo(oIterator.Current.Metrics.Position+1);
        SelectSection(False);
        ChangeState;
      End;
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;

Function TWPRange.GetSelectionCapabilities: TWPCapabilities;
Begin
  If Selection.HasSelection Then
    Result := GetRangeCapabilities(Selection.SelStart, Selection.SelEnd)
  Else
    Result := [];
End;


Function TWPRange.CurrentFieldContent: String;
Var
  iStart, iStop : Integer;
Begin
  If GetFieldRange(False, iStart, iStop) Then
    Result := GetTextInRange(iStart, iStop)
  Else
    Result := '';
End;

Function TWPRange.SelectField(bOuter: Boolean): Boolean;
Var
  iStart, iStop : Integer;
Begin
  Result := GetFieldRange(False, iStart, iStop);
  LogAction('SelectField', Format('%d - %d', [iStart, iStop]), result);
  If Result Then
    Begin
    MoveTo(iStart, False);
    SelectTo(iStop, True);
    End;
End;

Function TWPRange.GetParaRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
var
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := HasCurrentParagraph;
  If Result Then
    Begin
    If bOuter Then
      iStop := CurrentParagraph.Metrics.Position + 1
    else
      iStop := CurrentParagraph.Metrics.Position;
    oPiece := CurrentParagraph.Prev;
    while (oPiece <> nil) and (oPiece.PieceType in INLINE_PIECE_TYPES) do
      oPiece := oPiece.Prev;
    if oPiece = nil then
      iStart := 0
    else
      iStart := oPiece.Next.Metrics.Position;
  end;
End;

Function TWPRange.GetFieldRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
Begin
  Result := HasCurrentFieldStart;
  If Result Then
    Begin
    If bOuter Then
      Begin
      iStart := CurrentFieldStart.Metrics.Position;
      iStop := CurrentFieldStop.Metrics.Position + CurrentFieldStop.Metrics.CharCount;
      End
    Else
      Begin
      iStart := CurrentFieldStart.Metrics.Position + CurrentFieldStart.Metrics.CharCount;
      iStop := CurrentFieldStop.Metrics.Position;
      End;
    End;
  If Not Result Then
  Begin
    Result := HasCurrentSectionStart And CurrentSectionStart.IsField;
    If Result Then
    Begin
      If bOuter Then
        Begin
        iStart := CurrentSectionStart.Metrics.Position;
        iStop := CurrentSectionStop.Metrics.Position + CurrentSectionStop.Metrics.CharCount;
        End
      Else
        Begin
        iStart := CurrentSectionStart.Metrics.Position + CurrentSectionStart.Metrics.CharCount;
        iStop := CurrentSectionStop.Metrics.Position - 1; // don't select last paragraph
        End;
    End;
  End;
End;


Function TWPRange.GetSectionRange(bOuter : Boolean; Out iStart, iStop : Integer) : Boolean;
Begin
  Result := HasCurrentSectionStart;
  If Result Then
    Begin
    If bOuter Then
      Begin
      iStart := CurrentSectionStart.Metrics.Position;
      iStop := CurrentSectionStop.Metrics.Position + CurrentSectionStop.Metrics.CharCount;
      End
    Else
      Begin
      iStart := CurrentSectionStart.Metrics.Position + CurrentSectionStart.Metrics.CharCount;
      iStop := CurrentSectionStop.Metrics.Position;
      End;
    End;
End;

Function TWPRange.SelectSection(bOuter: Boolean): Boolean;
Var
  iStart, iStop : Integer;
Begin
  Result := GetSectionRange(False, iStart, iStop);
  LogAction('SelectSection', Format('%d - %d', [iStart, iStop]), result);
  If Result Then
    Begin
    MoveTo(iStart, False);
    SelectTo(iStop, True);
    End;
End;

Function TWPRange.SelectWholeParagraphs: TWPWorkingDocumentPiece;
Var
  iStart : Integer;
  iStop : Integer;
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  IOffset : Integer;
Begin
  Result := Nil;

  If Selection.HasSelection Then
  Begin
    iStart := IntegerMax(Selection.SelStart, 0);
    iStop := Selection.SelEnd;
  End
  Else
  Begin
    iStart := IntegerMax(Selection.Cursor, 0);
    iStop := Selection.Cursor;
  End;

  If Document.GetPieceByPosition(iStart, oPiece, iOffset, iIndex) Then
  Begin
    If (oPiece.PieceType = ptPara) And (iIndex > 0) And (Document.Pieces[iIndex-1].PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak]) Then
    Begin
      Dec(iIndex);
      oPiece := Document.Pieces[iIndex];
    End;
    While (iIndex > 0) And (oPiece.PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak]) Do
    Begin
      Dec(iIndex);
      oPiece := Document.Pieces[iIndex];
    End;

    If (Not (oPiece.PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak]))
       And Assigned(oPiece.Next) And (oPiece.Next.PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak])
       And (oPiece.Metrics.Position < iStart) Then
      oPiece := oPiece.Next;

    If Document.GetPieceByPosition(iStop, Result, iOffset, iIndex) Then
    Begin
      While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
        Inc(iIndex);

      Result := FDocument.Pieces[iIndex];

      If Assigned(oPiece) Then
        Selection.Select(oPiece.Metrics.Position, Result.Metrics.Position+Result.Metrics.CharCount);
    End;
  End;
End;

Function TWPRange.ConvertParagraphsToTable(oLast : TWPWorkingDocumentPiece) : TWPWorkingDocumentPiece;
  Function InsertPiece(iIndex : Integer; aClass : TWPWorkingDocumentPieceClass) : TWPWorkingDocumentPiece;
  Var oPiece : TWPWorkingDocumentPiece;
  Begin
    oPiece := aClass.Create;
    Try
      ApplyBaseStyle(oPiece);
      FDocument.Pieces.Insert(iIndex, oPiece.Link);
      Result := oPiece;
    Finally
      oPiece.Free;
    End;
  End;
  Function InsertStopPiece(iIndex : Integer; aType : TWPWorkingDocumentStopType) : TWPWorkingDocumentPiece;
  Var oPiece : TWPWorkingDocumentPiece;
  Begin
    oPiece := TWPWorkingDocumentStopPiece.Create(aType);
    Try
      ApplyBaseStyle(oPiece);
      FDocument.Pieces.Insert(iIndex, oPiece.Link);
      Result := oPiece;
    Finally
      oPiece.Free;
    End;
  End;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  IOffset : Integer;
  iStart : Integer;
  oTable : TWPWorkingDocumentTableStartPiece;
Begin
  If Selection.hasSelection Then
    iStart := Selection.SelStart
  Else
    iStart := Selection.Cursor;
  If Not Document.GetPieceByPosition(iStart, oPiece, iOffset, iIndex) Then
    RaiseError('ConvertParagraphsToTable', 'Unable to find start');

  InsertPiece(iIndex, TWPWorkingDocumentTableCellStartPiece);
  InsertPiece(iIndex, TWPWorkingDocumentTableRowStartPiece);
  oTable := TWPWorkingDocumentTableStartPiece(InsertPiece(iIndex, TWPWorkingDocumentTableStartPiece));
  If (iIndex = 0) or ((iIndex > 0) and (Document.Pieces[iIndex-1].PieceType in [ptTableStop, ptSectionStart, ptSectionStop])) Then
  Begin
    InsertPiece(iIndex, TWPWorkingDocumentParaPiece);
    Inc(iIndex);
  End;
  While oPiece <> oLast Do
  Begin
    Inc(iIndex);
    If oPiece.PieceType = ptPara Then
    Begin
      InsertPiece(iIndex, TWPWorkingDocumentTableCellStartPiece);
      InsertPiece(iIndex, TWPWorkingDocumentTableRowStartPiece);
      InsertStopPiece(iIndex, stTableRow);
      InsertStopPiece(iIndex, stTableCell);
    End;
    oPiece := Document.Pieces[iIndex];
  End;
  iIndex := Document.Pieces.IndexByReference(oLast) + 1;
  If (iIndex >= Document.Pieces.Count - 1) or (Document.Pieces[iIndex].PieceType in [ptTableStart, ptSectionStart, ptSectionStop]) Then
  Begin
    Result := InsertPiece(iIndex, TWPWorkingDocumentParaPiece);
    InsertStopPiece(iIndex, stTable);
  End
  Else
    Result := InsertStopPiece(iIndex, stTable);

  InsertStopPiece(iIndex, stTableRow);
  InsertStopPiece(iIndex, stTableCell);
  ConfigureInsertedTable(oTable);
End;

Function TWPRange.CanMoveSelection(iDestination : Integer) : Boolean;
Begin
  Result := Selection.HasSelection And (CanDelete In GetSelectionCapabilities);
  // todo:
  //   check destination is not readonly
  //   check will not cause tables to nest
End;

Function TWPRange.MoveSelection(iDestination : Integer) : Boolean;
Var
  iNew : Integer;
Begin
  Result := Not Operator_.Settings.ReadOnly And CanMoveSelection(iDestination);
  LogAction('MoveSelection', Format('%d', [iDestination]));
  If Result Then
    Begin
    Operator_.StartOperation(FId, otMove, False, Selection);
    Operator_.MoveContent(iDestination);
    iNew := Operator_.EndOfMovedRange;
    Operator_.FinishOperation;
    ChangeContent;
    MoveTo(iNew);
    End;
End;


Function TWPRange.SelectedText: String;
Var
  oBuffer : TFslBuffer;
  oDocument : TWPWorkingDocument;
  canCopyAsText : Boolean;
  iLoop : Integer;
Begin
  Result := '';
  If canWrite In Capabilities Then
  Begin
    oDocument := TWPWorkingDocument.Create;
    Try
      oDocument.AllAnnotations.Assign(FDocument.AllAnnotations);
      PrepareBalancedFragment(oDocument);

      canCopyAsText := False;
      iLoop := 0;
      While (iLoop <= oDocument.Pieces.Count - 1) And (Not canCopyAsText) Do
      Begin
        canCopyAsText := canCopyAsText Or (oDocument.Pieces[iLoop].CanCopyAsText);
        iLoop := iLoop + 1;
      End;

      If canCopyAsText Then
      Begin
        oBuffer := TFslBuffer.Create;
        Try
          WriteRange(TWPTextWriter, oDocument, oBuffer, -1);
          Result := oBuffer.AsText;
        Finally
          oBuffer.Free;
        End;
      End;
    Finally
     oDocument.Free;
    End;
  End;
End;


Function TWPRange.TextByRange(Const iStart, iStop: Integer): String;
Var
  oBuffer : TFslBuffer;
  oDocument : TWPWorkingDocument;
  canCopyAsText : Boolean;
  iLoop : Integer;
Begin
  Result := '';
  If canWrite In Capabilities Then
  Begin
    oDocument := TWPWorkingDocument.Create;
    Try
      PrepareBalancedFragment(oDocument, iStart, iStop);

      canCopyAsText := False;
      iLoop := 0;
      While (iLoop <= oDocument.Pieces.Count - 1) And (Not canCopyAsText) Do
      Begin
        canCopyAsText := canCopyAsText Or (oDocument.Pieces[iLoop].CanCopyAsText);
        iLoop := iLoop + 1;
      End;

      If canCopyAsText Then
      Begin
        oBuffer := TFslBuffer.Create;
        Try
          WriteRange(TWPTextWriter, oDocument, oBuffer, -1);
          Result := oBuffer.AsText;
        Finally
          oBuffer.Free;
        End;
      End;
    Finally
     oDocument.Free;
    End;
  End;
End;


Function TWPRange.ContextIsReadOnly : Boolean;
Begin
  If HasCurrentFieldStart Then
    Result := (CurrentFieldStart.IsReadOnly And (CurrentFieldStart.ReadOnly <> tsFalse)) or (CurrentFieldStart.HasDefinitionProvider and not CurrentFieldStart.DefinitionProvider.UserCanEditTextInField(CurrentFieldStart.DocField))
  Else If Operator_.Settings.FormsMode Then
    Result := True
  Else If HasCurrentTableCellStart Then
    Result := CurrentTableCellStart.IsReadOnly
  Else If HasCurrentTableRowStart Then
    Result := CurrentTableRowStart.IsReadOnly
  Else If HasCurrentTableStart Then
    Result := CurrentTableStart.IsReadOnly
  Else If HasCurrentSectionStart Then
    Result := CurrentSectionStart.IsReadOnly
  Else
    Result := False;
End;


Function TWPRange.TrimSelectionStart(iCursor : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iOffset : Integer;
Begin
  If Document.GetPieceByPosition(iCursor, oPiece, iOffset, iIndex) Then
  Begin
    While Document.Pieces.ExistsByIndex(iIndex + 1) And (Document.Pieces[iIndex].PieceType In [ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop]) Do
    Begin
      iIndex := iIndex + 1;
      iOffset := 0;
    End;
    Result := Document.Pieces[iIndex].Metrics.Position + iOffset;
  End
  Else
    Result := iCursor;
End;

Function TWPRange.TrimSelectionEnd(iCursor : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iOffset : Integer;
Begin
  If Document.GetPieceByPosition(iCursor, oPiece, iOffset, iIndex) Then
  Begin
    If (iOffset = 0) Then
    Begin
      iIndex := iIndex - 1;
      iOffset := Document.Pieces[iIndex].Metrics.CharCount;
    End;
    
    While Document.Pieces.ExistsByIndex(iIndex - 1) And (Document.Pieces[iIndex].PieceType In [ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop]) Do
    Begin
      iIndex := iIndex -1;
      iOffset := Document.Pieces[iIndex].Metrics.CharCount;
    End;
    Result := Document.Pieces[iIndex].Metrics.Position + iOffset;
  End
  Else
    Result := iCursor;
End;



Procedure TWPRange.AddPriorPieces(oSource, oDest : TWPWorkingDocument; iPosition : Integer; aUntil : TWPWorkingDocumentPieceType; aInclude : TWPWorkingDocumentPieceTypes;
                                  Var aAdded : TWPWorkingDocumentPieceTypes; Var aBalanced : TPieceTypeCount);
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
  bFound : Boolean;
Begin
  bFound := False;
  iLoop := iPosition-1;
  While Not bFound And (iLoop >= 0) Do
  Begin
    oPiece := oSource.Pieces[iLoop];
    If (oPiece.PieceType In aInclude) Or (oPiece.PieceType = aUntil) Then
    Begin
      Include(aAdded, oPiece.PieceType);
      Case oPiece.PieceType Of
        ptTableStart : Inc(aBalanced[ptTableStart]);
        ptRowStart : Inc(aBalanced[ptRowStart]);
        ptCellStart : Inc(aBalanced[ptCellStart]);
        ptTableStop : Dec(aBalanced[ptTableStart]);
        ptRowStop : Dec(aBalanced[ptRowStart]);
        ptCellStop : Dec(aBalanced[ptCellStart]);
        ptSectionStart : Inc(aBalanced[ptSectionStart]);
        ptSectionStop : Dec(aBalanced[ptSectionStart]);
        ptFieldStart : Inc(aBalanced[ptFieldStart]);
        ptFieldStop : Dec(aBalanced[ptFieldStart]);
      End;
      If oPiece.PieceType In aInclude Then
        oDest.Pieces.Insert(0, oPiece.Clone)
      Else If oPiece.PieceType = aUntil Then
      Begin
        bFound := True;
        oDest.Pieces.Insert(0, oPiece.Clone)
      End;
    End;
    Dec(iLoop);
  End;
  If Not bFound Then
    RaiseError('AddPriorToPieces', 'Unable to find '+NAMES_WPPIECETYPE[aUntil]+' writing oRange');
End;


Procedure TWPRange.AddPostPieces(oSource, oDest : TWPWorkingDocument; iPosition : Integer; aUntil : TWPWorkingDocumentPieceType; aInclude : TWPWorkingDocumentPieceTypes);
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
  bFound : Boolean;
Begin
  bFound := False;
  iLoop := iPosition;
  While Not bFound And (iLoop < oSource.Pieces.Count) Do
  Begin
    oPiece := oSource.Pieces[iLoop];
    If oPiece.PieceType In aInclude Then
      oDest.Pieces.Add(oPiece.Clone)
    Else If oPiece.PieceType = aUntil Then
    Begin
      bFound := True;
      oDest.Pieces.Add(oPiece.Clone)
    End;
    Inc(iLoop);
  End;
  If Not bFound Then
    RaiseError('AddPostPieces', 'Unable to find '+NAMES_WPPIECETYPE[aUntil]+' writing oRange');
End;


Procedure TWPRange.InsertMissingParagraphs(oDocument : TWPWorkingDocument);
Var
  iLoop : Integer;
  oPara : TWPWorkingDocumentParaPiece;
  oPiece : TWPWorkingDocumentPiece;
Begin
  For iLoop := oDocument.Pieces.Count - 1 DownTo 1 Do
  Begin
    oPiece := oDocument.Pieces[iLoop];
    If (oPiece.PieceType = ptCellStop) And (oDocument.Pieces[iLoop - 1].PieceType = ptCellStart) Then
      Begin
      oPara := TWPWorkingDocumentParaPiece.Create;
      Try
        oPara.Style := oPiece.Style;
        oPara.Font.Assign(oPiece.Font);
        oDocument.Pieces.Insert(iLoop, oPara.Link);
      Finally
        oPara.Free;
      End;
      End;
  End;
End;


Type
  TFormatStatus = (fsYes, fsIfFieldCloses, fsNo);

Function TWPRange.GetRangeCapabilities(iStart, iEnd : Integer): TWPCapabilities;
Var
  oIterator : TWPPieceIterator;
  bInField : Boolean;
  bInSectionField : Boolean;
  bCanDelete : Boolean;
  bCanWrite : Boolean;
  aFormat : TFormatStatus;
  bSawField : Boolean;
  bSawSectionField : Boolean;
  bCanAnnotate : Boolean;
  field : TWPWorkingDocumentFieldStartPiece;
  section : TWPWorkingDocumentSectionStartPiece;
Begin
  bCanDelete := True;
  bCanWrite := True;
  aFormat := fsYes;
  bSawField := False;
  bSawSectionField := False;
  bCanAnnotate := True;

  field := getApplicableField(iStart);
  if (field <> nil) then
  begin
    if (field.HasDefinitionProvider) and not field.DefinitionProvider.UserCanEditTextInField(field.DocField) then
      bCanWrite := false;
    field := nil;
  end;

  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.SetBounds(iStart, iEnd);
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.First;
    bInField := False;
    bInSectionField := False;
    While oIterator.More And (bCanDelete Or bCanWrite Or (aFormat <> fsNo)) Do
      Begin
      If oIterator.Current.IsReadOnly Then
      Begin
        bCanDelete := False;
        aFormat := fsNo;
      End;

      Case oIterator.Current.PieceType Of
        ptFieldStart :
          Begin
          field := TWPWorkingDocumentFieldStartPiece(oIterator.Current);
          bSawField := True;
          If Operator_.Settings.FormsMode Then
          Begin
            bCanDelete := False;
            bCanWrite := False;
            aFormat := fsNo;
          End;
          bInField := True;
          If Not (Operator_.Settings.FieldWrappers In [wpfpNone, wpfpInvisible]) Then
          Begin
            bCanAnnotate := False;
            bCanDelete := bCanDelete And (field.Deletable Or (field.HasDefinitionProvider And field.DefinitionProvider.UserCanDeleteField(field.DocField)));
            case field.FixedFormat of
              fffWholeField : if (aFormat <> fsNo) then aFormat := fsIfFieldCloses;
              fffFixed : aFormat := fsNo;
            end;
            End;
          End;
        ptFieldStop :
          Begin
          field := TWPWorkingDocumentFieldStopPiece(oIterator.Current).MatchingStart;
          bSawField := True;
          If Operator_.Settings.FormsMode Then
          Begin
            bCanDelete := False;
            bCanWrite := False;
            aFormat := fsNo;
          End;
          If Not bInField Then
          Begin
            bCanDelete := False;
            bCanWrite := False;
            bCanAnnotate := False;
            if field.FixedFormat <> fffAnyPart then
              aFormat := fsNo;
          End
          Else if field.FixedFormat = fffFixed then
            aFormat := fsNo
          else if aFormat = fsIfFieldCloses then
            aFormat := fsYes;
          bInField := False;
          End;
        ptSectionStart :
          Begin
            section := TWPWorkingDocumentSectionStartPiece(oIterator.Current);
            if section.IsField then
            begin
              bSawSectionField := True;
              If Operator_.Settings.FormsMode Then
              Begin
                bCanDelete := False;
                bCanWrite := False;
                aFormat := fsNo;
              End;
              bInSectionField := True;
              bCanAnnotate := False;
              If Not (Operator_.Settings.FieldWrappers In [wpfpNone, wpfpInvisible]) Then
                bCanDelete := bCanDelete And (section.Deletable Or (section.HasDefinitionProvider And section.DefinitionProvider.UserCanDeleteField(section.DocSection)));
            end
            else
            begin
              bCanDelete := False; // can never delete sections - they are controlled outside the scope of the word processor
              bCanAnnotate := False;
            end;
          End;

        ptSectionStop :
          Begin
            section := TWPWorkingDocumentSectionStartPiece(TWPWorkingDocumentStopPiece(oIterator.Current).MatchingStart);
            if section.IsField then
            begin
              bSawSectionField := True;
              If Operator_.Settings.FormsMode Then
              Begin
                bCanDelete := False;
                bCanWrite := False;
                aFormat := fsNo;
              End;
              If Not bInSectionField Then
              Begin
                bCanDelete := False;
                bCanWrite := False;
                bCanAnnotate := False;
              End;
              bInSectionField := False;
            end
            else
            begin
              bCanDelete := False; // can never delete sections - they are controlled outside the scope of the word processor
              bCanAnnotate := False;
            end;
          End;

        ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop :
          Begin
          bCanDelete := False; // not yet
          bCanAnnotate := False;
          End;
      Else
        // ptText, ptImage, ptLineBreak, ptPara are all ok, they can be deleted
        bCanAnnotate := bCanAnnotate And (oIterator.Current.PieceType <> ptPara);
      End;
      bCanAnnotate := bCanAnnotate And (oIterator.Current.AnnotationId = 0);
      oIterator.Next;
      End;
    bCanDelete := bCanDelete And Not (bInField or bInSectionField) And Not IsProtectedPara(iEnd);
    bCanWrite := bCanWrite And Not (bInField or bInSectionField);
    bCanAnnotate := bCanAnnotate And Not (bInField or bInSectionField);
  Finally
    oIterator.Free;
  End;
  If Not (bSawField or bSawSectionField) And Operator_.Settings.FormsMode And Not HasCurrentFieldStart Then
  Begin
    bCanDelete := False;
    bCanWrite := False;
    aFormat := fsNo;
  End;

  Result := [];
  If bCanAnnotate Then
    Include(Result, canInsertAnnotation);
  If bCanDelete Then
    Include(Result, canDelete);
  If bCanWrite Then
    Include(Result, canWrite);
  If aFormat = fsYes Then
    Include(Result, canFormat);
End;


Function TWPRange.GetTextInRange(iStart, iStop : Integer) : String;
Var
  oIterator : TWPCharIterator;
Begin
  If iStart = iStop Then
    Result := ''
  Else
  Begin
    oIterator := TWPCharIterator.Create(Document.Link);
    Try
      oIterator.SetBounds(iStart, iStop);
      Result := '';
      oIterator.First;
      While oIterator.More Do
        Begin
        Result := Result + oIterator.Current;
        oIterator.Next;
        End;
    Finally
      oIterator.Free;
    End;
  End;
End;


Procedure TWPRange.ConfigureInsertedTable(oTable : TWPWorkingDocumentTableStartPiece);
Begin
  oTable.BorderPolicy := tbpGrid;
  oTable.HorizontalMargin := 0;
  oTable.VerticalMargin := 0;
  oTable.ApplyPolicy;
End;



Function TWPRange.IsProtectedPara(iPosition : Integer):Boolean;
Var
  iIndex : Integer;
  iOffset : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := Document.GetPieceByPosition(iPosition, oPiece, iIndex, iOffset);
  If Result Then
  Begin
    If iOffset > 0 Then
    Begin
      If oPiece.HasPrev Then
        oPiece := oPiece.Prev
      Else
        oPiece := Nil;
    End;

    If Assigned(oPiece) Then
    Begin
      // you can't delete the paragraph if there's a break following it - unless it's empty
      If oPiece.PieceType = ptPara Then
      Begin
        If (oPiece.HasPrev) And (oPiece.Prev.PieceType = ptPara) Then
          Result := False
        Else
          Result := Not oPiece.HasNext Or (oPiece.Next.PieceType = ptBreak);
      End
      Else
        Result := False;
    End;
  End;
End;


Procedure TWPRange.ReadParagraph;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  oLast : TWPWorkingDocumentParaPiece;
  oCurrent : TWPWorkingDocumentParaPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Paragraph.Clear;
  If Selection.HasSelection Then
    Begin
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.First;
      If oIterator.More Then
        Begin
        oLast := oIterator.CurrentParagraph(True);
        If Assigned(oLast) Then
          Paragraph.Assign(oLast.Format);
        While oIterator.More Do
          Begin
          oCurrent := oIterator.CurrentParagraph(True);
          If oLast <> oCurrent Then
            Begin
            oLast := oCurrent;
            If Assigned(oLast) Then
              Paragraph.Merge(oLast.Format);
            End;
          oIterator.Next;
          End;
        End
    Finally
      oIterator.Free;
    End;
    End
  Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
    Begin
    While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
      Inc(iIndex);
    If iIndex < FDocument.Pieces.count Then
      Paragraph.Assign(TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]).Format);
    End;
End;


Procedure TWPRange.ReadFont;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  bBack : Boolean;
  oStyle : TWPStyle;
Begin
  Font.Clear;
  If Selection.HasSelection Then
    Begin
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.First;
      If oIterator.More Then
        Begin
        Font.Assign(oIterator.Current.Font);
        While oIterator.More Do
          Begin
          Font.Merge(oIterator.Current.Font);
          oIterator.Next;
          End;
        End;
    Finally
      oIterator.Free;
    End;
    End
  Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
  Begin
    Font.Assign(Operator_.Styles.DefaultStyle.Font);
    If (iInternal = 0) And (iIndex > 0) And (Document.Pieces[iIndex - 1].PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptPara]) Then
    Begin
      bBack := True;
      If (Document.Pieces[iIndex - 1].PieceType = ptPara) Then
      Begin
        oStyle := Operator_.Styles.GetByName(Document.Pieces[iIndex - 1].Style);
        bBack := (oStyle = Nil) Or Not oStyle.ResetOnNewParagraph;
      End;
      If bBack Then
        oPiece := Document.Pieces[iIndex - 1];
    End;

    Font.Assign(oPiece.Font);
  End;
End;

Function TWPRange.ReadStyle : String;
Var
  oIterator : TWPPieceIterator;
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  bBack : Boolean;
  oStyle : TWPStyle;
Begin
  If Selection.HasSelection Then
    Begin
    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.First;
      If oIterator.More Then
        Begin
        Result := oIterator.Current.Style;
        While oIterator.More And (Result <> '') Do
          Begin
          If oIterator.Current.Style <> Result Then
            Result := '';
          oIterator.Next;
          End;
        End;
    Finally
      oIterator.Free;
    End;
    End
  Else
  Begin
    If (Operator_.Version = FLastStyleVersion) And (FSelection.Summary = FLastStyleSummary) Then
      Result := FStyle
    Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
    Begin
      Result := Operator_.Styles.DefaultStyle.Name;
      If (iInternal = 0) And (iIndex > 0) And (Document.Pieces[iIndex - 1].PieceType In [ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak]) Then
      Begin
        bBack := True;
        If (Document.Pieces[iIndex - 1].PieceType = ptPara) Then
        Begin
          oStyle := Operator_.Styles.GetByName(Document.Pieces[iIndex - 1].Style);
          bBack := (oStyle = Nil) Or Not oStyle.ResetOnNewParagraph;
        End;
        If bBack Then
          oPiece := Document.Pieces[iIndex - 1];
      End;

      Result := oPiece.Style;
      FLastStyleVersion := Operator_.Version;
      FLastStyleSummary := FSelection.Summary;
    End
    Else
      Result := '';
  End;
End;


  // todo - limit by scope, and undo/redo
Function TWPRange.ClearFormatting : Boolean;
Var
  oIterator : TWPPieceIterator;
Begin
  oIterator := TWPPieceIterator.Create(Document.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    Result := True;
    oIterator.First;
    While oIterator.More Do
    Begin
      Result := Result And (oIterator.Current.PieceType In [ptText, ptPara]);
      oIterator.Next;
    End;
  Finally
     oIterator.Free;
  End;

  If Result Then
    Begin
    Operator_.ClearUndo;

    oIterator := TWPPieceIterator.Create(Document.Link);
    Try
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      Result := True;
      oIterator.First;
      While oIterator.More Do
      Begin
        oIterator.Current.Style := DEFAULT_STYLE_NAME;
        oIterator.Current.Font.Assign(Operator_.Styles.DefaultStyle.Font);
        oIterator.Current.Change(ctLayout, oIterator.Current);
        oIterator.Next;
      End;
    Finally
       oIterator.Free;
    End;
    ChangeContent;
    End;
End;



Procedure TWPRange.CheckSpelling(sWord : String = '');
Var
  iLoop : Integer;
  iIndex : Integer;
  iInner : Integer;
  bCheck : Boolean;
  sText : String;
  oPiece : TWPWorkingDocumentPiece;
  aState : TWPWorkingDocumentSpellCheckingState;
  iStart : Integer;
  iStop : Integer;
  aFieldValidate : TFieldSpellingCheckState;
  oField : TWPWorkingDocumentFieldStartPiece;
  oList : TWPWorkingDocumentPieces;
  sHint : String;
  iList : Integer;
  sError : String;
Begin
  iLoop := 0;
  sWord := StringReplace(sWord, CHAR_SPELL_EXCLUDE, '');
  oList := Nil;
  oField := Nil;

  If Operator_.HasSpeller And Not Operator_.Settings.Readonly Then
  Begin
    aFieldValidate := fscsSpelling;
    While iLoop < Document.Pieces.Count Do
    Begin
      oPiece := Document.Pieces[iLoop];
      If oPiece.PieceType = ptFieldStart Then
      Begin
        sHint := '';

        oField := TWPWorkingDocumentFieldStartPiece(oPiece);
        If oField.HasDefinitionProvider Then
        Begin
          aFieldValidate := oField.DefinitionProvider.ShouldCheckSpelling(oField.DocField);
          sHint := oField.DefinitionProvider.HintForField(oField.DocField, fhmError);
        End
        Else
          aFieldValidate := fscsNone;
        oList := TWPWorkingDocumentPieces.Create;
        oList.Hooking := False;
        If not (aFieldValidate In [fscsField, fscsFieldSpelling]) Then
          oField.InError := False;
        oField.FieldHint := sHint;
      End
      Else If oPiece.PieceType = ptFieldStop Then
      Begin
        sText := '';
        For iList := 0 To oList.Count - 1 Do
          sText := sText + oList[iList].LogicalText;
        If aFieldValidate In [fscsField, fscsFieldSpelling] Then
        Begin
          oPiece.FieldHint := sHint;
          bCheck := True;

          If (bCheck) Then
          Begin
            oField.InError := Not oField.DefinitionProvider.CheckContent(oField.DocField, sText, sError);
            oPiece.Change(ctPresentation, oPiece);
            If (aFieldValidate = fscsField) And oField.InError Then
            begin
              aState := scsFieldWrong;
              oField.FieldError := sError;
              oPiece.FieldError := sError;
            end
            Else
              aState := scsOK;
            If (aFieldValidate = fscsField) Or oField.InError Then
              For iList := 0 To oList.Count - 1 Do
              begin
                TWPWorkingDocumentTextPiece(oList[iList]).SpellState := aState;
                TWPWorkingDocumentTextPiece(oList[iList]).FieldError := sError;
              end;
          End;
          sWord := sText;
          sHint := '';
        End;
        if assigned(TWordProcessor(Owner).OnUpdateField) and (oField.LastUpdateValue <> sText) then
        begin
          oField.LastUpdateValue := sText;
          if oField.DefinitionProvider <> nil then
            TWordProcessor(Owner).OnUpdateField(TWordProcessor(Owner), fusChange, oField.DefinitionProvider, oField.DocField, oField.DefinitionProvider.PreparePublicValue(oField.DocField, sText), not oField.InError)
          else
          TWordProcessor(Owner).OnUpdateField(TWordProcessor(Owner), fusChange, oField.DefinitionProvider, oField.DocField, sText, not oField.InError);
        end;
        oField := Nil;
        aFieldValidate := fscsSpelling;
        oList.Free;
        oList := Nil;
      End
      Else If (oPiece.PieceType = ptText) Then
      Begin
        If oList <> nil Then
          oList.Add(oPiece.Link);
        oPiece.FieldHint := sHint;
        If (aFieldValidate In [fscsSpelling, fscsFieldSpelling]) And ((TWPWorkingDocumentTextPiece(oPiece).SpellState = scsNotChecked) Or (sWord <> '')) Then
        Begin
          iIndex := iLoop;
          While (iIndex < Document.Pieces.Count) And (Document.Pieces[iIndex].pieceType = ptText)
              And Not (TWPWorkingDocumentTextPiece(Document.Pieces[iIndex]).SpellState = scsExempt)
              And Not IsWordBreak((TWPWorkingDocumentTextPiece(Document.Pieces[iIndex]).Content))
              And (TWPWorkingDocumentTextPiece(Document.Pieces[iIndex]).DrawnFont = '') Do
            Inc(iIndex);

          If iIndex > iLoop Then
          Begin
            bCheck := False;
            sText := '';
            For iInner := iLoop To iIndex - 1 Do
            Begin
              sText := sText + (TWPWorkingDocumentTextPiece(Document.Pieces[iInner]).Content);
              bCheck := bCheck Or (TWPWorkingDocumentTextPiece(Document.Pieces[iInner]).SpellState = scsNotChecked);
            End;

            iStart := Document.Pieces[iLoop].Metrics.Position;
            iStop := Document.Pieces[iIndex - 1].Metrics.Position + Document.Pieces[iIndex - 1].Metrics.CharCount;
            sText := StringReplace(sText, CHAR_SPELL_EXCLUDE, '');

            If (StringEquals(sWord, sText) Or (bCheck And (Selection.HasSelection Or ((iStart > Selection.Cursor) Or (iStop < Selection.Cursor))))) Then
            Begin
              If (sText = '') Or (isWordBreak(sText)) Or (StringFind(sText, CHAR_SPELL_EXEMPT) > 0) Then
                aState := scsExempt
              Else
                aState := Operator_.Speller.CheckSpelling(sText);

              For iInner := iLoop To iIndex - 1 Do
                TWPWorkingDocumentTextPiece(Document.Pieces[iInner]).SpellState := aState;
            End;

            iLoop := iIndex -1;
          End;
        End;
      End;
      Inc(iLoop);
    End;

    If sWord <> '' Then
      ChangeState;
  End;
End;


Procedure TWPRange.ChangeContent;
Begin
  If HasDocument And Assigned(FOnContentChange) Then
    FOnContentChange(Self);
End;

Procedure TWPRange.ChangeState(bDraw: Boolean);
Begin
  FStyle := ReadStyle;
  ReadFont;
  ReadParagraph;
  UpdateCurrentStatus;
  FCapabilities := GetCapabilities;
  ChangeReadyState;
End;

Procedure TWPRange.ChangeReadyState;
Begin
  If Assigned(FOnSelectionChange) Then
    FOnSelectionChange(Self)
End;

Function TWPRange.HasDocument: Boolean;
Begin
  Result := Assigned(FDocument);
End;

Function TWPRange.CanRedo_: Boolean;
Begin
  Result := Operator_.CanRedo(FId);
End;

Function TWPRange.CanUndo_: Boolean;
Begin
  Result := Operator_.CanUndo(FId);
End;


Function TWPRange.ApplyChangeCase(aCaseType: TWPChangeCaseType): Boolean;
Var
  oIterator : TWPPieceIterator;
  oTextPiece: TWPWorkingDocumentTextPiece;
  bFirstText: Boolean;
  bStartSentence: Boolean;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canWrite In Capabilities) And Selection.HasSelection;
  If Result Then
    Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.CleanRange;
    oIterator := TWPPieceIterator.Create(FDocument.Link);
    Try
      bFirstText := True;
      bStartSentence := False;
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.First;
      While oIterator.More And Result Do
      Begin
        If (oIterator.Current.PieceType <> ptText) Then
          bStartSentence := True
        Else
          Begin
          oTextPiece := TWPWorkingDocumentTextPiece(oIterator.Current);
          Case aCaseType Of
            cctUpperCase:         oTextPiece.Content := StringUpper(oTextPiece.Content);
            cctLowerCase:         oTextPiece.Content := Lowercase(oTextPiece.Content);
            cctTitleCase:         oTextPiece.Content := StringTitleCase(oTextPiece.Content);
            cctToggleCase:        oTextPiece.Content := StringToggleCase(oTextPiece.Content);
            cctSentenceCase:      // can't use StringSentence as sentence may be splitted
              Begin
                // checking if the first text piece is also start of a sentence
                If bFirstText And (Not bStartSentence) Then
                  bStartSentence := IsStartOfSentence(oTextPiece);

                // apply case
                If Not bStartSentence Then
                  oTextPiece.Content := lowerCase(oTextPiece.Content)
                Else
                  oTextPiece.Content := StringSentence(oTextPiece.Content);

                // is this text piece end of current sentence
                If StringTrimWhitespace(oTextPiece.Content) <> '' Then
                  bStartSentence := StringEndsWith(oTextPiece.Content, '.');
              End;
          End;
          oTextPiece.Change(ctTextContents, oTextPiece);
          bFirstText := False;
          End;
        oIterator.Next;
      End;
    Finally
      oIterator.Free;
    End;
    Operator_.FinishOperation;
    ChangeContent;
    ChangeState;
    End
End;

Function TWPRange.IsStartOfSentence(oText: TWPWorkingDocumentTextPiece): Boolean;
Var
  oPrev: TWPWorkingDocumentPiece;
  sText: String;
Begin
  If (oText.Prev = Nil) Then
    Result := True
  Else
    Begin
      Result := False;
      oPrev := oText.Prev;
      While (oPrev <> Nil) And (Not Result) Do
      Begin
        If (oPrev.PieceType <> ptText) Then
          Result := True
        Else
          Begin
            sText := StringTrimWhitespace(TWPWorkingDocumentTextPiece(oPrev).Content);
            Result := StringEndsWith(sText, '.');
            If sText <> '' Then
              Break;
            oPrev := oPrev.Prev;
          End;
      End;
    End;
End;

Procedure TWPRange.Consume;
Begin
  TWordProcessor(FOwner).ConsumeRange(Self);
End;

Procedure TWPRange.CutOff;
Begin
  Operator_ := Nil;
  Document := Nil;
End;

Function TWPRange.DoInsertCopiedRow(oDocument: TWPWorkingDocument; oStyles: TWPStyles; iPos: Integer): Boolean;
Begin
  Operator_.StartOperation(FId, otInsert, False, Selection);
  Operator_.SetRange(iPos, iPos);
  Operator_.CleanRange;

  // do the insert
  DoInsertCopiedRow(oDocument);

  CurrentTableStart.StructureDirty := True;
  Document.RegenerateMetrics(False, False);
  Operator_.FinishOperation;
  Operator_.Styles.MergeStyles(oStyles, False);
  ChangeContent;
  DoMove(iPos, cdJump);

  Result := True;
End;

Procedure TWPRange.DoInsertCopiedRow(oDocument: TWPWorkingDocument);
Var
  oStartRow : TWPWorkingDocumentTableRowStartPiece;
  oStartCell : TWPWorkingDocumentTableCellStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  iLoop : Integer;
  oPara : TWPWorkingDocumentParaPiece;
Begin
  // start row
  oStartRow := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ApplyBaseStyle(oStartRow);
    Operator_.AppendPiece(oStartRow.Link);
  Finally
    oStartRow.Free;
  End;

  // start cell
  oStartCell := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ApplyBaseStyle(oStartCell);
    Operator_.AppendPiece(oStartCell.Link);
  Finally
    oStartCell.Free;
  End;

  // cell content
  For iLoop := 0 To oDocument.Pieces.Count - 1 Do
    Operator_.AppendPiece(oDocument.Pieces[iLoop].Link);
  If oDocument.Pieces[oDocument.Pieces.Count - 1].PieceType <> ptPara Then
  Begin
    oPara := TWPWorkingDocumentParaPiece.Create;
    Try
      ApplyBaseStyle(oPara);
      Operator_.AppendPiece(oPara.Link);
    Finally
      oPara.Free;
    End;
  End;

  // end cell
  oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
  Try
    ApplyBaseStyle(oStop);
    Operator_.AppendPiece(oStop.Link);
  Finally
    oStop.Free;
  End;

  // end row
  oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
  Try
    ApplyBaseStyle(oStop);
    Operator_.AppendPiece(oStop.Link);
  Finally
    oStop.Free;
  End;
End;

Function TWPRange.DoInsertCopiedRows(oDocument: TWPWorkingDocument; oStyles: TWPStyles; iPos: Integer): Boolean;
Var
  iIndex: Integer;
Begin
  Operator_.StartOperation(FId, otInsert, False, Selection);
  Operator_.SetRange(iPos, iPos);
  Operator_.CleanRange;

  // do the insert
  iIndex := Operator_.DirectAppendIndex;
  DoInsertCopiedRows(oDocument, iIndex);
  Operator_.EndDirectAppend;

  CurrentTableStart.StructureDirty := True;
  Document.RegenerateMetrics(False, False);
  Operator_.FinishOperation;
  Operator_.Styles.MergeStyles(oStyles, False);
  ChangeContent;
  DoMove(iPos, cdJump);

  Result := True;
End;


Procedure TWPRange.DoInsertCopiedRows(oDocument: TWPWorkingDocument; iIndex : Integer);
Var
  oPiece: TWPWorkingDocumentPiece;
  iLoop : Integer;
  bInRow: Boolean;
Begin
  bInRow := False;
  For iLoop := 0 To oDocument.Pieces.Count -1 Do
  Begin
    oPiece := oDocument.Pieces[iLoop];
    If (oPiece.PieceType = ptRowStart) And (bInRow = False) Then
      bInRow := True;
    If bInRow Then
    Begin
      iIndex := Operator_.DirectAppendPiece(oPiece.Link, iIndex);
      If oPiece.PieceType = ptRowStop Then
        bInRow := False;
    End;
  End;
End;

Function TWPRange.CheckNoColumnsToInsert(oDocument: TWPWorkingDocument; iColumnCount: Integer; Var sError: String): Boolean;
Var
  iLoop: Integer;
  iCount : Integer;
Begin
  Result := (oDocument.Pieces.Count > 2) And (oDocument.Pieces[0].PieceType = ptTableStart)
        And (oDocument.Pieces[oDocument.Pieces.Count - 1].PieceType = ptTableStop);
  If Not Result Then
    sError := 'Clipboard data is not a table'
  Else
  Begin
    iCount := 0;
    iLoop := 1;
    While (Result) And (iLoop < oDocument.Pieces.Count) Do
    Begin
      If oDocument.Pieces[iLoop].PieceType = ptTableStart Then
      Begin
        Result := False;
        sError := 'More than 1 table is found in the clipboard';
      End;

      If oDocument.Pieces[iLoop].PieceType = ptRowStart Then
        iCount := 0
      Else If oDocument.Pieces[iLoop].PieceType = ptRowStop Then
      Begin
        Result := iColumnCount = iCount;
        If Not Result Then
          sError := 'Jagged table found where jagged is not allowed';
      End
      Else If oDocument.Pieces[iLoop].PieceType = ptCellStart Then
        Inc(iCount);

      Inc(iLoop);
    End;
  End;
End;

Function TWPRange.CanTableBeSorted(oTable: TWPWorkingDocumentTableStartPiece): Boolean;
Var
  iLoop: Integer;
Begin
  Result := Not oTable.Jagged;
  If Result Then
  Begin
    For iLoop := 0 To oTable.Rows.Count -1 Do
      If oTable.Rows[iLoop].IsReadOnly Then
      Begin
        Result := False;
        Break;
      End;
  End;
End;

Function TWPRange.ReorderTableRows(Const aSortedPos: Array Of Integer): Boolean;
Var
  iLoop: Integer;
  iStart, iStop: Integer;
  iNewPos: Integer;
  oPieces: TWPWorkingDocumentPieces;
Begin
  LogAction('ReorderTableRows', '');
  Result := Not Operator_.Settings.ReadOnly And Not Operator_.Settings.ConsoleMode;
  If Result Then
    Begin
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.SetRange(SelectedTable.Metrics.Position,
                    SelectedTableStop.Metrics.Position + SelectedTableStop.Metrics.CharCount);
    iNewPos := SelectedTable.Metrics.Position;
    iStart := Document.Pieces.IndexByReference(SelectedTable);
    iStop := Document.Pieces.IndexByReference(SelectedTableStop);

    oPieces := TWPWorkingDocumentPieces.Create(False);
    Try
     // retrieve pieces from table, but retrieve rows in the sorted order
     DoReorderRows(SelectedTable.Rows, aSortedPos, iStart, iStop, oPieces);
     
    // delete the table pieces from document
     Operator_.DeleteRange;

     // insert it in the sorted order
     For iLoop := 0 To oPieces.Count - 1 Do
       Operator_.DirectAppendPiece(oPieces[iLoop].Link, iStart + iLoop);
     Operator_.EndDirectAppend;
    Finally
      oPieces.Free;
    End;

    SelectedTable.StructureDirty := True;
    Document.RegenerateMetrics(False, False);

    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNewPos, cdJump);
    End;
End;

Procedure TWPRange.DoReorderRows(Const oRows: TWPWorkingDocumentPieces; Const aSortedPos: Array Of Integer;
                Const iStart, iStop: Integer; Var oPieces: TWPWorkingDocumentPieces);
Var
  iLoop: Integer;
  iRowCount: Integer;
  bInRow: Boolean;
  oPiece: TWPWorkingDocumentPiece;
Begin
  bInRow := False;
  iRowCount := 0;
  For iLoop := iStart To iStop Do
  Begin
    oPiece := Document.Pieces[iLoop];
    Case oPiece.PieceType Of
    ptRowStart:
      Begin
        bInRow := True;
        CopyRowByReference(TWPWorkingDocumentTableRowStartPiece(oRows[aSortedPos[iRowCount]]), oPieces);
        Inc(iRowCount);
      End;
    ptRowStop:  bInRow := False;
    Else
      // skip it if inside a row
      If Not bInRow Then
        oPieces.Add(oPiece.Clone);
    End;
  End;
End;

Procedure TWPRange.CopyRowByReference(Const oStart: TWPWorkingDocumentTableRowStartPiece;  Var oPieces: TWPWorkingDocumentPieces);
Var
  oStop: TWPWorkingDocumentPiece;
  iLoop: Integer;
Begin
  oStop := FindNextPiece(oStart, ptRowStop, ptTableStop, False);
  For iLoop := Document.Pieces.IndexByReference(oStart) To Document.Pieces.IndexByReference(oStop) Do
    oPieces.Add(Document.Pieces[iLoop].Clone);
End;

Procedure TWPRange.UpdateTextMetrics;
Begin
  TWordProcessor(FOwner).UpdateTextMetrics;
End;

Function TWPRange.InsertTemplate(oBuffer: TFslBuffer): Boolean;
Var
  oMem : TFslMemoryStream;
  sError : String;
  oSpeechMagicOptions : TWPSpeechMagicInsertOptions;
Begin
  Result := (canInsertTemplate In Capabilities) And Not Operator_.Settings.ConsoleMode;
  If Result And HasCurrentFieldStart Then
    Result := RemoveField(False);
  If Result Then
  Begin
    oMem := TFslMemoryStream.Create;
    Try
      oMem.Buffer := oBuffer.Link;
      oSpeechMagicOptions := TWPSpeechMagicInsertOptions.Create;
      Try
        oSpeechMagicOptions.SpeechMagicDouble := True {?};
        oSpeechMagicOptions.IgnoreBackground := True;
        Result := Insert(oMem, wpfNative, False, oSpeechMagicOptions, sError);
        If (Not Result) And (sError <> '') Then
          RaiseError('Insert Template', sError);
      Finally
        oSpeechMagicOptions.Free;
      End;
    Finally
      oMem.Free;
    End;
  End;
End;

Procedure TWPRange.ClearUndoRedo;
Begin
  LogAction('ClearUndoRedo', '');
  Operator_.ClearUndo;
End;

Function TWPRange.WorkingStyle: TWPStyle;
Begin
  If Operator_.Styles.ExistsByName(Style) Then
    Result := Operator_.Styles.GetByName(Style)
  Else
    Result := Operator_.Styles.DefaultStyle;
End;

Function TWPRange.ConsoleFontOk(oFont, oCompare: TWPSFontDetails): Boolean;
Begin
  Result := Not Operator_.Settings.ConsoleMode Or
     ((Not oFont.HasName Or (oFont.Name = oCompare.Name)) And
      (Not oFont.HasSize Or (oFont.Size = oCompare.Size)) And
      (Not oFont.HasStrikethrough Or (oFont.Strikethrough = oCompare.Strikethrough)) And
      (Not oFont.HasState Or (oFont.State = oCompare.State)) And
      (Not oFont.HasForeground Or (oFont.Foreground = oCompare.Foreground)) And
      (Not oFont.HasBackground Or (oFont.Background = oCompare.Background)) And
      (Not oFont.HasCapitalization Or (oFont.Capitalization = oCompare.Capitalization)));
End;

Function TWPRange.ConsoleParaOK(oPara: TWPSParagraphDetails): Boolean;
Begin
  Result := Not Operator_.Settings.ConsoleMode;
End;

Procedure TWPRange.DefaultLoadImage(oSender: TObject; Const Context : String; Const sName: String; Out oBuffer: TFslBuffer);
Var
  oUri : TIdUri;
  oClient : TFslWinInetClient;
Begin
  oUri := TIdURI.Create(sName);
  Try
    if (oUri.Host = '') and (oUri.Path = '') then
    begin
      oUri.Free;
      oUri := TIdURI.create(ExtractFilePath(Context)+sName);
    end;

    if (oUri.Protocol = 'file') and (Length(oUri.Host) = 1) Then
    begin
      oBuffer := TFslBuffer.create;
      {$IFDEF UNICODE}
      oBuffer.LoadFromFileName(oUri.Host+':'+oUri.Path+oUri.Document);
      {$ELSE}
      oBuffer.LoadFromFileName(System.copy(oUri.Uri, 8, $FFFF));
      {$ENDIF}
    end
    else
    begin
      oClient := TFslWinInetClient.Create;
      Try
        oClient.Server := oUri.Host;
        oClient.Port := oUri.Port;
        If oUri.Path <> '' Then
          oClient.Resource := oUri.Path+'/'+oUri.Document
        Else
          oClient.Resource := oUri.Path;
        If oUri.Params <> '' Then
          oClient.Resource := oClient.Resource+'/'+oUri.Params;
        oClient.Secure := oUri.Protocol = 'https';
        oClient.Request := TFslBuffer.Create;
        oClient.Response := TFslBuffer.Create;
        oClient.RequestMethod := 'GET';

        oClient.Execute;
        If StringStartsWith(oClient.ResponseCode, '2') Then
          oBuffer := oClient.Response.Link
        Else
          oBuffer := Nil;
      Finally
        oClient.Free;
      End;
    end;
  Finally
    oUri.Free;
  End;
End;

Procedure TWPRange.ResetSpelling;
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentPiece;
Begin
  LogAction('ResetSpelling', '');
  iLoop := 0;
  If Operator_.HasSpeller Then
  Begin
    While iLoop < Document.Pieces.Count Do
    Begin
      oPiece := Document.Pieces[iLoop];

      If (oPiece.PieceType = ptText) And (oPiece.LogicalText <> ' ') Then
      Begin
        TWPWorkingDocumentTextPiece(oPiece).SpellState := scsNotChecked;
        TWPWorkingDocumentTextPiece(oPiece).Change(ctPresentation, Nil);
      End;
      Inc(iLoop);
    End;
  End;
  CheckSpelling;
End;

Function TWPRange.SplitCell: Boolean;
Var
  s : String;
  oPiece : TWPWorkingDocumentPiece;
  iOffset, iIndex, iNew : Integer;
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop :  TWPWorkingDocumentStopPiece;
  oParagraph : TWPWorkingDocumentParaPiece;
Begin
  Result := canSplitCell In Capabilities;
  LogAction('SplitCell', '', result);
  If Result Then
  Begin

    s := Selection.Save;
    Operator_.StartOperation(FId, otTable, False, Selection);
    Operator_.SetRange(CurrentTableCellStart.Metrics.Position, CurrentTableCellStop.Metrics.Position + CurrentTableStop.Metrics.CharCount);
    Operator_.SetPriorCursor(s);
    Operator_.CleanRange;
    Operator_.MakeCleanCut(Selection.Cursor);
    Document.RegenerateMetrics(False, False);

    If Not Document.GetPieceByPosition(Selection.Cursor, oPiece, iOffset, iIndex) Then
      raise EWPException.create('unable to find position');
    If iOffset <> 0 Then
      raise EWPException.create('internal logic problem');

    CurrentTableCellStart.Span := CurrentTableCellStart.Span - 1;
    oStart := TWPWorkingDocumentTableCellStartPiece.Create;
    Try
      ApplyBaseStyle(oStart);
      Operator_.DirectAppendPiece(oStart.Link, iIndex);
    Finally
      oStart.Free;
    End;

    oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
    Try
      ApplyBaseStyle(oStop);
      Operator_.DirectAppendPiece(oStop.Link, iIndex);
    Finally
      oStop.Free;
    End;

    oParagraph := TWPWorkingDocumentParaPiece.Create;
    Try
      ApplyBaseStyle(oParagraph);
      Operator_.DirectAppendPiece(oParagraph.Link, iIndex);
    Finally
      oParagraph.Free;
    End;

    CurrentTableStart.StructureDirty := True;
    Document.RegenerateMetrics(False, False);
    iNew := oStart.Metrics.Position + 1;
    Operator_.SetClosingRange(CurrentTableCellStop.Metrics.Position + CurrentTableCellStop.Metrics.CharCount);
    Operator_.FinishOperation;
    ChangeContent;
    DoMove(iNew, cdJump);
  End;
End;

Function TWPRange.InsideField: Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  If Document.GetPieceByPosition(FSelection.Cursor, oPiece, iInternal, iIndex) Then
    Result := (oPiece.PieceType = ptFieldStart) And (iInternal <> 0)
  Else
    Result := False;
End;


Function TWPRange.SetFieldCheckIndex(oField: TWPWorkingDocumentFieldStartPiece; iIndex: Integer): Boolean;
Var
  oCurrent : TWPWorkingDocumentFieldStartPiece;
  sSelection : String;
Begin
  Result := Not Operator_.Settings.ReadOnly;
  LogAction('SetFieldCheckBox', oField.describe+' '+IntToStr(iIndex), result);
  If Result Then
    Begin
    oCurrent := CurrentFieldStart;
    If Not Assigned(oCurrent) Then
      RaiseError('SetFieldProperties', 'No Field in Focus');

    sSelection := Selection.Save;
    MoveTo(oCurrent.Metrics.Position, False);
    SelectTo(oCurrent.Metrics.Position + oCurrent.Metrics.CharCount, False);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.SetPriorCursor(sSelection);
    Operator_.CleanRange;

    If oCurrent.CheckedIndex = iIndex Then
      oCurrent.CheckedIndex := 0
    Else
      oCurrent.CheckedIndex := iIndex;
    oCurrent.Change(ctPresentation, oCurrent);

    Operator_.FinishOperation;
    Selection.Restore(sSelection);
    CheckSpelling;
    ChangeContent;
    ChangeState;
    End;
End;

Function TWPRange.InsertAnnotation(oOwner : TWPAnnotationDefinitionProvider; Const sText: String): Boolean;
Var
  oAnnotation : TWPWorkingAnnotation;
  iId : Integer;
  oIterator : TWPPieceIterator;
Begin
  if oOwner <> nil then
    LogAction('InsertAnnotation', oOwner.GetNamespace+': '+sText, true)
  else
    LogAction('InsertAnnotation', 'nil: '+sText, true);

  oAnnotation := TWPWorkingAnnotation.Create;
  Try
    oAnnotation.Owner := oOwner.GetNamespace;
    oAnnotation.Text := sText;
    oAnnotation.Colour := oOwner.GetColour(sText);
    Document.AllAnnotations.Add(oAnnotation.Link);
    iId := Document.AllAnnotations.Count;

    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.CleanRange;
    oIterator := TWPPieceIterator.Create(FDocument.Link);
    Try
      oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
      oIterator.PieceTypes := ALL_PIECE_TYPES;
      oIterator.First;
      If oIterator.More Then
        oIterator.CurrentParagraph(True).Change(ctLayout, oIterator.Current);
      While oIterator.More Do
        Begin
        oIterator.Current.AnnotationId := iId;
        oIterator.Current.Change(ctTextContents, oIterator.Current);
        oIterator.Next;
        End;
    Finally
      oIterator.Free;
    End;
    Operator_.FinishOperation;
    Document.RegenerateMetrics(True, False);
    ChangeContent;
    ChangeState;
  Finally
    oAnnotation.Free;
  End;
  Result := True;
End;

Function TWPRange.EditAnnotation(Const sText: String): Boolean;
Var
  oOldAnnotation : TWPWorkingAnnotation;
  oNewAnnotation : TWPWorkingAnnotation;
  oIterator : TWPPieceIterator;
  iId : Integer;
Begin
  LogAction('EditAnnotation', sText);
  Operator_.StartOperation(FId, otChange, False, Selection);
  oOldAnnotation := FDocument.AllAnnotations.GetBySelection(Selection.WorkingSelStart, Selection.WorkingSelEnd);
  If (oOldAnnotation = Nil) Then
    RaiseError('EditComment', 'Cannot edit comment - comment not found');
  oNewAnnotation := oOldAnnotation.Clone;
  Document.AllAnnotations.Add(oNewAnnotation);
  iId := Document.AllAnnotations.Count;
  Operator_.SetRange(oOldAnnotation.OffsetStart, oOldAnnotation.OffsetEnd);
  Operator_.CleanRange;
  oNewAnnotation.Text := sText;
  oIterator := TWPPieceIterator.Create(FDocument.Link);
  Try
    oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.First;
    If oIterator.More Then
      oIterator.CurrentParagraph(True).Change(ctLayout, oIterator.Current);
    While oIterator.More Do
      Begin
      oIterator.Current.AnnotationId := iId;
      oIterator.Current.Change(ctLayout, oIterator.Current);
      oIterator.Next;
      End;
  Finally
    oIterator.Free;
  End;
  Operator_.FinishOperation;
  Document.RegenerateMetrics(True, False);
  ChangeContent;
  ChangeState;
  Result := True;
End;

Procedure TWPRange.DeleteAnnotation();
Var
  oAnnotation : TWPWorkingAnnotation;
  oIterator : TWPPieceIterator;
Begin
  LogAction('DeleteAnnotation', '', true);
  Operator_.StartOperation(FId, otChange, False, Selection);
  oAnnotation := FDocument.AllAnnotations.GetBySelection(Selection.WorkingSelStart, Selection.WorkingSelEnd).Link;
  If (oAnnotation = Nil) Then
    RaiseError('DeleteComment', 'Cannot Delete comment - comment not found');
  Try
    Operator_.SetRange(oAnnotation.OffsetStart, oAnnotation.OffsetEnd);
    Operator_.CleanRange;
  Finally
    oAnnotation.Free;
  End;

  oIterator := TWPPieceIterator.Create(FDocument.Link);
  Try
    oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.First;
    If oIterator.More Then
      oIterator.CurrentParagraph(True).Change(ctLayout, oIterator.Current);
    While oIterator.More Do
      Begin
      oIterator.Current.AnnotationId := 0;
      oIterator.Current.Change(ctTextContents, oIterator.Current);
      oIterator.Next;
      End;
  Finally
    oIterator.Free;
  End;

  Operator_.FinishOperation;
  Document.RegenerateMetrics(True, False);
  ChangeContent;
  ChangeState;
End;

Function TWPRange.AtStartOfSentence(checkEmpty : Boolean): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
  iWork : Integer;
Begin
  Result := False;
  If Document.GetPieceByPosition(IntegerMax(0, Selection.Cursor), oPiece, iInternal, iIndex) And (iInternal = 0) Then
  Begin
    If iIndex = 0 Then
      Result := True
    Else
    Begin
      iWork := iIndex;
      Dec(iWork);
      oPiece := Document.Pieces[iWork];

      While (iWork > 0) And (oPiece.PieceType = ptText) And StringIsWhitespace(oPiece.LogicalText) Do
      Begin
        Dec(iWork);
        oPiece := Document.Pieces[iWork];
      End;
      Result := (oPiece.PieceType In BLOCK_PIECE_TYPES) Or ((oPiece.PieceType = ptText) And StringEndsWith(oPiece.LogicalText, '.') And Not StringEndsWith(oPiece.LogicalText, '..'));
    End;
    result := result and (not checkEmpty or (((iIndex + 1) >= Document.Pieces.Count) or (Document.Pieces[iIndex + 1].PieceType In BLOCK_PIECE_TYPES)));
  End;
End;

Function TWPRange.AtStartOfPara(checkEmpty : Boolean): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := False;
  If Document.GetPieceByPosition(IntegerMax(0, Selection.Cursor), oPiece, iInternal, iIndex) And (iInternal = 0) Then
  Begin
    if iInternal > 0 then
      result := false
    else If iIndex = 0 Then
      Result := True
    Else
    Begin
      Result := (Document.Pieces[iIndex-1].PieceType In BLOCK_PIECE_TYPES);
      result := result and (not checkEmpty or (((iIndex) >= Document.Pieces.Count) or (Document.Pieces[iIndex].PieceType In BLOCK_PIECE_TYPES)));
    End;
  End;
End;

Function TWPRange.ParaText : String;
Var
  iStart, iStop : Integer;
Begin
  If GetParaRange(False, iStart, iStop) Then
    Result := GetTextInRange(iStart, iStop)
  Else
    Result := '';
end;

Function TWPRange.AtEndOfPara : Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := False;
  If Document.GetPieceByPosition(IntegerMax(0, Selection.Cursor), oPiece, iInternal, iIndex) And (iInternal = 0) Then
    Result := (Document.Pieces[iIndex].PieceType = ptPara);
End;

Function TWPRange.HasFieldCheckables(oDocument: TWPWorkingDocument): Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;

  iLoop := 0;
  while iLoop < oDocument.Pieces.Count Do
  Begin
    If Not Result And (oDocument.Pieces[iLoop].PieceType = ptFieldStart) Then
    Begin
      TWordProcessor(FOwner).BindToField(oDocument, iLoop, TWPWorkingDocumentFieldStartPiece(oDocument.Pieces[iLoop]));
      If TWPWorkingDocumentFieldStartPiece(oDocument.Pieces[iLoop]).Checkables.Count > 0 Then
        Result := True;
    End;
    inc(iLoop);
  End;
End;
{
function TWPRange.NextParaIsNotList: Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
Begin
  Result := False;
  oPiece := CurrentParagraph.Next;
  while (oPiece <> nil) and not (oPiece.PieceType in BLOCK_PIECE_TYPES) do
    oPiece := oPiece.Next;
  if (oPiece = nil) then
    result := true
  else if (oPiece.PieceType = ptPara) then
    result := (TWPWorkingDocumentParaPiece(oPiece).Format.ListType in [WPSParagraphListTypeUnknown, WPSParagraphListTypeNone]);
end;
}
function TWPRange.AdjacentIsPara(toRight: Boolean): Boolean;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
begin
  if Selection.HasSelection then
    result := false
  else
  begin
    iIndex := Selection.Cursor;
    if not toRight then
      dec(iIndex);
    result := Document.GetPieceByPosition(iIndex, oPiece, iInternal, iIndex) And (iInternal = 0) And (oPiece.PieceType = ptPara) and not oPiece.IsReadOnly;
  end;
end;

function TWPRange.MergeParagraph(toRight: Boolean): Boolean;
var
  sSaved : String;
  cursor, start, stop : integer;
  oPiece : TWPWorkingDocumentPiece;
  oSource, oDest : TWPWorkingDocumentParaPiece;
  iInternal : Integer;
  iIndex : Integer;
  oIterator : TWPPieceIterator;
begin
  LogAction('MergeParagraph', BooleanToString(toRight));

  result := false;
  cursor := Selection.Cursor;
  if not toRight then
    dec(cursor);
  if not Document.GetPieceByPosition(cursor, oPiece, iInternal, iIndex) or (oPiece.PieceType <> ptPara) then
    exit;
  oSource := oPiece as TWPWorkingDocumentParaPiece;
  // now cursor is to left of paragraph to be deleted
  start := cursor;
  while (start > 0) and Document.GetPieceByPosition(start-1, oPiece, iInternal, iIndex) and (oPiece.PieceType in INLINE_PIECE_TYPES) do
    dec(start);
  stop := cursor + 1;
  oPiece := nil;
  while (stop < Document.CharCount) and Document.GetPieceByPosition(stop, oPiece, iInternal, iIndex) and (oPiece.PieceType in INLINE_PIECE_TYPES) do
    inc(stop);
  if (oPiece <> nil) and (oPiece.PieceType = ptPara) then
  begin
    oDest := oPiece as TWPWorkingDocumentParaPiece;
    sSaved := Selection.Save;
    Selection.Select(start, stop+1);
    Operator_.StartOperation(FId, otChange, False, Selection);
    Operator_.SetPriorCursor(sSaved);
    Operator_.CleanRange;
    oDest.Style := oSource.Style;
    oDest.Font.Assign(oSource.Font);
    oDest.Format.Assign(oSource.Format);
    oDest.Change(ctTextContents, oDest);
    if (oDest.Style <> '') and Operator_.Styles.ExistsByName(oDest.Style) and Operator_.Styles.GetByName(oDest.Style).HasParagraphAspect then
    begin
      oIterator := TWPPieceIterator.Create(Document.Link);
      Try
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
        oIterator.First;
        While oIterator.More Do
        Begin
          if oIterator.Current <> oDest then
          begin
            oIterator.Current.AssignStyle(oDest);
            oIterator.Current.Change(ctTextContents, oIterator.Current);
          end;
          oIterator.Next;
        end;
      finally
        oIterator.Free;
      end;
    end;
    Operator_.RemovePiece(oSource);
    Operator_.FinishOperation;
    Selection.MoveTo(cursor);
    ChangeContent;
    ChangeState;
    result := true;
  end;
End;

{

  get this paragraph and next paragraph
  expand selection
  start operation
  update next paragraph settings
  delete this paragraph
  if style is paragraph update all pieces
  close operation
end;
Var
  oLast : TWPWorkingDocumentParaPiece;
  sSaved : String;
Begin
  Result := Not Operator_.Settings.ReadOnly And (canFormat In Capabilities) And ConsoleParaOK(oParagraph);
  If Result Then
    Begin
    // we may actually make changes outside the selected
    // So the first thing we have to do is to set the Selection up properly

    If Selection.HasSelection Then
      Begin
      oIterator := TWPPieceIterator.Create(Document.Link);
      Try
        If Not Document.GetPieceByPosition(Selection.SelEnd, oPiece, iInternal, iIndex) Then
          RaiseError('ApplyParagraph', 'Unable to find Selection end');
        While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
          Inc(iIndex);
        If FDocument.Pieces.ExistsByIndex(iIndex) Then
        Begin
          oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
          Selection.Select(Selection.SelStart, oLast.Metrics.Position+oLast.Metrics.CharCount);
        End;

        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);

        Operator_.CleanRange;
        oIterator.PieceTypes := ALL_PIECE_TYPES;
        oIterator.SetBounds(Selection.SelStart, Selection.SelEnd);
        oIterator.First;
        If oIterator.More Then
          Begin
          oLast := oIterator.CurrentParagraph;
          oLast.Change(oLast.Format.Update(oParagraph), oLast);
          While oIterator.More Do
            Begin
            If oLast <> oIterator.CurrentParagraph(True) Then
              Begin
              oLast := oIterator.CurrentParagraph(True);
              If Assigned(oLast) Then
                Begin
                oLast.Change(oLast.Format.Update(oParagraph), oLast);
                End;
              End;
            oIterator.Next;
            End;
          End
      Finally
        oIterator.Free;
      End;
      End
    Else If Document.GetPieceByPosition(Selection.Cursor, oPiece, iInternal, iIndex) Then
      Begin
      While (iIndex < FDocument.Pieces.count) And (FDocument.Pieces[iIndex].PieceType <> ptPara) Do
        Inc(iIndex);
      If iIndex < FDocument.Pieces.count Then
        Begin
        oLast := TWPWorkingDocumentParaPiece(FDocument.Pieces[iIndex]);
        sSaved := Selection.Save;
        Selection.Select(oLast.Metrics.Position, oLast.Metrics.Position+oLast.Metrics.CharCount);
        Operator_.StartOperation(FId, otChange, False, Selection);
        Operator_.SetPriorCursor(sSaved);
        Operator_.CleanRange;
        oLast.Change(oLast.Format.Update(oParagraph), oLast);
        End;
      End;
    Operator_.FinishOperation;
    Selection.Restore(sSaved);
    ChangeContent;
    ChangeState;
    End;           }

function TWPRange.SetFieldContents(namespace, name, content: String; annotation : TWPAnnotationDefinitionProvider): Boolean;
var
  s : String;
begin
  result := false;
  LogAction('SetFieldContents', namespace+'#'+name+': '+content, result);
  result := GotoFieldByName(namespace, name);
  s := SelectedText;
  if result then
  begin
    result := Insert(content);
    if annotation <> nil then
    begin
      SelectField(False);
      InsertAnnotation(annotation, 'Value changed from "'+s+'"');
    end;
  end;
end;

function TWPRange.SetFieldContents(oField: TWPDocumentField; content: String; annotation : TWPAnnotationDefinitionProvider): Boolean;
var
  s : String;
begin
  result := GotoFieldByDoc(oField);
  LogAction('SetFieldContents', ofield.namespace+'#'+ofield.name+': '+content, result);
  s := SelectedText;
  if result then
  begin
    result := Insert(content);
    if annotation <> nil then
    begin
      SelectField(False);
      InsertAnnotation(annotation, 'Value changed from "'+s+'"');
    end;
  end;
end;

function TWPRange.InsertImage(Const sName: String; stream: TFslAccessStream): Boolean;
Var
  oLoader : TWPImageLoader;
  oImage : TFslGraphic;
Begin
  LogAction('InsertImage(stream)', sName);
  oLoader := TWPImageLoader.Create;
  Try
    oLoader.DicomDictionary := Operator_.Settings.DicomDictionary.Link;
    oLoader.Source := stream.link;
    oImage := oLoader.Load;
    Try
      Result := InsertImage(sName, oImage);
    Finally
      oImage.Free;
    End;
  Finally
    oLoader.Free;
  End;
end;

function TWPRange.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FOperator.sizeInBytes);
  inc(result, FFont.sizeInBytes);
  inc(result, FParagraph.sizeInBytes);
  inc(result, (FStyle.length * sizeof(char)) + 12);
  inc(result, (FLastStyleSummary.length * sizeof(char)) + 12);
  inc(result, FSelection.sizeInBytes);
  inc(result, FLog.sizeInBytes);
  inc(result, FCurrentParagraph.sizeInBytes);
  inc(result, FCurrentImage.sizeInBytes);
  inc(result, FCurrentLine.sizeInBytes);
  inc(result, FCurrentSectionStart.sizeInBytes);
  inc(result, FCurrentSectionStop.sizeInBytes);
  inc(result, FCurrentFieldStart.sizeInBytes);
  inc(result, FCurrentFieldStop.sizeInBytes);
  inc(result, FCurrentTableStart.sizeInBytes);
  inc(result, FCurrentTableStop.sizeInBytes);
  inc(result, FCurrentTableRowStart.sizeInBytes);
  inc(result, FCurrentTableRowStop.sizeInBytes);
  inc(result, FCurrentTableCellStart.sizeInBytes);
  inc(result, FCurrentTableCellStop.sizeInBytes);
  inc(result, FSelectedTable.sizeInBytes);
  inc(result, FSelectedRows.sizeInBytes);
  inc(result, FSelectedCells.sizeInBytes);
end;

{ TWPRangeList }

Function TWPRangeList.Clone: TWPRangeList;
Begin
  Result := TWPRangeList(Inherited Clone);
End;

Function TWPRangeList.GetRange(iIndex: Integer): TWPRange;
Begin
  Result := TWPRange(ObjectByIndex[iIndex]);
End;

Function TWPRangeList.ItemClass: TFslObjectClass;
Begin
  Result := TWPRange;
End;

Function TWPRangeList.Link: TWPRangeList;
Begin
  Result := TWPRangeList(Inherited Link);
End;



{ TWPActionsList }

function TWPActionsList.GetAction(index: integer): TWPAction;
begin
  result := TWPAction(ObjectByIndex[index]);
end;

function TWPActionsList.ItemClass: TFslObjectClass;
begin
  result := TWPAction;
end;


{ TWPOperator }

Constructor TWPOperator.Create;
Begin
  Inherited;
  FUndoStack := TWPOperationStack.Create;
  FRedoStack := TWPOperationStack.Create;
  FVersion := 0;
  FRendererRange := TOperationRange.Create;
End;

Destructor TWPOperator.Destroy;
Begin
  FRendererRange.Free;
  FUndoStack.Free;
  FRedoStack.Free;

  FDocument.Free;
  FStyles.Free;
  FSpeller.Free;

  Inherited;
End;

Function TWPOperator.Link : TWPOperator;
Begin
  Result := TWPOperator(Inherited Link);
End;


Procedure TWPOperator.SetStyles(Const Value: TWPStyles);
Begin
  FStyles.Free;
  FStyles := Value;
End;

Procedure TWPOperator.SetSpeller(Const Value: TWPSpeller);
Begin
  FSpeller.Free;
  FSpeller := Value;
End;


Function TWPOperator.HasSpeller: Boolean;
Begin
  Result := Assigned(FSpeller);
End;

Function TWPOperator.GetSpeller : TWPSpeller;
Begin
  Assert(Invariants('GetSpeller', FSpeller, TWPSpeller, 'Speller'));
  Result := FSpeller;
End;

Function TWPOperator.GetStyles : TWPStyles;
Begin
  Assert(Invariants('GetStyles', FStyles, TWPStyles, 'Styles'));
  Result := FStyles;
End;




Function TWPOperator.HasDocument: Boolean;
Begin
  Result := Assigned(FDocument);
End;

Function TWPOperator.HasStyles : Boolean;
Begin
  Result := Assigned(FStyles);
End;



Procedure TWPOperator.SetDocument(Const Value: TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
  ClearUndo;
  FVersion := 0;
  FLastAction := 0;
End;

Function TWPOperator.CanUndo(iRangeId : Integer): Boolean;
Begin
  Result := (FUndoStack.Count > 0) And ((iRangeId = MasterRangeId) Or (FUndoStack.Current.RangeId = iRangeId));
End;

Function TWPOperator.CanRedo(iRangeId : Integer): Boolean;
Begin
  Result := (FRedoStack.Count > 0) And ((iRangeId = MasterRangeId) Or (FRedoStack.Current.RangeId = iRangeId));;
End;

Function TWPOperator.Undo(iRangeId : Integer; oSelection : TWPSelection) : Boolean;
Var
  oOperation : TWPOperation;
  oPieces : TWPWorkingDocumentPieces;
  iLoop : Integer;
Begin
  Assert(Invariants('StartOperation', oSelection, TWPSelection, 'Selection'));
  Assert(CheckCondition(FStatus = osNone, 'Undo', 'Cannot Undo while an operation is in progress ['+NAMES_WPOPERATIONSTATUS[FStatus]+']'));
  Result := CanUndo(iRangeId);
  If Result Then
    Begin
    Inc(FVersion);
    oOperation := FUndoStack.Current.Link;
    Try
      FUndoStack.Pop;
      oOperation.RedoCursor := oSelection.Save;
      If oOperation.OpType = otMove Then
        CutMovedContent(oOperation);
      CleanEndRange(oOperation);
      CopyUndoContent(oOperation);
      DeleteEndRange(oOperation);
      InsertContent(oOperation);
      MergeDocument;
      Document.RegenerateMetrics(True, false);
      oSelection.Restore(oOperation.UndoCursor);
      If Assigned(FOnUndo) Then
        FOnUndo(oOperation);
      If Assigned(FOnDeleteContent) Then
        If oOperation.OpType = otMove Then
          FOnDeleteContent(Self, oOperation.MoveDestination, oOperation.OriginalPieces)
        Else If (oOperation.AddedAmount > 0) Then
          FOnDeleteContent(Self, oOperation.Start, oOperation.ModifiedPieces);
      If Assigned(FOnInsertContent) Then
        If (oOperation.OpType = otTrim) Then
        Begin
          For iLoop := 0 To oOperation.RemovedPieces.Count - 1 Do
          Begin
            oPieces := TWPWorkingDocumentPieces.Create;
            Try
              oPieces.Hooking := False;
              oPieces.Add(oOperation.RemovedPieces[iLoop].Piece.Link);
              FOnInsertContent(Self, oOperation.RemovedPieces[iLoop].Offset, oPieces);
            Finally
              oPieces.Free;
            End;
          End;
        End
        Else If (oOperation.OpType = otTable) Then
        Begin
          For iLoop := 0 To oOperation.TablePieces.Count - 1 Do
          Begin
            oPieces := TWPWorkingDocumentPieces.Create;
            Try
              oPieces.Hooking := False;
              oPieces.Add(oOperation.TablePieces[iLoop].Piece.Link);
              FOnInsertContent(Self, oOperation.TablePieces[iLoop].Offset, oPieces);
            Finally
              oPieces.Free;
            End;
          End;
        End
        Else If (oOperation.DeletedAmount > 0) Then
          FOnInsertContent(Self, oOperation.Start, oOperation.OriginalPieces);

      FRedoStack.Push(oOperation.Link);
    Finally
      oOperation.Free;
    End;
    End;
End;

Function TWPOperator.Redo(iRangeId : Integer; oSelection : TWPSelection) : Boolean;
Var
  oOperation : TWPOperation;
  oPieces : TWPWorkingDocumentPieces;
  iLoop : Integer;
Begin
  Assert(Invariants('StartOperation', oSelection, TWPSelection, 'Selection'));
  Assert(CheckCondition(FStatus = osNone, 'Undo', 'Cannot Undo while an operation is in progress ['+NAMES_WPOPERATIONSTATUS[FStatus]+']'));
  Result := CanRedo(iRangeId);
  If Result Then
    Begin
    Inc(FVersion);
    oOperation := FRedoStack.Current.Link;
    Try
      FRedoStack.Pop;
      DeleteRange(oOperation);
      If Assigned(FOnDeleteContent) Then
        If (oOperation.OpType = otTrim) Then
        Begin
          For iLoop := oOperation.RemovedPieces.Count - 1 DownTo 0 Do
          Begin
            oPieces := TWPWorkingDocumentPieces.Create;
            Try
              oPieces.Hooking := False;
              oPieces.Add(oOperation.RemovedPieces[iLoop].Piece.Link);
              FOnDeleteContent(Self, oOperation.RemovedPieces[iLoop].Offset, oPieces);
            Finally
              oPieces.Free;
            End;
          End;
        End
        Else If (oOperation.DeletedAmount > 0) Then
          FOnDeleteContent(Self, oOperation.Start, oOperation.OriginalPieces);

      If Assigned(FOnInsertContent) Then
        If oOperation.OpType = otMove Then
          FOnInsertContent(Self, oOperation.MoveDestination, oOperation.OriginalPieces)
        Else If (oOperation.AddedAmount > 0) Then
          FOnInsertContent(Self, oOperation.Start, oOperation.ModifiedPieces);

      InsertRedoContent(oOperation);
      MergeDocument;
      Document.RegenerateMetrics(True, false);
      oSelection.Restore(oOperation.RedoCursor);
      If Assigned(FOnRedo) Then
        FOnRedo(oOperation);
      FUndoStack.Push(oOperation.Link);
      FStatus := osNone;
    Finally
      oOperation.Free;
    End;
    End;
End;

Function TWPOperator.StartOperation(iRangeId : Integer; aOpType: TWPOperationType; bReopen: Boolean; oSelection : TWPSelection) : Boolean;
Begin
  Assert(Invariants('StartOperation', oSelection, TWPSelection, 'Selection'));
  Assert(CheckCondition(FStatus = osNone, 'Undo', 'Cannot start an operation while another operation is in progress ['+NAMES_WPOPERATIONSTATUS[FStatus]+']'));
  Inc(FVersion);
  Result := Not ((aOpType = otCharInsert) And bReOpen And FUndoStack.LastUnClosed And (FUndoStack.Current.OpType = otCharInsert));
  If Result Then
    Begin
    FCurrentOp := TWPOperation.Create;
    Try
      FCurrentOp.RangeId := iRangeId;
      FCurrentOp.OpType := aOpType;
      FCurrentOp.Closed := False;
      FCurrentOp.UndoCursor := oSelection.Save;
      If oSelection.HasSelection Then
        Begin
        FCurrentOp.Start := oSelection.SelStart;
        FCurrentOp.CommencementFinish := oSelection.SelEnd;
        End
      Else
        Begin
        FCurrentOp.Start := oSelection.Cursor;
        FCurrentOp.CommencementFinish := oSelection.Cursor;
        End;
      FUndostack.Add(FCurrentOp.Link);
    Finally
      FCurrentOp.Free;
    End;
    FStatus := osRanging;
    FRedoStack.Clear;
    End
  Else
    Begin
    FCurrentOp := FUndoStack.Current;
    FStatus := osExtending;
    FCurrentOp.ReIterate;
    End;
End;

Procedure TWPOperator.ExpandRange(iStartDelta, iEndDelta: Integer);
Begin
  Assert(CheckCondition(FStatus = osRanging, 'ExpandRange', 'Cannot expand range when not in ranging mode'));
  If FCurrentOp.Start > 0 - iStartDelta Then
    FCurrentOp.Start := FCurrentOp.Start + iStartDelta
  Else
    FCurrentOp.Start := 0;
  FCurrentOp.CommencementFinish := FCurrentOp.CommencementFinish + iEndDelta;
End;

Procedure TWPOperator.SetRange(iStart, iEnd: Integer);
Begin
  Assert(CheckCondition(FStatus = osRanging, 'SetRange', 'Cannot expand range when not in ranging mode'));
  FCurrentOp.Start := iStart;
  FCurrentOp.CommencementFinish := iEnd;
End;

Procedure TWPOperator.CleanRange;
Begin
  Assert(CheckCondition(FStatus In [osRanging, osExtending], 'CleanRange', 'Cannot clean range when not in ranging mode'));
  CleanRange(FCurrentOp);
End;

Procedure TWPOperator.CleanRange(oOperation : TWPOperation);
Begin
  MakeCleanCut(oOperation.Start);
  If FStatus <> osExtending Then
    Begin
    If oOperation.Start <> oOperation.CommencementFinish Then
      MakeCleanCut(oOperation.CommencementFinish);
    End
  Else
    Begin // we are extending an existing operation
    If oOperation.AddedAmount <> 0 Then
      MakeCleanCut(oOperation.Start + oOperation.AddedAmount);
    End;
  If FStatus <> osExtending Then
    CopyContent(oOperation);
  FStatus := osChanging;
End;

Procedure TWPOperator.CleanRange(iStart, iEnd : Integer);
Begin
  MakeCleanCut(iStart);
  MakeCleanCut(iEnd);
End;

Function TWPOperator.MakeCleanCut(iOffset : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
Begin
  If Not FDocument.GetPieceByPosition(iOffset, oPiece, iInternal, Result) Then
    RaiseError('CleanRange', 'Unable to find Cut Point '+IntegerToString(iOffset));
  If (iInternal > 0) And (iInternal < Length(oPiece.LogicalText)) Then
    Begin
    Inc(Result);
    FDocument.Pieces.Insert(Result, oPiece.Split(iInternal));
    If oPiece.PieceType = ptText Then
      TWPWorkingDocumentTextPiece(oPiece).SpellState := scsNotChecked;
    End;
End;

Function TWPOperator.FindPoint(iOffset : Integer) : Integer;
Var
  oPiece : TWPWorkingDocumentPiece;
  iInternal : Integer;
Begin
  if (iOffset = 0) then
    result := 0
  Else
  Begin
    If Not FDocument.GetPieceByPosition(iOffset, oPiece, iInternal, Result) Then
      RaiseError('FindPoint', 'Unable to find Cut Point '+IntegerToString(iOffset));
    if iInternal > 0 Then
    Begin
      if result = FDocument.Pieces.Count - 1 Then
        result := FDocument.Pieces.Count
      Else
        RaiseError('FindPoint', 'Cut Point is not clean');
    End;
  End;
End;

Procedure TWPOperator.MoveContent(iDestination : Integer);
Var
  iStart : Integer;
  iLoop : Integer;
Begin
  Assert(CheckCondition(FStatus = osRanging, 'CleanRange', 'Cannot move content when not in ranging mode'));
  DeleteRange(FCurrentOp);

  If iDestination > FCurrentOp.Start Then
    iDestination := iDestination - (FCurrentOp.CommencementFinish - FCurrentOp.Start);

  FCurrentOp.MoveDestination := iDestination;
  MakeCleanCut(iDestination);
  iStart := FindPoint(iDestination);

  For iLoop := 0 To FCurrentOp.OriginalPieces.Count - 1 Do
    FDocument.Pieces.Insert(iStart + iLoop, FCurrentOp.OriginalPieces[iLoop].Clone);
  FStatus := osMoved;
  FRendererRange.Clear;
End;


Function TWPOperator.EndOfMovedRange : Integer;
Begin
  Result := FCurrentOp.MoveDestination + FCurrentOp.OriginalLength;
End;


Procedure TWPOperator.DeleteRange(oOperation : TWPOperation);
Var
  iStart : Integer;
  iEnd : Integer;
Begin
  CleanRange(oOperation);

  If oOperation.Start <> oOperation.CommencementFinish Then
    Begin
    iStart := FindPoint(oOperation.Start);
    iEnd := FindPoint(oOperation.CommencementFinish);
    If iEnd > iStart Then
    Begin
      FDocument.Pieces.DeleteRange(iStart, iEnd-1);
      oOperation.DeletedAmount := oOperation.CommencementFinish - oOperation.Start;
    End;
    End
  Else
    MakeCleanCut(oOperation.Start);
End;

Procedure TWPOperator.DeleteRange;
Begin
  Assert(CheckCondition(FStatus = osRanging, 'DeleteRange', 'Cannot clean range when not in ranging mode'));
  DeleteRange(FCurrentOp);
End;

Procedure TWPOperator.InsertPiece(oPiece : TWPWorkingDocumentPiece);
Begin
  Assert(CheckCondition(FStatus = osChanging, 'InsertPiece', 'Cannot Add content unless in changing mode'));
  FDocument.InsertAtOffset(oPiece, FCurrentOP.Start);
  FCurrentOp.InsertedPieces.Insert(0, oPiece.Link);
  FCurrentOp.Add(Length(oPiece.LogicalText), oPiece.LogicalText);
End;

Procedure TWPOperator.InsertPiece(oPiece, oBefore : TWPWorkingDocumentPiece);
Var
  iIndex : Integer;
  oTracker : TWPWorkingDocumentPieceTracker;
Begin
  Assert(CheckCondition(FStatus = osChanging, 'InsertPiece', 'Cannot Add content unless in changing mode'));
  iIndex := FDocument.Pieces.IndexByReference(oBefore);
  FDocument.Pieces.Insert(iIndex, oPiece);
  oTracker := TWPWorkingDocumentPieceTracker.Create;
  Try
    oTracker.Piece := oPiece.Link;
    oTracker.Offset := oBefore.Metrics.Position;
    FCurrentOp.TablePieces.Add(oTracker.Link);
  Finally
    oTracker.Free;
  End;
  FCurrentOp.Add(Length(oPiece.LogicalText), oPiece.LogicalText);
End;


Function TWPOperator.PieceBeforeAppend : TWPWorkingDocumentPiece;
Var
  iOffset, iIndex : Integer;
Begin
  Assert(CheckCondition(FStatus In [osChanging, osExtending], 'PieceBeforeAppend', 'Cannot Add content unless in changing mode'));
  If FDocument.GetPieceByPosition(FCurrentOP.Start + FCurrentOp.AddedAmount, Result, iOffset, iIndex) And (iIndex > 0) Then
    Result := FDocument.Pieces[iIndex - 1]
  Else
    Result := Nil;
End;


Function TWPOperator.DirectAppendIndex : Integer;
Var
  oExist : TWPWorkingDocumentPiece;
  iInternal : Integer;
  iIndex : Integer;
Begin
  Result := 0;
  If Not FDocument.GetPieceByPosition(FCurrentOP.Start + FCurrentOp.AddedAmount, oExist, iInternal, iIndex) Then
    RaiseError('DirectAppendIndex', 'Offset not found')
  Else If iInternal = 0 Then
    Result := iIndex
  Else If iInternal < Length(oExist.LogicalText) Then
    RaiseError('DirectAppendIndex', 'Cannot insert in the middle of a content piece ('+IntegerToString(iInternal)+'/'+IntegerToString(Length(oExist.LogicalText))+' at '+IntegerToString(iIndex)+')')
  Else
    Result := iIndex+1;
  FDirectCount := 0;
  FDirectText := '';
End;


Function TWPOperator.DirectAppendPiece(oPiece: TWPWorkingDocumentPiece; iIndex : Integer) : Integer;
Begin
  Result := iIndex + 1;
  FDocument.Pieces.Insert(iIndex, oPiece);
  Inc(FDirectCount, Length(oPiece.LogicalText));
  FDirectText := FDirectText + oPiece.LogicalText;
  FCurrentOp.InsertedPieces.Add(oPiece.Link);
End;


Procedure TWPOperator.EndDirectAppend;
Begin
  FCurrentOp.Add(FDirectCount, FDirectText);
End;


Procedure TWPOperator.AppendPiece(oPiece: TWPWorkingDocumentPiece);
Begin
  Assert(CheckCondition(FStatus In [osChanging, osExtending], 'AppendPiece', 'Cannot Add content unless in changing mode'));
  // if the previous piece has an annotation, and we can have an annotation, let us have one
  if (oPiece.PieceType in INLINE_PIECE_TYPES) Then
    oPiece.AnnotationId := FDocument.GetByOffset(FCurrentOP.Start + FCurrentOp.AddedAmount).AnnotationId;
  FDocument.InsertAtOffset(oPiece, FCurrentOP.Start + FCurrentOp.AddedAmount);
  FCurrentOp.InsertedPieces.Add(oPiece.Link);
  FCurrentOp.Add(Length(oPiece.LogicalText), oPiece.LogicalText);
End;


Procedure TWPOperator.RemovePiece(oPiece : TWPWorkingDocumentPiece);
Var
  oTracker : TWPWorkingDocumentPieceTracker;
Begin
  Assert(CheckCondition((FCurrentOp.OpType = otTrim) or (FCurrentOp.OpType = otChange), 'RemovePiece', 'Can only remove a piece in a trim operation'));
  Assert(CheckCondition(FStatus = osChanging, 'RemovePiece', 'Cannot Remove content unless in ranging or deleting mode'));

  // check piece is in range
  oTracker := TWPWorkingDocumentPieceTracker.Create;
  Try
    oTracker.Piece := oPiece.Link;
    oTracker.Offset := oPiece.Metrics.Position;
    FCurrentOp.RemovedPieces.Add(oTracker.Link);
  Finally
    oTracker.Free;
  End;

  FCurrentOp.Remove(oPiece.Metrics.CharCount, oPiece.LogicalText);
  FDocument.Pieces.DeleteByReference(oPiece);
End;


Procedure TWPOperator.SetClosingRange(iEnd : Integer);
Begin
  Assert(CheckCondition(FStatus In [osChanging], 'SetClosingRange', 'Cannot Set Closing Range unless in changing mode'));
  FCurrentOp.TerminationFinish := iEnd;
End;


Procedure TWPOperator.FinishOperation;
Var
  oPieces : TWPWorkingDocumentPieces;
  iLoop : Integer;
Begin
  Assert(CheckCondition(FStatus <> osNone, 'FinishOperation', 'Cannot finish operation when none exists'));
  FStatus := osNone;
  Case FCurrentOp.OpType Of
    otChange : FCurrentOp.TerminationFinish := FCurrentOp.CommencementFinish;

    otInsert,
    otDelete,
    otCharInsert,
    otMove :
      FCurrentOp.TerminationFinish := FCurrentOp.Start + FCurrentOp.AddedAmount;

    otTable : ; // nothing, will be set manually

    otTrim : FCurrentOp.TerminationFinish := FCurrentOp.CommencementFinish + FCurrentOp.AddedAmount;
  End;

  FLastChange := UniversalDateTime;
  If Assigned(FOnDo) Then
    FOnDo(FCurrentOp);
  If Assigned(FOnDeleteContent) Then
    If FCurrentOp.OpType = otTrim Then
    Begin
      For iLoop := FCurrentOp.RemovedPieces.Count - 1 DownTo 0 Do
      Begin
        oPieces := TWPWorkingDocumentPieces.Create;
        Try
          oPieces.Hooking := False;
          oPieces.Add(FCurrentOp.RemovedPieces[iLoop].Piece.Link);
          FOnDeleteContent(Self, FCurrentOp.RemovedPieces[iLoop].Offset, oPieces);
        Finally
          oPieces.Free;
        End;
      End;
    End
    Else If (FCurrentOp.DeletedAmount > 0) And (Not FCurrentOp.ReIterating) Then
      FOnDeleteContent(Self, FCurrentOp.Start, FCurrentOp.OriginalPieces);
  If Assigned(FOnInsertContent) Then
    If FCurrentOp.OpType = otTable Then
    Begin
      For iLoop := FCurrentOp.TablePieces.Count - 1 DownTo 0 Do
      Begin
        oPieces := TWPWorkingDocumentPieces.Create;
        Try
          oPieces.Hooking := False;
          oPieces.Add(FCurrentOp.TablePieces[iLoop].Piece.Link);
          FOnInsertContent(Self, FCurrentOp.TablePieces[iLoop].Offset, oPieces);
        Finally
          oPieces.Free;
        End;
      End;
    End
    Else If FCurrentOp.OpType = otMove Then
      FOnInsertContent(Self, FCurrentOp.MoveDestination, FCurrentOp.OriginalPieces)
    Else If (FCurrentOp.AddedAmountThisIteration > 0) Or (FCurrentOp.InsertedPieces.Count > 0) Then
      FOnInsertContent(Self, FCurrentOp.Start + (FCurrentOp.AddedAmount - FCurrentOp.AddedAmountThisIteration), FCurrentOp.InsertedPieces);

  If FCurrentOp.OpType <> otMove Then
    FRendererRange.Update(FCurrentOp.Start, IntegerMax(FCurrentOp.TerminationFinish, FCurrentOp.CommencementFinish));
  FCurrentOp := Nil;
  MergeDocument;
  If MaxUndoDepth <> 0 Then
    FUndoStack.Trim(MaxUndoDepth);
  FLastAction := getTickCount;
End;

Procedure TWPOperator.CloseLastOperation;
Begin
  FUndoStack.CloseLast;
End;

Function TWPOperator.EndOfRange: Integer;
Begin
  If FCurrentOp.OpType = otTrim Then
    Result := FCurrentOp.CommencementFinish + FCurrentOp.AddedAmount
  Else
    Result := FCurrentOp.Start + FCurrentOp.AddedAmount;
End;

Function TWPOperator.StartOfRange: Integer;
Begin
  Result := FCurrentOp.Start;
End;

Procedure TWPOperator.AbortOperation;
Begin

End;

Procedure TWPOperator.MergeDocument;
Var
  iLoop : Integer;
  oOne : TWPWorkingDocumentTextPiece;
  oTwo : TWPWorkingDocumentTextPiece;
Begin
  For iLoop := FDocument.Pieces.Count - 2 DownTo 0 Do
    If (FDocument.Pieces[iLoop].PieceType = ptText) And
       (FDocument.Pieces[iLoop + 1].PieceType = ptText) Then
      Begin
      oOne := TWPWorkingDocumentTextPiece(FDocument.Pieces[iLoop]);
      oTwo := TWPWorkingDocumentTextPiece(FDocument.Pieces[iLoop + 1]);
      If (Not IsWordBreak(oOne.content)) And (Not IsWordBreak(oTwo.content)) And oOne.AnnotationMatches(oTwo) And oOne.StyleMatches(oTwo) And (Length(oOne.content) + Length(oTwo.content) < MAX_WORD_LENGTH)
          And (oOne.DrawnFont = '') and (oTwo.DrawnFont = '') Then
        Begin
        oOne.Content := oOne.Content + oTwo.Content;
        oOne.SpellState := scsNotChecked;
        FDocument.Pieces.DeleteByIndex(iLoop+1);
        End;
      End;
End;

Procedure TWPOperator.CleanEndRange(oOperation : TWPOperation);
Begin
  MakeCleanCut(oOperation.Start);
  If oOperation.TerminationFinish <> oOperation.Start Then
    MakeCleanCut(oOperation.TerminationFinish);
End;

Procedure TWPOperator.DeleteEndRange(oOperation : TWPOperation);
Var
  iStart : Integer;
  iEnd : Integer;
Begin
  If oOperation.TerminationFinish <> oOperation.Start Then
    Begin
    iStart := FindPoint(oOperation.Start);
    iEnd := FindPoint(oOperation.TerminationFinish);
    MarkTableStructureForUpdate(iStart, iEnd - 1);
    FDocument.Pieces.DeleteRange(iStart, iEnd-1);
    End;
End;

Procedure TWPOperator.InsertContent(oOperation : TWPOperation);
Var
  iStart : Integer;
  iLoop : Integer;
Begin
  If oOperation.OriginalPieces.Count > 0 Then
  Begin
    iStart := FindPoint(oOperation.Start);
    For iLoop := 0 To oOperation.OriginalPieces.Count - 1 Do
      FDocument.Pieces.Insert(iStart + iLoop, oOperation.OriginalPieces[iLoop].Clone);
    MarkTableStructureForUpdate(iStart, iStart + oOperation.OriginalPieces.Count - 1);
  End;
End;

Procedure TWPOperator.CutMovedContent(oOperation : TWPOperation);
Var
  iStart : Integer;
  iEnd : Integer;
  iLoop : Integer;
Begin
  MakeCleanCut(oOperation.MoveDestination);
  MakeCleanCut(oOperation.MoveDestination + oOperation.OriginalLength);
  iStart := FindPoint(oOperation.MoveDestination);
  iEnd := FindPoint(oOperation.MoveDestination + oOperation.OriginalLength);
  For iLoop := iStart To iEnd - 1 Do
    oOperation.ModifiedPieces.Add(FDocument.Pieces[iLoop].Clone);

  MarkTableStructureForUpdate(iStart, iEnd - 1);
  FDocument.Pieces.DeleteRange(iStart, iEnd-1);
End;

Procedure TWPOperator.InsertRedoContent(oOperation : TWPOperation);
Var
  iStart : Integer;
  iLoop : Integer;
Begin
  If oOperation.OpType = otMove Then
  Begin
    MakeCleanCut(oOperation.MoveDestination);
    iStart := FindPoint(oOperation.MoveDestination)
  End
  Else
    iStart := FindPoint(oOperation.Start);
  For iLoop := 0 To oOperation.ModifiedPieces.Count - 1 Do
    FDocument.Pieces.Insert(iStart + iLoop, oOperation.ModifiedPieces[iLoop].Clone);
  oOperation.ModifiedPieces.Clear;

  MarkTableStructureForUpdate(iStart);
End;

Procedure TWPOperator.CopyContent(oOperation : TWPOperation);
Var
  iLoop : Integer;
  iStart : Integer;
  iEnd : Integer;
Begin
  oOperation.OriginalPieces.Clear;
  iStart := FindPoint(oOperation.Start);
  iEnd := FindPoint(oOperation.CommencementFinish);
  For iLoop := iStart To iEnd - 1 Do
    oOperation.OriginalPieces.Add(FDocument.Pieces[iLoop].Clone);
End;

Procedure TWPOperator.CopyUndoContent(oOperation : TWPOperation);
Var
  iStart : Integer;
  iEnd : Integer;
  iLoop : Integer;
Begin
  iStart := oOperation.Start;
  iEnd := oOperation.TerminationFinish;

  If iEnd <> iStart Then
    Begin
    iStart := FindPoint(iStart);
    iEnd := FindPoint(iEnd);
    oOperation.ModifiedPieces.Clear;
    For iLoop := iStart To iEnd - 1 Do
      Begin
      oOperation.ModifiedPieces.Add(FDocument.Pieces[iLoop].Clone);
      oOperation.ModifiedPieces[oOperation.ModifiedPieces.count - 1].Maps.Clear;
      End;
    End;
End;


Function TWPOperator.GetRendererRange : TOperationRange;
Begin
  Assert(Invariants('GetRendererRange', FRendererRange, TOperationRange, 'RendererRange'));
  Result := FRendererRange;
End;


Function TWPOperator.GetDocument : TWPWorkingDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPWorkingDocument, 'Document'));
  Result := FDocument;
End;


Procedure TWPOperator.SetPriorCursor(sCursor: String);
Begin
  Assert(CheckCondition(FStatus = osRanging, 'ExpandRange', 'Cannot Set prior cursor when not in ranging mode'));
  FCurrentOp.UndoCursor := sCursor;
End;


Procedure TWPOperator.ClearUndo;
Begin
  FUndoStack.Clear;
  FRedoStack.Clear;
  FLastChange := 0;
End;


function TWPOperator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStyles.sizeInBytes);
  inc(result, FSpeller.sizeInBytes);
  inc(result, FDocument.sizeInBytes);
  inc(result, FUndoStack.sizeInBytes);
  inc(result, FRedoStack.sizeInBytes);
  inc(result, FCurrentOp.sizeInBytes);
  inc(result, FRendererRange.sizeInBytes);
  inc(result, (FDirectText.length * sizeof(char)) + 12);
end;

Constructor TOperationRange.Create;
Begin
  Inherited;
  Clear;
End;


Procedure TOperationRange.Update(iStart, iStop : Integer);
Begin
  If FValid Then
  Begin
    FStart := IntegerMin(FStart, iStart);
    FStop := IntegerMax(FStop, iStop)
  End
  Else
  Begin
    FValid := True;
    FStart := iStart;
    FStop := iStop;
  End;
End;


Procedure TOperationRange.Clear;
Begin
  FValid := False;
End;


Function TOperationRange.Valid : Boolean;
Begin
  Result := Assigned(Self) And FValid;
End;


function TOperationRange.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TWPOperator.RedoType: TWPOperationType;
Begin
  If FRedoStack.IsEmpty Then
    Result := otChange
  Else
    Result := FRedoStack.Current.OpType;
End;

Function TWPOperator.UndoType: TWPOperationType;
Begin
  If FUndoStack.IsEmpty Then
    Result := otChange
  Else
    Result := FUndoStack.Current.OpType;
End;

procedure TWPOperator.MarkChange;
begin
  FLastChange := UniversalDateTime;
end;

Procedure TWPOperator.MarkTableStructureForUpdate(Const iIndex: Integer);
Var
  iLoop: Integer;
Begin
  iLoop := iIndex;
  While (iLoop >= 0) Do
  Begin
    Case Document.Pieces[iLoop].PieceType Of
      ptTableStop:      Break;
      ptTableStart:
        Begin
          TWPWorkingDocumentTableStartPiece(Document.Pieces[iLoop]).StructureDirty := True;
          Break;
        End;
      Else      // ignore the rest
        Dec(iLoop);
    End;
  End;
End;


Procedure TWPOperator.MarkTableStructureForUpdate(Const iChangeStart, iChangeEnd: Integer);
Var
  iLoop, iStart, iEnd: Integer;
Begin
  If iChangeStart > iChangeEnd Then
    Begin
      iStart := iChangeEnd;
      iEnd := iChangeStart;
    End
  Else
    Begin
      iEnd := iChangeEnd;
      iStart := iChangeStart;
    End;

  For iLoop := iStart To iEnd Do
  Begin
    If Document.Pieces[iLoop].PieceType In [ptRowStart, ptRowStop, ptCellStart, ptCellStop] Then
    Begin
      MarkTableStructureForUpdate(iStart);
      Break;
    End;
  End;
End;

(*
procedure TWPOperator.AppendPieceAtEnd(oPiece: TWPWorkingDocumentPiece);
{Var
  iOffset, iIndex : Integer;
  piece : TWPWorkingDocumentPiece;
Begin
  Assert(CheckCondition(FStatus In [osChanging, osExtending], 'PieceBeforeAppend', 'Cannot Add content unless in changing mode'));
  If FDocument.GetPieceByPosition(FCurrentOP.CommencementFinish + FCurrentOP.AddedAmount, piece, iOffset, iIndex) And (iOffset = 0) Then
    InsertPiece(oPiece, piece);}
Begin
  Assert(CheckCondition(FStatus In [osChanging, osExtending], 'AppendPiece', 'Cannot Add content unless in changing mode'));
  // if the previous piece has an annotation, and we can have an annotation, let us have one
  if (oPiece.PieceType in INLINE_PIECE_TYPES) Then
    oPiece.AnnotationId := FDocument.GetByOffset(FCurrentOP.CommencementFinish + FCurrentOp.AddedAmount).AnnotationId;
  FDocument.InsertAtOffset(oPiece, FCurrentOP.CommencementFinish + FCurrentOp.AddedAmount);
  FCurrentOp.InsertedPieces.Add(oPiece.Link);
  FCurrentOp.Add(Length(oPiece.LogicalText), oPiece.LogicalText);
end;
*)


procedure TWPOperator.AppendDeletedPieces;
var
  i : integer;
begin
  for i := 0 to FCurrentOp.OriginalPieces.count - 1 do
    AppendPiece(FCurrentOp.OriginalPieces[i].clone);
end;

{ TWPOperations }

Function TWPOperations.GetOperation(iIndex: Integer): TWPOperation;
Begin
  Result := TWPOperation(ObjectByIndex[iindex]);
End;

Function TWPOperations.ItemClass: TFslObjectClass;
Begin
  Result := TWPOperation;
End;

{ TWPOperationStack }

Procedure TWPOperationStack.CloseLast;
Begin
  If Count > 0 Then
    Current.Closed := True;
End;

Function TWPOperationStack.Current: TWPOperation;
Begin
  Assert(CheckCondition(Count > 0, 'Current', 'Stack is empty'));
  Result := Operation[Count-1];
End;

Function TWPOperationStack.LastUnClosed: Boolean;
Begin
  Result := (Count > 0) And (Not Current.Closed);
End;

Procedure TWPOperationStack.Pop;
Begin
  Assert(CheckCondition(Count > 0, 'Pop', 'Stack is empty'));
  DeleteByIndex(count-1);
End;

Procedure TWPOperationStack.Push(oOp: TWPOperation);
Begin
  Add(oOp);
End;

Procedure TWPOperationStack.Trim(iMax : Integer);
Begin
  While Count > iMax Do
    DeleteByIndex(0);
End;

{ TWPOperation }

Destructor TWPOperation.Destroy;
Begin
  FOriginalPieces.Free;
  FModifiedPieces.Free;
  FInsertedPieces.Free;
  FRemovedPieces.Free;
  FTablePieces.Free;
  FUndoCursors.Free;
  Inherited;
End;

Function TWPOperation.Link: TWPOperation;
Begin
  Result := TWPOperation(Inherited Link);
End;

Procedure TWPOperation.Assign(oSource: TFslObject);
Begin
  Inherited;
  FOpType := TWPOperation(oSource).FOpType;
  FClosed := TWPOperation(oSource).FClosed;
  FStart := TWPOperation(oSource).FStart;
  FCommencementFinish := TWPOperation(oSource).FCommencementFinish;
  FTerminationFinish := TWPOperation(oSource).FTerminationFinish;
  FMoveDestination := TWPOperation(oSource).FMoveDestination;
  FAddedAmount := TWPOperation(oSource).FAddedAmount;
  FAddedAmountThisIteration := TWPOperation(oSource).FAddedAmountThisIteration;
  FUndoCursor := TWPOperation(oSource).FUndoCursor;
  FRedoCursor := TWPOperation(oSource).FRedoCursor;
  FOriginalPieces.Assign(TWPOperation(oSource).FOriginalPieces);
  FModifiedPieces.Assign(TWPOperation(oSource).FModifiedPieces);
  FInsertedPieces.Assign(TWPOperation(oSource).FInsertedPieces);
  FRemovedPieces.Assign(TWPOperation(oSource).FRemovedPieces);
  FTablePieces.Assign(TWPOperation(oSource).FTablePieces);
End;


Function TWPOperation.asText : String;
Begin
  If FClosed Then
    Result := '  Type: '+NAMES_WPOPERATIONTYPE[FOpType]+ '(Closed)'+cReturn
  Else
    Result := '  Type: '+NAMES_WPOPERATIONTYPE[FOpType]+cReturn;
  Result := Result +
    '  Range at Start: '+IntegerToString(FStart) + '-' + IntegerToString(FCommencementFinish)+cReturn+
    '  Range at End  : '+IntegerToString(FStart) + '-' + IntegerToString(FTerminationFinish)+cReturn;

  Case FOpType Of
    otMove : Result := Result + '  Move : '+IntegerToString(FMoveDestination);
    otInsert, otCharInsert : Result := Result + '  Added : ' + IntegerToString(FAddedAmount);
  End;

  Result := Result +
    '  Undo Cursor: '+FUndoCursor+cReturn+
    '  Redo Cursor: '+FRedoCursor+cReturn+
    '  Original Content: '+FOriginalPieces.AsText+cReturn+
    '  Modified Content   : '+FModifiedPieces.AsText + cReturn+
    '  Inserted Content   : '+FInsertedPieces.AsText + cReturn;
End;


Function TWPOperation.GetOriginalPieces: TWPWorkingDocumentPieces;
Begin
  If Not Assigned(FOriginalPieces) Then
    FOriginalPieces := TWPWorkingDocumentPieces.Create;
  Result := FOriginalPieces;
End;

Function TWPOperation.GetModifiedPieces: TWPWorkingDocumentPieces;
Begin
  If Not Assigned(FModifiedPieces) Then
    FModifiedPieces := TWPWorkingDocumentPieces.Create;
  Result := FModifiedPieces;
End;

Function TWPOperation.GetInsertedPieces: TWPWorkingDocumentPieces;
Begin
  If Not Assigned(FInsertedPieces) Then
  Begin
    FInsertedPieces := TWPWorkingDocumentPieces.Create;
    FInsertedPieces.Hooking := False;
  End;
  Result := FInsertedPieces;
End;

Function TWPOperation.GetRemovedPieces: TWPWorkingDocumentPieceTrackers;
Begin
  If Not Assigned(FRemovedPieces) Then
  Begin
    FRemovedPieces := TWPWorkingDocumentPieceTrackers.Create;
    FRemovedPieces.SortedByOffset;
  End;
  Result := FRemovedPieces;
End;

Function TWPOperation.GetTablePieces: TWPWorkingDocumentPieceTrackers;
Begin
  If Not Assigned(FTablePieces) Then
  Begin
    FTablePieces := TWPWorkingDocumentPieceTrackers.Create;
    FTablePieces.SortedByOffset;
  End;
  Result := FTablePieces;
End;

Procedure TWPOperation.SetOriginalPieces(Const Value: TWPWorkingDocumentPieces);
Begin
  FOriginalPieces.Free;
  FOriginalPieces := Value;
End;

Procedure TWPOperation.SetModifiedPieces(Const Value: TWPWorkingDocumentPieces);
Begin
  FModifiedPieces.Free;
  FModifiedPieces := Value;
End;

Procedure TWPOperation.SetInsertedPieces(Const Value: TWPWorkingDocumentPieces);
Begin
  FInsertedPieces.Free;
  FInsertedPieces := Value;
  FInsertedPieces.Hooking := False;
End;

Procedure TWPOperation.SetRemovedPieces(Const Value: TWPWorkingDocumentPieceTrackers);
Begin
  FRemovedPieces.Free;
  FRemovedPieces := Value;
  FRemovedPieces.SortedByOffset;
End;

Procedure TWPOperation.SetTablePieces(Const Value: TWPWorkingDocumentPieceTrackers);
Begin
  FTablePieces.Free;
  FTablePieces := Value;
  FTablePieces.SortedByOffset;
End;

Procedure TWPOperation.SetMoveDestination(Const Value : Integer);
Begin
  Assert(CheckCondition(OpType = otMove, 'SetMoveDestination', 'Not a Move operation'));
  FMoveDestination := Value;
End;


Function TWPOperation.OriginalLength : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  If Assigned(FOriginalPieces) Then
    For iLoop := 0 To FOriginalPieces.Count - 1 Do
      Inc(Result, FOriginalPieces[iLoop].Metrics.CharCount);
End;


Function TWPOperation.GetUndoCursors: TFslStringIntegerMatch;
Begin
  If FUndoCursors = Nil Then
  Begin
    FUndoCursors := TFslStringIntegerMatch.Create;
    FUndoCursors.Forced := True;
    FUndoCursors.SortedByValue;
  End;
  Result := FUndoCursors;
End;

Function TWPOperation.HasUndoCursors: Boolean;
Begin
  Result := Assigned(FUndoCursors);
End;

Procedure TWPOperation.ReIterate;
Begin
  FAddedAmountThisIteration := 0;
  FReiterating := True;
  FAddedTextThisIteration := '';
  InsertedPieces.Clear;
End;

Procedure TWPOperation.Add(iAmount: Integer; Const sText : String);
Begin
  Inc(FAddedAmount, iAmount);
  Inc(FAddedAmountThisIteration, iAmount);
  FAddedText := FAddedText + sText;
  FAddedTextThisIteration := FAddedTextThisIteration + sText;
End;

Procedure TWPOperation.Remove(iAmount: Integer; Const sText : String);
Var
  iLen : Integer;
Begin
  Dec(FAddedAmount, iAmount);
  Dec(FAddedAmountThisIteration, iAmount);
  iLen := Length(sText);
  Delete(FAddedText, Length(sText) - iLen, iLen);
  Delete(FAddedTextThisIteration, Length(sText) - iLen, iLen);
End;

Function TWPOperation.GetUndoText: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To FOriginalPieces.Count - 1 Do
    Result := Result + FOriginalPieces[iLoop].LogicalText;
End;

function TWPOperation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUndoCursor.length * sizeof(char)) + 12);
  inc(result, (FRedoCursor.length * sizeof(char)) + 12);
  inc(result, FUndoCursors.sizeInBytes);
  inc(result, FOriginalPieces.sizeInBytes);
  inc(result, FModifiedPieces.sizeInBytes);
  inc(result, FInsertedPieces.sizeInBytes);
  inc(result, FRemovedPieces.sizeInBytes);
  inc(result, FTablePieces.sizeInBytes);
  inc(result, (FAddedText.length * sizeof(char)) + 12);
  inc(result, (FAddedTextThisIteration.length * sizeof(char)) + 12);
end;

{ TWPSelection }

Procedure TWPSelection.Assign(oSource: TFslObject);
Begin
  Inherited;
  FSelStart := TWPSelection(oSource).FSelStart;
  FCursor := TWPSelection(oSource).FCursor;
  FSelEnd := TWPSelection(oSource).FSelEnd;
  FDesired := TWPSelection(oSource).FDesired;
End;

Function TWPSelection.Clone: TWPSelection;
Begin
  Result := TWPSelection(Inherited Clone);
End;

Function TWPSelection.HasSelection: Boolean;
Begin
  Result := (FSelStart <> -1);
End;

Function TWPSelection.Link: TWPSelection;
Begin
  Result := TWPSelection(Inherited Link);
End;

Procedure TWPSelection.Reset;
Begin
  MoveTo(0);
End;

Function TWPSelection.MoveTo(Const iPosition : Integer) : Boolean;
Begin
  Result := HasSelection Or (FCursor <> iPosition);
  If Result Then
  Begin
    FCursor := iPosition;
    FSelStart := -1;
    FSelEnd := -1;
    FDesired := -1;
  End;
End;

Function TWPSelection.SelectTo(Const iPosition, iDesired : Integer) : Boolean;
Begin
  If iDesired <> iPosition Then
    FDesired := iDesired
  Else
    FDesired := -1;
  If Cursor = iPosition Then
    Begin
    Result := HasSelection;
    FSelStart := -1;
    FSelEnd := -1;
    End
  Else If Cursor < iPosition Then
    Begin
    Result := Not HasSelection Or (iPosition <> FSelEnd);
    FSelStart := FCursor;
    FSelEnd := iPosition;
    End
  Else
    Begin
    Result := Not HasSelection Or (FSelStart <> iPosition);
    FSelStart := iPosition;
    FSelEnd := FCursor;
    End;
End;

Function TWPSelection.Select(Const iStart, iEnd: Integer) : Boolean;
Begin
  If iStart = iEnd Then
    Result := MoveTo(iStart)
  Else
  Begin
    FDesired := -1;
    Result := Not HasSelection Or (FSelStart <> iStart) Or (FCursor <> iStart) Or (FSelEnd <> iEnd);
    If Result Then
    Begin
      FCursor := iStart;
      FSelStart := iStart;
      FSelEnd := iEnd;
    End;
  End;
End;

Function TWPSelection.InSelection(iPosition : Integer): Boolean;
Begin
  Result := HasSelection And (iPosition >= FSelStart) And (iPosition <= FSelEnd);
End;

Function TWPSelection.SelToMove: Integer;
Begin
  If FDesired <> -1 Then
    Result := FDesired
  Else If FSelStart = FCursor Then
    Result := FSelEnd
  Else
    Result := FSelStart;
End;

Function TWPSelection.SelActive: Integer;
Begin
  If Not HasSelection Then
    Result := FCursor
  Else If FSelStart = FCursor Then
    Result := FSelEnd
  Else
    Result := FSelStart;
End;

Function TWPSelection.Summary: String;
Begin
  If Not HasSelection Then
    Result := IntegerToString(FCursor)
  Else If FDesired <> -1 Then
    Result := IntegerToString(FSelStart) + ' - ' + IntegerToString(FSelEnd)+' ('+IntegerToString(FDesired)+')'
  Else
    Result := IntegerToString(FSelStart) + ' - ' + IntegerToString(FSelEnd)
End;

Procedure TWPSelection.Restore(sSaved: String);
Var
  sItem : String;
Begin
  StringSplit(sSaved, '/', sItem, sSaved);
  FSelStart := StrToIntDef(sItem, -1);
  StringSplit(sSaved, '/', sItem, sSaved);
  FCursor := StrToIntDef(sItem, 0);
  StringSplit(sSaved, '/', sItem, sSaved);
  FSelEnd := StrToIntDef(sItem, -1);
  If sSaved <> '' Then
    FDesired := StrToIntDef(sSaved, -1)
  Else
    FDesired := -1;

End;


Function TWPSelection.Save: String;
Begin
  Result := IntegerToString(FSelStart) + '/' + IntegerToString(FCursor) + '/' + IntegerToString(FSelEnd);
  If FDesired <> -1 Then
    Result := Result + '/'+IntegerToString(FDesired);
End;



Function TWPSelection.GetWorkingSelEnd: Integer;
Begin
  If HasSelection Then
    Result := FSelEnd
  Else
    Result := FCursor;
End;

Function TWPSelection.SetSelEnd(Const iIndex: Integer): Boolean;
Begin
  If HasSelection Then
    If iIndex <= FSelStart Then
      MoveTo(iIndex)
    Else
      FSelEnd := iIndex
  Else
    If iIndex <= FCursor Then
      MoveTo(iIndex)
    Else
      SelectTo(iIndex, -1);
  Result := True;
End;

Function TWPSelection.SetSelStart(Const iIndex: Integer): Boolean;
Begin
  If HasSelection Then
    If iIndex >= FSelEnd Then
      MoveTo(iIndex)
    Else
      FSelStart := iIndex
  Else
    If iIndex >= FCursor Then
      MoveTo(iIndex)
    Else
      SelectTo(iIndex, -1);
  Result := True;
End;

Function TWPSelection.GetWorkingSelStart: Integer;
Begin
  If HasSelection Then
    Result := FSelStart
  Else
    Result := FCursor;
End;

Procedure TWPSelection.AdjustForDeletion(iStart, iLength: Integer);
Begin
  If Not HasSelection Then
  Begin
    If FCursor > iStart Then
      FCursor := IntegerMax(iStart, FCursor - iLength);
  End
  Else If FSelEnd > iStart Then
  Begin
    If FSelStart > iStart Then
      FSelStart := IntegerMax(iStart, FSelStart - iLength);
    FSelEnd := IntegerMax(iStart, FSelEnd - iLength);
    If FSelStart = FSelEnd Then
    Begin
      FCursor := FSelStart;
      FSelStart := -1;
      FSelEnd := -1;
    End;
  End;
End;

Procedure TWPSelection.AdjustForInsertion(iStart, iLength: Integer);
Begin
  If Not HasSelection Then
  Begin
    If FCursor >= iStart Then
      FCursor := FCursor + iLength;
  End
  Else If FSelEnd >= iStart Then
  Begin
    If FSelStart >= iStart Then
      FSelStart := FSelStart + iLength;
    FSelEnd := FSelEnd + iLength;
  End;
End;


function TWPSelection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TWPRangeManager }

Constructor TWPRangeManager.Create;
Begin
  Inherited;
  FList := TWPRangeList.Create;
End;

Destructor TWPRangeManager.Destroy;
Begin
  FList.Free;
  Inherited;
End;

Procedure TWPRangeManager.Add(oRange: TWPRange);
Begin
  FList.Add(oRange.Link);
End;

Procedure TWPRangeManager.Remove(oRange: TWPRange);
Begin
  FList.DeleteByReference(oRange);
End;

Procedure TWPRangeManager.Clear;
Begin
  // anything else to do?
  FList.Clear;
End;

Procedure TWPRangeManager.RangeDo(oOperation : TWPOperation);
Var
  iLoop : Integer;
  oRange : TWPRange;
Begin
  If oOperation.hasUndoCursors Then
    oOperation.UndoCursors.Clear;
  For iLoop := 0 To FList.Count - 1 Do
  Begin
    oRange := FList[iLoop];
    If oRange.Id <> oOperation.RangeId Then
    Begin
      If (Not oOperation.Reiterating) Then
      Begin
        oOperation.UndoCursors.Add(oRange.Selection.Save, oRange.Id);
        oRange.Selection.AdjustForDeletion(oOperation.Start, oOperation.DeletedAmount);
      End;
      oRange.Selection.AdjustForInsertion(oOperation.Start, oOperation.AddedAmountThisIteration);
    End;
  End;
End;

Procedure TWPRangeManager.RangeUndo(oOperation : TWPOperation);
Var
  iLoop : Integer;
  oRange : TWPRange;
Begin
  For iLoop := 0 To FList.Count - 1 Do
  Begin
    oRange := FList[iLoop];
    If oRange.Id <> oOperation.RangeId Then
    Begin
      If oOperation.HasUndoCursors And oOperation.UndoCursors.ExistsByValue(oRange.Id) Then
        oRange.Selection.Restore(oOperation.UndoCursors.GetKeyByValue(oRange.Id))
      Else
      Begin
        // the range didn't exist when the operation occurred. run things backward (rough)
        oRange.Selection.AdjustForDeletion(oOperation.Start, oOperation.AddedAmount);
        oRange.Selection.AdjustForInsertion(oOperation.Start, oOperation.DeletedAmount);
      End;
    End;
  End;
End;

Procedure TWPRangeManager.RangeReDo(oOperation : TWPOperation);
Var
  iLoop : Integer;
  oRange : TWPRange;
Begin
  If oOperation.hasUndoCursors Then
    oOperation.UndoCursors.Clear;
  For iLoop := 0 To FList.Count - 1 Do
  Begin
    oRange := FList[iLoop];
    If oRange.Id <> oOperation.RangeId Then
    Begin
      oOperation.UndoCursors.Add(oRange.Selection.Save, oRange.Id);
      oRange.Selection.AdjustForDeletion(oOperation.Start, oOperation.DeletedAmount);
      oRange.Selection.AdjustForInsertion(oOperation.Start, oOperation.AddedAmount);
    End;
  End;
End;

Const
  PROPS_IMAGEVERTICALALIGNMENT : Array [TWPImageVerticalAlignment] Of String = ('Base Line', 'Bottom', 'Centered', 'Top');
  PROPS_WPSALIGNMENT : Array [TWordProcessorAlignment] Of String = ('', 'Left', 'Middle', 'Right');
  PROPS_WPSPARAGRAPHLISTTYPE : Array [TWPSParagraphListType] Of String = ('', 'None', 'Bullets', 'Numbers');
  PROPS_WPSPARAGRAPHBULLETTYPE : Array [TWPSParagraphBulletType] Of String = ('', 'Disc', 'Circle', 'Square');
  PROPS_WPSPARAGRAPHNUMBERTYPE : Array [TWPSParagraphNumberType] Of String = ('', 'Arabic', 'Lower Alpha', 'Upper Alpha', 'Lower Roman', 'Upper Roman');
  PROPS_WPSPARAGRAPHNUMBERFORMAT : Array [TWPSParagraphNumberFormat] Of String = ('', '[none]', '.', '/', ')', ':', ';');
  PROPS_CELL_VERTICALALIGNMENT : Array [TWordProcessorVerticalAlignment] Of String = ('Bottom', 'Center', 'Top');
  PROPS_TABLE_BORDER : Array [TWPWorkingDocumentTableBorderPolicy] Of String = ('None', 'Grid', 'Lines', 'Inner Lines', 'Dotted Lines', 'Inner Dotted Lines', 'Outer Frame', 'Frame+Lines', 'Custom', 'Fancy');
  PROPS_TEXT_FONT_STATE : Array [TWPSFontState] Of String = ('', 'Normal', 'Superscript', 'Subscript');
  PROPS_SECTION_DISPLAY_TYPE : Array [TWPWorkingDocumentSectionDisplayType] Of String = ('None', 'Line', 'Name');
  PROPS_TEXT_FIELD_STATE : Array [TWPSFontState] Of String = ('Square Brackets', 'None', 'Hints', 'Invisible');

function TWPRangeManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

Function TWPPropertyServer.GetProperties: TWPPropertyList;
Begin
  Result := TWPPropertyList.Create;
  Try
    AddSelectionProperties(Result.AddGroup('Selection'));
    CheckAddTextProperties(Result);
    If TWordProcessor(Owner).PrimaryRange.HasCurrentFieldStart Then
      AddFieldProperties(Result.AddGroup('Field'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentImage Then
      AddImageProperties(Result.AddGroup('Image'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentParagraph Then
      AddParagraphProperties(Result.AddGroup('Paragraph'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentLine Then
      AddLineProperties(Result.AddGroup('Line'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentTableCellStart Then
      AddTableCellProperties(Result.AddGroup('Cell'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentTableRowStart Then
      AddTableRowProperties(Result.AddGroup('Row'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentTableStart Then
      AddTableProperties(Result.AddGroup('Table'));
    If TWordProcessor(Owner).PrimaryRange.HasCurrentSectionStart Then
      AddSectionProperties(Result.AddGroup('Section'));
    AddDocumentProperties(Result.AddGroup('Document'));
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure TWPPropertyServer.CheckAddTextProperties(oList : TWPPropertyList);
Begin
  oList := oList.AddGroup('Text');
  oList.AddEnum(PROP_ID_TEXT_STYLE, True, 'Style', TWordProcessor(Owner).PrimaryRange.Style, TWordProcessor(Owner).WorkingStyles);
  oList.AddEnum(PROP_ID_TEXT_FONT, True, 'Font', TWordProcessor(Owner).PrimaryRange.Font.Name, TWordProcessor(Owner).ListAllFonts);
  oList.AddWord(PROP_ID_TEXT_SIZE, True, 'Size', TWordProcessor(Owner).PrimaryRange.Font.Size);
  oList.AddColour(PROP_ID_TEXT_FOREGROUND, True, 'Foreground', TWordProcessor(Owner).PrimaryRange.Font.Foreground);
  oList.AddColour(PROP_ID_TEXT_BACKGROUND, True, 'Background', TWordProcessor(Owner).PrimaryRange.Font.Background);
  oList.AddTristate(PROP_ID_TEXT_BOLD, True, 'Bold', TWordProcessor(Owner).PrimaryRange.Font.Bold);
  oList.AddTristate(PROP_ID_TEXT_ITALIC, True, 'Italic', TWordProcessor(Owner).PrimaryRange.Font.Italic);
  oList.AddTristate(PROP_ID_TEXT_UNDERLINE, True, 'Underline', TWordProcessor(Owner).PrimaryRange.Font.Underline);
  oList.AddTristate(PROP_ID_TEXT_STRIKETHROUGH, True, 'Strikethrough', TWordProcessor(Owner).PrimaryRange.Font.Strikethrough);
  oList.AddEnum(PROP_ID_TEXT_STATE, True, 'State', ord(TWordProcessor(Owner).PrimaryRange.Font.State), PROPS_TEXT_FONT_STATE);
End;


Procedure TWPPropertyServer.AddFieldProperties(oList : TWPPropertyList);
Var
  oField : TWPWorkingDocumentFieldStartPiece;
Begin
  oField := TWordProcessor(Owner).PrimaryRange.CurrentFieldStart;
  oList.AddString(PROP_ID_FIELD_NAMESPACE, True, 'Namespace', oField.Namespace);
  oList.AddString(PROP_ID_FIELD_NAME, True, 'Name', oField.Name);
  oList.AddTristate(PROP_ID_FIELD_READONLY, True, 'Read Only', oField.ReadOnly);
  oList.AddString(PROP_ID_FIELD_DATA, True, 'Data', oField.RawDataAsText);
  oList.AddEnum(PROP_ID_FIELD_FIXED, True, 'Fixed Format', ord(oField.FixedFormat), TWPDOCUMENTOBJECT_FIXED_FORMAT);
  oList.AddBoolean(PROP_ID_FIELD_DELETABLE, True, 'Deletable', oField.Deletable);
  AddHotspot(oList, oField.HotSpot, PROP_ID_FIELD_HOTSPOT);
End;

Procedure TWPPropertyServer.AddImageProperties(oList : TWPPropertyList);
Var
  oImage : TWPWorkingDocumentImagePiece;
Begin
  oImage := TWordProcessor(Owner).PrimaryRange.CurrentImage;
  oList.addString(PROP_ID_IMAGE_TYPE, False, 'Type', oImage.ImageTypename);
  oList.addString(PROP_ID_IMAGE_NAME, True, 'Name', oImage.Name);
  oList.addInteger(PROP_ID_IMAGE_SRC_HEIGHT, False, 'Image Height', oImage.Image.Height);
  oList.addInteger(PROP_ID_IMAGE_SRC_WIDTH, False, 'Image Width', oImage.Image.Width);
  oList.addInteger(PROP_ID_IMAGE_HEIGHT, True, 'Height', oImage.Height);
  oList.addInteger(PROP_ID_IMAGE_WIDTH, True, 'Width', oImage.Width);
  oList.addInteger(PROP_ID_IMAGE_BORDER, True, 'Border', oImage.Border);
  oList.addColour(PROP_ID_IMAGE_BORDER, True, 'Border Colour', oImage.BorderColour);
  oList.addColour(PROP_ID_IMAGE_TRANSPARENT, Not (oImage.Image Is TFslBitmapGraphic), 'Transparent', oImage.TransparentColour);
  oList.addEnum(PROP_ID_IMAGE_VERTICAL, True, 'Alignment', ord(oImage.VerticalAlignment), PROPS_IMAGEVERTICALALIGNMENT);
End;

Procedure TWPPropertyServer.AddParagraphProperties(oList : TWPPropertyList);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := TWordProcessor(Owner).PrimaryRange.CurrentParagraph;
  oList.addEnum(PROP_ID_PARA_ALIGN, True, 'Alignment', ord(oPara.Format.Align), PROPS_WPSALIGNMENT);
  oList.addEnum(PROP_ID_PARA_LIST, True, 'List Type', ord(oPara.Format.ListType), PROPS_WPSPARAGRAPHLISTTYPE);
  If oPara.Format.ListType = WPSParagraphListTypeBullets Then
    oList.addEnum(PROP_ID_PARA_BULLET, True, 'Bullet Type', ord(oPara.Format.BulletType), PROPS_WPSPARAGRAPHBULLETTYPE);
  If oPara.Format.ListType = WPSParagraphListTypeNumbers Then
  Begin
    oList.addEnum(PROP_ID_PARA_NUMBER_TYPE, True, 'Number Type', ord(oPara.Format.NumberType), PROPS_WPSPARAGRAPHNUMBERTYPE);
    oList.addEnum(PROP_ID_PARA_NUMBER_FORMAT, True, 'Number Format', ord(oPara.Format.NumberType), PROPS_WPSPARAGRAPHNUMBERTYPE);
    oList.addWord(PROP_ID_PARA_NUMBER_FIXED, True, 'Fixed Number', oPara.Format.FixedNumber);
  End;

  oList.addWord(PROP_ID_PARA_LEFT, True, 'Left Margin', oPara.Format.LeftIndent);
  oList.addWord(PROP_ID_PARA_RIGHT, True, 'Right Margin', oPara.Format.RightIndent);
  oList.addWord(PROP_ID_PARA_BOTTOM, True, 'Bottom Margin', oPara.Format.MarginBottom);
End;


Procedure TWPPropertyServer.AddLineProperties(oList : TWPPropertyList);
Var
  oLine : TWPWorkingDocumentBreakPiece;
Begin
  oLine := TWordProcessor(Owner).PrimaryRange.CurrentLine;
  oList.AddEnum(PROP_ID_LINE_ALIGNMENT, True, 'Alignment', ord(oLine.Alignment), PROPS_WPSALIGNMENT);
  oList.AddFloat(PROP_ID_LINE_WIDTH, True, 'Width', oLine.Width);
  oList.addColour(PROP_ID_LINE_COLOUR, True, 'Colour', oLine.PenColour);
  oList.addWord(PROP_ID_LINE_PENWIDTH, True, 'Thickness', oLine.PenWidth);
  oList.AddEnum(PROP_ID_LINE_STYLE, True, 'Style', ord(oLine.PenStyle), ADVPENSTYLE_NAMES);
  oList.AddEnum(PROP_ID_LINE_END, True, 'Ends', ord(oLine.EndStyle), ADVPENENDSTYLE_CODES);
End;

Procedure TWPPropertyServer.AddTableCellProperties(oList : TWPPropertyList);
Var
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  oCell := TWordProcessor(Owner).PrimaryRange.CurrentTableCellStart;
  oList.AddInteger(PROP_ID_CELL_SPAN, False, 'Span', oCell.Span);
  oList.AddFloat(PROP_ID_CELL_WIDTH, True, 'Width', oCell.Width);
  oList.AddColour(PROP_ID_CELL_COLOUR, True, 'Background', oCell.Background);
  oList.AddWord(PROP_ID_CELL_MARGIN_LEFT, True, 'Left Margin', oCell.MarginLeft);
  oList.AddWord(PROP_ID_CELL_MARGIN_RIGHT, True, 'Right Margin', oCell.MarginRight);
  oList.AddWord(PROP_ID_CELL_MARGIN_TOP, True, 'Top Margin', oCell.MarginTop);
  oList.AddWord(PROP_ID_CELL_MARGIN_BOTTOM, True, 'Bottom Margin', oCell.MarginBottom);
  oList.AddEnum(PROP_ID_CELL_ALIGN, False, 'Alignment', ord(oCell.VerticalAlignment), PROPS_CELL_VERTICALALIGNMENT);
  AddTableItem(oList, oCell, PROP_ID_CELL_ITEM);
  AddHotspot(oList, oCell.HotSpot, PROP_ID_CELL_HOTSPOT);
End;

Procedure TWPPropertyServer.AddTableItem(oList : TWPPropertyList; oItem : TWPWorkingDocumentTableItemPiece; iOffset : Integer);
Begin
  oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_LEFT, 'Left Border', oItem.LeftBorder.PropertyDescription);
  oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_TOP, 'Top Border', oItem.TopBorder.PropertyDescription);
  oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_RIGHT, 'Right Border', oItem.RightBorder.PropertyDescription);
  oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_BOTTOM, 'Bottom Border', oItem.BottomBorder.PropertyDescription);

  If (oItem.HasWorkingLeftBorder) Then
    oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_LEFT_WORK, 'Left Wk. Border', oItem.WorkingLeftBorder.PropertyDescription);
  If (oItem.HasWorkingTopBorder) Then
    oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_TOP_WORK, 'Top Wk. Border', oItem.WorkingTopBorder.PropertyDescription);
  If (oItem.HasWorkingRightBorder) Then
    oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_RIGHT_WORK, 'Right Wk. Border', oItem.WorkingRightBorder.PropertyDescription);
  If (oItem.HasWorkingBottomBorder) Then
    oList.AddComplex(iOffset + PROP_ID_TABLE_BORDER_BOTTOM_WORK, 'Bottom Wk. Border', oItem.WorkingBottomBorder.PropertyDescription);
End;

Procedure TWPPropertyServer.AddTableRowProperties(oList : TWPPropertyList);
Var
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oRow := TWordProcessor(Owner).PrimaryRange.CurrentTableRowStart;
  oList.AddColour(PROP_ID_ROW_COLOUR, True, 'Background', oRow.Background);
  oList.AddBoolean(PROP_ID_ROW_HEADER, True, 'Header', oRow.Header);
  oList.AddBoolean(PROP_ID_ROW_BREAK_BEFORE, True, 'BreakBefore', oRow.BreakBefore);
  oList.AddInteger(PROP_ID_ROW_LOWER_PADDING_SIZE, True, 'LowerPaddingSize', oRow.LowerPaddingSize);
  oList.AddColour(PROP_ID_ROW_LOWER_PADDING_COLOUR, True, 'LowerPaddingColour', oRow.LowerPaddingColour);
  AddHotspot(oList, oRow.HotSpot, PROP_ID_ROW_HOTSPOT);
End;

Procedure TWPPropertyServer.AddTableProperties(oList : TWPPropertyList);
Var
  oTable : TWPWorkingDocumentTableStartPiece;
Begin
  oTable := TWordProcessor(Owner).PrimaryRange.CurrentTableStart;
  oList.AddColour(PROP_ID_TABLE_COLOUR, True, 'Background', oTable.Background);
  oList.addEnum(PROP_ID_TABLE_BORDER, True, 'Borders', ord(oTable.BorderPolicy), PROPS_TABLE_BORDER);
  oList.addWord(PROP_ID_TABLE_MARGIN_HORIZ, True, 'Side Margins', oTable.HorizontalMargin);
  oList.addWord(PROP_ID_TABLE_MARGIN_VERT, True, 'Top/Bottom Margins', oTable.VerticalMargin);
  oList.addInteger(PROP_ID_TABLE_COLCOUNT, False, 'Columns', oTable.ColumnCount);
  AddTableItem(oList, oTable, PROP_ID_TABLE_ITEM);
End;

Procedure TWPPropertyServer.AddSectionProperties(oList : TWPPropertyList);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
Begin
  oSection := TWordProcessor(Owner).PrimaryRange.CurrentSectionStart;
  oList.AddName(PROP_ID_SECTION_NAMESPACE, True, 'Name', oSection.Namespace);
  oList.AddName(PROP_ID_SECTION_NAME, True, 'Name', oSection.Name);
  oList.AddName(PROP_ID_SECTION_DATA, True, 'Data', oSection.RawDataAsText);
  oList.AddName(PROP_ID_SECTION_TITLE, True, 'Title', oSection.DisplayName);
  oList.addEnum(PROP_ID_SECTION_TYPE, True, 'Type', ord(oSection.DisplayType), PROPS_SECTION_DISPLAY_TYPE);
End;

Procedure TWPPropertyServer.AddSelectionProperties(oList : TWPPropertyList);
Begin
  If TWordProcessor(Owner).Selection.HasSelection Then
  Begin
    oList.AddInteger(PROP_ID_SEL_START, True, 'Start', TWordProcessor(Owner).Selection.SelStart);
    oList.AddInteger(PROP_ID_SEL_END, True, 'End', TWordProcessor(Owner).Selection.SelEnd);
  End
  Else
    oList.AddInteger(PROP_ID_SEL_CURSOR, True, 'Cursor', TWordProcessor(Owner).Selection.Cursor);
End;


Procedure TWPPropertyServer.SetProperty(oProperty: TWPProperty; Const sValue: String);
Begin
  // 1: error checking. do it all at once for convenience
  Case oProperty.Id Of
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,

    PROP_ID_FIELD_FIXED,
    PROP_ID_FIELD_READONLY,
    PROP_ID_FIELD_NAME,
    PROP_ID_FIELD_DATA,
    PROP_ID_FIELD_DELETABLE:
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentFieldStart Then
        RaiseError('SetProperty', 'Cannot set a field property when there is no field');

    PROP_ID_SEL_START,
    PROP_ID_SEL_END:
      If Not TWordProcessor(Owner).Selection.hasSelection Then
        RaiseError('SetProperty', 'Cannot set the selection start or end when there is no selection');

    // PROP_ID_SEL_CURSOR: nothing to test

    PROP_ID_IMAGE_NAME ,
    PROP_ID_IMAGE_HEIGHT ,
    PROP_ID_IMAGE_WIDTH ,
    PROP_ID_IMAGE_BORDER ,
    PROP_ID_IMAGE_TRANSPARENT ,
    PROP_ID_IMAGE_VERTICAL :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentImage Then
        RaiseError('SetProperty', 'Cannot set an image property when there is no image');

    PROP_ID_PARA_ALIGN ,
    PROP_ID_PARA_LIST ,
    PROP_ID_PARA_BULLET ,
    PROP_ID_PARA_NUMBER_TYPE ,
    PROP_ID_PARA_NUMBER_FORMAT ,
    PROP_ID_PARA_NUMBER_FIXED ,
    PROP_ID_PARA_LEFT ,
    PROP_ID_PARA_RIGHT ,
    PROP_ID_PARA_BOTTOM :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentParagraph Then
        RaiseError('SetProperty', 'Cannot set a paragraph property when there is no paragraph');

    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,
    PROP_ID_CELL_WIDTH ,
    PROP_ID_CELL_COLOUR ,
    PROP_ID_CELL_MARGIN_LEFT ,
    PROP_ID_CELL_MARGIN_RIGHT ,
    PROP_ID_CELL_MARGIN_TOP ,
    PROP_ID_CELL_MARGIN_BOTTOM ,
    PROP_ID_CELL_ALIGN :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentTableCellStart Then
        RaiseError('SetProperty', 'Cannot set a cell property when there is no cell');

    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,
    PROP_ID_ROW_COLOUR ,
    PROP_ID_ROW_HEADER,
    PROP_ID_ROW_BREAK_BEFORE,
    PROP_ID_ROW_LOWER_PADDING_SIZE,
    PROP_ID_ROW_LOWER_PADDING_COLOUR :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentTableRowStart Then
        RaiseError('SetProperty', 'Cannot set a row property when there is no row');

    PROP_ID_TABLE_COLOUR ,
    PROP_ID_TABLE_BORDER ,
    PROP_ID_TABLE_MARGIN_HORIZ ,
    PROP_ID_TABLE_MARGIN_VERT :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentTableStart Then
        RaiseError('SetProperty', 'Cannot set a table property when there is no table');

    PROP_ID_SECTION_NAME,
    PROP_ID_SECTION_NAMESPACE,
    PROP_ID_SECTION_DATA,
    PROP_ID_SECTION_TITLE ,
    PROP_ID_SECTION_TYPE :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentSectionStart Then
        RaiseError('SetProperty', 'Cannot set a section property when there is no section');

    PROP_ID_LINE_ALIGNMENT ,
    PROP_ID_LINE_WIDTH ,
    PROP_ID_LINE_COLOUR ,
    PROP_ID_LINE_PENWIDTH ,
    PROP_ID_LINE_STYLE ,
    PROP_ID_LINE_END :
      If Not TWordProcessor(Owner).PrimaryRange.HasCurrentLine Then
        RaiseError('SetProperty', 'Cannot set a line property when there is no line');

    PROP_ID_TEXT_FONT ,
    PROP_ID_TEXT_SIZE ,
    PROP_ID_TEXT_FOREGROUND ,
    PROP_ID_TEXT_BACKGROUND ,
    PROP_ID_TEXT_BOLD ,
    PROP_ID_TEXT_ITALIC ,
    PROP_ID_TEXT_UNDERLINE ,
    PROP_ID_TEXT_STRIKETHROUGH ,
    PROP_ID_TEXT_STATE :
       ; // not sure what to check

    PROP_ID_DOC_MODIFIED ,
    PROP_ID_DOC_BACKGROUND ,
    PROP_ID_DOC_LOWLIGHT ,
    PROP_ID_DOC_HORIZMARGIN ,
    PROP_ID_DOC_VERTMARGIN ,
    PROP_ID_DOC_SCALE ,
    PROP_ID_DOC_TABLEBORDERS ,
    PROP_ID_DOC_EDITHINTS ,
    PROP_ID_DOC_SPELLING ,
    PROP_ID_DOC_FIELDWRAPPERS ,
    PROP_ID_DOC_ID : ; // nothing to check

  Else
    RaiseError('SetProperty', 'Unknown Property Id '+IntegerToString(oProperty.Id));
  End;


  Case oProperty.Id Of
    PROP_ID_TEXT_STYLE : TWordProcessor(Owner).PrimaryRange.Style := ReadStyleName(sValue);
    PROP_ID_TEXT_FONT : TWordProcessor(Owner).PrimaryRange.ApplyFontName(ReadFontName(sValue));
    PROP_ID_TEXT_SIZE : TWordProcessor(Owner).PrimaryRange.ApplyFontSize(ReadWord(sValue));
    PROP_ID_TEXT_FOREGROUND : TWordProcessor(Owner).PrimaryRange.ApplyForeground(ReadColour(sValue));
    PROP_ID_TEXT_BACKGROUND : TWordProcessor(Owner).PrimaryRange.ApplyBackground(ReadColour(sValue));
    PROP_ID_TEXT_BOLD  : TWordProcessor(Owner).PrimaryRange.ApplyBold(ReadTriState(sValue));
    PROP_ID_TEXT_ITALIC : TWordProcessor(Owner).PrimaryRange.ApplyItalic(ReadTriState(sValue));
    PROP_ID_TEXT_UNDERLINE  : TWordProcessor(Owner).PrimaryRange.ApplyUnderline(ReadTriState(sValue));
    PROP_ID_TEXT_STRIKETHROUGH  : TWordProcessor(Owner).PrimaryRange.ApplyStrikeThrough(ReadTriState(sValue));
    PROP_ID_TEXT_STATE : TWordProcessor(Owner).PrimaryRange.ApplyState(TWPSFontState(ReadEnum(sValue, PROPS_TEXT_FONT_STATE)));

    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,
    PROP_ID_FIELD_NAME,
    PROP_ID_FIELD_FIXED,
    PROP_ID_FIELD_READONLY,
    PROP_ID_FIELD_DATA,
    PROP_ID_FIELD_DELETABLE: SetFieldProperty(oProperty.Id, sValue);

    PROP_ID_SEL_START : TWordProcessor(Owner).PrimaryRange.SelectRange(ReadInteger(sValue, True), TWordProcessor(Owner).selection.SelEnd);
    PROP_ID_SEL_END : TWordProcessor(Owner).PrimaryRange.SelectRange(TWordProcessor(Owner).selection.SelStart, ReadInteger(sValue, True));
    PROP_ID_SEL_CURSOR : TWordProcessor(Owner).PrimaryRange.MoveTo(ReadInteger(sValue, True));

    PROP_ID_IMAGE_NAME ,
    PROP_ID_IMAGE_HEIGHT ,
    PROP_ID_IMAGE_WIDTH ,
    PROP_ID_IMAGE_BORDER ,
    PROP_ID_IMAGE_TRANSPARENT ,
    PROP_ID_IMAGE_VERTICAL : SetImageProperty(oProperty.Id, sValue);

    PROP_ID_PARA_ALIGN ,
    PROP_ID_PARA_LIST ,
    PROP_ID_PARA_BULLET ,
    PROP_ID_PARA_NUMBER_TYPE ,
    PROP_ID_PARA_NUMBER_FORMAT ,
    PROP_ID_PARA_NUMBER_FIXED ,
    PROP_ID_PARA_LEFT ,
    PROP_ID_PARA_RIGHT ,
    PROP_ID_PARA_BOTTOM : SetParagraphProperty(oProperty.Id, sValue);

    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,
    PROP_ID_CELL_WIDTH ,
    PROP_ID_CELL_COLOUR ,
    PROP_ID_CELL_MARGIN_LEFT ,
    PROP_ID_CELL_MARGIN_RIGHT ,
    PROP_ID_CELL_MARGIN_TOP ,
    PROP_ID_CELL_MARGIN_BOTTOM ,
    PROP_ID_CELL_ALIGN : SetTableCellProperty(oProperty.Id, sValue);

    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_URL,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_KEY,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_TITLE,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_LINK,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_HOVER,
    PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE,
    PROP_ID_ROW_COLOUR ,
    PROP_ID_ROW_HEADER,
    PROP_ID_ROW_BREAK_BEFORE,
    PROP_ID_ROW_LOWER_PADDING_SIZE,
    PROP_ID_ROW_LOWER_PADDING_COLOUR : SetTableRowProperty(oProperty.Id, sValue);

    PROP_ID_TABLE_COLOUR ,
    PROP_ID_TABLE_BORDER ,
    PROP_ID_TABLE_MARGIN_HORIZ ,
    PROP_ID_TABLE_MARGIN_VERT : SetTableProperty(oProperty.Id, sValue);

    PROP_ID_SECTION_NAME,
    PROP_ID_SECTION_NAMESPACE,
    PROP_ID_SECTION_DATA,
    PROP_ID_SECTION_TITLE ,
    PROP_ID_SECTION_TYPE : SetSectionProperty(oProperty.Id, sValue);

    PROP_ID_LINE_ALIGNMENT ,
    PROP_ID_LINE_WIDTH ,
    PROP_ID_LINE_COLOUR ,
    PROP_ID_LINE_PENWIDTH ,
    PROP_ID_LINE_STYLE ,
    PROP_ID_LINE_END : SetLineProperty(oProperty.Id, sValue);

    PROP_ID_DOC_MODIFIED,
    PROP_ID_DOC_BACKGROUND,
    PROP_ID_DOC_LOWLIGHT,
    PROP_ID_DOC_HORIZMARGIN,
    PROP_ID_DOC_VERTMARGIN,
    PROP_ID_DOC_SCALE,
    PROP_ID_DOC_TABLEBORDERS,
    PROP_ID_DOC_EDITHINTS,
    PROP_ID_DOC_SPELLING,
    PROP_ID_DOC_FIELDWRAPPERS,
    PROP_ID_DOC_ID : SetDocProperty(oProperty.Id, sValue);
  End;
End;


Procedure TWPPropertyServer.AddHotspot(oList: TWPPropertyList; oHotSpot: TWPHotspot; iOffset : Integer);
Var
  oKeys : TStringList;
  ch : Char;
Begin
  oKeys := TStringList.Create;
  Try
    For ch := 'a' To 'z' Do
      oKeys.Add(ch);
    If Assigned(oHotspot) Then
    Begin
      oList.AddString(iOffset + PROP_ID_HOTSPOT_URL, True, 'URL', oHotSpot.URL);
      oList.AddEnum(iOffset + PROP_ID_HOTSPOT_KEY, True, 'Key', oHotSpot.Key, oKeys);
      oList.AddString(iOffset + PROP_ID_HOTSPOT_TITLE, True, 'Title', oHotSpot.Title);
      oList.AddColour(iOffset + PROP_ID_HOTSPOT_LINK, True, 'Link Colour', oHotSpot.LinkColour);
      oList.AddColour(iOffset + PROP_ID_HOTSPOT_HOVER, True, 'Hover Colour', oHotSpot.HoverColour);
      oList.AddBoolean(iOffset + PROP_ID_HOTSPOT_UNDERLINE, True, 'URL Underline', oHotSpot.LinkUnderline);
    End
    Else
    Begin
      oList.AddString(iOffset + PROP_ID_HOTSPOT_URL, True, 'URL', '');
      oList.AddEnum(iOffset + PROP_ID_HOTSPOT_KEY, True, 'Key', '', oKeys);
      oList.AddString(iOffset + PROP_ID_HOTSPOT_TITLE, True, 'Title', '');
      oList.AddColour(iOffset + PROP_ID_HOTSPOT_LINK, True, 'Link Colour', DEF_COLOUR);
      oList.AddColour(iOffset + PROP_ID_HOTSPOT_HOVER, True, 'Hover Colour', DEF_COLOUR);
      oList.AddString(iOffset + PROP_ID_HOTSPOT_UNDERLINE, True, 'URL Underline', '');
    End;
  Finally
    oKeys.Free;
  End;
End;

Function TWPPropertyServer.ReadFontName(Const sValue : String) : String;
Var
  oList : TStringList;
Begin
  oList := TWordProcessor(Owner).ListAllFonts;
  Try
    If (sValue = '') Or (oList.IndexOf(sValue) > -1) Then
      Result := sValue
    Else
      RaiseError('ReadFontName', 'Font "'+sValue+'" is not known');
  Finally
    oList.Free;
  End;
End;

Function TWPPropertyServer.ReadStyleName(Const sValue : String) : String;
Var
  oList : TWPStyles;
Begin
  oList := TWordProcessor(Owner).WorkingStyles;
  If (sValue = '') Or (oList.ExistsByName(sValue)) Then
    Result := sValue
  Else
    RaiseError('ReadFontName', 'Font "'+sValue+'" is not known');
End;

Function TWPPropertyServer.ReadKey(Const sValue : String) : String;
Begin
  If (sValue = '') Or ((Length(sValue) = 1) And StringIsAlphabetic(sValue[1])) Then
    Result := sValue
  Else
    RaiseError('ReadKey', 'Key "'+sValue+'" is not valid');
End;

Function TWPPropertyServer.ReadWord(Const sValue : String) : Word;
Begin
  If sValue = '' Then
    Result := DEF_WORD
  Else If Not StringIsInteger32(sValue) Then
  Begin
    Result := DEF_WORD;
    RaiseError('ReadInteger',  'Number "'+sValue+'" is not valid')
  End
  Else
    Result := StringToInteger32(sValue);
End;

Function TWPPropertyServer.ReadInteger(Const sValue : String; bNegAllowed : Boolean) : Integer;
Begin
  If sValue = '' Then
    Result := 0
  Else If Not StringIsInteger32(sValue) Then
  Begin
    Result := 0;
    RaiseError('ReadInteger',  'Number "'+sValue+'" is not valid')
  End
  Else
    Result := StringToInteger32(sValue);
  If Not bNegAllowed And (Result < 0) Then
    RaiseError('ReadInteger',  'Negative Number "'+sValue+'" is not valid');
End;

Function TWPPropertyServer.ReadFloat(Const sValue : String; bNonPosAllowed : Boolean) : Real;
Begin
  If Not StringIsNumeric(sValue) Then
  Begin
    Result := 0;
    RaiseError('ReadFloat',  'Number "'+sValue+'" is not valid')
  End
  Else
    Result := StringToReal(sValue);
  If Not bNonPosAllowed And (Result <= 0) Then
    RaiseError('ReadInteger',  ' Non Positive Number "'+sValue+'" is not valid');
End;

Function TWPPropertyServer.ReadColour(Const sValue : String) : TColour;
Begin
  If sValue = '' Then
    Result := DEF_COLOUR
  Else If Not StringIsHTMLColour(sValue) Then
  Begin
    RaiseError('ReadColour',  'Colour "'+sValue+'" is not valid');
    Result := DEF_COLOUR;
  End
  Else
    Result := HTMLColourStringToColour(sValue);
End;

Function TWPPropertyServer.ReadBoolean(Const sValue : String) : Boolean;
Begin
  If sValue = '' Then
    Result := False
  Else If StringEqualsInsensitive(sValue, 'yes') Then
    Result := True
  Else If StringEqualsInsensitive(sValue, 'no') Then
    Result := False
  Else
  Begin
    Result := False;
    RaiseError('ReadBoolean', '"'+sValue+'" is not valid');
  End;
End;

Function TWPPropertyServer.ReadTriState(Const sValue : String) : TWPSTriState;
Begin
  If sValue = '' Then
    Result := tsUnknown
  Else If StringEqualsInsensitive(sValue, 'yes') Then
    Result := tsTrue
  Else If StringEqualsInsensitive(sValue, 'no') Then
    Result := tsFalse
  Else
  Begin
    Result := tsUnknown;
    RaiseError('ReadTriState', '"'+sValue+'" is not valid');
  End;
End;

Function AsCSV(aValues : Array Of String) : String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := Low(aValues) To High(aValues) Do
  Begin
    If iLoop > 0 Then
      Result := Result + ', ';
    Result := Result + aValues[iLoop];
  End
End;

Function TWPPropertyServer.ReadEnum(Const sValue : String; aValues : Array Of String) : Integer;
Begin
  Result := StringArrayIndexOfInsensitive(aValues, sValue);
  If Result = -1 Then
    RaiseError('ReadEnum', 'Value "'+sValue+'" is not valid (must be one of '+AsCSV(aValues)+')');
End;


Procedure TWPPropertyServer.SetFieldProperty(iId : Integer; Const sValue : String);
Var
  oField : TWPWorkingDocumentFieldStartPiece;
Begin
  oField := TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.Clone;
  Try
    Case iId Of
      PROP_ID_FIELD_NAME : oField.Name := sValue;
      PROP_ID_FIELD_NAMESPACE : oField.Namespace := sValue;
      PROP_ID_FIELD_FIXED : oField.FixedFormat := ReadFixedFormatAttribute(sValue);
      PROP_ID_FIELD_READONLY : oField.ReadOnly := ReadTristate(sValue);
      PROP_ID_FIELD_DATA : oField.RawDataAsText := sValue;
      PROP_ID_FIELD_DELETABLE :  oField.Deletable := ReadBoolean(sValue);
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_URL : oField.ForceHotspot.Url := sValue;
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_KEY : oField.ForceHotspot.Key := ReadKey(sValue);
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_TITLE : oField.ForceHotspot.Title := sValue;
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_LINK : oField.ForceHotspot.LinkColour := ReadColour(sValue);
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_HOVER : oField.ForceHotspot.HoverColour := ReadColour(sValue);
      PROP_ID_FIELD_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE : oField.ForceHotspot.LinkUnderline := ReadBoolean(sValue);
    End;

    If oField.HasHotspot And oField.Hotspot.IsEmpty Then
      oField.HasHotspot := False;

    TWordProcessor(Owner).PrimaryRange.SetFieldProperties(oField);
  Finally
    oField.Free;
  End;
End;


Procedure TWPPropertyServer.SetImageProperty(iId : Integer; Const sValue : String);
Var
  oImage : TWPWorkingDocumentImagePiece;
Begin
  oImage := TWordProcessor(Owner).PrimaryRange.CurrentImage.Clone;
  Try
    Case iId Of
      PROP_ID_IMAGE_NAME : oImage.Name := sValue;
      PROP_ID_IMAGE_HEIGHT : oImage.Height := ReadWord(sValue);
      PROP_ID_IMAGE_WIDTH : oImage.Width := ReadWord(sValue);
      PROP_ID_IMAGE_BORDER : oImage.Border := ReadInteger(sValue, False);
      PROP_ID_IMAGE_TRANSPARENT : oImage.TransparentColour := ReadColour(sValue);
      PROP_ID_IMAGE_VERTICAL : oImage.VerticalAlignment := TWPImageVerticalAlignment(ReadEnum(sValue, PROPS_IMAGEVERTICALALIGNMENT));
    End;

    TWordProcessor(Owner).PrimaryRange.SetImageProperties(oImage);
  Finally
    oImage.Free;
  End;
End;

Procedure TWPPropertyServer.SetParagraphProperty(iId : Integer; Const sValue : String);
Var
  oFormat : TWPSParagraphDetails;
Begin
  oFormat := TWordProcessor(Owner).PrimaryRange.CurrentParagraph.Format.Clone;
  Try
    Case iId Of
      PROP_ID_PARA_ALIGN : oFormat.Align := TWordProcessorParagraphAlignment(ReadEnum(sValue, PROPS_WPSALIGNMENT));
      PROP_ID_PARA_LIST : oFormat.ListType := TWPSParagraphListType(ReadEnum(sValue, PROPS_WPSPARAGRAPHLISTTYPE));
      PROP_ID_PARA_BULLET : oFormat.BulletType := TWPSParagraphBulletType(ReadEnum(sValue, PROPS_WPSPARAGRAPHBULLETTYPE));
      PROP_ID_PARA_NUMBER_TYPE : oFormat.NumberType := TWPSParagraphNumberType(ReadEnum(sValue, PROPS_WPSPARAGRAPHNUMBERTYPE));
      PROP_ID_PARA_NUMBER_FORMAT: oFormat.NumberFormat := TWPSParagraphNumberFormat(ReadEnum(sValue, PROPS_WPSPARAGRAPHNUMBERFORMAT));
      PROP_ID_PARA_NUMBER_FIXED: oFormat.FixedNumber := ReadWord(sValue);
      PROP_ID_PARA_LEFT : oFormat.LeftIndent := ReadInteger(sValue, False);
      PROP_ID_PARA_RIGHT : oFormat.RightIndent := ReadInteger(sValue, False);
      PROP_ID_PARA_BOTTOM : oFormat.MarginBottom := ReadInteger(sValue, False);
    End;

    TWordProcessor(Owner).PrimaryRange.ApplyParagraph(oFormat);
  Finally
    oFormat.Free;
  End;
End;


Procedure TWPPropertyServer.SetTableCellProperty(iId : Integer; Const sValue : String);
Var
  oCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  oCell := TWordProcessor(Owner).PrimaryRange.CurrentTableCellStart.Clone;
  Try
    Case iId Of
      PROP_ID_CELL_WIDTH : oCell.Width := ReadFloat(sValue, False);
      PROP_ID_CELL_COLOUR : oCell.Background := ReadColour(sValue);
      PROP_ID_CELL_MARGIN_LEFT : oCell.MarginLeft := ReadWord(sValue);
      PROP_ID_CELL_MARGIN_RIGHT : oCell.MarginRight := ReadWord(sValue);
      PROP_ID_CELL_MARGIN_TOP : oCell.MarginTop := ReadWord(sValue);
      PROP_ID_CELL_MARGIN_BOTTOM : oCell.MarginBottom := ReadWord(sValue);
      PROP_ID_CELL_ALIGN : oCell.VerticalAlignment := TWordProcessorVerticalAlignment(ReadEnum(sValue, PROPS_CELL_VERTICALALIGNMENT));
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_URL : oCell.ForceHotspot.Url := sValue;
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_KEY : oCell.ForceHotspot.Key := ReadKey(sValue);
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_TITLE : oCell.ForceHotspot.Title := sValue;
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_LINK : oCell.ForceHotspot.LinkColour := ReadColour(sValue);
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_HOVER : oCell.ForceHotspot.HoverColour := ReadColour(sValue);
      PROP_ID_CELL_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE : oCell.ForceHotspot.LinkUnderline := ReadBoolean(sValue);
    End;

    If oCell.HasHotspot And oCell.Hotspot.IsEmpty Then
      oCell.HasHotspot := False;

    TWordProcessor(Owner).PrimaryRange.SetTableCellProperties(oCell);
  Finally
    oCell.Free;
  End;
End;

Procedure TWPPropertyServer.SetTableRowProperty(iId : Integer; Const sValue : String);
Var
  oRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oRow := TWordProcessor(Owner).PrimaryRange.CurrentTableRowStart.Clone;
  Try
    Case iId Of
      PROP_ID_ROW_COLOUR : oRow.Background := ReadColour(sValue);
      PROP_ID_ROW_HEADER : oRow.Header := ReadBoolean(sValue);
      PROP_ID_ROW_BREAK_BEFORE : oRow.BreakBefore := ReadBoolean(sValue);
      PROP_ID_ROW_LOWER_PADDING_SIZE : oRow.LowerPaddingSize := ReadInteger(sValue, False);
      PROP_ID_ROW_LOWER_PADDING_COLOUR : oRow.LowerPaddingColour := ReadColour(sValue);
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_URL : oRow.ForceHotspot.Url := sValue;
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_KEY : oRow.ForceHotspot.Key := ReadKey(sValue);
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_TITLE : oRow.ForceHotspot.Title := sValue;
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_LINK : oRow.ForceHotspot.LinkColour := ReadColour(sValue);
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_HOVER : oRow.ForceHotspot.HoverColour := ReadColour(sValue);
      PROP_ID_ROW_HOTSPOT+PROP_ID_HOTSPOT_UNDERLINE : oRow.ForceHotspot.LinkUnderline := ReadBoolean(sValue);
    End;

    If oRow.HasHotspot And oRow.Hotspot.IsEmpty Then
      oRow.HasHotspot := False;

    TWordProcessor(Owner).PrimaryRange.SetTableRowProperties(oRow);
  Finally
    oRow.Free;
  End;
End;



Procedure TWPPropertyServer.SetTableProperty(iId : Integer; Const sValue : String);
Var
  oTable : TWPWorkingDocumentTableStartPiece;
Begin
  oTable := TWordProcessor(Owner).PrimaryRange.CurrentTableStart.Clone;
  Try
    Case iId Of
      PROP_ID_TABLE_COLOUR : oTable.Background := ReadColour(sValue);
      PROP_ID_TABLE_BORDER : oTable.BorderPolicy := TWPWorkingDocumentTableBorderPolicy(ReadEnum(sValue, PROPS_TABLE_BORDER));
      PROP_ID_TABLE_MARGIN_HORIZ : oTable.HorizontalMargin := ReadWord(sValue);
      PROP_ID_TABLE_MARGIN_VERT :  oTable.VerticalMargin := ReadWord(sValue);
    End;

    TWordProcessor(Owner).PrimaryRange.SetTableProperties(oTable);
  Finally
    oTable.Free;
  End;
End;


Procedure TWPPropertyServer.SetSectionProperty(iId : Integer; Const sValue : String);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
Begin
  oSection := TWordProcessor(Owner).PrimaryRange.CurrentSectionStart.Clone;
  Try
    Case iId Of
      PROP_ID_SECTION_NAMESPACE : oSection.Namespace := sValue;
      PROP_ID_SECTION_NAME : oSection.Name := sValue;
      PROP_ID_SECTION_DATA : oSection.RawDataAsText := sValue;
      PROP_ID_SECTION_TITLE : oSection.DisplayName := sValue;
      PROP_ID_SECTION_TYPE : oSection.DisplayType := TWPWorkingDocumentSectionDisplayType(ReadEnum(sValue, PROPS_SECTION_DISPLAY_TYPE));
    End;

    TWordProcessor(Owner).PrimaryRange.SetSectionProperties(oSection);
  Finally
    oSection.Free;
  End;
End;


Procedure TWPPropertyServer.SetLineProperty(iId : Integer; Const sValue : String);
Var
  oLine : TWPWorkingDocumentBreakPiece;
Begin
  oLine := TWordProcessor(Owner).PrimaryRange.CurrentLine.Clone;
  Try
    Case iId Of
      PROP_ID_LINE_ALIGNMENT : oLine.Alignment := TWordProcessorAlignment(ReadEnum(sValue, PROPS_WPSALIGNMENT));
      PROP_ID_LINE_WIDTH : oLine.Width := ReadFloat(sValue, False);
      PROP_ID_LINE_COLOUR : oLine.PenColour := ReadColour(sValue);
      PROP_ID_LINE_PENWIDTH : oLine.PenWidth := ReadInteger(sValue, False);
      PROP_ID_LINE_STYLE : oLine.PenStyle := TFslPenStyle(ReadEnum(sValue, ADVPENSTYLE_NAMES));
      PROP_ID_LINE_END : oLine.EndStyle := TFslPenEndStyle(ReadEnum(sValue, ADVPENENDSTYLE_CODES));
    End;

    TWordProcessor(Owner).PrimaryRange.SetLineProperties(oLine);
  Finally
    oLine.Free;
  End;
End;


Procedure TWPPropertyServer.SetDocProperty(iId : Integer; Const sValue : String);
Var
  oWP : TWordProcessor;
Begin
  oWP := TWordProcessor(Owner);
  Case iId Of
    PROP_ID_DOC_MODIFIED : oWP.Modified := ReadBoolean(sValue);
    PROP_ID_DOC_BACKGROUND : oWP.Settings.Background := ReadColour(sValue);
    PROP_ID_DOC_LOWLIGHT : oWP.Settings.LowLight := ReadBoolean(sValue);
    PROP_ID_DOC_HORIZMARGIN : oWP.Settings.HorizontalMargin := ReadInteger(sValue, false);
    PROP_ID_DOC_VERTMARGIN : oWP.Settings.VerticalMargin := ReadInteger(sValue, false);
    PROP_ID_DOC_SCALE : oWP.Settings.Scale := ReadInteger(sValue, False) / 100;
    PROP_ID_DOC_TABLEBORDERS : oWP.Settings.TableBorders := ReadBoolean(sValue);
    PROP_ID_DOC_EDITHINTS : oWP.Settings.EditHints := ReadBoolean(sValue);
    PROP_ID_DOC_SPELLING : oWP.Settings.SpellingErrors := ReadBoolean(sValue);
    PROP_ID_DOC_FIELDWRAPPERS : oWP.Settings.FieldWrappers := TWPFieldPresentation(ReadEnum(sValue, PROPS_TEXT_FIELD_STATE));
  End;
End;


procedure TWPPropertyServer.AddDocumentProperties(oList: TWPPropertyList);
begin
  oList.AddColour(PROP_ID_DOC_BACKGROUND, True, 'Background', TWordProcessor(Owner).Settings.Background);
  oList.AddBoolean(PROP_ID_DOC_LOWLIGHT, True, 'Low Light', TWordProcessor(Owner).Settings.LowLight);
  oList.AddInteger(PROP_ID_DOC_HORIZMARGIN, True, 'Horizontal Margin', TWordProcessor(Owner).Settings.HorizontalMargin);
  oList.AddInteger(PROP_ID_DOC_VERTMARGIN, True, 'Vertical Margin', TWordProcessor(Owner).Settings.VerticalMargin);
  oList.AddInteger(PROP_ID_DOC_SCALE, True, 'Scale', Trunc(TWordProcessor(Owner).Settings.Scale*100));
  oList.AddBoolean(PROP_ID_DOC_TABLEBORDERS, True, 'Table Borders', TWordProcessor(Owner).Settings.TableBorders);
  oList.AddBoolean(PROP_ID_DOC_EDITHINTS, True, 'Edit Hints', TWordProcessor(Owner).Settings.EditHints);
  oList.AddBoolean(PROP_ID_DOC_SPELLING, True, 'Spelling Errors', TWordProcessor(Owner).Settings.SpellingErrors);
  oList.AddEnum(PROP_ID_DOC_FIELDWRAPPERS, True, 'Field Wrappers', ord(TWordProcessor(Owner).Settings.FieldWrappers), PROPS_TEXT_FIELD_STATE);
  oList.AddString(PROP_ID_DOC_ID, False, 'Autosave Id', TWordProcessor(Owner).Settings.AutosaveId);
  oList.AddBoolean(PROP_ID_DOC_MODIFIED, False, 'Modified', TWordProcessor(Owner).Modified);
end;

{ TWPMacroActions }

function TWPMacroActions.GetActions(iIndex: Integer): TWPMacroAction;
begin
  result := TWPMacroAction(ObjectByIndex[iindex]);
end;

function TWPMacroActions.ItemClass: TFslObjectClass;
begin
  result := TWPMacroAction;
end;

{ TWPMacroAction }

function TWPMacroAction.GetActionType: TWPMacroActionType;
begin
  result := matKey;
end;

function TWPMacroAction.Link: TWPMacroAction;
begin
  result := TWPMacroAction(Inherited Link);
end;

procedure TWPMacroAction.Save(oText: TFslTextFormatter);
begin

end;

{ TWPMacro }

constructor TWPMacro.Create;
begin
  inherited;
  FActions := TWPMacroActions.Create;
end;

destructor TWPMacro.Destroy;
begin
  FActions.Free;
  inherited;
end;



procedure TWPMacro.Load(oStream: TFslStream);
var
  oText : TFslTextExtractor;
  s : String;
begin
  oText := TFslTextExtractor.Create(oStream.Link);
  Try
    while oText.More do
    Begin
      s := oText.ConsumeLine;
      if StringStartsWithInsensitive(s, 'Key') Then
        Actions.Add(TWPMacroKeyAction.Read(s))
      else
        raise EWPException.create('Unknown macro line '+s);
    End;
  Finally
    oText.Free;
  End;
end;

function TWPMacro.Recording: Boolean;
begin
  result := State = msRecording;
end;

procedure TWPMacro.Save(oStream: TFslStream);
var
  oText : TFslTextFormatter;
  i : integer;
begin
  oText := TFslXMLFormatter.Create;
  Try
    oText.Stream := oStream.Link;
    for i := 0 to Actions.Count - 1 do
      Actions[i].Save(oText);
  Finally
    oText.Free;
  End;
end;

procedure TWPMacro.SetLastError(const Value: Boolean);
begin
  FLastError := Value;
  FLastErrorTime := now;
end;


procedure TWPMacro.SetState(const Value: TWPMacroState);
begin
  FState := Value;
  if Recording Then
    FActions.Clear;
end;

function TWPMacro.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FActions.sizeInBytes);
end;

{ TWPMacroKeyAction }

constructor TWPMacroKeyAction.Create(iKey: Word; aShift: TWPShiftStates);
begin
  Inherited Create;
  Key := iKey;
  Shift := aShift;
end;

function TWPMacroKeyAction.GetActionType: TWPMacroActionType;
begin
  result := matKey;
end;

Function DescribeShiftStates(aStates : TWPShiftStates) : String;
var
  a : TWPShiftState;
Begin
  result := '';
  for a := Low(TWPShiftState) to High(TWPShiftState) Do
    if a in aStates Then
      result := result +', '+NAMES_ShiftState[a];
  delete(result, 1, 1);
End;

class function TWPMacroKeyAction.read(sLine: String): TWPMacroKeyAction;
var
  sPart : String;
  iIndex : Integer;
begin
  StringSplit(sLine, ' ', sPart, sLine); // 'Key'
  StringSplit(sLine, ' ', sPart, sLine);
  result := TWPMacroKeyAction.Create;
  Try
    result.FKey := StrToInt(sPart);
    result.FShift := [];
    while sLine <> '' Do
    Begin
      StringSplit(sLine, ' ', sPart, sLine);
      iIndex := StringArrayIndexOfSensitive(NAMES_ShiftState, sPart);
      if iIndex > 0 then
        result.FShift := result.FShift + [TWPShiftState(iIndex)];
    End;
    result.Link;
  Finally
    result.Free;
  End;
end;

procedure TWPMacroKeyAction.Save(oText: TFslTextFormatter);
begin
  oText.ProduceLine('Key '+inttostr(FKey)+' '+DescribeShiftStates(FShift));
end;

{ TWPCodeCompletionListBox }

Procedure TWPCodeCompletionListBox.KeyDown(Var Key: Word; Shift: TShiftState);
begin
  If Key = vkEscape Then
    Visible := false
  Else If Key = vkEnter Then
  Begin
    OnInsert(FList[ItemIndex]);
    Visible := False;
  End;
end;

Destructor TWPCodeCompletionListBox.Destroy;
begin
  FList.Free;
  inherited;
end;


procedure TWPCodeCompletionListBox.DblClick;
begin
  OnInsert(FList[ItemIndex]);
  Visible := False;
end;

Procedure TWPCodeCompletionListBox.SetList(Const Value: TWPCompletionItems);
Var
  iWidth : Integer;
  iLoop : Integer;
Begin
  FList.Free;
  FList := Value;
  ClientHeight := ItemHeight * IntegerMin(MAX_ITEM_DISPLAY, FList.Count);
  Canvas.Font.Assign(Font);
  iWidth := 0;
  For iLoop := 0 To FList.Count - 1 Do
    iWidth := IntegerMax(iWidth, Canvas.TextWidth(FList[iLoop].Display)+20);
  ClientWidth := IntegerMax(IntegerMin(iWidth, MAX_WIDTH), MIN_WIDTH);

  For iLoop := 0 To FList.Count- 1 Do
    Items.Add(FList[iLoop].Display);
  ItemIndex := 0;
End;

Function TWPSearchDetails.Link : TWPSearchDetails;
Begin
  Result := TWPSearchDetails(Inherited Link);
End;


Function TWPSearchDetails.Clone : TWPSearchDetails;
Begin
  Result := TWPSearchDetails(Inherited Clone);
End;


Procedure TWPSearchDetails.Assign(oSource : TFslObject);
Begin
  Inherited;
  FText := TWPSearchDetails(oSource).FText;
  FDirection := TWPSearchDetails(oSource).FDirection;
  FWholeDocument := TWPSearchDetails(oSource).FWholeDocument;
  FCaseSensitive := TWPSearchDetails(oSource).FCaseSensitive;
  FWholeWords := TWPSearchDetails(oSource).FWholeWords;
End;



function TWPSearchDetails.Describe: string;
begin
  result := 'todo';
end;

function TWPSearchDetails.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Function TWPReplaceDetails.Link : TWPReplaceDetails;
Begin
  Result := TWPReplaceDetails(Inherited Link);
End;


Function TWPReplaceDetails.Clone : TWPReplaceDetails;
Begin
   Result := TWPReplaceDetails(Inherited Clone);
End;


Procedure TWPReplaceDetails.Assign(oSource : TFslObject);
Begin
  Inherited;
  FReplace := TWPReplaceDetails(oSource).FReplace;
  FSelection := TWPReplaceDetails(oSource).FSelection;
  FPrompt := TWPReplaceDetails(oSource).FPrompt;
End;


function TWPReplaceDetails.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FReplace.length * sizeof(char)) + 12);
end;

Destructor TWPSearchIterator.Destroy;
Begin
  FDocument.Free;
  Inherited;
End;

Function TWPSearchIterator.GetDocument : TWPWorkingDocument;
Begin
  Assert(CheckCondition(HasDocument, 'GetDocument', 'No Document'));
  Result := FDocument;
End;


Procedure TWPSearchIterator.SetDocument(Const Value : TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;


Function TWPSearchIterator.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Procedure TWPSearchIterator.Open(iLimit : Integer);
Begin
  FCursor := iLimit;
  FMore := InRange(iLimit) And Document.GetPieceByPosition(FCursor, FCurrent, FOffset, FIndex);
End;


Function TWPSearchIterator.More : Boolean;
Begin
  Result := FMore;
End;


Procedure TWPSearchIterator.Backwards;
Begin
  Dec(FCursor);
  Dec(FOffset);
  If FOffset < 0 Then
  Begin
    Dec(FIndex);
    If FIndex < 0 Then
      FMore := False
    Else
      FCurrent := FDocument.Pieces[FIndex];
    FOffset := FCurrent.Metrics.CharCount - 1;
  End;
 If Not InRange(FCursor) Then
   FMore := False;
End;

Procedure TWPSearchIterator.Forwards;
Begin
  Inc(FCursor);
  Inc(FOffset);
  If FOffset = FCurrent.Metrics.CharCount Then
  Begin
    Inc(FIndex);
    If FIndex >= FDocument.Pieces.Count Then
      FMore := False
    Else
      FCurrent := FDocument.Pieces[FIndex];
    FOffset := 0;
  End;
 If Not InRange(FCursor) Then
   FMore := False;
End;

Procedure TWPSearchIterator.Next;
Begin
  If FMore Then
  Begin
    If FDirection = sdForwards Then
      Forwards
    Else
      Backwards;
  End;
End;


Function TWPSearchIterator.GetText(iLength : Integer) : String;
Var
  iCursor : Integer;
  iIndex : Integer;
  iOffset : Integer;
  oCurrent : TWPWorkingDocumentPiece;
Begin
  Assert(CheckCondition(More, 'GetText', 'No more text to get'));
  oCurrent := FCurrent;
  iCursor := FCursor;
  iIndex := FIndex;
  iOffset := FOffset;
  Result := '';
  While More And InRange(iCursor) And (Length(Result) < iLength) And Assigned(oCurrent) Do
  Begin
    Assert(CheckCondition(iOffset < oCurrent.Metrics.CharCount, 'GetText', 'Overrun on piece'));
    Result := Result + oCurrent.LogicalText[iOffset+1];
    Inc(iCursor);
    If iOffset < oCurrent.Metrics.CharCount - 1 Then
      Inc(iOffset)
    Else
    Begin
      iOffset := 0;
      Inc(iIndex);
      If iIndex < FDocument.Pieces.Count Then
        oCurrent := FDocument.Pieces[iIndex]
      Else
        oCurrent := Nil;
    End;
  End;
End;

Function TWPSearchIterator.CharAtOffset(iPosition : Integer):Char;
Var
  oPiece : TWPWorkingDocumentPiece;
  iIndex : Integer;
  iOffset : Integer;
Begin
  If InRange(iPosition) And Document.GetPieceByPosition(iPosition, oPiece, iOffset, iIndex) Then
    Result := oPiece.LogicalText[iOffset + 1]
  Else
    Result := #0;
End;


Function TWPSearchIterator.InRange(iOffset : Integer) : Boolean;
Begin
  Result := (FLimitStart = -1) Or ((iOffset >= FLimitStart) And (iOffset < FLimitStop));
End;

function TWPSearchIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FCurrent.sizeInBytes);
end;

Constructor TWPSearch.Create(oDocument : TWPWorkingDocument; oSelection : TWPSelection; oSearchDetails : TWPSearchDetails);
Begin
  Create;
  Document := oDocument;
  Selection := oSelection;
  SearchDetails := oSearchDetails;
End;


Destructor TWPSearch.Destroy;
Begin
  FDocument.Free;
  FSelection.Free;
  FSearchDetails.Free;
  Inherited;
End;


Function TWPSearch.GetDocument : TWPWorkingDocument;
Begin
  Assert(CheckCondition(HasDocument, 'GetDocument', 'No Document'));
  Result := FDocument;
End;


Function TWPSearch.GetSelection : TWPSelection;
Begin
  Assert(CheckCondition(HasSelection, 'GetSelection', 'No Selection'));
  Result := FSelection;
End;


Function TWPSearch.GetSearchDetails : TWPSearchDetails;
Begin
  Assert(CheckCondition(HasSearchDetails, 'GetSearchDetails', 'No SearchDetails'));
  Result := FSearchDetails;
End;


Procedure TWPSearch.SetDocument(Const Value : TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;


Procedure TWPSearch.SetSelection(Const Value : TWPSelection);
Begin
  FSelection.Free;
  FSelection := Value;
End;


Procedure TWPSearch.SetSearchDetails(Const Value : TWPSearchDetails);
Begin
  FSearchDetails.Free;
  FSearchDetails := Value;
End;


Function TWPSearch.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWPSearch.HasSelection : Boolean;
Begin
  Result := Assigned(FSelection);
End;


Function TWPSearch.HasSearchDetails : Boolean;
Begin
  Result := Assigned(FSearchDetails);
End;


Function TWPSearch.Search(Out iStart, iEnd : Integer) : Boolean;
Begin
  If SearchDetails.Direction = sdBackwards Then
  Begin
    If SearchDetails.WholeDocument Then
      Result := Search(Document.CharCount - 1, iStart, iEnd)
    Else
      Result := Search(Selection.WorkingSelStart - 1, iStart, iEnd);
  End
  Else
  Begin
    If SearchDetails.WholeDocument Then
      Result := Search(0, iStart, iEnd)
    Else
      Result := Search(Selection.WorkingSelEnd, iStart, iEnd);
  End;
End;


Function TWPSearch.Search(iLimit : Integer; Out iStart, iEnd : Integer) : Boolean;
Var
  oIterator : TWPSearchIterator;
Begin
  Result := False;
  oIterator := TWPSearchIterator.Create;
  Try
    oIterator.LimitStart := LimitStart;
    oIterator.LimitStop := LimitStop;
    oIterator.Document := Document.Link;
    oIterator.Direction := SearchDetails.Direction;
    oIterator.Open(iLimit);
    While Not Result And oIterator.More Do
    Begin
      Result := Matches(oIterator, iStart, iEnd);
      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Function TWPSearch.Matches(oIterator : TWPSearchIterator; Out iStart, iEnd : Integer) : Boolean;
Var
  sText : String;
Begin
  sText := oIterator.GetText(Length(SearchDetails.Text));

  Result := ((SearchDetails.caseSensitive) And (sText = SearchDetails.Text)) Or
     ((Not SearchDetails.caseSensitive) And StringEquals(sText, SearchDetails.Text));

  If Result And SearchDetails.WholeWords Then
    Result := Not StringIsAlphanumeric(oIterator.CharAtOffset(oIterator.Cursor -1)) And
              Not StringIsAlphanumeric(oIterator.CharAtOffset(oIterator.Cursor + Length(SearchDetails.Text)));

  If Result Then
  Begin
    iStart := oIterator.Cursor;
    iEnd := iStart + Length(SearchDetails.Text);
  End;
End;




function TWPSearch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FSelection.sizeInBytes);
  inc(result, FSearchDetails.sizeInBytes);
end;

function TWPSpeechMagicInsertOptions.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

function TWPAction.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSelection.length * sizeof(char)) + 12);
  inc(result, (FDetails.length * sizeof(char)) + 12);
  inc(result, (FAction.length * sizeof(char)) + 12);
  inc(result, (FOutcome.length * sizeof(char)) + 12);
  inc(result, (FTime.length * sizeof(char)) + 12);
end;

End.
