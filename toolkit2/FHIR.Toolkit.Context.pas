unit FHIR.Toolkit.Context;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, ComCtrls,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream;

// supported formats:
// ini
// xml - plain, schema, xslt
// json
// hl7 v2 vertical bar
// cda
// dicom
// javascript
// map
// liquid
// markdown
// resource

type
  TToolkitContext = class;

  { TToolkitContextObject }

  TToolkitContextObject = class (TFslObject)
  private
    FContext : TToolkitContext; // no ownership
  public
    constructor create(context : TToolkitContext); virtual;
    property Context : TToolkitContext read FContext write FContext;
  end;


  TSourceEncoding = (seUnknown, seBinary, seUTF8, seASCII, seUTF16BE, seUTF16LE);

  { TToolkitEditSession }
  // this object keeps what is necessary to persist with the bytes for the file between sessions
  TToolkitEditSession = class (TFslObject)
  private
    FAddress: String;
    FEncoding: TSourceEncoding;
    FGuid : String;
  public
    function link : TToolkitEditSession; overload;

    property Guid : String read FGuid write FGuid;
    property Address : String read FAddress write FAddress;
    property Encoding : TSourceEncoding read FEncoding write FEncoding;

    class function makeNew : TToolkitEditSession;
  end;

  { TToolkitEditor }

  TToolkitEditor = class abstract (TToolkitContextObject)
  private
    FSession : TToolkitEditSession;
    FCanCut: boolean;
    FCanPaste: boolean;
    FCanRedo: boolean;
    FCanUndo: boolean;
    FHasText: boolean;
    FInSource: boolean;
    FLoadState: boolean;
  protected
    function GetBytes: TBytes; virtual; abstract;
    function GetCanBeSaved: boolean; virtual; abstract;
    procedure SetBytes(AValue: TBytes); virtual; abstract;
  public
    constructor create(context : TToolkitContext; session : TToolkitEditSession);
    destructor Destroy; override;

    property Context : TToolkitContext read FContext;

    // storage related properties
    property LoadState : boolean read FLoadState write FLoadState;

    property AsBytes : TBytes read GetBytes write SetBytes;

    // editing related functionality
    procedure newContent; virtual; abstract; // create new content
    procedure bindToTab(tab : TTabSheet); virtual; abstract; // set up the UI
    procedure locate(location : TSourceLocation); virtual; abstract;

    // status for actions
    property CanBeSaved : boolean read GetCanBeSaved;
    property hasText : boolean read FHasText write FHasText;
    property canUndo : boolean read FCanUndo write FCanUndo;
    property canRedo : boolean read FCanRedo write FCanRedo;
    property canCut : boolean read FCanCut write FCanCut;
    property canPaste : boolean read FCanPaste write FCanPaste;
    property inSource : boolean read FInSource write FInSource;
  end;
  TToolkitEditorClass = class of TToolkitEditor;

  { TToolkitContext }

  TToolkitContext = class (TFslObject)
  private
    FEditorSessions : TFslList<TToolkitEditSession>;
    FEditors : TFslList<TToolkitEditor>;
    FFocus: TToolkitEditor;
    function GetFocus: TToolkitEditor;
    function GetHasFocus: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property hasFocus : boolean read GetHasFocus;
    property focus : TToolkitEditor read FFocus write FFocus;

    property editorSessions : TFslList<TToolkitEditSession> read FEditorSessions;
//    property persistenceServices : TToolkitPersistenceServices;
//    property EditorList : TToolkitEditorList;
////
//    property ProjectManager : TToolkitProjectManagerView;
//    property Messages : TToolkitMessagesView;
//    property Console : TToolkitConsoleView;
//    property Search : TToolkitSearchView;
//    property Inspector : TToolkitEditorInspectorView;
//    property MetadataView : TToolkitEditorMetadataView;
//    property Tasks : TToolkitEditorTasksView;
//    property Variables : TToolkitEditorVariablesView;
//    property Breakpoints : TToolkitEditorBreakpointsView;
//    property CallStack : TToolkitEditorCallStackView;
//    property FHIRPath : TToolkitFHIRPathView;

    function addEditor(editorClass : TToolkitEditorClass; session : TToolkitEditSession) : TToolkitEditor;
  end;

implementation

{ TToolkitEditSession }

function TToolkitEditSession.link: TToolkitEditSession;
begin
  result := TToolkitEditSession(inherited link);
end;

class function TToolkitEditSession.makeNew: TToolkitEditSession;
begin
  result := TToolkitEditSession.create;
  result.Guid := NewGuidId;
end;

{ TToolkitEditor }

constructor TToolkitEditor.create(context : TToolkitContext; session : TToolkitEditSession);
begin
  inherited create(context);
  FSession := session;
  //FGuid := guid;
end;

destructor TToolkitEditor.Destroy;
begin
  FSession.Free;
  inherited Destroy;
end;

{ TToolkitContextObject }

constructor TToolkitContextObject.create(context: TToolkitContext);
begin
  inherited create;
  FContext := context;
end;

{ TToolkitContext }

function TToolkitContext.GetFocus: TToolkitEditor;
begin
  result := nil;
end;

function TToolkitContext.GetHasFocus: boolean;
begin
  result := false;
end;

constructor TToolkitContext.Create;
begin
  inherited Create;
  FEditorSessions := TFslList<TToolkitEditSession>.create;
  FEditors := TFslList<TToolkitEditor>.create;
end;

destructor TToolkitContext.Destroy;
begin
  FEditors.Free;
  FEditorSessions.Free;
  inherited Destroy;
end;

function TToolkitContext.addEditor(editorClass: TToolkitEditorClass; session: TToolkitEditSession) : TToolkitEditor;
begin
  result := editorClass.create(self, session);
  FEditors.add(result);
  FEditorSessions.add(session.link);
end;

end.

