unit FHIR.Toolkit.Context;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, ComCtrls, Dialogs,
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
  TSourceEncoding = (senUnknown, senBinary, senUTF8, senASCII, senUTF16BE, senUTF16LE);
  TSourceLineMarker = (slUnknown, slCRLF, slCR, slLF);

const
  {$IFDEF WINODWS}
  PLATFORM_DEFAULT_EOLN = slCRLF;
  {$ELSE}
  PLATFORM_DEFAULT_EOLN = slLF;
  {$ENDIF}

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

  { TToolkitEditSession }
  // this object keeps what is necessary to persist with the bytes for the file between sessions
  TToolkitEditSession = class (TFslObject)
  private
    FAddress: String;
    FCaption: String;
    FEncoding: TSourceEncoding;
    FEndOfLines: TSourceLineMarker;
    FGuid : String;
    FHasBOM: boolean;
    FInfo: TStringList;
    FNeedsSaving: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TToolkitEditSession; overload;

    property Guid : String read FGuid write FGuid;
    property Address : String read FAddress write FAddress;
    property Caption : String read FCaption write FCaption;
    property Encoding : TSourceEncoding read FEncoding write FEncoding;
    property EndOfLines : TSourceLineMarker read FEndOfLines write FEndOfLines;
    property HasBOM : boolean read FHasBOM write FHasBOM;
    property NeedsSaving : boolean read FNeedsSaving write FNeedsSaving;

    property Info : TStringList read FInfo;

    class function makeNew : TToolkitEditSession;
  end;

  { TToolkitEditor }

  TToolkitEditor = class abstract (TToolkitContextObject)
  private
    FIsFile: boolean;
    FSession : TToolkitEditSession;
    FCanCut: boolean;
    FCanPaste: boolean;
    FCanRedo: boolean;
    FCanUndo: boolean;
    FHasText: boolean;
    FInSource: boolean;
    FLoadState: boolean;
    FTab: TTabSheet;
    function GetHasAddress: boolean;
  protected
    function GetCanBeSaved: boolean; virtual; abstract;
  public
    constructor create(context : TToolkitContext; session : TToolkitEditSession);
    destructor Destroy; override;

    property Context : TToolkitContext read FContext;
    property Session : TToolkitEditSession read FSession;
    property Tab : TTabSheet read FTab write FTab;

    // storage related properties & procedures
    property LoadState : boolean read FLoadState write FLoadState;
    function FileExtension : String; virtual; abstract;
    function GetBytes: TBytes; virtual; abstract;

    // editing related functionality
    procedure bindToTab(tab : TTabSheet); virtual; // set up the UI - caleed before either newContent or LoadBytes is called
    procedure newContent(info : TStringList); virtual; abstract; // create new content
    procedure loadBytes(bytes : TBytes; info : TStringList); virtual; abstract;
    procedure locate(location : TSourceLocation); virtual; abstract;
    function location : String; virtual; abstract;
    procedure redo; virtual;

    // status for actions
    property CanBeSaved : boolean read GetCanBeSaved;
    property hasText : boolean read FHasText write FHasText;
    property canUndo : boolean read FCanUndo write FCanUndo;
    property canRedo : boolean read FCanRedo write FCanRedo;
    property canCut : boolean read FCanCut write FCanCut;
    property canPaste : boolean read FCanPaste write FCanPaste;
    property inSource : boolean read FInSource write FInSource;
    property isFile : boolean read FIsFile write FIsFile;
    property hasAddress : boolean read GetHasAddress;
  end;
  TToolkitEditorClass = class of TToolkitEditor;

  TStorageService = class abstract (TFslObject)
  public
    function scheme : String; virtual; abstract;
    function makeSession(address : String) : TToolkitEditSession; virtual; abstract;
    function CaptionForAddress(address : String) : String; virtual; abstract;
    procedure save(address : String; bytes : TBytes); virtual; abstract;
  end;

  { TToolkitContext }

  TToolkitContext = class (TFslObject)
  private
    FEditorSessions : TFslList<TToolkitEditSession>;
    FEditors : TFslList<TToolkitEditor>;
    FFocus: TToolkitEditor;
    FOnUpdateActions: TNotifyEvent;
    FStorages: TFslList<TStorageService>;
    function GetFocus: TToolkitEditor;
    function GetHasFocus: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property hasFocus : boolean read GetHasFocus;
    property focus : TToolkitEditor read FFocus write FFocus;

    property editorSessions : TFslList<TToolkitEditSession> read FEditorSessions;
    property storages : TFslList<TStorageService> read FStorages;
    function StorageForAddress(address : String) : TStorageService;
    property Editors : TFslList<TToolkitEditor> read FEditors;

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
    function EditorForTab(tab : TTabSheet) : TToolkitEditor;
    function EditorForAddress(address : String) : TToolkitEditor;
    function examineFile(handle : TComponent; filename : String; const bytes : TBytes; info : TStringList) : TToolkitEditorClass;
    function anyDirtyEditors : boolean;

    property OnUpdateActions : TNotifyEvent read FOnUpdateActions write FOnUpdateActions;
  end;

implementation

uses
  FHIR.Toolkit.IniEditor;

{ TToolkitEditSession }

constructor TToolkitEditSession.Create;
begin
  inherited Create;
  FInfo := TStringList.create;
end;

destructor TToolkitEditSession.Destroy;
begin
  FInfo.Free;
  inherited Destroy;
end;

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

function TToolkitEditor.GetHasAddress: boolean;
begin
  result := Session.Address <> '';
end;

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

procedure TToolkitEditor.bindToTab(tab: TTabSheet);
begin
  FTab := tab;
  tab.Caption := FSession.Caption;
  if FSession.address = '' then
    Tab.Hint := 'File has not been saved'
  else
    Tab.Hint := FSession.address;
end;

procedure TToolkitEditor.redo;
begin
  raise Exception.create('not implemented');
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
  result := FFocus <> nil;
end;

constructor TToolkitContext.Create;
begin
  inherited Create;
  FEditorSessions := TFslList<TToolkitEditSession>.create;
  FEditors := TFslList<TToolkitEditor>.create;
  FStorages := TFslList<TStorageService>.create;
end;

destructor TToolkitContext.Destroy;
begin
  FStorages.Free;
  FEditors.Free;
  FEditorSessions.Free;
  inherited Destroy;
end;

function TToolkitContext.StorageForAddress(address: String): TStorageService;
var
  scheme : String;
  storage : TStorageService;
begin
  scheme := address.Substring(0, address.IndexOf(':'));
  result := nil;
  for storage in storages do
    if storage.scheme = scheme then
      exit(storage);
end;

function TToolkitContext.addEditor(editorClass: TToolkitEditorClass; session: TToolkitEditSession) : TToolkitEditor;
  function hasCaption(s : String) : boolean;
  var ss : TToolkitEditSession;
  begin
    result := false;
    for ss in FEditorSessions do
      if (ss.Caption = s) then
        exit(true);
  end;
var
  i : integer;
begin
  result := editorClass.create(self, session);
  FEditors.add(result);
  FEditorSessions.add(session.link);

  if session.Address = '' then
  begin
    i := 1;
    while hasCaption('new_file '+inttostr(i)) do
      inc(i);
    session.Caption := 'new_file '+inttostr(i);
  end;
end;

function TToolkitContext.EditorForTab(tab: TTabSheet): TToolkitEditor;
var
  editor : TToolkitEditor;
begin
  result := nil;
  for editor in FEditors do
    if editor.tab = tab then
      exit(editor);
end;

function TToolkitContext.EditorForAddress(address: String): TToolkitEditor;
var
  editor : TToolkitEditor;
begin
  result := nil;
  for editor in FEditors do
    if editor.session.Address = address then
      exit(editor);
end;

function TToolkitContext.examineFile(handle: TComponent; filename: String; const bytes: TBytes; info : TStringList): TToolkitEditorClass;
var
  ext : String;
begin
  result := nil;
  ext := Lowercase(ExtractFileExt(filename));
  if (ext = '.ini') then
    result := TIniEditor
  else
    ShowMessage('The file '+filename+' isn''t recognised by this application');
end;

function TToolkitContext.anyDirtyEditors: boolean;
var
  editor : TToolkitEditor;
begin
  result := false;
  for editor in FEditors do
    if editor.Session.NeedsSaving then
      exit(true);
end;

end.

