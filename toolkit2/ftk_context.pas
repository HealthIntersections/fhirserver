unit ftk_context;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Graphics, Controls, ExtCtrls, ComCtrls, Menus, ActnList,
  fsl_base, fsl_utilities, fsl_stream, fsl_logging,
  fhir_objects,
  ftk_store, ftk_console;

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
  TSourceEditorKind = (sekNull, sekFHIR, sekv2, sekCDA, sekXML, sekJson, sekLiquid, sekMap, sekIni, sekText, sekMD, sekJS, sekHTML, sekDicom);
  TSourceEncoding = (senUnknown, senBinary, senUTF8, senASCII, senUTF16BE, senUTF16LE);
  TSourceLineMarker = (slUnknown, slCRLF, slCR, slLF);
  TToolkitMessageLevel = (msgError, msgWarning, msgHint);

const
  {$IFDEF WINDOWS}
  PLATFORM_DEFAULT_EOLN = slCRLF;
  {$ELSE}
  PLATFORM_DEFAULT_EOLN = slLF;
  {$ENDIF}

  CODES_TSourceEditorKind : Array [TSourceEditorKind] of String = ('Unknown', 'FHIR', 'v2', 'CDA', 'XML', 'Json', 'Liquid', 'Map', 'Ini', 'Text', 'MD', 'JS', 'HTML', 'Dicom');
  CODES_TSourceEncoding : Array [TSourceEncoding] of String = ('Unknown', 'Binary', 'UTF8', 'ASCII', 'UTF16BE', 'UTF16LE');
  CODES_TSourceLineMarker : Array [TSourceLineMarker] of String = ('Unknown', 'CRLF', 'CR', 'LF');
  CODES_TToolkitMessageLevel : Array [TToolkitMessageLevel] of String = ('Error', 'Warning', 'Hint');


type
  TToolkitContext = class;
  TToolkitEditor = class;


  { TToolkitMessage }

  TToolkitMessage = class (TFslObject)
  private
    FEditor : TToolkitEditor;
    FLocation : TSourceLocation;
    FLevel : TToolkitMessageLevel;
    FContent : String;
  public
    constructor Create(editor : TToolkitEditor; location : TSourceLocation; level : TToolkitMessageLevel; content : String); overload;
    property Editor : TToolkitEditor read FEditor;
    property Location : TSourceLocation read FLocation;
    property Level : TToolkitMessageLevel read FLevel;
    property Content : String read FContent;

    function summary : String;
  end;

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
    FKind: TSourceEditorKind;
    FNeedsSaving: boolean;
    FTimestamp: TDateTime;
  public
    constructor Create; override;
    constructor Create(kind : TSourceEditorKind); overload;
    constructor CreateResource(format : TFHIRFormat; name, value : string); overload;
    destructor Destroy; override;
    function link : TToolkitEditSession; overload;

    property Guid : String read FGuid write FGuid;
    property Address : String read FAddress write FAddress;
    function hasAddress : boolean;
    property Caption : String read FCaption write FCaption;
    property kind : TSourceEditorKind read FKind write FKind;
    property Encoding : TSourceEncoding read FEncoding write FEncoding;
    property EndOfLines : TSourceLineMarker read FEndOfLines write FEndOfLines;
    property HasBOM : boolean read FHasBOM write FHasBOM;
    property NeedsSaving : boolean read FNeedsSaving write FNeedsSaving;
    property Timestamp : TDateTime read FTimestamp write FTimestamp;

    property Info : TStringList read FInfo;
  end;

  { TToolkitEditor }

  TToolkitEditor = class abstract (TToolkitContextObject)
  private
    FLastMove: int64;
    FLastMoveChecked: boolean;
    FPause: integer;
    FStore : TStorageService;
    FValidationIssues : TFslList<TToolkitMessage>;
    FIsFile: boolean;
    FLastChange: int64;
    FLastChangeChecked: boolean;
    FSession : TToolkitEditSession;
    FCanCut: boolean;
    FCanPaste: boolean;
    FCanRedo: boolean;
    FCanUndo: boolean;
    FHasText: boolean;
    FInSource: boolean;
    FLoadState: boolean;

    function GetHasAddress: boolean;
    function GetHint: String;
    function getIsFile: boolean;
    procedure SetStore(AValue: TStorageService);
  protected
    FTab: TTabSheet;

    function GetCanBeSaved: boolean; virtual; abstract;

    function StartValidating : QWord;
    procedure validationError(loc : TSourceLocation; msg: String);
    procedure validationWarning(loc : TSourceLocation; msg: String);
    procedure finishValidating(validating : boolean; start : QWord);
  public
    constructor create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); virtual;
    destructor Destroy; override;
    function link : TToolkitEditor; overload;

    property Context : TToolkitContext read FContext;
    property Session : TToolkitEditSession read FSession;
    property Tab : TTabSheet read FTab write FTab;

    // storage related properties & procedures
    property Store : TStorageService read FStore write SetStore;
    function hasStore: boolean;
    property LoadState : boolean read FLoadState write FLoadState;
    function FileExtension : String; virtual; abstract;
    function GetBytes: TBytes; virtual; abstract;
    function describe : String;

    // editing related functionality
    property Pause : integer read FPause write FPause;
    procedure bindToTab(tab : TTabSheet); virtual;  // set up the UI - caleed before either newContent or LoadBytes is called
    procedure newContent(); virtual; abstract; // create new content
    procedure loadBytes(bytes : TBytes); virtual; abstract;
    procedure locate(location : TSourceLocation); virtual; abstract;
    function location : String; virtual; abstract;
    procedure redo; virtual; abstract;
    procedure editPause; virtual; abstract;
    procedure MovePause; virtual; abstract;
    procedure getFocus(content : TMenuItem); virtual; abstract;
    procedure loseFocus(); virtual; abstract;
    procedure ChangeSideBySideMode; virtual; abstract;
    function hasDesigner : boolean; virtual; abstract;
    function hasTextTab : boolean; virtual; abstract;
    function IsShowingDesigner : boolean; virtual; abstract;
    procedure showDesigner; virtual; abstract;
    procedure showTextTab; virtual; abstract;
    procedure BeginEndSelect; virtual; abstract;
    procedure updateFont; virtual; abstract;
    function getSource : String; virtual; abstract;
    procedure resizeControls; virtual; abstract;

    // status for actions
    property CanBeSaved : boolean read GetCanBeSaved;
    property hasText : boolean read FHasText write FHasText;
    property canUndo : boolean read FCanUndo write FCanUndo;
    property canRedo : boolean read FCanRedo write FCanRedo;
    property canCut : boolean read FCanCut write FCanCut;
    property canPaste : boolean read FCanPaste write FCanPaste;
    property isFile : boolean read getIsFile;
    property hasAddress : boolean read GetHasAddress;

    property lastChange : int64 read FLastChange write FLastChange;
    property lastChangeChecked : boolean read FLastChangeChecked write FLastChangeChecked;
    property lastMove : int64 read FLastMove write FLastMove;
    property lastMoveChecked : boolean read FLastMoveChecked write FLastMoveChecked;

    property Hint : String read GetHint;
  end;

  TLocateEvent = procedure(sender : TObject; x, y: integer; var point : TPoint) of Object;

  { TToolkitMessagesView }

  TToolkitMessagesView = class (TFslObject)
  private
    FMessages : TFslList<TToolkitMessage>;
    FOnChange: TNotifyEvent;
    function doSort(sender : TObject; const L, R: TToolkitMessage): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure setMessagesForEditor(editor : TToolkitEditor; messages : TFslList<TToolkitMessage>);
    procedure removeMessagesForEditor(editor : TToolkitEditor);

    property messages : TFslList<TToolkitMessage> read FMessages;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  { TToolkitEditorInspectorView }

  TToolkitEditorInspectorView = class (TFslObject)
  private
    FActive: boolean;
    FOnChange: TNotifyEvent;
    FSource : TStringList;
    procedure SetActive(AValue: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure populate(ts : TStringList);
    procedure clear;

    property active : boolean read FActive write SetActive;
    property content : TStringList read FSource;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  { TToolkitContext }

  TToolkitContext = class (TFslObject)
  private
    FActions: TActionList;
    FConsole: TToolkitConsole;
    FFont: TFont;
    FInspector: TToolkitEditorInspectorView;
    FMessageView : TToolkitMessagesView;
    FOnChangeFocus: TNotifyEvent;
    FOnLocate : TLocateEvent;
    FImages : TImageList;
    FEditorSessions : TFslList<TToolkitEditSession>;
    FEditors : TFslList<TToolkitEditor>;
    FFocus: TToolkitEditor;
    FOnUpdateActions: TNotifyEvent;
    FSideBySide: boolean;
    FStorages: TFslList<TStorageService>;
    FToolBarHeight: integer;
    function GetFocus: TToolkitEditor;
    function GetHasFocus: boolean;
    procedure SetFocus(AValue: TToolkitEditor);
    procedure SetSideBySide(AValue: boolean);
    procedure SetToolBarHeight(AValue: integer);
  public
    constructor Create(images : TImageList; actions : TActionList);
    destructor Destroy; override;
    function link : TToolkitContext; overload;

    // UI support
    property images : TImageList read FImages;
    property actions : TActionList read FActions;
    function locateOnScreen(x, y : Integer) : TPoint;
    property Font : TFont read FFont write FFont;
    procedure updateFont;
    property ToolBarHeight : integer read FToolBarHeight write SetToolBarHeight;

    property hasFocus : boolean read GetHasFocus;
    property focus : TToolkitEditor read FFocus write SetFocus;

    property editorSessions : TFslList<TToolkitEditSession> read FEditorSessions;
    property storages : TFslList<TStorageService> read FStorages;
    function StorageForAddress(address : String) : TStorageService;
    property Editors : TFslList<TToolkitEditor> read FEditors;

    // global settings
    property SideBySide : boolean read FSideBySide write SetSideBySide;

//    property ProjectManager : TToolkitProjectManagerView;
    property MessageView : TToolkitMessagesView read FMessageView;
    property Console : TToolkitConsole read FConsole;
//    property Search : TToolkitSearchView;
    property Inspector : TToolkitEditorInspectorView read FInspector;
//    property MetadataView : TToolkitEditorMetadataView;
//    property Tasks : TToolkitEditorTasksView;
//    property Variables : TToolkitEditorVariablesView;
//    property Breakpoints : TToolkitEditorBreakpointsView;
//    property CallStack : TToolkitEditorCallStackView;
//    property FHIRPath : TToolkitFHIRPathView;

    procedure addEditor(editor : TToolkitEditor);
    procedure removeEditor(editor : TToolkitEditor);
    function EditorForTab(tab : TTabSheet) : TToolkitEditor;
    function EditorForAddress(address : String) : TToolkitEditor;
    function anyDirtyEditors : boolean;

    property OnUpdateActions : TNotifyEvent read FOnUpdateActions write FOnUpdateActions;
    property OnChangeFocus : TNotifyEvent read FOnChangeFocus write FOnChangeFocus;
    property OnLocate : TLocateEvent read FOnLocate write FOnLocate;
  end;

implementation

{ TToolkitEditorInspectorView }

procedure TToolkitEditorInspectorView.SetActive(AValue: boolean);
begin
  FActive:=AValue;
  OnChange(self);
end;

constructor TToolkitEditorInspectorView.Create;
begin
  inherited Create;
  FSource := TStringList.Create;
end;

destructor TToolkitEditorInspectorView.Destroy;
begin
  FSource.Free;
  inherited Destroy;
end;

procedure TToolkitEditorInspectorView.populate(ts: TStringList);
begin
  FSource.Clear;
  FSource.Assign(ts);
  Active := true;
  OnChange(self);
end;

procedure TToolkitEditorInspectorView.clear;
begin
  FSource.Clear;
  Active := false;
  OnChange(self);
end;

{ TToolkitMessage }

constructor TToolkitMessage.Create(editor: TToolkitEditor; location: TSourceLocation; level: TToolkitMessageLevel; content: String);
begin
  inherited Create;
  FEditor := editor;
  FLocation := location;
  FLevel := level;
  FContent := content;
end;

function TToolkitMessage.summary: String;
begin
  result := CODES_TToolkitMessageLevel[level]+ ' at '+editor.session.caption+' Line '+ inttostr(Location.lineForHuman)+': '+Content;
end;

{ TToolkitMessagesView }

function TToolkitMessagesView.doSort(sender: TObject; const L, R: TToolkitMessage): Integer;
begin
  result := CompareStr(l.Editor.Session.caption, r.Editor.Session.caption);
  if (result = 0) then
    result := NativeUInt(l.editor) - NativeUInt(r.editor);
  if (result = 0) then
    result := l.Location.line - r.Location.line;
  if (result = 0) then
    result := ord(l.Level) - ord(r.level);
  if (result = 0) then
    result := CompareStr(l.Content, r.content);
end;

constructor TToolkitMessagesView.Create;
begin
  inherited Create;
  FMessages := TFslList<TToolkitMessage>.create;
end;

destructor TToolkitMessagesView.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TToolkitMessagesView.setMessagesForEditor(editor: TToolkitEditor; messages: TFslList<TToolkitMessage>);
begin
  removeMessagesForEditor(editor);
  FMessages.AddAll(messages);
  FMessages.SortE(doSort);
  FOnChange(editor);
end;

procedure TToolkitMessagesView.removeMessagesForEditor(editor: TToolkitEditor);
var
  i : integer;
begin
  for i := FMessages.count - 1 downto 0 do
    if FMessages[i].Editor = editor then
      FMessages.delete(i);
end;

{ TToolkitEditSession }

constructor TToolkitEditSession.Create;
begin
  inherited Create;
  FInfo := TStringList.create;
end;

constructor TToolkitEditSession.Create(kind: TSourceEditorKind);
begin
  Create;
  self.guid := NewGuidId;
  self.kind := kind;
end;

constructor TToolkitEditSession.CreateResource(format: TFHIRFormat; name, value: string);
begin
  Create;
  self.guid := NewGuidId;
  self.kind := sekFHIR;
  FInfo.AddPair('Format', CODES_TFHIRFormat[format]);
  FInfo.AddPair(name, value);
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

function TToolkitEditSession.hasAddress: boolean;
begin
  result := FAddress <> '';
end;

{ TToolkitEditor }

function TToolkitEditor.GetHasAddress: boolean;
begin
  result := Session.Address <> '';
end;

function TToolkitEditor.GetHint: String;
begin
  if FSession.address = '' then
    result := 'File has not been saved'
  else
    result := FSession.address;
end;

function TToolkitEditor.getIsFile: boolean;
begin
  result := session.address.startsWith('file:');
end;

procedure TToolkitEditor.SetStore(AValue: TStorageService);
begin
  FStore.Free;
  FStore:=AValue;
end;

function TToolkitEditor.StartValidating : QWord;
begin
  FValidationIssues := TFslList<TToolkitMessage>.create;
  result := GetTickCount64;
end;

procedure TToolkitEditor.validationError(loc : TSourceLocation; msg: String);
begin
  FValidationIssues.add(TToolkitMessage.create(self, loc, msgError, msg));
end;

procedure TToolkitEditor.validationWarning(loc : TSourceLocation; msg: String);
begin
  FValidationIssues.add(TToolkitMessage.create(self, loc, msgWarning, msg));
end;

procedure TToolkitEditor.finishValidating(validating: boolean; start: QWord);
begin
  if (validating) then
  begin
    Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - start)+'ms');
    Context.MessageView.SetMessagesForEditor(self, FValidationIssues);
  end;
  FValidationIssues.Free;
  FValidationIssues := nil;
end;

constructor TToolkitEditor.create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService);
var
  ss : TToolkitEditSession;
  i : integer;
  ok : boolean;
begin
  inherited create(context);
  FPause := 1000;
  FSession := session;
  FStore := store;
  if (session.caption = '') then
  begin
    if (FStore <> nil) then
      session.caption := store.CaptionForAddress(session.address)
    else
    begin
      i := 0;
      repeat
        inc(i);
        ok := true;
        for ss in context.FEditorSessions do
          if (ss.Caption = 'new_file_'+inttostr(i)+'.'+FileExtension) then
            ok := false;
      until ok;
      session.Caption := 'new_file_'+inttostr(i)+'.'+FileExtension;
    end;
  end;
end;

destructor TToolkitEditor.Destroy;
begin
  FSession.Free;
  FStore.Free;
  inherited Destroy;
end;

function TToolkitEditor.link: TToolkitEditor;
begin
  result := TToolkitEditor(inherited Link);
end;

function TToolkitEditor.hasStore: boolean;
begin
  result := FStore <> nil;
end;

function TToolkitEditor.describe: String;
begin
  if hasStore then
    result := FStore.describe(Session.Address)
  else
    result := 'File';
end;

procedure TToolkitEditor.bindToTab(tab: TTabSheet);
begin
  FTab := tab;
  tab.Caption := FSession.Caption;
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

procedure TToolkitContext.SetFocus(AValue: TToolkitEditor);
begin
  FFocus := AValue;
  FOnChangeFocus(self);
end;

procedure TToolkitContext.SetSideBySide(AValue: boolean);
var
  editor : TToolkitEditor;
begin
  if FSideBySide <> AValue then
  begin
    FSideBySide := AValue;
    for editor in Editors do
      editor.ChangeSideBySideMode;
  end;
end;

procedure TToolkitContext.SetToolBarHeight(AValue: integer);
var
 editor : TToolkitEditor;
begin
  if FToolBarHeight=AValue then Exit;
  FToolBarHeight:=AValue;
  for editor in FEditors do
    editor.resizeControls;
end;

constructor TToolkitContext.Create(images: TImageList; actions : TActionList);
begin
  inherited Create;
  FEditorSessions := TFslList<TToolkitEditSession>.create;
  FEditors := TFslList<TToolkitEditor>.create;
  FStorages := TFslList<TStorageService>.create;
  FMessageView := TToolkitMessagesView.create;
  FInspector := TToolkitEditorInspectorView.create;
  FConsole := TToolkitConsole.create;
  FImages := images;
  FActions := actions;
end;

destructor TToolkitContext.Destroy;
begin
  FInspector.Free;
  FMessageView.Free;
  FConsole.Free;
  FStorages.Free;
  FEditors.Free;
  FEditorSessions.Free;
  inherited Destroy;
end;

function TToolkitContext.link: TToolkitContext;
begin
  result := TToolkitContext(inherited Link);
end;

function TToolkitContext.locateOnScreen(x, y: Integer): TPoint;
begin
  OnLocate(self, x, y, result);
end;

procedure TToolkitContext.updateFont;
var
  editor : TToolkitEditor;
begin
  for editor in Editors do
    editor.updateFont;
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

procedure TToolkitContext.addEditor(editor : TToolkitEditor);
begin
  FMessageView.removeMessagesForEditor(editor);
  FEditors.add(editor);
  FEditorSessions.add(editor.Session.link);
end;

procedure TToolkitContext.removeEditor(editor: TToolkitEditor);
begin
  if (editor = FFocus) then
  begin
    editor.loseFocus();
    FFocus := nil;
  end;
  FMessageView.removeMessagesForEditor(editor);
  FEditorSessions.remove(editor.Session.link);
  FEditors.remove(editor);
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
  begin
    if editor.session.Address = address then
      exit(editor);
    if editor.session.Guid = address then
      exit(editor);
  end;
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

