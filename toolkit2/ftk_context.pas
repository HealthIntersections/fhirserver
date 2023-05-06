unit ftk_context;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Classes, SysUtils,
  Graphics, Controls, ExtCtrls, ComCtrls, Menus, ActnList, IniFiles,
  MarkdownProcessor,
  fsl_base, fsl_utilities, fsl_stream, fsl_logging, fsl_lang, fsl_npm_cache, fsl_http,
  fhir_objects, fhir_client, fhir_factory, fhir_parser,
  fhir4_factory, fhir3_factory,
  ftk_store, ftk_console, ftk_utilities;

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
  TSourceEditorKind = (sekNull, sekFHIR, sekv2, sekCDA, sekXML, sekJson, sekLiquid, sekMap, sekIni, sekText, sekMD, sekJS, sekHTML, sekDicom, sekServer, sekJWT, sekHome, sekIGs);
  TSourceEditorKindSet = set of TSourceEditorKind;
  TSourceEncoding = (senUnknown, senBinary, senUTF8, senASCII, senUTF16BE, senUTF16LE);
  TSourceLineMarker = (slUnknown, slCRLF, slCR, slLF);
  TToolkitMessageLevel = (msgError, msgWarning, msgHint);

const
  {$IFDEF WINDOWS}
  PLATFORM_DEFAULT_EOLN = slCRLF;
  {$ELSE}
  PLATFORM_DEFAULT_EOLN = slLF;
  {$ENDIF}

  CODES_TSourceEditorKind : Array [TSourceEditorKind] of String = ('Unknown', 'FHIR', 'v2', 'CDA', 'XML', 'Json', 'Liquid', 'Map', 'Ini', 'Text', 'MD', 'JS', 'HTML', 'Dicom', 'Server', 'JWT', 'HomePage', 'IGPage');
  EXTENSIONS_TSourceEditorKind : Array [TSourceEditorKind] of String = ('', '.json', '.hl7', '.xml', '.xml', '.json', '.liquid', '.map', '.ini', '.txt', '.md', '.js', '.html', '.dcm', '', '.jwt', '', '');
  NAMES_TSourceEditorKind : Array [TSourceEditorKind] of String = ('Unknown', 'FHIR Resource', 'v2 Message/Bach', 'CDA Document', 'XML Document', 'Json Document', 'Liquid Script', 'Structure Map', 'IniFile', 'Text', 'Markdown', 'Javascript', 'HTML', 'Dicom', 'Server Source', 'JWT (Json Web Token)', 'Home Page', 'IG Page');
  CODES_TSourceEncoding : Array [TSourceEncoding] of String = ('Unknown', 'Binary', 'UTF8', 'ASCII', 'UTF16BE', 'UTF16LE');
  CODES_TSourceLineMarker : Array [TSourceLineMarker] of String = ('Unknown', 'CRLF', 'CR', 'LF');
  CODES_TToolkitMessageLevel : Array [TToolkitMessageLevel] of String = ('Error', 'Warning', 'Hint');
  ICONS_TSourceEditorKind : Array [TSourceEditorKind] of integer = (-1, 42, 114, 113, 115, 116, 117, -1, 118, 119, 120, 121, 122, 123, -1, 124, -1, -1);
  ALL_SourceEditorKinds = [sekNull..sekIGs];
  FILE_SourceEditorKinds = [sekFHIR, sekv2, sekCDA, sekXML, sekJson, sekLiquid, sekMap, sekIni, sekText, sekMD, sekJS, sekHTML, sekDicom, sekJWT];


function onlySourceKind(kinds : TSourceEditorKindSet) : TSourceEditorKind;

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
    FKnownToBeDeleted: Boolean;
    FNeedsSaving: boolean;
    FNextCurrencyCheck: TDateTime;
    FNoCheckCurrency: Boolean;
    FTimestamp: TDateTime;
    procedure SetCaption(AValue: String);
  public
    constructor Create; override;
    constructor Create(kind : TSourceEditorKind); overload;
    constructor CreateResource(format : TFHIRFormat; name, value : string); overload;
    destructor Destroy; override;
    function link : TToolkitEditSession; overload;

    property Guid : String read FGuid write FGuid;
    property Address : String read FAddress write FAddress;
    function hasAddress : boolean;
    function presentedAddress : String;
    property Caption : String read FCaption write SetCaption;
    property kind : TSourceEditorKind read FKind write FKind;
    property Encoding : TSourceEncoding read FEncoding write FEncoding;
    property EndOfLines : TSourceLineMarker read FEndOfLines write FEndOfLines;
    property HasBOM : boolean read FHasBOM write FHasBOM;
    property NeedsSaving : boolean read FNeedsSaving write FNeedsSaving;
    property Timestamp : TDateTime read FTimestamp write FTimestamp;
    property KnownToBeDeleted : Boolean read FKnownToBeDeleted write FKnownToBeDeleted;
    property NoCheckCurrency : Boolean read FNoCheckCurrency write FNoCheckCurrency;
    property nextCurrencyCheck : TDateTime read FNextCurrencyCheck write FNextCurrencyCheck;

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
    procedure SetLastChange(AValue: int64);
    procedure SetLastChangeChecked(AValue: boolean);
    procedure SetStore(AValue: TStorageService);
  protected
    FTab: TTabSheet;

    function GetCanBeSaved: boolean; virtual; abstract;
    function GetCanEscape : boolean; virtual;

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
    function EditorTitle : String; virtual;

    // editing related functionality
    function makeRootPanel(tab: TTabSheet) : TPanel;
    property Pause : integer read FPause write FPause;
    procedure bindToTab(tab : TTabSheet; pnl : TPanel); virtual;  // set up the UI - caleed before either newContent or LoadBytes is called
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
    procedure ChangeToolbarButtons; virtual;
    function hasDesigner : boolean; virtual; abstract;
    function hasTextTab : boolean; virtual; abstract;
    function IsShowingDesigner : boolean; virtual; abstract;
    procedure showDesigner; virtual; abstract;
    procedure showTextTab; virtual; abstract;
    procedure BeginEndSelect; virtual; abstract;
    procedure updateSettings; virtual; abstract;
    function getSource : String; virtual; abstract;
    procedure resizeControls; virtual; abstract;
    procedure saveStatus; virtual; // called before shut down because shut down order isn't always predictable
    procedure insertText(text : String; escape : boolean); virtual; abstract;
    function pollForInspector : boolean; virtual;
    procedure inspect; virtual;

    // status for actions
    property CanBeSaved : boolean read GetCanBeSaved;
    property hasText : boolean read FHasText write FHasText;
    property canUndo : boolean read FCanUndo write FCanUndo;
    property canRedo : boolean read FCanRedo write FCanRedo;
    property canCut : boolean read FCanCut write FCanCut;
    property canPaste : boolean read FCanPaste write FCanPaste;
    property canEscape : boolean read GetCanEscape;
    property isFile : boolean read getIsFile;
    property hasAddress : boolean read GetHasAddress;

    property lastChange : int64 read FLastChange write SetLastChange;
    property lastChangeChecked : boolean read FLastChangeChecked write SetLastChangeChecked;
    property lastMove : int64 read FLastMove write FLastMove;
    property lastMoveChecked : boolean read FLastMoveChecked write FLastMoveChecked;

    property Hint : String read GetHint;
  end;

  TLocateEvent = procedure(sender : TObject; x, y: integer; var point : TPoint) of Object;
  TFetchServerEvent = function(sender : TObject; name : String) : TFHIRServerEntry of Object;
  TOpenResourceUrlEvent = procedure(sender : TObject; url : String) of Object;
  TOpenResourceObjEvent = procedure(sender : TObject; obj : TFHIRResourceV) of Object;
  TOpenResourceSrcEvent = procedure(sender : TObject; src : TBytes; format : TFHIRFormat; version : TFHIRVersion) of Object;
  TOpenSourceEvent = procedure(sender : TObject; src : TBytes; kind : TSourceEditorKind) of Object;
  TConnectToServerEvent = procedure (sender : TObject; server : TFHIRServerEntry) of object;

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

  { TToolkitContextTerminologyServer }

  TToolkitContextTerminologyServer = class (TFHIRTerminologyService)
  private
    FAddress: String;
    FDefault: boolean;
    FId: String;
    FName: String;
    FVersion: TFHIRVersion;
  public
    Constructor Create; override;
    function link : TToolkitContextTerminologyServer; overload;

    function copy : TToolkitContextTerminologyServer;
    procedure updateFrom(src : TToolkitContextTerminologyServer);

    property id : String read FId write FId; // not persistent
    property name : String read FName write FName;
    property address : String read FAddress write FAddress;
    property version : TFHIRVersion read FVersion write FVersion;
    property default : boolean read FDefault write FDefault;
  end;


  { TToolkitContextTerminologyServers }

  TToolkitContextTerminologyServers = class (TFslObject)
  private
    FCache: String;
    FList : TFslList<TToolkitContextTerminologyServer>;
    FLogFile: String;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure load(ini : TIniFile);
    procedure save(ini : TIniFile);

    function defaultServer : TToolkitContextTerminologyServer;
    function getByName(name : String) : TToolkitContextTerminologyServer;
    function getByID(id : String) : TToolkitContextTerminologyServer;

    property list : TFslList<TToolkitContextTerminologyServer> read FList;
    property logFile : String read FLogFile write FLogFile;
    property cache : String read FCache write FCache;
  end;

  { TToolkitContext }

  TToolkitContext = class (TFslObject)
  private
    FActions: TActionList;
    FConsole: TToolkitConsole;
    FEditorFont: TFont;
    FViewFont: TFont;
    FLogFont: TFont;
    FInspector: TToolkitEditorInspectorView;
    FLanguages: TIETFLanguageDefinitions;
    FMessageView : TToolkitMessagesView;
    FOnChangeFocus: TNotifyEvent;
    FOnConnectToServer: TConnectToServerEvent;
    FOnFetchServer: TFetchServerEvent;
    FOnLocate : TLocateEvent;
    FImages : TImageList;
    FEditorSessions : TFslList<TToolkitEditSession>;
    FEditors : TFslList<TToolkitEditor>;
    FFocus: TToolkitEditor;
    FOnOpenResourceUrl: TOpenResourceUrlEvent;
    FOnOpenResourceObj: TOpenResourceObjEvent;
    FOnOpenResourceSrc: TOpenResourceSrcEvent;
    FOnOpenSource: TOpenSourceEvent;
    FOnUpdateActions: TNotifyEvent;
    FSideBySide: boolean;
    FStorages: TFslList<TStorageService>;
    FToolbarCaptions: boolean;
    FTxServers: TToolkitContextTerminologyServers;
    FToolBarHeight: integer;
    FSettings : TiniFile;
    FContexts : Array [TFHIRVersion] of TFHIRWorkerContextWithFactory;
    FPcm : TFHIRPackageManager;

    function GetContext(version : TFHIRVersion): TFHIRWorkerContextWithFactory;
    function GetFocus: TToolkitEditor;
    function GetHasFocus: boolean;
    procedure SetContext(version : TFHIRVersion;
      AValue: TFHIRWorkerContextWithFactory);
    procedure SetFocus(AValue: TToolkitEditor);
    procedure SetLanguages(AValue: TIETFLanguageDefinitions);
    procedure SetSideBySide(AValue: boolean);
    procedure SetToolbarCaptions(AValue: boolean);
    procedure SetToolBarHeight(AValue: integer);
  public
    constructor Create(images : TImageList; actions : TActionList);
    destructor Destroy; override;
    function link : TToolkitContext; overload;

    // UI support
    property images : TImageList read FImages;
    property actions : TActionList read FActions;
    function locateOnScreen(x, y : Integer) : TPoint;
    property EditorFont : TFont read FEditorFont write FEditorFont;
    property ViewFont : TFont read FViewFont write FViewFont;
    property LogFont : TFont read FLogFont write FLogFont;
    procedure updateSettings;
    property ToolBarHeight : integer read FToolBarHeight write SetToolBarHeight;

    property hasFocus : boolean read GetHasFocus;
    property focus : TToolkitEditor read FFocus write SetFocus;

    property Settings : TIniFile read FSettings write FSettings;
    property editorSessions : TFslList<TToolkitEditSession> read FEditorSessions;
    property storages : TFslList<TStorageService> read FStorages;
    function StorageForAddress(address : String; server : TFHIRServerEntry = nil) : TStorageService;
    property Editors : TFslList<TToolkitEditor> read FEditors;
    property TxServers : TToolkitContextTerminologyServers read FTxServers;
    property Languages : TIETFLanguageDefinitions read FLanguages write SetLanguages;
    property pcm : TFHIRPackageManager read FPcm;

    // global settings
    property SideBySide : boolean read FSideBySide write SetSideBySide;
    property ToolbarCaptions : boolean read FToolbarCaptions write SetToolbarCaptions;

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

    function factory(version : TFHIRVersion) : TFHIRFactory;
    procedure OpenResource(url : String);

    property context[version : TFHIRVersion] : TFHIRWorkerContextWithFactory read GetContext write SetContext;

    procedure saveTempResource(r : TFHIRResourceV; filename : String);
    function processMarkdown(md : String) : String;

    property OnUpdateActions : TNotifyEvent read FOnUpdateActions write FOnUpdateActions;
    property OnChangeFocus : TNotifyEvent read FOnChangeFocus write FOnChangeFocus;
    property OnLocate : TLocateEvent read FOnLocate write FOnLocate;
    property OnFetchServer : TFetchServerEvent read FOnFetchServer write FOnFetchServer;
    property OnOpenResourceUrl : TOpenResourceUrlEvent read FOnOpenResourceUrl write FOnOpenResourceUrl;
    property OnOpenResourceObj : TOpenResourceObjEvent read FOnOpenResourceObj write FOnOpenResourceObj;
    property OnOpenResourceSrc : TOpenResourceSrcEvent read FOnOpenResourceSrc write FOnOpenResourceSrc;
    property OnOpenSource : TOpenSourceEvent read FOnOpenSource write FOnOpenSource;
    property OnConnectToServer : TConnectToServerEvent read FOnConnectToServer write FOnConnectToServer;
  end;

implementation

var
  GServerId : integer = 0;

{ TToolkitContextTerminologyServer }

constructor TToolkitContextTerminologyServer.Create;
begin
  inherited Create;
  inc(GServerId);
  FId := inttostr(GServerId);
end;

function TToolkitContextTerminologyServer.link: TToolkitContextTerminologyServer;
begin
  result := TToolkitContextTerminologyServer(inherited Link);
end;

function TToolkitContextTerminologyServer.copy: TToolkitContextTerminologyServer;
begin
  result := TToolkitContextTerminologyServer.create;
  try
    result.id := id;
    result.name := name;
    result.address := address;
    result.default := default;
    result.link;
  finally
    result.free;
  end;
end;

procedure TToolkitContextTerminologyServer.updateFrom(src: TToolkitContextTerminologyServer);
begin
  name := src.name;
  address := src.address;
  default := src.default;
end;

{ TToolkitContextTerminologyServers }

constructor TToolkitContextTerminologyServers.Create;
begin
  inherited Create;
  FList := TFslList<TToolkitContextTerminologyServer>.create;
end;

destructor TToolkitContextTerminologyServers.Destroy;
begin
  FList.free;
  inherited Destroy;
end;

procedure TToolkitContextTerminologyServers.load(ini: TIniFile);
var
  i, t : integer;
  s : String;
  srvr : TToolkitContextTerminologyServer;
begin
  t := ini.readInteger('tx', 'server-count', 0);

  for i := 1 to t do
  begin
    srvr := TToolkitContextTerminologyServer.create;
    try
      srvr.Name := ini.readString('tx', 'server'+inttostr(i), '');
      srvr.Address := ini.readString('tx', 'server'+inttostr(i)+'-address', '');
      FList.add(srvr.link);
    finally
      srvr.free;
    end;
  end;
  if (FList.Count = 0) then
  begin
    srvr := TToolkitContextTerminologyServer.create;
    try
      srvr.Name := 'tx.fhir.org';
      srvr.Address := 'http://tx.fhir.org';
      FList.add(srvr.link);
    finally
      srvr.free;
    end;
  end;
  s := ini.ReadString('tx', 'server', 'tx.fhir.org');
  srvr := getByName(s);
  if (srvr = nil) then
    FList[0].default := true
  else
    srvr.default := true;
  FLogFile := ini.ReadString('tx', 'log', '');
  FCache := IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-tx.cache';
end;

procedure TToolkitContextTerminologyServers.save(ini: TIniFile);
var
  i : integer;
  srvr : TToolkitContextTerminologyServer;
begin
  ini.WriteInteger('tx', 'server-count', FList.count);
  for i := 1 to FList.count  do
  begin
    srvr := FList[i-1];
    ini.writeString('tx', 'server'+inttostr(i), srvr.name);
    ini.writeString('tx', 'server'+inttostr(i)+'-address', srvr.Address);
    if (srvr.default) then
      ini.writeString('tx', 'server', srvr.name);
  end;
  ini.WriteString('tx', 'log', FLogFile);
end;

function TToolkitContextTerminologyServers.defaultServer: TToolkitContextTerminologyServer;
var
  t : TToolkitContextTerminologyServer;
begin
  result := nil;
  for t in FList do
    if t.default then
      exit(t);
  if (FList.count > 0) then
    result := FList[0];
end;

function TToolkitContextTerminologyServers.getByName(name: String): TToolkitContextTerminologyServer;
var
  t : TToolkitContextTerminologyServer;
begin
  result := nil;
  for t in FList do
    if (t.name = name) then
      exit(t);
end;

function TToolkitContextTerminologyServers.getByID(id: String): TToolkitContextTerminologyServer;
var
  t : TToolkitContextTerminologyServer;
begin
  result := nil;
  for t in FList do
    if (t.id = id) then
      exit(t);
end;

{ TToolkitEditorInspectorView }

procedure TToolkitEditorInspectorView.SetActive(AValue: boolean);
begin
  FActive := AValue;
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
var
  bDo : boolean;
  i : integer;
begin
  bDo := ts.count <> FSource.count;
  if not bDo then
  begin
    for i := 0 to ts.count - 1 do
      bDo := bDo or (ts[i] <> FSource[i]);
  end;
  if (bDo) then
  begin
    FSource.Clear;
    FSource.Assign(ts);
    Active := true;
    OnChange(self);
  end;
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

procedure TToolkitEditSession.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

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

function TToolkitEditSession.presentedAddress: String;
begin
  if Address.StartsWith('file:') then
    result := Address.Substring(5)
  else
    result := Address;
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

procedure TToolkitEditor.SetLastChange(AValue: int64);
begin
  if FLastChange=AValue then Exit;
  FLastChange:=AValue;
end;

procedure TToolkitEditor.SetLastChangeChecked(AValue: boolean);
begin
  if FLastChangeChecked=AValue then Exit;
  FLastChangeChecked:=AValue;
end;

procedure TToolkitEditor.SetStore(AValue: TStorageService);
begin
  FStore.Free;
  FStore := AValue;
end;

function TToolkitEditor.GetCanEscape: boolean;
begin
  result := false;
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
    session.Caption := EditorTitle;
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

function TToolkitEditor.EditorTitle: String;
begin
  result := '';
end;

function TToolkitEditor.makeRootPanel(tab: TTabSheet): TPanel;
begin
  result := TPanel.create(tab);
  result.Parent := tab;
  result.align := alClient;
  result.BevelInner := bvNone;
  result.BevelOuter := bvNone;
  result.BorderWidth := 0;
  result.PopupMenu := TPopupMenu.create(tab);
  result.ShowHint := true;
  result.Hint := '';
  result.ParentShowHint := false;
end;

procedure TToolkitEditor.bindToTab(tab: TTabSheet; pnl : TPanel);
begin
  FTab := tab;
  tab.Caption := FSession.Caption{$IFDEF WINDOWS}+'    '{$ENDIF};
end;

procedure TToolkitEditor.ChangeToolbarButtons;
begin
  // nothing
end;

procedure TToolkitEditor.saveStatus;
begin
  FContext := nil; // we're cut off after this executes
end;

function TToolkitEditor.pollForInspector: boolean;
begin
  result := false;
end;

procedure TToolkitEditor.inspect;
begin

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

function TToolkitContext.GetContext(version : TFHIRVersion): TFHIRWorkerContextWithFactory;
begin
  result := FContexts[version];
end;

procedure TToolkitContext.SetContext(version : TFHIRVersion; AValue: TFHIRWorkerContextWithFactory);
begin
  FContexts[version].Free;
  FContexts[version] := AValue;
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

procedure TToolkitContext.SetLanguages(AValue: TIETFLanguageDefinitions);
begin
  FLanguages.free;
  FLanguages := AValue;
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

procedure TToolkitContext.SetToolbarCaptions(AValue: boolean);
var
  editor : TToolkitEditor;
begin
  if FToolbarCaptions <> AValue then
  begin
    FToolbarCaptions := AValue;
    for editor in Editors do
      editor.ChangeToolbarButtons;
  end;
end;

procedure TToolkitContext.SetToolBarHeight(AValue: integer);
var
 editor : TToolkitEditor;
begin
  if FToolBarHeight=AValue then Exit;
  FToolBarHeight := AValue;
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
  Logging.addListener(FConsole);
  FImages := images;
  FActions := actions;
  FPcm := TFHIRPackageManager.create(npmModeUser);
  FTxServers := TToolkitContextTerminologyServers.create;
end;

destructor TToolkitContext.Destroy;
var
  a : TFHIRVersion;
begin
  FPcm.free;
  for a in TFHIRVersion do
    FContexts[a].Free;
  FTxServers.Free;
  FInspector.Free;
  FMessageView.Free;
  Logging.removeListener(FConsole);
  FConsole.Free;
  FStorages.Free;
  FEditors.Free;
  FEditorSessions.Free;
  FLanguages.free;
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

procedure TToolkitContext.updateSettings;
var
  editor : TToolkitEditor;
begin
  for editor in Editors do
    editor.updateSettings;
end;

function TToolkitContext.StorageForAddress(address: String; server : TFHIRServerEntry = nil): TStorageService;
var
  scheme : String;
  storage : TStorageService;
begin
  scheme := address.Substring(0, address.IndexOf(':'));
  result := nil;
  for storage in storages do
    if StringArrayExists(storage.schemes, scheme) and storage.inScope(address) then
      exit(storage);
  for storage in storages do
    if StringArrayExists(storage.schemes, scheme) then
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
  FEditorSessions.remove(editor.Session);
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

function TToolkitContext.factory(version: TFHIRVersion): TFHIRFactory;
begin
  case version of
    fhirVersionRelease3 : result := TFHIRFactoryR3.create;
    fhirVersionRelease4 : result := TFHIRFactoryR4.create;
  else
    raise EFHIRException.create('The version '+CODES_TFHIRVersion[version]+' is not supported by the FHI Toolkit');
  end;
end;

procedure TToolkitContext.OpenResource(url: String);
begin
  OnOpenResourceUrl(self, url);
end;

procedure TToolkitContext.saveTempResource(r: TFHIRResourceV; filename: String);
var
  ctxt : TFHIRWorkerContextWithFactory;
  parser : TFHIRComposer;
  f : TFileStream;
begin
  ctxt := context[r.fhirObjectVersion];
  parser := ctxt.factory.makeComposer(ctxt.link, ffJson, defLang, OutputStylePretty);
  try
    f := TFileStream.create(FilePath(['[tmp]', filename+'.json']), fmCreate);
    try
      parser.compose(f, r);
    finally
      f.free;
    end;
  finally
    parser.free;
  end;

end;

function TToolkitContext.processMarkdown(md: String): String;
var
  proc : TMarkdownProcessor;
begin
  proc := TMarkdownProcessor.createDialect(mdCommonMark);
  try
    proc.allowUnsafe := false;
    result := proc.process(md);
  finally
    proc.free;
  end;
end;

function onlySourceKind(kinds : TSourceEditorKindSet) : TSourceEditorKind;
var
  a : TSourceEditorKind;
  c : integer;
begin
  result := sekNull;
  c := 0;
  for a in TSourceEditorKindSet do
    if a in kinds then
      inc(c);
  if c = 1 then
    for a in TSourceEditorKindSet do
      if a in kinds then
        exit(a);
end;

end.

