unit ValueSetEditorCore;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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


interface

uses
  SysUtils, Classes, IniFiles, ZLib, Math, RegExpr,
  Dialogs,
  SystemSupport, StringSupport, FileSupport,  ShellSupport, GuidSupport, EncodeSupport,
  AdvObjects, AdvStringMatches, AdvStringObjectMatches, AdvObjectLists, AdvBuffers, AdvWinInetClients, AdvMemories, AdvFiles, AdvGenerics,
  MsXmlParser, IdUri, IdHTTP, AdvJSON,
  FHIRBase, FHIRTypes, FHIRResources, FHIRParser, FHIRParserBase, FHIRConstants,
  FHIRUtilities, FHIRClient;

Const
  UIState_Welcome = 0;
  UIState_Choose = 1;
  UIState_Edit = 2;

  ExpressionProperty = 'http://healthintersections.com.au/valueseteditor/expression';
  MASTER_SERVER = 'http://fhir2.healthintersections.com.au/open';

  CS_LIST = 'uri:uuid:185E783F-BE0D-451E-BEC1-2C92971BC762';
  VS_LIST = 'uri:uuid:3BCEFFA8-4FF0-4EAC-9328-1B671CEC0B55';


Type
  ENoServer = class (Exception);

  TValidationOutcomeKind = (voOK, voMissing, voError, voWarning, voHint);
  TValidationOutcome = record
    kind : TValidationOutcomeKind;
    msg : String;
  end;

  TClosureDirection = (cdNull, cdSubsumes, cdSubsumed);

  TClosureTableRecordSource = class (TFHIRCoding)
  private
    FTargets: TAdvList<TFHIRCoding>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link :  TClosureTableRecordSource; overload;

    property targets : TAdvList<TFHIRCoding> read FTargets;
    procedure add(uri, code : String);
  end;

  TClosureTableRecord = class (TAdvObject)
  private
    FConcepts: TAdvList<TFHIRCoding>;
    FMaps : TAdvList<TClosureTableRecordSource>;

    FId: string;
    FName: String;
    FFilename : String;
    FVersion: String;
    function GetLinks(row, col: integer): TClosureDirection;
    function hasMap(src, tgt : TFHIRCoding) : boolean;
    procedure save;
    procedure process(cm : TFHIRConceptMap);
    function getSource(uri, code : String) : TClosureTableRecordSource;
    function GetMapCount: integer;
  public
    constructor create; overload; override;
    constructor create(filename : String); overload;
    constructor create(filename, id, name : String); overload;
    destructor Destroy; override;

    function link :  TClosureTableRecord; overload;

    property id : string read FId write FId;
    property name : String read FName write FName;
    property version : String read FVersion write FVersion;
    property concepts : TAdvList<TFHIRCoding> read FConcepts;
    property maps : TAdvList<TClosureTableRecordSource> read FMaps;
    property links[row, col: integer] : TClosureDirection read GetLinks;
    property mapCount : integer read GetMapCount;
  end;

  TValidationOutcomeMark = class (TAdvObject)
  private
    FMessage: string;
    FObj: TFHIRElement;
    FField: integer;
    FKind: TValidationOutcomeKind;
    procedure SetObj(const Value: TFHIRElement);
  public
    destructor destroy; override;
    property obj : TFHIRElement read FObj write SetObj;
    property field : integer read FField write FField;
    property kind : TValidationOutcomeKind read FKind write FKind;
    property message : string read FMessage write FMessage;
  end;

  TValidationOutcomeMarkList = class (TAdvObjectList)
  private
    function GetMark(index: integer): TValidationOutcomeMark;
  protected
    Function CompareByElement(pA, pB : Pointer) : Integer;
    function itemClass : TAdvObjectClass; override;
  public
    Constructor Create; Override;
    property mark[index : integer] : TValidationOutcomeMark read GetMark; default;
    function GetByElement(elem : TFHIRElement; field : integer) : TValidationOutcomeMark;
    procedure AddElement(elem : TFHIRElement; field : Integer; kind : TValidationOutcomeKind; message : String);
  end;

  TValueSetEditorCoreSettings = Class (TAdvObject)
  private
    ini : TIniFile;
    FMRUList : TStringList;

    function ValuesetListPath : string;
    function ValuesetItemPath : string;

    procedure SetServerURL(const Value: String);
    function GetServerURL: String;
    function GetValueSetId: String;
    procedure SetValueSetId(const Value: String);
    function GetvalueSetFilename: String;
    procedure SetvalueSetFilename(const Value: String);
    function GetVersionHigh: integer;
    procedure SetVersionHigh(const Value: integer);
    function GetFormatIsJson: boolean;
    procedure SetFormatIsJson(const Value: boolean);
    function GetFilter: String;
    procedure SetFilter(const Value: String);
    function GetWindowHeight: Integer;
    function GetWindowLeft: Integer;
    function GetWindowState: Integer;
    function GetWindowTop: Integer;
    function GetWindowWidth: Integer;
    procedure SetWindowHeight(const Value: Integer);
    procedure SetWindowLeft(const Value: Integer);
    procedure SetWindowState(const Value: Integer);
    procedure SetWindowTop(const Value: Integer);
    procedure SetWindowWidth(const Value: Integer);
    function GetDocoVisible: boolean;
    procedure SetDocoVisible(const Value: boolean);
    function GetHasViewedWelcomeScreen: boolean;
    procedure SetHasViewedWelcomeScreen(const Value: boolean);
    function GetValueSetURL: String;
    procedure SetValueSetURL(const Value: String);
    function GetServerFilter: String;
    procedure SetServerFilter(const Value: String);

    property valueSetFilename : String read GetvalueSetFilename write SetvalueSetFilename;
    property valueSetServer : String read GetValueSetURL write SetValueSetURL;
    property valueSetId : String read GetValueSetId write SetValueSetId;
    property VersionHigh : integer read GetVersionHigh write SetVersionHigh;
    Property FormatIsJson : boolean read GetFormatIsJson write SetFormatIsJson;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    // mru list
    procedure mru(src : String);
    property MRUList : TStringList read FMRUList;

    // window
    function hasWindowState : Boolean;
    Property WindowState : Integer read GetWindowState write SetWindowState;
    Property WindowLeft : Integer read GetWindowLeft write SetWindowLeft;
    Property WindowWidth : Integer read GetWindowWidth write SetWindowWidth;
    Property WindowTop : Integer read GetWindowTop write SetWindowTop;
    Property WindowHeight : Integer read GetWindowHeight write SetWindowHeight;
    property DocoVisible : boolean read GetDocoVisible write SetDocoVisible;
    property HasViewedWelcomeScreen : boolean read GetHasViewedWelcomeScreen write SetHasViewedWelcomeScreen;

    // servers
    function ServerCount : integer;
    procedure getServer(index : integer; var name, address, username, password : String; var doesSearch : boolean);
    procedure AddServer(name, address, username, password : String; doesSearch : boolean);
    procedure UpdateServer(name, address, username, password : String);
    procedure DeleteServer(name : String);
    property WorkingServer : String read GetServerURL write SetServerURL;

    // choice browser
    function columnWidth(tree, name : string; default: integer) : integer;
    procedure setColumnWidth(tree, name : string; value : integer);

    function ValueSetNew : boolean;

    // expansion
    Property Filter : String read GetFilter write SetFilter;
    Property ServerFilter : String read GetServerFilter write SetServerFilter;
  End;

  TValueSetEditorCodeSystemCodeStatus = (cscsUnknown, cscsOK, cscsPending, cscsInvalidSystem);

  TValueSetEditorCodeSystem = class (TAdvObject)
  public
    function getCodeStatus(code : String; var msg : String) : TValueSetEditorCodeSystemCodeStatus; virtual;
    function isWrongDisplay(code : String; display : String) : boolean; virtual;
    function getDisplay(code : String; var display : String) : boolean; virtual;

    function filterPropertyOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; virtual;
    function filterOperationOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; virtual;
    function filterValueOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; virtual;
  end;

  TValueSetEditorCodeSystemValueSet = class (TValueSetEditorCodeSystem)
  private
    FConceptList : TFhirValueSetCodeSystemConceptList;
    FCase : boolean;
    function InList(list: TFhirValueSetCodeSystemConceptList; code: String): TFhirValueSetCodeSystemConcept;
  public
    constructor create(vs : TFhirValueSet); overload;
    destructor destroy; override;

    function getCodeStatus(code : String; var msg : String) : TValueSetEditorCodeSystemCodeStatus; override;
    function isWrongDisplay(code : String; display : String) : boolean; override;
    function getDisplay(code : String; var display : String) : boolean; override;

    function filterPropertyOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
    function filterOperationOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
    function filterValueOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
  end;

  TServerCodeSystemCacheItem = class (TAdvObject)
  private
    status : TValueSetEditorCodeSystemCodeStatus;
    message : String;
    displays : TStringList;
  public
    constructor Create; overload; override;
    constructor Create(status : TValueSetEditorCodeSystemCodeStatus; message : String); overload;
    destructor Destroy; override;
  end;

  TServerCodeSystem = class (TValueSetEditorCodeSystem)
  private
    FSystem : String;
    FClient : TFhirHTTPClient;
    FCache : TAdvStringObjectMatch;
    FFilename : String;

    function HasCache(code : String; var item : TServerCodeSystemCacheItem) : boolean;
    procedure loadCode(code : String; var item : TServerCodeSystemCacheItem);
    procedure Load;
    procedure Save;
  public
    constructor create(uri : String; url : String; filename : String); overload; virtual;
    destructor Destroy; override;
    function getCodeStatus(code : String; var msg : String) : TValueSetEditorCodeSystemCodeStatus; override;
    function isWrongDisplay(code : String; display : String) : boolean; override;
    function getDisplay(code : String; var display : String) : boolean; override;

    function filterPropertyOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
    function filterOperationOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
    function filterValueOK(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;
  end;

  TValueSetEditorServerCache = class (TAdvObject)
  private
    FLoaded : boolean;
    FDoesSearch : boolean;
    FName: String;
    FUrl : String;
    Fusername : String;
    Fpassword : String;
    FLastUpdated : String;
    FKey : integer;
    ini : TInifile;
    Flist : TFHIRValueSetList;
    valuesets : TAdvStringObjectMatch; // uri and actual value set (might be a summary)
    FCodeSystems : TAdvStringObjectMatch; // uri and actual value set (might be a summary)
    sortedCodesystems : TStringList;
    sortedValueSets : TStringList;
    specialCodeSystems : TAdvStringObjectMatch;
    closures : TAdvMap<TClosureTableRecord>;
    procedure SeeValueset(vs : TFhirValueSet; isSummary, loading : boolean);
    function LoadFullCodeSystem(uri : String) : TFhirValueSet;
    procedure CheckConnection;
    procedure SynchroniseServer(event : TFhirHTTPClientStatusEvent);
    procedure UpdateFromServer(event : TFhirHTTPClientStatusEvent);
  public
    Constructor Create(name, url, username, password : String; doesSearch : boolean; key : integer);
    destructor Destroy; override;
    Function Link : TValueSetEditorServerCache; overload;
    procedure checkLoad(event : TFhirHTTPClientStatusEvent; null : String);
    procedure load(event : TFhirHTTPClientStatusEvent = nil);
    procedure update(event : TFhirHTTPClientStatusEvent; null : String);
    procedure save;
    function base : String;
    Property URL : string read FUrl write FUrl;
    property List : TFHIRValueSetList read FList;
    Property Name : String read FName;
    Property CodeSystems : TAdvStringObjectMatch read FCodeSystems;
    Property LastUpdated : String read FLastUpdated;

    procedure loadClosures(list : TStrings);
    procedure addClosure(s : String);
    procedure ResetClosure(name : String);
    procedure updateClosure(name : String);
    procedure AddToClosure(name : String; coding : TFhirCoding);
    function expand(url, filter : String; count : integer; allowIncomplete : boolean): TFHIRValueSet;
    property username : String read Fusername write Fusername;
    property password : String read Fpassword write Fpassword;
  end;

  TValueSetEditorContext = class (TAdvObject)
  private
    FSettings: TValueSetEditorCoreSettings;
    FWorkingServer : TValueSetEditorServerCache;
    FServers : TAdvList<TValueSetEditorServerCache>;

    FValueSet : TFhirValueSet; // working value set
    FCodeSystemContexts : TAdvStringObjectMatch;
    FVersionCount : integer;
    FOnStateChange: TNotifyEvent;
    FLastCommitSource : String;
    FDirty: boolean;
    FExpansion: TFhirValueSetExpansion;
    FOnPreview: TNotifyEvent;
    FPreview: TFhirValueSetExpansion;
    FExpansions : TAdvStringObjectMatch;
    FValidationErrors : TValidationOutcomeMarkList;
    FOnValidate: TNotifyEvent;
    FServerFilter: String;
    Procedure LoadServers;
    Procedure CompressFile(source, dest : String);
    Procedure DeCompressFile(source, dest : String);
    procedure openValueSet(vs : TFhirValueSet);
    procedure GetDefinedCodesList(context : TFhirValueSetCodeSystemConceptList; list : TStrings);
    procedure GetDefinedCodeDisplayList(context : TFhirValueSetCodeSystemConceptList; list : TStrings; code : String);
    function FetchValueSetBySystem(uri : String) : TFhirValueSet;

    procedure LoadFromFile(fn : String);
    procedure ClearFutureVersions;
    procedure ClearAllVersions;
    function makeOutcome(kind : TValidationOutcomeKind; msg : String) : TValidationOutcome;
    procedure InternalValidation;
    procedure validateDefine;
    procedure validateInclude(inc : TFhirValueSetComposeInclude);
    procedure validateDefineConcepts(ts: TStringlist; list: TFhirValueSetCodeSystemConceptList);
    procedure SetServerFilter(const Value: String);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TValueSetEditorContext; overload;
    Property Settings : TValueSetEditorCoreSettings read FSettings;
    Property OnStateChange : TNotifyEvent read FOnStateChange write FOnStateChange;
    Property OnValidate : TNotifyEvent read FOnValidate write FOnValidate;

    // general
    Function UIState : integer;
    Property Dirty : boolean read FDirty;

    // registering a server
    function CheckServer(url : String; var msg : String; var doesSearch : boolean) : boolean;
    procedure SetNominatedServer(url : String);
    Property WorkingServer : TValueSetEditorServerCache read FWorkingServer;
    Property Servers : TAdvList<TValueSetEditorServerCache> read FServers;
    procedure AddServer(name, address, username, password : String; doesSearch : boolean);
    procedure DeleteServer(name : String);
    procedure UpdateServer(server : TValueSetEditorServerCache);

    // opening a value set
    procedure NewValueset;
    procedure openFromFile(event : TFhirHTTPClientStatusEvent; fn : String);
    procedure openFromServer(event : TFhirHTTPClientStatusEvent; id : String);
    procedure openFromURL(event : TFhirHTTPClientStatusEvent; url : String);


    // editing value set
    Property ValueSet : TFhirValueSet read FValueSet;
    function EditName : String;
    procedure Commit(source : string);
    function CanUndo : boolean;
    Function CanRedo : boolean;
    Function CanSave : boolean;
    function Undo : boolean;
    Function Redo : boolean;
    procedure UndoBreak;

    procedure Save;
    procedure saveAsFile(fn : String);
    procedure SaveAsServerNew;
    procedure Close;

    // editor grid support
    function NameCodeSystem(uri : String) : String;
    procedure GetList(context : String; list : TStrings);
    procedure PrepareSystemContext(uri : String; version : String); // the editor is going to start editing in the named URI - be ready to provide code system support
    function NameValueSet(uri : String) : String;
    function GetCodeSystemValidator(uri : String) : TValueSetEditorCodeSystem;
    function TryGetDisplay(system, code : String) : String;

    // expansion
    property Expansion : TFhirValueSetExpansion read FExpansion;
    procedure Expand(event : TFhirHTTPClientStatusEvent; text : String);
    procedure GetPreview(uri : String);
    property Preview : TFhirValueSetExpansion read FPreview;
    property OnPreview : TNotifyEvent read FOnPreview write FOnPreview;

    // validation:
    function validateIdentifier(value : string) : TValidationOutcome;
    function validateSystem(value : string) : TValidationOutcome;
    function validateImport(value : string) : TValidationOutcome;
    function validateReference(value : string) : TValidationOutcome;
    function validateName(value : string) : TValidationOutcome;
    function validateDescription(value: string): TValidationOutcome;
    function checkValidation(elem : TFHIRElement; field : integer; var kind : TValidationOutcomeKind; var msg : String): boolean;

    // closures
    procedure loadClosures(list : TStrings);
    procedure AddClosure(name : String);
    procedure ResetClosure(name : String);
    procedure UpdateClosure(name : String);
    procedure AddToClosure(name : String; coding : TFhirCoding);
    function ClosureDetails(name : String): TClosureTableRecord;
  end;

function IsURL(s : String) : boolean;

implementation

{ TValueSetEditorContext }

procedure TValueSetEditorContext.AddClosure(name: String);
begin
  WorkingServer.AddClosure(name);
end;

procedure TValueSetEditorContext.AddServer(name, address, username, password: String; doesSearch : boolean);
var
  server : TValueSetEditorServerCache;
begin
  Settings.AddServer(name, address, username, password, doesSearch);
  server := TValueSetEditorServerCache.Create(name, address, username, password, doesSearch, FServers.Count - 1);
  try
    server.load(nil);
    FServers.Add(server.Link);
  finally
    server.Free;
  end;
end;

procedure TValueSetEditorContext.AddToClosure(name: String; coding: TFhirCoding);
begin
  WorkingServer.AddToClosure(name, coding);
end;

function TValueSetEditorContext.CanRedo: boolean;
begin
  result := FileExists(Settings.ValuesetItemPath+'-'+inttostr(FVersionCount+1)+'.z');
end;

function TValueSetEditorContext.CanSave: boolean;
begin
  result := (Settings.valueSetFilename <> '') or (Settings.valueSetId <> '');
end;

function TValueSetEditorContext.CanUndo: boolean;
begin
  result := FileExists(Settings.ValuesetItemPath+'-'+inttostr(FVersionCount-1)+'.z')
end;

function TValueSetEditorContext.CheckServer(url: String; var msg: String; var doesSearch : boolean): boolean;
var
  client : TFhirHTTPClient;
  conf : TFhirConformance;
  rest : TFhirConformanceRestResource;
begin
  result := false;
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    try
      conf := client.conformance(false);
      try
        if (conf.fhirVersion <> FHIR_GENERATED_VERSION) then
          raise Exception.Create('The server is the wrong version. Expected '+FHIR_GENERATED_VERSION+', found '+conf.fhirVersion);
        rest := conf.rest(frtValueset);
        if (rest = nil) {or (rest.interaction(TypeRestfulInteractionSearchType) = nil) }or (rest.interaction(TypeRestfulInteractionRead) = nil) then
          raise Exception.Create('The server does not support the required opeerations for value sets');
        doesSearch := rest.interaction(TypeRestfulInteractionSearchType) <> nil;
        result := true;
      finally
        conf.free;
      end;
    except
      on e: Exception do
        msg := e.Message;
    end;
  finally
    client.Free;
  end;
end;

function TValueSetEditorContext.checkValidation(elem: TFHIRElement; field: integer; var kind: TValidationOutcomeKind; var msg: String): boolean;
var
  mark : TValidationOutcomeMark;
begin
  mark := FValidationErrors.GetByElement(elem, field);
  result := mark <> nil;
  if result then
  begin
    kind := mark.kind;
    msg := mark.message;
  end;
end;

procedure TValueSetEditorContext.ClearAllVersions;
var
  i : integer;
begin
  for I := 1 to Settings.VersionHigh do
    if FileExists(Settings.ValuesetItemPath+'-'+inttostr(i)+'.z') then
      DeleteFile(Settings.ValuesetItemPath+'-'+inttostr(i)+'.z');
end;

procedure TValueSetEditorContext.ClearFutureVersions;
var
  i : integer;
begin
  for I := FVersionCount + 1 to Settings.VersionHigh do
    if FileExists(Settings.ValuesetItemPath+'-'+inttostr(i)+'.z') then
      DeleteFile(Settings.ValuesetItemPath+'-'+inttostr(i)+'.z');
end;

procedure TValueSetEditorContext.Close;
begin
  FValueSet.Free;
  FValueset := nil;
  ClearAllVersions;
  DeleteFile(Settings.ValuesetItemPath);
  FVersionCount := 1;
  FSettings.valueSetId := '';
  FSettings.valueSetFilename := '';
  FSettings.VersionHigh := 1;
  FSettings.FormatIsJson := false;
  FLastCommitSource := '';
  FDirty := False;

  if assigned(FOnStateChange) then
    FOnStateChange(self);
end;

function TValueSetEditorContext.ClosureDetails(name: String): TClosureTableRecord;
begin
  if WorkingServer.closures.ContainsKey(name) then
    result := WorkingServer.closures[name]
  else
    result := nil;
end;

procedure TValueSetEditorContext.commit(source : string);
var
  c : TFHIRJsonComposer;
  f : TFileStream;
begin
  ClearFutureVersions;

  // current copy
  f := TFileStream.Create(Settings.ValuesetItemPath, fmCreate);
  try
    c := TFHIRJsonComposer.Create(nil, 'en');
    try
      c.Compose(f, FValueSet, true);
    finally
      c.Free;
    end;
  finally
    f.free;
  end;

  FDirty := true;
  // now, create the redo copy
  if (source = '') or (source <> FLastCommitSource) then
    inc(FVersionCount); // else we just keep updating this version
  FLastCommitSource := source;
  Settings.VersionHigh := Max(Settings.VersionHigh, FVersionCount);

  CompressFile(Settings.ValuesetItemPath, Settings.ValuesetItemPath+'-'+inttostr(FVersionCount)+'.z');
  if Assigned(FOnStateChange) then
    FOnStateChange(self);
  InternalValidation;
  if Assigned(FOnValidate) then
    FOnValidate(self);
end;

procedure TValueSetEditorContext.CompressFile(source, dest: String);
var
  LInput, LOutput: TFileStream;
  LZip: TZCompressionStream;
begin
  LOutput := TFileStream.Create(dest, fmCreate);
  try
    LInput := TFileStream.Create(source, fmOpenRead);
    try
      LZip := TZCompressionStream.Create(clMax, LOutput);
      try
        LZip.CopyFrom(LInput, LInput.Size);
      finally
        LZip.Free;
      end;
    finally
      LInput.Free;
    end;
  finally
    LOutput.Free;
  end;
end;

function TValueSetEditorContext.Undo: boolean;
begin
  result := CanUndo;
  if result then
  begin
    DecompressFile(Settings.ValuesetItemPath+'-'+inttostr(FVersionCount-1)+'.z', Settings.ValuesetItemPath);
    dec(FVersionCount);
    LoadFromFile(Settings.ValuesetItemPath);
    if FVersionCount = 0 then
      FDirty := false;
    FLastCommitSource := '';
    if Assigned(FOnStateChange) then
      FOnStateChange(self);
    InternalValidation;
    if Assigned(FOnValidate) then
      FOnValidate(self);
  end;
end;

procedure TValueSetEditorContext.UndoBreak;
begin
  FLastCommitSource := '';
end;

procedure TValueSetEditorContext.UpdateClosure(name: String);
begin
  WorkingServer.UpdateClosure(name);
end;

procedure TValueSetEditorContext.UpdateServer(server: TValueSetEditorServerCache);
begin
  FSettings.updateServer(server.Name, server.URL, server.username, server.password);
end;

function TValueSetEditorContext.Redo: boolean;
begin
  result := CanRedo;
  if result then
  begin
    DecompressFile(Settings.ValuesetItemPath+'-'+inttostr(FVersionCount+1)+'.z', Settings.ValuesetItemPath);
    inc(FVersionCount);
    LoadFromFile(Settings.ValuesetItemPath);
    FDirty := true;
    FLastCommitSource := '';
    if Assigned(FOnStateChange) then
      FOnStateChange(self);
    InternalValidation;
    if Assigned(FOnValidate) then
      FOnValidate(self);
  end;
end;


procedure TValueSetEditorContext.ResetClosure(name: String);
begin
  WorkingServer.ResetClosure(name);
end;

procedure TValueSetEditorContext.Save;
var
  c : TFHIRComposer;
  f : TFileStream;
  client : TFhirHTTPClient;
begin
  FValueSet.date := NowLocal;
  if (Settings.valueSetFilename <> '') then
  begin
    f := TFileStream.create(Settings.valueSetFilename, fmCreate);
    try
      if Settings.FormatIsJson then
        c := TFHIRJsonComposer.Create(nil, 'en')
      else
        c := TFHIRXmlComposer.Create(nil, 'en');
      try
        c.Compose(f, FValueSet, true);
      finally
        c.free;
      end;
    finally
      f.Free;
    end;
  end
  else
  begin
    if (Settings.valueSetId = '') then
      raise Exception.Create('Cannot save to server as value set id has been lost');

    client := TFhirHTTPClient.create(nil, Settings.valueSetServer, true);
    try
      client.UseIndy := true;
      client.OnClientStatus := nil;
      FValueSet.id := Settings.valueSetId;
      client.updateResource(FValueSet);
    finally
      client.free;
    end;
  end;
  FDirty := false;
  if assigned(FOnStateChange) then
    FOnStateChange(self);
end;

procedure TValueSetEditorContext.saveAsFile(fn: String);
begin
  Settings.valueSetFilename := fn;
  Settings.FormatIsJson := fn.EndsWith('.json');
  Save;
end;

procedure TValueSetEditorContext.SaveAsServerNew;
var
  client : TFhirHTTPClient;
  vs : TFHIRValueSet;
  id : String;
begin
  FValueSet.date := NowLocal;
  client := TFhirHTTPClient.create(nil, Settings.WorkingServer, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    vs := client.createResource(FValueSet, id) as TFhirValueSet;
    try
      Settings.valueSetId := id;
      FValueSet.free;
      FValueSet := vs.Link;
    finally
      vs.Free;
    end;
  finally
    client.free;
  end;
end;

procedure TValueSetEditorServerCache.UpdateFromServer(event : TFhirHTTPClientStatusEvent);
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  list : TFHIRBundle;
  i : integer;
  vs : TFhirValueSet;
begin
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := Event;
    params := TAdvStringMatch.Create;
    try
      params.Add('_since', FLastUpdated);
      params.Add('_count', '50');
      event(self, 'Fetch Valuesets');
      list := client.historyType(frtValueset, true, params);
      try
        for i := 0 to list.entryList.Count -1 do
        begin
          event(self, 'Process Valueset '+inttostr(i+1)+' if '+inttostr(list.entryList.Count));
          SeeValueset(list.entryList[i].resource as TFhirValueSet, false, false);
        end;
//        FServer.FLastUpdated := ;
      finally
        list.Free;
      end;
      save;
    finally
      params.free;
    end;
  finally
    client.Free;
  end;
end;


procedure TValueSetEditorServerCache.SynchroniseServer(event : TFhirHTTPClientStatusEvent);
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  list : TFHIRBundle;
  i : integer;
  vs : TFhirValueSet;
begin
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := event;
    params := TAdvStringMatch.Create;
    try
      params.Add('_summary', 'true');
      params.Add('_count', 'all');
      event(self, 'Fetch Valuesets');
      list := client.search(frtValueset, true, params);
      try
        for i := 0 to list.entryList.Count - 1 do
        begin
          event(self, 'Process Valueset '+inttostr(i+1)+' if '+inttostr(list.entryList.Count));
          SeeValueset(list.entryList[i].resource as TFhirValueSet, true, false);
        end;
        FLastUpdated := list.meta.lastUpdated.AsXML;
      finally
        list.Free;
      end;
      save;
    finally
      params.free;
    end;
  finally
    client.Free;
  end;
end;

procedure TValueSetEditorServerCache.update(event: TFhirHTTPClientStatusEvent; null: String);
begin
  if not FLoaded then
    load(event);
  if FLastUpdated = '' then
    SynchroniseServer(event)
  else
    UpdateFromServer(event);
end;

procedure TValueSetEditorServerCache.updateClosure(name: String);
var
  client : TFhirHTTPClient;
  pin : TFhirParameters;
  pout : TFhirResource;
  ct : TClosureTableRecord;
begin
  ct := closures[name];
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    pin := TFhirParameters.Create;
    try
      pin.AddParameter('name', ct.id);
      pin.AddParameter('version', ct.version);
      pout := client.operation(frtConceptMap, 'closure', pin);
      try
        if pout is TFHIRConceptMap then
          ct.process(pout as TFHIRConceptMap)
        else
          raise Exception.Create('Unexpected response from server');
      finally
        pout.Free;
      end;
    finally
      pin.Free;
    end;
  finally
    client.Free;
  end;
  ct.save;
end;

function TValueSetEditorContext.TryGetDisplay(system, code: String): String;
var
  cs : TValueSetEditorCodeSystem;
begin
  cs := GetCodeSystemValidator(system);
  if (cs = nil) or not cs.getDisplay(code, result) then
    result := '??';
end;

procedure TValueSetEditorContext.SetNominatedServer(url : String);
var
  i, t : integer;
begin
  t := -1;
  for i := 0 to FServers.Count - 1 do
    if FServers[i].FUrl = url then
      t := i;
  if (t = -1) then
    raise Exception.Create('URL not registered as a server: '+url);
  FWorkingServer.Free;
  FWorkingServer := Servers[t].Link;
  Settings.WorkingServer := FWorkingServer.FUrl;
end;

procedure TValueSetEditorContext.SetServerFilter(const Value: String);
begin
  FServerFilter := Value;
end;

constructor TValueSetEditorContext.Create;
var
  p : TFHIRJsonParser;
  f : TFileStream;
  list : TStringList;
  s : String;
begin
  inherited;
  FCodeSystemContexts := TAdvStringObjectMatch.create;
  FCodeSystemContexts.Forced := true;
  FCodeSystemContexts.PreventDuplicates;

  FValidationErrors := TValidationOutcomeMarkList.create;
  FSettings := TValueSetEditorCoreSettings.Create;
  if Settings.ServerCount = 0 then
    if FileExists('C:\work\fhirserver\Exec\fhir.ini') then
      Settings.AddServer('Local Development Server', 'http://local.healthintersections.com.au:960/open', '', '', true)
    else
      Settings.AddServer('Health Intersections General Server', 'http://fhir-dev.healthintersections.com.au/open', '', '', true);

  FServers := TAdvList<TValueSetEditorServerCache>.create;
  loadServers;
  if FSettings.WorkingServer <> '' then
    SetNominatedServer(FSettings.WorkingServer)
  else
    SetNominatedServer(Servers[0].URL);


  FExpansions := TAdvStringObjectMatch.create;
  ClearAllVersions;
  FVersionCount := 0;
  if FileExists(FSettings.ValuesetItemPath) then
  begin
    try
      LoadFromFile(FSettings.ValuesetItemPath);
      Commit('load');
    except
      showmessage('unable to load previous valueset');
    end;
  end
  else
    FValueSet := nil;
  FDirty := False;
end;

procedure TValueSetEditorContext.DeCompressFile(source, dest: String);
var
  LInput, LOutput: TFileStream;
  LUnZip: TZDecompressionStream;
begin
  LOutput := TFileStream.Create(dest, fmCreate);
  try
    LInput := TFileStream.Create(source, fmOpenRead);
    try
      LUnZip := TZDecompressionStream.Create(LInput);
      try
      LOutput.CopyFrom(LUnZip, 0);
      finally
        LUnZip.Free;
      end;
    finally
      LInput.Free;
    end;
  finally
    LOutput.Free;
  end;
end;

procedure TValueSetEditorContext.DeleteServer(name: String);
var
  i : integer;
  done : boolean;
begin
  done := false;
  for i := FServers.Count -1 downto 0 do
    if FServers[i].Name = name then
    begin
      FServers.Delete(i);
      done := true;
    end;
  if not done then
    raise Exception.Create('Server not found');
  Settings.DeleteServer(name);
end;

destructor TValueSetEditorContext.Destroy;
begin
  FWorkingServer.Free;
  FServers.Free;
  FValidationErrors.Free;
  FExpansions.Free;
  FValueSet.Free;
  FCodeSystemContexts.Free;
  FSettings.Free;
  FExpansion.Free;
  FPreview.Free;
  inherited;
end;

procedure TValueSetEditorContext.LoadFromFile(fn : String);
var
  p : TFHIRJsonParser;
  f : TFileStream;
begin
  f := TFileStream.Create(FSettings.ValuesetItemPath, fmOpenRead + fmShareDenyWrite);
  try
    p := TFHIRJsonParser.Create(nil, 'en');
    try
      p.source := f;
      p.Parse;
      FValueSet.Free;
      FValueSet := p.resource.Link as TFhirValueSet;
    finally
      p.free
    end;
  finally
    f.Free;
  end;
end;

procedure TValueSetEditorContext.LoadServers;
var
  i : integer;
  name, address, username, password : String;
  doesSearch : boolean;
  server : TValueSetEditorServerCache;
begin
  for i := 0 to Settings.ServerCount - 1 do
  begin
    settings.getServer(i, name, address, username, password, doesSearch);
    server := TValueSetEditorServerCache.Create(name, address, username, password, doesSearch, i);
    try
      server.load(nil);
      FServers.Add(server.Link);
    finally
      server.Free;
    end;
  end;
end;

function TValueSetEditorContext.EditName: String;
begin
  if Settings.valueSetFilename <> '' then
    result := Settings.valueSetFilename
  else if Settings.valueSetId <> '' then
    result := Settings.valueSetId +' on '+Settings.valueSetServer
  else
    result := 'New Value Set';
end;

procedure TValueSetEditorContext.Expand(event : TFhirHTTPClientStatusEvent; text : String);
var
  client : TFhirHTTPClient;
  pIn, pOut : TFhirParameters;
  rOut : TFHIRResource;
  feed : TFHIRBundle;
begin
  client := TFhirHTTPClient.create(nil, FWorkingServer.FUrl, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    pIn := TFhirParameters.Create;
    try
      if text <> '' then
        pIn.AddParameter('filter', text);
      pIn.AddParameter('valueSet', ValueSet.Link);
      rOut := client.operation(frtValueset, 'expand', pIn);
      try
        FExpansion.Free;
        FExpansion := nil;
        if rOut is TFhirValueSet then
          FExpansion := (rOut as TFhirValueSet).expansion.Link
        else if rOut is TFhirParameters then
        begin
          pOut := TFhirParameters(rOut);
          if pOut.hasParameter('return') then
            FExpansion := (pOut['return'] as TFHIRValueSet).expansion.Link
          else
            raise Exception.Create('Unable to process result from expansion server');
        end
        else
          raise Exception.Create('Unable to process result from expansion server');
      finally
        rOut.Free;
      end;
    finally
      pIn.free;
    end;
  finally
    client.Free;
  end;
end;

function TValueSetEditorContext.FetchValueSetBySystem(uri: String): TFhirValueSet;
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  feed : TFHIRBundle;
begin
  result := nil;
  client := TFhirHTTPClient.create(nil, FWorkingServer.url, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    params := TAdvStringMatch.Create;
    try
      params.Add('system', uri);
      feed := client.search(frtValueset, false, params);
      try
        if feed.entryList.Count > 0 then
        begin
          FWorkingServer.SeeValueset(feed.entryList[0].resource as TFhirValueSet, false, false);
          FWorkingServer.save;
          result := feed.entryList[0].resource.link as TFhirValueSet;
        end;
      finally
        feed.Free;
      end;
    finally
      params.free;
    end;
  finally
    client.Free;
  end;
end;

function TValueSetEditorContext.GetCodeSystemValidator(uri: String): TValueSetEditorCodeSystem;
var
  vs : TFhirValueSet;
  obj : TAdvObject;
  i : integer;
begin
  if FCodeSystemContexts.ExistsByKey(uri) then
  begin
    obj := FCodeSystemContexts.GetValueByKey(uri);
    if obj is TValueSetEditorCodeSystem then
      result := TValueSetEditorCodeSystem(obj.Link)
    else
      result := TValueSetEditorCodeSystemValueSet.Create(TFHIRValueSet(obj));
  end
  else if (FWorkingServer <> Nil) and (FWorkingServer.codesystems.ExistsByKey(uri)) then
  begin
    // this means that the server has code system. try and load the local copy
    vs := FWorkingServer.LoadFullCodeSystem(uri);
    try
      if vs = nil then
        vs := FetchValueSetBySystem(uri);
      if vs = nil then
        result := nil
      else
      begin
        FCodeSystemContexts.Add(uri, vs.link);
        result := TValueSetEditorCodeSystemValueSet.Create(TFHIRValueSet(vs));
      end;
    finally
      vs.Free;
    end;
  end
  else if (FWorkingServer <> nil) and FWorkingServer.specialCodeSystems.ExistsByKey(uri) then
    result := FWorkingServer.specialCodeSystems.GetValueByKey(uri).Link as TServerCodeSystem
  else
    result := nil; // Not done yet;
end;

procedure TValueSetEditorContext.GetDefinedCodeDisplayList(context: TFhirValueSetCodeSystemConceptList; list: TStrings; code: String);
var
  i : integer;
begin
  for i := 0 to context.count - 1 do
  begin
    if context[i].code = code then
      list.Add(context[i].display)
    else
      GetDefinedCodeDisplayList(context[i].conceptList, list, code);
  end;
end;

procedure TValueSetEditorContext.GetDefinedCodesList(context: TFhirValueSetCodeSystemConceptList; list: TStrings);
var
  i : integer;
begin
  for i := 0 to context.count - 1 do
  begin
    if not context[i].abstract then
      list.Add(context[i].code);
    GetDefinedCodesList(context[i].conceptList, list);
  end;
end;

procedure TValueSetEditorContext.GetList(context: String; list: TStrings);
var
  obj : TAdvObject;
  code : String;
  i: Integer;
  vs : TFhirValueSet;
begin
  if context = '' then
    exit;

  if Context.Contains('#') then
    StringSplitRight(Context, '#', Context, code);
  list.BeginUpdate;
  try

    if context = CS_LIST then
      list.AddStrings(FWorkingServer.sortedCodesystems)
    else if context = VS_LIST then
      list.AddStrings(FWorkingServer.sortedValueSets)
    else if FCodeSystemContexts.ExistsByKey(context) then
    begin
      obj := FCodeSystemContexts.Matches[context];
      if obj is TFhirValueSet then
        if code = '' then
          GetDefinedCodesList(TFhirValueSet(obj).codeSystem.conceptList, list)
        else
        begin
          GetDefinedCodeDisplayList(TFhirValueSet(obj).codeSystem.conceptList, list, code);
          if list.IndexOf('') = -1 then
            list.Insert(0, '');
        end;
    end
    else if context = ExpressionProperty then
    begin
      if (code = 'http://snomed.info/sct') then
      begin
        list.Add('concept');
        list.Add('expression');
      end
      else if (code = 'http://loinc.org') then
      begin
        list.Add('COMPONENT');
        list.Add('PROPERTY');
        list.Add('TIME_ASPCT');
        list.Add('SYSTEM');
        list.Add('SCALE_TYP');
        list.Add('METHOD_TYP');
        list.Add('CLASS');
        list.Add('Document.Kind');
        list.Add('Document.TypeOfService');
        list.Add('Document.Setting');
        list.Add('Document.Role');
        list.Add('Document.SubjectMatterDomain');
        list.Add('LOINC_NUM');
        list.Add('SOURCE');
        list.Add('DATE_LAST_CHANGED');
        list.Add('CHNG_TYPE');
        list.Add('COMMENTS');
        list.Add('STATUS');
        list.Add('CONSUMER_NAME');
        list.Add('MOLAR_MASS');
        list.Add('CLASSTYP');
        list.Add('FORMUL');
        list.Add('SPECIES');
        list.Add('EXMPL_ANSWERS');
        list.Add('ACSSYM');
        list.Add('BASE_NAME');
        list.Add('NAACCR_ID');
        list.Add('CODE_TABLE');
        list.Add('SURVEY_QUEST_TXT');
        list.Add('SURVEY_QUEST_SRC');
        list.Add('UNITSREQUIRED');
        list.Add('SUBMITTED_UNITS');
        list.Add('RELATEDNAMES2');
        list.Add('SHORTNAME');
        list.Add('ORDER_OBS');
        list.Add('CDISC_COMMON_TESTS');
        list.Add('HL7_FIELD_SUBFIELD_ID');
        list.Add('EXTERNAL_COPYRIGHT_NOTICE');
        list.Add('EXAMPLE_UNITS');
        list.Add('LONG_COMMON_NAME');
        list.Add('HL7_V2_DATATYPE');
        list.Add('HL7_V3_DATATYPE');
        list.Add('CURATED_RANGE_AND_UNITS');
        list.Add('DOCUMENT_SECTION');
        list.Add('EXAMPLE_UCUM_UNIT');
        list.Add('EXAMPLE_SI_UCUM_UNITS');
        list.Add('STATUS_REASON');
        list.Add('STATUS_TEXT');
        list.Add('CHANGE_REASON_PUBLIC');
        list.Add('COMMON_TEST_RANK');
        list.Add('COMMON_ORDER_RANK');
        list.Add('COMMON_SI_TEST_RANK');
        list.Add('HL7_ATTACHMENT_STRUCTURE');
      end
      else
        list.Add('concept');
    end
    else
      // ??
  finally
    list.EndUpdate;
  end;
end;


procedure TValueSetEditorContext.GetPreview(uri: String);
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  feed : TFHIRBundle;
begin
  FPreview.Free;
  FPreview := nil;
  if (uri = '') then
  begin
    FOnPreview(self);
  end
  else if (FExpansions.existsBykey(uri)) then
    FPreview := FExpansions.Matches[uri].Link as TFhirValueSetExpansion
  else
  begin
    // todo: make this a thread that waits
    client := TFhirHTTPClient.create(nil, FWorkingServer.FUrl, true);
    try
      client.UseIndy := true;
      client.OnClientStatus := nil;
      params := TAdvStringMatch.Create;
      try
        params.Add('_query', 'expand');
        params.Add('identifier', uri);
        feed := client.search(frtValueset, false, params);
        try
          if feed.entryList.Count > 0 then
          begin
            FPreview.Free;
            FPreview := nil;
            FPreview := TFHIRValueset(feed.entryList[0].resource).expansion.link;
            FExpansions.add(uri, FPreview.Link);
            FOnPreview(self);
          end;
        finally
          feed.Free;
        end;
      finally
        params.free;
      end;
    finally
      client.Free;
    end;
  end;
end;

procedure TValueSetEditorContext.validateDefineConcepts(ts : TStringlist; list : TFhirValueSetCodeSystemConceptList);
var
  i : integer;
  code : TFhirValueSetCodeSystemConcept;
begin
  for I := 0 to list.Count - 1 do
  begin
    code := list[i];
    if code.abstract and (code.code = '') then
      FValidationErrors.AddElement(code, 0, voError, 'Missing Code - required if not an abstract code')
    else if ts.IndexOf(code.code) > -1 then
      FValidationErrors.AddElement(code, 0, voError, 'Duplicate Code '+code.code)
    else
      ts.Add(code.code);
    if code.definition = '' then
      FValidationErrors.AddElement(code, 3, voHint, 'Codes should have definitions, or their use is always fragile');
    if code.abstract and code.conceptList.IsEmpty then
      FValidationErrors.AddElement(code, 1, voWarning, 'This abstract element has no children');
    validateDefineConcepts(ts, code.conceptList);
  end;
end;

procedure TValueSetEditorContext.validateDefine;
var
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    validateDefineConcepts(ts, FValueSet.codeSystem.conceptList);
  finally
    ts.Free;
  end;
end;

procedure TValueSetEditorContext.InternalValidation;
var
  i : integer;
begin
  FValidationErrors.Clear;
  if (FValueSet <> nil) then
  begin
    if FValueSet.codeSystem <> nil then
      validateDefine;
    if FValueSet.compose <> nil then
    begin
      for i := 0 to FValueSet.compose.includeList.Count - 1 do
        validateInclude(FValueSet.compose.includeList[i]);
      for i := 0 to FValueSet.compose.excludeList.Count - 1 do
        validateInclude(FValueSet.compose.excludeList[i]);

    end;
  end;
end;

function TValueSetEditorContext.Link: TValueSetEditorContext;
begin
  result := TValueSetEditorContext(inherited Link);
end;

procedure TValueSetEditorContext.loadClosures(list: TStrings);
begin
  WorkingServer.loadClosures(list);
end;

function TValueSetEditorContext.NameCodeSystem(uri: String): String;
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  list : TFHIRBundle;
begin
  if uri = '' then
    result := '?Unnamed'
  else if uri = 'http://snomed.info/sct' then
    result := 'SNOMED CT'
  else if uri = 'http://loinc.org' then
    result := 'LOINC'
  else if uri = 'http://unitsofmeasure.org' then
    result := 'UCUM'
  else if uri = 'http://www.radlex.org/' then
    result := 'RadLex'
  else if uri = 'http://hl7.org/fhir/sid/icd-10' then
    result := 'ICD-10'
  else if uri = 'http://hl7.org/fhir/sid/icd-9  ' then
    result := 'ICD-9 USA'
  else if uri = 'http://www.whocc.no/atc' then
    result := 'ATC codes'
  else if uri = 'urn:std:iso:11073:10101' then
    result := 'ISO 11073'
  else if uri = 'http://nema.org/dicom/dicm' then
    result := 'DICOM codes'
  else if FWorkingServer.codesystems.ExistsByKey(uri) then
    result := TFhirValueSet(FWorkingServer.codesystems.GetValueByKey(uri)).name
  else if not isURL(uri) then
    result := uri
  else
  begin
    try
      client := TFhirHTTPClient.create(nil, FWorkingServer.URL, true);
      try
        client.UseIndy := true;
        client.OnClientStatus := nil;
        params := TAdvStringMatch.Create;
        try
          params.Add('system', uri);
          list := client.search(frtValueset, false, params);
          try
            if list.entryList.Count > 0 then
            begin
              result := (list.entryList[0].resource as TFHIRValueSet).name;
              FWorkingServer.SeeValueset(list.entryList[0].resource as TFHIRValueSet, false, false);
              FWorkingServer.save;
            end
            else
              result := uri;
          finally
            list.Free;
          end;
        finally
          params.free;
        end;
      finally
        client.Free;
      end;
    except
      result := uri;
    end;
  end
end;


function TValueSetEditorContext.NameValueSet(uri: String): String;
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  list : TFHIRBundle;
begin
  if (uri = '') then
    result := '??'
  else if FWorkingServer.valuesets.ExistsByKey(uri) then
    result := TFHIRValueSet(FWorkingServer.valuesets.Matches[uri]).name
  else if not isUrl(uri) then
    result := uri
  else
  begin
    try
      client := TFhirHTTPClient.create(nil, FWorkingServer.URL, true);
      try
        client.UseIndy := true;
        client.OnClientStatus := nil;
        params := TAdvStringMatch.Create;
        try
          params.Add('identifier', uri);
          list := client.search(frtValueset, false, params);
          try
            if list.entryList.Count > 0 then
            begin
              result := (list.entryList[0].resource as TFHIRValueSet).name;
              FWorkingServer.SeeValueset(list.entryList[0].resource as TFhirValueSet, false, false);
              FWorkingServer.save;
            end
            else
              result := uri;
          finally
            list.Free;
          end;
        finally
          params.free;
        end;
      finally
        client.Free;
      end;
    except
      result := uri;
    end;

  end;
end;

procedure TValueSetEditorContext.NewValueset;
var
  vs : TFhirValueSet;
begin
  vs := TFhirValueSet.Create;
  try
    vs.experimental := true;
    openValueSet(vs);
  finally
    vs.Free;
  end;
  Settings.valueSetFilename := '';
  Settings.valueSetId := '';
end;


procedure TValueSetEditorContext.openFromFile(event : TFhirHTTPClientStatusEvent; fn: String);
var
  p : TFHIRParser;
  s : AnsiString;
  f : TFileStream;
begin
  Settings.mru('file:'+fn);
  Settings.valueSetFilename := fn;
  f := TFileStream.create(fn, fmOpenRead + fmShareDenyWrite);
  try
    SetLength(s, f.Size);
    f.Read(s[1], f.Size);
    f.Position := 0;
    if pos('<', s) = 0 then
      p := TFHIRJsonParser.Create(nil, 'en')
    else if pos('{', s) = 0 then
      p := TFHIRXmlParser.Create(nil, 'en')
    else if pos('<', s) < pos('{', s) then
      p := TFHIRXmlParser.Create(nil, 'en')
    else
      p := TFHIRJsonParser.Create(nil, 'en');
    try
      Settings.FormatIsJson := p is TFHIRJsonParser;
      p.source := f;
      p.Parse;
      openValueSet(p.resource as TFhirValueSet);
    finally
      p.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TValueSetEditorContext.openFromServer(event : TFhirHTTPClientStatusEvent; id: String);
var
  client : TFhirHTTPClient;
  vs : TFhirValueSet;
begin
  Settings.mru('id:'+id+':'+FWorkingServer.URL);
  Settings.valueSetId := id;
  client := TFhirHTTPClient.create(nil, FWorkingServer.URL, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    vs := client.readResource(frtValueSet, Settings.valueSetId) as TFhirValueSet;
    try
      openValueSet(vs);
    finally
      vs.Free;
    end;
  finally
    client.free;
  end;
end;

procedure TValueSetEditorContext.openFromURL(event : TFhirHTTPClientStatusEvent; url: String);
var
  web : TIdHTTP;
  vs : TFhirValueSet;
  p : TFHIRParser;
  mem : TMemoryStream;
begin
  Settings.mru('url:'+url);
  web := TIdHTTP.Create(nil);
  try
    web.HandleRedirects := true;
    mem := TMemoryStream.Create;
    try
      web.Get(url, mem);
//      mem.Position := 0;
//      mem.SaveToFile('c:\temp\test.web');
      mem.Position := 0;
      p := MakeParser(nil, 'en', ffUnspecified, mem, xppAllow);
      try
        vs := p.resource as TFhirValueSet;
        Settings.valueSetServer := url;
        raise Exception.Create('not done yet');
        openValueSet(vs);
      finally
        p.Free;
      end;
    finally
      mem.Free;
    end;
  Finally
    web.free;
  End;
end;

procedure TValueSetEditorContext.openValueSet(vs: TFhirValueSet);
begin
  FExpansion.Free;
  FExpansion := nil;
  FValueSet.Free;
  FValueSet := vs.Link;
  FVersionCount := 0;
  ClearAllVersions;
  Commit('open');

  FDirty := False;
  if Assigned(FOnStateChange) then
    FOnStateChange(self);
end;

procedure TValueSetEditorContext.PrepareSystemContext(uri: String; version : String);
begin
  if uri = '' then
    exit;
  GetCodeSystemValidator(uri);
end;

function TValueSetEditorContext.UIState: integer;
begin
  if FValueSet <> nil then
    result := UIState_Edit
//  else if FValueSetList <> nil then
//    result := UIState_Choose
  else
    result := UIState_Welcome;
end;

function TValueSetEditorContext.makeOutcome(kind: TValidationOutcomeKind; msg: String): TValidationOutcome;
begin
  result.kind := kind;
  result.msg := msg;
end;


function TValueSetEditorContext.validateIdentifier(value: string): TValidationOutcome;
begin
  if value = '' then
    result := makeOutcome(voMissing, 'A URL is required')
  else if not isURL(value) then
    result := makeOutcome(voError, 'A URL is required (not a valid url)')
  else
    result := makeOutcome(voOk, '');
end;

function TValueSetEditorContext.validateImport(value: string): TValidationOutcome;
begin
  if value = '' then
    result := makeOutcome(voMissing, 'A URL is required')
  else if not isURL(value) then
    result := makeOutcome(voError, 'A URL is required (not a valid url)')
  else if not FWorkingServer.valuesets.ExistsByKey(value) then
    result := makeOutcome(voWarning, 'No value set known by this URI')
  else
    result := makeOutcome(voOk, '');
end;

procedure TValueSetEditorContext.validateInclude(inc: TFhirValueSetComposeInclude);
var
  i : integer;
  ts : TStringList;
  cs : TValueSetEditorCodeSystem;
  c : TFhirCode;
  status : TValueSetEditorCodeSystemCodeStatus;
  filter : TFhirValueSetComposeIncludeFilter;
  msg : String;
begin
  cs := GetCodeSystemValidator(inc.system);
  try
    ts := TStringList.Create;
    try
      for i := 0 to inc.conceptList.Count - 1 do
      begin
        status := cscsUnknown;
        c := inc.conceptList[i].codeElement;
        if (c.value = '') then
          FValidationErrors.AddElement(c, 0, voMissing, 'Code is required')
        else if (ts.IndexOf(c.value) > -1) then
          FValidationErrors.AddElement(c, 0, voError, 'Duplicate Code "'+c.value+'"')
        else
        begin
          if cs = nil then
            status := cscsInvalidSystem
          else
            status := cs.getCodeStatus(c.value, msg);
          case status  of
            cscsOK: ; // nothing to do
            cscsInvalidSystem: FValidationErrors.AddElement(c, 0, voWarning, 'Code system is not known');
            cscsUnknown: FValidationErrors.AddElement(c, 0, voError, 'Code "'+c.value+'" is not valid in the code system ("'+msg+'")');
            cscsPending: FValidationErrors.AddElement(c, 0, voHint, 'Code "'+c.value+'" stll being validated');
          end;
        end;
        if (status = cscsOK) and (c.hasExtension('http://hl7.org/fhir/Profile/tools-extensions#display')) then
          if cs.isWrongDisplay(c.Value, c.getExtensionString('http://hl7.org/fhir/Profile/tools-extensions#display')) then
            FValidationErrors.AddElement(c, 1, voWarning, 'Display '+c.getExtensionString('http://hl7.org/fhir/Profile/tools-extensions#display')
              +' is not valid in the code system for Code "'+c.value+'"');
      end;
      if cs <> nil then
      begin
        for i := 0 to inc.filterList.Count - 1 do
        begin
          filter := inc.filterList[i];
          if not cs.filterPropertyOK(filter.property_, filter.op, filter.value) then
            FValidationErrors.AddElement(c, 0, voWarning, 'Property '''+filter.property_+''' not known for code system '+inc.system)
          else if not cs.filterOperationOK(filter.property_, filter.op, filter.value) then
            FValidationErrors.AddElement(c, 0, voWarning, 'Operation '''+filter.opElement.value+''' not known for code system '+inc.system)
          else if not cs.filterValueOK(filter.property_, filter.op, filter.value) then
            FValidationErrors.AddElement(c, 0, voWarning, 'Property '''+filter.value+''' not known for code system '+inc.system);
        end;
      end;
    finally
      ts.Free;
    end;
    // ok, that was the codes. Now the filters....
  finally
    cs.free;
  end;
end;

function TValueSetEditorContext.validateName(value: string): TValidationOutcome;
begin
  if value = '' then
    result := makeOutcome(voMissing, 'A Name is required')
  else
    result := makeOutcome(voOk, '');
end;

function TValueSetEditorContext.validateDescription(value: string): TValidationOutcome;
begin
  if value = '' then
    result := makeOutcome(voMissing, 'Some description is required')
  else
    result := makeOutcome(voOk, '');
end;

function TValueSetEditorContext.validateReference(value: string): TValidationOutcome;
begin
  if value = '' then
    result := makeOutcome(voMissing, 'A URL is required')
  else if not isURL(value) then
    result := makeOutcome(voError, 'A URL is required (not a valid url)')
  else if not FWorkingServer.codesystems.ExistsByKey(value) and not FWorkingServer.specialCodeSystems.ExistsByKey(value) then
    result := makeOutcome(voWarning, 'No code system known by this URI')
  else
    result := makeOutcome(voOk, '');
end;

function TValueSetEditorContext.validateSystem(value: string): TValidationOutcome;
begin
  if (value = '') and (FValueSet <> nil) and (FValueSet.codeSystem <> nil) and not FValueSet.codeSystem.conceptList.IsEmpty then
    result := makeOutcome(voMissing, 'A URL is required')
  else if not isURL(value) then
    result := makeOutcome(voError, 'A URL is required (not a valid url)')
  else if (value <> '') and SameText(value, FValueSet.url) then
    result := makeOutcome(voError, 'Cannot be the same as the value set identifier')
  else
    result := makeOutcome(voOk, '');
end;


{ TValueSetEditorCoreSettings }

procedure TValueSetEditorCoreSettings.AddServer(name, address, username, password: String; doesSearch : boolean);
begin
  ini.WriteString('servers', name, address);
  ini.WriteBool('servers-search', name, doesSearch);
  if username <> '' then
    ini.WriteString('servers-sec', name, EncodeMIME(username)+':'+EncodeMIME(password));
end;

function TValueSetEditorCoreSettings.columnWidth(tree, name : string; default: integer) : integer;
begin
  result := ini.ReadInteger(tree, name, default);
end;

constructor TValueSetEditorCoreSettings.Create;
var
  i : integer;
begin
  inherited;
  if not FileExists(Path([ShellLocalAppDataFolder, 'Health Intersections'])) then
    CreateDir(Path([ShellLocalAppDataFolder, 'Health Intersections']));
  if not FileExists(Path([ShellLocalAppDataFolder, 'Health Intersections', 'ValueSetEditor'])) then
    CreateDir(Path([ShellLocalAppDataFolder, 'Health Intersections', 'ValueSetEditor']));
  ini := TIniFile.create(Path([ShellLocalAppDataFolder, 'Health Intersections', 'ValueSetEditor', 'valueseteditor.ini']));
  FMRUList := TStringList.Create;
  for i := 0 to ini.ReadInteger('mru', 'count', 0) - 1 do
    FMRUList.add(ini.ReadString('mru', 'item'+inttostr(i), ''));
end;

procedure TValueSetEditorCoreSettings.DeleteServer(name: String);
begin
  ini.DeleteKey('servers', name);
end;

destructor TValueSetEditorCoreSettings.Destroy;
begin
  FMRUList.Free;
  ini.free;
  inherited;
end;

function TValueSetEditorCoreSettings.GetDocoVisible: boolean;
begin
  result := ini.ReadBool('window', 'doco', true);
end;

function TValueSetEditorCoreSettings.GetFilter: String;
begin
  result := ini.ReadString('state', 'filter', '');
end;

function TValueSetEditorCoreSettings.GetFormatIsJson: boolean;
begin
  result := ini.ReadBool('state', 'json', false);
end;

function TValueSetEditorCoreSettings.GetHasViewedWelcomeScreen: boolean;
begin
  result := ini.ReadBool('window', 'HasViewedWelcomeScreen', false);
end;

procedure TValueSetEditorCoreSettings.getServer(index : integer; var name, address, username, password : String; var doesSearch : boolean);
var
  list : TStringList;
  s, l, r : String;
begin
  list := TStringList.create;
  try
    ini.ReadSection('servers', list);
    name := list[index];
    address := ini.ReadString('servers', name, '');
    doesSearch := ini.ReadBool('servers-search', name, false);
    s := ini.ReadString('servers-sec', name, '');
    if (s <> '') then
    begin
      StringSplit(s, [':'], l, r);
      username := DecodeMIME(l);
      password := DecodeMIME(r);
    end;
  finally
    list.free;
  end;
end;

function TValueSetEditorCoreSettings.GetServerFilter: String;
begin
  result := ini.ReadString('ui', 'server-filter', '');
end;

function TValueSetEditorCoreSettings.GetServerURL: String;
begin
  result := ini.ReadString('state', 'server', '');
end;

function TValueSetEditorCoreSettings.GetvalueSetFilename: String;
begin
  result := ini.ReadString('state', 'valueset-filename', '');
end;

function TValueSetEditorCoreSettings.GetValueSetId: String;
begin
  result := ini.ReadString('state', 'valueset-id', '');
end;

function TValueSetEditorCoreSettings.GetValueSetURL: String;
begin
  result := ini.ReadString('state', 'valueset-url', '');
end;

function TValueSetEditorCoreSettings.GetVersionHigh: integer;
begin
  result := ini.ReadInteger('state', 'version-high', 1);
end;

function TValueSetEditorCoreSettings.GetWindowHeight: Integer;
begin
  result := ini.ReadInteger('window', 'height', 0);
end;

function TValueSetEditorCoreSettings.GetWindowLeft: Integer;
begin
  result := ini.ReadInteger('window', 'left', 0);
end;

function TValueSetEditorCoreSettings.GetWindowState: Integer;
begin
  result := ini.ReadInteger('window', 'state', 0);
end;

function TValueSetEditorCoreSettings.GetWindowTop: Integer;
begin
  result := ini.ReadInteger('window', 'top', 0);
end;

function TValueSetEditorCoreSettings.GetWindowWidth: Integer;
begin
  result := ini.ReadInteger('window', 'width', 0);
end;

function TValueSetEditorCoreSettings.hasWindowState: Boolean;
begin
  result := WindowHeight > 0;
end;

procedure TValueSetEditorCoreSettings.mru(src: String);
var
  i : integer;
begin
  i := FMRUList.IndexOf(src);
  if (i > -1) then
    FMRUList.Delete(i);
  FMRUList.Insert(0, src);
  ini.WriteInteger('mru', 'count', FMRUList.Count);
  for i := 0 to FMRUList.Count - 1 do
    ini.WriteString('mru', 'item'+inttostr(i), FMRUList[i]);
end;

function TValueSetEditorCoreSettings.ServerCount: integer;
var
  list : TStringList;
begin
  list := TStringList.Create;
  try
    ini.ReadSection('servers', list);
    result := list.Count;
  finally
    list.Free;
  end;
end;

procedure TValueSetEditorCoreSettings.setColumnWidth(tree, name: string; value: integer);
begin
  ini.WriteInteger(tree, name, value);
end;

procedure TValueSetEditorCoreSettings.SetDocoVisible(const Value: boolean);
begin
  ini.WriteBool('window', 'doco', value);
end;

procedure TValueSetEditorCoreSettings.SetFilter(const Value: String);
begin
  ini.WriteString('state', 'filter', value);
end;

procedure TValueSetEditorCoreSettings.SetFormatIsJson(const Value: boolean);
begin
  ini.WriteBool('state', 'json', Value);
end;

procedure TValueSetEditorCoreSettings.SetHasViewedWelcomeScreen(const Value: boolean);
begin
  ini.WriteBool('window', 'HasViewedWelcomeScreen', value);
end;

procedure TValueSetEditorCoreSettings.SetServerFilter(const Value: String);
begin
  ini.WriteString('ui', 'server-filter', value);
end;

procedure TValueSetEditorCoreSettings.SetServerURL(const Value: String);
begin
  ini.WriteString('state', 'server', value);
end;

procedure TValueSetEditorCoreSettings.SetvalueSetFilename(const Value: String);
begin
  ini.WriteString('state', 'valueset-filename', value);
end;

procedure TValueSetEditorCoreSettings.SetValueSetId(const Value: String);
begin
  ini.WriteString('state', 'valueset-id', value);
end;

procedure TValueSetEditorCoreSettings.SetValueSetURL(const Value: String);
begin
  ini.WriteString('state', 'valueset-url', value);
end;

procedure TValueSetEditorCoreSettings.SetVersionHigh(const Value: integer);
begin
  ini.WriteInteger('state', 'version-high', value);
end;

procedure TValueSetEditorCoreSettings.SetWindowHeight(const Value: Integer);
begin
  ini.WriteInteger('window', 'height', value);
end;

procedure TValueSetEditorCoreSettings.SetWindowLeft(const Value: Integer);
begin
  ini.WriteInteger('window', 'left', value);
end;

procedure TValueSetEditorCoreSettings.SetWindowState(const Value: Integer);
begin
  ini.WriteInteger('window', 'state', value);
end;

procedure TValueSetEditorCoreSettings.SetWindowTop(const Value: Integer);
begin
  ini.WriteInteger('window', 'top', value);
end;

procedure TValueSetEditorCoreSettings.SetWindowWidth(const Value: Integer);
begin
  ini.WriteInteger('window', 'width', value);
end;

procedure TValueSetEditorCoreSettings.UpdateServer(name, address, username, password: String);
begin
  ini.WriteString('servers', name, address);
  if username <> '' then
    ini.WriteString('servers-sec', name, EncodeMIME(username)+':'+EncodeMIME(password));
end;

function TValueSetEditorCoreSettings.ValuesetItemPath: string;
begin
  result := IncludeTrailingPathDelimiter(SystemTemp)+'vs.json';
end;

function TValueSetEditorCoreSettings.ValuesetListPath: string;
begin
  result := IncludeTrailingPathDelimiter(SystemTemp)+'vslist.json';
end;

function TValueSetEditorCoreSettings.ValueSetNew: boolean;
begin
  result := (valueSetFilename = '') and (valueSetId = '');
end;

function IsURL(s : String) : boolean;
var
  r : TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := 'https?://([-\w\.]+)+(:\d+)?(/([\w/_\.]*(\?\S+)?)?)?'; // http://www.regexguru.com/2008/11/detecting-urls-in-a-block-of-text/
    result := r.Exec(s);
  finally
    r.free;
  end;
end;


{ TValidationOutcomeMark }

destructor TValidationOutcomeMark.destroy;
begin
  FObj.Free;
  inherited;
end;

procedure TValidationOutcomeMark.SetObj(const Value: TFHIRElement);
begin
  FObj.Free;
  FObj := Value;
end;

{ TValidationOutcomeMarkList }

constructor TValidationOutcomeMarkList.Create;
begin
  inherited;
  PreventDuplicates;
  SortedBy(CompareByElement);
end;

function TValidationOutcomeMarkList.itemClass: TAdvObjectClass;
begin
  result := TValidationOutcomeMark;
end;

function TValidationOutcomeMarkList.GetMark(index: integer): TValidationOutcomeMark;
begin
  result := TValidationOutcomeMark(objectByIndex[index]);
end;

procedure TValidationOutcomeMarkList.AddElement(elem: TFHIRElement; field: Integer; kind: TValidationOutcomeKind; message: String);
var
  mark : TValidationOutcomeMark;
begin
  mark := TValidationOutcomeMark.Create;
  try
    mark.obj := elem.Link;
    mark.FField := field;
    mark.FKind := kind;
    mark.FMessage := message;
    add(mark.Link);
  finally
    mark.Free;
  end;
end;

function TValidationOutcomeMarkList.CompareByElement(pA, pB: Pointer): Integer;
begin
  result := integer(TValidationOutcomeMark(pA).FObj) - integer(TValidationOutcomeMark(pB).FObj);
  if result = 0 then
    result := TValidationOutcomeMark(pA).field - TValidationOutcomeMark(pB).field;
end;

function TValidationOutcomeMarkList.GetByElement(elem: TFHIRElement; field : integer): TValidationOutcomeMark;
var
  mark : TValidationOutcomeMark;
  index : integer;
begin
  mark := TValidationOutcomeMark.Create;
  try
    mark.obj := elem.Link;
    mark.field := field;
    if Find(mark, index, CompareByElement) then
      result := GetMark(index)
    else
      result := nil;
  finally
    mark.free;
  end;
end;

{ TValueSetEditorCodeSystem }

function TValueSetEditorCodeSystem.filterPropertyOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
end;

function TValueSetEditorCodeSystem.filterOperationOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
end;

function TValueSetEditorCodeSystem.filterValueOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
end;

function TValueSetEditorCodeSystem.getCodeStatus(code: String; var msg: String): TValueSetEditorCodeSystemCodeStatus;
begin
  raise Exception.Create('Need to override getCodeStatus in '+className);
end;

function TValueSetEditorCodeSystem.getDisplay(code: String; var display: String): boolean;
begin
  raise Exception.Create('Need to override getDisplay in '+className);
end;

function TValueSetEditorCodeSystem.isWrongDisplay(code, display: String): boolean;
begin
  raise Exception.Create('Need to override isWrongDisplay in '+className);
end;

{ TValueSetEditorCodeSystemValueSet }

constructor TValueSetEditorCodeSystemValueSet.create(vs: TFhirValueSet);
begin
  Create;
  FConceptlist := vs.codeSystem.conceptList.Link;
  FCase := vs.codeSystem.caseSensitive;
end;

destructor TValueSetEditorCodeSystemValueSet.destroy;
begin
  FConceptList.Free;
  inherited;
end;

function TValueSetEditorCodeSystemValueSet.filterPropertyOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := prop = 'concept';
end;

function TValueSetEditorCodeSystemValueSet.filterOperationOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := op = FilterOperatorIsA;
end;

function TValueSetEditorCodeSystemValueSet.filterValueOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
var
  msg : String;
begin
  result := getCodeStatus(value, msg) = cscsOK;
end;

function TValueSetEditorCodeSystemValueSet.InList(list : TFhirValueSetCodeSystemConceptList; code : String) : TFhirValueSetCodeSystemConcept;
var
  c : TFhirValueSetCodeSystemConcept;
  i : integer;
begin
  result := nil;
  for i := 0 to list.count - 1 do
  begin
    c := list[i];
    if FCase then
    begin
      if c.code = code then
        result := c
    end
    else if SameText(c.code, code) then
      result := c;
    if result = nil then
      result := InList(c.conceptList, code);
    if result <> nil then
      break;
  end;
end;

function TValueSetEditorCodeSystemValueSet.getCodeStatus(code: String; var msg: String): TValueSetEditorCodeSystemCodeStatus;
begin
  if inList(FConceptList, code) <> nil then
    result := cscsOK
  else
  begin
    result := cscsUnknown;
    msg := 'No definition found for "'+code+'"';
  end;
end;

function TValueSetEditorCodeSystemValueSet.getDisplay(code: String; var display: String): boolean;
var
  c : TFhirValueSetCodeSystemConcept;
begin
  c := inList(FConceptList, code);
  if (c <> nil) and (c.display <> '') then
  begin
    result := true;
    display := c.display;
  end
  else
    result := false;
end;

function TValueSetEditorCodeSystemValueSet.isWrongDisplay(code, display: String): boolean;
var
  c : TFhirValueSetCodeSystemConcept;
begin
  c := inList(FConceptList, code);
  if (c <> nil) and (c.display <> '') and not SameText(display, c.display) then
    result := false
  else
    result := true;
end;

{ TValueSetEditorServerCache }

procedure TValueSetEditorServerCache.addClosure(s: String);
var
  client : TFhirHTTPClient;
  pin : TFhirParameters;
  pout : TFhirResource;
  ct : TClosureTableRecord;
begin
  ct := TClosureTableRecord.create;
  try
    ct.id := NewGuidId;
    ct.Name := s;
    ct.version := '0';
    ct.FFilename := IncludeTrailingBackslash(base)+'ct-'+ct.id+'.json';
    client := TFhirHTTPClient.create(nil, url, true);
    try
      client.UseIndy := true;
      pin := TFhirParameters.Create;
      try
        pin.AddParameter('name', ct.id);
        pout := client.operation(frtConceptMap, 'closure', pin);
        try
          if not (pout as TFhirParameters).bool['outcome'] then
            raise Exception.Create('Unexpected response from server');
        finally
          pout.Free;
        end;
      finally
        pin.Free;
      end;
    finally
      client.Free;
    end;
    ct.save;
    ini.WriteString('closures', ct.id, ct.name);
    closures.Add(s, ct.link);
  finally
    ct.Free;
  end;
end;

procedure TValueSetEditorServerCache.AddToClosure(name: String; coding: TFhirCoding);
var
  ct : TClosureTableRecord;
  client : TFhirHTTPClient;
  pin : TFhirParameters;
  pout : TFhirResource;
  fn : String;
begin
  ct := closures[name];
  ct.FConcepts.Add(coding.link);
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    pin := TFhirParameters.Create;
    try
      pin.AddParameter('name', ct.id);
      pin.AddParameter('concept', coding.Link);
      pout := client.operation(frtConceptMap, 'closure', pin);
      try
        if pout is TFHIRConceptMap then
          ct.process(pout as TFHIRConceptMap)
        else
          raise Exception.Create('Unexpected response from server');
      finally
        pout.Free;
      end;
    finally
      pin.Free;
    end;
  finally
    client.Free;
  end;
  ct.save;
end;

function TValueSetEditorServerCache.base: String;
begin
  result := Path([ShellLocalAppDataFolder, 'Health Intersections', 'ValueSetEditor' , 'server'+inttostr(FKey)]);
end;

procedure TValueSetEditorServerCache.CheckConnection;
var
  client : TFhirHTTPClient;
  conf : TFhirConformance;
  rest : TFhirConformanceRestResource;
  i, id : integer;
  uri : string;
  ini : TIniFile;
  fn : String;
begin
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    conf := client.conformance(false);
    try
      if not conf.fhirVersion.StartsWith(FHIR_GENERATED_VERSION+'-') then
        raise Exception.Create('Version Mismatch');
      rest := conf.rest(frtValueset);
      if (rest = nil) or (rest.interaction(TypeRestfulInteractionSearchType) = nil) or (rest.interaction(TypeRestfulInteractionRead) = nil) then
        raise Exception.Create('The server does not support the required opeerations for value sets');
      ini := TIniFile.Create(base+'server.ini');
      try
        for i := 0 to conf.getExtensionCount('http://hl7.org/fhir/Profile/tools-extensions#supported-system') - 1 do
        begin
          uri := conf.getExtensionString('http://hl7.org/fhir/Profile/tools-extensions#supported-system', i);
          fn := ini.ReadString('system', uri, '');
          if (fn = '') then
          begin
            id := ini.ReadInteger('system', 'last', 0) + 1;
            ini.WriteInteger('system', 'last', id);
            fn := 'cache-'+inttostr(id)+'.json';
            ini.writeString('system', uri, fn);
          end;
          specialCodeSystems.Add(uri, TServerCodeSystem.create(uri, FUrl, base+fn));
        end;
      finally
        ini.Free;
      end;
    finally
      conf.free;
    end;
  finally
    client.Free;
  end;
end;

procedure TValueSetEditorServerCache.checkLoad(event: TFhirHTTPClientStatusEvent; null : String);
begin
  if not FLoaded then
  begin
    load(event);
    update(event, null);
  end;
end;

constructor TValueSetEditorServerCache.Create(name, url, username, password : String; doesSearch : boolean; key : integer);
begin
  inherited Create;
  FKey := key;
  Furl := url;
  FName := name;
  FLoaded := false;
  Fusername := username;
  Fpassword := password;
  FDoesSearch := doesSearch;
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(base)+'server.ini');
  Flist := TFHIRValueSetList.Create;
  valuesets := TAdvStringObjectMatch.Create;
  valuesets.Forced := true;
  valuesets.PreventDuplicates;
  Fcodesystems := TAdvStringObjectMatch.Create;
  Fcodesystems.Forced := true;
  Fcodesystems.PreventDuplicates;
  sortedCodesystems := TStringList.Create;
  sortedCodesystems.Sorted := true;
  sortedValueSets := TStringList.Create;
  sortedValueSets.Sorted := true;
  specialCodeSystems := TAdvStringObjectMatch.create;
  closures := TAdvMap<TClosureTableRecord>.create;

end;

destructor TValueSetEditorServerCache.Destroy;
begin
  closures.Free;
  specialCodeSystems.Free;
  valuesets.Free;
  codesystems.Free;
  sortedCodesystems.Free;
  sortedValueSets.Free;
  FList.Free;
  ini.Free;
  inherited;
end;

function TValueSetEditorServerCache.expand(url, filter: String; count: integer; allowIncomplete : boolean): TFHIRValueSet;
var
  client : TFhirHTTPClient;
  pIn, pOut : TFhirParameters;
  rOut : TFHIRResource;
  feed : TFHIRBundle;
begin
  client := TFhirHTTPClient.create(nil, FUrl, true);
  try
    client.UseIndy := true;
    client.OnClientStatus := nil;
    pIn := TFhirParameters.Create;
    try
      pIn.AddParameter('identifier', url);
      if filter <> '' then
        pIn.AddParameter('filter', filter);
      if count <> 0 then
        pIn.AddParameter('count', inttostr(count));
      if allowIncomplete then
        pIn.AddParameter('_incomplete', 'true');

      rOut := client.operation(frtValueset, 'expand', pIn);
      try
        if rOut is TFhirValueSet then
          result := (rOut as TFhirValueSet).Link
        else if rOut is TFhirParameters then
        begin
          pOut := TFhirParameters(rOut);
          if pOut.hasParameter('return') then
            result := (pOut['return'] as TFHIRValueSet).Link
          else
            raise Exception.Create('Unable to process result from expansion server');
        end
        else
          raise Exception.Create('Unable to process result from expansion server');
      finally
        rOut.Free;
      end;
    finally
      pIn.free;
    end;
  finally
    client.Free;
  end;
end;

function TValueSetEditorServerCache.Link: TValueSetEditorServerCache;
begin
  result := TValueSetEditorServerCache(inherited Link);
end;

procedure TValueSetEditorServerCache.load(event : TFhirHTTPClientStatusEvent);
var
  json : TFHIRJsonParser;
  f : TFileStream;
  i : integer;
  bundle : TFHIRBundle;
  ts : TStringList;
  fn : String;
begin
  if not FileExists(ExtractFilePath(ini.FileName)) then
  begin
    CreateDir(ExtractFilePath(ini.FileName));
    ini.WriteString('id', 'url', url)
  end
  else if (ini.ReadString('id', 'url', '') <> url) then
    raise Exception.Create('Ini mismatch');

  FLastUpdated := ini.ReadString('id', 'last-updated', '');
  if FileExists(base+'list.json') then
  begin
    if assigned(event) then event(self, 'Load Cache');
    json := TFHIRJsonParser.create(nil, 'en');
    try
      f := TFileStream.Create(base+'list.json', fmOpenRead + fmShareDenyWrite);
      try
        json.source := f;
        json.Parse;
        bundle := json.resource as TFhirBundle;
        for i := 0 to bundle.entryList.Count - 1 do
        begin
          if assigned(event) then event(self, 'Load Cache, '+inttostr(i)+' of '+inttostr(bundle.entryList.Count));
          SeeValueset(bundle.entryList[i].resource as TFhirValueSet, true, true);
        end;
      finally
        f.Free;
      end;
    finally
      json.free;
    end;
    FLoaded := true;
  end;

  ts := TStringList.Create;
  try
    ini.ReadSection('closures', ts);
    for i := 0 to ts.Count - 1 do
    begin
      fn := IncludeTrailingBackslash(base)+'ct-'+ts[i]+'.json';
      if FileExists(fn) then
        closures.Add(ts[i], TClosureTableRecord.Create(fn))
      else
        closures.Add(ts[i], TClosureTableRecord.Create(fn, ts[i], ini.ReadString('closures', ts[i], '')));
    end;
  finally
    ts.Free;
  end;
end;

procedure TValueSetEditorServerCache.loadClosures(list: TStrings);
var
  s : String;
begin
  list.Clear;
  for s in closures.Keys do
    list.Add(s);
end;

procedure TValueSetEditorServerCache.save;
var
  json : TFHIRComposer;
  f : TFileStream;
  bnd : TFHIRBundle;
  vs : TFhirValueSet;
begin
  ini.WriteString('id', 'last-updated', FLastUpdated);
  bnd := TFhirBundle.Create(BundleTypeCollection);
  try
    for vs in FList do
      bnd.entryList.Append.resource := vs.Link;

    f := TFileStream.Create(base+'list.json', fmCreate);
    try
      json := TFHIRJsonComposer.Create(nil, 'en');
      try
        json.Compose(f, bnd, false);
      finally
        json.Free;
      end;
    finally
      f.free;
    end;
  finally
    bnd.free;
  end;
end;

function TValueSetEditorServerCache.LoadFullCodeSystem(uri : String) : TFhirValueSet;
var
  id : String;
  json : TFHIRJsonParser;
  f : TFileStream;
begin
  id := ini.ReadString('codesystems', uri, '');
  if (id = '') or not FileExists(base+id+'.json') then
    result := nil
  else
  begin
    f := TFileStream.Create(base+id+'.json', fmOpenRead + fmShareDenyWrite);
    try
      json := TFHIRJsonParser.Create(nil, 'en');
      try
        json.source := f;
        json.Parse;
        result := json.resource.link as TFhirValueSet;
      finally
        json.free;
      end;
    finally
      f.Free;
    end;
  end;
end;

procedure TValueSetEditorServerCache.ResetClosure(name: String);
var
  client : TFhirHTTPClient;
  pin : TFhirParameters;
  pout : TFhirResource;
  ct : TClosureTableRecord;
begin
  ct := closures[name];
  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    pin := TFhirParameters.Create;
    try
      pin.AddParameter('name', ct.id);
      pout := client.operation(frtConceptMap, 'closure', pin);
      try
        if not (pout as TFhirParameters).bool['outcome'] then
          raise Exception.Create('Unexpected response from server');
      finally
        pout.Free;
      end;
    finally
      pin.Free;
    end;
  finally
    client.Free;
  end;
  ct.FConcepts.Clear;
  ct.FMaps.Clear;
  ct.save;
end;

procedure TValueSetEditorServerCache.SeeValueset(vs : TFHIRValueSet; isSummary, loading : boolean);
var
  json : TFHIRComposer;
  f : TFileStream;
  id : String;
  i : integer;
  b : boolean;
begin
  if not loading then
  begin
    if FileExists(base+vs.id+'.json') then
      DeleteFile(base+vs.id+'.json');

    if not (isSummary or vs.meta.HasTag('http://hl7.org/fhir/tag', 'http://healthintersections.com.au/fhir/tags/summary')) then
    begin
      f := TFileStream.Create(base+id+'.json', fmCreate);
      try
        json := TFHIRJsonComposer.Create(nil, 'en');
        try
          json.Compose(f, vs, false);
        finally
          json.Free;
        end;
      finally
        f.free;
      end;
      if (vs.codeSystem <> nil) then
        ini.WriteString('codeSystems', vs.codeSystem.system, id);
    end;
    // now, make it empty
    if vs.codeSystem <> nil then
      vs.codeSystem.conceptList.Clear;
    if vs.compose <> nil then
      for i := 0 to vs.compose.includeList.Count - 1 do
      begin
        vs.compose.includeList[i].conceptList.Clear;
        vs.compose.includeList[i].filterList.Clear;
        vs.text := nil;
      end;
    vs.expansion := nil;
  end;

  // registering it..
  b := false;
  for i := 0 to Flist.count - 1 do
    if Flist[i].id = vs.id then
    begin
      Flist[i] := vs.link;
      b := true;
    end;
  if not b then
    Flist.add(vs.Link);

  valuesets.Matches[vs.url] := vs.Link;
  if not sortedValueSets.Find(vs.url, i) then
    sortedValueSets.Add(vs.url);

  if vs.codeSystem <> nil then
  begin
    codesystems.Matches[vs.codeSystem.System] := vs.Link;
    if not sortedCodesystems.Find(vs.codeSystem.System, i) then
      sortedCodesystems.Add(vs.codeSystem.System);
  end;
end;

{ TServerCodeSystem }

constructor TServerCodeSystem.create(uri, url, filename: String);
begin
  Create;
  FSystem := uri;
  FClient := TFhirHTTPClient.create(nil, url, true);
  Fclient.UseIndy := true;
  FCache := TAdvStringObjectMatch.create;
  FFilename := filename;
  if FileExists(FFilename) then
    Load;
end;

destructor TServerCodeSystem.Destroy;
begin
  FClient.Free;
  FCache.Free;
  inherited;
end;


function TServerCodeSystem.filterPropertyOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
  if FSystem = 'http://snomed.info' then
  begin
    result := (prop = 'expression') or (prop = 'concept');
  end
  else if FSystem = 'http://loinc.org' then
  begin
    result := StringArrayExistsSensitive(['COMPONENT', 'PROPERTY', 'TIME_ASPCT', 'SYSTEM', 'SCALE_TYP', 'METHOD_TYP',
       'Document.Kind', 'Document.TypeOfService', 'Document.Setting', 'Document.Role', 'Document.SubjectMatterDomain'], prop)
  end;
end;

function TServerCodeSystem.filterOperationOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
  if FSystem = 'http://snomed.info' then
  begin
    result := ((prop = 'expression') and (op in [FilterOperatorEqual])) or ((prop = 'concept') and (op in [FilterOperatorIsA, FilterOperatorIn]));
  end
  else if FSystem = 'http://loinc.org' then
  begin
    result := (StringArrayExistsSensitive(['COMPONENT', 'PROPERTY', 'TIME_ASPCT', 'SYSTEM', 'SCALE_TYP', 'METHOD_TYP',
       'Document.Kind', 'Document.TypeOfService', 'Document.Setting', 'Document.Role', 'Document.SubjectMatterDomain'], prop) and (op in [FilterOperatorEqual, FilterOperatorRegex]))
       or (('Type' = prop) and (op in [FilterOperatorEqual]));
  end;
end;

function TServerCodeSystem.filterValueOK(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
var
  msg : String;
begin
  result := false;
  if FSystem = 'http://snomed.info' then
  begin
    if prop = 'expression' then
      // we do not presently validate expressions
      result := true
    else if prop = 'concept' then
      // todo: validate that a "in" is associated with a reference set
      result := getCodeStatus(value, msg) = cscsOK;
  end
  else if FSystem = 'http://loinc.org' then
  begin
    // todo...
  end;
end;

function TServerCodeSystem.getCodeStatus(code: String; var msg: String): TValueSetEditorCodeSystemCodeStatus;
var
  item : TServerCodeSystemCacheItem;
begin
  if not hasCache(code, item) then
    loadCode(code, item);
  result := item.status;
  msg := item.message;
end;

function TServerCodeSystem.getDisplay(code: String; var display: String): boolean;
var
  item : TServerCodeSystemCacheItem;
begin
  if not hasCache(code, item) then
    loadCode(code, item);
  result := item.status = cscsOK;
  if result and (item.displays.count > 0) then
    display := item.displays[0];
end;


function TServerCodeSystem.HasCache(code: String; var item: TServerCodeSystemCacheItem): boolean;
var
  i : integer;
begin
  i := FCache.IndexByKey(code);
  result := i > -1;
  if result then
    item := FCache.ValueByIndex[i] as TServerCodeSystemCacheItem;
end;

function TServerCodeSystem.isWrongDisplay(code, display: String): boolean;
var
  item : TServerCodeSystemCacheItem;
begin
  if not hasCache(code, item) then
    loadCode(code, item);
  result := item.displays.IndexOf(display) > -1;
end;

procedure TServerCodeSystem.loadCode(code: String; var item: TServerCodeSystemCacheItem);
var
  params : TAdvStringMatch;
  feed : TFHIRBundle;
  vs : TFhirValueSet;
begin
  params := TAdvStringMatch.Create;
  try
    params.forced := true;
    params.Matches['_query'] := 'expand';
    params.Matches['system'] := FSystem;
    params.Matches['code'] := code;
    feed := FClient.search(frtValueSet, true, params);
    try
      if (feed.entryList.Count = 1) and (feed.entryList[0].resource.ResourceType = frtValueSet) then
      begin
        vs := feed.entryList[0].resource as TFhirValueSet;
        if (vs.expansion <> nil) and (vs.expansion.containsList.Count = 1)
           and (vs.expansion.containsList[0].System = FSystem) and (vs.expansion.containsList[0].code = code) then
        begin
          item := TServerCodeSystemCacheItem.Create(cscsOK, '');
          item.displays.Add(vs.expansion.containsList[0].display);
        end
        else
          item := TServerCodeSystemCacheItem.Create(cscsUnknown, 'Code '''+code+''' not found');
      end
      else
        item := TServerCodeSystemCacheItem.Create(cscsUnknown, 'Code '''+code+''' not found');
      FCache.Add(code, item);
      Save;
    finally
      feed.Free;
    end;
  finally
    params.Free;
  end;
end;

procedure TServerCodeSystem.Load;
var
  json, o, d : TJsonObject;
  n, n1 : TJsonNode;
  f : TAdvFile;
  item : TServerCodeSystemCacheItem;
begin
  f := TAdvFile.Create;
  try
    f.Name := FFilename;
    f.OpenRead;
    json := TJSONParser.Parse(f);
    try
      for n in json.vArr['items'] do
      begin
        o := n as TJsonObject;
        item := TServerCodeSystemCacheItem.Create;
        try
          if o['ok'] = 'no' then
            item.status := cscsUnknown
          else
            item.status := cscsOK;
          item.message := o['msg'];
          for n1 in o.vArr['displays'] do
          begin
            d := n1 as TJsonObject;
            item.displays.Add(d['value']);
          end;
          FCache.Add(o['code'], item.Link);
        finally
          item.Free;
        end;
      end;
    finally
      json.free;
    end;
  finally
    f.Free;
  end;
end;

procedure TServerCodeSystem.Save;
var
  json : TJSONWriter;
  f : TAdvFile;
  i : integer;
  c : String;
  item : TServerCodeSystemCacheItem;
begin
  f := TAdvFile.Create;
  try
    f.Name := FFilename;
    f.OpenCreate;
    json := TJSONWriter.Create;
    try
      json.Stream := f.link;
      json.Start;
      json.ValueArray('items');
      for i := 0 to FCache.Count - 1 do
      begin
        json.ValueObject;
        c := FCache.KeyByIndex[i];
        item := FCache.ValueByIndex[i] as TServerCodeSystemCacheItem;
        json.Value('code', c);
        if item.status = cscsUnknown then
          json.Value('ok', 'no');
        if (item.message <> '') then
          json.Value('msg', item.message);
        json.ValueArray('displays');
        for c in item.displays do
        begin
          json.ValueObject;
          json.Value('value', c);
          json.FinishObject;
        end;
        json.FinishArray;
        json.FinishObject;
      end;
      json.FinishArray;
      json.Finish;
    finally
      json.Free;
    end;
  finally
   f.free;
  end;
end;

{ TServerCodeSystemCacheItem }

constructor TServerCodeSystemCacheItem.Create;
begin
  inherited;
  displays := TStringList.Create;
end;

constructor TServerCodeSystemCacheItem.Create(status: TValueSetEditorCodeSystemCodeStatus; message: String);
begin
  Create;
  self.status := status;
  self.message := message;
end;

destructor TServerCodeSystemCacheItem.Destroy;
begin
  displays.Free;
  inherited;
end;

{ TClosureTableRecord }

constructor TClosureTableRecord.create(filename: String);
var
  f : TFileStream;
  json, ao, ao1 : TJsonObject;
  a, a1 : TJsonNode;
  c : TFhirCoding;
  s : TClosureTableRecordSource;
begin
  Create;
  FFilename := filename;
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f);
    try
      id := json.str['id'];
      name := json.str['name'];
      version := json.str['version'];
      for a in json.arr['concepts'] do
      begin
        ao := a as TJsonObject;
        c := TFhirCoding.Create;
        FConcepts.Add(c);
        c.system := ao.str['system'];
        c.code := ao.str['code'];
        c.display := ao.str['display'];
      end;
      for a in json.arr['maps'] do
      begin
        ao := a as TJsonObject;
        s := TClosureTableRecordSource.Create;
        FMaps.Add(s);
        s.system := ao.str['system'];
        s.code := ao.str['code'];
        for a1 in ao.arr['targets'] do
        begin
          ao1 := a1 as TJsonObject;
          c := TFhirCoding.Create;
          s.targets.Add(c);
          c.system := ao1.str['system'];
          c.code := ao1.str['code'];
        end;
      end;
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TClosureTableRecord.save;
var
  f : TFileStream;
  json, ao, ao1 : TJsonObject;
  arr, arr1 : TJsonArray;
  c : TFhirCoding;
  s : TClosureTableRecordSource;
begin
  json := TJsonObject.Create;
  try
    json.str['id'] := id;
    json.str['name'] := name;
    json.str['version'] := version;
    arr := json.forceArr['concepts'];
    for c in FConcepts do
    begin
      ao := arr.addObject;
      ao.str['system'] := c.system;
      ao.str['code'] := c.code;
      ao.str['display'] := c.display;
    end;
    arr := json.forceArr['maps'];
    for s in FMaps do
    begin
      ao := arr.addObject;
      ao.str['system'] := s.system;
      ao.str['code'] := s.code;
      arr1 := ao.forceArr['targets'];
      for c in s.targets do
      begin
        ao1 := arr1.addObject;
        ao1.str['system'] := c.system;
        ao1.str['code'] := c.code;
      end;
    end;

    f := TFileStream.Create(Ffilename, fmCreate);
    try
      TJSONWriter.writeObject(f, json, true);
    finally
      f.Free;
    end;
  finally
    json.Free;
  end;
end;

constructor TClosureTableRecord.create(filename, id, name: String);
begin
  Create;
  FFilename := filename;
  self.id := id;
  self.name := name;
  version := '0';
end;

destructor TClosureTableRecord.Destroy;
begin
  FMaps.Free;
  FConcepts.Free;
  inherited;
end;

constructor TClosureTableRecord.create;
begin
  inherited;
  FMaps := TAdvList<TClosureTableRecordSource>.create;
  FConcepts := TAdvList<TFHIRCoding>.create;
end;

function TClosureTableRecord.GetLinks(row, col: integer): TClosureDirection;
var
  r, c : TFHIRCoding;
begin
  r := FConcepts[row];
  c := FConcepts[col];
  if hasMap(r, c) then
    result := cdSubsumes
  else if hasMap(c, r) then
    result := cdSubsumed
  else
    result := cdNull;
end;

function TClosureTableRecord.GetMapCount: integer;
var
  s : TClosureTableRecordSource;
begin
  result := FMaps.Count;
  for s in FMaps do
    result := result + s.targets.Count;

end;

function TClosureTableRecord.getSource(uri, code: String): TClosureTableRecordSource;
begin
  for result in FMaps do
    if (result.system = uri) and (result.code = code) then
      exit;
  result := TClosureTableRecordSource.Create;
  try
    result.system := uri;
    result.code := code;
    FMaps.Add(result.link);
  finally
    result.Free;
  end;
end;

function TClosureTableRecord.hasMap(src, tgt: TFHIRCoding): boolean;
var
  s : TClosureTableRecordSource;
  c : TFHIRCoding;
begin
  result := false;
  for s in FMaps do
    if (s.system = src.system) and (s.code = src.code) then
      for c in s.targets do
        if (c.system = tgt.system) and (c.code = tgt.code) then
          exit(true);
end;

function TClosureTableRecord.link: TClosureTableRecord;
begin
  result := TClosureTableRecord(inherited Link);
end;

procedure TClosureTableRecord.process(cm: TFHIRConceptMap);
var
  element : TFhirConceptMapElement;
  src : TClosureTableRecordSource;
  target : TFhirConceptMapElementTarget;
begin
  version := cm.version;
  for element in cm.elementList do
    for target in element.targetList do
      if target.equivalence = ConceptMapEquivalenceSubsumes then
      begin
        src := getSource(element.codeSystem, element.code);
        src.add(target.codeSystem, target.code);
      end
      else if target.equivalence = ConceptMapEquivalenceSpecializes then
      begin
        src := getSource(target.codeSystem, target.code);
        src.add(element.codeSystem, element.code);
      end
      else
        raise Exception.Create('Unhandled equivalance '+CODES_TFhirConceptMapEquivalenceEnum[target.equivalence]);
end;

{ TClosureTableRecordSource }

procedure TClosureTableRecordSource.add(uri, code: String);
var
  c : TFhirCoding;
begin
  for c in targets do
    if (c.system = uri) and (c.code = code) then
      exit;
  c := TFhirCoding.Create;
  try
    c.system := uri;
    c.code := code;
    targets.Add(c.link);
  finally
    c.Free;
  end;
end;

constructor TClosureTableRecordSource.Create;
begin
  inherited Create;
  FTargets := TAdvList<TFHIRCoding>.create;
end;

destructor TClosureTableRecordSource.Destroy;
begin
  FTargets.Free;
  inherited;
end;

function TClosureTableRecordSource.link: TClosureTableRecordSource;
begin
  result := TClosureTableRecordSource(inherited Link);
end;

end.


(*


  list := TStringList.Create;
  try
    Settings.ini.ReadSection('systems', list);
    for s in list do
      FCodeSystemNames.Add(s, Settings.ini.ReadString('systems', s, '??'));
    Settings.ini.ReadSection('valuesets', list);
    for s in list do
      FValueSetNames.Add(s, Settings.ini.ReadString('valuesets', s, '??'));
  finally
    list.Free;
  end;
  if FileExists(FSettings.ValuesetListPath) then
  begin
    f := TFileStream.Create(FSettings.ValuesetListPath, fmOpenRead + fmShareDenyWrite);
    try
      p := TFHIRJsonParser.Create('en');
      try
        p.source := F;
        p.Parse;
        FValueSetlist := p.feed.Link;
      finally
        p.free
      end;
    finally
      f.Free;
    end;
  end
  else
    FValueSetlist := nil;

  FCodeSystemNames.Free;
  FValueSetNames.Free;
  FValueSetList.Free;

procedure TValueSetEditorContext.listServerValuesets(url: String);
var
  client : TFhirHTTPClient;
  params : TAdvStringMatch;
  c : TFHIRJsonComposer;
  f : TFileStream;
  i : Integer;
  vs : TFhirValueSet;
begin
  Settings.ServerURL := url;

  client := TFhirHTTPClient.create(nil, url, true);
  try
    client.UseIndy := true;
    params := TAdvStringMatch.Create;
    try
      params.Add('_summary', 'true');
      params.Add('_count', '1000');
      FValueSetlist := client.search(frtValueset, true, params);
      for i := 0 to FValueSetList.entryList.Count -1 do
      begin
        vs := TFhirValueSet(FValueSetList.entryList[i].resource);
        FValueSetNames.Add(vs.identifier, vs.nameST);
        FSettings.ini.WriteString('valuesets', vs.identifier, vs.nameST);
      end;
      f := TFileStream.Create(Settings.ValuesetListPath, fmCreate);
      try
        c := TFHIRJsonComposer.Create('en');
        try
          c.Compose(f, FValueSetList, true);
        finally
          c.Free;
        end;
      finally
        f.free;
      end;
    finally
      params.free;
    end;
  finally
    client.Free;
  end;
end;


*)
