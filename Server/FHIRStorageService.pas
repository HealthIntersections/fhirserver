unit FHIRStorageService;

interface

uses
  SysUtils, Classes, System.Generics.Collections,
  KCritSct,
  AdvObjects, AdvGenerics, AdvStringMatches,  ThreadSupport,
  KDBManager,
  SCIMServer,
  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, FHIRConstants, FHIRIndexManagers, FHIRUtilities,
  FHIRValidator, ServerValidator, FHIRSubscriptionManager, TerminologyServer, ServerUtilities;

Const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'b35b7vX3KTAe-';

Type
  TQuestionnaireCache = class(TAdvObject)
  private
    FLock: TCriticalSection;
    FQuestionnaires: TAdvMap<TFhirQuestionnaire>;
    FForms: TAdvStringMatch;
    FValueSetDependencies: TDictionary<String, TList<string>>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure putQuestionnaire(rtype: TFHIRResourceType; id: String; q: TFhirQuestionnaire; dependencies: TList<String>);
    procedure putForm(rtype: TFHIRResourceType; id: String; form: String; dependencies: TList<String>);

    function getQuestionnaire(rtype: TFHIRResourceType; id: String) : TFhirQuestionnaire;
    function getForm(rtype: TFHIRResourceType; id: String): String;

    procedure clear(rtype: TFHIRResourceType; id: String); overload;
    procedure clearVS(id: string);
    procedure clear; overload;
  end;

  TPopulateConformanceEvent = procedure (sender : TObject; conf : TFhirCapabilityStatement) of object;

  TOperationContext = class (TAdvObject)
  private
    FUpload : boolean;
    FCallback : TInstallerCallback;
    FMessage : String;
  public
    constructor Create; overload; override;
    constructor Create(upload : boolean; callback : TInstallerCallback; message : String); overload;

    property upload : boolean read FUpload write FUpload;
    property callback : TInstallerCallback read FCallback write FCallback;
    property message : String read FMessage write FMessage;

    procedure progress(i : integer);
  end;

  TFhirOperationManagerBase = class (TAdvObject)
  private
    FOnPopulateConformance : TPopulateConformanceEvent;
  public
    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;

    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String;  virtual;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; virtual;
  end;

  TFHIRStorageService = class (TAdvObject)
  private
    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;
    FOwnerName: String;
    FBases: TStringList;
    FQuestionnaireCache: TQuestionnaireCache;
    FResConfig: TAdvMap<TFHIRResourceConfig>;
    FForLoad : boolean;
    procedure SetSCIMServer(const Value: TSCIMServer);
  protected
    FValidatorContext : TFHIRServerWorkerContext;
    FDB: TKDBManager;
    FSCIMServer: TSCIMServer;
    FIndexes : TFHIRIndexInformation;
    FSubscriptionManager: TSubscriptionManager;
    FValidator: TFHIRValidator;
    FTerminologyServer: TTerminologyServer;
    function GetTotalResourceCount: integer; virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function Link : TFHIRStorageService; overload;
    Property DB: TKDBManager read FDB;
    Property OwnerName: String read FOwnerName write FOwnerName;
    Property Bases: TStringList read FBases;
    property QuestionnaireCache: TQuestionnaireCache read FQuestionnaireCache;

    Property TotalResourceCount: integer read GetTotalResourceCount;
    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure write FFormalURLSecure;
    property FormalURLPlainOpen: String read FFormalURLPlainOpen write FFormalURLPlainOpen;
    property FormalURLSecureOpen: String read FFormalURLSecureOpen write FFormalURLSecureOpen;
    property FormalURLSecureClosed: String read FFormalURLSecureClosed write FFormalURLSecureClosed;
    Property ValidatorContext : TFHIRServerWorkerContext read FValidatorContext;
    Property TerminologyServer: TTerminologyServer read FTerminologyServer;

    procedure RegisterConsentRecord(session: TFhirSession); virtual;
    function RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession; virtual;
    function GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean; virtual;
    function GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean; virtual;
    Function CreateImplicitSession(clientInfo: String; server: Boolean) : TFhirSession; virtual;
    Procedure EndSession(sCookie, ip: String); virtual;
    function isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean; virtual;
    procedure MarkSessionChecked(sCookie, sName: String); virtual;
    function DumpSessions : String; virtual;
    procedure Sweep; virtual;

    function ProfilesAsOptionList : String; virtual;

    procedure ProcessSubscriptions; virtual;
    procedure ProcessObservations; virtual;
    procedure RunValidation; virtual;

    procedure CloseAll; virtual;
    property Indexes : TFHIRIndexInformation read FIndexes;
    property SubscriptionManager : TSubscriptionManager read FSubscriptionManager;
    Property Validator: TFHIRValidator read FValidator;
    Property ResConfig: TAdvMap<TFHIRResourceConfig> read FResConfig;

    function createOperationContext(lang : String) : TFhirOperationManagerBase; virtual;
    Procedure Yield(op : TFhirOperationManagerBase; exception : Exception); virtual;
    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet; virtual;
    function LookupCode(system, version, code: String): String; virtual;
    property ForLoad : boolean read FForLoad write FForLoad;
    property SCIMServer : TSCIMServer read FSCIMServer write SetSCIMServer;
  end;


implementation

{ TFHIRStorageService }

procedure TFHIRStorageService.CloseAll;
begin
  raise Exception.Create('The function "CloseAll" must be overridden in '+className);
end;

constructor TFHIRStorageService.Create;
var
  a: TFHIRResourceType;
  cfg : TFHIRResourceConfig;
begin
  inherited;
  FBases := TStringList.Create;
  FBases.add('http://localhost/');
  FQuestionnaireCache := TQuestionnaireCache.Create;
  FResConfig := TAdvMap<TFHIRResourceConfig>.create;
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
  begin
    cfg := TFHIRResourceConfig.Create;
    cfg.name := CODES_TFHIRResourceType[a];
    cfg.Supported := false;
    FResConfig.Add(cfg.name,  cfg);
  end;
end;

function TFHIRStorageService.CreateImplicitSession(clientInfo: String;  server: Boolean): TFhirSession;
begin
  raise Exception.Create('The function "CreateImplicitSession(clientInfo: String;  server: Boolean): TFhirSession" must be overridden in '+className);
end;

function TFHIRStorageService.createOperationContext(lang: String): TFhirOperationManagerBase;
begin
  raise Exception.Create('The function "createOperationContext(lang: String): TFhirOperationManagerBase" must be overridden in '+className);
end;

destructor TFHIRStorageService.Destroy;
begin
  FSCIMServer.Free;
  FQuestionnaireCache.free;
  FBases.free;
  inherited;
end;

function TFHIRStorageService.DumpSessions: String;
begin
  raise Exception.Create('The function "DumpSessions: String" must be overridden in '+className);
end;

procedure TFHIRStorageService.EndSession(sCookie, ip: String);
begin
  raise Exception.Create('The function "EndSession(sCookie, ip: String)" must be overridden in '+className);
end;

function TFHIRStorageService.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet;
begin
  raise Exception.Create('The function "ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet" must be overridden in '+className);
end;

function TFHIRStorageService.GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
begin
  raise Exception.Create('The function "GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
begin
  raise Exception.Create('The function "GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.GetTotalResourceCount: integer;
begin
  raise Exception.Create('The function "GetTotalResourceCount: integer" must be overridden in '+className);
end;

function TFHIRStorageService.isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
begin
  raise Exception.Create('The function "isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.Link: TFHIRStorageService;
begin
  result := TFHIRStorageService(inherited Link);
end;

function TFHIRStorageService.LookupCode(system, version, code: String): String;
begin
  raise Exception.Create('The function "LookupCode(system, version, code: String): String" must be overridden in '+className);
end;

procedure TFHIRStorageService.MarkSessionChecked(sCookie, sName: String);
begin
  raise Exception.Create('The function "MarkSessionChecked(sCookie, sName: String)" must be overridden in '+className);
end;

procedure TFHIRStorageService.ProcessObservations;
begin
  raise Exception.Create('The function "ProcessObservations" must be overridden in '+className);
end;

procedure TFHIRStorageService.ProcessSubscriptions;
begin
  raise Exception.Create('The function "ProcessSubscriptions" must be overridden in '+className);
end;

function TFHIRStorageService.ProfilesAsOptionList: String;
begin
  raise Exception.Create('The function "ProfilesAsOptionList: String" must be overridden in '+className);
end;

procedure TFHIRStorageService.RegisterConsentRecord(session: TFhirSession);
begin
  raise Exception.Create('The function "RegisterConsentRecord(session: TFhirSession)" must be overridden in '+className);
end;

function TFHIRStorageService.RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
begin
  raise Exception.Create('The function "RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession" must be overridden in '+className);
end;

procedure TFHIRStorageService.RunValidation;
begin
  raise Exception.Create('The function "RunValidation" must be overridden in '+className);
end;

procedure TFHIRStorageService.SetSCIMServer(const Value: TSCIMServer);
begin
  FSCIMServer.Free;
  FSCIMServer := Value;
end;

procedure TFHIRStorageService.Sweep;
begin
  raise Exception.Create('The function "Sweep" must be overridden in '+className);
end;

procedure TFHIRStorageService.Yield(op: TFhirOperationManagerBase; exception : Exception);
begin
  raise Exception.Create('The function "Yield(op: TFhirOperationManagerBase; exception : Exception)" must be overridden in '+className);
end;

{ TQuestionnaireCache }

constructor TQuestionnaireCache.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('TQuestionnaireCache');
  FQuestionnaires := TAdvMap<TFhirQuestionnaire>.Create;
  FForms := TAdvStringMatch.Create;
  FForms.Forced := true;
  FValueSetDependencies := TDictionary < String, TList < string >>.Create;
end;

destructor TQuestionnaireCache.Destroy;
begin
  FValueSetDependencies.free;
  FForms.free;
  FQuestionnaires.free;
  FLock.free;
  inherited;
end;

procedure TQuestionnaireCache.clear;
begin
  FLock.Lock('clear');
  try
    FQuestionnaires.clear;
    FForms.clear;
    FValueSetDependencies.clear;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clearVS(id: string);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('clear(id)');
  try
    if FValueSetDependencies.TryGetValue(id, l) then
    begin
      for s in l do
      begin
        if FQuestionnaires.ContainsKey(s) then
          FQuestionnaires.Remove(s);
        if FForms.ExistsByKey(s) then
          FForms.DeleteByKey(s);
      end;
      FValueSetDependencies.Remove(s);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clear(rtype: TFHIRResourceType; id: String);
var
  s: String;
begin
  s := CODES_TFHIRResourceType[rtype] + '/' + id;
  FLock.Lock('clear(id)');
  try
    if FQuestionnaires.ContainsKey(s) then
      FQuestionnaires.Remove(s);
    if FForms.ExistsByKey(s) then
      FForms.DeleteByKey(s);
  finally
    FLock.Unlock;
  end;
end;

function TQuestionnaireCache.getForm(rtype: TFHIRResourceType;
  id: String): String;
begin
  FLock.Lock('getForm');
  try
    result := FForms[CODES_TFHIRResourceType[rtype] + '/' + id];
  finally
    FLock.Unlock;
  end;

end;

function TQuestionnaireCache.getQuestionnaire(rtype: TFHIRResourceType;
  id: String): TFhirQuestionnaire;
begin
  FLock.Lock('getQuestionnaire');
  try
    result := FQuestionnaires[CODES_TFHIRResourceType[rtype] + '/' + id]
      .Link as TFhirQuestionnaire;
    // comes off linked - must happen inside the lock
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putForm(rtype: TFHIRResourceType;
  id, form: String; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('putForm');
  try
    FForms[CODES_TFHIRResourceType[rtype] + '/' + id] := form;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.Create;
        FValueSetDependencies.add(s, l);
      end;
      if not l.Contains(id) then
        l.add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putQuestionnaire(rtype: TFHIRResourceType;
  id: String; q: TFhirQuestionnaire; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('putQuestionnaire');
  try
    FQuestionnaires[CODES_TFHIRResourceType[rtype] + '/' + id] := q.Link;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.Create;
        FValueSetDependencies.add(s, l);
      end;
      if not l.Contains(id) then
        l.add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

{ TFhirOperationManagerBase }


{ TOperationContext }

constructor TOperationContext.Create;
begin
  inherited Create;
end;

constructor TOperationContext.Create(upload: boolean; callback: TInstallerCallback; message : String);
begin
  Create;
  FUpload := upload;
  FCallback := callback;
  FMessage := message;
end;

procedure TOperationContext.progress(i: integer);
begin
  if assigned(FCallback) then
    FCallback(i, FMessage);
end;


{ TFhirOperationManagerBase }

function TFhirOperationManagerBase.Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  raise Exception.Create('The function "Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String" must be overridden in '+className);
end;

function TFhirOperationManagerBase.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise Exception.Create('The function "LookupReference(context: TFHIRRequest; id: String): TResourceWithReference" must be overridden in '+className);
end;

end.

