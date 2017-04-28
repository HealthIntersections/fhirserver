unit FHIRServerContext;

interface

uses
  SysUtils, Classes, Generics.Collections, kCritSct,
  AdvObjects, AdvGenerics, AdvStringMatches,
  FHIRTypes, FHIRResources, FHIRConstants, FHIRIndexManagers,
  FHIRValidator, ServerValidator, SCIMServer, FHIRStorageService, ServerUtilities, TerminologyServer;

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

  TFHIRServerContext = class (TAdvObject)
  private
    FStorage : TFHIRStorageService;
    FQuestionnaireCache: TQuestionnaireCache;
    FBases: TStringList;
    FResConfig: TAdvMap<TFHIRResourceConfig>;
    FSCIMServer: TSCIMServer;
    FValidatorContext : TFHIRServerWorkerContext;
    FValidator: TFHIRValidator;
    FTerminologyServer: TTerminologyServer;
    FIndexes : TFHIRIndexInformation;

    FOwnerName: String;
    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;
    FForLoad : boolean;

    procedure SetSCIMServer(const Value: TSCIMServer);
    procedure SetTerminologyServer(const Value: TTerminologyServer);
  public
    Constructor Create(storage : TFHIRStorageService);
    Destructor Destroy; override;
    Function Link : TFHIRServerContext; overload;

    property QuestionnaireCache: TQuestionnaireCache read FQuestionnaireCache;
    property Storage : TFHIRStorageService read FStorage;
    Property Bases: TStringList read FBases;
    Property ResConfig: TAdvMap<TFHIRResourceConfig> read FResConfig;
    Property ValidatorContext : TFHIRServerWorkerContext read FValidatorContext;
    Property Validator: TFHIRValidator read FValidator;
    Property TerminologyServer: TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    property Indexes : TFHIRIndexInformation read FIndexes;

    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure write FFormalURLSecure;
    property FormalURLPlainOpen: String read FFormalURLPlainOpen write FFormalURLPlainOpen;
    property FormalURLSecureOpen: String read FFormalURLSecureOpen write FFormalURLSecureOpen;
    property FormalURLSecureClosed: String read FFormalURLSecureClosed write FFormalURLSecureClosed;
    Property OwnerName: String read FOwnerName write FOwnerName;
    property ForLoad : boolean read FForLoad write FForLoad;
    property SCIMServer : TSCIMServer read FSCIMServer write SetSCIMServer;
  end;



implementation

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

{ TFHIRServerContext }

constructor TFHIRServerContext.Create(storage: TFHIRStorageService);
var
  a: TFHIRResourceType;
  cfg : TFHIRResourceConfig;
begin
  Inherited Create;
  FIndexes := TFHIRIndexInformation.create;
  FStorage := storage;
  FQuestionnaireCache := TQuestionnaireCache.Create;
  FBases := TStringList.Create;
  FBases.add('http://localhost/');
  FResConfig := TAdvMap<TFHIRResourceConfig>.create;
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
  begin
    cfg := TFHIRResourceConfig.Create;
    cfg.name := CODES_TFHIRResourceType[a];
    cfg.Supported := false;
    FResConfig.Add(cfg.name,  cfg);
  end;
  FValidatorContext := TFHIRServerWorkerContext.Create;
  FValidator := TFHIRValidator.Create(FValidatorContext.link);
end;

destructor TFHIRServerContext.Destroy;
begin
  FIndexes.free;
  FTerminologyServer.Free;
  FValidator.free;
  FValidatorContext.Free;
  FResConfig.free;
  FQuestionnaireCache.free;
  FStorage.Free;
  FBases.free;
  FSCIMServer.Free;
  inherited;
end;


function TFHIRServerContext.Link: TFHIRServerContext;
begin
  result := TFHIRServerContext(inherited Link);
end;

procedure TFHIRServerContext.SetSCIMServer(const Value: TSCIMServer);
begin
  FSCIMServer.Free;
  FSCIMServer := Value;
end;


procedure TFHIRServerContext.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

end.
