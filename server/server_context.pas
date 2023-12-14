unit server_context;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_threads, fsl_utilities, fsl_collections, fsl_logging, fsl_npm_cache, fsl_i18n,
  fhir_objects, fhir_factory, fhir_common, fhir_validator, fdb_manager, fhir_uris,
  fhir_indexing,
  indexing, user_manager, storage, utilities, tx_server,
  subscriptions, session_manager, tag_manager, jwt, server_factory, consent_engine, server_constants,
  client_cache_manager;

Const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'urn:oauth:';

Type
  TQuestionnaireCache = class(TFslObject)
  private
    FLock: TFslLock;
    FQuestionnaires: TFslMap<TFhirQuestionnaireW>;
    FForms: TFslStringMatch;
    FValueSetDependencies: TDictionary<String, TList<string>>;
    FCaching: boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure putQuestionnaire(rtype: string; id: String; q: TFhirQuestionnaireW; dependencies: TList<String>);
    procedure putForm(rtype: string; id: String; form: String; dependencies: TList<String>);

    function getQuestionnaire(rtype: string; id: String) : TFhirQuestionnaireW;
    function getForm(rtype: string; id: String): String;

    procedure clear(rtype: string; id: String); overload;
    procedure clearVS(id: string);
    procedure clear; overload;
    function cacheSize(magic : integer) : UInt64;
    procedure clearCache;
    property caching : boolean read FCaching write FCaching;
  end;

  TFHIRServerContext = class;

  TGetNamedContextEvent = function (Sender : TObject; name : String) : TFHIRServerContext of object;

  { TFHIRServerContext }

  TFHIRServerContext = class (TFslObject)
  private
    FI18nSupport: TI18nSupport;
    FLock: TFslLock;
    FName : string;
    FStorage : TFHIRStorageService;
    FQuestionnaireCache: TQuestionnaireCache;
    FResConfig: TFslMap<TFHIRResourceConfig>;
    FUserProvider : TFHIRUserProvider;
    FValidatorContext : TFHIRWorkerContextWithFactory;
    FValidator: TFHIRValidatorV;
    FTerminologyServer: TTerminologyServer;
    FIndexes : TFHIRIndexInformation;
    FSubscriptionManager : TSubscriptionManager;
    FSessionManager : TFHIRSessionManager;
    FTagManager : TFHIRTagManager;
    FNamingSystems : TFslMap<TFHIRNamingSystemW>;
    FMaps : TFslMap<TFHIRStructureMapW>;
    FSystemId: String;

    FFormalURLPlain: String;
    FFormalURLSecure: String;

    FSupportTransaction: Boolean;
    FDoAudit: Boolean;
    FSupportSystemHistory: Boolean;
    FValidate: Boolean;
    FJWTServices: TJWTServices;
    FTaskFolder: String;
    FGlobals: TFHIRServerSettings;
    FServerFactory : TFHIRServerFactory;
    FConsentEngine: TFHIRConsentEngine;
    FClientCacheManager: TClientCacheManager;
    FOnGetNamedContext : TGetNamedContextEvent;
    FPcm: TFHIRPackageManager;

    procedure updateSettings;
    procedure SetI18nSupport(AValue: TI18nSupport);
    procedure SetUserProvider(const Value: TFHIRUserProvider);
    procedure SetTerminologyServer(const Value: TTerminologyServer);
    procedure SetSubscriptionManager(const Value: TSubscriptionManager);
    procedure SetJWTServices(const Value: TJWTServices);
    function GetFactory: TFHIRFactory;
    procedure SetGlobals(const Value: TFHIRServerSettings);

    procedure SetConsentEngine(const Value: TFHIRConsentEngine);
    procedure SetValidate(const Value: Boolean);

    procedure SetClientCacheManager(const Value: TClientCacheManager);
    procedure SetPcm(const Value: TFHIRPackageManager);
  public
    constructor Create(name : String; storage : TFHIRStorageService; serverFactory : TFHIRServerFactory; pcm : TFHIRPackageManager);
    destructor Destroy; override;
    Function Link : TFHIRServerContext; overload;

    property Name : string read FName;
    property pcm : TFHIRPackageManager read FPcm;
    property Globals : TFHIRServerSettings read FGlobals write SetGlobals;
    property DatabaseId: String read FSystemId write FSystemId;
    property QuestionnaireCache: TQuestionnaireCache read FQuestionnaireCache;
    property Storage : TFHIRStorageService read FStorage;
    Property ResConfig: TFslMap<TFHIRResourceConfig> read FResConfig;
    Property ValidatorContext : TFHIRWorkerContextWithFactory read FValidatorContext;
    Property Validator: TFHIRValidatorV read FValidator;
    Property TerminologyServer: TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    property Indexes : TFHIRIndexInformation read FIndexes;
    property SubscriptionManager : TSubscriptionManager read FSubscriptionManager write SetSubscriptionManager;
    property SessionManager : TFHIRSessionManager read FSessionManager;
    property ConsentEngine : TFHIRConsentEngine read FConsentEngine write SetConsentEngine;
    property TagManager : TFHIRTagManager read FTagManager;
    property UserProvider : TFHIRUserProvider read FUserProvider write SetUserProvider;
    property Factory : TFHIRFactory read GetFactory;
    property ServerFactory : TFHIRServerFactory read FServerFactory;
    property ClientCacheManager: TClientCacheManager read FClientCacheManager write SetClientCacheManager;
    property i18nSupport : TI18nSupport read FI18nSupport write SetI18nSupport;

    property JWTServices : TJWTServices read FJWTServices write SetJWTServices;

    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure write FFormalURLSecure;
    property TaskFolder : String read FTaskFolder;
    Property SupportTransaction: Boolean read FSupportTransaction write FSupportTransaction;
    Property DoAudit: Boolean read FDoAudit write FDoAudit;
    Property SupportSystemHistory: Boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property Validate: Boolean read FValidate write SetValidate;

    function oid2Uri(oid : String) : String;
    procedure seeNamingSystem(key : integer; ns : TFhirNamingSystemW);
    procedure seeMap(map : TFHIRStructureMapW);
    function getMaps : TFslMap<TFHIRStructureMapW>;

    procedure DoneLoading(conn : TFDBConnection);

    function cacheSize(magic : integer) : UInt64;
    procedure clearCache;
    procedure sweepCache;
    procedure SetCacheStatus(status : boolean);
    procedure getCacheInfo(ci: TCacheInformation);
    procedure UnLoad;

    property OnGetNamedContext : TGetNamedContextEvent read FOnGetNamedContext write FOnGetNamedContext;
  end;

implementation

{ TQuestionnaireCache }

constructor TQuestionnaireCache.Create;
begin
  inherited;
  FLock := TFslLock.Create('TQuestionnaireCache');
  FQuestionnaires := TFslMap<TFhirQuestionnaireW>.Create('questionnaires');
  FForms := TFslStringMatch.Create;
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

function TQuestionnaireCache.cacheSize(magic : integer) : UInt64;
begin
  FLock.Lock;
  try
    result := FQuestionnaires.sizeInBytes(magic) + FForms.sizeInBytes(magic);
  finally
    FLock.Unlock;
  end;
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

procedure TQuestionnaireCache.clearCache;
begin
  FLock.Lock;
  try
    FQuestionnaires.Clear;
    FForms.Clear;
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

procedure TQuestionnaireCache.clear(rtype: String; id: String);
var
  s: String;
begin
  s := rtype + '/' + id;
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

function TQuestionnaireCache.getForm(rtype: String;
  id: String): String;
begin
  if FCaching then
  begin
    FLock.Lock('getForm');
    try
      result := FForms[rtype + '/' + id];
    finally
      FLock.Unlock;
    end;
  end
  else
    result := '';
end;

function TQuestionnaireCache.getQuestionnaire(rtype: String;
  id: String): TFhirQuestionnaireW;
begin
  if FCaching then
  begin
    FLock.Lock('getQuestionnaire');
    try
      result := FQuestionnaires[rtype + '/' + id]
        .Link as TFhirQuestionnaireW;
      // comes off linked - must happen inside the lock
    finally
      FLock.Unlock;
    end;
  end
  else
    result := nil;
end;

procedure TQuestionnaireCache.putForm(rtype: String;
  id, form: String; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  if FCaching then
  begin
    FLock.Lock('putForm');
    try
      FForms[rtype + '/' + id] := form;
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
end;

procedure TQuestionnaireCache.putQuestionnaire(rtype: String;
  id: String; q: TFhirQuestionnaireW; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  if FCaching then
  begin
    FLock.Lock('putQuestionnaire');
    try
      FQuestionnaires[rtype + '/' + id] := q.Link;
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
end;

{ TFHIRServerContext }

function TFHIRServerContext.cacheSize(magic : integer): UInt64;
begin
  result := FQuestionnaireCache.cacheSize(magic) + FClientCacheManager.cacheSize(magic);
  if FTerminologyServer <> nil then
    result := result + FTerminologyServer.cacheSize(magic);
end;

procedure TFHIRServerContext.clearCache;
begin
  FQuestionnaireCache.clearCache;
  if FTerminologyServer <> nil then
    FTerminologyServer.clearCache;
  FClientCacheManager.clearCache;
end;

procedure TFHIRServerContext.sweepCache;
begin
  if FTerminologyServer <> nil then
    FTerminologyServer.sweep;
  FClientCacheManager.sweep;
end;

constructor TFHIRServerContext.Create(name : String; storage: TFHIRStorageService; serverFactory : TFHIRServerFactory; pcm : TFHIRPackageManager);
var
  a: String;
  cfg : TFHIRResourceConfig;
begin
  Inherited Create;
  FName := name;
  FLock := TFslLock.Create('ServerContext');
  FStorage := storage;
  FPcm := pcm;
  FServerFactory := serverFactory;
  FQuestionnaireCache := TQuestionnaireCache.Create;
  FResConfig := TFslMap<TFHIRResourceConfig>.Create('res.config');
  for a in Factory.ResourceNames do
  begin
    cfg := TFHIRResourceConfig.Create;
    cfg.name := a;
    cfg.Supported := false;
    FResConfig.Add(cfg.name, cfg);
  end;
  cfg := TFHIRResourceConfig.Create;
  cfg.name := '';
  cfg.Supported := false;
  FResConfig.Add(cfg.name, cfg);
  FValidator := serverFactory.makeValidator(pcm);
  FValidatorContext := FValidator.Context.link;
  FIndexes := TFHIRIndexInformation.Create(FValidatorContext.factory.link, ServerFactory.link);
  FSessionManager := TFHIRSessionManager.Create(Globals.link, self);
  FTagManager := TFHIRTagManager.Create(storage.Factory.link);
  FNamingSystems := TFslMap<TFHIRNamingSystemW>.Create('naming');
  FConsentEngine := TFHIRNullConsentEngine.Create(storage.Factory.link);
  FClientCacheManager := TClientCacheManager.Create;

  FMaps := TFslMap<TFHIRStructureMapW>.Create('tx.maps');
  FTaskFolder := FilePath(['[tmp]', 'fhir-server-tasks']);
  ForceFolder(FTaskFolder);
end;

destructor TFHIRServerContext.Destroy;
begin
  FPcm.free;
  FConsentEngine.free;
  FGlobals.free;
  FMaps.free;
  FJWTServices.free;
  FNamingSystems.free;
  FTagManager.free;
  FSessionManager.CloseAll;
  FSessionManager.free;
  FSubscriptionManager.free;
  FIndexes.free;
  FStorage.free;
  FQuestionnaireCache.free;
  UserProvider.free;
  FServerFactory.free;
  FTerminologyServer.free;
  FClientCacheManager.free;
  FI18nSupport.free;

  FValidatorContext.free;
  FValidator.free;

  FResConfig.free;
  FLock.free;
  inherited;
end;


procedure TFHIRServerContext.DoneLoading(conn : TFDBConnection);
begin
  FSubscriptionManager.DoneLoading(conn);
end;

procedure TFHIRServerContext.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
  ci.Add('FQuestionnaireCache', FQuestionnaireCache.sizeInBytes(ci.magic));
  ci.Add('FClientCacheManager', FClientCacheManager.sizeInBytes(ci.magic));
end;

procedure TFHIRServerContext.UnLoad;
begin
  if FStorage <> nil then
    FStorage.UnLoad;
  FQuestionnaireCache.clearCache;
  FValidatorContext.UnLoad;
  FValidator.Unload;
  FTerminologyServer.UnLoad;
  if FSubscriptionManager <> nil then
    FSubscriptionManager.UnLoad;
  FSessionManager.Clear;
  FNamingSystems.Clear;
  FMaps.Clear;
  FConsentEngine.UnLoad;
  FClientCacheManager.clearCache;
  FPcm.UnLoad;
end;

function TFHIRServerContext.GetFactory: TFHIRFactory;
begin
  result := Storage.Factory;
end;

function TFHIRServerContext.Link: TFHIRServerContext;
begin
  result := TFHIRServerContext(inherited Link);
end;

procedure TFHIRServerContext.updateSettings;
begin
  if (FGlobals <> nil) and (FClientCacheManager <> nil) then
    FClientCacheManager.cacheDwellTime := FGlobals.Ini.service['cache-time'].readAsInt(DEFAULT_DWELL_TIME_MIN) / (24*60);
  if (FGlobals <> nil) and (FTerminologyServer <> nil) then
    FTerminologyServer.cacheDwellTime := FGlobals.Ini.service['cache-time'].readAsInt(DEFAULT_DWELL_TIME_MIN) / (24*60);
end;

procedure TFHIRServerContext.seeMap(map: TFHIRStructureMapW);
begin
  FLock.Lock;
  try
    FMaps.AddOrSetValue(map.url, map);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerContext.seeNamingSystem(key : integer; ns: TFhirNamingSystemW);
begin
  FLock.Lock;
  try
    FNamingSystems.AddOrSetValue(inttostr(key), ns);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerContext.SetUserProvider(const Value: TFHIRUserProvider);
begin
  FUserProvider.free;
  FUserProvider := Value;
end;

procedure TFHIRServerContext.SetI18nSupport(AValue: TI18nSupport);
begin
  FI18nSupport.free;
  FI18nSupport := AValue;
end;

procedure TFHIRServerContext.SetValidate(const Value: Boolean);
begin
  if (Value <> false) then
    raise EFslException.Create('Validation is not currently supported');
  FValidate := Value;
end;

procedure TFHIRServerContext.SetJWTServices(const Value: TJWTServices);
begin
  FJWTServices.free;
  FJWTServices := Value;
end;

procedure TFHIRServerContext.SetPcm(const Value: TFHIRPackageManager);
begin
  FPcm.free;
  FPcm := Value;
end;

procedure TFHIRServerContext.SetCacheStatus(status: boolean);
begin
  FQuestionnaireCache.caching := status;
  if FTerminologyServer <> nil then
    FTerminologyServer.SetCacheStatus(status);
end;

procedure TFHIRServerContext.SetClientCacheManager(const Value: TClientCacheManager);
begin
  FClientCacheManager.free;
  FClientCacheManager := Value;
  updateSettings;
end;

procedure TFHIRServerContext.SetConsentEngine(const Value: TFHIRConsentEngine);
begin
  FConsentEngine.free;
  FConsentEngine := Value;
end;

procedure TFHIRServerContext.SetGlobals(const Value: TFHIRServerSettings);
begin
  FGlobals.free;
  FGlobals := Value;
  updateSettings;
end;

procedure TFHIRServerContext.SetSubscriptionManager(const Value: TSubscriptionManager);
begin
  FSubscriptionManager.free;
  FSubscriptionManager := Value;
end;

procedure TFHIRServerContext.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.free;
  FTerminologyServer := Value;
  ServerFactory.setTerminologyServer(FValidatorContext, value.link);
  updateSettings;
end;

function TFHIRServerContext.getMaps: TFslMap<TFHIRStructureMapW>;
var
  s : String;
begin
  FLock.Lock;
  try
    result := TFslMap<TFHIRStructureMapW>.Create('maps');
    for s in FMaps.Keys do
      result.Add(s, FMaps[s].Link);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRServerContext.oid2Uri(oid: String): String;
var
  ns : TFHIRNamingSystemW;
begin
  result := '';
  FLock.Lock;
  try
    result := UriForKnownOid(oid);
    if (result = '') then
    begin
      for ns in FNamingSystems.Values do
      begin
        if ns.hasOid(oid) then
        begin
          result := ns.getUri;
          if (result <> '') then
            exit;
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

end.

