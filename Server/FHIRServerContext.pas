unit FHIRServerContext;

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
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Generics.Collections,
  OIDSupport, kCritSct, FileSupport, SystemSupport,
  AdvObjects, AdvGenerics, AdvStringMatches,
  FHIRTypes, FHIRResources, FHIRConstants, FHIRIndexManagers, FHIRUtilities, FHIRFactory,
  FHIRValidator, ServerValidator, FHIRUserProvider, FHIRStorageService, ServerUtilities, TerminologyServer,
  FHIRSubscriptionManager, FHIRSessionManager, FHIRTagManager, JWTService, ClientApplicationVerifier,
  ApplicationCache, ServerJavascriptHost, JavaBridge;

Const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'urn:oauth:';

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
    FLock: TCriticalSection;
    FStorage : TFHIRStorageService;
    FQuestionnaireCache: TQuestionnaireCache;
    FBases: TStringList;
    FResConfig: TAdvMap<TFHIRResourceConfig>;
    FUserProvider : TFHIRUserProvider;
    FValidatorContext : TFHIRServerWorkerContext;
    FValidator: TFHIRValidator;
    FTerminologyServer: TTerminologyServer;
    FIndexes : TFHIRIndexInformation;
    FSubscriptionManager : TSubscriptionManager;
    FSessionManager : TFHIRSessionManager;
    FTagManager : TFHIRTagManager;
    FNamingSystems : TAdvMap<TFHIRNamingSystem>;
    FApplicationCache : TApplicationCache;
    FEventScriptRegistry : TEventScriptRegistry;
    FFactory: TFHIRFactory;
    {$IFNDEF FHIR2}
    FMaps : TAdvMap<TFHIRStructureMap>;
    FJavaServices: TJavaLibraryWrapper;
    {$ENDIF}

    FOwnerName: String;
    FSystemId: String;
    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;

    FForLoad : boolean;
    FSupportTransaction: Boolean;
    FDoAudit: Boolean;
    FSupportSystemHistory: Boolean;
    FValidate: Boolean;
    FClientApplicationVerifier: TClientApplicationVerifier;
    FJWTServices: TJWTServices;
    FTaskFolder: String;
    FRunNumber: integer;
    FRequestId : integer;

    procedure SetFactory(const Value: TFHIRFactory);
    procedure SetUserProvider(const Value: TFHIRUserProvider);
    procedure SetTerminologyServer(const Value: TTerminologyServer);
    procedure SetSubscriptionManager(const Value: TSubscriptionManager);
    procedure SetClientApplicationVerifier(const Value: TClientApplicationVerifier);
    procedure SetJWTServices(const Value: TJWTServices);
    {$IFNDEF FHIR2}
    procedure SetJavaServices(const Value: TJavaLibraryWrapper);
    {$ENDIF}
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
    property SubscriptionManager : TSubscriptionManager read FSubscriptionManager write SetSubscriptionManager;
    property SessionManager : TFHIRSessionManager read FSessionManager;
    property TagManager : TFHIRTagManager read FTagManager;
    property UserProvider : TFHIRUserProvider read FUserProvider write SetUserProvider;
    property ClientApplicationVerifier : TClientApplicationVerifier read FClientApplicationVerifier write SetClientApplicationVerifier;
    property ApplicationCache : TApplicationCache read FApplicationCache;
    property EventScriptRegistry : TEventScriptRegistry read FEventScriptRegistry;
    property Factory : TFHIRFactory read FFactory write SetFactory;
    {$IFNDEF FHIR2}
    property JavaServices : TJavaLibraryWrapper read FJavaServices write SetJavaServices;
    {$ENDIF}

    property JWTServices : TJWTServices read FJWTServices write SetJWTServices;

    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure write FFormalURLSecure;
    property FormalURLPlainOpen: String read FFormalURLPlainOpen write FFormalURLPlainOpen;
    property FormalURLSecureOpen: String read FFormalURLSecureOpen write FFormalURLSecureOpen;
    property FormalURLSecureClosed: String read FFormalURLSecureClosed write FFormalURLSecureClosed;
    Property OwnerName: String read FOwnerName write FOwnerName;
    property SystemId: String read FSystemId write FSystemId;

    property ForLoad : boolean read FForLoad write FForLoad;
    property TaskFolder : String read FTaskFolder;
    Property SupportTransaction: Boolean read FSupportTransaction write FSupportTransaction;
    Property DoAudit: Boolean read FDoAudit write FDoAudit;
    Property SupportSystemHistory: Boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property Validate: Boolean read FValidate write FValidate;
    Property RunNumber : integer read FRunNumber write FRunNumber;

    function oid2Uri(oid : String) : String;
    procedure seeNamingSystem(key : integer; ns : TFhirNamingSystem);
    {$IFNDEF FHIR2}
    procedure seeMap(map : TFHIRStructureMap);
    function getMaps : TAdvMap<TFHIRStructureMap>;
    {$ENDIF}
    function nextRequestId : string;
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
  FLock := TCriticalSection.Create('ServerContext');
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
    cfg.enum := a;
    FResConfig.Add(cfg.name,  cfg);
  end;
  FValidatorContext := TFHIRServerWorkerContext.Create;
  FValidator := TFHIRValidator.Create(FValidatorContext.link);
  FSessionManager := TFHIRSessionManager.Create(self);
  FTagManager := TFHIRTagManager.create;
  FNamingSystems := TAdvMap<TFHIRNamingSystem>.create;
  FApplicationCache := TApplicationCache.create;
  FEventScriptRegistry := TEventScriptRegistry.Create;

  {$IFNDEF FHIR2}
  FMaps := TAdvMap<TFHIRStructureMap>.create;
  {$ENDIF}
  if DirectoryExists('c:\temp') then
    FTaskFolder := 'c:\temp\fhir-server-tasks'
  else
    FTaskFolder := path([SystemTemp, 'fhir-server-tasks']);
  ForceFolder(FTaskFolder);
end;

destructor TFHIRServerContext.Destroy;
begin
  {$IFNDEF FHIR2}
  FMaps.Free;
  FJavaServices.Free;
  {$ENDIF}
  FEventScriptRegistry.Free;
  FApplicationCache.Free;
  FJWTServices.Free;
  FClientApplicationVerifier.Free;
  FNamingSystems.Free;
  FTagManager.Free;
  FSessionManager.CloseAll;
  FSessionManager.Free;
  FSubscriptionManager.Free;
  FIndexes.free;
  FTerminologyServer.Free;
  FStorage.Free;
  FQuestionnaireCache.free;
  UserProvider.Free;
  FFactory.free;

  FValidator.free;
//  if FValidatorContext.AdvObjectReferenceCount > 0 then
//    raise Exception.Create('There are still '+inttostr(FValidatorContext.AdvObjectReferenceCount)+' uses of the WorkerContext live');

  FValidatorContext.Free;
  FResConfig.free;
  FBases.free;
  FLock.Free;
  inherited;
end;


function TFHIRServerContext.Link: TFHIRServerContext;
begin
  result := TFHIRServerContext(inherited Link);
end;

function TFHIRServerContext.nextRequestId: string;
var
  v : integer;
begin
  v := InterlockedIncrement(FRequestId);
  result := inttostr(FRunNumber)+'-'+inttostr(v);
end;

{$IFNDEF FHIR2}
procedure TFHIRServerContext.seeMap(map: TFHIRStructureMap);
begin
  FLock.Lock;
  try
    FMaps.AddOrSetValue(map.url, map.Link);
  finally
    FLock.Unlock;
  end;
end;
    {$ENDIF}

procedure TFHIRServerContext.seeNamingSystem(key : integer; ns: TFhirNamingSystem);
begin
  FLock.Lock;
  try
    FNamingSystems.AddOrSetValue(inttostr(key), ns.Link);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerContext.SetUserProvider(const Value: TFHIRUserProvider);
begin
  FUserProvider.Free;
  FUserProvider := Value;
end;



procedure TFHIRServerContext.SetClientApplicationVerifier(const Value: TClientApplicationVerifier);
begin
  FClientApplicationVerifier.Free;
  FClientApplicationVerifier := Value;
end;

{$IFNDEF FHIR2}
procedure TFHIRServerContext.SetJavaServices(const Value: TJavaLibraryWrapper);
begin
  FJavaServices.Free;
  FJavaServices := Value;
end;
{$ENDIF}

procedure TFHIRServerContext.SetJWTServices(const Value: TJWTServices);
begin
  FJWTServices.Free;
  FJWTServices := Value;
end;

procedure TFHIRServerContext.SetSubscriptionManager(const Value: TSubscriptionManager);
begin
  FSubscriptionManager.Free;
  FSubscriptionManager := Value;
end;

procedure TFHIRServerContext.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
  ValidatorContext.TerminologyServer := Value.Link;
end;

procedure TFHIRServerContext.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
end;


{$IFNDEF FHIR2}
function TFHIRServerContext.getMaps: TAdvMap<TFHIRStructureMap>;
var
  s : String;
begin
  FLock.Lock;
  try
    result := TAdvMap<TFHIRStructureMap>.create;
    for s in FMaps.Keys do
      result.Add(s, FMaps[s].Link);
  finally
    FLock.Unlock;
  end;
end;
{$ENDIF}

function TFHIRServerContext.oid2Uri(oid: String): String;
var
  ns : TFHIRNamingSystem;
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
