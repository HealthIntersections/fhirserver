unit FHIR.Server.Context;

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
  {$IFDEF MACOS} FHIR.Support.Osx, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Utilities, FHIR.Support.Collections,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common, FHIR.Base.Validator,
  FHIR.Tools.Indexing,
  FHIR.Server.Indexing, FHIR.Server.UserMgr, FHIR.Server.Storage, FHIR.Server.Utilities, FHIR.Tx.Server,
  FHIR.Server.Subscriptions, FHIR.Server.SessionMgr, FHIR.Server.TagMgr, FHIR.Server.Jwt, FHIR.Server.Factory
  {$IFNDEF NO_JS}, FHIR.Server.Javascript {$ENDIF};

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
  end;

  TFHIRServerContext = class abstract (TFslObject)
  private
    FLock: TFslLock;
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
    {$IFNDEF NO_JS}
    FEventScriptRegistry : TEventScriptRegistry;
    {$ENDIF}
    FMaps : TFslMap<TFHIRStructureMapW>;
    FSystemId: String;

    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;

    FSupportTransaction: Boolean;
    FDoAudit: Boolean;
    FSupportSystemHistory: Boolean;
    FValidate: Boolean;
    FJWTServices: TJWTServices;
    FTaskFolder: String;
    FGlobals: TFHIRServerSettings;
    FServerFactory : TFHIRServerFactory;

    procedure SetUserProvider(const Value: TFHIRUserProvider);
    procedure SetTerminologyServer(const Value: TTerminologyServer);
    procedure SetSubscriptionManager(const Value: TSubscriptionManager);
    procedure SetJWTServices(const Value: TJWTServices);
    function GetFactory: TFHIRFactory;
    procedure SetGlobals(const Value: TFHIRServerSettings);
  public
    constructor Create(storage : TFHIRStorageService; serverFactory : TFHIRServerFactory);
    destructor Destroy; override;
    Function Link : TFHIRServerContext; overload;

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
    property TagManager : TFHIRTagManager read FTagManager;
    property UserProvider : TFHIRUserProvider read FUserProvider write SetUserProvider;
    {$IFNDEF NO_JS}
    property EventScriptRegistry : TEventScriptRegistry read FEventScriptRegistry;
    {$ENDIF}
    property Factory : TFHIRFactory read GetFactory;
    property ServerFactory : TFHIRServerFactory read FServerFactory;

    property JWTServices : TJWTServices read FJWTServices write SetJWTServices;

    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure write FFormalURLSecure;
    property FormalURLPlainOpen: String read FFormalURLPlainOpen write FFormalURLPlainOpen;
    property FormalURLSecureOpen: String read FFormalURLSecureOpen write FFormalURLSecureOpen;
    property FormalURLSecureClosed: String read FFormalURLSecureClosed write FFormalURLSecureClosed;
    property TaskFolder : String read FTaskFolder;
    Property SupportTransaction: Boolean read FSupportTransaction write FSupportTransaction;
    Property DoAudit: Boolean read FDoAudit write FDoAudit;
    Property SupportSystemHistory: Boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property Validate: Boolean read FValidate write FValidate;

    function oid2Uri(oid : String) : String;
    procedure seeNamingSystem(key : integer; ns : TFhirNamingSystemW);
    procedure seeMap(map : TFHIRStructureMapW);
    function getMaps : TFslMap<TFHIRStructureMapW>;
  end;

implementation

{ TQuestionnaireCache }

constructor TQuestionnaireCache.Create;
begin
  inherited;
  FLock := TFslLock.Create('TQuestionnaireCache');
  FQuestionnaires := TFslMap<TFhirQuestionnaireW>.Create;
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
  FLock.Lock('getForm');
  try
    result := FForms[rtype + '/' + id];
  finally
    FLock.Unlock;
  end;

end;

function TQuestionnaireCache.getQuestionnaire(rtype: String;
  id: String): TFhirQuestionnaireW;
begin
  FLock.Lock('getQuestionnaire');
  try
    result := FQuestionnaires[rtype + '/' + id]
      .Link as TFhirQuestionnaireW;
    // comes off linked - must happen inside the lock
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putForm(rtype: String;
  id, form: String; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
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

procedure TQuestionnaireCache.putQuestionnaire(rtype: String;
  id: String; q: TFhirQuestionnaireW; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
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

{ TFHIRServerContext }

constructor TFHIRServerContext.Create(storage: TFHIRStorageService; serverFactory : TFHIRServerFactory);
var
  a: String;
  cfg : TFHIRResourceConfig;
begin
  Inherited Create;
  FLock := TFslLock.Create('ServerContext');
  FStorage := storage;
  FServerFactory := serverFactory;
  FQuestionnaireCache := TQuestionnaireCache.Create;
  FResConfig := TFslMap<TFHIRResourceConfig>.create;
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
  FValidator := serverFactory.makeValidator;
  FValidatorContext := FValidator.Context.link;
  FIndexes := TFHIRIndexInformation.create(FValidatorContext.factory.link, ServerFactory.link);
  FSessionManager := TFHIRSessionManager.Create(Globals.link, self);
  FTagManager := TFHIRTagManager.create(storage.Factory.link);
  FNamingSystems := TFslMap<TFHIRNamingSystemW>.create;
  {$IFNDEF NO_JS}
  FEventScriptRegistry := TEventScriptRegistry.Create(storage.Factory.link);
  {$ENDIF}

  FMaps := TFslMap<TFHIRStructureMapW>.create;
  if DirectoryExists('c:\temp') then
    FTaskFolder := 'c:\temp\fhir-server-tasks'
  else
    FTaskFolder := path([SystemTemp, 'fhir-server-tasks']);
  ForceFolder(FTaskFolder);
end;

destructor TFHIRServerContext.Destroy;
begin
  FGlobals.Free;
  FMaps.Free;
  {$IFNDEF NO_JS}
  FEventScriptRegistry.Free;
  {$ENDIF}
  FJWTServices.Free;
  FNamingSystems.Free;
  FTagManager.Free;
  FSessionManager.CloseAll;
  FSessionManager.Free;
  FSubscriptionManager.Free;
  FIndexes.free;
  FStorage.Free;
  FQuestionnaireCache.free;
  UserProvider.Free;
  FServerFactory.Free;
  FTerminologyServer.Free;

  FValidatorContext.Free;
  FValidator.free;

  FResConfig.free;
  FLock.Free;
  inherited;
end;


function TFHIRServerContext.GetFactory: TFHIRFactory;
begin
  result := Storage.Factory;
end;

function TFHIRServerContext.Link: TFHIRServerContext;
begin
  result := TFHIRServerContext(inherited Link);
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
  FUserProvider.Free;
  FUserProvider := Value;
end;

procedure TFHIRServerContext.SetJWTServices(const Value: TJWTServices);
begin
  FJWTServices.Free;
  FJWTServices := Value;
end;

procedure TFHIRServerContext.SetGlobals(const Value: TFHIRServerSettings);
begin
  FGlobals.Free;
  FGlobals := Value;
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
  ServerFactory.setTerminologyServer(FValidatorContext, value.link);
end;

function TFHIRServerContext.getMaps: TFslMap<TFHIRStructureMapW>;
var
  s : String;
begin
  FLock.Lock;
  try
    result := TFslMap<TFHIRStructureMapW>.create;
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
