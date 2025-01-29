unit endpoint_full;

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
  SysUtils, StrUtils, Classes, IniFiles, Generics.Collections,
  {$IFDEF WINDOWS}
  ActiveX, ComObj,
  {$ENDIF}

  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer, IdTCPServer, IdContext, IdHTTP, IdCookie, IdZLibCompressorBase, IdSSL, IdSMTP,
  IdCompressorZLib, IdZLib, IdSchedulerOfThreadPool, IdGlobalProtocols, IdMessage, IdExplicitTLSClientServerBase, IdGlobal, fsl_websocket,
  IdOpenSSLIOHandlerServer, IdOpenSSLIOHandlerClient, IdOpenSSLVersion, IdOpenSSLX509,

  fsl_base, fsl_utilities, fsl_logging, fsl_threads, fsl_collections, fsl_stream, fsl_msxml, fsl_crypto, fsl_npm_cache, fsl_i18n, fsl_lang,
  ftx_ucum_services, fsl_http,
  fhir_objects,  fhir_factory, fhir_pathengine, fhir_parser, fhir_common, fhir_xhtml, fhir_cdshooks,

  fhir3_factory, fhir4_factory, fhir4b_factory, fhir5_factory,
  fhir3_context, fhir4_context, fhir4b_context, fhir5_context,
  fhir3_indexinfo, fhir4_indexinfo, fhir4b_indexinfo, fhir5_indexinfo,
  indexing_r3, indexing_r4, indexing_r4b, indexing_r5,
  subscriptions_r3, subscriptions_r4, subscriptions_r4b, subscriptions_r5,
  operations_r3, operations_r4, operations_r4b, operations_r5,
  fhir3_validator, fhir4_validator, fhir4b_validator, fhir5_validator,
  validator_r3, validator_r4, validator_r4b, validator_r5,
  fhir3_pathengine, fhir4_pathengine, fhir4b_pathengine, fhir5_pathengine,

  fdb_manager,
  fhir_indexing,
  tx_manager, tx_server, tx_unii,
  scim_server, telnet_server, session, security, jwt,
  database_installer, server_version, server_config, utilities, bundlebuilder, html_builder, server_constants,
  server_context, auth_manager,
  storage, database,  kernel_thread, server_stats,
  server_factory, indexing, subscriptions,
  web_base, endpoint, endpoint_storage;

Type
  TFullServerEndPoint = class;

  { TFullServerFactory }

  TFullServerFactory = class (TFHIRServerFactory)
  protected
    FLanguages : TIETFLanguageDefinitions;
    FVersion : TFHIRVersion;
  public
    constructor Create(Languages : TIETFLanguageDefinitions; version : TFHIRVersion);  
    destructor Destroy; override;

    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator(pcm : TFHIRPackageManager): TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeEngine(context : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TPackageLoader = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FBundle: TFHIRBundleW;
    FList : TStringList;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;

    property bundle : TFHIRBundleW read FBundle;
    procedure load(packageId : String; rType, id : String; stream : TStream);
  end;

  TFHIRServerThread = class (TFslThread)
  protected
    FServer: TFullServerEndPoint;
  public
    constructor Create(server: TFullServerEndPoint);
  end;

  TFhirServerEmailThread = class (TFHIRServerThread)
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
    procedure Finalise; override;
  end;

  TFhirServerSubscriptionThread = class(TFHIRServerThread)
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
    procedure Finalise; override;
  end;

  TFHIRWebServerPatientViewContext = class(TFslObject)
  private
    FContext : TFHIRServerContext;
    FCards: TFslList<TCDSHookCard>;
    FErrors: TStringList;
    FManager: TCDSHooksManager;
    procedure SetManager(const Value: TCDSHooksManager);
  public
    constructor Create(context : TFHIRServerContext);
    destructor Destroy; Override;
    property manager: TCDSHooksManager read FManager write SetManager;
    property Errors: TStringList read FErrors;
    property cards: TFslList<TCDSHookCard> read FCards;
  end;

  TFullServerWebEndPoint = class (TStorageWebEndpoint)
  private
    FEndPoint : TFullServerEndPoint;
    carry: TFslZipReader; // for uploading support
    carryName: String;
    function factory : TFHIRFactory;
//    function store : TFHIRNativeStorageService;
//    function terminologies : TCommonTerminologies;
    procedure GetPatients(details : TFslStringDictionary);
    function GetLaunchParameters(request: TIdHTTPRequestInfo; session : TFhirSession; launchContext : String; params : TAuthLaunchParamsSet) : TDictionary<String, String>;
    function GetResource(Session: TFHIRSession; rtype: String; langList : THTTPLanguageList; id, ver, op: String): TFhirResourceV;
    function HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function HandleWebEncounter(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function makingAudits : boolean; override;

    {$IFDEF WINDOWS}
    function transform1(resource: TFhirResourceV; langList : THTTPLanguageList; xslt: String; saveOnly: boolean): string;
    function HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    {$ENDIF}
//    Procedure HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
  protected

    Function BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; langList : THTTPLanguageList; host, rawHost, sBaseURL: String; Session: TFHIRSession; secure: boolean): String; override;
    Function BuildFhirUploadPage(langList : THTTPLanguageList; host, sBaseURL: String; aType: String; Session: TFHIRSession): String; override;
    Function BuildFhirAuthenticationPage(langList : THTTPLanguageList; host, path, logId, Msg: String; secure: boolean; params : String): String; override;
    function HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime; override;
    function HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    procedure GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String); override;
    Function ProcessZip(langList : THTTPLanguageList; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW; override;
    function DoSearch(Session: TFHIRSession; rtype: string; langList : THTTPLanguageList; params: String): TFHIRBundleW; override;
  public
    destructor Destroy; override;
    function link : TFullServerWebEndPoint; overload;
    function logId : string; override;
    function description : String; override;
  end;


  { TFullServerEndPoint }

  TFullServerEndPoint = class (TStorageEndPoint)
  private
    FSubscriptionThread: TFhirServerSubscriptionThread;
    FEmailThread: TFhirServerEmailThread;
    FStore : TFHIRNativeStorageService;
    FConfig : TFslStringDictionary;
    FStopping : boolean;
    FWeb : TFullServerWebEndPoint;
    FOnGetNamedContext: TGetNamedContextEvent;
    function version : TFHIRVersion;
    function makeFactory : TFHIRFactory;
    function makeServerFactory : TFHIRServerFactory;
    function makeScimServer(defaultRights : String; forInstall : boolean) : TSCIMServer;
    function makeStorage : TFHIRNativeStorageService;

    function lookupConfig(id : integer) : String;
    procedure checkDatabase;
    procedure Transaction(bundle: TFHIRBundleW; init: boolean; name, base: String; mode : TOperationMode; logLevel : TOperationLoggingLevel);
    procedure doGetBundleBuilder(request: TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
    destructor Destroy; override;
    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;

    property OnGetNamedContext : TGetNamedContextEvent read FOnGetNamedContext write FOnGetNamedContext;

    procedure Load; override;
    procedure Unload; override;
    procedure InstallDatabase(params : TCommandLineParameters); override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(installer : boolean; plist : String); override;
    procedure updateAdminPassword(pw : String); override;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override; 
    procedure recordStats(rec : TStatusRecord); override;
  end;

implementation

{ TFullServerFactory }

constructor TFullServerFactory.Create(Languages: TIETFLanguageDefinitions; version: TFHIRVersion);
begin
  inherited Create;
  FLanguages := Languages;
  FVersion := version;
end;

destructor TFullServerFactory.Destroy;
begin
  FLanguages.free;
  inherited Destroy;
end;

function TFullServerFactory.makeValidator(pcm : TFHIRPackageManager): TFHIRValidatorV;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(FLanguages.link, TFHIRFactoryR3.create, pcm.link));
    fhirVersionRelease4 : result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(FLanguages.link, TFHIRFactoryR4.create, pcm.link));
    fhirVersionRelease4B : result := TFHIRValidator4B.Create(TFHIRServerWorkerContextR4B.Create(FLanguages.link, TFHIRFactoryR4B.create, pcm.link));
    fhirVersionRelease5 : result := TFHIRValidator5.Create(TFHIRServerWorkerContextR5.Create(FLanguages.link, TFHIRFactoryR5.create, pcm.link));
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Validator');
  end;
end;

function TFullServerFactory.makeEngine(context: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRPathEngine3.Create(context as TFHIRWorkerContext3, ucum);
    fhirVersionRelease4 : result := TFHIRPathEngine4.Create(context as TFHIRWorkerContext4, ucum);
    fhirVersionRelease4B : result := TFHIRPathEngine4B.Create(context as TFHIRWorkerContext4B, ucum);
    fhirVersionRelease5 : result := TFHIRPathEngine5.Create(context as TFHIRWorkerContext5, ucum);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating FHIRPathEngine');
  end;
end;

function TFullServerFactory.makeIndexer : TFHIRIndexManager;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFhirIndexManager3.Create;
    fhirVersionRelease4 : result := TFhirIndexManager4.Create;
    fhirVersionRelease4B : result := TFhirIndexManager4B.Create;
    fhirVersionRelease5 : result := TFhirIndexManager5.Create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Indexes');
  end;
end;

function TFullServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRIndexBuilderR3.create;
    fhirVersionRelease4 : result := TFHIRIndexBuilderR4.create;
    fhirVersionRelease4B : result := TFHIRIndexBuilderR4B.create;
    fhirVersionRelease5 : result := TFHIRIndexBuilderR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating index information');
  end;
end;

function TFullServerFactory.makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager;
begin
  case FVersion of
    fhirVersionRelease3 : result := TSubscriptionManagerR3.Create(ServerContext);
    fhirVersionRelease4 : result := TSubscriptionManagerR4.Create(ServerContext);
    fhirVersionRelease4B : result := TSubscriptionManagerR4B.Create(ServerContext);
    fhirVersionRelease5 : result := TSubscriptionManagerR5.Create(ServerContext);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating subcription manager');
  end;
end;

procedure TFullServerFactory.setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer});
begin
  case FVersion of
    fhirVersionRelease3 : TFHIRServerWorkerContextR3(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4 : TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4B : TFHIRServerWorkerContextR4B(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease5 : TFHIRServerWorkerContextR5(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Setting Terminology Server');
  end;
end;

function factoryFactory(v : TFHIRVersion) : TFHIRFactory;
begin
  case v of
    fhirVersionRelease3: result := TFHIRFactoryR3.create;
    fhirVersionRelease4: result := TFHIRFactoryR4.create;
    fhirVersionRelease4B: result := TFHIRFactoryR4B.create;
    fhirVersionRelease5: result := TFHIRFactoryR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(v, 'creating factory');
  end;
end;

{ TPackageLoader }

constructor TPackageLoader.Create(factory: TFHIRFactory);
begin
  inherited Create;
  FFactory := factory;
  FBundle := FFactory.wrapBundle(FFactory.makeResource('Bundle'));
  FList := TStringList.create;
  FList.Sorted := false;
end;

destructor TPackageLoader.Destroy;
begin
  FList.free;
  FFactory.free;
  FBundle.free;
  inherited;
end;

procedure TPackageLoader.load(packageId : String; rType, id: String; stream: TStream);
var
  p : TFHIRParser;
  s : String;
  r : TFHIRResourceV;
begin
  p := FFactory.makeParser(nil, ffJson, nil);
  try
    s := rType + '|' + id;
    if FList.IndexOf(s) = -1 then
    begin
      r := p.parseResource(stream);
      r.SourcePackage := packageId;
      FBundle.addEntry.resource := r;
      Flist.Add(s);
    end;
  finally
    p.free;
  end;
end;

{ TFhirServerSubscriptionThread }

function TFhirServerSubscriptionThread.ThreadName: String;
begin
  result := 'Server Subscription Thread';
end;

procedure TFhirServerSubscriptionThread.Initialise;
begin
  TimePeriod := 1000;
end;

procedure TFhirServerSubscriptionThread.Execute;
begin
  FServer.FStore.ProcessSubscriptions;
end;

procedure TFhirServerSubscriptionThread.Finalise;
begin
  Logging.log('Close TFhirServerSubscriptionThread');
  try
    if not FServer.FStopping then
      FServer.FSubscriptionThread := nil;
  except
  end;
end;

{ TFhirServerEmailThread }

function TFhirServerEmailThread.ThreadName: String;
begin
  result := 'Server Email Thread';
end;

procedure TFhirServerEmailThread.Initialise;
begin
  Logging.log('Starting TFhirServerEmailThread');
  TimePeriod := 60000;
end;

procedure TFhirServerEmailThread.Execute;
begin
  setThreadStatus('processing Emails');
  FServer.FStore.ProcessEmails;
end;

procedure TFhirServerEmailThread.Finalise;
begin
  try
    if not FServer.FStopping then
      FServer.FEmailThread := nil;
  except
  end;
end;

{ TFHIRServerThread }

constructor TFHIRServerThread.Create(server: TFullServerEndPoint);
begin
  inherited Create();
  FServer := server;
end;

{ TFullServerEndPoint }

procedure TFullServerEndPoint.clearCache;
begin
  inherited;
  FStore.clearCache;
end;

procedure TFullServerEndPoint.SweepCaches;
begin
  inherited SweepCaches;
  FStore.Sweep;
end;

constructor TFullServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
begin
  inherited create(config, settings, db, common, pcm, i18n);
  FConfig := TFslStringDictionary.create;
end;

destructor TFullServerEndPoint.Destroy;
begin
  FConfig.free;
  FStore.free;
  inherited;
end;

function TFullServerEndPoint.summary: String;
begin
  result := 'Full Server for '+Config['version'].value+' using '+describeDatabase(Config);
end;

procedure TFullServerEndPoint.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
var
  b : TFHIRBundleW;
begin
  b := FServerContext.factory.wrapBundle(FServerContext.factory.makeResource('Bundle'));
  b.type_ := aType;
  builder := TFHIRBundleBuilderSimple.Create(FServerContext.factory.link, b);
end;

procedure TFullServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
  FStore.getCacheInfo(ci);
end;

procedure TFullServerEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
end;

procedure TFullServerEndPoint.Transaction(bundle: TFHIRBundleW; init: boolean; name, base: String; mode: TOperationMode; logLevel : TOperationLoggingLevel);
var
  req: TFHIRRequest;
  resp: TFHIRResponse;
  Context: TOperationContext;
  op: TFHIROperationEngine;
  t: UInt64;
  tt : TFslTimeTracker;
begin
  Context := TOperationContext.Create(mode, logLevel);
  try
    req := TFHIRRequest.Create(FServerContext.ValidatorContext.link, roUpload, FServerContext.Indexes.Compartments.link);
    try
      req.CommandType := fcmdTransaction;
      req.resource := bundle.Resource.link;
      req.resource.tags['duplicates'] := 'ignore';
      req.Session := FServerContext.SessionManager.CreateImplicitSession('n/a', FServerContext.Globals.OwnerName, 'Service Manager', systemInternal, true, false);
      req.Session.allowAll;
      req.LoadParams('');
      req.baseUrl := FServerContext.Globals.Bases[0];

      // GJSHost.registry := FContext.EventScriptRegistry.link;
      resp := TFHIRResponse.Create(FServerContext.ValidatorContext.link);
      tt := TFslTimeTracker.create;
      try
        t := GetTickCount64;
        req.internalRequestId := FServerContext.Globals.nextRequestId;
        op := FStore.createOperationContext(req.langList.link);
        try
          op.OnCreateBuilder := doGetBundleBuilder;
          op.Execute(Context, req, resp, tt);
          FStore.yield(op, nil);
        except
          on e: exception do
          begin
            FStore.yield(op, e);
            raise;
          end;
        end;
        Logging.log('Upload took '+inttostr((GetTickCount64 - t) div 1000)+' seconds');
      finally
        tt.free;
        resp.free;
      end;
    finally
      req.free;
    end;
  finally
    Context.free;
  end;
end;

function TFullServerEndPoint.version: TFHIRVersion;
var
  v : String;
begin
  v := Config['version'].value.toLower;
  if (v = 'r2') then
    result := fhirVersionRelease2
  else if (v = 'r3') then
    result := fhirVersionRelease3
  else if (v = 'r4') then
    result := fhirVersionRelease4
  else if (v = 'r4b') then
    result := fhirVersionRelease4B
  else if (v = 'r5') then
    result := fhirVersionRelease5
  else
    raise EFslException.Create('Unknown version "'+v+'"');
end;

function TFullServerEndPoint.makeFactory: TFHIRFactory;
begin
  case version of
    fhirVersionRelease3 : result := TFHIRFactoryR3.create;
    fhirVersionRelease4 : result := TFHIRFactoryR4.create;
    fhirVersionRelease4B : result := TFHIRFactoryR4B.create;
    fhirVersionRelease5 : result := TFHIRFactoryR5.create;
  else
    raise EFslException.Create('Unsupported Version');
  end;
end;

function TFullServerEndPoint.makeServerFactory: TFHIRServerFactory;
begin
  result := TFullServerFactory.Create(i18n.Languages.link, version);
end;

function TFullServerEndPoint.makeScimServer(defaultRights : String; forInstall : boolean) : TSCIMServer;
var
  salt : String;
begin
  if not Settings.Ini.admin.getProp('scim-salt', salt) then
    raise EFHIRException.create('You must define a scim salt in the config file');
  result := TSCIMServer.Create(Database.Link, salt, settings.Ini.web['host'].value, defaultRights, true);
end;

function TFullServerEndPoint.makeStorage : TFHIRNativeStorageService;
begin
  case version of
    fhirVersionRelease3: result := TFHIRNativeStorageServiceR3.create(Database.Link, TFHIRFactoryR3.Create);
    fhirVersionRelease4: result := TFHIRNativeStorageServiceR4.create(Database.Link, TFHIRFactoryR4.Create);
    fhirVersionRelease4B: result := TFHIRNativeStorageServiceR4B.create(Database.Link, TFHIRFactoryR4B.Create);
    fhirVersionRelease5: result := TFHIRNativeStorageServiceR5.create(Database.Link, TFHIRFactoryR5.Create);
  else
    raise EFslException.Create('Unsupported Version');
  end;
end;

function TFullServerWebEndPoint.makingAudits: boolean;
begin
  result := true;
end;

function TFullServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  inherited makeWebEndPoint(common);
  FWeb := TFullServerWebEndPoint.Create(Config.name, Config['path'].value, common, self);
  FWeb.FEndPoint := self;
  FServerContext.userProvider.OnProcessFile := FWeb.ReturnProcessedFile;
  FWeb.AuthServer := TAuth2Server.Create(makeFactory, Settings.ini, Common.Host, inttostr(Common.StatedSslPort), FWeb.pathNoSlash);
  FWeb.AuthServer.UserProvider := FServerContext.userProvider.Link;
  FWeb.AuthServer.ServerContext := FServerContext.Link;
  FWeb.AuthServer.EndPoint := FWeb.ClientAddress(true);
  FWeb.AuthServer.OnProcessFile := FWeb.ReturnProcessedFile;
  FWeb.AuthServer.OnGetPatients := FWeb.GetPatients;
  FWeb.AuthServer.OnProcessLaunchParams := FWeb.GetLaunchParameters;
  FWeb.AuthServer.Active := true;
  result := FWeb;
  WebEndPoint := result;
  FSubscriptionThread.Start;
  FEmailThread.Start;
  FServerContext.FormalURLPlain := 'http://'+Common.host+nonDefPort(Common.statedPort, 80)+WebEndPoint.PathNoSlash;
  FServerContext.FormalURLSecure := 'https://'+Common.host+nonDefPort(Common.statedSSLPort, 443)+WebEndPoint.PathNoSlash;
end;

procedure TFullServerEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
  FStore.SetCacheStatus(status);
end;

function TFullServerEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited CacheSize(magic);
  if FStore <> nil then
    result := result + FStore.cacheSize(magic);
end;

procedure TFullServerEndPoint.checkDatabase;
var
  ver : integer;
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  meta : TFDBMetaData;
begin
  conn := Database.GetConnection('check-version');
  try
    meta := conn.FetchMetaData;
    try
      if meta.HasTable('Config') then
      begin
        // db version check
        ver := conn.CountSQL('Select Value from Config where ConfigKey = '+inttostr(CONFIG_DATABASE_VERSION));
        if (ver <> ServerDBVersion) then
        begin
          Logging.log('Upgrade Database from version '+inttostr(ver)+' to '+inttostr(ServerDBVersion));
          dbi := TFHIRDatabaseInstaller.create(conn, makeFactory, makeServerFactory);
          try
            dbi.upgrade(ver);
          finally
            dbi.free;
          end;
        end;
      end;
      FConfig.Clear;
      conn.SQL := 'Select ConfigKey, Value from Config';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        FConfig.AddOrSetValue(Conn.ColStringByName['ConfigKey'], Conn.ColStringByName['Value']);
      conn.Terminate;
    finally
      meta.free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TFullServerEndPoint.Load;
var
  dr : String;
begin
  checkDatabase;

  dr := lookupConfig(CONFIG_DEFAULT_RIGHTS);
  FStore := makeStorage;
  FServerContext := TFHIRServerContext.Create(Config.name, FStore.link, makeServerFactory, FPcm.link);
  FStore.ServerContext := FServerContext;
  FServerContext.Globals := Settings.Link;
  FServerContext.userProvider := makeScimServer(dr, false);
  FServerContext.JWTServices := TJWTServices.Create;
  FServerContext.JWTServices.Cert := Settings.Ini.web['certname'].value;
  FServerContext.JWTServices.Password := Settings.Ini.web['password'].value;
  FServerContext.JWTServices.DatabaseId := FServerContext.DatabaseId;
  FServerContext.JWTServices.Host := Settings.Ini.web['host'].value;
  FServerContext.JWTServices.cardKey := TJWK.loadFromFile(Settings.Ini.web['card-key'].value);
  FServerContext.JWTServices.cardJWKSFile := Settings.Ini.web['card-jwks'].value;
  FServerContext.i18nSupport := I18n.link;
  //  FServerContext.JWTServices.JWKAddress := ?;

  FServerContext.TerminologyServer := TTerminologyServer.Create(Database.link, makeFactory, Terminologies.link, FServerContext.i18nSupport.link);
  FStore.Initialise;
  FServerContext.OnGetNamedContext := OnGetNamedContext;

  FSubscriptionThread := TFhirServerSubscriptionThread.Create(self);
  FEmailThread := TFhirServerEmailThread.Create(self);
end;

procedure TFullServerEndPoint.Unload;
begin
  FStopping := true;
  FSubscriptionThread.StopAndWait(100);
  FSubscriptionThread.free;
  FEmailThread.StopAndWait(100);
  FEmailThread.free;

  FServerContext.free;
  FServerContext := nil;
  FStore.free;
  FStore := nil;
  inherited;
end;

procedure TFullServerEndPoint.internalThread(callback: TFhirServerMaintenanceThreadTaskCallBack);
begin
  try
    if FStopping then exit;
    callback(self, 'Sweeping Sessions', 0);
    FStore.Sweep;
    if FStopping then exit;
    callback(self, 'Google Commit', 10);
    FWeb.Common.Google.commit;
    if FStopping then exit;
    callback(self, 'Checking Async Tasks', 20);
    FWeb.CheckAsyncTasks;
    if FStopping then exit;
    callback(self, 'Processing Observations', 30);
    FStore.ProcessObservations;
    if FStopping then exit;
    callback(self, 'Sweeping Client Cache', 50);
    FServerContext.ClientCacheManager.sweep;
    if FStopping then exit;
    callback(self, 'Build Terminology Indexes', 60);
    FServerContext.TerminologyServer.BuildIndexes(false);
  except
    on e : exception do
      Logging.log('Error in internal thread for '+Config.name+': '+e.Message);
  end;
end;

procedure TFullServerEndPoint.InstallDatabase(params : TCommandLineParameters);
var
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  rights, un, pw, em : String;
begin
  if not params.get('default-rights', rights) then
    raise EFslException.Create('Some default rights are required');
  if not params.get('username', un) then
    raise EFslException.Create('An Administrator username is required');
  if not params.get('password', pw) then
    raise EFslException.Create('An Administrator password is required');
  if not Settings.Ini.admin.getProp('email', em) then
    raise EFslException.Create('An Administrator email address is required in the configuration');

  Logging.log('Install database '+Database.DBDetails);
  Logging.log('Admin User = '+un);
  conn := Database.getConnection('install');
  try
    scim := makeScimServer(rights, true);
    try
      dbi := TFHIRDatabaseInstaller.Create(conn, makeFactory, makeServerFactory);
      try
        dbi.DefaultRights := rights;
        dbi.Bases.Add('http://healthintersections.com.au/fhir/argonaut');
        dbi.Bases.Add('http://hl7.org/fhir');
        dbi.Install(scim);
      finally
        dbi.free;
      end;
      scim.DefineAnonymousUser(conn);
      scim.DefineAdminUser(conn, un, pw, em);
    finally
      scim.free;
    end;
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
  Logging.log('Database Tables Created');
end;

procedure TFullServerEndPoint.UninstallDatabase;
var
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
begin
  Logging.log('Wiping database '+Database.DBDetails);
  conn := Database.GetConnection('uninstall');
  try
    dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
    try
      dbi.Uninstall;
    finally
      dbi.free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
  Logging.log('Wiping database Done');
end;

procedure TFullServerEndPoint.updateAdminPassword(pw : String);
var
  scim : TSCIMServer;
  conn : TFDBConnection;
begin
  if pw = '' then
    raise EFslException.Create('An Administrator password is required');
  conn := Database.getConnection('install');
  try
    scim := makeScimServer('', false);
    try
      Logging.log('fix admin password for '+Config.name);
      try
        scim.UpdateAdminUser(conn, '', pw, '');
        conn.Release;
        Logging.log('done');
      except
         on e:exception do
         begin
           Logging.log('Error: '+e.Message);
           conn.Error(e);
           recordStack(e);
           raise;
         end;
      end;
    finally
      scim.free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;



procedure TFullServerEndPoint.LoadPackages(installer : boolean; plist: String);
var
  p, pi, pv : String;
  pl : TArray<String>;
  ploader : TPackageLoader;
  li : TPackageLoadingInformation;
  loadList : TArray<String>;
  logLevel : TOperationLoggingLevel;
begin
  Logging.log('Getting ready to load Packages');
  Load;
  Logging.log('Load Packages');
  li := TPackageLoadingInformation.Create(PF_CONST[version]);
  try
    pl := plist.Split([',']);
    for p in pl do
    begin
      if p <> '' then
      begin
        StringSplit(p, '#', pi, pv);
        Logging.log('Check Package '+p+' installed');
        if not FServerContext.pcm.autoInstallPackage(pi, pv) then
          raise EFHIRException.create('Package '+p+' not found');
        Logging.log('ok. Loading to database');
        ploader := TPackageLoader.create(factoryFactory(version));
        try
          li.OnLoadEvent := ploader.load;
          loadList := ploader.FFactory.resourceNames;
          FServerContext.pcm.loadPackage(pi, pv, loadList, li);
          logLevel := ollHuman;
          if installer then
            logLevel := ollInstaller;
          Transaction(ploader.bundle, true, p, '', opmCmdLine, logLevel);
        finally
          ploader.free;
        end;
        Logging.log('Done');
      end;
    end;
  finally
    li.free;
  end;

  Logging.log('All Packages Loaded');
  Logging.log('Building Terminology Closure Tables');
  FServerContext.TerminologyServer.BuildIndexes(true);
  Logging.log('Finishing');
  Unload;
end;

function TFullServerEndPoint.lookupConfig(id: integer): String;
begin
  result := FConfig[inttostr(id)];
end;

{ TFHIRWebServerPatientViewContext }

constructor TFHIRWebServerPatientViewContext.Create(context : TFHIRServerContext);
begin
  inherited Create;
  FContext := context;
  FCards := TFslList<TCDSHookCard>.Create;
  FErrors := TStringList.Create;
end;

destructor TFHIRWebServerPatientViewContext.Destroy;
begin
  FContext.free;
  FErrors.free;
  FCards.free;
  FManager.free;
  inherited;
end;

procedure TFHIRWebServerPatientViewContext.SetManager(const Value: TCDSHooksManager);
begin
  FManager.free;
  FManager := Value;
end;

{ TFullServerWebEndPoint }

destructor TFullServerWebEndPoint.Destroy;
begin
  // nothing
  carry.free;
  inherited;
end;

function TFullServerWebEndPoint.link: TFullServerWebEndPoint;
begin
  result := TFullServerWebEndPoint(inherited link);
end;

function TFullServerWebEndPoint.logId: string;
begin
  result := factory.versionName;
end;

function TFullServerWebEndPoint.description: String;
begin
  result := 'Full functionality FHIR Server, for v'+factory.versionString;
end;

function TFullServerWebEndPoint.factory: TFHIRFactory;
begin
  result := FEndPoint.FServerContext.Factory;
end;

//function TFullServerWebEndPoint.store: TFHIRNativeStorageService;
//begin
//  result := FEndPoint.FStore;
//end;
//
//function TFullServerWebEndPoint.terminologies: TCommonTerminologies;
//begin
//  result := FEndPoint.FServerContext.TerminologyServer.CommonTerminologies;
//end;
//
function TFullServerWebEndPoint.BuildFhirAuthenticationPage(langList : THTTPLanguageList; host, path, logId, Msg: String; secure: boolean; params: String): String;
var
  authurl: string;
  p : THTTPParameters;
begin
  authurl := OAuthPath(secure);

  result := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' + Factory.versionString
    + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + #13#10 + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 + ''#13#10 +
    TFHIRXhtmlComposer.header(factory, nil, PathNoSlash, langList, SERVER_FULL_VERSION) + '<h2>' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', langList) + '</h2>'#13#10;

  result := result + '<p>'#13#10 + GetFhirMessage('MSG_AUTH_REQUIRED', langList) + '</p>'#13#10;
  if (Msg = '') and (params <> '') then
  begin
    p := THTTPParameters.Create(params);
    try
      msg := p['error_description'];
    finally
      p.free;
    end;
  end;

  if Msg <> '' then
    result := result + '<p><b>' + FormatTextToHTML(Msg) + '</b></p>'#13#10;

  result := result + '<p><a href="' + AuthServer.BasePath + '/auth?client_id=c.1&response_type=code&scope=openid%20profile%20fhirUser%20user/*.*%20' + SCIM_ADMINISTRATOR
    + '&redirect_uri=' + authurl + '/internal&aud=' + authurl + '&state=' + AuthServer.MakeLoginToken(path, apGoogle) + '">Login using OAuth</a></p>' + #13#10;

  if Common.StatedPort <> 0 then
    result := result + '<p>Or use the <a href="http://' + Host + port(Common.StatedPort, 80) + PathNoSlash + '">unsecured API</a>.</p>'#13#10;

  result := result + '<p>&nbsp;</p>'#13#10 +
    '<p>This server uses <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">Smart App Launch</a> for OAuth logins</p>'#13#10;
  result := result + TFHIRXhtmlComposer.Footer(factory, path, langList, logid);
end;

function TFullServerWebEndPoint.BuildFhirHomePage(compList: TFslList<TFHIRCompartmentId>; logId: String; langList : THTTPLanguageList; host, rawhost, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
var
  counts: TStringList;
  a: String;
  s: String;
  names: TStringList;
  profiles: TFslStringMatch;
  i, j, ix: integer;
  b: TFslStringBuilder;
  pol: String;
begin
  counts := TStringList.Create;
  try

    for a in self.Context.ValidatorContext.allResourceNames do
    begin
      ix := counts.Add(a);
      if (compList.Empty) or self.Context.Indexes.Compartments.existsInCompartment('Patient', a) then
        counts.Objects[ix] := TObject(0)
      else
        counts.Objects[ix] := TObject(-1);
    end;

    try
      pol := self.Context.Storage.ProfilesAsOptionList;
    except
      on e : Exception do
        pol := e.message;
    end;
    profiles := TFslStringMatch.Create;
    try
      profiles.forced := true;

      self.Context.Storage.FetchResourceCounts(compList, counts);

      s := host + sBaseURL;
      b := TFslStringBuilder.Create;
      try
        b.Append('<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
          '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
          '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
          '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' +
          Factory.versionString + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 +
          TFHIRXhtmlComposer.header(factory, Session, sBaseURL, langList, SERVER_FULL_VERSION));

        b.Append('<h2>' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', langList) + '</h2>'#13#10);

        if Session <> nil then
          if secure then
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10)
          else if Common.StatedSSLPort = 0 then
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10)
          else
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + ' (or use <a href="https://' + rawHost + port(Common.StatedSSLPort, 443) + PathNoSlash +
              '">Secure API</a>)</p>'#13#10);

        b.Append('<p>'#13#10 + StringFormat(GetFhirMessage('MSG_HOME_PAGE_1', langList), ['<a href="http://hl7.org/fhir">http://hl7.org/fhir</a>']) + #13#10 +
          StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', langList), [s]) +
          ' This server defines some <a href="'+PathNoSlash+'/local.hts">extensions to the API</a>, and also offers <a href="'+PathNoSlash+'/tx">Terminology Services</a> or '+
            '(or you can browse <a href="/snomed/doco/">SNOMED-CT</a> or <a href="/loinc/doco/">LOINC</a> directly)' + #13#10);
        if Session.canGetUser and (Session.User <> nil) and not Session.isAnonymous then
        begin
          b.Append('. You can also <a href="'+PathNoSlash+'/registerclient.html">Register a client</a>.'+#13#10);
          b.Append(' or <a href="'+PathNoSlash+'/token.hts">get your bearer token</a> (use this to get access to the secure API without needing OAuth login).</p>');
        end
        else
        begin
          b.Append('. If you login through OAuth, you can also Register a client'+#13#10);
          b.Append(' or get your bearer token (use this to get access to the secure API without needing OAuth login in the application).</p>');
        end;

        b.Append('<p>GDPR-Disclosure: All access to this server is logged as AuditEvent Resources, and these store your ip address (and '+'logged in user, if one exists). Also, your IP address is logged with Google Analytics for building geomaps of server usage. Your continued use of the API constitutes agreement to these terms. See [link] for erasure requests.</p>');

        b.Append(
          '</p>'#13#10 + '<hr/>'#13#10 + ''#13#10 + '<p>' + GetFhirMessage('SYSTEM_OPERATIONS', langList) + ':</p><ul><li> <a href="' + sBaseURL + '/metadata">' +
          GetFhirMessage('CONF_PROFILE', langList) + '</a> ' + '(' + GetFhirMessage('OR', langList) + ' <a href="' + sBaseURL +
          '/metadata?_format=text/xml">as xml</a> (' + GetFhirMessage('OR', langList) + ' <a href="' + sBaseURL +
          '/metadata?_format=application/json">JSON</a>)</li>' + #13#10);
        if not IsTerminologyServerOnly then
          b.Append('<li><a class="tag" href="' + sBaseURL + '/$meta">' + GetFhirMessage('SYSTEM_TAGS', langList) + '</a></li>');
        b.Append('<li><a href="' + sBaseURL + '/_search">' + GetFhirMessage('GENERAL_SEARCH', langList) + '</a></li>');
        if not IsTerminologyServerOnly then
          b.Append('<li><a href="' + sBaseURL + '/_history">' + StringFormat(GetFhirMessage('MSG_HISTORY', langList), [GetFhirMessage('NAME_SYSTEM', langList)]) +
            '</a> (History of all resources)</li>' + #13#10);
        if not IsTerminologyServerOnly then
          b.Append('<li><a href="#upload">' + GetFhirMessage('NAME_UPLOAD_SERVICES', langList) + '</a></li>' + #13#10);
        b.Append('<li><a href="' + sBaseURL + '/package-client.hts">Maintain Packages</a></li>' + #13#10);
        if (secure) then
        begin
          b.Append('<li><a href="' + sBaseURL + '/sessions.hts">Manage Sessions</a></li>' + #13#10);
          b.Append('<li><a href="' + sBaseURL + '/auth/logout-all">Log all users out</a></li>' + #13#10);
        end;

        if not IsTerminologyServerOnly then
          b.Append('<li>Create/Edit a new resource based on the profile: <form action="' + sBaseURL + '/_web/Create" method="GET"><select name="profile">' + pol
            + '</select> <input type="submit" value="GO"></form></li>' + #13#10);

        if (Session.canAdministerUsers) then
          b.Append('<li><a href="'+PathNoSlash+'/scim/web">Manage Users</a></li>' + #13#10);

        b.Append('</ul>' + #13#10 + ''#13#10 + '<hr/>'#13#10 + '<p>' + GetFhirMessage('MSG_HOME_PAGE_3', langList) + '</p>' + #13#10);

        b.Append('<table class="lines">'#13#10 +

          '<tr><th>' + GetFhirMessage('NAME_TYPE', langList) + '</th>' + '<th>' + GetFhirMessage('NAME_STORED', langList) + '</th>' + '<th colspan="4">' +
          GetFhirMessage('NAME_OPERATIONS', langList) + '</th><td style="border-left: 1px solid grey"/><th>' + GetFhirMessage('NAME_TYPE', langList) + '</th>' + '<th>'
          + GetFhirMessage('NAME_STORED', langList) + '</th>' + '<th colspan="4">' + GetFhirMessage('NAME_OPERATIONS', langList) + '</th></tr>'#13#10);

        names := TStringList.Create;
        Try
          for a in self.Context.ValidatorContext.allResourceNames do
          begin
            ix := counts.IndexOf(a);
            if (ix >= 0) and (integer(counts.Objects[ix]) > -1) and (self.Context.ResConfig[a].Supported) then
              names.Add(a);
          end;

          names.Sort;
          j := 0;
          if names.count > 0 then
          begin
            for i := 0 to names.Count div 2 do
            begin
              inc(j);
              if j mod 2 = 1 then
                b.Append('<tr bgcolor="#F0F0F0">')
              else
                b.Append('<tr bgcolor="#FFFFFF">');

              a := names[i];
              ix := counts.IndexOf(a);
              b.Append(TFHIRXhtmlComposer.ResourceLinks(a, langList, sBaseURL, integer(counts.Objects[ix]), true, true, Session.canRead(a)));

              b.Append('<td style="border-left: 1px solid grey"/>');

              if (i + names.Count div 2) + 1 < names.Count then
              begin
                a := names[1 + i + names.Count div 2];
                ix := counts.IndexOf(a);
                b.Append(TFHIRXhtmlComposer.ResourceLinks(a, langList, sBaseURL, integer(counts.Objects[ix]), true, true, Session.canRead(a)));
              end;

              b.Append('</tr>');

            end;
          end;
        finally
          names.free;
        end;
        b.Append('</table>'#13#10);
        if not IsTerminologyServerOnly then
          b.Append('<hr/><h2>' + GetFhirMessage('NAME_UPLOAD_SERVICES', langList) + '</h2>'#13#10 +
            '<a name="upload"> </a><form enctype="multipart/form-data" method="POST">' + #13#10 +
            '<p><input type="hidden" name="_format" value="text/html"/><br/>' + #13#10 + '' + GetFhirMessage('MSG_CONTENT_MESSAGE', langList) + '.<br/><br/>' +
            #13#10 + '' + GetFhirMessage('MSG_CONTENT_UPLOAD', langList) + ': <br/><input type="file" name="file" size="60"/><br/>' + #13#10 + '' +
            GetFhirMessage('MSG_CONTENT_PASTE', langList) + ':<br/> <textarea name="src" cols="70" rows="5"/>' + #13#10 + '</textarea><br/><br/>' + #13#10 +
            '<table class="none"><tr><td>Operation:</td><td> <select size="1" name="op">' + #13#10 + ' <option value="transaction">Transaction</option>' +
            #13#10 + ' <option value="batch">Batch</option>' + #13#10 + ' <option value="validation">Validation</option>' + #13#10 + '</select></td></tr>' +
            #13#10 + '<tr><td>Profile:</td><td> <select size="1" name="profile">' + #13#10 + '<option value=""></option>' + #13#10 + pol +
            '</select> (if validating, use the selected profile)</td></tr></table><br/>' + #13#10 + '<input type="submit" value="' +
            GetFhirMessage('NAME_UPLOAD', langList) + '"/>'#13#10 + '</p></form>'#13#10);
        b.Append(TFHIRXhtmlComposer.Footer(factory, sBaseURL, langList, logId));
        result := b.ToString;
      finally
        b.free;
      end;
    finally
      profiles.free;
    end;
  finally
    counts.free;
  end;
end;

function TFullServerWebEndPoint.BuildFhirUploadPage(langList : THTTPLanguageList; host, sBaseURL, aType: String; Session: TFHIRSession): String;
var
  s: String;
begin
  s := host + sBaseURL;

  result := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>' + StringFormat(GetFhirMessage('UPLOAD', langList),
    [aType]) + '</title>'#13#10 + '    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10 + FHIR_JS + '</head>'#13#10 +
    ''#13#10 + '<body>'#13#10 + ''#13#10 + '<div class="header">'#13#10 + '  <a href="http://www.hl7.org/fhir" title="' +
    GetFhirMessage('MSG_HOME_PAGE_TITLE', langList) + '"><img border="0" src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10 +
    ''#13#10 + '  &copy; HL7.org 2011+'#13#10 + '  &nbsp;'#13#10 + '  ' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', langList) + ''#13#10 +
    '  &nbsp;'#13#10 + '  FHIR ' + GetFhirMessage('NAME_VERSION', langList) + ' ' + Factory.versionString + ''#13#10;

  if Session <> nil then
    result := result + '&nbsp;&nbsp;' + FormatTextToXML(Session.SessionName, xmlText);

  result := result + '  &nbsp;<a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', langList) + '</a>'#13#10 + '</div>'#13#10 + ''#13#10 +
    '<div id="div-cnt" class="content">'#13#10 + '<h2>' + StringFormat(GetFhirMessage('UPLOAD', langList), [aType]) + '</h2>'#13#10 + '<form action="' + s +
    lowercase(aType) + '/upload" enctype="multipart/form-data" method="POST">' + #13#10 + '<input type="hidden" name="format" size="text/html"/><br/>' + #13#10
    + '' + GetFhirMessage('MSG_CONTENT_UPLOAD', langList) + ': <input type="file" name="file" size="60"/><br/>' + #13#10 +
    '<input type="submit" value="Upload"/>'#13#10 + '</form>'#13#10 + ''#13#10 + '<p><br/><a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', langList) +
    '</a></p>' + '</div>'#13#10 + '</body>'#13#10 + '</html>'#13#10 + ''#13#10
end;

function TFullServerWebEndPoint.DoSearch(Session: TFHIRSession; rtype: string; langlist  : THTTPLanguageList; params: String): TFHIRBundleW;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
  tt : TFslTimeTracker;
begin
  request := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
  Context := TOperationContext.Create;
  try
    response := TFHIRResponse.Create(self.Context.ValidatorContext.link);
    tt := TFslTimeTracker.Create;
    try
      request.Session := Session.link;
      request.ResourceName := rtype;
      request.langList := langList.link;
      request.LoadParams(params);
      request.CommandType := fcmdSearch;
      ProcessRequest(Context, request, response, tt);
      result := factory.wrapBundle(response.resource.link);
    finally
      tt.free;
      response.free;
      request.free;
    end;
  finally
    Context.free;
  end;
end;

function TFullServerWebEndPoint.GetLaunchParameters(request: TIdHTTPRequestInfo; session: TFhirSession; launchContext: String; params: TAuthLaunchParamsSet): TDictionary<String, String>;
var
  enc : TFhirEncounterW;
begin
  result := TDictionary<String, String>.create;
  if launchContext <> '' then
  begin
    if launchContext.StartsWith('Patient/') then
    begin
      if alpPatient in params then
        result.Add('patient', launchContext.Substring(8));
    end
    else if launchContext.StartsWith('Encounter/') then
    begin
      if alpPatient in params then
      begin
        enc := factory.wrapEncounter(GetResource(session, 'Encounter', THTTPLanguageList.Create(Context.i18nSupport.Languages.link, request.AcceptLanguage, true), launchContext.Substring(10), '', ''));
        try
          result.Add('patient', enc.PatientId);
        finally
          enc.free;
        end;
      end;
      if alpEncounter in params then
        result.Add('encounter', launchContext.Substring(10));
    end;
  end;
end;

procedure TFullServerWebEndPoint.GetPatients(details: TFslStringDictionary);
var
  b : TFHIRBundleW;
  be : TFhirBundleEntryW;
  p : TFhirPatientW;
begin
  b := DoSearch(nil, 'Patient', nil, '_summary=true&__wantObject=true');
  try
    for be in b.entries.forEnum do
    begin
      p := factory.wrapPatient(be.resource.link);
      try
        details.Add(p.id, p.nameSummary);
      finally
        p.free;
      end;
    end;
  finally
    b.free;
  end;
end;

procedure TFullServerWebEndPoint.GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String);
var
  tail: String;
begin
  link := '';
  if (resource <> nil) and (id <> '') then
  begin
    tail := id;
    if ver <> '' then
      tail := tail + '/' + ver;
    if resource.fhirType = 'Questionnaire' then
    begin
      text := 'Try out the Questionnaire as a web form';
      if statedType = 'Profile' then
        link := PathNoSlash + '/_web/StructureDefinition/' + tail
      else
        link := PathNoSlash + '/_web/Questionnaire/' + tail;
    end;
    if resource.fhirType = 'StructureDefinition' then
    begin
      link := PathNoSlash + '/_web/StructureDefinition/' + tail;
      text := 'Try out the Profile as a questionnaire based web form';
    end;
    if resource.fhirType = 'ValueSet' then
    begin
      link := PathNoSlash + '/ValueSet/' + id + '/$expand?filter=';
      text := 'Expand this value set';
    end;
    if resource.fhirType = 'Patient' then
    begin
      link := PathNoSlash + '/_web/Patient/' + id;
      text := 'Patient Record Page';
    end;
    if resource.fhirType = 'Encounter' then
    begin
      link := PathNoSlash + '/_web/Encounter/' + id;
      text := 'Patient Encounter Page';
    end;
  end;
end;

function TFullServerWebEndPoint.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
begin
  if request.id.EndsWith('$edit') then
    result := HandleWebEdit(request, response)
  else if request.id.EndsWith('$post') then
    result := HandleWebPost(request, response)
  {$IFDEF WINDOWS}
  else if request.id.EndsWith('$qa-edit') then
    result := HandleWebQuestionnaireInstance(request, response)
  else if request.id.StartsWith('Questionnaire/') then
    result := HandleWebQuestionnaire(request, response)
  else if request.id.StartsWith('StructureDefinition/') then
    result := HandleWebProfile(request, response)
  {$ENDIF}
  else if request.id = 'Create' then
    result := HandleWebCreate(request, response)
  else if request.id.StartsWith('PatientHooks/') then
    result := HandleWebPatientHooks(request, response, secure)
  else if request.id.StartsWith('Patient/') then
    result := HandleWebPatient(request, response, secure)
  else if request.id.StartsWith('Encounter/') then
    result := HandleWebEncounter(request, response, secure)
  else
    raise EFHIRException.CreateLang('MSG_UNKNOWN_CONTENT', request.langList, [request.id, 'web UI']);
end;

function TFullServerWebEndPoint.ProcessZip(langList : THTTPLanguageList; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW;
var
  rdr: TFslZipReader;
  p: TFHIRParser;
  i: integer;
  s: TFslVCLStream;
  e: TFHIRBundleEntryW;
  bnd: TFHIRBundleW;
  inc: TStringList;
  istart, iend: integer;
  function ok(res: TFhirResourceV): boolean;
  begin
    result := (inc.Count = 0) or (inc.IndexOf(res.fhirType) > -1);
  end;

begin
  inc := TStringList.Create;
  result := factory.wrapBundle(factory.makeResource('Bundle'));
  try
    result.type_ := btTransaction;
    result.id := NewGuidURN;
    // result.base := base;
    rdr := carry.link as TFslZipReader;
    try
      if (rdr = nil) or (name <> carryName) then
      begin
        rdr.free;
        carry.free;
        rdr := TFslZipReader.Create;
        s := TFslVCLStream.Create;
        s.stream := oStream;
        rdr.stream := s;
        rdr.Parts := TFslZipPartList.Create;
        rdr.ReadZip;
        carry := rdr.link as TFslZipReader;
        carryName := name;
      end;

      if (init) or (ini = nil) then
      begin
        istart := 0;
        iend := rdr.Parts.Count - 1;
      end
      else
      begin
        istart := 0;
        iend := istart + 1000;
        if iend > rdr.Parts.Count - 1 then
          iend := rdr.Parts.Count - 1;
      end;

      for i := istart to iend Do
      begin
        if not StringArrayExistsInsensitive(['.info', '.internals', '.zip'], extractFileExt(rdr.Parts[i].name)) then
        begin
          if DebugConsoleMessages then
            writeln('Parse ' + rdr.Parts[i].name);
          if (rdr.Parts[i].name <> 'package.json') then
          begin
            if rdr.Parts[i].name.EndsWith('.json') then
              p := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffJson, langList)
            else if rdr.Parts[i].name.EndsWith('.map') then
              p := TFHIRTextParser.Create(self.Context.ValidatorContext.link, langList.link)
            else
              p := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffXml, langList);
            try
              p.Source := TBytesStream.Create(rdr.Parts[i].AsBytes);
              p.AllowUnknownContent := true;
              p.Parse;
              if p.resource.fhirType = 'Bundle' then
              begin
                bnd := factory.wrapBundle(p.resource.Link);
                try
                case bnd.type_ of
                  btDocument, btMessage, btHistory, btSearchset, btCollection:
                    for e in bnd.entries.forEnum do
                      if ok(e.resource) then
                        result.addEntry(e, false);
                  btTransaction, btTransactionResponse:
                    ; // we ignore these for now
                end;
                finally
                  bnd.free;
                end;
              end
              else if (p.resource.fhirType <> 'Parameters') and ok(p.resource) then
              begin
                result.addEntry('', p.resource.link);
              end;
            finally
              p.Source.free;
              p.free;
            end;
          end;
        end;
      end;
      if iend < rdr.Parts.Count - 1 then
        cursor := iend + 1
      else
        cursor := -1;
    finally
      rdr.free;
    end;
    result.link;
  finally
    result.free;
    inc.free;
  end;
end;

function TFullServerWebEndPoint.GetResource(Session: TFHIRSession; rtype: string; langList : THTTPLanguageList; id, ver, op: String): TFhirResourceV;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
  tt : TFslTimeTracker;
begin
  request := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
  response := TFHIRResponse.Create(self.Context.ValidatorContext.link);
  try
    request.Session := Session.link;
    request.ResourceName := rtype;
    request.langList := langList.link;
    request.id := id;
    request.LoadParams('');
    if (op <> '') then
    begin
      request.CommandType := fcmdOperation;
      request.OperationName := op;
    end
    else if (ver = '') then
      request.CommandType := fcmdRead
    else
    begin
      request.CommandType := fcmdVersionRead;
      request.SubId := ver;
    end;
    Context := TOperationContext.Create;
    tt := TFslTimeTracker.create;
    try
      ProcessRequest(Context, request, response, tt);
    finally
      tt.free;
      Context.free;
    end;
    if response.resource <> nil then
      result := response.resource.link
    else
      raise EFHIRException.CreateLang('MSG_NO_MATCH', langList, [rtype + '/' + id + '/_history/' + ver]);
  finally
    response.free;
    request.free;
  end;
end;

function TFullServerWebEndPoint.HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  profile: TFhirStructureDefinition;
//  builder: TQuestionnaireBuilder;
//  questionnaire: TFHIRQuestionnaire;
//  s, id, fid: String;
begin
  result := 0;
//  // get the right questionnaire
//  if request.Parameters['profile').StartsWith('Profile/') then
//  begin
//    id := request.Parameters['profile').Substring(8);
//    profile := GetResource(request.Session, 'StructureDefinition', request.lang, id, '', '') as TFhirStructureDefinition;
//  end
//  else
//    profile := FindResource(request.Session, 'StructureDefinition', request.lang, 'url=' + request.Parameters['profile')) as TFhirStructureDefinition;
//  try
//    id := profile.id;
//    fid := request.baseUrl + 'StructureDefinition/' + id + '/$questionnaire';
//    s := FContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
//    if s = '' then
//    begin
//      builder := TQuestionnaireBuilder.Create(request.lang);
//      try
//        questionnaire := FContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
//        try
//          if questionnaire = nil then
//          begin
//            builder.profile := profile.link;
//            builder.OnExpand := FContext.Storage.ExpandVS;
//            builder.onLookupCode := FContext.Storage.LookupCode;
//            builder.QuestionnaireId := fid;
//            builder.onLookupReference := LookupReference;
//            builder.Context := request.link;
//
//            builder.build;
//            questionnaire := builder.questionnaire.link;
//            FContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
//          end;
//          // convert to xhtml
//          s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', true);
//          FContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
//        finally
//          questionnaire.free;
//        end;
//        // insert page headers:
//        s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
//        s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION) +
//          '<p><a href="' + builder.QuestionnaireId + '">Questionnaire for this form</a>.' + ' The QuestionnaireAnswers should be submitted as a POST to <i>' +
//          request.baseUrl + '$qa-post</i> with a questionnaire reference of <a href="' + builder.QuestionnaireId + '">' + builder.QuestionnaireId +
//          '</a></p>'#13#10);
//        s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
//        s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '$qa-post";');
//      finally
//        builder.free;
//      end;
//    end;
//
//    response.Body := s;
//    result := Now; // don't want anyone caching anything
//    response.contentType := 'text/html; charset=UTF-8';
//  finally
//    profile.free;
//  end;
  raise ETodo.create('TFullServerWebEndPoint.HandleWebCreate');
end;

function TFullServerWebEndPoint.HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  typ, id, ver: String;
//  r: TFhirResource;
//  s: String;
//  comp: TFHIRComposer;
begin
  result := 0;
  raise ETodo.create('TFullServerWebEndPoint.HandleWebEdit');
//  result := 0;
//
//  // get the right questionnaire
//  StringSplit(request.id, '/', typ, s);
//  StringSplit(s, '/', id, ver);
//  if not(StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FContext.ValidatorContext.hasCustomResource(typ)) then
//    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [typ]);
//
//  r := GetResource(request.Session, typ, request.lang, id, '', '');
//  try
//    if r is TFhirOperationOutcome then
//    begin
//      response.resource := r.link;
//      response.HTTPCode := 500;
//      response.message := 'Internal Error';
//    end
//    else
//    begin
//
//      if request.Parameters['srcformat') = 'json' then
//        comp := TFHIRJsonComposer.Create(FContext.ValidatorContext.link, OutputStylePretty, request.lang)
//      else
//        comp := TFHIRXMLComposer.Create(FContext.ValidatorContext.link, OutputStylePretty, request.lang);
//      try
//        comp.LogId := request.internalRequestId;
//        s := comp.Compose(r);
//      finally
//        comp.free;
//      end;
//
//      s := '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<!DOCTYPE HTML "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + #13#10 + '' +
//        #13#10 + '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">' + #13#10 + '<head>' + #13#10 +
//        '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>Direct Edit for /' + typ + '/' + id + '</title>' +
//        #13#10 + TFHIRXhtmlComposer.PageLinks + FHIR_JS + '</head>' + #13#10 + '' + #13#10 + '<body>' + #13#10 + '' + #13#10 +
//        TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION) + '<h2>Direct Edit for ' + request.id + '</h2>' + #13#10 +
//        '<form action="' + request.baseUrl + '_web/' + typ + '/' + id + '/$post" method="POST">'#13#10 + '  <input type="hidden" name="srcformat" value="' +
//        request.Parameters['srcformat') + '"/>'#13#10 + '  <textarea cols="80" rows="24" name="source" style="white-space:pre">' +
//        FormatXMLForTextArea(s) + #13#10'</textarea><br/>'#13#10 + '  <input type="submit" value="Save"/>'#13#10 + '</form>'#13#10 +
//        TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, true);
//
//      response.Body := s;
//      result := Now; // don't want anyone caching anything
//      response.contentType := 'text/html; charset=UTF-8';
//    end;
//  finally
//    r.free;
//  end;
end;

function TFullServerWebEndPoint.HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  s, typ, id, ver: String;
  p: THTTPParameters;
  prsr: TFHIRParser;
  Context: TOperationContext;
  tt : TFslTimeTracker;
begin
  StringSplit(request.id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  request.id := id;
  if not StringArrayExistsSensitive(factory.ResourceNames, typ) {or FContext.ValidatorContext.hasCustomResource(typ))} then
    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.langList, [typ]);
  request.ResourceName := typ;
  request.CommandType := fcmdUpdate;

  Context := TOperationContext.Create;
  try
    p := THTTPParameters.Create(TEncoding.UTF8.GetString(request.Source.AsBytes), true);
    try
      if p['srcformat'] = 'json' then
        prsr := self.Context.Factory.makeParser(self.Context.ValidatorContext, ffJson, request.langList)
      else
        prsr := self.Context.Factory.makeParser(self.Context.ValidatorContext, ffXml, request.langList);
      try
        s := p['source'];
        prsr.Source := TStringStream.Create(s, TEncoding.UTF8);
        tt := TFslTimeTracker.create;
        try
          prsr.Parse;
          request.resource := prsr.resource.link;
          ProcessRequest(Context, request, response, tt);
          if response.HTTPCode < 300 then
          begin
            response.HTTPCode := 303;
            response.Location := request.baseUrl + typ + '/' + id;
          end;
        finally
          prsr.Source.free;
          tt.free;
        end;
      finally
        prsr.free;
      end;
    finally
      p.free;
    end;
    result := 0;
  finally
    Context.free;
  end;
end;

{$IFDEF WINDOWS}

function TFullServerWebEndPoint.HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  id, ver, fullid: String;
//  profile: TFhirStructureDefinition;
//  builder: TQuestionnaireBuilder;
//  questionnaire: TFHIRQuestionnaire;
//  s: String;
begin
  raise ETodo.create('TFullServerWebEndPoint.HandleWebProfile');
//  // get the right questionnaire
//  StringSplit(request.id.Substring(8), '/', id, ver);
//  profile := GetResource(request.Session, 'StructureDefinition', request.lang, id, ver, '') as TFhirStructureDefinition;
//  try
//    fullid := request.baseUrl + 'StructureDefinition/' + id + '/$questionnaire';
//    s := FContext.QuestionnaireCache.getForm(frtStructureDefinition, id);
//    if s = '' then
//    begin
//      builder := TQuestionnaireBuilder.Create(request.lang);
//      try
//        questionnaire := FContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
//        try
//          if questionnaire = nil then
//          begin
//            builder.profile := profile.link;
//            builder.OnExpand := FContext.Storage.ExpandVS;
//            builder.onLookupCode := FContext.Storage.LookupCode;
//            builder.onLookupReference := LookupReference;
//            builder.Context := request.link;
//            builder.QuestionnaireId := fullid;
//            builder.build;
//            questionnaire := builder.questionnaire.link;
//            FContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.Dependencies);
//          end;
//          // convert to xhtml
//          s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', true);
//          FContext.QuestionnaireCache.putForm(frtStructureDefinition, id, s, builder.Dependencies);
//        finally
//          questionnaire.free;
//        end;
//      finally
//        builder.free;
//      end;
//    end;
//    // insert page headers:
//    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
//    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION));
//    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
//    s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '$qa-post";');
//    response.Body := s;
//    result := Now; // don't want anyone caching anything
//    response.contentType := 'text/html; charset=UTF-8';
//  finally
//    profile.free;
//  end;
end;
{$ENDIF}

function TFullServerWebEndPoint.HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id, ver: String;
  {s, }xhtml: String;
  patient: TFHIRPatientW;
  hookid: String;
  hooks: TFHIRWebServerPatientViewContext;
begin
  result := 0;
  StringSplit(request.id.Substring(8), '/', id, ver);
  hookid := NewGuidId;
  hooks := TFHIRWebServerPatientViewContext.Create(self.Context.Link);
  hooks.manager := TCDSHooksManager.Create;
//  FWebServer.FLock.Lock;
//  try
//    FPatientHooks.Add(hookid, hooks);
//  finally
//    FWebServer.FLock.Unlock;
//  end;
  patient := factory.wrapPatient(GetResource(request.Session, 'Patient', request.langList, id, ver, ''));
  try
    xhtml := factory.getXhtml(patient.Resource).AsPlainText;
 //   startHooks(hooks, patient, request.baseUrl);
  finally
    patient.free;
  end;

//  s := Common.SourceProvider.getSource('patient.html');
//  s := s.Replace('[%id%]', FWebServer.FName, [rfReplaceAll]);
//  s := s.Replace('[%hookid%]', hookid, [rfReplaceAll]);
//  s := s.Replace('[%ver%]', factory.versionString, [rfReplaceAll]);
//  s := s.Replace('[%web%]', FWebServer.WebDesc, [rfReplaceAll]);
//  s := s.Replace('[%patient-details%]', xhtml, [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%patient-app-list%]', patientAppList(FWebServer.FHost + FPath, id), [rfReplaceAll])
//  else
//    s := s.Replace('[%patient-app-list%]', patientAppList(FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort) + FPath, id), [rfReplaceAll]);
//  s := s.Replace('[%patient-id%]', id, [rfReplaceAll]);
//  s := s.Replace('[%admin%]', FWebServer.FAdminEmail, [rfReplaceAll]);
//  if FWebServer.FActualPort = 80 then
//    s := s.Replace('[%host%]', FWebServer.FHost, [rfReplaceAll])
//  else
//    s := s.Replace('[%host%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualPort), [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%securehost%]', FWebServer.FHost, [rfReplaceAll])
//  else
//    s := s.Replace('[%securehost%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort), [rfReplaceAll]);
//  if FWebServer.FActualPort = 80 then
//    s := s.Replace('[%baseOpen%]', FWebServer.FHost + FPath, [rfReplaceAll])
//  else
//    s := s.Replace('[%baseOpen%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualPort) + FPath, [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%baseSecure%]', FWebServer.FHost + FPath, [rfReplaceAll])
//  else
//    s := s.Replace('[%baseSecure%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort) + FPath, [rfReplaceAll]);
//  s := s.Replace('[%root%]', FPath, [rfReplaceAll]);
//
//  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
//
//  response.Body := s;
//  response.contentType := 'text/html; charset=UTF-8';
end;

function TFullServerWebEndPoint.HandleWebEncounter(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
//var
//  id, ver: String;
//  s, xhtml: String;
//  encounter: TFHIREncounterW;
//  hookid: String;
//  hooks: TFHIRWebServerPatientViewContext;
begin
//  result := 0;
//  StringSplit(request.id.Substring(10), '/', id, ver);
//  hookid := NewGuidId;
//  hooks := TFHIRWebServerPatientViewContext.Create(FContext.Link);
//  hooks.manager := TCDSHooksManager.Create;
//  FWebServer.FLock.Lock;
//  try
//    FPatientHooks.Add(hookid, hooks);
//  finally
//    FWebServer.FLock.Unlock;
//  end;
//  encounter := factory.wrapEncounter(GetResource(request.Session, 'Encounter', request.lang, id, ver, ''));
//  try
//    xhtml := factory.getXhtml(encounter.Resource).AsPlainText;
// //   startHooks(hooks, patient, request.baseUrl);
//  finally
//    encounter.free;
//  end;
//
//  s := FWebServer.FCommon.SourceProvider.getSource('encounter.html');
//  s := s.Replace('[%id%]', FWebServer.FName, [rfReplaceAll]);
//  s := s.Replace('[%hookid%]', hookid, [rfReplaceAll]);
//  s := s.Replace('[%ver%]', factory.versionString, [rfReplaceAll]);
//  s := s.Replace('[%web%]', FWebServer.WebDesc, [rfReplaceAll]);
//  s := s.Replace('[%encounter-details%]', xhtml, [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%encounter-app-list%]', encounterAppList(FWebServer.FHost + FPath, id), [rfReplaceAll])
//  else
//    s := s.Replace('[%encounter-app-list%]', encounterAppList(FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort) + FPath, id), [rfReplaceAll]);
//  s := s.Replace('[%patient-id%]', id, [rfReplaceAll]);
//  s := s.Replace('[%admin%]', FWebServer.FAdminEmail, [rfReplaceAll]);
//  if FWebServer.FActualPort = 80 then
//    s := s.Replace('[%host%]', FWebServer.FHost, [rfReplaceAll])
//  else
//    s := s.Replace('[%host%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualPort), [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%securehost%]', FWebServer.FHost, [rfReplaceAll])
//  else
//    s := s.Replace('[%securehost%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort), [rfReplaceAll]);
//  if FWebServer.FActualPort = 80 then
//    s := s.Replace('[%baseOpen%]', FWebServer.FHost + FPath, [rfReplaceAll])
//  else
//    s := s.Replace('[%baseOpen%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualPort) + FPath, [rfReplaceAll]);
//  if FWebServer.FActualSSLPort = 443 then
//    s := s.Replace('[%baseSecure%]', FWebServer.FHost + FPath, [rfReplaceAll])
//  else
//    s := s.Replace('[%baseSecure%]', FWebServer.FHost + ':' + inttostr(FWebServer.FActualSSLPort) + FPath, [rfReplaceAll]);
//  s := s.Replace('[%root%]', FPath, [rfReplaceAll]);
//
//  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
//
//  response.Body := s;
//  response.contentType := 'text/html; charset=UTF-8';
result := 0;
end;

function TFullServerWebEndPoint.HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
//var
//  id: String;
  // s, xhtml : String;
  // patient : TFHIRPatient;
  // hookid : String;
//  hooks: TFHIRWebServerPatientViewContext;
begin
//  result := 0;
//
//  id := request.id.Substring(13);
//  FWebServer.FLock.Lock;
//  try
//    if FPatientHooks.TryGetValue(id, hooks) then
//      hooks.link
//    else
//      hooks := nil;
//  finally
//    FWebServer.FLock.Unlock;
//  end;
//
//  if hooks <> nil then
//  begin
//    try
//      while hooks.manager.waiting do
//        sleep(1000);
//      FWebServer.FLock.Lock;
//      try
//        response.Body := presentAsHtml(hooks.cards, nil, hooks.Errors);
//        FPatientHooks.Remove(id);
//      finally
//        FWebServer.FLock.Unlock;
//      end;
//      response.HTTPCode := 200;
//      response.contentType := 'text/html; charset=UTF-8';
//    finally
//      hooks.free;
//    end;
//  end;
result := 0;
end;

{$IFDEF WINDOWS}
function TFullServerWebEndPoint.HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  id, ver: String;
  questionnaire: TFHIRResourceV;
  s: String;
begin
  // get the right questionnaire
  StringSplit(request.id.Substring(14), '/', id, ver);
  questionnaire := GetResource(request.Session, 'Questionnaire', request.langList.link, id, ver, '');
  try
    // convert to xhtml
    s := transform1(questionnaire, request.langList, 'QuestionnaireToHTML.xslt', false);
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(factory, request.Session, request.baseUrl, request.langList, SERVER_FULL_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(factory, request.baseUrl, request.langList, request.internalRequestId, false));
    s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '/QuestionnaireAnswers";');
    response.Body := s;
    result := Now; // don't want anyone caching anything
    response.contentType := 'text/html; charset=UTF-8';
  finally
    questionnaire.free;
  end;
end;

function TFullServerWebEndPoint.HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  typ, id, ver: String;
//  r: TFhirResourceV;
//  qa: TFhirQuestionnaireResponse;
//  q: TFHIRQuestionnaire;
//  s, j: String;
//  json: TFHIRJsonComposer;
begin
  raise ETodo.create('TFullServerWebEndPoint.HandleWebQuestionnaireInstance');
//  result := 0;
//
//  // get the right questionnaire
//  StringSplit(request.id, '/', typ, s);
//  StringSplit(s, '/', id, ver);
//  if not(StringArrayExistsSensitive(CODES_TFhirResourceType, typ) or FContext.ValidatorContext.hasCustomResource(typ)) then
//    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.lang, [typ]);
//
//  r := GetResource(request.Session, typ, request.lang, id, ver, 'qa-edit');
//  try
//    if r is TFhirOperationOutcome then
//    begin
//      response.resource := r.link;
//      response.HTTPCode := 500;
//      response.message := 'Internal Error';
//    end
//    else
//    begin
//      qa := r as TFhirQuestionnaireResponse;
//      q := (FindContainedResource(qa, qa.questionnaire) as TFHIRQuestionnaire).link;
//      if q = nil then
//        raise EFHIRException.CreateLang('CANNOT_FIND', request.lang, ['Questionnaire', qa.questionnaireElement.reference.Substring(1)]);
//
//      // convert to xhtml
//      s := transform1(q, request.lang, 'QuestionnaireToHTML.xslt', true);
//
//      // make clean qa
//      qa.questionnaireElement.reference := 'Questionnaire/' + qa.questionnaireElement.reference.Substring(1);
//      qa.containedList.Clear;
//      json := TFHIRJsonComposer.Create(request.Context.link, OutputStyleNormal, request.lang);
//      try
//        j := json.Compose(qa);
//      finally
//        json.free;
//      end;
//
//      // insert page headers:
//      s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
//      s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(request.Session, request.baseUrl, request.lang, SERVER_VERSION));
//      s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(request.baseUrl, request.lang, request.internalRequestId, false));
//      // insert the answer:
//      s := s.Replace('var QuestionnaireResponse=null;', 'var QuestionnaireResponse=' + j + ';');
//      response.Body := s;
//      result := Now; // don't want anyone caching anything
//      response.contentType := 'text/html; charset=UTF-8';
//    end;
//  finally
//    r.free;
//  end;
end;
{$ENDIF}

//procedure TFullServerWebEndPoint.HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
//var
//  ws: TIdWebSocket;
//begin
//  ws := TIdWebSocket.Create(nil);
//  try
//    if ws.open(AContext, request, response) then
//      self.Context.SubscriptionManager.HandleWebSocket(ws);
//  finally
//    ws.free;
//  end;
//end;
//
{$IFDEF WINDOWS}
function TFullServerWebEndPoint.transform1(resource: TFhirResourceV; langList : THTTPLanguageList; xslt: String; saveOnly: boolean): string;
var
  xml: TFHIRComposer;
  msx: TMsXmlParser;
  b: TBytesStream;
  v: variant;
  doc, src: IXMLDOMDocument2;
  xform: IXSLTemplate;
  proc: IXSLProcessor;
  url: String;
begin
  // result := transform2(resource, lang, xslt);
  // exit;

  b := TBytesStream.Create;
  try
    xml := factory.makeComposer(Context.ValidatorContext, ffXml, langList, OutputStyleNormal);
    try
      xml.Compose(b, resource);
    finally
      xml.free;
    end;
    b.Position := 0;
    msx := TMsXmlParser.Create;
    try
      doc := msx.Parse(b);
    finally
      msx.free;
    end;
  finally
    b.free;
  end;
//  Logging.log(doc.documentElement.namespaceURI + ', ' + doc.documentElement.nodeName);

  v := CreateOLEObject('MSXML2.FreeThreadedDOMDocument.6.0');
  src := IUnknown(TVarData(v).VDispatch) as IXMLDOMDocument2;
  src.async := false;
  src.resolveExternals := false;
  src.validateOnParse := false;
  src.setProperty('AllowDocumentFunction', true);
  if not src.loadXML(Common.SourceProvider.getSource(xslt)) then
    raise EXmlException.create('unable to parse XSLT: ' + src.parseError.reason);

  v := CreateOLEObject('MSXML2.XSLTemplate.6.0');
  xform := IUnknown(TVarData(v).VDispatch) as IXSLTemplate;
  xform.stylesheet := src;

  proc := xform.createProcessor;
  proc.Input := doc;
  proc.addParameter('useMicrosoft', 'true', '');

  if Common.StatedPort <> 0 then
    url := 'http://' + Common.Host + ':' + inttostr(Common.StatedPort)
  else
    url := 'https://' + Common.Host + ':' + inttostr(Common.StatedSSLPort);

  if saveOnly then
    proc.addParameter('saveOnly', 'true', '');

  proc.addParameter('expansionServer', url + PathNoSlash, '');
  proc.addParameter('iconPath', url, '');
  proc.addParameter('jQueryPath', url + '/js', '');

  proc.Transform;
  result := proc.Output;
end;
{$ENDIF}

end.


