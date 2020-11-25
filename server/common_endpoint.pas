unit common_endpoint;

{$i fhir.inc}

// shared between terminology server and general server

interface

Uses
  {$IFDEF WINDOWS} Windows, ActiveX, ComObj, {$ENDIF}
  SysUtils, Classes, IniFiles, Generics.Collections, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer, IdTCPServer, IdContext, IdHTTP, IdCookie, IdZLibCompressorBase, IdSSL, IdSMTP,
  IdCompressorZLib, IdZLib, IdSchedulerOfThreadPool, IdGlobalProtocols, IdMessage, IdExplicitTLSClientServerBase, IdGlobal, fsl_websocket,
  IdOpenSSLIOHandlerServer, IdOpenSSLIOHandlerClient, IdOpenSSLVersion, IdOpenSSLX509,

  fsl_base, fsl_utilities, fsl_crypto, fsl_logging, fsl_stream, fsl_collections, fsl_threads, fsl_json, fsl_xml,
  {$IFDEF WINDOWS} fsl_msxml, fsl_service_win, {$ENDIF}
  fsl_openssl, fsl_http, fdb_manager, fsl_htmlgen, fdb_dialects, fsl_rdf, fsl_graphql, fsl_twilio, fsl_oauth,

  {$IFDEF WINDOWS}
  fdb_odbc,
  {$ENDIF}
  fhir_objects, fhir_parser,  fhir_xhtml, fhir_utilities, fhir_common, fhir_factory, fhir_client, fhir_pathengine,
  fhir_client_http,
  package_spider, fsl_npm_client, fsl_npm, fsl_npm_cache,
  fhir_oauth, fhir_cdshooks,
  fhir_graphql, fhir_ndjson,
  {$IFNDEF NO_CONVERSION} fxver_convertors,{$ENDIF}
  tx_server, tx_manager, ftx_sct_expressions, ftx_loinc_services, ftx_loinc_publisher, ftx_service,
  tags, session, storage, security, html_builder, ftx_sct_services, ftx_sct_publisher, server_config,
  scim_server,
  auth_manager, reverse_client, cds_hooks_server, web_source, analytics, bundlebuilder, server_factory,
  user_manager, server_context, server_constants, utilities, jwt, usage_stats,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  subscriptions, twilio, telnet_server,
  tx_webserver, web_base, endpoint;

type
  TFhirWebServerCommonEndpoint = class;

  ERestfulAuthenticationNeeded = class(ERestfulException)
  private
    FMsg: String;
  public
    constructor Create(Const sContext : String; sMessage, sCaption : String; const lang : THTTPLanguages); overload;
    Property Msg: String read FMsg;
  end;

  TAsyncTaskThread = class(TThread)
  private
    FKey : integer;
    FServer : TFhirWebServerCommonEndpoint;
    FRequest : TFHIRRequest;
    FFormat : TFHIRFormat;
    files : TFslMap<TFslFile>;
    FBundle : TFHIRBundleW;
    procedure SetRequest(const Value: TFHIRRequest);
    procedure SetServer(const Value: TFhirWebServerCommonEndpoint);

    procedure status(status : TAsyncTaskStatus; message : String);
    procedure details;
    procedure callback(IntParam: Integer; StrParam: String);

    procedure saveOutcome(response : TFHIRResponse);
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TBundleType; out builder : TFhirBundleBuilder);
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServerCommonEndpoint);
    destructor Destroy; override;

    procedure kill;

    property Key : integer read FKey write FKey;
    property Format : TFHIRFormat read FFormat write FFormat;
    Property Server : TFhirWebServerCommonEndpoint read FServer write SetServer;
    Property Request : TFHIRRequest read FRequest write SetRequest;
  end;

  TFHIRWebServerScriptPlugin = class abstract (TFslObject)
  private
    FEndPoint : TFhirWebServerCommonEndpoint;
  protected
    procedure ProcessFile(name : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; secure : boolean; variables : TFslMap<TFhirObject>);
  public
    function process(s : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV) : String; virtual; abstract;
    function processPage(request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV) : boolean; virtual;
  end;

  THtmlFormScriptPlugin = class (TFHIRWebServerScriptPlugin)
  public
    function process(s : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV) : String; override;
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

  TFHIRServerPostHandler = class abstract (TFslObject)
  private
    FServer : TFhirWebServerCommonEndpoint;
    FParams: THTTPParameters;
    FSecure: boolean;
    FVariables: TFslMap<TFHIRObject>;
    FContext : TFHIRServerContext;
    FSession: TFHIRSession;
    procedure SetParams(const Value: THTTPParameters);
    procedure SetVariables(const Value: TFslMap<TFHIRObject>);
    procedure SetContext(const Value: TFHIRServerContext);
    procedure SetSession(const Value: TFHIRSession);
  protected
    FRedirect: String;
  public
    constructor Create(server : TFhirWebServerCommonEndpoint);
    destructor Destroy; override;

    procedure execute; virtual; abstract;

    property context : TFHIRServerContext read FContext write SetContext;
    property params : THTTPParameters read FParams write SetParams;
    property redirect : String read FRedirect;
    property secure : boolean read FSecure write FSecure;
    property session: TFHIRSession read FSession write SetSession;
    property variables : TFslMap<TFHIRObject> read FVariables write SetVariables;
  end;

  TPackageLoader = class (TFHIRServerPostHandler)
  private
    procedure load;
    procedure reload;
  public
    procedure execute; override;
  end;

  TFHIRWebServerCommunicator = class (TFHIRClientCommunicator)
  private
    FEndPoint : TFhirWebServerCommonEndpoint;
    FSession : TFHIRSession;
    FSecure : boolean;
    function fetchResource(command, url : String; resource : TFHIRResourceV) : TFhirResourceV;
    function makeUrl(tail : String) : String;
  public
    constructor Create(ep : TFhirWebServerCommonEndpoint; secure : boolean; session : TFHIRSession);
    function conformanceV(summary : boolean) : TFHIRResourceV; override;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; override;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; override;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV; override;
    function vreadResourceV(atype : TFhirResourceTypeV; id, vid : String) : TFHIRResourceV; override;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; patch : TJsonArray) : TFHIRResourceV; overload; override;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String); override;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload; override;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; override;
    function searchAgainV(link : String) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; override;
    function historyInstanceV(atype : TFHIRResourceTypeV; id : String; allRecords : boolean; params : string) : TFHIRResourceV; override;
    function address : String; override;
    function customGet(path : String; headers : THTTPHeaders) : TFslBuffer; override;
    function customPost(path : String; headers : THTTPHeaders; body : TFslBuffer) : TFslBuffer; override;
    procedure terminate; override;
  end;

  TFhirWebServerCommonEndpoint = class (TFhirWebServerEndpoint)
  private
    FContext : TFHIRServerContext;
    FPath: String;
    FAuthServer: TAuth2Server;
    FCDSHooksServer: TCDSHooksServer;
    FPatientHooks: TFslMap<TFHIRWebServerPatientViewContext>;
    FAdaptors: TFslMap<TFHIRFormatAdaptor>;
    carry: TFslZipReader; // for uploading support
    carryName: String;
    FCode: String;
    FPlugins : TFslList<TFHIRWebServerScriptPlugin>;
    FThreads : TList<TAsyncTaskThread>;

    function readVersion(mt : String) : TFHIRVersion;
    Procedure ReadTags(header: String; request: TFHIRRequest); overload;
    procedure cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
    function EndPointDesc(secure: boolean): String;
    function loadFromRsaDer(cert : string) : TJWKList;

    function parseFile(fmt: TFHIRFormat; name: String): TFHIRResourceV;
    function EncodeVersionsJson(r: TFHIRResourceV): TBytes;
    function EncodeVersionsXml(r: TFHIRResourceV): TBytes;
    function processProvenanceHeader(header : String; const lang : THTTPLanguages): TFhirProvenanceW;
//    function LookupReference(Context: TFHIRRequest; id: String): TResourceWithReference;
    function patientAppList(base, id : String) : string;
    function encounterAppList(base, id : String) : string;

    procedure registerScriptPlugins;

    function HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    function HandleWebEncounter(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    procedure GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String);
    function getReferencesByType(t : String) : String;
    Procedure RunPostHandler(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean);
    procedure PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String);
    Procedure HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    function HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TBundleType; out builder : TFhirBundleBuilder);
{$IFDEF WINDOWS}
    function transform1(resource: TFhirResourceV; const lang : THTTPLanguages; xslt: String; saveOnly: boolean): string;
    function HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
{$ENDIF}
    Procedure HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);

    Function BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; const lang : THTTPLanguages; host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
    Function BuildFhirUploadPage(const lang : THTTPLanguages; host, sBaseURL: String; aType: String; Session: TFHIRSession): String;
    Function BuildFhirAuthenticationPage(const lang : THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params : String): String;
    function buildPackageList : String;
    function buildSessionsTable : String;

    function GetResource(Session: TFHIRSession; rtype: String; const lang : THTTPLanguages; id, ver, op: String): TFhirResourceV;
    function CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
//    function FindResource(Session: TFHIRSession; rtype: String; lang, params: String): TFhirResourceV;
    function HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
    procedure startHooks(ctxt: TFHIRWebServerPatientViewContext; patient: TFHIRPatientW; url: String);
    function HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
   {$IFDEF WINDOWS}
    function HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
    {$ENDIF}
    function makeTaskRedirect(base, id : String; msg : String; fmt : TFHIRFormat; names : TStringList) : string;
    Procedure ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; prefix : String);
    function ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
    function ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
    procedure SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; const lang : THTTPLanguages; message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFHIRIssueType);
    Procedure ReturnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    procedure checkRequestByJs(context : TOperationContext; request : TFHIRRequest);
    function ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
    procedure OnCDSResponse(manager: TCDSHooksManager; server: TRegisteredFHIRServer; Context: TObject; response: TCDSHookResponse; error: String);
    function DoSearch(Session: TFHIRSession; rtype: string; const lang : THTTPLanguages; params: String): TFHIRBundleW;
    function processRegistration(request: TIdHTTPRequestInfo; session : TFhirSession): String;

    Function ProcessZip(const lang : THTTPLanguages; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW;

    function BuildRequest(const lang : THTTPLanguages; sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sContentEncoding,
      sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse; var aFormat: TFHIRFormat; var redirect: boolean; form: TMimeMessage;
      bAuth, secure: boolean; out relativeReferenceAdjustment: integer; var style : TFHIROutputStyle; Session: TFHIRSession; cert: TIdOpenSSLX509): TFHIRRequest;
    Procedure ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip, cache: boolean);
    procedure SetAuthServer(const Value: TAuth2Server);
    procedure StopAsyncTasks;
  public
    constructor Create(code : String; path : String; context : TFHIRServerContext; common : TFHIRWebServerCommon);
    destructor Destroy; override;
    property Context : TFHIRServerContext read FContext;
    property CDSHooksServer: TCDSHooksServer read FCDSHooksServer;
    property path : String read FPath;
    function ClientAddress(secure: boolean): String;
    property code : String read FCode;
    property AuthServer : TAuth2Server read FAuthServer write SetAuthServer;

    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    procedure GetPatients(details : TFslStringDictionary);
    function GetLaunchParameters(request: TIdHTTPRequestInfo; session : TFhirSession; launchContext : String; params : TAuthLaunchParamsSet) : TDictionary<String, String>;
    procedure CheckAsyncTasks;

    function factory : TFHIRFactory;
    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; override;
    Procedure Transaction(stream: TStream; init : boolean; name, base: String; ini: TFHIRServerConfigFile; mode : TOperationMode; callback: TInstallerCallback); overload;
    Procedure Transaction(bundle : TFHIRBundleW; init : boolean; name, base: String; mode : TOperationMode; callback: TInstallerCallback); overload;
    property PatientHooks: TFslMap<TFHIRWebServerPatientViewContext> read FPatientHooks;
  end;

  { TFHIRServerEndPoint }
  // this is the base for actual end points - they are a pair - a web end point - the class above, a
  // and this class, which is the administrative base

  TFHIRServerEndPoint = class (TFslObject)
  private
    FDatabase : TFDBManager;
  public
    constructor Create(settings : TFHIRServerConfigSection; db : TFDBManager);
    destructor Destroy; override;

    procedure InstallDatabase;
    procedure UninstallDatabase;
    procedure LoadPackages(plist : String);
    procedure updateAdminPassword;
    procedure Load;
    Procedure Unload;
  end;

implementation

{ TFHIRWebServerScriptPlugin }

procedure TFHIRWebServerScriptPlugin.ProcessFile(name: String; request: TIdHTTPRequestInfo; pm: THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; secure : boolean; variables : TFslMap<TFhirObject>);
begin
  FEndPoint.ReturnSecureFile(request, response, session, name, ChangeFileExt(FEndPoint.Common.SourceProvider.AltFile(request.Document, FEndPoint.path), '.secure.html'), '', secure, variables);
end;

function TFHIRWebServerScriptPlugin.processPage(request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV): boolean;
begin
  result := false;
end;

{ TFhirWebServerCommonEndpoint }

constructor TFhirWebServerCommonEndpoint.create(code : String; path: String; context: TFHIRServerContext; common : TFHIRWebServerCommon);
begin
  inherited create(code, path, common);
  FContext := context;
  FThreads := TList<TAsyncTaskThread>.create;

  FPath := path;
  FCode := code;
  FPatientHooks := TFslMap<TFHIRWebServerPatientViewContext>.Create('patient.hooks');
  FAdaptors := TFslMap<TFHIRFormatAdaptor>.Create('adaptors');
//  FAdaptors.Add('dataPoints', TOpenMHealthAdaptor.Create);
  // FAuthServer: TAuth2Server;
  //  FCDSHooksServer: TCDSHooksServer;
  TerminologyWebServer := TTerminologyWebServer.create(Context.TerminologyServer.Link, Context.ValidatorContext.link, FPath, '?', ReturnProcessedFile);
  FPlugins := TFslList<TFHIRWebServerScriptPlugin>.create;
  registerScriptPlugins;
end;

destructor TFhirWebServerCommonEndpoint.Destroy;
begin
  FContext.Free;
  StopAsyncTasks;
  FThreads.Free;
  FPlugins.free;
  FCDSHooksServer.Free;
  FAuthServer.Free;
  carry.Free;
  FAdaptors.Free;
  FPatientHooks.Free;
  inherited;
end;

function TFhirWebServerCommonEndpoint.DoSearch(Session: TFHIRSession; rtype: string; const lang : THTTPLanguages; params: String): TFHIRBundleW;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
begin
  request := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
  Context := TOperationContext.Create;
  try
    response := TFHIRResponse.Create(self.Context.ValidatorContext.link);
    try
      response.OnCreateBuilder := doGetBundleBuilder;
      request.Session := Session.link;
      request.ResourceName := rtype;
      request.lang := lang;
      request.LoadParams(params);
      request.CommandType := fcmdSearch;
      checkRequestByJs(context, request);
      ProcessRequest(Context, request, response);
      result := factory.wrapBundle(response.resource.link);
    finally
      response.Free;
      request.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.Transaction(bundle: TFHIRBundleW; init: boolean; name, base: String; mode : TOperationMode; callback: TInstallerCallback);
var
  req: TFHIRRequest;
  resp: TFHIRResponse;
  // op : TFHIRNativeOperationEngine;
  Context: TOperationContext;
begin
  // if init then
  // op := FServerContext.Storage.createOperationContext(THTTPLanguages.create('en'));
  Context := TOperationContext.Create(mode, callback, 'Load from ' + name);
  try
    req := TFHIRRequest.Create(self.Context.ValidatorContext.link, roUpload, self.Context.Indexes.Compartments.link);
    try
      req.CommandType := fcmdTransaction;
      req.resource := bundle.Resource.link;
      req.resource.tags['duplicates'] := 'ignore';
      req.Session := self.Context.SessionManager.CreateImplicitSession('n/a', self.Context.Globals.OwnerName, 'Service Manager', systemInternal, true, false);
      req.Session.allowAll;
      req.LoadParams('');
      req.baseUrl := self.Context.Globals.Bases[0];
      Context.message := 'Process ' + name;
      // GJSHost.registry := FContext.EventScriptRegistry.link;
      resp := TFHIRResponse.Create(self.Context.ValidatorContext.link);
      try
        resp.OnCreateBuilder := doGetBundleBuilder;
        checkRequestByJs(context, req);
        ProcessRequest(Context, req, resp);
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    Context.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.parseFile(fmt : TFHIRFormat; name : String) : TFHIRResourceV;
var
  p : TFHIRParser;
  f : TFileStream;
begin
  p := factory.makeParser(self.Context.ValidatorContext.link, fmt, THTTPLanguages.create('en'));
  try
    f := TFileStream.Create(name, fmOpenRead);
    try
      result := p.parseResource(f)
    finally
      f.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.Transaction(stream: TStream; init : boolean; name, base: String; ini: TFHIRServerConfigFile; mode : TOperationMode; callback: TInstallerCallback);
var
  req: TFHIRRequest;
  resp: TFHIRResponse;
  // op : TFHIRNativeOperationEngine;
  cursor: integer;
  Context: TOperationContext;
  b : TFHIRBundleW;
begin
  // if init then
  // op := FServerContext.Storage.createOperationContext(THTTPLanguages.create('en'));
  Context := TOperationContext.Create(mode, callback, 'Load from ' + name);
  try
    req := TFHIRRequest.Create(self.Context.ValidatorContext.link, roUpload, self.Context.Indexes.Compartments.link);
    try
      req.CommandType := fcmdTransaction;
      if ExtractFileExt(name) = '.xml' then
        req.resource := ParseFile(ffXml, name)
      else if ExtractFileExt(name) = '.json' then
        req.resource := ParseFile(ffJson, name)
      else
      begin
        b := ProcessZip(THTTPLanguages.create('en'), stream, name, base, init, ini, Context, cursor);
        try
          req.resource := b.Resource.link;
        finally
          b.free;
        end;
      end;
      if req.Resource.fhirType <> 'Bundle' then
      begin
        b := factory.wrapBundle(factory.makeResource('Bundle'));
        try
          b.type_ := btTransaction;
          b.addEntry('', req.Resource.link);
          req.resource := b.Resource.link;
        finally
          b.Free;
        end;
      end;

      req.resource.tags['duplicates'] := 'ignore';
      req.Session := self.Context.SessionManager.CreateImplicitSession('n/a', self.Context.Globals.OwnerName, 'Service Manager', systemInternal, true, false);
      req.Session.allowAll;
      req.LoadParams('');
      req.baseUrl := self.Context.Globals.Bases[0];
      Context.message := 'Process ' + name;
      {$IFNDEF NO_JS}
      GJSHost.registry := self.Context.EventScriptRegistry.link;
      {$ENDIF}
      resp := TFHIRResponse.Create(self.Context.ValidatorContext.link);
      try
        resp.OnCreateBuilder := doGetBundleBuilder;
        checkRequestByJs(context, req);
        ProcessRequest(Context, req, resp);
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    Context.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.patientAppList(base, id : String): string;
var
  b : TStringBuilder;
  apps : TFslList<TRegisteredClientInformation>;
  app : TRegisteredClientInformation;
begin
  b := TStringBuilder.Create;
  try
    apps := TFslList<TRegisteredClientInformation>.create;
    try
      Context.Storage.fetchClients(apps);
      for app in apps do
      begin
        if app.patientContext then
        begin
          b.Append('  <li><a href="');
          b.Append(app.url);
          b.Append('?iss=https://');
          b.Append(base);
          b.Append('&launch=Patient/');
          b.Append(id);
          b.Append('">');
          b.Append(FormatTextToXml(app.name, xmlText));
          b.Append('</a></li>'#13#10);
        end;
      end;
    finally
      apps.Free;
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.encounterAppList(base, id : String): string;
var
  b : TStringBuilder;
  apps : TFslList<TRegisteredClientInformation>;
  app : TRegisteredClientInformation;
begin
  b := TStringBuilder.Create;
  try
    apps := TFslList<TRegisteredClientInformation>.create;
    try
      Context.Storage.fetchClients(apps);
      for app in apps do
      begin
        if app.patientContext then
        begin
          b.Append('  <li><a href="');
          b.Append(app.url);
          b.Append('?iss=https://');
          b.Append(base);
          b.Append('&launch=Encounter/');
          b.Append(id);
          b.Append('">');
          b.Append(FormatTextToXml(app.name, xmlText));
          b.Append('</a></li>'#13#10);
        end;
      end;
    finally
      apps.Free;
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String;
var
  Session: TFHIRSession;
  c: integer;
  check: boolean;
begin
  Session := nil;
  try
    if (request.AuthUsername = INTERNAL_SECRET) then
      Context.SessionManager.GetSession(request.AuthPassword, Session, check);

    if (Session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        Context.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1), Session, check);
    end;

    if OWinSecurityPlain and (((Session = nil) and (request.Document <> FPath + OWIN_TOKEN_PATH)) or not Context.UserProvider.allowInsecure) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + FPath + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
    else if OWinSecurityPlain and Context.UserProvider.allowInsecure and (request.Document = FPath + OWIN_TOKEN_PATH) then
      HandleOWinToken(AContext, false, request, response);

    if request.Document.StartsWith(FAuthServer.path) then
    begin
      result := 'Authorization Request';
      FAuthServer.HandleRequest(AContext, request, Session, response, false)
    end
    else if Common.SourceProvider.exists(Common.SourceProvider.AltFile(request.Document, FPath)) then
    begin
      result := 'Static File';
      ReturnSpecFile(response, request.Document, Common.SourceProvider.AltFile(request.Document, FPath), false)
    end
    else if request.Document.EndsWith('.hts') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html')) then
    begin
      result := 'Processed File';
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html'), false)
    end
    else if request.Document.EndsWith('.phs') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html')) then
    begin
      result := 'Post Handler';
      runPostHandler(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html'), false)
    end
    else if (request.Document = path+'/.well-known/smart-configuration') then
    begin
      result := 'Smart Configuration';
      FAuthServer.HandleDiscovery(AContext, request, response)
    end
    else if request.Document.StartsWith(FPath + '/cds-services') and FCDSHooksServer.active then
    begin
      result := 'CDS Hooks Service';
      FCDSHooksServer.HandleRequest(false, FPath, Session, AContext, request, response)
    end
    else if request.Document.StartsWith(AppendForwardSlash(FPath) + 'websockets', false) then
    begin
      result := 'Web Sockets';
      HandleWebSockets(AContext, request, response, false, false, FPath)
    end
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestVersion(request.Document) then
    begin
      result := TerminologyWebServer.ProcessVersion(AContext, request, Session, response, false)
    end
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestInNoVersion(request.Document) then
    begin
      result := TerminologyWebServer.redirectToNoVersion(AContext, request, Session, response, false)
    end
    else if request.Document.StartsWith(FPath, false) then
    begin
      result := HandleRequest(AContext, request, response, false, false, FPath, id, Session, nil);
    end
    else if request.Document.StartsWith(AppendForwardSlash(FPath) + 'FSecurePath', false) then
    begin
      result := 'Web Sockets';
      HandleWebSockets(AContext, request, response, false, false, FPath)
    end
    else
    begin
      result := 'Not Found';
      response.ResponseNo := 404;
      response.ContentText := 'Document ' + request.Document + ' not found';
    end;
  finally
    session.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String);
begin
  if (FAuthServer <> nil) and (Common.ActualSSLPort <> 0) then
    conf.addSmartExtensions(
      UrlPath([baseUrl, FAuthServer.AuthPath]),
      UrlPath([baseUrl, FAuthServer.TokenPath]),
      UrlPath([baseUrl, FAuthServer.RegisterPath]),
      UrlPath([baseUrl, FAuthServer.ManagePath]), caps)
  else
    conf.addSmartExtensions('', '', '', '', []); // just set cors
end;

function TFhirWebServerCommonEndpoint.secureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String;
var
  Session: TFHIRSession;
  check: boolean;
  c: integer;
  JWT: TJWT;
begin
  Session := nil;
  try
    check := false;
    if (request.AuthUsername = INTERNAL_SECRET) then
      if request.AuthPassword.StartsWith('urn:') then
        Context.SessionManager.GetSession(request.AuthPassword, Session, check)
      else
      begin
        JWT := TJWTUtils.unpack(request.AuthPassword, false, nil);
        // todo: change this to true, and validate the JWT, under the right conditions
        try
          if cert = nil then
            Session := Context.SessionManager.getSessionFromJWT(request.RemoteIP, 'Unknown', systemUnknown, JWT)
          else
            Session := Context.SessionManager.getSessionFromJWT(request.RemoteIP, cert.Subject.CN, systemFromCertificate, JWT);
        finally
          JWT.Free;
        end;
      end;

    if (Session = nil) then
    begin
      c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
      if c > -1 then
        Context.SessionManager.GetSession(request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1), Session, check);
      // actually, in this place, we ignore check.  we just established the session
    end;

    if request.Document.StartsWith(FAuthServer.path) then
    begin
      result := 'OAuth';
      FAuthServer.HandleRequest(AContext, request, Session, response, true)
    end
    else if OWinSecuritySecure and (request.Document = URLPath([FPath, OWIN_TOKEN_PATH])) then
    begin
      result := 'OWin';
      HandleOWinToken(AContext, true, request, response)
    end
    else if Common.SourceProvider.exists(Common.SourceProvider.AltFile(request.Document, FPath)) then
    begin
      result := 'Spec file '+request.Document;
      ReturnSpecFile(response, request.Document, Common.SourceProvider.AltFile(request.Document, FPath), true)
    end
    else if request.Document.EndsWith('.hts') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html')) then
    begin
      result := 'Processed File '+request.Document;
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.html'), true)
    end
    else if request.Document.EndsWith('.html') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.secure.html')) then
    begin
      result := 'Secure File '+request.Document;
      ReturnSecureFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, FPath), '.secure.html'), id, true)
    end
    else if request.Document.StartsWith(FPath+'/scim') then
    begin
      result := 'SCIM';
      ProcessScimRequest(AContext, request, response, FPath)
    end
    else if request.Document.StartsWith(FPath, false) then
      result := HandleRequest(AContext, request, response, true, true, FPath, id, Session, cert)
    else if OWinSecuritySecure and ((Session = nil) and (request.Document <> URLPath([FPath, OWIN_TOKEN_PATH]))) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + FPath + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
      result := 'Unauthorized';
    end
    else if request.Document = '/.well-known/openid-configuration' then
    begin
      result := 'OAuth Discovery';
      FAuthServer.HandleDiscovery(AContext, request, response)
    end
    else if request.Document.StartsWith(FPath, false) then
      result := HandleRequest(AContext, request, response, true, true, FPath, id, session, cert)
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestVersion(request.Document) then
      result := TerminologyWebServer.ProcessVersion(AContext, request, Session, response, true)
    else if request.Document.StartsWith(FPath + '/cds-services') and FCDSHooksServer.active then
    begin
      result := 'CDS Hooks request';
      FCDSHooksServer.HandleRequest(true, FPath, Session, AContext, request, response)
    end
    else if request.Document = FPath then
    begin
      result := 'Home Page';
      ReturnProcessedFile(request, response, Session, '/hompage.html', Common.SourceProvider.AltFile('/homepage.html', FPath), true)
    end
    else
    begin
      response.ResponseNo := 404;
      response.ContentText := 'Document ' + request.Document + ' not found';
      result := 'Not Found';
    end;
  finally
    session.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  pm: THTTPParameters;
  json: TJsonObject;
  userkey: integer;
  Session: TFHIRSession;
begin
  response.ResponseNo := 400;
  response.ResponseText := 'Request Error';
  if request.contentType <> 'application/x-www-form-urlencoded' then
    response.ContentText := 'Unknown content type - must be application/x-www-form-urlencoded'
  else
  begin
    try
      if request.PostStream <> Nil then
        pm := THTTPParameters.Create(StreamToString(request.PostStream, TEncoding.UTF8))
      else
        pm := THTTPParameters.Create(request.UnparsedParams);
      try
        if pm['grant_type'] <> 'password' then
          response.ContentText := 'Unknown content type - must be ''password'''
        else if not Context.UserProvider.CheckLogin(pm['username'], pm['password'], userkey) then
          response.ContentText := 'Unknown username/password'
        else
        begin
          Session := Context.SessionManager.CreateImplicitSession(request.RemoteIP, pm['username'], 'Anonymous', systemFromOWin, false, true);
          try
            Session.ExternalUserKey := userkey;
            json := TJsonObject.Create;
            try
              json.str['access_token'] := Session.Cookie;
              json.num['expires_in'] := inttostr(trunc((Session.Expires - TFslDateTime.makeUTC.DateTime) / DATETIME_SECOND_ONE));
              json.str['token_type'] := 'bearer';
              response.ResponseNo := 200;
              response.ResponseText := 'OK';
              response.contentType := 'application/json';
              response.ContentText := TJSONWriter.writeObjectStr(json, true);
            finally
              json.Free;
            end;
          finally
            Session.Free;
          end;
        end;
      finally
        pm.Free;
      end;
    except
      on e: exception do
        response.ContentText := e.message;
    end;
  end;
end;

function asLinkHeader(links : TFslStringDictionary) : String;
var
  s : String;
begin
  result := '';
  for s in links.Keys do
  begin
    if (result <> '') then
      result := result +', ';
    result := result + '<'+links[s]+'>;rel='+s;
  end;
end;

function processIfModifiedSince(Value: String): TDateTime;
begin
  if Value <> '' then
    result := GMTToLocalDateTime(Value)
  else
    result := 0;
end;

function processIfMatch(Value: string): String;
begin
  if Value.StartsWith('W/') then
    Value := Value.Substring(2);
  if Value.StartsWith('"') and Value.EndsWith('"') then
    result := copy(Value, 2, Length(Value) - 2)
  else
    result := Value;

end;



function TFhirWebServerCommonEndpoint.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
var
  sHost, token, url: string;
  oRequest: TFHIRRequest;
  oResponse: TFHIRResponse;
  sCookie: string;
  sContentType: String;
  oStream: TStream;
  sDoc: String;
  s: String;
  aFormat: TFHIRFormat;
  lang: THTTPLanguages;
  sPath: String;
  redirect: boolean;
  form: TMimeMessage;
  relativeReferenceAdjustment: integer;
  style : TFHIROutputStyle;
  c: integer;
  domain: String;
  sBearer: String;
  noErrCode : boolean;
  mode : TOperationMode;
  Context: TOperationContext;
  Session: TFHIRSession;
  cache : boolean;
Begin
  result := '??';
  noErrCode := false;
  mode := opmRestful;
  cache := false;

  Session := nil;
  try
    if ssl then
      sHost := 'https://' + request.host
    else
      sHost := 'http://' + request.host;
    domain := request.host;
    if domain.Contains(':') then
      domain := domain.Substring(0, domain.IndexOf(':'));

    lang := THTTPLanguages.Create(request.AcceptLanguage);
    s := request.contentType;
    if pos(';', s) > 0 then
      s := copy(s, 1, pos(';', s) - 1); // only read up to the first ';'
    if SameText(s, 'application/x-www-form-urlencoded') or (request.UnparsedParams <> '') then
      sDoc := request.Document + '?' + request.UnparsedParams
    else
      sDoc := request.Document;
    try
      sContentType := request.contentType;

      if s.StartsWith('multipart/form-data', true) then
      begin
        form := loadMultipartForm(request.PostStream, request.contentType, mode);
      end
      else
        form := nil;
      try
        if s.StartsWith('multipart/form-data', true) then
        begin
          oStream := extractFileData(Lang, form, 'file', sContentType);
          // though this might not return the data if we have an operation request
        end
        else if request.PostStream <> nil then
        begin
          oStream := TMemoryStream.Create;
          oStream.CopyFrom(request.PostStream, request.PostStream.Size);
          oStream.Position := 0;
        end
        else
          oStream := TStringStream.Create(request.UnparsedParams);

        try
          oResponse := TFHIRResponse.Create(self.Context.ValidatorContext.link);
          try
            oResponse.OnCreateBuilder := doGetBundleBuilder;
            response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
            // response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE');
            response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
            // response.CustomHeaders.add('Access-Control-Expose-Headers: *');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
            if request.RawHeaders.Values[REVERSE_HOST_HEADER] <> '' then
              sHost := request.RawHeaders.Values[REVERSE_HOST_HEADER];
            if request.RawHeaders.Values['X-Request-Id'] <> '' then
              response.CustomHeaders.Add('X-Request-Id: ' + request.RawHeaders.Values['X-Request-Id']);
            oRequest := nil;
            Try
              if request.AuthUsername = INTERNAL_SECRET then
                sCookie := request.AuthPassword
              else
              begin
                c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
                if c > -1 then
                  sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1);
              end;

              if not FCache.respond(request, response) then
              begin
                sBearer := sCookie;
                oRequest := BuildRequest(lang, path, sHost, request.CustomHeaders.Values['Origin'], request.RemoteIP,
                  request.CustomHeaders.Values['content-location'], request.Command, sDoc, sContentType, request.Accept, request.ContentEncoding, sCookie,
                  request.RawHeaders.Values['X-Provenance'], sBearer, oStream, oResponse, aFormat, redirect, form, secure, ssl, relativeReferenceAdjustment, style,
                  esession, cert);
                try
                  oRequest.externalRequestId := request.RawHeaders.Values['X-Request-Id'];
                  oRequest.internalRequestId := logId;
                  if TFHIRWebServerClientInfo(AContext.Data).Session = nil then
                    TFHIRWebServerClientInfo(AContext.Data).Session := oRequest.Session.link;

                  oRequest.IfMatch := processIfMatch(request.RawHeaders.Values['If-Match']);
                  oRequest.IfNoneMatch := processIfMatch(request.RawHeaders.Values['If-None-Match']);
                  oRequest.IfNoneExist := request.RawHeaders.Values['If-None-Exist'];
                  oRequest.IfModifiedSince := processIfModifiedSince(request.RawHeaders.Values['If-Modified-Since']);
                  oRequest.strictSearch := request.RawHeaders.Values['Prefer'] = 'handling=strict';

                  noErrCode := StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters['nohttperr']) or
                    StringArrayExistsInsensitive(['yes', 'true', '1'], oRequest.Parameters['_nohttperr']);
                  ReadTags(request.RawHeaders.Values['Category'], oRequest);
                  Session := oRequest.Session.link;

                  // allow scripting to change anything about the request
                  {$IFNDEF NO_JS}
                  GJsHost.previewRequest(Session, oRequest);
                  {$ENDIF}

                  if redirect then
                  begin
                    if oRequest.Session <> nil then
                    begin
                      FAuthServer.setCookie(response, FHIR_COOKIE_NAME, oRequest.Session.Cookie, domain, '', oRequest.Session.Expires, false);
                      cacheResponse(response, cacheNotAtAll);
                      response.redirect(oRequest.Session.OriginalUrl);
                    end
                    else if request.unparsedParams.contains('error=') then // oAuth failure
                      response.redirect(oRequest.baseUrl+'?'+request.unparsedParams)
                    else
                    begin
                      token := oRequest.parameters['state'];
                      if FTokenRedirects.getRedirect(token, url) then
                        response.redirect(url)
                      else
                        response.redirect(oRequest.baseUrl);
                    end;
                  end
                  else if oRequest.CommandType = fcmdNull then
                  begin
                    response.CustomHeaders.Add('Access-Control-Request-Method: GET, POST, PUT, PATCH, DELETE');
                    cacheResponse(response, cacheNormal);
                  end
                  else if oRequest.CommandType = fcmdUnknown then
                  begin
                    cacheResponse(response, oResponse.CacheControl);
                    if oResponse.format = ffXhtml then
                    begin
                      response.ResponseNo := 200;
                      response.contentType := 'text/html; charset=UTF-8';
                      response.FreeContentStream := true;
                      response.ContentStream := StringToUTF8Stream(BuildFhirHomePage(oRequest.SessionCompartments, logId, lang, sHost, path, oRequest.Session, secure));
                    end
                    else
                    begin
                      response.ResponseNo := 404;
                      response.ContentText := 'Document ' + request.Document + ' not found';
                    end;
                  end
                  else if (oRequest.CommandType = fcmdUpload) and (oRequest.resource = nil) Then
                  begin
                    cacheResponse(response, oResponse.CacheControl);
                    response.ResponseNo := 200;
                    response.contentType := 'text/html; charset=UTF-8';
                    response.FreeContentStream := true;
                    response.ContentStream := StringToUTF8Stream(BuildFhirUploadPage(lang, sHost, '', oRequest.ResourceName, oRequest.Session));
                  end
                  else if (oRequest.CommandType = fcmdMetadata) and (oRequest.ResourceName <> '') then
                  begin
                    cacheResponse(response, oResponse.CacheControl);
                    response.ResponseNo := 200;
                    response.contentType := 'text/html; charset=UTF-8';
                    // no - just use *              response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
                    response.CustomHeaders.Add('Access-Control-Request-Method: GET, POST, PUT, PATCH, DELETE');
                    response.FreeContentStream := true;
                    response.ContentStream := StringToUTF8Stream('OK');
                  end
                  else
                  begin
                    try
                      Context := TOperationContext.Create;
                      try
                        Context.mode := mode;
                        checkRequestByJs(context, oRequest);
                        if (oRequest.CommandType = fcmdOperation) then
                          Common.Google.recordEvent(request.Document, '$'+oRequest.OperationName, oRequest.Session.UserName, request.RemoteIP, request.UserAgent)
                        else
                          Common.Google.recordEvent(request.Document, CODES_TFHIRCommandType[oRequest.CommandType], oRequest.Session.UserName, request.RemoteIP, request.UserAgent);

                        if oRequest.CommandType = fcmdWebUI then
                        begin
                          result := 'Web Request';
                          HandleWebUIRequest(oRequest, oResponse, secure)
                        end
                        else if oRequest.commandType in [fcmdTask, fcmdDeleteTask] then
                          result := ProcessTaskRequest(Context, oRequest, oResponse)
                        else if (request.RawHeaders.Values['Prefer'] = 'respond-async') or (oRequest.Parameters['_async'] = 'true') then
                          result := ProcessAsyncRequest(Context, oRequest, oResponse)
                        else
                          result := ProcessRequest(Context, oRequest, oResponse);
                        cache := context.CacheResponse;
                      finally
                        Context.Free;
                      end;
                    except
                      on e: EAbort do
                      begin
                        if oResponse.HTTPCode < 300 then
                        begin
                          recordStack(e);
                          raise;
                        end;
                      end;
                      on e: exception do
                      begin
                        recordStack(e);
                        raise;
                      end;
                    end;
                    cacheResponse(response, oResponse.CacheControl);
                    self.Context.Storage.RecordExchange(oRequest, oResponse, nil);
                    ProcessOutput(oRequest, oResponse, request, response, relativeReferenceAdjustment, style, request.AcceptEncoding.Contains('gzip'), cache);
                    // no - just use *              if request.RawHeaders.Values['Origin'] <> '' then
                    // response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
                    if oResponse.versionId <> '' then
                      response.ETag := 'W/"' + oResponse.versionId + '"';
                    response.LastModified := oResponse.lastModifiedDate;
                    // todo: timezone
                    response.CustomHeaders.Add('X-GDPR-Disclosure: All access to this server is logged as AuditEvent Resources, and these store your ip address '+
                      '(and logged in user, if one exists). Also, your IP address is logged with Google Analytics for building geomaps of server usage. Your continued '+
                      'use of the API constitutes agreement to these terms. See [link] for erasure requests');
                    if oResponse.tags.Count > 0 then
                      response.CustomHeaders.Add('Category: ' + oResponse.tags.AsHeader);
                    if oResponse.links.Count > 0 then
                      response.CustomHeaders.Add('Link: ' + asLinkHeader(oResponse.links));
                    if oResponse.originalId <> '' then
                      response.CustomHeaders.Add('X-Original-Location: ' + oResponse.originalId);
                    if oResponse.Progress <> '' then
                      response.CustomHeaders.Add('X-Progress: ' + oResponse.Progress);
                    if oResponse.ContentLocation <> '' then
                      response.CustomHeaders.Add('Content-Location: ' + oResponse.ContentLocation);
                    if oResponse.Location <> '' then
                      response.Location := oResponse.Location;
                  end;
                  if noErrCode then
                    response.ResponseNo := 200;
                except
                  on e: exception do
                  begin
                    self.Context.Storage.RecordExchange(oRequest, oResponse, e);
                    raise;
                  end;
                end;
              end;
            finally
              oRequest.Free;
            end;
          Finally
            oResponse.Free;
          End;
        finally
          oStream.Free;
        end;
      finally
        form.Free;
      end;
    except
      on e: ERestfulAuthenticationNeeded do
      begin
        if aFormat = ffXhtml then
        begin
          response.ResponseNo := 200;
          response.contentType := 'text/html; charset=UTF-8';
          response.FreeContentStream := true;
          response.ContentStream := StringToUTF8Stream(BuildFhirAuthenticationPage(lang, sHost, sPath + sDoc, logId, e.Msg, ssl, request.unparsedParams));
        end
        else
          SendError(response, logId, e.status, aFormat, lang, e.message, sPath, e, Session, true, sPath + sDoc, relativeReferenceAdjustment, itLogin);
      end;
      on e: ETerminologyError do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, itNotSupported)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            itNotSupported);
      end;
      on e: ETerminologySetup do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, itNotSupported)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            itNotSupported);
      end;
      on e: ETooCostly do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, itTooCostly)
        else
          SendError(response, logId, HTTP_ERR_BUSINESS_RULES_FAILED, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment,
            itTooCostly);
      end;
      on e: ERestfulException do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, e.code)
        else
          SendError(response, logId, e.status, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, e.code);
      end;
      on e: exception do
      begin
        if noErrCode then
          SendError(response, logId, 200, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, itNull)
        else
          SendError(response, logId, HTTP_ERR_INTERNAL, aFormat, lang, e.message, sPath, e, Session, false, path, relativeReferenceAdjustment, itNull);
      end;
    end;
  finally
    Session.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.startHooks(ctxt: TFHIRWebServerPatientViewContext; patient: TFHIRPatientW; url: String);
var
  server: TRegisteredFHIRServer;
  req: TCDSHookRequest;
  s, u, i : String;
  be: TFHIRBundleEntryW;
begin
  for s in PatientViewServers.Keys do
  begin
    server := TRegisteredFHIRServer.Create;
    try
      server.name := s;
      StringSplit(PatientViewServers[s], '|', u, i);
      server.fhirEndpoint := u;
      server.addCdsHook(i, TCDSHooks.patientView);
      ctxt.manager.registerServer(server);
    finally
      server.Free;
    end;
  end;

  req := TCDSHookRequest.Create;
  try
    req.hook := TCDSHooks.patientView;
    req.hookInstance := ctxt.FContext.FormalURLPlain; // arbitrary global
    req.patient := patient.id;
    be := factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
    req.preFetch.Add('patient', be);
    be.resource := patient.Resource.link;
    ctxt.manager.makeRequest(req, OnCDSResponse, ctxt);
  finally
    req.Free;
  end;
end;



{$IFDEF WINDOWS}

function TFhirWebServerCommonEndpoint.HandleWebCreate(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  profile: TFhirStructureDefinition;
//  builder: TQuestionnaireBuilder;
//  questionnaire: TFHIRQuestionnaire;
//  s, id, fid: String;
begin
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
//          questionnaire.Free;
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
//        builder.Free;
//      end;
//    end;
//
//    response.Body := s;
//    result := Now; // don't want anyone caching anything
//    response.contentType := 'text/html; charset=UTF-8';
//  finally
//    profile.Free;
//  end;
  raise ETodo.create('TFhirWebServerCommonEndpoint.HandleWebCreate');
end;
{$ENDIF}
function TFhirWebServerCommonEndpoint.HandleWebEdit(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  typ, id, ver: String;
//  r: TFhirResource;
//  s: String;
//  comp: TFHIRComposer;
begin
  raise ETodo.create('TFhirWebServerCommonEndpoint.HandleWebEdit');
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
//        comp.Free;
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
//    r.Free;
//  end;
end;

function TFhirWebServerCommonEndpoint.HandleWebPost(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  s, typ, id, ver: String;
  p: THTTPParameters;
  prsr: TFHIRParser;
  Context: TOperationContext;
begin
  StringSplit(request.id, '/', typ, s);
  StringSplit(s, '/', id, ver);
  request.id := id;
  if not StringArrayExistsSensitive(factory.ResourceNames, typ) {or FContext.ValidatorContext.hasCustomResource(typ))} then
    raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.lang, [typ]);
  request.ResourceName := typ;
  request.CommandType := fcmdUpdate;

  Context := TOperationContext.Create;
  try
    p := THTTPParameters.Create(TEncoding.UTF8.GetString(request.Source.AsBytes), true);
    try
      if p['srcformat'] = 'json' then
        prsr := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffJson, request.lang)
      else
        prsr := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffXml, request.lang);
      try
        s := p['source'];
        prsr.Source := TStringStream.Create(s, TEncoding.UTF8);
        try
          prsr.Parse;
          request.resource := prsr.resource.link;
          checkRequestByJs(context, request);
          ProcessRequest(Context, request, response);
          if response.HTTPCode < 300 then
          begin
            response.HTTPCode := 303;
            response.Location := request.baseUrl + typ + '/' + id;
          end;
        finally
          prsr.Source.Free;
        end;
      finally
        prsr.Free;
      end;
    finally
      p.Free;
    end;
    result := 0;
  finally
    Context.Free;
  end;
end;

{$IFDEF WINDOWS}

function TFhirWebServerCommonEndpoint.HandleWebProfile(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  id, ver, fullid: String;
//  profile: TFhirStructureDefinition;
//  builder: TQuestionnaireBuilder;
//  questionnaire: TFHIRQuestionnaire;
//  s: String;
begin
  raise ETodo.create('TFhirWebServerCommonEndpoint.HandleWebProfile');
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
//          questionnaire.Free;
//        end;
//      finally
//        builder.Free;
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
//    profile.Free;
//  end;
end;
{$ENDIF}

function TFhirWebServerCommonEndpoint.HandleWebPatient(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id, ver: String;
  s, xhtml: String;
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
  patient := factory.wrapPatient(GetResource(request.Session, 'Patient', request.lang, id, ver, ''));
  try
    xhtml := factory.getXhtml(patient.Resource).AsPlainText;
 //   startHooks(hooks, patient, request.baseUrl);
  finally
    patient.Free;
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

function TFhirWebServerCommonEndpoint.HandleWebEncounter(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id, ver: String;
  s, xhtml: String;
  encounter: TFHIREncounterW;
  hookid: String;
  hooks: TFHIRWebServerPatientViewContext;
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
//    encounter.Free;
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

function TFhirWebServerCommonEndpoint.HandleWebPatientHooks(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
var
  id: String;
  // s, xhtml : String;
  // patient : TFHIRPatient;
  // hookid : String;
  hooks: TFHIRWebServerPatientViewContext;

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
//      hooks.Free;
//    end;
//  end;
result := 0;
end;

{$IFDEF WINDOWS}
function TFhirWebServerCommonEndpoint.HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
var
  id, ver: String;
  questionnaire: TFHIRResourceV;
  s: String;
begin
  // get the right questionnaire
  StringSplit(request.id.Substring(14), '/', id, ver);
  questionnaire := GetResource(request.Session, 'Questionnaire', request.lang, id, ver, '');
  try
    // convert to xhtml
    s := transform1(questionnaire, request.lang, 'QuestionnaireToHTML.xslt', false);
    // insert page headers:
    s := s.Replace('<!--header insertion point-->', TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<!--body top insertion point-->', TFHIRXhtmlComposer.header(factory, request.Session, request.baseUrl, request.lang, SERVER_VERSION));
    s := s.Replace('<!--body bottom insertion point-->', TFHIRXhtmlComposer.Footer(factory, request.baseUrl, request.lang, request.internalRequestId, false));
    s := s.Replace('var questionnaireAnswersEndpoint = null;', 'var questionnaireAnswersEndpoint = "' + request.baseUrl + '/QuestionnaireAnswers";');
    response.Body := s;
    result := Now; // don't want anyone caching anything
    response.contentType := 'text/html; charset=UTF-8';
  finally
    questionnaire.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.HandleWebQuestionnaireInstance(request: TFHIRRequest; response: TFHIRResponse): TDateTime;
//var
//  typ, id, ver: String;
//  r: TFhirResourceV;
//  qa: TFhirQuestionnaireResponse;
//  q: TFHIRQuestionnaire;
//  s, j: String;
//  json: TFHIRJsonComposer;
begin
  raise ETodo.create('TFhirWebServerCommonEndpoint.HandleWebQuestionnaireInstance');
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
//        json.Free;
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
//    r.Free;
//  end;
end;
{$ENDIF}

procedure TFhirWebServerCommonEndpoint.HandleWebSockets(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String);
var
  ws: TIdWebSocket;
begin
  ws := TIdWebSocket.Create(nil);
  try
    if ws.open(AContext, request, response) then
      self.Context.SubscriptionManager.HandleWebSocket(ws);
  finally
    ws.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
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
  else if request.id = 'Create' then
    result := HandleWebCreate(request, response)
{$ENDIF}
  else if request.id.StartsWith('PatientHooks/') then
    result := HandleWebPatientHooks(request, response, secure)
  else if request.id.StartsWith('Patient/') then
    result := HandleWebPatient(request, response, secure)
  else if request.id.StartsWith('Encounter/') then
    result := HandleWebEncounter(request, response, secure)
  else
    raise EFHIRException.CreateLang('MSG_UNKNOWN_CONTENT', request.lang, [request.id, 'web UI']);
end;

procedure TFhirWebServerCommonEndpoint.SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; const lang : THTTPLanguages; message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFhirIssueType);
var
  issue: TFhirOperationOutcomeW;
  oComp: TFHIRComposer;
  iss : TFhirOperationOutcomeIssueW;
begin
  response.ResponseNo := status;
  response.FreeContentStream := true;
  if format = ffUnspecified then
  begin
    response.contentType := 'text/plain';
    response.ContentStream := StringToUTF8Stream(message);
  end
  else
  begin
    issue := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
    try
      factory.setXhtml(issue.Resource, TFHIRXhtmlParser.Parse(lang, xppReject, [], '<div><p>' + FormatTextToXML(message, xmlText) + '</p></div>'));
      iss := factory.makeIssue(isError, code, '', message);
      try
        iss.diagnostics := ExceptionStack(e);
        issue.addIssue(iss, false);
      finally
        iss.Free;
      end;
      response.ContentStream := TMemoryStream.Create;
      oComp := nil;
      case format of
        ffXml:
          oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffXml, lang, OutputStyleNormal);
        ffXhtml:
          begin
            oComp := TFHIRXhtmlComposer.Create(self.Context.ValidatorContext.link, OutputStyleNormal, lang, AppendForwardSlash(url));
            TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
            TFHIRXhtmlComposer(oComp).Session := Session.link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          end;
        ffJson, ffNDJson:
          oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffJson, lang, OutputStyleNormal);
        ffText:
          oComp := TFHIRTextComposer.Create(self.Context.ValidatorContext.link, OutputStyleNormal, lang);
      end;
      try
        response.contentType := oComp.MimeType;
        oComp.LogId := logId;
        oComp.Compose(response.ContentStream, issue.Resource);
        response.ContentStream.Position := 0;
      finally
        oComp.Free;
      end;
    finally
      issue.Free;
    end;
  end;
end;

procedure TFhirWebServerCommonEndpoint.SetAuthServer(const Value: TAuth2Server);
begin
  FAuthServer.Free;
  FAuthServer := Value;
end;

function extractProp(contentType, name: String): string;
begin
  if contentType.Contains(name + '=') then
  begin
    result := contentType.Substring(contentType.IndexOf(name + '=') + name.Length + 1);
    if result.Contains(';') then
      result := result.Substring(0, result.IndexOf(';'));
  end
  else
    result := '';
end;

procedure TFhirWebServerCommonEndpoint.StopAsyncTasks;
var
  task : TAsyncTaskThread;
  done : boolean;
  i : integer;
  ep : TFhirWebServerCommonEndpoint;
begin
  done := false;
  Common.Lock.Lock;
  try
    for task in FThreads do
    begin
      task.Terminate;
      done := true;
    end;
  finally
    Common.Lock.Unlock;
  end;
  if done then
  begin
    i := 0;
    repeat
      sleep(100);
      inc(i);
      done := true;
      Common.Lock.Lock;
      try
        for task in FThreads do
          done := false;
      finally
        Common.Lock.Unlock;
      end;
    until done or (i = 10);
    if not done then
    begin
      Common.Lock.Lock;
      try
        for task in FThreads do
        begin
          task.kill;
          Context.Storage.updateAsyncTaskStatus(task.Key, atsTerminated, 'Terminated due to system shut down');
        end;
      finally
        Common.Lock.Unlock;
      end;
    end;
  end;
end;


Function TFhirWebServerCommonEndpoint.BuildRequest(const lang : THTTPLanguages; sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept,
  sContentEncoding, sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse; var aFormat: TFHIRFormat; var redirect: boolean;
  form: TMimeMessage; bAuth, secure: boolean; out relativeReferenceAdjustment: integer; var style : TFHIROutputStyle; Session: TFHIRSession; cert: TIdOpenSSLX509)
  : TFHIRRequest;
Var
  sURL, Msg: String;
  oRequest: TFHIRRequest;
  parser: TFHIRParser;
  check: boolean;
  comp: TIdCompressorZLib;
  mem: TMemoryStream;
  cursor: integer;
  bundle: TFHIRBundleW;
  b : TBytes;
Begin

  relativeReferenceAdjustment := 0;
  oRequest := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
  try
    oRequest.lang := lang;
    oResponse.origin := sOrigin;
    oRequest.PostFormat := ffUnspecified;
    oResponse.format := ffUnspecified;
    oRequest.secure := secure;
    aFormat := ffUnspecified;
    oRequest.baseUrl := sHost + AppendForwardSlash(sBaseURL);
    oRequest.url := sHost + sResource;
    oRequest.lastModifiedDate := 0; // Xml
    // oRequest.contentLocation := sContentLocation; // for version aware updates
    oRequest.form := form.link;
    oRequest.provenance := processProvenanceHeader(provenance, lang);

    If Not StringStartsWithSensitive(sResource, sBaseURL) Then
    begin
      if StringStartsWith(sResource, '/images/', false) then
        Raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_NOTFOUND, itNotFound, 'images not served', lang)
      else
        Raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_NOTFOUND, itNotFound, 'MSG_NO_MODULE', lang, [sResource]);
    end;

    sURL := copy(sResource, Length(sBaseURL) + 1, $FFF);
    sURL := oRequest.preanalyse(sURL);

    if (sCommand <> 'GET') then
    begin
      oRequest.Version := readVersion(sContentType);
      oRequest.PostFormat := mimeTypeToFormat(sContentType, oRequest.PostFormat);
      if (sContentType <> 'application/x-www-form-urlencoded') and oRequest.Parameters.has('_format') and (form = nil) and (oRequest.Parameters['_format'] <> '') then
        sContentType := oRequest.Parameters['_format'];
    end;

    oResponse.Version := readVersion(sContentAccept);
    if oRequest.Parameters.has('_format') and (oRequest.Parameters['_format'] <> '') then
      sContentAccept := oRequest.Parameters['_format'];
    oResponse.format := mimeTypeListToFormat(sContentAccept, oResponse.Format);
    if oRequest.Parameters.has('_pretty') and (oRequest.Parameters['_pretty'] = 'true') then
      style := OutputStylePretty
    else if sContentAccept.Contains('pretty=') and (extractProp(sContentAccept, 'pretty') = 'true') then
      style := OutputStylePretty
    else
      style := OutputStyleNormal;

    aFormat := oResponse.format;

    // ok, now that we've read the content types, security check
    if bAuth and not(sURL = 'metadata') then
    begin
      if sURL = 'logout' then
      begin
        self.Context.SessionManager.EndSession(sCookie, sClient);
        oRequest.Session := nil;
        redirect := true;
      end
      else if (sURL = 'internal') then
        redirect := true
      else if (Session <> nil) and self.Context.SessionManager.isOkSession(Session) then
        oRequest.Session := Session.link
      else if (sURL <> 'auth-login') and self.Context.SessionManager.GetSession(sCookie, Session, check) then
      begin
        if check and not CheckSessionOK(Session, sClient) then
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', Msg, lang);
        oRequest.Session := Session
      end
      else if (secure and self.Context.SessionManager.isOkBearer(sBearer, sClient, Session)) then
        oRequest.Session := Session
      else
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', Msg, lang);
    end
    else if cert <> nil then
      oRequest.Session := self.Context.SessionManager.CreateImplicitSession(sClient, cert.Subject.CN, 'Anonymous', systemFromCertificate, false, false)
    else
      oRequest.Session := self.Context.SessionManager.CreateImplicitSession(sClient, 'Unknown', 'Anonymous', systemUnknown, false, false);

    if not redirect then
    begin
      oRequest.analyse(sCommand, sURL, relativeReferenceAdjustment, FAdaptors);

      if (oRequest.CommandType <> fcmdNull) then
      begin
        if (oRequest.CommandType in [fcmdTransaction, fcmdBatch, fcmdUpdate, fcmdPatch, fcmdValidate, fcmdCreate]) or
          ((oRequest.CommandType in [fcmdUpload, fcmdSearch, fcmdWebUI, fcmdOperation]) and (sCommand = 'POST') and (oPostStream <> nil) and
          (oPostStream.Size > 0)) or ((oRequest.CommandType in [fcmdDelete]) and ((sCommand = 'DELETE')) and (oPostStream <> nil) and (oPostStream.Size > 0) and
          (sContentType <> '')) Then
        begin
          oRequest.CopyPost(oPostStream);
          if (sContentType = 'application/x-zip-compressed') or (sContentType = 'application/zip') then
          begin
            bundle := ProcessZip(lang, oPostStream, NewGuidURN, 'http://hl7.org/fhir', false, nil, nil, cursor);
            try
              oRequest.resource := bundle.Resource.link;
            finally
              bundle.Free;
            end;
          end
          else
          begin
            oRequest.Source := TFslBuffer.Create;
            if sContentEncoding = 'gzip' then
            begin
              mem := TMemoryStream.Create;
              comp := TIdCompressorZLib.Create(nil);
              try
                comp.DecompressStream(oPostStream, mem, 0);
                mem.Position := 0;
                oRequest.Source.LoadFromStream(mem);
              finally
                comp.Free;
                mem.Free;
              end;
            end
            else
              oRequest.Source.LoadFromStream(oPostStream);

            oPostStream.Position := 0;
            if (oRequest.ResourceName = 'Binary') and (oRequest.PostFormat = ffUnspecified) then
            begin
              SetLength(b, oPostStream.Size - oPostStream.Position);
              if oPostStream.Size - oPostStream.Position > 0 then
                oPostStream.Read(b[0], oPostStream.Size - oPostStream.Position);
              oRequest.resource := factory.makeBinary(b, sContentType);
            end
            else if (oRequest.Adaptor <> nil) then
              oRequest.Adaptor.Load(oRequest, oPostStream)
            else if (oRequest.CommandType = fcmdPatch) and (sContentType = 'application/json-patch+json') then
            begin
              oRequest.patchJson := TJsonParser.ParseNode(oPostStream) as TJsonArray
            end
            else if (oRequest.CommandType = fcmdPatch) and (sContentType = 'application/xml-patch+xml') then
            begin
              oRequest.patchXml := TMXmlParser.Parse(oPostStream, [xpResolveNamespaces]);
            end
            else if (oRequest.CommandType in [fcmdOperation, fcmdSearch]) and (sContentType = 'application/x-www-form-urlencoded') then
            begin
              oRequest.resource := factory.makeParamsFromForm(oPostStream);
            end
            else if (oRequest.CommandType = fcmdOperation) and (sContentType = 'application/graphql') then
            begin
              oRequest.GraphQL := TGraphQLParser.Parse(oPostStream);
            end
            else if (oRequest.CommandType = fcmdOperation) and (oRequest.OperationName = 'graphql') and (sContentType = 'application/json') then
            begin
              oRequest.GraphQL := TGraphQLParser.parseJson(oPostStream);
            end
            else if oRequest.CommandType <> fcmdWebUI then
              try
                if oRequest.Version <> factory.version then
                begin
                  // todo FWebServer.convertFromVersion(oPostStream, oRequest.PostFormat, oRequest.Version, oRequest.Lang);
                  oRequest.CopyPost(oPostStream);
                end;

                if oRequest.PostFormat = ffUnspecified then
                  oRequest.PostFormat := detectFormat(oPostStream);

                if (oRequest.Version = fhirVersionRelease4) and (oRequest.PostFormat = ffunspecified) then
                  Raise ERestfulException.Create('TFhirWebServerCommonEndpoint.BuildRequest', HTTP_ERR_NOT_UNSUPPORTED_MEDIA_TYPE, itUnknown, 'Unsupported media type: '+sContentType, lang);

                parser := factory.makeParser(self.Context.ValidatorContext.link, oRequest.PostFormat, lang);
                try
                  oRequest.resource := parser.parseresource(oPostStream);

                  if (oRequest.CommandType = fcmdTransaction) and (oRequest.resource.fhirType <> 'Bundle') then
                  begin
                    bundle := factory.wrapBundle(factory.makeResource('Bundle'));
                    try
                      bundle.type_ := btTransactionResponse;
                      oRequest.resource.id := FhirGUIDToString(CreateGUID);
                      bundle.addEntry('', oRequest.resource.link);
                      oRequest.resource := bundle.Resource.link;
                    finally
                      bundle.Free;
                    end;
                  end;
                finally
                  parser.Free;
                end;
              except
                on e: exception do
                  if oRequest.CommandType = fcmdValidate then
                    oResponse.message := e.message
                  else
                  begin
                    recordStack(e);
                    raise;
                  end;
              end;
          end;
        end;
      end;
    end;

    if (oRequest.Version = fhirVersionRelease4) and (oResponse.Format = ffunspecified) then
      Raise ERestfulException.Create('TFhirWebServerCommonEndpoint.BuildRequest', HTTP_ERR_NOT_ACCEPTABLE, itUnknown, 'Accept header not supported: '+sContentAccept, lang);

    result := oRequest.link;
  Finally
    oRequest.Free;
  End;
End;

function TFhirWebServerCommonEndpoint.buildSessionsTable: String;
begin
  result := self.Context.SessionManager.buildTable;
end;

Function TFhirWebServerCommonEndpoint.ProcessZip(const lang : THTTPLanguages; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW;
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
        rdr.Free;
        carry.Free;
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
        if Context <> nil then
          Context.progress(trunc(100 * (i - istart) / (iend - istart)));
        if not StringArrayExistsInsensitive(['.info', '.internals', '.zip'], extractFileExt(rdr.Parts[i].name)) then
        begin
          if DebugConsoleMessages then
            writeln('Parse ' + rdr.Parts[i].name);
          if (rdr.Parts[i].name <> 'package.json') then
          begin
            if rdr.Parts[i].name.EndsWith('.json') then
              p := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffJson, lang)
            else if rdr.Parts[i].name.EndsWith('.map') then
              p := TFHIRTextParser.Create(self.Context.ValidatorContext.link, lang)
            else
              p := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffXml, lang);
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
                  bnd.Free;
                end;
              end
              else if (p.resource.fhirType <> 'Parameters') and ok(p.resource) then
              begin
                result.addEntry('', p.resource.link);
              end;
            finally
              p.Source.Free;
              p.Free;
            end;
          end;
        end;
      end;
      if iend < rdr.Parts.Count - 1 then
        cursor := iend + 1
      else
        cursor := -1;
    finally
      rdr.Free;
    end;
    result.link;
  finally
    result.Free;
    inc.Free;
  end;
end;

Procedure TFhirWebServerCommonEndpoint.ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;
  relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip, cache: boolean);
var
  oComp: TFHIRComposer;
  b: TBytes;
  stream: TStream;
  ownsStream: boolean;
  comp: TIdCompressorZLib;
  Body: boolean;
  res: TFhirResourceV;
  bin : TFHIRBinaryW;
  meta : TFhirMetaW;
begin
  ownsStream := false;
  gzip := false;
  response.ResponseNo := oResponse.HTTPCode;
  response.contentType := oResponse.contentType;
  res := oResponse.resource;
  if (res = nil) and (oResponse.outcome <> nil) then
    res := oResponse.outcome.Resource;
  Body := (request.Command = 'GET') or (request.RawHeaders.Values['Prefer'] <> 'return=minimal') or (oResponse.format = ffXhtml);
  if Body and (request.RawHeaders.Values['Prefer'] = 'return=OperationOutcome') and (oResponse.outcome <> nil) then
    res := oResponse.outcome.Resource;

  if oResponse.Stream <> nil then
    stream := TVCLStream.Create(oResponse.Stream.Link)
  else
    stream := TMemoryStream.Create;
  try
    ownsStream := true;
    if res <> nil then
    Begin
      if Body then
      begin
        if res.fhirType = 'Binary' then
        begin
          bin := factory.wrapBinary(res.link);
          try
            b := bin.content;
            if (Length(b) > 0) and (Body) then
              stream.Write(b[0], Length(b));
            stream.Position := 0;
            response.contentType := bin.contentType;
            if StrToBoolDef(oRequest.Parameters['no-attachment'], false) then
              response.ContentDisposition := 'attachment;';
            response.Expires := Now + 0.25;
          finally
            bin.Free;
          end;
        end
        // special $versions support
        else if (oResponse.format = ffJson) and (request.Accept = 'application/json') and (oResponse.Resource.fhirType = 'Parameters') and (oRequest.OperationName = 'versions') then
        begin
          response.contentType := 'application/json';
          b := EncodeVersionsJson(oResponse.Resource);
          stream.Write(b, length(b));
        end
        else if (oResponse.format = ffXml) and (request.Accept = 'application/xml') and (oResponse.Resource.fhirType = 'Parameters') and (oRequest.OperationName = 'versions') then
        begin
          response.contentType := 'application/xml';
          b := EncodeVersionsXml(oResponse.Resource);
          stream.Write(b, length(b));
        end
        else if (oRequest.Adaptor <> nil) then
        begin
          oRequest.Adaptor.Compose(oResponse, stream);
          response.contentType := oRequest.Adaptor.MimeType;
        end
        else
        begin
          // response.Expires := Now; //don't want anyone caching anything
          response.Pragma := 'no-cache';
          if oResponse.format = ffJson then
            oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffJson, oRequest.lang, style)
          else if oResponse.format = ffXhtml then
          begin
            oComp := TFHIRXhtmlComposer.Create(self.Context.ValidatorContext.link, style, oRequest.lang, oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).baseUrl := AppendForwardSlash(oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).Version := SERVER_VERSION;
            TFHIRXhtmlComposer(oComp).Session := oRequest.Session.link;
            TFHIRXhtmlComposer(oComp).tags := oResponse.tags.link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
            TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
            TFHIRXhtmlComposer(oComp).OperationName := oRequest.OperationName;
            TFHIRXhtmlComposer(oComp).Links := oResponse.links.link;
            // response.Expires := 0;
            response.Pragma := '';
          end
          else if oResponse.format = ffNDJson then
            oComp := TFHIRNDJsonComposer.Create(self.Context.ValidatorContext.link, style, oRequest.lang)
          else if oResponse.format = ffXml then
            oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffXml, oRequest.lang, style)
          else if oResponse.format = ffText then
            oComp := TFHIRTextComposer.Create(self.Context.ValidatorContext.link, style, oRequest.lang)
          else if (factory.version <> fhirVersionRelease2) and ((oResponse.format = ffTurtle) or (res._source_format = ffTurtle)) then
          begin
            oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffTurtle, oRequest.lang, style);
            if (res <> nil) and (res.id <> '') then
              TFHIRTurtleComposerBase(oComp).url := AppendForwardSlash(oRequest.baseUrl) + res.fhirType + '/' + res.id;
          end
          else if res._source_format = ffJson then
            oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffJson, oRequest.lang, style)
          else
            oComp := factory.makeComposer(self.Context.ValidatorContext.link, ffXml, oRequest.lang, style);
          try
            response.contentType := oComp.MimeType;
            oComp.SummaryOption := oRequest.Summary;
            oComp.ElementToCompose.Assign(oRequest.Elements);
            if (oComp.ElementToCompose.Count > 0) or (oComp.SummaryOption in [soSummary, soText, soData]) then
            begin
              meta := factory.wrapMeta(res);
              try
                if (res.fhirType <> 'Bundle') and not meta.HasTag('http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED') then
                  meta.addTag('http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED', 'Subsetted');
              finally
                meta.Free;
              end;
            end;

            oComp.LogId := oRequest.internalRequestId;
            oComp.Compose(stream, res);
          finally
            oComp.Free;
          end;
// todo         if oResponse.Version <> factory.version then
//            FWebServer.convertToVersion(stream, oResponse.Format, oResponse.Version, oRequest.lang);
        end;
      end
    end
    else if oResponse.Stream = nil then
    begin
      if response.contentType = '' then
        response.contentType := 'text/plain';
      b := TEncoding.UTF8.GetBytes(oResponse.Body);
      stream.Write(b, Length(b));
    end;
    stream.Position := 0;
    if gzip and (stream.Size > 0) then
    begin
      response.ContentStream := TMemoryStream.Create;
      comp := TIdCompressorZLib.Create(nil);
      try
        comp.CompressStream(stream, response.ContentStream, 9, GZIP_WINBITS, 9, 0);
      finally
        comp.Free;
      end;
      response.ContentStream.Position := 0;
      response.ContentEncoding := 'gzip';
    end
    else
    begin
      response.ContentStream := stream;
      ownsStream := false;
    end;
    if (cache) then
      FCache.recordResponse(request, response);
  finally
    if ownsStream then
      stream.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.processProvenanceHeader(header : String; const lang : THTTPLanguages): TFhirProvenanceW;
var
  json: TFHIRParser;
  ss: TStringStream;
begin
  if header = '' then
    result := nil
  else
  begin
    ss := TStringStream.Create(header, TEncoding.UTF8);
    try
      json := self.Context.Factory.makeParser(self.Context.ValidatorContext.Link, ffJson, lang);
      try
        json.Source := ss;
        json.Parse;
        result := factory.wrapProvenance(json.resource.link);
      finally
        json.Free;
      end;
    finally
      ss.Free;
    end;
  end;
end;

function TFhirWebServerCommonEndpoint.processRegistration(request: TIdHTTPRequestInfo; session : TFhirSession): String;
var
  pm : THTTPParameters;
  client : TRegisteredClientInformation;
  s : String;
  jwks : TJWKList;
  json : TJsonObject;
  lang : THTTPLanguages;
begin
  lang := THTTPLanguages.Create(request.AcceptLanguage);

  if session = nil then
    raise EFHIRException.Createlang('MSG_AUTH_REQUIRED', lang);

  pm := THTTPParameters.create(request.UnparsedParams);
  try
    client := TRegisteredClientInformation.Create;
    try
      client.name := pm['client_name'].Trim;
      if client.name = '' then
        raise EFHIRException.Createlang('INFO_MISSING', lang, ['client_name']);
      client.url := pm['client_uri'].Trim;
      client.logo := pm['logo_uri'].Trim;
      client.softwareId := pm['software_id'].Trim;
      client.softwareVersion := pm['software_version'].Trim;
      client.PatientContext := pm['ctxt-patient'] <> '';
      case StrToIntDef(pm['mode'], 0) of
        1: begin
           client.mode := rcmOAuthClient;
           client.secret := NewGuidId;
           client.redirects.Text := pm['redirect_uris'];
           end;
        2: begin
           client.mode := rcmOAuthClient;
           client.redirects.Text := pm['redirect_uris'];
           end;
        3: begin
           client.mode := rcmBackendServices;
           client.issuer := pm['issuer'].Trim;
           if (client.issuer = '') then
            raise EFHIRException.Createlang('INFO_MISSING', lang, ['issuer']);
           s := pm['public_key'].Trim;
           if s = '' then
             raise EFHIRException.Createlang('INFO_MISSING', lang, ['A public key is required']);
           if s.StartsWith('-----BEGIN CERTIFICATE-----') then
             jwks := loadFromRsaDer(s)
           else
             jwks := TJWKList.create(s);
           try
             json := TJsonObject.Create;
             try
               jwks.writeToJson(json);
               client.publicKey := TJSONWriter.writeObjectStr(json);
             finally
               json.free;
             end;
           finally
             jwks.free;
           end;
           end;
      else
        raise EFHIRException.Createlang('MSG_UNKNOWN_CONTENT', lang, ['Mode', 'Processing Registration']);
      end;

      if client.secret <> ''  then
        result := '<p><b>Success</b><br/>Your client has been Registered and assigned a client_id of "'+self.Context.Storage.storeClient(client, session.Key)+'". Use "'+client.secret+'" as your client secret</p>'
      else
        result := '<p><b>Success</b><br/>Your client has been Registered and assigned a client_id of "'+self.Context.Storage.storeClient(client, session.Key)+'"</p>'
    finally
      client.Free;
    end;
  finally
    pm.free;
  end;
end;


function TFhirWebServerCommonEndpoint.ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
//  thread : TAsyncTaskThread;
  id : String;
begin
  if not (request.CommandType in [fcmdSearch, fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem, fcmdTransaction, fcmdBatch, fcmdUpload, fcmdOperation]) then
    raise EFHIRException.CreateLang('NO_ASYNC', request.Lang);
//  thread := TAsyncTaskThread.create(FWebServer);
//  FWebServer.FLock.Lock;
//  try
//    FWebServer.FThreads.add(thread);
//  finally
//    FWebServer.FLock.Unlock;
//  end;
//  id := NewGuidId;
//  if request.Parameters.has('_outputFormat') then
//    thread.Format := mimeTypeToFormat(request.Parameters['_outputFormat'])
//  else
//    thread.Format := response.Format;
//  thread.key := self.Context.Storage.createAsyncTask(request.url, id, thread.Format, request.secure);
//  result := 'Async Request => '+id;
//  thread.server := self.link as TFhirWebServerCommonEndpoint;
//  thread.request := request.Link;
//  thread.Start;
//  response.HTTPCode := 202;
//  response.Message := 'Accepted';
//  response.ContentLocation := request.baseUrl+'task/'+id;
//  if response.format = ffXhtml then
//  begin
//    response.ContentType := 'text/html';
//    response.Body := makeTaskRedirect(request.baseUrl, id, 'Preparing', thread.Format, nil);
//  end;
end;

function TFhirWebServerCommonEndpoint.ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  op: TFHIROperationEngine;
  t: cardinal;
  us, cs: String;
begin
  Common.stats.restStart;
  t := GetTickCount;
  if request.internalRequestId = '' then
    request.internalRequestId := self.Context.Globals.nextRequestId;

  op := self.Context.Storage.createOperationContext(request.lang);
  try
    op.OnPopulateConformance := PopulateConformance;
    result := op.Execute(Context, request, response);
    self.Context.Storage.yield(op, nil);
  except
    on e: exception do
    begin
      self.Context.Storage.yield(op, e);
      raise;
    end;
  end;
  Common.Stats.restFinish(GetTickCount - t);
  if request.Session = nil then // during OAuth only
    us := 'user=(in-oauth)'
  else
    us := 'user=' + request.Session.UserName;
  if request.CommandType = fcmdOperation then
    cs := '$' + request.OperationName
  else
    cs := 'cmd=' + CODES_TFHIRCommandType[request.CommandType];
end;


procedure TFhirWebServerCommonEndpoint.ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; prefix : String);
var
  sCookie: String;
  c: integer;
  Session: TFHIRSession;
  check: boolean;
  lang : THTTPLanguages;
begin
  lang := THTTPLanguages.Create(request.AcceptLanguage);

  if request.AuthUsername = 'Bearer' then
    sCookie := request.AuthPassword
  else
  begin
    c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
    if c > -1 then
      sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1);
  end;

  if (sCookie <> '') and request.Document.StartsWith('/scim/logout') then
  begin
    self.Context.SessionManager.EndSession(sCookie, request.RemoteIP);
    response.redirect(FPath+'/logout');
  end
  else if (self.Context.SessionManager.GetSession(sCookie, Session, check)) then
  begin
    try
      if check and not CheckSessionOK(Session, request.RemoteIP) then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', 'Session Expired', lang);
      if not Session.canAdministerUsers then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', 'This Session is not authorised to manage users', lang);
      self.Context.UserProvider.ProcessRequest(AContext, request, response, Session, prefix);
    finally
      Session.Free;
    end;
  end
  else
    Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', 'Authentication required', lang);
end;

function TFhirWebServerCommonEndpoint.ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  status : TAsyncTaskStatus;
  message, s, originalRequest : String;
  transactionTime, expires : TFslDateTime;
  secure : boolean;
  names : TStringList;
  outcome : TBytes;
  fmt : TFHIRFormat;
  key : integer;
  n, f : string;
  zip : TFslZipWriter;
  m : TFslMemoryStream;
  p : TFHIRParser;
begin
  names := TStringList.Create;
  try
    result := 'Task Request for '+request.id;
    if self.Context.Storage.fetchTaskDetails(request.Id, key, status, fmt, secure, message, originalRequest, transactionTime, expires, names, outcome) then
    begin
      if request.CommandType = fcmdDeleteTask then
      begin
        self.Context.Storage.MarkTaskDeleted(key);
        for n in names do
        begin
          f := fsl_utilities.Path([self.Context.TaskFolder, 'task-'+inttostr(key)+'-'+n+EXT_WEB_TFHIRFormat[fmt]]);
          if FileExists(f) then
            DeleteFile(f);
        end;
        response.HTTPCode := 204;
        response.Message := 'Deleted';
      end
      else if request.subId <> '' then
      begin
        if request.SubId = 'zip' then
        begin
          m := TFslMemoryStream.Create;
          try
            zip := TFslZipWriter.Create;
            try
              zip.Stream := m.Link;
              for n in names do
              begin
                f := fsl_utilities.Path([self.Context.TaskFolder, 'task-'+inttostr(key)+'-'+n+EXT_WEB_TFHIRFormat[fmt]]);
                zip.addFile(n+EXT_WEB_TFHIRFormat[fmt], f);
              end;
              zip.WriteZip;
            finally
              zip.Free;
            end;
            m.Position := 0;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Stream := m.Link;
            response.ContentType := 'application/zip';
            self.Context.Storage.recordDownload(key, request.subId);
          finally
            m.Free;
          end;
        end
        else
        begin
          f := fsl_utilities.Path([self.Context.TaskFolder, 'task-'+inttostr(key)+'-'+request.SubId]);
          if not FileExists(f) then
          begin
            response.HTTPCode := 500;
            response.Message := 'Server Error';
            response.resource := factory.BuildOperationOutcome(request.Lang, 'The source for file '+ExtractFileName(f)+' could not be found');
          end
          else
          begin
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Stream := TFslFile.create(f, fmOpenRead + fmShareDenyWrite);
            response.ContentType := MIMETYPES_TFHIRFormat[response.format];
            self.Context.Storage.recordDownload(key, request.subId);
          end;
        end;
      end
      else
      begin
        case status of
          atsCreated, atsWaiting, atsProcessing :
            begin
            response.HTTPCode := 202;
            response.Message := 'Accepted';
            response.ContentLocation := request.baseUrl+'task/'+request.id;
            if response.format = ffXhtml then
            begin
              response.ContentType := 'text/html';
              response.Body := makeTaskRedirect(request.baseUrl, request.id, '', response.Format, nil);
            end;
            response.Progress := Message;
            end;
          atsComplete:
            begin
            // check format
            response.HTTPCode := 200;
            response.Message := 'OK';
            for s in names do
              response.links.add(s, request.baseUrl+'task/'+request.id+'/'+s+EXT_WEB_TFHIRFormat[fmt]);
            response.links.add('collection', request.baseUrl+'task/'+request.id+'.zip');
            if response.format = ffXhtml then
            begin
              response.ContentType := 'text/html';
              response.Body := makeTaskRedirect(request.baseUrl, request.id, '', fmt, names);
            end
            else
            begin
              response.ContentType := 'application/json';
// todo              response.Body := FWebServer.encodeAsyncResponseAsJson(request, originalRequest, secure, fmt, transactionTime, names);
            end;
            end;
          atsTerminated, atsError :
            begin
            response.HTTPCode := 500;
            response.Message := 'Error';
            fmt := ffJson;
            if length(outcome) > 0 then
            begin
              p := factory.makeParser(self.Context.ValidatorContext.link, fmt, THTTPLanguages.create('en'));
              try
                response.resource := p.parseResource(outcome);
              finally
                p.Free;
              end;
            end
            else
              response.resource := factory.BuildOperationOutcome(request.Lang, message);
            end;
          atsAborted:
            begin
            response.HTTPCode := 400;
            response.Message := 'Error';
            response.resource := factory.BuildOperationOutcome(request.Lang, 'This task has been cancelled');
            end;
          atsDeleted:
            begin
            response.HTTPCode := 404;
            response.Message := 'Not found';
            response.Resource := factory.BuildOperationOutcome(THTTPLanguages.create('en'), 'Task has been deleted', itUnknown);
            end;
        end;
      end
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not found';
      response.Resource := factory.BuildOperationOutcome(THTTPLanguages.create('en'), 'Unknown task', itUnknown);
    end;
  finally
    names.Free;
  end;
end;


function TFhirWebServerCommonEndpoint.BuildFhirAuthenticationPage(const lang : THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params : String): String;
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
    TFHIRXhtmlComposer.header(factory, nil, FPath, lang, SERVER_VERSION) + '<h2>' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + '</h2>'#13#10;

  result := result + '<p>'#13#10 + GetFhirMessage('MSG_AUTH_REQUIRED', lang) + '</p>'#13#10;
  if (Msg = '') and (params <> '') then
  begin
    p := THTTPParameters.Create(params);
    try
      msg := p['error_description'];
    finally
      p.Free;
    end;
  end;

  if Msg <> '' then
    result := result + '<p><b>' + FormatTextToHTML(Msg) + '</b></p>'#13#10;

  result := result + '<p><a href="' + FAuthServer.BasePath + '/auth?client_id=c.1&response_type=code&scope=openid%20profile%20fhirUser%20user/*.*%20' + SCIM_ADMINISTRATOR
    + '&redirect_uri=' + authurl + '/internal&aud=' + authurl + '&state=' + FAuthServer.MakeLoginToken(path, apGoogle) + '">Login using OAuth</a></p>' + #13#10;

  if Common.ActualSSLPort <> 0 then
    result := result + '<p>Or use the <a href="http://' + Host + port(Common.ActualPort, 80) + FPath + '">unsecured API</a>.</p>'#13#10;

  result := result + '<p>&nbsp;</p>'#13#10 +
    '<p>This server uses <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">Smart App Launch</a> for OAuth logins</p>'#13#10;
  result := result + TFHIRXhtmlComposer.Footer(factory, path, lang, logid);
end;

function TFhirWebServerCommonEndpoint.BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; const lang : THTTPLanguages; host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
var
  counts: TStringList;
  a: String;
  s: String;
  names: TStringList;
  profiles: TFslStringMatch;
  i, j, ix: integer;
  b: TStringBuilder;
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
      b := TStringBuilder.Create;
      try
        b.Append('<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
          '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
          '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
          '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>FHIR RESTful Server - FHIR v' +
          Factory.versionString + '</title>'#13#10 + TFHIRXhtmlComposer.PageLinks + FHIR_JS + '</head>'#13#10 + ''#13#10 + '<body>'#13#10 +
          TFHIRXhtmlComposer.header(factory, Session, sBaseURL, lang, SERVER_VERSION));

        b.Append('<h2>' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + '</h2>'#13#10);

        if Session <> nil then
          if secure then
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10)
          else if Common.ActualSSLPort = 0 then
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + '</p>'#13#10)
          else
            b.Append('<p>Welcome ' + FormatTextToXML(Session.SessionName, xmlText) + ' (or use <a href="https://' + Host + port(Common.ActualSSLPort, 443) + FPath +
              '">Secure API</a>)</p>'#13#10);

        b.Append('<p>'#13#10 + StringFormat(GetFhirMessage('MSG_HOME_PAGE_1', lang), ['<a href="http://hl7.org/fhir">http://hl7.org/fhir</a>']) + #13#10 +
          StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', lang), [s]) +
          ' This server defines some <a href="'+FPath+'/local.hts">extensions to the API</a>, and also offers <a href="'+FPath+'/tx">Terminology Services</a> or '+
            '(or you can browse <a href="/snomed/doco/">SNOMED-CT</a> or <a href="/loinc/doco/">LOINC</a> directly)' + #13#10);
        if Session.canGetUser and (Session.User <> nil) and not Session.isAnonymous then
        begin
          b.Append('. You can also <a href="'+FPath+'/registerclient.html">Register a client</a>.'+#13#10);
          b.Append(' or <a href="'+FPath+'/token.hts">get your bearer token</a> (use this to get access to the secure API without needing OAuth login).</p>');
        end
        else
        begin
          b.Append('. If you login through OAuth, you can also Register a client'+#13#10);
          b.Append(' or get your bearer token (use this to get access to the secure API without needing OAuth login in the application).</p>');
        end;

        b.Append('<p>GDPR-Disclosure: All access to this server is logged as AuditEvent Resources, and these store your ip address (and '+'logged in user, if one exists). Also, your IP address is logged with Google Analytics for building geomaps of server usage. Your continued use of the API constitutes agreement to these terms. See [link] for erasure requests.</p>');

        b.Append(
          '</p>'#13#10 + '<hr/>'#13#10 + ''#13#10 + '<p>' + GetFhirMessage('SYSTEM_OPERATIONS', lang) + ':</p><ul><li> <a href="' + sBaseURL + '/metadata">' +
          GetFhirMessage('CONF_PROFILE', lang) + '</a> ' + '(' + GetFhirMessage('OR', lang) + ' <a href="' + sBaseURL +
          '/metadata?_format=text/xml">as xml</a> (' + GetFhirMessage('OR', lang) + ' <a href="' + sBaseURL +
          '/metadata?_format=application/json">JSON</a>)</li>' + #13#10);
        if not IsTerminologyServerOnly then
          b.Append('<li><a class="tag" href="' + sBaseURL + '/$meta">' + GetFhirMessage('SYSTEM_TAGS', lang) + '</a></li>');
        b.Append('<li><a href="' + sBaseURL + '/_search">' + GetFhirMessage('GENERAL_SEARCH', lang) + '</a></li>');
        if not IsTerminologyServerOnly then
          b.Append('<li><a href="' + sBaseURL + '/_history">' + StringFormat(GetFhirMessage('MSG_HISTORY', lang), [GetFhirMessage('NAME_SYSTEM', lang)]) +
            '</a> (History of all resources)</li>' + #13#10);
        if not IsTerminologyServerOnly then
          b.Append('<li><a href="#upload">' + GetFhirMessage('NAME_UPLOAD_SERVICES', lang) + '</a></li>' + #13#10);
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
          b.Append('<li><a href="'+FPath+'/scim/web">Manage Users</a></li>' + #13#10);

        b.Append('</ul>' + #13#10 + ''#13#10 + '<hr/>'#13#10 + '<p>' + GetFhirMessage('MSG_HOME_PAGE_3', lang) + '</p>' + #13#10);

        b.Append('<table class="lines">'#13#10 +

          '<tr><th>' + GetFhirMessage('NAME_TYPE', lang) + '</th>' + '<th>' + GetFhirMessage('NAME_STORED', lang) + '</th>' + '<th colspan="4">' +
          GetFhirMessage('NAME_OPERATIONS', lang) + '</th><td style="border-left: 1px solid grey"/><th>' + GetFhirMessage('NAME_TYPE', lang) + '</th>' + '<th>'
          + GetFhirMessage('NAME_STORED', lang) + '</th>' + '<th colspan="4">' + GetFhirMessage('NAME_OPERATIONS', lang) + '</th></tr>'#13#10);

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
              b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseURL, integer(counts.Objects[ix]), true, true, Session.canRead(a)));

              b.Append('<td style="border-left: 1px solid grey"/>');

              if (i + names.Count div 2) + 1 < names.Count then
              begin
                a := names[1 + i + names.Count div 2];
                ix := counts.IndexOf(a);
                b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseURL, integer(counts.Objects[ix]), true, true, Session.canRead(a)));
              end;

              b.Append('</tr>');

            end;
          end;
        finally
          names.Free;
        end;
        b.Append('</table>'#13#10);
        if not IsTerminologyServerOnly then
          b.Append('<hr/><h2>' + GetFhirMessage('NAME_UPLOAD_SERVICES', lang) + '</h2>'#13#10 +
            '<a name="upload"> </a><form enctype="multipart/form-data" method="POST">' + #13#10 +
            '<p><input type="hidden" name="_format" value="text/html"/><br/>' + #13#10 + '' + GetFhirMessage('MSG_CONTENT_MESSAGE', lang) + '.<br/><br/>' +
            #13#10 + '' + GetFhirMessage('MSG_CONTENT_UPLOAD', lang) + ': <br/><input type="file" name="file" size="60"/><br/>' + #13#10 + '' +
            GetFhirMessage('MSG_CONTENT_PASTE', lang) + ':<br/> <textarea name="src" cols="70" rows="5"/>' + #13#10 + '</textarea><br/><br/>' + #13#10 +
            '<table class="none"><tr><td>Operation:</td><td> <select size="1" name="op">' + #13#10 + ' <option value="transaction">Transaction</option>' +
            #13#10 + ' <option value="batch">Batch</option>' + #13#10 + ' <option value="validation">Validation</option>' + #13#10 + '</select></td></tr>' +
            #13#10 + '<tr><td>Profile:</td><td> <select size="1" name="profile">' + #13#10 + '<option value=""></option>' + #13#10 + pol +
            '</select> (if validating, use the selected profile)</td></tr></table><br/>' + #13#10 + '<input type="submit" value="' +
            GetFhirMessage('NAME_UPLOAD', lang) + '"/>'#13#10 + '</p></form>'#13#10);
        b.Append(TFHIRXhtmlComposer.Footer(factory, sBaseURL, lang, logId));
        result := b.ToString;
      finally
        b.Free;
      end;
    finally
      profiles.Free;
    end;
  finally
    counts.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.BuildFhirUploadPage(const lang : THTTPLanguages; host, sBaseURL: String; aType: String; Session: TFHIRSession): String;
var
  s: String;
begin
  s := host + sBaseURL;

  result := '<?xml version="1.0" encoding="UTF-8"?>'#13#10 + '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10 +
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 + ''#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10 + '<head>'#13#10 +
    '    <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 + '    <title>' + StringFormat(GetFhirMessage('UPLOAD', lang),
    [aType]) + '</title>'#13#10 + '    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10 + FHIR_JS + '</head>'#13#10 +
    ''#13#10 + '<body>'#13#10 + ''#13#10 + '<div class="header">'#13#10 + '  <a href="http://www.hl7.org/fhir" title="' +
    GetFhirMessage('MSG_HOME_PAGE_TITLE', lang) + '"><img border="0" src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10 +
    ''#13#10 + '  &copy; HL7.org 2011+'#13#10 + '  &nbsp;'#13#10 + '  ' + Common.OwnerName + ' ' + GetFhirMessage('NAME_SERVER', lang) + ''#13#10 +
    '  &nbsp;'#13#10 + '  FHIR ' + GetFhirMessage('NAME_VERSION', lang) + ' ' + Factory.versionString + ''#13#10;

  if Session <> nil then
    result := result + '&nbsp;&nbsp;' + FormatTextToXML(Session.SessionName, xmlText);

  result := result + '  &nbsp;<a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', lang) + '</a>'#13#10 + '</div>'#13#10 + ''#13#10 +
    '<div id="div-cnt" class="content">'#13#10 + '<h2>' + StringFormat(GetFhirMessage('UPLOAD', lang), [aType]) + '</h2>'#13#10 + '<form action="' + s +
    lowercase(aType) + '/upload" enctype="multipart/form-data" method="POST">' + #13#10 + '<input type="hidden" name="format" size="text/html"/><br/>' + #13#10
    + '' + GetFhirMessage('MSG_CONTENT_UPLOAD', lang) + ': <input type="file" name="file" size="60"/><br/>' + #13#10 +
    '<input type="submit" value="Upload"/>'#13#10 + '</form>'#13#10 + ''#13#10 + '<p><br/><a href="' + s + '">' + GetFhirMessage('MSG_BACK_HOME', lang) +
    '</a></p>' + '</div>'#13#10 + '</body>'#13#10 + '</html>'#13#10 + ''#13#10
end;

type
  TPackageListSorter = class (TFslComparer<TFHIRPackageInfo>)
  public
    function compare(const l, r : TFHIRPackageInfo) : integer; override;
  end;

function TPackageListSorter.compare(const l, r : TFHIRPackageInfo) : integer;
begin
  result := CompareText(l.id, r.id);
end;

function TFhirWebServerCommonEndpoint.buildPackageList: String;
var
  pcm : TFHIRPackageManager;
  list : TFslList<TFHIRPackageInfo>;
  i : TFHIRPackageInfo;
  loaded : TFslMap<TLoadedPackageInformation>;
  b : TFslStringBuilder;
  lp : TLoadedPackageInformation;
  links : String;
begin
  pcm := TFHIRPackageManager.Create(false);
  try
    list := TFslList<TFHIRPackageInfo>.create;
    try
      pcm.listAllKnownPackages(list, self.Context.Factory.versionName);
      loaded := self.Context.Storage.loadPackages;
      try
        loaded.defaultValue := nil;
        b := TFslStringBuilder.Create;
        try
          b.append('<table>'#13#10);
          b.append(' <tr><td><b>Package Id</b></td><td><b>Latest Version</b></td><td><b>Loaded Info</b></td><td><b>Actions</b></td></tr>'#13#10);
          list.Sort(TPackageListSorter.create);

          for i in list do
          begin
            lp := loaded[i.id];
            links := '';
            if (lp <> nil) then
            begin
              links := '<a href="package-client.phs?handler=packageloader&reload='+i.id+'">reload</a> ';
            end;
            links := links + '<a href="package-client.phs?handler=packageloader&load='+i.id+'">load</a>';

            if lp <> nil then
              b.append(' <tr><td>'+i.id+'</td><td>'+i.version+'</td><td>'+loaded[i.id].summary+'</td><td>'+links+'</td></tr>'#13#10)
            else
              b.append(' <tr><td>'+i.id+'</td><td>'+i.version+'</td><td>-</td><td>'+links+'</td></tr>'#13#10);
          end;
          b.append('</table>'#13#10);
          result := b.toString;
        finally
          b.Free;
        end;
      finally
        loaded.Free;
      end;
    finally
      list.free;
    end;
  finally
    pcm.Free;
  end;
end;

//function TFhirWebServerCommonEndpoint.LookupReference(Context: TFHIRRequest; id: String): TResourceWithReference;
//var
//  store: TFHIROperationEngine;
//begin
//  store := self.Context.Storage.createOperationContext(TFHIRRequest(Context).lang);
//  try
//    result := store.LookupReference(Context, id);
//    self.Context.Storage.yield(store, nil);
//  except
//    on e: exception do
//    begin
//      self.Context.Storage.yield(store, e);
//      raise;
//    end;
//  end;
//end;

function TFhirWebServerCommonEndpoint.makeTaskRedirect(base, id: String; msg : String; fmt : TFHIRFormat; names: TStringList): string;
var
  s, n, body, r : String;
begin
  s := Common.SourceProvider.getSource('task-redirect.html');
  if (names <> nil) and (names.count > 0) then
  begin
    r := '';
    body := '<p>';
    for n in names do
      body := body+'<a href="'+base+'task/'+id+'/'+n+EXT_WEB_TFHIRFormat[fmt]+'">'+n+EXT_WEB_TFHIRFormat[fmt]+'</a><br/>'+#13#10;
    body := body+'</p>';
  end
  else
  begin
    body := '<p>';
    body := body+'Working: '+msg;
    body := body+'</p>';
    body := body+'<p>';
    body := body+'<a href="'+base+'task/'+id+'">Try Again</a>';
    body := body+'</p>';
    r := '<META HTTP-EQUIV="Refresh" CONTENT="30;URL='+base+'task/'+id+'"/>'
  end;
  result := s.Replace('${body}', body);
  result := result.Replace('${redirect}', r);
  result := result.Replace('${title}', 'Task '+id);
end;

procedure TFhirWebServerCommonEndpoint.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
var
  b : TFHIRBundleW;
begin
  if context.Format = ffNDJson then
    raise EFHIRException.CreateLang('NDJSON-ASYNC', request.Lang);
  b := factory.wrapBundle(factory.makeResource('Bundle'));
  b.type_ := aType;
  builder := TFHIRBundleBuilderSimple.Create(factory.link, b);
end;

function TFhirWebServerCommonEndpoint.EncodeVersionsJson(r : TFHIRResourceV): TBytes;
var
  j : TJsonObject;
  a : TJsonArray;
  p : TFhirParametersW;
  pp : TFhirParametersParameterW;
  s : String;
begin
  p := factory.wrapParams(r.link);
  try
    j := TJsonObject.create;
    try
      a := j.forceArr['versions'];
      for pp in p.parameterList do
        if pp.name = 'version' then
          a.add(pp.value.primitiveValue);
      s := TJSONWriter.writeObjectStr(j, true);
    finally
      j.free;
    end;
    result := TEncoding.UTF8.GetBytes(s);
  finally
    p.free;
  end;
end;

function TFhirWebServerCommonEndpoint.EncodeVersionsXml(r : TFHIRResourceV): TBytes;
var
  x : TMXmlDocument;
  p : TFhirParametersW;
  pp : TFhirParametersParameterW;
  s : String;
begin
  p := factory.wrapParams(r.link);
  try
    x := TMXmlDocument.Create;
    try
      for pp in p.parameterList do
        if pp.name = 'version' then
          x.addElement('version').addText(pp.value.primitiveValue);
      s := x.ToXml(true);
    finally
      x.free;
    end;
    result := TEncoding.UTF8.GetBytes(s);
  finally
    p.free;
  end;
end;

function TFhirWebServerCommonEndpoint.factory: TFHIRFactory;
begin
  result := self.Context.Factory;
end;

function TFhirWebServerCommonEndpoint.GetResource(Session: TFHIRSession; rtype: string; const lang : THTTPLanguages; id, ver, op: String): TFhirResourceV;
var
  request: TFHIRRequest;
  response: TFHIRResponse;
  Context: TOperationContext;
begin
  request := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
  response := TFHIRResponse.Create(self.Context.ValidatorContext.link);
  try
    response.OnCreateBuilder := doGetBundleBuilder;
    request.Session := Session.link;
    request.ResourceName := rtype;
    request.lang := lang;
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
    try
      checkRequestByJs(context, request);
      ProcessRequest(Context, request, response);
    finally
      Context.Free;
    end;
    if response.resource <> nil then
      result := response.resource.link
    else
      raise EFHIRException.CreateLang('MSG_NO_MATCH', lang, [rtype + '/' + id + '/_history/' + ver]);
  finally
    response.Free;
    request.Free;
  end;
end;

//function TFhirWebServerCommonEndpoint.FindResource(Session: TFHIRSession; rtype: string; lang, params: String): TFhirResourceV;
//var
//  request: TFHIRRequest;
//  response: TFHIRResponse;
//  Context: TOperationContext;
//  b : TFHIRBundleW;
//  be : TFhirBundleEntryW;
//begin
//  request := TFHIRRequest.Create(self.Context.ValidatorContext.link, roRest, self.Context.Indexes.Compartments.link);
//  response := TFHIRResponse.Create(self.Context.ValidatorContext.link);
//  try
//    response.OnCreateBuilder := doGetBundleBuilder;
//    request.Session := Session.link;
//    request.ResourceName := rtype;
//    request.lang := lang;
//    request.LoadParams(params);
//    request.CommandType := fcmdSearch;
//    Context := TOperationContext.Create;
//    try
//      checkRequestByJs(context, request);
//      ProcessRequest(Context, request, response);
//    finally
//      Context.Free;
//    end;
//    if (response.resource <> nil) and (response.Resource.fhirType = 'Bundle') then
//    begin
//      b := factory.wrapBundle(response.Resource.link);
//      try
//        for be in b.entries.forEnum do
//          if be.resource <> nil then
//            exit(be.resource.link);
//      finally
//        b.Free;
//      end;
//    end;
//    raise EFHIRException.CreateLang('MSG_NO_MATCH', lang, [rtype + '?' + params]);
//  finally
//    response.Free;
//    request.Free;
//  end;
//end;

function TFhirWebServerCommonEndpoint.GetLaunchParameters(request: TIdHTTPRequestInfo; session : TFhirSession; launchContext: String; params: TAuthLaunchParamsSet): TDictionary<String, String>;
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
        enc := factory.wrapEncounter(GetResource(session, 'Encounter', THTTPLanguages.Create(request.AcceptLanguage), launchContext.Substring(10), '', ''));
        try
          result.Add('patient', enc.PatientId);
        finally
          enc.Free;
        end;
      end;
      if alpEncounter in params then
        result.Add('encounter', launchContext.Substring(10));
    end;
  end;
end;

procedure TFhirWebServerCommonEndpoint.GetPatients(details: TFslStringDictionary);
var
  b : TFHIRBundleW;
  be : TFhirBundleEntryW;
  p : TFhirPatientW;
begin
  b := DoSearch(nil, 'Patient', THTTPLanguages.create('en'), '_summary=true&__wantObject=true');
  try
    for be in b.entries.forEnum do
    begin
      p := factory.wrapPatient(be.resource.link);
      try
        details.Add(p.id, p.nameSummary);
      finally
        p.Free;
      end;
    end;
  finally
    b.Free;
  end;
end;

function TFhirWebServerCommonEndpoint.getReferencesByType(t: String): String;
var
  bundle : TFHIRBundleW;
  entry : TFhirBundleEntryW;
  b : TStringBuilder;
  s : String;
begin
  bundle := nil;

  b := TStringBuilder.create;
  try
    for s in t.trim.Split(['|']) do
    begin
      bundle := DoSearch(nil, s, THTTPLanguages.create('en'), '_summary=true&__wantObject=true&_sort=name&_count=50');
      for entry in bundle.entries.forEnum do
      begin
        b.Append('<option value="');
        b.Append(entry.resource.id);
        b.Append('">');
//        if entry.resource is TFhirPatient then
//          b.Append(HumanNamesAsText(TFhirPatient(entry.resource).nameList))
//        else if entry.resource is TFhirRelatedPerson then
//        !{$IFDEF FHIR2}
//          b.Append(HumanNameAsText(TFhirRelatedPerson(entry.resource).name))
//        {$ELSE}
//          b.Append(HumanNamesAsText(TFhirRelatedPerson(entry.resource).nameList))
//        {$ENDIF}
//        else if entry.resource is TFhirOrganization then
//          b.Append(TFhirOrganization(entry.resource).name)
//        else
          b.Append('??');
        b.Append(' (');
        b.Append(entry.resource.fhirType);
        b.Append('/');
        b.Append(entry.resource.id);
        b.Append(')</option>');
      end;
    end;
    result := b.ToString;
  finally
    b.Free;
    bundle.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String);
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
        link := FPath + '/_web/StructureDefinition/' + tail
      else
        link := FPath + '/_web/Questionnaire/' + tail;
    end;
    if resource.fhirType = 'StructureDefinition' then
    begin
      link := FPath + '/_web/StructureDefinition/' + tail;
      text := 'Try out the Profile as a questionnaire based web form';
    end;
    if resource.fhirType = 'ValueSet' then
    begin
      link := FPath + '/ValueSet/' + id + '/$expand?filter=';
      text := 'Expand this value set';
    end;
    if resource.fhirType = 'Patient' then
    begin
      link := FPath + '/_web/Patient/' + id;
      text := 'Patient Record Page';
    end;
    if resource.fhirType = 'Encounter' then
    begin
      link := FPath + '/_web/Encounter/' + id;
      text := 'Patient Encounter Page';
    end;
  end;
end;

procedure TFhirWebServerCommonEndpoint.OnCDSResponse(manager: TCDSHooksManager; server: TRegisteredFHIRServer; Context: TObject; response: TCDSHookResponse; error: String);
var
  ctxt: TFHIRWebServerPatientViewContext;
begin
  ctxt := TFHIRWebServerPatientViewContext(Context);

//  FWebServer.FLock.Lock;
//  try
//    if error <> '' then
//      ctxt.Errors.Add(error + ' (from ' + server.name + ')')
//    else if response = nil then
//      ctxt.Errors.Add('Unknown Error (from ' + server.name + ')')
//    else
//      ctxt.cards.AddAll(response.cards);
//  finally
//    FWebServer.FLock.Unlock;
//  end;
end;

procedure TFhirWebServerCommonEndpoint.CheckAsyncTasks;
var
  tasks : TFslList<TAsyncTaskInformation>;
  task : TAsyncTaskInformation;
  n, fn : string;
begin
  tasks := TFslList<TAsyncTaskInformation>.create;
  try
    self.Context.Storage.fetchExpiredTasks(tasks);
    for task in tasks do
    begin
      self.Context.Storage.MarkTaskDeleted(task.key);
      for n in task.names do
      begin
        fn := fsl_utilities.Path([self.Context.TaskFolder, 'task-'+inttostr(task.key)+'-'+n+EXT_WEB_TFHIRFormat[task.format]]);
        if FileExists(fn) then
          DeleteFile(fn);
      end;
    end;
  finally
    tasks.free;
  end;
end;

function TFhirWebServerCommonEndpoint.CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
var
  id, name, email, Msg: String;
begin
  if Session.providerCode = apGoogle then
    result := GoogleGetDetails(Session.InnerToken, FAuthServer.GoogleAppKey, '', id, name, email, Msg)
  else if Session.providerCode = apFacebook then
    result := FacebookGetDetails(Session.InnerToken, id, name, email, Msg)
  else
    result := false;
  if result then
    result := Session.id = id;
  if result then
    self.Context.SessionManager.MarkSessionChecked(Session.Cookie)
  else
    self.Context.SessionManager.EndSession(Session.Cookie, ip);
end;

function TFhirWebServerCommonEndpoint.ClientAddress(secure: boolean): String;
begin
  if secure then
    result := 'https://'+Common.host+ port(Common.ActualSSLPort, 443) + FPath
  else
    result := 'http://'+Common.host+port(Common.ActualPort, 80) + FPath;
end;

procedure TFhirWebServerCommonEndpoint.RunPostHandler(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean);
var
  handler : TFHIRServerPostHandler;
  params : THTTPParameters;
  s : string;
begin
  params := THTTPParameters.create(request.UnparsedParams);
  try
    s := params['handler'];
    if (s = 'packageloader') then
      handler := TPackageLoader.create(self)
    else
      raise EFHIRException.create('Unknown Handler '+s);

//  !{$IFDEF FHIR3}
//    if s = 'coverage' then
//      handler := TFHIRServerCoveragePostHandler.Create
//    else {$ENDIF}
    try
      handler.secure := secure;
      handler.params := params.Link;
      handler.context := self.Context.Link;
      handler.session := Session.Link;
      handler.variables := TFslMap<TFHIRObject>.create('post.variables');
      handler.execute;

      ;
      if handler.redirect <> '' then
        response.Redirect(handler.redirect)
      else
        ReturnProcessedFile(request, response, session, claimed, actual, secure, handler.variables);
    finally
      handler.Free;
    end;
  finally
    params.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
begin
  ReturnProcessedFile(request, response, Session, path, path, secure, variables);
end;

procedure TFhirWebServerCommonEndpoint.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
var
  pm : THTTPParameters;
  s, n, p, v, t: String;
  plugin : TFHIRWebServerScriptPlugin;
  client : TFhirClientV;
begin
  pm := THTTPParameters.create(request.UnparsedParams);
  try
    client := self.Context.Factory.makeClientInt(self.Context.ValidatorContext.Link, THTTPLanguages.create('en'), TFHIRWebServerCommunicator.Create(self, secure, session));
    try
      for plugin in FPlugins do
      begin
        plugin.FEndPoint := self;
        if plugin.processPage(request, pm, response, session, claimed, actual, secure, variables, client) then
          exit;
      end;

      s := Common.SourceProvider.getSource(actual);
      // actions....
      if s.Contains('<!--[%clientregistration%]-->') then
      begin
        s := s.Replace('<!--[%clientregistration%]-->', processRegistration(request, session), [rfReplaceAll]);
      end;

      s := s.Replace('[%id%]', Common.Name, [rfReplaceAll]);
      s := s.Replace('[%specurl%]', factory.specUrl, [rfReplaceAll]);
      s := s.Replace('[%ver%]', Factory.versionString, [rfReplaceAll]);
      s := s.Replace('[%path%]', FPath, [rfReplaceAll]);
      s := s.Replace('[%spath%]', FPath, [rfReplaceAll]);
//      s := s.Replace('[%web%]', WebDesc(secure), [rfReplaceAll]);
      s := s.Replace('[%admin%]', Common.AdminEmail, [rfReplaceAll]);
      if (Session = nil) then
        s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll])
      else
        s := s.Replace('[%logout%]', '|&nbsp;User: ' + Session.SessionName + '&nbsp; <a href="'+FPath+'/logout" title="Log Out"><img src="/logout.png"></a>  &nbsp;',
          [rfReplaceAll]);
      if Common.ActualPort = 80 then
        s := s.Replace('[%host%]', Common.Host, [rfReplaceAll])
      else
        s := s.Replace('[%host%]', Common.Host + ':' + inttostr(Common.ActualPort), [rfReplaceAll]);
      if (Session <> nil) and Session.canGetUser and (Session.User <> nil) then
        s := s.Replace('[%jwt%]', Session.JWTPacked, [rfReplaceAll])
      else
        s := s.Replace('[%jwt%]', 'JWT not available', [rfReplaceAll]);

      if Common.ActualSSLPort = 443 then
        s := s.Replace('[%securehost%]', Common.Host, [rfReplaceAll])
      else
        s := s.Replace('[%securehost%]', Common.Host + ':' + inttostr(Common.ActualSSLPort), [rfReplaceAll]);
    //  if s.Contains('[%fitbit-redirect%]') then
    //    s := s.Replace('[%fitbit-redirect%]', FitBitInitiate(FAuthServer.ini.ReadString(voVersioningNotApplicable, 'fitbit', 'secret', ''), // secret,
    //      FAuthServer.ini.ReadString(voVersioningNotApplicable, 'fitbit', 'key', ''), // key
    //      NewGuidId, // nonce
    //      'https://local.healthintersections.com.au:961/_web/fitbit.html')
    //      // callback
    //      , [rfReplaceAll]);

      s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
      if variables <> nil then
        for n in variables.Keys do
          if s.contains('[%' + n + '%]') then
            s := s.Replace('[%' + n + '%]', variables[n].primitiveValue, [rfReplaceAll]);

      while s.contains('[%options-reference') do
      begin
        StringSplit(s, '[%options-reference', p, v);
        StringSplit(v, '%]', v, t);
        v := getReferencesByType(v);
        s := p+v+t;
      end;

      if s.contains('[%package-list%]') then
        s := s.Replace('[%package-list%]', buildPackageList, [rfReplaceAll]);
      if s.contains('[%sessions-table%]') then
        s := s.Replace('[%sessions-table%]', buildSessionsTable, [rfReplaceAll]);

      for plugin in FPlugins do
        s := plugin.process(s, request, pm, variables, session, client);
    finally
      client.Free;
    end;

    response.Expires := Now + 1;
    response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
    response.FreeContentStream := true;
    response.contentType := 'text/html; charset=UTF-8';
  finally
    pm.Free;
  end;
end;

procedure TFhirWebServerCommonEndpoint.ReturnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject>);
var
  sCookie, url, token : String;
  c : integer;
  check : boolean;
begin
  if request.AuthUsername = 'Bearer' then
    sCookie := request.AuthPassword
  else
  begin
    c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
    if c > -1 then
      sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length + 1);
  end;

  if (sCookie <> '') and request.Document.StartsWith('/logout') then
  begin
    self.Context.SessionManager.EndSession(sCookie, request.RemoteIP);
    response.redirect(FPath+'/logout');
  end
  else if (self.Context.SessionManager.GetSession(sCookie, Session, check)) then
  begin
    try
      if check and not CheckSessionOK(Session, request.RemoteIP) then
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer.HTTPRequest', 'MSG_AUTH_REQUIRED', 'Session Expired', THTTPLanguages.Create(request.AcceptLanguage));
      if (variables = nil) then
      begin
        variables := TFslMap<TFHIRObject>.create('file.vars');
        try
          ReturnProcessedFile(request, response, Session, claimed, actual, true, variables);
        finally
          variables.free;
        end;
      end
      else
        ReturnProcessedFile(request, response, Session, claimed, actual, true, variables);
    finally
      Session.Free;
    end;
  end
  else
  begin
    token := FAuthServer.MakeLoginToken(path, apGoogle);
    if (request.CommandType in [hcGet, hcHead]) then
    begin
      if (request.UnparsedParams <> '') then
        FTokenRedirects.recordRedirect(token, request.URI+'?'+request.UnparsedParams)
      else
        FTokenRedirects.recordRedirect(token, request.URI)
    end;
    url := OAuthPath(secure);
    url := FAuthServer.BasePath + '/auth?client_id=c.1&response_type=code&scope=openid%20profile%20fhirUser%20user/*.*%20' + SCIM_ADMINISTRATOR + '&redirect_uri=' + url + '/internal&aud=' + url + '&state=' + token;
    response.ResponseNo := 302;
    response.Location := url;
  end;
end;

procedure TFhirWebServerCommonEndpoint.ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
var
  src : String;
begin
  if not secure and path.EndsWith('.html') then
  begin
    src := Common.SourceProvider.getSource(path);
    if src.Contains('<!--[%requires-secure=true%]-->') then
    begin
      response.Expires := Now + 1;
      response.Redirect(Context.FormalURLSecure+stated);
      exit;
    end;
  end;

  response.Expires := Now + 1;
  response.ContentStream := Common.SourceProvider.asStream(path);
  response.FreeContentStream := true;
  response.contentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

procedure TFhirWebServerCommonEndpoint.cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);
begin
  case caching of
    cacheNotAtAll:
      response.CacheControl := 'no-cache, no-store, must-revalidate';
    cacheAsException:
      response.CacheControl := 'public, max-age=600, error';
    cacheNormal:
      response.CacheControl := 'public, max-age=600';
    cacheLong:
      response.CacheControl := 'public, max-age=31536000';
  end;
end;

{$IFDEF WINDOWS}

function TFhirWebServerCommonEndpoint.transform1(resource: TFhirResourceV; const lang : THTTPLanguages; xslt: String; saveOnly: boolean): string;
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
    xml := factory.makeComposer(Context.ValidatorContext.link, ffXml, lang, OutputStyleNormal);
    try
      xml.Compose(b, resource);
    finally
      xml.Free;
    end;
    b.Position := 0;
    msx := TMsXmlParser.Create;
    try
      doc := msx.Parse(b);
    finally
      msx.Free;
    end;
  finally
    b.Free;
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

  if Common.ActualPort <> 0 then
    url := 'http://' + Common.Host + ':' + inttostr(Common.ActualPort)
  else
    url := 'https://' + Common.Host + ':' + inttostr(Common.ActualSSLPort);

  if saveOnly then
    proc.addParameter('saveOnly', 'true', '');

  proc.addParameter('expansionServer', url + FPath, '');
  proc.addParameter('iconPath', url, '');
  proc.addParameter('jQueryPath', url + '/js', '');

  proc.Transform;
  result := proc.Output;
end;
{$ENDIF}

function TFhirWebServerCommonEndpoint.loadFromRsaDer(cert: string): TJWKList;
var
  fn : String;
begin
  fn := fsl_utilities.Path([SystemTemp, TFslDateTime.makeUTC.toString('yyyymmmddhhnnss')+'.'+inttostr(HashStringToCode32(cert))+'.cer']);
  StringToFile(cert, fn, TEncoding.UTF8);
  try
    result := TJWKList.create;
    try
      result.Add(TJWTUtils.loadKeyFromRSACert(ansiString(fn)));
      result.Link;
    finally
      result.Free;
    end;
  finally
    DeleteFile(fn);
  end;
end;

procedure TFhirWebServerCommonEndpoint.checkRequestByJs(context: TOperationContext; request: TFHIRRequest);
begin
  // js-todo - figure out which scripts to run, and then run them
end;

// procedure TFhirWebServer.ReadTags(Headers: TIdHeaderList; Request: TFHIRRequest);
// var
// i : integer;
// begin
// for i := 0 to Headers.Count - 1 do
// if Headers.Names[i] = 'Category' then
// ReadTags(Headers.Strings[i], Request);
// end;
//
Procedure TFhirWebServerCommonEndpoint.ReadTags(header: String; request: TFHIRRequest);
// var
// s, s1, l, r, n, v : string;
// cat : TFHIRAtomCategory;
begin
  // raise EFHIRException.create('todo');
end;

function TFhirWebServerCommonEndpoint.readVersion(mt : String): TFHIRVersion;
var
  i, s, p, pi,l,r : string;
begin
  result := factory.version;

  for i in mt.Split([',']) do
  begin
    s := i.Trim;
    if s.StartsWith('application/fhir.r') and (s.Length > 18) then
      case s[19] of
        '2': exit(fhirVersionRelease2);
        '3': exit(fhirVersionRelease3);
        '4': exit(fhirVersionRelease4);
      end
    else for p in s.Split([';']) do
    begin
      pi := p.Trim;
      StringSplit(pi, '=', l, r);
      if l = 'fhir-version' then
      begin
        if r = 'r3' then
          exit(fhirVersionRelease3)
        else if r = '3.0' then
          exit(fhirVersionRelease3)
        else if r = '3.0.2' then
          exit(fhirVersionRelease3)
        else if r = 'r2' then
          exit(fhirVersionRelease2)
        else if r = '1.0' then
          exit(fhirVersionRelease2)
        else if r = '1.0.2' then
          exit(fhirVersionRelease2)
        else if r = 'r4' then
          exit(fhirVersionRelease4)
        else if r = '3.2' then
          exit(fhirVersionRelease4)
        else if r = '3.2.0' then
          exit(fhirVersionRelease4)
      end;
    end;
  end;
end;

procedure TFhirWebServerCommonEndpoint.registerScriptPlugins;
begin
  if factory.version = fhirVersionRelease4 then
  begin
    FPlugins.add(THtmlFormScriptPlugin.create);
    {$IFDEF COVID}
    FPlugins.add(TCovidScriptPlugin.create(FWebServer.Common.SourceProvider.link, self.Context.Link));
    {$ENDIF}
  end;
end;

function TFhirWebServerCommonEndpoint.EndPointDesc(secure: boolean): String;
begin
  result := '';
  if (secure) then
  begin
    if FPath <> '' then
      result := result + ' <li><a href="http://' + Common.Host + port(Common.ActualPort, 80) + FPath + '">Unsecured access at ' + FPath +
        '</a> - direct access with no security considerations</li>'#13#10;
    if Common.ActualSSLPort <> 0 then
      result := result + ' <li><a href="' + FPath + '">Secured access at ' + FPath +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end
  else
  begin
    result := result + ' <li><a href="' + FPath + '">Unsecured access at ' + FPath +
        '</a> - direct access with no security considerations</li>'#13#10;
    if FPath <> '' then
      result := result + ' <li><a href="https://' + Common.Host + port(Common.ActualSSLPort, 443) + FPath + '">Secured access at ' + FPath +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end;
end;


{ THtmlFormScriptPlugin }

function THtmlFormScriptPlugin.process(s: String; request: TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV): String;
var
  b, e : integer;
  t, n : String;
begin
  b := s.IndexOf('[%html.param ');
  while b > 0 do
  begin
    t := s.Substring(b);
    s := s.Substring(0, b);
    e := t.IndexOf('%]');
    n := t.Substring(13, e-13);
    s := s + pm[n]+t.Substring(e+2);
    b := s.IndexOf('[%html.param ');
  end;
  b := s.IndexOf('[%html.submit');
  while b > 0 do
  begin
    t := s.Substring(b);
    s := s.Substring(0, b);
    e := t.IndexOf('%]');
    if (pm['id'] = '') then
      s := s + 'Create'+t.Substring(e+2)
    else
      s := s + 'Update'+t.Substring(e+2);
    b := s.IndexOf('[%html.submit');
  end;
  result := s;
end;

{ ERestfulAuthenticationNeeded }

Constructor ERestfulAuthenticationNeeded.Create(Const sContext : String; sMessage, sCaption : String; const lang : THTTPLanguages);
begin
  inherited Create(sContext, HTTP_ERR_UNAUTHORIZED, itLogin, sMessage, lang);
  FMsg := sCaption;
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
  FContext.Free;
  FErrors.Free;
  FCards.Free;
  FManager.Free;
  inherited;
end;

procedure TFHIRWebServerPatientViewContext.SetManager(const Value: TCDSHooksManager);
begin
  FManager.Free;
  FManager := Value;
end;

{ TFHIRServerPostHandler }

constructor TFHIRServerPostHandler.Create(server: TFhirWebServerCommonEndpoint);
begin
  inherited Create;
  FServer := server;
end;

destructor TFHIRServerPostHandler.Destroy;
begin
  FSession.Free;
  FParams.Free;
  FVariables.Free;
  FContext.Free;
  inherited;
end;

procedure TFHIRServerPostHandler.SetContext(const Value: TFHIRServerContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRServerPostHandler.SetParams(const Value: THTTPParameters);
begin
  FParams.Free;
  FParams := Value;
end;

procedure TFHIRServerPostHandler.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

procedure TFHIRServerPostHandler.SetVariables(const Value: TFslMap<TFHIRObject>);
begin
  FVariables.Free;
  FVariables := Value;
end;

{ TPackageLoader }

procedure TPackageLoader.execute;
begin
  if params.has('load') then
    load
  else if params.has('reload') then
    reload
  else
    raise Exception.Create('Unknown mode');
end;

procedure TPackageLoader.load;
var
  id, ver, n : String;
  cnt : TBytes;
  pcm : TFHIRPackageManager;
  npm : TNpmPackage;
  all : TFslList<TFHIRResourceV>;
  parser : TFHIRParser;
  bundle : TFHIRBundleW;
  pck : TFHIRPackageClient;
  vl : TFslList<TFHIRPackageInfo>;
  v : TFHIRPackageInfo;
  links : String;
begin
  id := params['load'];
  if (id = '') then
    raise Exception.Create('No package id to load provided');
  ver := params['version'];
  if (ver = '') then
  begin
    pck := TFHIRPackageClient.Create(PACKAGE_SERVER_PRIMARY);
    try
      vl := pck.getVersions(id);
      try
        if vl.count = 0 then
        begin
          vl.free;
          vl := nil;
          pck.free;
          pck := TFHIRPackageClient.Create(PACKAGE_SERVER_BACKUP);
          vl := pck.getVersions(id);
        end;
        links := '<p>Package '+id+':</p><ul>';
        for v in vl do
        begin
          if (v.fhirVersion = '') or (v.fhirVersion = FServer.Context.Factory.versionString) then
          begin
            links := links + '<li><a href="package-client.phs?handler=packageloader&load='+id+'&version='+v.version+'">'+v.version+'</a></li>';
          end;
        end;
        links := links + '</ul>';
        variables.Add('package-list', FServer.Context.Factory.makeString(links));
      finally
        vl.Free;
      end;
    finally
      pck.Free;
    end;
  end
  else
  begin
    pcm := TFHIRPackageManager.Create(false);
    try
      npm := pcm.loadPackage(id, ver);
      try
        parser := FServer.Context.Factory.makeParser(FServer.Context.ValidatorContext.link, ffJson, THTTPLanguages.Create('en'));
        try
          all := TFslList<TFHIRResourceV>.create;
          try
            for n in npm.list('package') do
              if (n <> 'ig-r4.json') then
                all.Add(parser.parseResource(npm.load('package', n)));
            bundle := FServer.Context.Factory.makeBundle(all);
            try
              FServer.Transaction(bundle, false, id+'#'+npm.version, '', opmCmdLine, nil);
              FContext.Storage.recordPackageLoaded(id, npm.version, all.Count, cnt);
            finally
              bundle.Free;
            end;
          finally
            all.Free;
          end;
        finally
          parser.Free;
        end;
      finally
        npm.Free;
      end;
    finally
      pcm.Free;
    end;
    FRedirect := FServer.FPath+'/package-client.hts';
  end;
end;

procedure TPackageLoader.reload;
var
  id, n : String;
  cnt : TBytes;
  npm : TNpmPackage;
  all : TFslList<TFHIRResourceV>;
  parser : TFHIRParser;
  bundle : TFHIRBundleW;
begin
  id := params['reload'];
  if (id = '') then
    raise Exception.Create('No package id to load provided');
  cnt := FContext.Storage.fetchLoadedPackage(id);
  if length(cnt) = 0 then
    raise Exception.Create('Unable to locate package '+id);
  npm := TNpmPackage.fromPackage(cnt, id, nil);
  try
    parser := FServer.Context.Factory.makeParser(FServer.Context.ValidatorContext.link, ffJson, THTTPLanguages.Create('en'));
    try
      all := TFslList<TFHIRResourceV>.create;
      try
        for n in npm.list('package') do
          if (n <> 'ig-r4.json') then
            all.Add(parser.parseResource(npm.load('package', n)));
        bundle := FServer.Context.Factory.makeBundle(all);
        try
          FServer.Transaction(bundle, false, id+'#'+npm.version, '', opmCmdLine, nil);
          FContext.Storage.recordPackageLoaded(id, npm.version, all.Count, cnt);
        finally
          bundle.Free;
        end;
      finally
        all.Free;
      end;
    finally
      parser.Free;
    end;
  finally
    npm.Free;
  end;
  FRedirect := FServer.FPath+'/package-client.hts';
end;

{ TFHIRWebServerCommunicator }

constructor TFHIRWebServerCommunicator.Create(ep: TFhirWebServerCommonEndpoint; secure : boolean; session : TFHIRSession);
begin
  inherited create;
  FEndPoint := ep; // (no link)
  FSecure := secure;
  FSession := session;
end;

function TFHIRWebServerCommunicator.address: String;
begin
  result := FEndPoint.path;
end;

function TFHIRWebServerCommunicator.conformanceV(summary: boolean): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.createResourceV(resource: TFHIRResourceV; var id: String): TFHIRResourceV;
begin
  result := fetchResource('POST', makeUrl(resource.fhirType), resource);
  id := result.id;
end;

function TFHIRWebServerCommunicator.customGet(path: String; headers: THTTPHeaders): TFslBuffer;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.customPost(path: String; headers: THTTPHeaders; body: TFslBuffer): TFslBuffer;
begin
  raise Exception.Create('Not done yet');
end;

procedure TFHIRWebServerCommunicator.deleteResourceV(atype: TFHIRResourceTypeV; id: String);
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.historyInstanceV(atype: TFHIRResourceTypeV; id: String; allRecords: boolean; params: string): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.historyTypeV(atype: TFHIRResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.operationV(atype: TFHIRResourceTypeV; opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  result := fetchResource('POST', makeUrl(aType+'/$'+opName), params);
end;

function TFHIRWebServerCommunicator.operationV(atype: TFHIRResourceTypeV; id, opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; patch: TJsonArray): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.readResourceV(atype: TFhirResourceTypeV; id: String): TFHIRResourceV;
begin
  result := fetchResource('GET', makeUrl(aType+'/'+id), nil);
end;

function TFHIRWebServerCommunicator.searchAgainV(link: String): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.searchPostV(atype: TFHIRResourceTypeV; allRecords: boolean; params: TStringList; resource: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.searchV(atype: TFHIRResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
var
  s : String;
  bnd : TFHIRResourceV;
  bh : TFHIRBundleW;
  res : TFHIRResourceV;
begin
  res := fetchResource('GET', makeUrl(aType)+'?'+params, nil);
  if res = nil then
    raise Exception.Create('Network error: nothing returned from server?');
  bh := FClient.BundleFactory.Create(res);
  try
    if bh.resource.fhirType <> 'Bundle' then
      raise EFHIRException.create('Found a resource of type '+bh.resource.fhirType+' expecting a Bundle');
    s := bh.next;
    while AllRecords and (s <> '') do
    begin
      bnd := fetchResource('GET', s, nil);
      try
        bh.addEntries(bnd);
        s := bh.next(bnd);
      finally
        bnd.free;
      end;
    end;
    if allRecords then
      bh.clearLinks;
    result := bh.resource.Link;
  finally
    bh.Free;
  end;
end;

procedure TFHIRWebServerCommunicator.terminate;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.transactionV(bundle: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.updateResourceV(resource: TFHIRResourceV): TFHIRResourceV;
begin
  result := fetchResource('PUT', makeUrl(resource.fhirType+'/'+resource.id), resource);
end;

function TFHIRWebServerCommunicator.vreadResourceV(atype: TFhirResourceTypeV; id, vid: String): TFHIRResourceV;
begin
  raise Exception.Create('Not done yet');
end;

function TFHIRWebServerCommunicator.makeUrl(tail: String): String;
begin
  result := tail;
end;

function TFHIRWebServerCommunicator.fetchResource(command, url : String; resource : TFHIRResourceV): TFhirResourceV;
var
  ctxt : TOperationContext;
  req : TFHIRRequest;
  resp : TFHIRResponse;
  dummy : integer;
  l, r : String;
begin
  ctxt := TOperationContext.Create(opmInternal, 'internal');
  try
    req := TFHIRRequest.Create(FEndPoint.Context.ValidatorContext.link, roRestInternal, FEndPoint.Context.Indexes.Compartments.link);
    try
      StringSplit(url, '?', l, r);
      req.LoadParams(r);
      req.secure := FSecure;
      req.url := l;
      req.Session := FSession.link;
      req.lang := THTTPLanguages.create('en'); // todo...
      req.analyse(command, l, dummy, nil);
      req.resource := resource.link;

      resp := TFHIRResponse.Create(FEndPoint.Context.ValidatorContext.link);
      try
        resp.OnCreateBuilder := FEndPoint.doGetBundleBuilder;
        FEndPoint.ProcessRequest(ctxt, req, resp);
        result := resp.Resource.link;
        if resp.HTTPCode >= 300 then
          raise Exception.Create(FEndPoint.Context.Factory.getXhtml(result).AsPlainText);
      finally
        resp.free;
      end;
    finally
      req.free;
    end;
  finally
    ctxt.Free;
  end;
end;


{ TAsyncTaskThread }

constructor TAsyncTaskThread.Create(server : TFhirWebServerCommonEndpoint);
begin
  inherited Create(true); // suspended
  FServer := server;
end;

destructor TAsyncTaskThread.Destroy;
begin
  Files.free;
  FRequest.Free;
  FServer.Free;
  FBundle.Free;
  inherited;
end;

procedure TAsyncTaskThread.callback(IntParam: Integer; StrParam: String);
begin
  status(atsProcessing, StrParam);
end;

procedure TAsyncTaskThread.details;
begin
  if FBundle <> nil then
    FServer.Context.Storage.setAsyncTaskDetails(key, FBundle.timestamp, Fbundle.Links['self']);
end;

procedure TAsyncTaskThread.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
begin
  FBundle := FServer.factory.wrapBundle(fserver.factory.makeResource('Bundle'));
  FBundle.type_ := aType;
  if context.Format = ffNDJson then
  begin
    files := TFslMap<TFslFile>.create('async.files');
    builder := TFHIRBundleBuilderNDJson.Create(FServer.factory.link, FBundle.link, IncludeTrailingPathDelimiter(FServer.Context.TaskFolder)+'task-'+inttostr(FKey), files.link)
  end
  else
    builder := TFHIRBundleBuilderSimple.Create(FServer.factory.link, FBundle.link);
end;

procedure TAsyncTaskThread.Execute;
var
  response : TFHIRResponse;
  op: TFHIROperationEngine;
  t: cardinal;
  us, cs: String;
  ctxt : TOperationContext;
begin
  t := 0;

  SetThreadName('Server Async Thread');
  SetThreadStatus('Working');
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  {$ENDIF}
  try
    {$IFNDEF NO_JS}
    FServer.Common.OnRegisterJs(self, GJsHost);
    {$ENDIF}
    status(atsWaiting, 'Waiting to start');
    sleep(100);
    response := TFHIRResponse.Create(FServer.Context.ValidatorContext.link);
    try
      response.format := FFormat;
      response.OnCreateBuilder := doGetBundleBuilder;

      t := GetTickCount;
      if request.Session = nil then // during OAuth only
        us := 'user=(in-oauth)'
      else
        us := 'user=' + request.Session.UserName;
      if request.CommandType = fcmdOperation then
        cs := '$' + request.OperationName
      else
        cs := 'cmd=' + CODES_TFHIRCommandType[request.CommandType];
      status(atsProcessing, 'Processing');
      Logging.log('Start Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source);
      op := FServer.Context.Storage.createOperationContext(request.lang);
      try
        op.OnPopulateConformance := FServer.PopulateConformance;
        ctxt := TOperationContext.create(opmRestful, callback, 'starting');
        try
          op.Execute(ctxt, request, response);
        finally
          ctxt.Free;
        end;
        FServer.Context.Storage.yield(op, nil);
      except
        on e: exception do
        begin
          FServer.Context.Storage.yield(op, e);
          raise;
        end;
      end;
      details;
      saveOutcome(response);
      status(atsComplete, 'Complete');
      t := GetTickCount - t;
      Logging.log('Finish Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source + '. rt = ' + inttostr(t)+'ms');
    finally
      response.Free;
    end;
  except
    on e : exception do
    begin
      Logging.log('Error Task ('+inttostr(key)+'): ' + cs + ', type=' + request.ResourceName + ', id=' + request.id + ', ' + us + ', params=' + request.Parameters.Source + '. rt = ' + inttostr(t)+'ms: '+e.Message);
      status(atsError, e.Message);
    end;
  end;
  FServer.Common.Lock.Lock;
  try
    FServer.FThreads.Remove(self);
  finally
    FServer.Common.Lock.Unlock;
  end;
  FreeOnTerminate := true;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
  closeThread;
end;

procedure TAsyncTaskThread.kill;
begin
  {$IFDEF WINDOWS}
  TerminateThread(ThreadHandle, 1);
  {$ENDIF}
end;

procedure TAsyncTaskThread.saveOutcome;
var
  names : TStringList;
  f : TFileStream;
  n : String;
  c : TFHIRComposer;
begin
  names := TStringList.Create;
  try
    if files = nil then
    begin
      names.Add('content');
      f := TFileStream.Create(Path([FServer.Context.TaskFolder, 'task-'+inttostr(key)+'-content'+EXT_WEB_TFHIRFormat[format]]), fmCreate);
      try
        if fFormat = ffNDJson then
          c := FServer.factory.makeComposer(fserver.Context.ValidatorContext.link, ffJson, THTTPLanguages.create('en'), OutputStyleNormal)
        else
          c := FServer.factory.makeComposer(fserver.Context.ValidatorContext.link, Format, THTTPLanguages.create('en'), OutputStyleNormal);
        try
          c.Compose(f, response.Resource);
        finally
          c.Free;
        end;
      finally
        f.Free;
      end;
    end
    else
    begin
      for n in files.Keys do
        names.Add(n);
      files.Clear;
    end;
    FServer.Context.Storage.MarkTaskForDownload(key, names);
  finally
    names.Free;
  end;
end;

procedure TAsyncTaskThread.SetRequest(const Value: TFHIRRequest);
begin
  FRequest.Free;
  FRequest := Value;
end;

procedure TAsyncTaskThread.SetServer(const Value: TFhirWebServerCommonEndpoint);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TAsyncTaskThread.status(status: TAsyncTaskStatus; message: String);
begin
  FServer.Context.Storage.updateAsyncTaskStatus(key, status, message);
end;

{ TFHIRServerEndPoint }

constructor TFHIRServerEndPoint.Create(settings : TFHIRServerConfigSection; db : TFDBManager);
begin

end;

destructor TFHIRServerEndPoint.Destroy;
begin

  inherited;
end;

procedure TFHIRServerEndPoint.InstallDatabase;
begin

end;

procedure TFHIRServerEndPoint.Load;
begin

end;

procedure TFHIRServerEndPoint.LoadPackages(plist: String);
begin

end;

procedure TFHIRServerEndPoint.updateAdminPassword;
begin

end;

procedure TFHIRServerEndPoint.UninstallDatabase;
begin

end;

procedure TFHIRServerEndPoint.Unload;
begin

end;

//function TFhirWebServer.encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; secure : boolean; fmt : TFHIRFormat; transactionTime: TFslDateTime; names: TStringList): string;
//var
//  j, o : TJsonObject;
//  a : TJsonArray;
//  s : String;
//begin
//  j := TJsonObject.Create;
//  try
//    j.str['transactionTime'] := transactionTime.toXML;
//    j.str['request'] := reqUrl;
//    j.bool['secure'] := secure;
//    a := j.forceArr['output'];
//    for s in names do
//    begin
//      o := a.addObject;
//      o.str['url'] := request.baseUrl+'task/'+request.id+'/'+s+EXT_WEB_TFHIRFormat[fmt];
//      o.str['type'] := s;
//    end;
//
//    result := TJSONWriter.writeObjectStr(j, true);
//  finally
//    j.Free;
//  end;
//end;
//

end.
