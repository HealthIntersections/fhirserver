unit endpoint_storage;

{$i fhir.inc}

{
This is the actual core of the RESTful FHIR server. It's an abstract class - there's
multiple implementations which provide different stores and supporting/indexing functionality,
but all use this as the restful interface
}

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509, IdGlobalProtocols, IdCompressorZLib, IdZlib,
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_stream, fsl_crypto, fsl_oauth, fsl_xml, fsl_graphql, fsl_npm_cache, fsl_npm_client, fsl_threads, fsl_logging,
  fhir_objects, fhir_client, fhir_common, fhir_parser, fhir_utilities, fhir_xhtml, fhir_ndjson,
  fdb_manager,
  server_config, utilities, bundlebuilder, reverse_client, security, html_builder,
  storage, user_manager, session, auth_manager, server_context, server_constants,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  tx_manager, tx_webserver, telnet_server,
  web_base, web_cache, endpoint;

type
  TStorageEndPoint = class;
  TStorageWebEndpoint = class;

  ERestfulAuthenticationNeeded = class(ERestfulException)
  private
    FMsg: String;
  public
    constructor Create(Const sContext : String; sMessage, sCaption : String; const lang : THTTPLanguages); overload;
    Property Msg: String read FMsg;
  end;

  TFHIRWebServerScriptPlugin = class abstract (TFslObject)
  private
    FEndPoint : TStorageWebEndpoint;
  protected
    procedure ProcessFile(name : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; secure : boolean; variables : TFslMap<TFhirObject>);
  public
    function process(s : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV) : String; virtual; abstract;
    function processPage(request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV) : boolean; virtual;
  end;

  TFHIRWebServerCommunicator = class (TFHIRClientCommunicator)
  private
    FEndPoint : TStorageWebEndpoint;
    FSession : TFHIRSession;
    FSecure : boolean;
    function fetchResource(command, url : String; resource : TFHIRResourceV) : TFhirResourceV;
    function makeUrl(tail : String) : String;
  public
    constructor Create(ep : TStorageWebEndpoint; secure : boolean; session : TFHIRSession);
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

  TAsyncTaskThread = class (TFslThread)
  private
    FKey : integer;
    FServer : TStorageWebEndpoint;
    FRequest : TFHIRRequest;
    FFormat : TFHIRFormat;
    files : TFslMap<TFslFile>;
    FBundle : TFHIRBundleW;
    procedure SetRequest(const Value: TFHIRRequest);

    procedure status(status : TAsyncTaskStatus; message : String);
    procedure details;

    procedure saveOutcome(response : TFHIRResponse);
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TBundleType; out builder : TFhirBundleBuilder);
  protected
    procedure Execute; override;
  public
    constructor Create(server: TStorageWebEndpoint);
    destructor Destroy; override;

    procedure SetServer(const Value: TStorageWebEndpoint); // private (hint busting)
    procedure callback(IntParam: Integer; StrParam: String);// private (hint busting)

    property Key : integer read FKey write FKey;
    property Format : TFHIRFormat read FFormat write FFormat;
    Property Server : TStorageWebEndpoint read FServer;
    Property Request : TFHIRRequest read FRequest write SetRequest;
  end;

  TStorageWebEndpoint = class (TFhirWebServerEndpoint)
  private
    FContext : TFHIRServerContext;
    FEndPoint : TStorageEndPoint;
    FAuthServer: TAuth2Server;
    FTerminologyWebServer: TTerminologyWebServer;
    FPlugins : TFslList<TFHIRWebServerScriptPlugin>;
    FAdaptors: TFslMap<TFHIRFormatAdaptor>;
    FThreads : TFslList<TAsyncTaskThread>;

    procedure SetTerminologyWebServer(const Value: TTerminologyWebServer);
    Procedure HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    function HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
    procedure ReturnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
    Procedure ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; prefix : String);
    Procedure ReadTags(header: String; request: TFHIRRequest); overload;
    function CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
    procedure PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String);
    function loadFromRsaDer(cert : string) : TJWKList;

    function readVersion(mt : String) : TFHIRVersion;
    function BuildRequest(const lang : THTTPLanguages; sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sContentEncoding,
      sCookie, provenance, sBearer: String; oPostStream: TStream; oResponse: TFHIRResponse; var aFormat: TFHIRFormat; var redirect: boolean; form: TMimeMessage;
      bAuth, secure: boolean; out relativeReferenceAdjustment: integer; var style : TFHIROutputStyle; Session: TFHIRSession; cert: TIdOpenSSLX509): TFHIRRequest;
    Procedure ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip, cache: boolean; summary : String);
    procedure SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; const lang : THTTPLanguages; message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFHIRIssueType);
    function processProvenanceHeader(header : String; const lang : THTTPLanguages): TFhirProvenanceW;
    function EncodeVersionsJson(r: TFHIRResourceV): TBytes;
    function EncodeVersionsXml(r: TFHIRResourceV): TBytes;

    function processRegistration(request: TIdHTTPRequestInfo; session : TFhirSession): String;
    function EndPointDesc(secure: boolean): String;
    function getReferencesByType(t : String) : String;
    function buildPackageList : String;
    function buildSessionsTable : String;

    function ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
    function ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
    function makeTaskRedirect(base, id : String; msg : String; fmt : TFHIRFormat; names : TStringList) : string;
    procedure SetAuthServer(const Value: TAuth2Server);
    function encodeAsyncResponseAsJson(request: TFHIRRequest; reqUrl: String; secure: boolean; fmt: TFHIRFormat; transactionTime: TFslDateTime; names: TStringList): string;
    procedure StopAsyncTasks;
  protected
    Function BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; const lang : THTTPLanguages; host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String; virtual; abstract;
    Function BuildFhirUploadPage(const lang : THTTPLanguages; host, sBaseURL: String; aType: String; Session: TFHIRSession): String; virtual; abstract;
    Function BuildFhirAuthenticationPage(const lang : THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params : String): String; virtual; abstract;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime; virtual; abstract;
    procedure GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String); virtual; abstract;
    Function ProcessZip(const lang : THTTPLanguages; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW; virtual; abstract;
    function DoSearch(Session: TFHIRSession; rtype: string; const lang : THTTPLanguages; params: String): TFHIRBundleW; virtual; abstract;
    function ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;

    procedure returnContent(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure : boolean; title, content : String); overload;
    function processContent(path: String; secure : boolean; title, content : String) : String;
    procedure checkRequestByJs(context : TOperationContext; request : TFHIRRequest);
    procedure doGetBundleBuilder(request : TFHIRRequest; context : TFHIRResponse; aType : TBundleType; out builder : TFhirBundleBuilder);

    function AutoCache : boolean; virtual;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon; endPoint : TStorageEndPoint);
    destructor Destroy; override;

    property Plugins : TFslList<TFHIRWebServerScriptPlugin> read FPlugins;
    property Context : TFHIRServerContext read FContext;
    property AuthServer : TAuth2Server read FAuthServer write SetAuthServer;
    property ServerContext : TFHIRServerContext read FContext;
    property TerminologyWebServer: TTerminologyWebServer read FTerminologyWebServer write SetTerminologyWebServer;

    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    procedure CheckAsyncTasks;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; override;
  end;

  TStorageEndPoint = class abstract (TFHIRServerEndPoint)
  private
  protected
    FServerContext : TFHIRServerContext;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
    destructor Destroy; override;
    property ServerContext : TFHIRServerContext read FServerContext;
    function cacheSize : UInt64; override;
    procedure clearCache; override;
  end;

implementation

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


{ ERestfulAuthenticationNeeded }

Constructor ERestfulAuthenticationNeeded.Create(Const sContext : String; sMessage, sCaption : String; const lang : THTTPLanguages);
begin
  inherited Create(sContext, HTTP_ERR_UNAUTHORIZED, itLogin, sMessage, lang);
  FMsg := sCaption;
end;

{ TStorageEndPoint }

function TStorageEndPoint.cacheSize: UInt64;
begin
  result := inherited cacheSize;
  if WebEndPoint <> nil then
    result := result  + (WebEndPoint as TStorageWebEndpoint).FContext.cacheSize;
end;

procedure TStorageEndPoint.clearCache;
begin
  inherited;
  if WebEndPoint <> nil then
    (WebEndPoint as TStorageWebEndpoint).FContext.clearCache;
end;

constructor TStorageEndPoint.Create(config: TFHIRServerConfigSection; settings: TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
begin
  inherited create(config, settings, db, common);
end;

destructor TStorageEndPoint.Destroy;
begin
  FServerContext.Free;
  inherited;
end;

{ TFHIRWebServerScriptPlugin }

procedure TFHIRWebServerScriptPlugin.ProcessFile(name: String; request: TIdHTTPRequestInfo; pm: THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; secure : boolean; variables : TFslMap<TFhirObject>);
begin
  FEndPoint.ReturnSecureFile(request, response, session, name, ChangeFileExt(FEndPoint.Common.SourceProvider.AltFile(request.Document, FEndPoint.PathNoSlash), '.secure.html'), '', secure, variables);
end;

function TFHIRWebServerScriptPlugin.processPage(request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV): boolean;
begin
  result := false;
end;

{ TFHIRWebServerCommunicator }

constructor TFHIRWebServerCommunicator.Create(ep: TStorageWebEndpoint; secure : boolean; session : TFHIRSession);
begin
  inherited create;
  FEndPoint := ep; // (no link)
  FSecure := secure;
  FSession := session;
end;

function TFHIRWebServerCommunicator.address: String;
begin
  result := FEndPoint.pathNoSlash;
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
  ctxt := TOperationContext.Create(opmInternal);
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

constructor TAsyncTaskThread.Create(server : TStorageWebEndpoint);
begin
  inherited Create; // suspended
  FServer := server;
  AutoFree := true;
end;

destructor TAsyncTaskThread.Destroy;
begin
  Files.free;
  FRequest.Free;
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
  FBundle := FServer.FContext.factory.wrapBundle(fserver.FContext.factory.makeResource('Bundle'));
  FBundle.type_ := aType;
  if context.Format = ffNDJson then
  begin
    files := TFslMap<TFslFile>.create('async.files');
    builder := TFHIRBundleBuilderNDJson.Create(FServer.FContext.factory.link, FBundle.link, IncludeTrailingPathDelimiter(FServer.Context.TaskFolder)+'task-'+inttostr(FKey), files.link)
  end
  else
    builder := TFHIRBundleBuilderSimple.Create(FServer.FContext.factory.link, FBundle.link);
end;

procedure TAsyncTaskThread.Execute;
var
  response : TFHIRResponse;
  op: TFHIROperationEngine;
  t: UInt64;
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

      t := GetTickCount64;
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
        ctxt := TOperationContext.create(opmRestful, ollNone);
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
      t := GetTickCount64 - t;
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
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
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
          c := FServer.FContext.factory.makeComposer(fserver.Context.ValidatorContext.link, ffJson, THTTPLanguages.create('en'), OutputStyleNormal)
        else
          c := FServer.FContext.factory.makeComposer(fserver.Context.ValidatorContext.link, Format, THTTPLanguages.create('en'), OutputStyleNormal);
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

procedure TAsyncTaskThread.SetServer(const Value: TStorageWebEndpoint);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TAsyncTaskThread.status(status: TAsyncTaskStatus; message: String);
begin
  FServer.Context.Storage.updateAsyncTaskStatus(key, status, message);
end;

{ TStorageWebEndpoint }

constructor TStorageWebEndpoint.Create(code, path: String; common: TFHIRWebServerCommon; endPoint: TStorageEndPoint);
begin
  inherited create(code, path, common);
  FEndPoint := endPoint;
  FContext := FEndPoint.FServerContext;
  FPlugins := TFslList<TFHIRWebServerScriptPlugin>.create;
  FAdaptors := TFslMap<TFHIRFormatAdaptor>.Create('adaptors');
  FThreads := TFslList<TAsyncTaskThread>.create;
end;

destructor TStorageWebEndpoint.Destroy;
begin
  StopAsyncTasks;
  FThreads.Free;
  FAdaptors.Free;
  FPlugins.Free;
  FTerminologyWebServer.free;
  FAuthServer.Free;
  inherited;
end;

procedure TStorageWebEndpoint.StopAsyncTasks;
var
  task : TAsyncTaskThread;
  done : boolean;
  i : integer;
begin
  done := false;
  Common.Lock.Lock;
  try
    for task in FThreads do
    begin
      task.Stop;
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


procedure TStorageWebEndpoint.SetAuthServer(const Value: TAuth2Server);
begin
  FAuthServer.Free;
  FAuthServer := Value;
end;

procedure TStorageWebEndpoint.SetTerminologyWebServer(const Value: TTerminologyWebServer);
begin
  FTerminologyWebServer.free;
  FTerminologyWebServer := Value;
end;

function TStorageWebEndpoint.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String;
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

    if OWinSecurityPlain and (((Session = nil) and (request.Document <> PathWithSlash + OWIN_TOKEN_PATH)) or not Context.UserProvider.allowInsecure) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + PathWithSlash + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
    end
    else if OWinSecurityPlain and Context.UserProvider.allowInsecure and (request.Document = PathWithSlash + OWIN_TOKEN_PATH) then
      HandleOWinToken(AContext, false, request, response);

    if (FAuthServer <> nil) and request.Document.StartsWith(FAuthServer.path) then
    begin
      result := 'Authorization Request';
      FAuthServer.HandleRequest(AContext, request, Session, response, false)
    end
    else if Common.SourceProvider.exists(Common.SourceProvider.AltFile(request.Document, PathNoSlash)) then
    begin
      result := 'Static File';
      ReturnSpecFile(response, request.Document, Common.SourceProvider.AltFile(request.Document, PathNoSlash), false)
    end
    else if request.Document.EndsWith('.hts') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.html')) then
    begin
      result := 'Processed File';
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.html'), false)
    end
    else if (request.Document = PathWithSlash+'.well-known/smart-configuration') then
    begin
      result := 'Smart Configuration';
      FAuthServer.HandleDiscovery(AContext, request, response)
    end
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestVersion(request.Document) then
    begin
      result := TerminologyWebServer.ProcessVersion(AContext, request, Session, response, false)
    end
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestInNoVersion(request.Document) then
    begin
      result := TerminologyWebServer.redirectToNoVersion(AContext, request, Session, response, false)
    end
    else if request.Document.StartsWith(PathWithSlash, false) then
    begin
      result := HandleRequest(AContext, request, response, false, false, PathWithSlash, id, Session, nil);
    end
    else
    // todo: extensions go here
    begin
      result := 'Not Found';
      response.ResponseNo := 404;
      response.ContentText := 'Document ' + request.Document + ' not found';
    end;
  finally
    session.Free;
  end;
end;

procedure TStorageWebEndpoint.HandleOWinToken(AContext: TIdContext; secure: boolean; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
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

function TStorageWebEndpoint.secureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String;
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

    if (FAuthServer <> nil) and request.Document.StartsWith(FAuthServer.path) then
    begin
      result := 'OAuth';
      FAuthServer.HandleRequest(AContext, request, Session, response, true)
    end
    else if OWinSecuritySecure and (request.Document = URLPath([PathNoSlash, OWIN_TOKEN_PATH])) then
    begin
      result := 'OWin';
      HandleOWinToken(AContext, true, request, response)
    end
    else if Common.SourceProvider.exists(Common.SourceProvider.AltFile(request.Document, PathNoSlash)) then
    begin
      result := 'Spec file '+request.Document;
      ReturnSpecFile(response, request.Document, Common.SourceProvider.AltFile(request.Document, PathNoSlash), true)
    end
    else if request.Document.EndsWith('.hts') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.html')) then
    begin
      result := 'Processed File '+request.Document;
      ReturnProcessedFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.html'), true)
    end
    else if request.Document.EndsWith('.html') and Common.SourceProvider.exists(ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.secure.html')) then
    begin
      result := 'Secure File '+request.Document;
      ReturnSecureFile(request, response, Session, request.Document, ChangeFileExt(Common.SourceProvider.AltFile(request.Document, PathNoSlash), '.secure.html'), id, true)
    end
    else if request.Document.StartsWith(PathNoSlash+'/scim') then
    begin
      result := 'SCIM';
      ProcessScimRequest(AContext, request, response, PathNoSlash)
    end
    else if request.Document.StartsWith(PathNoSlash, false) then
      result := HandleRequest(AContext, request, response, true, FAuthServer <> nil, PathNoSlash, id, Session, cert)
    else if OWinSecuritySecure and ((Session = nil) and (request.Document <> URLPath([PathNoSlash, OWIN_TOKEN_PATH]))) then
    begin
      response.ResponseNo := 401;
      response.ResponseText := 'Unauthorized';
      response.ContentText := 'Authorization is required (OWin at ' + PathNoSlash + OWIN_TOKEN_PATH + ')';
      response.CustomHeaders.AddValue('WWW-Authenticate', 'Bearer');
      result := 'Unauthorized';
    end
    else if request.Document = '/.well-known/openid-configuration' then
    begin
      result := 'OAuth Discovery';
      FAuthServer.HandleDiscovery(AContext, request, response)
    end
    else if request.Document.StartsWith(PathNoSlash, false) then
      result := HandleRequest(AContext, request, response, true, true, PathNoSlash, id, session, cert)
    else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestVersion(request.Document) then
      result := TerminologyWebServer.ProcessVersion(AContext, request, Session, response, true)
    else if request.Document = PathNoSlash then
    begin
      result := 'Home Page';
      ReturnProcessedFile(request, response, Session, '/hompage.html', Common.SourceProvider.AltFile('/homepage.html', PathNoSlash), true)
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

procedure TStorageWebEndpoint.ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
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




procedure TStorageWebEndpoint.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
begin
  ReturnProcessedFile(request, response, Session, path, path, secure, variables);
end;

procedure TStorageWebEndpoint.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
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
      s := s.Replace('[%specurl%]', Context.Factory.specUrl, [rfReplaceAll]);
      s := s.Replace('[%ver%]', Context.Factory.versionString, [rfReplaceAll]);
      s := s.Replace('[%path%]', PathNoSlash, [rfReplaceAll]);
      s := s.Replace('[%spath%]', PathNoSlash, [rfReplaceAll]);
//      s := s.Replace('[%web%]', WebDesc(secure), [rfReplaceAll]);
      s := s.Replace('[%admin%]', Common.AdminEmail, [rfReplaceAll]);
      if (Session = nil) then
        s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll])
      else
        s := s.Replace('[%logout%]', '|&nbsp;User: ' + Session.SessionName + '&nbsp; <a href="'+PathNoSlash+'/logout" title="Log Out"><img src="/logout.png"></a>  &nbsp;',
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

function TStorageWebEndpoint.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean; path: String; logId : String; esession: TFHIRSession; cert: TIdOpenSSLX509) : String;
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

              if not Common.Cache.respond(code, request, response, result) then
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
                        if AutoCache then
                          Context.CacheResponse := true;
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
                    ProcessOutput(oRequest, oResponse, request, response, relativeReferenceAdjustment, style, request.AcceptEncoding.Contains('gzip'), cache, result);
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

procedure TStorageWebEndpoint.ReturnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual, logid: String; secure: boolean; variables: TFslMap<TFHIRObject>);
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
    response.redirect(PathNoSlash+'/logout');
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
    token := FAuthServer.MakeLoginToken(PathNoSlash, apGoogle);
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


procedure TStorageWebEndpoint.ProcessScimRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; prefix : String);
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
    response.redirect(PathNoSlash+'/logout');
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

procedure TStorageWebEndpoint.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
var
  b : TFHIRBundleW;
begin
  if context.Format = ffNDJson then
    raise EFHIRException.CreateLang('NDJSON-ASYNC', request.Lang);
  b := FContext.factory.wrapBundle(FContext.factory.makeResource('Bundle'));
  b.type_ := aType;
  builder := TFHIRBundleBuilderSimple.Create(FContext.factory.link, b);
end;


Procedure TStorageWebEndpoint.ReadTags(header: String; request: TFHIRRequest);
// var
// s, s1, l, r, n, v : string;
// cat : TFHIRAtomCategory;
begin
  // raise EFHIRException.create('todo');
end;


function TStorageWebEndpoint.CheckSessionOK(Session: TFHIRSession; ip: string): boolean;
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

function TStorageWebEndpoint.ProcessRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  op: TFHIROperationEngine;
  t: UInt64;
  us, cs: String;
begin
  Common.stats.restStart;
  t := GetTickCount64;
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
  Common.Stats.restFinish(GetTickCount64 - t);
  if request.Session = nil then // during OAuth only
    us := 'user=(in-oauth)'
  else
    us := 'user=' + request.Session.UserName;
  if request.CommandType = fcmdOperation then
    cs := '$' + request.OperationName
  else
    cs := 'cmd=' + CODES_TFHIRCommandType[request.CommandType];
end;

procedure TStorageWebEndpoint.PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String);
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

Function TStorageWebEndpoint.BuildRequest(const lang : THTTPLanguages; sBaseURL, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept,
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
              oRequest.resource := FContext.factory.makeBinary(b, sContentType);
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
              oRequest.resource := FContext.factory.makeParamsFromForm(oPostStream);
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
                if oRequest.Version <> FContext.factory.version then
                begin
                  // todo FWebServer.convertFromVersion(oPostStream, oRequest.PostFormat, oRequest.Version, oRequest.Lang);
                  oRequest.CopyPost(oPostStream);
                end;

                if oRequest.PostFormat = ffUnspecified then
                  oRequest.PostFormat := detectFormat(oPostStream);

                if (oRequest.Version = fhirVersionRelease4) and (oRequest.PostFormat = ffunspecified) then
                  Raise ERestfulException.Create('TFhirWebServerCommonEndpoint.BuildRequest', HTTP_ERR_NOT_UNSUPPORTED_MEDIA_TYPE, itUnknown, 'Unsupported media type: '+sContentType, lang);

                parser := FContext.factory.makeParser(self.Context.ValidatorContext.link, oRequest.PostFormat, lang);
                try
                  oRequest.resource := parser.parseresource(oPostStream);

                  if (oRequest.CommandType = fcmdTransaction) and (oRequest.resource.fhirType <> 'Bundle') then
                  begin
                    bundle := FContext.factory.wrapBundle(FContext.factory.makeResource('Bundle'));
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

function TStorageWebEndpoint.readVersion(mt : String): TFHIRVersion;
var
  i, s, p, pi,l,r : string;
begin
  result := FContext.factory.version;

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

procedure TStorageWebEndpoint.returnContent(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure: boolean; title, content: String);
var
  vars :  TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.create;
  try
    vars.add('title', TFHIRObjectText.Create(title));
    vars.add('content', TFHIRObjectText.Create(content));
    vars.add('ver', TFHIRObjectText.Create(FContext.Factory.versionString));
    vars.add('specurl', TFHIRObjectText.Create(FContext.Factory.specUrl));
    returnFile(request, response, nil, path, 'template-nfhir.html', secure, vars);
  finally
    vars.free;
  end;
end;

Procedure TStorageWebEndpoint.ProcessOutput(oRequest: TFHIRRequest; oResponse: TFHIRResponse; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;
  relativeReferenceAdjustment: integer; style : TFHIROutputStyle; gzip, cache: boolean; summary : String);
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
          bin := FContext.factory.wrapBinary(res.link);
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
            oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffJson, oRequest.lang, style)
          else if oResponse.format = ffXhtml then
          begin
            oComp := TFHIRXhtmlComposer.Create(self.Context.ValidatorContext.link, style, oRequest.lang, oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).baseUrl := AppendForwardSlash(oRequest.baseUrl);
            TFHIRXhtmlComposer(oComp).Version := SERVER_FULL_VERSION;
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
            oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffXml, oRequest.lang, style)
          else if oResponse.format = ffText then
            oComp := TFHIRTextComposer.Create(self.Context.ValidatorContext.link, style, oRequest.lang)
          else if (FContext.factory.version <> fhirVersionRelease2) and ((oResponse.format = ffTurtle) or (res._source_format = ffTurtle)) then
          begin
            oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffTurtle, oRequest.lang, style);
            if (res <> nil) and (res.id <> '') then
              TFHIRTurtleComposerBase(oComp).url := AppendForwardSlash(oRequest.baseUrl) + res.fhirType + '/' + res.id;
          end
          else if res._source_format = ffJson then
            oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffJson, oRequest.lang, style)
          else
            oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffXml, oRequest.lang, style);
          try
            response.contentType := oComp.MimeType;
            oComp.SummaryOption := oRequest.Summary;
            oComp.ElementToCompose.Assign(oRequest.Elements);
            if (oComp.ElementToCompose.Count > 0) or (oComp.SummaryOption in [soSummary, soText, soData]) then
            begin
              meta := FContext.factory.wrapMeta(res);
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
// todo         if oResponse.Version <> FContext.factory.version then
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
      Common.Cache.recordResponse(code, request, response, summary);
  finally
    if ownsStream then
      stream.Free;
  end;
end;

procedure TStorageWebEndpoint.SendError(response: TIdHTTPResponseInfo; logid : string; status: word; format: TFHIRFormat; const lang : THTTPLanguages; message, url: String; e: exception; Session: TFHIRSession; addLogins: boolean; path: String; relativeReferenceAdjustment: integer; code: TFhirIssueType);
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
    issue := FContext.factory.wrapOperationOutcome(FContext.factory.makeResource('OperationOutcome'));
    try
      FContext.factory.setXhtml(issue.Resource, TFHIRXhtmlParser.Parse(lang, xppReject, [], '<div><p>' + FormatTextToXML(message, xmlText) + '</p></div>'));
      iss := FContext.factory.makeIssue(isError, code, '', message);
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
          oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffXml, lang, OutputStyleNormal);
        ffXhtml:
          begin
            oComp := TFHIRXhtmlComposer.Create(self.Context.ValidatorContext.link, OutputStyleNormal, lang, AppendForwardSlash(url));
            TFHIRXhtmlComposer(oComp).Version := SERVER_FULL_VERSION;
            TFHIRXhtmlComposer(oComp).Session := Session.link;
            TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          end;
        ffJson, ffNDJson:
          oComp := FContext.factory.makeComposer(self.Context.ValidatorContext.link, ffJson, lang, OutputStyleNormal);
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

function TStorageWebEndpoint.processProvenanceHeader(header : String; const lang : THTTPLanguages): TFhirProvenanceW;
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
      json := FContext.Factory.makeParser(self.Context.ValidatorContext.Link, ffJson, lang);
      try
        json.Source := ss;
        json.Parse;
        result := FContext.factory.wrapProvenance(json.resource.link);
      finally
        json.Free;
      end;
    finally
      ss.Free;
    end;
  end;
end;

function TStorageWebEndpoint.EncodeVersionsJson(r : TFHIRResourceV): TBytes;
var
  j : TJsonObject;
  a : TJsonArray;
  p : TFhirParametersW;
  pp : TFhirParametersParameterW;
  s : String;
begin
  p := FContext.factory.wrapParams(r.link);
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

function TStorageWebEndpoint.EncodeVersionsXml(r : TFHIRResourceV): TBytes;
var
  x : TMXmlDocument;
  p : TFhirParametersW;
  pp : TFhirParametersParameterW;
  s : String;
begin
  p := FContext.factory.wrapParams(r.link);
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

function TStorageWebEndpoint.processRegistration(request: TIdHTTPRequestInfo; session : TFhirSession): String;
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

function TStorageWebEndpoint.loadFromRsaDer(cert: string): TJWKList;
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

function TStorageWebEndpoint.EndPointDesc(secure: boolean): String;
begin
  result := '';
  if (secure) then
  begin
    result := result + ' <li><a href="http://' + Common.Host + port(Common.ActualPort, 80) + PathNoSlash + '">Unsecured access at ' + PathNoSlash +
        '</a> - direct access with no security considerations</li>'#13#10;
    if Common.ActualSSLPort <> 0 then
      result := result + ' <li><a href="' + PathNoSlash + '">Secured access at ' + PathNoSlash +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end
  else
  begin
    result := result + ' <li><a href="' + PathNoSlash + '">Unsecured access at ' + PathNoSlash +
        '</a> - direct access with no security considerations</li>'#13#10;
    if PathNoSlash <> '' then
      result := result + ' <li><a href="https://' + Common.Host + port(Common.ActualSSLPort, 443) + PathNoSlash + '">Secured access at ' + PathNoSlash +
        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
  end;
end;

function TStorageWebEndpoint.getReferencesByType(t: String): String;
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

type
  TPackageListSorter = class (TFslComparer<TFHIRPackageInfo>)
  public
    function Compare(const l, r : TFHIRPackageInfo) : integer; override;
  end;

function TPackageListSorter.Compare(const l, r : TFHIRPackageInfo) : integer;
begin
  result := CompareText(l.id, r.id);
end;

function TStorageWebEndpoint.AutoCache: boolean;
begin
  result := false;
end;

function TStorageWebEndpoint.buildPackageList: String;
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

function TStorageWebEndpoint.buildSessionsTable: String;
begin
  result := self.Context.SessionManager.buildTable;
end;

procedure TStorageWebEndpoint.checkRequestByJs(context: TOperationContext; request: TFHIRRequest);
begin
  // js-todo - figure out which scripts to run, and then run them
end;

function TStorageWebEndpoint.encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; secure : boolean; fmt : TFHIRFormat; transactionTime: TFslDateTime; names: TStringList): string;
var
  j, o : TJsonObject;
  a : TJsonArray;
  s : String;
begin
  j := TJsonObject.Create;
  try
    j.str['transactionTime'] := transactionTime.toXML;
    j.str['request'] := reqUrl;
    j.bool['secure'] := secure;
    a := j.forceArr['output'];
    for s in names do
    begin
      o := a.addObject;
      o.str['url'] := request.baseUrl+'task/'+request.id+'/'+s+EXT_WEB_TFHIRFormat[fmt];
      o.str['type'] := s;
    end;

    result := TJSONWriter.writeObjectStr(j, true);
  finally
    j.Free;
  end;
end;


function TStorageWebEndpoint.ProcessTaskRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
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
            response.resource := FContext.factory.BuildOperationOutcome(request.Lang, 'The source for file '+ExtractFileName(f)+' could not be found');
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
              response.Body := encodeAsyncResponseAsJson(request, originalRequest, secure, fmt, transactionTime, names);
            end;
            end;
          atsTerminated, atsError :
            begin
            response.HTTPCode := 500;
            response.Message := 'Error';
            fmt := ffJson;
            if length(outcome) > 0 then
            begin
              p := FContext.factory.makeParser(self.Context.ValidatorContext.link, fmt, THTTPLanguages.create('en'));
              try
                response.resource := p.parseResource(outcome);
              finally
                p.Free;
              end;
            end
            else
              response.resource := FContext.factory.BuildOperationOutcome(request.Lang, message);
            end;
          atsAborted:
            begin
            response.HTTPCode := 400;
            response.Message := 'Error';
            response.resource := FContext.factory.BuildOperationOutcome(request.Lang, 'This task has been cancelled');
            end;
          atsDeleted:
            begin
            response.HTTPCode := 404;
            response.Message := 'Not found';
            response.Resource := FContext.factory.BuildOperationOutcome(THTTPLanguages.create('en'), 'Task has been deleted', itUnknown);
            end;
        end;
      end
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not found';
      response.Resource := FContext.factory.BuildOperationOutcome(THTTPLanguages.create('en'), 'Unknown task', itUnknown);
    end;
  finally
    names.Free;
  end;
end;

function TStorageWebEndpoint.ProcessAsyncRequest(Context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  thread : TAsyncTaskThread;
  id : String;
begin
  if not (request.CommandType in [fcmdSearch, fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem, fcmdTransaction, fcmdBatch, fcmdUpload, fcmdOperation]) then
    raise EFHIRException.CreateLang('NO_ASYNC', request.Lang);
  thread := TAsyncTaskThread.create(self);
  Common.Lock.Lock;
  try
    FThreads.add(thread);
  finally
    Common.Lock.Unlock;
  end;
  id := NewGuidId;
  if request.Parameters.has('_outputFormat') then
    thread.Format := mimeTypeToFormat(request.Parameters['_outputFormat'])
  else
    thread.Format := response.Format;
  thread.key := self.Context.Storage.createAsyncTask(request.url, id, thread.Format, request.secure);
  result := 'Async Request => '+id;
  thread.request := request.Link;
  thread.Start;
  response.HTTPCode := 202;
  response.Message := 'Accepted';
  response.ContentLocation := request.baseUrl+'task/'+id;
  if response.format = ffXhtml then
  begin
    response.ContentType := 'text/html';
    response.Body := makeTaskRedirect(request.baseUrl, id, 'Preparing', thread.Format, nil);
  end;
end;

procedure TStorageWebEndpoint.CheckAsyncTasks;
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

function TStorageWebEndpoint.processContent(path: String; secure: boolean; title, content: String): String;
var
  vars :  TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.create;
  try
    vars.add('title', TFHIRObjectText.Create(title));
    vars.add('content', TFHIRObjectText.Create(content));
    vars.add('ver', TFHIRObjectText.Create(FContext.Factory.versionString));
    vars.add('specurl', TFHIRObjectText.Create(FContext.Factory.specUrl));
    result := processFile(nil, path, 'template-fhir.html', secure, vars);
  finally
    vars.free;
  end;
end;

function TStorageWebEndpoint.makeTaskRedirect(base, id: String; msg : String; fmt : TFHIRFormat; names: TStringList): string;
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

end.



