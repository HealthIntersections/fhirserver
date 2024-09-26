  Unit web_server;

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

{

  how security works for the FHIR Server

  The FHIR Server offers 2 end-points - secure, and insecure.

  The insecure API grants full access to the store, and never applies any security, on the specified port and path.
  You can turn this on or off by controlling seting the port in the ini file that controls the server

  [web]
  http=[port]
  base=/path

  The server can also provide a secure end-point:

  [web]
  https=[port]
  secure=/path  ; must be a different port to the unsecured end-point.
  modes=

  The modes setting controls which kind of security models are applied to the interface.
  It is a comma separated list of the following codes:

  oauth - OAuth2 using the Smart-on-fhir profile. Users must use a registered client, and idenitfy themselves
  notes:
  - clients are configured in auth.ini
  - user accounts / default right are configured through the SCIM server

  owin - Client must authenticate using the owin protocol, and then pass the owin token in the bearer.
  notes:
  - client details are configured.... where....?
  - client is trusted, and users are not authenticated. Client has access to all resources

  cert - Client must provide an SSL certificate, which must be pre-registered on the server.
  notes:
  - client certificates are registered in auth.ini. See notes there
  - how users are handled depends on settings in auth.ini
}

Interface

Uses
  {$IFDEF WINDOWS} Windows, ActiveX, ComObj, {$ENDIF}
  SysUtils, Classes, IniFiles, Generics.Collections, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer, IdTCPServer, IdContext, IdHTTP, IdCookie, IdZLibCompressorBase, IdSSL, IdSMTP,
  IdCompressorZLib, IdZLib, IdSchedulerOfThreadPool, IdGlobalProtocols, IdMessage, IdExplicitTLSClientServerBase, IdGlobal, fsl_websocket,
  IdOpenSSLIOHandlerServer, IdOpenSSLIOHandlerClient, IdOpenSSLVersion, IdOpenSSLX509, IdIOHandler,

  fsl_base, fsl_utilities, fsl_crypto, fsl_logging, fsl_stream, fsl_collections, fsl_threads, fsl_json, fsl_xml,
  {$IFDEF WINDOWS} fsl_msxml, fsl_service_win, {$ENDIF}
  fsl_openssl, fsl_http, fdb_manager, fdb_logging, fsl_htmlgen, fdb_dialects, fsl_rdf, fsl_graphql, fsl_twilio,

  {$IFDEF WINDOWS}
  fdb_odbc,
  {$ENDIF}
  fhir_objects, fhir_parser,  fhir_xhtml, fhir_utilities, fhir_common, fhir_factory, fhir_client, fhir_pathengine,
  fhir_client_http,
  package_spider, fsl_npm_client, fsl_npm, fsl_npm_cache,
  fhir_oauth, fhir_cdshooks,
  fhir_graphql, fhir_ndjson,
  {$IFNDEF NO_CONVERSION} fxver_convertors,{$ENDIF}
  tx_server, tx_manager, ftx_sct_expressions, ftx_loinc_services, ftx_loinc_publisher, tx_webserver, ftx_service,
  tags, session, storage, security, html_builder, ftx_sct_services, ftx_sct_publisher, server_config, server_stats,
  scim_server,
  auth_manager, reverse_client, cds_hooks_server, web_source, analytics, bundlebuilder, server_factory,
  user_manager, server_context, server_constants, utilities, jwt, usage_stats,
  subscriptions, twilio, time_tracker,
  web_base, endpoint, endpoint_storage;

Type

  { TFHIRHTTPConnectionInfo }

  TFHIRHTTPConnectionInfo = class (TFslObject)
  private
    FRequest : TIdHTTPRequestInfo;
    FThreadId : TThreadID;
    FContext: TIdContext;
    FClientIP : string;
  public
    constructor create(request : TIdHTTPRequestInfo; context: TIdContext);
    destructor Destroy; override;
    function link : TFHIRHTTPConnectionInfo;
    function log : String;
  end;

  TFhirWebServer = class;

  TFHIRHTTPServer = class(TIdHTTPServer)
  private
    FServer : TFhirWebServer;
  protected
    procedure DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler); override;
  end;

  TFHIRPathServerObject = class (TFHIRObject)
  protected
    FContext : TFHIRServerContext;
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function GetFhirObjectVersion: TFHIRVersion; override;
  public
    Constructor Create(context : TFHIRServerContext);
    destructor Destroy; override;

    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function fhirType : String; override;
    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function primitiveValue : string; override;
    function ToString : String; override;
  end;

  TFHIRWebServerExtension = class abstract (TFHIRPathEngineExtension)
  protected
    FContext : TFHIRServerContext;
  public
    Constructor Create(context : TFHIRServerContext);
    destructor Destroy; override;

    function resolveConstant(context : TFHIRPathExecutionContext; s : String; var obj : TFHIRObject) : boolean; override;
  end;

  { TFhirWebServer }

  TFhirWebServer = Class (TFHIRWebServerBase)
  Private
    FSettings : TFHIRServerSettings;

    // base web server configuration
    FActive: boolean;
    // can start without actually making the web servers available - for internal use e.g. loading...
    FHomePage: String;
    FFacebookLike: boolean;

    // web configuration
    FCertFile: String;
    FRootCertFile: String;
    FSSLPassword: String;
    FInLog : TLogger;
    FOutLog : TLogger;
    FLogFolder : String;
    FRobotsText : String;

    // operational fields
    FUsageServer : TUsageStatsServer;
    FPlainServer: TIdHTTPServer;
    FSSLServer: TIdHTTPServer;
    FIOHandler: TIdOpenSSLIOHandlerServer {TIdServerIOHandlerSSLOpenSSL};
    FClients: TFslList<TFHIRWebServerClientInfo>;
    FEndPoints : TFslList<TFhirWebServerEndpoint>;
    FSecureCount, FPlainCount : Integer;
    FStats : TStatusRecords;
    FLock : TFslLock;
    FLiveConnections : TFslList<TFHIRHTTPConnectionInfo>;
    function insertValue(n: String; secure: boolean; variables: TFslMap<TFHIRObject>): String;
    function isLogging : boolean;
    procedure logRequest(secure : boolean; id, clientIP : String; request : TIdHTTPRequestInfo);
    procedure logResponse(id : String; resp : TIdHTTPResponseInfo);
    procedure logCrash(secure : boolean; id, clientIP : String; request : TIdHTTPRequestInfo; resp : TIdHTTPResponseInfo);
    function WebDump: String;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    procedure MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkExit(AContext: TIdContext);
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);

    function endpointList: String;
    Function WebDesc(secure : boolean): String;
    function AbsoluteURL(secure: boolean) : String;

    Procedure ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: boolean);
    procedure SSLPassword(Sender: TObject; var Password: string; const IsWrite: Boolean);
    procedure DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);

    function getClientId(AContext: TIdContext; request: TIdHTTPRequestInfo) : String;
    function getClientIP(AContext: TIdContext; request: TIdHTTPRequestInfo) : String;

    procedure ProcessFile(sender : TObject; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>; var result : String);
    procedure ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>); overload;
    procedure ReturnFileSource(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String); overload;
    Procedure ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    function  ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
    function  ReturnDBDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
    function  ReturnStatistics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure, asHtml: boolean) : String;

    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure logOutput(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : string; tt : TTimeTracker; secure : boolean; epn, summ : string);

    Procedure StartServer();
    Procedure StopServer;
  Public
    constructor Create(settings : TFHIRServerSettings; name: String);
    destructor Destroy; Override;
    procedure loadConfiguration(ini : TFHIRServerConfigFile);
    property settings : TFHIRServerSettings read FSettings;
    property stats : TStatusRecords read FStats;
    property RobotsText : String read FRobotsText write FRobotsText;

    procedure DoVerifyPeer(Sender: TObject; const x509: TIdOpenSSLX509; const VerifyResult: Integer; const Depth: Integer; var Accepted: Boolean); // private (hint busting)

    Procedure Start; // (active, threads: boolean);
    Procedure Close;
    Procedure Stop;
    procedure recordStats(rec : TStatusRecord);

    Procedure clearCache;
    procedure SetCacheStatus(status : boolean);
    procedure getCacheInfo(ci: TCacheInformation);
    property EndPoints : TFslList<TFhirWebServerEndpoint> read FEndPoints;
    function EndPoint(name : String) : TFhirWebServerEndpoint;

    function GetCurrentRequestReport : String;
    function GetCurrentRequestCount : integer;

    procedure registerEndPoint(endPoint : TFHIRServerEndPoint);
  End;

Implementation

Uses
{$IFDEF WINDOWS}
  Registry,
{$ENDIF}
  fsl_oauth{$IFDEF COVID}, FHIR.Server.Covid{$ENDIF};

{ TFHIRHTTPConnectionInfo }

constructor TFHIRHTTPConnectionInfo.create(request: TIdHTTPRequestInfo; context: TIdContext);
begin
  inherited Create;
  FRequest := request;
  FContext := context;
  FThreadId := GetCurrentThreadId;
end;

destructor TFHIRHTTPConnectionInfo.Destroy;
begin
  // nothing
  inherited Destroy;
end;

function TFHIRHTTPConnectionInfo.link: TFHIRHTTPConnectionInfo;
begin
  result := TFHIRHTTPConnectionInfo(inherited Link);

end;

function TFHIRHTTPConnectionInfo.log: String;
var
  s : String;
begin
  s := NameLockedToThread(FThreadId);
  if s <> '' then
    result := '-->! '+FClientIP+'/'+FRequest.RawHTTPCommand+': '+GetThreadInfoForThread(FThreadId)+', locks = '+s
  else
    result := FClientIP+'/'+FRequest.RawHTTPCommand+': '+GetThreadInfoForThread(FThreadId);
end;


{ TFhirWebServer }

function TFhirWebServer.AbsoluteURL(secure: boolean): String;
begin
  if secure then
    result := 'https://'+common.host+SSLPort(false)+'/'
  else
    result := 'http://'+common.host+HTTPPort(false)+'/'
end;

procedure TFhirWebServer.clearCache;
begin
  Common.cache.Clear;
end;

procedure TFhirWebServer.Close;
begin
  FActive := false;
end;

constructor TFhirWebServer.Create(settings: TFHIRServerSettings; name: String);
Begin
  Inherited Create(nil);
  FEndPoints := TFslList<TFhirWebServerEndpoint>.Create;
  self.Common.Name := Name;
  FInLog := nil;

  FSettings := settings;
  FClients := TFslList<TFHIRWebServerClientInfo>.Create;
  FStats := TStatusRecords.Create;
  FLock := TFslLock.create('web.connnections');
  FLiveConnections := TFslList<TFHIRHTTPConnectionInfo>.create;
End;

destructor TFhirWebServer.Destroy;
Begin
  FLiveConnections.free;
  FLock.free;
  FStats.free;
  FUsageServer.free;
  FEndPoints.free;
  FSettings.free;
  FClients.free;
  FInLog.free;
  FOutLog.free;
  Inherited;
End;


procedure TFhirWebServer.loadConfiguration(ini : TFHIRServerConfigFile);
var
  fn: String;
  txu: String;
begin
  FLogFolder := ini.admin['log-folder'].value;
  if (FLogFolder <> '') then
    Logging.log('Logging HTTP Requests/Responses to '+FLogFolder);
  fn := ini.admin['logging-in'].value;
  if (fn <> '') and ((fn <> '-')) then
  begin
    FInLog := TLogger.Create(filePath(['[tmp]', fn]));
    FInLog.Policy.FullPolicy := lfpChop;
    FInLog.Policy.MaximumSize := 100*1024*1024;
    FInLog.Policy.AllowExceptions := false;
  end;

  fn := ini.admin['logging-out'].value;
  if (fn <> '') and ((fn <> '-')) then
  begin
    FOutLog := TLogger.Create(filePath(['[tmp]', fn]));
    FOutLog.Policy.FullPolicy := lfpChop;
    FOutLog.Policy.MaximumSize := 300*1024*1024;
    FOutLog.Policy.AllowExceptions := false;
  end;

  // web identity / configuration
  FHomePage := 'homepage.html';
  FFacebookLike := ini.identityProviders.section['facebook.com']['like'].value = 'true';
  Common.Host := ini.web['host'].value;

  // web server configuration
  Common.workingPort := ini.web['http'].readAsInt;
  Common.workingSSLPort := ini.web['https'].readAsInt;
  Common.StatedPort := ini.web['rproxy-http'].readAsInt(Common.workingPort);
  Common.StatedSSLPort := ini.web['rproxy-https'].readAsInt(Common.workingSSLPort);
  Common.CertHeader := ini.web['rproxy-cert-header'].value;
  Common.SSLHeaderValue := ini.web['rproxy-ssl-value'].value;

  FCertFile := ini.web['certname'].value;
  FRootCertFile := ini.web['cacertname'].value;
  FSSLPassword := ini.web['password'].value;
  Common.ConnLimit := ini.web['http-max-conn'].readAsInt(DEF_SERVER_CONN_LIMIT);

  NoUserAuthentication := ini.web['no-auth'].readAsBool;
  UseOAuth := ini.web['oauth'].readAsBool(true);
  OWinSecuritySecure := ini.web['owin'].readAsBool;
  OWinSecurityPlain := ini.web['owin-http'].readAsBool;
  ServeMissingCertificate := ini.web['no-cert'].readAsBool;
  ServeUnknownCertificate := ini.web['unknown-cert'].readAsBool;
  ServeMissingJWT := ini.web['no-jwt'].readAsBool(true);
  ServeUnverifiedJWT := ini.web['unverified-jwt'].readAsBool;
  FUsageServer := TUsageStatsServer.Create(ini.web['stats-dir'].value);

  Common.OwnerName := ini.admin['ownername'].value;
  if Common.OwnerName = '' then
    Common.OwnerName := 'Health Intersections';
  Common.AdminEmail := ini.admin['email'].value;
  if Common.AdminEmail = '' then
    raise EFHIRException.Create('An admin email is required');

  if (ini.web['robots.txt'].value <> '') then
    FRobotsText := FileToString(ini.web['robots.txt'].value, TEncoding.UTF8);

  if Common.StatedPort = 80 then
    txu := 'http://' + Common.Host
  else
    txu := 'http://' + Common.Host + ':' + inttostr(Common.StatedPort);

  Common.Google.serverId := ini.web['googleid'].value;
  Common.Cache.caching := ini.web['caching'].readAsBool;
end;

procedure TFhirWebServer.DoConnect(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
  SetThreadStatus('Connecting');
  ci := TFHIRWebServerClientInfo.Create;

  Common.Lock.Lock('DoConnect');
  try
    FClients.Add(ci);
    AContext.Data := ci;
    ci.Context := AContext;
    if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
    begin
      inc(FSecureCount);
      SetThreadName('https:'+AContext.Binding.PeerIP);
      TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;
    end
    else
    begin
      inc(FPlainCount);
      SetThreadName('http:'+AContext.Binding.PeerIP);
    end;
    inc(GCounterWebConnections);
  finally
    Common.Lock.Unlock;
  end;
  AContext.Connection.IOHandler.ReadTimeout := 60*1000;
  AContext.Connection.IOHandler.MaxLineLength := 100 * 1024;

{$IFDEF WINDOWS}
  CoInitialize(nil);
{$ENDIF}
  SetThreadStatus('Connected');
end;

procedure TFhirWebServer.DoDisconnect(AContext: TIdContext);
begin
  try
    SetThreadStatus('Disconnecting');
    if AContext.Data <> nil then
    begin
      Common.Lock.Lock('DoDisconnect');
      try
        FClients.Remove(TFHIRWebServerClientInfo(AContext.Data));
        AContext.Data := nil;
        InterlockedDecrement(GCounterWebConnections);
        if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
          InterlockedDecrement(FSecureCount)
        else
          InterlockedDecrement(FPlainCount);
      finally
        Common.Lock.Unlock;
      end;
    {$IFDEF WINDOWS}
      CoUninitialize;
    {$ENDIF}
    end;
    SetThreadStatus('Disconnected');
  except
    on e : Exception do
      writeln('Exception on disconnect: '+e.Message);
  end;
end;

procedure TFhirWebServer.DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  VUseSSL := true;
end;

procedure TFhirWebServer.DoVerifyPeer(Sender: TObject; const x509: TIdOpenSSLX509; const VerifyResult: Integer; const Depth: Integer; var Accepted: Boolean);
var
  i: integer;
begin
  Accepted := ServeUnknownCertificate or CertificateIdList.Find(x509.SerialNumber, i);
end;


function TFhirWebServer.WebDesc(secure : boolean): String;
begin
  if (Common.StatedPort = 0) then
    result := 'Port ' + inttostr(Common.StatedSSLPort) + ' (https)'
  else if Common.StatedSSLPort = 0 then
    result := 'Port ' + inttostr(Common.StatedPort) + ' (http)'
  else if secure then
    result := '<a href="'+absoluteUrl(false)+'">Port ' + inttostr(Common.StatedPort) + ' (http)</a> and Port ' + inttostr(Common.StatedSSLPort) + ' (https - this server)'
  else
    result := 'Port ' + inttostr(Common.StatedPort) + ' (http - this server) and <a href="'+absoluteUrl(true)+'">Port ' + inttostr(Common.StatedSSLPort) + ' (https)</a>'
end;


function TFhirWebServer.EndPoint(name: String): TFhirWebServerEndpoint;
var
  t : TFhirWebServerEndpoint;
begin
  result := nil;
  for t in FEndPoints do
    if t.code = name then
      exit(t);
end;

function TFhirWebServer.GetCurrentRequestReport: String;
var
  conn : TFHIRHTTPConnectionInfo;
begin
  FLock.lock('GetCurrentRequestReport');
  try
    result := 'Current Web Requests: '+inttostr(FLiveConnections.count);
    for conn in FLiveConnections do
      result := result + '|' + conn.log;
  finally
    FLock.Unlock;
  end;
end;

function TFhirWebServer.GetCurrentRequestCount: integer;
var
  conn : TFHIRHTTPConnectionInfo;
begin
  FLock.lock('GetCurrentRequestCount');
  try
    result := FLiveConnections.count;
  finally
    FLock.Unlock;
  end;
end;

procedure TFhirWebServer.Start; // (active, threads: boolean);
var
  s : String;
Begin
  if Common.ConnLimit = 0 then
    s := ', No connection limit'
  else
    s := ', Limited to '+inttostr(Common.ConnLimit)+' connections';

  Logging.log('Start Web Server:');
  if (Common.StatedPort = 0) then
    Logging.log('  http: not active')
  else if Common.workingPort = common.statedPort then
    Logging.log('  http: listen on ' + inttostr(Common.StatedPort)+s)
  else
    Logging.log('  http: listen on ' + inttostr(Common.WorkingPort)+' for '+inttostr(common.statedPort)+s);

  if (Common.StatedSSLPort = 0) then
    Logging.log('  https: not active')
  else if Common.workingSSLPort = common.statedSSLPort then
    Logging.log('  https: listen on ' + inttostr(Common.statedSSLPort)+s)
  else
    Logging.log('  https: listen on ' + inttostr(Common.WorkingSSLPort)+' for '+inttostr(common.statedSSLPort)+s);
  FActive := true;
  Common.Stats.Start;
  StartServer;
End;

procedure TFhirWebServer.Stop;
Begin
  FActive := false;
  StopServer;
End;

procedure TFhirWebServer.recordStats(rec : TStatusRecord);
begin
  rec.Requests := Common.Stats.TotalCount;
  rec.RequestTime := Common.Stats.TotalTime;
  rec.ConnCount := FClients.Count;
  FStats.addToList(rec);
end;

procedure TFhirWebServer.StartServer;
Begin
  if Common.WorkingPort > 0 then
  begin
    FPlainServer := TFHIRHTTPServer.Create(Nil);
    (FPlainServer as TFHIRHTTPServer).FServer := self;
    FPlainServer.Name := 'http';
//    FPlainServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
//    TIdSchedulerOfThreadPool(FPlainServer.Scheduler).PoolSize := 20;
//    TIdSchedulerOfThreadPool(FPlainServer.Scheduler).RetainThreads := false;
    FPlainServer.ServerSoftware := 'Health Intersections FHIR Server';
    FPlainServer.ParseParams := false;
    FPlainServer.DefaultPort := Common.WorkingPort;
    FPlainServer.KeepAlive := PLAIN_KEEP_ALIVE;
    FPlainServer.OnCreatePostStream := CreatePostStream;
    FPlainServer.OnCommandGet := PlainRequest;
    FPlainServer.OnCommandOther := PlainRequest;
    FPlainServer.OnConnect := DoConnect;
    FPlainServer.OnDisconnect := DoDisconnect;
    FPlainServer.OnParseAuthentication := ParseAuthenticationHeader;
    FPlainServer.MaxConnections := Common.ConnLimit;
    FPlainServer.active := true;
  end;
  if Common.WorkingSSLPort > 0 then
  begin
    Logging.log('  SSL Certificate: '+FCertFile);
    Logging.log('  SSL CA Cert '+FRootCertFile);
    Logging.log('  SSL Key File: '+ChangeFileExt(FCertFile, '.key'));
    Logging.log('  SSL Password: '+PadString('', length(FSSLPassword), '*'));

    If Not FileExists(FCertFile) Then
      raise EIOException.Create('SSL Certificate "' + FCertFile + ' could not be found');
    If Not FileExists(ChangeFileExt(FCertFile, '.key')) Then
      raise EIOException.Create('SSL Certificate Private Key "' + ChangeFileExt(FCertFile, '.key') + ' could not be found');
    If (FRootCertFile <> '') and (Not FileExists(FRootCertFile)) Then
      raise EIOException.Create('SSL Certificate "' + FRootCertFile + ' could not be found');
    FSSLServer := TFHIRHTTPServer.Create(Nil);
    (FSSLServer as TFHIRHTTPServer).FServer := self;
    FSSLServer.Name := 'https';
//    FSSLServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
//    TIdSchedulerOfThreadPool(FSSLServer.Scheduler).PoolSize := 20;
    FSSLServer.ServerSoftware := 'Health Intersections FHIR Server';
    FSSLServer.ParseParams := false;
    FSSLServer.DefaultPort := Common.WorkingSSLPort;
    FSSLServer.KeepAlive := SECURE_KEEP_ALIVE;
    FSSLServer.OnCreatePostStream := CreatePostStream;
    FIOHandler := TIdOpenSSLIOHandlerServer.Create(nil);
    FSSLServer.IOHandler := FIOHandler;
    FSSLServer.OnQuerySSLPort := DoQuerySSLPort;

    FIOHandler.Options.CertFile := FCertFile;
    FIOHandler.Options.CertKey := ChangeFileExt(FCertFile, '.key');
    FIOHandler.Options.VerifyCertificate := FRootCertFile;
    FIOHandler.Options.OnGetPassword := SSLPassword;

    FIOHandler.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
    FIOHandler.Options.TLSVersionMaximum := TIdOpenSSLVersion.TLSv1_3;
    FIOHandler.Options.UseServerCipherPreferences := true;
    FIOHandler.Options.AllowUnsafeLegacyRenegotiation := true;
    FIOHandler.Options.UseLegacyServerConnect := true;

    FSSLServer.OnCommandGet := SecureRequest;
    FSSLServer.OnCommandOther := SecureRequest;
    FSSLServer.OnConnect := DoConnect;
    FSSLServer.OnDisconnect := DoDisconnect;
    FSSLServer.OnParseAuthentication := ParseAuthenticationHeader;
    FSSLServer.MaxConnections := Common.ConnLimit;
    FSSLServer.active := true;
    Logging.log('  SSL Started');
  end;
end;

procedure TFhirWebServer.StopServer;
Begin
  if FSSLServer <> nil then
  begin
    FSSLServer.active := false;
    FSSLServer.Scheduler.free;
    FreeAndNil(FSSLServer);
    FreeAndNil(FIOHandler);
  end;
  if FPlainServer <> nil then
  begin
    FPlainServer.active := false;
    FPlainServer.Scheduler.free;
    FreeAndNil(FPlainServer);
  end;
End;

function TFhirWebServer.WebDump: String;
var
  b: TFslStringBuilder;
  ci: TFHIRWebServerClientInfo;
begin
  b := TFslStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.Append('<tr><td>IP address</td><td>Count</td><td>Session</td><td>Activity</td><td>Length</td></tr>'#13#10);
    Common.Lock.Lock('WebDump');
    try
      for ci in FClients do
      begin
        b.Append('<tr><td>');
        b.Append(ci.Context.Binding.PeerIP);
        b.Append('</td><td>');
        b.Append(inttostr(ci.Count));
        b.Append('</td><td>');
        if (ci.Session <> nil) then
          b.Append(inttostr(ci.Session.Key));
        b.Append('</td><td>');
        b.Append(ci.Activity);
        b.Append('</td><td>');
        if ci.Start > 0 then
          b.Append(inttostr(GetTickCount64 - ci.Start));
        b.Append('</td></tr>'#13#10);
      end;
    finally
      Common.Lock.Unlock;
    end;
    b.Append('</table>'#13#10);
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TFhirWebServer.CreatePostStream(AContext: TIdContext;
  AHeaders: TIdHeaderList; var VPostStream: TStream);
Begin
  VPostStream := TMemoryStream.Create;
End;

procedure TFhirWebServer.ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: boolean);
begin
  VHandled := SameText(AAuthType, 'Bearer');
  VUsername := INTERNAL_SECRET;
  VPassword := AAuthData;
end;

function letterForOp(request : TIdHTTPRequestInfo) : String;
begin
  case request.CommandType of
    hcUnknown : result := 'u';
    hcHEAD : result := 'h';
    hcGET : result := 'g';
    hcPOST : result := 'p';
    hcDELETE : result := 'd';
    hcPUT : result := 'l';
    hcTRACE : result := 't';
    hcOPTION  : result := 'o';
    hcPATCH : result := 'a';
  end;
end;

procedure TFhirWebServer.logOutput(AContext: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: string;
  tt: TTimeTracker; secure: boolean; epn, summ: string);
  function mimeType(mt : String) : String;
  var
    f : TFHIRFormat;
  begin
    if mt = '' then
      result := '-'
    else
    begin
      f := mimeTypeToFormat(mt, ffUnspecified);
      case f of
        ffUnspecified: result := '?';
        ffXml: result := 'x';
        ffJson: result := 'j';
        ffTurtle: result := 'r';
        ffText: result := 't';
        ffNDJson: result := 'n';
        ffXhtml: result := 'h';
      end;
    end;
  end;
var
  s : String;
begin
  s := id + ' ' +
       StringPadLeft(inttostr(tt.total), ' ', 4) + ' ' +
       Logging.MemoryStatus(false) +' '+Logging.CPU.usage+ ' #'+inttostr(GetCurrentRequestCount)+' ';
  if (FPlainServer <> nil) and (FSSLServer <> nil) then
    s := s + StringPadLeft(inttostr(FPlainServer.Contexts.count)+':'+inttostr(FSSLServer.Contexts.count), ' ', 5) + ' '
  else if (FPlainServer <> nil) then
    s := s + StringPadLeft(inttostr(FPlainServer.Contexts.count), ' ', 2) + ' '
  else // (FSSLServer <> nil)
    s := s + StringPadLeft(inttostr(FSSLServer.Contexts.count), ' ', 2) + ' ';

  if secure then
    s := s + letterForOp(request).ToUpper
  else
    s := s + letterForOp(request);
  s := s + mimeType(request.ContentType);
  s := s + mimeType(response.ContentType)+' ';
  s := s + inttostr(response.ResponseNo)+' '+
           epn+' '+
           getClientId(aContext, request)+' ';
  if (summ = '') then
    s := s + '('+request.RawHTTPCommand+')'
  else
    s := s + summ;
  Logging.log(s);
end;

procedure TFhirWebServer.PlainRequest(AContext: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  id, summ : string;
  ep : TFhirWebServerEndpoint;
  ok : boolean;
  epn, cid, ip : String;
  tt : TTimeTracker;
  ci : TFHIRHTTPConnectionInfo;
begin
  ci := TFHIRHTTPConnectionInfo.create(request, AContext);
  try
    FLock.lock('PlainRequest');
    try
      FLiveConnections.add(ci.link);
    finally
      FLock.Unlock;
    end;

    // when running with a reverse proxy, it's easier to let the reverse proxy just use non-ssl upstream, and pass through the certificate details se we know SSL is being used
    if (Common.SSLHeaderValue <> '') and (request.RawHeaders.Values['X-Client-SSL'] = Common.SSLHeaderValue) then
      SecureRequest(aContext, request, response)
    else
    begin
      ip := getClientIP(AContext, request);
      ci.FClientIP := ip;
      tt := TTimeTracker.Create;
      try
        InterlockedIncrement(GCounterWebRequests);
        SetThreadStatus('Processing '+request.Document);
        epn := '??preq';
        summ := request.document;
        MarkEntry(AContext, request, response);
        try
          id := FSettings.nextRequestId;
          logRequest(false, id, ip, request);
          response.CustomHeaders.Add('X-Request-Id: '+id);
          if (request.CommandType = hcOption) then
          begin
            response.ResponseNo := 200;
            response.ContentText := 'ok';
            response.CustomHeaders.Add('Access-Control-Allow-Credentials: true');
            response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
            response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
            epn := '--';
            summ := 'options?';
          end
          else if (request.Document = '/robots.txt') and (RobotsText <> '') then
          begin
            response.ResponseNo := 200;
            response.ResponseText := 'OK';
            response.ContentText := RobotsText;
          end
          else if FUsageServer.enabled and request.Document.StartsWith(FUsageServer.path) then
          begin
            response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
            // response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE');
            response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
            // response.CustomHeaders.add('Access-Control-Expose-Headers: *');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
            FUsageServer.HandleRequest(AContext, request, response);
            epn := '--';
            summ := 'options?';
          end
          else
          begin
            ok := false;
            for ep in FEndPoints do
              if request.Document.StartsWith(ep.PathWithSlash) then
              begin
                ok := true;
                epn := ep.logId;
                summ := ep.PlainRequest(AContext, ip, request, response, id, tt);
                break;
              end else if (request.Document = ep.PathNoSlash) then
              begin
                ok := true;
                epn := ep.logId;
                response.Redirect(request.Document+'/');
                summ := '--> redirect to '+request.Document+'/';
                break;
              end;

            if not ok then
            begin
              if request.Document = '/diagnostics' then
              begin
                epn := 'WS';
                summ := 'diagnostics';
                summ := ReturnDiagnostics(AContext, request, response, false, false)
              end
              else if request.Document.startsWith('/diagnostics/db/') then
              begin
                epn := 'WS';
                summ := 'diagnostics';
                summ := ReturnDBDiagnostics(AContext, request, response, false, false)
              end
              else if request.Document = '/statistics' then
              begin
                epn := 'WS';
                summ := ReturnStatistics(AContext, request, response, false, false, true)
              end
              else if request.Document = '/stats' then
              begin
                epn := 'WS';
                summ := ReturnStatistics(AContext, request, response, false, false, false)
              end
              else if Common.SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
              begin
                summ := 'Static File '+request.Document;
                epn := 'WS';
                ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
              end
              else if request.Document = '/' then
              begin
                epn := 'WS';
                summ := 'processed File '+request.Document;
                ReturnProcessedFile(self, request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), false);
              end
              else
              begin
                response.ResponseNo := 404;
                response.ContentText := 'Document ' + request.Document + ' not found';
                summ := 'Not Found: '+request.Document;
                epn := 'XX';
              end;
            end;
          end;
          if (summ.contains('err:') or (summ.contains('too long')) and (not summ.contains('msg:') or UnderDebugger)) then
            logCrash(false, id, ip, request, response);

          logResponse(id, response);
          logOutput(AContext, request, response, id, tt, false, epn, summ);
          response.CloseConnection := not PLAIN_KEEP_ALIVE;
        finally
          InterlockedDecrement(GCounterWebRequests);
          MarkExit(AContext);
          SetThreadStatus('Done');
        end;
      finally
        tt.free;
      end;
    end;
  finally
    FLock.lock('PlainRequest2');
    try
      FLiveConnections.remove(ci);
    finally
      FLock.Unlock;
    end;
    ci.free;
  end;
end;

procedure TFhirWebServer.ProcessFile(sender: TObject; session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>; var result: String);
var
  s, n, h, t: String;
  i : integer;
begin
  s := SourceProvider.getSource(named);
  i := s.IndexOf('[%');
  while (i > -1) do
  begin
    h := s.subString(0, i);
    s := s.subString(i);
    i := s.indexOf('%]');
    t := s.subString(i+2);
    n := s.Substring(2, i-2);
    s := h + insertValue(n, secure, variables) + t;
    i := s.IndexOf('[%');
  end;
  result := s;
end;

procedure TFhirWebServer.registerEndPoint(endPoint: TFHIRServerEndPoint);
var
  wep : TFhirWebServerEndpoint;
begin
  wep := endPoint.makeWebEndPoint(Common.link);
  FEndPoints.add(wep);
  wep.OnReturnFile := ReturnProcessedFile;
  wep.OnReturnFileSource := ReturnFileSource;
  wep.OnProcessFile := ProcessFile;
  if (endPoint is TStorageEndPoint) then
    (endPoint as TStorageEndPoint).ServerContext.TerminologyServer.OnGetCurrentRequestCount := GetCurrentRequestCount;
  FStats.EndPointNames.add(endPoint.WebEndPoint.code);
end;

procedure TFhirWebServer.SecureRequest(AContext: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  cert: TIdOpenSSLX509;
  id, summ : String;
  tt : TTimeTracker;
  ok : boolean;
  ep: TFhirWebServerEndpoint;
  epn, ip: String;
  ci : TFHIRHTTPConnectionInfo;
begin
  ci := TFHIRHTTPConnectionInfo.create(request, AContext);
  try
    FLock.lock('SecureRequest');
    try
      FLiveConnections.add(ci.link);
    finally
      FLock.Unlock;
    end;
    if NoUserAuthentication then // we treat this as if it's a plain request
      PlainRequest(AContext, request, response)
    else
    begin
      ip := getClientIP(AContext, request);
      ci.FClientIP := ip;
      tt := TTimeTracker.Create;
      try
        InterlockedIncrement(GCounterWebRequests);
        cert := nil; // (AContext.Connection.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLSocket.PeerCert;
        epn := '??sreq';

        SetThreadStatus('Processing '+request.Document);
        MarkEntry(AContext, request, response);
        try
          id := FSettings.nextRequestId;
          logRequest(true, id, ip, request);
          response.CustomHeaders.Add('X-Request-Id: '+id);
          if isLogging then
            response.CustomHeaders.Add('X-GDPR-Disclosure: All access to this server is logged internally for debugging purposes; your continued use of the API constitutes agreement to this use');

          if (request.CommandType = hcOption) then
          begin
            response.ResponseNo := 200;
            response.ContentText := 'ok';
            response.CustomHeaders.Add('Access-Control-Allow-Credentials: true');
            response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
            response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
            response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');
            if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
              response.CustomHeaders.Add('Access-Control-Allow-Headers: ' + request.RawHeaders.Values['Access-Control-Request-Headers']);
            summ := 'options?';
            epn := '--';
          end
          else if (request.Document = 'robots.txt') and (RobotsText <> '') then
          begin
            response.ResponseNo := 200;
            response.ResponseText := 'OK';
            response.ContentText := RobotsText;
          end
          else
          begin
            ok := false;
            for ep in FEndPoints do
              if request.Document.StartsWith(ep.PathWithSlash) then
              begin
                ok := true;
                epn := ep.logId;
                summ := ep.SecureRequest(AContext, ip, request, response, cert, id, tt);
              end
              else if request.Document = ep.PathNoSlash then
              begin
                ok := true;
                epn := ep.logid;
                response.Redirect(ep.PathWithSlash);
                summ := '--> redirect to '+request.Document+'/';
              end;
            if not ok then
            begin
              if request.Document = '/diagnostics' then
              begin
                summ := ReturnDiagnostics(AContext, request, response, false, false);
                epn := 'WS';
              end
              else if SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
              begin
                summ := 'Static File '+request.Document;
                epn := 'WS';
                ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
              end
              else if request.Document = '/' then
              begin
                summ := 'Processed File '+request.Document;
                epn := 'WS';
                ReturnProcessedFile(self, request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), true)
              end
              else
              begin
                epn := 'XX';
                response.ResponseNo := 404;
                response.ContentText := 'Document ' + request.Document + ' not found';
                summ := 'Not Found: '+request.Document;
              end;
            end;
          end;

          logResponse(id, response);
          logOutput(AContext, request, response, id, tt, true, epn, summ);

          if (summ.contains('err:') and not summ.contains('msg:') ) then
            logCrash(false, id, ip, request, response);

          response.CloseConnection := not SECURE_KEEP_ALIVE;
        finally
          InterlockedDecrement(GCounterWebRequests);
          MarkExit(AContext);
          SetThreadStatus('Done');
        end;
      finally
        tt.free;
      end;
    end;
  finally
    FLock.lock('SecureRequest2');
    try
      FLiveConnections.remove(ci);
    finally
      FLock.Unlock;
    end;
    ci.free;
  end;
end;

procedure TFhirWebServer.SetCacheStatus(status: boolean);
begin
  Common.cache.Caching := status;
end;

procedure TFhirWebServer.SSLPassword(Sender: TObject; var Password: string; const IsWrite: Boolean);
begin
  Password := FSSLPassword;
end;

function sNow: String;
begin
  result := FormatDateTime('c', Now);
end;

function isText(ct : String) : boolean;
begin
  result := ct.Contains('text/') or
    ct.Contains('application/fhir') or
    ct.Contains('xml') or
    ct.Contains('json') or
    ct.Contains('turtle');
end;

function TFhirWebServer.isLogging: boolean;
begin
  result := (FInLog <> nil) or (FLogFolder <> '');
end;

procedure TFhirWebServer.logRequest(secure : boolean; id, clientIP: String; request: TIdHTTPRequestInfo);
var
  package : TFslBytesBuilder;
  b : TBytes;
begin
  if (FInLog = nil) and (FLogFolder = '') then
    exit;

  package := TFslBytesBuilder.Create;
  try
    package.addStringUtf8('-----------------------------------------------------------------'#13#10);
    package.addStringUtf8(id);
    package.addStringUtf8(' @ ');
    package.addStringUtf8(TFslDateTime.makeUTC.toXML);
    package.addStringUtf8(' from ');
    package.addStringUtf8(clientIP);
    if secure then
      package.addStringUtf8(' (https)');
    package.addStringUtf8(#13#10);
    package.addStringUtf8(request.RawHTTPCommand);
    package.addStringUtf8(#13#10);
    package.addStringUtf8(request.RawHeaders.Text);
    if request.PostStream <> nil then
    begin
      package.addStringUtf8(#13#10);
      SetLength(b, request.PostStream.Size);
      request.PostStream.Read(b[0], length(b));
      request.PostStream.Position := 0;
      if isText(request.ContentType) and (request.ContentEncoding = '') then
        package.Append(b)
      else
        package.addBase64(b);
      package.addStringUtf8(#13#10);
    end
    else if request.ContentType = 'application/x-www-form-urlencoded' then
    begin
      package.addStringUtf8(#13#10);
      package.addStringUtf8(request.UnparsedParams);
      package.addStringUtf8(#13#10);
    end;

    if FInLog <> nil then
      FInLog.WriteToLog(package.AsBytes);
    if FLogFolder <> '' then
      BytesToFile(package.AsBytes, FilePath([FLogFolder, id+'.log']));
  finally
    package.free;
  end;
end;

procedure TFhirWebServer.logResponse(id: String; resp: TIdHTTPResponseInfo);
  procedure log(msg : String);
  var
    package : TFslBytesBuilder;
    b : TBytes;
  begin
    package := TFslBytesBuilder.Create;
    try
      package.addStringUtf8('-----------------------------------------------------------------'#13#10);
      package.addStringUtf8(id);
      package.addStringUtf8(' @ ');
      package.addStringUtf8(TFslDateTime.makeUTC.toXML);
      if (msg <> '') then
        package.addStringUtf8(' '+msg);
      package.addStringUtf8(#13#10);
      package.addStringUtf8(inttostr(resp.ResponseNo)+' '+resp.ResponseText);
      package.addStringUtf8(#13#10);
      package.addStringUtf8(resp.RawHeaders.Text);
      if resp.ContentStream <> nil then
      begin
        package.addStringUtf8(#13#10);
        SetLength(b, resp.ContentStream.Size);
        if (length(b) > 0) then
          resp.ContentStream.Read(b[0], length(b));
        resp.ContentStream.Position := 0;
        if isText(resp.ContentType) and (resp.ContentEncoding = '') then
          package.Append(b)
        else
          package.addBase64(b);
        package.addStringUtf8(#13#10);
      end
      else if resp.ContentText <> '' then
      begin
        package.addStringUtf8(#13#10);
        package.addStringUtf8(resp.ContentText);
        package.addStringUtf8(#13#10);
      end;

      b := package.AsBytes;
      if FOutLog <> nil then
        FOutLog.WriteToLog(b);
      if FLogFolder <> '' then
      begin
        package.Clear;
        package.Append(FileToBytes(FilePath([FLogFolder, id+'.log'])));
        package.append(b);
        BytesToFile(package.AsBytes, FilePath([FLogFolder, id+'.log']));
      end;
    finally
      package.free;
    end;
  end;
begin
  if (FOutLog = nil) and (FLogFolder = '') then
    exit;

  try
    resp.WriteHeader;
    log('');
  except
    on e : exception do
    begin
      log(e.Message);
      raise;
    end;
  end;
end;


procedure TFhirWebServer.logCrash(secure : boolean; id, clientIP: String; request: TIdHTTPRequestInfo; resp: TIdHTTPResponseInfo);
var
  package : TFslBytesBuilder;
  b : TBytes;
  folder : String;
begin
  try
    folder := FLogFolder;
    if folder = '' then
      folder := FilePath(['[tmp]', 'fhir-server-crash']);
    if not FolderExists(folder) then
      ForceFolder(folder);
    Logging.log('Save crash request to '+FilePath([folder, 'crash-'+id+'.log']));

    package := TFslBytesBuilder.Create;
    try
      package.addStringUtf8('-- Request ');
      package.addStringUtf8(id);
      package.addStringUtf8(' @ ');
      package.addStringUtf8(TFslDateTime.makeUTC.toXML);
      package.addStringUtf8(' from ');
      package.addStringUtf8(clientIP);
      if secure then
        package.addStringUtf8(' (https)');
      package.addStringUtf8('------------------------------------------'#13#10);
      package.addStringUtf8(request.RawHTTPCommand);
      package.addStringUtf8(#13#10);
      package.addStringUtf8(request.RawHeaders.Text);
      if request.PostStream <> nil then
      begin
        package.addStringUtf8(#13#10);
        request.PostStream.Position := 0; // it's almost certainly been read;
        SetLength(b, request.PostStream.Size);
        request.PostStream.Read(b[0], length(b));
        request.PostStream.Position := 0;
        if isText(request.ContentType) and (request.ContentEncoding = '') then
          package.Append(b)
        else
          package.addBase64(b);
        package.addStringUtf8(#13#10);
      end
      else if request.ContentType = 'application/x-www-form-urlencoded' then
      begin
        package.addStringUtf8(#13#10);
        package.addStringUtf8(request.UnparsedParams);
        package.addStringUtf8(#13#10);
      end;

      package.addStringUtf8('-- Response --------------------------------------------------'#13#10);
      package.addStringUtf8(inttostr(resp.ResponseNo)+' '+resp.ResponseText);
      package.addStringUtf8(#13#10);
      package.addStringUtf8(resp.RawHeaders.Text);
      if resp.ContentStream <> nil then
      begin
        package.addStringUtf8(#13#10);
        SetLength(b, resp.ContentStream.Size);
        if (length(b) > 0) then
          resp.ContentStream.Read(b[0], length(b));
        resp.ContentStream.Position := 0;
        if isText(resp.ContentType) and (resp.ContentEncoding = '') then
          package.Append(b)
        else
          package.addBase64(b);
        package.addStringUtf8(#13#10);
      end
      else if resp.ContentText <> '' then
      begin
        package.addStringUtf8(#13#10);
        package.addStringUtf8(resp.ContentText);
        package.addStringUtf8(#13#10);
      end;

      BytesToFile(package.AsBytes, FilePath([folder, 'crash-'+id+'.log']));
    finally
      package.free;
    end;
  except
    on e : exception do
    begin
      Logging.log('Error writing crash log: '+e.Message);
      raise;
    end;
  end;
end;


procedure TFhirWebServer.MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  Common.Lock.Lock('MarkEntry');
  try
    ci.Activity := request.Command + ' ' + request.Document + '?' + request.UnparsedParams;
    ci.Count := ci.Count + 1;
    Common.Stats.totalStart;
    ci.Start := GetTickCount64;
  finally
    Common.Lock.Unlock;
  end;
end;

procedure TFhirWebServer.MarkExit(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  Common.Lock.Lock('MarkExit');
  try
    ci.Activity := '';
    Common.Stats.totalFinish(GetTickCount64 - ci.Start);
    ci.Start := 0;
  finally
    Common.Lock.Unlock;
  end;
end;


function TFhirWebServer.ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
var
  vars : TFslMap<TFHIRObject>;
  ep : TFhirWebServerEndpoint;
  sp : TStorageWebEndpoint;
begin
  vars := TFslMap<TFHIRObject>.Create('dx.vars');
  try
    vars.Add('status.db', TFHIRSystemString.Create(FormatTextToHTML(DBManagers.dump)));
    vars.Add('live.connections', TFHIRSystemString.Create(inttostr(GCounterWebConnections)));
    vars.Add('live.requests', TFHIRSystemString.Create(inttostr(GCounterWebRequests)));
    vars.Add('live.requests.kernel', TFHIRSystemString.Create(inttostr(GCounterFHIRRequests)));
    vars.Add('status.locks', TFHIRSystemString.Create(FormatTextToHTML(DumpLocks(true))));
    for ep in FEndPoints do
    begin
      if ep is TStorageWebEndpoint then
      begin
        sp := ep as TStorageWebEndpoint;
        vars.Add('status.'+ep.Code+'.sessions', TFHIRSystemString.Create(sp.ServerContext.SessionManager.DumpSessions));
        vars.Add('status.'+ep.Code+'.tx', TFHIRSystemString.Create(sp.ServerContext.TerminologyServer.Summary));
      end;
    end;
    vars.Add('status.web', TFHIRSystemString.Create(WebDump));
    vars.Add('status.web-total-count', TFHIRSystemString.Create(inttostr(Common.Stats.TotalCount)));
    vars.Add('status.web-rest-count', TFHIRSystemString.Create(inttostr(Common.Stats.RestCount)));
    vars.Add('status.web-total-time', TFHIRSystemString.Create(inttostr(Common.Stats.TotalTime)));
    vars.Add('status.web-rest-time', TFHIRSystemString.Create(inttostr(Common.Stats.RestTime)));
    vars.Add('status.run-time', TFHIRSystemString.Create(DescribePeriod((GetTickCount64 - Common.Stats.StartTime) * DATETIME_MILLISECOND_ONE)));
    vars.Add('status.run-time.ms', TFHIRSystemString.Create(inttostr(GetTickCount64 - Common.Stats.StartTime)));
    ReturnProcessedFile(self, request, response, 'Diagnostics', SourceProvider.AltFile('/diagnostics.html', ''), false, vars);
  finally
    vars.free;
  end;
  result := 'Diagnostics';
end;

function TFhirWebServer.ReturnDBDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
var
  id : String;
  db : TFDBManager;
  vars : TFslMap<TFHIRObject>;
  ep : TFhirWebServerEndpoint;
  sp : TStorageWebEndpoint;
begin
  id := request.Document.subString(16);
  db := DBManagers.ConnManByName[id];
  vars := TFslMap<TFHIRObject>.Create('dx.vars');
  try
    if (db = nil) then
      vars.Add('status.db', TFHIRSystemString.Create(FormatTextToHTML('Database '+id+' not known')))
    else
      vars.Add('status.db', TFHIRSystemString.Create(db.logger.Report(krfHTML)));
    ReturnProcessedFile(self, request, response, 'Diagnostics', SourceProvider.AltFile('/diagnostics-db.html', ''), false, vars);
  finally
    vars.free;
  end;
  result := 'Diagnostics';
end;

function TFhirWebServer.ReturnStatistics(AContext: TIdContext;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure,
  asHtml: boolean): String;
begin
  response.Expires := Now + 1;
  if (asHtml) then
  begin
    response.ContentStream := TStringStream.Create(FStats.asHtml);
    response.contentType := 'text/html';
  end
  else
  begin
    response.ContentStream := TStringStream.Create(FStats.asCSV);
    response.contentType := 'text/csv';
  end;
  response.FreeContentStream := true;
  result := 'Statistics'
end;

procedure TFhirWebServer.ReturnFileSource(sender: TObject; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String);
var
  b : TBytes;
begin
  if FileExists(path) then
    b := FileToBytes(path)
  else
    b := SourceProvider.asBytes(path);
  response.Expires := Now + 1;
  response.ContentStream := TBytesStream.Create(b);
  response.FreeContentStream := true;
  response.contentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

function TFhirWebServer.endpointList : String;
var
  b : TFslStringBuilder;
  ep : TFhirWebServerEndpoint;
begin
  b := TFslStringBuilder.Create;
  try
    b.append('<ul>');
    if FEndPoints.Count = 0 then
      b.Append('<li><i>(No configured end-points)</i></li>')
    else
      for ep in FEndPoints do
        b.Append('<li><a href="'+ep.PathWithSlash+'">'+ep.PathNoSlash+'</a>: '+ep.description+'</li>');
    b.append('</ul>');
    result := b.toString;
  finally
    b.free;
  end;
end;

procedure TFhirWebServer.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
  ci.Add('Common.cache', Common.cache.sizeInBytes(ci.magic));
end;

function TFhirWebServer.getClientId(AContext: TIdContext; request: TIdHTTPRequestInfo): String;
begin
  result := request.rawHeaders.Values['X-Real-IP'];
  if result = '' then
    result := AContext.Binding.PeerIP;
  if request.UserAgent <> '' then
    if request.UserAgent.StartsWith('fhir/') then
      result := request.UserAgent.Substring(5);
end;

function TFhirWebServer.getClientIP(AContext: TIdContext; request: TIdHTTPRequestInfo): String;
begin
  result := request.rawHeaders.Values['X-Real-IP'];
  if result = '' then
    result := AContext.Binding.PeerIP;
end;

procedure TFhirWebServer.ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>);
begin
  ReturnProcessedFile(sender, request, response, named, path, secure, variables);
end;

function  TFhirWebServer.insertValue(n : String; secure: boolean; variables: TFslMap<TFHIRObject>) : String;
begin
  if n.startsWith('include ') then
    result := SourceProvider.getSource(n.subString(8))
  else if n = 'id' then
    result := Common.Name
  else if n = 'specurl' then
    result := 'http://hl7.org/fhir'
  else if n = 'web' then
    result := WebDesc(secure)
  else if n = 'admin' then
    result := Common.AdminEmail
  else if n = 'server-ver' then
    result := FHIR_CODE_FULL_VERSION
  else if n = 'os' then
    result := SERVER_OS
  else if n = 'logout' then
    result := 'User: [n/a]'
  else if n = 'endpoints' then
    result := endpointList
  else if n = 'host' then
    if Common.StatedPort = 80 then
      result := Common.Host
    else
      result := Common.Host + ':' + inttostr(Common.StatedPort)
  else if n = 'securehost' then
    if Common.StatedSSLPort = 80 then
      result := Common.Host
    else
      result := Common.Host + ':' + inttostr(Common.StatedSSLPort)
  else if (variables <> nil) and variables.ContainsKey(n) then
    result := variables[n].primitiveValue
  else if n = 'ver' then
    result := 'n/a'
  else
    result := '??'+n+'??';
end;

procedure TFhirWebServer.ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
var
  s, n, h, t: String;
  i : integer;
begin
  s := SourceProvider.getSource(actual);
  i := s.IndexOf('[%');
  while (i > -1) do
  begin
    h := s.subString(0, i);
    s := s.subString(i);
    i := s.indexOf('%]');
    t := s.subString(i+2);
    n := s.Substring(2, i-2);
    s := h + insertValue(n, secure, variables) + t;
    i := s.IndexOf('[%');
  end;

  response.Expires := Now + 1;
  response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
  response.FreeContentStream := true;
  response.contentType := 'text/html; charset=UTF-8';
end;

procedure TFhirWebServer.ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure: boolean);
begin
  response.Expires := Now + 1;
  response.ContentStream := SourceProvider.asStream(path);
  response.FreeContentStream := true;
  response.contentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

{ TFHIRWebServerExtension }

constructor TFHIRWebServerExtension.Create(context: TFHIRServerContext);
begin
  inherited Create;
  FContext := context;
end;

destructor TFHIRWebServerExtension.Destroy;
begin
  FContext.free;
  inherited;
end;

function TFHIRWebServerExtension.resolveConstant(context: TFHIRPathExecutionContext; s: String; var obj: TFHIRObject): boolean;
begin
  if (s = '%server') then
  begin
    result := true;
    obj := TFHIRPathServerObject.Create(FContext.link)
  end
  else
    result := false;
end;

{ TFHIRPathServerObject }

constructor TFHIRPathServerObject.Create(context: TFHIRServerContext);
begin
  inherited Create;
  FContext := context;
end;

destructor TFHIRPathServerObject.Destroy;
begin
  FContext.free;
  inherited;
end;

function TFHIRPathServerObject.fhirType: String;
begin
  result := 'FHIRServer';
end;

procedure TFHIRPathServerObject.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
end;

function TFHIRPathServerObject.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRPathServerObject.getId: String;
begin
  result := FContext.DatabaseId;

end;

function TFHIRPathServerObject.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRPathServerObject.hasPrimitiveValue: boolean;
begin
  result := false;
end;

function TFHIRPathServerObject.isPrimitive: boolean;
begin
  result := false;
end;

procedure TFHIRPathServerObject.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
end;

function TFHIRPathServerObject.makeCodeValue(v: String): TFHIRObject;
begin
  result := FContext.Factory.makeCode(v);
end;

function TFHIRPathServerObject.makeIntValue(v: String): TFHIRObject;
begin
  result := FContext.Factory.makeInteger(v);
end;

function TFHIRPathServerObject.makeStringValue(v: String): TFHIRObject;
begin
  result := FContext.Factory.makeString(v);
end;

function TFHIRPathServerObject.primitiveValue: string;
begin
  result := '';
end;

procedure TFHIRPathServerObject.setIdValue(id: String);
begin
  // nothing
end;

function TFHIRPathServerObject.ToString: String;
begin
  result := '(Server '+FContext.DatabaseId+'';
end;


{ TFHIRHTTPServer }

procedure TFHIRHTTPServer.DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler);
var
  conn : TFHIRHTTPConnectionInfo;
begin
  logging.log('Max Web Connections Exceeded ('+inttostr(MaxConnections)+')');
  FServer.FLock.lock('DoMaxConnectionsExceeded');
  try
    for conn in FServer.FLiveConnections do
      logging.log(conn.log);
  finally
    FServer.FLock.Unlock;
  end;
end;

End.


