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
  IdOpenSSLIOHandlerServer, IdOpenSSLIOHandlerClient, IdOpenSSLVersion, IdOpenSSLX509,

  fsl_base, fsl_utilities, fsl_crypto, fsl_logging, fsl_stream, fsl_collections, fsl_threads, fsl_json, fsl_xml,
  {$IFDEF WINDOWS} fsl_msxml, fsl_service_win, {$ENDIF}
  fsl_openssl, fsl_http, fdb_manager, fsl_htmlgen, fdb_dialects, fsl_rdf, fsl_graphql, fsl_twilio,

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
  tags, session, storage, security, html_builder, ftx_sct_services, ftx_sct_publisher, server_config,
  scim_server,
  auth_manager, reverse_client, cds_hooks_server, web_source, analytics, bundlebuilder, server_factory,
  user_manager, server_context, server_constants, utilities, jwt, usage_stats,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  subscriptions, twilio, telnet_server,
  web_base, endpoint, endpoint_storage;

Type
  TFhirWebServer = class;

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

    // operational fields
    FUsageServer : TUsageStatsServer;
    FPlainServer: TIdHTTPServer;
    FSSLServer: TIdHTTPServer;
    FIOHandler: TIdOpenSSLIOHandlerServer {TIdServerIOHandlerSSLOpenSSL};
    FClients: TFslList<TFHIRWebServerClientInfo>;
    FEndPoints : TFslList<TFhirWebServerEndpoint>;

    procedure logRequest(secure : boolean; id : String; request : TIdHTTPRequestInfo);
    procedure logResponse(id : String; resp : TIdHTTPResponseInfo);
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

    procedure ProcessFile(sender : TObject; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>; var result : String);
    procedure ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>); overload;
    Procedure ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);
    function  ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;

    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    Procedure StartServer();
    Procedure StopServer;
  Public
    constructor Create(settings : TFHIRServerSettings; name: String);
    destructor Destroy; Override;
    procedure loadConfiguration(ini : TFHIRServerConfigFile);
    property settings : TFHIRServerSettings read FSettings;

    procedure DoVerifyPeer(Sender: TObject; const x509: TIdOpenSSLX509; const VerifyResult: Integer; const Depth: Integer; var Accepted: Boolean); // private (hint busting)

    Procedure Start; // (active, threads: boolean);
    Procedure Stop;

    property EndPoints : TFslList<TFhirWebServerEndpoint> read FEndPoints;
    function EndPoint(name : String) : TFhirWebServerEndpoint;

    procedure registerEndPoint(endPoint : TFHIRServerEndPoint);
  End;


Implementation

Uses
{$IFDEF WINDOWS}
  Registry,
{$ENDIF}
  fsl_oauth{$IFDEF COVID}, FHIR.Server.Covid{$ENDIF};


{ TFhirWebServer }

function TFhirWebServer.AbsoluteURL(secure: boolean): String;
begin
  if secure then
    result := 'https://'+common.host+SSLPort+'/'
  else
    result := 'http://'+common.host+HTTPPort+'/'
end;

Constructor TFhirWebServer.Create(settings : TFHIRServerSettings; name: String);
Begin
  Inherited Create(nil);
  FEndPoints := TFslList<TFhirWebServerEndpoint>.create;
  self.Common.Name := Name;
  FInLog := nil;

  FSettings := settings;
  FClients := TFslList<TFHIRWebServerClientInfo>.Create;
End;

Destructor TFhirWebServer.Destroy;
Begin
  FUsageServer.Free;
  FEndPoints.Free;
  FSettings.Free;
  FClients.Free;
  FInLog.Free;
  FOutLog.Free;
  Inherited;
End;


procedure TFhirWebServer.loadConfiguration(ini : TFHIRServerConfigFile);
var
  fn: String;
  txu: String;
begin
  fn := ini.admin['logging-in'].value;
  if (fn <> '') and ((fn <> '-')) then
  begin
    if (FolderExists('c:\temp')) then
      FInLog := TLogger.Create('c:\temp\'+fn)
    else
      FInLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+fn);
    FInLog.Policy.FullPolicy := lfpChop;
    FInLog.Policy.MaximumSize := 100*1024*1024;
    FInLog.Policy.AllowExceptions := false;
  end;

  fn := ini.admin['logging-out'].value;
  if (fn <> '') and ((fn <> '-')) then
  begin
    if (FolderExists('c:\temp')) then
      FOutLog := TLogger.Create('c:\temp\'+fn)
    else
      FOutLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+fn);
    FOutLog.Policy.FullPolicy := lfpChop;
    FOutLog.Policy.MaximumSize := 300*1024*1024;
    FOutLog.Policy.AllowExceptions := false;
  end;

  // web identity / configuration
  FHomePage := 'homepage.html';
  FFacebookLike := ini.identityProviders.section['facebook.com']['like'].value = 'true';
  Common.Host := ini.web['host'].value;

  // web server configuration
  Common.ActualPort := ini.web['http'].readAsInt;
  Common.ActualSSLPort := ini.web['https'].readAsInt;
  FCertFile := ini.web['certname'].value;
  FRootCertFile := ini.web['cacertname'].value;
  FSSLPassword := ini.web['password'].value;

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
    raise EFHIRException.create('An admin email is required');

  if Common.ActualPort = 80 then
    txu := 'http://' + Common.Host
  else
    txu := 'http://' + Common.Host + ':' + inttostr(Common.ActualPort);

  Common.Google.serverId := ini.web['googleid'].value;
end;

procedure TFhirWebServer.DoConnect(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
  SetThreadStatus('Connecting');
  if (AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase) then
  begin
    SetThreadName('https:'+AContext.Binding.PeerIP);
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;
  end
  else
    SetThreadName('http:'+AContext.Binding.PeerIP);

  AContext.Connection.IOHandler.ReadTimeout := 60*1000;

  InterlockedIncrement(GCounterWebConnections);
{$IFDEF WINDOWS}
  CoInitialize(nil);
{$ENDIF}
{$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  Common.OnRegisterJs(self, GJsHost);
{$ENDIF}
//  GJsHost.registry := ServerContext.EventScriptRegistry.Link;
  AContext.Connection.IOHandler.MaxLineLength := 100 * 1024;
  Common.Lock.Lock;
  try
    ci := TFHIRWebServerClientInfo.Create;
    FClients.Add(ci);
    AContext.Data := ci;
    ci.Context := AContext;
  finally
    Common.Lock.Unlock;
  end;
  SetThreadStatus('Connected');
end;

procedure TFhirWebServer.DoDisconnect(AContext: TIdContext);
begin
  SetThreadStatus('Disconnecting');
  InterlockedDecrement(GCounterWebConnections);
  Common.Lock.Lock;
  try
    FClients.Remove(TFHIRWebServerClientInfo(AContext.Data));
    AContext.Data := nil;
  finally
    Common.Lock.Unlock;
  end;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJshost := nil;
  {$ENDIF}
{$IFDEF WINDOWS}
  CoUninitialize;
{$ENDIF}
  SetThreadStatus('Disconnected');
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
  if (Common.ActualPort = 0) then
    result := 'Port ' + inttostr(Common.ActualSSLPort) + ' (https).'
  else if Common.ActualSSLPort = 0 then
    result := 'Port ' + inttostr(Common.ActualPort) + ' (http).'
  else if secure then
    result := '<a href="'+absoluteUrl(false)+'">Port ' + inttostr(Common.ActualPort) + ' (http)</a> and Port ' + inttostr(Common.ActualSSLPort) + ' (https - this server).'
  else
    result := 'Port ' + inttostr(Common.ActualPort) + ' (http - this server) and <a href="'+absoluteUrl(true)+'">Port ' + inttostr(Common.ActualSSLPort) + ' (https)</a>.'
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

Procedure TFhirWebServer.Start; // (active, threads: boolean);
Begin

  Logging.log('Start Web Server:');
  if (Common.ActualPort = 0) then
    Logging.log('  http: not active')
  else
    Logging.log('  http: listen on ' + inttostr(Common.ActualPort));

  if (Common.ActualSSLPort = 0) then
    Logging.log('  https: not active')
  else
    Logging.log('  https: listen on ' + inttostr(Common.ActualSSLPort));
  FActive := true;
  Common.Stats.Start;
  StartServer;

End;

Procedure TFhirWebServer.Stop;
Begin
  FActive := false;
  StopServer;
End;

Procedure TFhirWebServer.StartServer();
Begin
  if Common.ActualPort > 0 then
  begin
    FPlainServer := TIdHTTPServer.Create(Nil);
    FPlainServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
    TIdSchedulerOfThreadPool(FPlainServer.Scheduler).PoolSize := 20;
    FPlainServer.ServerSoftware := 'Health Intersections FHIR Server';
    FPlainServer.ParseParams := false;
    FPlainServer.DefaultPort := Common.ActualPort;
    FPlainServer.KeepAlive := PLAIN_KEEP_ALIVE;
    FPlainServer.OnCreatePostStream := CreatePostStream;
    FPlainServer.OnCommandGet := PlainRequest;
    FPlainServer.OnCommandOther := PlainRequest;
    FPlainServer.OnConnect := DoConnect;
    FPlainServer.OnDisconnect := DoDisconnect;
    FPlainServer.OnParseAuthentication := ParseAuthenticationHeader;
    FPlainServer.active := true;
  end;
  if Common.ActualSSLPort > 0 then
  begin
    If Not FileExists(FCertFile) Then
      raise EIOException.create('SSL Certificate "' + FCertFile + ' could not be found');
    If Not FileExists(ChangeFileExt(FCertFile, '.key')) Then
      raise EIOException.create('SSL Certificate Private Key "' + ChangeFileExt(FCertFile, '.key') + ' could not be found');
    If (FRootCertFile <> '') and (Not FileExists(FRootCertFile)) Then
      raise EIOException.create('SSL Certificate "' + FRootCertFile + ' could not be found');
    FSSLServer := TIdHTTPServer.Create(Nil);
    FSSLServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
    TIdSchedulerOfThreadPool(FSSLServer.Scheduler).PoolSize := 20;
    FSSLServer.ServerSoftware := 'Health Intersections FHIR Server';
    FSSLServer.ParseParams := false;
    FSSLServer.DefaultPort := Common.ActualSSLPort;
    FSSLServer.KeepAlive := SECURE_KEEP_ALIVE;
    FSSLServer.OnCreatePostStream := CreatePostStream;
    FIOHandler := TIdOpenSSLIOHandlerServer.Create(Nil);
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
    FSSLServer.active := true;
  end;
end;

Procedure TFhirWebServer.StopServer;
Begin
  if FSSLServer <> nil then
  begin
    FSSLServer.active := false;
    FSSLServer.Scheduler.Free;
    FreeAndNil(FSSLServer);
    FreeAndNil(FIOHandler);
  end;
  if FPlainServer <> nil then
  begin
    FPlainServer.active := false;
    FPlainServer.Scheduler.Free;
    FreeAndNil(FPlainServer);
  end;
End;

function TFhirWebServer.WebDump: String;
var
  b: TStringBuilder;
  ci: TFHIRWebServerClientInfo;
begin
  b := TStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.Append('<tr><td>IP address</td><td>Count</td><td>Session</td><td>Activity</td><td>Length</td></tr>'#13#10);
    Common.Lock.Lock;
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
          b.Append(inttostr(GetTickCount - ci.Start));
        b.Append('</td></tr>'#13#10);
      end;
    finally
      Common.Lock.Unlock;
    end;
    b.Append('</table>'#13#10);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

Procedure TFhirWebServer.CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
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

Procedure TFhirWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  id, summ : string;
  t: cardinal;
  ep : TFhirWebServerEndpoint;
  ok : boolean;
begin
  InterlockedIncrement(GCounterWebRequests);
  t := GetTickCount;
  SetThreadStatus('Processing '+request.Document);
  MarkEntry(AContext, request, response);
  try
    id := FSettings.nextRequestId;
    logRequest(false, id, request);
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
      FUsageServer.HandleRequest(AContext, request, response)
    end
    else
    begin
      ok := false;
      for ep in FEndPoints do
        if request.Document.StartsWith(ep.PathWithSlash) then
        begin
          ok := true;
          summ := ep.PlainRequest(AContext, request, response, id);
          break;
        end else if (request.Document = ep.PathNoSlash) then
        begin
          ok := true;
          response.Redirect(request.Document+'/');
          summ := '--> redirect to '+request.Document+'/';
          break;
        end;

      if not ok then
      begin
        if request.Document = '/diagnostics' then
          summ := ReturnDiagnostics(AContext, request, response, false, false)
        else if Common.SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
        begin
          summ := 'Static File';
          ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
        end
        else if request.Document = '/' then
        begin
          summ := 'processed File';
          ReturnProcessedFile(self, request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), false);
        end
        else
        begin
          response.ResponseNo := 404;
          response.ContentText := 'Document ' + request.Document + ' not found';
          summ := 'Not Found';
        end;
      end;
    end;
    logResponse(id, response);
    t := GetTickCount - t;
    Logging.log(id+' '+StringPadLeft(inttostr(t), ' ', 4)+'ms '+Logging.MemoryStatus+' #'+inttostr(GCounterWebRequests)+' '+AContext.Binding.PeerIP+' '+inttostr(response.ResponseNo)+' http: '+request.RawHTTPCommand+': '+summ);
    response.CloseConnection := not PLAIN_KEEP_ALIVE;
  finally
    InterlockedDecrement(GCounterWebRequests);
    MarkExit(AContext);
    SetThreadStatus('Done');
  end;
end;

procedure TFhirWebServer.ProcessFile(sender: TObject; session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>; var result: String);
var
  s, n: String;
begin
  s := SourceProvider.getSource(named);
  s := s.Replace('[%id%]', Common.Name, [rfReplaceAll]);
  s := s.Replace('[%specurl%]', 'http://hl7.org/fhir', [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc(secure), [rfReplaceAll]);
  s := s.Replace('[%admin%]', Common.AdminEmail, [rfReplaceAll]);
  s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll]);
  s := s.Replace('[%endpoints%]', endpointList, [rfReplaceAll]);
  if Common.ActualPort = 80 then
    s := s.Replace('[%host%]', Common.Host, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', Common.Host + ':' + inttostr(Common.ActualPort), [rfReplaceAll]);

  if Common.ActualSSLPort = 443 then
    s := s.Replace('[%securehost%]', Common.Host, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', Common.Host + ':' + inttostr(Common.ActualSSLPort), [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%' + n + '%]', variables[n].primitiveValue, [rfReplaceAll]);
  s := s.Replace('[%ver%]', 'n/a', [rfReplaceAll]);
  result := s;
end;

procedure TFhirWebServer.registerEndPoint(endPoint: TFHIRServerEndPoint);
var
  wep : TFhirWebServerEndpoint;
begin
  wep := endPoint.makeWebEndPoint(Common.link);
  FEndPoints.add(wep);
  wep.OnReturnFile := ReturnProcessedFile;
  wep.OnProcessFile := ProcessFile;
end;

Procedure TFhirWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  cert: TIdOpenSSLX509;
  id, summ : String;
  t: cardinal;
  ok : boolean;
  ep: TFhirWebServerEndpoint;
begin
  if NoUserAuthentication then // we treat this as if it's a plain request
    PlainRequest(AContext, request, response)
  else
  begin
    InterlockedIncrement(GCounterWebRequests);
    t := GetTickCount;
    cert := nil; // (AContext.Connection.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLSocket.PeerCert;

    SetThreadStatus('Processing '+request.Document);
    MarkEntry(AContext, request, response);
    try
      id := FSettings.nextRequestId;
      logRequest(true, id, request);
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
      end
      else
      begin
        ok := false;
        for ep in FEndPoints do
          if request.Document.StartsWith(ep.PathWithSlash) then
          begin
            ok := true;
            summ := ep.SecureRequest(AContext, request, response, cert, id);
          end
          else if request.Document = ep.PathNoSlash then
          begin
            ok := true;
            response.Redirect(ep.PathWithSlash);
          end;
        if not ok then
        begin
          if request.Document = '/diagnostics' then
            summ := ReturnDiagnostics(AContext, request, response, false, false)
          else if SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
          begin
            summ := 'Static File';
            ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
          end
          else if request.Document = '/' then
          begin
            summ := 'Processed File';
            ReturnProcessedFile(self, request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), true)
          end
          else
          begin
            response.ResponseNo := 404;
            response.ContentText := 'Document ' + request.Document + ' not found';
          end;
        end;
      end;

      logResponse(id, response);
      t := GetTickCount - t;
      Logging.log(id+' https: '+inttostr(t)+'ms '+request.RawHTTPCommand+' '+inttostr(t)+' for '+AContext.Binding.PeerIP+' => '+inttostr(response.ResponseNo)+'. mem= '+Logging.MemoryStatus);
      Logging.log(id+' '+StringPadLeft(inttostr(t), ' ', 4)+'ms '+Logging.MemoryStatus+' #'+inttostr(GCounterWebRequests)+' '+AContext.Binding.PeerIP+' '+inttostr(response.ResponseNo)+' https: '+request.RawHTTPCommand+': '+summ);
      response.CloseConnection := not SECURE_KEEP_ALIVE;
    finally
      InterlockedDecrement(GCounterWebRequests);
      MarkExit(AContext);
      SetThreadStatus('Done');
    end;
  end;
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

procedure TFhirWebServer.logRequest(secure : boolean; id: String; request: TIdHTTPRequestInfo);
var
  package : TFslBytesBuilder;
  b : TBytes;
begin
  if FInLog = nil then
    exit;

  package := TFslBytesBuilder.Create;
  try
    package.addStringUtf8('-----------------------------------------------------------------'#13#10);
    package.addStringUtf8(id);
    package.addStringUtf8(' @ ');
    package.addStringUtf8(TFslDateTime.makeUTC.toXML);
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

    FInLog.WriteToLog(package.AsBytes);
  finally
    package.Free;
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

      FOutLog.WriteToLog(package.AsBytes);
    finally
      package.Free;
    end;
  end;
begin
  if FOutLog = nil then
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

procedure TFhirWebServer.MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  Common.Lock.Lock;
  try
    ci.Activity := request.Command + ' ' + request.Document + '?' + request.UnparsedParams;
    ci.Count := ci.Count + 1;
    Common.Stats.totalStart;
    ci.Start := GetTickCount;
  finally
    Common.Lock.Unlock;
  end;
end;

procedure TFhirWebServer.MarkExit(AContext: TIdContext);
var
  ci: TFHIRWebServerClientInfo;
begin
  ci := TFHIRWebServerClientInfo(AContext.Data);

  Common.Lock.Lock;
  try
    ci.Activity := '';
    Common.Stats.totalFinish(GetTickCount - ci.Start);
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
    vars.Add('status.locks', TFHIRSystemString.Create(FormatTextToHTML(DumpLocks)));
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
    vars.Add('status.run-time', TFHIRSystemString.Create(DescribePeriod((GetTickCount - Common.Stats.StartTime) * DATETIME_MILLISECOND_ONE)));
    vars.Add('status.run-time.ms', TFHIRSystemString.Create(inttostr(GetTickCount - Common.Stats.StartTime)));
    ReturnProcessedFile(self, request, response, 'Diagnostics', SourceProvider.AltFile('/diagnostics.html', ''), false, vars);
  finally
    vars.Free;
  end;
  result := 'Diagnostics';
end;

function TFhirWebServer.endpointList : String;
var
  b : TStringBuilder;
  ep : TFhirWebServerEndpoint;
begin
  b := TStringBuilder.create;
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

procedure TFhirWebServer.ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>);
begin
  ReturnProcessedFile(sender, request, response, named, path, secure, variables);
end;

procedure TFhirWebServer.ReturnProcessedFile(sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
var
  s, n: String;
begin
  s := SourceProvider.getSource(actual);
  s := s.Replace('[%id%]', Common.Name, [rfReplaceAll]);
  s := s.Replace('[%specurl%]', 'http://hl7.org/fhir', [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc(secure), [rfReplaceAll]);
  s := s.Replace('[%admin%]', Common.AdminEmail, [rfReplaceAll]);
  s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll]);
  s := s.Replace('[%endpoints%]', endpointList, [rfReplaceAll]);
  if Common.ActualPort = 80 then
    s := s.Replace('[%host%]', Common.Host, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', Common.Host + ':' + inttostr(Common.ActualPort), [rfReplaceAll]);

  if Common.ActualSSLPort = 443 then
    s := s.Replace('[%securehost%]', Common.Host, [rfReplaceAll])
  else
    s := s.Replace('[%securehost%]', Common.Host + ':' + inttostr(Common.ActualSSLPort), [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%' + n + '%]', variables[n].primitiveValue, [rfReplaceAll]);
  s := s.Replace('[%ver%]', 'n/a', [rfReplaceAll]);

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
  FContext.Free;
  inherited;
end;

function TFHIRWebServerExtension.resolveConstant(context: TFHIRPathExecutionContext; s: String; var obj: TFHIRObject): boolean;
begin
  if (s = '%server') then
  begin
    result := true;
    obj := TFHIRPathServerObject.create(FContext.link)
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
  FContext.Free;
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


End.


