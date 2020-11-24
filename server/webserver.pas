  Unit webserver;

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
  fsl_openssl, fsl_http, fdb_manager, fhir_htmlgen, fdb_dialects, fsl_rdf, fsl_graphql, fsl_twilio,

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
  tags, session, storage, security, html_builder, ftx_sct_services, ftx_sct_publisher, server_ini,
  scim_server,
  auth_manager, reverse_client, cds_hooks_server, web_source, analytics, bundlebuilder, server_factory,
  user_manager, server_context, server_constants, utilities, jwt, usage_stats,
  {$IFNDEF NO_JS} server_javascript, {$ENDIF}
  subscriptions, {$IFNDEF FHIR3}packages, {$ENDIF}twilio, telnet_server,
  web_base, endpoint;

Type
  TFhirWebServer = class;

  TFHIRServerThread = class (TThread)
  protected
    FServer: TFhirWebServer;

    procedure sendEmail(dest, subj, body : String);
  public
    constructor Create(server: TFhirWebServer; suspended : boolean);
  end;

  TFhirServerMaintenanceThread = class(TFHIRServerThread)
  private
    FLastSweep: TDateTime;
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  TFhirServerSubscriptionThread = class(TFHIRServerThread)
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  TFhirServerEmailThread = class(TFHIRServerThread)
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer);
  end;

  {$IFNDEF FHIR3}
  TPackageUpdaterThread  = class(TFHIRServerThread)
  private
    FDB : TFDBManager;
    FNextRun : TDateTime;
    FLastEmail : TDateTime;
    procedure RunUpdater;
  protected
    procedure Execute; override;
  public
    constructor Create(server: TFhirWebServer; db : TFDBManager);
    destructor Destroy; override;
  end;
  {$ENDIF}

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
    FHostSms: String; // for status update messages
    FTwilioDB : String;
    FTwilioResponse : String;

    // web configuration
    FCertFile: String;
    FRootCertFile: String;
    FSSLPassword: String;
    FInLog : TLogger;
    FOutLog : TLogger;

    // operational fields
    FPlainServer: TIdHTTPServer;
    FSSLServer: TIdHTTPServer;
    FIOHandler: TIdOpenSSLIOHandlerServer {TIdServerIOHandlerSSLOpenSSL};
    FClients: TFslList<TFHIRWebServerClientInfo>;
    FEndPoints : TFslList<TFhirWebServerEndpoint>;
    FMaintenanceThread: TFhirServerMaintenanceThread;
    FSubscriptionThread: TFhirServerSubscriptionThread;
    FEmailThread: TFhirServerEmailThread;
    FIsTerminologyServerOnly: boolean;
    FUsageServer : TUsageStatsServer;
    {$IFNDEF FHIR3}
    FPackageServer : TFHIRPackageServer;
    {$ENDIF}
    FTwilioServer : TTwilioServer;
    FTelnet : TFHIRTelnetServer;

    function TerminologyWebServer: TTerminologyWebServer;

    procedure convertFromVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion; const lang : THTTPLanguages);
    procedure convertToVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion; const lang : THTTPLanguages);
    function WebDump: String;
    procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>); overload;
    Procedure ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil); overload;
    Procedure ReturnSpecFile(response: TIdHTTPResponseInfo; stated, path: String; secure : boolean);

//    function hasInternalSSLToken(request: TIdHTTPRequestInfo): boolean;

    procedure logRequest(secure : boolean; id : String; request : TIdHTTPRequestInfo);
    procedure logResponse(id : String; resp : TIdHTTPResponseInfo);

    // Procedure ReadTags(Headers: TIdHeaderList; Request : TFHIRRequest); overload;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    Procedure ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: boolean);
    procedure MarkEntry(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure MarkExit(AContext: TIdContext);
    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure StartServer(active: boolean);
    Procedure StopServer;
    procedure SSLPassword(Sender: TObject; var Password: string; const IsWrite: Boolean);
    function encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; secure : boolean; fmt : TFHIRFormat; transactionTime : TFslDateTime; names : TStringList) : string;
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    function packageLink: String;
    procedure DoVerifyPeer(Sender: TObject; const x509: TIdOpenSSLX509; const VerifyResult: Integer; const Depth: Integer; var Accepted: Boolean);
    function ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
    function HandleTwilio(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : string;
    procedure smsStatus(Msg: String);
    function endpointList: String;
    procedure DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
  Public
    constructor Create(settings : TFHIRServerSettings; telnet : TFHIRTelnetServer; name: String);
    destructor Destroy; Override;
    procedure loadConfiguration(ini : TFHIRServerIniFile);

    Procedure Start(active, threads: boolean);
    Procedure Stop;

    property EndPoints : TFslList<TFhirWebServerEndpoint> read FEndPoints;
    function EndPoint(name : String) : TFhirWebServerEndpoint;

    property settings : TFHIRServerSettings read FSettings;
    {$IFNDEF FHIR3}
    property PackageServer : TFHIRPackageServer read FPackageServer;
    {$ENDIF}

    property IsTerminologyServerOnly: boolean read FIsTerminologyServerOnly write FIsTerminologyServerOnly;
    function registerEndPoint(code, path : String; context : TFHIRServerContext; ini : TFHIRServerIniFile) : TFhirWebServerEndpoint;
  End;


Function ProcessPath(base, path: String): string;

Implementation

Uses
{$IFDEF WINDOWS}
  Registry,
{$ENDIF}
  fsl_oauth{$IFDEF COVID}, FHIR.Server.Covid{$ENDIF};


{ TFhirWebServer }

Constructor TFhirWebServer.Create(settings : TFHIRServerSettings; telnet : TFHIRTelnetServer; name: String);
Begin
  Inherited Create(nil);
  FTelnet := telnet;
  FEndPoints := TFslList<TFhirWebServerEndpoint>.create;
  self.Common.Name := Name;
  FInLog := nil;

  FSettings := settings;
  FClients := TFslList<TFHIRWebServerClientInfo>.Create;

  {$IFNDEF FHIR3}
  FPackageServer := TFHIRPackageServer.create;
  FPackageServer.OnReturnProcessFileEvent := ReturnProcessedFile;
  {$ENDIF}
  // FAuthRequired := ini.ReadString('fhir', 'oauth-secure', '') = '1';
  // FAppSecrets := ini.ReadString('fhir', 'oauth-secrets', '');
End;

Destructor TFhirWebServer.Destroy;
Begin
  FTelnet.Free;
  FTwilioServer.Free;
  {$IFNDEF FHIR3}
  FPackageServer.Free;
  {$ENDIF}
  FUsageServer.Free;
  FEndPoints.Free;
  FSettings.Free;
  FClients.Free;
  FInLog.Free;
  FOutLog.Free;
  Inherited;
End;


Function ProcessPath(base, path: String): string;
var
  s: String;
begin
  base := base.Substring(0, base.Length - 1);
  if path.StartsWith('..\') then
  begin
    s := base;
    while path.StartsWith('..\') do
    begin
      path := path.Substring(3);
      s := ExtractFilePath(s);
      s := s.Substring(0, s.Length - 1);
    end;
    result := IncludeTrailingPathDelimiter(s) + IncludeTrailingPathDelimiter(path);
  end
  else
    result := IncludeTrailingPathDelimiter(path);
end;

procedure TFhirWebServer.loadConfiguration(ini : TFHIRServerIniFile);
var
  fn: String;
  txu: String;
begin
  fn := ini.admin['logging-in'];
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

  fn := ini.admin['logging-out'];
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
  FFacebookLike := ini.identityProviders['facebook.com']['like'] = 'true';
  Common.Host := ini.web['host'];


  // web server configuration
  Common.ActualPort := StrToIntDef(ini.web['http'], 0);
  Common.ActualSSLPort := StrToIntDef(ini.web['https'], 0);
  {$IFNDEF FHIR3}
  if Common.ActualPort <> 80 then
    FPackageServer.pathAbsolute := 'http://'+Common.host+':'+inttostr(Common.ActualPort)+'/packages'
  else
    FPackageServer.pathAbsolute := 'http://'+Common.host+'/packages';
  FPackageServer.pathRelative := '/packages';
  {$ENDIF}
  FCertFile := ini.web['certname'];
  FRootCertFile := ini.web['cacertname'];
  FSSLPassword := ini.web['password'];

  NoUserAuthentication := ini.web['no-auth'] = 'true';
  UseOAuth := ini.web['oauth'] <> 'false';
  OWinSecuritySecure := ini.web['owin'] = 'true';
  OWinSecurityPlain := ini.web['owin-http'] = 'true';
  ServeMissingCertificate := ini.web['no-cert'] <> 'false';
  ServeUnknownCertificate := ini.web['unknown-cert'] = 'true';
  ServeMissingJWT := ini.web['no-jwt'] = 'true';
  ServeUnverifiedJWT := ini.web['unverified-jwt'] = 'true';
  FUsageServer := TUsageStatsServer.Create(ini.web['stats-dir']);

//  if ini.SectionExists(voVersioningNotApplicable, 'patient-view') then
//  begin
//    ts := TStringList.Create;
//    try
//      ini.ReadSection(voVersioningNotApplicable, 'patient-view', ts);
//      for s in ts do
//        FPatientViewServers.Add(s, ini.ReadString(voVersioningNotApplicable, 'patient-view', s, ''));
//    finally
//      ts.Free;
//    end;
//  end;

  Common.OwnerName := ini.admin['ownername'];
  if Common.OwnerName = '' then
    Common.OwnerName := 'Health Intersections';
  Common.AdminEmail := ini.admin['email'];
  if Common.AdminEmail = '' then
    raise EFHIRException.create('An admin email is required');

  FHostSms := ini.admin['owner-sms'];
  if Common.ActualPort = 80 then
    txu := 'http://' + Common.Host
  else
    txu := 'http://' + Common.Host + ':' + inttostr(Common.ActualPort);

  Common.Google.serverId := ini.web['googleid'];
  FTwilioDB := ini.admin['twilio'];
  FTwilioResponse := ini.admin['twilio-text'];
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

function TFhirWebServer.encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; secure : boolean; fmt : TFHIRFormat; transactionTime: TFslDateTime; names: TStringList): string;
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

function TFhirWebServer.EndPoint(name: String): TFhirWebServerEndpoint;
var
  t : TFhirWebServerEndpoint;
begin
  result := nil;
  for t in FEndPoints do
    if t.code = name then
      exit(t);
end;

Procedure TFhirWebServer.Start(active, threads: boolean);
Begin
  {$IFDEF WINDOWS}
  if FTwilioDB <> '' then
    FTwilioServer := TTwilioServer.Create(TFDBOdbcManager.create('twilio', kdbSqlServer, 20, 5000, 'SQL Server Native Client 11.0', '(local)', FTwilioDB, '', ''), FTwilioResponse);
  {$ENDIF}

  Logging.log('Start Web Server:');
  if (Common.ActualPort = 0) then
    Logging.log('  http: not active')
  else
    Logging.log('  http: listen on ' + inttostr(Common.ActualPort));

  if (Common.ActualSSLPort = 0) then
    Logging.log('  https: not active')
  else
    Logging.log('  https: listen on ' + inttostr(Common.ActualSSLPort));
  FActive := active;
  Common.Stats.Start;
  StartServer(active);

  if (active and threads) then
  begin
    FMaintenanceThread := TFhirServerMaintenanceThread.Create(self);
    FSubscriptionThread := TFhirServerSubscriptionThread.Create(self);
    FEmailThread := TFhirServerEmailThread.Create(self);
  end;
  smsStatus('The server ' + Common.Host + ' for ' + FSettings.OwnerName + ' has started');
End;

procedure TFhirWebServer.smsStatus(Msg: String);
var
  client: TTwilioClient;
begin
  try
    client := TTwilioClient.Create;
    try
      client.Account := FSettings.SMSAccount;
      if (client.Account <> '') and (FHostSms <> '') then
      begin
        client.Token := FSettings.SMSToken;
        client.From := FSettings.SMSFrom;
        client.dest := FHostSms;
        client.Body := Msg;
        client.send;
      end;
    finally
      client.Free;
    end;
  except
  end;
end;

Procedure TFhirWebServer.Stop;
Begin
  if FActive then
    smsStatus('The server ' + Common.Host + ' for ' + FSettings.OwnerName + ' is stopping');
  FActive := false;
  if FSubscriptionThread <> nil then
    FSubscriptionThread.Terminate;
  if FMaintenanceThread <> nil then
    FMaintenanceThread.Terminate;
  if FEmailThread <> nil then
    FEmailThread.Terminate;
  StopServer;
End;

Procedure TFhirWebServer.StartServer(active: boolean);
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
    FPlainServer.active := active;
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

//    FIOHandler.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
//    FIOHandler.Options.TLSVersionMaximum := TIdOpenSSLVersion.TLSv1_3;
//    FIOHandler.Options.UseServerCipherPreferences := true;
//    FIOHandler.Options.AllowUnsafeLegacyRenegotiation := true;
//    FIOHandler.Options.UseLegacyServerConnect := true;
//
//   FIOHandler.Options.CipherList := {$IFDEF NCTS}'ALL:!SSLv2:!DES:!RC4:!MD5:!SHA-1'{$ELSE}'ALL:!SSLv2:!DES'{$ENDIF};
//   FIOHandler.Options.CipherSuites := '';
//    FIOHandler.Options.RequestCertificate := true;
//    FIOHandler.Options.RequestCertificateOnlyOnce := true;
//    FIOHandler.Options.FailIfNoPeerCertificate := not FServeMissingCertificate;
//    FIOHandler.Options.OnVerify := DoVerifyPeer;
    FSSLServer.OnCommandGet := SecureRequest;
    FSSLServer.OnCommandOther := SecureRequest;
    FSSLServer.OnConnect := DoConnect;
    FSSLServer.OnDisconnect := DoDisconnect;
    FSSLServer.OnParseAuthentication := ParseAuthenticationHeader;
    FSSLServer.active := active;
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

function TFhirWebServer.TerminologyWebServer: TTerminologyWebServer;
begin
  result := EndPoints[0].TerminologyWebServer;
end;

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
        if request.Document.StartsWith(ep.path) then
        begin
          ok := true;
          summ := ep.PlainRequest(AContext, request, response, id);
        end;
      if not ok then
      begin
        if request.Document = '/diagnostics' then
          summ := ReturnDiagnostics(AContext, request, response, false, false)
        else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestNoVersion(request.Document) then
        begin
          summ := TerminologyWebServer.ProcessNoVersion(AContext, request, nil, response, false)
        end
        {$IFNDEF FHIR3}
        else if (FPackageServer.DB <> nil) and request.Document.startsWith('/packages') then
          summ := FPackageServer.serve(request, response)
        {$ENDIF}
        else if Common.SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
        begin
          summ := 'Static File';
          ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
        end
        else if request.Document = '/' then
        begin
          summ := 'processed File';
          ReturnProcessedFile(request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), false);
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
    raise Exception.Create('todo');
    cert := nil; // todo (AContext.Connection.IOHandler as TIdSSLIOHandlerSocketOpenSSL).SSLSocket.PeerCert;

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
          if request.Document.StartsWith(ep.path) then
          begin
            ok := true;
            summ := ep.SecureRequest(AContext, request, response, cert, id);
          end;
        if not ok then
        begin
          if request.Document = '/diagnostics' then
            summ := ReturnDiagnostics(AContext, request, response, false, false)
          else if (TerminologyWebServer <> nil) and TerminologyWebServer.handlesRequestNoVersion(request.Document) then
          begin
            summ := TerminologyWebServer.ProcessNoVersion(AContext, request, nil, response, false)
          end
          {$IFNDEF FHIR3}
          else if (FPackageServer.DB <> nil) and request.Document.startsWith('/packages') then
            summ := FPackageServer.serve(request, response)
          {$ENDIF}
          else if request.Document = '/twilio' then
            summ := HandleTwilio(AContext, request, response, false, false)
          else if SourceProvider.exists(SourceProvider.AltFile(request.Document, '/')) then
          begin
            summ := 'Static File';
            ReturnSpecFile(response, request.Document, SourceProvider.AltFile(request.Document, '/'), false)
          end
          else if request.Document = '/' then
          begin
            summ := 'Processed File';
            ReturnProcessedFile(request, response, '/' + FHomePage, SourceProvider.AltFile('/' + FHomePage, ''), true)
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
      {$IFNDEF OSX}
  //    if GService <> nil then
  //      Logging.log(GService.ThreadStatus);
      {$ENDIF}
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

function TFhirWebServer.packageLink: String;
begin
  {$IFNDEF FHIR3}
  if FPackageServer <> nil then
    result := '<p>This server also runs as a <a href="'+FPackageServer.pathRelative+'">package server</a></p>'
  else
  {$ENDIF}
    result := '';
end;

function TFhirWebServer.HandleTwilio(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
begin
  result := FTwilioServer.process(AContext, request, response);
end;

procedure TFhirWebServer.convertFromVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion; const lang : THTTPLanguages);
var
  b : TBytes;
begin
  {$IfDEF NO_CONVERSION}
  raise EFHIRException.create('Version Conversion Services are not made available on this server');
  {$ELSE}
  b := StreamToBytes(stream);
  b := TFhirVersionConvertors.convertResource(b, format, OutputStyleNormal, lang, version, CURRENT_FHIR_VERSION);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.Position := 0;
  {$ENDIF}
end;

procedure TFhirWebServer.convertToVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion; const lang : THTTPLanguages);
var
  b : TBytes;
begin
  {$IfDEF NO_CONVERSION}
  raise EFHIRException.create('Version Conversion Services are not made available on this server');
  {$ELSE}
  b := StreamToBytes(stream);
  b := TFhirVersionConvertors.convertResource(b, format, OutputStyleNormal, lang, CURRENT_FHIR_VERSION, version);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.Position := 0;
  {$ENDIF}
end;


function TFhirWebServer.ReturnDiagnostics(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
var
  vars : TFslMap<TFHIRObject>;
  ep : TFhirWebServerEndpoint;
begin
  vars := TFslMap<TFHIRObject>.Create('dx.vars');
  try
    vars.Add('status.db', TFHIRSystemString.Create(FormatTextToHTML(DBManagers.dump)));
    vars.Add('live.connections', TFHIRSystemString.Create(inttostr(GCounterWebConnections)));
    vars.Add('live.requests', TFHIRSystemString.Create(inttostr(GCounterWebRequests)));
    vars.Add('live.requests.kernel', TFHIRSystemString.Create(inttostr(GCounterFHIRRequests)));
    vars.Add('status.locks', TFHIRSystemString.Create(FormatTextToHTML(DumpLocks)));
    vars.Add('status.thread.maintenance', TFHIRSystemString.Create(FSettings.MaintenanceThreadStatus));
    vars.Add('status.thread.subscriptions', TFHIRSystemString.Create(FSettings.SubscriptionThreadStatus));
    vars.Add('status.thread.email', TFHIRSystemString.Create(FSettings.EmailThreadStatus));
    for ep in FEndPoints do
    begin
      vars.Add('status.'+ep.Code+'.sessions', TFHIRSystemString.Create(ep.Context.SessionManager.DumpSessions));
      vars.Add('status.'+ep.Code+'.tx', TFHIRSystemString.Create(ep.Context.TerminologyServer.Summary));
      vars.Add('status.'+ep.Code+'.cdsclient', TFHIRSystemString.Create(inttostr(ep.PatientHooks.Count)));
    end;
    vars.Add('status.web', TFHIRSystemString.Create(WebDump));
    vars.Add('status.web-total-count', TFHIRSystemString.Create(inttostr(Common.Stats.TotalCount)));
    vars.Add('status.web-rest-count', TFHIRSystemString.Create(inttostr(Common.Stats.RestCount)));
    vars.Add('status.web-total-time', TFHIRSystemString.Create(inttostr(Common.Stats.TotalTime)));
    vars.Add('status.web-rest-time', TFHIRSystemString.Create(inttostr(Common.Stats.RestTime)));
    vars.Add('status.run-time', TFHIRSystemString.Create(DescribePeriod((GetTickCount - Common.Stats.StartTime) * DATETIME_MILLISECOND_ONE)));
    vars.Add('status.run-time.ms', TFHIRSystemString.Create(inttostr(GetTickCount - Common.Stats.StartTime)));
    ReturnProcessedFile(request, response, 'Diagnostics', SourceProvider.AltFile('/diagnostics.html', ''), false, vars);
  finally
    vars.Free;
  end;
  result := 'Diagnostics';
end;

//procedure TFhirWebServer.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
//begin
//  ReturnProcessedFile(request, response, path, path, secure, variables);
//end;
//
function TFhirWebServer.endpointList : String;
var
  b : TStringBuilder;
  ep : TFhirWebServerEndpoint;
begin
  b := TStringBuilder.create;
  try
    b.append('<ul>');
    for ep in FEndPoints do
      b.Append('<li><a href="'+ep.Path+'">'+ep.Path+'</a>: v'+ep.factory.versionString+'</li>');

    b.append('</ul>');
    result := b.toString;
  finally
    b.free;
  end;
end;

procedure TFhirWebServer.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>);
begin
  ReturnProcessedFile(request, response, named, path, secure, variables);
end;

procedure TFhirWebServer.ReturnProcessedFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject> = nil);
var
  s, n: String;
begin
  s := SourceProvider.getSource(actual);
  s := s.Replace('[%id%]', Common.Name, [rfReplaceAll]);
  s := s.Replace('[%specurl%]', 'http://hl7.org/fhir', [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%admin%]', Common.AdminEmail, [rfReplaceAll]);
  s := s.Replace('[%logout%]', 'User: [n/a]', [rfReplaceAll]);
  s := s.Replace('[%endpoints%]', endpointList, [rfReplaceAll]);
  s := s.Replace('[%package-link%]', packageLink, [rfReplaceAll]);
  if Common.ActualPort = 80 then
    s := s.Replace('[%host%]', Common.Host, [rfReplaceAll])
  else
    s := s.Replace('[%host%]', Common.Host + ':' + inttostr(Common.ActualPort), [rfReplaceAll]);

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

//  s := s.Replace('[%endpoints%]', EndPointDesc(secure), [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%' + n + '%]', variables[n].primitiveValue, [rfReplaceAll]);

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

function TFhirWebServer.registerEndPoint(code, path: String; context: TFHIRServerContext; ini : TFHIRServerIniFile): TFhirWebServerEndpoint;
begin
  result := TFhirWebServerEndpoint.create(code, path, context, Common.Link);
  FEndPoints.Add(result);
  context.userProvider.OnProcessFile := result.ReturnProcessedFile;
  result.AuthServer := TAuth2Server.Create(context.Factory.link, ini, Common.Host, inttostr(Common.SslPort), path);
  result.AuthServer.UserProvider := context.userProvider.Link;
  result.AuthServer.ServerContext := context.Link;
  result.AuthServer.EndPoint := result.ClientAddress(true);
  result.AuthServer.OnProcessFile := result.ReturnProcessedFile;
  result.AuthServer.OnGetPatients := result.GetPatients;
  result.AuthServer.OnProcessLaunchParams := result.GetLaunchParameters;
  result.AuthServer.Active := true;
  context.JWTServices := TJWTServices.Create;
  context.JWTServices.Cert := FCertFile;
  context.JWTServices.Password := FSSLPassword;
  context.JWTServices.DatabaseId := context.DatabaseId;
  context.JWTServices.Host := Common.Host;
  context.FormalURLPlain := 'http://'+Common.host+port(Common.ActualPort, 80)+path;
  context.FormalURLSecure := 'https://'+Common.host+port(Common.ActualSSLPort, 443)+path;
//  context.JWTServices.JWKAddress := ?;
end;

// procedure TFhirWebServer.DoSendFHIR(iMsgKey, SrcID: Integer; request: TFHIRRequest; response: TFHIRResponse);
// var
// client : TFhirHTTPClient;
// begin
// client := TFhirHTTPClient.create(FBaseURL, false);
// try
// FClientLock.Lock('MakeClient');
// Try
// FClients.Add(client);
// Finally
// FClientLock.Unlock;
// End;
// try
// if (request.CommandType = fcmdUpdate) and (request.id = '') then
// request.id := 'test';
//
// client.doRequest(request, response);
// finally
// FClientLock.Lock('CloseClient');
// Try
// FClients.Remove(client);
// Finally
// FClientLock.Unlock;
// End;
// end;
// finally
// client.free;
// end;
// end;
//

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  FLastSweep := Now;
  inherited Create(server, false);
end;

procedure TFhirServerMaintenanceThread.Execute;
var
  ep : TFhirWebServerEndpoint;
begin
  SetThreadName('Server Maintenance Thread');
  SetThreadStatus('Working');
  Logging.log('Starting TFhirServerMaintenanceThread');
  try
    FServer.FSettings.MaintenanceThreadStatus := 'starting';
{$IFDEF WINDOWS}
    CoInitialize(nil);
{$ENDIF}
    {$IFNDEF NO_JS}
    GJsHost := TJsHost.Create;
    FServer.Common.OnRegisterJs(self, GJsHost);
    {$ENDIF}
//    GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;

    repeat
      FServer.Settings.MaintenanceThreadStatus := 'sleeping';
      sleep(1000);
      if not terminated and (FLastSweep < Now - (DATETIME_SECOND_ONE * 5)) then
      begin
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Sessions';
          for ep in FServer.FEndPoints do
            ep.Context.Storage.Sweep;
          if not FServer.settings.ForLoad then
            FServer.Common.Google.commit;
        except
        end;
        FLastSweep := Now;
      end;
      if not FServer.settings.ForLoad then
      begin
        if (not terminated) then
          try
            FServer.Settings.MaintenanceThreadStatus := 'Building Indexes';
            for ep in FServer.FEndPoints do
              ep.Context.TerminologyServer.BuildIndexes(false);
          except
          end;
      end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Processing Observations';
          for ep in FServer.FEndPoints do
            ep.Context.Storage.ProcessObservations;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Checking Snomed';
          FServer.FEndPoints.First.Context.TerminologyServer.CommonTerminologies.sweepSnomed;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Checking Async Tasks';
          for ep in FServer.FEndPoints do
            ep.CheckAsyncTasks;
        except
        end;
      if (not terminated) and (FServer.FTwilioServer <> nil) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Twilio';
          FServer.FTwilioServer.sweep;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Client Cache';
          for ep in FServer.FEndPoints do
            ep.Context.ClientCacheManager.sweep;
        except
        end;
    until terminated;
    try
      FServer.Settings.MaintenanceThreadStatus := 'dead';
    except
    end;
    try
      FServer.FMaintenanceThread := nil;
    except
    end;
    {$IFNDEF NO_JS}
    GJsHost.Free;
    GJsHost := nil;
    {$ENDIF}


{$IFDEF WINDOWS}
    CoUninitialize;
{$ENDIF}
    Logging.log('Ending TFhirServerMaintenanceThread');
  except
    Logging.log('Failing TFhirServerMaintenanceThread');
  end;
  SetThreadStatus('Done');
  closeThread;
end;

{ TFhirServerSubscriptionThread }

constructor TFhirServerSubscriptionThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create(server, false);
end;

procedure TFhirServerSubscriptionThread.Execute;
var
  ep : TFhirWebServerEndpoint;
begin
  SetThreadName('Server Subscription Thread');
  SetThreadStatus('Working');
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  FServer.Common.OnRegisterJs(self, GJsHost);
  {$ENDIF}
//  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  Logging.log('Starting TFhirServerSubscriptionThread');
  try
    FServer.Settings.SubscriptionThreadStatus := 'starting';
    repeat
      FServer.Settings.SubscriptionThreadStatus := 'sleeping';
      sleep(1000);
      if FServer.FActive then
      begin
        FServer.Settings.SubscriptionThreadStatus := 'processing subscriptions';
        for ep in FServer.FEndPoints do
          ep.Context.Storage.ProcessSubscriptions;
      end;
    until terminated;
    try
      FServer.Settings.SubscriptionThreadStatus := 'dead';
    except
    end;
    try
      FServer.FSubscriptionThread := nil;
    except
    end;
    Logging.log('Ending TFhirServerSubscriptionThread');
  except
    Logging.log('Failing TFhirServerSubscriptionThread');
  end;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
  closeThread;
end;

{ TFhirServerEmailThread }

constructor TFhirServerEmailThread.Create(server: TFhirWebServer);
begin
  FreeOnTerminate := true;
  inherited Create(server, false);
end;

procedure TFhirServerEmailThread.Execute;
var
  i: integer;
  ep : TFhirWebServerEndpoint;
begin
  SetThreadName('Server Email Thread');
  SetThreadStatus('Working');
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  FServer.Common.OnRegisterJs(self, GJsHost);
  {$ENDIF}
//  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  Logging.log('Starting TFhirServerEmailThread');
  try
    FServer.Settings.EmailThreadStatus := 'starting';
    repeat
      FServer.Settings.EmailThreadStatus := 'sleeping';
      i := 0;
      while not terminated and (i < 60) do
      begin
        sleep(1000);
        inc(i);
      end;
      if FServer.FActive and not terminated then
      begin
        FServer.Settings.EmailThreadStatus := 'processing Emails';
        for ep in FServer.FEndPoints do
          ep.Context.Storage.ProcessEmails;
      end;
    until terminated;
    try
      FServer.Settings.EmailThreadStatus := 'dead';
    except
    end;
    try
      FServer.FEmailThread := nil;
    except
    end;
    Logging.log('Ending TFhirServerEmailThread');
  except
    Logging.log('Failing TFhirServerEmailThread');
  end;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
  closeThread;
end;

{$IFNDEF FHIR3}
{ TPackageUpdaterThread }

constructor TPackageUpdaterThread.Create(server: TFhirWebServer; db: TFDBManager);
begin
  inherited create(server, false);
  FDB := db;
  FNextRun := now + 1/(24 * 60);
end;

destructor TPackageUpdaterThread.Destroy;
begin
  FDB.Free;
  inherited;
end;

procedure TPackageUpdaterThread.Execute;
begin
  repeat
    sleep(50);
    if not Terminated and (now > FNextRun) and (FNextRun > 0) then
    begin
      FServer.FPackageServer.scanning := true;
      try
        RunUpdater;
      finally
        FServer.FPackageServer.scanning := false;
      end;
      FNextRun := now + 1/24;
      FServer.FPackageServer.NextScan := FNextRun;
    end;
  until (Terminated);
end;

procedure TPackageUpdaterThread.RunUpdater;
var
  conn : TFDBConnection;
  upd : TPackageUpdater;
begin
  conn := FDB.getConnection('server.packages.update');
  try
    upd := TPackageUpdater.create;
    try
      try
        upd.update(conn);
        if (TFslDateTime.makeToday.DateTime <> FLastEmail) then
        begin
          if upd.errors <> '' then
            sendEmail('grahameg@gmail.com', 'Package Feed Errors', upd.errors);
          FLastEmail := TFslDateTime.makeToday.DateTime;
        end;
      except
        on e : exception do
        begin
          Logging.log('Exception updating packages: '+e.Message);
        end;
      end;
    finally
      upd.free;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;
{$ENDIF}

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

{ TFHIRServerThread }

constructor TFHIRServerThread.Create(server: TFhirWebServer;
  suspended: boolean);
begin
  FServer := server;
  inherited Create(suspended);
end;

procedure TFHIRServerThread.sendEmail(dest, subj, body: String);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
begin
  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := FServer.settings.SMTPHost;
    sender.port := StrToInt(FServer.settings.SMTPPort);
    sender.Username := FServer.settings.SMTPUsername;
    sender.Password := FServer.settings.SMTPPassword;
    if FServer.settings.SMTPUseTLS then
    begin
      ssl := TIdOpenSSLIOHandlerClient.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := FServer.settings.SMTPHost+':'+FServer.settings.SMTPPort;
      ssl.Host := FServer.settings.SMTPHost;
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(FServer.settings.SMTPPort);
      ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
      ssl.Options.VerifyServerCertificate := false;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := subj;
      msg.Recipients.Add.Address := dest;
      msg.From.Text := FServer.settings.SMTPSender;
      msg.Body.Text := body;
      Logging.log('Send '+msg.MsgId+' to '+dest);
      sender.Send(msg);
    Finally
      msg.Free;
    End;
    sender.Disconnect;
  Finally
    sender.IOHandler.free;
    sender.Free;
  End;
end;


End.


